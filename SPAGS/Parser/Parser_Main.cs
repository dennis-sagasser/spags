using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ScriptParser
    {
        Version engineVersion;

        public ScriptParser(Version engineVersion)
        {
            this.engineVersion = engineVersion;
        }

        Function CurrentFunction;

        Script script;
        public Script ReadScript(string scriptText, Script targetScript)
        {
            script = targetScript;
            script.Identifiers = Identifiers;
            InitialiseTokenStream(scriptText);

            while (token.Type != TokenType.EndOfInput)
            {
                switch (token.Type)
                {
                    case TokenType.Managed:
                    case TokenType.AutoPtr:
                    case TokenType.InternalString:
                    case TokenType.Struct:
                        bool mod_managed = false, mod_autoptr = false, mod_internalstring = false;
                        while (token.Type != TokenType.Struct)
                        {
                            switch (token.Type)
                            {
                                case TokenType.Managed:
                                    AdvanceToken(/* TokenType.Managed */);
                                    mod_managed = true;
                                    continue;
                                case TokenType.AutoPtr:
                                    AdvanceToken(/* TokenType.AutoPtr */);
                                    mod_autoptr = true;
                                    continue;
                                case TokenType.InternalString:
                                    AdvanceToken(/* TokenType.Enum */);
                                    mod_internalstring = true;
                                    continue;
                                default: throw new Exception("expecting struct");
                            }
                        }
                        AdvanceToken(/* TokenType.Struct */);
                        INameHolder existing;
                        string structName = AdvanceName(out existing);
                        ValueType.Struct newStruct;
                        if (existing != null)
                        {
                            newStruct = existing as ValueType.Struct;
                            if (existing == null)
                            {
                                throw new Exception("name already in use: " + structName);
                            }
                        }
                        else
                        {
                            newStruct = new ValueType.Struct(structName);
                            Identifiers.Add(newStruct);
                        }
                        newStruct.OwnerScript = script;
                        script.DefinedStructs.Add(newStruct);
                        newStruct.IsAutoPtr = mod_autoptr;
                        newStruct.IsInternalString = mod_internalstring;
                        newStruct.IsManaged = mod_managed;
                        if (token.Type == TokenType.Extends)
                        {
                            AdvanceToken(/* TokenType.Extends */);
                            ValueType.Struct extending = AdvanceNameHolder<ValueType.Struct>("name of struct to extend");
                            foreach (KeyValuePair<string,INameHolder> memberEntry in extending.Members)
                            {
                                newStruct.Members.Add(memberEntry.Key, memberEntry.Value);
                            }
                        }
                        if (token.Type == TokenType.Semicolon)
                        {
                            AdvanceToken(/* TokenType.Semicolon */);
                            continue;
                        }
                        AdvanceToken(TokenType.LeftCurlyBrace);
                        while (token.Type != TokenType.RightCurlyBrace)
                        {
                            bool mod_readonly = false, mod_static = false, mod_import = false, mod_tryimport = false,
                                mod_protected = false, mod_writeprotected = false;
                            for (; ; )
                            {
                                switch (token.Type)
                                {
                                    case TokenType.ReadOnly:
                                        AdvanceToken(/* TokenType.ReadOnly */);
                                        mod_readonly = true;
                                        continue;
                                    case TokenType.Static:
                                        AdvanceToken(/* TokenType.Static */);
                                        mod_static = true;
                                        continue;
                                    case TokenType.Import:
                                        AdvanceToken(/* TokenType.Import */);
                                        mod_import = true;
                                        continue;
                                    case TokenType._TryImport:
                                        AdvanceToken(/* TokenType._TryImport */);
                                        mod_tryimport = true;
                                        continue;
                                    case TokenType.Protected:
                                        AdvanceToken(/* TokenType.Protected */);
                                        mod_protected = true;
                                        continue;
                                    case TokenType.WriteProtected:
                                        AdvanceToken(/* TokenType.WriteProtected */);
                                        mod_writeprotected = true;
                                        continue;
                                    default: break;
                                }
                                break;
                            }
                            if (token.Type == TokenType.Attribute)
                            {
                                AdvanceToken(/* TokenType.Attribute */);
                                ValueType attrType = AdvanceValueType();
                                string attrName = AdvanceName();
                                if (newStruct.Members.ContainsKey(attrName))
                                {
                                    throw new Exception("struct " + newStruct + " already has a member called " + attrName);
                                }
                                Function getter, setter = null;
                                bool mod_array = (token.Type == TokenType.LeftSquareBracket);
                                if (mod_array)
                                {
                                    AdvanceToken(/* TokenType.LeftSquareBracket */);
                                    AdvanceToken(TokenType.RightSquareBracket);
                                    ParameterList getterParams = new ParameterList();
                                    if (!mod_static) getterParams.Add("this", newStruct);
                                    getterParams.Add("index", ValueType.Int);
                                    getter = new Function(structName + "::geti_" + attrName, attrType, getterParams);
                                    if (!mod_readonly)
                                    {
                                        ParameterList setterParams = new ParameterList();
                                        if (!mod_static) setterParams.Add("this", newStruct);
                                        setterParams.Add("index", ValueType.Int);
                                        setterParams.Add("value", attrType);
                                        setter = new Function(structName + "::seti_" + attrName, ValueType.Void, setterParams);
                                    }
                                }
                                else
                                {
                                    ParameterList getterParams = new ParameterList();
                                    if (!mod_static) getterParams.Add("this", newStruct);
                                    getter = new Function(structName + "::get_" + attrName, attrType, getterParams);
                                    if (!mod_readonly)
                                    {
                                        ParameterList setterParams = new ParameterList();
                                        if (!mod_static) setterParams.Add("this", newStruct);
                                        setterParams.Add("value", attrType);
                                        setter = new Function(structName + "::set_" + attrName, ValueType.Void, setterParams);
                                    }
                                }
                                AdvanceToken(TokenType.Semicolon);

                                newStruct.Members.Add(new StructMember.Attribute(attrName, attrType, mod_static, mod_array, getter, setter));

                                Identifiers.Add(getter);
                                if (!mod_readonly)
                                {
                                    Identifiers.Add(setter);
                                }

                                continue;
                            }
                            ValueType valueType = AdvanceValueType();
                            string name = AdvanceName();
                            if (token.Type == TokenType.LeftParenthesis)
                            {
                                // method
                                AdvanceToken(/* TokenType.LeftParenthesis */);
                                ParameterList methodParams = AdvanceParameterList();
                                AdvanceToken(TokenType.Semicolon);
                                if (!mod_static) methodParams.Insert(0, new ParameterDef("this", newStruct, null));
                                Function methodFunction = new Function(structName + "::" + name, valueType, methodParams);
                                Identifiers.Add(methodFunction);
                                StructMember.Method method = new StructMember.Method(name, methodFunction);
                                method.IsStatic = mod_static;
                                newStruct.Members.Add(method);
                                continue;
                            }
                            // field(s)
                            for (; ; )
                            {
                                if (token.Type == TokenType.LeftSquareBracket)
                                {
                                    AdvanceToken(/* TokenType.LeftSquareBracket */);
                                    Expression arrayLength = AdvanceExpression();
                                    AdvanceToken(TokenType.RightSquareBracket);
                                    newStruct.Members.Add(new StructMember.Field(name, new ValueType.Array(valueType, arrayLength)));

                                    if (valueType is ValueType.Struct) ((ValueType.Struct)valueType).InstantiatedArray = true;
                                }
                                else
                                {
                                    newStruct.Members.Add(new StructMember.Field(name, valueType));
                                    if (valueType is ValueType.Struct) ((ValueType.Struct)valueType).Instantiated = true;
                                }
                                if (token.Type == TokenType.Comma)
                                {
                                    AdvanceToken();
                                    name = AdvanceName();
                                    continue;
                                }
                                break;
                            }
                            AdvanceToken(TokenType.Semicolon);

                        }
                        AdvanceToken(/* Token.RightCurlyBrace */);
                        AdvanceToken(TokenType.Semicolon);
                        continue;

                    case TokenType.Enum:
                        {
                            AdvanceToken(/* TokenType.Enum */);
                            ValueType.Enum enumType = new ValueType.Enum(AdvanceUnusedName());
                            enumType.OwnerScript = script;
                            script.DefinedEnums.Add(enumType);
                            Identifiers.Add(enumType);
                            AdvanceToken(TokenType.LeftCurlyBrace);
                            EnumValue previous = null;
                            while (token.Type != TokenType.RightCurlyBrace)
                            {
                                string enumEntry = AdvanceUnusedName();
                                Expression explicitValue = AdvancePossibleAssignment();
                                EnumValue newValue = new EnumValue(enumType, enumEntry, explicitValue, previous);
                                enumType.Entries.Add(newValue);
                                Identifiers.Add(newValue);
                                previous = newValue;
                                if (token.Type == TokenType.Comma)
                                {
                                    AdvanceToken(/* TokenType.Comma */);
                                    // trailing commas are allowed in enums
                                    if (token.Type == TokenType.RightCurlyBrace)
                                    {
                                        break;
                                    }
                                    continue;
                                }
                                else if (token.Type != TokenType.RightCurlyBrace)
                                {
                                    throw new Exception("expecting , or }");
                                }
                            }
                            AdvanceToken(/* TokenType.RightCurlyBrace */);
                            AdvanceToken(TokenType.Semicolon);
                            continue;
                        }

                    case TokenType.Export:
                        {
                            AdvanceToken(/* TokenType.Export */);
                            for (; ; )
                            {
                                // Thanks to Downhill Module for revealing that functions CAN be explicitly exported
                                // even though they are automatically
                                INameHolder holder = AdvanceNameHolder<INameHolder>("export token");
                                if (holder is ScriptVariable) ((ScriptVariable)holder).Exported = true;
                                if (token.Type == TokenType.Comma)
                                {
                                    AdvanceToken(/* TokenType.Comma */);
                                    continue;
                                }
                                break;
                            }
                            AdvanceToken(TokenType.Semicolon);
                            continue;
                        }

                    case TokenType.ReadOnly:
                    case TokenType.Protected:
                    case TokenType.Import:
                    case TokenType._TryImport:
                    case TokenType.Static:
                    case TokenType.Const:
                    case TokenType.KnownWord:
                        {
                            bool mod_static = false, mod_import = false, mod_readonly = false, mod_protected = false,
                                mod_tryimport = false;
                            for (; ; )
                            {
                                switch (token.Type)
                                {
                                    case TokenType.Static:
                                        AdvanceToken(/* TokenType.Static */);
                                        mod_static = true;
                                        continue;
                                    case TokenType.Protected:
                                        AdvanceToken(/* TokenType.Protected */);
                                        mod_protected = true;
                                        continue;
                                    case TokenType.Import:
                                        AdvanceToken(/* TokenType.Import */);
                                        mod_import = true;
                                        continue;
                                    case TokenType._TryImport:
                                        AdvanceToken(/* TokenType._TryImport */);
                                        mod_tryimport = true;
                                        continue;
                                    case TokenType.ReadOnly:
                                        AdvanceToken(/* TokenType.ReadOnly */);
                                        mod_readonly = true;
                                        continue;
                                }
                                break;
                            }

                            ValueType valueType = AdvanceValueType();

                            bool mod_array = (token.Type == TokenType.LeftSquareBracket);
                            if (mod_array)
                            {
                                AdvanceToken(/* TokenType.LeftSquareBracket */);
                                AdvanceToken(TokenType.RightSquareBracket);
                            }

                            bool mod_noloopcheck = (token.Type == TokenType.NoLoopCheck);
                            if (mod_noloopcheck) AdvanceToken(/* TokenType.NoLoopCheck */);

                            INameHolder existingHolder;
                            string name = AdvanceName(out existingHolder);

                            if (token.Type != TokenType.DoubleColon && token.Type != TokenType.LeftParenthesis && token.Type != TokenType.NoLoopCheck)
                            {
                                // script variable
                                for(; ; )
                                {
                                    ValueType individualValueType;
                                    Expression initialValue = null;
                                    if (token.Type == TokenType.LeftSquareBracket)
                                    {
                                        AdvanceToken(/* TokenType.LeftSquareBracket */);
                                        if (token.Type == TokenType.RightSquareBracket)
                                        {
                                            AdvanceToken(/* TokenType.RightSquareBracket */);
                                            individualValueType = new ValueType.Array(valueType, null);
                                        }
                                        else
                                        {
                                            individualValueType = new ValueType.Array(valueType, AdvanceExpression());
                                            AdvanceToken(TokenType.RightSquareBracket);
                                        }
                                        if (!(mod_import || mod_tryimport) && valueType is ValueType.Struct) ((ValueType.Struct)valueType).InstantiatedArray = true;
                                    }
                                    else
                                    {
                                        individualValueType = valueType;
                                        initialValue = AdvancePossibleAssignment();
                                        if (!(mod_import || mod_tryimport) && valueType is ValueType.Struct) ((ValueType.Struct)valueType).Instantiated = true;
                                    }

                                    if (existingHolder != null)
                                    {
                                        ScriptVariable earlierVariable = existingHolder as ScriptVariable;
                                        if (earlierVariable == null || earlierVariable.Defined)
                                        {
                                            throw new Exception("name already in use: " + name);
                                        }
                                        earlierVariable.Defined = true;
                                        earlierVariable.OwnerScript = script;
                                        script.DefinedVariables.Add(earlierVariable);
                                    }
                                    else
                                    {
                                        ScriptVariable newVar = new ScriptVariable(name, individualValueType, initialValue);
                                        newVar.Imported = mod_import || mod_tryimport;
                                        newVar.Defined = !newVar.Imported;
                                        if (!newVar.Imported)
                                        {
                                            newVar.OwnerScript = script;
                                            script.DefinedVariables.Add(newVar);
                                        }
                                        newVar.ReadOnly = mod_readonly;
                                        Identifiers.Add(newVar);
                                    }

                                    if (token.Type == TokenType.Comma)
                                    {
                                        AdvanceToken(/* TokenType.Comma */);
                                        name = AdvanceName(out existingHolder);
                                        continue;
                                    }
                                    break;
                                }
                                AdvanceToken(TokenType.Semicolon);
                                continue;
                            }

                            // script function

                            ValueType thisType = null;

                            if (!(mod_import || mod_tryimport) && token.Type == TokenType.DoubleColon)
                            {
                                AdvanceToken(/* TokenType.DoubleColon */);
                                string memberName = AdvanceName();
                                ValueType.Struct implementingType;
                                if (!Identifiers.TryGetValue2<ValueType.Struct>(name, out implementingType))
                                {
                                    throw new Exception("struct not found: " + name);
                                }
                                StructMember.Method implementingMethod;
                                if (!implementingType.Members.TryGetValue2<StructMember.Method>(memberName, out implementingMethod))
                                {
                                    throw new Exception("method not found: " + name + "." + memberName + "()");
                                }
                                if (!implementingMethod.IsStatic) thisType = implementingType;
                                name += "::" + memberName;
                            }

                            Function function;

                            AdvanceToken(TokenType.LeftParenthesis);
                            bool extending = (CurrentName == "this");
                            string extenderName = name;
                            if (extending)
                            {
                                AdvanceToken();
                                thisType = AdvanceValueType();
                                if (thisType.Category != ValueType.ValueTypeCategory.Struct)
                                {
                                    throw new Exception("cannot add extender method to " + thisType);
                                }
                                name = thisType.Name + "::" + name;
                                if (token.Type == TokenType.Comma)
                                {
                                    AdvanceToken(/* TokenType.Comma */);
                                }
                            }

                            ParameterList parameters = AdvanceParameterList();

                            if (thisType != null)
                            {
                                parameters.Insert(0, new ParameterDef("this", thisType, null));
                            }

                            if (LookUpName(name, out existing))
                            {
                                function = existing as Function;
                                if (function == null)
                                {
                                    throw new Exception("name already in use: " + name);
                                }
                            }
                            else
                            {
                                if (mod_array) valueType = new ValueType.Array(valueType, null);
                                function = new Function(name, valueType, parameters);
                                Identifiers.Add(function);
                            }

                            function.NoLoopCheck = mod_noloopcheck;

                            if (mod_import || mod_tryimport)
                            {
                                AdvanceToken(TokenType.Semicolon);
                            }
                            else
                            {
                                CurrentFunction = function;

                                NameDictionary parameterScope = new NameDictionary();
                                foreach (ParameterDef pdef in parameters)
                                {
                                    Parameter param = new Parameter(pdef.Name, pdef.Type);
                                    function.ParameterVariables.Add(param);
                                    parameterScope.Add(param);
                                }

                                function.Body = AdvanceBlock(parameterScope);
                                function.OwnerScript = script;
                                script.DefinedFunctions.Add(function);
                            }

                            if (extending)
                            {
                                ValueType.Struct thisStruct = (ValueType.Struct)thisType;
                                StructMember.Method extenderMethod;
                                if (thisStruct.Members.TryGetValue2<StructMember.Method>(extenderName, out extenderMethod))
                                {
                                    extenderMethod.Function.Body = function.Body;
                                }
                                else
                                {
                                    extenderMethod = new StructMember.Method(extenderName, function);
                                    extenderMethod.IsExtender = true;
                                    thisStruct.Members.Add(extenderMethod);
                                }
                            }

                            continue;
                        }

                    default:
                        throw new Exception("unexpected " + token);
                }
            }

            return script;
        }

        private ParameterList AdvanceParameterList()
        {
            ParameterList parameters = new ParameterList();
            while (token.Type != TokenType.RightParenthesis)
            {
                if (token.Type == TokenType.DotDotDot)
                {
                    AdvanceToken(/* TokenType.DotDotDot */);
                    parameters.HasVarArgs = true;
                    AdvanceToken(TokenType.RightParenthesis);
                    return parameters;
                }
                ValueType parameterType = AdvanceValueType();
                string parameterName = AdvancePossibleName();
                if (token.Type == TokenType.LeftSquareBracket)
                {
                    AdvanceToken(/* TokenType.LeftSquareBracket */);
                    AdvanceToken(TokenType.RightSquareBracket);
                    parameterType = new ValueType.Array(parameterType, null);
                }
                Expression parameterDefault = AdvancePossibleAssignment();
                parameters.Add(new ParameterDef(parameterName, parameterType, parameterDefault));
                if (token.Type != TokenType.RightParenthesis)
                {
                    AdvanceToken(TokenType.Comma);
                    continue;
                }
            }
            AdvanceToken(/* TokenType.RightParenthesis */);
            return parameters;
        }
    }
}
