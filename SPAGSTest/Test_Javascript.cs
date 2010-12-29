using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using AGS.Types;
using SPAGS.Util;

namespace SPAGS
{
    public partial class TestPlugin : IEditorComponent, IAGSEditorPlugin
    {
        void WriteJavascripts(ScriptCollection scripts, TextWriter output)
        {
            usedWords = new Dictionary<string,bool>();
            foreach (string word in YieldJavascriptKeywords()) usedWords.Add(word, true);
            usedWords.Add(EXTRAS_OBJECT, true);
            usedWords.Add(UTIL_OBJECT, true);
            List<Script> list = new List<Script>();
            foreach (Script header in scripts.Headers)
            {
                list.Add(header);
            }
            Script globVarsScript = scripts.CompileGlobalVariablesScript(_editor);
            if (globVarsScript != null)
            {
                list.Add(globVarsScript);
            }
            foreach (AGS.Types.Script script in _editor.CurrentGame.Scripts)
            {
                if (!script.IsHeader)
                {
                    list.Add(scripts.CompileScript(script.FileName, script.Text));
                }
            }
            list.Add(scripts.CompileDialogScript(_editor));
            foreach (IRoom unloadedRoom in _editor.CurrentGame.Rooms)
            {
                list.Add(scripts.CompileRoomScript(_editor, unloadedRoom.Number));
            }

            Dictionary<Function,List<CodeUnit>> callSites = new Dictionary<Function,List<CodeUnit>>();
            Dictionary<Function,List<Function>> calledBy = new Dictionary<Function,List<Function>>();
            foreach (Script script in list)
            {
                foreach (Function func in script.DefinedFunctions)
                {
                    foreach (CodeUnit codeUnit in func.Body.YieldChildCodeUnitsRecursive())
                    {
                        Function f;
                        List<Expression> parameters;
                        if (codeUnit.TryGetSimpleCall(out f, out parameters))
                        {
                            List<CodeUnit> callSitesList;
                            List<Function> calledByList;
                            if (!callSites.TryGetValue(f, out callSitesList))
                            {
                                callSitesList = new List<CodeUnit>();
                                callSites[f] = callSitesList;
                            }
                            if (!calledBy.TryGetValue(f, out calledByList))
                            {
                                calledByList = new List<Function>();
                                calledBy[f] = calledByList;
                            }
                            callSitesList.Add(codeUnit);
                            if (!calledByList.Contains(func))
                            {
                                calledByList.Add(func);
                            }
                        }
                    }
                }
            }
            foreach (Function f in calledBy.Keys)
            {
                if (f.Body != null)
                {
                    continue;
                }
                List<Function> calledByList = calledBy[f];
                List<CodeUnit> callSitesList = callSites[f];
                switch (f.Name)
                {
                    case "Game::InputBox":
                    case "Display":
                    case "DisplayAt":
                    case "DisplayMessage":
                    case "DisplayMessageAtY":
                    case "DisplayTopBar":
                    case "DisplayMessageBar":
                    case "ProcessClick":
                    case "QuitGame":
                    case "AbortGame":
                    case "RestoreGameDialog":
                    case "SaveGameDialog":
                    case "RestartGame":
                    case "InputBox":
                    case "InventoryScreen":
                    case "RunObjectInteraction":
                    case "AnimateObject":
                    case "AnimateObjectEx":
                    case "MoveObject":
                    case "MoveObjectDirect":
                    case "RunCharacterInteraction":
                    case "DisplaySpeech":
                    case "DisplayThought":
                    case "AnimateCharacter":
                    case "AnimateCharacterEx":
                    case "MoveCharacter":
                    case "MoveCharacterDirect":
                    case "MoveCharacterPath":
                    case "MoveCharacterStraight":
                    case "MoveCharacterToHotspot":
                    case "MoveCharacterToObject":
                    case "MoveCharacterBlocking":
                    case "FaceCharacter":
                    case "FaceLocation":
                    case "RunHotspotInteraction":
                    case "RunRegionInteraction":
                    case "RunInventoryInteraction":
                    case "InventoryItem::RunInteraction":
                    case "FadeIn":
                    case "FadeOut":
                    case "ShakeScreen":
                    case "PlayFlic":
                    case "PlayVideo":
                    case "Wait":
                    case "WaitKey":
                    case "WaitMouseKey":
                    case "Hotspot::RunInteraction":
                    case "Region::RunInteraction":
                    case "Dialog::DisplayOptions":
                    case "Object::Animate":
                    case "Object::Move":
                    case "Object::RunInteraction":
                    case "Character::Animate":
                    case "Character::FaceCharacter":
                    case "Character::FaceLocation":
                    case "Character::FaceObject":
                    case "Character::Move":
                    case "Character::RunInteraction":
                    case "Character::Say":
                    case "Character::SayAt":
                    case "Character::Think":
                    case "Character::Walk":
                    case "Character::WalkStraight":
                        break;
                    default:
                        continue;
                }
                FunctionData fdata = UserData<Function, FunctionData>.Get(f);
                fdata.Blocking = true;
                foreach (CodeUnit callSite in callSitesList)
                {
                    CodeUnitData cudata = UserData<CodeUnit, CodeUnitData>.Get(callSite);
                    cudata.MarkAsBlocked();
                }
                calledByList = new List<Function>(calledByList);
                Dictionary<Function,bool> doneFuncs = new Dictionary<Function,bool>();
                while (calledByList.Count > 0)
                {
                    Function f2 = calledByList[0];
                    calledByList.RemoveAt(0);
                    if (doneFuncs.ContainsKey(f2)) continue;
                    doneFuncs[f2] = true;
                    FunctionData f2data = UserData<Function,FunctionData>.Get(f2);
                    if (callSites.ContainsKey(f2))
                    {
                        foreach (CodeUnit callSite in callSites[f2])
                        {
                            CodeUnitData cudata = UserData<CodeUnit, CodeUnitData>.Get(callSite);
                            cudata.MarkAsBlocked();
                        }
                    }
                    if (calledBy.ContainsKey(f2))
                    {
                        calledByList.AddRange(calledBy[f2]);
                    }
                }
            }

            foreach (Script script in list)
            {
                WriteJavascript(script, output, 0);
            }
        }

        Script currentScript;
        Function currentFunction;
        Dictionary<string, bool> usedWords;

        void WriteJavascript(Script script, TextWriter output, int indent)
        {
            currentScript = script;
            Indented(output, indent, "scripts[\"" + script.Name + "\"] = {");
            foreach (Variable var in script.DefinedVariables)
            {
                VariableData vdata = UserData<Variable, VariableData>.Get(var);
                if (usedWords.ContainsKey(vdata.Name)) vdata.Name = "v$" + vdata.Name;
                output.Write("  " + vdata.Name + ": ");
                if (var.InitialValue == null)
                {
                    WriteExpressionJS(var.Type.CreateDefaultValueExpression(), output, indent);
                }
                else
                {
                    WriteExpressionJS(var.InitialValue, output, indent + 1, var.Type);
                }
                output.WriteLine(",");
            }
            foreach (ValueType.Struct structType in script.DefinedStructs)
            {
                if (structType.IsManaged) continue;
                StructData sdata = UserData<ValueType.Struct, StructData>.Get(structType);
                if (usedWords.ContainsKey(sdata.Name)) sdata.Name = "s$" + sdata.Name;
                output.WriteLine("  \"" + sdata.Name + "\": " + UTIL_OBJECT + ".createStruct(function() {");
                foreach (StructMember.Field field in structType.Members.EachOf<StructMember.Field>())
                {
                    switch (field.Type.Category)
                    {
                        case ValueTypeCategory.Array:
                            ValueType.Array arrayType = (ValueType.Array)field.Type;
                            output.Write("      this." + field.Name + " = ");
                            switch (arrayType.ElementType.Category)
                            {
                                case ValueTypeCategory.Float:
                                case ValueTypeCategory.Int:
                                    output.Write(UTIL_OBJECT + ".arrayZeroes(");
                                    break;
                                default:
                                    output.Write(UTIL_OBJECT + ".arrayNulls(");
                                    break;
                            }
                            WriteExpressionJS(arrayType.LengthExpression, output, indent);
                            output.WriteLine(");");
                            break;
                    }
                }
                output.WriteLine("    }, {");
                List<StructMember.Field> fields = new List<StructMember.Field>();
                foreach (StructMember.Field field in structType.Members.EachOf<StructMember.Field>())
                {
                    fields.Add(field);
                }
                for(int i = 0; i < fields.Count; i++)
                {
                    StructMember.Field field = fields[i];
                    switch (field.Type.Category)
                    {
                        case ValueTypeCategory.Int:
                        case ValueTypeCategory.Float:
                        case ValueTypeCategory.Struct:
                        case ValueTypeCategory.StringValue:
                        case ValueTypeCategory.StringBuffer:
                            output.Write("      \"" + field.Name + "\": ");
                            WriteExpressionJS(field.Type.CreateDefaultValueExpression(), output, indent);
                            if (i == fields.Count - 1)
                            {
                                output.WriteLine();
                            }
                            else
                            {
                                output.WriteLine(",");
                            }
                            break;
                    }
                }
                output.WriteLine("    }),");
            }
            foreach (Function func in script.DefinedFunctions)
            {
                currentFunction = func;
                FunctionData fdata = UserData<Function, FunctionData>.Get(func);
                fdata.Name = fdata.Name.Replace("::", DOUBLE_COLON_REPLACE);
                if (usedWords.ContainsKey(fdata.Name)) fdata.Name = "f$" + fdata.Name;
                output.Write("  \"" + fdata.Name + "\": function(");
                if (fdata.Blocking)
                {
                    output.Write("$ctx, $stk, $vars");
                    for (int i = 0; i < func.ParameterVariables.Count; i++)
                    {
                        Variable paramVar = func.ParameterVariables[i];
                        VariableData vdef = UserData<Variable, VariableData>.Get(paramVar);
                        vdef.Blocked = true;
                        if (usedWords.ContainsKey(vdef.Name)) vdef.Name = "p$" + vdef.Name;
                    }
                }
                else
                {
                    for (int i = 0; i < func.ParameterVariables.Count; i++)
                    {
                        if (i != 0) output.Write(", ");
                        Variable paramVar = func.ParameterVariables[i];
                        VariableData vdef = UserData<Variable, VariableData>.Get(paramVar);
                        if (usedWords.ContainsKey(vdef.Name)) vdef.Name = "p$" + vdef.Name;
                        output.Write(vdef.Name);
                    }
                }
                output.WriteLine(") {");
                NameDictionary tempUsedNames = new NameDictionary();
                foreach (Variable var in func.YieldLocalVariables())
                {
                    if (var is Parameter) continue;
                    LocalVariable localVar = (LocalVariable)var;
                    VariableData vdata = UserData<Variable, VariableData>.Get(var);
                    if (usedWords.ContainsKey(vdata.Name)) vdata.Name = "v$" + vdata.Name;
                    if (UserData<CodeUnit, CodeUnitData>.Get(localVar.OwnerScope).Blocked)
                    {
                        vdata.Blocked = true;
                    }
                    else
                    {
                        if (!tempUsedNames.ContainsKey(var.Name)) tempUsedNames.Add(var);
                    }
                }
                if (tempUsedNames.Count > 0)
                {
                    Indent(output, indent + 2);
                    output.Write("var ");
                    bool firstValue = true;
                    foreach (Variable var in tempUsedNames.EachOf<Variable>())
                    {
                        if (firstValue)
                        {
                            firstValue = false;
                        }
                        else
                        {
                            output.Write(", ");
                        }
                        VariableData vdata = UserData<Variable, VariableData>.Get(var);
                        output.Write(vdata.Name);
                    }
                    output.WriteLine(";");
                }
                if (func.Signature.ReturnType.Category != ValueTypeCategory.Void
                    && !func.Body.Returns())
                {
                    func.Body.ChildStatements.Add(
                        new Statement.Return(
                            func.Signature.ReturnType.CreateDefaultValueExpression()));
                }
                if (fdata.Blocking)
                {
                    Flattener flattener = new Flattener(func);
                    flattener.Go();
                    foreach (Statement stmt in flattener.output)
                    {
                        Indent(output, indent + 2);
                        WriteStatementJS(stmt, output, indent + 2);
                    }
                }
                else
                {
                    foreach (Statement stmt in func.Body.ChildStatements)
                    {
                        Indent(output, indent + 2);
                        WriteStatementJS(stmt, output, indent + 2);
                    }
                }
                output.WriteLine("  },");
            }

            output.WriteLine("  \"$serialize\": function(slzr) {");
            /*
            foreach (Variable var in script.DefinedVariables)
            {
                output.WriteLine("  slzr.label(\"" + var.Name + "\");");
                switch (var.Type.Category)
                {
                    case ValueTypeCategory.Int:
                        if (var.Type.Name == "char")
                        {
                            output.Write("  slzr.writeChar(");
                            WriteScriptOwnerJS(script, output);
                            output.WriteLine(");");
                        }
                        else if (var.Type.Name == "short")
                        {
                            output.Write("  slzr.writeShort(");
                            WriteScriptOwnerJS(script, output);
                            output.WriteLine(");");
                        }
                        else
                        {
                            output.Write("  slzr.writeInt(");
                            WriteScriptOwnerJS(script, output);
                            output.WriteLine(");");
                        }
                    case ValueTypeCategory.Array:
                        output.Write("    slzr.beginArray(");
                }
            }
            */
            output.WriteLine("  },");
            output.WriteLine("  \"$deserialize\": function(dslzr) {");
            output.WriteLine("  }");

            Indented(output, indent, "};");
        }

        void WriteStatementJS(Statement stmt, TextWriter output, int indent)
        {
            switch (stmt.Type)
            {
                case StatementType.Call:
                    Statement.Call callStmt = (Statement.Call)stmt;
                    WriteExpressionJS(callStmt.CallExpression, output, indent);
                    output.WriteLine(";");
                    break;
                case StatementType.Assign:
                    Statement.Assign assign = (Statement.Assign)stmt;

                    Expression.Attribute attr = assign.Target as Expression.Attribute;
                    Expression.ArrayIndex index = assign.Target as Expression.ArrayIndex;
                    if (index != null)
                    {
                        attr = index.Target as Expression.Attribute;
                    }
                    if (attr != null)
                    {
                        WriteFunctionJS(attr.TheAttribute.Setter, output);
                        output.Write("(");
                        if (attr.Target != null)
                        {
                            WriteExpressionJS(attr.Target, output, indent);
                            output.Write(", ");
                        }
                        if (index != null)
                        {
                            WriteExpressionJS(index.Index, output, indent);
                            output.Write(", ");
                        }
                        Expression newValue;
                        switch (assign.AssignType)
                        {
                            case TokenType.Assign:
                                newValue = assign.Value;
                                break;
                            case TokenType.Increment:
                                newValue = new Expression.BinaryOperator(
                                    Token.Add, attr, new Expression.IntegerLiteral(1));
                                break;
                            case TokenType.Decrement:
                                newValue = new Expression.BinaryOperator(
                                    Token.Subtract, attr, new Expression.IntegerLiteral(1));
                                break;
                            case TokenType.AddAssign:
                                newValue = new Expression.BinaryOperator(
                                    Token.Add, attr, assign.Value);
                                break;
                            case TokenType.SubtractAssign:
                                newValue = new Expression.BinaryOperator(
                                    Token.Subtract, attr, assign.Value);
                                break;
                            default:
                                throw new Exception("unrecognised assign token: " + assign.AssignType.ToString());
                        }
                        WriteExpressionJS(newValue, output, indent);
                        output.WriteLine(");");
                        break;
                    }
                    else
                    {
                        WriteExpressionJS(assign.Target, output, indent + 1);
                        output.Write(" = ");
                        Expression newValue;
                        switch (assign.AssignType)
                        {
                            case TokenType.Assign:
                                newValue = assign.Value;
                                break;
                            case TokenType.Increment:
                                newValue = new Expression.BinaryOperator(Token.Add, assign.Target, new Expression.IntegerLiteral(1));
                                break;
                            case TokenType.Decrement:
                                newValue = new Expression.BinaryOperator(Token.Subtract, assign.Target, new Expression.IntegerLiteral(1));
                                break;
                            case TokenType.AddAssign:
                                newValue = new Expression.BinaryOperator(Token.Add, assign.Target, assign.Value);
                                break;
                            case TokenType.SubtractAssign:
                                newValue = new Expression.BinaryOperator(Token.Subtract, assign.Target, assign.Value);
                                break;
                            default:
                                throw new Exception("unrecognised assign token: " + assign.AssignType.ToString());
                        }
                        WriteExpressionJS(newValue, output, indent + 1, assign.Target.GetValueType());
                        output.WriteLine(";");
                        break;
                    }
                case StatementType.Block:
                    Statement.Block block = (Statement.Block)stmt;
                    if (block.ChildStatements.Count == 0)
                    {
                        output.WriteLine("{ }");
                    }
                    else
                    {
                        output.WriteLine("{");
                        foreach (Statement child in block.ChildStatements)
                        {
                            Indent(output, indent + 1);
                            WriteStatementJS(child, output, indent + 1);
                        }
                        Indented(output, indent, "}");
                    }
                    break;
                case StatementType.Custom:
                    if (stmt is FlatStatement)
                    {
                        WriteFlatStatementJS((FlatStatement)stmt, output, indent);
                        break;
                    }
                    break;
                case StatementType.If:
                    Statement.If conditional = (Statement.If)stmt;
                    Expression switchTest;
                    List<Expression> switchValues;
                    List<Statement> switchCases;
                    Statement switchDefault;
                    if (TryGetSwitchBlock(conditional, out switchTest, out switchValues, out switchCases, out switchDefault))
                    {
                        output.Write("switch (");
                        WriteExpressionJS(switchTest, output, indent);
                        output.WriteLine(") {");
                        for (int i = 0; i < switchCases.Count; i++)
                        {
                            Indent(output, indent + 1);
                            output.Write("case ");
                            WriteExpressionJS(switchValues[i], output, indent + 1);
                            output.Write(":");
                            Statement.Block caseBlock = switchCases[i] as Statement.Block;
                            if (caseBlock == null)
                            {
                                output.WriteLine();
                                Indent(output, indent + 2);
                                WriteStatementJS(switchCases[i], output, indent + 2);
                            }
                            else
                            {
                                output.WriteLine();
                                foreach (Statement caseStmt in caseBlock.ChildStatements)
                                {
                                    Indent(output, indent + 2);
                                    WriteStatementJS(caseStmt, output, indent + 2);
                                }
                            }
                            if (!switchCases[i].Returns())
                            {
                                Indent(output, indent + 2);
                                output.WriteLine("break;");
                            }
                        }
                        if (switchDefault != null)
                        {
                            Indent(output, indent + 1);
                            output.Write("default: ");

                            Statement.Block defaultBlock = switchDefault as Statement.Block;
                            if (defaultBlock == null)
                            {
                                output.WriteLine();
                                Indent(output, indent + 2);
                                WriteStatementJS(switchDefault, output, indent + 2);
                            }
                            else if (defaultBlock.Scope.Count != 0)
                            {
                                output.Write(" ");
                                WriteStatementJS(switchDefault, output, indent + 2);
                            }
                            else
                            {
                                output.WriteLine();
                                foreach (Statement caseStmt in defaultBlock.ChildStatements)
                                {
                                    Indent(output, indent + 2);
                                    WriteStatementJS(caseStmt, output, indent + 2);
                                }
                            }
                            if (!switchDefault.Returns())
                            {
                                Indent(output, indent + 2);
                                output.WriteLine("break;");
                            }
                        }
                        Indent(output, indent);
                        output.WriteLine("}");
                    }
                    else
                    {
                        output.Write("if (");
                        WriteExpressionJS(conditional.IfThisIsTrue, output, indent);
                        output.Write(") ");
                        WriteStatementJS(Blockify(conditional.ThenDoThis), output, indent);
                        if (conditional.ElseDoThis != null)
                        {
                            Indent(output, indent);
                            output.Write("else ");
                            if (conditional.ElseDoThis.Type == StatementType.If)
                            {
                                WriteStatementJS(conditional.ElseDoThis, output, indent);
                            }
                            else
                            {
                                WriteStatementJS(Blockify(conditional.ElseDoThis), output, indent);
                            }
                        }
                    }
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    Expression val = ret.Value;
                    if (val == null)
                    {
                        ValueType returnType = currentFunction.Signature.ReturnType;
                        if (returnType.Category != ValueTypeCategory.Void)
                        {
                            val = returnType.CreateDefaultValueExpression();
                        }
                    }
                    FunctionData fdata = UserData<Function,FunctionData>.Get(currentFunction);
                    if (fdata.Blocking)
                    {
                        if (val == null)
                        {
                            output.WriteLine("return $ctx.finish();");
                        }
                        else
                        {
                            output.Write("return $ctx.finish(");
                            WriteExpressionJS(val, output, indent);
                            output.WriteLine(");");
                        }
                    }
                    else
                    {
                        if (val == null)
                        {
                            output.WriteLine("return;");
                        }
                        else
                        {
                            output.Write("return ");
                            WriteExpressionJS(val, output, indent);
                            output.WriteLine(";");
                        }
                    }
                    break;
                case StatementType.VariableDeclaration:
                    Statement.VariableDeclaration varDef = (Statement.VariableDeclaration)stmt;
                    for (int i = 0; i < varDef.Variables.Count; i++)
                    {
                        Variable var = varDef.Variables[i];
                        VariableData vdata = UserData<Variable, VariableData>.Get(var);
                        if (vdata.Blocked)
                        {
                            output.Write("$vars." + vdata.Name + " = ");
                        }
                        else
                        {
                            output.Write(vdata.Name + " = ");
                        }
                        if (var.InitialValue == null)
                        {
                            WriteExpressionJS(var.Type.CreateDefaultValueExpression(), output, indent+1);
                        }
                        else
                        {
                            WriteExpressionJS(var.InitialValue, output, indent + 1, var.Type);
                        }
                        output.WriteLine(";");
                        if (i < varDef.Variables.Count-1) Indent(output, indent);
                    }
                    break;
                case StatementType.While:
                    Statement.While loop = (Statement.While)stmt;
                    Expression ivar, initVal, modify;
                    Expression.BinaryOperator comparison;
                    Statement forBody;
                    if (TryGetForLoop(loop, out ivar, out initVal, out comparison, out modify, out forBody))
                    {
                        output.Write("for (");
                        if (initVal != null)
                        {
                            WriteExpressionJS(ivar, output, indent);
                            output.Write(" = ");
                            WriteExpressionJS(initVal, output, indent);
                        }
                        output.Write("; ");
                        if (comparison != null)
                        {
                            WriteExpressionJS(comparison, output, indent);
                        }
                        output.Write("; ");
                        if (modify != null)
                        {
                            WriteExpressionJS(ivar, output, indent);
                            output.Write(" = ");
                            WriteExpressionJS(modify, output, indent);
                        }
                        output.Write(") ");
                        if (forBody == null) { output.Write("{ }"); break; }
                        WriteStatementJS(forBody, output, indent);
                    }
                    else
                    {
                        output.Write("while (");
                        WriteExpressionJS(loop.WhileThisIsTrue, output, indent);
                        output.Write(") ");
                        WriteStatementJS(Blockify(loop.KeepDoingThis), output, indent);
                    }
                    break;
            }
        }

        void WriteFlatStatementJS(FlatStatement flat, TextWriter output, int indent)
        {
            switch (flat.FlatStatementType)
            {
                case FlatStatementType.EntryPoint:
                    FlatStatement.EntryPoint entryPoint = (FlatStatement.EntryPoint)flat;
                    output.WriteLine("// entry point " + entryPoint.Number + ":");
                    break;
                case FlatStatementType.InitParameters:
                    output.Write("$ctx.initParams(");
                    for (int i = 0; i < currentFunction.ParameterVariables.Count; i++)
                    {
                        Variable var = currentFunction.ParameterVariables[i];
                        if (i > 0) output.Write(", ");
                        VariableData vdata = UserData<Variable,VariableData>.Get(var);
                        output.Write("'" + vdata.Name + "'");
                    }
                    output.WriteLine(");");
                    break;
                case FlatStatementType.Suspend:
                    FlatStatement.Suspend suspend = (FlatStatement.Suspend)flat;
                    if (suspend.EntryPoint == null)
                    {
                        output.WriteLine("return $ctx.finish();");
                    }
                    else
                    {
                        FlatStatement.EntryPoint suspendEntryPoint = suspend.EntryPoint.Redirected;
                        if (suspendEntryPoint.UseFinishValue)
                        {
                            output.Write("return $ctx.finish(");
                            if (suspendEntryPoint.FinishValue != null)
                            {
                                WriteExpressionJS(suspendEntryPoint.FinishValue, output, indent);
                            }
                            output.WriteLine(");");
                        }
                        else
                        {
                            output.WriteLine("return $ctx.nextEntryPoint(" + suspend.EntryPoint.Number + ");");
                        }
                    }
                    break;
                case FlatStatementType.Push:
                    FlatStatement.Push push = (FlatStatement.Push)flat;
                    if (push.Values.Count > 0)
                    {
                        output.Write("$stk.push(");
                        for (int i = 0; i < push.Values.Count; i++)
                        {
                            if (i != 0) output.Write(", ");
                            WriteExpressionJS(push.Values[i], output, indent);
                        }
                        output.WriteLine(");");
                    }
                    break;
                case FlatStatementType.Finish:
                    output.Write("return $ctx.finish(");
                    WriteExpressionJS(((FlatStatement.Finish)flat).Value, output, indent);
                    output.WriteLine(");");
                    break;
                case FlatStatementType.Begin:
                    FlatStatement.Begin begin = (FlatStatement.Begin)flat;
                    if (begin.IgnoreReturnValue)
                    {
                        output.Write("$ctx.queue(");
                    }
                    else
                    {
                        output.Write("$ctx.begin(");
                    }
                    WriteFunctionJS(begin.Function, output);
                    if (begin.StackParams > 0)
                    {
                        output.Write(", $stk.splice(-" + begin.StackParams + ")");
                        if (begin.DirectParams.Count > 0)
                        {
                            output.Write(".concat([");
                            for (int i = 0; i < begin.DirectParams.Count; i++)
                            {
                                if (i > 0) output.Write(", ");
                                WriteExpressionJS(begin.DirectParams[i], output, indent);
                            }
                            output.Write("])");
                        }
                    }
                    else if (begin.DirectParams.Count > 0)
                    {
                        output.Write(", [");
                        for (int i = 0; i < begin.DirectParams.Count; i++)
                        {
                            if (i > 0) output.Write(", ");
                            WriteExpressionJS(begin.DirectParams[i], output, indent);
                        }
                        output.Write("]");
                    }
                    output.WriteLine(");");
                    break;
                case FlatStatementType.StackArrayIndex:
                    output.WriteLine("$ctx.index();");
                    break;
                case FlatStatementType.Pop:
                    output.WriteLine("$stk.pop();");
                    break;
                case FlatStatementType.StackBinOp:
                    switch (((FlatStatement.StackBinOp)flat).Token.Type)
                    {
                        case TokenType.Add:
                            output.WriteLine("$ctx.s_add();");
                            break;
                        case TokenType.BitwiseAnd:
                            output.WriteLine("$ctx.s_band();");
                            break;
                        case TokenType.BitwiseLeftShift:
                            output.WriteLine("$ctx.s_lshift();");
                            break;
                        case TokenType.BitwiseOr:
                            output.WriteLine("$ctx.s_bor();");
                            break;
                        case TokenType.BitwiseRightShift:
                            output.WriteLine("$ctx.s_rshift();");
                            break;
                        case TokenType.BitwiseXor:
                            output.WriteLine("$ctx.s_bxor();");
                            break;
                        case TokenType.Divide:
                            output.WriteLine("$ctx.s_div();");
                            break;
                        case TokenType.IsEqualTo:
                            output.WriteLine("$ctx.s_eq();");
                            break;
                        case TokenType.IsGreaterThan:
                            output.WriteLine("$ctx.s_gt()");
                            break;
                        case TokenType.IsGreaterThanOrEqualTo:
                            output.WriteLine("$ctx.s_gte()");
                            break;
                        case TokenType.IsLessThan:
                            output.WriteLine("$ctx.s_lt();");
                            break;
                        case TokenType.IsLessThanOrEqualTo:
                            output.WriteLine("$ctx.s_lte();");
                            break;
                        case TokenType.IsNotEqualTo:
                            output.WriteLine("$ctx.s_neq();");
                            break;
                        case TokenType.LogicalAnd:
                            output.WriteLine("$ctx.s_and();");
                            break;
                        case TokenType.LogicalOr:
                            output.WriteLine("$ctx.s_or();");
                            break;
                        case TokenType.Modulus:
                            output.WriteLine("$ctx.s_mod();");
                            break;
                        case TokenType.Multiply:
                            output.WriteLine("$ctx.s_mul();");
                            break;
                        case TokenType.Subtract:
                            output.WriteLine("$ctx.s_sub();");
                            break;
                    }
                    break;
            }
        }

        Statement.Block Blockify(Statement stmt)
        {
            if (stmt == null) return null;
            if (stmt is Statement.Block) return (Statement.Block)stmt;
            Statement.Block block = new Statement.Block(new NameDictionary());
            block.ChildStatements.Add(stmt);
            return block;
        }

        bool TryGetForLoop(Statement.While whileLoop,
            out Expression ivar,
            out Expression initVal,
            out Expression.BinaryOperator comparison,
            out Expression modify,
            out Statement body)
        {
            ivar = null;
            initVal = null;
            modify = null;
            body = null;
            comparison = whileLoop.WhileThisIsTrue as Expression.BinaryOperator;
            if (comparison == null) return false;
            switch (comparison.Token.Type)
            {
                case TokenType.IsEqualTo:
                case TokenType.IsGreaterThan:
                case TokenType.IsGreaterThanOrEqualTo:
                case TokenType.IsLessThan:
                case TokenType.IsLessThanOrEqualTo:
                case TokenType.IsNotEqualTo:
                    break;
                default:
                    return false;
            }
            ivar = comparison.Left;
            Statement.Block bodyBlock = whileLoop.KeepDoingThis as Statement.Block;
            if (bodyBlock == null || bodyBlock.ChildStatements.Count == 0) return false;
            Statement.Assign assign = bodyBlock.ChildStatements[bodyBlock.ChildStatements.Count - 1] as Statement.Assign;
            if (assign == null || !assign.Target.Equals(comparison.Left)) return false;
            modify = assign.SimpleAssignValue();
            if (UsesScope(modify, bodyBlock.Scope)) return false;
            Statement.Block newBodyBlock = new Statement.Block(bodyBlock.Scope);
            for (int i = 0; i < bodyBlock.ChildStatements.Count - 1; i++)
            {
                newBodyBlock.ChildStatements.Add(bodyBlock.ChildStatements[i]);
            }
            body = newBodyBlock;
            return true;
        }

        bool UsesScope(Expression expr, NameDictionary scope)
        {
            foreach (Expression subexpr in expr.YieldSubExpressions())
            {
                if (UsesScope(subexpr, scope)) return true;
            }
            Expression.Variable vexpr = expr as Expression.Variable;
            if (vexpr != null && scope.ContainsKey(vexpr.TheVariable.Name)
                && scope[vexpr.TheVariable.Name] == vexpr.TheVariable)
            {
                return true;
            }
            return false;
        }

        const string EXTRAS_OBJECT = "engine";
        const string UTIL_OBJECT = "util";

        void WriteScriptOwnerJS(Script script, TextWriter output)
        {
            if (script == null)
            {
                output.Write(EXTRAS_OBJECT);
            }
            else
            {
                output.Write("scripts[\"" + script.Name + "\"]");
            }
        }

        void WriteFunctionJS(Function function, TextWriter output)
        {
            WriteScriptOwnerJS(function.OwnerScript, output);
            output.Write("." + function.Name.Replace("::", DOUBLE_COLON_REPLACE));
        }

        const string DOUBLE_COLON_REPLACE = "$$";

        void WriteExpressionJS(Expression expr, TextWriter output, int indent)
        {
            WriteExpressionJS(expr, output, indent, null, false);
        }
        void WriteExpressionJS(Expression expr, TextWriter output, int indent, ValueType expectedType)
        {
            WriteExpressionJS(expr, output, indent, expectedType, false);
        }
        void WriteExpressionJS(Expression expr, TextWriter output, int indent, ValueType expectedType, bool selfContained)
        {
            if (expectedType != null)
            {
                ValueType exprValType = expr.GetValueType();
                if (expectedType.IsInternalString
                    && exprValType.Category == ValueTypeCategory.StringBuffer)
                {
                    WriteExpressionJS(expr, output, indent, null);
                    output.Write(".value");
                    return;
                }
                if (expectedType.Category == ValueTypeCategory.Struct)
                {
                    int v;
                    if (expr.TryGetIntValue(out v) && v == 0)
                    {
                        output.Write("null");
                        return;
                    }
                }
                if (expectedType.Category == ValueTypeCategory.Int)
                {
                    int value;
                    bool constantValue = expr.TryGetIntValue(out value);
                    if (expectedType.Name == "short" && exprValType.Name == "int"
                        && !(constantValue && value == (short)value))
                    {
                        if (selfContained) output.Write("(");
                        WriteExpressionJS(expr, output, indent, null, true);
                        output.Write(" << 16 >> 16");
                        if (selfContained) output.Write(")");
                        return;
                    }
                    else if (expectedType.Name == "char" && exprValType.Name != "char"
                        && !(constantValue && value == (byte)value))
                    {
                        if (selfContained) output.Write("(");
                        WriteExpressionJS(expr, output, indent, null, true);
                        output.Write(" & 0xff");
                        if (selfContained) output.Write(")");
                        return;
                    }
                }
            }
            switch (expr.Type)
            {
                case ExpressionType.Custom:
                    if (expr is FlatExpression)
                    {
                        WriteFlatExpressionJS((FlatExpression)expr, output, indent);
                        break;
                    }
                    break;
                case ExpressionType.AllocateArray:
                    Expression.AllocateArray allocArray = (Expression.AllocateArray)expr;
                    switch (allocArray.ElementType.Category)
                    {
                        case ValueTypeCategory.Float:
                        case ValueTypeCategory.Int:
                            output.Write(UTIL_OBJECT + ".arrayZeroes(");
                            break;
                        case ValueTypeCategory.Struct:
                            ValueType.Struct structType = (ValueType.Struct)allocArray.ElementType;
                            if (structType.IsManaged) goto default;
                            WriteScriptOwnerJS(structType.OwnerScript, output);
                            output.Write("." + structType.Name + ".createArray(");
                            break;
                        default:
                            output.Write(UTIL_OBJECT + ".arrayNulls(");
                            break;
                    }
                    WriteExpressionJS(allocArray.Length, output, indent + 2);
                    output.Write(")");
                    break;
                case ExpressionType.ArrayIndex:
                    Expression.ArrayIndex arrayIndex = (Expression.ArrayIndex)expr;
                    Expression.Attribute indexAttr = arrayIndex.Target as Expression.Attribute;
                    if (indexAttr != null && indexAttr.TheAttribute.IsArray)
                    {
                        WriteFunctionJS(indexAttr.TheAttribute.Getter, output);
                        output.Write("(");
                        if (indexAttr.Target != null)
                        {
                            WriteExpressionJS(indexAttr.Target, output, indent);
                            output.Write(", ");
                        }
                        WriteExpressionJS(arrayIndex.Index, output, indent + 1);
                        output.Write(")");
                    }
                    else
                    {
                        WriteExpressionJS(arrayIndex.Target, output, indent + 1);
                        output.Write("[");
                        WriteExpressionJS(arrayIndex.Index, output, indent + 1);
                        output.Write("]");
                    }
                    break;
                case ExpressionType.Attribute:
                    Expression.Attribute attr = (Expression.Attribute)expr;
                    WriteFunctionJS(attr.TheAttribute.Getter, output);
                    if (attr.Target == null)
                    {
                        output.Write("()");
                    }
                    else
                    {
                        output.Write("(");
                        WriteExpressionJS(attr.Target, output, indent + 1);
                        output.Write(")");
                    }
                    break;
                case ExpressionType.BinaryOperator:
                    WriteBinaryOperatorJS((Expression.BinaryOperator)expr, output, indent, selfContained);
                    break;
                case ExpressionType.Call:
                    Expression.Call call = (Expression.Call)expr;
                    Expression.Method callingMethod = call.CallingOn as Expression.Method;
                    if (callingMethod != null)
                    {
                        if (callingMethod.TheMethod.IsStatic)
                        {
                            Expression.Call newCall = new Expression.Call(
                                new Expression.Function(callingMethod.TheMethod.Function),
                                call.Parameters);
                            WriteExpressionJS(newCall, output, indent, expectedType);
                        }
                        else
                        {
                            List<Expression> parameters = new List<Expression>(call.Parameters);
                            parameters.Insert(0, callingMethod.Target);
                            Expression.Call newCall = new Expression.Call(
                                new Expression.Function(callingMethod.TheMethod.Function),
                                parameters);
                            WriteExpressionJS(newCall, output, indent, expectedType);
                        }
                        break;
                    }
                    WriteExpressionJS(call.CallingOn, output, indent + 1);
                    output.Write("(");
                    ValueType.FunctionSignature signature = call.CallingOn.GetValueType() as ValueType.FunctionSignature;
                    if (signature == null)
                    {
                        throw new Exception("Calling on a non-callable object: " + call.CallingOn.ToString());
                    }
                    for (int i = 0; i < Math.Max(signature.Parameters.Count, call.Parameters.Count); i++)
                    {
                        if (i < call.Parameters.Count)
                        {
                            if (i != 0) output.Write(", ");
                            if (i < signature.Parameters.Count)
                            {
                                WriteExpressionJS(call.Parameters[i], output, indent + 1, signature.Parameters[i].Type);
                            }
                            else
                            {
                                WriteExpressionJS(call.Parameters[i], output, indent + 1);
                            }
                        }
                        else
                        {
                            Expression paramValue = signature.Parameters[i].DefaultValue;
                            if (paramValue == null)
                            {
                                paramValue = signature.Parameters[i].Type.CreateDefaultValueExpression();
                            }
                            if (paramValue != null)
                            {
                                if (i != 0) output.Write(", ");
                                WriteExpressionJS(paramValue, output, indent, signature.Parameters[i].Type);
                            }
                        }
                    }
                    output.Write(")");
                    break;
                case ExpressionType.CharLiteral:
                    Expression.CharLiteral charLiteral = (Expression.CharLiteral)expr;
                    output.Write((int)charLiteral.Value);
                    switch (charLiteral.Value)
                    {
                        case '\r':
                            output.Write(" /* '\\r' */");
                            break;
                        case '\n':
                            output.Write(" /* '\\n' */");
                            break;
                        case '\t':
                            output.Write(" /* '\\t' */");
                            break;
                        case '\'':
                            output.Write(" /* '\\'' */");
                            break;
                        case '\\':
                            output.Write(" /* '\\\\' */");
                            break;
                        default:
                            if ((int)charLiteral.Value < 32 || (int)charLiteral.Value >= 127)
                            {
                                output.Write((int)charLiteral.Value);
                            }
                            else
                            {
                                output.Write(" /* '" + charLiteral.Value + "' */");
                            }
                            break;
                    }
                    break;
                case ExpressionType.Constant:
                    Expression.Constant constant = (Expression.Constant)expr;
                    WriteExpressionJS(constant.TheConstant.TheExpression, output, indent);
                    output.Write(" /* " + constant.TheConstant.Name + " */");
                    break;
                case ExpressionType.EnumValue:
                    Expression.EnumValue enumValue = (Expression.EnumValue)expr;
                    output.Write(enumValue.TheValue.Value + " /* "+ enumValue.TheValue.Name + " */");
                    break;
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    WriteExpressionJS(field.Target, output, indent + 1);
                    output.Write("." + field.TheField.Name);
                    break;
                case ExpressionType.FloatLiteral:
                    Expression.FloatLiteral floatLiteral = (Expression.FloatLiteral)expr;
                    output.Write(String.Format("{0:0.0}", floatLiteral.Value));
                    break;
                case ExpressionType.Function:
                    Expression.Function func = (Expression.Function)expr;
                    WriteFunctionJS(func.TheFunction, output);
                    break;
                case ExpressionType.IntegerLiteral:
                    Expression.IntegerLiteral intLiteral = (Expression.IntegerLiteral)expr;
                    output.Write(intLiteral.Value.ToString());
                    break;
                case ExpressionType.Method:
                    Expression.Method method = (Expression.Method)expr;
                    WriteFunctionJS(method.TheMethod.Function, output);
                    break;
                case ExpressionType.Null:
                    output.Write("null");
                    break;
                case ExpressionType.StringLiteral:
                    Expression.StringLiteral stringLiteral = (Expression.StringLiteral)expr;
                    output.Write("\"" + stringLiteral.Value.Replace("\"", "\\\"") + "\"");
                    break;
                case ExpressionType.UnaryOperator:
                    WriteUnaryOperatorJS((Expression.UnaryOperator)expr, output, indent);
                    break;
                case ExpressionType.AllocStringBuffer:
                    output.Write("new " + UTIL_OBJECT + ".StringBuffer()");
                    break;
                case ExpressionType.AllocStruct:
                    Expression.AllocateStruct newStruct = (Expression.AllocateStruct)expr;
                    output.Write("new ");
                    WriteScriptOwnerJS(newStruct.TheStructType.OwnerScript, output);
                    output.Write("." + newStruct.TheStructType.Name + "()");
                    break;
                case ExpressionType.Variable:
                    Expression.Variable var = (Expression.Variable)expr;
                    Variable v = var.TheVariable;
                    VariableData vdata = UserData<Variable, VariableData>.Get(v);
                    if (v is ScriptVariable)
                    {
                        WriteScriptOwnerJS(v.OwnerScript, output);
                        output.Write("." + vdata.Name);
                    }
                    else if (vdata.Blocked)
                    {
                        output.Write("$vars." + vdata.Name);
                    }
                    else
                    {
                        output.Write(vdata.Name);
                    }
                    break;
                default:
                    Indented(output, indent, expr.ToString());
                    break;
            }
        }

        void WriteFlatExpressionJS(FlatExpression flat, TextWriter output, int indent)
        {
            switch (flat.FlatExpressionType)
            {
                case FlatExpressionType.StackPop:
                    output.Write("$stk.pop()");
                    break;
                case FlatExpressionType.StackPeek:
                    output.Write("$stk[$stk.length-1]");
                    break;
            }
        }

        void WriteBinaryOperatorJS(Expression.BinaryOperator op, TextWriter output, int indent, bool parens)
        {
            bool forceInt = false;
            if (
                (op.Token.Type == TokenType.Divide && op.Right.GetValueType().Category == ValueTypeCategory.Int) 
                ||
                ((op.Token.Type == TokenType.Add || op.Token.Type == TokenType.Subtract || op.Token.Type == TokenType.Multiply)
                  && op.GetValueType().IntType == "int32")
                )
            {
                forceInt = true;

                if (op.Token.Type == TokenType.Multiply)
                {
                    output.Write("util.imul(");
                    WriteExpressionJS(op.Left, output, indent);
                    output.Write(", ");
                    WriteExpressionJS(op.Right, output, indent);
                    output.Write(")");
                    return;
                }

                output.Write("(");
            }
            if (parens)
            {
                output.Write("(");
            }
            WriteExpressionJS(op.Left, output, indent + 1, null, true);
            switch (op.Token.Type)
            {
                case TokenType.Add:
                    output.Write(" + ");
                    break;
                case TokenType.BitwiseAnd:
                    output.Write(" & ");
                    break;
                case TokenType.BitwiseLeftShift:
                    output.Write(" << ");
                    break;
                case TokenType.BitwiseOr:
                    output.Write(" | ");
                    break;
                case TokenType.BitwiseRightShift:
                    output.Write(" >> ");
                    break;
                case TokenType.BitwiseXor:
                    output.Write(" ^ ");
                    break;
                case TokenType.Divide:
                    output.Write(" / ");
                    break;
                case TokenType.IsEqualTo:
                    output.Write(" === ");
                    break;
                case TokenType.IsGreaterThan:
                    output.Write(" > ");
                    break;
                case TokenType.IsGreaterThanOrEqualTo:
                    output.Write(" >= ");
                    break;
                case TokenType.IsLessThan:
                    output.Write(" < ");
                    break;
                case TokenType.IsLessThanOrEqualTo:
                    output.Write(" <= ");
                    break;
                case TokenType.IsNotEqualTo:
                    output.Write(" !== ");
                    break;
                case TokenType.LogicalAnd:
                    output.Write(" && ");
                    break;
                case TokenType.LogicalOr:
                    output.Write(" || ");
                    break;
                case TokenType.Modulus:
                    output.Write(" % ");
                    break;
                case TokenType.Multiply:
                    output.Write(" * ");
                    break;
                case TokenType.Subtract:
                    output.Write(" - ");
                    break;
            }
            WriteExpressionJS(op.Right, output, indent + 1, null, true);
            if (forceInt)
            {
                output.Write(") | 0");
            }
            if (parens)
            {
                output.Write(")");
            }
        }

        void WriteUnaryOperatorJS(Expression.UnaryOperator op, TextWriter output, int indent)
        {
            switch (op.Token.Type)
            {
                case TokenType.LogicalNot:
                    output.Write("!");
                    break;
                case TokenType.Subtract:
                    output.Write("-");
                    break;
            }
            WriteExpressionJS(op.Operand, output, indent + 1, null, true);
        }
        bool StaticValue(Expression expr)
        {
            if (expr.IsConstant()) return true;
            switch(expr.Type)
            {
                case ExpressionType.Variable:
                    return true;
                case ExpressionType.ArrayIndex:
                    Expression.ArrayIndex arrayIndex = (Expression.ArrayIndex)expr;
                    return StaticValue(arrayIndex.Target) && StaticValue(arrayIndex.Index);
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    return StaticValue(field.Target);
                default:
                    return false;
            }
        }
        bool TryGetSwitchBlock(Statement.If ifBlock,
            out Expression testExpr, out List<Expression> values,
            out List<Statement> cases, out Statement defaultCase)
        {
            testExpr = null;
            values = null;
            cases = null;
            defaultCase = null;

            Expression.BinaryOperator binop = ifBlock.IfThisIsTrue as Expression.BinaryOperator;
            if (!(ifBlock.ElseDoThis is Statement.If) || binop == null || binop.Token.Type != TokenType.IsEqualTo
                || !StaticValue(binop.Left) || !binop.Right.IsConstant()) return false;

            testExpr = binop.Left;
            values = new List<Expression>();
            values.Add(binop.Right);
            cases = new List<Statement>();
            cases.Add(ifBlock.ThenDoThis);

            for (Statement otherCase = ifBlock.ElseDoThis; otherCase != null; otherCase = ifBlock.ElseDoThis)
            {
                ifBlock = otherCase as Statement.If;
                if (ifBlock == null)
                {
                    defaultCase = otherCase;
                    break;
                }
                binop = ifBlock.IfThisIsTrue as Expression.BinaryOperator;
                if (binop == null || binop.Token.Type != TokenType.IsEqualTo
                    || !binop.Left.Equals(testExpr) || !binop.Right.IsConstant())
                {
                    return false;
                }
                values.Add(binop.Right);
                cases.Add(ifBlock.ThenDoThis);
            }

            return true;
        }
        IEnumerable<string> YieldJavascriptKeywords()
        {
            // http://bclary.com/2004/11/07/#a-7.5.2

            // reserved words
            yield return "break";
            yield return "else";
            yield return "new";
            yield return "var";
            yield return "case";
            yield return "finally";
            yield return "return";
            yield return "void";
            yield return "catch";
            yield return "for";
            yield return "switch";
            yield return "while";
            yield return "continue";
            yield return "function";
            yield return "this";
            yield return "with";
            yield return "default";
            yield return "if";
            yield return "throw";
            yield return "delete";
            yield return "in";
            yield return "try";
            yield return "do";
            yield return "instanceof";
            yield return "typeof";
            // named values
            yield return "true";
            yield return "false";
            yield return "null";
            yield return "undefined";
            yield return "NaN";
            // future reserved words
            yield return "abstract";
            yield return "enum";
            yield return "int";
            yield return "short";
            yield return "boolean";
            yield return "export";
            yield return "interface";
            yield return "static";
            yield return "byte";
            yield return "extends";
            yield return "long";
            yield return "super";
            yield return "char";
            yield return "final";
            yield return "native";
            yield return "synchronized";
            yield return "class";
            yield return "float";
            yield return "package";
            yield return "throws";
            yield return "const";
            yield return "goto";
            yield return "private";
            yield return "transient";
            yield return "debugger";
            yield return "implements";
            yield return "protected";
            yield return "volatile";
            yield return "double";
            yield return "import";
            yield return "public";
        }
    }
}
