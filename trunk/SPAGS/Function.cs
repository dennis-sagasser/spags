﻿using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public class Function : INameHolder
    {
        public NameHolderType NameHolderType { get { return NameHolderType.Function; } }
        public Function(string name, SignatureDef signature)
        {
            this.name = name;
            Signature = signature;
        }
        public List<Parameter> ParameterVariables = new List<Parameter>();
        public List<Function> CalledBy = new List<Function>();
        public bool NoLoopCheck;
        public void AddCalledBy(Function func)
        {
            if (!CalledBy.Contains(func)) CalledBy.Add(func);
        }
        public Function(string name, ValueType returnType, ParameterList parameters)
            : this(name, new SignatureDef(returnType, parameters))
        {
        }
        public object UserData;
        public Script OwnerScript;
        private readonly string name;
        public string Name { get { return name; } }
        public readonly SignatureDef Signature;
        public bool Imported;
        public bool MarkedAsBlocking;
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (StringWriter output = new StringWriter(sb))
            {
                WriteTo(output);
            }
            return sb.ToString();
        }
        public Statement.Block Body;
        public void WriteTo(TextWriter output)
        {
            output.Write(Signature.ReturnType.Name + " " + Name + Signature.Parameters);
            if (Body == null)
            {
                output.Write(";");
                return;
            }
            output.Write(" ");
            Body.WriteTo(output, 0);
        }
        public class SignatureDef : ValueType
        {
            public SignatureDef(ValueType returnType, ParameterList parameters)
                : base("(function signature)", ValueTypeCategory.FunctionSignature)
            {
                ReturnType = returnType;
                Parameters = parameters;
            }
            public readonly ValueType ReturnType;
            public readonly ParameterList Parameters;
            public override string Name
            {
                get
                {
                    return "<<" + Parameters + " -> " + ReturnType.Name + ">>";
                }
            }
        }
    }
    public class ParameterDef
    {
        public ParameterDef(string name, ValueType type, Expression defaultValue)
        {
            Name = name;
            Type = type;
            DefaultValue = defaultValue;
        }
        public string Name;
        public ValueType Type;
        public Expression DefaultValue;
        public override string ToString()
        {
            string str = Type.Name;
            if (Name != null) str += " " + Name;
            if (DefaultValue != null) str += " = " + DefaultValue;
            return str;
        }
    }
    public class ParameterList : List<ParameterDef>
    {
        public void Add(string name, ValueType type, Expression defaultValue)
        {
            Add(new ParameterDef(name, type, defaultValue));
        }
        public void Add(string name, ValueType type)
        {
            Add(new ParameterDef(name, type, null));
        }
        public bool HasVarArgs;
        public override string ToString()
        {
            StringBuilder paramsSB = new StringBuilder("(");
            for (int i = 0; i < Count; i++)
            {
                if (i > 0) paramsSB.Append(", ");
                paramsSB.Append(this[i].ToString());
            }
            if (HasVarArgs) paramsSB.Append(Count == 0 ? "..." : ", ...");
            paramsSB.Append(")");
            return paramsSB.ToString();
        }
    }
}
