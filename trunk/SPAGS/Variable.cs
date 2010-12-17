using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public class Variable : INameHolder
    {
        public Variable(string name, ValueType type, Expression initialValue)
        {
            this.name = name;
            Type = type;
            InitialValue = initialValue;
        }
        public object UserData;
        public readonly Script ParentScript;
        public readonly ValueType Type;
        private readonly string name;
        public string Name { get { return name; } }
        public readonly Expression InitialValue;
        public NameHolderType NameHolderType { get { return NameHolderType.Variable; } }
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (TextWriter output = new StringWriter(sb))
            {
                this.WriteTo(output);
            }
            return sb.ToString();
        }
        public virtual void WriteTo(TextWriter output)
        {
            ArrayType asArray = Type as ArrayType;
            if (asArray != null)
            {
                output.Write(asArray.ElementType.Name + " " + Name + "[");
                if (asArray.LengthExpression != null) asArray.LengthExpression.WriteTo(output);
                output.Write("];");
            }
            else
            {
                output.Write(Type.Name + " " + Name);
                if (InitialValue != null)
                {
                    output.Write(" = ");
                    InitialValue.WriteTo(output);
                }
                output.Write(";");
            }
        }
    }
    public class Parameter : Variable
    {
        public Parameter(string name, ValueType valueType)
            : base(name, valueType, null)
        {
        }
    }
    public class ScriptVariable : Variable
    {
        public ScriptVariable(string name, ValueType valueType, Expression initialValue)
            : base(name, valueType, initialValue)
        {
        }
        public Script OwnerScript;
        public bool ReadOnly;
        public bool Imported;
        public bool Defined;
        public bool Exported;
        public override void WriteTo(TextWriter output)
        {
            base.WriteTo(output);
            if (Exported) output.Write(" /* exported */");
        }
    }
}
