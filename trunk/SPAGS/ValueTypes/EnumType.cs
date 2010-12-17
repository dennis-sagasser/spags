using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public class EnumType : ValueType.Named
    {
        public EnumType(string name)
            : base(name, ValueTypeCategory.Int)
        {
        }
        public Script OwnerScript;
        public List<Value> Entries = new List<Value>();
        public override NameHolderType NameHolderType { get { return NameHolderType.EnumType; } }
        public class Value : Token, INameHolder
        {
            internal Value(EnumType ownerType, string name, Expression explicitValue, Value previous)
                : base(TokenType.EnumValue)
            {
                OwnerType = ownerType;
                this.name = name;
                Previous = previous;
                ExplicitValue = explicitValue;
            }
            public object UserData;
            public readonly EnumType OwnerType;
            public NameHolderType NameHolderType { get { return NameHolderType.EnumValue; } }
            private string name;
            public string Name { get { return name; } }
            public readonly Expression ExplicitValue;
            public readonly Value Previous;
            public override string ToString()
            {
                return name;
            }
            public bool TryGetIntValue(out int value)
            {
                if (ExplicitValue != null)
                {
                    return ExplicitValue.TryGetIntValue(out value);
                }
                if (Previous == null)
                {
                    value = 0;
                    return true;
                }
                if (Previous.TryGetIntValue(out value))
                {
                    value++;
                    return true;
                }
                return false;
            }
        }
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (TextWriter output = new StringWriter(sb))
            {
                this.WriteTo(output);
            }
            return sb.ToString();
        }
        public void WriteTo(TextWriter output)
        {
            output.Write("enum " + Name + " {");
            for (int i = 0; i < Entries.Count; i++)
            {
                output.WriteLine((i > 0) ? "," : "");
                output.Write("\t" + Entries[i].Name);
                if (Entries[i].ExplicitValue != null)
                {
                    output.Write(" = ");
                    Entries[i].ExplicitValue.WriteTo(output);
                }
            }
            output.Write("\n}");
        }
    }
}
