using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ValueType
    {
        public class Enum : ValueType.Named
        {
            public Enum(string name)
                : base(name, ValueTypeCategory.Int)
            {
            }
            public Script OwnerScript;
            public List<EnumValue> Entries = new List<EnumValue>();
            public override NameHolderType NameHolderType { get { return NameHolderType.EnumType; } }
            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();
                using (TextWriter output = new StringWriter(sb))
                {
                    this.WriteTo(output);
                }
                return sb.ToString();
            }
            public override Expression CreateDefaultValueExpression()
            {
                foreach (EnumValue val in this.Entries)
                {
                    if (val.Value == 0) return new Expression.EnumValue(val);
                }
                return new Expression.IntegerLiteral(0);
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
    public class EnumValue : Token, INameHolder
    {
        internal EnumValue(ValueType.Enum ownerType, string name, Expression explicitValue, EnumValue previous)
            : base(TokenType.EnumValue)
        {
            OwnerType = ownerType;
            this.name = name;
            Previous = previous;
            ExplicitValue = explicitValue;
        }
        public object UserData;
        public readonly ValueType.Enum OwnerType;
        public NameHolderType NameHolderType { get { return NameHolderType.EnumValue; } }
        private string name;
        public string Name { get { return name; } }
        public readonly Expression ExplicitValue;
        public readonly EnumValue Previous;
        public override string ToString()
        {
            return name;
        }
        public int Value
        {
            get
            {
                int value;
                if (!TryGetIntValue(out value))
                {
                    throw new Exception("unable to get enum value");
                }
                return value;
            }
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
}
