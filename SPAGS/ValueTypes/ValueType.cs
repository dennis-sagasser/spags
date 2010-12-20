using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ValueType
    {
        public enum ValueTypeCategory
        {
            Int, Float, StringValue, StringBuffer, Void, Struct, FunctionSignature, Array, Null
        }
        protected ValueType(string name, ValueTypeCategory category)
        {
            this.name = name;
            this.Category = category;
        }
        public object UserData;
        public readonly ValueTypeCategory Category;
        private readonly string name;
        public virtual string Name { get { return name; } }
        public override string ToString()
        {
            return Name;
        }
        public class Named : ValueType, INameHolder
        {
            public Named(string name, ValueTypeCategory cat) : base(name, cat) { }
            public virtual NameHolderType NameHolderType { get { return NameHolderType.BasicType; } }
        }
        public Expression CreateDefaultValueExpression()
        {
            switch (Category)
            {
                case ValueTypeCategory.Int:
                    return new Expression.IntegerLiteral(0);
                case ValueTypeCategory.Float:
                    return new Expression.FloatLiteral(0);
                default:
                    return Expression.Null;
            }
        }
        public static readonly Named Int = new Named("int", ValueTypeCategory.Int);
        public static readonly Named Long = new Named("long", ValueTypeCategory.Int);
        public static readonly Named Float = new Named("float", ValueTypeCategory.Float);
        public static readonly Named Char = new Named("char", ValueTypeCategory.Int);
        public static readonly Named Short = new Named("short", ValueTypeCategory.Int);
        public static readonly Named Void = new Named("void", ValueTypeCategory.Void);
        public static readonly Named StringBuffer = new Named("string", ValueTypeCategory.StringBuffer);
        public static readonly Named Null = new Named("null", ValueTypeCategory.Null);

        public static readonly ValueType StringValue = new ValueType("const string", ValueTypeCategory.StringValue);

        public static IEnumerable<Named> YieldValueTypes()
        {
            yield return Int;
            yield return Long;
            yield return Float;
            yield return Char;
            yield return Short;
            yield return Void;
            yield return StringBuffer;
        }
    }
}
