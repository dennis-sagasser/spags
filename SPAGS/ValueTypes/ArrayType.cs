using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS
{
    public partial class ValueType
    {
        public class Array : ValueType
        {
            public Array(ValueType elementType, Expression length)
                : base("(array)", ValueTypeCategory.Array)
            {
                ElementType = elementType;
                LengthExpression = length;
            }
            public readonly ValueType ElementType;
            public readonly Expression LengthExpression;
            public override string Name
            {
                get
                {
                    if (LengthExpression != null) return ElementType.Name + "[" + LengthExpression + "]";
                    return ElementType.Name + "[]";
                }
            }
            public override Expression CreateDefaultValueExpression()
            {
                if (this.LengthExpression == null) return Expression.Null;
                return new Expression.AllocateArray(this.ElementType, this.LengthExpression);
            }
        }
    }
}
