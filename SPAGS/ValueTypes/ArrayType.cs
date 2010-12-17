using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS
{
    public class ArrayType : ValueType
    {
        public ArrayType(ValueType elementType, Expression length)
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
    }
}
