using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using ActualStructType = SPAGS.ValueType.Struct;
using ActualVariable = SPAGS.Variable;
using ActualEnumValue = SPAGS.EnumValue;
using ActualConstant = SPAGS.Constant.Expression;
using ActualFunction = SPAGS.Function;
using ActualAttribute = SPAGS.StructMember.Attribute;
using ActualField = SPAGS.StructMember.Field;
using ActualMethod = SPAGS.StructMember.Method;

namespace SPAGS
{
    public enum ExpressionType
    {
        UnaryOperator,
        BinaryOperator,
        AllocateArray,
        Function,
        StructType,
        Variable,
        ArrayIndex,
        Attribute,
        Method,
        Call,
        Null,
        EnumValue,
        IntegerLiteral,
        StringLiteral,
        Constant,
        CharLiteral,
        FloatLiteral,
        Field
    }
    public abstract class Expression
    {
        protected Expression(ExpressionType type)
        {
            Type = type;
        }
        public readonly ExpressionType Type;
        public object UserData;
        public abstract bool IsConstant();
        public virtual bool TryGetIntValue(out int value) { value = 0; return false; }
        public virtual bool TryGetFloatValue(out double value) { value = 0; return false; }
        public virtual bool TryGetStringValue(out string value) { value = null; return false; }
        public virtual IEnumerable<Expression> YieldSubExpressions()
        {
            yield break;
        }
        public virtual IEnumerable<ActualFunction> YieldFunctions()
        {
            yield break;
        }
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (TextWriter output = new StringWriter(sb))
            {
                WriteTo(output);
            }
            return sb.ToString();
        }
        public abstract void WriteTo(TextWriter output);

        public abstract ValueType GetValueType();

        public class IntegerLiteral : Expression
        {
            public IntegerLiteral(int value)
                : base(ExpressionType.IntegerLiteral)
            {
                Value = value;
            }
            public readonly int Value;
            public override void WriteTo(TextWriter output)
            {
                output.Write(Value);
            }
            public override bool IsConstant()
            {
                return true;
            }
            public override ValueType GetValueType()
            {
                return ValueType.Int;
            }
            public override bool TryGetIntValue(out int value)
            {
                value = Value;
                return true;
            }
        }

        public class FloatLiteral : Expression
        {
            public FloatLiteral(double value)
                : base(ExpressionType.FloatLiteral)
            {
                Value = value;
            }
            public readonly double Value;
            public override bool IsConstant()
            {
                return true;
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write(Value.ToString("0.0####################"));
            }
            public override ValueType GetValueType()
            {
                return ValueType.Float;
            }
            public override bool TryGetFloatValue(out double value)
            {
                value = Value;
                return true;
            }
        }

        public class Function : Expression
        {
            public Function(ActualFunction func)
                : base(ExpressionType.Function)
            {
                TheFunction = func;
            }
            public readonly ActualFunction TheFunction;
            public override bool IsConstant()
            {
                return false;
            }
            public override IEnumerable<ActualFunction> YieldFunctions()
            {
                yield return TheFunction;
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheFunction.Name);
            }
            public override ValueType GetValueType()
            {
                return TheFunction.Signature;
            }
        }

        public class Call : Expression
        {
            public Call(Expression callingOn, List<Expression> parameters)
                : base(ExpressionType.Call)
            {
                CallingOn = callingOn;
                Parameters = parameters;
            }
            public readonly Expression CallingOn;
            public readonly List<Expression> Parameters;
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return CallingOn;
                foreach (Expression expr in Parameters) yield return expr;
            }
            public override void WriteTo(TextWriter output)
            {
                if (ParsingSettings.FullMethodMode && CallingOn.Type == ExpressionType.Method)
                {
                    Expression.Method method = (Expression.Method)CallingOn;
                    output.Write(method.TheMethod.Function.Name);
                    output.Write("(");
                    if (method.Target != null)
                    {
                        method.Target.WriteTo(output);
                        if (Parameters.Count > 0) output.Write(", ");
                    }
                    for (int i = 0; i < Parameters.Count; i++)
                    {
                        if (i > 0) output.Write(", ");
                        Parameters[i].WriteTo(output);
                    }
                    output.Write(")");
                }
                else
                {
                    CallingOn.WriteTo(output);
                    output.Write("(");
                    for (int i = 0; i < Parameters.Count; i++)
                    {
                        if (i > 0) output.Write(", ");
                        Parameters[i].WriteTo(output);
                    }
                    output.Write(")");
                }
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                switch (CallingOn.Type)
                {
                    case ExpressionType.Method:
                        return ((Expression.Method)CallingOn).TheMethod.Function.Signature.ReturnType;
                    case ExpressionType.Function:
                        return ((Expression.Function)CallingOn).TheFunction.Signature.ReturnType;
                    default:
                        throw new Exception("unrecognised callable: " + this);
                }
            }
        }

        public class ArrayIndex : Expression
        {
            public ArrayIndex(Expression target, Expression index)
                : base(ExpressionType.ArrayIndex)
            {
                Target = target;
                Index = index;
            }
            public readonly Expression Target;
            public readonly Expression Index;
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return Target;
                yield return Index;
            }
            public override void WriteTo(TextWriter output)
            {
                Target.WriteTo(output);
                output.Write("[");
                Index.WriteTo(output);
                output.Write("]");
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                switch (Target.Type)
                {
                    case ExpressionType.Variable:
                        ActualVariable v = ((Expression.Variable)Target).TheVariable;
                        if (v.Type.Category != ValueType.ValueTypeCategory.Array)
                        {
                            throw new Exception(v.Name + " is not an array");
                        }
                        return ((ValueType.Array)v.Type).ElementType;
                    case ExpressionType.Attribute:
                        ActualAttribute attr = ((Expression.Attribute)Target).TheAttribute;
                        if (!attr.IsArray)
                        {
                            throw new Exception(attr + " is not an array attribute");
                        }
                        return attr.Getter.Signature.ReturnType;
                    case ExpressionType.Field:
                        ActualField field = ((Expression.Field)Target).TheField;
                        if (field.Type.Category != ValueType.ValueTypeCategory.Array)
                        {
                            throw new Exception(field.Name + " is not an array field");
                        }
                        return ((ValueType.Array)field.Type).ElementType;
                    default:
                        throw new Exception("unrecognised array: " + this);
                }
            }
        }

        public class CharLiteral : Expression
        {
            public CharLiteral(char value)
                : base(ExpressionType.CharLiteral)
            {
                Value = value;
            }
            public readonly char Value;
            public override bool IsConstant()
            {
                return true;
            }
            public override void WriteTo(TextWriter output)
            {
                if (Value == '\'')
                {
                    output.Write("'\\''");
                }
                else
                {
                    output.Write("'" + Value + "'");
                }
            }
            public override ValueType GetValueType()
            {
                return ValueType.Char;
            }
            public override bool TryGetIntValue(out int value)
            {
                value = (int)Value;
                return true;
            }
        }

        public class StructType : Expression
        {
            public StructType(ActualStructType structType)
                : base(ExpressionType.StructType)
            {
                TheStructType = structType;
            }
            public ActualStructType TheStructType;
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheStructType.Name);
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                return TheStructType;
            }
        }

        public class Constant : Expression
        {
            public Constant(ActualConstant constant)
                : base(ExpressionType.Constant)
            {
                TheConstant = constant;
            }
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return TheConstant.TheExpression;
            }
            public readonly ActualConstant TheConstant;
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheConstant.Name);
            }
            public override bool IsConstant()
            {
                // very much so
                return true;
            }
            public override ValueType GetValueType()
            {
                return TheConstant.TheExpression.GetValueType();
            }
            public override bool TryGetFloatValue(out double value)
            {
                return this.TheConstant.TheExpression.TryGetFloatValue(out value);
            }
            public override bool TryGetIntValue(out int value)
            {
                return this.TheConstant.TheExpression.TryGetIntValue(out value);
            }
            public override bool TryGetStringValue(out string value)
            {
                return this.TheConstant.TheExpression.TryGetStringValue(out value);
            }
        }

        public class Attribute : Expression
        {
            public Attribute(ActualStructType structType, ActualAttribute attribute, Expression target)
                : base(ExpressionType.Attribute)
            {
                TheStructType = structType;
                TheAttribute = attribute;
                Target = target;
            }
            public readonly ActualStructType TheStructType;
            public readonly ActualAttribute TheAttribute;
            public readonly Expression Target;
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                if (Target != null) yield return Target;
            }
            public override IEnumerable<ActualFunction> YieldFunctions()
            {
                yield return TheAttribute.Getter;
            }
            public override void WriteTo(TextWriter output)
            {
                if (ParsingSettings.FullAttributeMode)
                {
                    output.Write(TheAttribute.Getter.Name + "(");
                    if (Target != null) Target.WriteTo(output);
                    output.Write(")");
                    return;
                }
                else
                {
                    if (Target == null)
                    {
                        output.Write(TheStructType.Name);
                    }
                    else
                    {
                        Target.WriteTo(output);
                    }
                    output.Write("." + TheAttribute.Name);
                }
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                if (TheAttribute.IsArray) throw new Exception(this + " must be accessed as an array");
                return TheAttribute.Getter.Signature.ReturnType;
            }
        }

        public class Field : Expression
        {
            public Field(ActualStructType structType, ActualField field, Expression target)
                : base(ExpressionType.Field)
            {
                TheStructType = structType;
                TheField = field;
                Target = target;
            }
            public readonly ActualStructType TheStructType;
            public readonly ActualField TheField;
            public readonly Expression Target;
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return Target;
            }
            public override void WriteTo(TextWriter output)
            {
                Target.WriteTo(output);
                output.Write("." + TheField.Name);
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                return TheField.Type;
            }
        }

        public class Method : Expression
        {
            public Method(ActualStructType structType, ActualMethod method, Expression target)
                : base(ExpressionType.Method)
            {
                TheStructType = structType;
                TheMethod = method;
                Target = target;
            }
            public readonly ActualStructType TheStructType;
            public readonly ActualMethod TheMethod;
            public readonly Expression Target;
            public override IEnumerable<ActualFunction> YieldFunctions()
            {
                yield return TheMethod.Function;
            }
            public override void WriteTo(TextWriter output)
            {
                if (Target == null) output.Write(TheStructType.Name);
                else Target.WriteTo(output);
                output.Write("." + TheMethod.Name);
            }
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                if (Target != null) yield return Target;
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                return TheMethod.Function.Signature;
            }
        }

        public class Variable : Expression
        {
            public Variable(ActualVariable var)
                : base(ExpressionType.Variable)
            {
                TheVariable = var;
            }
            public readonly ActualVariable TheVariable;
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheVariable.Name);
            }
            public override bool IsConstant()
            {
                // the very opposite, in fact
                return false;
            }
            public override ValueType GetValueType()
            {
                return TheVariable.Type;
            }
        }

        public class EnumValue : Expression
        {
            public EnumValue(ActualEnumValue enumValue)
                : base(ExpressionType.EnumValue)
            {
                TheValue = enumValue;
            }
            public readonly ActualEnumValue TheValue;
            public override bool IsConstant()
            {
                return true;
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheValue.Name);
            }
            public override ValueType GetValueType()
            {
                return ValueType.Int;
            }
            public override bool TryGetIntValue(out int value)
            {
                return TheValue.TryGetIntValue(out value);
            }
        }

        public class StringLiteral : Expression
        {
            public StringLiteral(string value)
                : base(ExpressionType.StringLiteral)
            {
                Value = value;
            }
            public readonly string Value;
            public override void WriteTo(TextWriter output)
            {
                output.Write('"' + Value.Replace("\"", "\\\"") + '"');
            }
            public override bool IsConstant()
            {
                return true;
            }
            public override ValueType GetValueType()
            {
                return ValueType.StringValue;
            }
            public override bool TryGetStringValue(out string value)
            {
                value = Value;
                return true;
            }
        }

        public class NullType : Expression
        {
            public NullType()
                : base (ExpressionType.Null)
            {
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write("null");
            }
            public override bool IsConstant()
            {
                return true;
            }
            public override ValueType GetValueType()
            {
                return ValueType.Null;
            }
        }

        public static readonly NullType Null = new NullType();

        /*
        public class Name : Expression
        {
            public Name(INameHolder holder)
                : base(ExpressionType.Name)
            {
                NameHolder = holder;
            }
            public INameHolder NameHolder;
            public override void WriteTo(TextWriter output)
            {
                Constant.ConstantExpression constant = NameHolder as Constant.ConstantExpression;
                if (constant != null && constant.Undefined) constant.TheExpression.WriteTo(output);
                else output.Write(NameHolder.Name);
            }
            public override bool IsConstant()
            {
                throw new Exception("do not know if " + NameHolder.Name + " is constant");
            }
        }
         */

        public class UnaryOperator : Expression
        {
            public UnaryOperator(Token opToken, Expression operand)
                : base(ExpressionType.UnaryOperator)
            {
                Token = opToken;
                Operand = operand;
            }
            public readonly Token Token;
            public readonly Expression Operand;
            public override void WriteTo(TextWriter output)
            {
                output.Write(Token);
                Operand.WriteTo(output);
            }
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return Operand;
            }
            public override bool IsConstant()
            {
                return Operand.IsConstant();
            }
            public override ValueType GetValueType()
            {
                switch (Token.Type)
                {
                    case TokenType.LogicalNot: return ValueType.Int;
                    case TokenType.Subtract: return Operand.GetValueType();
                    default: throw new Exception("unhandled operator: " + this);
                }
            }
            public override bool TryGetIntValue(out int value)
            {
                switch (Token.Type)
                {
                    case TokenType.LogicalNot:
                        if (Operand.TryGetIntValue(out value))
                        {
                            value = (value == 0) ? 1 : 0;
                            return true;
                        }
                        return false;
                    case TokenType.Subtract:
                        if (Operand.TryGetIntValue(out value))
                        {
                            value = -value;
                            return true;
                        }
                        return false;
                }
                value = 0;
                return false;
            }
            public override bool TryGetFloatValue(out double value)
            {
                switch (Token.Type)
                {
                    case TokenType.Subtract:
                        if (Operand.TryGetFloatValue(out value))
                        {
                            value = -value;
                            return true;
                        }
                        return false;
                }
                value = 0;
                return false;
            }
        }

        public class BinaryOperator : Expression
        {
            public BinaryOperator(Token opToken, Expression left, Expression right)
                : base(ExpressionType.BinaryOperator)
            {
                Token = opToken;
                Left = left;
                Right = right;
            }
            public readonly Token Token;
            public readonly Expression Left;
            public readonly Expression Right;
            public override void WriteTo(TextWriter output)
            {
                Left.WriteTo(output);
                output.Write(" " + Token + " ");
                Right.WriteTo(output);
            }
            public override bool IsConstant()
            {
                return Left.IsConstant() && Right.IsConstant();
            }
            public override ValueType GetValueType()
            {
                switch (Token.Type)
                {
                    case TokenType.Add:
                    case TokenType.Subtract:
                    case TokenType.Multiply:
                    case TokenType.Divide:
                    case TokenType.Modulus:
                        if (Left.GetValueType().Category == ValueType.ValueTypeCategory.Float
                            || Right.GetValueType().Category == ValueType.ValueTypeCategory.Float)
                        {
                            return ValueType.Float;
                        }
                        return ValueType.Int;

                        // bitwise
                    case TokenType.BitwiseAnd:
                    case TokenType.BitwiseLeftShift:
                    case TokenType.BitwiseOr:
                    case TokenType.BitwiseRightShift:
                    case TokenType.BitwiseXor:

                        // booleans
                    case TokenType.IsEqualTo:
                    case TokenType.IsGreaterThan:
                    case TokenType.IsGreaterThanOrEqualTo:
                    case TokenType.IsLessThan:
                    case TokenType.IsLessThanOrEqualTo:
                    case TokenType.IsNotEqualTo:
                    case TokenType.LogicalAnd:
                    case TokenType.LogicalOr:
                        return ValueType.Int;

                    default: throw new Exception("unhandled operator: " + this);
                }
            }
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return Left;
                yield return Right;
            }
        }
        public class AllocateArray : Expression
        {
            public AllocateArray(ValueType elementType, Expression length)
                : base(ExpressionType.AllocateArray)
            {
                ElementType = elementType;
                Length = length;
            }
            public readonly ValueType ElementType;
            public readonly Expression Length;
            public override void WriteTo(TextWriter output)
            {
                output.Write("new " + ElementType.Name + "[");
                Length.WriteTo(output);
                output.Write("]");
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override ValueType GetValueType()
            {
                return new ValueType.Array(ElementType, Length);
            }
            public override IEnumerable<Expression> YieldSubExpressions()
            {
                yield return Length;
            }
        }
    }
    public enum OpPrecedence : int
    {
        // http://www.difranco.net/cop2220/op-prec.htm
        FunctionCall = 150,
        MemberAccess = 150,
        Postfix = 150,
        Prefix = 130,
        MultiplyDivideModulus = 120,
        AddSubtract = 110,
        BitwiseShift = 100,
        LessGreaterLteGte = 90,
        EqualNotEqual = 80,
        BitwiseAnd = 70,
        BitwiseXor = 60,
        BitwiseOr = 50,
        LogicalAnd = 30,
        LogicalOr = 20,
        AssignMutate = 10
    }
}
