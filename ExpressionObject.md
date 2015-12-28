
```
namespace SPAGS
{
  enum ExpressionType
  {
    UnaryOperator, BinaryOperator, AllocateArray, Function,
    StructType, Variable, ArrayIndex, Attribute, Method,
    Call, Null, EnumValue, IntegerLiteral, StringLiteral,
    Constant, CharLiteral, FloatLiteral, Field
  }

  abstract class Expression
  {
    ExpressionType Type;
    bool IsConstant();
    static Expression.NullType Null;
  }

  class Expression.IntegerLiteral : Expression
  {
    int Value;
  }

  class Expression.FloatLiteral : Expression
  {
    double Value;
  }

  class Expression.Function : Expression
  {
  }

  class Expression.Call : Expression
  {
    Expression CallingOn;
    List<Expression> Parameters;
  }

  class Expression.ArrayIndex : Expression
  {
    Expression Target;
    Expression Index;
  }

  class Expression.CharLiteral : Expression
  {
    char Value;
  }

  class Expression.StructType : Expression
  {
  }

  class Expression.Constant : Expression
  {
  }

  class Expression.Attribute : Expression
  {
    Expression Target;
  }

  class Expression.Field : Expression
  {
  }

  class Expression.Method : Expression
  {
  }

  class Expression.Variable : Expression
  {
  }

  class Expression.EnumValue : Expression
  {
  }

  class Expression.StringLiteral : Expression
  {
  }

  class Expression.NullType : Expression
  {
  }

  class Expression.UnaryOperator : Expression
  {
  }

  class Expression.BinaryOperator : Expression
  {
  }

  class Expression.AllocateArray : Expression
  {
  }
}
```