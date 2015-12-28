
```
namespace SPAGS
{
  enum TokenType
  {
    UnknownWord, KnownWord, DotWord,

    LeftCurlyBrace, RightCurlyBrace,
    LeftParenthesis, RightParenthesis,
    LeftSquareBracket, RightSquareBracket,

    Assign,

    Increment, Decrement,

    Add, AddAssign,
    Subtract, SubtractAssign,

    Divide,
    Multiply,
    Modulus,

    IsEqualTo, IsNotEqualTo,
    IsLessThan, IsLessThanOrEqualTo,
    IsGreaterThan, IsGreaterThanOrEqualTo,

    StringLiteral, IntegerLiteral, FloatLiteral, CharLiteral,
    EnumValue,

    LogicalAnd, LogicalOr, LogicalNot,

    BitwiseAnd, BitwiseOr, BitwiseXor, BitwiseLeftShift, BitwiseRightShift,

    Comma, Semicolon, DotDotDot, DoubleColon,

    If, While, Return, Else, Import, Export, Managed, Struct, AutoPtr, InternalString, Extends,
    NoLoopCheck, Attribute, Static, ReadOnly, Protected, WriteProtected, Const, Null, New, Enum,

    ConstantExpression,

    EndOfInput, Unknown
  }

  abstract class Token
  {
    TokenType Type;
  }
}
```