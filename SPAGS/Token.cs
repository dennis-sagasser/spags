using System;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;
using SPAGS.Util;

namespace SPAGS
{
    public enum TokenType
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

        BitwiseAnd,
        BitwiseOr,
        BitwiseXor,
        BitwiseLeftShift,
        BitwiseRightShift,

        Comma, Semicolon, DotDotDot, DoubleColon,

        If, While, Return, Else, Import, _TryImport, Export, Managed, Struct, AutoPtr, InternalString, Extends,
        NoLoopCheck, Attribute, Static, ReadOnly, Protected, WriteProtected, Const, Null, New, Enum,

        ConstantExpression,

        EndOfInput, Unknown
    }
    public abstract class Token
    {

        protected Token(TokenType type)
        {
            Type = type;
        }
        public readonly TokenType Type;

        public class SimpleToken : Token
        {
            internal SimpleToken(string asString, TokenType type) : base(type)
            {
                this.asString = asString;
            }
            protected string asString;
            public override string ToString()
            {
                return asString;
            }
        }

        public class Keyword : SimpleToken, INameHolder
        {
            public NameHolderType NameHolderType { get { return NameHolderType.Keyword; } }
            internal Keyword(string keyword, TokenType type)
                : base(keyword, type)
            {
            }
            public string Name { get { return asString; } }
        }

        public static Token GetUnknownSymbol(string symbol)
        {
            return new SimpleToken("(unknown symbol: '" + symbol + "')", TokenType.Unknown);
        }

        public static Token GetKnownWord(INameHolder nameHolder)
        {
            return new KnownWord(nameHolder);
        }

        public static Token GetUnknownWord(string word)
        {
            return new UnknownWord(word);
        }

        public class UnknownWord : Token
        {
            internal UnknownWord(string word) : base(TokenType.UnknownWord)
            {
                TheWord = word;
            }
            public readonly string TheWord;
            public override string ToString()
            {
                return TheWord;
            }
        }

        public static Token GetWordForPreproc(string word)
        {
            return new WordForPreproc(word);
        }

        public class WordForPreproc : UnknownWord
        {
            internal WordForPreproc(string word)
                : base(word)
            {
            }
        }

        public class KnownWord : Token
        {
            internal KnownWord(INameHolder nameHolder) : base(TokenType.KnownWord)
            {
                NameHolder = nameHolder;
            }
            public readonly INameHolder NameHolder;
            public override string ToString()
            {
                return NameHolder.Name;
            }
        }

        public class DotWord : Token
        {
            private DotWord(string word) : base(TokenType.DotWord)
            {
                TheWord = word;
            }
            public readonly string TheWord;
            public override string ToString()
            {
                return "." + TheWord;
            }
            private static LiteralCache<string, DotWord> cache = new LiteralCache<string, DotWord>();
            public static DotWord Get(string wordString)
            {
                DotWord wordToken;
                if (!cache.TryGetCached(wordString, out wordToken))
                {
                    wordToken = cache.AddToCache(wordString, new DotWord(wordString));
                }
                return wordToken;
            }
        }

        public static readonly Token EndOfInput = new SimpleToken("(end of input)", TokenType.EndOfInput);

        public static readonly Token LeftCurlyBrace     = new SimpleToken("{", TokenType.LeftCurlyBrace);
        public static readonly Token RightCurlyBrace    = new SimpleToken("}", TokenType.RightCurlyBrace);
        public static readonly Token LeftParenthesis    = new SimpleToken("(", TokenType.LeftParenthesis);
        public static readonly Token RightParenthesis   = new SimpleToken(")", TokenType.RightParenthesis);
        public static readonly Token LeftSquareBracket  = new SimpleToken("[", TokenType.LeftSquareBracket);
        public static readonly Token RightSquareBracket = new SimpleToken("]", TokenType.RightSquareBracket);

        public static readonly Token Assign = new SimpleToken("=", TokenType.Assign);

        public static readonly Token Increment = new SimpleToken("++", TokenType.Increment);
        public static readonly Token Decrement = new SimpleToken("--", TokenType.Decrement);

        public static readonly Token Add            = new SimpleToken("+", TokenType.Add);
        public static readonly Token AddAssign      = new SimpleToken("+=", TokenType.AddAssign);
        public static readonly Token Subtract       = new SimpleToken("-", TokenType.Subtract);
        public static readonly Token SubtractAssign = new SimpleToken("-=", TokenType.SubtractAssign);

        public static readonly Token Comma = new SimpleToken(",", TokenType.Comma);
        public static readonly Token Semicolon = new SimpleToken(";", TokenType.Semicolon);
        public static readonly Token DoubleColon = new SimpleToken("::", TokenType.DoubleColon);
        public static readonly Token DotDotDot = new SimpleToken("...", TokenType.DotDotDot);

        public static readonly Token Divide         = new SimpleToken("/", TokenType.Divide);
        public static readonly Token Multiply       = new SimpleToken("*", TokenType.Multiply);
        public static readonly Token Modulus        = new SimpleToken("%", TokenType.Modulus);

        public static readonly Token IsEqualTo              = new SimpleToken("==", TokenType.IsEqualTo);
        public static readonly Token IsNotEqualTo           = new SimpleToken("!=", TokenType.IsNotEqualTo);
        public static readonly Token IsGreaterThan          = new SimpleToken(">", TokenType.IsGreaterThan);
        public static readonly Token IsLessThan             = new SimpleToken("<", TokenType.IsLessThan);
        public static readonly Token IsGreaterThanOrEqualTo   = new SimpleToken(">=", TokenType.IsGreaterThanOrEqualTo);
        public static readonly Token IsLessThanOrEqualTo      = new SimpleToken("<=", TokenType.IsLessThanOrEqualTo);

        public static readonly Token LogicalAnd = new SimpleToken("&&", TokenType.LogicalAnd);
        public static readonly Token LogicalOr  = new SimpleToken("||", TokenType.LogicalOr);
        public static readonly Token LogicalNot = new SimpleToken("!", TokenType.LogicalNot);

        public static readonly Token BitwiseAnd         = new SimpleToken("&", TokenType.BitwiseAnd);
        public static readonly Token BitwiseOr          = new SimpleToken("|", TokenType.BitwiseOr);
        public static readonly Token BitwiseXor         = new SimpleToken("^", TokenType.BitwiseXor);

        public static readonly Token BitwiseLeftShift           = new SimpleToken("<<", TokenType.BitwiseLeftShift);
        public static readonly Token BitwiseRightShift          = new SimpleToken(">>", TokenType.BitwiseRightShift);

        public static readonly Keyword If               = new Keyword("if", TokenType.If);
        public static readonly Keyword While            = new Keyword("while", TokenType.While);
        public static readonly Keyword Return           = new Keyword("return", TokenType.Return);
        public static readonly Keyword Else             = new Keyword("else", TokenType.Else);
        public static readonly Keyword Import           = new Keyword("import", TokenType.Import);
        public static readonly Keyword _TryImport       = new Keyword("_tryimport", TokenType._TryImport);
        public static readonly Keyword Export           = new Keyword("export", TokenType.Export);
        public static readonly Keyword Managed          = new Keyword("managed", TokenType.Managed);
        public static readonly Keyword Struct           = new Keyword("struct", TokenType.Struct);
        public static readonly Keyword AutoPtr          = new Keyword("autoptr", TokenType.AutoPtr);
        public static readonly Keyword InternalString   = new Keyword("internalstring", TokenType.InternalString);
        public static readonly Keyword Extends          = new Keyword("extends", TokenType.Extends);
        public static readonly Keyword NoLoopCheck      = new Keyword("noloopcheck", TokenType.NoLoopCheck);
        public static readonly Keyword Attribute        = new Keyword("attribute", TokenType.Attribute);
        public static readonly Keyword Static           = new Keyword("static", TokenType.Static);
        public static readonly Keyword ReadOnly         = new Keyword("readonly", TokenType.ReadOnly);
        public static readonly Keyword Protected        = new Keyword("protected", TokenType.Protected);
        public static readonly Keyword WriteProtected   = new Keyword("writeprotected", TokenType.WriteProtected);
        public static readonly Keyword Const            = new Keyword("const", TokenType.Const);
        public static readonly Keyword Null             = new Keyword("null", TokenType.Null);
        public static readonly Keyword New              = new Keyword("new", TokenType.New);
        public static readonly Keyword Enum             = new Keyword("enum", TokenType.Enum);

        public static IEnumerable<Keyword> YieldKeywords()
        {
            yield return If;
            yield return While;
            yield return Return;
            yield return Else;
            yield return Import;
            yield return _TryImport;
            yield return Export;
            yield return Managed;
            yield return Struct;
            yield return AutoPtr;
            yield return InternalString;
            yield return Extends;
            yield return NoLoopCheck;
            yield return Attribute;
            yield return Static;
            yield return ReadOnly;
            yield return Protected;
            yield return WriteProtected;
            yield return Const;
            yield return Null;
            yield return New;
            yield return Enum;
        }

        public abstract class Literal : Token
        {
            protected Literal(TokenType type) : base(type) { }
            public abstract Expression Expression { get; }
        }

        public class IntegerLiteral : Literal
        {
            public IntegerLiteral(int value) : base(TokenType.IntegerLiteral)
            {
                Value = value;
            }
            public readonly int Value;
            public override string ToString()
            {
                return Value.ToString();
            }
            private static LiteralCache<int, IntegerLiteral> cache = new LiteralCache<int, IntegerLiteral>();
            public static IntegerLiteral Get(int value)
            {
                IntegerLiteral literal;
                if (!cache.TryGetCached(value, out literal))
                {
                    literal = cache.AddToCache(value, new IntegerLiteral(value));
                }
                return literal;
            }
            private Expression _expr;
            public override Expression Expression
            {
                get
                {
                    if (_expr == null) _expr = new Expression.IntegerLiteral(Value);
                    return _expr;
                }
            }
        }

        public class StringLiteral : Literal
        {
            public StringLiteral(string value)
                : base(TokenType.StringLiteral)
            {
                Value = value;
            }
            public readonly string Value;
            public override string ToString()
            {
                return "\""
                    + Regex.Replace(Value, @"[\r\n\\""]",
                        delegate(Match m)
                        {
                            switch (m.Value[0])
                            {
                                case '\r': return "\\r";
                                case '\n': return "\\n";
                                default: return "\\" + m.Value;
                            }
                        })
                    + "\"";
            }
            private static LiteralCache<string, StringLiteral> cache = new LiteralCache<string, StringLiteral>();
            public static StringLiteral Get(string value)
            {
                StringLiteral literal;
                if (!cache.TryGetCached(value, out literal))
                {
                    literal = cache.AddToCache(value, new StringLiteral(value));
                }
                return literal;
            }
            private Expression _expr;
            public override Expression Expression
            {
                get
                {
                    if (_expr == null) _expr = new Expression.StringLiteral(Value);
                    return _expr;
                }
            }
        }

        public class FloatLiteral : Literal
        {
            public FloatLiteral(double value)
                : base(TokenType.FloatLiteral)
            {
                Value = value;
            }
            public readonly double Value;
            public override string ToString()
            {
                return Value.ToString("0.0###################################");
            }
            private static LiteralCache<double, FloatLiteral> cache = new LiteralCache<double, FloatLiteral>();
            public static FloatLiteral Get(double value)
            {
                FloatLiteral literal;
                if (!cache.TryGetCached(value, out literal))
                {
                    literal = cache.AddToCache(value, new FloatLiteral(value));
                }
                return literal;
            }
            private Expression _expr;
            public override Expression Expression
            {
                get
                {
                    if (_expr == null) _expr = new Expression.FloatLiteral(Value);
                    return _expr;
                }
            }
        }

        public class CharLiteral : Literal
        {
            public CharLiteral(char value)
                : base(TokenType.CharLiteral)
            {
                Value = value;
            }
            public readonly char Value;
            public override string ToString()
            {
                switch (Value)
                {
                    case '\r':
                        return "'\r'";
                    case '\n':
                        return "'\n'";
                    case '\'':
                    case '\\':
                        return "'\\" + Value + "'";
                    default:
                        return "'" + Value + "'";
                }
            }
            private static LiteralCache<char, CharLiteral> cache = new LiteralCache<char, CharLiteral>();
            public static CharLiteral Get(char value)
            {
                CharLiteral literal;
                if (!cache.TryGetCached(value, out literal))
                {
                    literal = cache.AddToCache(value, new CharLiteral(value));
                }
                return literal;
            }
            private Expression _expr;
            public override Expression Expression
            {
                get
                {
                    if (_expr == null) _expr = new Expression.CharLiteral(Value);
                    return _expr;
                }
            }
        }

    }
}
