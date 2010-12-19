using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ScriptParser
    {
        private void InitialiseTokenStream(string source)
        {
            this.source = source;
            this.pos = 0;
            this.endPos = source.Length;
            AdvanceToken();
        }
        private string source;
        private int pos;
        private int endPos;
        Token token;

        private string AdvanceUnusedName()
        {
            if (token.Type != TokenType.UnknownWord)
            {
                if (token.Type == TokenType.KnownWord)
                {
                    throw new Exception("name already in use: " + ((Token.KnownWord)token).NameHolder.Name);
                }
                throw new Exception("expecting name, got " + token);
            }
            string name = ((Token.UnknownWord)token).TheWord;
            AdvanceToken();
            return name;
        }

        private string CurrentName
        {
            get
            {
                switch (token.Type)
                {
                    case TokenType.KnownWord:
                        return ((Token.KnownWord)token).NameHolder.Name;
                    case TokenType.UnknownWord:
                        return ((Token.UnknownWord)token).TheWord;
                    default:
                        return null;
                }
            }
        }

        private string AdvanceName(out INameHolder nameHolder)
        {
            string name;
            switch (token.Type)
            {
                case TokenType.KnownWord:
                    nameHolder = ((Token.KnownWord)token).NameHolder;
                    name = nameHolder.Name;
                    AdvanceToken(/* TokenType.KnownWord */);
                    return name;
                case TokenType.UnknownWord:
                    name = ((Token.UnknownWord)token).TheWord;
                    AdvanceToken(/* TokenType.UnknownWord */);
                    nameHolder = null;
                    return name;
                default:
                    nameHolder = null;
                    return null;
            }
        }

        private string AdvancePossibleName()
        {
            string name;
            switch (token.Type)
            {
                case TokenType.KnownWord:
                    name = ((Token.KnownWord)token).NameHolder.Name;
                    AdvanceToken(/* TokenType.KnownWord */);
                    return name;
                case TokenType.UnknownWord:
                    name = ((Token.UnknownWord)token).TheWord;
                    AdvanceToken(/* TokenType.UnknownWord */);
                    return name;
                default:
                    return null;
            }
        }

        private string AdvanceName()
        {
            string name = AdvancePossibleName();
            if (name == null)
            {
                throw new Exception("expecting name, got: " + token);
            }
            return name;
        }

        private T AdvanceNameHolder<T>(string description) where T : class, INameHolder
        {
            T nameHolder = (token.Type != TokenType.KnownWord) ? null : ((Token.KnownWord)token).NameHolder as T;
            if (nameHolder == null) throw new Exception("expecting " + description + ", got " + token);
            AdvanceToken();
            return nameHolder;
        }

        private Token AdvanceToken(TokenType onlyType)
        {
            if (token.Type != onlyType) throw new Exception("expecting " + onlyType + ", got " + token);
            return AdvanceToken();
        }

        private ValueType AdvanceValueType()
        {
            if (token.Type == TokenType.Const)
            {
                AdvanceToken(/* TokenType.Const */);
                if (token.Type != TokenType.KnownWord && ((Token.KnownWord)token).NameHolder != ValueType.StringBuffer)
                {
                    throw new Exception("expecting 'string' after 'const'");
                }
                AdvanceToken(/* TokenType.KnownWord */);
                return ValueType.StringValue;
            }
            ValueType valueType = AdvanceNameHolder<ValueType.Named>("value type");

            // Ignore whether this type is marked as a pointer or not. I honestly don't think it matters -
            // you cannot have pointers to unmanaged structs, or non-pointer references to managed structs
            if (token.Type == TokenType.Multiply)
            {
                AdvanceToken(/* TokenType.Multiply */);
            }

            return valueType;
        }

        private Queue<Token> preprocInjectedTokens = new Queue<Token>();

        // SINGLE LINE MODE: treat \n as EndOfInput
        // for use in preprocessor
        private bool preprocLineMode;
        // PREPROC WORD MODE: this is to make sure names
        // used in a #define are still looked up *at the
        // time that the macro is used*, not at the point
        // it was defined.
        private bool preprocWordMode;

        private Token AdvanceToken()
        {
            bool dot = false;

        findToken:
            if (preprocInjectedTokens.Count > 0)
            {
                token = preprocInjectedTokens.Dequeue();
                if (token is Token.WordForPreproc)
                {
                    string name = ((Token.WordForPreproc)token).TheWord;
                    INameHolder nameHolder;
                    if (LookUpName(name, out nameHolder)) token = Token.GetKnownWord(nameHolder);
                    else token = Token.GetUnknownWord(name);
                }
                goto foundToken;
            }
            if (pos >= endPos) return (token = Token.EndOfInput);
            switch (source[pos++])
            {
                case ' ':
                case '\t':
                case '\r':
                    goto findToken;

                case '\n':
                    if (preprocLineMode) return (token = Token.EndOfInput);
                    goto findToken;

                case '+':
                    if (pos < endPos)
                    {
                        switch (source[pos])
                        {
                            case '+': pos++; token = Token.Increment; goto foundToken;
                            case '=': pos++; token = Token.AddAssign; goto foundToken;
                        }
                    }
                    token = Token.Add;
                    goto foundToken;

                case '-':
                    if (pos < endPos)
                    {
                        switch (source[pos])
                        {
                            case '-': pos++; token = Token.Decrement; goto foundToken;
                            case '=': pos++; token = Token.SubtractAssign; goto foundToken;
                        }
                    }
                    token = Token.Subtract;
                    goto foundToken;

                case '*':
                    token = Token.Multiply;
                    goto foundToken;

                case '%':
                    token = Token.Modulus;
                    goto foundToken;

                case '&':
                    if (pos < endPos && source[pos] == '&')
                    {
                        pos++;
                        token = Token.LogicalAnd;
                    }
                    else
                    {
                        token = Token.BitwiseAnd;
                    }
                    goto foundToken;

                case '|':
                    if (pos < endPos && source[pos] == '|')
                    {
                        pos++;
                        token = Token.LogicalOr;
                    }
                    else
                    {
                        token = Token.BitwiseOr;
                    }
                    goto foundToken;

                case '!':
                    if (pos < endPos && source[pos] == '=')
                    {
                        pos++;
                        token = Token.IsNotEqualTo;
                        goto foundToken;
                    }
                    token = Token.LogicalNot;
                    goto foundToken;

                case '^':
                    token = Token.BitwiseXor;
                    goto foundToken;

                case '.':
                    if (pos < endPos && source[pos] == '.')
                    {
                        pos++;
                        if (pos < endPos && source[pos] == '.')
                        {
                            pos++;
                            token = Token.DotDotDot;
                            goto foundToken;
                        }
                        goto default;
                    }
                    else
                    {
                        // avoid calling AdvanceToken() recursively if the preprocessor is blocking the stream
                        if (preprocSuccess < preprocLevel) goto findToken;
                        int prepos = pos - 1;
                        AdvanceToken();
                        if (CurrentName == null)
                        {
                            pos = prepos;
                            goto default;
                        }
                        token = Token.DotWord.Get(CurrentName);
                        goto foundToken;
                    }

                case ',': token = Token.Comma; goto foundToken;
                case ';': token = Token.Semicolon; goto foundToken;

                case '<':
                    if (pos < endPos)
                    {
                        switch (source[pos])
                        {
                            case '=':
                                pos++;
                                token = Token.IsLessThanOrEqualTo;
                                goto foundToken;
                            case '<':
                                pos++;
                                token = Token.BitwiseLeftShift;
                                goto foundToken;
                        }
                    }
                    token = Token.IsLessThan;
                    goto foundToken;

                case '>':
                    if (pos < endPos)
                    {
                        switch (source[pos])
                        {
                            case '=':
                                pos++;
                                token = Token.IsGreaterThanOrEqualTo;
                                goto foundToken;
                            case '>':
                                pos++;
                                token = Token.BitwiseRightShift;
                                goto foundToken;
                        }
                    }
                    token = Token.IsGreaterThan;
                    goto foundToken;

                case '(': token = Token.LeftParenthesis; goto foundToken;
                case ')': token = Token.RightParenthesis; goto foundToken;
                case '{': token = Token.LeftCurlyBrace; goto foundToken;
                case '}': token = Token.RightCurlyBrace; goto foundToken;
                case '[': token = Token.LeftSquareBracket; goto foundToken;
                case ']': token = Token.RightSquareBracket; goto foundToken;

                case '=':
                    if (pos < endPos && source[pos] == '=')
                    {
                        pos++;
                        token = Token.IsEqualTo;
                        goto foundToken;
                    }
                    token = Token.Assign;
                    goto foundToken;

                case '/':
                    if (pos < endPos)
                    {
                        switch (source[pos])
                        {
                            case '*':
                                // multi-line comment
                                pos++;
                                bool newLine = (pos >= endPos || source[pos] == '\n');
                                for (; ; )
                                {
                                    if (++pos >= endPos) throw new Exception("unterminated comment");
                                    if (source[pos] == '\n') newLine = true;
                                    if (source[pos] == '/' && source[pos - 1] == '*')
                                    {
                                        pos++;
                                        break;
                                    }
                                }
                                if (newLine && preprocLineMode) return (token = Token.EndOfInput);
                                goto findToken;
                            case '/':
                                // single-line comment
                                for (; ; )
                                {
                                    if (++pos >= endPos) return (token = Token.EndOfInput);
                                    if (source[pos] == '\n') break;
                                }
                                if (preprocLineMode) return (token = Token.EndOfInput);
                                goto findToken;
                        }
                    }
                    token = Token.Divide;
                    goto foundToken;

                case ':':
                    if (pos < endPos && source[pos] == ':')
                    {
                        pos++;
                        token = Token.DoubleColon;
                        goto foundToken;
                    }
                    goto default;

                case '#':
                    if (preprocWordMode || preprocLineMode)
                    {
                        throw new Exception("cannot handle preprocessor instruction inside preprocessor instruction");
                    }
                    HandlePreprocessorInstruction();
                    goto findToken;

                case '"':
                    StringBuilder stringLiteralBuilder = new StringBuilder();
                    for (; ; )
                    {
                        if (pos >= endPos) throw new Exception("unterminated string");
                        char c = source[pos++];
                        switch (c)
                        {
                            case '"': break;
                            case '\\':
                                if (pos >= endPos) throw new Exception("unterminated string");
                                switch (source[pos++])
                                {
                                    case '[': stringLiteralBuilder.Append("\\["); continue;
                                    case 'r': stringLiteralBuilder.Append("\r"); continue;
                                    case 'n': stringLiteralBuilder.Append("\n"); continue;
                                    default: stringLiteralBuilder.Append(source[pos - 1]); continue;
                                }
                            default: stringLiteralBuilder.Append(c); continue;
                        }
                        break;
                    }
                    token = Token.StringLiteral.Get(stringLiteralBuilder.ToString());
                    goto foundToken;

                case '\'':
                    if (pos >= endPos || source[pos] == '\r' || source[pos] == '\n')
                    {
                        throw new Exception("unterminated character literal");
                    }
                    switch (source[pos++])
                    {
                        case '\'': throw new Exception("malformed character literal");
                        case '\\':
                            if (pos >= endPos) throw new Exception("unterminated character literal");
                            if (source[pos] != '\'') throw new Exception("backslash only supported for ' in chars");
                            token = Token.CharLiteral.Get('\'');
                            pos++;
                            break;
                        default:
                            token = Token.CharLiteral.Get(source[pos - 1]);
                            break;
                    }
                    if (pos >= endPos) throw new Exception("unterminated character literal)");
                    if (source[pos++] != '\'') throw new Exception("malformed character literal");
                    goto foundToken;

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    int numStart = pos - 1;
                    while (pos < endPos)
                    {
                        if (source[pos] >= '0' && source[pos] <= '9')
                        {
                            pos++;
                            continue;
                        }
                        if (source[pos] == '.')
                        {
                            while (++pos < endPos && source[pos] >= '0' && source[pos] <= '9') ;
                            token = Token.FloatLiteral.Get(double.Parse(source.Substring(numStart, pos - numStart)));
                            goto foundToken;
                        }
                        break;
                    }
                    token = Token.IntegerLiteral.Get(int.Parse(source.Substring(numStart, pos - numStart)));
                    goto foundToken;

                    // I know it seems kinda ugly and ridiculous to have the whole alphabet
                    // spelled out case-by-case, but I'm hoping that it leads to a nice and speedy
                    // single underlying switch block in the generated IL...
                case 'a':
                case 'b':
                case 'c':
                case 'd':
                case 'e':
                case 'f':
                case 'g':
                case 'h':
                case 'i':
                case 'j':
                case 'k':
                case 'l':
                case 'm':
                case 'n':
                case 'o':
                case 'p':
                case 'q':
                case 'r':
                case 's':
                case 't':
                case 'u':
                case 'v':
                case 'w':
                case 'x':
                case 'y':
                case 'z':
                case 'A':
                case 'B':
                case 'C':
                case 'D':
                case 'E':
                case 'F':
                case 'G':
                case 'H':
                case 'I':
                case 'J':
                case 'K':
                case 'L':
                case 'M':
                case 'N':
                case 'O':
                case 'P':
                case 'Q':
                case 'R':
                case 'S':
                case 'T':
                case 'U':
                case 'V':
                case 'W':
                case 'X':
                case 'Y':
                case 'Z':
                case '_':
                    int wordStart = pos - 1;
                    while (pos < endPos)
                    {
                        char c = source[pos];
                        if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && (c < '0' || c > '9') && (c != '_'))
                        {
                            break;
                        }
                        pos++;
                    }
                    if (dot)
                    {
                        token = Token.DotWord.Get(source.Substring(wordStart, pos - wordStart));
                        goto foundToken;
                    }
                    else
                    {
                        string name = source.Substring(wordStart, pos - wordStart);
                        INameHolder nameHolder;
                        if (LookUpName(name, out nameHolder))
                        {
                            if (nameHolder is Token.Keyword)
                            {
                                token = (Token.Keyword)nameHolder;
                            }
                            else
                            {
                                if (preprocWordMode)
                                {
                                    token = Token.GetWordForPreproc(name);
                                    goto foundToken;
                                }
                                else if (nameHolder is Constant.TokenSequence)
                                {
                                    Constant.TokenSequence fragment = (Constant.TokenSequence)nameHolder;
                                    foreach (Token t in fragment.Tokens)
                                    {
                                        preprocInjectedTokens.Enqueue(t);
                                    }
                                    goto findToken;
                                }
                                else
                                {
                                    token = Token.GetKnownWord(nameHolder);
                                }
                            }
                        }
                        else
                        {
                            if (preprocWordMode)
                            {
                                token = Token.GetWordForPreproc(name);
                            }
                            else
                            {
                                token = Token.GetUnknownWord(name);
                            }
                        }
                        goto foundToken;
                    }

                default:
                    token = Token.GetUnknownSymbol(source.Substring(pos-1, 1));
                    goto foundToken;
            }

        foundToken:
            if (!preprocLineMode && (preprocSuccess < preprocLevel)) goto findToken;
            return token;
        }

        public NameDictionary Namespace = new NameDictionary();

        bool LookUpName(string name, out INameHolder nameHolder)
        {
            if (Namespace.TryGetValue(name, out nameHolder)) return true;
            if (CurrentBlock != null)
            {
                for (int i = 0; i < FunctionContext.Count; i++)
                {
                    if (FunctionContext[i].Scope.TryGetValue(name, out nameHolder)) return true;
                }
                if (CurrentBlock.Scope.TryGetValue(name, out nameHolder)) return true;
            }
            nameHolder = null;
            return false;
        }
    }
}
