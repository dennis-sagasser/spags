using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ScriptParser
    {
        /*
         * Top-Down Operator Precedence Parser
         *  for AGS-Script, by Duncan Cross
         * 
         * Sources:
         *  http://javascript.crockford.com/tdop/tdop.html
         *  http://effbot.org/zone/tdop-index.htm
         * 
         */

        // force left-to-right operators to be right-to-left instead
        // (AGS Script makes this an option - it used to be always
        // right-to-left but now it is a configurable option, by default
        // left-to-right)
        public bool OverrideLeftToRight;

        // helper overloads
        public Expression AdvanceExpression()
        {
            return ProcessExpression(0);
        }
        public Expression RightToLeft_RightSide(OpPrecedence precedence)
        {
            return ProcessExpression((int)precedence - 1);
        }
        public Expression LeftToRight_RightSide(OpPrecedence precedence)
        {
            if (OverrideLeftToRight)
            {
                return RightToLeft_RightSide(precedence);
            }
            return ProcessExpression((int)precedence);
        }

        // main function
        public Expression ProcessExpression(int rbp)
        {
            Token t = token;
            AdvanceToken();
            Expression left = NullDenotation(t);
            while (rbp < GetLeftBindingPower(token))
            {
                t = token;
                AdvanceToken();
                left = LeftDenotation(t, left);
            }
            return left;
        }

        /*
         * NullDenotation:
         *   This is the first token of an expression.
         *   
         *   It must either be:
         *    + a prefix, like the ! in (!a) or - in (-b)
         *    + a self-contained value, like a literal or variable name
         *    + the start of a parenthesised expression
         */
        private Expression NullDenotation(Token t)
        {
            switch (t.Type)
            {
                case TokenType.LeftParenthesis:
                    Expression expr = AdvanceExpression();
                    AdvanceToken(TokenType.RightParenthesis);
                    //expr.Parenthesised = true;
                    return expr;

                case TokenType.CharLiteral:
                    return ((Token.CharLiteral)t).Expression;

                case TokenType.Null:
                    return Expression.Null;

                case TokenType.StringLiteral:
                    return ((Token.StringLiteral)t).Expression;

                case TokenType.IntegerLiteral:
                    return ((Token.IntegerLiteral)t).Expression;

                case TokenType.FloatLiteral:
                    return ((Token.FloatLiteral)t).Expression;

                case TokenType.KnownWord:
                    INameHolder nameHolder = ((Token.KnownWord)t).NameHolder;
                    switch (nameHolder.NameHolderType)
                    {
                        case NameHolderType.StructType:
                            return new Expression.StructType((StructType)nameHolder);
                        case NameHolderType.Variable:
                            return new Expression.Variable((Variable)nameHolder);
                        case NameHolderType.EnumValue:
                            return new Expression.EnumValue((EnumType.Value)nameHolder);
                        case NameHolderType.Constant:
                            return new Expression.Constant((Constant.Expression)nameHolder);
                        case NameHolderType.Function:
                            return new Expression.Function((Function)nameHolder);
                        default:
                            throw new Exception("expecting variable name, got: " + t);
                    }

                case TokenType.UnknownWord:
                    throw new Exception("unrecognised variable: " + t);

                case TokenType.New:
                    {
                        ValueType elementType = AdvanceValueType();
                        if (elementType is StructType) ((StructType)elementType).InstantiatedArray = true;
                        AdvanceToken(TokenType.LeftSquareBracket);
                        Expression length = AdvanceExpression();
                        AdvanceToken(TokenType.RightSquareBracket);
                        return new Expression.AllocateArray(elementType, length);
                    }


                case TokenType.LogicalNot:
                    return new Expression.UnaryOperator(t, RightToLeft_RightSide(OpPrecedence.Prefix));

                case TokenType.Subtract:
                    Expression operand = RightToLeft_RightSide(OpPrecedence.Prefix);
                    switch (operand.Type)
                    {
                        case ExpressionType.IntegerLiteral:
                            Expression.IntegerLiteral literal = (Expression.IntegerLiteral)operand;
                            return Token.IntegerLiteral.Get(-literal.Value).Expression;
                        case ExpressionType.FloatLiteral:
                            Expression.FloatLiteral floatLiteral = (Expression.FloatLiteral)operand;
                            return Token.FloatLiteral.Get(-floatLiteral.Value).Expression;
                    }
                    return new Expression.UnaryOperator(t, operand);

                default:
                    throw new Exception("unexpected token in expression: " + t);
            }
        }

        /*
         * GetLeftBindingPower:
         *   The left binding power/right binding power stuff
         *   determines whether a previous expression should
         *   be gobbled up into the next one coming along. It
         *   combines both precedence and left-to-right/
         *   right-to-left.
         *   
         *   Look on the Internet for resources that can
         *   explain this better than me.
         */
        public static int GetLeftBindingPower(Token t)
        {
            switch (t.Type)
            {
                case TokenType.LeftParenthesis:
                    return (int)OpPrecedence.FunctionCall;
                case TokenType.LeftSquareBracket:
                case TokenType.DotWord:
                    return (int)OpPrecedence.MemberAccess;
                case TokenType.Multiply:
                case TokenType.Divide:
                case TokenType.Modulus:
                    return (int)OpPrecedence.MultiplyDivideModulus;
                case TokenType.Add:
                case TokenType.Subtract:
                    return (int)OpPrecedence.AddSubtract;
                case TokenType.BitwiseLeftShift:
                case TokenType.BitwiseRightShift:
                    return (int)OpPrecedence.BitwiseShift;
                case TokenType.IsGreaterThan:
                case TokenType.IsGreaterThanOrEqualTo:
                case TokenType.IsLessThan:
                case TokenType.IsLessThanOrEqualTo:
                    return (int)OpPrecedence.LessGreaterLteGte;
                case TokenType.IsEqualTo:
                case TokenType.IsNotEqualTo:
                    return (int)OpPrecedence.EqualNotEqual;
                case TokenType.BitwiseAnd:
                    return (int)OpPrecedence.BitwiseAnd;
                case TokenType.BitwiseXor:
                    return (int)OpPrecedence.BitwiseXor;
                case TokenType.BitwiseOr:
                    return (int)OpPrecedence.BitwiseOr;
                case TokenType.LogicalAnd:
                    return (int)OpPrecedence.LogicalAnd;
                case TokenType.LogicalOr:
                    return (int)OpPrecedence.LogicalOr;
                default:
                    return 0;
            }
        }

        /*
         * LeftDenotation:
         *   This token follows what is, already, a valid
         *   expression. It must be:
         *    + an infix operator like &&
         *    + a field accessor like the .b in a.b
         *    + a function call site like the ( in a()
         *    + an array index like the [ in a[1]
         */
        private Expression LeftDenotation(Token t, Expression left)
        {
            switch (t.Type)
            {
                case TokenType.LeftParenthesis:
                    List<Expression> paramValues = new List<Expression>();
                    if (token.Type == TokenType.RightParenthesis)
                    {
                        AdvanceToken(/* TokenType.RightParenthesis */);
                    }
                    else
                    {
                        for (; ; )
                        {
                            paramValues.Add(AdvanceExpression());
                            if (token.Type == TokenType.Comma)
                            {
                                AdvanceToken(/* TokenType.Comma */);
                                continue;
                            }
                            break;
                        }
                        AdvanceToken(TokenType.RightParenthesis);
                    }
                    switch (left.Type)
                    {
                        case ExpressionType.Function:
                            ((Expression.Function)left).TheFunction.AddCalledBy(CurrentFunction);
                            break;
                        case ExpressionType.Method:
                            ((Expression.Method)left).TheMethod.Function.AddCalledBy(CurrentFunction);
                            break;
                    }
                    return new Expression.Call(left, paramValues);

                case TokenType.DotWord:
                    Token.DotWord dotWord = (Token.DotWord)t;
                    if (left.Type == ExpressionType.StructType)
                    {
                        // static method or attribute
                        StructType structType = ((Expression.StructType)left).TheStructType;
                        StructType.Member staticMember;
                        if (!structType.Members.TryGetValue2<StructType.Member>(dotWord.TheWord, out staticMember))
                        {
                            throw new Exception("static method or attribute not found: " + structType.Name + "." + dotWord.TheWord);
                        }
                        switch (staticMember.MemberType)
                        {
                            case StructMemberType.Attribute:
                                StructType.Attribute attr = (StructType.Attribute)staticMember;
                                if (!attr.IsStatic)
                                {
                                    throw new Exception(structType.Name + "." + attr.Name + " is not a static attribute");
                                }
                                return new Expression.Attribute(structType, attr, null);
                            case StructMemberType.Field:
                                throw new Exception(structType.Name + "." + staticMember.Name + " is a non-static field");
                            case StructMemberType.Method:
                                StructType.Method method = (StructType.Method)staticMember;
                                if (!method.IsStatic)
                                {
                                    throw new Exception(structType.Name + "." + method.Name + " is not a static method");
                                }
                                return new Expression.Method(structType, method, null);
                            default:
                                throw new Exception("internal error - unrecognised struct member: " + staticMember);
                        }
                    }
                    ValueType leftType = left.GetValueType();
                    StructType asStruct = leftType as StructType;
                    if (asStruct == null)
                    {
                        throw new Exception(leftType.Name + " type does not have a member ." + dotWord.TheWord);
                    }
                    else
                    {
                        StructType.Member member;
                        if (!asStruct.Members.TryGetValue2<StructType.Member>(dotWord.TheWord, out member))
                        {
                            throw new Exception("method/field/attribute not found: " + asStruct.Name + "." + dotWord.TheWord);
                        }
                        switch (member.MemberType)
                        {
                            case StructMemberType.Attribute:
                                StructType.Attribute attr = (StructType.Attribute)member;
                                if (attr.IsStatic)
                                {
                                    return new Expression.Attribute(asStruct, attr, null);
                                }
                                else
                                {
                                    return new Expression.Attribute(asStruct, attr, left);
                                }
                            case StructMemberType.Field:
                                StructType.Field field = (StructType.Field)member;
                                return new Expression.Field(asStruct, field, left);
                            case StructMemberType.Method:
                                StructType.Method method = (StructType.Method)member;
                                if (method.IsStatic)
                                {
                                    return new Expression.Method(asStruct, method, null);
                                }
                                else
                                {
                                    return new Expression.Method(asStruct, method, left);
                                }
                            default:
                                throw new Exception("internal error - unrecognised struct member: " + member);
                        }
                    }

                case TokenType.LeftSquareBracket:
                    Expression index = AdvanceExpression();
                    AdvanceToken(TokenType.RightSquareBracket);
                    return new Expression.ArrayIndex(left, index);

                case TokenType.Multiply:
                case TokenType.Divide:
                case TokenType.Modulus:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.MultiplyDivideModulus));

                case TokenType.Add:
                case TokenType.Subtract:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.AddSubtract));

                case TokenType.BitwiseLeftShift:
                case TokenType.BitwiseRightShift:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseShift));

                case TokenType.IsGreaterThan:
                case TokenType.IsGreaterThanOrEqualTo:
                case TokenType.IsLessThan:
                case TokenType.IsLessThanOrEqualTo:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LessGreaterLteGte));

                case TokenType.IsEqualTo:
                case TokenType.IsNotEqualTo:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.EqualNotEqual));

                case TokenType.BitwiseAnd:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseAnd));

                case TokenType.BitwiseXor:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseXor));

                case TokenType.BitwiseOr:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseOr));

                case TokenType.LogicalAnd:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LogicalAnd));

                case TokenType.LogicalOr:
                    return new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LogicalOr));

                default:
                    throw new Exception("unexpected " + t.ToString());
            }
        }

        public Expression AdvancePossibleAssignment()
        {
            if (token.Type == TokenType.Assign)
            {
                AdvanceToken(/* TokenType.Assign */);
                return AdvanceExpression();
            }
            return null;
        }
    }
}
