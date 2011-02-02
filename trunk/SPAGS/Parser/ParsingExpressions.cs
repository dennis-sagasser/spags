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
                        case NameHolderType.Struct:
                            return new Expression.StructType((ValueType.Struct)nameHolder);
                        case NameHolderType.Variable:
                            return new Expression.Variable((Variable)nameHolder);
                        case NameHolderType.EnumValue:
                            return new Expression.EnumValue((EnumValue)nameHolder);
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
                        if (elementType is ValueType.Struct) ((ValueType.Struct)elementType).InstantiatedArray = true;
                        AdvanceToken(TokenType.LeftSquareBracket);
                        Expression length = AdvanceExpression();
                        AdvanceToken(TokenType.RightSquareBracket);
                        Expression.AllocateArray allocArray = new Expression.AllocateArray(elementType, length);
                        length.ParentCodeUnit = allocArray;
                        return allocArray;
                    }


                case TokenType.LogicalNot:
                    Expression.UnaryOperator not = new Expression.UnaryOperator(t, RightToLeft_RightSide(OpPrecedence.Prefix));
                    not.Operand.ParentCodeUnit = not;
                    return not;

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
                    Expression.UnaryOperator unm = new Expression.UnaryOperator(t, operand);
                    unm.Operand.ParentCodeUnit = unm;
                    return unm;

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
                    Expression.Call call = new Expression.Call(left, paramValues);
                    left.ParentCodeUnit = call;
                    foreach (Expression expr in paramValues)
                    {
                        expr.ParentCodeUnit = call;
                    }
                    return call;

                case TokenType.DotWord:
                    Token.DotWord dotWord = (Token.DotWord)t;
                    if (left.Type == ExpressionType.StructType)
                    {
                        // static method or attribute
                        ValueType.Struct structType = ((Expression.StructType)left).TheStructType;
                        StructMember staticMember;
                        if (!structType.Members.TryGetValue2<StructMember>(dotWord.TheWord, out staticMember))
                        {
                            throw new Exception("static method or attribute not found: " + structType.Name + "." + dotWord.TheWord);
                        }
                        switch (staticMember.MemberType)
                        {
                            case StructMemberType.Attribute:
                                StructMember.Attribute attr = (StructMember.Attribute)staticMember;
                                if (!attr.IsStatic)
                                {
                                    throw new Exception(structType.Name + "." + attr.Name + " is not a static attribute");
                                }
                                return new Expression.Attribute(structType, attr, null);
                            case StructMemberType.Field:
                                throw new Exception(structType.Name + "." + staticMember.Name + " is a non-static field");
                            case StructMemberType.Method:
                                StructMember.Method method = (StructMember.Method)staticMember;
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
                    ValueType.Struct asStruct = leftType as ValueType.Struct;
                    if (asStruct == null)
                    {
                        throw new Exception(leftType.Name + " type does not have a member ." + dotWord.TheWord);
                    }
                    else
                    {
                        StructMember member;
                        if (!asStruct.Members.TryGetValue2<StructMember>(dotWord.TheWord, out member))
                        {
                            throw new Exception("method/field/attribute not found: " + asStruct.Name + "." + dotWord.TheWord);
                        }
                        switch (member.MemberType)
                        {
                            case StructMemberType.Attribute:
                                StructMember.Attribute attr = (StructMember.Attribute)member;
                                if (attr.IsStatic)
                                {
                                    return new Expression.Attribute(asStruct, attr, null);
                                }
                                else
                                {
                                    Expression.Attribute attrExpr = new Expression.Attribute(asStruct, attr, left);
                                    left.ParentCodeUnit = attrExpr;
                                    return attrExpr;
                                }
                            case StructMemberType.Field:
                                StructMember.Field field = (StructMember.Field)member;
                                Expression.Field fieldExpr = new Expression.Field(asStruct, field, left);
                                left.ParentCodeUnit = fieldExpr;
                                return fieldExpr;
                            case StructMemberType.Method:
                                StructMember.Method method = (StructMember.Method)member;
                                if (method.IsStatic)
                                {
                                    return new Expression.Method(asStruct, method, null);
                                }
                                else
                                {
                                    Expression.Method methodExpr = new Expression.Method(asStruct, method, left);
                                    left.ParentCodeUnit = methodExpr;
                                    return methodExpr;
                                }
                            default:
                                throw new Exception("internal error - unrecognised struct member: " + member);
                        }
                    }

                case TokenType.LeftSquareBracket:
                    Expression index = AdvanceExpression();
                    AdvanceToken(TokenType.RightSquareBracket);
                    Expression.ArrayIndex arrayIndex = new Expression.ArrayIndex(left, index);
                    left.ParentCodeUnit = arrayIndex;
                    index.ParentCodeUnit = arrayIndex;
                    return arrayIndex;

                case TokenType.Multiply:
                case TokenType.Divide:
                case TokenType.Modulus:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.MultiplyDivideModulus));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.Add:
                case TokenType.Subtract:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.AddSubtract));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.BitwiseLeftShift:
                case TokenType.BitwiseRightShift:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseShift));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.IsGreaterThan:
                case TokenType.IsGreaterThanOrEqualTo:
                case TokenType.IsLessThan:
                case TokenType.IsLessThanOrEqualTo:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LessGreaterLteGte));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.IsEqualTo:
                case TokenType.IsNotEqualTo:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.EqualNotEqual));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.BitwiseAnd:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseAnd));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.BitwiseXor:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseXor));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.BitwiseOr:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.BitwiseOr));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.LogicalAnd:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LogicalAnd));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

                case TokenType.LogicalOr:
                    {
                        Expression.BinaryOperator binop = new Expression.BinaryOperator(t, left, LeftToRight_RightSide(OpPrecedence.LogicalOr));
                        binop.Left.ParentCodeUnit = binop;
                        binop.Right.ParentCodeUnit = binop;
                        return binop;
                    }

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
