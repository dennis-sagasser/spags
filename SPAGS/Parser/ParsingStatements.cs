using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public partial class ScriptParser
    {
        private List<Statement.Block> FunctionContext = new List<Statement.Block>();
        private Statement.Block CurrentBlock;
        private void PushScope(Statement.Block block)
        {
            if (CurrentBlock != null) FunctionContext.Add(CurrentBlock);
            CurrentBlock = block;
        }
        private void PopScope()
        {
            int count = FunctionContext.Count;
            if (count == 0)
            {
                CurrentBlock = null;
            }
            else
            {
                CurrentBlock = FunctionContext[count - 1];
                FunctionContext.RemoveAt(count - 1);
            }
        }
        private void AddToScope(Variable variable)
        {
            CurrentBlock.Scope.Add(variable);
        }
        private Statement.Block AdvanceBlock()
        {
            return AdvanceBlock(new NameDictionary());
        }
        private Statement.Block AdvanceBlock(NameDictionary initialScope)
        {
            Statement.Block newBlock = new Statement.Block(initialScope);
            PushScope(newBlock);
            AdvanceToken(TokenType.LeftCurlyBrace);
            while (token.Type != TokenType.RightCurlyBrace)
            {
                newBlock.ChildStatements.Add(AdvanceStatement(true));
            }
            AdvanceToken(/* TokenType.RightCurlyBrace */);
            PopScope();
            return newBlock;
        }

        private Stack<Statement> injectedStatements = new Stack<Statement>();

        private Statement AdvanceStatement(bool defineVariablesOK)
        {
            if (injectedStatements.Count > 0) return injectedStatements.Pop();
            switch (token.Type)
            {
                case TokenType.EndOfInput:
                    throw new Exception("unterminated statement block");
                case TokenType.LeftCurlyBrace:
                    return AdvanceBlock();
                case TokenType.Return:
                    AdvanceToken(/* TokenType.Return */);
                    if (token.Type == TokenType.Semicolon)
                    {
                        AdvanceToken(/* TokenType.Semicolon */);
                        return new Statement.Return(null);
                    }
                    Statement.Return ret = new Statement.Return(AdvanceExpression());
                    AdvanceToken(TokenType.Semicolon);
                    return ret;
                case TokenType.If:
                    AdvanceToken(/* TokenType.If */);
                    AdvanceToken(TokenType.LeftParenthesis);
                    Expression ifThisIsTrue = AdvanceExpression();
                    AdvanceToken(TokenType.RightParenthesis);
                    Statement thenDoThis = AdvanceStatement(false);
                    Statement elseDoThis;
                    if (token.Type == TokenType.Else)
                    {
                        AdvanceToken(/* TokenType.Else */);
                        elseDoThis = AdvanceStatement(false);
                    }
                    else
                    {
                        elseDoThis = null;
                    }
                    return new Statement.If(ifThisIsTrue, thenDoThis, elseDoThis);
                case TokenType.While:
                    AdvanceToken(/* TokenType.While */);
                    AdvanceToken(TokenType.LeftParenthesis);
                    Expression whileThisIsTrue = AdvanceExpression();
                    AdvanceToken(TokenType.RightParenthesis);
                    Statement doThis = AdvanceStatement(false);
                    return new Statement.While(whileThisIsTrue, doThis);
                case TokenType.KnownWord:
                    Token.KnownWord knownToken = (Token.KnownWord)token;
                    switch (knownToken.NameHolder.NameHolderType)
                    {
                        case NameHolderType.StructType:
                            AdvanceToken(/* Token.KnownWord */);
                            Token nextToken = token;
                            preprocInjectedTokens.Enqueue(knownToken);
                            preprocInjectedTokens.Enqueue(nextToken);
                            AdvanceToken();
                            if (nextToken.Type == TokenType.DotWord)
                            {
                                return AdvanceExpressionBasedStatement();
                            }
                            goto case NameHolderType.BasicType;
                        case NameHolderType.BasicType:
                        case NameHolderType.EnumType:
                            return AdvanceVariableDeclarationStatement();
                        case NameHolderType.Variable:
                        case NameHolderType.Function:
                            return AdvanceExpressionBasedStatement();
                        default:
                            throw new Exception("expecting statement, got: " + knownToken);
                    }
                default:
                    if (token.Type == TokenType.KnownWord)
                    {

                    }
                    Expression expression = AdvanceExpression();
                    switch (token.Type)
                    {
                        case TokenType.Semicolon:
                            // must be a function call
                            break;
                        case TokenType.Assign:
                            break;
                        case TokenType.Increment:
                            break;
                        case TokenType.Decrement:
                            break;
                        case TokenType.AddAssign:
                            break;
                        case TokenType.SubtractAssign:
                            break;
                    }
                    throw new Exception("illegal statement");
            }
        }
        Statement AdvanceExpressionBasedStatement()
        {
            Expression left = AdvanceExpression();
            switch (token.Type)
            {
                case TokenType.Semicolon:
                    switch (left.Type)
                    {
                        case ExpressionType.Call:
                            AdvanceToken(/* TokenType.Semicolon */);
                            Expression.Call call = (Expression.Call)left;
                            return new Statement.Call(call);
                        default:
                            throw new Exception("invalid statement: " + left);
                    }

                case TokenType.Assign:
                case TokenType.AddAssign:
                case TokenType.SubtractAssign:
                    Token assignToken = token;
                    AdvanceToken();
                    Expression right = AdvanceExpression();
                    AdvanceToken(TokenType.Semicolon);
                    return new Statement.Assign(left, right, assignToken.Type);

                case TokenType.Increment:
                case TokenType.Decrement:
                    Token crementToken = token;
                    AdvanceToken();
                    AdvanceToken(TokenType.Semicolon);
                    return new Statement.Assign(left, null, token.Type);

                default:
                    throw new Exception("unexpected " + token);
            }
        }
        Statement AdvanceVariableDeclarationStatement()
        {
            ValueType valueType = AdvanceValueType();
            Statement.VariableDeclaration vars = new Statement.VariableDeclaration();
            for (; ; )
            {
                string name = AdvanceUnusedName();
                ValueType variableType;
                if (token.Type == TokenType.LeftSquareBracket)
                {
                    AdvanceToken(/* TokenType.LeftSquareBracket */);
                    if (token.Type == TokenType.RightSquareBracket)
                    {
                        AdvanceToken(/* TokenType.RightSquareBracket */);
                        variableType = new ArrayType(valueType, null);
                    }
                    else
                    {
                        variableType = new ArrayType(valueType, AdvanceExpression());
                        AdvanceToken(TokenType.RightSquareBracket);
                    }
                    if (valueType is StructType) ((StructType)valueType).InstantiatedArray = true;
                }
                else
                {
                    variableType = valueType;
                    if (valueType is StructType) ((StructType)valueType).Instantiated = true;
                }
                Variable newVariable = new Variable(name, variableType, AdvancePossibleAssignment());
                AddToScope(newVariable);
                vars.Variables.Add(newVariable);
                if (token.Type == TokenType.Comma)
                {
                    AdvanceToken(/* TokenType.Comma */);
                    continue;
                }
                else
                {
                    AdvanceToken(TokenType.Semicolon);
                    return vars;
                }
            }
        }
    }
}
