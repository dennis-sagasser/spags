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
        private void AddToScope(LocalVariable variable)
        {
            variable.OwnerScope = CurrentBlock;
            variable.OwnerFunction = CurrentFunction;
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
                Statement stmt = AdvanceStatement(true);
                stmt.ParentCodeUnit = newBlock;
                newBlock.ChildStatements.Add(stmt);
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
                    ret.Value.ParentCodeUnit = ret;
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
                    Statement.If conditional = new Statement.If(ifThisIsTrue, thenDoThis, elseDoThis);
                    ifThisIsTrue.ParentCodeUnit = conditional;
                    thenDoThis.ParentCodeUnit = conditional;
                    if (elseDoThis != null)
                    {
                        elseDoThis.ParentCodeUnit = conditional;
                    }
                    return conditional;
                case TokenType.While:
                    AdvanceToken(/* TokenType.While */);
                    AdvanceToken(TokenType.LeftParenthesis);
                    Expression whileThisIsTrue = AdvanceExpression();
                    AdvanceToken(TokenType.RightParenthesis);
                    Statement keepDoingThis = AdvanceStatement(false);
                    Statement.While loop = new Statement.While(whileThisIsTrue, keepDoingThis);
                    whileThisIsTrue.ParentCodeUnit = loop;
                    keepDoingThis.ParentCodeUnit = loop;
                    return loop;
                case TokenType.KnownWord:
                    Token.KnownWord knownToken = (Token.KnownWord)token;
                    switch (knownToken.NameHolder.NameHolderType)
                    {
                        case NameHolderType.Struct:
                            AdvanceToken(/* Token.KnownWord */);
                            Token nextToken = token;
                            Token[] queue = preprocInjectedTokens.ToArray();
                            preprocInjectedTokens.Clear();
                            preprocInjectedTokens.Enqueue(knownToken);
                            preprocInjectedTokens.Enqueue(nextToken);
                            foreach (Token t in queue)
                            {
                                preprocInjectedTokens.Enqueue(t);
                            }
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
                            Expression.Call callExpr = (Expression.Call)left;
                            Statement.Call callStmt = new Statement.Call(callExpr);
                            callExpr.ParentCodeUnit = callStmt;
                            return callStmt;
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
                    Statement.Assign assign = new Statement.Assign(left, right, assignToken.Type);
                    left.ParentCodeUnit = assign;
                    right.ParentCodeUnit = assign;
                    return assign;

                case TokenType.Increment:
                case TokenType.Decrement:
                    Token crementToken = token;
                    AdvanceToken();
                    AdvanceToken(TokenType.Semicolon);
                    Statement.Assign crement = new Statement.Assign(left, null, crementToken.Type);
                    left.ParentCodeUnit = crement;
                    return crement;

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
                        variableType = new ValueType.Array(valueType, null);
                    }
                    else
                    {
                        variableType = new ValueType.Array(valueType, AdvanceExpression());
                        AdvanceToken(TokenType.RightSquareBracket);
                    }
                    if (valueType is ValueType.Struct) ((ValueType.Struct)valueType).InstantiatedArray = true;
                }
                else
                {
                    variableType = valueType;
                    if (valueType is ValueType.Struct) ((ValueType.Struct)valueType).Instantiated = true;
                }
                LocalVariable newVariable = new LocalVariable(name, variableType, AdvancePossibleAssignment());
                if (newVariable.InitialValue != null)
                {
                    newVariable.InitialValue.ParentCodeUnit = vars;
                }
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
