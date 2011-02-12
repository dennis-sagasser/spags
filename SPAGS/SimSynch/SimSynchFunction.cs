using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS.SimSynch
{
    public class SimSynchFunction : INameHolder
    {
        private SimSynchFunction(Function func)
        {
            OriginalFunction = func;
            Chunks = new List<SimSynchChunk>();
        }

        public string Name
        {
            get { return OriginalFunction.Name; }
        }

        public readonly Function OriginalFunction;

        private SimSynchChunk lastChunk
        {
            get { return Chunks[Chunks.Count - 1]; }
        }

        private Statement backStatement()
        {
            return backStatement(1);
        }
        private Statement backStatement(int i)
        {
            SimSynchChunk last = lastChunk;
            if (last.ChildStatements.Count < i)
            {
                return null;
            }
            return last.ChildStatements[last.ChildStatements.Count - i];
        }

        private void AddCall(Function func, List<Expression> parameters, List<Expression> varargs)
        {
            SimSynchExpression.StackAugmentedCall call = new SimSynchExpression.StackAugmentedCall();
            call.CallingFunction = func;

            foreach (Expression parameter in parameters)
            {
                AddExpression(parameter);
            }
            call.StackParameterCount = parameters.Count;
            if (varargs != null && varargs.Count > 0)
            {
                foreach (Expression vararg in varargs)
                {
                    AddExpression(vararg);
                }
                call.StackVarargCount = varargs.Count;
            }
            while (backStatement(1) is SimSynchStatement.Push)
            {
                if (call.StackVarargCount > 0)
                {
                    call.StackVarargCount--;
                    SimSynchStatement.Push push = (SimSynchStatement.Push)backStatement(1);
                    lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count-1);
                    call.DirectVarargs.Insert(0, push.Value);
                }
                else if (call.StackParameterCount > 0)
                {
                    call.StackParameterCount--;
                    SimSynchStatement.Push push = (SimSynchStatement.Push)backStatement(1);
                    lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count - 1);
                    call.DirectParameters.Insert(0, push.Value);
                }
                else
                {
                    break;
                }
            }
            SimSynchChunk retChunk = new SimSynchChunk(this, new NameDictionary());
            AddStatement(new SimSynchStatement.CallSuspend(call, retChunk));
            Chunks.Add(retChunk);
        }

        private void AddExpression(Expression expr)
        {
            Function callFunc;
            List<Expression> callParams, callVarargs;
            if (expr.TryGetSimpleCall(out callFunc, out callParams, out callVarargs))
            {
                AddCall(callFunc, callParams, callVarargs);
                return;
            }
            switch (expr.Type)
            {
                case ExpressionType.AllocateArray:
                case ExpressionType.AllocStringBuffer:
                case ExpressionType.AllocStruct:
                case ExpressionType.CharLiteral:
                case ExpressionType.Constant:
                case ExpressionType.EnumValue:
                case ExpressionType.FloatLiteral:
                case ExpressionType.IntegerLiteral:
                case ExpressionType.Null:
                case ExpressionType.StringLiteral:
                case ExpressionType.Variable:
                    AddStatement(new SimSynchStatement.Push(expr));
                    break;
                case ExpressionType.ArrayIndex:
                    Expression.ArrayIndex indexer = (Expression.ArrayIndex)expr;
                    AddExpression(indexer.Target);
                    AddExpression(indexer.Index);
                    SimSynchStatement.Push backTarget = backStatement(2) as SimSynchStatement.Push;
                    SimSynchStatement.Push backIndex = backStatement(1) as SimSynchStatement.Push;
                    if (backTarget != null && backIndex != null)
                    {
                        lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count - 1);
                        backTarget.Value = new Expression.ArrayIndex(backTarget.Value, backIndex.Value);
                    }
                    else
                    {
                        lastChunk.ChildStatements.Add(new SimSynchStatement.StackArrayIndex());
                    }
                    break;
                case ExpressionType.BinaryOperator:
                    Expression.BinaryOperator binop = (Expression.BinaryOperator)expr;
                    switch (binop.Token.Type)
                    {
                        case TokenType.LogicalAnd:
                            SimSynchChunk preAndChunk = lastChunk;
                            SimSynchChunk firstAndChunk = new SimSynchChunk(this, new NameDictionary());
                            SimSynchChunk firstPopChunk = new SimSynchChunk(this, new NameDictionary());
                            SimSynchChunk afterAndChunk = new SimSynchChunk(this, new NameDictionary());
                            Chunks.Add(firstAndChunk);
                            AddExpression(binop.Left);
                            SimSynchChunk lastAndChunk = lastChunk;
                            Statement.If andTest = new Statement.If(
                                new SimSynchExpression.Peek(ValueType.Int),
                                new SimSynchStatement.Suspend(firstPopChunk),
                                new SimSynchStatement.Suspend(afterAndChunk));
                            lastAndChunk.ChildStatements.Add(andTest);
                            Chunks.Add(firstPopChunk);
                            firstPopChunk.ChildStatements.Add(new SimSynchStatement.Pop());
                            AddExpression(binop.Right);
                            lastChunk.ChildStatements.Add(new SimSynchStatement.Suspend(afterAndChunk));
                            SimSynchChunk lastPopChunk = lastChunk;
                            if (firstAndChunk == lastAndChunk && firstPopChunk == lastPopChunk)
                            {
                                Chunks.Remove(firstAndChunk);
                                Chunks.Remove(firstPopChunk);
                                goto default;
                            }
                            else
                            {
                                Chunks.Remove(firstAndChunk);
                                preAndChunk.ChildStatements.AddRange(firstAndChunk.ChildStatements);
                                Chunks.Add(afterAndChunk);
                            }
                            break;
                        case TokenType.LogicalOr:
                            SimSynchChunk preOrChunk = lastChunk;
                            SimSynchChunk firstOrChunk = new SimSynchChunk(this, new NameDictionary());
                            SimSynchChunk firstOrPopChunk = new SimSynchChunk(this, new NameDictionary());
                            SimSynchChunk afterOrChunk = new SimSynchChunk(this, new NameDictionary());
                            Chunks.Add(firstOrChunk);
                            AddExpression(binop.Left);
                            SimSynchChunk lastOrChunk = lastChunk;
                            Statement.If orTest = new Statement.If(
                                new SimSynchExpression.Peek(ValueType.Int),
                                new SimSynchStatement.Suspend(afterOrChunk),
                                new SimSynchStatement.Suspend(firstOrPopChunk));
                            lastChunk.ChildStatements.Add(orTest);
                            Chunks.Add(firstOrPopChunk);
                            firstOrPopChunk.ChildStatements.Add(new SimSynchStatement.Pop());
                            AddExpression(binop.Right);
                            lastChunk.ChildStatements.Add(new SimSynchStatement.Suspend(afterOrChunk));
                            SimSynchChunk lastOrPopChunk = lastChunk;
                            if (firstOrChunk == lastOrChunk && firstOrPopChunk == lastOrPopChunk)
                            {
                                Chunks.Remove(firstOrChunk);
                                Chunks.Remove(firstOrPopChunk);
                                goto default;
                            }
                            else
                            {
                                Chunks.Remove(firstOrChunk);
                                preOrChunk.ChildStatements.AddRange(firstOrChunk.ChildStatements);
                                Chunks.Add(afterOrChunk);
                            }
                            break;
                        default:
                            AddExpression(binop.Left);
                            AddExpression(binop.Right);
                            SimSynchStatement.Push backLeft = backStatement(2) as SimSynchStatement.Push;
                            SimSynchStatement.Push backRight = backStatement(1) as SimSynchStatement.Push;
                            if (backLeft != null && backRight != null)
                            {
                                lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count - 1);
                                backLeft.Value = new Expression.BinaryOperator(binop.Token, backLeft.Value, backRight.Value);
                            }
                            else
                            {
                                lastChunk.ChildStatements.Add(
                                    new SimSynchStatement.StackBinOp(
                                        binop.Token, 
                                        binop.Left.GetValueType(),
                                        binop.Right.GetValueType()));
                            }
                            break;
                    }
                    break;
                case ExpressionType.UnaryOperator:
                    Expression.UnaryOperator unop = (Expression.UnaryOperator)expr;
                    AddExpression(unop.Operand);
                    SimSynchStatement.Push operandPush = backStatement(1) as SimSynchStatement.Push;
                    if (operandPush == null)
                    {
                        AddStatement(new SimSynchStatement.StackUnOp(unop.Token, unop.Operand.GetValueType()));
                    }
                    else
                    {
                        operandPush.Value = new Expression.UnaryOperator(unop.Token, operandPush.Value);
                    }
                    break;
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    AddExpression(field.Target);
                    SimSynchStatement.Push structPush = backStatement(1) as SimSynchStatement.Push;
                    if (structPush == null)
                    {
                        lastChunk.ChildStatements.Add(new SimSynchStatement.StackFieldAccess(field.TheField));
                    }
                    else
                    {
                        structPush.Value = new Expression.Field(field.TheStructType, field.TheField, structPush.Value);
                    }
                    break;
                default:
                    throw new Exception("Unhandled Expression Type: " + expr.Type);
            }
        }

        private Expression Pop(ValueType valueType)
        {
            Statement stmt = backStatement(1);
            if (stmt is SimSynchStatement.Push)
            {
                lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count-1);
                return ((SimSynchStatement.Push)stmt).Value;
            }
            if (stmt is SimSynchStatement.StackUnOp)
            {
                SimSynchStatement.StackUnOp unop = (SimSynchStatement.StackUnOp)stmt;
                lastChunk.ChildStatements.RemoveAt(lastChunk.ChildStatements.Count-1);
                return new Expression.UnaryOperator(unop.OpToken, Pop(unop.OperandType));
            }
            if (stmt is SimSynchStatement.StackBinOp)
            {
                SimSynchStatement.StackBinOp binop = (SimSynchStatement.StackBinOp)stmt;
                SimSynchStatement.Push rightPush = backStatement(2) as SimSynchStatement.Push;
                if (rightPush != null)
                {
                    lastChunk.ChildStatements.RemoveRange(lastChunk.ChildStatements.Count-2,2);
                    return new Expression.BinaryOperator(binop.OpToken, Pop(binop.LeftType), rightPush.Value);
                }
            }
            return new SimSynchExpression.Pop(valueType);
        }

        private void AddStatement(Statement stmt)
        {
            Function callFunc;
            List<Expression> callParams, callVarargs;
            if (stmt.TryGetSimpleCall(out callFunc, out callParams, out callVarargs))
            {
                AddCall(callFunc, callParams, callVarargs);
                lastChunk.ChildStatements.Add(new SimSynchStatement.Pop());
                return;
            }
            switch (stmt.Type)
            {
                case StatementType.VariableDeclaration:
                    Statement.VariableDeclaration vardef = (Statement.VariableDeclaration)stmt;
                    foreach (Variable variable in vardef.Variables)
                    {
                        if (variable.InitialValue == null)
                        {
                            AddExpression(variable.Type.CreateDefaultValueExpression());
                        }
                        else
                        {
                            AddExpression(variable.InitialValue);
                        }
                        lastChunk.ChildStatements.Add(
                            new Statement.Assign(
                                new Expression.Variable(variable),
                                Pop(variable.Type),
                                TokenType.Assign));
                    }
                    break;
                case StatementType.Assign:
                    Statement.Assign assign = (Statement.Assign)stmt;
                    Expression assignValue = assign.SimpleAssignValue();
                    AddExpression(assignValue);
                    lastChunk.ChildStatements.Add(
                        new Statement.Assign(assign.Target,
                            Pop(assignValue.GetValueType()),
                            TokenType.Assign));
                    break;
                case StatementType.Block:
                    foreach (Statement child in ((Statement.Block)stmt).ChildStatements)
                    {
                        AddStatement(child);
                    }
                    break;
                case StatementType.While:
                    SimSynchChunk preLoopChunk = lastChunk;
                    SimSynchChunk firstLoopChunk = new SimSynchChunk(this, new NameDictionary());
                    Chunks.Add(firstLoopChunk);
                    Statement.While loop = (Statement.While)stmt;
                    SimSynchChunk endLoopChunk = new SimSynchChunk(this, new NameDictionary());
                    AddExpression(loop.WhileThisIsTrue);
                    lastChunk.ChildStatements.Add(
                        new Statement.If(
                            Expression.LogicalNegation(Pop(ValueType.Int)),
                            new SimSynchStatement.Suspend(endLoopChunk),
                            null));
                    AddStatement(loop.KeepDoingThis);
                    if (lastChunk == firstLoopChunk)
                    {
                        // throw away
                        Chunks.Remove(firstLoopChunk);
                        AddExpression(loop.WhileThisIsTrue);
                        SimSynchChunk tempChunk = new SimSynchChunk(this, new NameDictionary());
                        Chunks.Add(tempChunk);
                        AddStatement(loop.KeepDoingThis);
                        Chunks.Remove(tempChunk);
                        Statement.While newLoop = new Statement.While(Pop(ValueType.Int), tempChunk);
                        preLoopChunk.ChildStatements.Add(newLoop);
                    }
                    else
                    {
                        lastChunk.ChildStatements.Add(new SimSynchStatement.Suspend(firstLoopChunk));
                        Chunks.Add(endLoopChunk);
                        preLoopChunk.ChildStatements.Add(new SimSynchStatement.Suspend(firstLoopChunk));
                    }
                    break;
                case StatementType.If:

                    Statement.If conditional = (Statement.If)stmt;
                    AddExpression(conditional.IfThisIsTrue);

                    Statement.If newConditional = new Statement.If(
                        Pop(ValueType.Int),
                        new Statement.Block(new NameDictionary()),
                        null);
                    SimSynchChunk originalChunk = lastChunk;
                    originalChunk.ChildStatements.Add(newConditional);

                    SimSynchChunk firstThenChunk = new SimSynchChunk(this, new NameDictionary());
                    Chunks.Add(firstThenChunk);
                    AddStatement(conditional.ThenDoThis);
                    SimSynchChunk lastThenChunk = lastChunk;
                    Chunks.Remove(firstThenChunk);
                    newConditional.ThenDoThis = firstThenChunk;

                    SimSynchChunk firstElseChunk, lastElseChunk;

                    if (conditional.ElseDoThis == null)
                    {
                        firstElseChunk = null;
                        lastElseChunk = null;
                    }
                    else
                    {
                        firstElseChunk = new SimSynchChunk(this, new NameDictionary());
                        Chunks.Add(firstElseChunk);
                        AddStatement(conditional.ElseDoThis);
                        lastElseChunk = lastChunk;
                        Chunks.Remove(firstElseChunk);
                        newConditional.ElseDoThis = firstElseChunk;
                    }

                    if (firstThenChunk != lastThenChunk || firstElseChunk != lastElseChunk)
                    {
                        SimSynchChunk endChunk = new SimSynchChunk(this, new NameDictionary());

                        /*
                        if (firstElseChunk == null)
                        {
                            originalChunk.ChildStatements.Add(new SimSynchStatement.Suspend(endChunk));
                        }
                         */
                        if (!lastThenChunk.Returns())
                        {
                            lastThenChunk.ChildStatements.Add(new SimSynchStatement.Suspend(endChunk));
                        }
                        if (lastElseChunk == null)
                        {
                            lastElseChunk = new SimSynchChunk(this, new NameDictionary());
                            newConditional.ElseDoThis = lastElseChunk;
                        }
                        if (!lastElseChunk.Returns())
                        {
                            lastElseChunk.ChildStatements.Add(new SimSynchStatement.Suspend(endChunk));
                        }

                        Chunks.Add(endChunk);
                    }

                    Statement.Block elseBlock = newConditional.ElseDoThis as Statement.Block;
                    if (elseBlock != null && elseBlock.ChildStatements.Count == 1
                        && elseBlock.ChildStatements[0] is Statement.If)
                    {
                        newConditional.ElseDoThis = elseBlock.ChildStatements[0];
                    }
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    if (ret.Value == null)
                    {
                        if (OriginalFunction.Signature.ReturnType.Category == ValueTypeCategory.Void)
                        {
                            AddStatement(new SimSynchStatement.Finish(null));
                        }
                        else
                        {
                            AddStatement(new SimSynchStatement.Finish(OriginalFunction.Signature.ReturnType.CreateDefaultValueExpression()));
                        }
                    }
                    else
                    {
                        AddExpression(ret.Value);
                        AddStatement(new SimSynchStatement.Finish(Pop(ret.Value.GetValueType())));
                    }
                    break;
                default:
                    lastChunk.ChildStatements.Add(stmt);
                    break;
            }
        }

        public static SimSynchFunction Create(Function func)
        {
            SimSynchFunction ssf = new SimSynchFunction(func);

            SimSynchChunk mainChunk = new SimSynchChunk(ssf, func.Body.Scope);
            ssf.Chunks.Add(mainChunk);

            foreach (Statement stmt in func.Body.ChildStatements)
            {
                ssf.AddStatement(stmt);
            }

            if (!func.Body.Returns())
            {
                if (func.Signature.ReturnType.Category == ValueTypeCategory.Void)
                {
                    ssf.AddStatement(new Statement.Return(null));
                }
                else
                {
                    ssf.AddStatement(new Statement.Return(func.Signature.ReturnType.CreateDefaultValueExpression()));
                }
            }

            int i = 0;
            while (i < ssf.Chunks.Count)
            {
                SimSynchChunk chunk = ssf.Chunks[i];
                if (chunk.ChildStatements.Count == 1
                    && chunk.ChildStatements[0] is SimSynchStatement.Suspend)
                {
                    SimSynchStatement.Suspend suspend = (SimSynchStatement.Suspend)chunk.ChildStatements[0];
                    chunk.Redirect = suspend.NextChunk;
                    ssf.Chunks.RemoveAt(i);
                }
                else
                {
                    i++;
                }
            }

            return ssf;
        }

        public List<SimSynchChunk> Chunks;
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (StringWriter output = new StringWriter(sb))
            {
                WriteTo(output);
            }
            return sb.ToString();
        }
        public void WriteTo(TextWriter output)
        {
            output.Write("@simsynch " + OriginalFunction.Signature.ReturnType
                + " " + OriginalFunction.Name + "(");
            // TODO: parameters
            output.Write(") {");
            for (int i = 0; i < Chunks.Count; i++)
            {
                output.WriteLine();
                output.Write("\t@entrypoint_" + i + ": ");
                Chunks[i].WriteTo(output, 1);
            }
            output.WriteLine();
            output.Write("}");
        }

        public NameHolderType NameHolderType
        {
            get { return 0; }
        }
    }
}
