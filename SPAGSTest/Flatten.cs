using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS
{
    public enum FlatStatementType
    {
        EntryPoint, SetReturnValue, Suspend, StackBinOp, StackArrayIndex, IfBranch, Push, Begin, AllocateArray, Pop
    }
    public abstract class FlatStatement : Statement
    {
        private FlatStatement() : base(StatementType.Custom)
        {
        }
        public override void WriteTo(System.IO.TextWriter output, int indent)
        {
            output.Write("<<FlatStatement>>");
        }
        public class EntryPoint : FlatStatement
        {
            public int Number;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.EntryPoint; }
            }
        }
        public abstract FlatStatementType FlatStatementType
        {
            get;
        }
        public class SetReturnValue : FlatStatement
        {
            public Expression Value;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.SetReturnValue; }
            }
        }
        public class Suspend : FlatStatement
        {
            public Suspend()
            {
            }
            public Suspend(EntryPoint entryPoint)
            {
                EntryPoint = entryPoint;
            }
            public EntryPoint EntryPoint;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Suspend; }
            }
        }
        public class StackBinOp : FlatStatement
        {
            public Token Token;
            public StackBinOp(Token token)
            {
                Token = token;
            }
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.StackBinOp; }
            }
        }
        public class StackArrayIndex : FlatStatement
        {
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.StackArrayIndex; }
            }
        }
        public class IfBranch : FlatStatement
        {
            public EntryPoint TrueBranch;
            public EntryPoint FalseBranch;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.IfBranch; }
            }
        }
        public class Push : FlatStatement
        {
            public Push(params Expression[] values)
                : base()
            {
                Values.AddRange(values);
            }
            public List<Expression> Values = new List<Expression>();
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Push; }
            }
        }
        public class Begin : FlatStatement
        {
            public Begin(bool ignoreReturnValue)
            {
                IgnoreReturnValue = ignoreReturnValue;
            }
            public Function Function;
            public int StackParams;
            public List<Expression> DirectParams;
            public bool IgnoreReturnValue;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Begin; }
            }
        }
        public class AllocateArray : FlatStatement
        {
            public AllocateArray(ValueType elementType)
            {
                ElementType = elementType;
            }
            public ValueType ElementType;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.AllocateArray; }
            }
        }
        public class Pop : FlatStatement
        {
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Pop; }
            }
        }
    }
    public enum FlatExpressionType
    {
        StackPop
    }
    public abstract class FlatExpression : Expression
    {
        private FlatExpression()
            : base(ExpressionType.Custom)
        {
        }
        public override void WriteTo(System.IO.TextWriter output)
        {
            output.Write("<<FlatExpression>>");
        }
        public abstract FlatExpressionType FlatExpressionType
        {
            get;
        }
        public class StackPop : FlatExpression
        {
            private ValueType valueType;
            public StackPop(ValueType valueType)
            {
                this.valueType = valueType;
            }
            public override bool Equals(Expression ex)
            {
                return ex is StackPop;
            }
            public override ValueType GetValueType()
            {
                return valueType;
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override FlatExpressionType FlatExpressionType
            {
                get { return FlatExpressionType.StackPop; }
            }
        }
    }
    public class Flattener
    {
        Function function;
        public List<Statement> output = new List<Statement>();
        public Stack<Expression> stackPushStack = new Stack<Expression>();
        public Flattener(Function function)
        {
            this.function = function;
        }
        public void Go()
        {
            output.Add(new FlatStatement.EntryPoint());
            Statement(function.Body);
            Statement prev = null;
            int pointNum = 0;
            foreach (Statement stmt in output)
            {
                if (stmt is FlatStatement.EntryPoint)
                {
                    FlatStatement.EntryPoint entryPoint = (FlatStatement.EntryPoint)stmt;
                    if (prev is FlatStatement.EntryPoint)
                    {
                        entryPoint.Number = ((FlatStatement.EntryPoint)prev).Number;
                    }
                    else
                    {
                        entryPoint.Number = pointNum++;
                    }
                }
                prev = stmt;
            }
        }
        void PushExpression(Expression expr)
        {
            stackPushStack.Push(expr);
        }
        Expression PopExpression()
        {
            return stackPushStack.Pop();
        }
        void FlushStackPushStack()
        {
            if (stackPushStack.Count > 0)
            {
                output.Add(new FlatStatement.Push(stackPushStack.ToArray()));
                stackPushStack.Clear();
            }
        }
        void Statement(Statement stmt)
        {
            Expression.Call call;
            if (stmt.TryGetSimpleCall(out call))
            {
                Call(((Expression.Function)call.CallingOn).TheFunction, call.Parameters, true);
                return;
            }
            switch (stmt.Type)
            {
                case StatementType.Assign:
                    Statement.Assign assign = (Statement.Assign)stmt;
                    Expression v = assign.SimpleAssignValue();
                    Expression(v);
                    output.Add(new Statement.Assign(assign.Target, Pop(assign.Target.GetValueType()), TokenType.Assign));
                    break;
                case StatementType.Block:
                    Statement.Block block = (Statement.Block)stmt;
                    foreach (Statement childStmt in block.ChildStatements)
                    {
                        Statement(childStmt);
                    }
                    break;
                case StatementType.If:
                    Statement.If conditional = (Statement.If)stmt;
                    Expression(conditional.IfThisIsTrue);
                    FlatStatement.EntryPoint thenPoint = new FlatStatement.EntryPoint();
                    FlatStatement.Suspend thenSuspend = new FlatStatement.Suspend(thenPoint);
                    FlatStatement.Suspend elseSuspend = new FlatStatement.Suspend();
                    output.Add(new Statement.If(Pop(conditional.IfThisIsTrue.GetValueType()), thenSuspend, elseSuspend));
                    output.Add(thenPoint);
                    Statement(conditional.ThenDoThis);
                    FlatStatement.EntryPoint endPoint = new FlatStatement.EntryPoint();
                    output.Add(new FlatStatement.Suspend(endPoint));
                    if (conditional.ElseDoThis == null)
                    {
                        elseSuspend.EntryPoint = endPoint;
                    }
                    else
                    {
                        FlatStatement.EntryPoint elsePoint = new FlatStatement.EntryPoint();
                        elseSuspend.EntryPoint = elsePoint;
                        output.Add(elsePoint);
                        Statement(conditional.ElseDoThis);
                        output.Add(new FlatStatement.Suspend(endPoint));
                    }
                    output.Add(endPoint);
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    if (ret.Value != null)
                    {
                        Expression(ret.Value);
                        FlatStatement.SetReturnValue setReturnValue = new FlatStatement.SetReturnValue();
                        setReturnValue.Value = Pop(ret.Value.GetValueType());
                        output.Add(setReturnValue);
                    }
                    output.Add(new Statement.Return(new Expression.IntegerLiteral(-1)));
                    break;
                case StatementType.VariableDeclaration:
                    Statement.VariableDeclaration varDef = (Statement.VariableDeclaration)stmt;
                    foreach (Variable var in varDef.Variables)
                    {
                        if (var.InitialValue == null)
                        {
                            Expression(var.Type.CreateDefaultValueExpression());
                        }
                        else
                        {
                            Expression(var.InitialValue);
                        }
                        output.Add(
                            new Statement.Assign(
                                new Expression.Variable(var),
                                Pop(var.Type),
                                TokenType.Assign));
                    }
                    break;
                case StatementType.While:
                    Statement.While loop = (Statement.While)stmt;
                    FlatStatement.EntryPoint startPoint = new FlatStatement.EntryPoint();
                    output.Add(new FlatStatement.Suspend(startPoint));
                    output.Add(startPoint);
                    Expression(loop.WhileThisIsTrue);
                    FlatStatement.EntryPoint loopBodyPoint = new FlatStatement.EntryPoint();
                    FlatStatement.EntryPoint endLoopPoint = new FlatStatement.EntryPoint();
                    output.Add(new Statement.If(Pop(loop.WhileThisIsTrue.GetValueType()), new FlatStatement.Suspend(loopBodyPoint), new FlatStatement.Suspend(endLoopPoint)));
                    output.Add(loopBodyPoint);
                    Statement(loop.KeepDoingThis);
                    output.Add(new FlatStatement.Suspend(startPoint));
                    output.Add(endLoopPoint);
                    break;
            }
        }
        void Expression(Expression expr)
        {
            Expression(expr, false);
        }
        Expression Pop(ValueType vtype)
        {
            if (stackPushStack.Count > 0) return PopExpression();
            return new FlatExpression.StackPop(vtype);
        }
        void Call(Function func, List<Expression> parameters, bool ignoreReturnValue)
        {
            FlushStackPushStack();
            foreach (Expression param in parameters)
            {
                Expression(param);
            }
            FlatStatement.Begin begin = new FlatStatement.Begin(ignoreReturnValue);
            begin.Function = func;
            begin.StackParams = parameters.Count - stackPushStack.Count;
            begin.DirectParams = new List<Expression>();
            for (int i = 0; i < parameters.Count - begin.StackParams; i++)
            {
                begin.DirectParams.Insert(0, PopExpression());
            }
            if (ignoreReturnValue && output.Count >= 3)
            {
                FlatStatement.EntryPoint entryPoint = output[output.Count - 1] as FlatStatement.EntryPoint;
                FlatStatement.Suspend suspend = output[output.Count - 2] as FlatStatement.Suspend;
                FlatStatement.Begin previousBegin = output[output.Count - 3] as FlatStatement.Begin;
                if (entryPoint != null && suspend != null && previousBegin != null
                    && suspend.EntryPoint == entryPoint && previousBegin.IgnoreReturnValue)
                {
                    output.Insert(output.Count - 2, begin);
                    return;
                }
            }
            else
            {
                output.Add(begin);
                FlatStatement.EntryPoint entryPoint = new FlatStatement.EntryPoint();
                output.Add(new FlatStatement.Suspend(entryPoint));
                output.Add(entryPoint);
            }
        }
        void Expression(Expression expr, bool isolated)
        {
            Expression.Call call;
            if (expr.TryGetSimpleCall(out call))
            {
                Call(((Expression.Function)call.CallingOn).TheFunction, call.Parameters, false);
                return;
            }
            switch (expr.Type)
            {
                case ExpressionType.AllocStringBuffer:
                case ExpressionType.AllocStruct:
                case ExpressionType.FloatLiteral:
                case ExpressionType.CharLiteral:
                case ExpressionType.Constant:
                case ExpressionType.EnumValue:
                case ExpressionType.Null:
                case ExpressionType.IntegerLiteral:
                case ExpressionType.StringLiteral:
                case ExpressionType.Variable:
                    PushExpression(expr);
                    break;
                case ExpressionType.AllocateArray:
                    Expression.AllocateArray allocArray = (Expression.AllocateArray)expr;
                    Expression(allocArray.Length);
                    if (stackPushStack.Count == 0)
                    {
                        output.Add(new FlatStatement.AllocateArray(allocArray.ElementType));
                    }
                    else
                    {
                        PushExpression(
                            new Expression.AllocateArray(
                                allocArray.ElementType,
                                PopExpression()));
                    }
                    break;
                case ExpressionType.ArrayIndex:
                    Expression.ArrayIndex arrayIndex = (Expression.ArrayIndex)expr;
                    Expression(arrayIndex.Target);
                    Expression(arrayIndex.Index);
                    if (stackPushStack.Count == 0)
                    {
                        output.Add(new FlatStatement.StackArrayIndex());
                    }
                    else if (stackPushStack.Count == 1)
                    {
                        Expression index = PopExpression();
                        output.Add(
                            new FlatStatement.Push(
                                new Expression.ArrayIndex(
                                    Pop(arrayIndex.Target.GetValueType()),
                                    index)));
                    }
                    else
                    {
                        Expression index = PopExpression();
                        Expression target = PopExpression();
                        PushExpression(new Expression.ArrayIndex(target, index));
                    }
                    break;
                case ExpressionType.BinaryOperator:
                    Expression.BinaryOperator binop = (Expression.BinaryOperator)expr;
                    Expression(binop.Left);
                    Expression(binop.Right);
                    if (stackPushStack.Count == 0)
                    {
                        output.Add(new FlatStatement.StackBinOp(binop.Token));
                    }
                    else if (stackPushStack.Count == 1)
                    {
                        output.Add(
                            new FlatStatement.Push(
                                new Expression.BinaryOperator(
                                    binop.Token,
                                    new FlatExpression.StackPop(binop.Left.GetValueType()),
                                    PopExpression())));
                    }
                    else
                    {
                        Expression right = PopExpression();
                        Expression left = PopExpression();
                        PushExpression(new Expression.BinaryOperator(binop.Token, left, right));
                    }
                    break;
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    Expression(field.Target);
                    if (stackPushStack.Count == 0)
                    {
                        output.Add(
                            new FlatStatement.Push(
                                new Expression.Field(
                                    field.TheStructType,
                                    field.TheField,
                                    new FlatExpression.StackPop(field.Target.GetValueType()))));
                    }
                    else
                    {
                        PushExpression(
                            new Expression.Field(
                                field.TheStructType,
                                field.TheField,
                                PopExpression()));
                    }
                    break;
                case ExpressionType.UnaryOperator:
                    Expression.UnaryOperator unop = (Expression.UnaryOperator)expr;
                    Expression(unop.Operand);
                    if (stackPushStack.Count == 0)
                    {
                        output.Add(
                            new FlatStatement.Push(
                                new Expression.UnaryOperator(
                                    unop.Token,
                                    new FlatExpression.StackPop(unop.Operand.GetValueType()))));
                    }
                    else
                    {
                        PushExpression(
                            new Expression.UnaryOperator(
                                unop.Token,
                                PopExpression()));
                    }
                    break;
            }
        }
    }
}
