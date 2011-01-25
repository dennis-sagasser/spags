using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public enum FlatStatementType
    {
        EntryPoint, Finish, Suspend, StackBinOp, StackArrayIndex, IfBranch, Push, Begin, AllocateArray, Pop
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
            public int Number = 666;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.EntryPoint; }
            }
            public EntryPoint RedirectTo;
            public Expression FinishValue;
            public bool UseFinishValue;
            public EntryPoint Redirected
            {
                get
                {
                    if (RedirectTo == null) return this;
                    return RedirectTo.Redirected;
                }
            }
        }
        public abstract FlatStatementType FlatStatementType
        {
            get;
        }
        public class Finish : FlatStatement
        {
            public Expression Value;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Finish; }
            }
            public override bool Returns()
            {
                return true;
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
            public override bool Returns()
            {
                return true;
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
            public int StackVarargs;
            public List<Expression> DirectParams;
            public List<Expression> DirectVarargs;
            public bool IgnoreReturnValue;
            public override FlatStatementType FlatStatementType
            {
                get { return FlatStatementType.Begin; }
            }
            public bool NonChangingParams()
            {
                if (DirectParams == null) return true;
                foreach (Expression expr in DirectParams)
                {
                    if (!expr.IsConstant())
                    {
                        switch(expr.Type)
                        {
                            case ExpressionType.Variable:
                                Expression.Variable varExpr = (Expression.Variable)expr;
                                if (varExpr.TheVariable is ScriptVariable)
                                {
                                    ScriptVariable scriptVar = (ScriptVariable)varExpr.TheVariable;
                                    // assume that imported variables do not change in the middle of execution
                                    // TODO: make sure this assumption is actually true!
                                    if (scriptVar.OwnerScript != null)
                                    {
                                        return false;
                                    }
                                }
                                break;
                            default:
                                return false;
                        }
                    }
                }
                return true;
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
        StackPop, StackPeek, CurrentEntryPoint, Placeholder
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
        public class Placeholder : FlatExpression
        {
            public Placeholder(string name, ValueType valueType)
            {
                Name = name;
                ValueType = valueType;
            }
            public string Name;
            public ValueType ValueType;
            public override ValueType GetValueType()
            {
                return ValueType;
            }
            public override bool Equals(Expression ex)
            {
                return (this == ex);
            }
            public override bool IsConstant()
            {
                return false;
            }
            public override FlatExpressionType FlatExpressionType
            {
                get { return FlatExpressionType.Placeholder; }
            }
        }
        public class CurrentEntryPoint : FlatExpression
        {
            public override FlatExpressionType FlatExpressionType
            {
                get { return FlatExpressionType.CurrentEntryPoint; }
            }
            public override ValueType GetValueType()
            {
                return ValueType.Int;
            }
            public override bool Equals(Expression ex)
            {
                return ex is CurrentEntryPoint;
            }
            public override bool IsConstant()
            {
                return false;
            }
        }
        public class StackPeek : FlatExpression
        {
            private ValueType valueType;
            public StackPeek(ValueType valueType)
            {
                this.valueType = valueType;
            }
            public override bool Equals(Expression ex)
            {
                return ex is StackPeek;
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
                get { return FlatExpressionType.StackPeek; }
            }
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
            for (int i = output.Count - 1; i >= 0; i--)
            {
                Statement stmt = output[i];
                if (stmt is FlatStatement.Suspend)
                {
                    FlatStatement.Suspend suspend = (FlatStatement.Suspend)stmt;
                    while (i > 0 && output[i - 1] is FlatStatement.EntryPoint)
                    {
                        i--;
                        ((FlatStatement.EntryPoint)output[i]).RedirectTo = suspend.EntryPoint;
                        output.RemoveAt(i + 1);
                        output.RemoveAt(i);
                    }
                }
            }
            for (int i = output.Count - 1; i >= 0; i--)
            {
                Statement stmt = output[i];
                if (stmt is FlatStatement.EntryPoint)
                {
                    FlatStatement.EntryPoint entryPoint = (FlatStatement.EntryPoint)stmt;
                    if (i > 0 && output[i-1] is FlatStatement.EntryPoint)
                    {
                        entryPoint.RedirectTo = ((FlatStatement.EntryPoint)output[i-1]);
                        output.RemoveAt(i);
                    }
                }
                else if (stmt is FlatStatement.Finish || stmt is Statement.Return)
                {
                    Expression value;
                    if (stmt is FlatStatement.Finish)
                    {
                        value = ((FlatStatement.Finish)stmt).Value;
                    }
                    else
                    {
                        value = ((Statement.Return)stmt).Value;
                    }
                    if (output[i - 1] is FlatStatement.EntryPoint && (value == null || value.IsConstant()))
                    {
                        output.RemoveAt(i);
                        while (i > 0 && output[i - 1] is FlatStatement.EntryPoint)
                        {
                            i--;
                            FlatStatement.EntryPoint entryPoint = (FlatStatement.EntryPoint)output[i];
                            entryPoint.UseFinishValue = true;
                            entryPoint.FinishValue = value;
                            output.RemoveAt(i);
                        }
                    }
                }
            }
            int pointNum = 0;
            foreach (Statement stmt in output)
            {
                if (stmt is FlatStatement.EntryPoint)
                {
                    ((FlatStatement.EntryPoint)stmt).Number = pointNum++;
                }
            }
            List<Statement> oldOutput = output;
            output = new List<Statement>();
            int pos;
            for (pos = 0; pos < oldOutput.Count; pos++)
            {
                if (oldOutput[pos] is FlatStatement.EntryPoint)
                {
                    break;
                }
                output.Add(oldOutput[pos]);
            }
            if (pos >= oldOutput.Count)
            {
                return;
            }
            Statement.Block firstBlock = new Statement.Block(new NameDictionary());
            Statement.If entryPointSwitch = new Statement.If(
                new Expression.BinaryOperator(
                    Token.IsEqualTo,
                    new FlatExpression.CurrentEntryPoint(),
                    new Expression.IntegerLiteral(0)),
                firstBlock,
                null);
            output.Add(entryPointSwitch);
            Statement.If currentIf = entryPointSwitch;
            Statement.Block currentBlock = firstBlock;
            while (++pos < oldOutput.Count)
            {
                FlatStatement.EntryPoint currentEntryPoint = oldOutput[pos] as FlatStatement.EntryPoint;
                if (currentEntryPoint != null)
                {
                    currentBlock = new Statement.Block(new NameDictionary());
                    Statement.If newIf = new Statement.If(
                        new Expression.BinaryOperator(
                            Token.IsEqualTo,
                            new FlatExpression.CurrentEntryPoint(),
                            new Expression.IntegerLiteral(currentEntryPoint.Number)),
                        currentBlock,
                        null);
                    currentIf.ElseDoThis = newIf;
                    currentIf = newIf;
                }
                else
                {
                    currentBlock.ChildStatements.Add(oldOutput[pos]);
                }
            }
            if (entryPointSwitch.ElseDoThis == null)
            {
                output = firstBlock.ChildStatements;
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
        bool Ending()
        {
            if (output.Count == 0) return false;
            return output[output.Count-1].Returns();
        }
        void MoveUpToEntryPoint(int start, Statement.Block block)
        {
            int i = start;
            while (i < output.Count)
            {
                Statement stmt = output[i];
                if (stmt is FlatStatement.EntryPoint)
                {
                    return;
                }
                output.RemoveAt(i);
                block.ChildStatements.Add(stmt);
            }
        }
        void Statement(Statement stmt)
        {
            CodeUnitData cudata = UserData<CodeUnit, CodeUnitData>.Get(stmt);
            if (!cudata.Blocked && !(stmt is Statement.Block))
            {
                if (output.Count >= 3)
                {
                    FlatStatement.EntryPoint prevEntryPoint = output[output.Count - 1] as FlatStatement.EntryPoint;
                    FlatStatement.Suspend suspend = output[output.Count - 2] as FlatStatement.Suspend;
                    FlatStatement.Begin previousBegin = output[output.Count - 3] as FlatStatement.Begin;
                    if (prevEntryPoint != null && suspend != null && previousBegin != null
                        && suspend.EntryPoint == prevEntryPoint && previousBegin.IgnoreReturnValue)
                    {
                        Function func;
                        List<Expression> fparams;
                        List<Expression> varargs;
                        if (stmt.TryGetSimpleCall(out func, out fparams, out varargs))
                        {
                            FlatStatement.Begin begin = new FlatStatement.Begin(true);
                            begin.Function = func;
                            begin.DirectParams = fparams;
                            begin.DirectVarargs = varargs;
                            if (begin.NonChangingParams())
                            {
                                output.Insert(output.Count - 2, begin);
                                return;
                            }
                        }
                    }
                }

                output.Add(stmt);
                return;
            }

            Function callFunc;
            List<Expression> callParams;
            List<Expression> callVarargs;
            if (stmt.TryGetSimpleCall(out callFunc, out callParams, out callVarargs))
            {
                Call(callFunc, callParams, callVarargs, true);
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
                    CodeUnitData conditionData = UserData<CodeUnit, CodeUnitData>.Get(conditional.IfThisIsTrue);
                    if (conditionData.Blocked)
                    {
                        Expression(conditional.IfThisIsTrue);
                    }
                    else
                    {
                        stackPushStack.Push(conditional.IfThisIsTrue);
                    }
                    CodeUnitData thenData = UserData<CodeUnit, CodeUnitData>.Get(conditional.ThenDoThis);
                    CodeUnitData elseData = (conditional.ElseDoThis == null) ? null :
                        UserData<CodeUnit, CodeUnitData>.Get(conditional.ElseDoThis);
                    if (thenData.Blocked)
                    {
                        if (elseData != null && elseData.Blocked)
                        {
                            int moveStart;
                            Statement.Block thenBlock = new Statement.Block(new NameDictionary());
                            Statement.Block elseBlock = new Statement.Block(new NameDictionary());
                            Statement.If theIf = new Statement.If(Pop(conditional.IfThisIsTrue.GetValueType()), thenBlock, elseBlock);
                            output.Add(theIf);
                            moveStart = output.Count;
                            Statement(conditional.ThenDoThis);
                            MoveUpToEntryPoint(moveStart, thenBlock);
                            FlatStatement.EntryPoint endPoint = null;
                            if (!Ending())
                            {
                                endPoint = endPoint ?? new FlatStatement.EntryPoint();
                                output.Add(new FlatStatement.Suspend(endPoint));
                            }
                            moveStart = output.Count;
                            Statement(conditional.ElseDoThis);
                            MoveUpToEntryPoint(moveStart, elseBlock);
                            if (elseBlock.ChildStatements.Count == 1)
                            {
                                theIf.ElseDoThis = elseBlock.ChildStatements[0];
                            }
                            if (!Ending())
                            {
                                endPoint = endPoint ?? new FlatStatement.EntryPoint();
                                output.Add(new FlatStatement.Suspend(endPoint));
                            }
                            if (endPoint != null)
                            {
                                output.Add(endPoint);
                            }
                            break;
                        }
                        else
                        {
                            int moveStart;
                            FlatStatement.Block thenBlock = new Statement.Block(new NameDictionary());
                            Statement.If theIf = new Statement.If(Pop(conditional.IfThisIsTrue.GetValueType()), thenBlock, conditional.ElseDoThis);
                            output.Add(theIf);
                            FlatStatement.EntryPoint endPoint = null;
                            if (theIf.ElseDoThis == null || !theIf.ElseDoThis.Returns())
                            {
                                if (endPoint == null)
                                {
                                    endPoint = new FlatStatement.EntryPoint();
                                }
                                if (theIf.ElseDoThis == null)
                                {
                                    theIf.ElseDoThis = new FlatStatement.Suspend(endPoint);
                                }
                                else
                                {
                                    output.Add(new FlatStatement.Suspend(endPoint));
                                }
                            }
                            moveStart = output.Count;
                            Statement(conditional.ThenDoThis);
                            if (!Ending())
                            {
                                if (endPoint == null)
                                {
                                    endPoint = new FlatStatement.EntryPoint();
                                }
                                output.Add(new FlatStatement.Suspend(endPoint));
                            }
                            MoveUpToEntryPoint(moveStart, thenBlock);
                            if (endPoint != null)
                            {
                                output.Add(endPoint);
                            }
                            break;
                        }
                    }
                    else
                    {
                        if (elseData != null && elseData.Blocked)
                        {
                            int moveStart;
                            Statement.Block elseBlock = new Statement.Block(new NameDictionary());
                            Statement.If theIf = new Statement.If(
                                Pop(conditional.IfThisIsTrue.GetValueType()),
                                conditional.ThenDoThis,
                                elseBlock);
                            output.Add(theIf);
                            FlatStatement.EntryPoint endPoint = null;
                            if (!conditional.ThenDoThis.Returns())
                            {
                                endPoint = endPoint ?? new FlatStatement.EntryPoint();
                                output.Add(new FlatStatement.Suspend(endPoint));
                            }
                            moveStart = output.Count;
                            Statement(conditional.ElseDoThis);
                            MoveUpToEntryPoint(moveStart, elseBlock);
                            if (elseBlock.ChildStatements.Count == 1 && elseBlock.ChildStatements[0].Type == StatementType.If)
                            {
                                theIf.ElseDoThis = elseBlock.ChildStatements[0];
                            }
                            if (!Ending())
                            {
                                endPoint = endPoint ?? new FlatStatement.EntryPoint();
                                output.Add(new FlatStatement.Suspend(endPoint));
                            }
                            if (endPoint != null)
                            {
                                output.Add(endPoint);
                            }
                            break;
                        }
                        else
                        {
                            output.Add(new Statement.If(Pop(conditional.IfThisIsTrue.GetValueType()), conditional.ThenDoThis, conditional.ElseDoThis));
                            break;
                        }
                    }
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    FlatStatement.Finish setReturnValue = new FlatStatement.Finish();
                    if (ret.Value != null)
                    {
                        Expression(ret.Value);
                        setReturnValue.Value = Pop(ret.Value.GetValueType());
                        output.Add(setReturnValue);
                    }
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
                    FlatStatement.EntryPoint endLoopPoint = new FlatStatement.EntryPoint();
                    output.Add(new Statement.If(
                        SPAGS.Expression.LogicalNegation(Pop(loop.WhileThisIsTrue.GetValueType())),
                        new FlatStatement.Suspend(endLoopPoint),
                        null));
                    Statement(loop.KeepDoingThis);
                    if (!Ending())
                    {
                        output.Add(new FlatStatement.Suspend(startPoint));
                    }
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
        void Call(Function func, List<Expression> parameters, List<Expression> varargs, bool ignoreReturnValue)
        {
            FunctionData fdata = UserData<Function, FunctionData>.Get(func);
            FlatStatement.Begin begin = new FlatStatement.Begin(ignoreReturnValue);
            begin.Function = func;
            FlushStackPushStack();
            foreach (Expression param in parameters)
            {
                Expression(param);
            }
            begin.StackParams = parameters.Count - stackPushStack.Count;
            begin.DirectParams = new List<Expression>();
            for (int i = 0; i < parameters.Count - begin.StackParams; i++)
            {
                begin.DirectParams.Insert(0, PopExpression());
            }
            if (varargs != null)
            {
                foreach (Expression vararg in varargs)
                {
                    Expression(vararg);
                }
                begin.StackVarargs = varargs.Count - stackPushStack.Count;
                begin.DirectVarargs = new List<Expression>();
                for (int i = 0; i < (varargs.Count - begin.StackVarargs); i++)
                {
                    begin.DirectVarargs.Insert(0, PopExpression());
                }
            }
            if (ignoreReturnValue && output.Count >= 3)
            {
                FlatStatement.EntryPoint prevEntryPoint = output[output.Count - 1] as FlatStatement.EntryPoint;
                FlatStatement.Suspend suspend = output[output.Count - 2] as FlatStatement.Suspend;
                FlatStatement.Begin previousBegin = output[output.Count - 3] as FlatStatement.Begin;
                if (prevEntryPoint != null && suspend != null && previousBegin != null
                    && begin.NonChangingParams()
                    && suspend.EntryPoint == prevEntryPoint && previousBegin.IgnoreReturnValue)
                {
                    output.Insert(output.Count - 2, begin);
                    return;
                }
            }
            output.Add(begin);
            FlatStatement.EntryPoint entryPoint = new FlatStatement.EntryPoint();
            output.Add(new FlatStatement.Suspend(entryPoint));
            output.Add(entryPoint);
        }
        void Expression(Expression expr, bool isolated)
        {
            CodeUnitData cudata = UserData<CodeUnit, CodeUnitData>.Get(expr);
            if (!cudata.Blocked)
            {
                stackPushStack.Push(expr);
                return;
            }

            Function callFunc;
            List<Expression> callParams;
            List<Expression> varargs;
            if (expr.TryGetSimpleCall(out callFunc, out callParams, out varargs))
            {
                Call(callFunc, callParams, varargs, false);
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
                    if (binop.Token.Type == TokenType.LogicalAnd)
                    {
                        int moveFrom;
                        Expression(binop.Left);
                        FlushStackPushStack();
                        Statement.Block thenBlock = new Statement.Block(new NameDictionary());
                        output.Add(
                            new Statement.If(
                                new FlatExpression.StackPeek(binop.Left.GetValueType()),
                                thenBlock,
                                null));
                        FlatStatement.EntryPoint endPoint = null;
                        if (UserData<CodeUnit, CodeUnitData>.Get(binop.Right).Blocked)
                        {
                            endPoint = endPoint ?? new FlatStatement.EntryPoint();
                            output.Add(new FlatStatement.Suspend(endPoint));
                        }
                        moveFrom = output.Count;
                        output.Add(new FlatStatement.Pop());
                        Expression(binop.Right);
                        FlushStackPushStack();
                        MoveUpToEntryPoint(moveFrom, thenBlock);
                        if (endPoint != null)
                        {
                            output.Add(endPoint);
                        }
                        break;
                    }
                    else if (binop.Token.Type == TokenType.LogicalOr)
                    {
                        int moveFrom;
                        Expression(binop.Left);
                        FlushStackPushStack();
                        Statement.Block thenBlock = new Statement.Block(new NameDictionary());
                        output.Add(
                            new Statement.If(
                                SPAGS.Expression.LogicalNegation(new FlatExpression.StackPeek(binop.Left.GetValueType())),
                                thenBlock,
                                null));
                        FlatStatement.EntryPoint endPoint = null;
                        if (UserData<CodeUnit, CodeUnitData>.Get(binop.Right).Blocked)
                        {
                            endPoint = endPoint ?? new FlatStatement.EntryPoint();
                            output.Add(new FlatStatement.Suspend(endPoint));
                        }
                        moveFrom = output.Count;
                        output.Add(new FlatStatement.Pop());
                        Expression(binop.Right);
                        FlushStackPushStack();
                        MoveUpToEntryPoint(moveFrom, thenBlock);
                        if (endPoint != null)
                        {
                            output.Add(endPoint);
                        }
                        break;
                    }
                    else
                    {
                        Expression(binop.Left);
                        Expression(binop.Right);
                    }
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
