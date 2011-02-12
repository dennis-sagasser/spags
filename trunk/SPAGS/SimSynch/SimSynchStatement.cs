﻿using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace SPAGS.SimSynch
{
    public abstract class SimSynchStatement : Statement
    {
        protected SimSynchStatement(StatementType stype)
            : base(stype)
        {
        }

        public class CallSuspend : SimSynchStatement
        {
            public CallSuspend(SimSynchExpression.StackAugmentedCall callExpr, SimSynchChunk resumeChunk)
                : base(StatementType.SimSynchCallSuspend)
            {
                CallExpression = callExpr;
                ResumeChunk = resumeChunk;
            }
            public SimSynchExpression.StackAugmentedCall CallExpression;
            public SimSynchChunk ResumeChunk;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@suspend(@entrypoint_" + ResumeChunk.ID + ", " + CallExpression.CallingFunction.Name);
                if (CallExpression.StackVarargCount > 0)
                {
                    output.Write(", @stackparams(" + CallExpression.StackParameterCount + ", " + CallExpression.StackVarargCount + ")");
                    foreach (Expression directVararg in CallExpression.DirectVarargs)
                    {
                        output.Write(", ");
                        directVararg.WriteTo(output);
                    }
                }
                else
                {
                    if (CallExpression.StackParameterCount > 0)
                    {
                        output.Write(", @stackparams(" + CallExpression.StackParameterCount + ")");
                    }
                    foreach (Expression directParam in CallExpression.DirectParameters)
                    {
                        output.Write(", ");
                        directParam.WriteTo(output);
                    }
                    foreach (Expression directVararg in CallExpression.DirectVarargs)
                    {
                        output.Write(", ");
                        directVararg.WriteTo(output);
                    }
                }
                output.Write(");");
            }
            public override bool Returns()
            {
                return true;
            }
        }

        public class Suspend : SimSynchStatement
        {
            public Suspend(SimSynchChunk nextChunk)
                : base(StatementType.SimSynchSuspend)
            {
                NextChunk = nextChunk;
            }
            public Suspend()
                : this(null)
            {
            }
            public SimSynchChunk NextChunk;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@suspend(@entrypoint_" + NextChunk.ID + ");");
            }
            public override bool Returns()
            {
                return true;
            }
        }

        public class Finish : SimSynchStatement
        {
            public Finish(Expression returnValue)
                : base(StatementType.SimSynchFinish)
            {
                ReturnValue = returnValue;
            }
            public Expression ReturnValue;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@finish(");
                if (ReturnValue != null)
                {
                    ReturnValue.WriteTo(output);
                }
                output.Write(");");
            }
            public override bool Returns()
            {
                return true;
            }
        }

        public class Push : SimSynchStatement
        {
            public Push(Expression value)
                : base(StatementType.SimSynchPush)
            {
                Value = value;
            }
            public Expression Value;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@push(");
                Value.WriteTo(output);
                output.Write(");");
            }
        }

        public class Pop : SimSynchStatement
        {
            public Pop()
                : base(StatementType.SimSynchPop)
            {
            }
            public Expression Value;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@pop();");
            }
        }

        public class StackBinOp : SimSynchStatement
        {
            public StackBinOp(Token opToken, ValueType leftType, ValueType rightType)
                : base(StatementType.SimSynchBinOp)
            {
                OpToken = opToken;
                LeftType = leftType;
                RightType = rightType;
            }
            public Token OpToken;
            public ValueType LeftType, RightType;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@stackbinop(\"" + OpToken + "\");");
            }
        }

        public class StackUnOp : SimSynchStatement
        {
            public StackUnOp(Token opToken, ValueType operandType)
                : base(StatementType.SimSynchUnOp)
            {
                OpToken = opToken;
                OperandType = operandType;
            }
            public Token OpToken;
            public ValueType OperandType;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@stackunop(\"" + OpToken + "\");");
            }
        }

        public class StackArrayIndex : SimSynchStatement
        {
            public StackArrayIndex()
                : base(StatementType.SimSynchArrayIndex)
            {
            }
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@stackarrayindex();");
            }
        }

        public class StackFieldAccess : SimSynchStatement
        {
            public StackFieldAccess(StructMember.Field theField)
                : base(StatementType.SimSynchFieldAccess)
            {
                Field = theField;
            }
            public StructMember.Field Field;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("@stackfieldaccess(\"" + Field.Name + "\");");
            }
        }

        public class StackAugmentedCall : SimSynchStatement
        {
            public StackAugmentedCall(SimSynchExpression.StackAugmentedCall callExpr)
                : base(StatementType.SimSynchStackAugmentedCall)
            {
                CallExpression = callExpr;
            }
            public SimSynchExpression.StackAugmentedCall CallExpression;
            public override void WriteTo(TextWriter output, int indent)
            {
                CallExpression.WriteTo(output);
                output.Write(";");
            }
        }
    }
}
