using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace SPAGS.SimSynch
{
    public abstract class SimSynchExpression : Expression
    {
        protected SimSynchExpression(ExpressionType etype)
            : base(etype)
        {
        }
        public class Peek : SimSynchExpression
        {
            public Peek(ValueType valueType)
                : base(ExpressionType.SimSynchPeek)
            {
                PeekedType = valueType;
            }
            public ValueType PeekedType;
            public override bool Equals(Expression ex)
            {
                return (ex is Peek) && (((Peek)ex).PeekedType == PeekedType);
            }
            public override ValueType GetValueType()
            {
                return PeekedType;
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write("@peek<" + PeekedType + ">()");
            }
            public override bool IsConstant()
            {
                return false;
            }
        }
        public class Pop : SimSynchExpression
        {
            public Pop(ValueType valueType)
                : base(ExpressionType.SimSynchPop)
            {
                PoppedType = valueType;
            }
            public ValueType PoppedType;
            public override void WriteTo(TextWriter output)
            {
                output.Write("@pop<" + PoppedType.Name + ">()");
            }
            public override bool Equals(Expression ex)
            {
                return (ex is Pop) && (((Pop)ex).PoppedType == PoppedType);
                throw new NotImplementedException();
            }
            public override ValueType GetValueType()
            {
                return PoppedType;
            }
            public override bool IsConstant()
            {
                return false;
            }
        }
        public new class Variable : SimSynchExpression
        {
            public Variable(SPAGS.LocalVariable variable)
                : base(ExpressionType.Variable)
            {
                TheVariable = variable;
            }
            public readonly SPAGS.LocalVariable TheVariable;
            public override bool UnchangingWhileThreadSuspended
            {
                get { return true; }
            }
            public override ValueType GetValueType()
            {
                return TheVariable.Type;
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write(TheVariable.Name);
            }
            public override bool Equals(Expression ex)
            {
                return (ex is Variable) && (((Variable)ex).TheVariable == this.TheVariable);
            }
            public override bool IsConstant()
            {
                return false;
            }
        }
        public class StackAugmentedCall : SimSynchExpression
        {
            public StackAugmentedCall()
                : base(ExpressionType.SimSynchStackAugmentedCall)
            {
            }
            public SPAGS.Function CallingFunction;
            public int StackParameterCount;
            public int StackVarargCount;
            public List<Expression> DirectParameters = new List<Expression>();
            public List<Expression> DirectVarargs = new List<Expression>();
            public bool ParametersUnchangingWhileThreadSuspended
            {
                get
                {
                    if (StackParameterCount > 0 || StackVarargCount > 0) return false;
                    foreach (Expression directParam in DirectParameters)
                    {
                        if (!directParam.UnchangingWhileThreadSuspended)
                        {
                            return false;
                        }
                    }
                    foreach (Expression directVararg in DirectVarargs)
                    {
                        if (!directVararg.UnchangingWhileThreadSuspended)
                        {
                            return false;
                        }
                    }
                    return true;
                }
            }
            public override void WriteTo(TextWriter output)
            {
                output.Write(CallingFunction.Name);
                output.Write("(");
                if (StackVarargCount > 0)
                {
                    output.Write("@multipop(" + (StackParameterCount + StackVarargCount) + ")...");
                    for (int i = 0; i < DirectVarargs.Count; i++)
                    {
                        output.Write(", ");
                        DirectVarargs[i].WriteTo(output);
                    }
                }
                else
                {
                    if (StackParameterCount > 0)
                    {
                        output.Write("@multipop(" + StackParameterCount + ")...");
                    }
                    for (int i = 0; i < DirectParameters.Count; i++)
                    {
                        if (i > 0 || StackParameterCount > 0)
                        {
                            output.Write(", ");
                        }
                        DirectParameters[i].WriteTo(output);
                    }
                    for (int i = 0; i < DirectVarargs.Count; i++)
                    {
                        if (i > 0 || StackParameterCount > 0 || DirectParameters.Count > 0)
                        {
                            output.Write(", ");
                        }
                        DirectParameters[i].WriteTo(output);
                    }
                }
                output.Write(")");
            }
            public override ValueType GetValueType()
            {
                return CallingFunction.Signature.ReturnType;
            }
            public override bool Equals(Expression ex)
            {
                StackAugmentedCall stackCall = ex as StackAugmentedCall;
                if (stackCall == null
                    || stackCall.StackParameterCount != this.StackParameterCount
                    || stackCall.DirectParameters.Count != this.DirectParameters.Count)
                {
                    return false;
                }
                for (int i = 0; i < DirectParameters.Count; i++)
                {
                    if (!DirectParameters[i].Equals(stackCall.DirectParameters[i]))
                    {
                        return false;
                    }
                }
                return true;
            }
            public override bool IsConstant()
            {
                return false;
            }
        }
    }
}
