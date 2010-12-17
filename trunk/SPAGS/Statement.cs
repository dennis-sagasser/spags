using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public enum StatementType
    {
        Block,
        If,
        While,
        Return,
        Call,
        Assign,
        VariableDeclaration
    }
    public abstract class Statement
    {
        protected Statement(StatementType type)
        {
            Type = type;
        }
        public virtual IEnumerable<Statement> YieldChildStatements()
        {
            yield break;
        }
        public virtual IEnumerable<Expression> YieldExpressions()
        {
            yield break;
        }
        public virtual IEnumerable<Function> YieldFunctions()
        {
            foreach (Expression expr in this.YieldExpressions())
            {
                foreach (Function func in expr.YieldFunctions())
                {
                    yield return func;
                }
            }
            yield break;
        }
        public readonly StatementType Type;
        public object UserData;
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (TextWriter output = new StringWriter(sb))
            {
                WriteTo(output, 0);
            }
            return sb.ToString();
        }

        protected void WriteIndent(TextWriter output, int indent)
        {
            while (indent-- > 0) output.Write('\t');
        }

        public bool ContainsBlockingCalls()
        {
            foreach (Function func in this.YieldFunctions())
            {
                if (func.MarkedAsBlocking) return true;
            }
            return false;
        }

        public abstract void WriteTo(TextWriter output, int indent);

        public class Block : Statement
        {
            public Block(NameDictionary initialScope) : base(StatementType.Block)
            {
                Scope = initialScope;
            }

            public readonly List<Statement> ChildStatements = new List<Statement>();

            public override IEnumerable<Statement> YieldChildStatements()
            {
                foreach (Statement child in ChildStatements) yield return child;
            }

            public override IEnumerable<Expression> YieldExpressions()
            {
                foreach (Statement child in ChildStatements)
                {
                    foreach (Expression expr in child.YieldExpressions())
                    {
                        yield return expr;
                    }
                }
            }

            public readonly NameDictionary Scope;

            public override void WriteTo(TextWriter output, int indent)
            {
                if (ChildStatements.Count == 0)
                {
                    output.Write("{ }");
                    return;
                }
                output.Write("{\n");
                indent++;
                foreach (Statement kid in ChildStatements)
                {
                    WriteIndent(output, indent);
                    kid.WriteTo(output, indent);
                    output.Write('\n');
                }
                indent--;
                WriteIndent(output, indent);
                output.Write("}");
            }
        }

        public class VariableDeclaration : Statement
        {
            public VariableDeclaration() : base(StatementType.VariableDeclaration) { }
            public List<Variable> Variables = new List<Variable>();
            public override IEnumerable<Expression> YieldExpressions()
            {
                foreach (Variable var in Variables)
                {
                    if (var.InitialValue != null) yield return var.InitialValue;
                }
            }
            public override void WriteTo(TextWriter output, int indent)
            {
                ValueType vtype = Variables[0].Type;
                if (vtype is ArrayType) vtype = ((ArrayType)vtype).ElementType;
                output.Write(Variables[0].Type.Name + " ");
                for (int i = 0; i < Variables.Count; i++)
                {
                    if (i > 0) output.Write(", ");
                    output.Write(Variables[i].Name);
                    ArrayType asArray = Variables[i].Type as ArrayType;
                    if (asArray != null)
                    {
                        output.Write('[');
                        if (asArray.LengthExpression != null) asArray.LengthExpression.WriteTo(output);
                        output.Write(']');
                    }
                    if (Variables[i].InitialValue != null)
                    {
                        output.Write(" = ");
                        Variables[i].InitialValue.WriteTo(output);
                    }
                }
                output.Write(';');
            }
        }

        public class If : Statement
        {
            public If(Expression ifThisIsTrue, Statement thenDoThis, Statement elseDoThis)
                : base(StatementType.If)
            {
                IfThisIsTrue = ifThisIsTrue;
                ThenDoThis = thenDoThis;
                ElseDoThis = elseDoThis;

                switch (ThenDoThis.Type)
                {
                    case StatementType.If:
                    case StatementType.While:
                        Block block = new Block(new NameDictionary());
                        block.ChildStatements.Add(ThenDoThis);
                        ThenDoThis = block;
                        break;
                    case StatementType.Block:
                        Block _block = (Block)ThenDoThis;
                        if (_block.ChildStatements.Count == 1)
                        {
                            switch (_block.ChildStatements[0].Type)
                            {
                                case StatementType.VariableDeclaration:
                                case StatementType.If:
                                case StatementType.While:
                                    break;
                                default:
                                    ThenDoThis = _block.ChildStatements[0];
                                    break;
                            }
                        }
                        break;
                }
            }
            public readonly Expression IfThisIsTrue;
            public readonly Statement ThenDoThis;
            public readonly Statement ElseDoThis;
            public override IEnumerable<Expression> YieldExpressions()
            {
                yield return IfThisIsTrue;
                foreach (Expression expr in ThenDoThis.YieldExpressions())
                {
                    yield return expr;
                }
                if (ElseDoThis != null)
                {
                    foreach (Expression expr in ElseDoThis.YieldExpressions())
                    {
                        yield return expr;
                    }
                }
            }
            public override IEnumerable<Statement> YieldChildStatements()
            {
                yield return ThenDoThis;
                if (ElseDoThis != null) yield return ElseDoThis;
            }
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("if (");
                IfThisIsTrue.WriteTo(output);
                output.Write(") ");
                ThenDoThis.WriteTo(output, indent);
                if (ElseDoThis != null)
                {
                    output.Write("\n");
                    WriteIndent(output, indent);
                    output.Write("else ");
                    ElseDoThis.WriteTo(output, indent);
                }
            }
        }

        public class Assign : Statement
        {
            public Assign(Expression target, Expression value, Token assignToken)
                : base(StatementType.Assign)
            {
                Target = target;
                Value = value;
                AssignToken = assignToken;
            }
            public readonly Expression Target;
            public readonly Expression Value;
            public readonly Token AssignToken;
            public override IEnumerable<Function> YieldFunctions()
            {
                foreach (Function func in base.YieldFunctions())
                {
                    yield return func;
                }
            }
            public override IEnumerable<Expression> YieldExpressions()
            {
                if (Target != null) yield return Target;
                if (Value != null) yield return Value;
            }            public override void WriteTo(TextWriter output, int indent)
            {
                Target.WriteTo(output);
                switch(AssignToken.Type)
                {
                    case TokenType.Increment:
                    case TokenType.Decrement:
                        output.Write(AssignToken + ";");
                        return;
                }
                output.Write(" " + AssignToken + " ");
                Value.WriteTo(output);
            }
        }

        public class Return : Statement
        {
            public Return(Expression value) : base(StatementType.Return)
            {
                Value = value;
            }
            public readonly Expression Value;
            public override IEnumerable<Expression> YieldExpressions()
            {
                if (Value != null) yield return Value;
            }
            public override void WriteTo(TextWriter output, int indent)
            {
                if (Value == null)
                {
                    output.Write("return;");
                }
                else
                {
                    output.Write("return ");
                    Value.WriteTo(output);
                    output.Write(";");
                }
            }
        }

        public class Call : Statement
        {
            public Call(Expression callExpression)
                : base(StatementType.Call)
            {
                CallExpression = callExpression;
            }
            public readonly Expression CallExpression;
            public override IEnumerable<Function> YieldFunctions()
            {
                foreach (Function func in base.YieldFunctions())
                {
                    yield return func;
                }
            }
            public override void WriteTo(TextWriter output, int indent)
            {
                CallExpression.WriteTo(output);
            }
        }

        public class While : Statement
        {
            public While(Expression whileThisIsTrue, Statement doThis)
                : base(StatementType.While)
            {
                WhileThisIsTrue = whileThisIsTrue;
                KeepDoingThis = doThis;
            }
            public readonly Expression WhileThisIsTrue;
            public readonly Statement KeepDoingThis;
            public override void WriteTo(TextWriter output, int indent)
            {
                output.Write("while (");
                WhileThisIsTrue.WriteTo(output);
                output.Write(") ");
                KeepDoingThis.WriteTo(output, indent);
            }
            public override IEnumerable<Expression> YieldExpressions()
            {
                yield return WhileThisIsTrue;
                foreach (Expression expr in KeepDoingThis.YieldExpressions()) yield return expr;
            }
            public override IEnumerable<Statement> YieldChildStatements()
            {
                yield return KeepDoingThis;
            }
        }
    }

}
