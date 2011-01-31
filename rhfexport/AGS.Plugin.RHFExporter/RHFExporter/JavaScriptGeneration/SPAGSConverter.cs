using System;
using System.Collections.Generic;
using System.Text;

namespace RedHerringFarm.JavaScriptGeneration
{
    public class SPAGSConverter
    {
        public Dictionary<SPAGS.Function, string> SPAGSFunctionNames
            = new Dictionary<SPAGS.Function,string>();

        public FunctionDefinition FromSPAGS(SPAGS.Function spagsFunc)
        {
            FunctionDefinition jsFunc = new FunctionDefinition();
            for (int i = 0; i < spagsFunc.ParameterVariables.Count; i++)
            {
                SPAGS.Parameter spagsParam = spagsFunc.ParameterVariables[i];
                Variable p = new Variable(spagsParam.Name, GetValueTypes(spagsParam.Type));
                AddReference(spagsParam, p);
                jsFunc.Parameters.Add(p);
            }
            foreach (SPAGS.Statement statement in spagsFunc.Body.ChildStatements)
            {
                jsFunc.Body.Add(FromSPAGS(spagsFunc, statement, jsFunc.Body));
            }
            return jsFunc;
        }

        public ScopedBlock FunctionBodyFromSPAGS(SPAGS.Function spagsFunc)
        {
            ScopedBlock body = new ScopedBlock();
            foreach (SPAGS.Statement statement in spagsFunc.Body.ChildStatements)
            {
                body.Add(FromSPAGS(spagsFunc, statement, body));
            }
            if (spagsFunc.Signature.ReturnType.Category != SPAGS.ValueTypeCategory.Void
                && !spagsFunc.Body.Returns())
            {
                body.Add(FromSPAGS(spagsFunc, new SPAGS.Statement.Return(spagsFunc.Signature.ReturnType.CreateDefaultValueExpression()), body));
            }
            return body;
        }

        public PossibleValueTypes GetValueTypes(SPAGS.ValueType spagsVT)
        {
            switch (spagsVT.Category)
            {
                case SPAGS.ValueTypeCategory.Int:
                    switch (spagsVT.IntType)
                    {
                        case "uint8":
                            return PossibleValueTypes.UInt8;
                        case "int16":
                            return PossibleValueTypes.Int16;
                        default:
                            return PossibleValueTypes.Int32;
                    }
                case SPAGS.ValueTypeCategory.Float:
                    return PossibleValueTypes.Number;
                case SPAGS.ValueTypeCategory.StringValue:
                    return PossibleValueTypes.String;
                default:
                    return PossibleValueTypes.Any;
            }
        }

        public Statement FromSPAGS(SPAGS.Function spagsFunc, SPAGS.Statement spagsStatement, ScopedBlock jsScope)
        {
            SPAGS.Function callFunction;
            List<SPAGS.Expression> callParams;
            List<SPAGS.Expression> callVarargs;
            if (spagsStatement.TryGetSimpleCall(out callFunction, out callParams, out callVarargs))
            {
                return (Statement)FromSPAGS(callFunction, callParams, callVarargs);
            }
            switch (spagsStatement.Type)
            {
                case SPAGS.StatementType.Block:
                    SPAGS.Statement.Block spagsBlock = (SPAGS.Statement.Block)spagsStatement;
                    Statement.GenericBlock jsBlock = new Statement.GenericBlock();
                    foreach (SPAGS.Statement statement in spagsBlock.ChildStatements)
                    {
                        jsBlock.Block.Add(FromSPAGS(spagsFunc, statement, jsScope));
                    }
                    return jsBlock;
                case SPAGS.StatementType.Assign:
                    SPAGS.Statement.Assign spagsAssign = (SPAGS.Statement.Assign)spagsStatement;
                    PossibleValueTypes assignType = GetValueTypes(spagsAssign.Target.GetValueType());
                    Expression jsAssign = new Expression.InfixOperation(
                        FromSPAGS(spagsAssign.Target),
                        Infix.Assign,
                        FromSPAGS(spagsAssign.SimpleAssignValue()).Cast(assignType));
                    return (Statement)jsAssign;
                case SPAGS.StatementType.If:
                    SPAGS.Statement.If spagsIf = (SPAGS.Statement.If)spagsStatement;
                    Statement.If jsIf = new Statement.If(
                        FromSPAGS(spagsIf.IfThisIsTrue).Cast(PossibleValueTypes.Boolean));
                    if (spagsIf.ThenDoThis.Type == SPAGS.StatementType.Block)
                    {
                        SPAGS.Statement.Block thenBlock = (SPAGS.Statement.Block)spagsIf.ThenDoThis;
                        foreach (SPAGS.Statement statement in thenBlock.ChildStatements)
                        {
                            jsIf.ThenDoThis.Add(FromSPAGS(spagsFunc, statement, jsScope));
                        }
                    }
                    else
                    {
                        jsIf.ThenDoThis.Add(FromSPAGS(spagsFunc, spagsIf.ThenDoThis, jsScope));
                    }
                    if (spagsIf.ElseDoThis != null)
                    {
                        if (spagsIf.ElseDoThis.Type == SPAGS.StatementType.Block)
                        {
                            SPAGS.Statement.Block elseBlock = (SPAGS.Statement.Block)spagsIf.ElseDoThis;
                            foreach (SPAGS.Statement statement in elseBlock.ChildStatements)
                            {
                                jsIf.ElseDoThis.Add(FromSPAGS(spagsFunc, statement, jsScope));
                            }
                        }
                        else
                        {
                            jsIf.ElseDoThis.Add(FromSPAGS(spagsFunc, spagsIf.ElseDoThis, jsScope));
                        }
                    }
                    return jsIf;
                case SPAGS.StatementType.Return:
                    SPAGS.Statement.Return spagsReturn = (SPAGS.Statement.Return)spagsStatement;
                    if (spagsReturn.Value == null)
                    {
                        if (spagsFunc.Signature.ReturnType.Category == SPAGS.ValueTypeCategory.Void)
                        {
                            return new Statement.Return();
                        }
                        else
                        {
                            return new Statement.Return(FromSPAGS(spagsFunc.Signature.ReturnType.CreateDefaultValueExpression()));
                        }
                    }
                    else
                    {
                        PossibleValueTypes returnType = GetValueTypes(spagsFunc.Signature.ReturnType);
                        return new Statement.Return(FromSPAGS(spagsReturn.Value).Cast(returnType));
                    }
                case SPAGS.StatementType.VariableDeclaration:
                    SPAGS.Statement.VariableDeclaration varDef = (SPAGS.Statement.VariableDeclaration)spagsStatement;
                    Statement.InitVariables assignment = new Statement.InitVariables();
                    foreach (SPAGS.Variable variable in varDef.Variables)
                    {
                        Variable v;
                        if (jsScope.Variables.ContainsKey(variable.Name))
                        {
                            v = jsScope.Variables[variable.Name];
                        }
                        else
                        {
                            v = new Variable(variable.Name, GetValueTypes(variable.Type));
                            jsScope.Variables.Add(variable.Name, v);
                        }
                        SetReference(variable, v);
                        if (variable.InitialValue != null)
                        {
                            PossibleValueTypes variableType = GetValueTypes(variable.Type);
                            assignment.Add(v, FromSPAGS(variable.InitialValue).Cast(variableType));
                        }
                        else
                        {
                            assignment.Add(v, FromSPAGS(variable.Type.CreateDefaultValueExpression()));
                        }
                    }
                    return assignment;
                case SPAGS.StatementType.While:
                    SPAGS.Statement.While spagsLoop = (SPAGS.Statement.While)spagsStatement;
                    Statement.While jsLoop = new Statement.While(
                        FromSPAGS(spagsLoop.WhileThisIsTrue).Cast(PossibleValueTypes.Boolean));
                    if (spagsLoop.KeepDoingThis.Type == SPAGS.StatementType.Block)
                    {
                        SPAGS.Statement.Block body = (SPAGS.Statement.Block)spagsLoop.KeepDoingThis;
                        foreach (SPAGS.Statement statement in body.ChildStatements)
                        {
                            jsLoop.KeepDoingThis.Add(FromSPAGS(spagsFunc, statement, jsScope));
                        }
                    }
                    else
                    {
                        jsLoop.KeepDoingThis.Add(FromSPAGS(spagsFunc, spagsLoop.KeepDoingThis, jsScope));
                    }
                    return jsLoop;
            }
            return (Statement)(new Expression.Custom("(" + spagsStatement.Type + ")"));
        }

        private Dictionary<SPAGS.Function, Expression> functionExpressions
            = new Dictionary<SPAGS.Function,Expression>();
        private Dictionary<SPAGS.Variable, Expression> variableExpressions
            = new Dictionary<SPAGS.Variable,Expression>();
        private Dictionary<SPAGS.ValueType.Struct, Expression> structConstructors
            = new Dictionary<SPAGS.ValueType.Struct,Expression>();

        public void AddReference(SPAGS.Function func, Expression expr)
        {
            functionExpressions.Add(func, expr);
        }

        public void AddReference(SPAGS.Variable variable, Expression expr)
        {
            variableExpressions.Add(variable, expr);
        }

        public void AddReference(SPAGS.ValueType.Struct structType, Expression expr)
        {
            structConstructors.Add(structType, expr);
        }

        public void SetReference(SPAGS.Function func, Expression expr)
        {
            functionExpressions[func] = expr;
        }

        public void SetReference(SPAGS.Variable variable, Expression expr)
        {
            variableExpressions[variable] = expr;
        }

        public void SetReference(SPAGS.ValueType.Struct structType, Expression expr)
        {
            structConstructors[structType] = expr;
        }

        private Expression GetReference(SPAGS.Function func)
        {
            if (!functionExpressions.ContainsKey(func))
            {
                return new Expression.Custom("(UNKNOWN FUNC: " + func.Name + ")");
            }
            return functionExpressions[func];
        }

        private Expression GetReference(SPAGS.Variable variable)
        {
            if (!variableExpressions.ContainsKey(variable))
            {
                return new Expression.Custom("(UNKNOWN VARIBALE: " + variable.Name + ")");
            }
            return variableExpressions[variable];
        }

        private Expression GetReference(SPAGS.ValueType.Struct structType)
        {
            if (!structConstructors.ContainsKey(structType))
            {
                return new Expression.Custom("(UNKNOWN STRUCT: " + structType.Name + ")");
            }
            return structConstructors[structType];
        }

        public Expression FromSPAGS(
            SPAGS.Function func,
            List<SPAGS.Expression> callParameters,
            List<SPAGS.Expression> callVarargs)
        {
            Expression funcRef = GetReference(func);
            List<Expression> parameters = new List<Expression>();
            for (int i = 0; i < callParameters.Count; i++)
            {
                PossibleValueTypes paramVT = GetValueTypes(func.Signature.Parameters[i].Type);
                parameters.Add(FromSPAGS(callParameters[i]).Cast(paramVT));
            }
            if (callVarargs != null && callVarargs.Count != 0)
            {
                Expression.ArrayLiteral arr = new Expression.ArrayLiteral();
                foreach (SPAGS.Expression callVararg in callVarargs)
                {
                    arr.Entries.Add(FromSPAGS(callVararg));
                }
                parameters.Add(arr);
            }
            Expression call = funcRef.Call(parameters);
            return call;
        }

        public class AllocateStringBufferExpression : Expression
        {
            public override void WriteTo(Writer writer)
            {
                writer.Write("new util.StringBuffer()");
            }
        }

        public class FillArrayExpression : Expression
        {
            public Expression Value;
            public Expression Length;
            public FillArrayExpression(Expression value, Expression length)
            {
                Value = value;
                Length = length;
            }
            public override void WriteTo(Writer writer)
            {
                writer.Write("util.fillArray(");
                Value.WriteTo(writer);
                writer.Write(", ");
                Length.WriteTo(writer);
                writer.Write(")");
            }
        }

        public class MultiplyIntExpression : Expression
        {
            public Expression Left, Right;
            public MultiplyIntExpression(Expression left, Expression right)
            {
                Left = left;
                Right = right;
            }
            public override void WriteTo(Writer writer)
            {
                writer.Write("util.imul(");
                Left.WriteTo(writer);
                writer.Write(", ");
                Right.WriteTo(writer);
                writer.Write(")");
            }
            public override PossibleValueTypes ValueTypes
            {
                get { return PossibleValueTypes.Int32; }
            }
        }

        public class StructDefinitionExpression : Expression
        {
            public ObjectLiteral InitialValues = new ObjectLiteral();
            public override void WriteTo(Writer writer)
            {
                writer.Write("new util.StructDefinition(");
                InitialValues.WriteTo(writer);
                writer.Write(")");
            }
        }

        public StructDefinitionExpression FromSPAGS(SPAGS.ValueType.Struct spagsStruct)
        {
            StructDefinitionExpression structDef = new StructDefinitionExpression();
            foreach (SPAGS.StructMember.Field field
                in spagsStruct.Members.EachOf<SPAGS.StructMember.Field>())
            {
                structDef.InitialValues.Add(
                    field.Name,
                    FromSPAGS(field.Type.CreateDefaultValueExpression()));
            }
            return structDef;
        }

        public Expression FromSPAGS(SPAGS.Expression spagsExpr)
        {
            SPAGS.Function callFunction;
            List<SPAGS.Expression> callParameters, callVarargs;
            if (spagsExpr.TryGetSimpleCall(out callFunction, out callParameters, out callVarargs))
            {
                return FromSPAGS(callFunction, callParameters, callVarargs);
            }
            switch (spagsExpr.Type)
            {
                case SPAGS.ExpressionType.AllocateArray:
                    SPAGS.Expression.AllocateArray arr = (SPAGS.Expression.AllocateArray)spagsExpr;
                    if (arr.ElementType.Category == SPAGS.ValueTypeCategory.Struct
                        && !((SPAGS.ValueType.Struct)arr.ElementType).IsManaged)
                    {
                        List<Expression> arguments = new List<Expression>();
                        arguments.Add(GetReference((SPAGS.ValueType.Struct)arr.ElementType));
                        arguments.Add(FromSPAGS(arr.Length));
                        return OtherLibraries.Util.structArray.Call(arguments);
                    }
                    else
                    {
                        return new FillArrayExpression(
                            FromSPAGS(arr.ElementType.CreateDefaultValueExpression()),
                            FromSPAGS(arr.Length));
                    }
                case SPAGS.ExpressionType.AllocStringBuffer:
                    return new AllocateStringBufferExpression();
                case SPAGS.ExpressionType.AllocStruct:
                    SPAGS.Expression.AllocateStruct allocStruct = (SPAGS.Expression.AllocateStruct)spagsExpr;
                    Expression structCtor = GetReference(allocStruct.TheStructType);
                    return new Expression.New(structCtor);
                case SPAGS.ExpressionType.ArrayIndex:
                    SPAGS.Expression.ArrayIndex arrayIndex = (SPAGS.Expression.ArrayIndex)spagsExpr;
                    return FromSPAGS(arrayIndex.Target).Index(FromSPAGS(arrayIndex.Index));
                case SPAGS.ExpressionType.BinaryOperator:
                    SPAGS.Expression.BinaryOperator spagsBinOp = (SPAGS.Expression.BinaryOperator)spagsExpr;
                    Expression left = FromSPAGS(spagsBinOp.Left);
                    Expression right = FromSPAGS(spagsBinOp.Right);
                    PossibleValueTypes mathCast;
                    if (spagsBinOp.Left.GetValueType().Category == SPAGS.ValueTypeCategory.Int)
                    {
                        mathCast = PossibleValueTypes.Int32;
                    }
                    else
                    {
                        mathCast = PossibleValueTypes.Any;
                    }
                    switch (spagsBinOp.Token.Type)
                    {
                        case SPAGS.TokenType.Add:
                            return left.BinOp(Infix.Add, right).Cast(mathCast);
                        case SPAGS.TokenType.BitwiseAnd:
                            return left.BinOp(Infix.BitwiseAnd, right);
                        case SPAGS.TokenType.BitwiseLeftShift:
                            return left.BinOp(Infix.BitwiseLeftShift, right);
                        case SPAGS.TokenType.BitwiseOr:
                            return left.BinOp(Infix.BitwiseOr, right);
                        case SPAGS.TokenType.BitwiseRightShift:
                            return left.BinOp(Infix.BitwiseSignedRightShift, right);
                        case SPAGS.TokenType.BitwiseXor:
                            return left.BinOp(Infix.BitwiseXor, right);
                        case SPAGS.TokenType.Divide:
                            return left.BinOp(Infix.Divide, right).Cast(mathCast);
                        case SPAGS.TokenType.IsEqualTo:
                            return left.BinOp(Infix.IsEqualTo, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.IsGreaterThan:
                            return left.BinOp(Infix.IsGreaterThan, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.IsGreaterThanOrEqualTo:
                            return left.BinOp(Infix.IsGreaterThanOrEqualTo, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.IsLessThan:
                            return left.BinOp(Infix.IsLessThan, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.IsLessThanOrEqualTo:
                            return left.BinOp(Infix.IsLessThanOrEqualTo, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.IsNotEqualTo:
                            return left.BinOp(Infix.IsNotEqualTo, right).Cast(PossibleValueTypes.UInt8);
                        case SPAGS.TokenType.LogicalAnd:
                            return left.BinOp(Infix.LogicalAnd, right);
                        case SPAGS.TokenType.LogicalOr:
                            return left.BinOp(Infix.LogicalOr, right);
                        case SPAGS.TokenType.Modulus:
                            return left.BinOp(Infix.Modulus, right);
                        case SPAGS.TokenType.Multiply:
                            if (mathCast == PossibleValueTypes.Int32)
                            {
                                bool useSpecialFunction = true;
                                SPAGS.ValueType leftType = spagsBinOp.Left.GetValueType();
                                SPAGS.ValueType rightType = spagsBinOp.Right.GetValueType();
                                if (leftType.IntType != "int32" || rightType.IntType != "int32")
                                {
                                    useSpecialFunction = false;
                                }
                                else
                                {
                                    int leftVal, rightVal;
                                    if (spagsBinOp.Left.TryGetIntValue(out leftVal)
                                        && (leftVal > -1000000) && (leftVal < 1000000))
                                    {
                                        useSpecialFunction = false;
                                    }
                                    if (spagsBinOp.Right.TryGetIntValue(out rightVal)
                                        && (rightVal > -1000000) && (rightVal < 1000000))
                                    {
                                        useSpecialFunction = false;
                                    }
                                }
                                if (useSpecialFunction)
                                {
                                    return new MultiplyIntExpression(left, right);
                                }
                                else
                                {
                                    return left.BinOp(Infix.Multiply, right).Cast(PossibleValueTypes.Int32);
                                }
                            }
                            return left.BinOp(Infix.Multiply, right);
                        case SPAGS.TokenType.Subtract:
                            return left.BinOp(Infix.Subtract, right.Cast(mathCast));
                        default:
                            throw new Exception("Unknown binop: " + spagsBinOp.Token);
                    }
                case SPAGS.ExpressionType.CharLiteral:
                    return (Expression)(int)((SPAGS.Expression.CharLiteral)spagsExpr).Value;
                case SPAGS.ExpressionType.Constant:
                    return FromSPAGS(((SPAGS.Expression.Constant)spagsExpr).TheConstant.TheExpression);
                case SPAGS.ExpressionType.EnumValue:
                    return (Expression)((SPAGS.Expression.EnumValue)spagsExpr).TheValue.Value;
                case SPAGS.ExpressionType.Field:
                    SPAGS.Expression.Field field = (SPAGS.Expression.Field)spagsExpr;
                    return FromSPAGS(field.Target).Index(new Expression.StringLiteral(field.TheField.Name));
                case SPAGS.ExpressionType.FloatLiteral:
                    return (Expression)((SPAGS.Expression.FloatLiteral)spagsExpr).Value;
                case SPAGS.ExpressionType.IntegerLiteral:
                    return (Expression)((SPAGS.Expression.IntegerLiteral)spagsExpr).Value;
                case SPAGS.ExpressionType.Null:
                    return Expression.Null;
                case SPAGS.ExpressionType.StringLiteral:
                    return new Expression.StringLiteral(((SPAGS.Expression.StringLiteral)spagsExpr).Value);
                case SPAGS.ExpressionType.UnaryOperator:
                    SPAGS.Expression.UnaryOperator spagsUnOp = (SPAGS.Expression.UnaryOperator)spagsExpr;
                    Expression operand = FromSPAGS(spagsUnOp.Operand);
                    switch(spagsUnOp.Token.Type)
                    {
                        case SPAGS.TokenType.Subtract:
                            if (spagsUnOp.Operand.GetValueType().Category == SPAGS.ValueTypeCategory.Int)
                            {
                                return operand.UnOp(Prefix.Negative).Cast(PossibleValueTypes.Int32);
                            }
                            else
                            {
                                return operand.UnOp(Prefix.Negative);
                            }
                        case SPAGS.TokenType.LogicalNot:
                            return operand.BinOp(Infix.IsEqualTo, (Expression)0).Cast(PossibleValueTypes.UInt8);
                        default:
                            throw new Exception("Unknown unop: " + spagsUnOp.Token.Type);
                    }
                case SPAGS.ExpressionType.Variable:
                    SPAGS.Expression.Variable spagsVar = (SPAGS.Expression.Variable)spagsExpr;
                    Expression expr = GetReference(spagsVar.TheVariable);
                    // TODO: string buffers...
                    return expr;
            }
            return new Expression.Custom("(" + spagsExpr.Type + ")");
        }

    }
}
