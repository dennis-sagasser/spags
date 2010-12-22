using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using AGS.Types;

namespace SPAGS
{
    public partial class TestPlugin : IEditorComponent, IAGSEditorPlugin
    {
        void WriteJavascripts(ScriptCollection scripts, TextWriter output)
        {
            foreach (Script header in scripts.Headers)
            {
                WriteJavascript(header, output, 0, true);
            }
            Script globVarsScript = scripts.CompileGlobalVariablesScript(_editor);
            if (globVarsScript != null)
            {
                WriteJavascript(globVarsScript, output, 0, false);
            }
            foreach (AGS.Types.Script script in _editor.CurrentGame.Scripts)
            {
                if (!script.IsHeader)
                {
                    WriteJavascript(scripts.CompileScript(script.FileName, script.Text), output, 0, false);
                }
            }
            WriteJavascript(scripts.CompileDialogScript(_editor), output, 0, false);
            foreach (IRoom unloadedRoom in _editor.CurrentGame.Rooms)
            {
                WriteJavascript(scripts.CompileRoomScript(_editor, unloadedRoom.Number), output, 0, false);
            }
        }

        Script currentScript;
        Function currentFunction;

        void WriteJavascript(Script script, TextWriter output, int indent, bool header)
        {
            currentScript = script;
            Indented(output, indent, "scripts[\"" + script.Name + "\"] = {");
            /*
            foreach (Constant constant in script.DefinedConstants)
            {
                if (constant.Undefined) continue;
                if (constant is Constant.Expression)
                {
                    Constant.Expression constantExpr = (Constant.Expression)constant;
                    output.Write("  \"" + constantExpr.Name + "\": ");
                    WriteExpressionJS(constantExpr.TheExpression, output, indent + 1);
                    output.WriteLine(",");
                }
            }
            foreach (ValueType.Enum enumType in script.DefinedEnums)
            {
                if (enumType.Name == "bool") continue;
                foreach (EnumValue enumValue in enumType.Entries)
                {
                    Indented(output, indent + 1, "\"" + enumValue.Name + "\": " + enumValue.Value + ",");
                }
            }
             */
            foreach (Variable var in script.DefinedVariables)
            {
                output.Write("  \"" + var.Name + "\": ");
                if (var.InitialValue == null)
                {
                    WriteExpressionJS(var.Type.CreateDefaultValueExpression(), output, indent);
                }
                else
                {
                    WriteExpressionJS(var.InitialValue, output, indent + 1);
                }
                output.WriteLine(",");
            }
            foreach (ValueType.Struct structType in script.DefinedStructs)
            {
                if (structType.IsManaged) continue;
                output.WriteLine("  \"" + structType.Name + "\": util.createStruct(function() {");
                foreach (StructMember.Field field in structType.Members.EachOf<StructMember.Field>())
                {
                    switch (field.Type.Category)
                    {
                        case ValueTypeCategory.Array:
                            ValueType.Array arrayType = (ValueType.Array)field.Type;
                            output.Write("      this." + field.Name + " = ");
                            switch (arrayType.ElementType.Category)
                            {
                                case ValueTypeCategory.Float:
                                case ValueTypeCategory.Int:
                                    output.Write("util.arrayZeroes(");
                                    break;
                                default:
                                    output.Write("util.arrayNulls(");
                                    break;
                            }
                            WriteExpressionJS(arrayType.LengthExpression, output, indent);
                            output.WriteLine(");");
                            break;
                    }
                }
                output.WriteLine("    }, {");
                List<StructMember.Field> fields = new List<StructMember.Field>();
                foreach (StructMember.Field field in structType.Members.EachOf<StructMember.Field>())
                {
                    fields.Add(field);
                }
                for(int i = 0; i < fields.Count; i++)
                {
                    StructMember.Field field = fields[i];
                    switch (field.Type.Category)
                    {
                        case ValueTypeCategory.Int:
                        case ValueTypeCategory.Float:
                        case ValueTypeCategory.Struct:
                        case ValueTypeCategory.StringValue:
                        case ValueTypeCategory.StringBuffer:
                            output.Write("      \"" + field.Name + "\": ");
                            WriteExpressionJS(field.Type.CreateDefaultValueExpression(), output, indent);
                            if (i == fields.Count - 1)
                            {
                                output.WriteLine();
                            }
                            else
                            {
                                output.WriteLine(",");
                            }
                            break;
                    }
                }
                output.WriteLine("    }),");
            }
            foreach (Function func in script.DefinedFunctions)
            {
                currentFunction = func;
                output.Write("  \"" + func.Name.Replace("::","$") + "\": function(");
                for (int i = 0; i < func.Signature.Parameters.Count; i++)
                {
                    if (i != 0) output.Write(", ");
                    output.Write(func.Signature.Parameters[i].Name);
                }
                output.WriteLine(") {");
                foreach (Statement stmt in func.Body.ChildStatements)
                {
                    Indent(output, indent + 2);
                    WriteStatementJS(stmt, output, indent + 2);
                }
                if (func.Signature.ReturnType.Category != ValueTypeCategory.Void
                    && !func.Body.Returns())
                {
                    output.Write("    return ");
                    WriteExpressionJS(func.Signature.ReturnType.CreateDefaultValueExpression(), output, indent);
                    output.WriteLine(";");
                }
                output.WriteLine("  },");
            }

            output.WriteLine("  \"\": null");

            Indented(output, indent, "};");
        }

        void WriteStatementJS(Statement stmt, TextWriter output, int indent)
        {
            switch (stmt.Type)
            {
                case StatementType.Call:
                    Statement.Call callStmt = (Statement.Call)stmt;
                    WriteExpressionJS(callStmt.CallExpression, output, indent);
                    output.WriteLine(";");
                    break;
                case StatementType.Assign:
                    Statement.Assign assign = (Statement.Assign)stmt;

                    Expression.Attribute attr = assign.Target as Expression.Attribute;
                    Expression.ArrayIndex index = assign.Target as Expression.ArrayIndex;
                    if (index != null)
                    {
                        attr = index.Target as Expression.Attribute;
                    }
                    if (attr != null)
                    {
                        WriteFunctionJS(attr.TheAttribute.Setter, output);
                        output.Write("(");
                        if (attr.Target != null)
                        {
                            WriteExpressionJS(attr.Target, output, indent);
                            output.Write(", ");
                        }
                        if (index != null)
                        {
                            WriteExpressionJS(index.Index, output, indent);
                            output.Write(", ");
                        }
                        if (assign.AssignType == TokenType.Assign)
                        {
                            WriteExpressionJS(assign.Value, output, indent);
                            output.WriteLine(");");
                        }
                        else
                        {
                            WriteFunctionJS(attr.TheAttribute.Getter, output);
                            output.Write("(");
                            bool values = false;
                            if (attr.Target != null)
                            {
                                values = true;
                                WriteExpressionJS(attr.Target, output, indent);
                            }
                            if (index != null)
                            {
                                if (values)
                                {
                                    output.Write(", ");
                                }
                                WriteExpressionJS(index.Index, output, indent);
                            }
                            if (assign.Value == null)
                            {
                                switch (assign.AssignType)
                                {
                                    case TokenType.Increment:
                                        output.WriteLine(") + 1);");
                                        break;
                                    case TokenType.Decrement:
                                        output.WriteLine(") - 1);");
                                        break;
                                }
                            }
                            else
                            {

                                switch (assign.AssignType)
                                {
                                    case TokenType.AddAssign:
                                        output.Write(") + ");
                                        break;
                                    case TokenType.SubtractAssign:
                                        output.Write(") - ");
                                        break;
                                }
                                WriteExpressionJS(assign.Value, output, indent, attr.TheAttribute.Type);
                                output.WriteLine(");");
                            }
                        }
                        break;
                    }

                    if (assign.Value == null)
                    {
                        WriteExpressionJS(assign.Target, output, indent + 1);
                        switch (assign.AssignType)
                        {
                            case TokenType.Increment:
                                output.WriteLine("++;");
                                break;
                            case TokenType.Decrement:
                                output.WriteLine("--;");
                                break;
                            default:
                                throw new Exception("Unexpected AssignType: " + assign.AssignType);
                        }
                    }
                    else
                    {
                        WriteExpressionJS(assign.Target, output, indent + 1);
                        switch (assign.AssignType)
                        {
                            case TokenType.Assign:
                                output.Write(" = ");
                                break;
                            case TokenType.AddAssign:
                                output.Write(" -= ");
                                break;
                            case TokenType.SubtractAssign:
                                output.Write(" += ");
                                break;
                            default:
                                throw new Exception("Unexpected AssignType: " + assign.AssignType);
                        }
                        WriteExpressionJS(assign.Value, output, indent + 1, assign.Target.GetValueType());
                        output.WriteLine(";");
                    }
                    break;
                case StatementType.Block:
                    Statement.Block block = (Statement.Block)stmt;
                    if (block.ChildStatements.Count == 0)
                    {
                        output.WriteLine("{ }");
                    }
                    else
                    {
                        output.WriteLine("{");
                        foreach (Statement child in block.ChildStatements)
                        {
                            Indent(output, indent + 1);
                            WriteStatementJS(child, output, indent + 1);
                        }
                        Indented(output, indent, "}");
                    }
                    break;
                case StatementType.If:
                    Statement.If conditional = (Statement.If)stmt;
                    Expression switchTest;
                    List<Expression> switchValues;
                    List<Statement> switchCases;
                    Statement switchDefault;
                    if (TryGetSwitchBlock(conditional, out switchTest, out switchValues, out switchCases, out switchDefault))
                    {
                        output.Write("switch (");
                        WriteExpressionJS(switchTest, output, indent);
                        output.WriteLine(") {");
                        for (int i = 0; i < switchCases.Count; i++)
                        {
                            Indent(output, indent + 1);
                            output.Write("case ");
                            WriteExpressionJS(switchValues[i], output, indent + 1);
                            output.Write(":");
                            Statement.Block caseBlock = switchCases[i] as Statement.Block;
                            if (caseBlock == null)
                            {
                                output.WriteLine();
                                Indent(output, indent + 2);
                                WriteStatementJS(switchCases[i], output, indent + 2);
                            }
                            else if (caseBlock.Scope.Count != 0)
                            {
                                output.Write(" ");
                                WriteStatementJS(switchCases[i], output, indent + 2);
                            }
                            else
                            {
                                output.WriteLine();
                                foreach (Statement caseStmt in caseBlock.ChildStatements)
                                {
                                    Indent(output, indent + 2);
                                    WriteStatementJS(caseStmt, output, indent + 2);
                                }
                            }
                            if (!switchCases[i].Returns())
                            {
                                Indent(output, indent + 2);
                                output.WriteLine("break;");
                            }
                        }
                        if (switchDefault != null)
                        {
                            Indent(output, indent + 1);
                            output.Write("default: ");

                            Statement.Block defaultBlock = switchDefault as Statement.Block;
                            if (defaultBlock == null)
                            {
                                output.WriteLine();
                                Indent(output, indent + 2);
                                WriteStatementJS(switchDefault, output, indent + 2);
                            }
                            else if (defaultBlock.Scope.Count != 0)
                            {
                                output.Write(" ");
                                WriteStatementJS(switchDefault, output, indent + 2);
                            }
                            else
                            {
                                output.WriteLine();
                                foreach (Statement caseStmt in defaultBlock.ChildStatements)
                                {
                                    Indent(output, indent + 2);
                                    WriteStatementJS(caseStmt, output, indent + 2);
                                }
                            }
                            if (!switchDefault.Returns())
                            {
                                Indent(output, indent + 2);
                                output.WriteLine("break;");
                            }
                        }
                        Indent(output, indent);
                        output.WriteLine("}");
                    }
                    else
                    {
                        output.Write("if (");
                        WriteExpressionJS(conditional.IfThisIsTrue, output, indent);
                        output.Write(") ");
                        WriteStatementJS(conditional.ThenDoThis, output, indent);
                        if (conditional.ElseDoThis != null)
                        {
                            Indent(output, indent);
                            output.Write("else ");
                            WriteStatementJS(conditional.ElseDoThis, output, indent);
                        }
                    }
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    if (ret.Value == null)
                    {
                        ValueType returnType = currentFunction.Signature.ReturnType;
                        if (returnType.Category != ValueTypeCategory.Void)
                        {
                            output.Write("return ");
                            WriteExpressionJS(returnType.CreateDefaultValueExpression(), output, indent);
                            output.WriteLine(";");
                        }
                        else
                        {
                            output.WriteLine("return;");
                        }
                    }
                    else
                    {
                        output.Write("return ");
                        WriteExpressionJS(ret.Value, output, indent + 1, currentFunction.Signature.ReturnType);
                        output.WriteLine(";");
                    }
                    break;
                case StatementType.VariableDeclaration:
                    Statement.VariableDeclaration varDef = (Statement.VariableDeclaration)stmt;
                    output.Write("var ");
                    for (int i = 0; i < varDef.Variables.Count; i++)
                    {
                        if (i > 0) output.Write(", ");
                        Variable var = varDef.Variables[i];
                        output.Write(var.Name + " = ");
                        if (var.InitialValue == null)
                        {
                            WriteExpressionJS(var.Type.CreateDefaultValueExpression(), output, indent+1);
                        }
                        else
                        {
                            WriteExpressionJS(var.InitialValue, output, indent + 1);
                        }
                    }
                    output.WriteLine(";");
                    break;
                case StatementType.While:
                    Statement.While loop = (Statement.While)stmt;
                    output.Write("while (");
                    WriteExpressionJS(loop.WhileThisIsTrue, output, indent);
                    output.Write(") ");
                    WriteStatementJS(loop.KeepDoingThis, output, indent);
                    break;
            }
        }

        const string EXTRAS_OBJECT = "engine";

        void WriteScriptOwnerJS(Script script, TextWriter output)
        {
            if (script == null)
            {
                output.Write(EXTRAS_OBJECT);
            }
            else
            {
                output.Write("scripts[\"" + script.Name + "\"]");
            }
        }

        void WriteFunctionJS(Function function, TextWriter output)
        {
            WriteScriptOwnerJS(function.OwnerScript, output);
            output.Write("." + function.Name.Replace("::","$"));
        }

        void WriteExpressionJS(Expression expr, TextWriter output, int indent)
        {
            WriteExpressionJS(expr, output, indent, null);
        }
        void WriteExpressionJS(Expression expr, TextWriter output, int indent, ValueType expectedType)
        {
            if (expectedType != null)
            {
                ValueType exprValType = expr.GetValueType();
                if (expectedType.IsInternalString
                    && exprValType.Category == ValueTypeCategory.StringBuffer)
                {
                    WriteExpressionJS(expr, output, indent, null);
                    output.Write(".value");
                    return;
                }
                if (expectedType.Category == ValueTypeCategory.Int)
                {
                    int value;
                    bool constantValue = expr.TryGetIntValue(out value);
                    if (expectedType.Name == "short" && exprValType.Name == "int"
                        && !(constantValue && value == ((int)(short)value)))
                    {
                        output.Write("((");
                        WriteExpressionJS(expr, output, indent, null);
                        output.Write(") << 16 >> 16)");
                        return;
                    }
                    else if (expectedType.Name == "char" && exprValType.Name != "char"
                        && !(constantValue && value==((int)(char)value)))
                    {
                        output.Write("((");
                        WriteExpressionJS(expr, output, indent, null);
                        output.Write(") & 0xff)");
                        return;
                    }
                }
            }
            switch (expr.Type)
            {
                case ExpressionType.AllocateArray:
                    Expression.AllocateArray allocArray = (Expression.AllocateArray)expr;
                    switch (allocArray.ElementType.Category)
                    {
                        case ValueTypeCategory.Float:
                        case ValueTypeCategory.Int:
                            output.Write("util.arrayZeroes(");
                            break;
                        case ValueTypeCategory.Struct:
                            ValueType.Struct structType = (ValueType.Struct)allocArray.ElementType;
                            if (structType.IsManaged) goto default;
                            WriteScriptOwnerJS(structType.OwnerScript, output);
                            output.Write("." + structType.Name + ".createArray(");
                            break;
                        default:
                            output.Write("util.arrayNulls(");
                            break;
                    }
                    WriteExpressionJS(allocArray.Length, output, indent + 2);
                    output.Write(")");
                    break;
                case ExpressionType.ArrayIndex:
                    Expression.ArrayIndex arrayIndex = (Expression.ArrayIndex)expr;
                    Expression.Attribute indexAttr = arrayIndex.Target as Expression.Attribute;
                    if (indexAttr != null && indexAttr.TheAttribute.IsArray)
                    {
                        WriteFunctionJS(indexAttr.TheAttribute.Getter, output);
                        output.Write("(");
                        if (indexAttr.Target != null)
                        {
                            WriteExpressionJS(indexAttr.Target, output, indent);
                            output.Write(", ");
                        }
                        WriteExpressionJS(arrayIndex.Index, output, indent + 1);
                        output.Write(")");
                    }
                    else
                    {
                        WriteExpressionJS(arrayIndex.Target, output, indent + 1);
                        output.Write("[");
                        WriteExpressionJS(arrayIndex.Index, output, indent + 1);
                        output.Write("]");
                    }
                    break;
                case ExpressionType.Attribute:
                    Expression.Attribute attr = (Expression.Attribute)expr;
                    WriteFunctionJS(attr.TheAttribute.Getter, output);
                    if (attr.Target == null)
                    {
                        output.Write("()");
                    }
                    else
                    {
                        output.Write("(");
                        WriteExpressionJS(attr.Target, output, indent + 1);
                        output.Write(")");
                    }
                    break;
                case ExpressionType.BinaryOperator:
                    WriteBinaryOperatorJS((Expression.BinaryOperator)expr, output, indent, false);
                    break;
                case ExpressionType.Call:
                    Expression.Call call = (Expression.Call)expr;
                    WriteExpressionJS(call.CallingOn, output, indent + 1);
                    output.Write("(");
                    Expression.Method callingMethod = call.CallingOn as Expression.Method;
                    Expression thisObj = (callingMethod == null) ? null : callingMethod.Target;
                    if (thisObj != null)
                    {
                        WriteExpressionJS(thisObj, output, indent + 1);
                    }
                    ValueType.FunctionSignature signature = call.CallingOn.GetValueType() as ValueType.FunctionSignature;
                    if (signature == null)
                    {
                        throw new Exception("Calling on a non-callable object: " + call.CallingOn.ToString());
                    }
                    for (int i = 0; i < call.Parameters.Count; i++)
                    {
                        if (i != 0 || thisObj != null) output.Write(", ");
                        if (i < signature.Parameters.Count)
                        {
                            WriteExpressionJS(call.Parameters[i], output, indent + 1, signature.Parameters[i].Type);
                        }
                        else
                        {
                            WriteExpressionJS(call.Parameters[i], output, indent + 1);
                        }
                    }
                    output.Write(")");
                    break;
                case ExpressionType.CharLiteral:
                    Expression.CharLiteral charLiteral = (Expression.CharLiteral)expr;
                    output.Write((int)charLiteral.Value);
                    switch (charLiteral.Value)
                    {
                        case '\r':
                            output.Write(" /* '\\r' */");
                            break;
                        case '\n':
                            output.Write(" /* '\\n' */");
                            break;
                        case '\t':
                            output.Write(" /* '\\t' */");
                            break;
                        case '\'':
                            output.Write(" /* '\\'' */");
                            break;
                        case '\\':
                            output.Write(" /* '\\\\' */");
                            break;
                        default:
                            if ((int)charLiteral.Value < 32 || (int)charLiteral.Value >= 127)
                            {
                                output.Write((int)charLiteral.Value);
                            }
                            else
                            {
                                output.Write(" /* '" + charLiteral.Value + "' */");
                            }
                            break;
                    }
                    break;
                case ExpressionType.Constant:
                    Expression.Constant constant = (Expression.Constant)expr;
                    WriteExpressionJS(constant.TheConstant.TheExpression, output, indent);
                    output.Write(" /* " + constant.TheConstant.Name + " */");
                    break;
                case ExpressionType.EnumValue:
                    Expression.EnumValue enumValue = (Expression.EnumValue)expr;
                    output.Write(enumValue.TheValue.Value + " /* "+ enumValue.TheValue.Name + " */");
                    break;
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    WriteExpressionJS(field.Target, output, indent + 1);
                    output.Write("." + field.TheField.Name);
                    break;
                case ExpressionType.FloatLiteral:
                    Expression.FloatLiteral floatLiteral = (Expression.FloatLiteral)expr;
                    output.Write(String.Format("{0:0.0}", floatLiteral.Value));
                    break;
                case ExpressionType.Function:
                    Expression.Function func = (Expression.Function)expr;
                    WriteFunctionJS(func.TheFunction, output);
                    break;
                case ExpressionType.IntegerLiteral:
                    Expression.IntegerLiteral intLiteral = (Expression.IntegerLiteral)expr;
                    output.Write(intLiteral.Value.ToString());
                    break;
                case ExpressionType.Method:
                    Expression.Method method = (Expression.Method)expr;
                    WriteFunctionJS(method.TheMethod.Function, output);
                    break;
                case ExpressionType.Null:
                    output.Write("null");
                    break;
                case ExpressionType.StringLiteral:
                    Expression.StringLiteral stringLiteral = (Expression.StringLiteral)expr;
                    output.Write("\"" + stringLiteral.Value.Replace("\"", "\\\"") + "\"");
                    break;
                case ExpressionType.UnaryOperator:
                    WriteUnaryOperatorJS((Expression.UnaryOperator)expr, output, indent);
                    break;
                case ExpressionType.AllocStringBuffer:
                    output.Write("new util.StringBuffer()");
                    break;
                case ExpressionType.AllocStruct:
                    Expression.AllocateStruct newStruct = (Expression.AllocateStruct)expr;
                    output.Write("new ");
                    WriteScriptOwnerJS(newStruct.TheStructType.OwnerScript, output);
                    output.Write("." + newStruct.TheStructType.Name + "()");
                    break;
                case ExpressionType.Variable:
                    Expression.Variable var = (Expression.Variable)expr;
                    Variable v = var.TheVariable;
                    if (v is ScriptVariable)
                    {
                        WriteScriptOwnerJS(v.OwnerScript, output);
                        output.Write("." + var.TheVariable.Name);
                    }
                    else
                    {
                        output.Write(var.TheVariable.Name);
                    }
                    break;
                default:
                    Indented(output, indent, expr.ToString());
                    break;
            }
        }

        void WriteBinaryOperatorJS(Expression.BinaryOperator op, TextWriter output, int indent, bool parens)
        {
            bool forceInt = false;
            if ((op.Token.Type == TokenType.Divide || op.Token.Type == TokenType.Add || op.Token.Type == TokenType.Subtract
                || op.Token.Type == TokenType.Multiply)
                && op.Right.GetValueType().Category == ValueTypeCategory.Int)
            {
                output.Write("((");
                forceInt = true;
            }
            else if (parens)
            {
                output.Write("(");
            }
            Expression.BinaryOperator leftBinOp = op.Left as Expression.BinaryOperator;
            if (leftBinOp != null && leftBinOp.Token.Type != op.Token.Type)
            {
                WriteBinaryOperatorJS(leftBinOp, output, indent, true);
            }
            else
            {
                WriteExpressionJS(op.Left, output, indent + 1);
            }
            switch (op.Token.Type)
            {
                case TokenType.Add:
                    output.Write(" + ");
                    break;
                case TokenType.BitwiseAnd:
                    output.Write(" & ");
                    break;
                case TokenType.BitwiseLeftShift:
                    output.Write(" << ");
                    break;
                case TokenType.BitwiseOr:
                    output.Write(" | ");
                    break;
                case TokenType.BitwiseRightShift:
                    output.Write(" >> ");
                    break;
                case TokenType.BitwiseXor:
                    output.Write(" ^ ");
                    break;
                case TokenType.Divide:
                    output.Write(" / ");
                    break;
                case TokenType.IsEqualTo:
                    output.Write(" === ");
                    break;
                case TokenType.IsGreaterThan:
                    output.Write(" > ");
                    break;
                case TokenType.IsGreaterThanOrEqualTo:
                    output.Write(" >= ");
                    break;
                case TokenType.IsLessThan:
                    output.Write(" < ");
                    break;
                case TokenType.IsLessThanOrEqualTo:
                    output.Write(" <= ");
                    break;
                case TokenType.IsNotEqualTo:
                    output.Write(" !== ");
                    break;
                case TokenType.LogicalAnd:
                    output.Write(" && ");
                    break;
                case TokenType.LogicalOr:
                    output.Write(" || ");
                    break;
                case TokenType.Modulus:
                    output.Write(" % ");
                    break;
                case TokenType.Multiply:
                    output.Write(" * ");
                    break;
                case TokenType.Subtract:
                    output.Write(" - ");
                    break;
            }
            Expression.BinaryOperator rightBinOp = op.Right as Expression.BinaryOperator;
            if (rightBinOp != null)
            {
                WriteBinaryOperatorJS(rightBinOp, output, indent, true);
            }
            else
            {
                WriteExpressionJS(op.Right, output, indent + 1);
            }
            if (forceInt)
            {
                output.Write(") | 0)");
            }
            else if (parens)
            {
                output.Write(")");
            }
        }

        void WriteUnaryOperatorJS(Expression.UnaryOperator op, TextWriter output, int indent)
        {
            switch (op.Token.Type)
            {
                case TokenType.LogicalNot:
                    output.Write("!");
                    break;
                case TokenType.Subtract:
                    output.Write("-");
                    break;
            }
            bool parens = (op.Operand is Expression.BinaryOperator);
            if (parens) output.Write("(");
            WriteExpressionJS(op.Operand, output, indent + 1);
            if (parens) output.Write(")");
        }
        bool StaticValue(Expression expr)
        {
            return (expr.Type == ExpressionType.Variable);
        }
        bool TryGetSwitchBlock(Statement.If ifBlock,
            out Expression testExpr, out List<Expression> values,
            out List<Statement> cases, out Statement defaultCase)
        {
            testExpr = null;
            values = null;
            cases = null;
            defaultCase = null;

            Expression.BinaryOperator binop = ifBlock.IfThisIsTrue as Expression.BinaryOperator;
            if (!(ifBlock.ElseDoThis is Statement.If) || binop == null || binop.Token.Type != TokenType.IsEqualTo
                || !StaticValue(binop.Left) || !binop.Right.IsConstant()) return false;

            testExpr = binop.Left;
            values = new List<Expression>();
            values.Add(binop.Right);
            cases = new List<Statement>();
            cases.Add(ifBlock.ThenDoThis);

            for (Statement otherCase = ifBlock.ElseDoThis; otherCase != null; otherCase = ifBlock.ElseDoThis)
            {
                ifBlock = otherCase as Statement.If;
                if (ifBlock == null)
                {
                    defaultCase = otherCase;
                    break;
                }
                binop = ifBlock.IfThisIsTrue as Expression.BinaryOperator;
                if (binop == null || !binop.Left.Equals(testExpr) || !binop.Right.IsConstant())
                {
                    return false;
                }
                values.Add(binop.Right);
                cases.Add(ifBlock.ThenDoThis);
            }

            return true;
        }
    }
}
