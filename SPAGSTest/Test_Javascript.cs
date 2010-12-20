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

        void WriteJavascript(Script script, TextWriter output, int indent, bool header)
        {
            currentScript = script;
            Indented(output, indent, "scripts[\"" + script.Name + "\"] = {");
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
                    Indented(output, indent + 1, "\"" + enumValue.Name + "\": " + enumValue.Value);
                }
            }
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
            /*
            foreach (ValueType.Struct structType in script.DefinedStructs)
            {
                string name = "STRUCT";
                if (structType.IsManaged) name = "MANAGED " + name;
                Indented(output, indent, "DEFINE " + name + " \"" + structType.Name + "\":");
                foreach (StructMember member in structType.Members.EachOf<StructMember>())
                {
                    switch (member.MemberType)
                    {
                        case StructMemberType.Attribute:
                            StructMember.Attribute attr = (StructMember.Attribute)member;
                            name = "ATTRIBUTE";
                            if (attr.IsArray)
                            {
                                name = "ARRAY " + name;
                            }
                            if (attr.IsStatic)
                            {
                                name = "STATIC " + name;
                            }
                            if (attr.IsReadOnly)
                            {
                                name = "READONLY " + name;
                            }
                            Indented(output, indent + 1, name + " \"" + attr.Name + "\":");
                            WriteValueType(attr.Type, output, indent + 2);
                            break;
                        case StructMemberType.Field:
                            StructMember.Field field = (StructMember.Field)member;
                            Indented(output, indent + 1, "FIELD \"" + field.Name + "\":");
                            WriteValueType(field.Type, output, indent + 2);
                            break;
                        case StructMemberType.Method:
                            StructMember.Method method = (StructMember.Method)member;
                            name = "METHOD";
                            if (method.IsExtender)
                            {
                                name = "EXTENDER " + name;
                            }
                            if (method.IsStatic)
                            {
                                name = "STATIC " + name;
                            }
                            Indented(output, indent + 1, name + " \"" + method.Name + "\"");
                            break;
                    }
                }
            }
            */
            foreach (Function func in script.DefinedFunctions)
            {
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
                            if (assign.Value == null)
                            {
                                switch (assign.AssignType)
                                {
                                    case TokenType.Increment:
                                        output.WriteLine(" + 1);");
                                        break;
                                    case TokenType.Decrement:
                                        output.WriteLine(" - 1);");
                                        break;
                                }
                            }
                            else
                            {
                                switch (assign.AssignType)
                                {
                                    case TokenType.AddAssign:
                                        output.Write(" + ");
                                        break;
                                    case TokenType.SubtractAssign:
                                        output.Write(" - ");
                                        break;
                                }
                                WriteExpressionJS(assign.Value, output, indent);
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
                        WriteExpressionJS(assign.Value, output, indent + 1);
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
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    if (ret.Value == null)
                    {
                        output.WriteLine("return;");
                    }
                    else
                    {
                        output.Write("return ");
                        WriteExpressionJS(ret.Value, output, indent + 1);
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
            switch (expr.Type)
            {
                case ExpressionType.AllocateArray:
                    Expression.AllocateArray allocArray = (Expression.AllocateArray)expr;
                    switch (allocArray.ElementType.Category)
                    {
                        case ValueType.ValueTypeCategory.Float:
                        case ValueType.ValueTypeCategory.Int:
                            output.Write("util.arrayZeroes(");
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
                    for (int i = 0; i < call.Parameters.Count; i++)
                    {
                        if (i != 0 || thisObj != null) output.Write(", ");
                        WriteExpressionJS(call.Parameters[i], output, indent + 1);
                    }
                    output.Write(")");
                    break;
                case ExpressionType.CharLiteral:
                    Expression.CharLiteral charLiteral = (Expression.CharLiteral)expr;
                    output.Write("'" + charLiteral.Value + "'.charCodeAt(0)");
                    break;
                case ExpressionType.Constant:
                    Expression.Constant constant = (Expression.Constant)expr;
                    WriteScriptOwnerJS(constant.TheConstant.OwnerScript, output);
                    output.Write("." + constant.TheConstant.Name);
                    break;
                case ExpressionType.EnumValue:
                    Expression.EnumValue enumValue = (Expression.EnumValue)expr;
                    if (enumValue.TheValue.Name == "true" || enumValue.TheValue.Name == "false")
                    {
                        output.Write(enumValue.TheValue.Name);
                    }
                    else
                    {
                        WriteScriptOwnerJS(enumValue.TheValue.OwnerType.OwnerScript, output);
                        output.Write("." + enumValue.TheValue.Name);
                    }
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
                case ExpressionType.StructType:
                    break;
                case ExpressionType.UnaryOperator:
                    WriteUnaryOperatorJS((Expression.UnaryOperator)expr, output, indent);
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
            if (op.Token.Type == TokenType.Divide && op.Right.GetValueType().Category == ValueType.ValueTypeCategory.Int)
            {
                output.Write("Math.floor(");
                parens = true;
            }
            else if (parens)
            {
                output.Write("(");
            }
            Expression.BinaryOperator leftBinOp = op.Left as Expression.BinaryOperator;
            if (leftBinOp != null)
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
            if (parens)
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
            WriteExpressionJS(op.Operand, output, indent + 1);
        }
    }
}
