using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Xml;
using AGS.Types;

namespace SPAGS
{
    [RequiredAGSVersion("3.2.0.0")]
    public class TestPlugin : IEditorComponent, IAGSEditorPlugin
    {

        /*** Plugin Management ***/

        const string COMPONENT = "SPAGS_Test";
        const string MENU_SPAGS = "SPAGS_Menu";
        const string COMMAND_DUMP_SCRIPTS = "DumpScripts";
        const string DUMP_FILENAME = "scripts_dump.txt";

        IAGSEditor _editor;

        public TestPlugin(IAGSEditor editor)
        {
            _editor = editor;

            _editor.GUIController.AddMenu(this, MENU_SPAGS, "SPAGS", _editor.GUIController.FileMenuID);
            MenuCommands newCommands = new MenuCommands(MENU_SPAGS);
            newCommands.Commands.Add(new MenuCommand(COMMAND_DUMP_SCRIPTS, "Dump Scripts"));
            editor.GUIController.AddMenuItems(this, newCommands);

            _editor.AddComponent(this);
        }

        public void CommandClick(string controlID)
        {
            switch (controlID)
            {
                case COMMAND_DUMP_SCRIPTS:
                    string path = Path.Combine(_editor.CurrentGame.DirectoryPath, DUMP_FILENAME);
                    using (TextWriter output = new StreamWriter(path))
                    {
                        WriteAllScripts(output);
                    }
                    MessageBox.Show(DUMP_FILENAME + " created!", "Script dump", MessageBoxButtons.OK);
                    break;
            }
        }

        public string ComponentID
        {
            get { return COMPONENT; }
        }

        /*** Output ***/

        static void Indented(TextWriter output, int indent, string line)
        {
            for (int i = 0; i < indent; i++)
            {
                output.Write("  ");
            }
            output.WriteLine(line);
        }

        void WriteAllScripts(TextWriter output)
        {
            ScriptCollection scripts = new ScriptCollection(_editor);
            foreach (Script script in scripts.YieldScripts())
            {
                WriteScript(script, output, 0);
            }
        }

        void WriteScript(Script script, TextWriter output, int indent)
        {
            Indented(output, indent, "SCRIPT \"" + script.Name + "\":");
            foreach (Function func in script.DefinedFunctions)
            {
                Indented(output, indent + 1, "DEFINE FUNCTION \"" + func.Name + "\":");
                Indented(output, indent + 2, "RETURN TYPE:");
                WriteValueType(func.Signature.ReturnType, output, indent + 3);
                Indented(output, indent + 2, "BODY:");
                WriteStatement(func.Body, output, indent + 3);
            }
            Indented(output, indent, "");
        }

        void WriteStatement(Statement stmt, TextWriter output, int indent)
        {
            switch (stmt.Type)
            {
                case StatementType.Call:
                    Statement.Call callStmt = (Statement.Call)stmt;
                    WriteExpression(callStmt.CallExpression, output, indent);
                    break;
                case StatementType.Assign:
                    Statement.Assign assign = (Statement.Assign)stmt;
                    Indented(output, indent, "ASSIGN:");
                    WriteExpression(assign.Target, output, indent + 1);
                    WriteExpression(assign.Value, output, indent + 1);
                    break;
                case StatementType.Block:
                    Statement.Block block = (Statement.Block)stmt;
                    foreach (Statement child in block.ChildStatements)
                    {
                        WriteStatement(child, output, indent + 1);
                    }
                    break;
                case StatementType.If:
                    Statement.If conditional = (Statement.If)stmt;
                    Indented(output, indent, "IF:");
                    WriteExpression(conditional.IfThisIsTrue, output, indent + 1);
                    Indented(output, indent, "THEN:");
                    WriteStatement(conditional.ThenDoThis, output, indent + 1);
                    if (conditional.ElseDoThis != null)
                    {
                        Indented(output, indent, "ELSE:");
                        WriteStatement(conditional.ElseDoThis, output, indent + 1);
                    }
                    break;
                case StatementType.Return:
                    Statement.Return ret = (Statement.Return)stmt;
                    if (ret.Value == null)
                    {
                        Indented(output, indent, "RETURN");
                    }
                    else
                    {
                        Indented(output, indent, "RETURN:");
                        WriteExpression(ret.Value, output, indent + 1);
                    }
                    break;
                case StatementType.VariableDeclaration:
                    Statement.VariableDeclaration varDef = (Statement.VariableDeclaration)stmt;
                    foreach (Variable var in varDef.Variables)
                    {
                        Indented(output, indent, "DECLARE VARIABLE \"" + var.Name + "\":");
                        Indented(output, indent+1, "TYPE:");
                        WriteValueType(var.Type, output, indent +2);
                        if (var.InitialValue != null)
                        {
                            Indented(output, indent+1, "VALUE:");
                            WriteExpression(var.InitialValue, output, indent+2);
                        }
                    }
                    break;
                case StatementType.While:
                    Statement.While loop = (Statement.While)stmt;
                    Indented(output, indent, "WHILE:");
                    WriteExpression(loop.WhileThisIsTrue, output, indent + 1);
                    Indented(output, indent, "DO:");
                    WriteStatement(loop.KeepDoingThis, output, indent + 1);
                    break;
            }
        }

        void WriteExpression(Expression expr, TextWriter output, int indent)
        {
            switch (expr.Type)
            {
                case ExpressionType.AllocateArray:
                    Expression.AllocateArray allocArray = (Expression.AllocateArray)expr;
                    Indented(output, indent, "NEW ARRAY:");
                    Indented(output, indent+1, "TYPE:");
                    WriteValueType(allocArray.ElementType, output, indent+2);
                    Indented(output, indent+1, "LENGTH:");
                    WriteExpression(allocArray.Length, output, indent+2);
                    break;
                case ExpressionType.ArrayIndex:
                    break;
                case ExpressionType.Attribute:
                    break;
                case ExpressionType.BinaryOperator:
                    WriteBinaryOperator((Expression.BinaryOperator)expr, output, indent);
                    break;
                case ExpressionType.Call:
                    Expression.Call call = (Expression.Call)expr;
                    Indented(output, indent, "CALL:");
                    WriteExpression(call.CallingOn, output, indent+1);
                    if (call.Parameters.Count != 0)
                    {
                        Indented(output, indent + 1, "PARAMETERS:");
                        foreach (Expression param in call.Parameters)
                        {
                            WriteExpression(param, output, indent + 2);
                        }
                    }
                    break;
                case ExpressionType.CharLiteral:
                    Expression.CharLiteral charLiteral = (Expression.CharLiteral)expr;
                    Indented(output, indent, "'" + charLiteral.Value + "'");
                    break;
                case ExpressionType.Constant:
                    Expression.Constant constant = (Expression.Constant)expr;
                    Indented(output, indent, "CONSTANT \"" + constant.TheConstant.Name + "\"");
                    break;
                case ExpressionType.EnumValue:
                    Expression.EnumValue enumValue = (Expression.EnumValue)expr;
                    Indented(output, indent, "ENUM \"" + enumValue.TheValue.Name + "\"");
                    break;
                case ExpressionType.Field:
                    break;
                case ExpressionType.FloatLiteral:
                    Expression.FloatLiteral floatLiteral = (Expression.FloatLiteral)expr;
                    Indented(output, indent, String.Format("{0.0}", floatLiteral.Value));
                    break;
                case ExpressionType.Function:
                    Expression.Function func = (Expression.Function)expr;
                    Indented(output, indent, "FUNCTION \"" + func.TheFunction.Name + "\"");
                    break;
                case ExpressionType.IntegerLiteral:
                    Expression.IntegerLiteral intLiteral = (Expression.IntegerLiteral)expr;
                    Indented(output, indent, intLiteral.Value.ToString());
                    break;
                case ExpressionType.Method:
                    break;
                case ExpressionType.Null:
                    Indented(output, indent, "NULL");
                    break;
                case ExpressionType.StringLiteral:
                    Expression.StringLiteral stringLiteral = (Expression.StringLiteral)expr;
                    Indented(output, indent, "\"" + stringLiteral.Value.Replace("\"", "\\\"") + "\"");
                    break;
                case ExpressionType.StructType:
                    break;
                case ExpressionType.UnaryOperator:
                    WriteUnaryOperator((Expression.UnaryOperator)expr, output, indent);
                    break;
                case ExpressionType.Variable:
                    Expression.Variable var = (Expression.Variable)expr;
                    Indented(output, indent, "VARIABLE \"" + var.TheVariable.Name + "\"");
                    break;
            }
        }

        void WriteBinaryOperator(Expression.BinaryOperator op, TextWriter output, int indent)
        {
            switch (op.Token.Type)
            {
                case TokenType.Add:
                    Indented(output, indent, "ADD:");
                    break;
                case TokenType.BitwiseAnd:
                    Indented(output, indent, "BITWISE AND:");
                    break;
                case TokenType.BitwiseLeftShift:
                    Indented(output, indent, "BITWISE LEFT SHIFT:");
                    break;
                case TokenType.BitwiseOr:
                    Indented(output, indent, "BITWISE OR:");
                    break;
                case TokenType.BitwiseRightShift:
                    Indented(output, indent, "BITWISE RIGHT SHIFT:");
                    break;
                case TokenType.BitwiseXor:
                    Indented(output, indent, "BITWISE EXCLUSIVE OR:");
                    break;
                case TokenType.Divide:
                    Indented(output, indent, "DIVIDE:");
                    break;
                case TokenType.IsEqualTo:
                    Indented(output, indent, "EQUALS:");
                    break;
                case TokenType.IsGreaterThan:
                    Indented(output, indent, "GREATER THAN:");
                    break;
                case TokenType.IsGreaterThanOrEqualTo:
                    Indented(output, indent, "GREATER OR EQUAL:");
                    break;
                case TokenType.IsLessThan:
                    Indented(output, indent, "LESS THAN:");
                    break;
                case TokenType.IsLessThanOrEqualTo:
                    Indented(output, indent, "LESS OR EQUAL:");
                    break;
                case TokenType.IsNotEqualTo:
                    Indented(output, indent, "NOT EQUAL:");
                    break;
                case TokenType.LogicalAnd:
                    Indented(output, indent, "AND:");
                    break;
                case TokenType.LogicalOr:
                    Indented(output, indent, "OR:");
                    break;
                case TokenType.Modulus:
                    Indented(output, indent, "MODULUS:");
                    break;
                case TokenType.Multiply:
                    Indented(output, indent, "MULTIPLY:");
                    break;
                case TokenType.Subtract:
                    Indented(output, indent, "SUBTRACT:");
                    break;
            }
            WriteExpression(op.Left, output, indent + 1);
            WriteExpression(op.Right, output, indent + 1);
        }

        void WriteUnaryOperator(Expression.UnaryOperator op, TextWriter output, int indent)
        {
            switch (op.Token.Type)
            {
                case TokenType.LogicalNot:
                    Indented(output, indent, "NOT:");
                    break;
                case TokenType.Subtract:
                    Indented(output, indent, "NEGATIVE:");
                    break;
            }
            WriteExpression(op.Operand, output, indent + 1);
        }

        void WriteValueType(ValueType valueType, TextWriter output, int indent)
        {
            switch (valueType.Category)
            {
                case ValueType.ValueTypeCategory.Array:
                    ArrayType arrayType = (ArrayType)valueType;
                    if (arrayType.LengthExpression == null)
                    {
                        Indented(output, indent, "DYNAMIC ARRAY:");
                        Indented(output, indent + 1, "TYPE:");
                        WriteValueType(arrayType.ElementType, output, indent + 1);
                    }
                    else
                    {
                        Indented(output, indent, "STATIC ARRAY:");
                        Indented(output, indent + 1, "TYPE:");
                        WriteValueType(arrayType.ElementType, output, indent + 1);
                        Indented(output, indent + 1, "LENGTH:");
                        WriteExpression(arrayType.LengthExpression, output, indent + 1);
                    }
                    break;
                case ValueType.ValueTypeCategory.Float:
                    Indented(output, indent, "FLOAT");
                    break;
                case ValueType.ValueTypeCategory.FunctionSignature:
                    Indented(output, indent, "FUNCTION SIGNATURE");
                    break;
                case ValueType.ValueTypeCategory.Int:
                    Indented(output, indent, "INT");
                    break;
                case ValueType.ValueTypeCategory.Null:
                    Indented(output, indent, "NULL");
                    break;
                case ValueType.ValueTypeCategory.StringBuffer:
                    Indented(output, indent, "STRING BUFFER");
                    break;
                case ValueType.ValueTypeCategory.StringValue:
                    Indented(output, indent, "STRING");
                    break;
                case ValueType.ValueTypeCategory.Struct:
                    StructType structType = (StructType)valueType;
                    Indented(output, indent, "STRUCT \"" + structType.Name + "\"");
                    break;
                case ValueType.ValueTypeCategory.Void:
                    Indented(output, indent, "VOID");
                    break;
            }
        }

        #region Stubs

        public void EditorShutdown()
        {
        }

        public void FromXml(XmlNode node)
        {
        }

        public void GameSettingsChanged()
        {
        }

        public IList<MenuCommand> GetContextMenu(string controlID)
        {
            List<MenuCommand> items = new List<MenuCommand>();
            return items;
        }

        public void PropertyChanged(string propertyName, object oldValue)
        {
        }

        public void RefreshDataFromGame()
        {
        }

        public void ToXml(XmlTextWriter writer)
        {
        }

        public void Dispose()
        {
        }

        public void BeforeSaveGame()
        {
        }
        #endregion
    }
}
