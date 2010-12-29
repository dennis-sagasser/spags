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
    public partial class TestPlugin : IEditorComponent, IAGSEditorPlugin
    {

        /*** Plugin Management ***/

        const string COMPONENT = "SPAGS_Test";
        const string MENU_SPAGS = "SPAGS_Menu";
        const string COMMAND_DUMP_SCRIPTS = "DumpScripts";
        const string COMMAND_JAVASCRIPT = "Javascript";
        const string DUMP_FILENAME = "scripts_dump.txt";
        const string JS_FILENAME = "scripts_dump.js";

        IAGSEditor _editor;

        public TestPlugin(IAGSEditor editor)
        {
            _editor = editor;

            _editor.GUIController.AddMenu(this, MENU_SPAGS, "SPAGS", _editor.GUIController.FileMenuID);
            MenuCommands newCommands = new MenuCommands(MENU_SPAGS);
            newCommands.Commands.Add(new MenuCommand(COMMAND_DUMP_SCRIPTS, "Dump Scripts"));
            newCommands.Commands.Add(new MenuCommand(COMMAND_JAVASCRIPT, "Dump Javascript (experimental!)"));
            editor.GUIController.AddMenuItems(this, newCommands);

            _editor.AddComponent(this);
        }

        public void CommandClick(string controlID)
        {
            switch (controlID)
            {
                case COMMAND_DUMP_SCRIPTS:
                    foreach (Dialog dialog in _editor.CurrentGame.Dialogs)
                    {
                        if (String.IsNullOrEmpty(dialog.CachedConvertedScript))
                        {
                            MessageBox.Show("Please rebuild the game (e.g. by pressing F7) and try again.");
                            return;
                        }
                    }

                    try
                    {
                        string path = Path.Combine(_editor.CurrentGame.DirectoryPath, DUMP_FILENAME);
                        ScriptCollection scripts = new ScriptCollection(_editor.Version);
                        scripts.SetStandardConstants(_editor.CurrentGame.Settings);
                        scripts.AddStandardHeaders(_editor);
                        using (TextWriter output = new StreamWriter(path))
                        {
                            WriteAllScripts(scripts, output);
                            WriteGlobalNamespace(scripts, output);
                        }
                        MessageBox.Show(DUMP_FILENAME + " created!", "Script dump", MessageBoxButtons.OK);
                    }
                    catch (SPAGS.Util.EditorUsageException ex)
                    {
                        MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                    break;

                case COMMAND_JAVASCRIPT:
                    foreach (Dialog dialog in _editor.CurrentGame.Dialogs)
                    {
                        if (String.IsNullOrEmpty(dialog.CachedConvertedScript))
                        {
                            MessageBox.Show("Please rebuild the game (e.g. by pressing F7) and try again.");
                            return;
                        }
                    }

                    try
                    {
                        string path = Path.Combine(_editor.CurrentGame.DirectoryPath, JS_FILENAME);
                        ScriptCollection scripts = new ScriptCollection(_editor.Version);
                        scripts.SetStandardConstants(_editor.CurrentGame.Settings);
                        scripts.AddStandardHeaders(_editor);
                        using (TextWriter output = new StreamWriter(path))
                        {
                            WriteJavascripts(scripts, output, _editor.CurrentGame.Settings.GUID.ToString("N"));
                        }
                        MessageBox.Show(JS_FILENAME + " created!", "Script dump", MessageBoxButtons.OK);
                    }
                    catch (SPAGS.Util.EditorUsageException ex)
                    {
                        MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                    break;
            }
        }

        public string ComponentID
        {
            get { return COMPONENT; }
        }

        /*** Output ***/

        static void Indent(TextWriter output, int indent)
        {
            for (int i = 0; i < indent; i++)
            {
                output.Write("  ");
            }
        }

        static void Indented(TextWriter output, int indent, string line)
        {
            Indent(output, indent);
            output.WriteLine(line);
        }

        void WriteAllScripts(ScriptCollection scripts, TextWriter output)
        {
            foreach (Script header in scripts.Headers)
            {
                WriteScript(header, output, 0, true);
            }
            Script globVarsScript = scripts.CompileGlobalVariablesScript(_editor);
            if (globVarsScript != null)
            {
                WriteScript(globVarsScript, output, 0, false);
            }
            foreach (AGS.Types.Script script in _editor.CurrentGame.Scripts)
            {
                if (!script.IsHeader)
                {
                    WriteScript(scripts.CompileScript(script.FileName, script.Text), output, 0, false);
                }
            }
            WriteScript(scripts.CompileDialogScript(_editor), output, 0, false);
            foreach (IRoom unloadedRoom in _editor.CurrentGame.Rooms)
            {
                WriteScript(scripts.CompileRoomScript(_editor, unloadedRoom.Number), output, 0, false);
            }
        }

        void WriteScript(Script script, TextWriter output, int indent, bool header)
        {
            Indented(output, indent, "=== " + (header ? "HEADER" : "SCRIPT") + " \"" + script.Name + "\" ===");
            foreach (Variable var in script.DefinedVariables)
            {
                Indented(output, indent, "DECLARE VARIABLE \"" + var.Name + "\":");
                WriteValueType(var.Type, output, indent+1);
                if (var.InitialValue != null)
                {
                    WriteExpression(var.InitialValue, output, indent + 1);
                }
            }
            foreach (Constant constant in script.DefinedConstants)
            {
                if (constant.Undefined) continue;
                if (constant is Constant.Expression)
                {
                    Constant.Expression constantExpr = (Constant.Expression)constant;
                    Indented(output, indent, "DEFINE CONSTANT EXPRESSION \"" + constantExpr.Name + "\":");
                    WriteExpression(constantExpr.TheExpression, output, indent + 1);
                }
                else
                {
                    Constant.TokenSequence tokens = (Constant.TokenSequence)constant;
                    if (tokens.Tokens.Count == 0)
                    {
                        Indented(output, indent, "DEFINE EMPTY CONSTANT \"" + tokens.Name + "\"");
                    }
                    else
                    {
                        Indented(output, indent, "DEFINE CONSTANT TOKEN SEQUENCE \"" + tokens.Name + "\":");
                        foreach (Token token in tokens.Tokens)
                        {
                            Indented(output, indent + 1, token.ToString());
                        }
                    }
                }
            }
            foreach (ValueType.Enum enumType in script.DefinedEnums)
            {
                Indented(output, indent, "DEFINE ENUM \"" + enumType.Name + "\":");
                foreach (EnumValue enumValue in enumType.Entries)
                {
                    Indented(output, indent+1, enumValue.Name + " = " + enumValue.Value);
                }
            }
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
                            Indented(output, indent+1, name + " \"" + attr.Name + "\":");
                            WriteValueType(attr.Type, output, indent+2);
                            break;
                        case StructMemberType.Field:
                            StructMember.Field field = (StructMember.Field)member;
                            Indented(output, indent+1, "FIELD \"" + field.Name + "\":");
                            WriteValueType(field.Type, output, indent+2);
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
                            Indented(output, indent+1, name + " \"" + method.Name + "\"");
                            break;
                    }
                }
            }
            foreach (Function func in script.DefinedFunctions)
            {
                if (func.Name.Contains("::"))
                {
                    Indented(output, indent, "DEFINE METHOD \"" + func.Name + "\":");
                }
                else
                {
                    Indented(output, indent, "DEFINE FUNCTION \"" + func.Name + "\":");
                }
                Indented(output, indent + 1, "RETURN TYPE:");
                WriteValueType(func.Signature.ReturnType, output, indent + 2);
                foreach (ParameterDef param in func.Signature.Parameters)
                {
                    Indented(output, indent+1, "PARAMETER \"" + param.Name + "\":");
                    WriteValueType(param.Type, output, indent + 2);
                }
                Indented(output, indent + 1, "FUNCTION BODY:");
                WriteStatement(func.Body, output, indent + 2);
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
                    if (assign.Value == null)
                    {
                        switch (assign.AssignType)
                        {
                            case TokenType.Increment:
                                Indented(output, indent, "INCREMENT:");
                                break;
                            case TokenType.Decrement:
                                Indented(output, indent, "DECREMENT:");
                                break;
                            default:
                                throw new Exception("Unexpected AssignType: " + assign.AssignType);
                        }
                        WriteExpression(assign.Target, output, indent + 1);
                    }
                    else
                    {
                        switch (assign.AssignType)
                        {
                            case TokenType.Assign:
                                Indented(output, indent, "ASSIGN:");
                                break;
                            case TokenType.AddAssign:
                                Indented(output, indent, "ADD-ASSIGN:");
                                break;
                            case TokenType.SubtractAssign:
                                Indented(output, indent, "SUBTRACT-ASSIGN:");
                                break;
                            default:
                                throw new Exception("Unexpected AssignType: " + assign.AssignType);
                        }
                        WriteExpression(assign.Target, output, indent + 1);
                        WriteExpression(assign.Value, output, indent + 1);
                    }
                    break;
                case StatementType.Block:
                    Statement.Block block = (Statement.Block)stmt;
                    foreach (Statement child in block.ChildStatements)
                    {
                        WriteStatement(child, output, indent);
                    }
                    break;
                case StatementType.If:
                    Statement.If conditional = (Statement.If)stmt;
                    Indented(output, indent, "IF:");
                    WriteExpression(conditional.IfThisIsTrue, output, indent + 1);
                    Indented(output, indent, "...THEN:");
                    WriteStatement(conditional.ThenDoThis, output, indent + 1);
                    if (conditional.ElseDoThis != null)
                    {
                        Indented(output, indent, "...ELSE:");
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
                        WriteValueType(var.Type, output, indent +1);
                        if (var.InitialValue != null)
                        {
                            WriteExpression(var.InitialValue, output, indent+1);
                        }
                    }
                    break;
                case StatementType.While:
                    Statement.While loop = (Statement.While)stmt;
                    Indented(output, indent, "WHILE:");
                    WriteExpression(loop.WhileThisIsTrue, output, indent + 1);
                    Indented(output, indent, "...DO:");
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
                    Expression.ArrayIndex arrayIndex = (Expression.ArrayIndex)expr;
                    Indented(output, indent, "ARRAY LOOKUP:");
                    // Target[Index]
                    WriteExpression(arrayIndex.Target, output, indent+1);
                    WriteExpression(arrayIndex.Index, output, indent+1);
                    break;
                case ExpressionType.Attribute:
                    Expression.Attribute attr = (Expression.Attribute)expr;
                    if (attr.Target == null)
                    {
                        Indented(output, indent, "STATIC ATTRIBUTE \"" + attr.TheStructType.Name + "." + attr.TheAttribute.Name + "\"");
                    }
                    else
                    {
                        Indented(output, indent, "ATTRIBUTE \"" + attr.TheStructType.Name + "." + attr.TheAttribute.Name + "\":");
                        WriteExpression(attr.Target, output, indent+1);
                    }
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
                        foreach (Expression param in call.Parameters)
                        {
                            WriteExpression(param, output, indent + 1);
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
                    Indented(output, indent, "ENUM \"" + enumValue.TheValue.OwnerType.Name + "." + enumValue.TheValue.Name + "\"");
                    break;
                case ExpressionType.Field:
                    Expression.Field field = (Expression.Field)expr;
                    Indented(output, indent, "FIELD \"" + field.TheStructType.Name + "." + field.TheField.Name + "\":");
                    WriteExpression(field.Target, output, indent + 1);
                    break;
                case ExpressionType.FloatLiteral:
                    Expression.FloatLiteral floatLiteral = (Expression.FloatLiteral)expr;
                    Indented(output, indent, String.Format("{0:0.0}", floatLiteral.Value));
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
                    Expression.Method method = (Expression.Method)expr;
                    if (method.Target == null)
                    {
                        Indented(output, indent, "STATIC METHOD \"" + method.TheStructType.Name + "." + method.TheMethod.Name + "\"");
                    }
                    else
                    {
                        Indented(output, indent, "METHOD \"" + method.TheStructType.Name + "." + method.TheMethod.Name + "\":");
                        WriteExpression(method.Target, output, indent + 1);
                    }
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
                default:
                    Indented(output, indent, expr.ToString());
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
                case ValueTypeCategory.Array:
                    ValueType.Array arrayType = (ValueType.Array)valueType;
                    if (arrayType.LengthExpression == null)
                    {
                        Indented(output, indent, "DYNAMIC ARRAY:");
                        Indented(output, indent + 1, "TYPE:");
                        WriteValueType(arrayType.ElementType, output, indent + 1);
                    }
                    else
                    {
                        Indented(output, indent, "STATIC ARRAY:");
                        WriteValueType(arrayType.ElementType, output, indent + 1);
                        WriteExpression(arrayType.LengthExpression, output, indent + 1);
                    }
                    break;
                case ValueTypeCategory.Float:
                    Indented(output, indent, "FLOAT");
                    break;
                case ValueTypeCategory.FunctionSignature:
                    Indented(output, indent, "FUNCTION SIGNATURE");
                    break;
                case ValueTypeCategory.Int:
                    if (valueType is ValueType.Enum)
                    {
                        ValueType.Enum enumType = (ValueType.Enum)valueType;
                        Indented(output, indent, "ENUM \"" + enumType.Name + "\"");

                    }
                    else
                    {
                        Indented(output, indent, "INT");
                    }
                    break;
                case ValueTypeCategory.Null:
                    Indented(output, indent, "NULL");
                    break;
                case ValueTypeCategory.StringBuffer:
                    Indented(output, indent, "STRING BUFFER");
                    break;
                case ValueTypeCategory.StringValue:
                    Indented(output, indent, "STRING");
                    break;
                case ValueTypeCategory.Struct:
                    ValueType.Struct structType = (ValueType.Struct)valueType;
                    if (structType.IsInternalString)
                    {
                        Indented(output, indent, "STRING");
                    }
                    else
                    {
                        if (structType.IsManaged)
                        {
                            Indented(output, indent, "MANAGED STRUCT \"" + structType.Name + "\"");
                        }
                        else
                        {
                            Indented(output, indent, "STRUCT \"" + structType.Name + "\"");
                        }
                    }
                    break;
                case ValueTypeCategory.Void:
                    Indented(output, indent, "VOID");
                    break;
            }
        }


        void WriteGlobalNamespace(ScriptCollection scripts, TextWriter output)
        {
            Indented(output, 0, "=== Global Namespace ===");
            foreach (SPAGS.Util.INameHolder named in scripts.GlobalNamespace.Values)
            {
                switch (named.NameHolderType)
                {
                    case SPAGS.Util.NameHolderType.Constant:
                        Constant constant = (Constant)named;
                        Indented(output, 0, "CONSTANT \"" + constant.Name
                            + "\" (" + (constant.OwnerScript == null ? "?" : constant.OwnerScript.Name) + ")");
                        break;
                    case SPAGS.Util.NameHolderType.EnumType:
                        ValueType.Enum enumType = (ValueType.Enum)named;
                        Indented(output, 0, "ENUM TYPE \"" + enumType.Name
                            + "\" (" + (enumType.OwnerScript == null ? "?" : enumType.OwnerScript.Name) + ")");
                        break;
                    case SPAGS.Util.NameHolderType.EnumValue:
                        EnumValue enumValue = (EnumValue)named;
                        ValueType.Enum type = enumValue.OwnerType;
                        Indented(output, 0, "ENUM VALUE \"" + enumValue.Name
                            + "\" (" + (type.OwnerScript == null ? "?" : type.OwnerScript.Name) + ")");
                        break;
                    case SPAGS.Util.NameHolderType.Function:
                        Function func = (Function)named;
                        Indented(output, 0, "FUNCTION \"" + func.Name + "\" ("
                            + (func.OwnerScript == null ? "?" : func.OwnerScript.Name) + ")");
                        break;
                    case SPAGS.Util.NameHolderType.Struct:
                        ValueType.Struct structType = (ValueType.Struct)named;
                        if (structType.IsManaged)
                        {
                            Indented(output, 0, "MANAGED STRUCT \"" + structType.Name
                                + "\" (" + (structType.OwnerScript == null ? "?" : structType.OwnerScript.Name) + ")");
                        }
                        else
                        {
                            Indented(output, 0, "STRUCT \"" + structType.Name
                                + "\" (" + (structType.OwnerScript == null ? "?" : structType.OwnerScript.Name) + ")");
                        }
                        break;
                    case SPAGS.Util.NameHolderType.Variable:
                        Variable var = (Variable)named;
                        Indented(output, 0, "VARIABLE \"" + var.Name + "\" ("
                            + (var.OwnerScript == null ? "?" : var.OwnerScript.Name) + ")");
                        break;

                    case SPAGS.Util.NameHolderType.BasicType:
                    case SPAGS.Util.NameHolderType.Keyword:
                    case SPAGS.Util.NameHolderType.StructMember:
                        break;
                }
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
