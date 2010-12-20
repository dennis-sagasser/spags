using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public class ScriptCollection
    {
        private ScriptParser parser;
        public NameDictionary GlobalNamespace = new NameDictionary();
        public NameDictionary Exported = new NameDictionary();
        public List<Script> Headers = new List<Script>();

        public ScriptCollection(string agsVersion)
        {
            parser = new ScriptParser(new System.Version(agsVersion));
            parser.Exported = Exported;

            foreach (Token.Keyword keyword in Token.YieldKeywords())
            {
                GlobalNamespace.Add(keyword);
            }
            foreach (ValueType.Named vtype in ValueType.YieldValueTypes())
            {
                GlobalNamespace.Add(vtype);
            }
        }
        public bool LeftToRightPrecedence
        {
            get
            {
                return !parser.OverrideLeftToRight;
            }
            set
            {
                parser.OverrideLeftToRight = !value;
            }
        }
        public void SetConstant(string name)
        {
            GlobalNamespace.Add(new Constant.Expression(name, new Expression.IntegerLiteral(1)));
        }
        public void SetStandardConstants(AGS.Types.Settings settings)
        {
            SetConstant("AGS_NEW_STRINGS");
            SetConstant("AGS_SUPPORTS_IFVER");

            LeftToRightPrecedence = settings.LeftToRightPrecedence;
            if (LeftToRightPrecedence) SetConstant("LRPRECEDENCE");
            if (settings.DebugMode) SetConstant("DEBUG");
            if (settings.EnforceObjectBasedScript) SetConstant("STRICT");
            if (settings.EnforceNewStrings) SetConstant("STRICT_STRINGS");
            if (settings.EnforceNewAudio) SetConstant("STRICT_AUDIO");
        }
        public Script AddHeader(string name, string header)
        {
            Script headerScript = new Script(name);
            parser.Namespace = GlobalNamespace;
            parser.ReadScript(header, headerScript);
            Headers.Add(headerScript);
            return headerScript;
        }
        public Script CompileGlobalVariablesScript(AGS.Types.IAGSEditor editor)
        {
            AGS.Types.Game game = (AGS.Types.Game)editor.CurrentGame;
            IList<AGS.Types.GlobalVariable> globVars = game.GlobalVariables.ToList();
            if (globVars.Count == 0) return null;
            StringBuilder globVarsSB = new StringBuilder();
            foreach (AGS.Types.GlobalVariable globVar in game.GlobalVariables.ToList())
            {
                string declaration = globVar.Type + " " + globVar.Name;
                if (((globVar.Type == "int") ||
                     (globVar.Type == "bool") ||
                     (globVar.Type == "float")) &&
                    (!string.IsNullOrEmpty(globVar.DefaultValue)))
                {
                    declaration += " = " + globVar.DefaultValue.ToLower();
                }
                globVarsSB.AppendLine(declaration + ";");
                globVarsSB.AppendLine("export " + globVar.Name + ";");
            }

            globVarsSB.AppendLine("function game_start() {");
            foreach (AGS.Types.GlobalVariable globVar in globVars)
            {
                if (globVar.Type == "String")
                {
                    string varValue = globVar.DefaultValue.Replace("\\", "\\\\").Replace("\"", "\\\"");
                    globVarsSB.AppendLine(globVar.Name + " = \"" + varValue + "\";");
                }
            }
            globVarsSB.AppendLine("}");

            return CompileScript("_GlobalVariables.asc", globVarsSB.ToString());
        }
        public void AddStandardHeaders(AGS.Types.IAGSEditor editor)
        {
            foreach (AGS.Types.Script agsScript in editor.GetAllScriptHeaders())
            {
                AddHeader(agsScript.FileName, agsScript.Text);
            }
        }
        public Script CompileScript(string name, string script)
        {
            Script compiledScript = new Script(name);
            parser.Namespace = new NameDictionary(GlobalNamespace);
            parser.ReadScript(script, compiledScript);
            return compiledScript;
        }
        public Script CompileDialogScript(AGS.Types.IAGSEditor editor)
        {
            StringBuilder dialogScriptSB = new StringBuilder(@"
#define DIALOG_NONE      0
#define DIALOG_RUNNING   1
#define DIALOG_STOP      2
#define DIALOG_NEWROOM   100
#define DIALOG_NEWTOPIC  12000

_tryimport function dialog_request(int);
int __dlgscript_tempval;

function _run_dialog_request (int parmtr) {
  game.stop_dialog_at_end = DIALOG_RUNNING;
  dialog_request(parmtr);

  if (game.stop_dialog_at_end == DIALOG_STOP) {
    game.stop_dialog_at_end = DIALOG_NONE;
    return -2;
  }
  if (game.stop_dialog_at_end >= DIALOG_NEWTOPIC) {
    int tval = game.stop_dialog_at_end - DIALOG_NEWTOPIC;
    game.stop_dialog_at_end = DIALOG_NONE;
    return tval;
  }
  if (game.stop_dialog_at_end >= DIALOG_NEWROOM) {
    int roomnum = game.stop_dialog_at_end - DIALOG_NEWROOM;
    game.stop_dialog_at_end = DIALOG_NONE;
    player.ChangeRoom(roomnum);
    return -2;
  }
  game.stop_dialog_at_end = DIALOG_NONE;
  return -1;
}
");
            foreach (AGS.Types.Dialog dialog in editor.CurrentGame.Dialogs)
            {
                if (string.IsNullOrEmpty(dialog.CachedConvertedScript))
                {
                    throw new EditorUsageException("Unable to get dialog scripts: Project must be re-built");
                }
                dialogScriptSB.Append(dialog.CachedConvertedScript);
            }

            return CompileScript("__DialogScripts.asc", dialogScriptSB.ToString());
        }
        public Script CompileRoomScript(AGS.Types.IAGSEditor editor, int roomNumber)
        {
            AGS.Types.IRoom unloadedRoom = null;
            foreach (AGS.Types.IRoom r in editor.CurrentGame.Rooms)
            {
                if (r.Number == roomNumber)
                {
                    unloadedRoom = r;
                    break;
                }
            }
            if (unloadedRoom == null)
            {
                throw new SPAGS.Util.EditorUsageException("Room not found: " + roomNumber);
            }
            if (!editor.RoomController.LoadRoom(unloadedRoom))
            {
                throw new SPAGS.Util.EditorUsageException("Unable to load room: " + roomNumber);
            }
            AGS.Types.Room room = (AGS.Types.Room)editor.RoomController.CurrentRoom;
            NameDictionary roomVariables = new NameDictionary();
            ValueType.Struct hotspotType, objectType;
            if (!GlobalNamespace.TryGetValue2<ValueType.Struct>("Hotspot", out hotspotType))
            {
                throw new System.Exception("Hotspot type not found!");
            }
            if (!GlobalNamespace.TryGetValue2<ValueType.Struct>("Object", out objectType))
            {
                throw new System.Exception("Object type not found!");
            }
            foreach (AGS.Types.RoomHotspot hotspot in room.Hotspots)
            {
                roomVariables.Add(new Variable(hotspot.Name, hotspotType, null));
            }
            foreach (AGS.Types.RoomObject obj in room.Objects)
            {
                roomVariables.Add(new Variable(obj.Name, objectType, null));
            }
            string scriptText = unloadedRoom.Script.Text;
            parser.Namespace = new NameDictionary(GlobalNamespace);
            foreach (INameHolder nameHolder in roomVariables.Values) parser.Namespace.Add(nameHolder);
            Script newScript = parser.ReadScript(scriptText, new Script(unloadedRoom.ScriptFileName));
            foreach (ScriptVariable scriptVar in roomVariables.EachOf<ScriptVariable>())
            {
                scriptVar.OwnerScript = newScript;
            }
            return newScript;
        }
    }
}
