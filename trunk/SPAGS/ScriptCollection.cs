using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public class ScriptCollection
    {
        public ScriptCollection(AGS.Types.IAGSEditor editor)
        {
            EngineVersion = new System.Version(editor.Version);
            RightAssociativity = !editor.CurrentGame.Settings.LeftToRightPrecedence;
            DebugMode = editor.CurrentGame.Settings.DebugMode;
            StrictObjectOrientedMode = editor.CurrentGame.Settings.EnforceObjectBasedScript;
            StrictStringMode = editor.CurrentGame.Settings.EnforceNewStrings;
            StrictAudioMode = editor.CurrentGame.Settings.EnforceNewAudio;

            parser = new ScriptParser(EngineVersion);
            foreach (Token.Keyword keyword in Token.YieldKeywords())
            {
                GlobalNamespace.Add(keyword);
            }
            foreach (ValueType.Named vtype in ValueType.YieldValueTypes())
            {
                GlobalNamespace.Add(vtype);
            }
            if (DebugMode)
            {
                GlobalNamespace.Add(new Constant.Expression("DEBUG", new Expression.IntegerLiteral(1)));
            }
            if (StrictObjectOrientedMode)
            {
                GlobalNamespace.Add(new Constant.Expression("STRICT", new Expression.IntegerLiteral(1)));
            }
            if (StrictStringMode)
            {
                GlobalNamespace.Add(new Constant.Expression("STRICT_STRINGS", new Expression.IntegerLiteral(1)));
            }
            if (StrictAudioMode)
            {
                GlobalNamespace.Add(new Constant.Expression("STRICT_AUDIO", new Expression.IntegerLiteral(1)));
            }
            if (RightAssociativity)
            {
                parser.OverrideLeftToRight = true;
            }
            else
            {
                GlobalNamespace.Add(new Constant.Expression("LRPRECEDENCE", new Expression.IntegerLiteral(1)));
            }
            if (EngineVersion >= new System.Version("2.71"))
            {
                GlobalNamespace.Add(new Constant.Expression("AGS_NEW_STRINGS", new Expression.IntegerLiteral(1)));
                GlobalNamespace.Add(new Constant.Expression("AGS_SUPPORTS_IFVER", new Expression.IntegerLiteral(1)));
            }

            foreach (AGS.Types.Script agsScript in editor.GetAllScriptHeaders())
            {
                AddGlobalScript(agsScript.FileName, agsScript.Text, "");
            }

            foreach (AGS.Types.Script agsScript in editor.CurrentGame.Scripts)
            {
                if (agsScript.IsHeader) continue;
                AddGlobalScript(agsScript.FileName, "", agsScript.Text);
            }

            foreach (AGS.Types.IRoom room in editor.CurrentGame.Rooms)
            {
                if (!editor.RoomController.LoadRoom(room))
                {
                    throw new EditorUsageException("Prevented from loading room " + room.Number);
                }
                AddRoomScript(room, (AGS.Types.Room)editor.RoomController.CurrentRoom);
            }

            StringBuilder dialogScriptSB = new StringBuilder(@"#define DIALOG_NONE      0
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
}");
            foreach (AGS.Types.Dialog dialog in editor.CurrentGame.Dialogs)
            {
                if (string.IsNullOrEmpty(dialog.CachedConvertedScript))
                {
                    throw new EditorUsageException("Unable to get dialog scripts: Project must be re-built");
                }
                dialogScriptSB.Append(dialog.CachedConvertedScript);
            }

            AddGlobalScript("__DialogScripts.asc", "", dialogScriptSB.ToString());
        }


        public IEnumerable<Script> YieldScripts()
        {
            foreach (Script script in MainHeaders) yield return script;
            foreach (Script script in GlobalScripts) yield return script;
            foreach (Script script in DialogScripts) yield return script;
            foreach (Script script in RoomScripts.Values) yield return script;
        }

        public void AddDefine(string name)
        {
            GlobalNamespace.Add(new Constant.TokenSequence(name, new Token[] { }));
        }

        public Script MainScript = new Script("MainScript");
        public List<Script> GlobalScripts = new List<Script>();

        public NameDictionary GlobalNamespace = new NameDictionary();

        private ScriptParser parser;

        public System.Version EngineVersion;

        public bool DebugMode;
        public bool RightAssociativity;
        public bool StrictObjectOrientedMode;
        public bool StrictAudioMode;
        public bool StrictStringMode;

        public List<Script> MainHeaders = new List<Script>();

        public void AddExtraHeader(string header)
        {
            if (header == null) throw new System.ArgumentNullException("header");
            parser.Identifiers = GlobalNamespace;
            MainHeaders.Add(parser.ReadScript(header, MainScript));
        }

        public void AddGlobalScript(string moduleName, string header, string script)
        {
            if (header == null) throw new System.ArgumentNullException("header");
            if (script == null) throw new System.ArgumentNullException("script");
            Script moduleScript = new Script(moduleName);
            GlobalScripts.Add(moduleScript);
            parser.Identifiers = GlobalNamespace;
            parser.ReadScript(header, moduleScript);
            parser.Identifiers = new NameDictionary(GlobalNamespace);
            parser.ReadScript(script, moduleScript);
        }

        public List<Script> DialogScripts = new List<Script>();

        public void AddDialogScript(string scriptText)
        {
            Script newDialogScript = new Script("Dialog" + DialogScripts.Count + "Script");
            parser.Identifiers = new NameDictionary(GlobalNamespace);
            parser.Identifiers.Add(new Variable("__dlgscript_tempval", ValueType.Int, null));
            parser.Identifiers.Add(new Variable("_run_dialog_request", ValueType.Int, null));
            parser.ReadScript(scriptText, newDialogScript);
            DialogScripts.Add(newDialogScript);
        }

        public Dictionary<int, Script> RoomScripts = new Dictionary<int, Script>();

        public void MarkBlockingFunctions()
        {
            for (bool anyChanges = false; anyChanges == true; anyChanges = false)
            {
                foreach (Script script in this.YieldScripts())
                {
                    foreach (Function func in script.DefinedFunctions)
                    {
                        if (func.MarkedAsBlocking) continue;
                        foreach (Function calledFunc in func.Body.YieldFunctions())
                        {
                            if (calledFunc.MarkedAsBlocking)
                            {
                                func.MarkedAsBlocking = true;
                                anyChanges = true;
                            }
                        }
                    }
                }
            }
        }

        public void AddRoomScript(AGS.Types.IRoom unloadedRoom, AGS.Types.Room room)
        {
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
            AddRoomScript(room.Number, unloadedRoom.Script.Text, roomVariables);
        }
        public void AddRoomScript(int roomNumber, string scriptText, NameDictionary roomVariables)
        {
            if (scriptText == null) throw new System.ArgumentNullException("script");
            parser.Identifiers = new NameDictionary(GlobalNamespace);
            foreach (INameHolder nameHolder in roomVariables.Values) parser.Identifiers.Add(nameHolder);
            RoomScripts[roomNumber] = parser.ReadScript(scriptText, new Script("Room" + roomNumber + "Script"));

            foreach (ScriptVariable scriptVar in roomVariables.EachOf<ScriptVariable>()) scriptVar.OwnerScript = RoomScripts[roomNumber];
        }
    }
}
