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
                GlobalNamespace.Add(new Constant.TokenSequence("DEBUG", new Token[] { }));
            }
            if (StrictObjectOrientedMode)
            {
                GlobalNamespace.Add(new Constant.TokenSequence("STRICT", new Token[] { }));
            }
            if (StrictStringMode)
            {
                GlobalNamespace.Add(new Constant.TokenSequence("STRICT_STRINGS", new Token[] { }));
            }
            if (StrictAudioMode)
            {
                GlobalNamespace.Add(new Constant.TokenSequence("STRICT_AUDIO", new Token[] { }));
            }
            if (RightAssociativity)
            {
                parser.OverrideLeftToRight = true;
                GlobalNamespace.Add(new Constant.TokenSequence("LRPRECEDENCE", new Token[] { }));
            }
            if (EngineVersion >= new System.Version("2.71"))
            {
                GlobalNamespace.Add(new Constant.TokenSequence("AGS_NEW_STRINGS", new Token[] { }));
                GlobalNamespace.Add(new Constant.TokenSequence("AGS_SUPPORTS_IFVER", new Token[] { }));
            }

            foreach (AGS.Types.Script agsScript in editor.GetAllScriptHeaders())
            {
                /*
                bool notExtraHeader = false;
                foreach (AGS.Types.Script otherScript in editor.CurrentGame.Scripts)
                {
                    if (otherScript == agsScript)
                    {
                        notExtraHeader = true;
                        break;
                    }
                }
                if (notExtraHeader)
                {
                    continue;
                }
                 */
                AddGlobalScript(agsScript.FileName, agsScript.Text, "");
            }

            foreach (AGS.Types.Script agsScript in editor.CurrentGame.Scripts)
            {
                if (agsScript.IsHeader) continue;
                AddGlobalScript(agsScript.FileName, "", agsScript.Text);
            }
            /*

            foreach (AGS.Types.IRoom room in editor.CurrentGame.Rooms)
            {
                if (!editor.RoomController.LoadRoom(room))
                {
                    throw new EditorUsageException("Prevented from loading room " + room.Number);
                }
                AddRoomScript((AGS.Types.Room)editor.RoomController.CurrentRoom);
            }

            foreach (AGS.Types.Dialog dialog in editor.CurrentGame.Dialogs)
            {
                if (string.IsNullOrEmpty(dialog.CachedConvertedScript))
                {
                    throw new EditorUsageException("Unable to get dialog scripts: Game has unsaved changes");
                }
                AddDialogScript(dialog.CachedConvertedScript);
            }
            */
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

        public void AddRoomScript(AGS.Types.Room room)
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
            AddRoomScript(room.Number, room.Script.Text, roomVariables);
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
