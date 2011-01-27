using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using JS = RedHerringFarm.JavaScriptGeneration;

namespace RedHerringFarm
{
    partial class ExporterPlugin
    {
        private void ExportGlobalScripts()
        {
            SPAGS.ScriptCollection scripts = new SPAGS.ScriptCollection(editor.Version);
            scripts.SetStandardConstants(editor.CurrentGame.Settings);
            scripts.AddStandardHeaders(editor);

            JS.SPAGSConverter convert = new JS.SPAGSConverter();

            JS.Script script = new JS.Script();

            List<SPAGS.Script> globalScripts = GetGlobalScripts(scripts);

            JS.Expression scriptsObj = new JS.Expression.Custom("scripts");

            PrepareScriptExternals(convert, scripts);
            foreach (SPAGS.Script globalScript in globalScripts)
            {
                PrepareScript(convert, globalScript);
            }

            foreach (SPAGS.Script globalScript in globalScripts)
            {
                JS.Expression scriptAssignTo = scriptsObj.Index(globalScript.Name);
                JS.Expression scriptValue = GetScript(convert, globalScript);
                script.Add((JS.Statement)scriptAssignTo.BinOp(JS.Infix.Assign, scriptValue));
            }

            using (JS.Writer output = JS.Writer.Create(InExportFolder(GLOBAL_SCRIPTS_FILENAME)))
            {
                script.WriteTo(output);
            }
        }
        private List<SPAGS.Script> GetGlobalScripts(SPAGS.ScriptCollection scripts)
        {
            List<SPAGS.Script> globalScripts = new List<SPAGS.Script>();
            globalScripts.AddRange(scripts.Headers);
            SPAGS.Script globVarsScript = scripts.CompileGlobalVariablesScript(editor);
            if (globVarsScript != null)
            {
                globalScripts.Add(globVarsScript);
            }
            foreach (AGS.Types.Script script in editor.CurrentGame.Scripts)
            {
                if (!script.IsHeader)
                {
                    globalScripts.Add(scripts.CompileScript(script.FileName, script.Text));
                }
            }
            globalScripts.Add(scripts.CompileDialogScript(editor));
            return globalScripts;
        }
        private void PrepareScriptExternals(JS.SPAGSConverter converter, SPAGS.ScriptCollection collection)
        {
            JS.Expression engineExpr = new JS.Expression.Custom("engine");
            JS.Expression gameExpr = new JS.Expression.Custom("game");
            foreach (SPAGS.Util.INameHolder named in collection.GlobalNamespace.Values)
            {
                if (named is SPAGS.Variable)
                {
                    SPAGS.Variable variable = (SPAGS.Variable)named;
                    if (variable.OwnerScript == null)
                    {
                        string variableName = variable.Name;
                        converter.AddReference(variable, gameExpr.Index(variableName, converter.GetValueTypes(variable.Type)));
                    }
                }
                else if (named is SPAGS.Function)
                {
                    SPAGS.Function func = (SPAGS.Function)named;
                    if (func.OwnerScript == null)
                    {
                        string funcName = func.Name.Replace("::","$$");
                        converter.AddReference(func, engineExpr.Index(funcName));
                    }
                }
            }
        }
        private void PrepareScript(JS.SPAGSConverter converter, SPAGS.Script script)
        {
            JS.Expression scriptsStore = new JS.Expression.Custom("scripts");
            JS.Expression scriptExpr = scriptsStore.Index(script.Name);
            foreach (SPAGS.Function func in script.DefinedFunctions)
            {
                string funcName = func.Name.Replace("::", "$$");
                converter.AddReference(func, scriptExpr.Index(funcName));
            }
            foreach (SPAGS.Variable variable in script.DefinedVariables)
            {
                string varName = variable.Name;
                converter.AddReference(variable, scriptExpr.Index(varName, converter.GetValueTypes(variable.Type)));
            }
        }
        private JS.Expression.ObjectLiteral GetScript(JS.SPAGSConverter converter, SPAGS.Script script)
        {
            JS.Expression.ObjectLiteral scriptObj = new JS.Expression.ObjectLiteral();

            foreach (SPAGS.ValueType.Struct structType in script.DefinedStructs)
            {
                if (structType.IsManaged)
                {
                    continue;
                }
                scriptObj[structType.Name] = converter.FromSPAGS(structType);
            }

            foreach (SPAGS.Variable variable in script.DefinedVariables)
            {
                if (variable.InitialValue == null)
                {
                    scriptObj[variable.Name] = converter.FromSPAGS(variable.Type.CreateDefaultValueExpression());
                }
                else
                {
                    scriptObj[variable.Name] = converter.FromSPAGS(variable.InitialValue);
                }
            }

            foreach (SPAGS.Function func in script.DefinedFunctions)
            {
                scriptObj[func.Name] = converter.FromSPAGS(func);
            }

            return scriptObj;
        }
    }
}
