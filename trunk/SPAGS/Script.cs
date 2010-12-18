using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public class Script
    {
        public Script(string name)
        {
            Name = name;
        }
        public string Name;
        public bool IsHeader;
        public object UserData;
        public NameDictionary Identifiers;
        public List<Function> DefinedFunctions = new List<Function>();
        public List<ScriptVariable> DefinedVariables = new List<ScriptVariable>();
        public List<ValueType.Enum> DefinedEnums = new List<ValueType.Enum>();
        public List<ValueType.Struct> DefinedStructs = new List<ValueType.Struct>();
        public List<Constant> DefinedConstants = new List<Constant>();
    }
}
