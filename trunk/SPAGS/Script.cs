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
        public List<EnumType> DefinedEnums = new List<EnumType>();
        public List<StructType> DefinedStructs = new List<StructType>();
        public List<Constant.Expression> DefinedConstantExpressions = new List<Constant.Expression>();
    }
}
