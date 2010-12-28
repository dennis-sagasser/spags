using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public class CodeUnitData : IUserData<CodeUnit>
    {
        public void Init(CodeUnit codeUnit)
        {
            _unit = codeUnit;
        }
        protected CodeUnit _unit;
        public CodeUnit TheCodeUnit
        {
            get
            {
                return _unit;
            }
        }
        protected bool _blocked;
        public bool Blocked
        {
            get
            {
                return _blocked;
            }
        }
        public void MarkAsBlocked()
        {
            _blocked = true;
            for (CodeUnit ancestor = _unit.ParentCodeUnit; ancestor != null; ancestor = ancestor.ParentCodeUnit)
            {
                CodeUnitData cudata = UserData<CodeUnit,CodeUnitData>.Get(ancestor);
                if (cudata._blocked) break;
                cudata._blocked = true;
            }
        }
    }
    public class VariableData : IUserData<Variable>
    {
        public void Init(Variable var)
        {
            TheVariable = var;
        }
        public Variable TheVariable;
        string _name = null;
        public string Name
        {
            get { return _name ?? TheVariable.Name; }
            set { _name = value; }
        }
    }
    public class FunctionData : IUserData<Function>
    {
        public void Init(Function func)
        {
            TheFunction = func;
        }
        public Function TheFunction;
        string _name = null;
        public string Name
        {
            get { return _name ?? TheFunction.Name; }
            set { _name = value; }
        }
        private bool _blocking;
        public bool Blocking
        {
            set
            {
                _blocking = value;
            }
            get
            {
                if (_blocking) return true;
                if (TheFunction.Body == null) return false;
                return UserData<CodeUnit,CodeUnitData>.Get(TheFunction.Body).Blocked;
            }
        }
    }
    public class StructData : IUserData<ValueType.Struct>
    {
        public void Init(ValueType.Struct structType)
        {
            TheStructType = structType;
        }
        public ValueType.Struct TheStructType;
        string _name = null;
        public string Name
        {
            get { return _name ?? TheStructType.Name; }
            set { _name = value; }
        }
    }
}
