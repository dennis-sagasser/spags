﻿using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public class Variable : INameHolder, IUserDataHolder
    {
        public Variable(string name, ValueType type, Expression initialValue)
        {
            this.name = name;
            Type = type;
            InitialValue = initialValue;
        }
        public object UserData
        {
            get { return _userdata; }
            set { _userdata = value; }
        }
        private object _userdata;
        public Script OwnerScript;
        public readonly ValueType Type;
        private readonly string name;
        public string Name { get { return name; } }
        public Expression InitialValue;
        public NameHolderType NameHolderType { get { return NameHolderType.Variable; } }
        protected bool? overrideUnchangingWhileThreadSuspended = null;
        public virtual bool UnchangingWhileThreadSuspended
        {
            get
            {
                if (overrideUnchangingWhileThreadSuspended != null)
                {
                    return (bool)overrideUnchangingWhileThreadSuspended;
                }
                // this is true at time of writing, may change in the future
                if (OwnerScript == null)
                {
                    return true;
                }
                return false;
            }
            set
            {
                overrideUnchangingWhileThreadSuspended = value;
            }
        }
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (TextWriter output = new StringWriter(sb))
            {
                this.WriteTo(output);
            }
            return sb.ToString();
        }
        public virtual void WriteTo(TextWriter output)
        {
            ValueType.Array asArray = Type as ValueType.Array;
            if (asArray != null)
            {
                output.Write(asArray.ElementType.Name + " " + Name + "[");
                if (asArray.LengthExpression != null) asArray.LengthExpression.WriteTo(output);
                output.Write("];");
            }
            else
            {
                output.Write(Type.Name + " " + Name);
                if (InitialValue != null)
                {
                    output.Write(" = ");
                    InitialValue.WriteTo(output);
                }
                output.Write(";");
            }
        }
    }
    public class LocalVariable : Variable
    {
        public LocalVariable(string name, ValueType valueType, Expression initialValue)
            : base(name, valueType, initialValue)
        {
        }
        public Function OwnerFunction;
        public Statement.Block OwnerScope;
        public override bool UnchangingWhileThreadSuspended
        {
            get
            {
                if (overrideUnchangingWhileThreadSuspended != null)
                {
                    return (bool)overrideUnchangingWhileThreadSuspended;
                }
                return true;
            }
            set
            {
                base.UnchangingWhileThreadSuspended = value;
            }
        }
    }
    public class Parameter : LocalVariable
    {
        public Parameter(string name, ValueType valueType)
            : base(name, valueType, null)
        {
        }
        public override bool UnchangingWhileThreadSuspended
        {
            get
            {
                if (overrideUnchangingWhileThreadSuspended != null)
                {
                    return (bool)overrideUnchangingWhileThreadSuspended;
                }
                return true;
            }
            set
            {
                base.UnchangingWhileThreadSuspended = value;
            }
        }
    }
    public class ScriptVariable : Variable
    {
        public ScriptVariable(string name, ValueType valueType, Expression initialValue)
            : base(name, valueType, initialValue)
        {
        }
        public bool ReadOnly;
        public bool Imported;
        public bool Defined;
        public bool Exported;
        public override void WriteTo(TextWriter output)
        {
            base.WriteTo(output);
            if (Exported) output.Write(" /* exported */");
        }
    }
}
