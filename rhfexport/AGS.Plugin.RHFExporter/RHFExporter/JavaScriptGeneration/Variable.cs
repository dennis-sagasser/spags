using System;
using System.Collections.Generic;
using System.Text;

namespace RedHerringFarm.JavaScriptGeneration
{
    public class Variable : Expression
    {
        public string Name;
        private PossibleValueTypes valueTypes;
        public override PossibleValueTypes ValueTypes
        {
            get { return valueTypes; }
        }
        public Variable(string name)
            : this(name, PossibleValueTypes.Any)
        {
        }
        public Variable(string name, PossibleValueTypes valueTypes)
        {
            Name = name;
            this.valueTypes = valueTypes;
        }
        public override void WriteTo(Writer writer)
        {
            writer.Write(Name);
        }
    }
}
