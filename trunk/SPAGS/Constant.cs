using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public abstract class Constant : INameHolder
    {
        protected Constant(string name)
        {
            this.name = name;
        }
        private string name;
        public string Name { get { return name; } }
        public NameHolderType NameHolderType
        {
            get { return NameHolderType.Constant; }
        }

        public Script OwnerScript;
        public bool Undefined;

        public class Expression : Constant, IUserDataHolder
        {
            public Expression(string name, SPAGS.Expression expression)
                : base(name)
            {
                TheExpression = expression;
            }
            public readonly SPAGS.Expression TheExpression;
            public object UserData
            {
                get { return _userdata; }
                set { _userdata = value; }
            }
            private object _userdata;
            public override string ToString()
            {
                return "#define " + Name + " " + TheExpression;
            }
        }

        public class TokenSequence : Constant
        {
            public TokenSequence(string name, IList<Token> tokens)
                : base(name)
            {
                Tokens = tokens;
            }
            public readonly IList<Token> Tokens;
            public override string ToString()
            {
                StringBuilder sb = new StringBuilder("#define " + Name);
                foreach (Token t in Tokens)
                {
                    sb.Append(" " + t);
                }
                return sb.ToString();
            }
        }
    }
}
