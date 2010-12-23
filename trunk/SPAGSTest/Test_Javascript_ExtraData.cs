using System;
using System.Collections.Generic;
using System.Text;
using SPAGS.Util;

namespace SPAGS
{
    public class ExpressionData : IUserData<Expression>
    {
        public void Init(Expression expr)
        {
            TheExpression = expr;
        }
        public Expression TheExpression;
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
