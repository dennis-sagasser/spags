using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS
{
    public enum CodeUnitType
    {
        Statement, Expression
    }
    public abstract class CodeUnit
    {
        public abstract CodeUnitType CodeUnitType { get; }
        protected CodeUnit _parentCodeUnit;
        public virtual CodeUnit ParentCodeUnit
        {
            get { return _parentCodeUnit; }
            set { _parentCodeUnit = value; }
        }
        public virtual IEnumerable<CodeUnit> YieldChildCodeUnits()
        {
            yield break;
        }
        public virtual IEnumerable<CodeUnit> YieldChildCodeUnitsRecursive()
        {
            foreach (CodeUnit child in YieldChildCodeUnits())
            {
                yield return child;
                foreach (CodeUnit descendant in child.YieldChildCodeUnitsRecursive())
                {
                    yield return descendant;
                }
            }
        }
        public virtual bool TryGetSimpleCall(out Function func, out List<Expression> parameters)
        {
            func = null;
            parameters = null;
            return false;
        }
    }
}
