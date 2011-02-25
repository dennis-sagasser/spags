using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS.SimSynch
{
    public class SimSynchChunk : Statement.Block
    {
        public SimSynchChunk(SimSynchFunction func, SPAGS.Util.NameDictionary initialScope)
            : base(initialScope)
        {
            ParentFunction = func;
        }
        public bool TryGetFinish(out Expression expr)
        {
            if (Redirect != null)
            {
                return Redirect.TryGetFinish(out expr);
            }
            if (ChildStatements.Count == 1)
            {
                SimSynchStatement.Finish fin = ChildStatements[0] as SimSynchStatement.Finish;
                if (fin != null)
                {
                    expr = fin.ReturnValue;
                    return expr.UnchangingWhileThreadSuspended;
                }
            }
            expr = null;
            return false;
        }
        public SimSynchFunction ParentFunction;
        public int ID
        {
            get
            {
                if (Redirect != null) return Redirect.ID;
                return ParentFunction.Chunks.IndexOf(this);
            }
        }
        public SimSynchChunk Redirect;
    }
}
