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
