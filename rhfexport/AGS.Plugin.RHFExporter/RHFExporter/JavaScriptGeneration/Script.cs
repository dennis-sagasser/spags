using System;
using System.Collections.Generic;
using System.Text;

namespace RedHerringFarm.JavaScriptGeneration
{
    public class Script : ScopedBlock
    {
        public override void WriteTo(Writer writer)
        {
            WriteBlockContents(writer);
            writer.WriteLineThenIndent();
        }
    }
}
