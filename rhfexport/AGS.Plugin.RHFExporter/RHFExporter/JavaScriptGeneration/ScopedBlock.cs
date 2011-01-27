using System;
using System.Collections.Generic;
using System.Text;

namespace RedHerringFarm.JavaScriptGeneration
{
    public class ScopedBlock : Block
    {
        public readonly Dictionary<string, Variable> Variables = new Dictionary<string, Variable>();
        protected override void WriteBlockContents(Writer writer)
        {
            if (Variables.Count > 0)
            {
                writer.Write("var ");
                bool first = true;
                foreach (string variableName in Variables.Keys)
                {
                    if (first)
                    {
                        first = false;
                    }
                    else
                    {
                        writer.Write(", ");
                    }
                    writer.Write(variableName);
                }
                writer.Write(";");
                if (this.Count > 0)
                {
                    writer.WriteLineThenIndent();
                }
            }
            base.WriteBlockContents(writer);
        }
    }
}
