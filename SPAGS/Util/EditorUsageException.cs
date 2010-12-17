using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS.Util
{
    public class EditorUsageException : Exception
    {
        public EditorUsageException(string message) : base(message) { }
    }
}
