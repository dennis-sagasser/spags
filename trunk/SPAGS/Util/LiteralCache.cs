using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS
{
    internal class LiteralCache<LiteralT, TokenT> : Dictionary<LiteralT, WeakReference> where TokenT : Token
    {
        internal bool TryGetCached(LiteralT literal, out TokenT cached)
        {
            WeakReference weakRef;
            if (this.TryGetValue(literal, out weakRef))
            {
                cached = weakRef.Target as TokenT;
            }
            else
            {
                cached = null;
            }
            return (cached != null);
        }
        internal TokenT AddToCache(LiteralT literal, TokenT token)
        {
            this[literal] = new WeakReference(token);
            return token;
        }
    }
}
