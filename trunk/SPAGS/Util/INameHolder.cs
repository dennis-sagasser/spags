using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS.Util
{
    public enum NameHolderType
    {
        BasicType, StructType, Function, Variable,
        Constant, StructMember,
        Keyword,
        EnumType, EnumValue
    }
    public interface INameHolder
    {
        string Name { get; }
        NameHolderType NameHolderType { get; }
    }
    public class NameDictionary : Dictionary<string, INameHolder>
    {
        public NameDictionary() : base() { }
        public NameDictionary(NameDictionary clone) : base(clone) { }
        public void Add(INameHolder named)
        {
            Add(named.Name, named);
        }
        public void Remove(INameHolder named)
        {
            Remove(named.Name);
        }
        public IEnumerable<T2> EachOf<T2>() where T2 : class, INameHolder
        {
            foreach (INameHolder named in this.Values)
            {
                if (named is T2) yield return (T2)named;
            }
        }
        public bool TryGetValue2<T>(string name, out T value) where T : class, INameHolder
        {
            INameHolder v;
            if (TryGetValue(name, out v)) value = v as T;
            else value = null;
            return (value != null);
        }
    }
}
