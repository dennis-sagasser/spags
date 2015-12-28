
```
namespace SPAGS.Util
{

  /* Name Holders */

  enum NameHolderType
  {
    BasicType, StructType, Function, Variable,
    Constant, StructMember, Keyword,
    EnumType, EnumValue
  }

  interface INameHolder
  {
    string Name;
    NameHolderType NameHolderType;
  }

  class NameDictionary : Dictionary<string, INameHolder>
  {
    NameDictionary(NameDictionary clone); // clone constructor
    void Add(INameHolder named);
    void Remove(INameHolder named);
    IEnumerable<T2> EachOf<T2>() where T2 : class, INameHolder;
    TryGetValue2<T>(string name, out T value) where T : class, INameHolder;
  }

}
```