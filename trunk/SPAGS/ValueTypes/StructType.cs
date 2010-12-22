using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using SPAGS.Util;

namespace SPAGS
{
    public enum StructMemberType
    {
        Field,
        Attribute,
        Method
    }
    public partial class ValueType
    {
        public class Struct : ValueType.Named
        {
            public Struct(string name)
                : this(name, false)
            {
            }
            public Struct(string name, bool isInternalString)
                : base(name, ValueTypeCategory.Struct)
            {
                _isInternalString = isInternalString;
            }
            public bool Instantiated;
            public bool InstantiatedArray;
            public Script OwnerScript;
            public override NameHolderType NameHolderType { get { return NameHolderType.Struct; } }
            public NameDictionary Members = new NameDictionary();
            public override string ToString()
            {
                StringBuilder sb = new StringBuilder();
                using (StringWriter output = new StringWriter(sb))
                {
                    this.WriteTo(output);
                }
                return sb.ToString();
            }
            public bool IsManaged;
            public bool IsAutoPtr;
            bool _isInternalString;
            public override bool IsInternalString
            {
                get
                {
                    return _isInternalString;
                }
            }
            protected void WriteTo(TextWriter output)
            {
                if (IsInternalString) output.Write("internalstring ");
                if (IsAutoPtr) output.Write("autoptr ");
                if (IsManaged) output.Write("managed ");
                output.Write("struct " + Name + " {\n");
                foreach (StructMember member in Members.EachOf<StructMember>())
                {
                    output.Write("\t");
                    member.WriteTo(output);
                    output.Write("\n");
                }
                output.Write("};");
            }
            public override Expression CreateDefaultValueExpression()
            {
                if (this.IsManaged) return Expression.Null;
                return new Expression.AllocateStruct(this);
            }
        }
    }

    public abstract class StructMember : INameHolder
    {
        protected StructMember(string name)
        {
            this.name = name;
        }
        public object UserData;
        public abstract StructMemberType MemberType { get; }
        public NameHolderType NameHolderType { get { return NameHolderType.StructMember; } }
        private string name;
        public string Name { get { return name; } }
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            using (StringWriter output = new StringWriter(sb))
            {
                WriteTo(output);
            }
            return sb.ToString();
        }
        public abstract void WriteTo(TextWriter output);
        public class Attribute : StructMember
        {
            public Attribute(string name, ValueType type, bool isStatic, bool isArray, Function getter, Function setter)
                : base(name)
            {
                Type = type;
                IsStatic = isStatic;
                IsArray = isArray;
                Getter = getter;
                Setter = setter;
            }
            public readonly bool IsStatic;
            public readonly bool IsArray;
            public readonly ValueType Type;
            public readonly Function Getter;
            public readonly Function Setter;
            public bool IsReadOnly { get { return (Setter == null); } }
            public override StructMemberType MemberType
            {
                get { return StructMemberType.Attribute; }
            }
            public override void WriteTo(TextWriter output)
            {
                if (IsReadOnly) output.Write("readonly ");
                output.Write("import ");
                if (IsStatic) output.Write("static ");
                output.Write("attribute " + Type.Name + " " + Name);
                if (IsArray) output.Write("[]");
                output.Write(";");
            }
        }
        public class Field : StructMember
        {
            public Field(string name, ValueType type)
                : base(name)
            {
                Type = type;
            }
            public override StructMemberType MemberType
            {
                get { return StructMemberType.Field; }
            }
            public readonly ValueType Type;
            public override void WriteTo(TextWriter output)
            {
                output.Write(Type.Name + " " + Name + ";");
            }
        }
        public class Method : StructMember
        {
            public Method(string name, Function function)
                : base(name)
            {
                Function = function;
            }
            public override StructMemberType MemberType
            {
                get { return StructMemberType.Method; }
            }
            public bool IsExtender;
            public bool IsStatic;
            public readonly Function Function;
            public override void WriteTo(TextWriter output)
            {
                if (IsExtender) output.Write("/* (extender)  ");
                output.Write(Function.Signature.ReturnType.Name + " " + Name + Function.Signature.Parameters + ";");
                if (IsExtender) output.Write(" */");
            }
        }
    }
}
