using System;
using System.Collections.Generic;
using System.Text;

namespace SPAGS.Util
{
    public interface IUserDataHolder
    {
        object UserData { get; set; }
    }
    public interface IUserData<Target>
    {
        void Init(Target target);
    }
    public static class UserData<Holder, Target>
        where Target : class, IUserData<Holder>, new()
        where Holder : IUserDataHolder
    {
        public static Target Get(Holder holder)
        {
            Target ud = holder.UserData as Target;
            if (ud == null)
            {
                holder.UserData = ud = new Target();
                ud.Init(holder);
            }
            return ud;
        }
    }
}
