using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;

namespace RedHerringFarm.ImageSheets
{
    public abstract class ImageSheetEntry
    {
        protected ImageSheetEntry()
        {
        }
        public abstract int Width { get; }
        public abstract int Height { get; }
        private int x, y, number;
        private ImageSheet owningSheet;
        public int X
        {
            get { return x; }
            internal set { x = value; }
        }
        public int Y
        {
            get { return y; }
            internal set { y = value; }
        }
        public ImageSheet OwningSheet
        {
            get { return owningSheet; }
            internal set { owningSheet = value; }
        }
        public int EntryNumber
        {
            get
            {
                return number;
            }
            internal set { number = value; }
        }
        public virtual string UniqueKey
        {
            get { return null; }
        }
        public abstract void Draw(Graphics g);
        public abstract void Draw(BitmapData bdata);
    }
}
