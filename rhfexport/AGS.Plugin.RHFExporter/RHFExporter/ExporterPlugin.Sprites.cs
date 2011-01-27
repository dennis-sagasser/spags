using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.Drawing.Imaging;
using RedHerringFarm.ImageSheets;

namespace RedHerringFarm
{
	public partial class ExporterPlugin
	{
        List<SpriteImageSheetEntry> spriteImageSheetEntries;
        private void PrepareSpriteImageSheets()
        {
            spriteImageSheetEntries = new List<SpriteImageSheetEntry>();
            ImageSheet nonAlphaSpriteImageSheet = new ImageSheet(2880, 2880, 0, 0);
            nonAlphaSpriteImageSheet.ClearColor = HacksAndKludges.GetTransparencyColor();
            nonAlphaSpriteImageSheet.MakeTransparent = true;
            ImageSheet alphaSpriteImageSheet = new ImageSheet(2880, 2880, 0, 0);
            foreach (AGS.Types.Sprite sprite in GetAllSprites())
            {
                if (sprite == null || sprite.Width == 0 || sprite.Height == 0)
                {
                    spriteImageSheetEntries.Add(null);
                    continue;
                }
                SpriteImageSheetEntry entry = new SpriteImageSheetEntry(editor, sprite);
                spriteImageSheetEntries.Add(entry);
                if (sprite.AlphaChannel)
                {
                    alphaSpriteImageSheet.AddEntry(entry);
                }
                else
                {
                    nonAlphaSpriteImageSheet.AddEntry(entry);
                }
            }
            if (!nonAlphaSpriteImageSheet.IsEmpty)
            {
                if (!nonAlphaSpriteImageSheet.Pack())
                {
                    throw new Exception("Unable to pack sprites!");
                }
                GameImageSheets.Add(nonAlphaSpriteImageSheet);
            }
            if (!alphaSpriteImageSheet.IsEmpty)
            {
                if (!alphaSpriteImageSheet.Pack())
                {
                    throw new Exception("Unable to pack sprites!");
                }
                GameImageSheets.Add(alphaSpriteImageSheet);
            }
        }
        private void WriteSpritesJson(JsonWriter output)
        {
            WriteSpritesJson(output, null);
        }
        private void WriteSpritesJson(JsonWriter output, string key)
        {
            using (output.BeginArray(key))
            {
                foreach (SpriteImageSheetEntry entry in spriteImageSheetEntries)
                {
                    if (entry == null)
                    {
                        output.WriteNull();
                    }
                    else
                    {
                        using (output.BeginObject())
                        {
                            output.WriteValue("s", GameImageSheets.IndexOf(entry.OwningSheet));
                            output.WriteValue("n", entry.EntryNumber);
                        }
                    }
                }
            }
        }

        private List<AGS.Types.Sprite> GetAllSprites()
        {
            List<AGS.Types.Sprite> allSprites = new List<AGS.Types.Sprite>();
            AddSpritesFromFolder(editor.CurrentGame.Sprites, allSprites);
            return allSprites;
        }
        private void AddSpritesFromFolder(AGS.Types.ISpriteFolder folder, List<AGS.Types.Sprite> list)
        {
            foreach (AGS.Types.Sprite sprite in folder.Sprites)
            {
                while (list.Count <= sprite.Number)
                {
                    list.Add(null);
                }
                list[sprite.Number] = sprite;
            }
            foreach (AGS.Types.ISpriteFolder subfolder in folder.SubFolders)
            {
                AddSpritesFromFolder(subfolder, list);
            }
        }
	}
    public class SpriteImageSheetEntry : ImageSheetEntry
    {
        public SpriteImageSheetEntry(AGS.Types.IAGSEditor editor, AGS.Types.Sprite sprite)
        {
            this.editor = editor;
            TheSprite = sprite;
        }
        private readonly AGS.Types.IAGSEditor editor;
        public readonly AGS.Types.Sprite TheSprite;
        public override int Width
        {
            get { return TheSprite.Width; }
        }
        public override int Height
        {
            get { return TheSprite.Height; }
        }
        public override void Draw(Graphics g)
        {
            g.DrawImage(editor.GetSpriteImage(TheSprite.Number), X, Y);
        }
        public override void Draw(BitmapData bdata)
        {
            throw new NotImplementedException();
        }
    }
}
