using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using System.IO;

namespace RedHerringFarm
{
    [XmlRoot(ElementName="Settings")]
    public class ExporterSettings
    {
        private string pngTool;
        public string PngTool
        {
            get { return pngTool; }
            set { pngTool = value; }
        }

        public readonly static ExporterSettings Default;
        static ExporterSettings()
        {
            Default = new ExporterSettings();
        }

        public void WriteXml(XmlWriter output)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(ExporterSettings), "");
            serializer.Serialize(output, this);
        }

        public static ExporterSettings ReadXml(XmlNode node)
        {
            XmlSerializer serializer = new XmlSerializer(typeof(ExporterSettings), "");
            using (XmlNodeReader nodeReader = new XmlNodeReader(node))
            {
                return (ExporterSettings)serializer.Deserialize(nodeReader);
            }
        }
    }
}
