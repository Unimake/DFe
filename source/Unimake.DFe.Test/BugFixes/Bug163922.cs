using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.BugFixes
{
    public class Bug163922
    {
        #region Public Methods

        [Fact]
        public void FIX_IndexWwasOutOfRange()
        {
            var file = Path.Combine(Environment.CurrentDirectory, @"BugFixes\XML\41170801761135000132550010000189121110755809-ev-110110-001.xml");
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(File.ReadAllText(file));
            _ = XMLUtility.Deserializar<ProcEventoNFe>(xmlDoc.InnerXml);
        }

        #endregion Public Methods
    }
}
