using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.BugFixes
{
    public class ProcEventoNFeDesserializacaoBugFixesTest
    {
        #region Public Methods

        [Fact]
        public void DeveDesserializarProcEventoNFeSemIndexOutOfRangeException()
        {
            var file = Path.Combine(Environment.CurrentDirectory, @"NFe\Resources\BugFixes\41170801761135000132550010000189121110755809-ev-110110-001.xml");
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(File.ReadAllText(file));
            _ = XMLUtility.Deserializar<ProcEventoNFe>(xmlDoc.InnerXml);
        }

        #endregion Public Methods
    }
}
