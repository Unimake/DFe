using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.BugFixes
{
    public class Bug162066
    {
        #region Public Methods

        [Fact]
        public void ArgumentOutOfRangeException()
        {
            var file = Path.Combine(Environment.CurrentDirectory, @"BugFixes\XML\Bug162066.xml");
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(File.ReadAllText(file));
            XMLUtility.Deserializar<ProcEventoNFe>(xmlDoc.InnerXml);
        }

        #endregion Public Methods
    }
}