using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.BugFixes
{
    public class ProcEventoNFeBugFixesTest
    {
        #region Public Methods

        [Fact]
        public void DeveDesserializarProcEventoNFeSemArgumentOutOfRangeException()
        {
            var file = Path.Combine(Environment.CurrentDirectory, @"NFe\Resources\BugFixes\Bug162066.xml");
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(File.ReadAllText(file));
            XMLUtility.Deserializar<ProcEventoNFe>(xmlDoc.InnerXml);
        }

        #endregion Public Methods
    }
}
