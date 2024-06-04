using System;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.BugFixes
{
    public class Bug164077
    {
        #region Public Methods

        [Fact]
        public void FIX_NFe_InfNFe_Null()
        {
            var file = Path.Combine(Environment.CurrentDirectory, @"BugFixes\XML\41190976430438002891550020000200981772597373-procNFe.xml");
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(File.ReadAllText(file));
            var nfe = XMLUtility.Deserializar<NfeProc>(xmlDoc.InnerXml);
            Assert.NotNull(nfe);
            Assert.NotNull(nfe.NFe);
            Assert.NotNull(nfe.NFe.InfNFe);
            Assert.NotNull(nfe.NFe.InfNFe?.FirstOrDefault());
            Assert.NotNull(nfe.ProtNFe);
            Assert.NotNull(nfe.ProtNFe.InfProt);
            Assert.NotNull(nfe.ProtNFe.InfProt.NProt);
            Assert.NotNull(nfe.NFe.Signature);
            Assert.NotNull(nfe.NFe.Signature.SignatureValue);
            Assert.NotNull(nfe.NFe.Signature.KeyInfo);
            Assert.NotNull(nfe.NFe.Signature.SignedInfo);
        }

        #endregion Public Methods
    }
}