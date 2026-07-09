using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Schema;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Xunit;

namespace Unimake.DFe.Test.DCe.Serializacao
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "DCe")]
        [InlineData(@"..\..\..\DCe\Resources\dce.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\dce_v1.00.xsd")]
        public void SerializacaoDesserializacaoDCe(string arqXML, string arqSchema)
        {
            var doc = CarregarValidando(arqXML, arqSchema);

            var dce = new Unimake.Business.DFe.Xml.DCe.DCe();
            var xml = dce.LerXML<Unimake.Business.DFe.Xml.DCe.DCe>(doc);

            Assert.Equal("Emitente Teste", xml.InfDCe.Emit.XNome);
            Assert.Equal("Produto teste", xml.InfDCe.Det[0].Prod.XProd);
            Assert.Equal(10.00, xml.InfDCe.Total.VDC);
            Assert.Equal(ModalidadeTransporteDCe.Correios, xml.InfDCe.Transp.ModTrans);
            Assert.Equal("https://dce.test/qr", xml.InfDCeSupl?.QrCodDCe);

            var doc2 = xml.GerarXML();
            ValidarSchema(doc2, arqSchema);

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        [Theory]
        [Trait("DFe", "DCe")]
        [InlineData(@"..\..\..\DCe\Resources\protDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\dce_v1.00.xsd")]
        public void SerializacaoDesserializacaoProtDCe(string arqXML, string arqSchema)
        {
            var doc = CarregarValidando(arqXML, arqSchema);

            var dce = new Unimake.Business.DFe.Xml.DCe.ProtDCe();
            var xml = dce.LerXML<Unimake.Business.DFe.Xml.DCe.ProtDCe>(doc);
            var doc2 = xml.GerarXML();

            Assert.Equal("3526050000000000019959900000000011000001234567", xml.InfProt.ChDCe);
            Assert.Equal(100, xml.InfProt.CStat);
            ValidarSchema(doc2, arqSchema);
            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        [Theory]
        [Trait("DFe", "DCe")]
        [InlineData(@"..\..\..\DCe\Resources\retDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\retDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.RetDCe))]
        [InlineData(@"..\..\..\DCe\Resources\consSitDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\consSitDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.ConsSitDCe))]
        [InlineData(@"..\..\..\DCe\Resources\retConsSitDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\retConsSitDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.RetConsSitDCe))]
        [InlineData(@"..\..\..\DCe\Resources\consStatServDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\consStatServDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.ConsStatServDCe))]
        [InlineData(@"..\..\..\DCe\Resources\retConsStatServDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\retConsStatServDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.RetConsStatServDCe))]
        [InlineData(@"..\..\..\DCe\Resources\eventoDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\eventoDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.EventoDCe))]
        [InlineData(@"..\..\..\DCe\Resources\retEventoDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\retEventoDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.RetEventoDCe))]
        [InlineData(@"..\..\..\DCe\Resources\procEventoDCe.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\procEventoDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.ProcEventoDCe))]
        [InlineData(@"..\..\..\DCe\Resources\dceProc.xml", @"..\.NET Standard\Unimake.Business.DFe\Xml\Schemas\DCe\procDCe_v1.00.xsd", typeof(Unimake.Business.DFe.Xml.DCe.DCeProc))]
        public void SerializacaoDesserializacaoDocumentosDCe(string arqXML, string arqSchema, Type type)
        {
            var doc = CarregarValidando(arqXML, arqSchema);
            var instancia = (XMLBase)Activator.CreateInstance(type);
            var metodo = typeof(XMLBase).GetMethod(nameof(XMLBase.LerXML)).MakeGenericMethod(type);
            var xml = (XMLBase)metodo.Invoke(instancia, new object[] { doc });

            var doc2 = xml.GerarXML();

            ValidarSchema(doc2, arqSchema);
            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL esta diferente do conteudo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        private static XmlDocument CarregarValidando(string arqXML, string arqSchema)
        {
            arqXML = ResolverCaminho(arqXML);
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");
            var doc = new XmlDocument();
            doc.Load(arqXML);
            ValidarSchema(doc, arqSchema);
            return doc;
        }

        private static void ValidarSchema(XmlDocument doc, string arqSchema)
        {
            arqSchema = ResolverCaminho(arqSchema);
            Assert.True(File.Exists(arqSchema), "Schema " + arqSchema + " não foi localizado para a validação.");

            Assert.NotNull(doc.DocumentElement);
            Assert.Equal("http://www.portalfiscal.inf.br/dce", doc.DocumentElement.NamespaceURI);
        }

        private static string ResolverCaminho(string caminho)
        {
            if (File.Exists(caminho))
            {
                return caminho;
            }

            var diretorioProjetoTeste = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, @"..\..\.."));
            var caminhoProjetoTeste = Path.GetFullPath(Path.Combine(diretorioProjetoTeste, caminho));

            return caminhoProjetoTeste;
        }
    }
}
