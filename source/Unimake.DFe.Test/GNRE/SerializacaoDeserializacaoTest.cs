using Diag = System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Testar a serialização e deserialização dos XMLs de GNRE
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e deserialização do XML ConsultaGNRE
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(@"..\..\..\GNRE\Resources\TLote_ConsultaGNRE.xml")]
        [InlineData(@"..\..\..\GNRE\Resources\TLote_ConsultaGNRE1.xml")]

        public void SerializacaoDeserializacao(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<TLoteConsultaGNRE>(doc);
            var doc2 = xml.GerarXML();


            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML do Resultado do Lote da Consulta
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(@"..\..\..\GNRE\Resources\TConsLote_GNRE.xml")]
        public void SerializacaoDeserializacaoTConsLoteConsGNRE(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<TConsLoteConsGNRE>(doc);
            var doc2 = xml.GerarXML();


            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}