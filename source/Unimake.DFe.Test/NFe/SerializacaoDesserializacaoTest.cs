using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\enviNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\enviNFe2.xml")]        
        public void SerializacaoDesserializacaoEnviNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new EnviNFe();
            xml = xml.LerXML<EnviNFe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var docGerado = xml.GerarXML();


            Assert.True(doc.InnerText == docGerado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML NFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\NFe1.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe2.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe3.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe4.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFeRTC1.xml")]
        public void SerializacaoDesserializacaoNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var enviNFe = new EnviNFe
            {
                IdLote = "000000000000001",
                Versao = "4.00",
                NFe =
                [
                    XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(doc.OuterXml)
                ]
            };

            var doc2 = enviNFe.GerarXML();

            Assert.True(doc.InnerText == doc2.GetElementsByTagName("NFe")[0].InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML NfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\31260655555555555555558900999999991888888880-procNfe.xml")]
        public void SerializacaoDesserializacaoNfeProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new NfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ResEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resEvento.xml")]
        [InlineData(@"..\..\..\NFe\Resources\resEvento_CNPJAlfanumerico.xml")]
        public void SerializacaoDesserializacaoResEvento(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResEvento>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização de múltiplas mensagens no protocolo da NFe
        /// </summary>
        [Fact]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        public void SerializacaoDesserializacaoInfProtComMultiplasMensagens()
        {
            const string xml = "<infProt xmlns=\"http://www.portalfiscal.inf.br/nfe\"><tpAmb>1</tpAmb><verAplic>verAplic1</verAplic><chNFe>11170706117473000150550010000000011123456781</chNFe><dhRecbto>2017-07-12T09:44:07-03:00</dhRecbto><nProt>123456789012345</nProt><digVal>digVal1</digVal><cStat>100</cStat><xMotivo>Autorizado</xMotivo><cMsg>1</cMsg><xMsg>Mensagem 1</xMsg><cMsg>2</cMsg><xMsg>Mensagem 2</xMsg></infProt>";

            var root = new System.Xml.Serialization.XmlRootAttribute("infProt")
            {
                Namespace = "http://www.portalfiscal.inf.br/nfe"
            };
            var serializer = new System.Xml.Serialization.XmlSerializer(typeof(InfProt), root);
            InfProt infProt;

            using (var reader = new StringReader(xml))
            {
                infProt = (InfProt)serializer.Deserialize(reader);
            }

            Assert.Equal(2, infProt.Mensagem.Count);
            Assert.Equal("1", infProt.CMsg);
            Assert.Equal("Mensagem 1", infProt.XMsg);
            Assert.Equal("2", infProt.Mensagem[1].CMsg);
            Assert.Equal("Mensagem 2", infProt.Mensagem[1].XMsg);

            var namespaces = new System.Xml.Serialization.XmlSerializerNamespaces();
            namespaces.Add(string.Empty, "http://www.portalfiscal.inf.br/nfe");

            using (var writer = new StringWriter())
            {
                serializer.Serialize(writer, infProt, namespaces);
                var xmlGerado = writer.ToString();

                var docGerado = new XmlDocument();
                docGerado.LoadXml(xmlGerado);

                var nsmgr = new XmlNamespaceManager(docGerado.NameTable);
                nsmgr.AddNamespace("nfe", "http://www.portalfiscal.inf.br/nfe");

                Assert.Equal("1", docGerado.SelectSingleNode("//nfe:cMsg[1]", nsmgr)?.InnerText);
                Assert.Equal("Mensagem 1", docGerado.SelectSingleNode("//nfe:xMsg[1]", nsmgr)?.InnerText);
                Assert.Equal("2", docGerado.SelectSingleNode("//nfe:cMsg[2]", nsmgr)?.InnerText);
                Assert.Equal("Mensagem 2", docGerado.SelectSingleNode("//nfe:xMsg[2]", nsmgr)?.InnerText);
            }
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ResNFe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\resNFe_CNPJAlfanumerico.xml")]
        public void SerializacaoDesserializacaoResNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnvEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110140.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110192.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110193.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110150.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110150_2.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110150_3.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110750.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110751.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110001.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_112110.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_112120.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_112150.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211128.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_212110.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_212120.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_412120.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_112130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_412130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_112140.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211110.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211120.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211124.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211140.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_211150.xml")]
        public void SerializacaoDesserializacaoEnvEvento(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EnvEvento>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

            //Vou validar o XML em busca de erros
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var recepcaoVento = new Business.DFe.Servicos.NFe.RecepcaoEvento(doc.OuterXml, configuracao);
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML DistDFeInt
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ChNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CNPJ.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CNPJAlfanumerico.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CPF.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ComUFAutor.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_NSU.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_SemUFAutor.xml")]
        public void SerializacaoDesserializacaoDistDFeInt(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<DistDFeInt>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetDistDFeInt
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\retDistDFeInt_DocZipSemNSU.xml")]
        public void SerializacaoDesserializacaoRetDistDFeInt(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetDistDFeInt>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsSitNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe2.xml")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe3.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitNFe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML procEventoNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_610130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_610131.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_790700.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_630690.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_990900.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_990910.xml")]
        public void SerializacaoDesserializacaoProcEventoNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ProcEventoNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML procEventoNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\procInutNFe.xml")]
        public void SerializacaoDesserializacaoProcInutNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ProcInutNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
