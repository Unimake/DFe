using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarSituacaoLoteRps
    /// </summary>
    public class ConsultarSituacaoLoteRpsTest
    {
        /// <summary>
        /// Monta o parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarSituacaoLoteRps");

        /// <summary>
        /// Consultar Nfse Serviço Prestado para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarSituacaoLoteRps(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            var nomeXMLEnvio = "ConsultarSituacaoLoteRpsEnvio-ped-sitloterps.xml";

            string arqXML;

            if (codMunicipio == 4125506)
            {
                arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\3.00 - SaoJoseDosPinhais\\" + nomeXMLEnvio;
            }
            else
            {
                arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;
            }

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeConsultarSituacaoLoteRps,
                SchemaVersao = versaoSchema
            };

            var consultarSituacaoLoteRps = new ConsultarSituacaoLoteRps(conteudoXML, configuracao);
            consultarSituacaoLoteRps.Executar();
        }
    }
}