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
                SchemaVersao = versaoSchema,
                MunicipioSenha = "123456",
                MunicipioUsuario = "01001001000113"
            };

            var consultarSituacaoLoteRps = new ConsultarSituacaoLoteRps(conteudoXML, configuracao);

            #region Tratamento de Erros

            try
            {
                Assert.Multiple(() => TestUtility.AnalisaResultado(consultarSituacaoLoteRps));
            }
            catch (System.Exception ex)
            {
                switch (padraoNFSe)
                {
                    // Alguns municípios do padrão TINUS exigem dados reais para concluir o envio.
                    // Quando recebem dados fictícios, podem retornar erro 500.
                    // Esta exceção evita falha indevida no teste unitário.
                    // Recomenda-se remover esta adaptação periodicamente para validar novamente a comunicação.
                    case PadraoNFSe.TINUS:
                        Assert.Contains("Este contexto necessita de dados reais", ex.Message);
                        Assert.True(
                            ex.Message.Contains("internal server error") ||
                            ex.Message.Contains("Internal server error") ||
                            ex.Message.Contains("Server Error"),
                            ex.Message);
                        break;

                    // Alguns municípios do padrão QUASAR retornam erro em ambiente de homologação.
                    // Nesses casos, o retorno vem como texto/log de erro, serviço temporariamente indisponível.
                    case PadraoNFSe.QUASAR:
                        Assert.Equal(TipoAmbiente.Homologacao, tipoAmbiente);
                        Assert.Contains("503 Service Temporarily Unavailable", ex.Message);
                        break;

                    default:
                        throw;
                }
            }

            #endregion
        }
    }
}