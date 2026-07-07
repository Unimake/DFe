using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarNFse
    /// </summary>
    public class ConsultarNfseTest
    {
        /// <summary>
        /// Monta o parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarNfse");

        /// <summary>
        /// Consultar Situação para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarNfse(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            var nomeXMLEnvio = "ConsultarNfseEnvio-ped-sitnfse.xml";

            string arqXML;

            if (codMunicipio == 4125506)
            {
                arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\3.00 - SaoJoseDosPinhais\\" + nomeXMLEnvio;
            }
            else
            {
                switch (padraoNFSe)
                {
                    case PadraoNFSe.NOBESISTEMAS:
                        arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + tipoAmbiente.ToString() + "\\" + nomeXMLEnvio;
                        break;

                    default:
                        arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;
                        break;
                }
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
                Servico = Servico.NFSeConsultarNfse,
                SchemaVersao = versaoSchema,
                MunicipioToken = "99n0556af8e4218e05b88e266fhca55be17b14a4495c269d1db0af57f925f04e77c38f9870842g5g60b6827a9fje8ec9", //Tem município que exige token, então já vamos deixar algo definido para que utilize nos padrões necessários durante o teste unitário. Não é obrigatório para todos os padrões e será utilizado somente nos que solicitam.
                MunicipioSenha = "123456",
                MunicipioUsuario = "01001001000113"
            };

            var consultarNfse = new ConsultarNfse(conteudoXML, configuracao);

            try
            {
                Assert.Multiple(() => TestUtility.AnalisaResultado(consultarNfse));
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

                    // O padrão único WEBFICOS retorna erro nos serviços de consulta e cancelamento.
                    // Nesses casos, o retorno vem como texto/log de erro, erro 500 ou erro 404.
                    case PadraoNFSe.WEBFISCO:
                        Assert.True(
                            ex.Message.Contains("erro 500 do servidor") ||
                            ex.Message.Contains("(404) Not Found"),
                            ex.Message);
                        break;

                    default:
                        throw;
                }

            }
        }
    }
}
