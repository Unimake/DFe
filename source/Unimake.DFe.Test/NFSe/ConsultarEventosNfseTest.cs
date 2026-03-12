using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarEventosNfse
    /// </summary>
    public class ConsultarEventosNfseTest
    {
        /// <summary>
        /// Monta o parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarEvento");

        /// <summary>
        /// Consultar Eventos NFSe para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void ConsultarEventosNfse(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            var nomeXMLEnvio = "ConsultarEventosNfse-ped-consevennfse.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeConsultarNfsePorRps,
                SchemaVersao = versaoSchema,
                MunicipioToken = "99n0556af8e4218e05b88e266fhca55be17b14a4495c269d1db0af57f925f04e77c38f9870842g5g60b6827a9fje8ec9", //Tem município que exige token, então já vamos deixar algo definido para que utilize nos padrões necessários durante o teste unitário. Não é obrigatório para todos os padrões e será utilizado somente nos que solicitam.
                MunicipioSenha = "123456",
                MunicipioUsuario = "01001001000113"
            };

            var consultarEventos = new ConsultarEvento(conteudoXML, configuracao);
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarEventos));

            var resultado = consultarEventos.Result;
            Assert.NotNull(resultado);

            Assert.NotNull(resultado.DataHoraProcessamento);
            Assert.NotNull(resultado.VersaoAplicativo);
            Assert.InRange(resultado.TipoAmbiente, 1, 2);

            if (resultado.Eventos != null)
            {
                Assert.NotNull(resultado.Eventos.ChaveAcesso);
                Assert.NotNull(resultado.Eventos.TipoEvento);
                Assert.NotNull(resultado.Eventos.ArquivoXml);
                Assert.NotNull(resultado.Eventos.ArquivoXml.Evento);

                var evento = resultado.Eventos.ArquivoXml.Evento;
                Assert.NotNull(evento.InfEvento);
                Assert.NotNull(evento.InfEvento.Id);
                Assert.NotNull(evento.InfEvento.PedRegEvento);

                var xmlEnvio = new Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento();
                xmlEnvio = xmlEnvio.LerXML<Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento>(conteudoXML);

                Assert.Equal(xmlEnvio.InfConsPedRegEvento.ChNFSe, resultado.Eventos.ChaveAcesso);
                Assert.Equal(xmlEnvio.InfConsPedRegEvento.TipoEvento, resultado.Eventos.TipoEvento);
            }
            else if (resultado.Erro != null)
            {
                Assert.NotNull(resultado.Erro.Codigo);
                Assert.NotNull(resultado.Erro.Descricao);
            }
            else
            {
                Assert.Fail("Retorno não contém eventos nem erro");
            }
        }
    }
}
