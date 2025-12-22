using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    public class RecepcionarEventosNfseTest
    {
        /// <summary>
        /// Parâmetros para os testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("RecepcionarEventosNfse");

        /// <summary>
        /// Testar o serviço de recepção de eventos da NFSe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void RecepcionarEventos(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            var nomeXMLEnvio = "PedRegEvento-ped-regev.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi encontrado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeRecepcionarEventosDiversos,
                SchemaVersao = versaoSchema,
                PadraoNFSe = padraoNFSe
            };

            var recepcionarEventos = new RecepcionarEventosNfse(conteudoXML, configuracao);

            Assert.Multiple(() => TestUtility.AnalisaResultado(recepcionarEventos));
        }
    }
}