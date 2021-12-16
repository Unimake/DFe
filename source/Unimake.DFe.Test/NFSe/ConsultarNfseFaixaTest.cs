using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarNfseFaixa
    /// </summary>
    public class ConsultarNfseFaixaTest
    {
        /// <summary>
        /// Monta o parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => TestUtility.PreparaDadosCenario("ConsultarNfseFaixa");

        /// <summary>
        /// Consulta NFSe por Faixa para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [MemberData(nameof(Parametros))]
        public void Consultar(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio, string nomeMunicipio)
        {
            var nomeXMLEnvio = "ConsultarNfseFaixaEnvio-ped-sitnfse.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

            Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            try
            {
                var conteudoXML = new XmlDocument();
                conteudoXML.Load(arqXML);

                var configuracao = new Configuracao
                {
                    TipoDFe = TipoDFe.NFSe,
                    CertificadoDigital = PropConfig.CertificadoDigital,
                    TipoAmbiente = tipoAmbiente,
                    CodigoMunicipio = codMunicipio,
                    Servico = Servico.NFSeConsultarNfseFaixa,
                    SchemaVersao = versaoSchema
                };

                var consultarNfseFaixa = new ConsultarNfseFaixa(conteudoXML, configuracao);
                consultarNfseFaixa.Executar();
            }
            catch(Exception ex)
            {
                Debug.Assert(false, "Falha na hora de consumir o serviço: " + nomeMunicipio + " - IBGE: " + codMunicipio + " - Padrão: " + padraoNFSe.ToString() + " - Versão schema: " + versaoSchema + "\r\nExceção: " + ex.Message, ex.StackTrace);
            }
        }
    }
}