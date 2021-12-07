using System;
using System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarLoteRps
    /// </summary>
    public class ConsultarLoteRpsTest
    {
        /// <summary>
        /// Consultar Lote Rps para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        /// <param name="versaoSchema">Versão do schema do XML que será enviado</param>
        /// <param name="padraoNFSe">Padrão da NFSe</param>
        [Theory]
        [Trait("DFe", "NFSe")]

        [InlineData(TipoAmbiente.Producao, "1.00", PadraoNFSe.BETHA)]
        [InlineData(TipoAmbiente.Homologacao, "1.00", PadraoNFSe.BETHA)]

        [InlineData(TipoAmbiente.Producao, "2.02", PadraoNFSe.BETHA)]
        [InlineData(TipoAmbiente.Homologacao, "2.02", PadraoNFSe.BETHA)]

        [InlineData(TipoAmbiente.Producao, "2.01", PadraoNFSe.PRODATA)]
        //[InlineData(TipoAmbiente.Homologacao, "2.01", PadraoNFSe.PRODATA)]

        [InlineData(TipoAmbiente.Producao, "2.02", PadraoNFSe.WEBISS)]
        [InlineData(TipoAmbiente.Homologacao, "2.02", PadraoNFSe.WEBISS)]

        [InlineData(TipoAmbiente.Producao, "2.02", PadraoNFSe.NOTAINTELIGENTE)]
        [InlineData(TipoAmbiente.Homologacao, "2.02", PadraoNFSe.NOTAINTELIGENTE)]

        //[InlineData(TipoAmbiente.Producao, "2.04", PadraoNFSe.EL)]
        //[InlineData(TipoAmbiente.Homologacao, "2.04", PadraoNFSe.EL)]

        [InlineData(TipoAmbiente.Producao, "2.02", PadraoNFSe.AVMB)]
        [InlineData(TipoAmbiente.Homologacao, "2.02", PadraoNFSe.AVMB)]
        public void Consultar(TipoAmbiente tipoAmbiente, string versaoSchema, PadraoNFSe padraoNFSe)
        {
            try
            {
                var arqConfig = @"..\..\..\..\.NET Standard\Unimake.Business.DFe\Servicos\Config\Config.xml";

                Debug.Assert(File.Exists(arqConfig), "Arquivo " + arqConfig + " não foi localizado.");

                var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\ConsultarLoteRpsEnvio-ped-loterps.xml";

                Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

                var conteudoXML = new XmlDocument();
                conteudoXML.Load(arqXML);

                //Pegar o arquivo de configurações para buscar todos os municípios implementados do padrão em questão para testar 1 por 1
                var xmlConfig = new XmlDocument();
                xmlConfig.Load(arqConfig);

                var configuracoesList = xmlConfig.GetElementsByTagName("Configuracoes");

                foreach(var configuracoesNode in configuracoesList)
                {
                    var configuracoesElement = (XmlElement)configuracoesNode;

                    var arquivoList = configuracoesElement.GetElementsByTagName("Arquivo");

                    foreach(var arquivoNode in arquivoList)
                    {
                        var arquivoElement = (XmlElement)arquivoNode;

                        if(arquivoElement.GetElementsByTagName("PadraoNFSe").Count > 0)
                        {
                            if(arquivoElement.GetElementsByTagName("PadraoNFSe")[0].InnerText == padraoNFSe.ToString())
                            {
                                var municipio = Convert.ToInt32(arquivoElement.GetAttribute("ID"));

                                var configuracao = new Configuracao
                                {
                                    TipoDFe = TipoDFe.NFSe,
                                    CertificadoDigital = PropConfig.CertificadoDigital,
                                    TipoAmbiente = tipoAmbiente,
                                    CodigoMunicipio = municipio,
                                    Servico = Servico.NFSeConsultarLoteRps,
                                    SchemaVersao = versaoSchema
                                };

                                var consultarLoteRps = new ConsultarLoteRps(conteudoXML, configuracao);
                                consultarLoteRps.Executar();

                                Debug.Assert(!string.IsNullOrWhiteSpace(consultarLoteRps.RetornoWSString), "Não conseguiu nenhum retorno do webservice da prefeitura de " + configuracao.Nome + " - IBGE: " + configuracao.CodigoMunicipio + " - Padrão: " + configuracao.PadraoNFSe.ToString());
                            }
                        }
                    }
                }
            }
            catch(Exception ex)
            {
                Debug.Assert(false, ex.Message, ex.StackTrace);
            }
        }
    }
}