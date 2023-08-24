using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    public static class TestUtility
    {
        /// <summary>
        /// Prepara uma Lista de parâmetros para montagem dos cenários que devem ser testados para os serviços da NFSe
        /// </summary>
        /// <returns></returns>
        public static List<object[]> PreparaDadosCenario(string servico)
        {
            var dados = new List<object[]>();

            var pastaConfigGeral = @"..\..\..\..\.NET Standard\Unimake.Business.DFe\Servicos\Config";
            var pastaConfigNFSe = pastaConfigGeral + "\\NFSe";
            var arqConfigGeral = pastaConfigGeral + "\\Config.xml"; //Arquivo de configuração que contem os municípios implementados

            Assert.True(File.Exists(arqConfigGeral), "Arquivo de configuração geral (" + arqConfigGeral + ") não foi localizado.");

            //Buscar todos os padrões de NFSe implementados para testar os municípios
            foreach (var nomePadrao in Enum.GetNames(typeof(PadraoNFSe)))
            {
                var padraoNFSe = (PadraoNFSe)Enum.Parse(typeof(PadraoNFSe), nomePadrao);

                if (padraoNFSe == PadraoNFSe.None)
                {
                    continue;
                }

                //Pegar o arquivo de configurações para buscar todos os municípios implementados do padrão em questão para testar 1 por 1
                var xmlConfig = new XmlDocument();
                xmlConfig.Load(arqConfigGeral);

                var configuracoesList = xmlConfig.GetElementsByTagName("Configuracoes");

                foreach (var configuracoesNode in configuracoesList)
                {
                    var configuracoesElement = (XmlElement)configuracoesNode;

                    var arquivoList = configuracoesElement.GetElementsByTagName("Arquivo");

                    foreach (var arquivoNode in arquivoList)
                    {
                        var arquivoElement = (XmlElement)arquivoNode;

                        if (arquivoElement.GetElementsByTagName("PadraoNFSe").Count > 0)
                        {
                            if (arquivoElement.GetElementsByTagName("PadraoNFSe")[0].InnerText == padraoNFSe.ToString())
                            {
                                var codMunicipio = Convert.ToInt32(arquivoElement.GetAttribute("ID"));
                                var nomeMunicipio = arquivoElement.GetElementsByTagName("Nome")[0].InnerText;

                                var ambientesVersoesSchema = TestUtility.BuscarDadosConfigNFSe(pastaConfigNFSe + "\\" + arquivoElement.GetElementsByTagName("ArqConfig")[0].InnerText, servico);

                                foreach (var ambienteVersaoSchema in ambientesVersoesSchema)
                                {
                                    if (!string.IsNullOrWhiteSpace(ambienteVersaoSchema.VersaoSchema))
                                    {
                                        dados.Add(new object[] { ambienteVersaoSchema.TipoAmbiente, padraoNFSe, ambienteVersaoSchema.VersaoSchema, codMunicipio });
                                        //dados.Add(new object[] { ambienteVersaoSchema.TipoAmbiente, padraoNFSe, ambienteVersaoSchema.VersaoSchema, codMunicipio, nomeMunicipio });
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return dados;
        }

        /// <summary>
        /// Buscar algumas informações das configurações da NFSe para montar os cenários de testes.
        /// </summary>
        /// <param name="arqConfigMunicipio">Nome do arquivo de configuração específico do município</param>
        /// <returns>Lista com dados das configurações da NFSe</returns>
        public static List<DadosConfigNFSe> BuscarDadosConfigNFSe(string arqConfigMunicipio, string servico)
        {
            Assert.True(File.Exists(arqConfigMunicipio), "Arquivo " + arqConfigMunicipio + " não foi localizado.");

            var retornar = new List<DadosConfigNFSe>();

            var xmlConfig = new XmlDocument();
            xmlConfig.Load(arqConfigMunicipio);

            var configuracoesList = xmlConfig.GetElementsByTagName("Configuracoes");

            foreach (var configuracoesNode in configuracoesList)
            {
                var configuracoesElement = (XmlElement)configuracoesNode;
                var servicosList = configuracoesElement.GetElementsByTagName("Servicos");

                foreach (var servicosNode in servicosList)
                {
                    var servicosElement = (XmlElement)servicosNode;

                    if (servicosElement.GetAttribute("ID").ToLower() == "nfse")
                    {
                        var tagServicoList = servicosElement.GetElementsByTagName(servico);

                        foreach (var tagServicoNode in tagServicoList)
                        {
                            var tagServicoElement = (XmlElement)tagServicoNode;

                            if (tagServicoElement.GetElementsByTagName("WebEnderecoHomologacao").Count > 0)
                            {

                                if (!string.IsNullOrWhiteSpace(tagServicoElement.GetElementsByTagName("WebEnderecoHomologacao")[0].InnerText))
                                {
                                    retornar.Add(new DadosConfigNFSe
                                    {
                                        TipoAmbiente = TipoAmbiente.Homologacao,
                                        VersaoSchema = tagServicoElement.GetAttribute("versao")
                                    });
                                }
                            }
                            else
                            {
                                if (tagServicoElement.GetElementsByTagName("RequestURIHomologacao").Count > 0)
                                {
                                    if (!string.IsNullOrWhiteSpace(tagServicoElement.GetElementsByTagName("RequestURIHomologacao")[0].InnerText))
                                    {
                                        retornar.Add(new DadosConfigNFSe
                                        {
                                            TipoAmbiente = TipoAmbiente.Homologacao,
                                            VersaoSchema = tagServicoElement.GetAttribute("versao")
                                        });
                                    }
                                }
                            }

                            if (tagServicoElement.GetElementsByTagName("WebEnderecoProducao").Count > 0)
                            {
                                if (!string.IsNullOrWhiteSpace(tagServicoElement.GetElementsByTagName("WebEnderecoProducao")[0].InnerText))
                                {
                                    retornar.Add(new DadosConfigNFSe
                                    {
                                        TipoAmbiente = TipoAmbiente.Producao,
                                        VersaoSchema = tagServicoElement.GetAttribute("versao")
                                    });
                                }
                            }
                            else
                            {
                                if (tagServicoElement.GetElementsByTagName("RequestURIProducao").Count > 0)
                                {
                                    if (!string.IsNullOrWhiteSpace(tagServicoElement.GetElementsByTagName("RequestURIProducao")[0].InnerText))
                                    {
                                        retornar.Add(new DadosConfigNFSe
                                        {
                                            TipoAmbiente = TipoAmbiente.Producao,
                                            VersaoSchema = tagServicoElement.GetAttribute("versao")
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return retornar;
        }

    }

    /// <summary>
    /// Classe auxiliar para elaboração da lista de configurações da NFSe
    /// </summary>
    public class DadosConfigNFSe
    {
        public TipoAmbiente TipoAmbiente { get; set; }
        public string VersaoSchema { get; set; }
        public bool IsAPI { get; set; }
    }
}
