using System;
using System.Collections.Generic;
using Diag = System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;

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

            Diag.Debug.Assert(File.Exists(arqConfigGeral), "Arquivo de configuração geral (" + arqConfigGeral + ") não foi localizado.");

            try
            {
                //Buscar todos os padrões de NFSe implementados para testar os municípios
                foreach(var nomePadrao in Enum.GetNames(typeof(PadraoNFSe)))
                {
                    var padraoNFSe = (PadraoNFSe)Enum.Parse(typeof(PadraoNFSe), nomePadrao);

                    if(padraoNFSe == PadraoNFSe.None)
                    {
                        continue;
                    }

                    //Pegar o arquivo de configurações para buscar todos os municípios implementados do padrão em questão para testar 1 por 1
                    var xmlConfig = new XmlDocument();
                    xmlConfig.Load(arqConfigGeral);

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
                                    var codMunicipio = Convert.ToInt32(arquivoElement.GetAttribute("ID"));
                                    var nomeMunicipio = arquivoElement.GetElementsByTagName("Nome")[0].InnerText;

                                    var ambientesVersoesSchema = TestUtility.BuscarDadosConfigNFSe(pastaConfigNFSe + "\\" + arquivoElement.GetElementsByTagName("ArqConfig")[0].InnerText, servico);

                                    foreach(var ambienteVersaoSchema in ambientesVersoesSchema)
                                    {
                                        if(!string.IsNullOrWhiteSpace(ambienteVersaoSchema.VersaoSchema))
                                        {
                                            dados.Add(new object[] { ambienteVersaoSchema.TipoAmbiente, padraoNFSe, ambienteVersaoSchema.VersaoSchema, codMunicipio, nomeMunicipio });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            catch(Exception ex)
            {
                Diag.Debug.Assert(false, ex.Message, ex.StackTrace);
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
            Diag.Debug.Assert(File.Exists(arqConfigMunicipio), "Arquivo " + arqConfigMunicipio + " não foi localizado.");

            var retornar = new List<DadosConfigNFSe>();

            var xmlConfig = new XmlDocument();
            xmlConfig.Load(arqConfigMunicipio);

            var configuracoesList = xmlConfig.GetElementsByTagName("Configuracoes");

            foreach(var configuracoesNode in configuracoesList)
            {
                var configuracoesElement = (XmlElement)configuracoesNode;
                var servicosList = configuracoesElement.GetElementsByTagName("Servicos");

                foreach(var servicosNode in servicosList)
                {
                    var servicosElement = (XmlElement)servicosNode;

                    if(servicosElement.GetAttribute("ID").ToLower() == "nfse")
                    {
                        var consultarLoteRpsList = servicosElement.GetElementsByTagName(servico);

                        foreach(var consultarLoteRpsNode in consultarLoteRpsList)
                        {
                            var consultarLoteRpsElement = (XmlElement)consultarLoteRpsNode;

                            if(consultarLoteRpsElement.GetElementsByTagName("WebEnderecoHomologacao") != null)
                            {
                                if(!string.IsNullOrWhiteSpace(consultarLoteRpsElement.GetElementsByTagName("WebEnderecoHomologacao")[0].InnerText))
                                {
                                    retornar.Add(new DadosConfigNFSe
                                    {
                                        TipoAmbiente = TipoAmbiente.Homologacao,
                                        VersaoSchema = consultarLoteRpsElement.GetAttribute("versao")
                                    });
                                }
                            }

                            if(consultarLoteRpsElement.GetElementsByTagName("WebEnderecoProducao") != null)
                            {
                                if(!string.IsNullOrWhiteSpace(consultarLoteRpsElement.GetElementsByTagName("WebEnderecoProducao")[0].InnerText))
                                {
                                    retornar.Add(new DadosConfigNFSe
                                    {
                                        TipoAmbiente = TipoAmbiente.Producao,
                                        VersaoSchema = consultarLoteRpsElement.GetAttribute("versao")
                                    });
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
    }
}
