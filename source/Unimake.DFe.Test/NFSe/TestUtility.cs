using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Net;
using System.Net.NetworkInformation;
using System.Xml;
using Microsoft.VisualStudio.TestPlatform.CommunicationUtilities;
using Unimake.Business.DFe.Servicos;
using Xunit;
using Xunit.Sdk;

namespace Unimake.DFe.Test.NFSe
{
    public static class TestUtility
    {
        #region Private Properties
        public static bool CanReadXML;
        #endregion Private Properties

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

        /// <summary>
        /// Analisa o resultado do testa para verificar se é satisfatório. Existem padrões com retornos negativos devido a sua autenticação de certificados digitais, autorização, etc..
        /// </summary>
        /// <param name="o"> Objeto serviço a ser analisado </param>
        public static void AnalisaResultado(object o)
        {
            var servico = (ServicoBase)o;

            // Precisei passar o executar aqui para dentro, por causa do padrão ADM_SISTEMAS.
            // O padrão necessita de autenticação de login e senha, porém a resposta em xml vem quebrada, gerando erro nos testes como estava antigamente.
            try
            {
                servico.Executar();
            }
            catch
            {
                //Este método irá analisar a comunicação HttpStatusCode,junto com os padrões...
                //O padrão BAUHAUS (exemplo), retorna um código 500, porém com uma comunicação satisfatória.
                //Caso queira verificar o motivo de algum erro ou comunicação não satisfatória (impedido por causa de autenticação de certificado ou login/senha),
                //Comente as 4 linhas abaixo que irá gerar a mensagem
                if (AnalisaComunicacao(servico))
                {
                    return;
                }

                var message = string.Empty;//$"O padrão {servico.Configuracoes.PadraoNFSe}, no ambiente de {servico.Configuracoes.TipoAmbiente}, ";

                if (servico.HttpStatusCode != HttpStatusCode.NotFound)
                {
                    message = $"\nPadrão: {servico.Configuracoes.PadraoNFSe}\n" +
                              $"Ambiente: {servico.Configuracoes.TipoAmbiente}\n" +
                              $"Utiliza autenticação: {servico.Configuracoes.LoginConexao} \n" +
                              $"HttpCode: {(int)servico.HttpStatusCode} \n" +
                              "Este contexto ";

                    var result = VerificaContexto(servico);

                    if (!string.IsNullOrEmpty(result))
                    {
                        message += result;
                    }
                }
                else
                {
                    message = "Provavelmente este município mudou de padrão ou o link está errado! ";
                }

                throw new Exception(message);
            }

        }

        #region Private Methods

        private static bool AnalisaComunicacao(ServicoBase o)
        {
            var servico = o;

            // Nesta parte, entendo que, alguns padrões retornam um HttpStatusCode diferente de 200 (OK), mesmo se a comunicação tenha sido satisfatória
            //Exemplo: BAUHAUS, precisa de um token válido
            if (servico.HttpStatusCode == System.Net.HttpStatusCode.NotFound)
            {
                return false;
            }
            if (servico.HttpStatusCode == System.Net.HttpStatusCode.OK ||
                (servico.HttpStatusCode == HttpStatusCode.InternalServerError && servico.Configuracoes.PadraoNFSe == PadraoNFSe.BAUHAUS) ||
                servico.HttpStatusCode == (HttpStatusCode)0 && servico.Configuracoes.PadraoNFSe == PadraoNFSe.ADM_SISTEMAS ||
                servico.HttpStatusCode == HttpStatusCode.BadRequest && 
                    (servico.Configuracoes.PadraoNFSe == PadraoNFSe.NACIONAL 
                    || servico.Configuracoes.PadraoNFSe == PadraoNFSe.CENTI 
                    || servico.Configuracoes.PadraoNFSe == PadraoNFSe.AGILI) ||
                servico.HttpStatusCode == HttpStatusCode.Unauthorized && servico.Configuracoes.PadraoNFSe == PadraoNFSe.IPM)
            {
                return true;
            }

            return false;
        }

        private static string VerificaContexto(ServicoBase servicoBase)
        {
            var servico = servicoBase;

            var message = string.Empty;

            switch (servico.Configuracoes.PadraoNFSe, servico.Configuracoes.TipoAmbiente)
            {
                case (PadraoNFSe.FIORILLI, TipoAmbiente.Producao):
                case (PadraoNFSe.SYSTEMPRO, TipoAmbiente.Homologacao):
                case (PadraoNFSe.SYSTEMPRO, TipoAmbiente.Producao):
                    message += "necessita de um certificado digital autorizado para consumir o Webservice";
                    break;

                case (PadraoNFSe.BAUHAUS, TipoAmbiente.Homologacao):
                case (PadraoNFSe.BAUHAUS, TipoAmbiente.Producao):
                case (PadraoNFSe.ADM_SISTEMAS, TipoAmbiente.Homologacao):
                case (PadraoNFSe.AVMB, TipoAmbiente.Producao):
                    message += "necessita de uma autenticação válida";
                    break;

                case (PadraoNFSe.TINUS, TipoAmbiente.Producao):
                case (PadraoNFSe.TINUS, TipoAmbiente.Homologacao):
                    if (servico.Configuracoes.Servico == Servico.NFSeCancelarNfse || servico.Configuracoes.Servico == Servico.NFSeConsultarNfse)
                    {
                        message += "necessita de dados reais para retorno correto da prefeitura.";
                    }

                    break;

                case (PadraoNFSe.WEBFISCO, TipoAmbiente.Producao):
                    message += "retornou erro 500 do servidor, podendo ser problema de comunicação ou informação não real no XML.";
                    break;

                default:
                    message += "ainda não possui o erro catalogado!";
                    break;
            }
            return message;
        }
        #endregion
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
