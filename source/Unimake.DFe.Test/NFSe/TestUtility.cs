using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;

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
        /// Analisa o resultado do teste para verificar se é satisfatório.
        /// Existem padrões com retornos negativos devido a sua autenticação de certificados digitais, autorização, entre outros.
        /// </summary>
        /// <param name="item">Objeto que contém o serviço que será testado</param>
        public static void AnalisaResultado(object item)
        {
            var servico = (ServicoBase)item;

            // Foi necessário passar o executar para esse método, por conta do padrão ADM_SISTEMAS.
            // Neste padrão, a resposta em xml vem quebrada, gerando erro nos testes como estava antigamente.
            try
            {
                servico.Executar();
            }
            catch (Exception ex)
            {
                // Analisa a comunicação com o município e separa cenários para diferentes statusCode e padrões
                if (AnalisaComunicacao(servico, ex.Message))
                {
                    return;
                }

                var message = string.Empty;

                if (servico.HttpStatusCode != HttpStatusCode.NotFound &&
                    !(ex.Message.Contains("404") || ex.Message.Contains("Not Found")))
                {
                    message = $"\nMunicípio: {servico.Configuracoes.Nome}\n" +
                              $"Código: {servico.Configuracoes.CodigoMunicipio}\n" +
                              $"Ambiente: {servico.Configuracoes.TipoAmbiente}\n" +
                              $"Utiliza autenticação: {servico.Configuracoes.LoginConexao} \n" +
                              $"HttpCode: {(int)servico.HttpStatusCode} \n";

                    var result = VerificaContexto(servico);

                    if (!string.IsNullOrEmpty(result))
                    {
                        message += result;
                    }
                }
                else
                {
                    message = $"Provavelmente o município {servico.Configuracoes.Nome} mudou de padrão ou o link está errado! ";
                }

                message += $"\nErro original: {ex.Message}";

                if (AnalisaExcecaoEsperadaNFSe(servico, message))
                {
                    return;
                }

                throw new Exception(message);
            }
        }

        #region Private Methods

        /// <summary>
        /// Analisa a comunicação da requisição com base no StatusCode, o padrão testado e ambiente testado
        /// </summary>
        /// <param name="servico">Objeto com as configurações do serviço</param>
        /// <param name="mensagemExcecao">Mensagem da exceção, se gerada, do ConsumirBase ou ConsumirAPI</param>
        /// <returns></returns>
        private static bool AnalisaComunicacao(ServicoBase servico, string mensagemExcecao)
        {
            var padraoNFSe = servico.Configuracoes.PadraoNFSe;
            var statusCode = servico.HttpStatusCode;
            var tipoAmbiente = servico.Configuracoes.TipoAmbiente;
            var comunicacaoFuncionou = false;

            switch (statusCode)
            {
                case HttpStatusCode.NotFound:
                case HttpStatusCode.InternalServerError:
                    return comunicacaoFuncionou;

                case HttpStatusCode.OK:
                    comunicacaoFuncionou = true;
                    break;

                case HttpStatusCode.BadRequest:
                    if (padraoNFSe == PadraoNFSe.NACIONAL ||
                        padraoNFSe == PadraoNFSe.DSF ||
                        padraoNFSe == PadraoNFSe.AGILI)
                    {
                        comunicacaoFuncionou = true;
                    }

                    break;

                case HttpStatusCode.Unauthorized:
                    if (padraoNFSe == PadraoNFSe.IPM)
                    {
                        comunicacaoFuncionou = true;
                    }

                    break;

                default:

                    if (padraoNFSe == PadraoNFSe.SONNER && tipoAmbiente == TipoAmbiente.Producao && mensagemExcecao.Contains("vazio"))
                    {
                        comunicacaoFuncionou = true;
                    }

                    break;
            }

            return comunicacaoFuncionou;
        }

        private static bool AnalisaExcecaoEsperadaNFSe(ServicoBase servico, string mensagem)
        {
            switch (servico.Configuracoes.PadraoNFSe)
            {
                // Alguns municípios do padrão TINUS exigem dados reais para concluir o envio.
                // Quando recebem dados fictícios, podem retornar erro 500.
                // Esta exceção evita falha indevida no teste unitário.
                // Recomenda-se remover esta adaptação periodicamente para validar novamente a comunicação.
                case PadraoNFSe.TINUS:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao, TipoAmbiente.Homologacao) &&
                           ServicoEsperado(servico,
                               Servico.NFSeCancelarNfse,
                               Servico.NFSeConsultarNfse,
                               Servico.NFSeConsultarNfsePorRps,
                               Servico.NFSeConsultarSituacaoLoteRps,
                               Servico.NFSeRecepcionarLoteRps) &&
                            (mensagem.Contains("internal server error") ||
                            mensagem.Contains("Server Error"));

                // Alguns municípios do padrão QUASAR retornam erro em ambiente de homologação.
                // Nesses casos, o retorno vem como texto/log de erro, serviço temporariamente indisponível.
                case PadraoNFSe.QUASAR:
                    return AmbienteEsperado(servico, TipoAmbiente.Homologacao) &&
                           ServicoEsperado(servico,
                                Servico.NFSeCancelarNfse,
                                Servico.NFSeConsultarNfsePorRps,
                                Servico.NFSeConsultarSituacaoLoteRps,
                                Servico.NFSeGerarNfse) &&
                           mensagem.Contains("503 Service Temporarily Unavailable");

                // O padrão único WEBFISCO retorna erro nos serviços de consulta e cancelamento.
                // Nesses casos, o retorno vem como texto/log de erro, erro 500 ou erro 404.
                case PadraoNFSe.WEBFISCO:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao) &&
                           ServicoEsperado(servico,
                                 Servico.NFSeCancelarNfse,
                                 Servico.NFSeConsultarNfse) &&
                           (mensagem.Contains("erro 500 do servidor") ||
                            mensagem.Contains("(404) Not Found"));

                // Alguns municípios do padrão SMARAPD retornam erro interno no serviço de cancelamento.
                // Nesses casos, o retorno vem como texto/log de erro, fora do XML esperado.
                case PadraoNFSe.SMARAPD:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao, TipoAmbiente.Homologacao) &&
                           ServicoEsperado(servico,
                                Servico.NFSeCancelarNfse,
                                Servico.NFSeConsultarNfseFaixa) &&
                           mensagem.Contains("Erro original: Data at the root level is invalid. Line 1, position 1.");

                // Alguns municípios do padrão GIF podem retornar erro quando a chave de acesso fictícia do teste não é encontrada.
                case PadraoNFSe.GIF:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao, TipoAmbiente.Homologacao) &&
                           ServicoEsperado(servico, Servico.NFSeConsultarNfsePDF) &&
                           (mensagem.Contains("Chave de acesso") ||
                           mensagem.Contains("encontrada"));

                // Alguns municípios do padrão FIORILLI podem retornar erro quando o certificado digital não é autorizado.
                case PadraoNFSe.FIORILLI:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao) &&
                           (mensagem.Contains("máquina de destino as recusou ativamente") ||
                            mensagem.Contains("host conectado não respondeu.") ||
                            mensagem.Contains("Nenhuma conexão pôde ser feita"));

                // Alguns municípios do padrão QUASAR retornam erro em ambiente de homologação.
                // Nesses casos, o retorno vem como texto/log de erro, ...Web server received an invalid response...
                case PadraoNFSe.PORTAL_FACIL:
                    return AmbienteEsperado(servico, TipoAmbiente.Homologacao) &&
                           ServicoEsperado(servico, 
                           Servico.NFSeConsultarLoteRps,
                           Servico.NFSeConsultarNfsePorRps,
                           Servico.NFSeGerarNfse) &&
                           mensagem.Contains("Web server received an invalid response");

                // Alguns municípios do padrão MODERNIZACAO_PUBLICA retornam erro no serviço de consulta.
                // Nesses casos, o retorno vem como texto/log de erro, ...java.lang.NullPointerException...
                case PadraoNFSe.MODERNIZACAO_PUBLICA:
                    return AmbienteEsperado(servico, TipoAmbiente.Producao) &&
                           ServicoEsperado(servico, Servico.NFSeConsultarNfseServicoTomado) &&
                           mensagem.Contains("<faultstring>java.lang.NullPointerException</faultstring>");
                default:
                    return false;
            }
        }

        private static bool AmbienteEsperado(ServicoBase servico, params TipoAmbiente[] ambientes)
        {
            foreach (var ambiente in ambientes)
            {
                if (servico.Configuracoes.TipoAmbiente == ambiente)
                {
                    return true;
                }
            }

            return false;
        }

        private static bool ServicoEsperado(ServicoBase servico, params Servico[] servicos)
        {
            foreach (var servicoEsperado in servicos)
            {
                if (servico.Configuracoes.Servico == servicoEsperado)
                {
                    return true;
                }
            }

            return false;
        }

        private static string VerificaContexto(ServicoBase servicoBase)
        {
            var message = string.Empty;

            switch (servicoBase.Configuracoes.PadraoNFSe, servicoBase.Configuracoes.TipoAmbiente)
            {
                case (PadraoNFSe.FIORILLI, TipoAmbiente.Producao):
                    message += "Este contexto necessita de um certificado digital autorizado para consumir o Webservice\n";
                    break;

                case (PadraoNFSe.ADM_SISTEMAS, TipoAmbiente.Homologacao):
                case (PadraoNFSe.AVMB, TipoAmbiente.Producao):
                    message += "Este contexto necessita de uma autenticação válida\n";
                    break;

                case (PadraoNFSe.TINUS, TipoAmbiente.Producao):
                case (PadraoNFSe.TINUS, TipoAmbiente.Homologacao):
                    if (servicoBase.Configuracoes.Servico == Servico.NFSeCancelarNfse || servicoBase.Configuracoes.Servico == Servico.NFSeConsultarNfse)
                    {
                        message += "Este contexto necessita de dados reais para retorno correto da prefeitura.\n";
                    }

                    break;

                case (PadraoNFSe.WEBFISCO, TipoAmbiente.Producao):
                    message += "Este contexto retornou erro 500 do servidor, podendo ser problema de comunicação ou informação não real no XML.\n";
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
