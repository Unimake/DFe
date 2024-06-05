using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe com utilitários para testar os web-services da SEFAZ
    /// </summary>
    public class TestWebservice
    {
        /// <summary>
        /// Tipo de emissão
        /// </summary>
        public TipoEmissao TipoEmissao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        public TipoAmbiente TipoAmbiente { get; set; }

        /// <summary>
        /// UF do web-service que será testado
        /// </summary>
        public UFBrasil UFBrasil { get; set; }

        /// <summary>
        /// Tipo do documento fiscal eletrônico que terá o web-service testado
        /// </summary>
        public TipoDFe TipoDFe { get; set; }

        /// <summary>
        /// Serviço que será testado
        /// </summary>
        public Servico Servico { get; set; }

        /// <summary>
        /// Versão do pacote de schema do serviço que terá o web-service testado
        /// </summary>
        public string SchemaVersao { get; set; }

        /// <summary>
        /// Certificado digital a ser utilizado nos testes do web-service
        /// </summary>
        public X509Certificate2 CertificadoDigital { get; set; }

        /// <summary>
        /// Modelo do documento do web-service que será testado
        /// </summary>
        private ModeloDFe Modelo
        {
            get
            {
                var modelo = ModeloDFe.NFe;

                switch (TipoDFe)
                {
                    case TipoDFe.NFe:
                        modelo = ModeloDFe.NFe;
                        break;
                    case TipoDFe.NFCe:
                        modelo = ModeloDFe.NFCe;
                        break;
                    case TipoDFe.CTe:
                        modelo = ModeloDFe.CTe;
                        break;
                    case TipoDFe.CTeOS:
                        modelo = ModeloDFe.CTeOS;
                        break;
                    case TipoDFe.MDFe:
                        modelo = ModeloDFe.MDFe;
                        break;

                    #region FALTA FAZER

                    case TipoDFe.NFSe:
                        break;
                    case TipoDFe.SAT:
                        break;
                    case TipoDFe.CFe:
                        break;
                    case TipoDFe.GNRE:
                        break;
                    case TipoDFe.SNCM:
                        break;
                    case TipoDFe.CCG:
                        break;
                    case TipoDFe.EFDReinf:
                        break;
                    case TipoDFe.ESocial:
                        break;

                        #endregion
                }

                return modelo;
            }
        }

        /// <summary>
        /// Executar o teste 
        /// </summary>
        public void Execute()
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe,
                TipoEmissao = TipoEmissao,
                CodigoUF = (int)UFBrasil,
                TipoAmbiente = TipoAmbiente,
                Servico = Servico,
                SchemaVersao = SchemaVersao,
                Modelo = Modelo,
                CertificadoDigital = CertificadoDigital
            };

            switch (Servico)
            {
                case Servico.NFeStatusServico:
                    configuracao.Load("StatusServico");
                    break;
                case Servico.NFeConsultaProtocolo:
                    configuracao.Load("ConsultaProtocolo");
                    break;
                case Servico.NFeConsultaRecibo:
                    configuracao.Load("RetAutorizacao");
                    break;
                case Servico.NFeInutilizacao:
                    configuracao.Load("Inutilizacao");
                    break;
                case Servico.NFeConsultaCadastro:
                    configuracao.Load("ConsultaCadastro");
                    break;
                case Servico.NFeRecepcaoEvento:
                    configuracao.Load("RecepcaoEvento");
                    break;
                case Servico.NFeAutorizacao:
                    configuracao.Load("Autorizacao");
                    break;
                case Servico.NFeDistribuicaoDFe:
                    configuracao.Load("DistribuicaoDFe");
                    break;

                #region A FAZER

                case Servico.CTeStatusServico:
                    break;
                case Servico.CTeConsultaProtocolo:
                    break;
                case Servico.CTeDistribuicaoDFe:
                    break;
                case Servico.CTeConsultaRecibo:
                    break;
                case Servico.CTeAutorizacao:
                    break;
                case Servico.CTeAutorizacaoOS:
                    break;
                case Servico.MDFeStatusServico:
                    break;
                case Servico.MDFeConsultaProtocolo:
                    break;
                case Servico.MDFeConsultaRecibo:
                    break;
                case Servico.MDFeConsultaNaoEnc:
                    break;
                case Servico.MDFeAutorizacao:
                    break;
                case Servico.MDFeAutorizacaoSinc:
                    break;
                case Servico.GNREConsultaConfigUF:
                    break;
                case Servico.GNREConsultaResultadoLote:
                    break;
                case Servico.GNRELoteRecepcao:
                    break;
                case Servico.NFSeCancelarNfse:
                    break;
                case Servico.NFSeConsultarNotaPrestador:
                    break;
                case Servico.NFSeConsultarNotaValida:
                    break;
                case Servico.NFSeGerarNfse:
                    break;
                case Servico.NFSeRecepcionarLoteRps:
                    break;
                case Servico.NFSeRecepcionarLoteRpsSincrono:
                    break;
                case Servico.NFSeSubstituirNfse:
                    break;
                case Servico.NFSeConsultarLoteRps:
                    break;
                case Servico.NFSeConsultarNfse:
                    break;
                case Servico.NFSeConsultarNfseServicoPrestado:
                    break;
                case Servico.NFSeConsultarNfseServicoTomado:
                    break;
                case Servico.NFSeConsultarNfseFaixa:
                    break;
                case Servico.NFSeConsultarNfsePorRps:
                    break;
                case Servico.NFSeConsultarNfsePDF:
                    break;
                case Servico.GNREConsultaLoteRecepcao:
                    break;
                case Servico.GNREConsultaResultadoLoteConsulta:
                    break;
                case Servico.NFSeConsultarSituacaoLoteRps:
                    break;
                case Servico.NFSeConsultaNFeRecebidas:
                    break;
                case Servico.NFSeConsultaNFeEmitidas:
                    break;
                case Servico.NFSeTesteEnvioLoteRps:
                    break;
                case Servico.NFSeEnvioLoteRps:
                    break;
                case Servico.NFSeEnvioRps:
                    break;
                case Servico.NFSeCancelamentoNfe:
                    break;
                case Servico.NFSeConsultaInformacoesLote:
                    break;
                case Servico.NFSeConsultaLote:
                    break;
                case Servico.CCGConsGTIN:
                    break;
                case Servico.NFSeCancelaNota:
                    break;
                case Servico.NFSeEmissaoNota:
                    break;
                case Servico.NFSeCancelarNotaFiscal:
                    break;
                case Servico.NFSeConsultaNotaFiscal:
                    break;
                case Servico.NFSeEnviarLoteNotas:
                    break;
                case Servico.NFSeConsultarRpsServicoPrestado:
                    break;
                case Servico.CTeAutorizacaoSinc:
                    break;
                case Servico.NFSeObterCriticaLote:
                    break;
                case Servico.NFSeConsultarUrlNfse:
                    break;
                case Servico.NFSeConsultarDadosCadastrais:
                    break;
                case Servico.NFSeConsultarRpsDisponivel:
                    break;
                case Servico.NFSeConsultarSequenciaLoteNotaRPS:
                    break;
                case Servico.NFSeObterNotaFiscalXml:
                    break;
                case Servico.NFSeSolicitacaoInutilizacao:
                    break;
                case Servico.NFSeConsultarRequerimentoCancelamento:
                    break;
                case Servico.EFDReinfConsultaReciboEvento:
                    break;
                case Servico.EFDReinfConsultaLoteAssincrono:
                    break;
                case Servico.EFDReinfRecepcionarLoteAssincrono:
                    break;
                case Servico.ESocialInformacoesDoEmpregador:
                    break;

                    #endregion
            }

            var url = "";
            if (configuracao.TipoAmbiente == TipoAmbiente.Homologacao)
            {
                url = configuracao.WebEnderecoHomologacao;
            }
            else
            {
                url = configuracao.WebEnderecoProducao;
            }

            TestURL(url);
        }

        /// <summary>
        /// Testar a URL
        /// </summary>
        /// <param name="url">URL a ser testada</param>
        private void TestURL(string url)
        {
            var domain = Net.ExtractDomain(url);

            //PING pelo Domínio
            var sucess = Net.PingHost(domain, 3);
            if (!sucess)
            {
                //TODO: Deu erro, ver o que vamos retornar
            }

            //PING pelo IP do Domínio
            var ipAddress = Net.GetIpAddressDomain(domain);
            sucess = Net.PingHost(ipAddress, 3);
            if (!sucess)
            {
                //TODO: Deu erro, ver o que vamos retornar
            }

            //Testar conexão http ou https
            sucess = Net.TestHttpConnection(url, CertificadoDigital);

            if (!sucess)
            {
                //TODO: Deu erro, ver o que vamos retornar
            }
        }
    }
}
