#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Authentication;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Consolida telemetria passiva e testes locais sem criar mensagens fiscais sintéticas.
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public class DiagnosticoDisponibilidadeDFe
    {
        /// <summary>Conjunto dos quatro documentos atendidos pela primeira versão do motor.</summary>
        private static readonly HashSet<TipoDFe> DocumentosSuportados = new HashSet<TipoDFe>
        {
            TipoDFe.NFe,
            TipoDFe.NFCe,
            TipoDFe.CTe,
            TipoDFe.MDFe
        };
        /// <summary>Configuração do contribuinte, documento, UF, ambiente e certificado.</summary>
        private readonly Configuracao configuracao;
        /// <summary>Opções que controlam timeout, cache, janela histórica e serviços selecionados.</summary>
        private readonly ConfiguracaoDiagnosticoDisponibilidade opcoes;
        /// <summary>Executor de DNS, TCP, TLS e proxy usado pelo diagnóstico local.</summary>
        private readonly IExecutorInfraestruturaDisponibilidade executorInfraestrutura;
        /// <summary>Função que consulta o StatusServico; substituível internamente nos testes.</summary>
        private readonly Func<Configuracao, string, ResultadoSondaDisponibilidade> executorStatus;

        /// <summary>Cria o motor de diagnóstico.</summary>
        /// <param name="configuracao">Configuração que identifica DFe, UF, ambiente e certificado.</param>
        /// <param name="opcoes">Opções; quando nulas, são usados os padrões seguros.</param>
        public DiagnosticoDisponibilidadeDFe(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes = null)
            : this(configuracao, opcoes, new ExecutorInfraestruturaDisponibilidade(), null) { }

        /// <summary>Cria o motor usando um executor local substituível para testes sem rede.</summary>
        /// <param name="configuracao">Configuração fiscal usada no diagnóstico.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <param name="executorInfraestrutura">Executor local que será usado.</param>
        internal DiagnosticoDisponibilidadeDFe(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes,
            IExecutorInfraestruturaDisponibilidade executorInfraestrutura)
            : this(configuracao, opcoes, executorInfraestrutura, null) { }

        /// <summary>Cria o motor com executores locais e de status substituíveis nos testes.</summary>
        /// <param name="configuracao">Configuração fiscal usada no diagnóstico.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <param name="executorInfraestrutura">Executor local que será usado.</param>
        /// <param name="executorStatus">Função que simula ou executa a consulta de status.</param>
        internal DiagnosticoDisponibilidadeDFe(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes,
            IExecutorInfraestruturaDisponibilidade executorInfraestrutura,
            Func<Configuracao, string, ResultadoSondaDisponibilidade> executorStatus)
        {
            this.configuracao = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            this.opcoes = opcoes ?? new ConfiguracaoDiagnosticoDisponibilidade();
            this.executorInfraestrutura = executorInfraestrutura ?? throw new ArgumentNullException(nameof(executorInfraestrutura));
            this.executorStatus = executorStatus ?? ExecutarStatusServico;
        }

        /// <summary>
        /// Obtém as evidências passivas e executa DNS, TCP e TLS com cache, sem enviar mensagem fiscal.
        /// </summary>
        /// <returns>Resultado consolidado.</returns>
        public ResultadoDiagnosticoDisponibilidade Executar()
        {
            opcoes.Validar();
            var resultado = CriarResultado();
            var cronometro = Stopwatch.StartNew();
            if (!DocumentoSuportado())
            {
                resultado.Sondas.Add(CriarNaoAplicavel("Diagnostico",
                    "O documento ainda não possui uma estratégia de diagnóstico registrada."));
                Finalizar(resultado, cronometro);
                return resultado;
            }
            AdicionarTelemetria(resultado);

            Configuracao configuracaoStatus;
            string endpoint;
            try
            {
                configuracaoStatus = CriarConfiguracaoStatus();
                endpoint = ObterEndpoint(configuracaoStatus);
                if (string.IsNullOrWhiteSpace(endpoint))
                {
                    resultado.Sondas.Add(CriarNaoAplicavel("Infraestrutura", "Não existe endpoint de status configurado."));
                }
                else
                {
                    var falhaCertificado = ValidarCertificado(configuracaoStatus.CertificadoDigital);
                    if (falhaCertificado != null) resultado.Sondas.Add(falhaCertificado);
                    foreach (var item in CacheInfraestruturaDisponibilidade.ObterOuExecutar(configuracaoStatus, endpoint, opcoes,
                        executorInfraestrutura))
                    {
                        resultado.Sondas.Add(item);
                    }
                }
            }
            catch (CertificadoDiagnosticoException ex)
            {
                resultado.Sondas.Add(CriarFalhaCertificado(
                    ClassificadorDisponibilidade.SanitizarExcecao(ex.InnerException ?? ex)));
            }
            catch (Exception ex)
            {
                resultado.Sondas.Add(CriarFalhaConfiguracao(ex));
            }

            Finalizar(resultado, cronometro);
            return resultado;
        }

        /// <summary>Obtém somente a telemetria passiva em memória, sem executar qualquer acesso de rede.</summary>
        /// <returns>Resultado consolidado.</returns>
        public ResultadoDiagnosticoDisponibilidade ObterDiagnosticoPassivo()
        {
            opcoes.Validar();
            var resultado = CriarResultado();
            var cronometro = Stopwatch.StartNew();
            if (!DocumentoSuportado())
            {
                resultado.Sondas.Add(CriarNaoAplicavel("Diagnostico",
                    "O documento ainda não possui uma estratégia de diagnóstico registrada."));
                Finalizar(resultado, cronometro);
                return resultado;
            }
            AdicionarTelemetria(resultado);
            Finalizar(resultado, cronometro);
            return resultado;
        }

        /// <summary>
        /// Consulta explicitamente o serviço oficial de status, respeitando cache mínimo de cinco minutos.
        /// Não são enviadas mensagens sintéticas para outros serviços.
        /// </summary>
        /// <returns>Resultado combinado com telemetria, infraestrutura e status.</returns>
        public ResultadoDiagnosticoDisponibilidade ConsultarStatusServico()
        {
            var resultado = Executar();
            var cronometro = Stopwatch.StartNew();
            try
            {
                if (resultado.Status == StatusDisponibilidade.NaoAplicavel)
                {
                    cronometro.Stop();
                    resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
                    return resultado;
                }

                if (PossuiFalhaEstruturalEssencial(resultado))
                {
                    cronometro.Stop();
                    resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
                    resultado.Status = StatusDisponibilidade.Inconclusivo;
                    resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.AmbienteLocal;
                    return resultado;
                }

                DateTime bloqueadoAte;
                if (TentarBloquearPelasEvidencias(resultado, out bloqueadoAte) ||
                    CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte))
                {
                    AdicionarStatusSuprimido(resultado, bloqueadoAte);
                    cronometro.Stop();
                    resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
                    AgregadorDisponibilidade.Agregar(resultado);
                    return resultado;
                }

                var configuracaoStatus = CriarConfiguracaoStatus();
                var endpoint = ObterEndpoint(configuracaoStatus);
                if (string.IsNullOrWhiteSpace(endpoint))
                {
                    resultado.Sondas.Add(CriarNaoAplicavel("StatusServico", "Não existe endpoint de status configurado."));
                }
                else
                {
                    var chave = CriarChaveStatus(configuracaoStatus, endpoint);
                    var contextoLocal = CacheInfraestruturaDisponibilidade.CriarIdentidadeContextoLocal(
                        configuracaoStatus, opcoes.TimeoutMilissegundos);
                    var status = CacheStatusDisponibilidade.ObterOuExecutar(chave, contextoLocal,
                        TimeSpan.FromMinutes(opcoes.IntervaloMinimoStatusMinutos),
                        () => executorStatus(configuracaoStatus, endpoint));
                    if (status.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
                    {
                        CacheStatusDisponibilidade.BloquearContexto(configuracaoStatus, status.DataHora.AddHours(1));
                    }
                    resultado.Sondas.Add(status);
                }
            }
            catch (CertificadoDiagnosticoException ex)
            {
                resultado.Sondas.Add(CriarFalhaCertificado(
                    ClassificadorDisponibilidade.SanitizarExcecao(ex.InnerException ?? ex)));
            }
            catch (Exception ex)
            {
                resultado.Sondas.Add(CriarFalhaConfiguracao(ex));
            }

            cronometro.Stop();
            resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
            AgregadorDisponibilidade.Agregar(resultado);
            return resultado;
        }

        /// <summary>Verifica se uma falha essencial de certificado ou configuração impede novas sondas.</summary>
        /// <param name="resultado">Resultado parcial que será examinado.</param>
        /// <returns><see langword="true"/> quando uma falha estrutural essencial existir.</returns>
        private static bool PossuiFalhaEstruturalEssencial(ResultadoDiagnosticoDisponibilidade resultado) =>
            resultado.Sondas.Itens.Any(x => x.Essencial &&
                (x.TipoFalha == TipoFalhaDisponibilidade.Certificado ||
                 x.TipoFalha == TipoFalhaDisponibilidade.Configuracao));

        /// <summary>Procura consumo indevido passivo e registra o bloqueio preventivo do contexto.</summary>
        /// <param name="resultado">Resultado com as evidências passivas atuais.</param>
        /// <param name="bloqueadoAte">Momento até o qual a consulta deve ser evitada.</param>
        /// <returns><see langword="true"/> quando uma evidência ainda estiver dentro da janela de bloqueio.</returns>
        private bool TentarBloquearPelasEvidencias(ResultadoDiagnosticoDisponibilidade resultado, out DateTime bloqueadoAte)
        {
            var evidencia = resultado.Sondas.Itens
                .Where(x => x.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
                .OrderByDescending(x => x.DataHora)
                .FirstOrDefault();
            if (evidencia == null)
            {
                bloqueadoAte = default(DateTime);
                return false;
            }

            bloqueadoAte = evidencia.DataHora.AddHours(1);
            if (RelogioDisponibilidade.Agora() >= bloqueadoAte)
            {
                return false;
            }

            CacheStatusDisponibilidade.BloquearContexto(configuracao, bloqueadoAte);
            return true;
        }

        /// <summary>Adiciona ao resultado uma sonda informando que a consulta foi suprimida por segurança.</summary>
        /// <param name="resultado">Resultado que receberá a sonda.</param>
        /// <param name="bloqueadoAte">Data e hora final do bloqueio.</param>
        private static void AdicionarStatusSuprimido(ResultadoDiagnosticoDisponibilidade resultado, DateTime bloqueadoAte)
        {
            resultado.Sondas.Add(new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                DataHora = RelogioDisponibilidade.Agora(),
                Status = StatusDisponibilidade.Degradado,
                TipoFalha = TipoFalhaDisponibilidade.ConsumoIndevido,
                XMotivo = "A consulta de status foi suprimida até " + bloqueadoAte.ToString("s", CultureInfo.InvariantCulture) + " por consumo indevido observado.",
                DoCache = true,
                Essencial = true
            });
        }

        /// <summary>Limpa telemetria e caches mantidos somente em memória no processo atual.</summary>
        public static void LimparMemoriaDiagnostico()
        {
            TelemetriaDisponibilidade.Limpar();
            CacheInfraestruturaDisponibilidade.Limpar();
            CacheStatusDisponibilidade.Limpar();
        }

        /// <summary>Cria o cabeçalho do resultado com documento, UF, ambiente e horário atual.</summary>
        /// <returns>Resultado pronto para receber as sondas.</returns>
        private ResultadoDiagnosticoDisponibilidade CriarResultado() => new ResultadoDiagnosticoDisponibilidade
        {
            TipoDFe = configuracao.TipoDFe,
            UFBrasil = (UFBrasil)configuracao.CodigoUF,
            TipoAmbiente = configuracao.TipoAmbiente,
            Inicio = RelogioDisponibilidade.Agora()
        };

        /// <summary>Transfere para o resultado as evidências passivas recentes mantidas em memória.</summary>
        /// <param name="resultado">Resultado que receberá as evidências.</param>
        private void AdicionarTelemetria(ResultadoDiagnosticoDisponibilidade resultado)
        {
            foreach (var item in TelemetriaDisponibilidade.Obter(configuracao, opcoes))
            {
                resultado.Sondas.Add(item);
            }
        }

        /// <summary>Cria uma cópia mínima da configuração necessária para consultar StatusServico.</summary>
        /// <returns>Configuração carregada com endpoint, versão, proxy e certificado.</returns>
        private Configuracao CriarConfiguracaoStatus()
        {
            X509Certificate2 certificado;
            try
            {
                certificado = configuracao.CertificadoDigital;
            }
            catch (Exception ex)
            {
                throw new CertificadoDiagnosticoException(ex);
            }

            var copia = new Configuracao
            {
                TipoDFe = configuracao.TipoDFe,
                TipoEmissao = configuracao.TipoEmissao,
                CodigoUF = configuracao.CodigoUF,
                CodigoMunicipio = configuracao.CodigoMunicipio,
                TipoAmbiente = configuracao.TipoAmbiente,
                SchemaVersao = ObterVersaoStatus(),
                CertificadoDigital = certificado,
                HasProxy = configuracao.HasProxy,
                ProxyAutoDetect = configuracao.ProxyAutoDetect,
                ProxyUser = configuracao.ProxyUser,
                ProxyPassword = configuracao.ProxyPassword,
                TimeOutWebServiceConnect = opcoes.TimeoutMilissegundos,
                ColetarTelemetriaDisponibilidade = false
            };
            copia.Load("StatusServico");
            return copia;
        }

        /// <summary>Informa se existe estratégia de diagnóstico para o documento configurado.</summary>
        /// <returns><see langword="true"/> para NFe, NFCe, CTe e MDFe.</returns>
        private bool DocumentoSuportado() => DocumentosSuportados.Contains(configuracao.TipoDFe);

        /// <summary>Escolhe a versão de schema usada pela consulta de status do documento.</summary>
        /// <returns>Versão fiscal compatível com a configuração atual.</returns>
        private string ObterVersaoStatus()
        {
            if (configuracao.TipoDFe == TipoDFe.MDFe) return "3.00";
            if (configuracao.TipoDFe == TipoDFe.CTe) return string.IsNullOrWhiteSpace(configuracao.SchemaVersao) ? "4.00" : configuracao.SchemaVersao;
            return "4.00";
        }

        /// <summary>Executa a chamada real ao StatusServico e transforma o retorno em uma sonda.</summary>
        /// <param name="configuracaoStatus">Configuração específica da consulta.</param>
        /// <param name="endpoint">Endereço sanitizado/configurado do serviço.</param>
        /// <returns>Sonda com duração, HTTP, cStat, motivo e classificação da falha.</returns>
        private ResultadoSondaDisponibilidade ExecutarStatusServico(Configuracao configuracaoStatus, string endpoint)
        {
            var resultado = new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint),
                Protocolo = "SOAP",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                DataHora = RelogioDisponibilidade.Agora(),
                Essencial = true
            };
            var cronometro = Stopwatch.StartNew();
            try
            {
                using (var consumidor = new ConsumirWS())
                {
                    var soap = new ConfiguracaoWSSoapMapper().Map(configuracaoStatus);
                    consumidor.ExecutarServico(CriarXmlStatus(configuracaoStatus), soap, configuracaoStatus.CertificadoDigital);
                    resultado.HttpStatusCode = (int)consumidor.HttpStatusCode;
                    PreencherCStat(consumidor.RetornoServicoXML, resultado);
                }

                ClassificadorDisponibilidade.ClassificarRespostaFiscal(resultado);
                if (resultado.CStat != 107 && resultado.CStat != 108 && resultado.CStat != 109 &&
                    resultado.CStat != 656 && resultado.CStat != 678)
                {
                    resultado.Status = StatusDisponibilidade.Degradado;
                    resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                    resultado.XMotivo = "O serviço de status respondeu com código fiscal inesperado.";
                }
            }
            catch (Exception ex)
            {
                ClassificadorDisponibilidade.PreencherFalha(ex, resultado);
            }
            finally
            {
                cronometro.Stop();
                resultado.DuracaoMilissegundos = cronometro.ElapsedMilliseconds;
            }

            if (resultado.Status == StatusDisponibilidade.Operacional && resultado.DuracaoMilissegundos >= opcoes.LimiteLentidaoMilissegundos)
            {
                resultado.Status = StatusDisponibilidade.Degradado;
                resultado.XMotivo = "O serviço de status respondeu acima do limite de lentidão.";
            }
            return resultado;
        }

        /// <summary>Monta o XML mínimo e somente de consulta para o StatusServico.</summary>
        /// <param name="configuracaoStatus">Dados de documento, UF, ambiente e versão.</param>
        /// <returns>XML de consulta fiscal sem mensagem sintética de emissão.</returns>
        private System.Xml.XmlDocument CriarXmlStatus(Configuracao configuracaoStatus)
        {
            switch (configuracaoStatus.TipoDFe)
            {
                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    return new Xml.NFe.ConsStatServ
                    {
                        Versao = "4.00",
                        TpAmb = configuracaoStatus.TipoAmbiente,
                        CUF = (UFBrasil)configuracaoStatus.CodigoUF
                    }.GerarXML();
                case TipoDFe.CTe:
                    return new Xml.CTe.ConsStatServCte
                    {
                        Versao = configuracaoStatus.SchemaVersao,
                        TpAmb = configuracaoStatus.TipoAmbiente,
                        CUF = (UFBrasil)configuracaoStatus.CodigoUF
                    }.GerarXML();
                case TipoDFe.MDFe:
                    return new Xml.MDFe.ConsStatServMDFe
                    {
                        Versao = "3.00",
                        TpAmb = configuracaoStatus.TipoAmbiente
                    }.GerarXML();
                default:
                    throw new NotSupportedException("O documento não possui consulta de status registrada no diagnóstico.");
            }
        }

        /// <summary>Lê o código fiscal cStat do retorno sem armazenar o XML completo.</summary>
        /// <param name="retorno">XML retornado pelo serviço.</param>
        /// <param name="resultado">Sonda que receberá o código localizado.</param>
        private static void PreencherCStat(System.Xml.XmlDocument retorno, ResultadoSondaDisponibilidade resultado)
        {
            var no = retorno?.SelectSingleNode("//*[local-name()='cStat']");
            int codigo;
            if (no != null && int.TryParse(no.InnerText, NumberStyles.Integer, CultureInfo.InvariantCulture, out codigo))
            {
                resultado.CStat = codigo;
            }
        }

        /// <summary>Obtém o endpoint de produção ou homologação conforme o tipo de transporte.</summary>
        /// <param name="item">Configuração cujo endereço será escolhido.</param>
        /// <returns>URL configurada para o ambiente atual.</returns>
        private static string ObterEndpoint(Configuracao item) => item.IsAPI
            ? (item.TipoAmbiente == TipoAmbiente.Producao ? item.RequestURIProducao : item.RequestURIHomologacao)
            : (item.TipoAmbiente == TipoAmbiente.Producao ? item.WebEnderecoProducao : item.WebEnderecoHomologacao);

        /// <summary>Verifica presença, chave privada e validade temporal do certificado.</summary>
        /// <param name="certificado">Certificado que será validado.</param>
        /// <returns>Falha de certificado quando inválido; caso contrário, <see langword="null"/>.</returns>
        private static ResultadoSondaDisponibilidade ValidarCertificado(X509Certificate2 certificado)
        {
            if (certificado == null) return CriarFalhaCertificado("O certificado digital não foi informado.");
            try
            {
                if (!certificado.HasPrivateKey) return CriarFalhaCertificado("O certificado digital não possui chave privada.");
                var agora = RelogioDisponibilidade.Agora();
                if (agora < certificado.NotBefore || agora > certificado.NotAfter)
                    return CriarFalhaCertificado("O certificado digital está fora do período de validade.");
            }
            catch (Exception ex)
            {
                return CriarFalhaCertificado(ClassificadorDisponibilidade.SanitizarExcecao(ex));
            }
            return null;
        }

        /// <summary>Cria uma sonda padronizada para um problema local de certificado.</summary>
        /// <param name="mensagem">Mensagem segura que explica o problema.</param>
        /// <returns>Sonda inconclusiva de origem local.</returns>
        private static ResultadoSondaDisponibilidade CriarFalhaCertificado(string mensagem) => new ResultadoSondaDisponibilidade
        {
            Servico = "CertificadoDigital",
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.Inconclusivo,
            TipoFalha = TipoFalhaDisponibilidade.Certificado,
            Excecao = mensagem,
            Essencial = true
        };

        /// <summary>Converte uma exceção de carregamento/configuração em uma sonda segura.</summary>
        /// <param name="ex">Exceção original que será sanitizada.</param>
        /// <returns>Sonda inconclusiva de configuração local.</returns>
        private static ResultadoSondaDisponibilidade CriarFalhaConfiguracao(Exception ex) => new ResultadoSondaDisponibilidade
        {
            Servico = "Configuracao",
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.Inconclusivo,
            TipoFalha = TipoFalhaDisponibilidade.Configuracao,
            Excecao = ClassificadorDisponibilidade.SanitizarExcecao(ex),
            Essencial = true
        };

        /// <summary>Cria uma sonda para uma etapa que não existe ou não se aplica ao contexto.</summary>
        /// <param name="servico">Nome da etapa que não pôde ser aplicada.</param>
        /// <param name="motivo">Explicação para o desenvolvedor.</param>
        /// <returns>Sonda com estado <see cref="StatusDisponibilidade.NaoAplicavel"/>.</returns>
        private static ResultadoSondaDisponibilidade CriarNaoAplicavel(string servico, string motivo) => new ResultadoSondaDisponibilidade
        {
            Servico = servico,
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.NaoAplicavel,
            TipoFalha = TipoFalhaDisponibilidade.Nenhuma,
            XMotivo = motivo
        };

        /// <summary>Cria a chave do cache compartilhado de status por documento, UF, ambiente e endpoint.</summary>
        /// <param name="item">Configuração que identifica o contexto fiscal.</param>
        /// <param name="endpoint">Endpoint que será consultado.</param>
        /// <returns>Chave sem dados sigilosos usada pelo cache do processo.</returns>
        private static string CriarChaveStatus(Configuracao item, string endpoint) =>
            ((int)item.TipoDFe).ToString(CultureInfo.InvariantCulture) + "|" + item.CodigoUF.ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)item.TipoAmbiente).ToString(CultureInfo.InvariantCulture) + "|" + ClassificadorDisponibilidade.SanitizarEndpoint(endpoint);

        /// <summary>Fecha a medição do diagnóstico e calcula o estado agregado.</summary>
        /// <param name="resultado">Resultado que será concluído.</param>
        /// <param name="cronometro">Medição iniciada no começo da operação.</param>
        private static void Finalizar(ResultadoDiagnosticoDisponibilidade resultado, Stopwatch cronometro)
        {
            cronometro.Stop();
            resultado.DuracaoTotalMilissegundos = cronometro.ElapsedMilliseconds;
            AgregadorDisponibilidade.Agregar(resultado);
        }

        /// <summary>Marca internamente uma exceção ocorrida ao obter o certificado.</summary>
        private sealed class CertificadoDiagnosticoException : Exception
        {
            /// <summary>Cria a exceção mantendo a causa original para sanitização.</summary>
            /// <param name="innerException">Erro original do carregamento do certificado.</param>
            public CertificadoDiagnosticoException(Exception innerException)
                : base(innerException == null ? string.Empty : innerException.Message, innerException) { }
        }
    }

    /// <summary>
    /// Contrato interno para executar as sondas de infraestrutura.
    /// </summary>
    /// <remarks>
    /// A abstração permite substituir DNS, TCP, TLS e proxy por um executor
    /// determinístico nos testes, sem depender da internet.
    /// </remarks>
    internal interface IExecutorInfraestruturaDisponibilidade
    {
        /// <summary>Executa as sondas locais para um endpoint.</summary>
        /// <param name="configuracao">Configuração de certificado e proxy.</param>
        /// <param name="endpoint">Endpoint que será testado.</param>
        /// <param name="timeoutMilissegundos">Tempo máximo de cada sonda.</param>
        /// <returns>Lista de evidências locais produzidas.</returns>
        IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos);
    }

    /// <summary>Executa testes reais de DNS, TCP, TLS e proxy sem enviar XML fiscal.</summary>
    internal sealed class ExecutorInfraestruturaDisponibilidade : IExecutorInfraestruturaDisponibilidade
    {
        /// <summary>Função de conexão HTTP detalhada, substituível nos testes.</summary>
        private readonly Func<string, X509Certificate2, int, IWebProxy, string, Unimake.Net.HttpConnectionResult> testarConexaoHttp;

        /// <summary>Cria o executor usando a implementação real da Unimake.Utils.</summary>
        internal ExecutorInfraestruturaDisponibilidade()
            : this(Unimake.Net.Utility.TestHttpConnectionDetailed) { }

        /// <summary>Cria o executor usando uma função de conexão fornecida pelo teste.</summary>
        /// <param name="testarConexaoHttp">Função que simula ou executa a conexão HTTP.</param>
        internal ExecutorInfraestruturaDisponibilidade(
            Func<string, X509Certificate2, int, IWebProxy, string, Unimake.Net.HttpConnectionResult> testarConexaoHttp)
        {
            this.testarConexaoHttp = testarConexaoHttp ?? throw new ArgumentNullException(nameof(testarConexaoHttp));
        }

        /// <summary>Executa as etapas locais na ordem proxy, DNS, TCP e TLS.</summary>
        /// <param name="configuracao">Configuração de certificado e proxy.</param>
        /// <param name="endpoint">URL do serviço a ser verificado.</param>
        /// <param name="timeoutMilissegundos">Tempo máximo de cada etapa.</param>
        /// <returns>Sondas com duração e classificação de cada etapa.</returns>
        public IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
        {
            var resultados = new List<ResultadoSondaDisponibilidade>();
            Uri uri;
            if (!Uri.TryCreate(endpoint, UriKind.Absolute, out uri))
            {
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.Configuracao, new UriFormatException("Endpoint inválido.")));
                return resultados;
            }

            var soap = new ConfiguracaoWSSoapMapper().Map(configuracao);
            if (soap.Proxy != null)
            {
                var cronometroProxy = Stopwatch.StartNew();
                var teste = testarConexaoHttp(endpoint, configuracao.CertificadoDigital,
                    Math.Max(1, timeoutMilissegundos / 1000), soap.Proxy, "HEAD");
                cronometroProxy.Stop();
                var proxy = Base("Proxy", endpoint, "HTTP", cronometroProxy.ElapsedMilliseconds);
                proxy.HttpStatusCode = teste.StatusCode;
                proxy.Status = teste.ResponseReceived ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo;
                if (!teste.ResponseReceived)
                {
                    proxy.TipoFalha = ConverterFalha(teste.FailureType);
                    proxy.Excecao = ClassificadorDisponibilidade.SanitizarMensagem(teste.ErrorMessage);
                }
                resultados.Add(proxy);
                return resultados;
            }

            IPAddress[] enderecos;
            var cronometroDns = Stopwatch.StartNew();
            try
            {
                var tarefaDns = Dns.GetHostAddressesAsync(uri.Host);
                if (!tarefaDns.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite ao resolver o host do endpoint.");
                enderecos = tarefaDns.Result;
                cronometroDns.Stop();
                resultados.Add(Sucesso("DNS", endpoint, "DNS", cronometroDns.ElapsedMilliseconds));
            }
            catch (Exception ex)
            {
                cronometroDns.Stop();
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.DNS, ex, cronometroDns.ElapsedMilliseconds));
                return resultados;
            }

            if (enderecos.Length == 0)
            {
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.DNS, new Exception("O host não retornou endereços IP.")));
                return resultados;
            }

            var porta = uri.IsDefaultPort ? (uri.Scheme == Uri.UriSchemeHttps ? 443 : 80) : uri.Port;
            using (var tcp = new TcpClient())
            {
                var cronometroTcp = Stopwatch.StartNew();
                try
                {
                    var tarefa = tcp.ConnectAsync(uri.Host, porta);
                    if (!tarefa.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite ao conectar ao endpoint.");
                    cronometroTcp.Stop();
                    resultados.Add(Sucesso("TCP", endpoint, "TCP", cronometroTcp.ElapsedMilliseconds));
                }
                catch (Exception ex)
                {
                    cronometroTcp.Stop();
                    var tipo = ex is TimeoutException ? TipoFalhaDisponibilidade.Timeout : TipoFalhaDisponibilidade.Conexao;
                    resultados.Add(Falha("TCP", endpoint, tipo, ex, cronometroTcp.ElapsedMilliseconds));
                    return resultados;
                }

                if (uri.Scheme == Uri.UriSchemeHttps)
                {
                    var cronometroTls = Stopwatch.StartNew();
                    try
                    {
                        tcp.ReceiveTimeout = timeoutMilissegundos;
                        tcp.SendTimeout = timeoutMilissegundos;
                        using (var ssl = new SslStream(tcp.GetStream(), false))
                        {
                            var certificados = new X509CertificateCollection();
                            if (configuracao.CertificadoDigital != null) certificados.Add(configuracao.CertificadoDigital);
                            var tarefaTls = Task.Run(() => ssl.AuthenticateAsClient(uri.Host, certificados, SslProtocols.Tls12, true));
                            if (!tarefaTls.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite durante a negociação TLS.");
                            tarefaTls.GetAwaiter().GetResult();
                        }
                        cronometroTls.Stop();
                        resultados.Add(Sucesso("TLS", endpoint, "TLS", cronometroTls.ElapsedMilliseconds));
                    }
                    catch (Exception ex)
                    {
                        cronometroTls.Stop();
                        resultados.Add(Falha("TLS", endpoint, TipoFalhaDisponibilidade.TLS, ex, cronometroTls.ElapsedMilliseconds));
                    }
                }
            }
            return resultados;
        }

        /// <summary>Cria uma sonda operacional para uma etapa que respondeu.</summary>
        /// <param name="servico">Nome da etapa.</param>
        /// <param name="endpoint">Endpoint sanitizado.</param>
        /// <param name="protocolo">Protocolo observado.</param>
        /// <param name="duracao">Duração da etapa.</param>
        /// <returns>Sonda operacional.</returns>
        private static ResultadoSondaDisponibilidade Sucesso(string servico, string endpoint, string protocolo, long duracao)
        {
            var item = Base(servico, endpoint, protocolo, duracao);
            item.Status = StatusDisponibilidade.Operacional;
            item.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
            return item;
        }

        /// <summary>Cria uma sonda inconclusiva para uma falha local capturada.</summary>
        /// <param name="servico">Nome da etapa que falhou.</param>
        /// <param name="endpoint">Endpoint sanitizado.</param>
        /// <param name="tipo">Categoria da falha.</param>
        /// <param name="ex">Exceção que será sanitizada.</param>
        /// <param name="duracao">Duração da tentativa.</param>
        /// <returns>Sonda de falha local.</returns>
        private static ResultadoSondaDisponibilidade Falha(string servico, string endpoint, TipoFalhaDisponibilidade tipo, Exception ex, long duracao = 0)
        {
            var item = Base(servico, endpoint, servico, duracao);
            item.Status = StatusDisponibilidade.Inconclusivo;
            item.TipoFalha = tipo;
            item.Excecao = ClassificadorDisponibilidade.SanitizarExcecao(ex);
            return item;
        }

        /// <summary>Cria os campos comuns de uma sonda de infraestrutura.</summary>
        /// <param name="servico">Nome da etapa.</param>
        /// <param name="endpoint">Endpoint sanitizado.</param>
        /// <param name="protocolo">Protocolo da etapa.</param>
        /// <param name="duracao">Duração medida.</param>
        /// <returns>Sonda parcialmente preenchida.</returns>
        private static ResultadoSondaDisponibilidade Base(string servico, string endpoint, string protocolo, long duracao) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = servico,
                Endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint),
                Protocolo = protocolo,
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                DataHora = RelogioDisponibilidade.Agora(),
                DuracaoMilissegundos = duracao,
                Essencial = true
            };

        /// <summary>Converte a categoria retornada pela Utils para o enum público da DLL.</summary>
        /// <param name="falha">Categoria técnica da conexão.</param>
        /// <returns>Categoria equivalente de disponibilidade.</returns>
        private static TipoFalhaDisponibilidade ConverterFalha(Unimake.Net.HttpConnectionFailureType falha)
        {
            switch (falha)
            {
                case Unimake.Net.HttpConnectionFailureType.Dns: return TipoFalhaDisponibilidade.DNS;
                case Unimake.Net.HttpConnectionFailureType.Connection: return TipoFalhaDisponibilidade.Conexao;
                case Unimake.Net.HttpConnectionFailureType.Timeout: return TipoFalhaDisponibilidade.Timeout;
                case Unimake.Net.HttpConnectionFailureType.Tls: return TipoFalhaDisponibilidade.TLS;
                case Unimake.Net.HttpConnectionFailureType.Proxy: return TipoFalhaDisponibilidade.Proxy;
                case Unimake.Net.HttpConnectionFailureType.Http: return TipoFalhaDisponibilidade.HTTP;
                default: return TipoFalhaDisponibilidade.Desconhecida;
            }
        }
    }

    /// <summary>Cache em memória das sondas locais para evitar repetição de testes caros.</summary>
    /// <remarks>O cache existe somente durante a vida do processo e nunca cria arquivos.</remarks>
    internal static class CacheInfraestruturaDisponibilidade
    {
        /// <summary>Quantidade máxima de combinações de endpoint e contexto local mantidas.</summary>
        private const int MaximoEntradas = 256;
        /// <summary>Chave aleatória usada para gerar identidades irreversíveis de proxy.</summary>
        private static readonly byte[] ChaveIdentidadeProxy = CriarChaveIdentidadeProxy();
        /// <summary>Dados de uma entrada individual do cache local.</summary>
        private sealed class Entrada
        {
            /// <summary>Lock exclusivo da entrada para evitar duas sondas simultâneas iguais.</summary>
            public readonly object SyncRoot = new object();
            /// <summary>Momento em que o resultado foi produzido.</summary>
            public DateTime DataHora;
            /// <summary>Lista de sondas armazenada.</summary>
            public IList<ResultadoSondaDisponibilidade> Resultados;
        }
        /// <summary>Lock usado somente para localizar ou inserir entradas do dicionário.</summary>
        private static readonly object SyncRoot = new object();
        /// <summary>Dicionário privado de resultados locais em memória.</summary>
        private static readonly Dictionary<string, Entrada> Cache = new Dictionary<string, Entrada>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Obtém o cache ou executa as sondas quando a entrada expirou.</summary>
        /// <param name="configuracao">Configuração de rede, proxy e certificado.</param>
        /// <param name="endpoint">Endpoint a testar.</param>
        /// <param name="opcoes">Tempo de cache e timeout.</param>
        /// <param name="executor">Executor real ou de teste.</param>
        /// <returns>Sondas novas ou cópias marcadas como cache.</returns>
        internal static IList<ResultadoSondaDisponibilidade> ObterOuExecutar(Configuracao configuracao, string endpoint,
            ConfiguracaoDiagnosticoDisponibilidade opcoes, IExecutorInfraestruturaDisponibilidade executor)
        {
            var chave = CriarChaveCache(configuracao, endpoint, opcoes.TimeoutMilissegundos);
            Entrada entrada;
            lock (SyncRoot)
            {
                if (!Cache.TryGetValue(chave, out entrada))
                {
                    if (Cache.Count >= MaximoEntradas)
                    {
                        var antiga = Cache.OrderBy(x => x.Value.DataHora).First().Key;
                        Cache.Remove(antiga);
                    }
                    entrada = new Entrada();
                    Cache.Add(chave, entrada);
                }
            }

            lock (entrada.SyncRoot)
            {
                var agora = RelogioDisponibilidade.Agora();
                if (entrada.Resultados != null && agora - entrada.DataHora < TimeSpan.FromSeconds(opcoes.CacheInfraestruturaSegundos))
                {
                    return entrada.Resultados.Select(x => { var c = ClassificadorDisponibilidade.Clonar(x); c.DoCache = true; c.IdadeSegundos = (long)(agora - entrada.DataHora).TotalSeconds; return c; }).ToList();
                }
                var resultados = executor.Executar(configuracao, endpoint, opcoes.TimeoutMilissegundos);
                entrada.DataHora = agora;
                entrada.Resultados = resultados.Select(ClassificadorDisponibilidade.Clonar).ToList();
                return resultados;
            }
        }

        /// <summary>Cria uma chave sanitizada para identificar uma combinação de teste local.</summary>
        /// <param name="configuracao">Configuração de certificado e proxy.</param>
        /// <param name="endpoint">Endpoint que será testado.</param>
        /// <param name="timeoutMilissegundos">Timeout que influencia o resultado.</param>
        /// <returns>Chave sem senha ou usuário em texto puro.</returns>
        internal static string CriarChaveCache(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
        {
            return ClassificadorDisponibilidade.SanitizarEndpoint(endpoint) + "|" +
                CriarIdentidadeContextoLocal(configuracao, timeoutMilissegundos);
        }

        /// <summary>Cria a identidade irreversível do contexto local usado no cache de status.</summary>
        /// <param name="configuracao">Certificado e proxy do contexto atual.</param>
        /// <param name="timeoutMilissegundos">Timeout que pode alterar a conclusão.</param>
        /// <returns>Identidade composta sem expor credenciais.</returns>
        internal static string CriarIdentidadeContextoLocal(Configuracao configuracao, int timeoutMilissegundos)
        {
            var identidadeCertificado = configuracao.CertificadoDigital == null
                ? "sem-certificado"
                : configuracao.CertificadoDigital.GetCertHashString();
            return identidadeCertificado + "|" +
                timeoutMilissegundos.ToString(CultureInfo.InvariantCulture) + "|" + IdentidadeProxy(configuracao);
        }

        /// <summary>Gera uma identidade HMAC do proxy sem guardar usuário ou senha.</summary>
        /// <param name="configuracao">Configuração que contém o proxy.</param>
        /// <returns>Identidade irreversível do proxy.</returns>
        private static string IdentidadeProxy(Configuracao configuracao)
        {
            if (!configuracao.HasProxy)
            {
                return "sem-proxy";
            }

            var material = configuracao.ProxyAutoDetect.ToString(CultureInfo.InvariantCulture) + "\0" +
                (configuracao.ProxyUser ?? string.Empty) + "\0" + (configuracao.ProxyPassword ?? string.Empty);
            using (var hmac = new HMACSHA256(ChaveIdentidadeProxy))
            {
                return BitConverter.ToString(hmac.ComputeHash(Encoding.UTF8.GetBytes(material))).Replace("-", string.Empty);
            }
        }

        /// <summary>Cria a chave aleatória usada pelo HMAC durante este processo.</summary>
        /// <returns>Chave privada mantida somente em memória.</returns>
        private static byte[] CriarChaveIdentidadeProxy()
        {
            var chave = new byte[32];
            using (var gerador = RandomNumberGenerator.Create())
            {
                gerador.GetBytes(chave);
            }
            return chave;
        }

        /// <summary>Remove todas as entradas do cache local.</summary>
        internal static void Limpar() { lock (SyncRoot) Cache.Clear(); }
    }

    /// <summary>Controla o cache e o intervalo mínimo das consultas de StatusServico.</summary>
    /// <remarks>O cache é compartilhado somente dentro do processo e protege a SEFAZ contra excesso de consultas.</remarks>
    internal static class CacheStatusDisponibilidade
    {
        /// <summary>Quantidade máxima de combinações de status mantidas.</summary>
        private const int MaximoEntradas = 256;
        /// <summary>Dados de uma entrada do cache de status.</summary>
        private sealed class Entrada
        {
            /// <summary>Lock que serializa consultas iguais.</summary>
            public readonly object SyncRoot = new object();
            /// <summary>Momento da última tentativa.</summary>
            public DateTime DataHora;
            /// <summary>Fim do bloqueio especial de consumo indevido.</summary>
            public DateTime BloqueadoAte;
            /// <summary>Última sonda armazenada.</summary>
            public ResultadoSondaDisponibilidade Resultado;
            /// <summary>Identidade local associada a uma falha não compartilhável.</summary>
            public string ContextoLocal;
            /// <summary>Indica se a sonda pode ser reutilizada por outros contextos.</summary>
            public bool ResultadoCompartilhavel;
        }

        /// <summary>Lock do dicionário e dos bloqueios nacionais/estaduais.</summary>
        private static readonly object CacheSync = new object();
        /// <summary>Resultados de status indexados por documento, UF, ambiente e endpoint.</summary>
        private static readonly Dictionary<string, Entrada> Cache = new Dictionary<string, Entrada>(StringComparer.OrdinalIgnoreCase);
        /// <summary>Bloqueios por tipo de documento e ambiente, incluindo evidências nacionais.</summary>
        private static readonly Dictionary<string, DateTime> BloqueiosContexto = new Dictionary<string, DateTime>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Registra que novas consultas de status devem ser evitadas até uma data.</summary>
        /// <param name="configuracao">Configuração cujo documento e ambiente serão bloqueados.</param>
        /// <param name="bloqueadoAte">Data e hora final do bloqueio.</param>
        internal static void BloquearContexto(Configuracao configuracao, DateTime bloqueadoAte)
        {
            var chave = ChaveContexto(configuracao);
            lock (CacheSync)
            {
                DateTime atual;
                if (!BloqueiosContexto.TryGetValue(chave, out atual) || atual < bloqueadoAte)
                {
                    BloqueiosContexto[chave] = bloqueadoAte;
                }
            }
        }

        /// <summary>Verifica se o documento e ambiente estão temporariamente bloqueados.</summary>
        /// <param name="configuracao">Configuração que será consultada.</param>
        /// <param name="bloqueadoAte">Data final encontrada, quando houver bloqueio.</param>
        /// <returns><see langword="true"/> se a nova consulta deve ser evitada.</returns>
        internal static bool ContextoBloqueado(Configuracao configuracao, out DateTime bloqueadoAte)
        {
            lock (CacheSync)
            {
                if (BloqueiosContexto.TryGetValue(ChaveContexto(configuracao), out bloqueadoAte))
                {
                    if (RelogioDisponibilidade.Agora() < bloqueadoAte) return true;
                    BloqueiosContexto.Remove(ChaveContexto(configuracao));
                }
                return false;
            }
        }

        /// <summary>Reutiliza uma sonda válida ou executa uma nova consulta de status.</summary>
        /// <param name="chave">Identidade compartilhada do endpoint fiscal.</param>
        /// <param name="intervalo">Intervalo mínimo entre tentativas.</param>
        /// <param name="executar">Função que realiza a consulta quando necessário.</param>
        /// <returns>Sonda nova, compartilhada ou suprimida pelo intervalo.</returns>
        internal static ResultadoSondaDisponibilidade ObterOuExecutar(string chave, TimeSpan intervalo,
            Func<ResultadoSondaDisponibilidade> executar) =>
            ObterOuExecutar(chave, "contexto-padrao", intervalo, executar);

        /// <summary>Versão que também considera a identidade local para falhas específicas.</summary>
        /// <param name="chave">Identidade compartilhada do endpoint fiscal.</param>
        /// <param name="contextoLocal">Identidade do certificado, proxy e timeout atuais.</param>
        /// <param name="intervalo">Intervalo mínimo entre tentativas.</param>
        /// <param name="executar">Função que realiza a consulta.</param>
        /// <returns>Sonda adequada ao contexto e ao intervalo configurado.</returns>
        internal static ResultadoSondaDisponibilidade ObterOuExecutar(string chave, string contextoLocal,
            TimeSpan intervalo, Func<ResultadoSondaDisponibilidade> executar)
        {
            Entrada entrada;
            lock (CacheSync)
            {
                if (!Cache.TryGetValue(chave, out entrada))
                {
                    if (Cache.Count >= MaximoEntradas)
                    {
                        var antiga = Cache.OrderBy(x => x.Value.DataHora).First().Key;
                        Cache.Remove(antiga);
                    }
                    entrada = new Entrada();
                    Cache.Add(chave, entrada);
                }
            }

            lock (entrada.SyncRoot)
            {
                var agora = RelogioDisponibilidade.Agora();
                if (entrada.Resultado != null && (agora < entrada.BloqueadoAte || agora - entrada.DataHora < intervalo))
                {
                    if (!entrada.ResultadoCompartilhavel &&
                        !string.Equals(entrada.ContextoLocal, contextoLocal, StringComparison.Ordinal))
                    {
                        return CriarResultadoSuprimido(entrada, agora);
                    }

                    var cache = ClassificadorDisponibilidade.Clonar(entrada.Resultado);
                    cache.DoCache = true;
                    cache.IdadeSegundos = Math.Max(0, (long)(agora - entrada.DataHora).TotalSeconds);
                    return cache;
                }

                var resultado = executar();
                entrada.DataHora = agora;
                entrada.Resultado = ClassificadorDisponibilidade.Clonar(resultado);
                entrada.ContextoLocal = contextoLocal;
                entrada.ResultadoCompartilhavel = ResultadoPodeSerCompartilhado(resultado);
                if (resultado.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
                {
                    entrada.BloqueadoAte = agora.AddHours(1);
                }
                return resultado;
            }
        }

        /// <summary>Decide se um retorno fiscal pode ser compartilhado entre contribuintes.</summary>
        /// <param name="resultado">Sonda que será avaliada.</param>
        /// <returns><see langword="true"/> somente para códigos fiscais oficiais conhecidos.</returns>
        private static bool ResultadoPodeSerCompartilhado(ResultadoSondaDisponibilidade resultado) =>
            resultado != null &&
            (resultado.CStat == 107 || resultado.CStat == 108 || resultado.CStat == 109 ||
             resultado.CStat == 656 || resultado.CStat == 678);

        /// <summary>Cria uma sonda neutra quando o intervalo impede reutilizar uma falha de outro contexto.</summary>
        /// <param name="entrada">Entrada que causou a supressão.</param>
        /// <param name="agora">Horário atual do diagnóstico.</param>
        /// <returns>Sonda inconclusiva marcada como cache.</returns>
        private static ResultadoSondaDisponibilidade CriarResultadoSuprimido(Entrada entrada, DateTime agora) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Endpoint = entrada.Resultado.Endpoint,
                Protocolo = entrada.Resultado.Protocolo,
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                DataHora = agora,
                IdadeSegundos = Math.Max(0, (long)(agora - entrada.DataHora).TotalSeconds),
                Status = StatusDisponibilidade.Inconclusivo,
                TipoFalha = TipoFalhaDisponibilidade.Protocolo,
                XMotivo = "A consulta foi suprimida pelo intervalo mínimo porque a evidência anterior pertence a outro contexto local.",
                DoCache = true,
                Essencial = true
            };

        /// <summary>Limpa resultados e bloqueios mantidos em memória no processo.</summary>
        internal static void Limpar()
        {
            lock (CacheSync)
            {
                Cache.Clear();
                BloqueiosContexto.Clear();
            }
        }

        /// <summary>Cria a chave nacionalizada por tipo de documento e ambiente.</summary>
        /// <param name="configuracao">Configuração que fornece documento e ambiente.</param>
        /// <returns>Chave usada para compartilhar bloqueios entre UFs.</returns>
        private static string ChaveContexto(Configuracao configuracao) =>
            ((int)configuracao.TipoDFe).ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)configuracao.TipoAmbiente).ToString(CultureInfo.InvariantCulture);
    }

    /// <summary>Fachada simples para usar o diagnóstico em Object Pascal, Lazarus e outras linguagens COM.</summary>
    /// <remarks>
    /// O núcleo do diagnóstico continua interno e seguro para a DLL. Esta classe oferece apenas métodos fáceis
    /// de chamar: executar a análise, ler a telemetria passiva, consultar o status controlado e limpar a memória.
    /// Não é necessário criar arquivos nem manter um objeto de cache no programa cliente.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.DiagnosticoDisponibilidadeDFeInterop")]
    [ComVisible(true)]
#endif
    public class DiagnosticoDisponibilidadeDFeInterop
    {
        /// <summary>Executa a análise usando somente telemetria já coletada e verificações locais.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado com estado, origem provável, descrição para o usuário e sondas detalhadas.</returns>
        public ResultadoDiagnosticoDisponibilidade Executar(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).Executar();

        /// <summary>Obtém somente evidências de operações reais que já ocorreram.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado sem enviar nova mensagem fiscal.</returns>
        public ResultadoDiagnosticoDisponibilidade ObterDiagnosticoPassivo(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ObterDiagnosticoPassivo();

        /// <summary>Consulta o StatusServico oficial, respeitando cache, intervalo mínimo e bloqueio de consumo indevido.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado da resposta oficial ou inconclusivo quando a configuração não permite consultar.</returns>
        public ResultadoDiagnosticoDisponibilidade ConsultarStatusServico(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

        /// <summary>Limpa histórico, cache e bloqueios mantidos em memória no processo atual.</summary>
        /// <remarks>Use somente quando o programa desejar iniciar um novo ciclo de diagnóstico.</remarks>
        public void LimparMemoriaDiagnostico() => DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
    }
}
