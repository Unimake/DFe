#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Builders;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator;
using Unimake.Business.DFe.Validator.Abstractions;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Classe base abstrata para elaboração dos serviços dos documentos fiscais eletrônicos (NFe, NFCe, MDFe, NFSe, CTe, GNRE, etc...)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : IDisposable
    {
        private bool _disposed = false;
        private XmlDocument _ConteudoXML;

        /// <summary>
        /// Verifica se o XML está assinado, se não estiver assina. Só faz isso para XMLs que tem tag de assinatura, demais ele mantem como está, sem assinar.
        /// </summary>
        /// <param name="tagAssinatura">Tag de assinatura</param>
        /// <param name="tagAtributoID">Tag que detêm o atributo ID</param>
        protected virtual void VerificarAssinarXML(string tagAssinatura, string tagAtributoID)
        {
            if (Configuracoes.UsaCertificadoDigital)
            {
                if (!string.IsNullOrWhiteSpace(tagAssinatura) && Configuracoes.NaoAssina == null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
                {
                    if (AssinaturaDigital.EstaAssinado(ConteudoXML, tagAssinatura))
                    {
                        AjustarXMLAposAssinado();
                    }
                    else
                    {
                        AssinaturaDigital.Assinar(ConteudoXML, tagAssinatura, tagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "", true);

                        AjustarXMLAposAssinado();
                    }
                }
            }
        }

        /// <summary>
        /// Conteúdo do XML, pode ou não estar assinado. Esta propriedade é utilizada em tempo de processamento.
        /// Utilize as propriedades ConteudoXMLOriginal ou ConteudoXMLAssinado para recuperar o que você deseja fora da classe.
        /// </summary>
        protected XmlDocument ConteudoXML
        {
            get => _ConteudoXML;
            set
            {
                if (ConteudoXMLOriginal == null)
                {
                    ConteudoXMLOriginal = new XmlDocument();
                    ConteudoXMLOriginal.LoadXml(value?.OuterXml);
                }

                _ConteudoXML = value;
            }
        }

        /// <summary>
        /// Exceções que não interrompem o fluxo do sistema, sendo registradas apenas como avisos ou alertas.
        /// </summary>
        public List<ValidatorDFeException> Warnings { get; protected set; } = new List<ValidatorDFeException>();

        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase()
        {
        }

        /// <summary>
        /// Este método é uma possibilidade de fazer ajustes no XML depois de assinado, pois ele é executado assim que a assinatura é feita. Basta implementar ele nas heranças.
        /// </summary>
        protected virtual void AjustarXMLAposAssinado() { }

        /// <summary>
        /// Defini o valor das propriedades do objeto "Configuracoes"
        /// </summary>
        protected virtual void DefinirConfiguracao()
        {
            /* 
             As alterações a seguir foram necessárias para impedir uma leitura errada das configurações:
            Ao passar a primeira vez pela função "Load(GetType().Name)", ela carregava o arquivo Config.xml onde contém as configurações individuais dos municípios e carrega as informações deste
            Ao ser chamada uma segunda vez (independente do motivo), ela procurava o arquivo de configuração do município dentro do próprio alvo da busca e isto gerava erro.
            
            Atualmente, a propriedade Configuracoes.Definida está sendo atribuído o valor true apenas para NFSe;

             "Ok, mas por que não definir todas as configurações dentro do ServiçoBaseNFSe?"
                Por que há configurações dentro do EnveloparXML() que estão sendo compartilhadas em mais de um DFe
             
             Nesta refatoração, estou colocando oque é específico de um serviço, dentro da classe do serviço (Ex: GerarJSON() para as classes EnvioDARE e EnvioDARELote);
                ....                            oque é específico entre um tipo de DFe, dentro da classe ServicoBaseDFe..

             */
            if (!Configuracoes.Definida)
            {
                Configuracoes.Load(GetType().Name);
            }

            Configuracoes.Definida = true;
        }

        /// <summary>
        /// Cria o <see cref="HttpContent"/> padrão para os serviços que consomem API e não possuem tratamento específico.
        /// </summary>
        /// <returns>Conteúdo HTTP pronto para envio.</returns>
        protected virtual HttpContent CriarHttpContentPadrao() =>
            new CommonApiPayloadBuilder().Build(Configuracoes, ConteudoXML);

        /// <summary>
        /// Validar o schema do XML
        /// </summary>
        protected abstract void XmlValidar();

        /// <summary>
        /// Validar o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected abstract void XmlValidarConteudo();

        /// <summary>
        /// Inicializa configurações, parâmetros e propriedades para execução do serviço.
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML a ser enviado para o web-service</param>
        /// <param name="configuracao">Configurações a serem utilizadas para conexão e envio do XML para o web-service</param>
#if INTEROP
        [ComVisible(false)]
#endif
        protected virtual void Inicializar(XmlDocument conteudoXML, Configuracao configuracao)
        {
            Configuracoes = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            ConteudoXML = conteudoXML ?? throw new ArgumentNullException(nameof(conteudoXML));

            Warnings.Clear();

            var validatorFactory = new ValidatorFactory();
            var validator = (XmlValidatorBase)(validatorFactory.BuidValidator(ConteudoXML.InnerXml));
            var validou = (validator?.Validate() ?? true);

            Warnings = validator?.Warnings;

            if (!validou)
            {
                return;
            }

            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            System.Diagnostics.Trace.WriteLine(ConteudoXML?.InnerXml, "Unimake.DFe");

            XmlValidar();
        }


        /// <summary>
        /// Configurações diversas para consumir os serviços
        /// </summary>
        public Configuracao Configuracoes { get; set; }

        /// <summary>
        /// Conteúdo do XML assinado.
        /// </summary>
        public virtual XmlDocument ConteudoXMLAssinado
        {
            get
            {
                return ConteudoXML;
            }
        }

        /// <summary>
        /// Helper que delega a validação para a implementação centralizada ValidarEstruturaXML.
        /// Retorna o resultado sem lançar exceção — o chamador decide como tratar o resultado e quando chamar AjustarXMLAposAssinado().
        /// </summary>
        protected Unimake.Business.DFe.ValidarEstruturaXML.ResultadoValidacao ValidarXMLCentralizado()
        {
            var validator = new Unimake.Business.DFe.ValidarEstruturaXML();
            var resultado = validator.ValidarServico(ConteudoXML, Configuracoes);
            return resultado;
        }

#if INTEROP

        /// <summary>
        /// Recupera o conteúdo do XML assinado.
        /// </summary>
        /// <returns>Retorna conteúdo do XML assinado</returns>
        public string GetConteudoXMLAssinado() => (ConteudoXMLAssinado != null ? ConteudoXMLAssinado.OuterXml : "");

        /// <summary>
        /// Recupera o conteúdo do XML original.
        /// </summary>
        /// <returns>Retorna conteúdo do XML original</returns>
        public string GetConteudoXMLOriginal() => (ConteudoXMLOriginal != null ? ConteudoXMLOriginal.OuterXml : "");


#endif

        /// <summary>
        /// Propriedade para uso interno nos testes unitários. 
        /// </summary>
        public HttpStatusCode HttpStatusCode { get; private set; }

        /// <summary>
        /// Conteúdo do XML original, para os que tem assinatura este está sem. Original conforme foi criado.
        /// </summary>
        public XmlDocument ConteudoXMLOriginal { get; private set; }

        /// <summary>
        /// String do XML retornado pelo WebService
        /// </summary>
        public string RetornoWSString { get; set; }

        /// <summary>
        /// Conteúdo bruto retornado pelo serviço antes da transformação para XML.
        /// </summary>
        public string RetornoWSRawString { get; set; }

        /// <summary>
        /// XML retornado pelo Web-service
        /// </summary>
        public XmlDocument RetornoWSXML { get; set; }

        /// <summary>
        /// Stream retornada pelo Webservice. Para consumo de serviços que retornam .pdf
        /// </summary>
        public Stream RetornoWSStream { get; set; }

        static ServicoBase() => AppDomain.CurrentDomain.AssemblyResolve += AssemblyResolver.AssemblyResolve;

        /// <summary>
        /// Executar o serviço para consumir o web-service
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public virtual void Executar()
        {
            if (!string.IsNullOrWhiteSpace(Configuracoes.TagAssinatura) && Configuracoes.NaoAssina != null && Configuracoes.NaoAssina != Configuracoes.TipoAmbiente)
            {
                if (!AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagAssinatura))
                {
                    AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagAssinatura, Configuracoes.TagAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
                    AjustarXMLAposAssinado();
                }
            }

            if (Configuracoes.IsAPI)
            {
                if (Configuracoes.RequestURI != null && !string.Equals(Configuracoes.MetodoAPI, "get", StringComparison.OrdinalIgnoreCase))
                {
                    Configuracoes.HttpContent = CriarHttpContentPadrao();
                }

                var apiConfig = new ConfiguracaoApiConfigMapper().Map(Configuracoes);

                var consumirAPI = new ConsumirAPI();
                if (!TelemetriaDisponibilidade.EstaHabilitada(Configuracoes))
                {
                    consumirAPI.ExecutarServico(apiConfig, Configuracoes.CertificadoDigital);
                }
                else
                {
                    var inicioTelemetria = Stopwatch.GetTimestamp();
                    Exception falhaTransporte = null;
                    try
                    {
                        consumirAPI.ExecutarServico(apiConfig, Configuracoes.CertificadoDigital);
                    }
                    catch (Exception ex)
                    {
                        falhaTransporte = ex;
                        throw;
                    }
                    finally
                    {
                        var duracaoTelemetria = (long)((Stopwatch.GetTimestamp() - inicioTelemetria) *
                            1000.0 / Stopwatch.Frequency);
                        TelemetriaDisponibilidade.Registrar(Configuracoes, apiConfig.RequestURI, "REST", duracaoTelemetria,
                            consumirAPI.HttpStatusCode, consumirAPI.RetornoServicoXML, falhaTransporte);
                    }
                }

                RetornoWSString = consumirAPI.RetornoServicoString;
                RetornoWSRawString = consumirAPI.RetornoServicoRawString;
                RetornoWSXML = consumirAPI.RetornoServicoXML;
                HttpStatusCode = consumirAPI.HttpStatusCode;

                // Copiar o stream para um MemoryStream antes do Dispose
                if (consumirAPI.RetornoServicoStream != null)
                {
                    var memoryStream = new MemoryStream();
                    consumirAPI.RetornoServicoStream.CopyTo(memoryStream);
                    memoryStream.Position = 0;
                    RetornoWSStream = memoryStream;
                }
                else
                {
                    RetornoWSStream = null;
                }

                apiConfig.Dispose();
                consumirAPI.Dispose();
            }
            else
            {
                var soap = new ConfiguracaoWSSoapMapper().Map(Configuracoes);

                var consumirWS = new ConsumirWS();
                if (!TelemetriaDisponibilidade.EstaHabilitada(Configuracoes))
                {
                    consumirWS.ExecutarServico(ConteudoXML, soap, Configuracoes.CertificadoDigital);
                }
                else
                {
                    var inicioTelemetria = Stopwatch.GetTimestamp();
                    Exception falhaTransporte = null;
                    try
                    {
                        consumirWS.ExecutarServico(ConteudoXML, soap, Configuracoes.CertificadoDigital);
                    }
                    catch (Exception ex)
                    {
                        falhaTransporte = ex;
                        throw;
                    }
                    finally
                    {
                        var duracaoTelemetria = (long)((Stopwatch.GetTimestamp() - inicioTelemetria) *
                            1000.0 / Stopwatch.Frequency);
                        TelemetriaDisponibilidade.Registrar(Configuracoes, soap.EnderecoWeb, "SOAP", duracaoTelemetria,
                            consumirWS.HttpStatusCode, consumirWS.RetornoServicoXML, falhaTransporte);
                    }
                }

                RetornoWSString = consumirWS.RetornoServicoString;
                RetornoWSXML = consumirWS.RetornoServicoXML;
                HttpStatusCode = consumirWS.HttpStatusCode;

                consumirWS.Dispose();
            }
        }

        /// <summary>
        /// Gravar o XML de distribuição em uma pasta no HD
        /// </summary>
        /// <param name="pasta">Pasta onde deve ser gravado o XML no HD</param>
        /// <param name="nomeArquivo">Nome do arquivo a ser gravado no HD</param>
        /// <param name="conteudoXML">String contendo o conteúdo do XML a ser gravado no HD</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public abstract void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML);

        /// <summary>
        /// Implementação do padrão Dispose para liberar recursos.
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        /// <summary>
        /// Dispose protegido para sobrescrita em classes derivadas.
        /// </summary>
        /// <param name="disposing">Indica se está liberando recursos gerenciados.</param>
        protected virtual void Dispose(bool disposing)
        {
            if (_disposed)
                return;

            if (disposing)
            {
                // Liberar recursos gerenciados
                if (RetornoWSStream != null)
                {
                    RetornoWSStream.Dispose();
                    RetornoWSStream = null;
                }
            }

            // Liberar recursos não gerenciados (se houver)

            _disposed = true;
        }

        /// <summary>
        /// Finalizador
        /// </summary>
        ~ServicoBase()
        {
            Dispose(false);
        }
    }
}
