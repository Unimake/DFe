#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Consulta NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse")]
    [ComVisible(true)]
#endif
    public class ConsultarNfse : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarNfse() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public ConsultarNfse(XmlDocument conteudoXML, Configuracao configuracao) : this() => Inicializar(conteudoXML, configuracao);

        /// <summary>
        /// Obtém a tag de configuração correspondente ao serviço de consulta solicitado.
        /// </summary>
        /// <returns>Nome da tag do serviço.</returns>
        protected override string ObterNomeTagServico()
        {
            if (GetType() != typeof(ConsultarNfse))
            {
                return base.ObterNomeTagServico();
            }

            switch (Configuracoes.Servico)
            {
                case Servico.NFSeConsultarNfse:
                    return nameof(ConsultarNfse);

                case Servico.NFSeConsultarNfseFaixa:
                    return "ConsultarNfseFaixa";

                case Servico.NFSeConsultarNfseServicoPrestado:
                    return "ConsultarNfseServicoPrestado";

                case Servico.NFSeConsultarNotaFiscal:
                    return "ConsultarNotaFiscal";

                case Servico.NFSeConsultarNotaValida:
                    return "ConsultarNotaValida";

                case Servico.NFSeObterNotaFiscalXml:
                    return "ObterNotaFiscalXml";

                case Servico.NFSeConsultaNFeEmitidas:
                    return "ConsultaNFeEmitidas";

                case Servico.NFSeConsultarNotaPrestador:
                    return "ConsultarNotaPrestador";

                default:
                    throw new System.InvalidOperationException(
                        $"O serviço {Configuracoes.Servico} não pode ser executado pela classe {nameof(ConsultarNfse)}. " +
                        $"Serviços aceitos: {Servico.NFSeConsultarNfse}, {Servico.NFSeConsultarNfseFaixa}, " +
                        $"{Servico.NFSeConsultarNfseServicoPrestado}, {Servico.NFSeConsultarNotaFiscal}, " +
                        $"{Servico.NFSeConsultarNotaValida}, {Servico.NFSeObterNotaFiscalXml}, " +
                        $"{Servico.NFSeConsultaNFeEmitidas} e {Servico.NFSeConsultarNotaPrestador}.");
            }
        }

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                base.DefinirConfiguracao();
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public override void Executar(string conteudoXML, Configuracao configuracao) => base.Executar(conteudoXML, configuracao);

#endif

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            base.Executar();
        }

        /// <summary>
        /// Resultado da consulta NFSe (apenas para padrão NACIONAL).
        /// Retorna a NFSe completa em caso de sucesso.
        /// Retorna null se não houver retorno ou se a tag raiz não for NFSe.
        /// </summary>
#if INTEROP
        [ComVisible(true)]
#endif
        public Xml.NFSe.NACIONAL.NFSe.NFSe Result
        {
            get
            {
                if (string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return null;
                }

                try
                {
                    var tagRaiz = RetornoWSXML.DocumentElement?.Name;
                    if (tagRaiz == "NFSe")
                    {
                        return XMLUtility.Deserializar<Xml.NFSe.NACIONAL.NFSe.NFSe>(RetornoWSXML);
                    }

                    return null;
                }
                catch
                {
                    return null;
                }
            }
        }
    }
}
