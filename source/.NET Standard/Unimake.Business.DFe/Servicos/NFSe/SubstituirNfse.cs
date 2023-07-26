#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Security;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Substituição de NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.SubstituirNfse")]
    [ComVisible(true)]
#endif
    public class SubstituirNfse : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public SubstituirNfse() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public SubstituirNfse(string conteudoXML, Configuracao configuracao) : this()
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(conteudoXML);

            Inicializar(xmlDoc, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public SubstituirNfse(XmlDocument conteudoXML, Configuracao configuracao) : this() => Inicializar(conteudoXML, configuracao);

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

            //Assinar a tag do pedido de cancelamento para posterior substituição
            if (!string.IsNullOrWhiteSpace(Configuracoes.TagExtraAssinatura) && !AssinaturaDigital.EstaAssinado(ConteudoXML, Configuracoes.TagExtraAssinatura))
            {
                AssinaturaDigital.Assinar(ConteudoXML, Configuracoes.TagExtraAssinatura, Configuracoes.TagExtraAtributoID, Configuracoes.CertificadoDigital, AlgorithmType.Sha1, true, "Id");
            }

            base.Executar();
        }
    }
}