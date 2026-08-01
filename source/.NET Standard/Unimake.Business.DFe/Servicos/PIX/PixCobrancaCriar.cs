#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.PIX;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.PIX
{
    /// <summary>
    /// Criar cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.PIX.PixCobrancaCriar")]
    [ComVisible(true)]
#endif
    public class PixCobrancaCriar : ServicoBase<Xml.PIX.PixCobrancaCriar>
    {
        /// <summary>
        /// Resultado do retorno da criação de cobrança PIX
        /// </summary>
        public retPIXCobrancaCriar Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retPIXCobrancaCriar>(RetornoWSXML)
            : new retPIXCobrancaCriar
            {
                Status = 999,
                Motivo = "Ocorreu um erro ao tentar obter o objeto no retorno da API",
                DLLVersao = Info.VersaoDLL
            };

        /// <inheritdoc />
        protected override Servico ServicoPIX => Servico.PIXCobrancaCriar;

        /// <inheritdoc />
        protected override string SchemaArquivoPIX => "PIXCobrancaCreateRequest_1_00.xsd";

        /// <inheritdoc />
        protected override void XmlValidarConteudo()
        {
            if (Envio.TipoCobranca != PixTipoCobranca.CobV)
            {
                return;
            }

            if (Envio.Calendario == null)
            {
                throw new ValidarXMLException("Se o conteudo da tag <TipoCobranca> for igual a 1 e obrigatorio informar o grupo de tag <Calendario>.");
            }

            if (Envio.Devedor == null)
            {
                throw new ValidarXMLException("Se o conteudo da tag <TipoCobranca> for igual a 1 e obrigatorio informar o grupo de tag <Devedor>.");
            }
        }

        /// <summary>
        /// Grava a imagem do QRCode retornada pela API e atualiza o caminho no XML de retorno.
        /// </summary>
        /// <param name="caminhoArquivo">Caminho completo do arquivo de imagem.</param>
        public void GravarQRCode(string caminhoArquivo)
        {
            if (string.IsNullOrWhiteSpace(caminhoArquivo))
            {
                throw new ArgumentException("O caminho do arquivo do QRCode deve ser informado.", nameof(caminhoArquivo));
            }

            var retorno = Result;

            if (Envio.GerarQRCode)
            {
                if (string.IsNullOrWhiteSpace(retorno.ImageQRCode))
                {
                    throw new InvalidOperationException("A API não retornou a imagem do QRCode para gravação.");
                }

                var conteudoBase64 = RemoverPrefixoDataUri(retorno.ImageQRCode);
                var arquivo = new FileInfo(caminhoArquivo);

                if (!arquivo.Directory.Exists)
                {
                    arquivo.Directory.Create();
                }

                File.WriteAllBytes(arquivo.FullName, Convert.FromBase64String(conteudoBase64));
            }

            retorno.ImageQRCode = caminhoArquivo;
            RetornoWSXML = retorno.GerarXML();
            RetornoWSString = RetornoWSXML.OuterXml;
        }

        private static string RemoverPrefixoDataUri(string conteudoBase64)
        {
            if (!conteudoBase64.StartsWith("data:", StringComparison.OrdinalIgnoreCase))
            {
                return conteudoBase64;
            }

            var separador = conteudoBase64.IndexOf(',');
            return separador >= 0 ? conteudoBase64.Substring(separador + 1) : conteudoBase64;
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar(Xml.PIX.PixCobrancaCriar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
    }
}
