#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>Obtém o PDF de uma operação de transporte na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ObterOperacaoTransportePdf")]
    [ComVisible(true)]
#endif
    public class ObterOperacaoTransportePdf : ServicoBase, IInteropService<Xml.CIOT.ObterOperacaoTransportePdf>
    {
        private Xml.CIOT.ObterOperacaoTransportePdf envio;

        /// <summary>Objeto do XML de envio.</summary>
        public Xml.CIOT.ObterOperacaoTransportePdf Envio => ObterEnvio(ref envio);

        /// <summary>Resultado do serviço.</summary>
        public RetObterOperacaoTransportePdf Result => ObterResult<RetObterOperacaoTransportePdf>();

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTObterOperacaoTransportePdf;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetObterOperacaoTransportePdf);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

        /// <summary>Construtor.</summary>
        public ObterOperacaoTransportePdf() : base() { }

        /// <summary>Construtor.</summary>
        public ObterOperacaoTransportePdf(Xml.CIOT.ObterOperacaoTransportePdf xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>Construtor.</summary>
        public ObterOperacaoTransportePdf(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

        /// <summary>Grava o PDF retornado pela eFrete.</summary>
        /// <param name="pasta">Pasta de destino.</param>
        /// <param name="nomeArquivo">Nome do arquivo PDF.</param>
        public void GravarPDF(string pasta, string nomeArquivo)
        {
            try
            {
                if (string.IsNullOrWhiteSpace(Result.Pdf))
                {
                    throw new Exception("A eFrete não retornou o PDF da operação de transporte.");
                }

                Convert.FromBase64String(Result.Pdf);
                Converter.Base64ToPDF(Result.Pdf, Path.Combine(pasta, nomeArquivo));
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#if INTEROP
        /// <summary>Executa o serviço via COM.</summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.ObterOperacaoTransportePdf xml, Configuracao configuracao) { InicializarServico(xml, configuracao); Executar(); }
#endif
    }
}
