﻿#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o xml para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.DownloadPorNrRec")]
    [ComVisible(true)]
#endif
    public class DownloadPorNrRec : ServicoBase, IInteropService<DownloadEventosPorNrRec>
    {
        /// <summary>
        /// 
        /// </summary>
        public DownloadPorNrRec(DownloadEventosPorNrRec consulta, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
        }

#if INTEROP
        /// <summary>
        /// 
        /// </summary>
        /// <param name="downloadEventosPorNrRec"></param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] DownloadEventosPorNrRec downloadEventosPorNrRec, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(downloadEventosPorNrRec?.GerarXML() ?? throw new ArgumentNullException(nameof(downloadEventosPorNrRec)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }
#endif

        /// <summary>
        /// 
        /// </summary>
        /// <param name="pasta"></param>
        /// <param name="nomeArquivo"></param>
        /// <param name="conteudoXML"></param>
        /// <exception cref="NotImplementedException"></exception>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            //throw new NotImplementedException();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void DefinirConfiguracao()
        {
            var xml = new DownloadEventosPorNrRec();
            xml = xml.LerXML<DownloadEventosPorNrRec>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialConsultaEvts;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

    }
}
