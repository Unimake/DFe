#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Interface COM para o servico de cancelamento da operacao de transporte.
    /// </summary>
    [ComVisible(true)]
    [Guid("D43D2505-8EBA-4EFD-925F-6B725CE72051")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface ICancelamentoOperacaoTransporteInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetCancelamentoOperacaoTransporte Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.CancelamentoOperacaoTransporte xml, Configuracao configuracao);

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetCancelamentoOperacaoTransporteProcResult();

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetCancelamentoOperacaoTransporteProcResults(string codigoIdentificacaoOperacao);

        /// <summary>
        /// Grava o XML de distribuicao.
        /// </summary>
        void GravarXmlDistribuicao(string pasta);
    }

    /// <summary>
    /// Interface COM para o servico de consulta do CIOT gerado.
    /// </summary>
    [ComVisible(true)]
    [Guid("AAF27505-A73F-4977-BB6A-5A49F2423387")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IConsultarCIOTGeradoInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetConsultarCIOTGerado Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.ConsultarCIOTGerado xml, Configuracao configuracao);
    }

    /// <summary>
    /// Interface COM para o servico de consulta de excecao.
    /// </summary>
    [ComVisible(true)]
    [Guid("54DC2EA5-6C0B-498A-94A6-A83F51474BA1")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IConsultarExcecaoInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetConsultarExcecao Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.ConsultarExcecao xml, Configuracao configuracao);
    }

    /// <summary>
    /// Interface COM para o servico de consulta de frota do transportador.
    /// </summary>
    [ComVisible(true)]
    [Guid("B6F102C6-029A-4AA4-90C3-C46D3C7A792B")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IConsultarFrotaTransportadorInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetConsultarFrotaTransportador Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.ConsultarFrotaTransportador xml, Configuracao configuracao);
    }

    /// <summary>
    /// Interface COM para o servico de consulta da situacao do transportador.
    /// </summary>
    [ComVisible(true)]
    [Guid("FA3F297B-058E-42AA-BF62-37E35B0EEFC5")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IConsultarSituacaoTransportadorInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetConsultarSituacaoTransportador Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.ConsultarSituacaoTransportador xml, Configuracao configuracao);
    }

    /// <summary>
    /// Interface COM para o servico de declaracao da operacao de transporte.
    /// </summary>
    [ComVisible(true)]
    [Guid("2DF38283-7FDE-4646-A11B-5D3FB6388AB6")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IDeclaracaoOperacaoTransporteInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetDeclaracaoOperacaoTransporte Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte xml, Configuracao configuracao);

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetDeclaracaoOperacaoTransporteProcResult();

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetDeclaracaoOperacaoTransporteProcResults(string idOperacaoTransporte);

        /// <summary>
        /// Grava o XML de distribuicao.
        /// </summary>
        void GravarXmlDistribuicao(string pasta);
    }

    /// <summary>
    /// Interface COM para o servico de encerramento da operacao de transporte.
    /// </summary>
    [ComVisible(true)]
    [Guid("F3EBDC9F-00F2-4617-B36E-2013EC13A3DF")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IEncerramentoOperacaoTransporteInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetEncerramentoOperacaoTransporte Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.EncerramentoOperacaoTransporte xml, Configuracao configuracao);

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetEncerramentoOperacaoTransporteProcResult();

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetEncerramentoOperacaoTransporteProcResults(string codigoIdentificacaoOperacao);

        /// <summary>
        /// Grava o XML de distribuicao.
        /// </summary>
        void GravarXmlDistribuicao(string pasta);
    }

    /// <summary>
    /// Interface COM para o servico de geracao do identificador da operacao de transporte.
    /// </summary>
    [ComVisible(true)]
    [Guid("F2728311-CF68-4AB5-B2F2-CEEC4E04C37B")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IGerarIdOperacaoTransporteInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetGerarIdOperacaoTransporte Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte xml, Configuracao configuracao);

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetGerarIdOperacaoTransporteProcResult();

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetGerarIdOperacaoTransporteProcResults(string idOperacaoTransporte);

        /// <summary>
        /// Grava o XML de distribuicao.
        /// </summary>
        void GravarXmlDistribuicao(string pasta);
    }

    /// <summary>
    /// Interface COM para o servico de retificacao da operacao de transporte.
    /// </summary>
    [ComVisible(true)]
    [Guid("0E1BCC52-EDC0-4F66-866C-BFFEB876F7B4")]
    [InterfaceType(ComInterfaceType.InterfaceIsDual)]
    public interface IRetificacaoOperacaoTransporteInterop
    {
        /// <summary>
        /// Resultado do servico.
        /// </summary>
        RetRetificacaoOperacaoTransporte Result { get; }

        /// <summary>
        /// Executa o servico.
        /// </summary>
        void Executar(Unimake.Business.DFe.Xml.CIOT.RetificacaoOperacaoTransporte xml, Configuracao configuracao);

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetRetificacaoOperacaoTransporteProcResult();

        /// <summary>
        /// Recupera o XML processado em formato string.
        /// </summary>
        string GetRetificacaoOperacaoTransporteProcResults(string codigoIdentificacaoOperacao);

        /// <summary>
        /// Grava o XML de distribuicao.
        /// </summary>
        void GravarXmlDistribuicao(string pasta);
    }
}
#endif
