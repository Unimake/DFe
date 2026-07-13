using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.Validar
{
    /// <summary>
    /// Mantém as raízes que precisam de round-trip pelo modelo tipado antes do envio.
    /// </summary>
    internal static class RegistroNormalizacaoXML
    {
        private static readonly IReadOnlyDictionary<TipoDFe, HashSet<string>> Raizes =
            new Dictionary<TipoDFe, HashSet<string>>
            {
                [TipoDFe.MDFe] = CriarConjunto(
                    "MDFe", "enviMDFe", "eventoMDFe", "consStatServMDFe",
                    "consSitMDFe", "consReciMDFe", "consMDFeNaoEnc"),
                [TipoDFe.CTe] = CriarConjunto(
                    "CTe", "enviCTe", "CTeSimp", "CTeOS", "eventoCTe",
                    "consStatServCte", "consStatServCTe", "consSitCTe",
                    "consReciCTe", "distDFeInt"),
                [TipoDFe.NFe] = CriarConjunto(
                    "NFe", "enviNFe", "envEvento", "inutNFe", "consStatServ",
                    "consSitNFe", "consReciNFe", "ConsCad", "nfceDownloadXML",
                    "nfceListagemChaves", "distDFeInt"),
                [TipoDFe.NFCe] = CriarConjunto(
                    "NFe", "enviNFe", "envEvento", "inutNFe", "consStatServ",
                    "consSitNFe", "consReciNFe", "ConsCad", "nfceDownloadXML",
                    "nfceListagemChaves", "distDFeInt"),
                [TipoDFe.NFCom] = CriarConjunto(
                    "NFCom", "eventoNFCom", "consStatServNFCom", "consSitNFCom"),
                [TipoDFe.NFGas] = CriarConjunto(
                    "NFGas", "eventoNFGas", "consStatServNFGas", "consSitNFGas"),
                [TipoDFe.BPe] = CriarConjunto(
                    "BPe", "BPeTM", "BPeTA", "eventoBPe", "consStatServBPe", "consSitBPe"),
                [TipoDFe.NF3e] = CriarConjunto(
                    "NF3e", "eventoNF3e", "consStatServNF3e", "consSitNF3e", "consReciNF3e"),
                [TipoDFe.DCe] = CriarConjunto(
                    "DCe", "eventoDCe", "consStatServDCe", "consSitDCe"),
                [TipoDFe.CCG] = CriarConjunto("consGTIN"),
                [TipoDFe.CIOT] = CriarConjunto(
                    "ConsultarSituacaoTransportador", "ConsultarFrotaTransportador",
                    "DeclaracaoOperacaoTransporte", "CancelamentoOperacaoTransporte",
                    "RetificacaoOperacaoTransporte", "EncerramentoOperacaoTransporte",
                    "ConsultarExcecao", "ConsultarCIOTGerado", "GerarIdOperacaoTransporte"),
                [TipoDFe.GNRE] = CriarConjunto(
                    "TConsultaConfigUf", "TConsLote_GNRE", "TLote_GNRE", "TLote_ConsultaGNRE"),
                [TipoDFe.DARE] = CriarConjunto("Dare", "DareLote", "Receitas"),
                [TipoDFe.EFDReinf] = CriarConjunto("Reinf"),
                [TipoDFe.ESocial] = CriarConjunto("eSocial")
            };

        internal static bool DeveNormalizar(TipoDFe tipoDFe, string tagRaiz)
        {
            return !string.IsNullOrEmpty(tagRaiz) &&
                Raizes.TryGetValue(tipoDFe, out var raizes) &&
                raizes.Contains(tagRaiz);
        }

        private static HashSet<string> CriarConjunto(params string[] raizes)
        {
            return new HashSet<string>(raizes, StringComparer.Ordinal);
        }
    }
}
