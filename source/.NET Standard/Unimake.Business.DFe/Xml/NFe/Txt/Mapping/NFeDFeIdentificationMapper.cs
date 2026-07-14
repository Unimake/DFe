using System;
using System.Globalization;
using System.Collections.Generic;
using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;
using DFeService = Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeIdentificationMapper
    {
        public DFeNFe.Ide Mapear(Ide origem)
        {
            var destino = new DFeNFe.Ide
            {
                CUF = (DFeService.UFBrasil)origem.cUF,
                CNF = origem.cNF > 0 ? origem.cNF.ToString("00000000") : null,
                NatOp = origem.natOp,
                Mod = (DFeService.ModeloDFe)(int)origem.mod,
                Serie = origem.serie,
                NNF = origem.nNF,
                DhEmi = DateTimeOffset.Parse(origem.dhEmi, CultureInfo.InvariantCulture),
                TpNF = (DFeService.TipoOperacao)(int)origem.tpNF,
                IdDest = origem.mod == TpcnMod.modNFCe
                    ? DFeService.DestinoOperacao.OperacaoInterna
                    : (DFeService.DestinoOperacao)(int)origem.idDest,
                CMunFG = origem.cMunFG,
                CMunFGIBS = origem.cMunFGIBS,
                TpImp = (DFeService.FormatoImpressaoDANFE)(int)origem.tpImp,
                TpEmis = origem.tpEmis,
                CDV = origem.cDV,
                TpAmb = origem.tpAmb,
                FinNFe = (DFeService.FinalidadeNFe)(int)origem.finNFe,
                IndFinal = (DFeService.SimNao)(int)origem.indFinal,
                IndPres = (DFeService.IndicadorPresenca)(int)origem.indPres,
                ProcEmi = (DFeService.ProcessoEmissao)(int)origem.procEmi,
                VerProc = origem.verProc,
                XJust = VazioParaNulo(origem.xJust)
            };

            if (origem.mod == TpcnMod.modNFe && !string.IsNullOrWhiteSpace(origem.dhSaiEnt))
            {
                destino.DhSaiEnt = DateTimeOffset.Parse(origem.dhSaiEnt, CultureInfo.InvariantCulture);
            }

            if (origem.mod == TpcnMod.modNFe && origem.dPrevEntrega > DateTime.MinValue)
            {
                destino.DPrevEntrega = origem.dPrevEntrega;
            }

            if (!string.IsNullOrWhiteSpace(origem.dhCont))
            {
                destino.DhCont = DateTimeOffset.Parse(origem.dhCont, CultureInfo.InvariantCulture);
            }

            if (Enum.IsDefined(typeof(TpcnTipoNFDebito), origem.tpNFDebito))
            {
                destino.TpNFDebito = (DFeService.TipoNFDebito)(int)origem.tpNFDebito;
            }

            if (Enum.IsDefined(typeof(TpcnTipoNFCredito), origem.tpNFCredito))
            {
                destino.TpNFCredito = (DFeService.TipoNFCredito)(int)origem.tpNFCredito;
            }

            if (origem.indIntermed.HasValue && origem.indIntermed.Value != TpcnIntermediario.NaoInserirTagNoXML &&
                (origem.indPres == TpcnPresencaComprador.pcPresencial || origem.indPres == TpcnPresencaComprador.pcInternet ||
                 origem.indPres == TpcnPresencaComprador.pcTeleatendimento || origem.indPres == TpcnPresencaComprador.pcEntregaDomicilio ||
                 origem.indPres == TpcnPresencaComprador.pcOutros))
            {
                destino.IndIntermed = (DFeService.IndicadorIntermediario)(int)origem.indIntermed.Value;
            }

            destino.NFref = MapearReferencias(origem.NFref);
            destino.GCompraGov = MapearCompraGovernamental(origem.gCompraGov);
            destino.GPagAntecipado = MapearPagamentoAntecipado(origem.gPagAntecipado);

            return destino;
        }

        private static List<DFeNFe.NFref> MapearReferencias(List<NFref> referencias)
        {
            if (referencias == null || referencias.Count == 0)
            {
                return null;
            }

            var resultado = new List<DFeNFe.NFref>();

            resultado.AddRange(referencias
                .Where(x => !string.IsNullOrWhiteSpace(x.refNFe))
                .Select(x => new DFeNFe.NFref { RefNFe = x.refNFe }));

            resultado.AddRange(referencias
                .Where(x => x.refNF != null && x.refNF.nNF > 0)
                .Select(x => new DFeNFe.NFref
                {
                    RefNF = new DFeNFe.RefNF
                    {
                        CUF = (DFeService.UFBrasil)x.refNF.cUF,
                        AAMM = x.refNF.AAMM,
                        CNPJ = x.refNF.CNPJ,
                        Mod = x.refNF.mod,
                        Serie = x.refNF.serie,
                        NNF = x.refNF.nNF
                    }
                }));

            resultado.AddRange(referencias
                .Where(x => x.refNFP != null && x.refNFP.nNF > 0)
                .Select(x => new DFeNFe.NFref
                {
                    RefNFP = new DFeNFe.RefNFP
                    {
                        CUF = (DFeService.UFBrasil)x.refNFP.cUF,
                        AAMM = x.refNFP.AAMM,
                        CNPJ = VazioParaNulo(x.refNFP.CNPJ),
                        CPF = VazioParaNulo(x.refNFP.CPF),
                        IE = VazioParaNulo(x.refNFP.IE),
                        Mod = x.refNFP.mod,
                        Serie = x.refNFP.serie,
                        NNF = x.refNFP.nNF
                    }
                }));

            resultado.AddRange(referencias
                .Where(x => !string.IsNullOrWhiteSpace(x.refCTe) && !x.refCTe.StartsWith("00", StringComparison.Ordinal))
                .Select(x => new DFeNFe.NFref { RefCTe = x.refCTe }));

            resultado.AddRange(referencias
                .Where(x => x.refECF != null && x.refECF.nCOO > 0)
                .Select(x => new DFeNFe.NFref
                {
                    RefECF = new DFeNFe.RefECF
                    {
                        Mod = x.refECF.mod,
                        NECF = x.refECF.nECF,
                        NCOO = x.refECF.nCOO
                    }
                }));

            return resultado.Count == 0 ? null : resultado;
        }

        private static DFeNFe.GCompraGov MapearCompraGovernamental(GCompraGov origem)
        {
            if (origem == null || !Enum.IsDefined(typeof(TpcnTipoEnteGovernamental), origem.tpEnteGov))
            {
                return null;
            }

            return new DFeNFe.GCompraGov
            {
                TpEnteGov = (DFeService.TipoEnteGovernamental)(int)origem.tpEnteGov,
                PRedutor = origem.pRedutor,
                TpOperGov = (DFeService.TipoOperacaoEnteGovernamental)(int)origem.tpOperGov
            };
        }

        private static DFeNFe.GPagAntecipado MapearPagamentoAntecipado(GPagAntecipado origem)
        {
            if (origem?.refNFe == null || origem.refNFe.Count == 0)
            {
                return null;
            }

            return new DFeNFe.GPagAntecipado { RefDFe = new List<string>(origem.refNFe) };
        }

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;
    }
}
