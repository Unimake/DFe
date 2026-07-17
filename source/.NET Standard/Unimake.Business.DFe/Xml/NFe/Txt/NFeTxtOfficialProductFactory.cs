using System.Collections.Generic;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal sealed class NFeTxtOfficialProductFactory
    {
        public List<DFeNFe.Det> Mapear(DFeNFe.Ide identificacao, List<DFeNFe.Det> detalhes)
        {
            if (detalhes == null || detalhes.Count == 0) return null;

            var compraGovernamental = identificacao.GCompraGov != null;
            foreach (var detalhe in detalhes)
            {
                CompletarDetalhe(compraGovernamental, detalhe);
            }

            return detalhes;
        }

        private static void CompletarDetalhe(bool compraGovernamental, DFeNFe.Det detalhe)
        {
            var imposto = detalhe.Imposto;
            if (imposto != null)
            {
                if (imposto.IPI != null && imposto.IPI.IPITrib == null && imposto.IPI.IPINT == null) imposto.IPI = null;
                if (imposto.II != null && imposto.II.VBC + imposto.II.VDespAdu + imposto.II.VII + imposto.II.VIOF <= 0) imposto.II = null;
                if (imposto.PIS != null && imposto.PIS.PISAliq == null && imposto.PIS.PISQtde == null && imposto.PIS.PISNT == null && imposto.PIS.PISOutr == null) imposto.PIS = null;
                if (!TemPISST(imposto.PISST)) imposto.PISST = null;
                if (imposto.COFINS != null && imposto.COFINS.COFINSAliq == null && imposto.COFINS.COFINSQtde == null && imposto.COFINS.COFINSNT == null && imposto.COFINS.COFINSOutr == null) imposto.COFINS = null;
                if (!TemCOFINSST(imposto.COFINSST)) imposto.COFINSST = null;
                if (!TemICMSUFDest(imposto.ICMSUFDest)) imposto.ICMSUFDest = null;
                if (imposto.IS != null && string.IsNullOrWhiteSpace(imposto.IS.CSTIS)) imposto.IS = null;
                imposto.IBSCBS = NormalizarIBSCBS(compraGovernamental, imposto.IBSCBS);
                detalhe.Imposto = TemImposto(imposto) ? imposto : null;
            }

            if (detalhe.ImpostoDevol != null && detalhe.ImpostoDevol.PDevol <= 0 && (detalhe.ImpostoDevol.IPI?.VIPIDevol ?? 0) <= 0)
            {
                detalhe.ImpostoDevol = null;
            }
        }

        private static DFeNFe.IBSCBS NormalizarIBSCBS(bool compraGovernamental, DFeNFe.IBSCBS imposto)
        {
            if (imposto == null || string.IsNullOrWhiteSpace(imposto.CST) && string.IsNullOrWhiteSpace(imposto.CClassTrib)) return null;

            imposto.GIBSCBS = NormalizarGIBSCBS(compraGovernamental, imposto);
            imposto.GIBSCBSMono = NormalizarGIBSCBSMono(imposto.GIBSCBSMono);
            if (imposto.GTransfCred != null && imposto.GTransfCred.VIBS + imposto.GTransfCred.VCBS <= 0) imposto.GTransfCred = null;
            imposto.GCredPresOper = NormalizarGCredPresOper(imposto.GCredPresOper);
            return imposto;
        }

        private static DFeNFe.GIBSCBS NormalizarGIBSCBS(bool compraGovernamental, DFeNFe.IBSCBS imposto)
        {
            var grupo = imposto.GIBSCBS;
            if (!TemDadosGIBSCBS(compraGovernamental, imposto)) return null;

            if (grupo == null) grupo = new DFeNFe.GIBSCBS();
            var ufOriginal = grupo.GIBSUF;
            var municipioOriginal = grupo.GIBSMun;
            var cbsOriginal = grupo.GCBS;
            grupo.GIBSUF = NormalizarGIBSUF(compraGovernamental, imposto.CST, ufOriginal) ?? new DFeNFe.GIBSUF
            {
                PIBSUF = ufOriginal?.PIBSUF ?? 0,
                VIBSUF = ufOriginal?.VIBSUF ?? 0
            };
            grupo.GIBSMun = NormalizarGIBSMun(compraGovernamental, imposto.CST, municipioOriginal) ?? new DFeNFe.GIBSMun
            {
                PIBSMun = municipioOriginal?.PIBSMun ?? 0,
                VIBSMun = municipioOriginal?.VIBSMun ?? 0
            };
            grupo.GCBS = NormalizarGCBS(compraGovernamental, imposto.CST, cbsOriginal) ?? new DFeNFe.GCBS
            {
                PCBS = cbsOriginal?.PCBS ?? 0,
                VCBS = cbsOriginal?.VCBS ?? 0
            };
            if (!TemDados(grupo.GTribRegular)) grupo.GTribRegular = null;
            if (!TemDados(grupo.GTribCompraGov)) grupo.GTribCompraGov = null;
            return grupo;
        }

        private static DFeNFe.GIBSUF NormalizarGIBSUF(bool compraGovernamental, string cst, DFeNFe.GIBSUF grupo)
        {
            var diferimento = NormalizarGDif(cst, grupo?.GDif);
            var devolucao = grupo?.GDevTrib != null && grupo.GDevTrib.VDevTrib > 0 ? grupo.GDevTrib : null;
            var reducao = NormalizarGRed(compraGovernamental, cst, grupo?.GRed);
            if ((grupo?.PIBSUF ?? 0) <= 0 && (grupo?.VIBSUF ?? 0) <= 0 && diferimento == null && devolucao == null && reducao == null) return null;

            if (grupo == null) grupo = new DFeNFe.GIBSUF();
            grupo.GDif = diferimento;
            grupo.GDevTrib = devolucao;
            grupo.GRed = reducao;
            return grupo;
        }

        private static DFeNFe.GIBSMun NormalizarGIBSMun(bool compraGovernamental, string cst, DFeNFe.GIBSMun grupo)
        {
            var diferimento = NormalizarGDif(cst, grupo?.GDif);
            var devolucao = grupo?.GDevTrib != null && grupo.GDevTrib.VDevTrib > 0 ? grupo.GDevTrib : null;
            var reducao = NormalizarGRed(compraGovernamental, cst, grupo?.GRed);
            if ((grupo?.PIBSMun ?? 0) <= 0 && (grupo?.VIBSMun ?? 0) <= 0 && diferimento == null && devolucao == null && reducao == null) return null;

            if (grupo == null) grupo = new DFeNFe.GIBSMun();
            grupo.GDif = diferimento;
            grupo.GDevTrib = devolucao;
            grupo.GRed = reducao;
            return grupo;
        }

        private static DFeNFe.GCBS NormalizarGCBS(bool compraGovernamental, string cst, DFeNFe.GCBS grupo)
        {
            var diferimento = NormalizarGDif(cst, grupo?.GDif);
            var devolucao = grupo?.GDevTrib != null && grupo.GDevTrib.VDevTrib > 0 ? grupo.GDevTrib : null;
            var reducao = NormalizarGRed(compraGovernamental, cst, grupo?.GRed);
            if ((grupo?.PCBS ?? 0) <= 0 && (grupo?.VCBS ?? 0) <= 0 && diferimento == null && devolucao == null && reducao == null) return null;

            if (grupo == null) grupo = new DFeNFe.GCBS();
            grupo.GDif = diferimento;
            grupo.GDevTrib = devolucao;
            grupo.GRed = reducao;
            return grupo;
        }

        private static DFeNFe.GDif NormalizarGDif(string cst, DFeNFe.GDif grupo)
        {
            return (grupo?.PDif ?? 0) > 0 || (grupo?.VDif ?? 0) > 0 || cst == "510"
                ? grupo ?? new DFeNFe.GDif()
                : null;
        }

        private static DFeNFe.GRed NormalizarGRed(bool compraGovernamental, string cst, DFeNFe.GRed grupo)
        {
            var permiteCompraGovernamental = compraGovernamental && cst != "510";
            return cst == "011" || cst == "200" || cst == "515" || permiteCompraGovernamental
                ? grupo ?? new DFeNFe.GRed()
                : null;
        }

        private static DFeNFe.GIBSCBSMono NormalizarGIBSCBSMono(DFeNFe.GIBSCBSMono grupo)
        {
            if (grupo == null) return null;
            grupo.GMonoPadrao = TemDados(grupo.GMonoPadrao) ? grupo.GMonoPadrao : null;
            grupo.GMonoReten = TemDados(grupo.GMonoReten) ? grupo.GMonoReten : null;
            grupo.GMonoRet = TemDados(grupo.GMonoRet) ? grupo.GMonoRet : null;
            grupo.GMonoDif = TemDados(grupo.GMonoDif) ? grupo.GMonoDif : null;
            return grupo.VTotIBSMonoItem <= 0 && grupo.VTotCBSMonoItem <= 0 && grupo.GMonoRet == null ? null : grupo;
        }

        private static DFeNFe.GCredPresOper NormalizarGCredPresOper(DFeNFe.GCredPresOper grupo)
        {
            if (grupo == null || grupo.VBCCredPres <= 0) return null;
            if (grupo.GIBSCredPres != null && grupo.GIBSCredPres.VCredPres + grupo.GIBSCredPres.VCredPresCondSus <= 0) grupo.GIBSCredPres = null;
            if (grupo.GCBSCredPres != null && grupo.GCBSCredPres.VCredPres + grupo.GCBSCredPres.VCredPresCondSus <= 0) grupo.GCBSCredPres = null;
            return grupo;
        }

        private static bool TemDadosGIBSCBS(bool compraGovernamental, DFeNFe.IBSCBS imposto)
        {
            var grupo = imposto.GIBSCBS;
            var permiteCompraGovernamental = compraGovernamental && imposto.CST != "510";
            var permiteReducao = imposto.CST == "011" || imposto.CST == "200" || imposto.CST == "515" || permiteCompraGovernamental;
            var reducao = permiteReducao && SomaReducao(grupo?.GIBSUF?.GRed) + SomaReducao(grupo?.GIBSMun?.GRed) + SomaReducao(grupo?.GCBS?.GRed) > 0;
            return (grupo?.VBC ?? 0) > 0 || (grupo?.VIBS ?? 0) > 0 || imposto.CST == "510" || reducao ||
                Soma(grupo?.GIBSUF) > 0 || Soma(grupo?.GIBSMun) > 0 || Soma(grupo?.GCBS) > 0 ||
                TemDados(grupo?.GTribRegular) || TemDados(grupo?.GTribCompraGov);
        }

        private static double Soma(DFeNFe.GIBSUF grupo) => grupo == null ? 0 : grupo.PIBSUF + grupo.VIBSUF + (grupo.GDif?.PDif ?? 0) + (grupo.GDif?.VDif ?? 0) + (grupo.GDevTrib?.VDevTrib ?? 0);
        private static double Soma(DFeNFe.GIBSMun grupo) => grupo == null ? 0 : grupo.PIBSMun + grupo.VIBSMun + (grupo.GDif?.PDif ?? 0) + (grupo.GDif?.VDif ?? 0) + (grupo.GDevTrib?.VDevTrib ?? 0);
        private static double Soma(DFeNFe.GCBS grupo) => grupo == null ? 0 : grupo.PCBS + grupo.VCBS + (grupo.GDif?.PDif ?? 0) + (grupo.GDif?.VDif ?? 0) + (grupo.GDevTrib?.VDevTrib ?? 0);
        private static double SomaReducao(DFeNFe.GRed grupo) => grupo == null ? 0 : grupo.PRedAliq + grupo.PAliqEfet;

        private static bool TemDados(DFeNFe.GTribRegular grupo) => grupo != null &&
            (!string.IsNullOrWhiteSpace(grupo.CSTReg) || !string.IsNullOrWhiteSpace(grupo.CClassTribReg) ||
             grupo.PAliqEfetRegIBSUF + grupo.VTribRegIBSUF + grupo.PAliqEfetRegIBSMun + grupo.VTribRegIBSMun + grupo.PAliqEfetRegCBS + grupo.VTribRegCBS > 0);

        private static bool TemDados(DFeNFe.GTribCompraGov grupo) => grupo != null &&
            grupo.PAliqIBSUF + grupo.VTribIBSUF + grupo.PAliqIBSMun + grupo.VTribIBSMun + grupo.PAliqCBS + grupo.VTribCBS > 0;

        private static bool TemDados(DFeNFe.GMonoPadrao grupo) => grupo != null && grupo.QBCMono + grupo.AdRemIBS + grupo.AdRemCBS + grupo.VIBSMono + grupo.VCBSMono > 0;
        private static bool TemDados(DFeNFe.GMonoReten grupo) => grupo != null && grupo.QBCMonoReten + grupo.AdRemIBSReten + grupo.VIBSMonoReten + grupo.AdRemCBSReten + grupo.VCBSMonoReten > 0;
        private static bool TemDados(DFeNFe.GMonoRet grupo) => grupo != null && grupo.QBCMonoRet + grupo.AdRemIBSRet + grupo.VIBSMonoRet + grupo.AdRemCBSRet + grupo.VCBSMonoRet > 0;
        private static bool TemDados(DFeNFe.GMonoDif grupo) => grupo != null && grupo.PDifIBS + grupo.VIBSMonoDif + grupo.PDifCBS + grupo.VCBSMonoDif > 0;

        private static bool TemImposto(DFeNFe.Imposto imposto) => imposto.VTotTrib > 0 || imposto.ICMS != null || imposto.IPI != null ||
            imposto.II != null || imposto.PIS != null || imposto.PISST != null || imposto.COFINS != null || imposto.COFINSST != null ||
            imposto.ISSQN != null || imposto.ICMSUFDest != null || imposto.IS != null || imposto.IBSCBS != null;

        private static bool TemPISST(DFeNFe.PISST imposto) => imposto != null &&
            (imposto.VBC ?? 0) + (imposto.PPIS ?? 0) + (imposto.QBCProd ?? 0) + (imposto.VAliqProd ?? 0) + (imposto.VPIS ?? 0) > 0;

        private static bool TemCOFINSST(DFeNFe.COFINSST imposto) => imposto != null &&
            (imposto.VBC ?? 0) + (imposto.PCOFINS ?? 0) + (imposto.QBCProd ?? 0) + (imposto.VAliqProd ?? 0) + (imposto.VCOFINS ?? 0) > 0;

        private static bool TemICMSUFDest(DFeNFe.ICMSUFDest imposto) => imposto != null &&
            imposto.VBCUFDest + imposto.VBCFCPUFDest + imposto.PFCPUFDest + imposto.PICMSUFDest + imposto.PICMSInter +
            imposto.PICMSInterPart + imposto.VFCPUFDest + imposto.VICMSUFDest + imposto.VICMSUFRemet > 0;
    }
}
