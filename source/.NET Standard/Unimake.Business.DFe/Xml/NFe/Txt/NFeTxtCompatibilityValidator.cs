using System;
using System.Collections.Generic;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Preserva as validações de compatibilidade existentes antes da serialização do modelo oficial.
    /// </summary>
    internal static class NFeTxtCompatibilityValidator
    {
        internal static void Validar(DFeNFe.NFe nfe, List<DFeNFe.Det> detalhes)
        {
            if (nfe.InfNFeField.Pag == null || nfe.InfNFeField.Pag.DetPag == null || nfe.InfNFeField.Pag.DetPag.Count == 0)
            {
                throw new InvalidOperationException("Falta definir valores do pagamento, tag <pag>.");
            }

            if (detalhes.Count > 990) throw new InvalidOperationException("Número máximo de itens excedeu o máximo permitido");
            foreach (var detalhe in detalhes)
            {
                ValidarProduto(detalhe.Prod);
                ValidarImpostos(detalhe.Imposto);
            }

            if (nfe.InfNFeField.InfAdic != null &&
                ((nfe.InfNFeField.InfAdic.ObsCont?.Count ?? 0) > 10 || (nfe.InfNFeField.InfAdic.ObsFisco?.Count ?? 0) > 10))
            {
                throw new InvalidOperationException("InfAdic: Excedeu o máximo permitido de observações");
            }
            if ((nfe.InfNFeField.Transp?.Reboque?.Count ?? 0) > 5)
            {
                throw new InvalidOperationException("Transp.reboque: Excedeu o máximo permitido de 5");
            }
        }

        private static void ValidarProduto(DFeNFe.Prod produto)
        {
            if ((produto.DI?.Count ?? 0) > 100 || (produto.DetExport?.Count ?? 0) > 500 ||
                (produto.Rastro?.Count ?? 0) > 500 || (produto.Arma?.Count ?? 0) > 500)
            {
                throw new InvalidOperationException("Número máximo de grupos do produto excedeu o máximo permitido");
            }

            foreach (var di in produto.DI ?? new List<DFeNFe.DI>())
            {
                if (di.Adi == null || di.Adi.Count == 0) throw new InvalidOperationException("Número minimo de itens DI->ADI não permitido");
                if (di.Adi.Count > 100) throw new InvalidOperationException("Número máximo de itens DI->ADI excedeu o máximo permitido");
            }
        }

        private static void ValidarImpostos(DFeNFe.Imposto imposto)
        {
            var pisOutros = imposto.PIS?.PISOutr;
            var pisSt = imposto.PISST;
            var cofinsOutros = imposto.COFINS?.COFINSOutr;
            var cofinsSt = imposto.COFINSST;
            var ipiTributado = imposto.IPI?.IPITrib;
            if ((pisOutros?.QBCProd ?? 0) > 0 && (pisOutros?.VAliqProd ?? 0) > 0 && (pisOutros?.VBC ?? 0) > 0 && (pisOutros?.PPIS ?? 0) > 0) throw new InvalidOperationException("PIS: As TAG's de cálculo não podem ser informadas em conjunto");
            if ((cofinsOutros?.QBCProd ?? 0) > 0 && (cofinsOutros?.VAliqProd ?? 0) > 0 && (cofinsOutros?.VBC ?? 0) > 0 && (cofinsOutros?.PCOFINS ?? 0) > 0) throw new InvalidOperationException("COFINS: As TAG's de cálculo não podem ser informadas em conjunto");
            if ((pisSt?.QBCProd ?? 0) > 0 && (pisSt?.VAliqProd ?? 0) > 0 && (pisSt?.VBC ?? 0) > 0 && (pisSt?.PPIS ?? 0) > 0) throw new InvalidOperationException("PISST: As TAG's de cálculo não podem ser informadas em conjunto");
            if ((cofinsSt?.QBCProd ?? 0) > 0 && (cofinsSt?.VAliqProd ?? 0) > 0 && (cofinsSt?.VBC ?? 0) > 0 && (cofinsSt?.PCOFINS ?? 0) > 0) throw new InvalidOperationException("COFINSST: As TAG's de cálculo não podem ser informadas em conjunto");
            if ((ipiTributado?.QUnid ?? 0) > 0 && (ipiTributado?.VUnid ?? 0) > 0 && (ipiTributado?.VBC ?? 0) > 0 && (ipiTributado?.PIPI ?? 0) > 0) throw new InvalidOperationException("IPITrib: As TAG's de cálculo não podem ser informadas em conjunto");
        }
    }
}
