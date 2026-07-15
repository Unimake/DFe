using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Consolida no modelo oficial os grupos lidos dos segmentos TXT.
    /// </summary>
    internal sealed class NFeTxtToNFeMapper
    {
        internal DFeNFe.Total CriarTotal() => new DFeNFe.Total
        {
            ICMSTot = new DFeNFe.ICMSTot(),
            ISSQNtot = new DFeNFe.ISSQNtot(),
            RetTrib = new DFeNFe.RetTrib(),
            ISTot = new DFeNFe.ISTot(),
            IBSCBSTot = new DFeNFe.IBSCBSTot
            {
                GIBS = new DFeNFe.GIBSTot
                {
                    GIBSUF = new DFeNFe.GIBSUFTot(),
                    GIBSMun = new DFeNFe.GIBSMunTot()
                },
                GCBS = new DFeNFe.GCBSTot(),
                GMono = new DFeNFe.GMono()
            }
        };

        internal void Mapear(DFeNFe.NFe destino, DFeNFe.Ide identificacao, DFeNFe.Dest destinatario,
            DFeNFe.Retirada retirada, DFeNFe.Entrega entrega, List<DFeNFe.AutXML> autorizadosXml,
            List<DFeNFe.Det> detalhes, DFeNFe.Total total, DFeNFe.Transp transporte)
        {
            destino.InfNFeField.Ide = identificacao;
            AplicarParticipantes(destino, identificacao, destinatario, retirada, entrega, autorizadosXml);
            destino.InfNFeField.Det = new NFeTxtOfficialProductFactory().Mapear(identificacao, detalhes);
            AplicarGruposComerciais(destino, detalhes, total, transporte);
        }

        private static void AplicarParticipantes(DFeNFe.NFe destino, DFeNFe.Ide identificacao,
            DFeNFe.Dest destinatario, DFeNFe.Retirada retirada, DFeNFe.Entrega entrega,
            List<DFeNFe.AutXML> autorizadosXml)
        {
            var deveGerarDestinatario = identificacao.Mod == ModeloDFe.NFe ||
                !string.IsNullOrWhiteSpace(destinatario.CNPJ) ||
                !string.IsNullOrWhiteSpace(destinatario.CPF) ||
                !string.IsNullOrWhiteSpace(destinatario.IdEstrangeiro) ||
                !string.IsNullOrWhiteSpace(destinatario.XNome);

            if (deveGerarDestinatario)
            {
                destinatario.IdEstrangeiro = destinatario.IdEstrangeiro == "NAO GERAR TAG" ? string.Empty : VazioParaNulo(destinatario.IdEstrangeiro);
                if (identificacao.TpAmb != TipoAmbiente.Producao)
                {
                    destinatario.XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL";
                }
                if (identificacao.Mod != ModeloDFe.NFe && string.IsNullOrWhiteSpace(destinatario.EnderDest.XLgr))
                {
                    destinatario.EnderDest = null;
                }
                destino.InfNFeField.Dest = destinatario;
            }

            destino.InfNFeField.Retirada = string.IsNullOrWhiteSpace(retirada.XLgr) ? null : retirada;
            destino.InfNFeField.Entrega = string.IsNullOrWhiteSpace(entrega.XLgr) ? null : entrega;
            destino.InfNFeField.AutXML = autorizadosXml.Count == 0 ? null : autorizadosXml;
        }

        private static void AplicarGruposComerciais(DFeNFe.NFe destino, List<DFeNFe.Det> detalhes,
            DFeNFe.Total total, DFeNFe.Transp transporte)
        {
            PrepararTotal(total, detalhes);
            destino.InfNFeField.Total = total;
            if (transporte.Transporta != null && !TemIdentificacaoTransportador(transporte.Transporta))
            {
                transporte.Transporta = null;
            }
            destino.InfNFeField.Transp = transporte;
        }

        private static void PrepararTotal(DFeNFe.Total total, List<DFeNFe.Det> detalhes)
        {
            var temIS = false;
            var temIBSCBS = false;
            foreach (var detalhe in detalhes)
            {
                temIS = temIS || !string.IsNullOrWhiteSpace(detalhe.Imposto?.IS?.CSTIS);
                temIBSCBS = temIBSCBS || !string.IsNullOrWhiteSpace(detalhe.Imposto?.IBSCBS?.CST) ||
                    !string.IsNullOrWhiteSpace(detalhe.Imposto?.IBSCBS?.CClassTrib);
            }

            if (!TemISSQN(total.ISSQNtot)) total.ISSQNtot = null;
            if (!TemRetencoes(total.RetTrib)) total.RetTrib = null;
            if (!temIS) total.ISTot = null;
            if (!temIBSCBS)
            {
                total.IBSCBSTot = null;
                return;
            }

            if (!TemMonofasia(total.IBSCBSTot.GMono)) total.IBSCBSTot.GMono = null;
            if (total.IBSCBSTot.GEstornoCred != null &&
                total.IBSCBSTot.GEstornoCred.VIBSEstCred + total.IBSCBSTot.GEstornoCred.VCBSEstCred <= 0)
            {
                total.IBSCBSTot.GEstornoCred = null;
            }
        }

        private static bool TemIdentificacaoTransportador(DFeNFe.Transporta transportador) =>
            !string.IsNullOrWhiteSpace(transportador.CNPJ) ||
            !string.IsNullOrWhiteSpace(transportador.CPF) ||
            !string.IsNullOrWhiteSpace(transportador.XNome);

        private static bool TemISSQN(DFeNFe.ISSQNtot total) =>
            total.VServ + total.VBC + total.VISS + total.VPIS + total.VCOFINS > 0;

        private static bool TemRetencoes(DFeNFe.RetTrib total) =>
            total.VRetPIS + total.VRetCOFINS + total.VRetCSLL + total.VBCIRRF + total.VIRRF + total.VBCRetPrev + total.VRetPrev > 0;

        private static bool TemMonofasia(DFeNFe.GMono total) =>
            total.VIBSMono + total.VCBSMono + total.VIBSMonoReten + total.VCBSMonoReten + total.VIBSMonoRet + total.VCBSMonoRet > 0;

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;
    }
}
