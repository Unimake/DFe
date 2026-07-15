using System;
using System.Collections.Generic;
using System.Xml;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal sealed class NFeTxtXmlGenerator
    {
        internal NFeTxtDocumento Gerar(NFe nfe, bool cDvInformado)
        {
            ValidarCompatibilidade(nfe);
            var documento = new NFeTxtXmlSerializer().Serializar(nfe);
            var infNFe = (XmlElement)documento.SelectSingleNode("/*[local-name()='NFe']/*[local-name()='infNFe']");
            var chave = infNFe.GetAttribute("Id").Substring(3);
            var digitoVerificadorCalculado = int.Parse(chave.Substring(43, 1));

            if (cDvInformado && nfe.ide.cDV != digitoVerificadorCalculado)
            {
                throw new InvalidOperationException("Dígito verificador informado no TXT diverge da chave de acesso calculada.");
            }

            nfe.infNFe.ID = chave;
            nfe.ide.cNF = int.Parse(chave.Substring(35, 8));
            nfe.ide.cDV = digitoVerificadorCalculado;
            return new NFeTxtDocumento(documento.OuterXml, chave, nfe.ide.nNF, nfe.ide.serie);
        }

        private static void ValidarCompatibilidade(NFe nfe)
        {
            if (nfe.pag == null || nfe.pag.Count == 0) throw new InvalidOperationException("Falta definir valores do pagamento, tag <pag>.");
            if (nfe.det != null && nfe.det.Count > 990) throw new InvalidOperationException("Número máximo de itens excedeu o máximo permitido");
            if (nfe.Transp != null && nfe.Transp.Reboque != null && nfe.Transp.Reboque.Count > 5) throw new InvalidOperationException("Transp.reboque: Excedeu o máximo permitido de 5");
            if (nfe.InfAdic != null && (nfe.InfAdic.obsCont.Count > 10 || nfe.InfAdic.obsFisco.Count > 10)) throw new InvalidOperationException("InfAdic: Excedeu o máximo permitido de observações");

            foreach (var detalhe in nfe.det ?? new List<Det>())
            {
                if (detalhe.Prod.DI.Count > 100 || detalhe.Prod.detExport.Count > 500 || detalhe.Prod.rastro.Count > 500 || detalhe.Prod.arma.Count > 500 || detalhe.Prod.med.Count > 500) throw new InvalidOperationException("Número máximo de grupos do produto excedeu o máximo permitido");
                foreach (var di in detalhe.Prod.DI)
                {
                    if (di.adi == null || di.adi.Count == 0) throw new InvalidOperationException("Número minimo de itens DI->ADI não permitido");
                    if (di.adi.Count > 100) throw new InvalidOperationException("Número máximo de itens DI->ADI excedeu o máximo permitido");
                }
                if (detalhe.Imposto.PIS.qBCProd > 0 && detalhe.Imposto.PIS.vAliqProd > 0 && detalhe.Imposto.PIS.vBC > 0 && detalhe.Imposto.PIS.pPIS > 0) throw new InvalidOperationException("PIS: As TAG's de cálculo não podem ser informadas em conjunto");
                if (detalhe.Imposto.COFINS.qBCProd > 0 && detalhe.Imposto.COFINS.vAliqProd > 0 && detalhe.Imposto.COFINS.vBC > 0 && detalhe.Imposto.COFINS.pCOFINS > 0) throw new InvalidOperationException("COFINS: As TAG's de cálculo não podem ser informadas em conjunto");
                if (detalhe.Imposto.PISST.qBCProd > 0 && detalhe.Imposto.PISST.vAliqProd > 0 && detalhe.Imposto.PISST.vBC > 0 && detalhe.Imposto.PISST.pPis > 0) throw new InvalidOperationException("PISST: As TAG's de cálculo não podem ser informadas em conjunto");
                if (detalhe.Imposto.COFINSST.qBCProd > 0 && detalhe.Imposto.COFINSST.vAliqProd > 0 && detalhe.Imposto.COFINSST.vBC > 0 && detalhe.Imposto.COFINSST.pCOFINS > 0) throw new InvalidOperationException("COFINSST: As TAG's de cálculo não podem ser informadas em conjunto");
                if (detalhe.Imposto.IPI.qUnid > 0 && detalhe.Imposto.IPI.vUnid > 0 && detalhe.Imposto.IPI.vBC > 0 && detalhe.Imposto.IPI.pIPI > 0) throw new InvalidOperationException("IPITrib: As TAG's de cálculo não podem ser informadas em conjunto");
            }
        }
    }
}
