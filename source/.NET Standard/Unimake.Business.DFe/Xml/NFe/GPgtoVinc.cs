using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Informações da vinculação da NF-e com transações de pagamento (TPgtoVinc).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPgtoVinc")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPgtoVinc
    {
        /// <summary>
        /// Pagamentos vinculados, de uma a 99 ocorrências.
        /// </summary>
        [XmlElement("gPgto", Order = 0)]
        public List<GPgto> GPgto { get; set; }

#if INTEROP
        /// <summary>
        /// Adiciona um pagamento à lista.
        /// </summary>
        public void AddGPgto(GPgto pagamento)
        {
            if (GPgto == null)
            {
                GPgto = new List<GPgto>();
            }
            GPgto.Add(pagamento);
        }

        /// <summary>
        /// Obtém um pagamento pelo índice.
        /// </summary>
        public GPgto GetGPgto(int index) => GPgto == null || GPgto.Count == 0 ? null : GPgto[index];

        /// <summary>
        /// Quantidade de pagamentos vinculados.
        /// </summary>
        public int GetGPgtoCount => GPgto == null ? 0 : GPgto.Count;
#endif
    }

    /// <summary>
    /// Dados comuns da transação de pagamento. O código do meio é textual, conforme o pattern do XSD.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EventoPgto")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class EventoPgto
    {
        /// <summary>
        /// Código do meio de pagamento com duas posições, conforme IT DFe 2026.001.
        /// </summary>
        [XmlElement("tpMeioPgto", Order = 0)]
        public string TpMeioPgto { get; set; }

        /// <summary>
        /// CNPJ do recebedor do pagamento, preservando zeros não significativos.
        /// </summary>
        [XmlElement("CNPJReceb", Order = 1)]
        public string CNPJReceb { get; set; }

        /// <summary>
        /// CNPJ base da instituição financeira ou de pagamento, com oito posições.
        /// </summary>
        [XmlElement("CNPJBasePSP", Order = 2)]
        public string CNPJBasePSP { get; set; }

        /// <summary>
        /// Identificador da transação financeira.
        /// </summary>
        [XmlAttribute("idTransacao")]
        public string IdTransacao { get; set; }
    }

    /// <summary>
    /// Pagamento vinculado na NF-e, incluindo o número sequencial exigido por TPgtoVinc.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPgto")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPgto : EventoPgto
    {
        /// <summary>
        /// Número sequencial do pagamento, de 1 a 990.
        /// </summary>
        [XmlAttribute("nPag")]
        public int NPag { get; set; }
    }

    /// <summary>
    /// ICMS previsto no momento do fornecimento futuro em operação com pagamento antecipado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSPrevistoPagtoAntecip")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSPrevistoPagtoAntecip
    {
        /// <summary>
        /// Valor do ICMS que incidirá no momento do fornecimento futuro.
        /// </summary>
        [XmlIgnore]
        public double VICMSPrevisto { get; set; }

        /// <summary>
        /// Representação XML do valor do ICMS previsto.
        /// </summary>
        [XmlElement("vICMSPrevisto", Order = 0)]
        public string VICMSPrevistoField
        {
            get => VICMSPrevisto.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSPrevisto = Converter.ToDouble(value);
        }
    }
}
