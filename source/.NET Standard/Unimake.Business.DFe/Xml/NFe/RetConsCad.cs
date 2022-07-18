#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetConsCad")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsCad : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infCons")]
        public InfConsRetorno InfCons { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfConsRetorno")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfConsRetorno
    {
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlIgnore]
        public UFBrasil UF { get; set; }

        [XmlElement("UF")]
        public string UFField
        {
            get => UF.ToString();
            set => UF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value);
        }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhCons { get; set; }
#else
        public DateTimeOffset DhCons { get; set; }
#endif

        [XmlElement("dhCons")]
        public string DhConsField
        {
            get => DhCons.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhCons = DateTime.Parse(value);
#else
            set => DhCons = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("infCad")]
        public InfCad InfCad { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfCad")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfCad
    {
        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cSit")]
        public int CSit { get; set; }

        [XmlElement("indCredNFe")]
        public int IndCredNFe { get; set; }

        [XmlElement("indCredCTe")]
        public int IndCredCTe { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("xRegApur")]
        public string XRegApur { get; set; }

        [XmlElement("CNAE")]
        public string CNAE { get; set; }

        [XmlIgnore]
        public DateTime DIniAtiv { get; set; }

        [XmlElement("dIniAtiv")]
        public string DIniAtivField
        {
            get => DIniAtiv.ToString("yyyy-MM-dd");
            set => DIniAtiv = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DUltSit { get; set; }

        [XmlElement("dUltSit")]
        public string DUltSitField
        {
            get => DUltSit.ToString("yyyy-MM-dd");
            set => DUltSit = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DBaixa { get; set; }

        [XmlElement("dBaixa")]
        public string DBaixaField
        {
            get => DBaixa.ToString("yyyy-MM-dd");
            set => DBaixa = DateTime.Parse(value);
        }

        [XmlElement("IEUnica")]
        public string IEUnica { get; set; }

        [XmlElement("IEAtual")]
        public string IEAtual { get; set; }

        [XmlElement("ender")]
        public Ender Ender { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Ender")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Ender
    {
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }
    }
}