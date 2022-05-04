#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InutNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("inutNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class InutNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infInut")]
        public InutNFeInfInut InfInut { get; set; } = new InutNFeInfInut();

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")]
    [ComVisible(true)]
#endif
    public class InutNFeInfInut
    {
        private string IdField;
        private string AnoField;

        [XmlAttribute(DataType = "ID")]
        public string Id
        {
            get => "ID" + ((int)CUF).ToString() + Ano + CNPJ + ((int)Mod).ToString() + Serie.ToString("000") + NNFIni.ToString("000000000") + NNFFin.ToString("000000000");
            set => IdField = value;
        }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }
        [XmlElement("xServ")]
        public string XServ { get; set; } = "INUTILIZAR";

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("ano")]
        public string Ano
        {
            get => AnoField;
            set
            {
                AnoField = value;

                if (AnoField.Length > 2)
                {
                    throw new Exception("TAG ano deve ter somente os dois últimos algarismos, exemplo: Para o ano de 2019 informe somente 19, ou para 2001 informe 01.");
                }
                else if (AnoField.Length < 2)
                {
                    throw new Exception("TAG ano deve ter dois 2 algarismos. Exemplo: Para o ano 2001 informe 01, ou para 2019 informe 19.");
                }
            }
        }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nNFIni")]
        public int NNFIni { get; set; }

        [XmlElement("nNFFin")]
        public int NNFFin { get; set; }

        [XmlElement("xJust")]
        public string XJust { get; set; }
    }
}
