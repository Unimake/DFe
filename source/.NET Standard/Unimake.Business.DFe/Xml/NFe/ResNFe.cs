#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ResNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("resNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ResNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpNF")]
        public TipoOperacao TpNF { get; set; }

        [XmlIgnore]
        public double VNF { get; set; }
        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        [XmlElement("digVal")]
        public string DigVal { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("cSitNFe")]
        public string CSitNFe { get; set; }

        private string CSitConfField;

        /// <summary>
        /// Situação da Manifestação do Destinatário:
        /// 0=Sem Manifestação do Destinatário;
        /// 1=Confirmada Operação;
        /// 2=Desconhecida;
        /// 3=Operação não Realizada;
        /// 4=Ciência.
        /// Esta propriedade só terá conteúdo no retorno da consulta com o schema versão 1.35. Na versão 1.01 a SEFAZ não retorna esta tag.
        /// </summary>
        [XmlElement("cSitConf")]
        public string CSitConf
        {
            get => CSitConfField;
            set
            {
                if (Versao == "1.35")
                {
                    CSitConfField = value;
                }
                else
                {
                    CSitConfField = string.Empty;
                }
            }
        }

        #region ShouldSerialize

        public bool ShouldSerializeCSitConf() => !string.IsNullOrWhiteSpace(CSitConf);
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }
}