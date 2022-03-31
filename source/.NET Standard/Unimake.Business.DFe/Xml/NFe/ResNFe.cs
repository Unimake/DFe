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

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlIgnore]
        public DateTime DhEmi { get; set; }

        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhEmi = DateTime.Parse(value);
        }

        [XmlElement("tpNF")]
        public TipoOperacao TpNF { get; set; }

        [XmlElement("vNF")]
        public string VNF { get; set; }

        [XmlElement("digVal")]
        public string DigVal { get; set; }

        [XmlIgnore]
        public DateTime DhRecbto { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhRecbto = DateTime.Parse(value);
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

        #endregion
    }
}