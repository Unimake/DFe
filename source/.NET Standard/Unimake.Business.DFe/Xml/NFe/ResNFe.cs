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
    /// <summary>
    /// Classe de informações resumidas da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ResNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("resNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ResNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de resumo da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Chave de acesso da NFe/NFCe
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// CNPJ do emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Razão social ou nome do emitente
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Inscrição estadual do emitente
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Data e hora de emissão do documento fiscal no formato AAAA-MM-DDThh:mm:ssTZD
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhEmi para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Tipo do documento fiscal
        /// </summary>
        [XmlElement("tpNF")]
        public TipoOperacao TpNF { get; set; }

        /// <summary>
        /// Valor total da NFe/NFCe
        /// </summary>
        [XmlIgnore]
        public double VNF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VNF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Digest Value da NFe/NFCe processada. Utilizado para conferir a integridade da NFe/NFCe original
        /// </summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }

        /// <summary>
        /// Data e hora da autorização da NFe/NFCe no formato AAAA-MM-DDThh:mm:ssTZD
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhRecbto para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Número do protocolo de status da NFe/NFCe
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }

        /// <summary>
        /// Situação da NFe/NFCe.
        /// 1 - Uso autorizado no momento da consulta; 2 - Uso denegado
        /// </summary>
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