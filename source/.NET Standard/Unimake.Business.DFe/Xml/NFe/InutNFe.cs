#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de inutilização de números de notas fiscais (NFe/NFCe)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InutNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("inutNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class InutNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de inutilização
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Dados da inutilização
        /// </summary>
        [XmlElement("infInut")]
        public InutNFeInfInut InfInut { get; set; } = new InutNFeInfInut();

        /// <summary>
        /// Assinatura digital do XML
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Classe de inutilização de números de notas fiscais (NFe/NFCe)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InutNFeInfInut
    {
        private string IdField;
        private string AnoField;
        private string CPFField;

        /// <summary>
        /// Identificador da inutilização (ID)
        /// </summary>
        [XmlAttribute(DataType = "ID")]
        public string Id
        {
            get => "ID" + ((int)CUF).ToString() + Ano + (string.IsNullOrWhiteSpace(CPF) || CUF != UFBrasil.MT ? CNPJ : "000" + CPF) + ((int)Mod).ToString() + Serie.ToString("000") + NNFIni.ToString("000000000") + NNFFin.ToString("000000000");
            set => IdField = value;
        }

        /// <summary>
        /// Tipo do ambiente (Homologação ou Produção)
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Serviço
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "INUTILIZAR";

        /// <summary>
        /// UF do contribuinte
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Ano que está ocorrendo a inutilização da numeração das notas
        /// </summary>
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

        /// <summary>
        /// CNPJ do contribuinte, se pessoa jurídica.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do contribuinte, se pessoa física.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF
        {
            get
            {
                return CPFField;
            }
            set
            {
                CPFField = value;
            }
        }

        /// <summary>
        /// Modelo da nota referente os números que serão inutilizados
        /// </summary>
        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        /// <summary>
        /// Série da nota referente aos números que serão inutilizados
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Intervalo de número de notas a serem inutilizados - Número da nota inicial
        /// </summary>
        [XmlElement("nNFIni")]
        public int NNFIni { get; set; }

        /// <summary>
        /// Intervalo de número de notas a serem inutilizados - Número da nota final
        /// </summary>
        [XmlElement("nNFFin")]
        public int NNFFin { get; set; }

        /// <summary>
        /// Justificativa da inutilização
        /// </summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Método auxiliar para serialização/desserialização do XML
        /// </summary>
        /// <returns>Se gera ou não a tag no XML(true or false)</returns>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Método auxiliar para serialização/desserialização do XML
        /// </summary>
        /// <returns>Se gera ou não a tag no XML(true or false)</returns>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }
}
