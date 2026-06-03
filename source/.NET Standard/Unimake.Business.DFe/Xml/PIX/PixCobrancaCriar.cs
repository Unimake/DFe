using System;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// XML de criação de cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixCobrancaCriar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PIXCobrancaCreateRequest", IsNullable = false)]
    public class PixCobrancaCriar : XMLBase
    {
        /// <summary>
        /// Identificador da configuração
        /// </summary>
        [XmlElement("ConfigurationId", Order = 0)]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Indicador de teste
        /// </summary>
        [XmlElement("Testing", Order = 7)]
        public bool Testing { get; set; }

        /// <summary>
        /// Indica se Testing deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TestingSpecified { get; set; }

        /// <summary>
        /// Beneficiário
        /// </summary>
        [XmlElement("Beneficiario", Order = 8)]
        public PixBeneficiario Beneficiario { get; set; }

        /// <summary>
        /// Gerar QRCode
        /// </summary>
        [XmlElement("GerarQRCode", Order = 6)]
        public bool GerarQRCode { get; set; }

        /// <summary>
        /// Indica se GerarQRCode deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool GerarQRCodeSpecified { get; set; }

        /// <summary>
        /// Configuração do QRCode
        /// </summary>
        [XmlElement("QRCodeConfig", Order = 10)]
        public PixQRCodeConfig QRCodeConfig { get; set; }

        /// <summary>
        /// Calendário da cobrança
        /// </summary>
        [XmlElement("Calendario", Order = 9)]
        public PixCalendario Calendario { get; set; }

        /// <summary>
        /// Chave DICT do recebedor
        /// </summary>
        [XmlElement("Chave", Order = 4)]
        public string Chave { get; set; }

        /// <summary>
        /// Devedor
        /// </summary>
        [XmlElement("Devedor", Order = 11)]
        public PixDevedor Devedor { get; set; }

        /// <summary>
        /// Solicitação ao pagador
        /// </summary>
        [XmlElement("SolicitacaoPagador", Order = 1)]
        public string SolicitacaoPagador { get; set; }

        /// <summary>
        /// Tipo de cobrança
        /// </summary>
        [XmlElement("TipoCobranca", Order = 2)]
        public PixTipoCobranca TipoCobranca { get; set; }

        /// <summary>
        /// Indica se TipoCobranca deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TipoCobrancaSpecified { get; set; }

        /// <summary>
        /// Identificador da transação
        /// </summary>
        [XmlElement("TxId", Order = 5)]
        public string TxId { get; set; }

        /// <summary>
        /// Valor da cobrança
        /// </summary>
        [XmlElement("Valor", Order = 3)]
        public decimal Valor { get; set; }

        /// <summary>
        /// Uso de servidor de homologação
        /// </summary>
        [XmlElement("UseHomologServer", Order = 12)]
        public bool UseHomologServer { get; set; }

        /// <summary>
        /// Indica se UseHomologServer deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool UseHomologServerSpecified { get; set; }

        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";
    }

    /// <summary>
    /// Beneficiário do PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixBeneficiario")]
    [ComVisible(true)]
#endif
    public class PixBeneficiario
    {
        /// <summary>
        /// Inscrição
        /// </summary>
        [XmlElement("Inscricao", Order = 0)]
        public string Inscricao { get; set; }

        /// <summary>
        /// Nome
        /// </summary>
        [XmlElement("Nome", Order = 1)]
        public string Nome { get; set; }

        /// <summary>
        /// Conta corrente
        /// </summary>
        [XmlElement("Conta", Order = 2)]
        public PixContaCorrente Conta { get; set; }
    }

    /// <summary>
    /// Conta corrente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixContaCorrente")]
    [ComVisible(true)]
#endif
    public class PixContaCorrente
    {
        /// <summary>
        /// Agência
        /// </summary>
        [XmlElement("Agencia", Order = 0)]
        public string Agencia { get; set; }

        /// <summary>
        /// Número da conta
        /// </summary>
        [XmlElement("Numero", Order = 1)]
        public string Numero { get; set; }

        /// <summary>
        /// Banco
        /// </summary>
        [XmlElement("Banco", Order = 2)]
        public string Banco { get; set; }
    }

    /// <summary>
    /// Configuração do QRCode
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixQRCodeConfig")]
    [ComVisible(true)]
#endif
    public class PixQRCodeConfig
    {
        /// <summary>
        /// Altura
        /// </summary>
        [XmlElement("Height", Order = 1)]
        public int Height { get; set; }

        /// <summary>
        /// Formato da imagem
        /// </summary>
        [XmlElement("ImageFormat", Order = 3)]
        public PixQrCodeImageFormat ImageFormat { get; set; }

        /// <summary>
        /// Qualidade
        /// </summary>
        [XmlElement("Quality", Order = 2)]
        public int Quality { get; set; }

        /// <summary>
        /// Largura
        /// </summary>
        [XmlElement("Width", Order = 0)]
        public int Width { get; set; }
    }

    /// <summary>
    /// Calendário da cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixCalendario")]
    [ComVisible(true)]
#endif
        public class PixCalendario
    {
        /// <summary>
        /// Data de criação
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Criacao { get; set; }
#else
        public DateTimeOffset Criacao { get; set; }
#endif

        /// <summary>
        /// Data de criação no formato AAAA-MM-DDTHH:MM:SS
        /// </summary>
        [XmlElement("Criacao", Order = 0)]
        public string CriacaoField
        {
            get => Criacao.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => Criacao = DateTime.Parse(value);
#else
            set => Criacao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataDeVencimento { get; set; }
#else
        public DateTimeOffset DataDeVencimento { get; set; }
#endif

        /// <summary>
        /// Data de vencimento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataDeVencimento", Order = 2)]
        public string DataDeVencimentoField
        {
            get => DataDeVencimento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DataDeVencimento = DateTime.Parse(value);
#else
            set => DataDeVencimento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tempo de vida da cobrança em segundos
        /// </summary>
        [XmlElement("Expiracao", Order = 1)]
        public int Expiracao { get; set; }

        /// <summary>
        /// Indica se Expiracao deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool ExpiracaoSpecified { get; set; }

        /// <summary>
        /// Validade após vencimento
        /// </summary>
        [XmlElement("ValidadeAposVencimento", Order = 3)]
        public int ValidadeAposVencimento { get; set; }

        /// <summary>
        /// Indica se ValidadeAposVencimento deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool ValidadeAposVencimentoSpecified { get; set; }

        /// <summary>
        /// Indica se DataDeVencimentoField deve ser serializado
        /// </summary>
        public bool ShouldSerializeDataDeVencimentoField()
        {
#if INTEROP
            return DataDeVencimento > DateTime.MinValue;
#else
            return DataDeVencimento > DateTimeOffset.MinValue;
#endif
        }

        /// <summary>
        /// Indica se CriacaoField deve ser serializado
        /// </summary>
        public bool ShouldSerializeCriacaoField()
        {
#if INTEROP
            return Criacao > DateTime.MinValue;
#else
            return Criacao > DateTimeOffset.MinValue;
#endif
        }
    }

    /// <summary>
    /// Devedor
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.PixDevedor")]
    [ComVisible(true)]
#endif
    public class PixDevedor
    {
        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP", Order = 2)]
        public string CEP { get; set; }

        /// <summary>
        /// Cidade
        /// </summary>
        [XmlElement("Cidade", Order = 4)]
        public string Cidade { get; set; }

        /// <summary>
        /// Inscrição
        /// </summary>
        [XmlElement("Inscricao", Order = 1)]
        public string Inscricao { get; set; }

        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("Logradouro", Order = 3)]
        public string Logradouro { get; set; }

        /// <summary>
        /// Nome
        /// </summary>
        [XmlElement("Nome", Order = 0)]
        public string Nome { get; set; }

        /// <summary>
        /// UF
        /// </summary>
        [XmlElement("UF", Order = 5)]
        public string UF { get; set; }
    }

}
