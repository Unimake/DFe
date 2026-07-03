using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Newtonsoft.Json;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Converters.Json;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// XML de registro de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.BoletoRegistrar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoRegistrar", IsNullable = false)]
    public class BoletoRegistrar : XMLBase
    {
        /// <summary>
        /// Identificador da configuracao
        /// </summary>
        [XmlElement("ConfigurationId")]
        public string ConfigurationId { get; set; }

        /// <summary>
        /// Indicador de aceite
        /// </summary>
        [XmlElement("Aceite")]
        public bool Aceite { get; set; }

        /// <summary>
        /// Agencia coletora (S/N)
        /// </summary>
        [JsonConverter(typeof(XmlEnumJsonConverter))]
        [XmlElement("AgenciaColetora")]
        public SimNaoLetra AgenciaColetora { get; set; }

        /// <summary>
        /// Indica se AgenciaColetora deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool AgenciaColetoraSpecified { get; set; }

        /// <summary>
        /// Avalista do boleto
        /// </summary>
        [XmlElement("Avalista")]
        public EBoletoAvalista Avalista { get; set; }

        /// <summary>
        /// Carteira
        /// </summary>
        [XmlElement("Carteira")]
        public string Carteira { get; set; }

        /// <summary>
        /// Codigo de barra numerico
        /// </summary>
        [XmlElement("CodigoBarraNumerico")]
        public string CodigoBarraNumerico { get; set; }

        /// <summary>
        /// Desconto
        /// </summary>
        [XmlElement("Desconto")]
        public EBoletoDesconto Desconto { get; set; }

        /// <summary>
        /// Dias para baixa ou devolucao
        /// </summary>
        [XmlElement("DiasParaBaixaOuDevolucao")]
        public int DiasParaBaixaOuDevolucao { get; set; }

        /// <summary>
        /// Indica se DiasParaBaixaOuDevolucao deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool DiasParaBaixaOuDevolucaoSpecified { get; set; }

        /// <summary>
        /// Digito verificador do numero no banco
        /// </summary>
        [XmlElement("DigitoVerificadorNumeroNoBanco")]
        public string DigitoVerificadorNumeroNoBanco { get; set; }

        /// <summary>
        /// Data de emissao
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Emissao { get; set; }
#else
        public DateTimeOffset Emissao { get; set; }
#endif

        /// <summary>
        /// Data de emissao no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Emissao")]
        public string EmissaoField
        {
            get => Emissao.ToString("yyyy-MM-dd");
#if INTEROP
            set => Emissao = DateTime.Parse(value);
#else
            set => Emissao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Especie do titulo
        /// </summary>
        [XmlElement("Especie")]
        public EBoletoEspecieTitulo Especie { get; set; }

        /// <summary>
        /// Configuracao de ficha de aceite
        /// </summary>
        [XmlElement("FichaAceiteConfig")]
        public EBoletoFichaAceiteConfig FichaAceiteConfig { get; set; }

        /// <summary>
        /// Identificacao de distribuicao
        /// </summary>
        [XmlElement("IdentificacaoDistribuicao")]
        public EBoletoIdentificacaoDistribuicao IdentificacaoDistribuicao { get; set; }

        /// <summary>
        /// Indica se IdentificacaoDistribuicao deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool IdentificacaoDistribuicaoSpecified { get; set; }

        /// <summary>
        /// Identificacao de emissao
        /// </summary>
        [XmlElement("IdentificacaoEmissao")]
        public EBoletoIdentificacaoEmissao IdentificacaoEmissao { get; set; }

        /// <summary>
        /// Indica se IdentificacaoEmissao deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool IdentificacaoEmissaoSpecified { get; set; }

        /// <summary>
        /// Juros
        /// </summary>
        [XmlElement("Juros")]
        public EBoletoJuros Juros { get; set; }

        /// <summary>
        /// Linha digitavel
        /// </summary>
        [XmlElement("LinhaDigitavel")]
        public string LinhaDigitavel { get; set; }

        /// <summary>
        /// Mensagens
        /// </summary>
        [XmlElement("Mensagens")]
        public EBoletoMensagens Mensagens { get; set; }

        /// <summary>
        /// Mensagens do recibo
        /// </summary>
        [XmlElement("MensagensRecibo")]
        public EBoletoMensagens MensagensRecibo { get; set; }

        /// <summary>
        /// Multa
        /// </summary>
        [XmlElement("Multa")]
        public EBoletoMulta Multa { get; set; }

        /// <summary>
        /// Numero de dias limite para recebimento
        /// </summary>
        [XmlElement("NumeroDiasLimiteRecebimento")]
        public int NumeroDiasLimiteRecebimento { get; set; }

        /// <summary>
        /// Indica se NumeroDiasLimiteRecebimento deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool NumeroDiasLimiteRecebimentoSpecified { get; set; }

        /// <summary>
        /// Numero na empresa
        /// </summary>
        [XmlElement("NumeroNaEmpresa")]
        public string NumeroNaEmpresa { get; set; }

        /// <summary>
        /// Numero no banco
        /// </summary>
        [XmlElement("NumeroNoBanco")]
        public string NumeroNoBanco { get; set; }

        /// <summary>
        /// Numero da parcela
        /// </summary>
        [XmlElement("NumeroParcela")]
        public int NumeroParcela { get; set; }

        /// <summary>
        /// Numero de variacao de carteira
        /// </summary>
        [XmlElement("NumeroVariacaoCarteira")]
        public int NumeroVariacaoCarteira { get; set; }

        /// <summary>
        /// Indica se NumeroVariacaoCarteira deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool NumeroVariacaoCarteiraSpecified { get; set; }

        /// <summary>
        /// Pagador
        /// </summary>
        [XmlElement("Pagador")]
        public EBoletoPagador Pagador { get; set; }

        /// <summary>
        /// Configuracao de PDF
        /// </summary>
        [XmlElement("PDFConfig")]
        public EBoletoPDFConfig PDFConfig { get; set; }

        /// <summary>
        /// Permite recebimento parcial (S/N)
        /// </summary>
        [JsonConverter(typeof(XmlEnumJsonConverter))]
        [XmlElement("PermiteRecebimentoParcial")]
        public SimNaoLetra PermiteRecebimentoParcial { get; set; }

        /// <summary>
        /// Indica se PermiteRecebimentoParcial deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermiteRecebimentoParcialSpecified { get; set; }

        /// <summary>
        /// Configuracao de PIX
        /// </summary>
        [XmlElement("PixConfig")]
        public EBoletoPixConfig PixConfig { get; set; }

        /// <summary>
        /// Protesto
        /// </summary>
        [XmlElement("Protesto")]
        public EBoletoProtesto Protesto { get; set; }

        /// <summary>
        /// Indicador de teste
        /// </summary>
        [XmlElement("Testing")]
        public bool Testing { get; set; }

        /// <summary>
        /// Indica se Testing deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TestingSpecified { get; set; }

        /// <summary>
        /// Tipo de baixa/devolucao
        /// </summary>
        [XmlElement("TipoBaixaDevolucao")]
        public EBoletoTipoBaixaDevolucao TipoBaixaDevolucao { get; set; }

        /// <summary>
        /// Indica se TipoBaixaDevolucao deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool TipoBaixaDevolucaoSpecified { get; set; }

        /// <summary>
        /// Valor de abatimento
        /// </summary>
        [XmlElement("ValorAbatimento")]
        public decimal ValorAbatimento { get; set; }

        /// <summary>
        /// Indica se ValorAbatimento deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool ValorAbatimentoSpecified { get; set; }

        /// <summary>
        /// Valor do IOF
        /// </summary>
        [XmlElement("ValorIOF")]
        public decimal ValorIOF { get; set; }

        /// <summary>
        /// Indica se ValorIOF deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool ValorIOFSpecified { get; set; }

        /// <summary>
        /// Valor nominal
        /// </summary>
        [XmlElement("ValorNominal")]
        public decimal ValorNominal { get; set; }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Vencimento { get; set; }
#else
        public DateTimeOffset Vencimento { get; set; }
#endif

        /// <summary>
        /// Data de vencimento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Vencimento")]
        public string VencimentoField
        {
            get => Vencimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => Vencimento = DateTime.Parse(value);
#else
            set => Vencimento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Uso de servidor de homologacao
        /// </summary>
        [XmlElement("UseHomologServer")]
        public bool UseHomologServer { get; set; }

        /// <summary>
        /// Indica se UseHomologServer deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool UseHomologServerSpecified { get; set; }

        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";
    }

    /// <summary>
    /// Avalista do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoAvalista")]
    [ComVisible(true)]
#endif
    public class EBoletoAvalista
    {
        /// <summary>
        /// Nome
        /// </summary>
        [XmlElement("Nome")]
        public string Nome { get; set; }

        /// <summary>
        /// Tipo de inscricao
        /// </summary>
        [XmlElement("TipoInscricao")]
        public EBoletoTipoInscricao TipoInscricao { get; set; }

        /// <summary>
        /// Inscricao
        /// </summary>
        [XmlElement("Inscricao")]
        public string Inscricao { get; set; }

        /// <summary>
        /// Endereco
        /// </summary>
        [XmlElement("Endereco")]
        public EBoletoEndereco Endereco { get; set; }
    }

    /// <summary>
    /// Endereco
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoEndereco")]
    [ComVisible(true)]
#endif
    public class EBoletoEndereco
    {
        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("Logradouro")]
        public string Logradouro { get; set; }

        /// <summary>
        /// Numero
        /// </summary>
        [XmlElement("Numero")]
        public string Numero { get; set; }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("Complemento")]
        public string Complemento { get; set; }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("Bairro")]
        public string Bairro { get; set; }

        /// <summary>
        /// Cidade
        /// </summary>
        [XmlElement("Cidade")]
        public string Cidade { get; set; }

        /// <summary>
        /// UF
        /// </summary>
        [XmlElement("UF")]
        public string UF { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }
    }

    /// <summary>
    /// Desconto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoDesconto")]
    [ComVisible(true)]
#endif
    public class EBoletoDesconto
    {
        /// <summary>
        /// Data do desconto
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Data { get; set; }
#else
        public DateTimeOffset Data { get; set; }
#endif

        /// <summary>
        /// Data do desconto no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Data")]
        public string DataField
        {
            get => Data.ToString("yyyy-MM-dd");
#if INTEROP
            set => Data = DateTime.Parse(value);
#else
            set => Data = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo do desconto
        /// </summary>
        [XmlElement("Tipo")]
        public EBoletoTipoDesconto Tipo { get; set; }

        /// <summary>
        /// Valor do desconto
        /// </summary>
        [XmlElement("Valor")]
        public decimal Valor { get; set; }
    }

    /// <summary>
    /// Juros
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoJuros")]
    [ComVisible(true)]
#endif
    public class EBoletoJuros
    {
        /// <summary>
        /// Data de juros
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Data { get; set; }
#else
        public DateTimeOffset Data { get; set; }
#endif

        /// <summary>
        /// Data de juros no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Data")]
        public string DataField
        {
            get => Data.ToString("yyyy-MM-dd");
#if INTEROP
            set => Data = DateTime.Parse(value);
#else
            set => Data = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo de juros
        /// </summary>
        [XmlElement("Tipo")]
        public EBoletoTipoJuros Tipo { get; set; }

        /// <summary>
        /// Valor de juros
        /// </summary>
        [XmlElement("Valor")]
        public decimal Valor { get; set; }
    }

    /// <summary>
    /// Multa
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoMulta")]
    [ComVisible(true)]
#endif
    public class EBoletoMulta
    {
        /// <summary>
        /// Data da multa
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Data { get; set; }
#else
        public DateTimeOffset Data { get; set; }
#endif

        /// <summary>
        /// Data da multa no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Data")]
        public string DataField
        {
            get => Data.ToString("yyyy-MM-dd");
#if INTEROP
            set => Data = DateTime.Parse(value);
#else
            set => Data = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo de multa
        /// </summary>
        [XmlElement("Tipo")]
        public EBoletoTipoMulta Tipo { get; set; }

        /// <summary>
        /// Valor da multa
        /// </summary>
        [XmlElement("Valor")]
        public decimal Valor { get; set; }
    }

    /// <summary>
    /// Pagador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoPagador")]
    [ComVisible(true)]
#endif
    public class EBoletoPagador
    {
        /// <summary>
        /// Nome
        /// </summary>
        [XmlElement("Nome")]
        public string Nome { get; set; }

        /// <summary>
        /// Inscricao
        /// </summary>
        [XmlElement("Inscricao")]
        public string Inscricao { get; set; }

        /// <summary>
        /// Tipo de inscricao
        /// </summary>
        [XmlElement("TipoInscricao")]
        public EBoletoTipoInscricao TipoInscricao { get; set; }

        /// <summary>
        /// Codigo
        /// </summary>
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        /// <summary>
        /// Email
        /// </summary>
        [XmlElement("Email")]
        public string Email { get; set; }

        /// <summary>
        /// Telefone
        /// </summary>
        [XmlElement("Telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Endereco
        /// </summary>
        [XmlElement("Endereco")]
        public EBoletoEndereco Endereco { get; set; }
    }

    /// <summary>
    /// Mensagens
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoMensagens")]
    [ComVisible(true)]
#endif
    public class EBoletoMensagens
    {
        /// <summary>
        /// Lista de mensagens
        /// </summary>
        [XmlElement("Mensagem")]
        public List<string> Mensagem { get; set; }
    }

    /// <summary>
    /// Configuracao de ficha de aceite
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoFichaAceiteConfig")]
    [ComVisible(true)]
#endif
    public class EBoletoFichaAceiteConfig
    {
        /// <summary>
        /// Indicador de ficha aceite
        /// </summary>
        [XmlElement("FichaAceite")]
        public bool FichaAceite { get; set; }

        /// <summary>
        /// Indicador de merge
        /// </summary>
        [XmlElement("Merge")]
        public bool Merge { get; set; }

        /// <summary>
        /// Detalhes de aceite
        /// </summary>
        [XmlElement("AceiteDetails")]
        public EBoletoAceiteDetails AceiteDetails { get; set; }
    }

    /// <summary>
    /// Detalhes de aceite
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoAceiteDetails")]
    [ComVisible(true)]
#endif
    public class EBoletoAceiteDetails
    {
        /// <summary>
        /// Indicador de aceite
        /// </summary>
        [XmlElement("Aceite")]
        public bool Aceite { get; set; }

        /// <summary>
        /// Agencia e codigo do beneficiario
        /// </summary>
        [XmlElement("AgenciaCodigoBeneficiario")]
        public string AgenciaCodigoBeneficiario { get; set; }

        /// <summary>
        /// Beneficiario
        /// </summary>
        [XmlElement("Beneficiario")]
        public string Beneficiario { get; set; }

        /// <summary>
        /// Carteira
        /// </summary>
        [XmlElement("Carteira")]
        public string Carteira { get; set; }

        /// <summary>
        /// Codigo do banco
        /// </summary>
        [XmlElement("CodigoBanco")]
        public int CodigoBanco { get; set; }

        /// <summary>
        /// Data do documento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataDocumento { get; set; }
#else
        public DateTimeOffset DataDocumento { get; set; }
#endif

        /// <summary>
        /// Data do documento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataDocumento")]
        public string DataDocumentoField
        {
            get => DataDocumento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataDocumento = DateTime.Parse(value);
#else
            set => DataDocumento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de processamento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataProcessamento { get; set; }
#else
        public DateTimeOffset DataProcessamento { get; set; }
#endif

        /// <summary>
        /// Data de processamento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("DataProcessamento")]
        public string DataProcessamentoField
        {
            get => DataProcessamento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataProcessamento = DateTime.Parse(value);
#else
            set => DataProcessamento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Valor dos descontos
        /// </summary>
        [XmlElement("Descontos")]
        public decimal Descontos { get; set; }

        /// <summary>
        /// Especie
        /// </summary>
        [XmlElement("Especie")]
        public string Especie { get; set; }

        /// <summary>
        /// Instrucoes
        /// </summary>
        [XmlElement("Instrucoes")]
        public EBoletoInstrucoes Instrucoes { get; set; }

        /// <summary>
        /// Logo do banco
        /// </summary>
        [XmlElement("LogoBanco")]
        public string LogoBanco { get; set; }

        /// <summary>
        /// Mora e multa
        /// </summary>
        [XmlElement("MoraMulta")]
        public int MoraMulta { get; set; }

        /// <summary>
        /// Nosso numero
        /// </summary>
        [XmlElement("NossoNumero")]
        public string NossoNumero { get; set; }

        /// <summary>
        /// Numero do documento
        /// </summary>
        [XmlElement("NumeroDocumento")]
        public string NumeroDocumento { get; set; }

        /// <summary>
        /// Outras deducoes
        /// </summary>
        [XmlElement("OutrasDeducoes")]
        public int OutrasDeducoes { get; set; }

        /// <summary>
        /// Outros acrescimos
        /// </summary>
        [XmlElement("OutrosAcrescimos")]
        public int OutrosAcrescimos { get; set; }

        /// <summary>
        /// Pagador
        /// </summary>
        [XmlElement("Pagador")]
        public string Pagador { get; set; }

        /// <summary>
        /// Rodape direito
        /// </summary>
        [XmlElement("RodapeDireito")]
        public EBoletoRodape RodapeDireito { get; set; }

        /// <summary>
        /// Rodape esquerdo
        /// </summary>
        [XmlElement("RodapeEsquerdo")]
        public EBoletoRodape RodapeEsquerdo { get; set; }

        /// <summary>
        /// Indicador de teste
        /// </summary>
        [XmlElement("Testing")]
        public bool Testing { get; set; }

        /// <summary>
        /// Valor cobrado
        /// </summary>
        [XmlElement("ValorCobrado")]
        public decimal ValorCobrado { get; set; }

        /// <summary>
        /// Valor do documento
        /// </summary>
        [XmlElement("ValorDocumento")]
        public decimal ValorDocumento { get; set; }

        /// <summary>
        /// Data de vencimento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime Vencimento { get; set; }
#else
        public DateTimeOffset Vencimento { get; set; }
#endif

        /// <summary>
        /// Data de vencimento no formato AAAA-MM-DD
        /// </summary>
        [XmlElement("Vencimento")]
        public string VencimentoField
        {
            get => Vencimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => Vencimento = DateTime.Parse(value);
#else
            set => Vencimento = DateTimeOffset.Parse(value);
#endif
        }
    }

    /// <summary>
    /// Instrucoes de ficha de aceite
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoInstrucoes")]
    [ComVisible(true)]
#endif
    public class EBoletoInstrucoes
    {
        /// <summary>
        /// Lista de instrucoes
        /// </summary>
        [XmlElement("Instrucao")]
        public List<string> Instrucao { get; set; }
    }

    /// <summary>
    /// Rodape da ficha de aceite
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoRodape")]
    [ComVisible(true)]
#endif
    public class EBoletoRodape
    {
        /// <summary>
        /// Codigos de barras
        /// </summary>
        [XmlElement("CodigoBarras")]
        public List<string> CodigoBarras { get; set; }

        /// <summary>
        /// Textos livres
        /// </summary>
        [XmlElement("TextoLivre")]
        public List<string> TextoLivre { get; set; }
    }

    /// <summary>
    /// Configuracao de PDF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoPDFConfig")]
    [ComVisible(true)]
#endif
    public class EBoletoPDFConfig
    {
        /// <summary>
        /// Senha do PDF
        /// </summary>
        [XmlElement("Password")]
        public string Password { get; set; }

        /// <summary>
        /// Permitir anotacoes
        /// </summary>
        [XmlElement("PermitAnnotations")]
        public bool PermitAnnotations { get; set; }

        /// <summary>
        /// Indica se PermitAnnotations deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitAnnotationsSpecified { get; set; }

        /// <summary>
        /// Permitir montagem de documento
        /// </summary>
        [XmlElement("PermitAssembleDocument")]
        public bool PermitAssembleDocument { get; set; }

        /// <summary>
        /// Indica se PermitAssembleDocument deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitAssembleDocumentSpecified { get; set; }

        /// <summary>
        /// Permitir extracao de conteudo
        /// </summary>
        [XmlElement("PermitExtractContent")]
        public bool PermitExtractContent { get; set; }

        /// <summary>
        /// Indica se PermitExtractContent deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitExtractContentSpecified { get; set; }

        /// <summary>
        /// Permitir preenchimento de formularios
        /// </summary>
        [XmlElement("PermitFormsFill")]
        public bool PermitFormsFill { get; set; }

        /// <summary>
        /// Indica se PermitFormsFill deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitFormsFillSpecified { get; set; }

        /// <summary>
        /// Permitir impressao de alta qualidade
        /// </summary>
        [XmlElement("PermitFullQualityPrint")]
        public bool PermitFullQualityPrint { get; set; }

        /// <summary>
        /// Indica se PermitFullQualityPrint deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitFullQualityPrintSpecified { get; set; }

        /// <summary>
        /// Permitir modificacao do documento
        /// </summary>
        [XmlElement("PermitModifyDocument")]
        public bool PermitModifyDocument { get; set; }

        /// <summary>
        /// Indica se PermitModifyDocument deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitModifyDocumentSpecified { get; set; }

        /// <summary>
        /// Permitir impressao
        /// </summary>
        [XmlElement("PermitPrint")]
        public bool PermitPrint { get; set; }

        /// <summary>
        /// Indica se PermitPrint deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool PermitPrintSpecified { get; set; }

        /// <summary>
        /// Assinar o PDF
        /// </summary>
        [XmlElement("SignPDF")]
        public bool SignPDF { get; set; }

        /// <summary>
        /// Indica se SignPDF deve ser serializado
        /// </summary>
        [XmlIgnore]
        public bool SignPDFSpecified { get; set; }

        /// <summary>
        /// Tentar gerar PDF
        /// </summary>
        [XmlElement("TryGeneratePDF")]
        public bool TryGeneratePDF { get; set; }
    }

    /// <summary>
    /// Configuracao de PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoPixConfig")]
    [ComVisible(true)]
#endif
    public class EBoletoPixConfig
    {
        /// <summary>
        /// Chave do PIX
        /// </summary>
        [XmlElement("Chave")]
        public string Chave { get; set; }

        /// <summary>
        /// Configuracao do QRCode
        /// </summary>
        [XmlElement("QrCodeConfig")]
        public EBoletoQrCodeConfig QrCodeConfig { get; set; }

        /// <summary>
        /// Indicador para registrar PIX
        /// </summary>
        [XmlElement("RegistrarPIX")]
        public bool RegistrarPIX { get; set; }
    }

    /// <summary>
    /// Configuracao de QRCode
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoQrCodeConfig")]
    [ComVisible(true)]
#endif
    public class EBoletoQrCodeConfig
    {
        /// <summary>
        /// Altura
        /// </summary>
        [XmlElement("Height")]
        public int Height { get; set; }

        /// <summary>
        /// Formato de imagem
        /// </summary>
        [XmlElement("ImageFormat")]
        public EBoletoQrCodeImageFormat ImageFormat { get; set; }

        /// <summary>
        /// Qualidade
        /// </summary>
        [XmlElement("Quality")]
        public int Quality { get; set; }

        /// <summary>
        /// Largura
        /// </summary>
        [XmlElement("Width")]
        public int Width { get; set; }
    }

    /// <summary>
    /// Protesto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.EBoletoProtesto")]
    [ComVisible(true)]
#endif
    public class EBoletoProtesto
    {
        /// <summary>
        /// Tipo do protesto
        /// </summary>
        [XmlElement("Tipo")]
        public EBoletoTipoProtesto Tipo { get; set; }

        /// <summary>
        /// Valor do protesto
        /// </summary>
        [XmlElement("Valor")]
        public decimal Valor { get; set; }
    }
}
