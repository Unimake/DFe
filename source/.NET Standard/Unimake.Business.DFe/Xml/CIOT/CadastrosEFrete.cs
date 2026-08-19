#if INTEROP
using System.Runtime.InteropServices;
#endif
using Newtonsoft.Json;
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
    /// <summary>
    /// Solicita a inclusão ou atualização de um motorista na eFrete.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.GravarMotorista")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("GravarMotorista", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class GravarMotorista : XMLBase
    {
        /// <summary>Provedor CIOT. Este serviço é exclusivo da eFrete.</summary>
        [XmlElement("ProvedorCIOT")]
        [JsonIgnore]
#if INTEROP
        public ProvedorCIOT ProvedorCIOT { get; set; } = (ProvedorCIOT)(-1);
#else
        public ProvedorCIOT? ProvedorCIOT { get; set; }
#endif

        /// <summary>Número da CNH.</summary>
        [XmlElement("CNH")]
        public string CNH { get; set; }

        /// <summary>CPF do motorista.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>Data de nascimento.</summary>
        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataNascimento { get; set; }
#else
        public DateTimeOffset DataNascimento { get; set; }
#endif

        /// <summary>Data de nascimento no formato utilizado pelo XML.</summary>
        [XmlElement("DataNascimento")]
        [JsonProperty("DataNascimento")]
        public string DataNascimentoField
        {
            get => DataNascimento.ToString("yyyy-MM-ddTHH:mm:ss", CultureInfo.InvariantCulture);
#if INTEROP
            set => DataNascimento = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DataNascimento = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Endereço do motorista.</summary>
        [XmlElement("Endereco")]
        public EnderecoCIOT Endereco { get; set; }

        /// <summary>Nome completo do motorista.</summary>
        [XmlElement("Nome")]
        public string Nome { get; set; }

        /// <summary>Telefones do motorista.</summary>
        [XmlElement("Telefones")]
        public TelefonesCIOT Telefones { get; set; }

#if INTEROP
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT != (ProvedorCIOT)(-1);
        /// <summary>Impede a serialização da propriedade tipada.</summary>
        public bool ShouldSerializeDataNascimento() => false;
        /// <summary>Indica se a data deve ser serializada.</summary>
        public bool ShouldSerializeDataNascimentoField() => DataNascimento > DateTime.MinValue;
#else
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT.HasValue;
        /// <summary>Impede a serialização da propriedade tipada.</summary>
        public bool ShouldSerializeDataNascimento() => false;
        /// <summary>Indica se a data deve ser serializada.</summary>
        public bool ShouldSerializeDataNascimentoField() => DataNascimento > DateTimeOffset.MinValue;
#endif
    }

    /// <summary>Solicita a inclusão ou atualização de um proprietário na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.GravarProprietario")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("GravarProprietario", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class GravarProprietario : XMLBase
    {
        /// <summary>Provedor CIOT. Este serviço é exclusivo da eFrete.</summary>
        [XmlElement("ProvedorCIOT")]
        [JsonIgnore]
#if INTEROP
        public ProvedorCIOT ProvedorCIOT { get; set; } = (ProvedorCIOT)(-1);
#else
        public ProvedorCIOT? ProvedorCIOT { get; set; }
#endif
        /// <summary>CPF ou CNPJ do proprietário.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Endereço do proprietário.</summary>
        [XmlElement("Endereco")]
        public EnderecoCIOT Endereco { get; set; }
        /// <summary>RNTRC do proprietário.</summary>
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }
        /// <summary>Nome ou razão social.</summary>
        [XmlElement("RazaoSocial")]
        public string RazaoSocial { get; set; }
        /// <summary>Telefones do proprietário.</summary>
        [XmlElement("Telefones")]
        public TelefonesCIOT Telefones { get; set; }
#if INTEROP
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT != (ProvedorCIOT)(-1);
#else
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT.HasValue;
#endif
    }

    /// <summary>Solicita a inclusão ou atualização de um veículo na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.GravarVeiculo")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("GravarVeiculo", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class GravarVeiculo : XMLBase
    {
        /// <summary>Provedor CIOT. Este serviço é exclusivo da eFrete.</summary>
        [XmlElement("ProvedorCIOT")]
        [JsonIgnore]
#if INTEROP
        public ProvedorCIOT ProvedorCIOT { get; set; } = (ProvedorCIOT)(-1);
#else
        public ProvedorCIOT? ProvedorCIOT { get; set; }
#endif
        /// <summary>Dados do veículo.</summary>
        [XmlElement("Veiculo")]
        public VeiculoCadastroCIOT Veiculo { get; set; }
#if INTEROP
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT != (ProvedorCIOT)(-1);
#else
        /// <summary>Indica se o provedor deve ser serializado.</summary>
        public bool ShouldSerializeProvedorCIOT() => ProvedorCIOT.HasValue;
#endif
    }

    /// <summary>Dados completos de motorista retornados pela eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.MotoristaCadastroCIOT")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class MotoristaCadastroCIOT
    {
        /// <summary>Número da CNH.</summary>
        [XmlElement("CNH")] public string CNH { get; set; }
        /// <summary>CPF.</summary>
        [XmlElement("CPF")] public string CPF { get; set; }
        /// <summary>Data de nascimento retornada pela eFrete.</summary>
        [XmlElement("DataNascimento")] public string DataNascimento { get; set; }
        /// <summary>Endereço.</summary>
        [XmlElement("Endereco")] public EnderecoCIOT Endereco { get; set; }
        /// <summary>Nome.</summary>
        [XmlElement("Nome")] public string Nome { get; set; }
        /// <summary>Nome de solteira da mãe.</summary>
        [XmlElement("NomeDeSolteiraDaMae")] public string NomeDeSolteiraDaMae { get; set; }
        /// <summary>Telefones.</summary>
        [XmlElement("Telefones")] public TelefonesCIOT Telefones { get; set; }
        /// <summary>Indica se o campo opcional deve ser serializado.</summary>
        public bool ShouldSerializeNomeDeSolteiraDaMae() => !string.IsNullOrWhiteSpace(NomeDeSolteiraDaMae);
    }

    /// <summary>Dados do proprietário retornados pela eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.ProprietarioCIOT")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class ProprietarioCIOT
    {
        /// <summary>CPF ou CNPJ.</summary>
        [XmlElement("CNPJ")] public string CNPJ { get; set; }
        /// <summary>Tipo da pessoa.</summary>
        [XmlElement("TipoPessoa")] public TipoPessoaCIOT TipoPessoa { get; set; }
        /// <summary>Endereço.</summary>
        [XmlElement("Endereco")] public EnderecoCIOT Endereco { get; set; }
        /// <summary>RNTRC.</summary>
        [XmlElement("RNTRC")] public string RNTRC { get; set; }
        /// <summary>Nome ou razão social.</summary>
        [XmlElement("RazaoSocial")] public string RazaoSocial { get; set; }
        /// <summary>Telefones.</summary>
        [XmlElement("Telefones")] public TelefonesCIOT Telefones { get; set; }
    }

    /// <summary>Dados completos do veículo no cadastro eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.VeiculoCadastroCIOT")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class VeiculoCadastroCIOT
    {
        /// <summary>Ano de fabricação.</summary>
        [XmlElement("AnoFabricacao")] public int AnoFabricacao { get; set; }
        /// <summary>Ano do modelo.</summary>
        [XmlElement("AnoModelo")] public int AnoModelo { get; set; }
        /// <summary>Capacidade em quilogramas.</summary>
        [XmlElement("CapacidadeKg")] public double CapacidadeKg { get; set; }
        /// <summary>Capacidade em metros cúbicos.</summary>
        [XmlElement("CapacidadeM3")] public double CapacidadeM3 { get; set; }
        /// <summary>Chassi.</summary>
        [XmlElement("Chassi")] public string Chassi { get; set; }
        /// <summary>Código IBGE do município retornado pela eFrete.</summary>
        [XmlElement("CodigoMunicipio")] public string CodigoMunicipio { get; set; }
        /// <summary>Cor.</summary>
        [XmlElement("Cor")] public string Cor { get; set; }
        /// <summary>Marca.</summary>
        [XmlElement("Marca")] public string Marca { get; set; }
        /// <summary>Modelo.</summary>
        [XmlElement("Modelo")] public string Modelo { get; set; }
        /// <summary>Número de eixos.</summary>
        [XmlElement("NumeroDeEixos")] public int NumeroDeEixos { get; set; }
        /// <summary>Placa.</summary>
        [XmlElement("Placa")] public string Placa { get; set; }
        /// <summary>RNTRC do proprietário.</summary>
        [XmlElement("RNTRC")] public string RNTRC { get; set; }
        /// <summary>RENAVAM.</summary>
        [XmlElement("Renavam")] public string Renavam { get; set; }
        /// <summary>Tara.</summary>
        [XmlElement("Tara")] public double Tara { get; set; }
        /// <summary>Tipo da carroceria.</summary>
        [XmlElement("TipoCarroceria")] public TipoCarroceriaCIOT TipoCarroceria { get; set; } = (TipoCarroceriaCIOT)(-1);
        /// <summary>Tipo do rodado.</summary>
        [XmlElement("TipoRodado")] public TipoRodadoCIOT TipoRodado { get; set; } = (TipoRodadoCIOT)(-1);
        /// <summary>Indica se o ano deve ser serializado.</summary>
        public bool ShouldSerializeAnoFabricacao() => AnoFabricacao > 0;
        /// <summary>Indica se o ano do modelo deve ser serializado.</summary>
        public bool ShouldSerializeAnoModelo() => AnoModelo > 0;
        /// <summary>Indica se a capacidade deve ser serializada.</summary>
        public bool ShouldSerializeCapacidadeKg() => CapacidadeKg > 0;
        /// <summary>Indica se a capacidade deve ser serializada.</summary>
        public bool ShouldSerializeCapacidadeM3() => CapacidadeM3 > 0;
        /// <summary>Indica se o município deve ser serializado.</summary>
        public bool ShouldSerializeCodigoMunicipio() => !string.IsNullOrWhiteSpace(CodigoMunicipio);
        /// <summary>Indica se a cor deve ser serializada.</summary>
        public bool ShouldSerializeCor() => !string.IsNullOrWhiteSpace(Cor);
        /// <summary>Indica se a marca deve ser serializada.</summary>
        public bool ShouldSerializeMarca() => !string.IsNullOrWhiteSpace(Marca);
        /// <summary>Indica se o modelo deve ser serializado.</summary>
        public bool ShouldSerializeModelo() => !string.IsNullOrWhiteSpace(Modelo);
        /// <summary>Indica se a tara deve ser serializada.</summary>
        public bool ShouldSerializeTara() => Tara > 0;
        /// <summary>Indica se o tipo deve ser serializado.</summary>
        public bool ShouldSerializeTipoCarroceria() => (int)TipoCarroceria >= 0;
        /// <summary>Indica se o tipo deve ser serializado.</summary>
        public bool ShouldSerializeTipoRodado() => (int)TipoRodado >= 0;
    }

    /// <summary>Base dos retornos dos cadastros eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetCadastroEFreteBase")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class RetCadastroEFreteBase : XMLBase
    {
        /// <summary>Detalhes de erro normalizados.</summary>
        [XmlElement("temp")] public Temp Temp { get; set; }
        /// <summary>Código retornado.</summary>
        [XmlElement("Codigo")] public string Codigo { get; set; }
        /// <summary>Mensagem retornada.</summary>
        [XmlElement("Mensagem")] public string Mensagem { get; set; }
        /// <summary>Indica sucesso.</summary>
        [XmlElement("Sucesso")] public bool Sucesso { get; set; }
        /// <summary>Versão retornada.</summary>
        [XmlElement("Versao")] public int Versao { get; set; }
        /// <summary>Indica se o erro deve ser serializado.</summary>
        public bool ShouldSerializeTemp() => Temp != null;
        /// <summary>Indica se o código deve ser serializado.</summary>
        public bool ShouldSerializeCodigo() => !string.IsNullOrWhiteSpace(Codigo);
        /// <summary>Indica se a mensagem deve ser serializada.</summary>
        public bool ShouldSerializeMensagem() => !string.IsNullOrWhiteSpace(Mensagem);
        /// <summary>Indica se a versão deve ser serializada.</summary>
        public bool ShouldSerializeVersao() => Versao > 0;
    }

    /// <summary>Retorno do cadastro de motorista.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetGravarMotorista")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("RetGravarMotorista", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetGravarMotorista : RetCadastroEFreteBase
    {
        /// <summary>Motorista retornado.</summary>
        [XmlElement("Motorista")] public MotoristaCadastroCIOT Motorista { get; set; }
    }

    /// <summary>Retorno do cadastro de proprietário.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetGravarProprietario")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("RetGravarProprietario", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetGravarProprietario : RetCadastroEFreteBase
    {
        /// <summary>Proprietário retornado.</summary>
        [XmlElement("Proprietario")] public ProprietarioCIOT Proprietario { get; set; }
    }

    /// <summary>Retorno do cadastro de veículo.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetGravarVeiculo")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("RetGravarVeiculo", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetGravarVeiculo : RetCadastroEFreteBase
    {
        /// <summary>Veículo retornado.</summary>
        [XmlElement("Veiculo")] public VeiculoCadastroCIOT Veiculo { get; set; }
    }

    /// <summary>Tipo da pessoa no cadastro do proprietário.</summary>
    public enum TipoPessoaCIOT
    {
        /// <summary>Pessoa física.</summary>
        Fisica,
        /// <summary>Pessoa jurídica.</summary>
        Juridica
    }
    /// <summary>Tipo de carroceria aceito pela eFrete.</summary>
    public enum TipoCarroceriaCIOT
    {
        /// <summary>Não aplicável.</summary>
        NaoAplicavel,
        /// <summary>Aberta.</summary>
        Aberta,
        /// <summary>Fechada ou baú.</summary>
        FechadaOuBau,
        /// <summary>Graneleira.</summary>
        Granelera,
        /// <summary>Porta-contêiner.</summary>
        PortaContainer,
        /// <summary>Sider.</summary>
        Sider
    }
    /// <summary>Tipo de rodado aceito pela eFrete.</summary>
    public enum TipoRodadoCIOT
    {
        /// <summary>Não aplicável.</summary>
        NaoAplicavel,
        /// <summary>Truck.</summary>
        Truck,
        /// <summary>Toco.</summary>
        Toco,
        /// <summary>Cavalo.</summary>
        Cavalo
    }
}
