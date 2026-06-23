#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CIOT
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.DeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("DeclaracaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class DeclaracaoOperacaoTransporte : XMLBase
    {
        [XmlElement("IdOperacaoTransporte")]
        public string IdOperacaoTransporte { get; set; }

        [XmlElement("TipoOperacao")]
        public TipoOperacaoTransporteCIOT TipoOperacao { get; set; }

        [XmlElement("CpfCnpjContratado")]
        public string CpfCnpjContratado { get; set; }

        [XmlElement("RNTRCContratado")]
        public string RNTRCContratado { get; set; }

        [XmlElement("CpfCnpjContratante")]
        public string CpfCnpjContratante { get; set; }

        [XmlElement("RNTRCContratante")]
        public string RNTRCContratante { get; set; }

        [XmlElement("CpfCnpjDestinatario")]
        public string CpfCnpjDestinatario { get; set; }

        [XmlIgnore]
        public double ValorFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ValorFrete para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("ValorFrete")]
        public string ValorFreteField
        {
            get => ValorFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorFrete = Converter.ToDouble(value);
        }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataDeclaracao { get; set; }
#else
        public DateTimeOffset DataDeclaracao { get; set; }
#endif

        [XmlElement("DataDeclaracao")]
        [JsonProperty("DataDeclaracao")]
        public string DataDeclaracaoField
        {
            get => CIOTDateTimeFormat.DateTime(DataDeclaracao);
#if INTEROP
            set => DataDeclaracao = DateTime.Parse(value);
#else
            set => DataDeclaracao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("IndContingencia")]
        public bool IndContingencia { get; set; }

        [XmlElement("JustificativaContingencia")]
        public string JustificativaContingencia { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataInicioViagem { get; set; }
#else
        public DateTimeOffset DataInicioViagem { get; set; }
#endif

        [XmlElement("DataInicioViagem")]
        [JsonProperty("DataInicioViagem")]
        public string DataInicioViagemField
        {
            get => DataInicioViagem.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataInicioViagem = DateTime.Parse(value);
#else
            set => DataInicioViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataFimViagem { get; set; }
#else
        public DateTimeOffset DataFimViagem { get; set; }
#endif

        [XmlElement("DataFimViagem")]
        [JsonProperty("DataFimViagem")]
        public string DataFimViagemField
        {
            get => DataFimViagem.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataFimViagem = DateTime.Parse(value);
#else
            set => DataFimViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlArray("Veiculos")]
        [XmlArrayItem("Veiculo")]
        public List<Veiculo> Veiculos { get; set; }

        [XmlArray("OrigemDestino")]
        [XmlArrayItem("ParOrigemDestino")]
        public List<OrigemDestino> OrigemDestino { get; set; }

        [XmlElement("DadosCarga")]
        public DadosCarga DadosCarga { get; set; }

        [XmlElement("InfPagamento")]
        public List<InfPagamento> InfPagamento { get; set; }

        [XmlElement("InfIndicadoresOperacionais")]
        public IndicadoresOperacionais InfIndicadoresOperacionais { get; set; }

        public bool ShouldSerializeRNTRCContratante() => !string.IsNullOrEmpty(RNTRCContratante);
        public bool ShouldSerializeCpfCnpjDestinatario() => !string.IsNullOrEmpty(CpfCnpjDestinatario);
        public bool ShouldSerializeJustificativaContingencia() => !string.IsNullOrEmpty(JustificativaContingencia);
        public bool ShouldSerializeDataDeclaracao() => false;
        public bool ShouldSerializeDataInicioViagem() => false;
        public bool ShouldSerializeDataFimViagem() => false;
        public bool ShouldSerializeValorFreteField() => ValorFrete > 0;
#if INTEROP
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTime.MinValue;
#else
        public bool ShouldSerializeDataFimViagemField() => DataFimViagem > DateTimeOffset.MinValue;
#endif
        public bool ShouldSerializeOrigemDestino() => OrigemDestino?.Count > 0;
        public bool ShouldSerializeDadosCarga() => DadosCarga != null;
        public bool ShouldSerializeInfIndicadoresOperacionais() => InfIndicadoresOperacionais != null;

#if INTEROP
        public void AddVeiculos(Veiculo veiculo)
        {
            if (Veiculos == null)
            {
                Veiculos = new List<Veiculo>();
            }

            Veiculos.Add(veiculo);
        }

        public Veiculo GetVeiculos(int index)
        {
            if ((Veiculos?.Count ?? 0) == 0)
            {
                return default;
            }

            return Veiculos[index];
        }

        public int GetVeiculosCount => (Veiculos != null ? Veiculos.Count : 0);

        public void AddOrigemDestino(OrigemDestino origemDestino)
        {
            if (OrigemDestino == null)
            {
                OrigemDestino = new List<OrigemDestino>();
            }

            OrigemDestino.Add(origemDestino);
        }

        public OrigemDestino GetOrigemDestino(int index)
        {
            if ((OrigemDestino?.Count ?? 0) == 0)
            {
                return default;
            }

            return OrigemDestino[index];
        }

        public int GetOrigemDestinoCount => (OrigemDestino != null ? OrigemDestino.Count : 0);

        public void AddInfPagamento(InfPagamento infPagamento)
        {
            if (InfPagamento == null)
            {
                InfPagamento = new List<InfPagamento>();
            }

            InfPagamento.Add(infPagamento);
        }

        public InfPagamento GetInfPagamento(int index)
        {
            if ((InfPagamento?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfPagamento[index];
        }

        public int GetInfPagamentoCount => (InfPagamento != null ? InfPagamento.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.RetDeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    [XmlRoot("RetDeclaracaoOperacaoTransporte", Namespace = CIOTNamespace.PortalANTT, IsNullable = false)]
    public class RetDeclaracaoOperacaoTransporte : XMLBase
    {
        private string codigo;
        private string mensagem;

        [XmlElement("temp")]
        public Temp Temp { get; set; }

        [XmlElement("IdOperacaoTransporte")]
        public string IdOperacaoTransporte { get; set; }

        [XmlElement("CodigoVerificador")]
        public string CodigoVerificador { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        [XmlElement("Codigo")]
        public string Codigo
        {
            get
            {
                if (Mensagens == null || Mensagens.Count == 0)
                {
                    return codigo;
                }

                return MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiroCodigo(Mensagens) ?? codigo;
            }
            set => codigo = MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiroCodigo(value);
        }

        [XmlElement("Mensagem")]
        public string Mensagem
        {
            get
            {
                if (Mensagens == null || Mensagens.Count == 0)
                {
                    return mensagem;
                }

                return MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiraDescricao(Mensagens) ?? mensagem;
            }
            set => mensagem = MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiraDescricao(value);
        }

        [XmlArray("Mensagens")]
        [XmlArrayItem("Mensagem")]
        public List<MensagemDeclaracaoOperacaoTransporte> Mensagens { get; set; }

        [XmlElement("AvisoTransportador")]
        public string AvisoTransportador { get; set; }

        public bool ShouldSerializeTemp() => Temp != null;
        public bool ShouldSerializeIdOperacaoTransporte() => !string.IsNullOrWhiteSpace(IdOperacaoTransporte);
        public bool ShouldSerializeCodigoVerificador() => !string.IsNullOrWhiteSpace(CodigoVerificador);
        public bool ShouldSerializeProtocolo() => !string.IsNullOrWhiteSpace(Protocolo);
        public bool ShouldSerializeCodigo() => !string.IsNullOrWhiteSpace(Codigo);
        public bool ShouldSerializeMensagem() => !string.IsNullOrWhiteSpace(Mensagem);
        public bool ShouldSerializeMensagens() => Mensagens != null && Mensagens.Count > 0;
        public bool ShouldSerializeAvisoTransportador() => !string.IsNullOrEmpty(AvisoTransportador);

#if INTEROP
        public void AddMensagens(MensagemDeclaracaoOperacaoTransporte mensagem)
        {
            if (Mensagens == null)
            {
                Mensagens = new List<MensagemDeclaracaoOperacaoTransporte>();
            }

            Mensagens.Add(mensagem);
        }

        public MensagemDeclaracaoOperacaoTransporte GetMensagens(int index)
        {
            if ((Mensagens?.Count ?? 0) == 0)
            {
                return default;
            }

            return Mensagens[index];
        }

        public int GetMensagensCount => (Mensagens != null ? Mensagens.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.MensagemDeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = CIOTNamespace.PortalANTT)]
    public class MensagemDeclaracaoOperacaoTransporte
    {
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        [XmlElement("Descricao")]
        public string Descricao { get; set; }

        public bool ShouldSerializeCodigo() => !string.IsNullOrWhiteSpace(Codigo);
    }

    internal static class MensagemDeclaracaoOperacaoTransporteHelper
    {
        internal static List<MensagemDeclaracaoOperacaoTransporte> CriarMensagens(string codigo, string mensagem)
        {
            var codigos = SepararCodigos(codigo);
            var descricoes = SepararDescricoes(mensagem);
            return CriarMensagens(codigos, descricoes);
        }

        internal static List<MensagemDeclaracaoOperacaoTransporte> CriarMensagens(List<string> codigos, List<string> descricoes)
        {
            var mensagens = new List<MensagemDeclaracaoOperacaoTransporte>();
            var quantidade = Math.Max(codigos.Count, descricoes.Count);

            for (var i = 0; i < quantidade; i++)
            {
                var descricao = i < descricoes.Count ? descricoes[i] : string.Empty;
                var codigo = i < codigos.Count ? codigos[i] : null;

                AplicarCodigoEmbutido(ref codigo, ref descricao);

                mensagens.Add(new MensagemDeclaracaoOperacaoTransporte
                {
                    Codigo = codigo,
                    Descricao = descricao
                });
            }

            return mensagens;
        }

        internal static string ObterPrimeiroCodigo(string value)
        {
            var codigos = SepararCodigos(value);
            return codigos.Count > 0 ? codigos[0] : null;
        }

        internal static string ObterPrimeiraDescricao(string value)
        {
            var descricoes = SepararDescricoes(value);
            return descricoes.Count > 0 ? descricoes[0] : null;
        }

        internal static string ObterPrimeiroCodigo(List<MensagemDeclaracaoOperacaoTransporte> mensagens)
        {
            if (mensagens == null)
            {
                return null;
            }

            foreach (var mensagem in mensagens)
            {
                if (!string.IsNullOrWhiteSpace(mensagem?.Codigo))
                {
                    return mensagem.Codigo;
                }
            }

            return null;
        }

        internal static string ObterPrimeiraDescricao(List<MensagemDeclaracaoOperacaoTransporte> mensagens)
        {
            if (mensagens == null)
            {
                return null;
            }

            foreach (var mensagem in mensagens)
            {
                if (!string.IsNullOrWhiteSpace(mensagem?.Descricao))
                {
                    return mensagem.Descricao;
                }
            }

            return null;
        }

        private static List<string> SepararCodigos(string codigo)
        {
            var codigos = new List<string>();
            if (string.IsNullOrWhiteSpace(codigo))
            {
                return codigos;
            }

            foreach (var item in codigo.Split(','))
            {
                codigos.Add(item.Trim());
            }

            return codigos;
        }

        internal static List<string> SepararDescricoes(string mensagem)
        {
            var descricoes = new List<string>();
            if (string.IsNullOrWhiteSpace(mensagem))
            {
                return descricoes;
            }

            var texto = mensagem.Trim();
            if (texto.StartsWith("[", StringComparison.Ordinal))
            {
                try
                {
                    var mensagens = JsonConvert.DeserializeObject<List<string>>(texto);
                    if (mensagens != null)
                    {
                        foreach (var item in mensagens)
                        {
                            descricoes.Add(item);
                        }

                        return descricoes;
                    }
                }
                catch (JsonException)
                {
                }
            }

            descricoes.Add(mensagem);
            return descricoes;
        }

        private static void AplicarCodigoEmbutido(ref string codigo, ref string descricao)
        {
            if (!string.IsNullOrWhiteSpace(codigo) || string.IsNullOrWhiteSpace(descricao))
            {
                return;
            }

            var texto = descricao.Trim();
            if (!texto.StartsWith("Código:", StringComparison.OrdinalIgnoreCase))
            {
                return;
            }

            var separador = texto.IndexOf(" - ", StringComparison.Ordinal);
            if (separador < 0)
            {
                return;
            }

            var codigoTexto = texto.Substring("Código:".Length, separador - "Código:".Length).Trim();
            if (string.IsNullOrWhiteSpace(codigoTexto))
            {
                return;
            }

            codigo = codigoTexto;
            descricao = texto.Substring(separador + 3).Trim();
        }
    }
}
