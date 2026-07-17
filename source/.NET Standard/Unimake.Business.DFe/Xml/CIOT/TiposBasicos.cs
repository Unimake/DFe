#pragma warning disable CS1591

using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CIOT
{
    internal static class CIOTDateTimeFormat
    {
#if INTEROP
        public static string DateTime(System.DateTime value) => value.Kind == DateTimeKind.Utc ? value.ToString("yyyy-MM-ddTHH:mm:ss") + "Z" : value.ToString("yyyy-MM-ddTHH:mm:sszzz");

        public static string DateTimeComFracoes(System.DateTime value) => value.Kind == DateTimeKind.Utc ? value.ToString("yyyy-MM-ddTHH:mm:ss.FFFFFFF") + "Z" : value.ToString("yyyy-MM-ddTHH:mm:ss.FFFFFFFzzz");

        public static System.DateTime ParseDateTimeOrMinValue(string value)
        {
            return string.IsNullOrWhiteSpace(value) ? System.DateTime.MinValue : System.DateTime.Parse(value);
        }
#else
        public static string DateTime(DateTimeOffset value) => value.Offset == TimeSpan.Zero ? value.UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ss") + "Z" : value.ToString("yyyy-MM-ddTHH:mm:sszzz");

        public static string DateTimeComFracoes(DateTimeOffset value) => value.Offset == TimeSpan.Zero ? value.UtcDateTime.ToString("yyyy-MM-ddTHH:mm:ss.FFFFFFF") + "Z" : value.ToString("yyyy-MM-ddTHH:mm:ss.FFFFFFFzzz");

        public static DateTimeOffset ParseDateTimeOrMinValue(string value)
        {
            return string.IsNullOrWhiteSpace(value) ? DateTimeOffset.MinValue : DateTimeOffset.Parse(value);
        }
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.VeiculoFrota")]
    [ComVisible(true)]
#endif
    public class VeiculoFrota
    {
        [XmlElement("PlacaVeiculo")]
        public string PlacaVeiculo { get; set; }

        [XmlElement("SituacaoVeiculoFrotaTransportador")]
        public bool SituacaoVeiculoFrotaTransportador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.Veiculo")]
    [ComVisible(true)]
#endif
    public class Veiculo
    {
        [XmlElement("Placa")]
        public string Placa { get; set; }

        [XmlElement("RNTRCVeiculo")]
        public string RNTRCVeiculo { get; set; }

        [XmlElement("NumeroEixos")]
        public string NumeroEixos { get; set; }

        public bool ShouldSerializeRNTRCVeiculo() => !string.IsNullOrEmpty(RNTRCVeiculo);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.OrigemDestino")]
    [ComVisible(true)]
#endif
    public class OrigemDestino
    {
        [XmlElement("Origem")]
        public Origem Origem { get; set; }

        [XmlElement("Destino")]
        public Destino Destino { get; set; }

        [XmlElement("DistanciaPercorrida")]
        public string DistanciaPercorrida { get; set; }

        [XmlElement("QtdViagens")]
        public string QtdViagens { get; set; }

        public bool ShouldSerializeDistanciaPercorrida() => !string.IsNullOrEmpty(DistanciaPercorrida);
        public bool ShouldSerializeQtdViagens() => !string.IsNullOrEmpty(QtdViagens);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.Origem")]
    [ComVisible(true)]
#endif
    public class Origem
    {
        [XmlElement("CodigoMunicipioOrigem")]
        public string CodigoMunicipioOrigem { get; set; }

        [XmlElement("CepOrigem")]
        public string CepOrigem { get; set; }

        [XmlElement("LatitudeOrigem")]
        public string LatitudeOrigem { get; set; }

        [XmlElement("LongitudeOrigem")]
        public string LongitudeOrigem { get; set; }

        public bool ShouldSerializeCodigoMunicipioOrigem() => !string.IsNullOrEmpty(CodigoMunicipioOrigem);
        public bool ShouldSerializeCepOrigem() => !string.IsNullOrEmpty(CepOrigem);
        public bool ShouldSerializeLatitudeOrigem() => !string.IsNullOrEmpty(LatitudeOrigem);
        public bool ShouldSerializeLongitudeOrigem() => !string.IsNullOrEmpty(LongitudeOrigem);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.Destino")]
    [ComVisible(true)]
#endif
    public class Destino
    {
        [XmlElement("CodigoMunicipioDestino")]
        public string CodigoMunicipioDestino { get; set; }

        [XmlElement("CepDestino")]
        public string CepDestino { get; set; }

        [XmlElement("LatitudeDestino")]
        public string LatitudeDestino { get; set; }

        [XmlElement("LongitudeDestino")]
        public string LongitudeDestino { get; set; }

        public bool ShouldSerializeCodigoMunicipioDestino() => !string.IsNullOrEmpty(CodigoMunicipioDestino);
        public bool ShouldSerializeCepDestino() => !string.IsNullOrEmpty(CepDestino);
        public bool ShouldSerializeLatitudeDestino() => !string.IsNullOrEmpty(LatitudeDestino);
        public bool ShouldSerializeLongitudeDestino() => !string.IsNullOrEmpty(LongitudeDestino);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.DadosCarga")]
    [ComVisible(true)]
#endif
    public class DadosCarga
    {
        [XmlElement("CodigoNaturezaCarga")]
        public string CodigoNaturezaCarga { get; set; }

        [XmlElement("PesoCarga")]
        public string PesoCarga { get; set; }

        [XmlElement("CodigoTipoCarga")]
        public TipoCargaCIOT CodigoTipoCarga { get; set; }

        [XmlElement("ContratantesCargFrac")]
        public List<string> ContratantesCargFrac { get; set; }

        public bool ShouldSerializeCodigoNaturezaCarga() => !string.IsNullOrEmpty(CodigoNaturezaCarga);
        public bool ShouldSerializePesoCarga() => !string.IsNullOrEmpty(PesoCarga);
        public bool ShouldSerializeCodigoTipoCarga() => CodigoTipoCarga != 0;
        public bool ShouldSerializeContratantesCargFrac() => ContratantesCargFrac?.Count > 0;

#if INTEROP
        public void AddContratantesCargFrac(string contratante)
        {
            if (ContratantesCargFrac == null)
            {
                ContratantesCargFrac = new List<string>();
            }

            ContratantesCargFrac.Add(contratante);
        }

        public string GetContratantesCargFrac(int index)
        {
            if ((ContratantesCargFrac?.Count ?? 0) == 0)
            {
                return default;
            }

            return ContratantesCargFrac[index];
        }

        public int GetContratantesCargFracCount => (ContratantesCargFrac != null ? ContratantesCargFrac.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.DadosCargaEncerramento")]
    [ComVisible(true)]
#endif
    public class DadosCargaEncerramento
    {
        [XmlElement("PesoTotalCarga")]
        public string PesoTotalCarga { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.InfPagamento")]
    [ComVisible(true)]
#endif
    public class InfPagamento
    {
        [XmlElement("TipoPagamento")]
        public TipoPagamentoFreteCIOT TipoPagamento { get; set; }

        [XmlElement("CodigoInstituicaoFinanceira")]
        public string CodigoInstituicaoFinanceira { get; set; }

        [XmlElement("NumeroAgencia")]
        public string NumeroAgencia { get; set; }

        [XmlElement("NumeroConta")]
        public string NumeroConta { get; set; }

        [XmlElement("ChavePix")]
        public string ChavePix { get; set; }

        [XmlElement("CpfCnpjCreditado")]
        public string CpfCnpjCreditado { get; set; }

        [XmlElement("CodigoPagamento")]
        public string CodigoPagamento { get; set; }

        [XmlElement("IdentificadorPix")]
        public string IdentificadorPix { get; set; }

        [XmlElement("IndPagamento")]
        public IndicadorPagamentoCIOT IndPagamento { get; set; }

        [XmlElement("NumeroParcela")]
        public string NumeroParcela { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataVencimento { get; set; }
#else
        public DateTimeOffset DataVencimento { get; set; }
#endif

        [XmlElement("DataVencimento")]
        [JsonProperty("DataVencimento")]
        public string DataVencimentoField
        {
            get => DataVencimento.ToString("yyyy-MM-dd");
#if INTEROP
            set => DataVencimento = DateTime.Parse(value);
#else
            set => DataVencimento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
        public double ValorParcela { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ValorParcela para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("ValorParcela")]
        public string ValorParcelaField
        {
            get => ValorParcela.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorParcela = Converter.ToDouble(value);
        }

        public bool ShouldSerializeDataVencimento() => false;
        public bool ShouldSerializeDataVencimentoField() =>
#if INTEROP
            DataVencimento > DateTime.MinValue;
#else
            DataVencimento > DateTimeOffset.MinValue;
#endif
        public bool ShouldSerializeValorParcelaField() => ValorParcela > 0;
        public bool ShouldSerializeCodigoInstituicaoFinanceira() => !string.IsNullOrEmpty(CodigoInstituicaoFinanceira);
        public bool ShouldSerializeNumeroAgencia() => !string.IsNullOrEmpty(NumeroAgencia);
        public bool ShouldSerializeNumeroConta() => !string.IsNullOrEmpty(NumeroConta);
        public bool ShouldSerializeChavePix() => !string.IsNullOrEmpty(ChavePix);
        public bool ShouldSerializeCodigoPagamento() => !string.IsNullOrEmpty(CodigoPagamento);
        public bool ShouldSerializeIdentificadorPix() => !string.IsNullOrEmpty(IdentificadorPix);
        public bool ShouldSerializeNumeroParcela() => !string.IsNullOrEmpty(NumeroParcela);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.IndicadoresOperacionais")]
    [ComVisible(true)]
#endif
    public class IndicadoresOperacionais
    {
        [XmlElement("IndAltoDesempenho")]
        public bool IndAltoDesempenho { get; set; }

        [XmlElement("IndRetornoVazio")]
        public bool IndRetornoVazio { get; set; }

        [XmlElement("ComposicaoVeicular")]
        public bool ComposicaoVeicular { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.Retorno")]
    [ComVisible(true)]
#endif
    public class Retorno
    {
        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }

        [XmlElement("Flag")]
        public bool Flag { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CIOT.Temp")]
    [ComVisible(true)]
#endif
    public class Temp
    {
        [XmlElement("error")]
        public string Error { get; set; }

        [XmlElement("message")]
        public string Message { get; set; }

        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime Timestamp { get; set; }
#else
        public DateTimeOffset Timestamp { get; set; }
#endif

        [XmlElement("timestamp")]
        [JsonProperty("timestamp")]
        public string TimestampField
        {
            get => CIOTDateTimeFormat.DateTimeComFracoes(Timestamp);
#if INTEROP
            set => Timestamp = DateTime.Parse(value);
#else
            set => Timestamp = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("correlationId")]
        public string CorrelationId { get; set; }

        [XmlElement("path")]
        public string Path { get; set; }

#if INTEROP
        public bool ShouldSerializeTimestamp() => false;
        public bool ShouldSerializeTimestampField() => Timestamp > DateTime.MinValue;
#else
        public bool ShouldSerializeTimestamp() => false;
        public bool ShouldSerializeTimestampField() => Timestamp > DateTimeOffset.MinValue;
#endif
    }
}
