#pragma warning disable CS1591

#if INTEROP
using System.Collections.Generic;
#else
using System.Collections.Generic;
#endif

using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CIOT
{
    public class VeiculoFrotaCIOT
    {
        [XmlElement("PlacaVeiculo")]
        public string PlacaVeiculo { get; set; }

        [XmlElement("SituacaoVeiculoFrotaTransportador")]
        public bool SituacaoVeiculoFrotaTransportador { get; set; }
    }

    public class VeiculoCIOT
    {
        [XmlElement("Placa")]
        public string Placa { get; set; }

        [XmlElement("RNTRCVeiculo")]
        public string RNTRCVeiculo { get; set; }

        [XmlElement("NumeroEixos")]
        public string NumeroEixos { get; set; }

        public bool ShouldSerializeRNTRCVeiculo() => !string.IsNullOrEmpty(RNTRCVeiculo);
    }

    public class ParOrigemDestinoCIOT
    {
        [XmlElement("Origem")]
        public OrigemCIOT Origem { get; set; }

        [XmlElement("Destino")]
        public DestinoCIOT Destino { get; set; }

        [XmlElement("DistanciaPercorrida")]
        public string DistanciaPercorrida { get; set; }

        [XmlElement("QtdViagens")]
        public string QtdViagens { get; set; }

        public bool ShouldSerializeDistanciaPercorrida() => !string.IsNullOrEmpty(DistanciaPercorrida);
        public bool ShouldSerializeQtdViagens() => !string.IsNullOrEmpty(QtdViagens);
    }

    public class OrigemCIOT
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

    public class DestinoCIOT
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

    public class DadosCargaCIOT
    {
        [XmlElement("CodigoNaturezaCarga")]
        public string CodigoNaturezaCarga { get; set; }

        [XmlElement("PesoCarga")]
        public string PesoCarga { get; set; }

        [XmlElement("CodigoTipoCarga")]
        public TipoCargaCIOT CodigoTipoCarga { get; set; }

        [XmlArray("ContratantesCargaFrac")]
        [XmlArrayItem("CpfCnpjContratante")]
        public List<string> ContratantesCargaFrac { get; set; }

        public bool ShouldSerializeCodigoNaturezaCarga() => !string.IsNullOrEmpty(CodigoNaturezaCarga);
        public bool ShouldSerializePesoCarga() => !string.IsNullOrEmpty(PesoCarga);
        public bool ShouldSerializeCodigoTipoCarga() => CodigoTipoCarga != 0;
        public bool ShouldSerializeContratantesCargaFrac() => ContratantesCargaFrac?.Count > 0;
    }

    public class DadosCargaEncerramentoCIOT
    {
        [XmlElement("PesoTotalCarga")]
        public string PesoTotalCarga { get; set; }
    }

    public class PagamentoCIOT
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

        [XmlArray("Parcelas")]
        [XmlArrayItem("Parcela")]
        public List<ParcelaCIOT> Parcelas { get; set; }

        public bool ShouldSerializeCodigoInstituicaoFinanceira() => !string.IsNullOrEmpty(CodigoInstituicaoFinanceira);
        public bool ShouldSerializeNumeroAgencia() => !string.IsNullOrEmpty(NumeroAgencia);
        public bool ShouldSerializeNumeroConta() => !string.IsNullOrEmpty(NumeroConta);
        public bool ShouldSerializeChavePix() => !string.IsNullOrEmpty(ChavePix);
        public bool ShouldSerializeCodigoPagamento() => !string.IsNullOrEmpty(CodigoPagamento);
        public bool ShouldSerializeIdentificadorPix() => !string.IsNullOrEmpty(IdentificadorPix);
        public bool ShouldSerializeParcelas() => Parcelas?.Count > 0;
    }

    public class ParcelaCIOT
    {
        [XmlElement("NumeroParcela")]
        public string NumeroParcela { get; set; }

        [XmlElement("DataVencimento")]
        public string DataVencimento { get; set; }

        [XmlElement("ValorParcela")]
        public string ValorParcela { get; set; }
    }

    public class IndicadoresOperacionaisCIOT
    {
        [XmlElement("IndAltoDesempenho")]
        public bool IndAltoDesempenho { get; set; }

        [XmlElement("IndRetornoVazio")]
        public bool IndRetornoVazio { get; set; }

        [XmlElement("ComposicaoVeicular")]
        public bool ComposicaoVeicular { get; set; }
    }

    public class RetornoExcecaoCIOT
    {
        [XmlElement("CpfCnpjTransportador")]
        public string CpfCnpjTransportador { get; set; }

        [XmlElement("Flag")]
        public bool Flag { get; set; }
    }

    public class TempCIOT
    {
        [XmlElement("error")]
        public string Error { get; set; }

        [XmlElement("message")]
        public string Message { get; set; }

        [XmlElement("timestamp")]
        public string Timestamp { get; set; }

        [XmlElement("correlationId")]
        public string CorrelationId { get; set; }

        [XmlElement("path")]
        public string Path { get; set; }
    }
}
