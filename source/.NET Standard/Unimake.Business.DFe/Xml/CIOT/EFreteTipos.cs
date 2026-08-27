#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CIOT
{
    public partial class DeclaracaoOperacaoTransporte
    {
        [XmlElement("IdOperacaoCliente")]
        public string IdOperacaoCliente { get; set; }

        [XmlElement("MatrizCNPJ")]
        public string MatrizCNPJ { get; set; }

        [XmlElement("FilialCNPJ")]
        public string FilialCNPJ { get; set; }

        [XmlElement("TipoEmbalagem")]
        public string TipoEmbalagem { get; set; }

        [XmlElement("TipoPagamentoEFrete")]
        public string TipoPagamentoEFrete { get; set; }

        [XmlElement("Motorista")]
        public MotoristaCIOT Motorista { get; set; }

        [XmlElement("Impostos")]
        public ImpostosCIOT Impostos { get; set; }

        [XmlElement("Contratado")]
        public PessoaCIOT Contratado { get; set; }

        [XmlElement("Contratante")]
        public PessoaCIOT Contratante { get; set; }

        [XmlElement("Destinatario")]
        public PessoaCIOT Destinatario { get; set; }

        [XmlElement("Subcontratante")]
        public PessoaCIOT Subcontratante { get; set; }

        [XmlElement("Consignatario")]
        public PessoaCIOT Consignatario { get; set; }

        [XmlElement("TomadorServico")]
        public PessoaCIOT TomadorServico { get; set; }

        [XmlArray("ObservacoesAoTransportador")]
        [XmlArrayItem("Observacao")]
        public List<string> ObservacoesAoTransportador { get; set; }

        [XmlArray("ObservacoesAoCredenciado")]
        [XmlArrayItem("Observacao")]
        public List<string> ObservacoesAoCredenciado { get; set; }

        [XmlElement("EntregaDocumentacao")]
        public string EntregaDocumentacao { get; set; }

        [XmlElement("QuantidadeSaques")]
        public int QuantidadeSaques { get; set; }

        [XmlElement("QuantidadeTransferencias")]
        public int QuantidadeTransferencias { get; set; }

        public bool ShouldSerializeIdOperacaoCliente() => !string.IsNullOrWhiteSpace(IdOperacaoCliente);
        public bool ShouldSerializeMatrizCNPJ() => !string.IsNullOrWhiteSpace(MatrizCNPJ);
        public bool ShouldSerializeFilialCNPJ() => !string.IsNullOrWhiteSpace(FilialCNPJ);
        public bool ShouldSerializeTipoEmbalagem() => !string.IsNullOrWhiteSpace(TipoEmbalagem);
        public bool ShouldSerializeTipoPagamentoEFrete() => !string.IsNullOrWhiteSpace(TipoPagamentoEFrete);
        public bool ShouldSerializeObservacoesAoTransportador() => ObservacoesAoTransportador?.Count > 0;
        public bool ShouldSerializeObservacoesAoCredenciado() => ObservacoesAoCredenciado?.Count > 0;
        public bool ShouldSerializeEntregaDocumentacao() => !string.IsNullOrWhiteSpace(EntregaDocumentacao);
        public bool ShouldSerializeQuantidadeSaques() => QuantidadeSaques > 0;
        public bool ShouldSerializeQuantidadeTransferencias() => QuantidadeTransferencias > 0;
#if INTEROP
        internal bool TemDataInicioViagemEFrete() => DataInicioViagem > DateTime.MinValue;
#else
        internal bool TemDataInicioViagemEFrete() => DataInicioViagem > DateTimeOffset.MinValue;
#endif

#if INTEROP
        public void AddObservacoesAoTransportador(string value) { if (ObservacoesAoTransportador == null) ObservacoesAoTransportador = new List<string>(); ObservacoesAoTransportador.Add(value); }
        public string GetObservacoesAoTransportador(int index) => ObservacoesAoTransportador[index];
        public int GetObservacoesAoTransportadorCount => ObservacoesAoTransportador?.Count ?? 0;
        public void AddObservacoesAoCredenciado(string value) { if (ObservacoesAoCredenciado == null) ObservacoesAoCredenciado = new List<string>(); ObservacoesAoCredenciado.Add(value); }
        public string GetObservacoesAoCredenciado(int index) => ObservacoesAoCredenciado[index];
        public int GetObservacoesAoCredenciadoCount => ObservacoesAoCredenciado?.Count ?? 0;
#endif
    }

    public partial class OrigemDestino
    {
        [XmlElement("DocumentoViagem")]
        public string DocumentoViagem { get; set; }

        [XmlElement("Valores")]
        public ValoresViagemCIOT Valores { get; set; }

        [XmlArray("NotasFiscais")]
        [XmlArrayItem("NotaFiscal")]
        public List<NotaFiscalCIOT> NotasFiscais { get; set; }

        [XmlElement("TipoPagamentoEFrete")]
        public string TipoPagamentoEFrete { get; set; }

        public bool ShouldSerializeDocumentoViagem() => !string.IsNullOrWhiteSpace(DocumentoViagem);
        public bool ShouldSerializeNotasFiscais() => NotasFiscais?.Count > 0;
        public bool ShouldSerializeTipoPagamentoEFrete() => !string.IsNullOrWhiteSpace(TipoPagamentoEFrete);

#if INTEROP
        public void AddNotasFiscais(NotaFiscalCIOT value) { if (NotasFiscais == null) NotasFiscais = new List<NotaFiscalCIOT>(); NotasFiscais.Add(value); }
        public NotaFiscalCIOT GetNotasFiscais(int index) => NotasFiscais[index];
        public int GetNotasFiscaisCount => NotasFiscais?.Count ?? 0;
#endif
    }

    public partial class InfPagamento
    {
        [XmlElement("TipoPagamentoEFrete")]
        public string TipoPagamentoEFrete { get; set; }

        [XmlElement("IdPagamentoCliente")]
        public string IdPagamentoCliente { get; set; }

        [XmlElement("DataDeLiberacao")]
        public string DataDeLiberacao { get; set; }

        [XmlElement("Categoria")]
        public string Categoria { get; set; }

        [XmlElement("Documento")]
        public string Documento { get; set; }

        [XmlElement("TipoConta")]
        public string TipoConta { get; set; }

        [XmlElement("TipoChavePix")]
        public string TipoChavePix { get; set; }

        [XmlElement("InformacaoAdicional")]
        public string InformacaoAdicional { get; set; }

        [XmlElement("CnpjFilialAbastecimento")]
        public string CnpjFilialAbastecimento { get; set; }

        public bool ShouldSerializeIdPagamentoCliente() => !string.IsNullOrWhiteSpace(IdPagamentoCliente);
        public bool ShouldSerializeTipoPagamentoEFrete() => !string.IsNullOrWhiteSpace(TipoPagamentoEFrete);
        public bool ShouldSerializeDataDeLiberacao() => !string.IsNullOrWhiteSpace(DataDeLiberacao);
        public bool ShouldSerializeCategoria() => !string.IsNullOrWhiteSpace(Categoria);
        public bool ShouldSerializeDocumento() => !string.IsNullOrWhiteSpace(Documento);
        public bool ShouldSerializeTipoConta() => !string.IsNullOrWhiteSpace(TipoConta);
        public bool ShouldSerializeTipoChavePix() => !string.IsNullOrWhiteSpace(TipoChavePix);
        public bool ShouldSerializeInformacaoAdicional() => !string.IsNullOrWhiteSpace(InformacaoAdicional);
        public bool ShouldSerializeCnpjFilialAbastecimento() => !string.IsNullOrWhiteSpace(CnpjFilialAbastecimento);
    }

    public partial class ConsultarCIOTGerado
    {
        [XmlElement("MatrizCNPJ")]
        public string MatrizCNPJ { get; set; }

        [XmlElement("IdOperacaoCliente")]
        public string IdOperacaoCliente { get; set; }

        public bool ShouldSerializeMatrizCNPJ() => !string.IsNullOrWhiteSpace(MatrizCNPJ);
        public bool ShouldSerializeIdOperacaoCliente() => !string.IsNullOrWhiteSpace(IdOperacaoCliente);
    }

    public partial class RetConsultarCIOTGerado
    {
        [XmlElement("EstadoCIOT")]
        public string EstadoCIOT { get; set; }

        [XmlElement("Protocolo")]
        public string Protocolo { get; set; }

        public bool ShouldSerializeEstadoCIOT() => !string.IsNullOrWhiteSpace(EstadoCIOT);
        public bool ShouldSerializeProtocolo() => !string.IsNullOrWhiteSpace(Protocolo);
    }

    public partial class ConsultarSituacaoTransportador
    {
        [XmlElement("DataPrevistaFimViagem")]
        public string DataPrevistaFimViagem { get; set; }

        [XmlArray("PlacasConsulta")]
        [XmlArrayItem("Placa")]
        public List<string> PlacasConsulta { get; set; }

        public bool ShouldSerializeDataPrevistaFimViagem() => !string.IsNullOrWhiteSpace(DataPrevistaFimViagem);
        public bool ShouldSerializePlacasConsulta() => PlacasConsulta?.Count > 0;

#if INTEROP
        public void AddPlacasConsulta(string value) { if (PlacasConsulta == null) PlacasConsulta = new List<string>(); PlacasConsulta.Add(value); }
        public string GetPlacasConsulta(int index) => PlacasConsulta[index];
        public int GetPlacasConsultaCount => PlacasConsulta?.Count ?? 0;
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.TelefoneCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class TelefoneCIOT
    {
        [XmlElement("DDD")] public string DDD { get; set; }
        [XmlElement("Numero")] public string Numero { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.TelefonesCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class TelefonesCIOT
    {
        [XmlElement("Celular")] public TelefoneCIOT Celular { get; set; }
        [XmlElement("Fixo")] public TelefoneCIOT Fixo { get; set; }
        [XmlElement("Fax")] public TelefoneCIOT Fax { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.EnderecoCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class EnderecoCIOT
    {
        [XmlElement("Bairro")] public string Bairro { get; set; }
        [XmlElement("Rua")] public string Rua { get; set; }
        [XmlElement("Numero")] public string Numero { get; set; }
        [XmlElement("Complemento")] public string Complemento { get; set; }
        [XmlElement("CEP")] public string CEP { get; set; }
        [XmlElement("CodigoMunicipio")] public string CodigoMunicipio { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.PessoaCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class PessoaCIOT
    {
        [XmlElement("NomeOuRazaoSocial")] public string NomeOuRazaoSocial { get; set; }
        [XmlElement("CpfOuCnpj")] public string CpfOuCnpj { get; set; }
        [XmlElement("RNTRC")] public string RNTRC { get; set; }
        [XmlElement("Endereco")] public EnderecoCIOT Endereco { get; set; }
        [XmlElement("EMail")] public string EMail { get; set; }
        [XmlElement("Telefones")] public TelefonesCIOT Telefones { get; set; }
        [XmlElement("ResponsavelPeloPagamento")] public bool ResponsavelPeloPagamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.MotoristaCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class MotoristaCIOT
    {
        [XmlElement("CpfOuCnpj")] public string CpfOuCnpj { get; set; }
        [XmlElement("CNH")] public string CNH { get; set; }
        [XmlElement("Celular")] public TelefoneCIOT Celular { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.ImpostosCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class ImpostosCIOT
    {
        [XmlElement("IRRF")] public double IRRF { get; set; }
        [XmlElement("SestSenat")] public double SestSenat { get; set; }
        [XmlElement("INSS")] public double INSS { get; set; }
        [XmlElement("ISSQN")] public double ISSQN { get; set; }
        [XmlElement("OutrosImpostos")] public double OutrosImpostos { get; set; }
        [XmlElement("DescricaoOutrosImpostos")] public string DescricaoOutrosImpostos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.ValoresViagemCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class ValoresViagemCIOT
    {
        [XmlElement("TotalOperacao")] public double TotalOperacao { get; set; }
        [XmlElement("TotalViagem")] public double TotalViagem { get; set; }
        [XmlElement("TotalDeAdiantamento")] public double TotalDeAdiantamento { get; set; }
        [XmlElement("TotalDeQuitacao")] public double TotalDeQuitacao { get; set; }
        [XmlElement("Combustivel")] public double Combustivel { get; set; }
        [XmlElement("Pedagio")] public double Pedagio { get; set; }
        [XmlElement("Seguro")] public double Seguro { get; set; }
        [XmlElement("OutrosCreditos")] public double OutrosCreditos { get; set; }
        [XmlElement("JustificativaOutrosCreditos")] public string JustificativaOutrosCreditos { get; set; }
        [XmlElement("OutrosDebitos")] public double OutrosDebitos { get; set; }
        [XmlElement("JustificativaOutrosDebitos")] public string JustificativaOutrosDebitos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.ToleranciaCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class ToleranciaCIOT
    {
        [XmlElement("Tipo")] public string Tipo { get; set; }
        [XmlElement("Valor")] public double Valor { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.DiferencaFreteCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class DiferencaFreteCIOT
    {
        [XmlElement("Tipo")] public string Tipo { get; set; }
        [XmlElement("Base")] public string Base { get; set; }
        [XmlElement("Tolerancia")] public ToleranciaCIOT Tolerancia { get; set; }
        [XmlElement("MargemGanho")] public ToleranciaCIOT MargemGanho { get; set; }
        [XmlElement("MargemPerda")] public ToleranciaCIOT MargemPerda { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual), ProgId("Unimake.Business.DFe.Xml.CIOT.NotaFiscalCIOT"), ComVisible(true)]
#endif
    [Serializable]
    public class NotaFiscalCIOT
    {
        [XmlElement("CnpjEmissor")] public string CnpjEmissor { get; set; }
        [XmlElement("Numero")] public string Numero { get; set; }
        [XmlElement("Serie")] public string Serie { get; set; }
        [XmlElement("Data")] public string Data { get; set; }
        [XmlElement("ValorTotal")] public double ValorTotal { get; set; }
        [XmlElement("ValorDaMercadoriaPorUnidade")] public double ValorDaMercadoriaPorUnidade { get; set; }
        [XmlElement("CodigoNCMNaturezaCarga")] public string CodigoNCMNaturezaCarga { get; set; }
        [XmlElement("DescricaoDaMercadoria")] public string DescricaoDaMercadoria { get; set; }
        [XmlElement("UnidadeDeMedidaDaMercadoria")] public string UnidadeDeMedidaDaMercadoria { get; set; }
        [XmlElement("TipoDeCalculo")] public string TipoDeCalculo { get; set; }
        [XmlElement("ValorDoFretePorUnidadeDeMercadoria")] public double ValorDoFretePorUnidadeDeMercadoria { get; set; }
        [XmlElement("QuantidadeDaMercadoriaNoEmbarque")] public double QuantidadeDaMercadoriaNoEmbarque { get; set; }
        [XmlElement("ToleranciaDePerdaDeMercadoria")] public ToleranciaCIOT ToleranciaDePerdaDeMercadoria { get; set; }
        [XmlElement("DiferencaDeFrete")] public DiferencaFreteCIOT DiferencaDeFrete { get; set; }

        public bool ShouldSerializeValorDoFretePorUnidadeDeMercadoria() => !string.Equals(TipoDeCalculo, "SemQuebra", StringComparison.OrdinalIgnoreCase);
    }
}
