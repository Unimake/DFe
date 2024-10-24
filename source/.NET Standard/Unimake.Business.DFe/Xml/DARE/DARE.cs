#pragma warning disable CS1591

using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.DARE
{
    /// <summary>
    /// DARE - SP - Documento de Arrecadação de Receitas Estaduais
    /// Envio individual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARE")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Dare", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARE : XMLBase
    {
        [XmlIgnore]
        [JsonIgnore]
        public string Versao { get; set; } = "1.00";

        /// <summary>
        /// Número de controle do DARE principal. 
        /// Este é um identificador único para o DARE, usado para rastreamento e referência.
        /// </summary>
        [XmlElement("numeroControleDarePrincipal")]
        [JsonProperty("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        /// <summary>
        /// Indica se um PDF do DARE deve ser gerado. 
        /// Pode ser um valor "sim" ou "não" que determina se o documento deve ser criado em formato PDF.
        /// </summary>
        [XmlElement("gerarPDF")]
        [JsonProperty("gerarPDF")]
        public bool GerarPDF { get; set; }

        /// <summary>
        ///  Código de barras com 44 posições associado ao DARE. 
        /// Este código é utilizado para a leitura e processamento automático do documento.
        /// </summary>
        [XmlElement("codigoBarra44")]
        [JsonProperty("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        /// <summary>
        ///  Código de barras com 48 posições associado ao DARE. 
        /// Este código é utilizado para a leitura e processamento automático do documento.
        /// </summary>
        [XmlElement("codigoBarra48")]
        [JsonProperty("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        /// <summary>
        /// O CNPJ da entidade a quem o DARE se refere.
        /// Este é um identificador único para pessoas jurídicas no Brasil.
        /// </summary>
        [XmlElement("cnpj")]
        [JsonProperty("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// O CPF do responsável pelo DARE, se aplicável. 
        /// Este é um identificador único para pessoas físicas no Brasil.
        /// </summary>
        [XmlElement("cpf")]
        [JsonProperty("cpf")]
        public string Cpf { get; set; }

        /// <summary>
        /// Nome da cidade onde a entidade está localizada. 
        /// Este campo descreve a localidade do endereço da entidade.
        /// </summary>
        [XmlElement("cidade")]
        [JsonProperty("cidade")]
        public string Cidade { get; set; }

        /// <summary>
        ///  Data de vencimento do DARE, informando quando o pagamento deve ser realizado.
        /// Este campo deve estar no formato apropriado para datas.
        /// </summary>
        [XmlIgnore]
        [JsonIgnore]
#if INTEROP
        public DateTime DataVencimento { get; set; }
#else
        public DateTimeOffset DataVencimento { get; set; }
#endif

        [XmlElement("dataVencimento")]
        [JsonProperty("dataVencimento")]
        public string DataVencimentoField 
        {
            get => DataVencimento.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DataVencimento = DateTime.Parse(value);
#else
            set => DataVencimento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informações adicionais sobre o documento para impressão, se aplicável. 
        /// Pode incluir instruções ou dados específicos necessários para a impressão do DARE
        /// </summary>
        [XmlElement("documentoImpressao")]
        [JsonProperty("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        /// <summary>
        /// Endereço completo da entidade que emitiu o DARE. 
        /// Este campo inclui informações como rua, número, complemento e bairro.
        /// </summary>
        [XmlElement("endereco")]
        [JsonProperty("endereco")]
        public string Endereco { get; set; }

        /// <summary>
        /// Código PIX que pode ser usado para pagamento via cópia e cola. 
        /// Este código é utilizado para facilitar pagamentos utilizando o sistema PIX.
        /// </summary>
        [XmlElement("pixCopiaCola")]
        [JsonProperty("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        /// <summary>
        /// Razão social da entidade que emitiu o DARE. 
        /// Este é o nome legal da entidade, geralmente uma empresa ou organização.
        /// </summary>
        [XmlElement("razaoSocial")]
        [JsonProperty("razaoSocial")]
        public string RazaoSocial { get; set; }

        /// <summary>
        /// Informações detalhadas sobre a receita tributária associada ao DARE. 
        /// Pode incluir descrições, códigos e outros dados relevantes sobre a receita.
        /// </summary>
        [XmlElement("receita")]
        [JsonProperty("receita")]
        public ReceitaDARE Receita { get; set; }

        /// <summary>
        /// Informações sobre erros relacionados à solicitação do DARE. 
        /// Este campo fornece detalhes sobre problemas ou falhas encontradas durante o processamento.
        /// </summary>
        [XmlElement("erro")]
        [JsonProperty("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Observações adicionais relacionadas ao pagamento do DARE. 
        /// Pode incluir comentários ou informações específicas que devem ser consideradas.
        /// </summary>
        [XmlElement("observacao")]
        [JsonProperty("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Referência adicional para o DARE. 
        /// Este campo pode incluir um número ou descrição que ajuda a identificar o documento de forma única.
        /// </summary>
        [XmlElement("referencia")]
        [JsonProperty("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        ///  Número de telefone para contato relacionado ao DARE. 
        /// Este é o número pelo qual a entidade pode ser contatada para questões relacionadas ao documento.
        /// </summary>
        [XmlElement("telefone")]
        [JsonProperty("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade Federativa (UF) onde a entidade está localizada. 
        /// Este campo indica o estado brasileiro em que a entidade está registrada.
        /// </summary>
        [XmlElement("uf")]
        [JsonProperty("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do DARE. 
        /// Este é o valor base a ser pago, antes da adição de juros e multas.
        /// </summary>
        [XmlIgnore]
        [JsonIgnore]
        public double Valor { get; set; }

        [XmlElement("valor")]
        [JsonProperty("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Converter.ToDouble(value);
        }

        /// <summary>
        ///  Valor dos juros aplicados ao DARE. 
        /// Este é o valor adicional cobrado como juros sobre o valor principal.
        /// </summary>
        [XmlIgnore]
        [JsonIgnore]
        public double ValorJuros { get; set; }

        [XmlElement("valorJuros")]
        [JsonProperty("valorJuros")]
        public string ValorJurosField
        {
            get => ValorJuros.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorJuros = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da multa aplicada ao DARE. 
        /// Este é o valor adicional cobrado como multa sobre o valor principal.
        /// </summary>
        [XmlIgnore]
        [JsonIgnore]
        public double ValorMulta { get; set; }

        [XmlElement("valorMulta")]
        [JsonProperty("valorMulta")]
        public string ValorMultaField
        {
            get => ValorMulta.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorMulta = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total a ser pago, incluindo juros e multa. 
        /// Este é o valor final que deve ser pago, somando o valor principal, juros e multas.
        /// </summary>
        [XmlIgnore]
        [JsonIgnore]
        public double ValorTotal { get; set; }

        [XmlElement("valorTotal")]
        [JsonProperty("valorTotal")]
        public string ValorTotalField
        {
            get => ValorTotal.ToString("F2", CultureInfo.InvariantCulture);
            set => ValorTotal = Converter.ToDouble(value);
        }

        /// <summary>
        ///  Informações adicionais na linha 06 do DARE, se aplicável. 
        /// Este campo pode incluir dados ou instruções específicas localizadas nesta linha do documento.
        /// </summary>
        [XmlElement("linha06")]
        [JsonProperty("linha06")]
        public string Linha06 { get; set; }

        /// <summary>
        ///  Informações adicionais na linha 08 do DARE, se aplicável. 
        /// Este campo pode incluir dados ou instruções específicas localizadas nesta linha do documento.
        /// </summary>
        [XmlElement("linha08")]
        [JsonProperty("linha08")]
        public string Linha08 { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpj() => !string.IsNullOrEmpty(Cnpj);
        public bool ShouldSerializeCpf() => !string.IsNullOrEmpty(Cpf);
        public bool ShouldSerializeCidade() => !string.IsNullOrEmpty(Cidade);
        public bool ShouldSerializeCodigoBarra44() => !string.IsNullOrEmpty(CodigoBarra44);
        public bool ShouldSerializeCodigoBarra48() => !string.IsNullOrEmpty(CodigoBarra48);
        public bool ShouldSerializeDocumentoImpressao() => !string.IsNullOrEmpty(DocumentoImpressao);
        public bool ShouldSerializeEndereco() => !string.IsNullOrEmpty(Endereco);
        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);
        public bool ShouldSerializePixCopiaCola() => !string.IsNullOrEmpty(PixCopiaCola);
        public bool ShouldSerializeTelefone() => !string.IsNullOrEmpty(Telefone);
        public bool ShouldSerializeUf() => !string.IsNullOrEmpty(Uf);
        public bool ShouldSerializeLinha06() => !string.IsNullOrEmpty(Linha06);
        public bool ShouldSerializeLinha08() => !string.IsNullOrEmpty(Linha08);

        #endregion ShouldSerialize
    }
}
