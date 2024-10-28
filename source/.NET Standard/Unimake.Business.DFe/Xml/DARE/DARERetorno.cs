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
    /// Retorno DARE Único (negativo e positivo)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARERetorno")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("DareRetorno", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARERetorno : XMLBase
    {
        [XmlElement("DARE")]
        public DAREUnicoRetorno DARE { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DAREUnicoRetorno")]
    [ComVisible(true)]
#endif
    public class DAREUnicoRetorno
    {
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
        public ReceitaDARERetorno Receita { get; set; }

        [XmlElement("erro")]
        public ErroRetorno Erro { get; set; }

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ErroRetorno")]
    [ComVisible(true)]
#endif
    public class ErroRetorno
    {
        /// <summary>
        /// Mensagens de erro associadas ao DARE. 
        /// </summary>
        [XmlElement("mensagens")]
        [JsonProperty("mensagens")]
        public MensagensErro Mensagens { get; set; }

        /// <summary>
        /// Indica se o processamento do DARE está OK ou não. 
        /// Um valor que indica o status geral do processamento.
        /// </summary>
        [XmlElement("estaOk")]
        [JsonProperty("estaOk")]
        public bool? EstaOk { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeEstaOk() => EstaOk.HasValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Classe das mensagens de erro
    /// Pode incluir uma ou mais mensagens explicando os problemas encontrados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.MensagensErro")]
    [ComVisible(true)]
#endif
    public class MensagensErro
    {
        [XmlElement("mensagem")]
        [JsonProperty("mensagem")]
        public List<string> Mensagem { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddMensagem(string item)
        {
            if (Mensagem == null)
            {
                Mensagem = new List<string>();
            }

            Mensagem.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Mensagem (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Mensagem</returns>
        public string GetMensagem(int index)
        {
            if ((Mensagem?.Count ?? 0) == 0)
            {
                return default;
            };

            return Mensagem[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Mensagem
        /// </summary>
        public int GetMensagemCount => (Mensagem != null ? Mensagem.Count : 0);
#endif
    }

    /// <summary>
    /// Classe de retorno do serviço de Receitas DARE - SP
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ReceitaDARERetorno")]
    [ComVisible(true)]
#endif
    public class ReceitaDARERetorno : ReceitaDARE { }
}
