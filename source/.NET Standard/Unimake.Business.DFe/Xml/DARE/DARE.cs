#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DAREUnico")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// DARE - SP - Documento de Arrecadação de Receitas Estaduais
    /// Envio individual
    /// </summary>

    [Serializable()]

    [XmlRoot("Dare", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DAREUnico : XMLBase

        /// <summary>
        /// O CNPJ da entidade
        /// </summary>
    {
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// O CPF do responsável, se aplicável.
        /// </summary>

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        /// <summary>
        /// 
        /// </summary>

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        /// <summary>
        /// A cidade do endereço da entidade.
        /// </summary>

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        /// <summary>
        ///  O código de barras de 44 posições.
        /// </summary>

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        /// <summary>
        ///  O código de barras de 48 posições.
        /// </summary>

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        /// <summary>
        ///  A data de vencimento do documento.
        /// </summary>

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        /// <summary>
        /// Informações sobre o documento para impressão, se aplicável.
        /// </summary>

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        /// <summary>
        /// O endereço da entidade.
        /// </summary>

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        /// <summary>
        /// Informações sobre erros relacionados à solicitação
        /// </summary>

        [XmlElement("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Indica se um PDF deve ser gerado.
        /// </summary>

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        /// <summary>
        /// O número de controle do DARE principal.
        /// </summary>

        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        /// <summary>
        /// Observações adicionais sobre o pagamento.
        /// </summary>

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// 
        /// </summary>

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        /// <summary>
        /// 
        /// </summary>

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        /// <summary>
        /// Código PIX para pagamento via cópia e cola.
        /// </summary>

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        /// <summary>
        /// 
        /// </summary>

        [XmlElement("possiveisReceitas")]
        public string PossiveisReceitas { get; set; }

        /// <summary>
        /// Razão social da entidade.
        /// </summary>

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        /// <summary>
        /// Informações sobre a receita tributária.
        /// </summary>

        [XmlElement("receita")]
        public ReceitaDARE Receita { get; set; }

        /// <summary>
        /// Referência para o documento.
        /// </summary>

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        ///  Número de telefone para contato..
        /// </summary>

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade federativa do endereço da entidade.
        /// </summary>

        [XmlElement("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do documento.
        /// </summary>

        [XmlElement("valor")]
        public string Valor { get; set; }

        /// <summary>
        ///  Valor dos juros aplicados.
        /// </summary>

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        /// <summary>
        /// Valor da multa aplicada.
        /// </summary>

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        /// <summary>
        /// Valor total a ser pago, incluindo juros e multa.
        /// </summary>

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        /// <summary>
        ///  Informações adicionais na linha 06, se aplicável.
        /// </summary>

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        /// <summary>
        ///  Informações adicionais na linha 08, se aplicável.
        /// </summary>

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        /// <summary>
        /// 
        /// </summary>

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }
    }
}
