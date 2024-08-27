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
    /// 
    /// </summary>

    [Serializable()]

    [XmlRoot("Dare", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DAREUnico : XMLBase
    {
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        [XmlElement("erro")]
        public Erro Erro { get; set; }

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        [XmlElement("possiveisReceitas")]
        public string PossiveisReceitas { get; set; }

        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        [XmlElement("receita")]
        public ReceitaDARE Receita { get; set; }

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        [XmlElement("uf")]
        public string Uf { get; set; }

        [XmlElement("valor")]
        public string Valor { get; set; }

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }
    }
}
