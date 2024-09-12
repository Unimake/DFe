#pragma warning disable CS1591

using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARE")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// DARE - SP - Documento de Arrecadação de Receitas Estaduais
    /// Envio individual
    /// </summary>
    [Serializable()]
    [XmlRoot("Dare", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARE : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.00";

        /// Número de controle do DARE principal. 
        /// Este é um identificador único para o DARE, usado para rastreamento e referência.
        /// </summary>
        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        /// <summary>
        /// Indica se um PDF do DARE deve ser gerado. 
        /// Pode ser um valor "sim" ou "não" que determina se o documento deve ser criado em formato PDF.
        /// </summary>
        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        /// <summary>
        ///  Código de barras com 44 posições associado ao DARE. 
        /// Este código é utilizado para a leitura e processamento automático do documento.
        /// </summary>
        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        /// <summary>
        ///  Código de barras com 48 posições associado ao DARE. 
        /// Este código é utilizado para a leitura e processamento automático do documento.
        /// </summary
        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        /// <summary>
        /// O CNPJ da entidade a quem o DARE se refere.
        /// Este é um identificador único para pessoas jurídicas no Brasil.
        /// </summary>
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// O CPF do responsável pelo DARE, se aplicável. 
        /// Este é um identificador único para pessoas físicas no Brasil.
        /// </summary>
        [XmlElement("cpf")]
        public string Cpf { get; set; }

        /// <summary>
        /// Código de Receita Federal relacionado ao DARE. 
        /// Este código é utilizado para identificar o tipo de receita tributária.
        /// </summary>
        [XmlElement("cpr")]
        public string Cpr { get; set; }

        /// <summary>
        /// Nome da cidade onde a entidade está localizada. 
        /// Este campo descreve a localidade do endereço da entidade.
        /// </summary>
        [XmlElement("cidade")]
        public string Cidade { get; set; }
        /// <summary>
        ///  Data de vencimento do DARE, informando quando o pagamento deve ser realizado.
        /// Este campo deve estar no formato apropriado para datas.
        /// </summary>
        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        /// <summary>
        /// Informações adicionais sobre o documento para impressão, se aplicável. 
        /// Pode incluir instruções ou dados específicos necessários para a impressão do DARE
        /// </summary>
        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        /// <summary>
        /// Endereço completo da entidade que emitiu o DARE. 
        /// Este campo inclui informações como rua, número, complemento e bairro.
        /// </summary>
        [XmlElement("endereco")]
        public string Endereco { get; set; }

        /// <summary>
        ///  Identificador da funcionalidade ou sistema de origem do DARE. 
        /// Este campo pode indicar qual sistema ou módulo gerou o documento.
        /// </summary>
        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        /// <summary>
        /// Inscrição Estadual da entidade. 
        /// Este número identifica a empresa ou entidade em nível estadual para fins de tributação.
        /// </summary>
        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        /// <summary>
        /// Código PIX que pode ser usado para pagamento via cópia e cola. 
        /// Este código é utilizado para facilitar pagamentos utilizando o sistema PIX.
        /// </summary>
        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        /// <summary>
        /// Informações sobre receitas tributárias possíveis associadas ao DARE. 
        /// Este campo pode incluir códigos ou descrições das receitas que o DARE abrange.
        /// </summary>
        [XmlElement("possiveisReceitas")]
        public List<string> PossiveisReceitas { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPossiveisReceitas(string item)
        {
            if (PossiveisReceitas == null)
            {
                PossiveisReceitas = new List<string>();
            }

            PossiveisReceitas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PossiveisReceitas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PossiveisReceitas</returns>
        public string GetPossiveisReceitas(int index)
        {
            if ((PossiveisReceitas?.Count ?? 0) == 0)
            {
                return default;
            };

            return PossiveisReceitas[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PossiveisReceitas
        /// </summary>
        public int GetPossiveisReceitasCount => (PossiveisReceitas != null ? PossiveisReceitas.Count : 0);
#endif

        /// <summary>
        /// Razão social da entidade que emitiu o DARE. 
        /// Este é o nome legal da entidade, geralmente uma empresa ou organização.
        /// </summary>
        [XmlElement("razaoSocial")]
        public string RazaoSocial { get; set; }

        /// <summary>
        /// Informações detalhadas sobre a receita tributária associada ao DARE. 
        /// Pode incluir descrições, códigos e outros dados relevantes sobre a receita.
        /// </summary>
        [XmlElement("receita")]
        public ReceitaDARE Receita { get; set; }

        /// <summary>
        /// Informações sobre erros relacionados à solicitação do DARE. 
        /// Este campo fornece detalhes sobre problemas ou falhas encontradas durante o processamento.
        /// </summary>
        [XmlElement("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Observações adicionais relacionadas ao pagamento do DARE. 
        /// Pode incluir comentários ou informações específicas que devem ser consideradas.
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Referência adicional para o DARE. 
        /// Este campo pode incluir um número ou descrição que ajuda a identificar o documento de forma única.
        /// </summary>
        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        ///  Número de telefone para contato relacionado ao DARE. 
        /// Este é o número pelo qual a entidade pode ser contatada para questões relacionadas ao documento.
        /// </summary>
        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade Federativa (UF) onde a entidade está localizada. 
        /// Este campo indica o estado brasileiro em que a entidade está registrada.
        /// </summary>
        [XmlElement("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do DARE. 
        /// Este é o valor base a ser pago, antes da adição de juros e multas.
        /// </summary>
        [XmlElement("valor")]
        public string Valor { get; set; }

        /// <summary>
        ///  Valor dos juros aplicados ao DARE. 
        /// Este é o valor adicional cobrado como juros sobre o valor principal.
        /// </summary>
        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        /// <summary>
        /// Valor da multa aplicada ao DARE. 
        /// Este é o valor adicional cobrado como multa sobre o valor principal.
        /// </summary>
        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        /// <summary>
        /// Valor total a ser pago, incluindo juros e multa. 
        /// Este é o valor final que deve ser pago, somando o valor principal, juros e multas.
        /// </summary>
        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        /// <summary>
        ///  Informações adicionais na linha 06 do DARE, se aplicável. 
        /// Este campo pode incluir dados ou instruções específicas localizadas nesta linha do documento.
        /// </summary>
        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        /// <summary>
        ///  Informações adicionais na linha 08 do DARE, se aplicável. 
        /// Este campo pode incluir dados ou instruções específicas localizadas nesta linha do documento.
        /// </summary>
        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        /// <summary>
        /// Número da guia do DARE. 
        /// Este é um identificador único que pode ser utilizado para localizar ou referenciar o documento.
        /// </summary>
        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }
    }
}
