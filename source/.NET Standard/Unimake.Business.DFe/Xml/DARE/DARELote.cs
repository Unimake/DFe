#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARELote")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// DARE - SP - Documento de Arrecadação de Receitas Estaduais
    /// Envio individual
    /// </summary>

    [Serializable()]
    [XmlRoot("DareLote", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = false)]
    public class DARELote : XMLBase

        /// <summary>
        /// Dados do contribuinte não cadastrado no sistema. 
        /// Contém informações relevantes sobre o contribuinte que não está registrado no cadastro oficial.
    {
        [XmlElement("dadosContribuinteNaoCadastrado")]


        public DadosContribuinteNaoCadastrado DadosContribuinteNaoCadastrado { get; set; }

        /// <summary>
        /// Informações sobre erros relacionados ao processamento do lote DARE. 
        /// Fornece detalhes sobre problemas ou falhas encontradas.

        [XmlElement("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Tipo de agrupamento dos "filhotes" no lote DARE. 
        /// Este campo define como os subelementos ou documentos são agrupados no lote.
        /// </summary>

        [XmlElement("tipoAgrupamentoFilhotes")]
        public string TipoAgrupamentoFilhotes { get; set; }

        /// <summary>
        /// Lista de itens para geração no lote DARE. 
        /// Contém os diferentes elementos que compõem o lote para processamento.
        /// </summary>

        [XmlElement("itensParaGeracao")]

        public List<ItensParaGeracao> ItensParaGeracao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensParaGeracao(ItensParaGeracao item)
        {
            if (ItensParaGeracao == null)
            {
                ItensParaGeracao = new List<ItensParaGeracao>();
            }

            ItensParaGeracao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensParaGeracao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensParaGeracao</returns>
        public ItensParaGeracao GetItensParaGeracao(int index)
        {
            if ((ItensParaGeracao?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensParaGeracao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensParaGeracao
        /// </summary>
        public int GetItensParaGeracaoCount => (ItensParaGeracao != null ? ItensParaGeracao.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DadosContribuinteNaoCadastrado")]
    [ComVisible(true)]
#endif

    public class DadosContribuinteNaoCadastrado

        /// <summary>
        /// Número de controle do DARE principal. 
        /// Este é um identificador único utilizado para rastreamento e referência do DARE.
        /// </summary>
    {
        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        /// <summary>
        /// Indica se deve ser gerado um PDF do DARE. 
        /// Pode ser "sim" ou "não", determinando se o documento deve ser criado em formato PDF.
        /// </summary>

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        /// <summary>
        /// Código de barras de 44 posições associado ao DARE. 
        /// Utilizado para leitura e processamento automático do documento.
        /// </summary>

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        /// <summary>
        /// Código de barras de 48 posições associado ao DARE. 
        /// Utilizado para leitura e processamento automático do documento.
        /// </summary>

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        /// <summary>
        /// CNPJ da entidade responsável pelo DARE. 
        /// Um identificador único para pessoas jurídicas no Brasil.
        /// </summary>

        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// CPF do responsável pelo DARE, se aplicável. 
        /// Um identificador único para pessoas físicas no Brasil.
        /// </summary>

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        // <summary>
        /// Código de Receita Federal relacionado ao DARE. 
        /// Utilizado para identificar o tipo de receita tributária.
        /// </summary>

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        // <summary>
        /// Nome da cidade onde a entidade está localizada. 
        /// Descreve a localidade do endereço da entidade.
        /// </summary>

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        /// <summary>
        /// Data de vencimento do DARE, informando quando o pagamento deve ser realizado. 
        /// Este campo deve estar no formato apropriado para datas.
        /// </summary>

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        //// <summary>
        /// Informações adicionais sobre o documento para impressão, se aplicável. 
        /// Pode incluir instruções ou dados específicos necessários para a impressão do DARE.
        /// </summary>

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        /// <summary>
        /// Endereço completo da entidade. 
        /// Inclui informações como rua, número, complemento e bairro.
        /// </summary>

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        /// <summary>
        /// Identificador da funcionalidade ou sistema de origem do DARE. 
        /// Indica qual sistema ou módulo gerou o documento.
        /// </summary>

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        /// <summary>
        /// Inscrição Estadual da entidade. 
        /// Identifica a empresa ou entidade em nível estadual para fins de tributação.
        /// </summary>

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        /// <summary>
        /// Código PIX para pagamento via cópia e cola. 
        /// Facilita pagamentos utilizando o sistema PIX.
        /// </summary>

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        /// <summary>
        /// Lista de possíveis receitas tributárias associadas ao DARE. 
        /// Pode incluir códigos ou descrições das receitas que o DARE abrange.
        /// </summary>

        [XmlElement("possiveisReceitas")]
        public List<string> PossiveisReceitas { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPossiveisReceitas(PossiveisReceitas item)
        {
            if (PossiveisReceitas == null)
            {
                PossiveisReceitas = new List<PossiveisReceitas>();
            }

            PossiveisReceitas.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PossiveisReceitas (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PossiveisReceitas</returns>
        public PossiveisReceitas GetPossiveisReceitas(int index)
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
        /// Razão social da entidade responsável pelo DARE. 
        /// Nome legal da entidade, geralmente uma empresa ou organização.
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
        /// Referência adicional para o DARE. 
        /// Pode incluir um número ou identificador que ajude na rastreabilidade do documento.
        /// </summary>

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        /// Número de telefone para contato da entidade responsável pelo DARE. 
        /// Utilizado para comunicações relacionadas ao documento.
        /// </summary>

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade Federativa (UF) onde a entidade está localizada. 
        /// Indica o estado brasileiro da sede da entidade.
        /// </summary>

        [XmlElement("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do documento relacionado ao DARE. 
        /// Este é o valor a ser pago antes da aplicação de juros e multas.
        /// </summary>

        [XmlElement("valor")]
        public string Valor { get; set; }

        /// <summary>
        /// Valor dos juros aplicados ao valor principal. 
        /// Indica os acréscimos devidos por atraso ou outras condições.
        /// </summary>

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        /// <summary>
        /// Valor da multa aplicada sobre o valor principal. 
        /// Inclui penalidades por atraso ou infrações específicas.
        /// </summary>

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        /// <summary>
        /// Valor total a ser pago, incluindo o valor principal, juros e multa. 
        /// Este é o valor final que deve ser quitado.
        /// </summary>

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        /// <summary>
        /// Informações adicionais que devem ser incluídas na linha 06 do DARE, se aplicável. 
        /// Pode conter detalhes extras ou notas explicativas.
        /// </summary>

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        /// <summary>
        /// Informações adicionais que devem ser incluídas na linha 08 do DARE, se aplicável. 
        /// Pode conter detalhes extras ou notas explicativas.
        /// </summary>

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        /// <summary>
        /// Número da guia associada ao DARE. 
        /// Este número é usado para identificar e rastrear a guia de pagamento.
        /// </summary>

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }

    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.Erro")]
    [ComVisible(true)]
#endif

    public class Erro

        /// <summary>
        /// Indica se o processamento do DARE está OK ou não. 
        /// Um valor que indica o status geral do processamento.
        /// </summary>
    {
        [XmlElement("estaOk")]
        public string EstaOk { get; set; }

        /// <summary>
        /// Mensagens de erro associadas ao DARE. 
        /// Pode incluir uma ou mais mensagens explicando os problemas encontrados.
        /// </summary>

        [XmlElement("mensagens")]
        public string Mensagens { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.ItensParaGeracao")]
    [ComVisible(true)]
#endif

    public class ItensParaGeracao

        /// <summary>
        /// Lista de objetos DARE para geração. 
        /// Cada item nesta lista representa um DARE individual que compõe o lote.
        /// </summary>
    {

        [XmlElement("Dare")]
        public List<DARE> DARE { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARE")]
    [ComVisible(true)]
#endif

    public class DARE

        /// <summary>
        /// Número de controle do DARE principal. 
        /// Um identificador único utilizado para rastreamento e referência do DARE.
        /// </summary>
    {
        [XmlElement("numeroControleDarePrincipal")]
        public string NumeroControleDarePrincipal { get; set; }

        /// <summary>
        /// Indica se deve ser gerado um PDF do DARE. 
        /// Pode ser "sim" ou "não", determinando se o documento deve ser criado em formato PDF.
        /// </summary>

        [XmlElement("gerarPDF")]
        public string GerarPDF { get; set; }

        /// <summary>
        /// Código de barras de 44 posições associado ao DARE. 
        /// Utilizado para leitura e processamento automático do documento.
        /// </summary>

        [XmlElement("codigoBarra44")]
        public string CodigoBarra44 { get; set; }

        /// <summary>
        /// Código de barras de 48 posições associado ao DARE. 
        /// Utilizado para leitura e processamento automático do documento.
        /// </summary>

        [XmlElement("codigoBarra48")]
        public string CodigoBarra48 { get; set; }

        /// <summary>
        /// CNPJ da entidade responsável pelo DARE. 
        /// Um identificador único para pessoas jurídicas no Brasil.
        /// </summary>

        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// CPF do responsável pelo DARE, se aplicável. 
        /// Um identificador único para pessoas físicas no Brasil.
        /// </summary>

        [XmlElement("cpf")]
        public string Cpf { get; set; }

        /// <summary>
        /// Código de Receita Federal relacionado ao DARE. 
        /// Utilizado para identificar o tipo de receita tributária.
        /// </summary>

        [XmlElement("cpr")]
        public string Cpr { get; set; }

        /// <summary>
        /// Nome da cidade onde a entidade está localizada. 
        /// Descreve a localidade do endereço da entidade.
        /// </summary>

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        /// <summary>
        /// Data de vencimento do DARE, informando quando o pagamento deve ser realizado. 
        /// Este campo deve estar no formato apropriado para datas.
        /// </summary>

        [XmlElement("dataVencimento")]
        public string DataVencimento { get; set; }

        /// <summary>
        /// Informações adicionais sobre o documento para impressão, se aplicável. 
        /// Pode incluir instruções ou dados específicos necessários para a impressão do DARE.
        /// </summary>

        [XmlElement("documentoImpressao")]
        public string DocumentoImpressao { get; set; }

        /// <summary>
        /// Endereço completo da entidade. 
        /// Inclui informações como rua, número, complemento e bairro.
        /// </summary>

        [XmlElement("endereco")]
        public string Endereco { get; set; }

        /// <summary>
        /// Identificador da funcionalidade ou sistema de origem do DARE. 
        /// Indica qual sistema ou módulo gerou o documento.
        /// </summary>

        [XmlElement("funcionalidadeOrigem")]
        public string FuncionalidadeOrigem { get; set; }

        /// <summary>
        /// Inscrição Estadual da entidade. 
        /// Identifica a empresa ou entidade em nível estadual para fins de tributação.
        /// </summary>

        [XmlElement("inscricaoEstadual")]
        public string InscricaoEstadual { get; set; }

        /// <summary>
        /// Código PIX para pagamento via cópia e cola. 
        /// Facilita pagamentos utilizando o sistema PIX.
        /// </summary>

        [XmlElement("pixCopiaCola")]
        public string PixCopiaCola { get; set; }

        /// <summary>
        /// Receita associada ao DARE, especificando o tipo de tributo ou valor a ser recolhido.
        /// </summary>

        [XmlElement("possiveisReceitas")]
        public string PossiveisReceitas { get; set; }

        /// <summary>
        /// Razão social da entidade responsável pelo DARE. 
        /// Nome legal da entidade, geralmente uma empresa ou organização.
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
        /// Referência adicional para o DARE. 
        /// Pode incluir um número ou identificador que ajude na rastreabilidade do documento.
        /// </summary>

        [XmlElement("referencia")]
        public string Referencia { get; set; }

        /// <summary>
        /// Número de telefone para contato da entidade responsável pelo DARE. 
        /// Utilizado para comunicações relacionadas ao documento.
        /// </summary>

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        /// <summary>
        /// Unidade Federativa (UF) onde a entidade está localizada. 
        /// Indica o estado brasileiro da sede da entidade.
        /// </summary>

        [XmlElement("uf")]
        public string Uf { get; set; }

        /// <summary>
        /// Valor principal do documento relacionado ao DARE. 
        /// Este é o valor a ser pago antes da aplicação de juros e multas.
        /// </summary>

        [XmlElement("valor")]
        public string Valor { get; set; }

        /// <summary>
        /// /// Valor dos juros aplicados ao valor principal.
        /// Indica os acréscimos devidos por atraso ou outras condições.
        /// </summary>

        [XmlElement("valorJuros")]
        public string ValorJuros { get; set; }

        /// <summary>
        /// Valor da multa aplicada sobre o valor principal.
        /// Inclui penalidades por atraso ou infrações específicas.
        /// </summary>

        [XmlElement("valorMulta")]
        public string ValorMulta { get; set; }

        /// <summary>
        /// Valor total a ser pago, incluindo o valor principal, juros e multa.
        /// Este é o valor final que deve ser quitado.
        /// </summary>

        [XmlElement("valorTotal")]
        public string ValorTotal { get; set; }

        /// <summary>
        /// Informações adicionais que devem ser incluídas na linha 06 do DARE, se aplicável.
        /// Pode conter detalhes extras ou notas explicativas.
        /// </summary>

        [XmlElement("linha06")]
        public string Linha06 { get; set; }

        /// <summary>
        /// Informações adicionais que devem ser incluídas na linha 08 do DARE, se aplicável.
        /// Pode conter detalhes extras ou notas explicativas.
        /// </summary>

        [XmlElement("linha08")]
        public string Linha08 { get; set; }

        /// <summary>
        /// Número da guia associada ao DARE.
        /// Este número é usado para identificar e rastrear a guia de pagamento.
        /// </summary>

        [XmlElement("numeroGuia")]
        public string NumeroGuia { get; set; }
    }
}