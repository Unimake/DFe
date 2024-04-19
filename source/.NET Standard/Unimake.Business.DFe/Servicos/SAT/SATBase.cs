using System;
using System.IO;
using System.Linq;
using System.Reflection;
//using Unimake.SAT.Contract;
//using Unimake.SAT.Utility;
//using Marca = Unimake.SAT.Enuns;

namespace Unimake.Business.DFe.Servicos.SAT
{
    /// <summary>
    /// Abstração do serviço de comunicação com equipamento SAT
    /// </summary>
    public abstract class SATBase : ISAT
    {
        /// <summary>
        /// Senha definida pelo contribuinte no software de ativação
        /// </summary>
        protected string CodigoAtivacao { get; set; }

        /// <summary>
        /// Gera valor aleatório para numero de sessão
        /// </summary>
        public int NumeroSessao
        {
            get
            {
                var numeroSessao = new Random();
                return numeroSessao.Next(100000, 999999);
            }
        }

        /// <summary>
        /// Marca do aparelho SAT
        /// </summary>
        protected MarcaEquipamentoSAT MarcaEquipamento { get; set; }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="marcaEquipamento">Marca do equipamento</param>
        /// <param name="codigoAtivacao">Código de ativação</param>
        public SATBase(MarcaEquipamentoSAT marcaEquipamento, string codigoAtivacao)
        {
            MarcaEquipamento = marcaEquipamento;
            CodigoAtivacao = codigoAtivacao;
            //LoadResource(fabricante, Environment.Is64BitProcess);
        }

        /// <summary>
        /// O contribuinte deverá associar a assinatura do Aplicativo Comercial com o SAT através da função AssociarAssinatura.
        /// </summary>
        /// <param name="CNPJvalue">
        /// CNPJ da empresa desenvolvedora do Aplicativo Comercial + CNPJ do Emitente (vide 2.1.3 da Especificação de Requisitos)
        /// </param>
        /// <param name="assinaturaCNPJs">
        /// Assinatura digital conjunto "CNPJ Software House" + "CNPJ do estabelecimento comercial". (vide 2.1.3 da Especificação de Requisitos)
        /// </param>
        /// <returns></returns>
        public abstract string AssociarAssinatura(string CNPJvalue, string assinaturaCNPJs);

        /// <summary>
        /// Esta função faz parte do processo de ativação do Equipamento SAT e será responsável por enviar ao SAT qual o tipo de ativação será efetuada pelo Contribuinte. SubComando: 1 - Tipo de Certificado = AC-SAT/SEFAZ 2 - Tipo de Certificado = ICP-BRASIL 3 - Renovação do Certificado ICP-BRASIL
        /// </summary>
        /// <param name="subComando">
        /// Identificador do tipo de Certificado, descritos na tabela 15
        /// </param>
        /// <param name="CNPJ">CNPJ do contribuinte, somente números</param>
        /// <param name="cUF">Código do Estado da Federação, segundo tabela do IBGE, onde o SAT será ativado</param>
        /// <returns></returns>
        public abstract string AtivarSAT(int subComando, string CNPJ, int cUF);

        /// <summary>
        /// Documentação não encontrada
        /// </summary>
        /// <returns></returns>
        public abstract string AtualizarSoftwareSAT();

        /// <summary>
        /// O Aplicativo Comercial ou outro software fornecido pelo Fabricante poderá realizar o
        /// bloqueio operacional do Equipamento SAT.
        /// </summary>
        /// <returns></returns>
        public abstract string BloquearSAT();

        /// <summary>
        /// O envio dos dados de cancelamento da venda ocorrerá de acordo com as definições a seguir.
        /// </summary>
        /// <param name="chave">
        /// Chave de acesso do CF-e-SAT a ser cancelado precedida do literal 'CFe' (vide 4.7 da
        /// Especificação de Requisitos)
        /// </param>
        /// <param name="dadosCancelamento">
        /// refere-se aos dados da venda gerados pelo AC e utilizados para compor o CF-e-SAT de
        /// cancelamento(vide 4.2.3 da Especificação de Requisitos)
        /// </param>
        /// <returns></returns>
        public abstract string CancelarUltimaVenda(string chave, string dadosCancelamento);

        /// <summary>
        /// O AC, ou outro software fornecido pelo Fabricante, poderá configurar a interface de
        /// comunicação do Equipamento SAT com a rede local do estabelecimento comercial através do
        /// envio de um arquivo de configuração no padrão XML.
        /// </summary>
        /// <param name="dadosConfiguracao">Arquivo de configuração no formato XML.</param>
        /// <returns></returns>
        public abstract string ConfigurarInterfaceDeRede(string dadosConfiguracao);

        /// <summary>
        /// O AC poderá verificar se a última sessão requisitada foi processada em caso de não
        /// recebimento do retorno da operação.O equipamento SAT-CF-e retornará exatamente o
        /// resultado da sessão consultada.
        /// </summary>
        /// <param name="cNumeroDeSessao">Número de sessão a ser consultado no SAT-CF-e</param>
        /// <returns></returns>
        public abstract string ConsultarNumeroSessao(int cNumeroDeSessao);

        /// <summary>
        /// Esta função é usada para testes de comunicação entre o AC e o Equipamento SAT.
        /// Header: char* ConsultarSAT(int numeroSessao);
        /// </summary>
        /// <returns></returns>
        public abstract string ConsultarSAT();

        /// <summary>
        /// Essa função é responsável por verificar a situação de funcionamento do Equipamento SAT
        /// </summary>
        /// <returns></returns>
        public abstract string ConsultarStatusOperacional();

        /// <summary>
        /// O Aplicativo Comercial ou outro software fornecido pelo Fabricante poderá realizar o
        /// desbloqueio operacional do Equipamento SAT.
        /// </summary>
        /// <returns></returns>
        public abstract string DesbloquearSAT();

        /// <summary>
        /// Esta função faz parte do processo de envio dos dados de venda do AC para o Equipamento SAT.
        /// </summary>
        /// <param name="dadosVenda">
        /// refere-se aos dados de venda gerados pelo AC e utilizados para compor o CF-e-SAT. (2.1.4
        /// da Especificação de Requisitos)
        /// </param>
        /// <returns></returns>
        public abstract string EnviarDadosVenda(string dadosVenda);

        /// <summary>
        /// O Aplicativo Comercial poderá extrair os arquivos de registro do Equipamento SAT por meio
        /// da função ExtrairLogs.
        /// </summary>
        /// <returns></returns>
        public abstract string ExtrairLogs();

        /// <summary>
        /// Esta função consiste em um teste de comunicação entre o AC, o Equipamento SAT e a SEFAZ.
        /// </summary>
        /// <param name="dadosVenda">
        /// refere-se aos dados de venda fictícios gerados pelo AC e utilizados para compor o
        /// CF-e-SAT de teste. (vide 2.1.4 da Especificação de Requisitos)
        /// </param>
        /// <returns></returns>
        public abstract string TesteFimAFim(string dadosVenda);

        /// <summary>
        /// O Aplicativo Comercial ou outro software fornecido pelo Fabricante poderá realizar a
        /// troca do código de ativação a qualquer momento.
        /// </summary>
        /// <param name="opcao">
        /// Refere-se a opção do conteúdo do parâmetro "codigoDeAtivacao", sendo : 1 - Código de
        /// Ativação 2 - Código de Ativação de Emergência
        /// </param>
        /// <param name="novoCodigo">Novo código de ativação escolhido pelo contribuinte</param>
        /// <param name="confNovoCodigo">Confirmação do novo código de ativação</param>
        /// <returns></returns>
        public abstract string TrocarCodigoDeAtivacao(string opcao, string novoCodigo, string confNovoCodigo);
    }
}