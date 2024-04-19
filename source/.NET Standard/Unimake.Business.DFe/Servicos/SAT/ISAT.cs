namespace Unimake.Business.DFe.Servicos.SAT
{
    /// <summary>
    /// Interface SAT São Paulo
    /// </summary>
    internal interface ISAT
    {
        string AssociarAssinatura(string CNPJvalue, string assinaturaCNPJs);
        string AtivarSAT(int subComando, string CNPJ, int cUF);
        string AtualizarSoftwareSAT();
        string BloquearSAT();
        string CancelarUltimaVenda(string chave, string dadosCancelamento);
        string ConfigurarInterfaceDeRede(string dadosConfiguracao);
        string ConsultarNumeroSessao(int cNumeroDeSessao);
        string ConsultarSAT();
        string ConsultarStatusOperacional();
        string DesbloquearSAT();
        string EnviarDadosVenda(string dadosVenda);
        string ExtrairLogs();
        string TesteFimAFim(string dadosVenda);
        string TrocarCodigoDeAtivacao(string opcao, string novoCodigo, string confNovoCodigo);
    }
}