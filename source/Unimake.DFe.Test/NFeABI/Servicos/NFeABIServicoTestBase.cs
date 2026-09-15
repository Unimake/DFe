using Unimake.Business.DFe.Servicos;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    /// <summary>
    /// Configuração comum dos testes de integração dos serviços NFeABI.
    /// </summary>
    public abstract class NFeABIServicoTestBase
    {
        /// <summary>
        /// Cria a configuração para execução em uma unidade federativa.
        /// </summary>
        /// <param name="ufBrasil">Unidade federativa.</param>
        /// <returns>Configuração com certificado do ambiente de testes.</returns>
        protected static Configuracao CriarConfiguracao(UFBrasil ufBrasil)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.NFeABI,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)ufBrasil,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }
    }
}
