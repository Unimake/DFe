using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;
using Xunit;
using NFeABIAutorizacaoSinc = Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    /// <summary>
    /// Testa o serviço de autorização síncrona da NFeABI em homologação.
    /// </summary>
    public class AutorizacaoSincTest : NFeABIServicoTestBase
    {
        /// <summary>
        /// Envia uma NFeABI tipada pelo pipeline real de homologação.
        /// </summary>
        /// <param name="arquivoXml">Arquivo XML usado como massa.</param>
        [Theory]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "AutorizacaoSinc")]
        [InlineData(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml")]
        public void AutorizacaoSinc(string arquivoXml)
        {
            var xml = new XmlDocument();
            xml.Load(arquivoXml);

            var nfeABI = new Business.DFe.Xml.NFeABI.NFeABI().LerXML<Business.DFe.Xml.NFeABI.NFeABI>(xml);
            nfeABI.Signature = null;
            nfeABI.InfNFeABI.Ide.TpAmb = TipoAmbiente.Homologacao;
            var configuracao = CriarConfiguracao(nfeABI.InfNFeABI.Ide.CUF);
            var autorizacao = new NFeABIAutorizacaoSinc(nfeABI, configuracao);

            autorizacao.Executar();

            Assert.Equal((int)nfeABI.InfNFeABI.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.IsType<RetNFeABI>(autorizacao.Result);
            Assert.Equal(TipoAmbiente.Homologacao, autorizacao.Result.TpAmb);
        }
    }
}
