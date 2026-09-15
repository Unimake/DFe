using System.IO;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;
using Xunit;
using NFeABIAutorizacaoSinc = Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc;
using NFeABIStatusServico = Unimake.Business.DFe.Servicos.NFeABI.StatusServico;

namespace Unimake.DFe.Test.NFeABI.Validacao
{
    /// <summary>
    /// Testes do contrato público final da NFeABI.
    /// </summary>
    public class PublicApiTest
    {
        /// <summary>
        /// Garante que o carregamento documentado permanece disponível na API pública.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void DeveCarregarNFeABIPelaApiPublica()
        {
            var documento = new Business.DFe.Xml.NFeABI.NFeABI()
                .LoadFromFile(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");

            Assert.NotNull(documento);
            Assert.Equal(ModeloDFe.NFeABI, documento.InfNFeABI.Ide.Mod);
            Assert.Equal(TipoAmbiente.Homologacao, documento.InfNFeABI.Ide.TpAmb);

            var peloConteudo = new Business.DFe.Xml.NFeABI.NFeABI()
                .LoadFromXML(File.ReadAllText(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml"));

            Assert.Equal(documento.InfNFeABI.Chave, peloConteudo.InfNFeABI.Chave);
        }

        /// <summary>
        /// Garante que os construtores tipados dos serviços publicados continuam públicos.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ServicosPublicadosDevemExporConstrutoresTipados()
        {
            var status = typeof(NFeABIStatusServico).GetConstructor(new[] { typeof(ConsStatServNFeABI), typeof(Configuracao) });
            var autorizacao = typeof(NFeABIAutorizacaoSinc).GetConstructor(new[] { typeof(Business.DFe.Xml.NFeABI.NFeABI), typeof(Configuracao) });

            Assert.NotNull(status);
            Assert.NotNull(autorizacao);
            Assert.True(typeof(NFeABIStatusServico).IsPublic);
            Assert.True(typeof(NFeABIAutorizacaoSinc).IsPublic);
        }
    }
}
