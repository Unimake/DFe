using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.CTe.Servicos
{
    public class StatusServicoOfflineTest
    {
        [Fact]
        [Trait("DFe", "CTe")]
        public void ConstrutorGeraEValidaConsulta400()
        {
            var consulta = new ConsStatServCte
            {
                Versao = "4.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR
            };

            var servico = new StatusServico(consulta, CriarConfiguracao());

            Assert.Equal(Servico.CTeStatusServico, servico.Configuracoes.Servico);
            Assert.Equal("4.00", servico.Configuracoes.SchemaVersao);
            Assert.Equal("consStatServCTe", servico.ConteudoXMLAssinado.DocumentElement.LocalName);
            Assert.Equal("41", servico.ConteudoXMLAssinado.DocumentElement["cUF", "http://www.portalfiscal.inf.br/cte"].InnerText);

            consulta.XServ = "INVALIDO";
            var erro = Assert.Throws<ValidarXMLException>(() => new StatusServico(consulta, CriarConfiguracao()));
            Assert.Contains("xServ", erro.Message);
        }

        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            TipoDFe = TipoDFe.CTe,
            TipoEmissao = TipoEmissao.Normal,
            CodigoUF = (int)UFBrasil.PR
        };
    }
}
