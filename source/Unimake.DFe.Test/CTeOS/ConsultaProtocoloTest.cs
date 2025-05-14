using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTeOS;
using Unimake.Business.DFe.Xml.CTeOS;
using Xunit;

namespace Unimake.DFe.Test.CTeOS
{
    /// <summary>
    /// Testar o serviço de consulta protocolo do CTeOS
    /// </summary>
    public class ConsultaProtocoloTest
    {
        
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void ConsultaProtocoloConstrutor(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var chave = ((int)ufBrasil).ToString() + "200106117473000150550010000606641403753210";

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS, 
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaProtocolo = new ConsultaProtocolo(chave, tipoAmbiente, configuracao);
            consultaProtocolo.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(consultaProtocolo.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(consultaProtocolo.Result.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            if (consultaProtocolo.Result.ProtCTe != null)
            {
                if (consultaProtocolo.Result.ProtCTe.InfProt != null)
                {
                    Assert.True(consultaProtocolo.Result.ProtCTe.InfProt.ChCTe.Equals(chave), "Webservice retornou uma chave da CTe diferente da enviada na consulta.");
                }
            }
        }

    }
}