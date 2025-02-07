using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCom;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    /// <summary>
    /// Testasr o serviço de envio da NFCom
    /// </summary>
    public class AutorizacaoSincTest
    {
        /// <summary>
        /// Enviar uma NFCom no modo síncrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a NFCom</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a NFCom</param>
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(UFBrasil.SVRS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SVRS, TipoAmbiente.Homologacao)]
        public void EnviarNFComSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var arqXML = "..\\..\\..\\NFCom\\Resources\\"+ "nfcom_completa-nfcom.xml";

            //Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.NFCom>(conteudoXML.OuterXml);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                CodigoUF = (int)ufBrasil,
                TipoEmissao = TipoEmissao.ContingenciaSVCRS,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSincNFCom = new AutorizacaoSinc(xml, configuracao);
            autorizacaoSincNFCom.Executar();
        }

    }
}
