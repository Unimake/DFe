using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Unimake.DFe.Test.DCe.Validacao;
using Xunit;
using DocumentoDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe.Serializacao
{
    /// <summary>Preservação do pedido explícito do Fisco, sem alteração das regras de emissão.</summary>
    [Trait("DFe", "DCe")]
    [Trait("Categoria", "Offline")]
    public class InfSolicDCeTest
    {
        /// <summary>Texto, namespace e posição do grupo permanecem estáveis no round-trip.</summary>
        [Theory]
        [InlineData("")]
        [InlineData("{\"origem\":\"APP FISCO SINTETICO\",\"texto\":\"ação <&> \\\"teste\\\"\"}")]
        [InlineData("  {\"pedido\":\"SINTETICO\"}  ")]
        [InlineData("CONTEUDO PRESERVADO SEM INTERPRETACAO")]
        public void GrupoExplicitoPreservaTextoNamespaceEOrdem(string texto)
        {
            var documento = DCeCenarioOffline.Documento(tipo: TipoEmitenteDCe.AppFisco);
            documento.InfSolicDCe = new InfSolicDCe { XSolic = texto };
            var xml = documento.GerarXML();
            var grupo = Assert.IsType<XmlElement>(xml.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace)[0]);
            Assert.Same(xml.DocumentElement, grupo.ParentNode);
            Assert.Equal(texto, grupo["xSolic", DCeCenarioOffline.Namespace].InnerText);
            Assert.Equal(new[] { "infDCe", "infSolicDCe", "infDCeSupl" },
                xml.DocumentElement.ChildNodes.Cast<XmlNode>().Select(x => x.LocalName));
            var lido = XMLUtility.Deserializar<DocumentoDCe>(xml);
            Assert.Equal(texto, lido.InfSolicDCe.XSolic);
            Assert.Equal(xml.DocumentElement.OuterXml, lido.GerarXML().DocumentElement.OuterXml);
        }

        /// <summary>Consumidores que não informam o grupo mantêm o XML e a validação anteriores.</summary>
        [Theory]
        [InlineData(TipoEmitenteDCe.AppFisco)]
        [InlineData(TipoEmitenteDCe.EmissorProprio)]
        public void AusenciaDoGrupoMantemContratoAnteriorSemPreenchimentoAutomatico(TipoEmitenteDCe tipo)
        {
            using var cenario = new DCeCenarioOffline();
            var documento = DCeCenarioOffline.Documento(tipo: tipo);
            Assert.Null(documento.InfSolicDCe);
            var original = cenario.Assinar(documento.GerarXML());
            var lido = XMLUtility.Deserializar<DocumentoDCe>(original);
            Assert.Null(lido.InfSolicDCe);
            var regravado = lido.GerarXML();
            Assert.Empty(regravado.GetElementsByTagName("infSolicDCe"));
            Assert.Equal(original.DocumentElement.OuterXml, regravado.DocumentElement.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(regravado);
            var resultado = new ValidarEstruturaXML().ValidarServico(documento.GerarXML(), cenario.Configuracao());
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
        }

        /// <summary>A montagem local conserva o documento assinado e dispensa certificado.</summary>
        [Theory]
        [InlineData(100, true)]
        [InlineData(150, true)]
        [InlineData(100, false)]
        [InlineData(150, false)]
        public void DistribuicaoPreservaDocumentoCompletoSemCertificadoOuNovaEmissao(int status, bool grupo)
        {
            XmlDocument original;
            DCeProc distribuicao;
            string chave;
            using (var cenario = new DCeCenarioOffline())
            {
                var documento = DCeCenarioOffline.Documento(tipo: TipoEmitenteDCe.AppFisco);
                if (grupo) documento.InfSolicDCe = new InfSolicDCe { XSolic = "{\"pedido\":\"SINTETICO_OFFLINE\"}" };
                original = cenario.Assinar(documento.GerarXML());
                var lido = XMLUtility.Deserializar<DocumentoDCe>(original);
                chave = lido.InfDCe.Chave;
                distribuicao = new DCeProc
                {
                    Versao = "1.00", DCe = lido,
                    ProtDCe = DCeCenarioOffline.Protocolo(chave, DCeCenarioOffline.Digest(original), status)
                };
            }
            // O certificado sintético já foi descartado: só se associa o protocolo ao original.
            var anterior = original.OuterXml;
            using var gravador = new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc();
            using var stream = new MemoryStream();
            gravador.GravarXmlDistribuicao(stream, distribuicao.GerarXML().OuterXml);
            var proc = DCeCenarioOffline.Ler(Encoding.UTF8.GetString(stream.ToArray()).TrimStart('\uFEFF'));
            Assert.Equal("dceProc", proc.DocumentElement.LocalName);
            Assert.Equal(chave + "-procDCe.xml", distribuicao.NomeArquivoDistribuicao);
            Assert.Equal(original.DocumentElement.OuterXml,
                proc.GetElementsByTagName("DCe", DCeCenarioOffline.Namespace)[0].OuterXml);
            Assert.Equal(grupo ? 1 : 0, proc.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace).Count);
            Assert.Equal(anterior, original.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(proc);
            var relido = XMLUtility.Deserializar<DCeProc>(proc).GerarXML();
            Assert.Equal(proc.DocumentElement.OuterXml, relido.DocumentElement.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(relido);
        }

        /// <summary>O modelo não altera as recusas de schema nos construtores públicos.</summary>
        [Theory]
        [InlineData(TipoEmitenteDCe.AppFisco)]
        [InlineData(TipoEmitenteDCe.EmissorProprio)]
        public void SuporteAoModeloNaoAlteraDecisaoDoSchemaDeEmissao(TipoEmitenteDCe tipo)
        {
            using var cenario = new DCeCenarioOffline();
            var documento = DCeCenarioOffline.Documento(tipo: tipo);
            documento.InfSolicDCe = new InfSolicDCe { XSolic = "{\"pedido\":\"SINTETICO_OFFLINE\"}" };
            var original = documento.GerarXML();
            var anterior = original.OuterXml;
            var resultado = new ValidarEstruturaXML().ValidarServico(original, cenario.Configuracao());
            Assert.False(resultado.Validado);
            Assert.Contains("infSolicDCe", resultado.MensagemRetorno);
            var entrada = DCeCenarioOffline.Ler(anterior);
            Assert.Equal(entrada.GetElementsByTagName("infDCe")[0].OuterXml, original.GetElementsByTagName("infDCe")[0].OuterXml);
            Assert.Equal(entrada.GetElementsByTagName("infSolicDCe")[0].OuterXml, original.GetElementsByTagName("infSolicDCe")[0].OuterXml);
            Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() =>
                new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(documento, cenario.Configuracao()));
            Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() =>
                new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(anterior, cenario.Configuracao()));
        }
    }
}
