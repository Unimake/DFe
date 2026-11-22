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
        [InlineData(TipoEmitenteDCe.Marketplace)]
        [InlineData(TipoEmitenteDCe.EmissorProprio)]
        [InlineData(TipoEmitenteDCe.Transportadora)]
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
            var schema = new ValidarSchema();
            schema.Validar(proc, "DCe.procDCe_v1.00.xsd");
            Assert.True(schema.Success, schema.ErrorMessage);
            Assert.Equal(original.DocumentElement.OuterXml,
                proc.GetElementsByTagName("DCe", DCeCenarioOffline.Namespace)[0].OuterXml);
            Assert.Equal(grupo ? 1 : 0, proc.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace).Count);
            Assert.Equal(anterior, original.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(proc);
            var relido = XMLUtility.Deserializar<DCeProc>(proc).GerarXML();
            Assert.Equal(proc.DocumentElement.OuterXml, relido.DocumentElement.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(relido);
        }

        /// <summary>Emissão pelo Fisco aceita e preserva a solicitação explícita nos caminhos públicos.</summary>
        [Theory]
        [InlineData("")]
        [InlineData("  {\"pedido\":\"AÇÃO SINTÉTICA <&>\"}  ")]
        public void EmissaoDoFiscoAceitaSolicitacaoOpcional(string texto)
        {
            using var cenario = new DCeCenarioOffline();
            var documento = DCeCenarioOffline.Documento(tipo: TipoEmitenteDCe.AppFisco);
            documento.InfSolicDCe = new InfSolicDCe { XSolic = texto };
            var original = cenario.Assinar(documento.GerarXML());
            var anterior = original.OuterXml;
            var infDCeOriginal = original.GetElementsByTagName("infDCe", DCeCenarioOffline.Namespace)[0].OuterXml;
            var resultado = new ValidarEstruturaXML().ValidarServico(original, cenario.Configuracao());
            Assert.True(resultado.Validado, resultado.MensagemRetorno);

            using var tipado = new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(documento, cenario.Configuracao());
            using var textual = new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(anterior, cenario.Configuracao());
            foreach (var assinado in new[] { resultado.XmlAssinado, tipado.ConteudoXMLAssinado, textual.ConteudoXMLAssinado })
            {
                Assert.Equal(texto, XMLUtility.Deserializar<DocumentoDCe>(assinado).InfSolicDCe.XSolic);
                Assert.Equal(infDCeOriginal, assinado.GetElementsByTagName("infDCe", DCeCenarioOffline.Namespace)[0].OuterXml);
                Assert.Equal(new[] { "infDCe", "infSolicDCe", "infDCeSupl", "Signature" },
                    assinado.DocumentElement.ChildNodes.Cast<XmlNode>().Select(x => x.LocalName));
                DCeCenarioOffline.VerificarAssinatura(assinado);
            }
        }

        /// <summary>A estrutura compartilhada admite o grupo, mas a emissão o reserva ao Fisco.</summary>
        [Theory]
        [InlineData(TipoEmitenteDCe.Marketplace)]
        [InlineData(TipoEmitenteDCe.EmissorProprio)]
        [InlineData(TipoEmitenteDCe.Transportadora)]
        public void EmissaoDeOutroEmitenteRecusaSolicitacaoSemAlterarOriginal(TipoEmitenteDCe tipo)
        {
            using var cenario = new DCeCenarioOffline();
            var documento = DCeCenarioOffline.Documento(tipo: tipo);
            documento.InfSolicDCe = new InfSolicDCe { XSolic = "{\"pedido\":\"SINTETICO_OFFLINE\"}" };
            var original = cenario.Assinar(documento.GerarXML());
            var anterior = original.OuterXml;
            var schema = new ValidarSchema();
            schema.Validar(original, "DCe.dce_v1.00.xsd");
            Assert.True(schema.Success, schema.ErrorMessage);

            var resultado = new ValidarEstruturaXML().ValidarServico(original, cenario.Configuracao());
            Assert.False(resultado.Validado);
            Assert.Contains("tpEmit=0", resultado.MensagemRetorno);
            Assert.Equal(anterior, original.OuterXml);
            DCeCenarioOffline.VerificarAssinatura(original);
            var erroTipado = Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() =>
                new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(documento, cenario.Configuracao()));
            var erroTextual = Assert.Throws<Unimake.Exceptions.ValidarXMLException>(() =>
                new Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc(anterior, cenario.Configuracao()));
            Assert.Contains("tpEmit=0", erroTipado.Message);
            Assert.Contains("tpEmit=0", erroTextual.Message);
        }

        /// <summary>Quando informado, xSolic segue o limite estrutural de 2.000 caracteres.</summary>
        [Theory]
        [InlineData(2000, true)]
        [InlineData(2001, false)]
        public void SchemaLimitaTamanhoDaSolicitacao(int tamanho, bool aceito)
        {
            using var cenario = new DCeCenarioOffline();
            var documento = DCeCenarioOffline.Documento(tipo: TipoEmitenteDCe.AppFisco);
            documento.InfSolicDCe = new InfSolicDCe { XSolic = new string('A', tamanho) };
            var original = cenario.Assinar(documento.GerarXML());
            var schema = new ValidarSchema();
            schema.Validar(original, "DCe.dce_v1.00.xsd");
            if (aceito) Assert.True(schema.Success, schema.ErrorMessage);
            else
            {
                Assert.False(schema.Success);
                Assert.Contains("xSolic", schema.ErrorMessage);
            }
        }
    }
}
