using System;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;
using Autorizacao = Unimake.Business.DFe.Servicos.DCe.AutorizacaoSinc;
using Consulta = Unimake.Business.DFe.Servicos.DCe.ConsultaProtocolo;
using XmlDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe.Validacao
{
    [Trait("DFe", "DCe")]
    [Trait("Categoria", "Offline")]
    public class DCeContratosOfflineTest
    {
        [Fact]
        public void SchemasOficiaisDevemConservarOsBytesEOrigemDocumentados() => DCeSchemasOficiaisOffline.ConferirProveniencia();

        [Theory]
        [InlineData(false, false)]
        [InlineData(false, true)]
        [InlineData(true, false)]
        [InlineData(true, true)]
        public void EmissaoOficialExigeCodigoENomeDoPais(bool codigo, bool nome)
        {
            using var cenario = new DCeCenarioOffline();
            var xml = DCeCenarioOffline.Documento(codigo, nome, TipoEmitenteDCe.AppFisco).GerarXML();
            DCeCenarioOffline.IncluirSolicitacaoFisco(xml);
            cenario.Assinar(xml);
            var erros = DCeSchemasOficiaisOffline.Validar(xml, "Emissao", "dce_v1.00.xsd");
            if (codigo && nome) Assert.Empty(erros);
            else
            {
                Assert.NotEmpty(erros);
                Assert.All(erros, erro => Assert.Contains(codigo ? "xPais" : "cPais", erro));
            }
        }

        [Theory]
        [InlineData(false, false)]
        [InlineData(false, true)]
        [InlineData(true, false)]
        [InlineData(true, true)]
        public void TipoDocumentoDoPacoteConsultaPreservaPaisOpcional(bool codigo, bool nome)
        {
            using var cenario = new DCeCenarioOffline();
            var xml = cenario.Assinar(DCeCenarioOffline.Documento(codigo, nome).GerarXML());
            Assert.Empty(DCeSchemasOficiaisOffline.Validar(xml, "Consulta", "dceTiposBasico_v1.00.xsd", true));
        }

        [Theory]
        [InlineData(TipoEmitenteDCe.AppFisco)]
        [InlineData(TipoEmitenteDCe.EmissorProprio)]
        public void EmissaoOficialExigeSolicitacaoEnquantoTipoConsultaNaoAExige(TipoEmitenteDCe tipo)
        {
            using var cenario = new DCeCenarioOffline();
            var xml = cenario.Assinar(DCeCenarioOffline.Documento(tipo: tipo).GerarXML());
            var erros = DCeSchemasOficiaisOffline.Validar(xml, "Emissao", "dce_v1.00.xsd");
            Assert.NotEmpty(erros);
            Assert.All(erros, erro => Assert.Contains("infSolicDCe", erro));
            Assert.Empty(DCeSchemasOficiaisOffline.Validar(xml, "Consulta", "dceTiposBasico_v1.00.xsd", true));
        }

        [Fact]
        public void CaracterizacaoAtualDoModeloNaoRepresentaSolicitacaoDoFisco()
        {
            var original = DCeCenarioOffline.Documento(tipo: TipoEmitenteDCe.AppFisco).GerarXML();
            DCeCenarioOffline.IncluirSolicitacaoFisco(original);
            var modelo = XMLUtility.Deserializar<XmlDCe>(original);
            var regravado = modelo.GerarXML();
            // Lacuna conhecida, não uma regra fiscal desejada: revisar esta caracterização
            // quando houver suporte público autorizado para o grupo de solicitação.
            Assert.Single(original.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace).Cast<XmlNode>());
            Assert.Empty(regravado.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace));
            Assert.Equal(original.GetElementsByTagName("infDCe")[0].OuterXml, regravado.GetElementsByTagName("infDCe")[0].OuterXml);
        }

        [Theory]
        [InlineData(false, TipoEmitenteDCe.AppFisco)]
        [InlineData(true, TipoEmitenteDCe.AppFisco)]
        [InlineData(false, TipoEmitenteDCe.EmissorProprio)]
        [InlineData(true, TipoEmitenteDCe.EmissorProprio)]
        public void CaracterizacaoAtualDaAutorizacaoEValidacaoDiretaUsaLeiautePermissivo(bool pais, TipoEmitenteDCe tipo)
        {
            using var cenario = new DCeCenarioOffline();
            using var servico = new Autorizacao(DCeCenarioOffline.Documento(pais, pais, tipo), cenario.Configuracao());
            // Construção, assinatura e validação reais, sem chamar Executar.
            Assert.Equal(Servico.DCeAutorizacaoSinc, servico.Configuracoes.Servico);
            DCeCenarioOffline.VerificarAssinatura(servico.ConteudoXMLAssinado);
            var xml = DCeCenarioOffline.Documento(pais, pais, tipo).GerarXML();
            var resultado = new ValidarEstruturaXML().ValidarServico(xml, cenario.Configuracao());
            Assert.True(resultado.Validado);
            Assert.Equal("1", resultado.StatusValidacao);
            DCeCenarioOffline.VerificarAssinatura(resultado.XmlAssinado);
            Assert.NotEmpty(DCeSchemasOficiaisOffline.Validar(resultado.XmlAssinado, "Emissao", "dce_v1.00.xsd"));
            Assert.Equal(pais ? 1 : 0, resultado.XmlAssinado.GetElementsByTagName("enderDest")[0]
                .ChildNodes.Cast<XmlNode>().Count(x => x.LocalName == "cPais"));
        }

        [Fact]
        public void ConsultaDeSituacaoUsaContratoDeChaveSemExigirDocumentoDeEmissao()
        {
            using var cenario = new DCeCenarioOffline();
            var chave = DCeCenarioOffline.Documento().InfDCe.Chave;
            var pedido = new ConsSitDCe { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, ChDCe = chave };
            using var consulta = new Consulta(pedido, cenario.Configuracao());
            Assert.Empty(DCeSchemasOficiaisOffline.Validar(consulta.ConteudoXMLAssinado, "Consulta", "consSitDCe_v1.00.xsd"));
            Assert.Equal(chave, consulta.ConteudoXMLAssinado.GetElementsByTagName("chDCe")[0].InnerText);
            Assert.Empty(consulta.ConteudoXMLAssinado.GetElementsByTagName("infDCe"));
            Assert.Empty(consulta.ConteudoXMLAssinado.GetElementsByTagName("Signature"));
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void RespostaConsultaPreservaProtocoloOpcionalSemDocumentoCompleto(bool autorizado)
        {
            var chave = DCeCenarioOffline.Documento().InfDCe.Chave;
            var protocolo = autorizado ? DCeCenarioOffline.Protocolo(chave, Convert.ToBase64String(new byte[20])) : null;
            var resposta = DCeCenarioOffline.RespostaConsulta(chave, protocolo);
            var xml = resposta.GerarXML();
            Assert.Empty(DCeSchemasOficiaisOffline.Validar(xml, "Consulta", "retConsSitDCe_v1.00.xsd"));
            using var cenario = new DCeCenarioOffline();
            using var consulta = new Consulta(new ConsSitDCe
            {
                Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, ChDCe = chave
            }, cenario.Configuracao());
            consulta.RetornoWSString = xml.OuterXml;
            consulta.RetornoWSXML = xml;
            var retorno = consulta.Result;
            Assert.Equal(autorizado ? 100 : 217, retorno.CStat);
            Assert.Equal(chave, retorno.ChDCe);
            Assert.Equal(autorizado, retorno.ProtDCe != null);
            Assert.Empty(xml.GetElementsByTagName("DCe"));
        }

        [Theory]
        [InlineData(false, 100)]
        [InlineData(true, 100)]
        [InlineData(false, 150)]
        [InlineData(true, 150)]
        public void DistribuicaoDaAutorizacaoPreservaDocumentoAssinadoEContratoDeArquivo(bool pais, int status)
        {
            using var cenario = new DCeCenarioOffline();
            using var servico = new Autorizacao(DCeCenarioOffline.Documento(pais, pais), cenario.Configuracao());
            var chave = servico.DCe.InfDCe.Chave;
            var protocolo = DCeCenarioOffline.Protocolo(chave, DCeCenarioOffline.Digest(servico.ConteudoXMLAssinado), status);
            var retorno = new RetDCe
            {
                Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, VerAplic = "AF05-OFFLINE", CStat = status,
                XMotivo = "RESPOSTA SINTETICA", CUF = UFBrasil.PR, DhRecbto = protocolo.InfProt.DhRecbto, ProtDCe = protocolo
            };
            servico.RetornoWSString = retorno.GerarXML().OuterXml;
            ConferirDistribuicao(servico, chave, pais, status);
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void DistribuicaoRecuperadaDaConsultaPreservaOriginalSemReautorizar(bool pais)
        {
            using var cenario = new DCeCenarioOffline();
            using var servico = new Autorizacao(DCeCenarioOffline.Documento(pais, pais), cenario.Configuracao());
            var chave = servico.DCe.InfDCe.Chave;
            var protocolo = DCeCenarioOffline.Protocolo(chave, DCeCenarioOffline.Digest(servico.ConteudoXMLAssinado));
            servico.RetConsSitDCe.Add(DCeCenarioOffline.RespostaConsulta(chave, protocolo));
            ConferirDistribuicao(servico, chave, pais, 100);
        }

        [Fact]
        public void ConsultaSemProtocoloDaChaveNaoDeveGerarDistribuicao()
        {
            using var cenario = new DCeCenarioOffline();
            using var servico = new Autorizacao(DCeCenarioOffline.Documento(), cenario.Configuracao());
            var outro = DCeCenarioOffline.Documento();
            outro.InfDCe.Ide.NDC = 51;
            outro.InfDCe.Id = null;
            var outraChave = outro.InfDCe.Chave;
            servico.RetConsSitDCe.Add(DCeCenarioOffline.RespostaConsulta(outraChave,
                DCeCenarioOffline.Protocolo(outraChave, Convert.ToBase64String(new byte[20]))));
            using var stream = new MemoryStream();
            servico.GravarXmlDistribuicao(stream);
            Assert.Empty(servico.DCeProcResults);
            Assert.Equal(0, stream.Length);
        }

        private static void ConferirDistribuicao(Autorizacao servico, string chave, bool pais, int status)
        {
            var distribuicao = Assert.Single(servico.DCeProcResults).Value;
            Assert.Equal(chave + "-procDCe.xml", distribuicao.NomeArquivoDistribuicao);
            using var stream = new MemoryStream();
            servico.GravarXmlDistribuicao(stream);
            var documento = DCeCenarioOffline.Ler(System.Text.Encoding.UTF8.GetString(stream.ToArray()).TrimStart('\uFEFF'));
            Assert.Equal("dceProc", documento.DocumentElement.LocalName);
            Assert.Equal(DCeCenarioOffline.Namespace, documento.DocumentElement.NamespaceURI);
            var schema = new ValidarSchema();
            schema.Validar(documento, "DCe.procDCe_v1.00.xsd");
            Assert.True(schema.Success, schema.ErrorMessage);
            DCeCenarioOffline.VerificarAssinatura(documento);
            Assert.Equal(DCeCenarioOffline.Digest(servico.ConteudoXMLAssinado), DCeCenarioOffline.Digest(documento));
            var modelo = XMLUtility.Deserializar<DCeProc>(documento);
            Assert.Equal(chave, modelo.DCe.InfDCe.Chave);
            Assert.Equal(chave, modelo.ProtDCe.InfProt.ChDCe);
            Assert.Equal(status, modelo.ProtDCe.InfProt.CStat);
            Assert.Equal(pais ? "1058" : null, modelo.DCe.InfDCe.Dest.EnderDest.CPais);
            Assert.Empty(documento.GetElementsByTagName("infSolicDCe", DCeCenarioOffline.Namespace));
        }
    }
}
