using System;
using System.IO;
using System.Reflection;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;
using Unimake.Exceptions;
using Xunit;
using NFeABIAutorizacaoSinc = Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc;
using NFeABIStatusServico = Unimake.Business.DFe.Servicos.NFeABI.StatusServico;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    /// <summary>
    /// Testes dos serviços publicados da NFeABI.
    /// </summary>
    public class ServicosPublicadosTest
    {
        /// <summary>
        /// Confere a configuração de status diretamente contra o WSDL oficial arquivado.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "StatusServico")]
        public void StatusServicoDeveRefletirWsdlOficial()
        {
            var contrato = LerContratoWsdl("NFeABIStatusServico.wsdl", "nfeabiStatusServico", "nfeabiStatusServicoSoap12In", "nfeabiStatusServicoSoap12Out");
            var consulta = new ConsStatServNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR,
                XServ = "STATUS"
            };

            var servico = new NFeABIStatusServico(consulta, new Configuracao());

            Assert.Equal(TipoDFe.NFeABI, servico.Configuracoes.TipoDFe);
            Assert.Equal(Servico.NFeABIStatusServico, servico.Configuracoes.Servico);
            Assert.Equal(contrato.Endereco, servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(contrato.Acao, servico.Configuracoes.WebActionHomologacao);
            Assert.Equal(contrato.Retorno, servico.Configuracoes.WebTagRetorno);
            Assert.Contains("<nfeabi:" + contrato.Wrapper + ">", servico.Configuracoes.WebSoapString);
            Assert.Contains(contrato.TargetNamespace, servico.Configuracoes.WebSoapString);
        }

        /// <summary>
        /// Garante que todas as UFs resolvem a configuração estadual e sua herança nacional.
        /// </summary>
        /// <param name="uf">Unidade federativa a validar.</param>
        [Theory]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.TO)]
        [Trait("DFe", "NFeABI")]
        public void TodasAsUFsDevemResolverConfiguracaoNacional(UFBrasil uf)
        {
            var consulta = new ConsStatServNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = uf,
                XServ = "STATUS"
            };

            var servico = new NFeABIStatusServico(consulta, new Configuracao());

            Assert.Equal((int)uf, servico.Configuracoes.CodigoUF);
            Assert.Equal(Servico.NFeABIStatusServico, servico.Configuracoes.Servico);
            Assert.Equal("https://homologacao.nfeabi.fazenda.pr.gov.br/nfeabi/NFeABIStatusServico", servico.Configuracoes.WebEnderecoHomologacao);
        }

        /// <summary>
        /// Garante que os arquivos estaduais embutidos são cópias exatas do modelo NFGas.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ArquivosDasUFsDevemRepetirModeloNFGas()
        {
            var assembly = typeof(Configuracao).Assembly;
            var ufs = new[] { "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO" };
            string modelo;

            using (var stream = assembly.GetManifestResourceStream("Unimake.Business.DFe.Servicos.Config.NFGas.AC.xml"))
            using (var reader = new StreamReader(stream))
            {
                modelo = reader.ReadToEnd();
            }

            foreach (var uf in ufs)
            {
                using (var stream = assembly.GetManifestResourceStream("Unimake.Business.DFe.Servicos.Config.NFeABI." + uf + ".xml"))
                {
                    Assert.NotNull(stream);
                    using (var reader = new StreamReader(stream))
                    {
                        Assert.Equal(modelo, reader.ReadToEnd());
                    }
                }
            }
        }

        /// <summary>
        /// Confere a configuração de autorização diretamente contra o WSDL oficial arquivado.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "AutorizacaoSinc")]
        public void AutorizacaoDeveRefletirWsdlOficial()
        {
            var contrato = LerContratoWsdl("NFeABIAutorizacao.wsdl", "nfeabiAutorizacao", "nfeabiAutorizacaoSoap12In", "nfeabiAutorizacaoSoap12Out");
            var documento = LerNFeABI();
            var servico = new NFeABIAutorizacaoSinc(documento, new Configuracao());

            Assert.Equal(TipoDFe.NFeABI, servico.Configuracoes.TipoDFe);
            Assert.Equal(Servico.NFeABIAutorizacaoSinc, servico.Configuracoes.Servico);
            Assert.Equal(ModeloDFe.NFeABI, servico.Configuracoes.Modelo);
            Assert.Equal(contrato.Endereco, servico.Configuracoes.WebEnderecoHomologacao);
            Assert.Equal(contrato.Acao, servico.Configuracoes.WebActionHomologacao);
            Assert.Equal(contrato.Retorno, servico.Configuracoes.WebTagRetorno);
            Assert.Contains("<nfeabi:" + contrato.Wrapper + ">", servico.Configuracoes.WebSoapString);
            Assert.Contains(contrato.TargetNamespace, servico.Configuracoes.WebSoapString);
        }

        /// <summary>
        /// Garante que produção falha fechada por não possuir endpoint publicado.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ProducaoDeveFalharSemEndpoint()
        {
            var consulta = new ConsStatServNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Producao,
                CUF = UFBrasil.PR
            };

            var exception = Assert.Throws<Exception>(() => new NFeABIStatusServico(consulta, new Configuracao()));
            Assert.Contains("não disponibiliza", exception.Message);
            Assert.Contains("produção", exception.Message);
        }

        /// <summary>
        /// Desserializa o retorno de status pelo pipeline público do serviço.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void StatusServicoDeveDesserializarRetorno()
        {
            const string retorno = "<retConsStatServNFeABI versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfeabi\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>107</cStat><xMotivo>Servico em Operacao</xMotivo><cUF>41</cUF><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto></retConsStatServNFeABI>";
            var consulta = new ConsStatServNFeABI { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, CUF = UFBrasil.PR };
            var servico = new NFeABIStatusServico(consulta, new Configuracao());
            var retornoXml = new XmlDocument();
            retornoXml.LoadXml(retorno);
            servico.RetornoWSString = retorno;
            servico.RetornoWSXML = retornoXml;

            Assert.IsType<RetConsStatServNFeABI>(servico.Result);
            Assert.Equal(107, servico.Result.CStat);
            Assert.Equal(TipoAmbiente.Homologacao, servico.Result.TpAmb);
        }

        /// <summary>
        /// Aceita a sigla de UF devolvida atualmente pelo ambiente de homologação.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "StatusServico")]
        public void StatusServicoDeveAceitarSiglaUFDoRetornoDeHomologacao()
        {
            const string retorno = "<n:retConsStatServNFeABI versao=\"1.00\" xmlns:n=\"http://www.portalfiscal.inf.br/nfeabi\"><n:tpAmb>2</n:tpAmb><n:verAplic>TESTE</n:verAplic><n:cStat>107</n:cStat><n:xMotivo>Servico em Operacao</n:xMotivo><n:cUF>PR</n:cUF><n:dhRecbto>2026-09-14T18:00:00-03:00</n:dhRecbto></n:retConsStatServNFeABI>";
            var consulta = new ConsStatServNFeABI { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, CUF = UFBrasil.PR };
            var retornoXml = new XmlDocument();
            retornoXml.LoadXml(retorno);
            var servico = new NFeABIStatusServico(consulta, new Configuracao()) { RetornoWSString = retorno, RetornoWSXML = retornoXml };

            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
            Assert.Equal(107, servico.Result.CStat);
            Assert.Contains("<n:cUF>PR</n:cUF>", servico.RetornoWSString);
            Assert.Equal("PR", servico.RetornoWSXML.GetElementsByTagName("cUF", "http://www.portalfiscal.inf.br/nfeabi")[0].InnerText);
        }

        /// <summary>
        /// Desserializa uma autorização aceita e compõe o processado correspondente.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoDeveComporNFeABIProcQuandoAceita()
        {
            var documento = LerNFeABI();
            var chave = documento.InfNFeABI.Chave;
            var retorno = "<retNFeABI versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfeabi\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>100</cStat><xMotivo>Autorizado</xMotivo><cUF>41</cUF><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto><protNFeABI versao=\"1.00\"><infProt Id=\"ID" + chave + "\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><chNFeABI>" + chave + "</chNFeABI><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto><nProt>141260000000001</nProt><digVal>QUJDRA==</digVal><cStat>100</cStat><xMotivo>Autorizado</xMotivo></infProt></protNFeABI></retNFeABI>";
            var servico = new NFeABIAutorizacaoSinc(documento, new Configuracao())
            {
                RetornoWSString = retorno
            };

            Assert.Equal(100, servico.Result.CStat);
            Assert.True(servico.NFeABIProcResults.ContainsKey(chave));
            Assert.Equal("141260000000001", servico.NFeABIProcResults[chave].ProtNFeABI.InfProt.NProt);
        }

        /// <summary>
        /// Retorna objeto amigável quando o web service não produziu XML.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ResultDeveSerAmigavelQuandoRetornoVazio()
        {
            var consulta = new ConsStatServNFeABI { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, CUF = UFBrasil.PR };
            var servico = new NFeABIStatusServico(consulta, new Configuracao());

            Assert.Equal(0, servico.Result.CStat);
            Assert.False(string.IsNullOrWhiteSpace(servico.Result.XMotivo));
        }

        /// <summary>
        /// Não cria processado quando a autorização é rejeitada.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoRejeitadaNaoDeveComporProcessado()
        {
            const string retorno = "<retNFeABI versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfeabi\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>539</cStat><xMotivo>Duplicidade</xMotivo><cUF>41</cUF><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto></retNFeABI>";
            var servico = new NFeABIAutorizacaoSinc(LerNFeABI(), new Configuracao()) { RetornoWSString = retorno };

            Assert.Equal(539, servico.Result.CStat);
            Assert.Empty(servico.NFeABIProcResults);
        }

        /// <summary>
        /// Aceita a sigla de UF devolvida atualmente pela autorização em homologação.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "AutorizacaoSinc")]
        public void AutorizacaoDeveAceitarSiglaUFDoRetornoDeHomologacao()
        {
            const string retorno = "<n:retNFeABI versao=\"1.00\" xmlns:n=\"http://www.portalfiscal.inf.br/nfeabi\"><n:tpAmb>2</n:tpAmb><n:verAplic>TESTE</n:verAplic><n:cStat>539</n:cStat><n:xMotivo>Duplicidade</n:xMotivo><n:cUF>PR</n:cUF><n:dhRecbto>2026-09-14T18:00:00-03:00</n:dhRecbto></n:retNFeABI>";
            var retornoXml = new XmlDocument();
            retornoXml.LoadXml(retorno);
            var servico = new NFeABIAutorizacaoSinc(LerNFeABI(), new Configuracao()) { RetornoWSString = retorno, RetornoWSXML = retornoXml };

            Assert.Equal(UFBrasil.PR, servico.Result.CUF);
            Assert.Equal(539, servico.Result.CStat);
            Assert.Contains("<n:cUF>PR</n:cUF>", servico.RetornoWSString);
            Assert.Equal("PR", servico.RetornoWSXML.GetElementsByTagName("cUF", "http://www.portalfiscal.inf.br/nfeabi")[0].InnerText);
        }

        /// <summary>
        /// A configuração explícita de proxy permanece disponível para o transporte padrão.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ConfiguracaoDeProxyDeveSerPreservada()
        {
            var configuracao = new Configuracao
            {
                HasProxy = true,
                ProxyAutoDetect = false,
                ProxyServer = "http://127.0.0.1",
                ProxyPort = 8888,
                ProxyUser = "usuario-teste",
                ProxyPassword = "senha-teste"
            };

            _ = new NFeABIStatusServico(new ConsStatServNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR
            }, configuracao);

            Assert.True(configuracao.HasProxy);
            Assert.False(configuracao.ProxyAutoDetect);
            Assert.Equal("http://127.0.0.1", configuracao.ProxyServer);
            Assert.Equal(8888, configuracao.ProxyPort);
        }

        /// <summary>
        /// A execução sem certificado falha antes de concluir o transporte fiscal.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ExecutarSemCertificadoDeveFalhar()
        {
            var servico = new NFeABIStatusServico(new ConsStatServNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR
            }, new Configuracao());

            var exception = Record.Exception(() => servico.Executar());

            Assert.NotNull(exception);
            Assert.Contains("certificado", exception.ToString(), StringComparison.OrdinalIgnoreCase);
        }

        /// <summary>
        /// Reutilizar a instância limpa documento, retorno e processados anteriores.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ReutilizacaoDeveLimparEstadoDaAutorizacaoAnterior()
        {
            var primeiro = LerNFeABI();
            var chaveAnterior = primeiro.InfNFeABI.Chave;
            var servico = new NFeABIAutorizacaoSinc(primeiro, new Configuracao());
            servico.RetornoWSString = CriarRetornoAutorizado(chaveAnterior);
            Assert.True(servico.NFeABIProcResults.ContainsKey(chaveAnterior));

            var segundo = LerNFeABI();
            segundo.InfNFeABI.Ide.NNF++;
            var chaveAtual = segundo.InfNFeABI.Chave;
            var preparar = typeof(NFeABIAutorizacaoSinc).GetMethod(
                "Preparar",
                BindingFlags.Instance | BindingFlags.NonPublic,
                null,
                new[] { typeof(Business.DFe.Xml.NFeABI.NFeABI), typeof(Configuracao) },
                null);
            preparar.Invoke(servico, new object[] { segundo, new Configuracao() });

            Assert.NotEqual(chaveAnterior, chaveAtual);
            Assert.Equal(chaveAtual, servico.NFeABI.InfNFeABI.Chave);
            Assert.Equal("2", servico.ConteudoXMLOriginal.GetElementsByTagName("nNF")[0].InnerText);
            Assert.Equal(0, servico.Result.CStat);
            Assert.Empty(servico.NFeABIProcResults);
        }

        /// <summary>
        /// Não associa à nota um protocolo autorizado de outra chave.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ProtocoloDeOutraChaveNaoDeveComporProcessado()
        {
            var documento = LerNFeABI();
            var servico = new NFeABIAutorizacaoSinc(documento, new Configuracao())
            {
                RetornoWSString = CriarRetornoAutorizado(new string('9', 44))
            };

            Assert.Empty(servico.NFeABIProcResults);
        }

        /// <summary>
        /// A validação central assina uma NFeABI sem assinatura usando certificado em memória.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void InicializacaoDeveAssinarDocumentoSemAssinatura()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=NFeABI Test", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var documento = LerNFeABI();
                    documento.Signature = null;
                    var configuracao = new Configuracao { CertificadoDigital = certificado };

                    var servico = new NFeABIAutorizacaoSinc(documento, configuracao);

                    Assert.NotNull(servico.ConteudoXMLAssinado.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0]);
                    Assert.NotNull(servico.NFeABI.Signature);
                }
            }
        }

        /// <summary>
        /// O construtor textual preserva o XML original para a validação de schema.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ConstrutorTextualNaoDeveNormalizarTagDesconhecidaAntesDaValidacao()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");
            var namespaceManager = new XmlNamespaceManager(xml.NameTable);
            namespaceManager.AddNamespace("n", "http://www.portalfiscal.inf.br/nfeabi");
            var infNFeABI = xml.SelectSingleNode("/n:NFeABI/n:infNFeABI", namespaceManager);
            var desconhecida = xml.CreateElement("tagDesconhecida", "http://www.portalfiscal.inf.br/nfeabi");
            desconhecida.InnerText = "INVALIDA";
            infNFeABI.AppendChild(desconhecida);

            Assert.Throws<ValidarXMLException>(() => new NFeABIAutorizacaoSinc(xml.OuterXml, new Configuracao()));
        }

        private static Business.DFe.Xml.NFeABI.NFeABI LerNFeABI()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");
            return new Business.DFe.Xml.NFeABI.NFeABI().LerXML<Business.DFe.Xml.NFeABI.NFeABI>(xml);
        }

        private static string CriarRetornoAutorizado(string chave)
        {
            return "<retNFeABI versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/nfeabi\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><cStat>100</cStat><xMotivo>Autorizado</xMotivo><cUF>41</cUF><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto><protNFeABI versao=\"1.00\"><infProt Id=\"ID" + chave + "\"><tpAmb>2</tpAmb><verAplic>TESTE</verAplic><chNFeABI>" + chave + "</chNFeABI><dhRecbto>2026-09-14T18:00:00-03:00</dhRecbto><nProt>141260000000001</nProt><digVal>QUJDRA==</digVal><cStat>100</cStat><xMotivo>Autorizado</xMotivo></infProt></protNFeABI></retNFeABI>";
        }

        private static ContratoWsdl LerContratoWsdl(string arquivo, string operacao, string mensagemEntrada, string mensagemSaida)
        {
            var wsdl = new XmlDocument();
            wsdl.Load(Path.Combine(@"..\..\..\NFeABI\Resources\Wsdl", arquivo));
            var ns = new XmlNamespaceManager(wsdl.NameTable);
            ns.AddNamespace("wsdl", "http://schemas.xmlsoap.org/wsdl/");
            ns.AddNamespace("soap12", "http://schemas.xmlsoap.org/wsdl/soap12/");

            var definitions = (XmlElement)wsdl.DocumentElement;
            var input = (XmlElement)wsdl.SelectSingleNode("//wsdl:message[@name='" + mensagemEntrada + "']/wsdl:part", ns);
            var output = (XmlElement)wsdl.SelectSingleNode("//wsdl:message[@name='" + mensagemSaida + "']/wsdl:part", ns);
            var operation = (XmlElement)wsdl.SelectSingleNode("//wsdl:binding/wsdl:operation[@name='" + operacao + "']/soap12:operation", ns);
            var address = (XmlElement)wsdl.SelectSingleNode("//wsdl:service/wsdl:port/soap12:address", ns);

            return new ContratoWsdl
            {
                TargetNamespace = definitions.GetAttribute("targetNamespace"),
                Wrapper = LocalName(input.GetAttribute("element")),
                Retorno = LocalName(output.GetAttribute("element")),
                Acao = operation.GetAttribute("soapAction"),
                Endereco = address.GetAttribute("location")
            };
        }

        private static string LocalName(string qualifiedName)
        {
            var separator = qualifiedName.IndexOf(':');
            return separator >= 0 ? qualifiedName.Substring(separator + 1) : qualifiedName;
        }

        private sealed class ContratoWsdl
        {
            public string TargetNamespace { get; set; }
            public string Wrapper { get; set; }
            public string Retorno { get; set; }
            public string Acao { get; set; }
            public string Endereco { get; set; }
        }
    }
}
