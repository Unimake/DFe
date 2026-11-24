using System;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.AACBS;
using Unimake.Business.DFe.Xml.Validar;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Validacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSValidacaoTest
    {
        private const string CaminhoDebito = "apuracao/item[1]/debitos/item[1]";
        private const string CaminhoCredito = "apuracao/item[1]/creditos/item[1]";
        private const string CaminhoRad = "apuracao/item[1]/pagamentos/item[1]";
        private const string CaminhoPcont = "apuracao/item[1]/pagamentos/item[2]";

        public static TheoryData<string, Type, Servico> Raizes => AACBSFixture.Raizes;

        [Theory]
        [MemberData(nameof(Raizes))]
        public void LocalizaRaizVersaoServicoESchemaEmbutido(string raiz, Type modelo, Servico servico)
        {
            var xml = AACBSFixture.Carregar(raiz);

            Assert.Equal(TipoDFe.AACBS, XMLUtility.DetectDFeType(xml));
            var catalogo = CatalogoValidacaoXML.CriarCopia();
            var entrada = Assert.IsAssignableFrom<XmlElement>(
                catalogo.SelectSingleNode($"/ServicosValidacao/AACBS/Servico[@tagRaiz='{raiz}'][@versao='1.00']"));
            Assert.True(Enum.TryParse<Servico>(entrada.SelectSingleNode("TipoServico")?.InnerText, out var registrado));
            Assert.Equal(servico, registrado);
            Assert.Equal(AACBSFixture.Namespace, entrada.SelectSingleNode("TargetNS")?.InnerText);
            Assert.Equal($"{raiz}_v1.00.xsd", entrada.SelectSingleNode("SchemaArquivo")?.InnerText);

            var configuracao = new Configuracao { CodigoUF = (int)UFBrasil.AN };
            var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);
            Assert.True(resultado.Validado, $"{modelo.Name}: {resultado.MensagemRetorno}");
            Assert.True(new AACBSValidator(xml).Validate());
        }

        [Theory]
        [InlineData("autenticar", "urn:externo", "1.00")]
        [InlineData("autenticar", "", "1.00")]
        [InlineData("autenticar", AACBSFixture.Namespace + "/", "1.00")]
        [InlineData("raizNaoCatalogada", AACBSFixture.Namespace, "1.00")]
        [InlineData("Autenticar", AACBSFixture.Namespace, "1.00")]
        public void DominioNaoEInferidoPorRaizOuNamespaceIsolado(string raiz, string ns, string versao)
        {
            var xml = AACBSFixture.Ler($"<{raiz} xmlns=\"{ns}\" versao=\"{versao}\" />");

            Assert.NotEqual(TipoDFe.AACBS, XMLUtility.DetectDFeType(xml));
            Assert.Throws<Exception>(() => new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN }));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("1.01")]
        [InlineData("1.0")]
        [InlineData("v2")]
        public void RejeitaVersaoAusenteOuDiferenteDaVersaoXml(string versao)
        {
            var xml = AACBSFixture.Carregar("autenticar");
            if (versao is null)
            {
                xml.DocumentElement.RemoveAttribute("versao");
            }
            else
            {
                xml.DocumentElement.SetAttribute("versao", versao);
            }

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, "autenticar").Success);
            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.False(resultado.Validado);
        }

        [Fact]
        public void PrefixoXmlNaoMudaIdentidadeDoDocumentoAacbs()
        {
            var xml = AACBSFixture.Ler(
                $"<a:consultarDebitos xmlns:a=\"{AACBSFixture.Namespace}\" versao=\"1.00\"><a:cnpj>00000001</a:cnpj></a:consultarDebitos>");

            Assert.Equal(TipoDFe.AACBS, XMLUtility.DetectDFeType(xml));
            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
        }

        [Theory]
        [InlineData("autenticar", "", "<clientSecret>MARCADOR-SINTETICO</clientSecret>")]
        [InlineData("consultarDebitos", "cnpj", "00000001000001")]
        [InlineData("consultarCreditos", "cnpj", "00.000.001")]
        [InlineData("consultarPagamentos", "cnpj", "０００００００１")]
        [InlineData("consultarRecolhimentos", "cnpj", "")]
        [InlineData("consultarSituacaoSolicitacao", "tiqueteSolicitacao", "")]
        [InlineData("consultarLinkDownload", "tiqueteSolicitacao", "")]
        [InlineData("baixarArquivoRetorno", "tipoConsulta", "OUTRO")]
        [InlineData("retConsultarSituacaoSolicitacao", "situacao", "concluida")]
        [InlineData("retConsultarDebitos", "tEASegundos", "-2")]
        [InlineData("retConsultarCreditos", "tEASegundos", "0.25")]
        [InlineData("retAutenticar", "expires_in", "0")]
        [InlineData("retAutenticar", "access_token", "")]
        [InlineData("retArquivoDebitos", "niConsumidor", "000000000001")]
        [InlineData("retArquivoDebitos", "geradoEm", "2028-02-29T12:00:00")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/origem", "1")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/documento", "56")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/apurado", "1.001")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/apurado", "0.000000000000000000000000000001")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/apurado", "10000000000000000")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/apurado", "-10000000000000000")]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/apurado", "1E2")]
        [InlineData("retArquivoCreditos", CaminhoCredito + "/chave", "")]
        [InlineData("retArquivoCreditos", CaminhoCredito + "/cbs/apropriacao/utilizacao/naoUtilizado/saldoCredor", "1,23")]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/numeroDARF", "0000000000000001")]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/tipo", "3")]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/composicao/item[1]/sequencial", "0")]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/composicao/item[1]/sequencial", "100000000")]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/composicao/item[1]/vencimento", "20/03/2028")]
        [InlineData("retArquivoRecolhimentos", CaminhoRad + "/tipo", "2")]
        [InlineData("retArquivoRecolhimentos", CaminhoRad + "/composicao/item[1]/vencimento", "2028-03-20T00:00:00Z")]
        [InlineData("retArquivoRecolhimentos", "apuracao/item[1]/dataArrecadacao", "2028-02-30")]
        [InlineData("gerarDarfAdquirente", "cnpj", "00000001")]
        [InlineData("gerarDarfContribuinte", "pa", "13/2028")]
        [InlineData("gerarDarfContribuinte", "opcaoPagamento", "pagamento_total")]
        [InlineData("gerarDarfContribuinte", "valorDarfParcial", "")]
        [InlineData("retGerarDarfAdquirente", "cdRetorno", "ERRO_GERAL")]
        [InlineData("retGerarDarfContribuinte", "informacoesPagamento/dataVencimento", "2028-02-29Z")]
        public void SchemaRejeitaLexicoCampoEDominioInvalidos(string raiz, string caminho, string valor)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var elemento = AACBSFixture.Procurar(xml, caminho);
            if (caminho.Length == 0)
            {
                elemento.InnerXml = valor;
            }
            else
            {
                elemento.InnerText = valor;
            }

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);
        }

        [Theory]
        [InlineData("retArquivoDebitos", "apuracao", true)]
        [InlineData("retArquivoDebitos", "apuracao/item[1]/debitos", false)]
        [InlineData("retArquivoDebitos", CaminhoDebito + "/cbs/saldoDevedor", true)]
        [InlineData("retArquivoCreditos", "apuracao/item[1]/creditos", false)]
        [InlineData("retArquivoCreditos", CaminhoCredito + "/cbs/apropriacao/utilizacao", true)]
        [InlineData("retArquivoPagamentos", "apuracao/item[1]/pagamentos", false)]
        [InlineData("retArquivoPagamentos", CaminhoRad + "/composicao", false)]
        [InlineData("retArquivoRecolhimentos", CaminhoRad + "/composicao/item[1]/niFornecedor", true)]
        [InlineData("retConsultarDebitos", "tEASegundos", true)]
        [InlineData("retBaixarArquivoRetorno", "", false)]
        [InlineData("gerarDarfAdquirente", "solicitacoes", false)]
        [InlineData("retGerarDarfAdquirente", "informacoesPagamento/dataMaximaPagamento", true)]
        [InlineData("retGerarDarfContribuinte", "informacoesPagamento/valorDarfMulta", true)]
        public void SchemaExigeGruposCamposEItensObrigatorios(string raiz, string caminho, bool removerElemento)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var elemento = AACBSFixture.Procurar(xml, caminho);
            if (removerElemento)
            {
                elemento.ParentNode.RemoveChild(elemento);
            }
            else
            {
                elemento.InnerXml = "";
            }

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);
        }

        [Theory]
        [InlineData("consultarDebitos", "urlRetorno", "http://example.com/retorno")]
        [InlineData("consultarDebitos", "urlRetorno", "/retorno")]
        [InlineData("retConsultarLinkDownload", "urlAssinada", "http://example.com/arquivo")]
        [InlineData("baixarArquivoRetorno", "urlAssinada", "https://example.com/arquivo#parte")]
        [InlineData("baixarArquivoRetorno", "urlAssinada", "https://usuario@example.com/arquivo")]
        [InlineData("baixarArquivoRetorno", "urlAssinada", "https://")]
        [InlineData("retGerarDarfAdquirente", "darf/linkDarfPDF", "/darf")]
        [InlineData("retGerarDarfContribuinte", "informacoesPagamento/dataVencimento", "2027-02-29")]
        [InlineData("retArquivoCreditos", "geradoEm", "2027-02-29T12:00:00Z")]
        [InlineData("retConsultarSituacaoSolicitacao", "situacao", "ERRO")]
        [InlineData("retConsultarSituacaoSolicitacao", "situacao", "EXPIRADA")]
        [InlineData("retGerarDarfAdquirente", "cdRetorno", "DFE_NAO_ENCONTRADO")]
        [InlineData("retAutenticar", "token_type", "Basic")]
        [InlineData("retArquivoPagamentos", CaminhoPcont + "/composicao/item[1]/vencimento", "2028-03-20T00:00:00.0100-03:00")]
        public void ValidacaoCentralRejeitaCamposIncompativeisComOConteudo(string raiz, string caminho, string valor)
        {
            var xml = AACBSFixture.Carregar(raiz);
            if (raiz == "consultarDebitos")
            {
                Adicionar(xml, "", "urlRetorno", valor);
            }
            else
            {
                AACBSFixture.Procurar(xml, caminho).InnerText = valor;
            }

            ConferirFalhaConteudo(xml);
        }

        [Theory]
        [InlineData("retArquivoPagamentos", CaminhoRad, "niAdquirente")]
        [InlineData("retArquivoPagamentos", CaminhoPcont, "niContribuinte")]
        [InlineData("gerarDarfAdquirente", "solicitacoes/item[1]", "valorDarfParcial")]
        [InlineData("gerarDarfContribuinte", "", "valorDarfParcial")]
        public void ValidacaoCentralExigeCamposCondicionais(string raiz, string caminho, string tag)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var elemento = AACBSFixture.Procurar(xml, caminho.Length == 0 ? tag : caminho + "/" + tag);
            elemento.ParentNode.RemoveChild(elemento);

            ConferirFalhaConteudo(xml);
        }

        [Theory]
        [InlineData("retArquivoPagamentos", CaminhoPcont + "/composicao/item[1]", "niContribuinte", "00000001000001", "vencimento")]
        [InlineData("retArquivoPagamentos", CaminhoPcont + "/composicao/item[1]", "chaveDFE", "CHAVE-SINTETICA", "vencimento")]
        [InlineData("retArquivoPagamentos", CaminhoRad, "niContribuinte", "00000001000001", "niAdquirente")]
        [InlineData("retGerarDarfAdquirente", "informacoesPagamento", "dataVencimento", "2028-02-29", "dataMaximaPagamento")]
        [InlineData("retGerarDarfContribuinte", "informacoesPagamento", "dataMaximaPagamento", "2028-02-29", "dataVencimento")]
        public void ValidacaoCentralRejeitaCamposDeRamoIncompativel(string raiz, string caminho, string tag, string valor, string apos)
        {
            var xml = AACBSFixture.Carregar(raiz);
            Adicionar(xml, caminho, tag, valor, apos);

            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.False(resultado.Validado);
        }

        [Theory]
        [InlineData("retArquivoDebitos", "")]
        [InlineData("retBaixarArquivoRetorno", "retArquivoDebitos")]
        [InlineData("retConsultarCreditos", "")]
        [InlineData("retGerarDarfAdquirente", "")]
        public void ErroLocalNaoCoexisteComDadosRemotos(string raiz, string caminho)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var erro = xml.CreateElement("erroLocal", AACBSFixture.Namespace);
            erro.InnerXml = "<codigo>SEM_RETORNO</codigo><mensagem>Mensagem sintética</mensagem>";
            AACBSFixture.Procurar(xml, caminho).AppendChild(erro);

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);
        }

        [Fact]
        public void EnvelopeNaoAceitaDuasFamilias()
        {
            var xml = AACBSFixture.Carregar("retBaixarArquivoRetorno");
            var outro = AACBSFixture.Carregar("retArquivoCreditos");
            xml.DocumentElement.AppendChild(xml.ImportNode(outro.DocumentElement, true));

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, "retBaixarArquivoRetorno").Success);
        }

        [Theory]
        [InlineData("retBaixarArquivoRetorno", "retArquivoDebitos")]
        [InlineData("retArquivoDebitos", "retBaixarArquivoRetorno")]
        public void SchemaDaOperacaoRejeitaOutraRaizMesmoQuandoImportada(string raiz, string raizSchema)
        {
            var xml = AACBSFixture.Carregar(raiz);

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raizSchema).Success);
        }

        [Theory]
        [InlineData("+2")]
        [InlineData("02")]
        [InlineData(" 2 ")]
        public void VariantesLexicaisDoInteiroTipoNaoContornamRegrasPcont(string tipo)
        {
            var xml = AACBSFixture.Carregar("retArquivoPagamentos");
            AACBSFixture.Procurar(xml, CaminhoPcont + "/tipo").InnerText = tipo;
            Assert.True(new AACBSValidator(xml).Validate());
            Adicionar(xml, CaminhoPcont + "/composicao/item[1]", "chaveDFE", "CHAVE-SINTETICA", "vencimento");

            var schema = AACBSFixture.ValidarSchemaDe(xml, "retArquivoPagamentos");
            Assert.True(schema.Success, schema.ErrorMessage);
            ConferirFalhaConteudo(xml);
        }

        [Fact]
        public void RecolhimentoTipoUmNaoExigeIdentificacaoDeAdquirente()
        {
            var xml = AACBSFixture.Carregar("retArquivoRecolhimentos");
            AACBSFixture.Procurar(xml, CaminhoRad + "/tipo").InnerText = "+1";

            Assert.True(new AACBSValidator(xml).Validate());
            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
        }

        [Theory]
        [InlineData("retConsultarLinkDownload", "urlAssinada", "http://example.com/a?assinatura=MARCADOR_QUERY_SINTETICO", "MARCADOR_QUERY_SINTETICO")]
        [InlineData("consultarDebitos", "cnpj", "00000001000099", "00000001000099")]
        [InlineData("autenticar", "@versao", "VERSAO-SINTETICA-INVALIDA", "VERSAO-SINTETICA-INVALIDA")]
        public void MensagensDeFalhaDeValidacaoNaoExpoemDadosDoXml(string raiz, string campo, string valor, string sentinela)
        {
            var xml = AACBSFixture.Carregar(raiz);
            if (campo == "@versao")
            {
                xml.DocumentElement.SetAttribute("versao", valor);
            }
            else
            {
                AACBSFixture.Procurar(xml, campo).InnerText = valor;
            }

            var schema = AACBSFixture.ValidarSchemaDe(xml, raiz);
            Assert.False(schema.Success);
            Assert.False(string.IsNullOrWhiteSpace(schema.ErrorMessage));
            Assert.DoesNotContain(sentinela, schema.ErrorMessage, StringComparison.Ordinal);

            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.False(resultado.Validado);
            Assert.False(string.IsNullOrWhiteSpace(resultado.MensagemRetorno));
            Assert.DoesNotContain(sentinela, resultado.MensagemRetorno, StringComparison.Ordinal);
        }

        [Fact]
        public void ValidacaoDeConteudoNaoExpoeQueryDeUrlAssinadaIncompativel()
        {
            const string sentinela = "MARCADOR_QUERY_SINTETICO";
            const string raiz = "baixarArquivoRetorno";
            var xml = AACBSFixture.Carregar(raiz);
            AACBSFixture.Procurar(xml, "urlAssinada").InnerText =
                $"https://example.com/a%GG?assinatura={sentinela}";

            var schema = AACBSFixture.ValidarSchemaDe(xml, raiz);
            Assert.True(schema.Success, schema.ErrorMessage);
            var excecao = Assert.Throws<ValidatorDFeException>(() => new AACBSValidator(xml).Validate());
            Assert.DoesNotContain(sentinela, excecao.Message, StringComparison.Ordinal);
            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.False(resultado.Validado);
            Assert.DoesNotContain(sentinela, resultado.MensagemRetorno, StringComparison.Ordinal);
        }

        [Theory]
        [InlineData("retGerarDarfAdquirente", "DADOS_DE_ENTRADA_INVALIDOS")]
        [InlineData("retGerarDarfContribuinte", "DADOS_DE_ENTRADA_INVALIDOS")]
        [InlineData("retGerarDarfAdquirente", "DARF_GERADO")]
        [InlineData("retGerarDarfContribuinte", "DARF_GERADO")]
        public void CodigoDarfNaoAceitaApenasMensagemQuandoExigeOutroGrupo(string raiz, string codigo)
        {
            var xml = AACBSFixture.Ler(
                $"<{raiz} xmlns=\"{AACBSFixture.Namespace}\" versao=\"1.00\">" +
                $"<cdRetorno>{codigo}</cdRetorno><mensagem>Mensagem sintética</mensagem></{raiz}>");

            ConferirFalhaConteudo(xml);
        }

        [Theory]
        [InlineData("")]
        [InlineData("<item/>")]
        public void SchemaRejeitaDadosInvalidosSemItensValidos(string itens)
        {
            const string raiz = "retGerarDarfAdquirente";
            var xml = AACBSFixture.Ler(
                $"<{raiz} xmlns=\"{AACBSFixture.Namespace}\" versao=\"1.00\">" +
                "<cdRetorno>DADOS_DE_ENTRADA_INVALIDOS</cdRetorno><mensagem>Mensagem sintética</mensagem>" +
                $"<dadosInvalidos>{itens}</dadosInvalidos></{raiz}>");

            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);
        }

        [Fact]
        public void SchemaRejeitaOrdemTrocadaEDuplicacaoDeCampoEscalar()
        {
            const string raiz = "retConsultarDebitos";
            var xml = AACBSFixture.Carregar(raiz);
            var estimativa = AACBSFixture.Procurar(xml, "tEASegundos");
            xml.DocumentElement.PrependChild(estimativa);
            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);

            xml = AACBSFixture.Carregar(raiz);
            estimativa = AACBSFixture.Procurar(xml, "tEASegundos");
            xml.DocumentElement.AppendChild(estimativa.CloneNode(true));
            Assert.False(AACBSFixture.ValidarSchemaDe(xml, raiz).Success);
        }

        private static void ConferirFalhaConteudo(XmlDocument xml)
        {
            Assert.Throws<ValidatorDFeException>(() => new AACBSValidator(xml).Validate());
            var resultado = new ValidarEstruturaXML().ValidarServico(xml,
                new Configuracao { CodigoUF = (int)UFBrasil.AN });
            Assert.False(resultado.Validado);
        }

        private static void Adicionar(XmlDocument xml, string caminho, string tag, string valor, string apos = null)
        {
            var pai = AACBSFixture.Procurar(xml, caminho);
            var elemento = xml.CreateElement(tag, AACBSFixture.Namespace);
            elemento.InnerText = valor;
            if (apos is null)
            {
                pai.AppendChild(elemento);
            }
            else
            {
                var caminhoReferencia = caminho.Length == 0 ? apos : caminho + "/" + apos;
                pai.InsertAfter(elemento, AACBSFixture.Procurar(xml, caminhoReferencia));
            }
        }
    }
}
