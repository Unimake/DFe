using System;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Validator.AACBS;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.DFe.Test.AACBS.Validacao;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Serializacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSSerializacaoTest
    {
        public static TheoryData<string, Type, Servico> Raizes => AACBSFixture.Raizes;

        [Theory]
        [MemberData(nameof(Raizes))]
        public void RoundTripPreservaCadaContrato(string raiz, Type modelo, Servico servico)
        {
            var original = AACBSFixture.Carregar(raiz);
            var gerado = AACBSFixture.Gerar(original, modelo);

            Assert.Equal(raiz, gerado.DocumentElement.LocalName);
            Assert.Equal(AACBSFixture.Namespace, gerado.DocumentElement.NamespaceURI);
            Assert.Equal("1.00", gerado.DocumentElement.GetAttribute("versao", string.Empty));
            CompararEstrutura(original.DocumentElement, gerado.DocumentElement);
            var validacao = AACBSFixture.ValidarSchemaDe(gerado, raiz);
            Assert.True(validacao.Success, $"{servico}: {validacao.ErrorMessage}");
        }

        [Theory]
        [InlineData("retArquivoDebitos", typeof(RetArquivoDebitos))]
        [InlineData("retArquivoCreditos", typeof(RetArquivoCreditos))]
        [InlineData("retArquivoPagamentos", typeof(RetArquivoPagamentos))]
        [InlineData("retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos))]
        public void ArquivoSemItensPreservaWrapperObrigatorio(string raiz, Type modelo)
        {
            var original = AACBSFixture.Carregar(raiz);
            AACBSFixture.Procurar(original, "apuracao").RemoveAll();

            ConferirRoundTrip(original, modelo);
        }

        [Theory]
        [InlineData("retArquivoDebitos", typeof(RetArquivoDebitos), "apuracao")]
        [InlineData("retArquivoDebitos", typeof(RetArquivoDebitos), "apuracao/item[1]/debitos")]
        [InlineData("retArquivoCreditos", typeof(RetArquivoCreditos), "apuracao")]
        [InlineData("retArquivoCreditos", typeof(RetArquivoCreditos), "apuracao/item[1]/creditos")]
        [InlineData("retArquivoPagamentos", typeof(RetArquivoPagamentos), "apuracao")]
        [InlineData("retArquivoPagamentos", typeof(RetArquivoPagamentos), "apuracao/item[1]/pagamentos")]
        [InlineData("retArquivoPagamentos", typeof(RetArquivoPagamentos), "apuracao/item[1]/pagamentos/item[1]/composicao")]
        [InlineData("retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos), "apuracao")]
        [InlineData("retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos), "apuracao/item[1]/pagamentos")]
        [InlineData("retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos), "apuracao/item[1]/pagamentos/item[1]/composicao")]
        [InlineData("gerarDarfAdquirente", typeof(GerarDarfAdquirente), "solicitacoes")]
        [InlineData("retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente), "dadosInvalidos")]
        [InlineData("retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte), "dadosInvalidos")]
        public void ItemNuloNaoDesapareceDaColecaoEPermaneceInvalidoNoSchema(string raiz, Type modelo, string caminho)
        {
            const string xsi = "http://www.w3.org/2001/XMLSchema-instance";
            var original = caminho == "dadosInvalidos"
                ? LerRetorno(raiz, "<cdRetorno>DADOS_DE_ENTRADA_INVALIDOS</cdRetorno><mensagem>Mensagem sintética</mensagem>" +
                                  "<dadosInvalidos><item>cnpj</item><item>pa</item></dadosInvalidos>")
                : AACBSFixture.Carregar(raiz);
            var objeto = AACBSFixture.Desserializar(original, modelo);
            var colecao = PrimeiraColecaoEm(objeto, caminho);
            colecao[0] = null;
            var wrapper = AACBSFixture.Procurar(original, caminho);
            var itemNulo = original.CreateElement("item", AACBSFixture.Namespace);
            var nil = original.CreateAttribute("xsi", "nil", xsi);
            nil.Value = "true";
            itemNulo.Attributes.Append(nil);
            wrapper.ReplaceChild(itemNulo, wrapper.FirstChild);

            var gerado = objeto.GerarXML();

            CompararEstrutura(original.DocumentElement, gerado.DocumentElement);
            Assert.Equal("true", AACBSFixture.Procurar(gerado, caminho + "/item[1]").GetAttribute("nil", xsi));
            Assert.False(AACBSFixture.ValidarSchemaDe(gerado, raiz).Success);
        }

        private static System.Collections.IList PrimeiraColecaoEm(object objeto, string caminho)
        {
            var segmentos = caminho.Split('/');
            for (var indice = 0; indice < segmentos.Length; indice++)
            {
                var segmento = segmentos[indice];
                objeto = segmento == "item[1]"
                    ? Assert.IsAssignableFrom<System.Collections.IList>(objeto)[0]
                    : objeto.GetType().GetProperty(char.ToUpperInvariant(segmento[0]) + segmento.Substring(1)).GetValue(objeto);
            }

            return Assert.IsAssignableFrom<System.Collections.IList>(objeto);
        }

        [Theory]
        [InlineData("retArquivoDebitos", "ArquivoDebitos")]
        [InlineData("retArquivoCreditos", "ArquivoCreditos")]
        [InlineData("retArquivoPagamentos", "ArquivoPagamentos")]
        [InlineData("retArquivoRecolhimentos", "ArquivoRecolhimentos")]
        public void EnvelopePreservaCadaFamiliaComoObjetoConcreto(string raiz, string propriedade)
        {
            var arquivo = AACBSFixture.Carregar(raiz).DocumentElement;
            var envelope = AACBSFixture.Carregar("retBaixarArquivoRetorno");
            envelope.DocumentElement.ReplaceChild(envelope.ImportNode(arquivo, true), envelope.DocumentElement.FirstChild);
            var resultado = XMLUtility.Deserializar<RetBaixarArquivoRetorno>(envelope);
            var familia = Assert.IsAssignableFrom<Unimake.Business.DFe.Xml.XMLBase>(
                typeof(RetBaixarArquivoRetorno).GetProperty(propriedade).GetValue(resultado));

            CompararEstrutura(arquivo, familia.GerarXML().DocumentElement);
            ConferirRoundTrip(envelope, typeof(RetBaixarArquivoRetorno));
        }

        [Theory]
        [InlineData("retAutenticar", typeof(RetAutenticar))]
        [InlineData("retConsultarDebitos", typeof(RetConsultarDebitos))]
        [InlineData("retConsultarCreditos", typeof(RetConsultarCreditos))]
        [InlineData("retConsultarPagamentos", typeof(RetConsultarPagamentos))]
        [InlineData("retConsultarRecolhimentos", typeof(RetConsultarRecolhimentos))]
        [InlineData("retConsultarSituacaoSolicitacao", typeof(RetConsultarSituacaoSolicitacao))]
        [InlineData("retConsultarLinkDownload", typeof(RetConsultarLinkDownload))]
        [InlineData("retBaixarArquivoRetorno", typeof(RetBaixarArquivoRetorno))]
        [InlineData("retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente))]
        [InlineData("retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte))]
        public void RetornoDeServicoPodeConterSomenteErroLocal(string raiz, Type modelo)
        {
            var original = AACBSFixture.Ler(
                $"<{raiz} xmlns=\"{AACBSFixture.Namespace}\" versao=\"1.00\">" +
                "<erroLocal><codigo>SEM_RETORNO</codigo><mensagem>Retorno sintético indisponível.</mensagem></erroLocal>" +
                $"</{raiz}>");

            ConferirRoundTrip(original, modelo);
        }

        [Fact]
        public void InstanteMantemTextoIntegralETruncaSomenteProjecaoSubTick()
        {
            var xml = AACBSFixture.Carregar("retArquivoDebitos");
            var arquivo = XMLUtility.Deserializar<RetArquivoDebitos>(xml);
            var esperado = new DateTimeOffset(2028, 2, 29, 23, 59, 59, TimeSpan.FromHours(-3)).AddTicks(1234567);

            Assert.Equal("2028-02-29T23:59:59.123456799-03:00", arquivo.GeradoEmField);
            Assert.Equal(esperado, arquivo.GeradoEm);
            Assert.Equal(esperado.Offset, arquivo.GeradoEm.Offset);
            Assert.Equal(1234567, arquivo.GeradoEm.Ticks % TimeSpan.TicksPerSecond);
            CompararEstrutura(xml.DocumentElement, arquivo.GerarXML().DocumentElement);
        }

        [Fact]
        public void SolicitacoesDarfConservamZeroAusenciaMagnitudeEChavesRepetidas()
        {
            var rad = XMLUtility.Deserializar<GerarDarfAdquirente>(AACBSFixture.Carregar("gerarDarfAdquirente"));

            Assert.Equal(3, rad.Solicitacoes.Count);
            Assert.Equal(0m, rad.Solicitacoes[0].ValorDarfParcial);
            Assert.Null(rad.Solicitacoes[1].ValorDarfParcial);
            Assert.Equal(9999999999999999.99m, rad.Solicitacoes[2].ValorDarfParcial);
            Assert.Equal(rad.Solicitacoes[0].ChaveDFE, rad.Solicitacoes[1].ChaveDFE);
            Assert.Equal(rad.Solicitacoes[1].ChaveDFE, rad.Solicitacoes[2].ChaveDFE);
        }

        [Fact]
        public void GeraEnvioPcontPeloModeloComParcialZeroExplicito()
        {
            var envio = new GerarDarfContribuinte
            {
                Cnpj = "00000001000001",
                Pa = "02/2028",
                OpcaoPagamento = "PAGAMENTO_PARCIAL",
                ValorDarfParcial = 0m
            };

            var esperado = AACBSFixture.Carregar("gerarDarfContribuinte");
            var gerado = envio.GerarXML();
            CompararEstrutura(esperado.DocumentElement, gerado.DocumentElement);
            var validacao = AACBSFixture.ValidarSchemaDe(gerado, "gerarDarfContribuinte");
            Assert.True(validacao.Success, validacao.ErrorMessage);
        }


        [Theory]
        [InlineData("retConsultarDebitos", typeof(RetConsultarDebitos))]
        [InlineData("retConsultarCreditos", typeof(RetConsultarCreditos))]
        [InlineData("retConsultarPagamentos", typeof(RetConsultarPagamentos))]
        [InlineData("retConsultarRecolhimentos", typeof(RetConsultarRecolhimentos))]
        [InlineData("retConsultarLinkDownload", typeof(RetConsultarLinkDownload))]
        public void ErroRemotoPreservaCodigoAbertoEMensagem(string raiz, Type modelo)
        {
            ConferirRoundTrip(LerRetorno(raiz,
                "<codigoErro>NOVO_CODIGO_SINTETICO</codigoErro><mensagemErro>Rejeição sintética &amp; detalhe.</mensagemErro>"), modelo);
        }

        [Fact]
        public void ErroOAuthPreservaCodigoAbertoEDetalhesOpcionais()
        {
            ConferirRoundTrip(LerRetorno("retAutenticar",
                "<error>erro_oauth_sintetico</error><error_description>Descrição sintética</error_description>" +
                "<error_uri>https://example.com/erro</error_uri>"), typeof(RetAutenticar));
            ConferirRoundTrip(LerRetorno("retAutenticar",
                "<error>erro_oauth_sintetico</error>"), typeof(RetAutenticar));
        }

        [Fact]
        public void SucessoOAuthMantemCaixaDoTipoEAusenciaDeOpcionais()
        {
            ConferirRoundTrip(LerRetorno("retAutenticar",
                "<access_token>MARCADOR-PUBLICO-SINTETICO</access_token><token_type>bEaReR</token_type>"), typeof(RetAutenticar));
        }

        [Theory]
        [InlineData("PENDENTE", false)]
        [InlineData("EM_PROCESSAMENTO", false)]
        [InlineData("CONCLUIDA", false)]
        [InlineData("ERRO", true)]
        [InlineData("EXPIRADA", true)]
        public void SituacaoPreservaRamoDoEstado(string situacao, bool possuiErro)
        {
            var conteudo = $"<situacao>{situacao}</situacao>";
            if (possuiErro)
            {
                conteudo += "<codigoErro>ERRO_SINTETICO</codigoErro><mensagemErro>Mensagem sintética</mensagemErro>";
            }

            ConferirRoundTrip(LerRetorno("retConsultarSituacaoSolicitacao", conteudo),
                typeof(RetConsultarSituacaoSolicitacao));
        }

        [Theory]
        [InlineData("retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente), "dataMaximaPagamento", "dataVencimento")]
        [InlineData("retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte), "dataVencimento", "dataMaximaPagamento")]
        public void SucessoDarfPreservaCadaNomeDeDataPublicado(string raiz, Type modelo, string nomeOriginal, string variante)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var original = AACBSFixture.Procurar(xml, "informacoesPagamento/" + nomeOriginal);
            var data = xml.CreateElement(variante, AACBSFixture.Namespace);
            data.InnerText = original.InnerText;
            original.ParentNode.ReplaceChild(data, original);

            ConferirRoundTrip(xml, modelo);
        }

        [Theory]
        [InlineData("DFE_NAO_ENCONTRADO", false)]
        [InlineData("VALOR_PARCIAL_SUPERIOR_DFE", false)]
        [InlineData("CNPJ_NAO_ADQUIRENTE", true)]
        [InlineData("DADOS_DE_ENTRADA_INVALIDOS", true)]
        public void ErroDarfEmAmbasAsOperacoesPreservaListaComDuplicatas(string codigo, bool informarLista)
        {
            var conteudo = $"<cdRetorno>{codigo}</cdRetorno><mensagem>Mensagem sintética</mensagem>";
            if (informarLista)
            {
                conteudo += "<dadosInvalidos><item>cnpj</item><item>pa</item><item>cnpj</item></dadosInvalidos>";
            }

            ConferirRoundTrip(LerRetorno("retGerarDarfAdquirente", conteudo), typeof(RetGerarDarfAdquirente));
            ConferirRoundTrip(LerRetorno("retGerarDarfContribuinte", conteudo), typeof(RetGerarDarfContribuinte));
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void PagamentoTotalPcontAceitaAusenciaOuValorValidoNoXml(bool informarValor)
        {
            var xml = AACBSFixture.Carregar("gerarDarfContribuinte");
            AACBSFixture.Procurar(xml, "opcaoPagamento").InnerText = "PAGAMENTO_TOTAL";
            if (!informarValor)
            {
                var parcela = AACBSFixture.Procurar(xml, "valorDarfParcial");
                parcela.ParentNode.RemoveChild(parcela);
            }

            ConferirRoundTrip(xml, typeof(GerarDarfContribuinte));
        }

        [Fact]
        public void LinkDownloadPreservaAusenciaDaExpiracao()
        {
            var xml = AACBSFixture.Carregar("retConsultarLinkDownload");
            var expiracao = AACBSFixture.Procurar(xml, "urlAssinadaExpiraEm");
            expiracao.ParentNode.RemoveChild(expiracao);

            ConferirRoundTrip(xml, typeof(RetConsultarLinkDownload));
        }

        [Theory]
        [InlineData("retConsultarLinkDownload", typeof(RetConsultarLinkDownload), "urlAssinada", "https://sintetico@example.com/a%2Fb?k=a%2Bb#parte")]
        [InlineData("retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente), "darf/linkDarfPDF", "https://example.com/darf.pdf#page=1")]
        [InlineData("retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte), "darf/linkDarfPDF", "urn:exemplo:darf#parte")]
        public void RetornoPreservaTextoIntegralDaUriInformada(string raiz, Type modelo, string caminho, string uri)
        {
            var xml = AACBSFixture.Carregar(raiz);
            AACBSFixture.Procurar(xml, caminho).InnerText = uri;

            ConferirRoundTrip(xml, modelo);
        }

        [Fact]
        public void ArquivoDebitosPreservaOrigemZeroEValoresDecimaisTipados()
        {
            var arquivo = XMLUtility.Deserializar<RetArquivoDebitos>(AACBSFixture.Carregar("retArquivoDebitos"));

            Assert.Equal(2, arquivo.Apuracao.Count);
            Assert.Equal(2, arquivo.Apuracao[0].Debitos.Count);
            var primeiro = arquivo.Apuracao[0].Debitos[0];
            var segundo = arquivo.Apuracao[0].Debitos[1];
            Assert.Equal(0, primeiro.Origem);
            Assert.Equal(primeiro.Chave, segundo.Chave);
            Assert.Equal(primeiro.Chave, arquivo.Apuracao[1].Debitos[0].Chave);
            Assert.Equal(9999999999999999.99m, primeiro.Cbs.Apurado);
            Assert.Equal(0m, primeiro.Cbs.Excedente);
            Assert.Null(segundo.Cbs.Excedente);
            Assert.Equal(-1.01m, primeiro.Cbs.Inexigivel);
        }

        [Theory]
        [InlineData("retArquivoDebitos", typeof(RetArquivoDebitos), "apuracao/item[1]/debitos/item[1]/cbs/apurado")]
        [InlineData("retArquivoCreditos", typeof(RetArquivoCreditos), "apuracao/item[1]/creditos/item[1]/cbs/excedente")]
        [InlineData("retArquivoPagamentos", typeof(RetArquivoPagamentos), "apuracao/item[1]/pagamentos/item[1]/composicao/item[1]/principal")]
        [InlineData("retArquivoRecolhimentos", typeof(RetArquivoRecolhimentos), "apuracao/item[1]/pagamentos/item[1]/composicao/item[1]/total")]
        [InlineData("gerarDarfAdquirente", typeof(GerarDarfAdquirente), "solicitacoes/item[1]/valorDarfParcial")]
        [InlineData("gerarDarfContribuinte", typeof(GerarDarfContribuinte), "valorDarfParcial")]
        [InlineData("retGerarDarfAdquirente", typeof(RetGerarDarfAdquirente), "informacoesPagamento/valorDarfPrincipal")]
        [InlineData("retGerarDarfContribuinte", typeof(RetGerarDarfContribuinte), "informacoesPagamento/valorDarfMulta")]
        public void LeituraDecimalNaoArredondaNemAceitaMagnitudeForaDoContrato(string raiz, Type modelo, string caminho)
        {
            var xml = AACBSFixture.Carregar(raiz);
            var campo = AACBSFixture.Procurar(xml, caminho);
            foreach (var valor in new[] { "0.000000000000000000000000000001", "10000000000000000", "-10000000000000000" })
            {
                campo.InnerText = valor;

                var excecao = Assert.Throws<System.Reflection.TargetInvocationException>(() => AACBSFixture.Gerar(xml, modelo));
                Assert.IsType<XmlException>(excecao.InnerException);
            }
        }

        [Fact]
        public void LeituraDecimalAceitaEspacosXmlEZerosDecimaisNaoSignificativos()
        {
            var xml = AACBSFixture.Carregar("gerarDarfContribuinte");
            AACBSFixture.Procurar(xml, "valorDarfParcial").InnerText = " \t1.25000\r\n";

            var envio = XMLUtility.Deserializar<GerarDarfContribuinte>(xml);

            Assert.Equal(1.25m, envio.ValorDarfParcial);
            Assert.Equal("1.25", AACBSFixture.Procurar(envio.GerarXML(), "valorDarfParcial").InnerText);
        }

        [Fact]
        public void CamposOpacosPodemConterEspacoSemPerderValor()
        {
            var consulta = AACBSFixture.Carregar("consultarSituacaoSolicitacao");
            AACBSFixture.Procurar(consulta, "tiqueteSolicitacao").InnerText = " ";
            var darf = AACBSFixture.Carregar("retGerarDarfAdquirente");
            AACBSFixture.Procurar(darf, "darf/chavePIX").InnerText = " ";

            ConferirRoundTrip(consulta, typeof(ConsultarSituacaoSolicitacao));
            ConferirRoundTrip(darf, typeof(RetGerarDarfAdquirente));
        }

        [Theory]
        [InlineData("<tiqueteSolicitacao>TIQUETE</tiqueteSolicitacao><desconhecido>MARCADOR_SINTETICO</desconhecido><tEASegundos>0</tEASegundos>")]
        [InlineData("<tiqueteSolicitacao>MARCADOR_SINTETICO<tiqueteSolicitacao>")]
        public void LeituraInvalidaNaoExpoeValoresEmExcecao(string conteudo)
        {
            var xml = $"<retConsultarDebitos xmlns=\"{AACBSFixture.Namespace}\" versao=\"1.00\">{conteudo}</retConsultarDebitos>";

            var excecao = Assert.Throws<XmlException>(() => XMLUtility.Deserializar<RetConsultarDebitos>(xml));
            Assert.DoesNotContain("MARCADOR_SINTETICO", excecao.ToString(), StringComparison.Ordinal);
            Assert.Null(excecao.InnerException);
        }

        [Fact]
        public void LeituraDeEnumInvalidoNaoExpoeValorEmExcecao()
        {
            var xml = AACBSFixture.Carregar("retConsultarSituacaoSolicitacao");
            AACBSFixture.Procurar(xml, "situacao").InnerText = "MARCADOR_ENUM_SINTETICO";

            var excecao = Assert.Throws<XmlException>(() => XMLUtility.Deserializar<RetConsultarSituacaoSolicitacao>(xml));
            Assert.DoesNotContain("MARCADOR_ENUM_SINTETICO", excecao.ToString(), StringComparison.Ordinal);
            Assert.Null(excecao.InnerException);
        }

        private static XmlDocument LerRetorno(string raiz, string conteudo) =>
            AACBSFixture.Ler($"<{raiz} xmlns=\"{AACBSFixture.Namespace}\" versao=\"1.00\">{conteudo}</{raiz}>");

        private static void ConferirRoundTrip(XmlDocument original, Type modelo)
        {
            var gerado = AACBSFixture.Gerar(original, modelo);
            CompararEstrutura(original.DocumentElement, gerado.DocumentElement);
            var validacao = AACBSFixture.ValidarSchemaDe(gerado, original.DocumentElement.LocalName);
            Assert.True(validacao.Success, validacao.ErrorMessage);
            Assert.True(new AACBSValidator(gerado).Validate());
        }

        private static void CompararEstrutura(XmlElement esperado, XmlElement atual)
        {
            Assert.Equal(esperado.LocalName, atual.LocalName);
            Assert.Equal(esperado.NamespaceURI, atual.NamespaceURI);
            Assert.Equal(Atributos(esperado), Atributos(atual));
            Assert.Equal(esperado.InnerText, atual.InnerText);

            var filhosEsperados = esperado.ChildNodes.OfType<XmlElement>().ToArray();
            var filhosAtuais = atual.ChildNodes.OfType<XmlElement>().ToArray();
            Assert.Equal(filhosEsperados.Length, filhosAtuais.Length);
            for (var indice = 0; indice < filhosEsperados.Length; indice++)
            {
                CompararEstrutura(filhosEsperados[indice], filhosAtuais[indice]);
            }
        }

        private static (string Namespace, string Nome, string Valor)[] Atributos(XmlElement elemento) =>
            elemento.Attributes.Cast<XmlAttribute>()
                .Where(atributo => atributo.NamespaceURI != "http://www.w3.org/2000/xmlns/")
                .OrderBy(atributo => atributo.NamespaceURI, StringComparer.Ordinal)
                .ThenBy(atributo => atributo.LocalName, StringComparer.Ordinal)
                .Select(atributo => (atributo.NamespaceURI, atributo.LocalName, atributo.Value))
                .ToArray();
    }
}
