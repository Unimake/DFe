using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    [Trait("DFe", "NFSe")]
    [Trait("Categoria", "Offline")]
    public class ConsultaParametrosMunicipaisOfflineTest
    {
        public static IEnumerable<object[]> Consultas()
        {
            foreach (var operacao in new[] { "Aliquotas", "Historico", "Regimes", "Beneficio" })
            foreach (var completo in new[] { false, true })
            foreach (var construtorTipado in new[] { false, true })
            foreach (var status in new[] { HttpStatusCode.OK, HttpStatusCode.BadRequest })
            {
                yield return new object[] { operacao, completo, construtorTipado, status };
            }
        }

        [Theory]
        [MemberData(nameof(Consultas))]
        public void ExecutarDevePreservarParametrosNaRequisicaoERespostaNegativa(
            string operacao, bool completo, bool construtorTipado, HttpStatusCode status)
        {
            var parametro = operacao == "Beneficio"
                ? (completo ? "00000189439" : "189439")
                : (completo ? "010101000" : "010101");
            var mensagem = operacao == "Beneficio"
                ? "Chamada mal formada. O número do benefício deve ser composto por 11 dígitos."
                : "Chamada mal formada. O código do serviço deve ser composto por nove dígitos.";
            var json = new JObject
            {
                [operacao == "Beneficio" ? "beneficio" : "aliquotas"] = new JArray(),
                ["mensagem"] = mensagem
            }.ToString();
            using (var certificado = CriarCertificado())
            {
                var configuracao = new Configuracao
                {
                    TipoDFe = TipoDFe.NFSe,
                    CodigoMunicipio = 1001058,
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    SchemaVersao = "1.01",
                    CertificadoDigital = certificado,
                    HasProxy = false
                };
                var transporte = new TransporteCaptura(json, status);
                // O override cobre também a construção: nenhuma etapa usa o transporte real.
                using (ApiTransportExecutorFactory.Override(() => transporte))
                using (var servico = CriarServico(operacao, parametro, construtorTipado, configuracao))
                {
                    servico.Executar();

                    var requisicao = Assert.Single(transporte.Requisicoes);
                    var competencia = construtorTipado ? "2026-11-11T00:00:00" : "2026-11-11";
                    var caminho = operacao switch
                    {
                        "Aliquotas" => $"/parametrizacao/4118402/{parametro}/{competencia}/aliquota",
                        "Historico" => $"/parametrizacao/4118402/{parametro}/historicoaliquotas",
                        "Regimes" => $"/parametrizacao/4118402/{parametro}/{competencia}/regimes_especiais",
                        "Beneficio" => $"/parametrizacao/4118402/{parametro}/{competencia}/beneficio",
                        _ => throw new InvalidOperationException()
                    };
                    var uriEsperada = "https://adn.producaorestrita.nfse.gov.br" + caminho;
                    Assert.Equal(uriEsperada, requisicao.RequestUri);
                    Assert.Equal(uriEsperada, configuracao.RequestURI);
                    Assert.Equal(parametro, new Uri(requisicao.RequestUri).Segments[3].TrimEnd('/'));
                    Assert.DoesNotContain("{", requisicao.RequestUri);
                    Assert.Equal("get", requisicao.Method, ignoreCase: true);
                    Assert.Null(requisicao.HttpContent);
                    Assert.Null(requisicao.Body);
                    Assert.Empty(requisicao.Headers);
                    Assert.True(requisicao.UseCertificate);
                    Assert.Same(certificado, requisicao.Certificate);
                    Assert.False(requisicao.UseDefaultCredentials);
                    Assert.False(requisicao.UseProxy);
                    Assert.Null(requisicao.Proxy);
                    Assert.Equal(status, servico.HttpStatusCode);
                    Assert.Equal(mensagem, servico.RetornoWSXML.SelectSingleNode("/temp/mensagem").InnerText);
                    var retorno = new XmlDocument { XmlResolver = null };
                    using (var reader = XmlReader.Create(new System.IO.StringReader(servico.RetornoWSString),
                        new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
                    {
                        retorno.Load(reader);
                    }
                    Assert.Equal(mensagem, retorno.SelectSingleNode("/temp/mensagem").InnerText);
                }
            }
        }

        private static Unimake.Business.DFe.Servicos.NFSe.ServicoBase CriarServico(
            string operacao, string parametro, bool construtorTipado, Configuracao configuracao)
        {
            var competencia = new DateTime(2026, 11, 11);
            if (construtorTipado)
            {
                return operacao switch
                {
                    "Aliquotas" => new ConsultarAliquotasMunicipais(4118402, parametro, competencia, configuracao),
                    "Historico" => new ConsultarHistoricoAliquotasMunicipais(4118402, parametro, configuracao),
                    "Regimes" => new ConsultarRegimesEspeciaisMunicipais(4118402, parametro, competencia, configuracao),
                    "Beneficio" => new ConsultarBeneficioMunicipal(4118402, parametro, competencia, configuracao),
                    _ => throw new InvalidOperationException()
                };
            }

            var xml = new XmlDocument { XmlResolver = null };
            var raiz = xml.CreateElement("ConsultaParametros");
            xml.AppendChild(raiz);
            AdicionarElemento(xml, raiz, "codigoMunicipio", "4118402");
            AdicionarElemento(xml, raiz, operacao == "Beneficio" ? "numeroBeneficio" : "codigoServico", parametro);
            AdicionarElemento(xml, raiz, "competencia", "2026-11-11");
            AdicionarElemento(xml, raiz, "tipoParametro", operacao.ToLowerInvariant());
            return operacao switch
            {
                "Aliquotas" => new ConsultarAliquotasMunicipais(xml, configuracao),
                "Historico" => new ConsultarHistoricoAliquotasMunicipais(xml, configuracao),
                "Regimes" => new ConsultarRegimesEspeciaisMunicipais(xml, configuracao),
                "Beneficio" => new ConsultarBeneficioMunicipal(xml, configuracao),
                _ => throw new InvalidOperationException()
            };
        }

        private static void AdicionarElemento(XmlDocument xml, XmlElement raiz, string nome, string valor)
        {
            var elemento = xml.CreateElement(nome);
            elemento.InnerText = valor;
            raiz.AppendChild(elemento);
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=NFSE PARAMETROS TESTE OFFLINE", rsa,
                    HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private sealed class TransporteCaptura : IApiTransportExecutor
        {
            private readonly string _resposta;
            private readonly HttpStatusCode _status;
            internal IList<TransportRequest> Requisicoes { get; } = new List<TransportRequest>();

            internal TransporteCaptura(string resposta, HttpStatusCode status)
            {
                _resposta = resposta;
                _status = status;
            }

            public TransportResponse Execute(TransportRequest request)
            {
                Requisicoes.Add(request);
                return new TransportResponse
                {
                    StatusCode = _status,
                    HttpResponseMessage = new HttpResponseMessage(_status)
                    {
                        Content = new StringContent(_resposta, Encoding.UTF8, "application/json")
                    }
                };
            }
        }
    }
}
