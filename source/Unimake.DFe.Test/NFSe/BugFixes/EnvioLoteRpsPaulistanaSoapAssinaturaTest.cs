using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.DFe.Test.NFSe.Utilitarios;
using Xunit;

namespace Unimake.DFe.Test.NFSe.BugFixes
{
    /// <summary>
    /// Regressões do envio SOAP da PAULISTANA.
    /// </summary>
    public class EnvioLoteRpsPaulistanaSoapAssinaturaTest
    {
        /// <summary>
        /// Verifica se o transporte SOAP recebe o XML com a assinatura transformada pela PAULISTANA.
        /// </summary>
        /// <param name="coletarTelemetriaDisponibilidade">Indica se a telemetria passiva deve ficar habilitada.</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(false)]
        [InlineData(true)]
        public async Task DeveEnviarConteudoXMLAssinadoNoSoap(bool coletarTelemetriaDisponibilidade)
        {
            const string assinaturaOriginal = "AAECAwQFBgcICQoLDA0ODw==";

            using (var servidor = new ServidorSoapLocal())
            {
                var conteudoXML = new XmlDocument();
                conteudoXML.Load(@"..\..\..\NFSe\Resources\PAULISTANA\2.00\EnvioLoteRps-env-loterps.xml");

                var configuracao = new Configuracao
                {
                    TipoDFe = TipoDFe.NFSe,
                    CertificadoDigital = PropConfig.CertificadoDigital,
                    TipoAmbiente = TipoAmbiente.Producao,
                    CodigoMunicipio = 3550308,
                    Servico = Servico.NFSeEnvioLoteRps,
                    SchemaVersao = "2.00",
                    ColetarTelemetriaDisponibilidade = coletarTelemetriaDisponibilidade,
                    TimeOutWebServiceConnect = 5000
                };

                var envioLoteRps = new EnvioLoteRps(conteudoXML, configuracao);
                envioLoteRps.Configuracoes.WebEnderecoProducao = servidor.Url;

                var requisicao = servidor.AguardarRequisicaoAsync();

                envioLoteRps.Executar();

                var envelopeSoap = await requisicao;

                Assert.Contains("<Assinatura>", envelopeSoap);
                Assert.DoesNotContain("<Assinatura>" + assinaturaOriginal + "</Assinatura>", envelopeSoap);
            }
        }

        private sealed class ServidorSoapLocal : System.IDisposable
        {
            private readonly TcpListener listener;

            public ServidorSoapLocal()
            {
                listener = new TcpListener(IPAddress.Loopback, 0);
                listener.Start();

                var endpoint = (IPEndPoint)listener.LocalEndpoint;
                Url = "http://127.0.0.1:" + endpoint.Port + "/";
            }

            public string Url { get; }

            public async Task<string> AguardarRequisicaoAsync()
            {
                using (var client = await listener.AcceptTcpClientAsync())
                using (var stream = client.GetStream())
                {
                    var request = await LerRequisicaoAsync(stream);
                    var responseBody = "<Resposta>&lt;RetornoEnvioLoteRPS xmlns=\"http://www.prefeitura.sp.gov.br/nfe\" Versao=\"2\" /&gt;</Resposta>";
                    var responseBytes = Encoding.UTF8.GetBytes(responseBody);
                    var responseHeader =
                        "HTTP/1.1 200 OK\r\n" +
                        "Content-Type: text/xml; charset=utf-8\r\n" +
                        "Content-Length: " + responseBytes.Length + "\r\n" +
                        "Connection: close\r\n\r\n";

                    var headerBytes = Encoding.ASCII.GetBytes(responseHeader);
                    await stream.WriteAsync(headerBytes, 0, headerBytes.Length);
                    await stream.WriteAsync(responseBytes, 0, responseBytes.Length);

                    return request.Body;
                }
            }

            public void Dispose() => listener.Stop();

            private static async Task<RequisicaoHttp> LerRequisicaoAsync(NetworkStream stream)
            {
                var bytes = new List<byte>();
                var buffer = new byte[4096];
                var fimCabecalho = -1;

                while (fimCabecalho < 0)
                {
                    var lidos = await stream.ReadAsync(buffer, 0, buffer.Length);
                    Assert.True(lidos > 0, "A conexão foi encerrada antes do cabeçalho HTTP.");

                    bytes.AddRange(buffer.Take(lidos));
                    fimCabecalho = EncontrarFimCabecalho(bytes);
                }

                var headerText = Encoding.ASCII.GetString(bytes.Take(fimCabecalho).ToArray());
                var contentLength = ObterContentLength(headerText);
                var inicioBody = fimCabecalho + 4;

                while (bytes.Count - inicioBody < contentLength)
                {
                    var lidos = await stream.ReadAsync(buffer, 0, buffer.Length);
                    Assert.True(lidos > 0, "A conexão foi encerrada antes do corpo HTTP.");

                    bytes.AddRange(buffer.Take(lidos));
                }

                var body = Encoding.UTF8.GetString(bytes.Skip(inicioBody).Take(contentLength).ToArray());
                return new RequisicaoHttp { Body = body };
            }

            private static int EncontrarFimCabecalho(List<byte> bytes)
            {
                for (var i = 3; i < bytes.Count; i++)
                {
                    if (bytes[i - 3] == '\r' && bytes[i - 2] == '\n' && bytes[i - 1] == '\r' && bytes[i] == '\n')
                    {
                        return i - 3;
                    }
                }

                return -1;
            }

            private static int ObterContentLength(string headerText)
            {
                foreach (var line in headerText.Split('\n'))
                {
                    var parts = line.Split(new[] { ':' }, 2);
                    if (parts.Length == 2 && parts[0].Trim().Equals("Content-Length", System.StringComparison.OrdinalIgnoreCase))
                    {
                        return int.Parse(parts[1].Trim());
                    }
                }

                return 0;
            }

            private sealed class RequisicaoHttp
            {
                public string Body { get; set; }
            }
        }
    }
}
