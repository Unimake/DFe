using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [CollectionDefinition(Nome)]
    public class AACBSExecucaoCollection
    {
        internal const string Nome = "AACBS.Execucao";
    }

    internal sealed class AACBSTransporteControlado : IApiTransportExecutor
    {
        private readonly Queue<Func<TransportResponse>> respostas = new();

        internal List<Requisicao> Requisicoes { get; } = new();

        internal void Enfileirar(HttpStatusCode status, string corpo, string tipoMidia = "application/json")
        {
            respostas.Enqueue(() => new TransportResponse
            {
                StatusCode = status,
                HttpResponseMessage = new HttpResponseMessage(status)
                {
                    Content = new StringContent(corpo, Encoding.UTF8, tipoMidia)
                }
            });
        }

        internal void EnfileirarFalha(Exception excecao) => respostas.Enqueue(() => throw excecao);

        public TransportResponse Execute(TransportRequest request)
        {
            Func<TransportResponse> responder;
            lock (respostas)
            {
                Requisicoes.Add(new Requisicao(request));
                if (!respostas.TryDequeue(out responder))
                {
                    throw new InvalidOperationException("Requisição inesperada no transporte AACBS controlado.");
                }
            }

            return responder();
        }

        internal sealed class Requisicao
        {
            internal TransportRequest Transporte { get; }
            internal string Corpo { get; }
            internal string TipoMidia { get; }
            internal string Charset { get; }
            internal IReadOnlyDictionary<string, string> Headers { get; }

            internal Requisicao(TransportRequest request)
            {
                Transporte = request;
                Corpo = request.HttpContent?.ReadAsStringAsync().GetAwaiter().GetResult();
                TipoMidia = request.HttpContent?.Headers.ContentType?.MediaType;
                Charset = request.HttpContent?.Headers.ContentType?.CharSet;
                Headers = request.Headers.ToDictionary(
                    header => header.Key, header => header.Value, StringComparer.OrdinalIgnoreCase);
            }
        }
    }
}
