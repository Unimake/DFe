using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal interface IApiTransportExecutor
    {
        TransportResponse Execute(TransportRequest request);
    }
}
