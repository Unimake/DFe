using Unimake.Business.DFe.ConsumirServico.Contracts;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal interface ISoapTransportExecutor
    {
        TransportResponse Execute(TransportRequest request);
    }
}
