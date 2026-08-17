using System;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores
{
    internal static class ProvedorCIOTFactory
    {
        internal static IProvedorCIOT Criar(Servicos.ProvedorCIOT provedor)
        {
            switch (provedor)
            {
                case Servicos.ProvedorCIOT.ANTT:
                    return new ANTT.ProvedorANTT();
                case Servicos.ProvedorCIOT.EFrete:
                    return new EFrete.ProvedorEFrete();
                default:
                    throw new NotSupportedException("O provedor de CIOT " + provedor + " não está implementado.");
            }
        }
    }
}
