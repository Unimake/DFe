using System;
using System.Threading;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal static class SoapTransportExecutorFactory
    {
        private static readonly AsyncLocal<Func<ISoapTransportExecutor>> ExecutorFactory = new AsyncLocal<Func<ISoapTransportExecutor>>();

        internal static ISoapTransportExecutor Create()
        {
            return ExecutorFactory.Value == null ? (ISoapTransportExecutor)new SoapTransportExecutor() : ExecutorFactory.Value();
        }

        internal static IDisposable Override(Func<ISoapTransportExecutor> executorFactory)
        {
            if (executorFactory == null)
            {
                throw new ArgumentNullException(nameof(executorFactory));
            }

            var anterior = ExecutorFactory.Value;
            ExecutorFactory.Value = executorFactory;
            return new OverrideScope(anterior);
        }

        private sealed class OverrideScope : IDisposable
        {
            private readonly Func<ISoapTransportExecutor> _anterior;
            private bool _disposed;

            internal OverrideScope(Func<ISoapTransportExecutor> anterior)
            {
                _anterior = anterior;
            }

            public void Dispose()
            {
                if (_disposed)
                {
                    return;
                }

                ExecutorFactory.Value = _anterior;
                _disposed = true;
            }
        }
    }
}
