using System;
using System.Threading;

namespace Unimake.Business.DFe.ConsumirServico.Transport
{
    internal static class ApiTransportExecutorFactory
    {
        private static readonly AsyncLocal<Func<IApiTransportExecutor>> ExecutorFactory = new AsyncLocal<Func<IApiTransportExecutor>>();

        internal static IApiTransportExecutor Create()
        {
            return ExecutorFactory.Value == null ? (IApiTransportExecutor)new ApiTransportExecutor() : ExecutorFactory.Value();
        }

        internal static IDisposable Override(Func<IApiTransportExecutor> executorFactory)
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
            private readonly Func<IApiTransportExecutor> _anterior;
            private bool _disposed;

            internal OverrideScope(Func<IApiTransportExecutor> anterior)
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
