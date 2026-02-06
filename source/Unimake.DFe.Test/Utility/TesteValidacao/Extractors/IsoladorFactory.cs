using System;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.Utility.TesteValidacao.Extractors;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Isoladores
{
    /// <summary>
    /// Factory para criar isoladores apropriados baseado no tipo de DFe.
    /// Centraliza a lógica de roteamento de isoladores.
    /// </summary>
    internal static class IsoladorFactory
    {
        public static IXmlEspecificoIsolador CriarIsolador(TipoDFe tipoDFe)
        {
            return tipoDFe switch
            {
                TipoDFe.NFe or TipoDFe.NFCe => new IsoladorNFe(),
                TipoDFe.CTe => new IsoladorCTe(),
                TipoDFe.MDFe => new IsoladorMDFe(),
                _ => throw new InvalidOperationException($"Tipo de DFe não suportado: {tipoDFe}"),
            };
        }
    }
}