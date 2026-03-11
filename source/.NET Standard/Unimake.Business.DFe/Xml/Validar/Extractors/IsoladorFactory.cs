using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Interfaces;

namespace Unimake.Business.DFe.Isoladores
{
    /// <summary>
    /// Factory para criar isoladores apropriados baseado no tipo de DFe.
    /// Centraliza a lógica de roteamento de isoladores.
    /// </summary>
    internal static class IsoladorFactory
    {
        public static IXmlEspecificoIsolador CriarIsolador(TipoDFe tipoDFe)
        {
            switch (tipoDFe)
            {
                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    return new IsoladorNFe();
                case TipoDFe.CTe:
                    return new IsoladorCTe();
                case TipoDFe.MDFe:
                    return new IsoladorMDFe();
                case TipoDFe.NFCom:
                    return new IsoladorNFCom();
                case TipoDFe.NF3e:
                    return new IsoladorNF3e();

                default:
                    throw new InvalidOperationException($"Tipo de DFe não suportado: {tipoDFe}");


            }


        }
    }
}