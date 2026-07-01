using System;
using Unimake.Business.DFe.Interfaces;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.Validar.Extractors;

namespace Unimake.Business.DFe.Isoladores
{
    /// <summary>
    /// Factory para criar isoladores apropriados baseado no tipo de DFe.
    /// Centraliza a lógica de roteamento de isoladores.
    /// </summary>
    internal static class IsoladorFactory
    {
        public static IXmlEspecificoIsolador CriarIsolador(TipoDFe tipoDFe, bool isEvento)
        {
            switch (tipoDFe)
            {
                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    return new IsoladorNFe();
                case TipoDFe.CTe:
                    if (isEvento)
                    {
                        return new IsoladorEventoCTe();
                    } 
                    return new IsoladorModalCTe();
                case TipoDFe.DCe:
                    return new IsoladorDCe();
                case TipoDFe.MDFe:
                    return new IsoladorMDFe();
                case TipoDFe.NFCom:
                    return new IsoladorNFCom();
                case TipoDFe.NF3e:
                    return new IsoladorNF3e();
                case TipoDFe.NFGas:
                    return new IsoladorNFGas();
                case TipoDFe.ESocial:
                    return new IsoladorESocial();
                case TipoDFe.EFDReinf:
                    return new IsoladorEFDReinf();
                default:
                    throw new InvalidOperationException($"Não existe um isolador de XML configurado para o tipo de DFe '{tipoDFe}'.");


            }


        }
    }
}