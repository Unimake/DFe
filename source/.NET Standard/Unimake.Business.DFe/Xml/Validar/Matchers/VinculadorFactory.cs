using System;
using Unimake.Business.DFe.Interfaces;
using Unimake.Business.DFe.Matchers;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.Validar.Matchers;

namespace Unimake.Business.DFe.Vinculadores
{
    /// <summary>
    /// Factory para criar vinculadores apropriados baseado no tipo de DFe.
    /// Centraliza a lógica de roteamento de vinculadores.
    /// 
    /// Tipos de vinculadores:
    /// - VinculadorEvento: para qualquer DFe com eventos (NFe, CTe, MDFe)
    /// - VinculadorCTe: para CTe/CTeSimp/enviCTe (modais)
    /// - VinculadorMDFe: para MDFe/enviMDFe (modais)
    /// </summary>
    internal static class VinculadorFactory
    {
        /// <summary>
        /// Cria um vinculador apropriado baseado no tipo de DFe e se é modal ou evento.
        /// </summary>
        /// <remarks>
        /// Esta é a forma mais conveniente de criar vinculadores automaticamente.
        /// O padrão detecta se tem TagEvento ou não na configuração.
        /// </remarks>
        public static IVinculadorSchema Criar(TipoDFe tipoDFe, bool isEvento)
        {
            switch (tipoDFe)
            {
                case TipoDFe.CTe:
                    if (isEvento)
                    {
                        return new VincularEvento();
                    }
                    return new VincularCTe();

                case TipoDFe.MDFe:
                    if (isEvento)
                    {
                        return new VincularEvento();
                    }
                    return new VincularMDFe();

                case TipoDFe.ESocial:
                    return new VincularEventoESocial();

                case TipoDFe.EFDReinf:
                    return new VincularEventoEFDReinf();

                default:
                    return new VincularEvento(); // Para NFe e outros, o vinculador de evento é genérico
            }
        }
    }
}