using System;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;
using Unimake.DFe.Test.Utility.TesteValidacao.Matchers;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Vinculadores
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
        /// Cria um vinculador para modais baseado no tipo de DFe.
        /// </summary>
        public static IVinculadorSchema CriarVinculadorModal(TipoDFe tipoDFe)
        {
            return tipoDFe switch
            {
                TipoDFe.CTe => new VincularCTe(),
                TipoDFe.MDFe => new VincularMDFe(),
                _ => throw new InvalidOperationException($"Tipo de DFe {tipoDFe} não suporta vinculação de modal."),
            };
        }

        /// <summary>
        /// Cria um vinculador para eventos (funciona para todos os DFe).
        /// </summary>
        public static IVinculadorSchema CriarVinculadorEvento()
        {
            return new VincularEvento();
        }

        /// <summary>
        /// Cria um vinculador apropriado baseado no tipo de DFe e se é modal ou evento.
        /// </summary>
        /// <remarks>
        /// Esta é a forma mais conveniente de criar vinculadores automaticamente.
        /// O padrão detecta se tem TagEvento ou não na configuração.
        /// </remarks>
        public static IVinculadorSchema Criar(TipoDFe tipoDFe, bool isEvento)
        {
            return isEvento
                ? CriarVinculadorEvento()
                : CriarVinculadorModal(tipoDFe);
        }
    }
}