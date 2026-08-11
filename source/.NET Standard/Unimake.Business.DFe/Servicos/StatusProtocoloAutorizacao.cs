namespace Unimake.Business.DFe.Servicos
{
    /// <summary>
    /// Classifica exclusivamente os códigos que representam protocolo de autorização por DFe.
    /// Status externos de lote ou consulta não pertencem a esta classificação.
    /// </summary>
    internal static class StatusProtocoloAutorizacao
    {
        internal static bool BPe(int cStat) => cStat == 100 || cStat == 150;

        internal static bool CTe(int cStat) => cStat == 100 || cStat == 150;

        internal static bool DCe(int cStat) => cStat == 100 || cStat == 150;

        internal static bool MDFe(int cStat) => cStat == 100;

        internal static bool NF3e(int cStat) => cStat == 100 || cStat == 150;

        internal static bool NFCom(int cStat) => cStat == 100 || cStat == 150;

        internal static bool NFe(int cStat) => cStat == 100 || cStat == 120 || cStat == 150;

        internal static bool NFGas(int cStat) => cStat == 100 || cStat == 150;
    }
}
