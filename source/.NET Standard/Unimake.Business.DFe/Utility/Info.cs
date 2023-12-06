using System.Reflection;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Informações diversas sobre a DLL
    /// </summary>
    public static class Info
    {
        /// <summary>
        /// Versão de compilação da DLL
        /// </summary>
        public static string VersaoDLL => $"{Assembly.GetExecutingAssembly().GetName().Version.Major:0000}{Assembly.GetExecutingAssembly().GetName().Version.Minor:00}{Assembly.GetExecutingAssembly().GetName().Version.Build:00}{Assembly.GetExecutingAssembly().GetName().Version.Revision:0000}";
    }
}