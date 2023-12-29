using System.Reflection;
using System.Runtime.InteropServices;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Informações diversas sobre a DLL
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public static class Info
    {
        /// <summary>
        /// Versão de compilação da DLL
        /// </summary>
        public static string VersaoDLL => $"{Assembly.GetExecutingAssembly().GetName().Version.Major:0000}{Assembly.GetExecutingAssembly().GetName().Version.Minor:00}{Assembly.GetExecutingAssembly().GetName().Version.Build:00}{Assembly.GetExecutingAssembly().GetName().Version.Revision:0000}";
    }

#if INTEROP

    /// <summary>
    /// Informações diversas sobre a DLL - INTEROP
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.InfoInterop")]
    [ComVisible(true)]
    public class InfoInterop
    {
        /// <summary>
        /// Versão de compilação da DLL
        /// </summary>
        public string VersaoDLL => Info.VersaoDLL;
    }

#endif
}