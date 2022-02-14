#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.ComponentModel;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Formatar/Definir máscara de valores
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.Format")]
    [ComVisible(true)]
#endif
    public static class Format
    {
        /// <summary>
        /// Formatar a string da chave da NFe, MDFe, CTe, CTeOS em um formato com uma máscara
        /// </summary>
        /// <returns>Chave formatada, com uma mascara</returns>
        /// <example>
        /// var chave = "35220100000000000100580020000004351000005350";
        /// MessageBox.Show(Format.ChaveDFe(chave)); //Resultado será: 35-2201-00000000000100-58-002-000000435-1-00000535-0
        /// </example>
        public static string ChaveDFe(string chave)
        {
            var mask = new MaskedTextProvider("##-####-##############-##-###-#########-#-########-#");
            mask.Set(chave, out _, out var hint);

            if (hint == MaskedTextResultHint.Success)
            {
                return mask.ToString();
            }
            else
            {
                return chave;
            }
        }
    }
}
