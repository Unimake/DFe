#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <inheritdoc/>
    /// <remarks>
    /// Estrutura idêntica a <see cref="ConsultarNfse"/>, 
    /// utilizada especificamente para a **consulta de PDF da NFS-e** 
    /// no padrão NACIONAL.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.Nacional.ConsultarNfsePDF")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFSe", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsultarNfsePDFEnvio : ConsultarNfse
    {
    }
}
