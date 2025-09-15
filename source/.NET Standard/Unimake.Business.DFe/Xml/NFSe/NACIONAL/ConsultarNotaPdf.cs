#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Envio para consulta de PDF da NFS-e (Padrão Nacional).
    /// Estrutura idêntica ao ConsultarNfse: &lt;NFSe versao="1.00" ...&gt;&lt;infNFSe Id="..."/&gt;&lt;/NFSe&gt;
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.Nacional.ConsultarNfsePDFEnvio")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFSe", Namespace = "http://www.sped.fazenda.gov.br/nfse", IsNullable = false)]
    public class ConsultarNfsePDFEnvio : ConsultarNfseEnvio
    {
    }
}
