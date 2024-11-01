#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.DARE
{
    /// <summary>
    /// Classe de retorno DARE Único
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DARERetorno")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("DareRetorno", Namespace = "https://portal.fazenda.sp.gov.br/servicos/dare", IsNullable = true)]
    public class DARERetorno : XMLBase
    {
        [XmlElement("DARE")]
        public DAREUnicoRetorno DARE { get; set; }
    }

    /// <summary>
    /// Classe de retorno com as informações do DARE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DARE.DAREUnicoRetorno")]
    [ComVisible(true)]
#endif
    public class DAREUnicoRetorno : DARE { }
}
