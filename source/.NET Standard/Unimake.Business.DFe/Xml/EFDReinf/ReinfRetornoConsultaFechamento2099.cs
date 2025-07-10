#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// Classe de retorno do consulta fechamento R-2099 do EFD Reinf
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfRetornoRecibo")]
    [ComVisible(true)]
#endif
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02", IsNullable = false)]
    public class ReinfRetornoConsultaFechamento2099 : XMLBase
    {
        [XmlElement("evtTotalContrib")]
        public EvtTotalContrib EvtTotalContrib { get; set; }
    }
}
