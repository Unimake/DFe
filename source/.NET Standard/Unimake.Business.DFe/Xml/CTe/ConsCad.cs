#pragma warning disable CS1591

using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
    [XmlRoot("ConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsCad : NFe.ConsCadBase
    {

    }
}