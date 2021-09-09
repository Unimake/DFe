#pragma warning disable CS1591

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
    [Serializable()]
    [XmlRoot("retConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsCad : NFe.RetConsCad
    {
    }
}