#pragma warning disable CS1591

using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{   
    /// <summary>
    /// Classe base para Reinf.
    /// </summary>
    public abstract class ReinfEventoBase
    {
        [XmlAttribute(AttributeName = "id", DataType = "token")]
        public string ID { get; set; }
    }
}
