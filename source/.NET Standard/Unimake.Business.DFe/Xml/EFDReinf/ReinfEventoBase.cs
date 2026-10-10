#pragma warning disable CS1591

using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{   
    /// <summary>
    /// Classe base para Reinf.
    /// </summary>
    public abstract class ReinfEventoBase
    {
        /// <summary>
        /// Identificador do evento. Nos eventos enviados do leiaute 2.1.2, o formato é
        /// ID, um dígito, 14 caracteres alfanuméricos maiúsculos e 19 dígitos, totalizando 36 caracteres.
        /// As restrições de formato são verificadas pela validação de schema.
        /// </summary>
        [XmlAttribute(AttributeName = "id", DataType = "token")]
        public string ID { get; set; }
    }
}
