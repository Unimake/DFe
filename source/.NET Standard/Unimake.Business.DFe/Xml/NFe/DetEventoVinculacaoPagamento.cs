using System;
using System.Globalization;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Evento 110300 de vinculação da transação de pagamento no DF-e.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DetEventoVinculacaoPagamento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("detEvento", Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DetEventoVinculacaoPagamento : EventoDetalhe
    {
        /// <summary>
        /// Descrição do evento, conforme e110300_v1.00.xsd.
        /// </summary>
        public override string DescEvento { get; set; } = "Vinculação Pagamento";

        /// <summary>
        /// Versão do leiaute do evento.
        /// </summary>
        public override string Versao { get; set; } = "1.00";

        /// <summary>
        /// Código da UF do órgão autor.
        /// </summary>
        [XmlIgnore]
        public UFBrasil COrgaoAutor { get; set; }

        /// <summary>
        /// Representação XML do código do órgão autor.
        /// </summary>
        [XmlElement("cOrgaoAutor", Order = 1)]
        public string COrgaoAutorField
        {
            get => ((int)COrgaoAutor).ToString(CultureInfo.InvariantCulture);
            set => COrgaoAutor = (UFBrasil)int.Parse(value, CultureInfo.InvariantCulture);
        }

        /// <summary>
        /// Autor do evento. Este schema admite somente a empresa emitente.
        /// </summary>
        [XmlElement("tpAutor", Order = 2)]
        public TipoAutor TpAutor { get; set; } = TipoAutor.EmpresaEmitente;

        /// <summary>
        /// Versão do aplicativo do autor do evento.
        /// </summary>
        [XmlElement("verAplic", Order = 3)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Protocolo de autorização do DF-e.
        /// </summary>
        [XmlElement("nProt", Order = 4)]
        public string NProt { get; set; }

        /// <summary>
        /// Dados de um pagamento. O evento admite exatamente uma ocorrência e não possui nPag.
        /// </summary>
        [XmlElement("gPgto", Order = 5)]
        public EventoPgto GPgto { get; set; }

        internal override void SetValue(PropertyInfo pi)
        {
            if (pi?.Name == nameof(GPgto))
            {
                var serializer = new XmlSerializer(typeof(EventoPgto),
                    new XmlRootAttribute("gPgto") { Namespace = "http://www.portalfiscal.inf.br/nfe" });
                using (var subtree = XmlReader.ReadSubtree())
                {
                    GPgto = (EventoPgto)serializer.Deserialize(subtree);
                }
                return;
            }
            base.SetValue(pi);
        }

        /// <summary>
        /// Grava o detalhe do evento preservando o escape de textos e atributos XML.
        /// </summary>
        public override void WriteXml(XmlWriter writer)
        {
            base.WriteXml(writer);
            const string ns = "http://www.portalfiscal.inf.br/nfe";
            writer.WriteElementString("descEvento", ns, DescEvento);
            writer.WriteElementString("cOrgaoAutor", ns, COrgaoAutorField);
            writer.WriteElementString("tpAutor", ns, ((int)TpAutor).ToString(CultureInfo.InvariantCulture));
            writer.WriteElementString("verAplic", ns, VerAplic);
            writer.WriteElementString("nProt", ns, NProt);
            if (GPgto != null)
            {
                var serializer = new XmlSerializer(typeof(EventoPgto), new XmlRootAttribute("gPgto") { Namespace = ns });
                var namespaces = new XmlSerializerNamespaces();
                namespaces.Add(string.Empty, ns);
                serializer.Serialize(writer, GPgto, namespaces);
            }
        }
    }
}
