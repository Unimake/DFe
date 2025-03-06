#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetEventoCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retEventoCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetEventoCTe
    {
        /// <summary>
        /// Informações do evento
        /// </summary>
        [XmlElement("infEvento", Order = 0)]
        public RetEventoCTeInfEvento InfEvento { get; set; }

        /// <summary>
        /// Versão do leiaute XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetEventoCTeInfEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class RetEventoCTeInfEvento
    {
        /// <summary>
        /// Identificador da TAG a ser assinada
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo do Ambiente
        /// </summary>
        [XmlElement("tpAmb", Order = 0)]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do Aplicativo
        /// </summary>
        [XmlElement("verAplic", Order = 1)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do órgão de recepção do Evento
        /// </summary>
        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "COrgao" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cOrgao", Order = 2)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Código do status da resposta
        /// </summary>
        [XmlElement("cStat", Order = 3)]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta
        /// </summary>
        [XmlElement("xMotivo", Order = 4)]
        public string XMotivo { get; set; }

        /// <summary>
        /// Chave de acesso do CT-e
        /// </summary>
        [XmlElement("chCTe", Order = 5)]
        public string ChCTe { get; set; }

        /// <summary>
        /// Tipo do Evento
        /// </summary>
        [XmlElement("tpEvento", Order = 6)]
        public TipoEventoCTe TpEvento { get; set; }

        /// <summary>
        /// Descrição do Evento
        /// </summary>
        [XmlElement("xEvento", Order = 7)]
        public string XEvento { get; set; }

        /// <summary>
        /// Sequencial do evento
        /// </summary>
        [XmlElement("nSeqEvento", Order = 8)]
        public int NSeqEvento { get; set; }

        /// <summary>
        /// Data e hora do registro do evento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhRegEvento" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhRegEvento", Order = 12)]
        public string DhRegEventoField
        {
            get => DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo do evento
        /// </summary>
        [XmlElementAttribute("nProt", Order = 13)]
        public string NProt { get; set; }
    }
}
