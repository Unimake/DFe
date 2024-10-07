#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/envioLoteEventosAssincrono/v1_00_00", IsNullable = true)]
    public class ReinfEnvioLoteEventos : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.00.00";

        [XmlElement("envioLoteEventos")]
        public EnvioLoteEventosReinf EnvioLoteEventos { get; set; }

        /// <summary>
        /// Desserializar o XML de Lote de Eventos do EFDReinf no objeto ReinfEnvioLoteEventos
        /// </summary>
        /// <param name="filename">Localização do arquivo XML de lote de eventos do EFDReinf</param>
        /// <returns>Objeto do ReinfEnvioLoteEventos</returns>
        public ReinfEnvioLoteEventos LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<ReinfEnvioLoteEventos>(doc);
        }

        /// <summary>
        /// Desserializar o XML de Lote de Eventos do EFDReinf no objeto ReinfEnvioLoteEventos
        /// </summary>
        /// <param name="xml">string do XML de lote de eventos do EFDReinf</param>
        /// <returns>Objeto do ReinfEnvioLoteEventos</returns>
        public ReinfEnvioLoteEventos LoadFromXML(string xml) => XMLUtility.Deserializar<ReinfEnvioLoteEventos>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EnvioLoteEventos")]
    [ComVisible(true)]
#endif
    public class EnvioLoteEventosReinf
    {
        [XmlElement("ideContribuinte")]
        public IdeContribuinte IdeContribuinte { get; set; }

        [XmlElement("eventos")]
        public EventosReinf Eventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContribuinte")]
    [ComVisible(true)]
#endif
    public class IdeContribuinte
    {
        [XmlElement("tpInsc")]
#if INTEROP
        public TiposInscricao TpInsc { get; set; } = (TiposInscricao)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EventosReinf")]
    [ComVisible(true)]
#endif
    public class EventosReinf
    {
        [XmlElement("evento")]
        public List<EventoReinf> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(EventoReinf item)
        {
            if (Evento == null)
            {
                Evento = new List<EventoReinf>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista EventoReinf (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public EventoReinf GetEvento(int index)
        {
            if ((Evento?.Count ?? 0) == 0)
            {
                return default;
            };

            return Evento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista EventoReinf
        /// </summary>
        public int GetEventoCount => (Evento != null ? Evento.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Evento")]
    [ComVisible(true)]
#endif
    public class EventoReinf
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02")]
        public Reinf1000 Reinf1000 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt1050TabLig/v2_01_02")]
        public Reinf1050 Reinf1050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02")]
        public Reinf1070 Reinf1070 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTomadorServicos/v2_01_02")]
        public Reinf2010 Reinf2010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtPrestadorServicos/v2_01_02")]
        public Reinf2020 Reinf2020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRecebidoAssociacao/v2_01_02")]
        public Reinf2030 Reinf2030 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRecursoRepassadoAssociacao/v2_01_02")]
        public Reinf2040 Reinf2040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoProdRural/v2_01_02")]
        public Reinf2050 Reinf2050 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt2055AquisicaoProdRural/v2_01_02")]
        public Reinf2055 Reinf2055 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoCPRB/v2_01_02")]
        public Reinf2060 Reinf2060 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtReabreEvPer/v2_01_02")]
        public Reinf2098 Reinf2098 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtFechamento/v2_01_02")]
        public Reinf2099 Reinf2099 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtEspDesportivo/v2_01_02")]
        public Reinf3010 Reinf3010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4010PagtoBeneficiarioPF/v2_01_02")]
        public Reinf4010 Reinf4010 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4020PagtoBeneficiarioPJ/v2_01_02")]
        public Reinf4020 Reinf4020 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4040PagtoBenefNaoIdentificado/v2_01_02")]
        public Reinf4040 Reinf4040 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4080RetencaoRecebimento/v2_01_02")]
        public Reinf4080 Reinf4080 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4099FechamentoDirf/v2_01_02")]
        public Reinf4099 Reinf4099 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtExclusao/v2_01_02")]
        public Reinf9000 Reinf9000 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02")]
        public Reinf9001 Reinf9001 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRet/v2_01_02")]
        public Reinf9005 Reinf9005 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02")]
        public Reinf9011 Reinf9011 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRetCons/v2_01_02")]
        public Reinf9015 Reinf9015 { get; set; }

    }

}