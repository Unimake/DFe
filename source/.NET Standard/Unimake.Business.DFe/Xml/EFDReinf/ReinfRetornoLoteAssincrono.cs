#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfRetornoLoteAssincrono")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/retornoLoteEventosAssincrono/v1_00_00", IsNullable = false)]
    public class ReinfRetornoLoteAssincrono : XMLBase
    {
        [XmlElement("retornoLoteEventosAssincrono")]
        public RetornoLoteEventosAssincrono RetornoLoteEventosAssincrono { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RetornoLoteEventosAssincrono")]
    [ComVisible(true)]
#endif
    public class RetornoLoteEventosAssincrono
    {
        [XmlElement("ideContribuinte")]
        public IdeContribuinte IdeContribuinte { get; set; }

        [XmlElement("status")]
        public Status Status { get; set; }

        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }

        [XmlElement("dadosProcessamentoLote")]
        public DadosProcessamentoLote DadosProcessamentoLote { set; get; }

        [XmlElement("retornoEventos")]
        public RetornoEventosLoteAssincrono RetornoEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Status")]
    [ComVisible(true)]
#endif
    public class Status
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DadosRecepcaoLote")]
    [ComVisible(true)]
#endif
    public class DadosRecepcaoLote
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTimeOffset DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("versaoAplicativoRecepcao")]
        public string VersaoAplicativoRecepcao { get; set; }

        [XmlElement("protocoloEnvio")]
        public string ProtocoloEnvio { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RetornoEventosLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class RetornoEventosLoteAssincrono
    {
        [XmlElement("evento")]
        public List<EventoRetorno> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(EventoRetorno item)
        {
            if (Evento == null)
            {
                Evento = new List<EventoRetorno>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public EventoRetorno GetEvento(int index)
        {
            if ((Evento?.Count ?? 0) == 0)
            {
                return default;
            };

            return Evento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Evento
        /// </summary>
        public int GetEventoCount => (Evento != null ? Evento.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EventoRetorno")]
    [ComVisible(true)]
#endif
    public class EventoRetorno
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("retornoEvento")]
        public RetornoEvento RetornoEvento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RetornoEvento")]
    [ComVisible(true)]
#endif
    public class RetornoEvento
    {
        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotal/v2_01_02")]
        public Reinf9001 Reinf9001 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRet/v2_01_02")]
        public Reinf9005 Reinf9005 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02")]
        public Reinf9011 Reinf9011 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRetCons/v2_01_02")]
        public Reinf9015 Reinf9015 { get; set; }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DadosProcessamentoLote")]
    [ComVisible(true)]
#endif
    public class DadosProcessamentoLote
    {
        [XmlElement("versaoAplicativoProcessamentoLote")]
        public string VersaoAplicativoProcessamentoLote { get; set; }
    }
}
