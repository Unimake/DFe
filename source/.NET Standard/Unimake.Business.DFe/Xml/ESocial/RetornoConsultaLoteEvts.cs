#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoConsultaLoteEvts")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoProcessamento/v1_3_0", IsNullable = true)]
    public class RetornoConsultaLoteEvts : XMLBase
    {
        [XmlElement("retornoProcessamentoLoteEventos")]
        public RetornoProcessamentoLoteEventos RetornoProcessamentoLoteEventos { get; set; }

    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoProcessamentoLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoProcessamentoLoteEventos
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        [XmlElement("status")]
        public StatusRetorno StatusRetorno { get; set; }

        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLoteRetorno DadosRecepcaoLote { get; set; }

        [XmlElement("dadosProcessamentoLote")]
        public DadosProcessamentoLote DadosProcessamentoLote { get; set; }

        [XmlElement("retornoEventos")]
        public RetornoEventosESocial RetornoEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.StatusRetorno")]
    [ComVisible(true)]
#endif
    public class StatusRetorno
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosRecepcaoLoteRetorno")]
    [ComVisible(true)]
#endif
    public class DadosRecepcaoLoteRetorno
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
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff");
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoEventosESocial")]
    [ComVisible(true)]
#endif
    public class RetornoEventosESocial
    {
        [XmlElement("evento")]
        public List<RetornoEventoESocial> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(RetornoEventoESocial item)
        {
            if (Evento == null)
            {
                Evento = new List<RetornoEventoESocial>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public RetornoEventoESocial GetEvento(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoEventoESocial")]
    [ComVisible(true)]
#endif
    public class RetornoEventoESocial
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("retornoEvento")]
        public EventoESocialRetorno RetornoEvento { get; set; }

        [XmlElement("tot")]
        public TotEsocial TotEsocial { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotEsocial")]
    [ComVisible(true)]
#endif
    public class TotEsocial
    {
        [XmlAttribute(AttributeName = "tipo", DataType = "token")]
        public string Tipo { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_02_00")]
        public ESocial5001 ESocial5001 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_02_00")]
        public ESocial5002 ESocial5002 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_02_00")]
        public ESocial5011 ESocial5011 { get; set; }

        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrf/v_S_01_02_00")]
        public ESocial5012 ESocial5012 { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventoESocialRetorno")]
    [ComVisible(true)]
#endif
    public class EventoESocialRetorno
    {
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/retornoEvento/v1_2_1")]
        public RetornoDoEventoESocial RetornoEvento { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RetornoDoEventoESocial")]
    [ComVisible(true)]
#endif
    public class RetornoDoEventoESocial
    {
        [XmlElement("retornoEvento")]
        public EventoDeRetornoESocial RetornoEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EventoDeRetornoESocial")]
    [ComVisible(true)]
#endif
    public class EventoDeRetornoESocial
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("recepcao")]
        public RecepcaoESocial Recepcao { get; set; }

        [XmlElement("processamento")]
        public ProcessamentoRetornoESocial Processamento { get; set; }

        [XmlElement("recibo")]
        public ReciboESocial Recibo { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RecepcaoESocial")]
    [ComVisible(true)]
#endif
    public class RecepcaoESocial
    {
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao {get; set; }
#else
        public DateTime DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff");
#if INTEROP
            set => DhRecepcao = DateTime.Parse(value);
#else
            set => DhRecepcao = DateTime.Parse(value);
#endif
        }

        [XmlElement("versaoAppRecepcao")]
        public string VersaoAppRecepcao { get; set; }

        [XmlElement("protocoloEnvioLote")]
        public string ProtocoloEnvioLote { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcessamentoRetornoESocial")]
    [ComVisible(true)]
#endif
    public class ProcessamentoRetornoESocial
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        [XmlElement("versaoAppProcessamento")]
        public string VersaoAppProcessamento { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhProcessamento {get; set; }
#else
        public DateTime DhProcessamento { get; set; }
#endif

        [XmlElement("dhProcessamento")]
        public string DhProcessamentoField
        {
            get => DhProcessamento.ToString("yyyy-MM-ddTHH:mm:ss.fff");
#if INTEROP
            set => DhProcessamento = DateTime.Parse(value);
#else
            set => DhProcessamento = DateTime.Parse(value);
#endif
        }

        [XmlElement("ocorrencias")]
        public OcorrenciasRetornoConsulta Ocorrencias { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.OcorrenciasRetornoConsulta")]
    [ComVisible(true)]
#endif
    public class OcorrenciasRetornoConsulta
    {
        [XmlElement("ocorrencia")]
        public List<OcorrenciaRetConsulta> Ocorrencia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOcorrencia(OcorrenciaRetConsulta item)
        {
            if (Ocorrencia == null)
            {
                Ocorrencia = new List<OcorrenciaRetConsulta>();
            }

            Ocorrencia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Ocorrencia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Ocorrencia</returns>
        public OcorrenciaRetConsulta GetOcorrencia(int index)
        {
            if ((Ocorrencia?.Count ?? 0) == 0)
            {
                return default;
            };

            return Ocorrencia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Ocorrencia
        /// </summary>
        public int GetOcorrenciaCount => (Ocorrencia != null ? Ocorrencia.Count : 0);
#endif    
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.OcorrenciaRetConsulta")]
    [ComVisible(true)]
#endif
    public class OcorrenciaRetConsulta
    {
        [XmlElement("tipo")]
        public int Tipo { get; set; }

        [XmlElement("codigo")]
        public int Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ReciboESocial")]
    [ComVisible(true)]
#endif
    public class ReciboESocial
    {
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        [XmlElement("hash")]
        public string Hash { get; set; }
    }
}