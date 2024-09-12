#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial.Retorno
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.ESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/lote/eventos/envio/retornoProcessamento/v1_3_0", IsNullable = true)]
    public class RetornoEventoProcessado : XMLBase
    {
        [XmlElement("retornoProcessamentoLoteEventos")]
        public RetornoProcessamentoLoteEventos RetornoProcessamentoLoteEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Ocorrencias")]
    [ComVisible(true)]
#endif
    public class Ocorrencias
    {
        [XmlElement("ocorrencia")]
        public List<Ocorrencia> Ocorrencia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOcorrencia(Ocorrencia item)
        {
            if (Ocorrencia == null)
            {
                Ocorrencia = new List<Ocorrencia>();
            }

            Ocorrencia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Ocorrencia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Ocorrencia</returns>
        public Ocorrencia GetOcorrencia(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Ocorrencia")]
    [ComVisible(true)]
#endif
    public class Ocorrencia
    {
        [XmlElement("tipo")]
        public int Tipo { get; set; }

        [XmlElement("codigo")]
        public int Codigo { get; set; }

        [XmlElement("descricao")]
        public string Descricao { get; set; }

        [XmlElement("localizacao")]
        public string Localizacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.DadosRecepcaoLote")]
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
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhRecepcao.Millisecond.ToString().EndsWith("0") ? "" : "f"));
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Status")]
    [ComVisible(true)]
#endif
    public class Status
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        [XmlElement("ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.DadosProcessamentoLote")]
    [ComVisible(true)]
#endif
    public class DadosProcessamentoLote
    {
        [XmlElement("versaoAplicativoProcessamentoLote")]
        public string VersaoAplicativoProcessamentoLote { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoProcessamentoLoteEventos")]
    [ComVisible(true)]
#endif
    public class RetornoProcessamentoLoteEventos
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTransmissor")]
        public IdeTransmissor IdeTransmissor { get; set; }

        [XmlElement("status")]
        public Status Status { get; set; }

        [XmlElement("dadosRecepcaoLote")]
        public DadosRecepcaoLote DadosRecepcaoLote { get; set; }

        [XmlElement("dadosProcessamentoLote")]
        public DadosProcessamentoLote DadosProcessamentoLote { get; set; }

        [XmlElement("retornoEventos")]
        public RetornoEventos RetornoEventos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEventos")]
    [ComVisible(true)]
#endif
    public class RetornoEventos
    {
        [XmlElement("evento")]
        public List<Evento> Evento { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEvento(Evento item)
        {
            if (Evento == null)
            {
                Evento = new List<Evento>();
            }

            Evento.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Evento (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Evento</returns>
        public Evento GetEvento(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Evento")]
    [ComVisible(true)]
#endif
    public class Evento
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("retornoEvento")]
        public RetornoEvento RetornoEvento { get; set; }

        [XmlElement("tot")]
        public Tot Tot { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Tot")]
    [ComVisible(true)]
#endif
    public class Tot
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEvento")]
    [ComVisible(true)]
#endif
    public class RetornoEvento
    {
        [XmlElement("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/retornoEvento/v1_2_1")]
        public RetornoEventoESocial ESocial { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.RetornoEventoESocial")]
    [ComVisible(true)]
#endif
    public class RetornoEventoESocial
    {
        [XmlElement("retornoEvento")]
        public EventoRetorno RetornoEvento { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.EventoRetorno")]
    [ComVisible(true)]
#endif
    public class EventoRetorno
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("recepcao")]
        public Recepcao Recepcao { get; set; }

        [XmlElement("processamento")]
        public Processamento Processamento { get; set; }

        [XmlElement("recibo")]
        public Recibo Recibo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Recepcao")]
    [ComVisible(true)]
#endif
    public class Recepcao
    {
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecepcao { get; set; }
#else
        public DateTime DhRecepcao { get; set; }
#endif

        [XmlElement("dhRecepcao")]
        public string DhRecepcaoField
        {
            get => DhRecepcao.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhRecepcao.Millisecond.ToString().EndsWith("0") ? "" : "f"));
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Processamento")]
    [ComVisible(true)]
#endif
    public class Processamento
    {
        [XmlElement("cdResposta")]
        public int CdResposta { get; set; }

        [XmlElement("descResposta")]
        public string DescResposta { get; set; }

        [XmlElement("versaoAppProcessamento")]
        public string VersaoAppProcessamento { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhProcessamento { get; set; }
#else
        public DateTime DhProcessamento { get; set; }
#endif

        [XmlElement("dhProcessamento")]
        public string DhProcessamentoField
        {
            get => DhProcessamento.ToString("yyyy-MM-ddTHH:mm:ss.ff" + (DhProcessamento.Millisecond.ToString().EndsWith("0") ? "" : "f"));
#if INTEROP
            set => DhProcessamento = DateTime.Parse(value);
#else
            set => DhProcessamento = DateTime.Parse(value);
#endif
        }

        [XmlElement("ocorrencias")]
        public Ocorrencias Ocorrencias { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Retorno.Recibo")]
    [ComVisible(true)]
#endif
    public class Recibo
    {
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        [XmlElement("hash")]
        public string Hash { get; set; }
    }
}
