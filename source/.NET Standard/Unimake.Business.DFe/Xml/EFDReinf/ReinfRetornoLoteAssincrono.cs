#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;

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
        public List<RetornoEventosLoteAssincrono> RetornoEventos { get; set; }
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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.DadosProcessamentoLote")]
    [ComVisible(true)]
#endif
    public class DadosProcessamentoLote
    {
        [XmlElement("versaoAplicativoProcessamentoLote")]
        public string VersaoAplicativoProcessamentoLote { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.RetornoEventosLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class RetornoEventosLoteAssincrono
    {
        [XmlElement("evento")]
        public EventoRetorno Evento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EventoRetorno")]
    [ComVisible(true)]
#endif
    public class EventoRetorno : ReinfEventoBase
    {
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
        public List<Reinf9001> Reinf9001 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRet/v2_01_02")]
        public List<Reinf9005> Reinf9005 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTotalContrib/v2_01_02")]
        public List<Reinf9011> Reinf9011 { get; set; }

        [XmlElement("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtRetCons/v2_01_02")]
        public List<Reinf9015> Reinf9015 { get; set; }

#if INTEROP

        #region Reinf9001

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9001(Reinf9001 item)
        {
            if (Reinf9001 == null)
            {
                Reinf9001 = new List<Reinf9001>();
            }

            Reinf9001.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9001</returns>
        public Reinf9001 GetReinf9001(int index)
        {
            if ((Reinf9001?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9001[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9001
        /// </summary>
        public int GetReinf9001Count => (Reinf9001 != null ? Reinf9001.Count : 0);

        #endregion

        #region Reinf9005

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9005(Reinf9005 item)
        {
            if (Reinf9005 == null)
            {
                Reinf9005 = new List<Reinf9005>();
            }

            Reinf9005.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9005 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9005</returns>
        public Reinf9005 GetReinf9005(int index)
        {
            if ((Reinf9005?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9005[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9005
        /// </summary>
        public int GetReinf9005Count => (Reinf9005 != null ? Reinf9005.Count : 0);

        #endregion

        #region Reinf9011

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9011(Reinf9011 item)
        {
            if (Reinf9011 == null)
            {
                Reinf9011 = new List<Reinf9011>();
            }

            Reinf9011.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9011 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9011</returns>
        public Reinf9011 GetReinf9011(int index)
        {
            if ((Reinf9011?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9011[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9011
        /// </summary>
        public int GetReinf9011Count => (Reinf9011 != null ? Reinf9011.Count : 0);

        #endregion

        #region Reinf9015

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddReinf9015(Reinf9015 item)
        {
            if (Reinf9015 == null)
            {
                Reinf9015 = new List<Reinf9015>();
            }

            Reinf9015.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Reinf9015 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reinf9015</returns>
        public Reinf9015 GetReinf9015(int index)
        {
            if ((Reinf9015?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reinf9015[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reinf9015
        /// </summary>
        public int GetReinf9015Count => (Reinf9015 != null ? Reinf9015.Count : 0);

        #endregion

#endif
    }
}
