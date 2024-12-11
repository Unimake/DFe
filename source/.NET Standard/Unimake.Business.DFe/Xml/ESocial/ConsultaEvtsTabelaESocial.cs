#pragma warning disable CS1591
using System;
using System.Xml;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    #region Consultar Empregador

    /// <summary>
    /// Consultar os eventos do empregador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsEmpregadorESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/empregador/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsEmpregadorESocial : XMLBase
    {
        /// <summary>
        /// Versão da consulta
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        /// <summary>
        /// Contém os parâmetros da consulta aos identificadores dos eventos.
        /// </summary>
        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consulta Empregador

    #region Consultar Tabela

    /// <summary>
    /// Consultar os eventos de tabela
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTabelaESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/tabela/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsTabelaESocial : XMLBase
    {
        /// <summary>
        /// Versão da consulta
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        /// <summary>
        /// Contém os parâmetros da consulta aos identificadores dos eventos.
        /// </summary>
        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consultar Tabela

    #region Consultar Trabalhador

    /// <summary>
    /// Consultar os eventos de trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTrabalhadorESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/trabalhador/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsTrabalhadorESocial : XMLBase
    {
        /// <summary>
        /// Versão da consulta
        /// </summary>
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        /// <summary>
        /// Contém os parâmetros da consulta aos identificadores dos eventos.
        /// </summary>
        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consulta Trabalhador

    /// <summary>
    /// Contém os parâmetros da consulta aos identificadores dos eventos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaIdentificadoresEvts")]
    [ComVisible(true)]
#endif
    public class ConsultaIdentificadoresEvts
    {
        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Contém o filtro para consulta aos eventos de tabela.
        /// </summary>
        [XmlElement("consultaEvtsTabela")]
        public ConsultaEvtsTabela ConsultaEvtsTabela { get; set; }

        /// <summary>
        /// Contém o filtro para consulta aos eventos de trabalhador
        /// </summary>
        [XmlElement("consultaEvtsTrabalhador")]
        public ConsultaEvtsTrabalhador ConsultaEvtsTrabalhador { get; set; }

        /// <summary>
        /// Contém o filtro para consulta aos eventos do empregador que não se enquadram nas categorias de eventos de tabela 
        /// ou eventos periódicos não periódicos do trabalhador.
        /// </summary>
        [XmlElement("consultaEvtsEmpregador")]
        public ConsultaEvtsEmpregador ConsultaEvtsEmpregador { get; set; }
    }

    #region  Consulta Evts Tabela

    /// <summary>
    /// Contém o filtro para consulta aos eventos de tabela.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabela")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsTabela
    {
        /// <summary>
        /// Contém o tipo do evento que será consultado
        /// </summary>
        [XmlElement("tpEvt")]
        public string TpEvt { get; set; }

        /// <summary>
        /// Contém a chave do evento que será consultado
        /// </summary>
        [XmlElement("chEvt")]
        public string ChEvt { get; set; }

        /// <summary>
        /// Contém a data/hora de início do período que será consultado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIni {get; set; }
#else
        public DateTimeOffset DtIni { get; set; }
#endif

        [XmlElement("dtIni")]
        public string DtIniField
        {
            get => DtIni.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DtIni = DateTime.Parse(value);
#else
            set => DtIni = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Contém a data/hora de fim do período que será consultado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtFim {get; set; }
#else
        public DateTimeOffset DtFim { get; set; }
#endif
        [XmlElement("dtFim")]
        public string DtFimField
        {
            get => DtFim.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DtFim = DateTime.Parse(value);
#else
            set => DtFim = DateTimeOffset.Parse(value);
#endif
        }
    }

    #endregion Consulta Evts Tabela

    #region Consulta Evts Trabalhador

    /// <summary>
    /// Contém o filtro para consulta aos eventos de trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTrabalhador")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsTrabalhador
    {
        /// <summary>
        /// Cpf do trabalhador que será consultado.
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Contém a data/hora de início do período que será consultado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIni {get; set; }
#else
        public DateTimeOffset DtIni { get; set; }
#endif

        [XmlElement("dtIni")]
        public string DtIniField
        {
            get => DtIni.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DtIni = DateTime.Parse(value);
#else
            set => DtIni = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Contém a data/hora de fim do período que será consultado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtFim {get; set; }
#else
        public DateTimeOffset DtFim { get; set; }
#endif
        [XmlElement("dtFim")]
        public string DtFimField
        {
            get => DtFim.ToString("yyyy-MM-ddTHH:mm:ss");
#if INTEROP
            set => DtFim = DateTime.Parse(value);
#else
            set => DtFim = DateTimeOffset.Parse(value);
#endif
        }
    }

    #endregion Consulta Evts Trabalhador

    #region Consulta Evts Empregador

    /// <summary>
    /// Contém o filtro para consulta aos eventos do empregador que não se enquadram nas categorias de eventos de tabela 
    /// ou eventos periódicos não periódicos do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsEmpregador")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsEmpregador
    {
        /// <summary>
        /// Contém o tipo do evento que será consultado
        /// </summary>
        [XmlElement("tpEvt")]
        public string TpEvt { get; set; }

        /// <summary>
        /// Contém o período de apuração que será consultado
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApur {get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif

        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }
    }

    #endregion Consulta Evts Empregador
}
