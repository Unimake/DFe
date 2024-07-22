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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabelaESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/empregador/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsEmpregadorESocial : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consulta Empregador

    #region Consultar Tabela
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabelaESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/tabela/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsTabelaESocial : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consultar Tabela

    #region Consultar Trabalhador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabelaESocial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/trabalhador/v1_0_0", IsNullable = false)]
    public class ConsultarEvtsTrabalhadorESocial : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.1.0";

        [XmlElement("consultaIdentificadoresEvts")]
        public ConsultaIdentificadoresEvts ConsultaIdentificadoresEvts { get; set; }
    }
    #endregion Consulta Trabalhador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaIdentificadoresEvts")]
    [ComVisible(true)]
#endif
    public class ConsultaIdentificadoresEvts
    {
        [XmlElement("ideEmpregador")]
        public IdeEmpregador Empregador { get; set; }

        [XmlElement("consultaEvtsTabela")]
        public ConsultaEvtsTabela ConsultaEvtsTabela { get; set; }

        [XmlElement("consultaEvtsTrabalhador")]
        public ConsultaEvtsTrabalhador ConsultaEvtsTrabalhador { get; set; }

        [XmlElement("consultaEvtsEmpregador")]
        public ConsultaEvtsEmpregador ConsultaEvtsEmpregador { get; set; }
    }

    #region  Consulta Evts Tabela
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabela")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsTabela
    {
        [XmlElement("tpEvt")]
        public string TpEvt { get; set; }

        [XmlElement("chEvt")]
        public string ChEvt { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIni {get; set; }
#else
        public DateTimeOffset DtIni { get; set; }
#endif
        [XmlElement("dtIni")]
        public string DtIniField
        {
            get => DtIni.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIni = DateTime.Parse(value);
#else
            set => DtIni = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtFim {get; set; }
#else
        public DateTimeOffset DtFim { get; set; }
#endif
        [XmlElement("dtFim")]
        public string DtFimField
        {
            get => DtFim.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFim = DateTime.Parse(value);
#else
            set => DtFim = DateTimeOffset.Parse(value);
#endif
        }
    }
    #endregion Consulta Evts Tabela

    #region Consulta Evts Trabalhador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTrabalhador")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsTrabalhador
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIni {get; set; }
#else
        public DateTimeOffset DtIni { get; set; }
#endif
        [XmlElement("dtIni")]
        public string DtIniField
        {
            get => DtIni.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIni = DateTime.Parse(value);
#else
            set => DtIni = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtFim {get; set; }
#else
        public DateTimeOffset DtFim { get; set; }
#endif
        [XmlElement("dtFim")]
        public string DtFimField
        {
            get => DtFim.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFim = DateTime.Parse(value);
#else
            set => DtFim = DateTimeOffset.Parse(value);
#endif
        }

    }
    #endregion

    #region Consulta Evts Empregador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsEmpregador")]
    [ComVisible(true)]
#endif
    public class ConsultaEvtsEmpregador
    {
        [XmlElement("tpEvt")]
        public string TpEvt { get; set; }

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
