using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using System.Collections.Generic;
#if INTEROP
using System.Runtime.InteropServices;
#endif


namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial1000")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// Evento 1000 do ESocial
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_02_00", IsNullable = false)]
    public class ESocial1000 : XMLBase
    {
        /// <summary>
        /// Evento informações do empregador. eSocial - 1000
        /// </summary>
        [XmlElement("evtInfoEmpregador")]
        public evtInfoEmpregador evtInfoEmpregador { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.evtInfoEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Tag root
    /// </summary>
    [Serializable()]
    public class evtInfoEmpregador
    {
        /// <summary>
        /// ID do xml
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// IdeEvento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        /// <summary>
        /// ideEmpregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public ideEmpregador ideEmpregador { get; set; }

        /// <summary>
        /// infoEmpregador
        /// </summary>
        [XmlElement("infoEmpregador")]
        public infoEmpregador infoEmpregador { get; set; }
    }

    #region IdeEvento
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.IdeEvento")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// IdeEvento
    /// </summary>
    [Serializable()]
    public class IdeEvento
    {
        /// <summary>
        /// TipoAmbiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// ProcessoEmissaoReinf
        /// </summary>
        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        /// <summary>
        /// VerProc
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

    #endregion

    #region ideEmpregador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.ideEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// 
    /// </summary>
    [Serializable()]
    public class ideEmpregador
    {
        /// <summary>
        /// TiposInscricao
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// NrInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

    #endregion

    #region infoEmpregador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.infoEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// infoEmpregador
    /// </summary>
    [Serializable()]
    public class infoEmpregador
    {
        /// <summary>
        /// Inclusao
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao Inclusao { get; set; }
    }

    #endregion


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Inclusao
    /// </summary>
    [Serializable()]
    public class Inclusao
    {
        /// <summary>
        /// IdePeriodo
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }

        /// <summary>
        /// InfoCadastro
        /// </summary>
        [XmlElement("infoCadastro")]
        public InfoCadastro InfoCadastro { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao.IdePeriodo")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// IdePeriodo
    /// </summary>
    [Serializable()]
    public class IdePeriodo
    {
        /// <summary>
        /// IniValid
        /// </summary>
        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        /// <summary>
        /// FimValid
        /// </summary>
        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// ShouldSerializeFimValid
        /// </summary>
        /// <returns></returns>
        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao.InfoCadastro")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// InfoCadastro
    /// </summary>
    [Serializable()]
    public class InfoCadastro
    {
        /// <summary>
        /// ClassTrib
        /// </summary>
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        /// <summary>
        /// IndCoop
        /// </summary>
        [XmlElement("indCoop")]
        public IndCoop IndCoop { get; set; }

        /// <summary>
        /// IndicativoConstr
        /// </summary>
        [XmlElement("indConstr")]
        public IndConstr IndConstr { get; set; }

        /// <summary>
        /// IndicativoDesFolha
        /// </summary>
        [XmlElement("indDesFolha")]
        public IndDesFolha IndDesFolha { get; set; }

        /// <summary>
        /// IndicativoOptRegimeEletronico
        /// </summary>
        [XmlElement("indOptRegEletron")]
        public IndOptRegEletron IndOptRegimeEletronico { get; set; }

        /// <summary>
        /// dadosIsencao
        /// </summary>
        [XmlElement("dadosIsencao")]
        public DadosIsencao DadosIsencao { get; set; }

        /// <summary>
        /// infoOrgInternacional
        /// </summary>
        [XmlElement("infoOrgInternacional")]
        public InfoOrgInternacional InfoOrgInternacional { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.InfoCadastro.DadosIsencao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// DadosIsencao
    /// </summary>
    [Serializable()]
    public class DadosIsencao
    {
        /// <summary>
        /// ideMinLei
        /// </summary>
        [XmlElement("ideMinLei")]
        public string IdeMinLei { get; set; }

        /// <summary>
        /// nrCertif
        /// </summary>
        [XmlElement("nrCertif")]
        public string NrCertif { get; set; }

#if INTEROP
        public DateTime DtEmisCertif { get; set; }
#else
        /// <summary>
        /// dtEmisCertif
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtEmisCertif { get; set; }
#endif
        /// <summary>
        /// dtEmisCertif
        /// </summary>
        [XmlElement("dtEmisCertif")]
        public string DtEmisCertifField
        {
            get => DtEmisCertif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEmisCertif = DateTime.Parse(value);
#else
            set => DtEmisCertif = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        public DateTime DtVencCertif { get; set; }
#else
        /// <summary>
        /// DtVencCertif
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtVencCertif { get; set; }
#endif

        /// <summary>
        /// DtVencCertif
        /// </summary>
        [XmlElement("dtVencCertif")]
        public string DtVencCertifField
        {
            get => DtVencCertif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtVencCertif = DateTime.Parse(value);'
#else
            set => DtVencCertif = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// nrProtRenov
        /// </summary>
        [XmlElement("nrProtRenov")]
        public string NrProtRenov { get; set; }

#if INTEROP
        public DateTime DtProtRenov { get; set; }
#else
        /// <summary>
        /// dtProtRenov
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtProtRenov { get; set; }
#endif

        /// <summary>
        /// dtProtRenov
        /// </summary>
        [XmlElement("dtProtRenov")]
        public string DtProtRenovField
        {
            get => DtProtRenov.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtProtRenov = DateTime.Parse(value);
#else
            set => DtProtRenov = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        public DateTime DtDou { get; set; }
#else
        /// <summary>
        /// dtDou
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtDou {  get; set; }
#endif

        /// <summary>
        /// dtDou
        /// </summary>
        [XmlElement("dtDou")]
        public string DtDouField
        {
            get => DtDou.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDou = DateTime.Parse(value);
#else
            set => DtDou = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// pagDou
        /// </summary>
        [XmlElement("pagDou")]
        public string PagDou { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.InfoCadastro.InfoOrgInternacional")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// InfoOrgInternacional
    /// </summary>
    [Serializable()]
    public class InfoOrgInternacional
    {
        /// <summary>
        /// indAcordoIsenMulta
        /// </summary>
        [XmlElement("indAcordoIsenMulta")]
        public IndAcordoIsenMulta indAcordoIsenMulta { get; set; }
    }
}
