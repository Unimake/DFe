using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif


namespace Unimake.Business.DFe.Xml.eSocial
{
    internal class eSocial
    {



#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial1000")]
    [ComVisible(true)]
#endif

        [Serializable()]
        [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_02_00", IsNullable = false)]
        public class ESocial1000 : XMLBase
        {
            [XmlElement("evtInfoEmpregador")]
            public evtInfoEmpregador evtInfoEmpregador { get; set; }

            //[XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
            //public Signature Signature { get; set; }
        }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.evtInfoEmpregador")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class evtInfoEmpregador
        {
            [XmlAttribute(AttributeName = "id", DataType = "token")]
            public string ID { get; set; }

            [XmlElement("ideEvento")]
            public IdeEvento IdeEvento { get; set; }

            [XmlElement("ideEmpregador")]
            public ideEmpregador ideEmpregador { get; set; }

            [XmlElement("infoEmpregador")]
            public infoEmpregador infoEmpregador { get; set; }
        }

        #region IdeEvento
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.IdeEvento")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class IdeEvento
        {
            [XmlElement("tpAmb")]
            public TipoAmbiente TpAmb { get; set; }

            [XmlElement("procEmi")]
            public ProcessoEmissaoReinf ProcEmi { get; set; }

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
        [Serializable()]
        public class ideEmpregador
        {
            [XmlElement("tpInsc")]
            public TiposInscricao TpInsc { get; set; }

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
        [Serializable()]
        public class infoEmpregador
        {
            [XmlElement("inclusao")]
            public Inclusao Inclusao { get; set; }
        }

        #endregion


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class Inclusao
        {
            [XmlElement("idePeriodo")]
            public IdePeriodo IdePeriodo { get; set; }

            [XmlElement("infoCadastro")]
            public InfoCadastro InfoCadastro { get; set; }
        }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao.IdePeriodo")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class IdePeriodo
        {
            [XmlElement("iniValid")]
            public string IniValid { get; set; }

            [XmlElement("fimValid")]
            public string FimValid { get; set; }

            #region ShouldSerialize

            public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

            #endregion
        }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.Inclusao.InfoCadastro")]
    [ComVisible(true)]
#endif

        [Serializable()]
        public class InfoCadastro
        {
            [XmlElement("classTrib")]
            public ClassificacaoTributaria ClassTrib { get; set; }

            [XmlElement("indCoop")]
            public indCoop IndCoop { get; set; }

            [XmlElement("indConstr")]
            public indConstr IndicativoConstr { get; set; }

            [XmlElement("indDesFolha")]
            public indDesFolha IndicativoDesFolha { get; set; }

            [XmlElement("indOptRegEletron")]
            public indOptRegEletron IndicativoOptRegimeEletronico { get; set; }

            [XmlElement("dadosIsencao")]
            public DadosIsencao dadosIsencao { get; set; }

            [XmlElement("infoOrgInternacional")]
            public InfoOrgInternacional infoOrgInternacional { get; set; }
        }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.InfoCadastro.DadosIsencao")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class DadosIsencao
        {
            [XmlElement("ideMinLei")]
            public string ideMinLei { get; set; }

            [XmlElement("nrCertif")]
            public string nrCertif { get; set; }

            [XmlElement("dtEmisCertif")]
            public DateTime dtEmisCertif { get; set; }

            [XmlElement("dtVencCertif")]
            public DateTime dtVencCertif { get; set; }

            [XmlElement("nrProtRenov")]
            public string nrProtRenov { get; set; }

            [XmlElement("dtProtRenov")]
            public DateTime dtProtRenov { get; set; }

            [XmlElement("dtDou")]
            public DateTime dtDou { get; set; }

            [XmlElement("pagDou")]
            public string pagDou { get; set; }
        }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.eSocial.InfoCadastro.InfoOrgInternacional")]
    [ComVisible(true)]
#endif
        [Serializable()]
        public class InfoOrgInternacional
        {
            [XmlElement("indAcordoIsenMulta")]
            public IndAcordoIsenMulta indAcordoIsenMulta { get; set; }
        }
    }
}
