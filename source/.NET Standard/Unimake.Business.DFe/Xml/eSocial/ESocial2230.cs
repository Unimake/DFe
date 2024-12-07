#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2230 - Afastamento Temporário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2230")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAfastTemp/v_S_01_02_00", IsNullable = false)]
    public class ESocial2230 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Afastamento Temporário
        /// </summary>
        [XmlElement("evtAfastTemp")]
        public EvtAfastTemp EvtAfastTemp { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Afastamento Temporário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAfastTemp")]
    [ComVisible(true)]
#endif
    public class EvtAfastTemp
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2230 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações de identificação do trabalhador e do vínculo
        /// </summary>
        [XmlElement("ideVinculo")]
        public IdeVinculo2230 IdeVinculo { get; set; }

        /// <summary>
        /// Informações do afastamento temporário
        /// </summary>
        [XmlElement("infoAfastamento")]
        public InfoAfastamento InfoAfastamento { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2230")]
    [ComVisible(true)]
#endif
    public class IdeEvento2230 : IdeEvento2205 { }

    /// <summary>
    /// Informações de identificação do trabalhador e do vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeVinculo2230")]
    [ComVisible(true)]
#endif
    public class IdeVinculo2230
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público, 
        /// a matrícula constante no Sistema de Administração de Recursos Humanos do órgão
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
#if INTEROP
        public CodCateg CodCateg { get; set; } = (CodCateg)(-1);
#else
        public CodCateg? CodCateg { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMatricula() => !string.IsNullOrEmpty(Matricula);

#if INTEROP
        public bool ShouldSerializeCodCateg() => CodCateg != (CodCateg)(-1);
#else
        public bool ShouldSerializeCodCateg() => CodCateg != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do afastamento temporário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAfastamento")]
    [ComVisible(true)]
#endif
    public class InfoAfastamento
    {
        /// <summary>
        /// Informações de início do afastamento
        /// </summary>
        [XmlElement("iniAfastamento")]
        public IniAfastamento IniAfastamento { get; set; }

        /// <summary>
        /// Informações de retificação do afastamento temporário.
        /// Preenchimento obrigatório caso codMotAfast seja retificado de[01] para[03] ou de[03] para[01]
        /// </summary>
        [XmlElement("infoRetif")]
        public InfoRetif InfoRetif { get; set; }

        /// <summary>
        /// Informação do término do afastamento
        /// </summary>
        [XmlElement("fimAfastamento")]
        public FimAfastamento FimAfastamento { get; set; }
    }

    /// <summary>
    /// Informações de início do afastamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IniAfastamento")]
    [ComVisible(true)]
#endif
    public class IniAfastamento
    {
        /// <summary>
        /// Data de início do afastamento
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniAfast { get; set; }
#else
        public DateTimeOffset DtIniAfast { get; set; }
#endif

        [XmlElement("dtIniAfast")]
        public string DtIniAfastField
        {
            get => DtIniAfast.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniAfast = DateTime.Parse(value);
#else
            set => DtIniAfast = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o código do motivo de afastamento temporário
        /// </summary>
        [XmlElement("codMotAfast")]
        public string CodMotAfast { get; set; }

        /// <summary>
        /// Informar se o afastamento decorre da mesma doença que gerou o 
        /// afastamento anterior (codMotAfast = [01, 03]), dentro de 60 dias.
        /// </summary>
        [XmlElement("infoMesmoMtv")]
#if INTEROP
        public SimNaoLetra InfoMesmoMtv { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? InfoMesmoMtv { get; set; }
#endif

        /// <summary>
        /// Tipo de acidente de trânsito
        /// </summary>
        [XmlElement("tpAcidTransito")]
#if INTEROP
        public TipoAcidenteTransito TpAcidTransito { get; set; } = (TipoAcidenteTransito)(-1);
#else
        public TipoAcidenteTransito? TpAcidTransito { get; set; }
#endif

        /// <summary>
        /// Detalhar as informações sobre o afastamento do trabalhador, de maneira a explicitar os motivos do mesmo
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Informações referentes ao período aquisitivo de férias
        /// </summary>
        [XmlElement("perAquis")]
        public PerAquis PerAquis { get; set; }

        /// <summary>
        /// Informações complementares - Cessão/Requisição de trabalhador
        /// </summary>
        [XmlElement("infoCessao")]
        public InfoCessao InfoCessao { get; set; }

        /// <summary>
        /// Informações complementares - Afastamento para exercício de mandato sindical
        /// </summary>
        [XmlElement("infoMandSind")]
        public InfoMandSind InfoMandSind { get; set; }

        /// <summary>
        /// Informações complementares - Afastamento para exercício de mandato eletivo
        /// </summary>
        [XmlElement("infoMandElet")]
        public InfoMandElet InfoMandElet { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoMesmoMtv() => InfoMesmoMtv != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeInfoMesmoMtv() => InfoMesmoMtv != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpAcidTransito() => TpAcidTransito != (TipoAcidenteTransito)(-1);
#else
        public bool ShouldSerializeTpAcidTransito() => TpAcidTransito != null;
#endif

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

    /// <summary>
    /// Informações referentes ao período aquisitivo de férias
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PerAquis")]
    [ComVisible(true)]
#endif
    public class PerAquis
    {
        /// <summary>
        /// Data de início do período aquisitivo de férias
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtInicio { get; set; }
#else
        public DateTimeOffset DtInicio { get; set; }
#endif

        [XmlElement("dtInicio")]
        public string DtIniAfastField
        {
            get => DtInicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtInicio = DateTime.Parse(value);
#else
            set => DtInicio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de término do período aquisitivo de férias
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtFim { get; set; }
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

        #region ShouldSerialize

        public bool ShouldSerializeDtFimField() => DtFim > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares - Cessão/Requisição de trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCessao")]
    [ComVisible(true)]
#endif
    public class InfoCessao
    {
        /// <summary>
        /// Preencher com o CNPJ do órgão/entidade para o qual o trabalhador foi cedido/requisitado
        /// </summary>
        [XmlElement("cnpjCess")]
        public string CnpjCess { get; set; }

        /// <summary>
        /// Ônus da cessão/requisição
        /// </summary>
        [XmlElement("infOnus")]
        public InfOnus InfOnus { get; set; }
    }

    /// <summary>
    /// Informações complementares - Afastamento para exercício de mandato sindical
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandSind")]
    [ComVisible(true)]
#endif
    public class InfoMandSind
    {
        /// <summary>
        /// CNPJ do sindicato no qual o trabalhador exercerá o mandato
        /// </summary>
        [XmlElement("cnpjSind")]
        public string CnpjSind { get; set; }

        /// <summary>
        /// Ônus da remuneração
        /// </summary>
        [XmlElement("infOnusRemun")]
        public OnusDaRemuneracao InfOnusRemun { get; set; }
    }

    /// <summary>
    /// Informações complementares - Afastamento para exercício de mandato eletivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandElet")]
    [ComVisible(true)]
#endif
    public class InfoMandElet
    {
        /// <summary>
        /// CNPJ do órgão no qual o trabalhador exercerá o mandato eletivo
        /// </summary>
        [XmlElement("cnpjMandElet")]
        public string CnpjMandElet { get; set; }

        /// <summary>
        /// Indicar se o servidor optou pela remuneração do cargo efetivo
        /// </summary>
        [XmlElement("indRemunCargo")]
#if INTEROP
        public SimNaoLetra IndRemunCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRemunCargo { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de retificação do afastamento temporário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRetif")]
    [ComVisible(true)]
#endif
    public class InfoRetif
    {
        /// <summary>
        /// Origem da retificação
        /// </summary>
        [XmlElement("origRetif")]
        public OrigemDaRetificacao OrigRetif { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProc")]
        public string TpProc { get; set; }

        /// <summary>
        /// Informar o número do processo administrativo/judicial ou do benefício de acordo com o tipo informado em tpProc
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeTpProc() => !string.IsNullOrEmpty(TpProc);
       
        public bool ShouldSerializeNrProc() => !string.IsNullOrEmpty(NrProc);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informação do término do afastamento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.FimAfastamento")]
    [ComVisible(true)]
#endif
    public class FimAfastamento
    {
        /// <summary>
        /// Preencher com a data do término do afastamento do trabalhador.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTermAfast { get; set; }
#else
        public DateTimeOffset DtTermAfast { get; set; }
#endif

        [XmlElement("dtTermAfast")]
        public string DtTermAfastField
        {
            get => DtTermAfast.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTermAfast = DateTime.Parse(value);
#else
            set => DtTermAfast = DateTimeOffset.Parse(value);
#endif
        }
    }

}
