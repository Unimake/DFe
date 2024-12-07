#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2306 - Trabalhador Sem Vínculo de Emprego/Estatutário - Alteração Contratual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2306")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVAltContr/v_S_01_02_00", IsNullable = false)]
    public class ESocial2306 : XMLBaseESocial
    {
        /// <summary>
        /// Evento TSVE - Alteração Contratual
        /// </summary>
        [XmlElement("evtTSVAltContr")]
        public EvtTSVAltContr EvtTSVAltContr { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento TSVE - Alteração Contratual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTSVAltContr")]
    [ComVisible(true)]
#endif
    public class EvtTSVAltContr
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
        public IdeEvento2306 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do TSVE
        /// </summary>
        [XmlElement("ideTrabSemVinculo")]
        public IdeTrabSemVinculo2306 IdeTrabSemVinculo { get; set; }

        /// <summary>
        /// TSVE - Alteração Contratual
        /// </summary>
        [XmlElement("infoTSVAlteracao")]
        public InfoTSVAlteracao InfoTSVAlteracao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2306")]
    [ComVisible(true)]
#endif
    public class IdeEvento2306 : IdeEvento2205 { }

    /// <summary>
    /// Identificação do TSVE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabSemVinculo2306")]
    [ComVisible(true)]
#endif
    public class IdeTrabSemVinculo2306
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa
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
    /// TSVE - Alteração Contratual
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTSVAlteracao")]
    [ComVisible(true)]
#endif
    public class InfoTSVAlteracao
    {
        /// <summary>
        /// Preencher com a data da alteração das informações
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAlteracao { get; set; }
#else
        public DateTimeOffset DtAlteracao { get; set; }
#endif

        [XmlElement("dtAlteracao")]
        public string DtAlteracaoField
        {
            get => DtAlteracao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtAlteracao = DateTime.Parse(value);
#else
            set => DtAlteracao = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Natureza da atividade
        /// </summary>
        [XmlElement("natAtividade")]
#if INTEROP
        public NatAtividade NatAtividade { get; set; } = (NatAtividade)(-1);
#else
        public NatAtividade? NatAtividade { get; set; }
#endif

        /// <summary>
        /// Grupo onde são fornecidas informações complementares, preenchidas conforme a categoria do TSVE
        /// </summary>
        [XmlElement("infoComplementares")]
        public InfoComplementares InfoComplementares { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeNatAtividade() => NatAtividade != (NatAtividade)(-1);
#else
        public bool ShouldSerializeNatAtividade() => NatAtividade != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo onde são fornecidas informações complementares, preenchidas conforme a categoria do TSVE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplementares")]
    [ComVisible(true)]
#endif
    public class InfoComplementares
    {
        /// <summary>
        /// Grupo que apresenta o cargo e/ou função ocupada pelo TSVE
        /// </summary>
        [XmlElement("cargoFuncao")]
        public CargoFuncao2306 CargoFuncao { get; set; }

        /// <summary>
        /// Informações da remuneração e periodicidade de pagamento
        /// </summary>
        [XmlElement("remuneracao")]
        public Remuneracao2306 Remuneracao { get; set; }

        /// <summary>
        /// Informações relativas ao dirigente sindical
        /// </summary>
        [XmlElement("infoDirigenteSindical")]
        public InfoDirigenteSindical2306 InfoDirigenteSindical { get; set; }

        /// <summary>
        /// Informações relativas ao trabalhador cedido/em exercício em outro órgão, preenchidas exclusivamente pelo cessionário/órgão de destino
        /// </summary>
        [XmlElement("infoTrabCedido")]
        public InfoTrabCedido2306 InfoTrabCedido { get; set; }

        /// <summary>
        /// Informações relativas a servidor público exercente de mandato eletivo
        /// </summary>
        [XmlElement("infoMandElet")]
        public InfoMandElet2306 InfoMandElet { get; set; }

        /// <summary>
        /// Informações relativas ao estagiário ou ao beneficiário do Programa Nacional de Prestação de Serviço Civil Voluntário
        /// </summary>
        [XmlElement("infoEstagiario")]
        public InfoEstagiario2306 InfoEstagiario { get; set; }

        /// <summary>
        /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador exercerá suas atividades
        /// </summary>
        [XmlElement("localTrabGeral")]
        public LocalTrabGeral2306 LocalTrabGeral { get; set; }
    }

    /// <summary>
    /// Grupo que apresenta o cargo e/ou função ocupada pelo TSVE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CargoFuncao2306")]
    [ComVisible(true)]
#endif
    public class CargoFuncao2306 : CargoFuncao2300 { }

    /// <summary>
    /// Informações da remuneração e periodicidade de pagamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Remuneracao2306")]
    [ComVisible(true)]
#endif
    public class Remuneracao2306 : Remuneracao2206 { }

    /// <summary>
    /// Informações relativas ao dirigente sindical
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDirigenteSindical2306")]
    [ComVisible(true)]
#endif
    public class InfoDirigenteSindical2306
    {
        /// <summary>
        /// Tipo de regime previdenciário
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }
    }

    /// <summary>
    /// Informações relativas ao trabalhador cedido/em exercício em outro órgão, preenchidas exclusivamente pelo cessionário/órgão de destino
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTrabCedido2306")]
    [ComVisible(true)]
#endif
    public class InfoTrabCedido2306
    {
        /// <summary>
        /// Tipo de regime previdenciário (ou Sistema de Proteção Social dos Militares das Forças Armadas)
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }
    }

    /// <summary>
    /// Informações relativas a servidor público exercente de mandato eletivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMandElet2306")]
    [ComVisible(true)]
#endif
    public class InfoMandElet2306
    {
        /// <summary>
        /// Indicar se o servidor optou pela remuneração do cargo efetivo
        /// </summary>
        [XmlElement("indRemunCargo")]
#if INTEROP
        public SimNaoLetra IndRemunCargo { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRemunCargo { get; set; }
#endif

        /// <summary>
        /// Tipo de regime previdenciário
        /// </summary>
        [XmlElement("tpRegPrev")]
        public TpRegPrev TpRegPrev { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRemunCargo() => IndRemunCargo != null;
#endif

        #endregion
    }

    /// <summary>
    /// Informações relativas ao estagiário ou ao beneficiário do Programa Nacional de Prestação de Serviço Civil Voluntário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstagiario2306")]
    [ComVisible(true)]
#endif
    public class InfoEstagiario2306 : InfoEstagiario2300 { }

    /// <summary>
    /// Estabelecimento (CNPJ, CNO, CAEPF) onde o trabalhador exercerá suas atividades. 
    /// Caso o trabalhador exerça suas atividades em instalações de terceiros, este campo deve ser preenchido com o estabelecimento 
    /// do próprio declarante ao qual o trabalhador esteja vinculado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.LocalTrabGeral2306")]
    [ComVisible(true)]
#endif
    public class LocalTrabGeral2306 : LocalTrabGeral2206 { }
}
