#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2205")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltCadastral/v_S_01_02_00", IsNullable = false)]
    public class ESocial2205 : XMLBase
    {
        [XmlElement("evtAltCadastral")]
        public EvtAltCadastral EvtAltCadastral { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAltCadastral")]
    [ComVisible(true)]
#endif
    public class EvtAltCadastral
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador2205 IdeTrabalhador { get; set; }

        [XmlElement("alteracao")]
        public Alteracao2205 Alteracao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2205")]
    [ComVisible(true)]
#endif
    public class IdeEvento2205
    {
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhadorESocial2205")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador2205
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AlteracaoESocial2205")]
    [ComVisible(true)]
#endif
    public class Alteracao2205
    {
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

        [XmlElement("dadosTrabalhador")]
        public DadosTrabalhador DadosTrabalhador { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosTrabalhador")]
    [ComVisible(true)]
#endif
    public class DadosTrabalhador
    {
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        [XmlElement("sexo")]
        public TipoSexo Sexo { get; set; }

        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstCiv { get; set; } = (EstadoCivil)(-1);
#else
        public EstadoCivil? EstCiv { get; set; }
#endif


        [XmlElement("grauInstr")]
        public GrauDeInstrucao GrauInstr { get; set; }

        [XmlElement("nmSoc")]
        public string NmSoc { get; set; }

        [XmlElement("paisNac")]
        public string PaisNac { get; set; }

        [XmlElement("endereco")]
        public Endereco Endereco { get; set; }

        [XmlElement("trabImig")]
        public TrabImig TrabImig { get; set; }

        [XmlElement("infoDeficiencia")]
        public InfoDeficiencia InfoDeficiencia { get; set; }

        [XmlElement("dependente")]
        public List<Dependente> Dependente { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente GetDependente(int index)
        {
            if ((Dependente?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dependente[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dependente
        /// </summary>
        public int GetDependenteCount => (Dependente != null ? Dependente.Count : 0);
#endif

        [XmlElement("contato")]
        public ContatoESocial2205 Contato { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeEstCivField() => EstCiv != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeEstCiv() => EstCiv != null;
#endif

        public bool ShouldSerializeNmSoc() => !string.IsNullOrEmpty(NmSoc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Endereco")]
    [ComVisible(true)]
#endif
    public class Endereco
    {
        [XmlElement("brasil")]
        public Brasil Brasil { get; set; }

        [XmlElement("exterior")]
        public Exterior Exterior { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Brasil")]
    [ComVisible(true)]
#endif
    public class Brasil
    {
        [XmlElement("tpLograd")]
        public string TpLograd { get; set; }

        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        [XmlElement("complemento")]
        public string Complemento { get; set; }

        [XmlElement("bairro")]
        public string Bairro { get; set; }

        [XmlElement("cep")]
        public string Cep { get; set; }

        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        [XmlElement("uf")]
        public string Uf { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeTpLograd() => !string.IsNullOrEmpty(TpLograd);

        public bool ShouldSerializeComplemento() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exterior")]
    [ComVisible(true)]
#endif
    public class Exterior
    {
        [XmlElement("paisResid")]
        public int PaisResid { get; set; }

        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        [XmlElement("complemento")]
        public string Complemento { get; set; }

        [XmlElement("bairro")]
        public string Bairro { get; set; }

        [XmlElement("nmCid")]
        public string NmCid { get; set; }

        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeComplemento() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSerializeCodPostal() => !string.IsNullOrEmpty(CodPostal);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabImig")]
    [ComVisible(true)]
#endif
    public class TrabImig
    {
        [XmlElement("tmpResid")]
#if INTEROP
        public TempoDeResidencia TmpResid { get; set; } = (TempoDeResidencia)(-1);
#else
        public TempoDeResidencia? TmpResid { get; set; }
#endif

        [XmlElement("condIng")]
        public CondicaoIngressoTrabalhador CondIng { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTmpResidField() => TmpResid != (TempoDeResidencia)(-1);
#else
        public bool ShouldSerializeTmpResid() => TmpResid != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeficiencia")]
    [ComVisible(true)]
#endif
    public class InfoDeficiencia
    {
        [XmlElement("defFisica")]
        public SimNaoLetra DefFisica { get; set; }

        [XmlElement("defVisual")]
        public SimNaoLetra DefVisual { get; set; }

        [XmlElement("defAuditiva")]
        public SimNaoLetra DefAuditiva { get; set; }

        [XmlElement("defMental")]
        public SimNaoLetra DefMental { get; set; }

        [XmlElement("defIntelectual")]
        public SimNaoLetra DefIntelectual { get; set; }

        [XmlElement("reabReadap")]
        public SimNaoLetra ReabReadap { get; set; }

        [XmlElement("infoCota")]
#if INTEROP
        public SimNaoLetra InfoCota { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? InfoCota { get; set; }
#endif

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeInfoCota() => InfoCota != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeInfoCota() => InfoCota != null;
#endif

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Dependente")]
    [ComVisible(true)]
#endif
    public class Dependente
    {
        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        [XmlElement("nmDep")]
        public string NmDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlElement("sexoDep")]
#if INTEROP
        public TipoSexo SexoDep { get; set; } = (TipoSexo)(-1);
#else
        public TipoSexo? SexoDep { get; set; }
#endif

        [XmlElement("depIRRF")]
        public SimNaoLetra DepIRRF { get; set; }

        [XmlElement("depSF")]
        public SimNaoLetra DepSF { get; set; }

        [XmlElement("incTrab")]
        public SimNaoLetra IncTrab { get; set; }

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpDep() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDep() => TpDep != null;
#endif

        public bool ShouldSerializeCpfDep() => !string.IsNullOrEmpty(CpfDep);

#if INTEROP
        public bool ShouldSerializeSexoDep() => SexoDep != (TipoSexo)(-1);
#else
        public bool ShouldSerializeSexoDep() => SexoDep != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ContatoESocial2205")]
    [ComVisible(true)]
#endif
    public class ContatoESocial2205
    {
        [XmlElement("fonePrinc")]
        public string FonePrinc { get; set; }

        [XmlElement("emailPrinc")]
        public string EmailPrinc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFonePrinc() => !string.IsNullOrEmpty(FonePrinc);

        public bool ShouldSerializeEmailPrinc() => !string.IsNullOrEmpty(EmailPrinc);

        #endregion
    }
}
