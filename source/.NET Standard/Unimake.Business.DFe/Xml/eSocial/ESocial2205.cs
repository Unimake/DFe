#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2205 - Alteração de Dados Cadastrais do Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2205")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtAltCadastral/v_S_01_02_00", IsNullable = false)]
    public class ESocial2205 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Alteração de Dados Cadastrais do Trabalhador
        /// </summary>
        [XmlElement("evtAltCadastral")]
        public EvtAltCadastral EvtAltCadastral { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Alteração de Dados Cadastrais do Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtAltCadastral")]
    [ComVisible(true)]
#endif
    public class EvtAltCadastral
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do 
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2205 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador2205 IdeTrabalhador { get; set; }

        /// <summary>
        /// Alteração de dados cadastrais do trabalhador
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao2205 Alteracao { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2205")]
    [ComVisible(true)]
#endif
    public class IdeEvento2205 : IdeEvento2200 { }

    /// <summary>
    /// Identificação do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador2205")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador2205
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }
    }

    /// <summary>
    /// Alteração de dados cadastrais do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao2205")]
    [ComVisible(true)]
#endif
    public class Alteracao2205
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
        /// Informações pessoais do trabalhador
        /// </summary>
        [XmlElement("dadosTrabalhador")]
        public DadosTrabalhador DadosTrabalhador { get; set; }
    }

    /// <summary>
    /// Informações pessoais do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosTrabalhador")]
    [ComVisible(true)]
#endif
    public class DadosTrabalhador
    {
        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        /// <summary>
        /// Sexo do trabalhador
        /// </summary>
        [XmlElement("sexo")]
        public TipoSexo Sexo { get; set; }

        /// <summary>
        /// Etnia e raça do trabalhador
        /// </summary>
        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        /// <summary>
        /// Estado civil do trabalhador
        /// </summary>
        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstCiv { get; set; } = (EstadoCivil)(-1);
#else
        public EstadoCivil? EstCiv { get; set; }
#endif

        /// <summary>
        /// Grau de instrução do trabalhador
        /// </summary>
        [XmlElement("grauInstr")]
        public GrauDeInstrucao GrauInstr { get; set; }

        /// <summary>
        /// Nome social para travesti ou transexual
        /// </summary>
        [XmlElement("nmSoc")]
        public string NmSoc { get; set; }

        /// <summary>
        /// Preencher com o código do país de nacionalidade do trabalhador
        /// </summary>
        [XmlElement("paisNac")]
        public string PaisNac { get; set; }

        /// <summary>
        /// Endereço do trabalhador
        /// </summary>
        [XmlElement("endereco")]
        public Endereco2205 Endereco { get; set; }

        /// <summary>
        /// Informações do trabalhador imigrante
        /// </summary>
        [XmlElement("trabImig")]
        public TrabImig2205 TrabImig { get; set; }

        /// <summary>
        /// Pessoa com deficiência
        /// </summary>
        [XmlElement("infoDeficiencia")]
        public InfoDeficiencia2205 InfoDeficiencia { get; set; }

        /// <summary>
        /// Informações dos dependentes
        /// </summary>
        [XmlElement("dependente")]
        public List<Dependente2205> Dependente { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente2205 item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente2205>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente2205 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente2205 GetDependente(int index)
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

        /// <summary>
        /// Informações de contato
        /// </summary>
        [XmlElement("contato")]
        public Contato2205 Contato { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeEstCivField() => EstCiv != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeEstCiv() => EstCiv != null;
#endif

        public bool ShouldSerializeNmSoc() => !string.IsNullOrEmpty(NmSoc);

        #endregion
    }

    /// <summary>
    /// Grupo de informações do endereço do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Endereco2205")]
    [ComVisible(true)]
#endif
    public class Endereco2205
    {
        /// <summary>
        /// Endereço no Brasil
        /// </summary>
        [XmlElement("brasil")]
        public Brasil Brasil { get; set; }

        /// <summary>
        /// Endereço no exterior
        /// </summary>
        [XmlElement("exterior")]
        public Exterior Exterior { get; set; }
    }

    /// <summary>
    /// Endereço no Brasil
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Brasil")]
    [ComVisible(true)]
#endif
    public class Brasil
    {
        /// <summary>
        /// Tipo de logradouro
        /// </summary>
        [XmlElement("tpLograd")]
        public string TpLograd { get; set; }

        /// <summary>
        /// Descrição do logradouro
        /// </summary>
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        /// <summary>
        /// Número do logradouro
        /// </summary>
        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        /// <summary>
        /// Complemento do logradouro
        /// </summary>
        [XmlElement("complemento")]
        public string Complemento { get; set; }

        /// <summary>
        /// Nome do bairro/distrito
        /// </summary>
        [XmlElement("bairro")]
        public string Bairro { get; set; }

        /// <summary>
        /// Código de Endereçamento Postal - CEP.
        /// </summary>
        [XmlElement("cep")]
        public string Cep { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme tabela do IBGE
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        /// <summary>
        /// Preencher com a sigla da Unidade da Federação - UF
        /// </summary>
        [XmlElement("uf")]
        public string Uf { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeTpLograd() => !string.IsNullOrEmpty(TpLograd);

        public bool ShouldSerializeComplemento() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        #endregion
    }

    /// <summary>
    /// Endereço no exterior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exterior")]
    [ComVisible(true)]
#endif
    public class Exterior
    {
        /// <summary>
        /// Preencher com o código do país
        /// </summary>
        [XmlElement("paisResid")]
        public string PaisResid { get; set; }

        /// <summary>
        /// Descrição do logradouro
        /// </summary>
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        /// <summary>
        /// Número do logradouro.
        /// Se não houver número a ser informado, preencher com "S/N".
        /// </summary>
        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        /// <summary>
        /// Complemento do logradouro
        /// </summary>
        [XmlElement("complemento")]
        public string Complemento { get; set; }

        /// <summary>
        /// Nome do bairro/distrito
        /// </summary>
        [XmlElement("bairro")]
        public string Bairro { get; set; }

        /// <summary>
        /// Nome da cidade
        /// </summary>
        [XmlElement("nmCid")]
        public string NmCid { get; set; }

        /// <summary>
        /// Código de Endereçamento Postal
        /// </summary>
        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeComplemento() => !string.IsNullOrEmpty(Complemento);

        public bool ShouldSerializeBairro() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSerializeCodPostal() => !string.IsNullOrEmpty(CodPostal);

        #endregion
    }

    /// <summary>
    /// Informações do trabalhador imigrante
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TrabImig2205")]
    [ComVisible(true)]
#endif
    public class TrabImig2205
    {
        /// <summary>
        /// Tempo de residência do trabalhador imigrante
        /// </summary>
        [XmlElement("tmpResid")]
#if INTEROP
        public TempoDeResidencia TmpResid { get; set; } = (TempoDeResidencia)(-1);
#else
        public TempoDeResidencia? TmpResid { get; set; }
#endif

        /// <summary>
        /// Condição de ingresso do trabalhador imigrante
        /// </summary>
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

    /// <summary>
    /// Pessoa com deficiência
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDeficiencia2205")]
    [ComVisible(true)]
#endif
    public class InfoDeficiencia2205
    {
        /// <summary>
        /// Deficiência física
        /// </summary>
        [XmlElement("defFisica")]
        public SimNaoLetra DefFisica { get; set; }

        /// <summary>
        /// Deficiência visual
        /// </summary>
        [XmlElement("defVisual")]
        public SimNaoLetra DefVisual { get; set; }

        /// <summary>
        /// Deficiência auditiva
        /// </summary>
        [XmlElement("defAuditiva")]
        public SimNaoLetra DefAuditiva { get; set; }

        /// <summary>
        /// Deficiência mental
        /// </summary>
        [XmlElement("defMental")]
        public SimNaoLetra DefMental { get; set; }

        /// <summary>
        /// Deficiência intelectual
        /// </summary>
        [XmlElement("defIntelectual")]
        public SimNaoLetra DefIntelectual { get; set; }

        /// <summary>
        /// Informar se o trabalhador é reabilitado (empregado) ou readaptado (servidor público/militar).
        /// </summary>
        [XmlElement("reabReadap")]
        public SimNaoLetra ReabReadap { get; set; }

        /// <summary>
        /// Informar se o trabalhador deve ser contabilizado no preenchimento de cota de pessoas com deficiência habilitadas ou de beneficiários reabilitados
        /// </summary>
        [XmlElement("infoCota")]
#if INTEROP
        public SimNaoLetra InfoCota { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? InfoCota { get; set; }
#endif

        /// <summary>
        /// Observação
        /// </summary>
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

    /// <summary>
    /// Informações dos dependentes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Dependente2205")]
    [ComVisible(true)]
#endif
    public class Dependente2205
    {
        /// <summary>
        /// Tipo de dependente
        /// </summary>
        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        /// <summary>
        /// Nome do dependente
        /// </summary>
        [XmlElement("nmDep")]
        public string NmDep { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento
        /// </summary>
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

        /// <summary>
        /// Número de inscrição no CPF
        /// </summary>
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Sexo do dependente
        /// </summary>
        [XmlElement("sexoDep")]
#if INTEROP
        public TipoSexo SexoDep { get; set; } = (TipoSexo)(-1);
#else
        public TipoSexo? SexoDep { get; set; }
#endif

        /// <summary>
        /// Informar se é dependente do trabalhador para fins de dedução de seu rendimento tributável pelo Imposto de Renda
        /// </summary>
        [XmlElement("depIRRF")]
        public SimNaoLetra DepIRRF { get; set; }

        /// <summary>
        /// Informar se é dependente para fins de recebimento do benefício de salário-família
        /// </summary>
        [XmlElement("depSF")]
        public SimNaoLetra DepSF { get; set; }

        /// <summary>
        /// Informar se o dependente tem incapacidade física ou mental para o trabalho
        /// </summary>
        [XmlElement("incTrab")]
#if INTEROP
        public SimNaoLetra IncTrab { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IncTrab { get; set; }
#endif

        /// <summary>
        /// Informar a descrição da dependência
        /// </summary>
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

#if INTEROP
        public bool ShouldSerializeIncTrab() => IncTrab != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIncTrab() => IncTrab != null;
#endif

        public bool ShoulSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }

    /// <summary>
    /// Informações de contato
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Contato2205")]
    [ComVisible(true)]
#endif
    public class Contato2205
    {
        /// <summary>
        /// Número de telefone do trabalhador, com DDD
        /// </summary>
        [XmlElement("fonePrinc")]
        public string FonePrinc { get; set; }

        /// <summary>
        /// Endereço eletrônico
        /// </summary>
        [XmlElement("emailPrinc")]
        public string EmailPrinc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFonePrinc() => FonePrinc.HasOnlyNumbers() && FonePrinc.Length >= 8;

        public bool ShouldSerializeEmailPrinc() => !string.IsNullOrEmpty(EmailPrinc) &&
                                                         EmailPrinc.Contains("@") &&
                                                         EmailPrinc.Contains(".") &&
                                                        !EmailPrinc.StartsWith("@") &&
                                                        !EmailPrinc.EndsWith("@") &&
                                                        !EmailPrinc.StartsWith(".") &&
                                                        !EmailPrinc.EndsWith(".");

        #endregion ShouldSerialize
    }
}
