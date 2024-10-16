#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2405 - Cadastro de Beneficiário - Entes Públicos - Alteração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2405")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCdBenefAlt/v_S_01_02_00", IsNullable = false)]
    public class ESocial2405 : XMLBase
    {
        /// <summary>
        /// Evento Cadastro de Beneficiário - Entes Públicos - Alteração
        /// </summary>
        [XmlElement("evtCdBenefAlt")]
        public EvtCdBenefAlt EvtCdBenefAlt { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Cadastro de Beneficiário - Entes Públicos - Alteração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCdBenefAlt")]
    [ComVisible(true)]
#endif
    public class EvtCdBenefAlt
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2405 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário.
        /// </summary>
        [XmlElement("ideBenef")]
        public IdeBenef2405 IdeBenef { get; set; }

        /// <summary>
        /// Alteração de dados do beneficiário.
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao2405 Alteracao { get; set; }
    }

    #region IdeEvento2405

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2405")]
    [ComVisible(true)]
#endif
    public class IdeEvento2405
    {
        /// <summary>
        /// Indicativo de retificação
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Número Recibo de Entrega da(s) transmissão(ões) anterior(es) para as quais se está incluindo informações complementares.
        /// Preencher com o número do recibo de cada um dos eventos S-1200 que se deseja complementar as informações.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; } // Valores válidos: 1-Produção, 2-Produção restrita

        /// <summary>
        /// Processo de emissão do evento.
        /// </summary>
        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi { get; set; } // Valores válidos: 1-Aplicativo do empregador, 3-Aplicativo governamental - Web Geral, 4-Aplicativo governamental - Simplificado Pessoa Jurídica

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize
        /// <summary>
        /// Verifica se a tag 'nrRecibo' deve ser serializada
        /// </summary>
        public bool ShouldSerializeNrRecibo() => IndRetif == IndicativoRetificacao.ArquivoRetificacao;
        #endregion ShouldSerialize
    }
    #endregion IdeEvento

    #region IdeBenef2405

    /// <summary>
    /// Identificação do beneficiário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBenef2405")]
    [ComVisible(true)]
#endif
    public class IdeBenef2405
    {
        /// <summary>
        /// Informar o CPF do beneficiário.
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }
    }
    #endregion IdeBenef

    /// <summary>
    /// Alteração de dados do beneficiário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao2405")]
    [ComVisible(true)]
#endif
    public class Alteracao2405
    {
        /// <summary>
        /// Preencher com a data de alteração.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtAlteracao { get; set; }
#else
        public DateTimeOffset DtAlteracao { get; set; }
#endif

        /// <summary>
        /// Preencher com a data de alteração.
        /// </summary>
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
        /// Dados do beneficiário.
        /// </summary>
        [XmlElement("dadosBenef")]
        public DadosBenef DadosBenef { get; set; }
    }

    /// <summary>
    /// Dados do beneficiário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2405.DadosBenef")]
    [ComVisible(true)]
#endif
    public class DadosBenef
    {
        /// <summary>
        /// Informar o nome do beneficiário.
        /// </summary>
        [XmlElement("nmBenefic")]
        public string NmBenefic { get; set; }

        /// <summary>
        /// Sexo do beneficiário.
        /// </summary>
        [XmlElement("sexo")]
        public TipoSexo Sexo { get; set; }

        /// <summary>
        /// Etnia e raça do beneficiário, conforme sua
        /// autoclassificação(art. 39, § 8º, da Lei 12.288/2010).
        /// </summary>
        [XmlElement("racaCor")]
        public RacaCor RacaCor { get; set; }

        /// <summary>
        /// Estado civil do beneficiário.
        /// </summary>
        [XmlElement("estCiv")]
#if INTEROP
        public EstadoCivil EstadoCivil { get; set; } = (EstadoCivil)(-1);
#else 
        public EstadoCivil? EstadoCivil { get; set; }
#endif
        /// <summary>
        /// Informar se o beneficiário é pessoa com doença
        /// incapacitante que isenta da contribuição previdenciária,
        /// total ou parcialmente, reconhecida administrativa ou
        /// judicialmente, na forma da lei.
        /// </summary>
        [XmlElement("incFisMen")]
        public SimNaoLetra IncFisMen { get; set; }

        /// <summary>
        ///  Grupo de informações do endereço do beneficiário
        /// </summary>
        [XmlElement("endereco")]
        public Endereco Endereco { get; set; }

        /// <summary>
        /// Informações dos dependentes.
        /// </summary>
        [XmlElement("dependente")]
        public List<Dependente2405> Dependente { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDependente(Dependente2405 item)
        {
            if (Dependente == null)
            {
                Dependente = new List<Dependente2405>();
            }

            Dependente.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Dependente (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dependente</returns>
        public Dependente2405 GetDependente(int index)
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
        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeEstadoCivilField() => EstadoCivil != (EstadoCivil)(-1);
#else
        public bool ShouldSerializeEstadoCivil() => EstadoCivil != null;
#endif
        #endregion ShouldSerialize
    }

    public class Dependente2405
    {
        /// <summary>
        ///  Tipo de dependente.
        /// Validação: Preenchimento obrigatório se depIRRF = [S].
        /// Deve ser um código válido e existente na Tabela 07.
        /// </summary>
        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif
        /// <summary>
        ///  Nome do dependente.
        /// </summary>
        [XmlElement("nmDep")]
        public string NmDep { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif
        /// <summary>
        /// Preencher com a data de nascimento.
        /// </summary>
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
        /// Número de inscrição no CPF.
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
        /// Informar se é dependente do beneficiário para fins de
        /// dedução de seu rendimento tributável pelo Imposto de Renda.
        /// </summary>
        [XmlElement("depIRRF")]
        public SimNaoLetra DepIRRF { get; set; }

        /// <summary>
        /// Informar se o dependente é pessoa com doença incapacitante, na forma da lei
        /// </summary>
        [XmlElement("incFisMen")]
        public SimNaoLetra IncFisMen { get; set; }

        /// <summary>
        /// Informar a descrição da dependência.
        /// Validação: Informação obrigatória e exclusiva se tpDep = [99].
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
        public bool ShouldSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }

}