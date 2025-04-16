#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1010 - Tabela de Rubricas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabRubrica/v_S_01_03_00", IsNullable = false)]
    public class ESocial1010 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Tabela de Rubricas
        /// </summary>
        [XmlElement("evtTabRubrica")]
        public EvtTabRubrica EvtTabRubrica { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Tabela de Rubricas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabRubrica")]
    [ComVisible(true)]
#endif
    public class EvtTabRubrica
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
        public IdeEvento1010 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações da rubrica
        /// </summary>
        [XmlElement("infoRubrica")]
        public InfoRubrica InfoRubrica { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1010")]
    [ComVisible(true)]
#endif
    public class IdeEvento1010 : IdeEvento { }

    /// <summary>
    /// Informações da rubrica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRubrica")]
    [ComVisible(true)]
#endif
    public class InfoRubrica
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1010 Inclusao { get; set; }

        /// <summary>
        /// Alteração das informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1010 Alteracao { get; set; }

        /// <summary>
        /// Exclusão das informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclusao1010 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1010")]
    [ComVisible(true)]
#endif
    public class Inclusao1010
    {
        /// <summary>
        /// Identificação da rubrica e validade das informações
        /// </summary>
        [XmlElement("ideRubrica")]
        public IdeRubrica IdeRubrica { get; set; }

        /// <summary>
        /// Detalhamento das informações da rubrica
        /// </summary>
        [XmlElement("dadosRubrica")]
        public DadosRubrica DadosRubrica { get; set; }
    }

    /// <summary>
    /// Identificação da rubrica e período de validade das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeRubrica")]
    [ComVisible(true)]
#endif
    public class IdeRubrica
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador que identifica a rubrica em sua folha de pagamento
        /// </summary>
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        /// <summary>
        /// Preencher com o identificador da Tabela de Rubricas no âmbito do empregador
        /// </summary>
        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
#else
        public DateTimeOffset IniValid { get; set; }
#endif

        [XmlElement("iniValid")]
        public string IniValidField
        {
            get => IniValid.ToString("yyyy-MM");
#if INTEROP
            set => IniValid = DateTime.Parse(value);
#else
            set => IniValid = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
#else
        public DateTimeOffset FimValid { get; set; }
#endif

        [XmlElement("fimValid")]
        public string FimValidField
        {
            get => FimValid.ToString("yyyy-MM");
#if INTEROP
            set => FimValid = DateTime.Parse(value);
#else
            set => FimValid = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeFimValidField() => FimValid > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Detalhamento das informações da rubrica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosRubrica")]
    [ComVisible(true)]
#endif
    public class DadosRubrica
    {
        /// <summary>
        /// Informar a descrição (nome) da rubrica no sistema de folha de pagamento da empresa
        /// </summary>
        [XmlElement("dscRubr")]
        public string DscRubr { get; set; }

        /// <summary>
        /// Informar o código de classificação da rubrica
        /// </summary>
        [XmlElement("natRubr")]
        public string NatRubr { get; set; }

        /// <summary>
        /// Tipo de rubrica
        /// </summary>
        [XmlElement("tpRubr")]
        public TipoRubrica TpRubr { get; set; }

        /// <summary>
        /// Código de incidência tributária da rubrica para a Previdência Social.
        /// </summary>
        [XmlElement("codIncCP")]
        public CodigoIncidenciaTributaria CodIncCP { get; set; }

        /// <summary>
        /// Código de incidência tributária da rubrica para o Imposto de Renda Retido na Fonte - IRRF.
        /// </summary>
        [XmlElement("codIncIRRF")]
        public string CodIncIRRF { get; set; }

        /// <summary>
        /// Código de incidência da rubrica para o Fundo de Garantia do Tempo de Serviço - FGTS.
        /// </summary>
        [XmlElement("codIncFGTS")]
        public CodigoIncidenciaRubricaFGTS CodIncFGTS { get; set; }

        /// <summary>
        /// Código de incidência da rubrica para as contribuições do Regime Próprio de Previdência Social - RPPS ou do Sistema de Proteção Social dos Militares das Forças Armadas - SPSMFA.
        /// </summary>
        [XmlElement("codIncCPRP")]
#if INTEROP
        public CodigoIncidenciaDaRubrica CodIncCPRP { get; set; } = (CodigoIncidenciaDaRubrica)(-1);
#else
        public CodigoIncidenciaDaRubrica? CodIncCPRP { get; set; }
#endif

        /// <summary>
        /// Código de incidência da rubrica para o PIS/PASEP sobre a folha de salários a ser utilizado quando indTribFolhaPisPasep = [S] em S-1000.
        /// </summary>
        [XmlElement("codIncPisPasep")]
#if INTEROP
        public CodigoIncidenciaDaRubrica? CodIncPisPasep { get; set; } = (CodigoIncidenciaDaRubrica)(-1);
#else
        public CodigoIncidenciaDaRubrica? CodIncPisPasep { get; set; }
#endif

        /// <summary>
        /// Informar se a rubrica compõe o teto remuneratório específico (art. 37, XI, da CF/1988).
        /// Validação: Preenchimento obrigatório se a natureza jurídica do declarante for Administração Pública (grupo [1]).
        /// </summary>
        [XmlElement("tetoRemun")]
#if INTEROP
        public SimNaoLetra TetoRemun { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? TetoRemun { get; set; }
#endif

        /// <summary>
        /// Observações relacionadas à rubrica ou à sua utilização
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Identificação de processo - Incidência de Contrib. Previdenciária
        /// </summary>
        [XmlElement("ideProcessoCP")]
        public List<IdeProcessoCP> IdeProcessoCP { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoCP(IdeProcessoCP item)
        {
            if (IdeProcessoCP == null)
            {
                IdeProcessoCP = new List<IdeProcessoCP>();
            }

            IdeProcessoCP.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoCP (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoCP</returns>
        public IdeProcessoCP GetIdeProcessoCP(int index)
        {
            if ((IdeProcessoCP?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeProcessoCP[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeProcessoCP
        /// </summary>
        public int GetIdeProcessoCPCount => (IdeProcessoCP != null ? IdeProcessoCP.Count : 0);
#endif

        /// <summary>
        /// Identificação de processo - Incidência de IRRF
        /// </summary>
        [XmlElement("ideProcessoIRRF")]
        public List<IdeProcessoIRRF> IdeProcessoIRRF { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoIRRF(IdeProcessoIRRF item)
        {
            if (IdeProcessoIRRF == null)
            {
                IdeProcessoIRRF = new List<IdeProcessoIRRF>();
            }

            IdeProcessoIRRF.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoIRRF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoIRRF</returns>
        public IdeProcessoIRRF GetIdeProcessoIRRF(int index)
        {
            if ((IdeProcessoIRRF?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeProcessoIRRF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeProcessoIRRF
        /// </summary>
        public int GetIdeProcessoIRRFCount => (IdeProcessoIRRF != null ? IdeProcessoIRRF.Count : 0);
#endif

        /// <summary>
        /// Identificação de processo - Incidência de FGTS
        /// </summary>
        [XmlElement("ideProcessoFGTS")]
        public List<IdeProcessoFGTS1010> IdeProcessoFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoFGTS(IdeProcessoFGTS1010 item)
        {
            if (IdeProcessoFGTS == null)
            {
                IdeProcessoFGTS = new List<IdeProcessoFGTS1010>();
            }

            IdeProcessoFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoFGTS1010 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoFGTS</returns>
        public IdeProcessoFGTS1010 GetIdeProcessoFGTS(int index)
        {
            if ((IdeProcessoFGTS?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeProcessoFGTS[index];
        }
        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeProcessoFGTS
        /// </summary>
        public int GetIdeProcessoFGTSCount => (IdeProcessoFGTS != null ? IdeProcessoFGTS.Count : 0);

#endif

        /// <summary>
        /// Caso a empresa possua processo judicial com decisão/sentença favorável, determinando a não incidência de contribuição para o PIS/PASEP relativo à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado através de evento específico na Tabela de Processos(S-1070).
        /// </summary>
        [XmlElement("ideProcessoPisPasep")]
        public List<IdeProcessoPisPasep> IdeProcessoPisPasep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoPisPasep(IdeProcessoPisPasep item)
        {
            if (IdeProcessoPisPasep == null)
            {
                IdeProcessoPisPasep = new List<IdeProcessoPisPasep>();
            }

            IdeProcessoPisPasep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoPisPasep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoPisPasep</returns>
        public IdeProcessoPisPasep GetIdeProcessoPisPasep(int index)
        {
            if ((IdeProcessoPisPasep?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeProcessoPisPasep[index];
        }
        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeProcessoPisPasep
        /// </summary>
        public int GetIdeProcessoPisPasepCount => (IdeProcessoPisPasep != null ? IdeProcessoPisPasep.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

#if INTEROP
        public bool ShouldSerializeCodIncCPRP() => CodIncCPRP != (CodigoIncidenciaDaRubrica)(-1);
#else
        public bool ShouldSerializeCodIncCPRP() => CodIncCPRP != null;
#endif

#if INTEROP
        public bool ShouldSerializeTetoRemun() => TetoRemun != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeTetoRemun() => TetoRemun != null;
#endif

#if INTEROP
        public bool ShouldSerializeCodIncPisPasep() => CodIncPisPasep != (CodigoIncidenciaDaRubrica)(-1);
#else
        public bool ShouldSerializeCodIncPisPasep() => CodIncPisPasep != null;
#endif


        #endregion
    }

    /// <summary>
    /// Caso a empresa possua processo administrativo ou judicial com decisão/sentença favorável, determinando a não incidência de contribuição previdenciária relativa à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado através de evento específico na Tabela de Processos (S-1070).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoCP")]
    [ComVisible(true)]
#endif
    public class IdeProcessoCP
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        /// <summary>
        /// Informar um número de processo cadastrado através do evento S-1070, cujo indMatProc seja igual a [1]
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Extensão da decisão/sentença.
        /// </summary>
        [XmlElement("extDecisao")]
        public ExtensaoDecisao ExtDecisao { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    /// <summary>
    /// Caso a empresa possua processo judicial com decisão/sentença favorável, 
    /// determinando a não incidência de imposto de renda relativo à rubrica identificada no evento, 
    /// as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado 
    /// através de evento específico na Tabela de Processos (S-1070)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoIRRF")]
    [ComVisible(true)]
#endif
    public class IdeProcessoIRRF
    {
        /// <summary>
        /// Informar um número de processo judicial cadastrado através do evento S-1070, cujo indMatProc seja igual a [1]
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

    /// <summary>
    /// Caso a empresa possua processo judicial com decisão/sentença favorável, determinando a não incidência de FGTS 
    /// relativo à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo 
    /// deverá ser efetuado através de evento específico na Tabela de Processos (S-1070)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoFGTS1010")]
    [ComVisible(true)]
#endif
    public class IdeProcessoFGTS1010
    {
        /// <summary>
        /// Informar um número de processo judicial cadastrado através do evento S-1070, cujo indMatProc seja igual a [1, 7]
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }
    }

    /// <summary>
    /// Alteração das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1010")]
    [ComVisible(true)]
#endif
    public class Alteracao1010
    {
        /// <summary>
        /// Identificação da rubrica e validade das informações
        /// </summary>
        [XmlElement("ideRubrica")]
        public IdeRubrica IdeRubrica { get; set; }

        /// <summary>
        /// Detalhamento das informações da rubrica
        /// </summary>
        [XmlElement("dadosRubrica")]
        public DadosRubrica DadosRubrica { get; set; }

        /// <summary>
        /// Novo período de validade das informações
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1010 NovaValidade { get; set; }
    }

    /// <summary>
    /// Informação preenchida exclusivamente em caso de alteração do período de validade das informações, apresentando o novo período de validade
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1010")]
    [ComVisible(true)]
#endif
    public class NovaValidade1010 : NovaValidade1005 { }

    /// <summary>
    /// Exclusão das informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1010")]
    [ComVisible(true)]
#endif
    public class Exclusao1010
    {
        /// <summary>
        /// Identificação da rubrica e validade das informações
        /// </summary>
        [XmlElement("ideRubrica")]
        public IdeRubrica IdeRubrica { get; set; }
    }

    /// <summary>
    /// Caso a empresa possua processo judicial com decisão/sentença favorável, determinando a não incidência de contribuição para o PIS/PASEP relativo à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado através de evento específico na Tabela de Processos(S-1070).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoPisPasep")]
    [ComVisible(true)]
#endif
    public class IdeProcessoPisPasep
    {
        /// <summary>
        /// Informar um número de processo judicial cadastrado  através do evento S-1070, cujo indMatProc seja igual a [1].
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }
}
