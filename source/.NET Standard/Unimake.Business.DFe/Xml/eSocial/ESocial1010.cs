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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1010")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTabRubrica/v_S_01_02_00", IsNullable = false)]
    public class ESocial1010 : XMLBase
    {
        [XmlElement("evtTabRubrica")]
        public EvtTabRubrica EvtTabRubrica { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTabRubrica")]
    [ComVisible(true)]
#endif
    public class EvtTabRubrica
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento1010 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoRubrica")]
        public InfoRubrica InfoRubrica { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1010")]
    [ComVisible(true)]
#endif
    public class IdeEvento1010 : IdeEvento { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRubrica")]
    [ComVisible(true)]
#endif
    public class InfoRubrica
    {
        [XmlElement("inclusao")]
        public Inclusao1010 Inclusao { get; set; }

        [XmlElement("alteracao")]
        public Alteracao1010 Alteracao { get; set; }

        [XmlElement("exclusao")]
        public Exclusao1010 Exclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Inclusao1010")]
    [ComVisible(true)]
#endif
    public class Inclusao1010
    {
        [XmlElement("ideRubrica")]
        public IdeRubrica IdeRubrica { get; set; }

        [XmlElement("dadosRubrica")]
        public DadosRubrica DadosRubrica { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeRubrica")]
    [ComVisible(true)]
#endif
    public class IdeRubrica
    {
        [XmlElement("codRubr")]
        public string CodRubr { get; set; }

        [XmlElement("ideTabRubr")]
        public string IdeTabRubr { get; set; }

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

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosRubrica")]
    [ComVisible(true)]
#endif
    public class DadosRubrica
    {
        [XmlElement("dscRubr")]
        public string DscRubr { get; set; }

        [XmlElement("natRubr")]
        public int NatRubr { get; set; }

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
        public CodIncFGTS CodIncFGTS { get; set; }

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
        /// Informar se a rubrica compõe o teto remuneratório específico (art. 37, XI, da CF/1988).
        /// Validação: Preenchimento obrigatório se a natureza jurídica do declarante for Administração Pública (grupo [1]).
        /// </summary>
        [XmlElement("tetoRemun")]
#if INTEROP
        public SimNaoLetra TetoRemun { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? TetoRemun { get; set; }
#endif

        [XmlElement("observacao")]
        public string Observacao { get; set; }

        /// <summary>
        /// Caso a empresa possua processo administrativo ou judicial com decisão/sentença favorável, determinando a não incidência de contribuição previdenciária relativa à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado através de evento específico na Tabela de Processos (S-1070).
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
        /// Caso a empresa possua processo judicial com decisão/sentença favorável, determinando a não incidência de imposto de renda relativo à rubrica identificada no evento, as informações deverão ser incluídas neste grupo, e o detalhamento do processo deverá ser efetuado através de evento específico na Tabela de Processos (S-1070).
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

        [XmlElement("ideProcessoFGTS")]
        public List<IdeProcessoFGTS> IdeProcessoFGTS { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeProcessoFGTS(IdeProcessoFGTS item)
        {
            if (IdeProcessoFGTS == null)
            {
                IdeProcessoFGTS = new List<IdeProcessoFGTS>();
            }

            IdeProcessoFGTS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeProcessoFGTS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeProcessoFGTS</returns>
        public IdeProcessoFGTS GetIdeProcessoFGTS(int index)
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

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoCP")]
    [ComVisible(true)]
#endif
    public class IdeProcessoCP
    {
        [XmlElement("tpProc")]
        public TipoProcesso TpProc { get; set; }

        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Extensão da decisão/sentença.
        /// </summary>
        [XmlElement("extDecisao")]
        public ExtensaoDecisao ExtDecisao { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoIRRF")]
    [ComVisible(true)]
#endif
    public class IdeProcessoIRRF
    {
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcessoFGTS")]
    [ComVisible(true)]
#endif
    public class IdeProcessoFGTS
    {
        [XmlElement("nrProc")]
        public string NrProc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1010")]
    [ComVisible(true)]
#endif
    public class Alteracao1010
    {
        [XmlElement("dadosRubrica")]
        public DadosRubrica DadosRubrica { get; set; }

        [XmlElement("novaValidade")]
        public NovaValidade1010 NovaValidade { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1010")]
    [ComVisible(true)]
#endif
    public class NovaValidade1010 : NovaValidade1005 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclusao1010")]
    [ComVisible(true)]
#endif
    public class Exclusao1010
    {
        [XmlElement("ideRubrica")]
        public IdeRubrica IdeRubrica { get; set; }
    }
}
