#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2399 - Trabalhador Sem Vínculo de Emprego/Estatutário - Término
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2399")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVTermino/v_S_01_02_00", IsNullable = false)]
    public class ESocial2399 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Trabalhador Sem Vínculo de Emprego/Estatutário - Término
        /// </summary>
        [XmlElement("evtTSVTermino")]
        public EvtTSVTermino EvtTSVTermino { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Trabalhador Sem Vínculo de Emprego/Estatutário - Término
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTSVTermino")]
    [ComVisible(true)]
#endif
    public class EvtTSVTermino
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
        public IdeEvento2399 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do Trabalhador Sem Vínculo de Emprego/Estatutário - TSVE
        /// </summary>
        [XmlElement("ideTrabSemVinculo")]
        public IdeTrabSemVinculo2399 IdeTrabSemVinculo { get; set; }

        [XmlElement("infoTSVTermino")]
        public InfoTSVTermino InfoTSVTermino { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2399")]
    [ComVisible(true)]
#endif
    public class IdeEvento2399 : IdeEvento2299 { }

    /// <summary>
    /// Identificação do Trabalhador Sem Vínculo de Emprego/Estatutário - TSVE
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabSemVinculo2399")]
    [ComVisible(true)]
#endif
    public class IdeTrabSemVinculo2399 : IdeTrabSemVinculo2306 { }

    /// <summary>
    /// TSVE - Término
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTSVTermino")]
    [ComVisible(true)]
#endif
    public class InfoTSVTermino
    {
        /// <summary>
        /// Data do término
        /// Validação: Deve ser uma data igual ou anterior à data atual acrescida de 10 (dez) dias.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtTerm { get; set; }
#else
        public DateTimeOffset DtTerm { get; set; }
#endif

        [XmlElement("dtTerm")]
        public string DtTermField
        {
            get => DtTerm.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTerm = DateTime.Parse(value);
#else
            set => DtTerm = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Motivo do término.
        /// </summary>
        [XmlElement("mtvDesligTSV")]
#if INTEROP
        public MtvDesligTSV MtvDesligTSV { get; set; } = (MtvDesligTSV)(-1);
#else
        public MtvDesligTSV? MtvDesligTSV { get; set; }
#endif

        /// <summary>
        /// Indicativo de pensão alimentícia para fins de retenção de FGTS.
        /// </summary>
        [XmlElement("pensAlim")]
#if INTEROP
        public PensAlim PensAlim { get; set; } = (PensAlim)(-1);
#else
        public PensAlim? PensAlim { get; set; }
#endif

        /// <summary>
        /// Percentual a ser destinado a pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero) e menor ou igual
        /// a 100 (cem).
        /// Informação obrigatória e exclusiva se pensAlim = [1, 3].
        /// </summary>
        [XmlIgnore]
        public double PercAliment { get; set; }

        [XmlElement("percAliment")]
        public string PercAlimentField
        {
            get => PercAliment.ToString("F2", CultureInfo.InvariantCulture);
            set => PercAliment = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// Informação obrigatória e exclusiva se pensAlim = [2, 3].
        /// </summary>
        [XmlIgnore]
        public double VrAlim { get; set; }

        [XmlElement("vrAlim")]
        public string VrAlimField
        {
            get => VrAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VrAlim = Converter.ToDouble(value);
        }

        /// <summary>
        /// Número que identifica o processo trabalhista, quando o
        /// término de TSVE se der por decisão judicial.
        /// Validação: Se preenchido, deve ser um processo judicial
        /// válido, com 20 (vinte) algarismos.
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Informação do novo CPF do trabalhador
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCPF2399 MudancaCPF { get; set; }

        /// <summary>
        /// Grupo onde são prestadas as informações relativas às verbas rescisórias do diretor não empregado, com FGTS
        /// </summary>
        [XmlElement("verbasResc")]
        public VerbasResc2399 VerbasResc { get; set; }

        /// <summary>
        /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
        /// </summary>
        [XmlElement("remunAposTerm")]
        public RemunAposTerm2399 RemunAposTerm { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeMtvDesligTsVField() => MtvDesligTSV != (MtvDesligTSV)(-1);
#else
        public bool ShouldSerializeMtvDesligTsV() => MtvDesligTSV != null;
#endif
#if INTEROP
        public bool ShouldSerializePensAlimField() => PensAlim != (PensAlim)(-1);
#else
        public bool ShouldSerializePensAlim() => PensAlim != null;
#endif

        public bool ShouldSerializePercAlimentField() => PercAliment > 0;
        public bool ShouldSerializeVrAlimField() => VrAlim > 0;
        public bool ShouldSerializeNrProcTrab() => !string.IsNullOrEmpty(NrProcTrab);

        #endregion ShouldSerialize
    }

    #region MudancaCPF2399

    /// <summary>
    /// Informação do novo CPF do trabalhador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPF2399")]
    [ComVisible(true)]
#endif
    public class MudancaCPF2399
    {
        /// <summary>
        /// Preencher com o novo CPF do trabalhador.
        /// Validação: Deve ser um CPF válido e diferente do CPF
        /// do empregador e do antigo CPF do trabalhador
        /// </summary>
        [XmlElement("novoCPF")]
        public string NovoCPF { get; set; }
    }
    #endregion MudancaCPF2399

    /// <summary>
    /// Grupo onde são prestadas as informações relativas às verbas rescisórias do diretor não empregado, com FGTS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.VerbasResc2399")]
    [ComVisible(true)]
#endif
    public class VerbasResc2399
    {
        /// <summary>
        /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("dmDev")]
        public List<DmDev2399> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev2399 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev2399>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDev2399 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDev
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif

        /// <summary>
        /// Informações sobre a existência de processos judiciais do trabalhador com decisão favorável quanto à 
        /// não incidência de contribuições sociais e/ou Imposto de Renda
        /// </summary>
        [XmlElement("procJudTrab")]
        public List<ProcJudTrab2399> ProcJudTrab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrab2399 item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrab2399>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrab2399 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrab2399 GetProcJudTrab(int index)
        {
            if ((ProcJudTrab?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcJudTrab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcJudTrab
        /// </summary>
        public int GetProcJudTrabCount => (ProcJudTrab != null ? ProcJudTrab.Count : 0);
#endif

        /// <summary>
        /// Grupo preenchido exclusivamente em caso de trabalhador que possua outros vínculos/atividades nos quais já tenha ocorrido desconto de contribuição previdenciária
        /// </summary>
        [XmlElement("infoMV")]
        public InfoMV2399 InfoMV { get; set; }

    }

    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev2399")]
    [ComVisible(true)]
#endif
    public class DmDev2399
    {
        /// <summary>
        /// Identificador atribuído pela empresa para o demonstrativo de valores devidos ao trabalhador relativo a verbas rescisórias
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Indicativo de Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("indRRA")]
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRRA { get; set; }
#endif

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA2399 InfoRRA { get; set; }

        /// <summary>
        /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLot2399> IdeEstabLot { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLot2399 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLot2399>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLot2399 GetIdeEstabLot(int index)
        {
            if ((IdeEstabLot?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstabLot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabLot
        /// </summary>
        public int GetIdeEstabLotCount => (IdeEstabLot != null ? IdeEstabLot.Count : 0);
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif
        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA2399")]
    [ComVisible(true)]
#endif
    public class InfoRRA2399 : InfoRRA1202 { }

    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLot2399")]
    [ComVisible(true)]
#endif
    public class IdeEstabLot2399 : IdeEstabLot2299 { }

    /// <summary>
    /// Informações sobre a existência de processos judiciais do trabalhador com decisão favorável 
    /// quanto à não incidência de contribuições sociais e/ou Imposto de Renda
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ProcJudTrab2399")]
    [ComVisible(true)]
#endif
    public class ProcJudTrab2399 : ProcJudTrab1200 { }

    /// <summary>
    /// Grupo preenchido exclusivamente em caso de trabalhador que possua outros 
    /// vínculos/atividades nos quais já tenha ocorrido desconto de contribuição previdenciária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoMV2399")]
    [ComVisible(true)]
#endif
    public class InfoMV2399 : InfoMV1200 { }

    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAposTerm2399")]
    [ComVisible(true)]
#endif
    public class RemunAposTerm2399 : RemunAposDeslig2299 { }

}
