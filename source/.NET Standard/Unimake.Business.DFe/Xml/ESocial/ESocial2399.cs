#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Business.DFe.Xml.SNCM;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2399")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// S-2399 - Trabalhador Sem Vínculo de Emprego/Estatutário - Término
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTSVTermino/v_S_01_02_00", IsNullable = false)]
    public class ESocial2399 : XMLBase
    {
        /// <summary>
        /// Evento TSVE - Término
        /// </summary>
        [XmlElement("evtTSVTermino")]
        public EvtTSVTermino EvtTSVTermino { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTSVTermino")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento TSVE - Término
    /// </summary>
    public class EvtTSVTermino
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Informações de identificação do evento.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial2399 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador.
        /// </summary>
        [XmlElement("ideTrabSemVinculo")]
        public IdeTrabSemVinculo IdeTrabSemVinculo { get; set; }

        /// <summary>
        /// Informações do término da prestação de serviço ou da execução da obra.
        /// </summary>
        [XmlElement("infoTSVTermino")]
        public InfoTSVTermino InfoTSVTermino { get; set; }
    }

    #region IdeEventoESocial2399
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial2399")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    public class IdeEventoESocial2399
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação.
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado.
        /// Validação: O preenchimento é obrigatório se indRetif = [2].
        /// Deve ser um recibo de entrega válido, correspondente
        /// ao arquivo que está sendo retificado.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Indicativo do tipo de guia. Valores válidos:
        /// 1 - Documento de Arrecadação do eSocial - DAE
        /// </summary>
        [XmlElement("indGuia")]
        public IndGuia? IndGuia { get; set; }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Processo de emissão do evento.
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeIndGuiaField() => !IndGuia.IsNullOrEmpty();
#else
        public bool ShouldSerializeIndGuiaField() => !IndGuia.IsNullOrEmpty();
#endif
        #endregion ShouldSerialize
    }
    #endregion IdeEventoESocial2399

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTSVTermino")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações do término da prestação de serviço ou da execução da obra.
    /// </summary>
    [Serializable()]
    public class InfoTSVTermino
    {
#if INTEROP
        public DateTime DtTerm { get; set; }
#else
        /// <summary>
        /// Validação: Deve ser uma data igual ou anterior à data atual acrescida de 10 (dez) dias.
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset DtTerm { get; set; }
#endif
        /// <summary>
        /// Validação: Deve ser uma data igual ou anterior à data atual acrescida de 10 (dez) dias.
        /// </summary>
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
        public MtvDesligTSV MtvDesligTSV {  get; set; } = (MtvDesligTSV)(-1);
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
        [XmlElement("percAliment")]
        public int PercAliment { get; set; }

        /// <summary>
        /// Valor da pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// Informação obrigatória e exclusiva se pensAlim = [2, 3].
        /// </summary>
        [XmlIgnore]
        public double VrAlim { get; set; }

        /// <summary>
        /// Valor da pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// Informação obrigatória e exclusiva se pensAlim = [2, 3].
        /// </summary>
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
        /// Informação do novo CPF do trabalhador.
        /// </summary>
        [XmlElement("mudancaCPF")]
        public MudancaCPFESocial2399 MudancaCPF { get; set; }

        /// <summary>
        /// Grupo onde são prestadas as informações relativas às
        ///verbas rescisórias do diretor não empregado, com FGTS.
        /// </summary>
        [XmlElement("verbasResc")]
        public VerbasRescESocial2399 VerbasResc { get; set; }

        /// <summary>
        /// Informações sobre a "quarentena" remunerada de
        /// trabalhador desligado ou outra situação de término com data anterior.
        /// O grupo deve ser preenchido apenas no caso do
        /// trabalhador que recebe remuneração após o
        /// desligamento por estar impossibilitado de exercer
        /// atividade remunerada ou no caso de término
        /// reconhecido judicialmente com data anterior a
        /// competências com remunerações já informadas no eSocial.
        /// </summary>
        [XmlElement("remunAposTerm")]
        public RemunAposTerm RemunAposTerm { get; set; }

        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeMtvDesligTsVField() => MtvDesligTSV != (MtvDesligTSV)(-1);
#else
        public bool ShouldSerializeMtvDesligTsVField() => !MtvDesligTSV.IsNullOrEmpty();
#endif
#if INTEROP
        public bool ShouldSerializePensAlimField() => PensAlim != (PensAlim)(-1);
#else
        public bool ShouldSerializePensAlimField() => !PensAlim.IsNullOrEmpty();
#endif

        public bool ShouldSerializePercAlimentField() => PercAliment > 0;
        public bool ShouldSerializeVrAlimField() => VrAlim > 0;
        public bool ShouldSerializeNrProcTrabField() => !string.IsNullOrEmpty(NrProcTrab);

        #endregion ShouldSerialize
    }

    #region MudancaCPFESocial2399
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.MudancaCPFESocial2399")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informação do novo CPF do trabalhador.
    /// </summary>
    public class MudancaCPFESocial2399
    {
        /// <summary>
        /// Preencher com o novo CPF do trabalhador.
        /// Validação: Deve ser um CPF válido e diferente do CPF
        /// do empregador e do antigo CPF do trabalhador
        /// </summary>
        [XmlElement("novoCPF")]
        public string NovoCPF { get; set; }
    }
    #endregion MudancaCPFESocial2399

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.VerbasRescESocial2399")]
    [ComVisible(true)]
#endif
    public class VerbasRescESocial2399
    {
        [XmlElement("dmDev")]
        public List<DmDevESocial2399> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDevESocial2399 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDevESocial2399>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDevESocial2399 GetDmDev(int index)
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

        [XmlElement("procJudTrab")]
        public List<ProcJudTrab> ProcJudTrab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrab item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrab>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrab GetProcJudTrab(int index)
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

        [XmlElement("infoMV")]
        public InfoMV InfoMV { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDevESocial2299")]
    [ComVisible(true)]
#endif
    public class DmDevESocial2399
    {
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        [XmlElement("indRRA")]
        public string IndRRA { get; set; }

        [XmlElement("infoRRA")]
        public InfoRRA InfoRRA { get; set; }

        /// <summary>
        /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLotESocial2399> IdeEstabLot { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLotESocial2399 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLotESocial2399>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLotESocial2399 GetIdeEstabLot(int index)
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

        [XmlElement("infoPerApur")]
        public InfoPerApurESocial2299 InfoPerApur { get; set; }

        [XmlElement("infoPerAnt")]
        public InfoPerAntESocial2299 InfoPerAnt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIndRRAField() => !string.IsNullOrEmpty(IndRRA);

        #endregion
    }

    #region IdeEstabLotESocial2399
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLotESocial2399")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
    /// </summary>
    public class IdeEstabLotESocial2399 : IdeEstabLotESocial2299 { }

    #endregion  IdeEstabLotESocial2399

    #region RemunAposTerm
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunAposTerm")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento e da lotação nos quais o trabalhador possui remuneração no período de apuração.
    /// </summary>
    public class RemunAposTerm : RemunAposDeslig { }

    #endregion  RemunAposTerm
}
