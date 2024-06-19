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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2240")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExpRisco/v_S_01_02_00", IsNullable = false)]
    public class ESocial2240 : XMLBase
    {
        [XmlElement("evtExpRisco")]
        public EvtExpRisco EvtExpRisco { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtExpRisco")]
    [ComVisible(true)]
#endif
    public class EvtExpRisco
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculoESocial2220 IdeVinculo { get; set; }

        [XmlElement("infoExpRisco")]
        public InfoExpRisco InfoExpRisco { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoExpRisco")]
    [ComVisible(true)]
#endif
    public class InfoExpRisco
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtIniCondicao { get; set; }
#else
        public DateTimeOffset DtIniCondicao { get; set; }
#endif

        [XmlElement("dtIniCondicao")]
        public string DtIniCondicaoField
        {
            get => DtIniCondicao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtIniCondicao = DateTime.Parse(value);
#else
            set => DtIniCondicao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtFimCondicao { get; set; }
#else
        public DateTimeOffset DtFimCondicao { get; set; }
#endif

        [XmlElement("dtFimCondicao")]
        public string DtFimCondicaoField
        {
            get => DtFimCondicao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtFimCondicao = DateTime.Parse(value);
#else
            set => DtFimCondicao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("infoAmb")]
        public List<InfoAmb> InfoAmb { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoAmb(InfoAmb item)
        {
            if (InfoAmb == null)
            {
                InfoAmb = new List<InfoAmb>();
            }

            InfoAmb.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoAmb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoAmb</returns>
        public InfoAmb GetInfoAmb(int index)
        {
            if ((InfoAmb?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoAmb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoAmb
        /// </summary>
        public int GetInfoAmbCount => (InfoAmb != null ? InfoAmb.Count : 0);
#endif

        [XmlElement("infoAtiv")]
        public InfoAtiv InfoAtiv { get; set; }

        [XmlElement("agNoc")]
        public List<AgNoc> AgNoc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddAgNoc(AgNoc item)
        {
            if (AgNoc == null)
            {
                AgNoc = new List<AgNoc>();
            }

            AgNoc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista AgNoc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AgNoc</returns>
        public AgNoc GetAgNoc(int index)
        {
            if ((AgNoc?.Count ?? 0) == 0)
            {
                return default;
            };

            return AgNoc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AgNoc
        /// </summary>
        public int GetAgNocCount => (AgNoc != null ? AgNoc.Count : 0);
#endif

        [XmlElement("respReg")]
        public List<RespReg> RespReg { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRespReg(RespReg item)
        {
            if (RespReg == null)
            {
                RespReg = new List<RespReg>();
            }

            RespReg.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RespReg (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RespReg</returns>
        public RespReg GetRespReg(int index)
        {
            if ((RespReg?.Count ?? 0) == 0)
            {
                return default;
            };

            return RespReg[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RespReg
        /// </summary>
        public int GetRespRegCount => (RespReg != null ? RespReg.Count : 0);
#endif

        [XmlElement("obs")]
        public Obs Obs { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtFimCondicaoField() => DtFimCondicao > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAmb")]
    [ComVisible(true)]
#endif
    public class InfoAmb
    {
        [XmlElement("localAmb")]
        public LocalAmb LocalAmb { get; set; }

        [XmlElement("dscSetor")]
        public string DscSetor { get; set; }

        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtiv")]
    [ComVisible(true)]
#endif
    public class InfoAtiv
    {
        [XmlElement("dscAtivDes")]
        public string DscAtivDes { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.AgNoc")]
    [ComVisible(true)]
#endif
    public class AgNoc
    {
        [XmlElement("codAgNoc")]
        public string CodAgNoc { get; set; }

        [XmlElement("dscAgNoc")]
        public string DscAgNoc { get; set; }

        [XmlElement("tpAval")]
#if INTEROP
        public TpAval TpAval { get; set; } = (TpAval)(-1);
#else
        public TpAval? TpAval { get; set; }
#endif

        [XmlElement("intConc")]
        public string IntConc { get; set; }

        [XmlElement("limTol")]
        public string LimTol { get; set; }

        [XmlElement("unMed")]
#if INTEROP
        public UnMed UnMed { get; set; } = (UnMed)(-1);
#else
        public UnMed? UnMed { get; set; }
#endif

        [XmlElement("tecMedicao")]
        public string TecMedicao { get; set; }

        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        [XmlElement("epcEpi")]
        public EpcEpi EpcEpi { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDscAgNocField() => !string.IsNullOrEmpty(DscAgNoc);

#if INTEROP
        public bool ShouldSerializeTpAval() => TpAval != (TpAval)(-1);
#else
        public bool ShouldSerializeTpAval() => TpAval != null;
#endif

        public bool ShouldSerializeIntConcField() => !string.IsNullOrEmpty(IntConc);

        public bool ShouldSerializeLimTolField() => !string.IsNullOrEmpty(LimTol);

#if INTEROP
        public bool ShouldSerializeUnMed() => UnMed != (UnMed)(-1);
#else
        public bool ShouldSerializeUnMed() => UnMed != null;
#endif

        public bool ShouldSerializeTecMedicaoField() => !string.IsNullOrEmpty(TecMedicao);

        public bool ShouldSerializeNrProcJudField() => !string.IsNullOrEmpty(NrProcJud);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EpcEpi")]
    [ComVisible(true)]
#endif
    public class EpcEpi
    {
        [XmlElement("utilizEPC")]
        public UtilizEPC UtilizEPC { get; set; }

        [XmlElement("eficEpc")]
#if INTEROP
        public SimNaoLetra EficEpc { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? EficEpc { get; set; }
#endif

        [XmlElement("utilizEPI")]
        public UtilizEPI UtilizEPI { get; set; }

        [XmlElement("eficEpi")]
#if INTEROP
        public SimNaoLetra EficEpi { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? EficEpi { get; set; }
#endif

        [XmlElement("epi")]
        public List<Epi> Epi { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddEpi(Epi item)
        {
            if (Epi == null)
            {
                Epi = new List<Epi>();
            }

            Epi.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Epi (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Epi</returns>
        public Epi GetEpi(int index)
        {
            if ((Epi?.Count ?? 0) == 0)
            {
                return default;
            };

            return Epi[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Epi
        /// </summary>
        public int GetEpiCount => (Epi != null ? Epi.Count : 0);
#endif

        [XmlElement("epiCompl")]
        public EpiCompl EpiCompl { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeEficEpc() => EficEpc != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeEficEpc() => EficEpc != null;
#endif

#if INTEROP
        public bool ShouldSerializeEficEpi() => EficEpi != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeEficEpi() => EficEpi != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Epi")]
    [ComVisible(true)]
#endif
    public class Epi
    {
        [XmlElement("docAval")]
        public string DocAval { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EpiCompl")]
    [ComVisible(true)]
#endif
    public class EpiCompl
    {
        [XmlElement("medProtecao")]
        public SimNaoLetra MedProtecao { get; set; }

        [XmlElement("condFuncto")]
        public SimNaoLetra CondFuncto { get; set; }

        [XmlElement("usoInint")]
        public SimNaoLetra UsoInint { get; set; }

        [XmlElement("przValid")]
        public SimNaoLetra PrzValid { get; set; }

        [XmlElement("periodicTroca")]
        public SimNaoLetra PeriodicTroca { get; set; }

        [XmlElement("higienizacao")]
        public SimNaoLetra Higienizacao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RespReg")]
    [ComVisible(true)]
#endif
    public class RespReg
    {
        [XmlElement("cpfResp")]
        public string CpfResp { get; set; }

        [XmlElement("ideOC")]
#if INTEROP
        public IdeOc IdeOc { get; set; } = (IdeOc)(-1);
#else
        public IdeOc? IdeOc { get; set; }
#endif

        [XmlElement("dscOC")]
        public string DscOC { get; set; }

        [XmlElement("nrOC")]
        public string NrOC { get; set; }

        [XmlElement("ufOC")]
#if INTEROP
        public UFBrasil UfOC { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UfOC { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIdeOc() => IdeOc != (IdeOc)(-1);
#else
        public bool ShouldSerializeIdeOc() => IdeOc != null;
#endif

        public bool ShouldSerializeDscOCField() => !string.IsNullOrEmpty(DscOC);

        public bool ShouldSerializeNrOCField() => !string.IsNullOrEmpty(NrOC);

#if INTEROP
        public bool ShouldSerializeUfOC() => UfOC != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUfOC() => UfOC != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Obs")]
    [ComVisible(true)]
#endif
    public class Obs
    {
        [XmlElement("obsCompl")]
        public string ObsCompl { get; set; }
    }
}
