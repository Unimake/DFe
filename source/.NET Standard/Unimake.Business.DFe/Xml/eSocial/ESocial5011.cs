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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5011")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_02_00", IsNullable = false)]
    public class ESocial5011 : XMLBase
    {
        [XmlElement("evtCS")]
        public EvtCS EvtCS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCS")]
    [ComVisible(true)]
#endif
    public class EvtCS
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial5011 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoCS")]
        public InfoCS InfoCS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5011")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial5011
    {
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif

        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCS")]
    [ComVisible(true)]
#endif
    public class InfoCS
    {
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        [XmlElement("indExistInfo")]
        public IndicativoExistenciaTributos IndExistInfo { get; set; }

        [XmlElement("infoCPSeg")]
        public InfoCPSeg InfoCPSeg { get; set; }

        [XmlElement("infoContrib")]
        public InfoContrib InfoContrib { get; set; }

        [XmlElement("ideEstab")]
        public List<IdeEstabESocial5011> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstabESocial5011 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstabESocial5011>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstabESocial5011 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstab
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif

        [XmlElement("infoCRContrib")]
        public List<InfoCRContribESocial5011> InfoCRContrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRContrib(InfoCRContribESocial5011 item)
        {
            if (InfoCRContrib == null)
            {
                InfoCRContrib = new List<InfoCRContribESocial5011>();
            }

            InfoCRContrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRContrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRContrib</returns>
        public InfoCRContribESocial5011 GetInfoCRContrib(int index)
        {
            if ((InfoCRContrib?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRContrib[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRContrib
        /// </summary>
        public int GetInfoCRContribCount => (InfoCRContrib != null ? InfoCRContrib.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCPSeg")]
    [ComVisible(true)]
#endif
    public class InfoCPSeg
    {
        [XmlElement("vrDescCP")]
        public double VrDescCP { get; set; }

        [XmlElement("vrCpSeg")]
        public double VrCpSeg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContrib")]
    [ComVisible(true)]
#endif
    public class InfoContrib
    {
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        [XmlElement("infoPJ")]
        public InfoPJ InfoPJ { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPJ")]
    [ComVisible(true)]
#endif
    public class InfoPJ
    {
        [XmlElement("indCoop")]
#if INTEROP
        public IndCoop IndCoop { get; set; } = (IndCoop)(-1);
#else
        public IndCoop ? IndCoop { get; set; }
#endif

        [XmlElement("indConstr")]
        public IndConstr IndConstr { get; set; }

        [XmlElement("indSubstPatr")]
#if INTEROP
        public IndSubstPatr IndSubstPatr { get; set; } = (IndSubstPatr)(-1);
#else
        public IndSubstPatr ? IndSubstPatr { get; set; }
#endif

        [XmlElement("percRedContrib")]
        public double PercRedContrib { get; set; }

        [XmlElement("percTransf")]
        public string PercTransf { get; set; }

        [XmlElement("indTribFolhaPisCofins")]
        public string IndTribFolhaPisCofins { get; set; }

        [XmlElement("infoAtConc")]
        public InfoAtConc InfoAtConc { get; set; }

        #region ShouldSerialize


#if INTEROP
        public bool ShouldSerializeIndCoop() => IndCoop != (IndCoop)(-1);
#else
        public bool ShouldSerializeIndCoop() => IndCoop != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndSubstPatr() => IndSubstPatr != (IndSubstPatr)(-1);
#else
        public bool ShouldSerializeIndSubstPatr() => IndSubstPatr != null;
#endif

        public bool ShouldSerializePercRedContrib() => PercRedContrib > 0;

        public bool ShouldSerializePercTransfField() => !string.IsNullOrEmpty(PercTransf);

        public bool ShouldSerializeIndTribFolhaPisCofins() => !string.IsNullOrEmpty(IndTribFolhaPisCofins);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtConc")]
    [ComVisible(true)]
#endif
    public class InfoAtConc
    {
        [XmlElement("fatorMes")]
        public double FatorMes { get; set; }

        [XmlElement("fator13")]
        public double Fator13 { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabESocial5011")]
    [ComVisible(true)]
#endif
    public class IdeEstabESocial5011
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        [XmlElement("infoEstab")]
        public InfoEstabESocial5011 InfoEstab { get; set; }

        [XmlElement("ideLotacao")]
        public List<IdeLotacaoESocial5011> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacaoESocial5011 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacaoESocial5011>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacaoESocial5011 GetIdeLotacao(int index)
        {
            if ((IdeLotacao?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeLotacao[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeLotacao
        /// </summary>
        public int GetIdeLotacaoCount => (IdeLotacao != null ? IdeLotacao.Count : 0);
#endif

        [XmlElement("basesAquis")]
        public List<BasesAquis> BasesAquis { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesAquis(BasesAquis item)
        {
            if (BasesAquis == null)
            {
                BasesAquis = new List<BasesAquis>();
            }

            BasesAquis.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesAquis (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesAquis</returns>
        public BasesAquis GetBasesAquis(int index)
        {
            if ((BasesAquis?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesAquis[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesAquis
        /// </summary>
        public int GetBasesAquisCount => (BasesAquis != null ? BasesAquis.Count : 0);
#endif

        [XmlElement("basesComerc")]
        public List<BasesComerc> BasesComerc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesComerc(BasesComerc item)
        {
            if (BasesComerc == null)
            {
                BasesComerc = new List<BasesComerc>();
            }

            BasesComerc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesComerc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesComerc</returns>
        public BasesComerc GetBasesComerc(int index)
        {
            if ((BasesComerc?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesComerc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesComerc
        /// </summary>
        public int GetBasesComercCount => (BasesComerc != null ? BasesComerc.Count : 0);
#endif

        [XmlElement("infoCREstab")]
        public List<InfoCREstab> InfoCREstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCREstab(InfoCREstab item)
        {
            if (InfoCREstab == null)
            {
                InfoCREstab = new List<InfoCREstab>();
            }

            InfoCREstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCREstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCREstab</returns>
        public InfoCREstab GetInfoCREstab(int index)
        {
            if ((InfoCREstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCREstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCREstab
        /// </summary>
        public int GetInfoCREstabCount => (InfoCREstab != null ? InfoCREstab.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstabESocial5011")]
    [ComVisible(true)]
#endif
    public class InfoEstabESocial5011
    {
        [XmlElement("cnaePrep")]
        public string CnaePrep { get; set; }

        [XmlElement("cnpjResp")]
        public string CnpjResp { get; set; }

        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        [XmlElement("fap")]
        public double Fap { get; set; }

        [XmlElement("aliqRatAjust")]
        public double AliqRatAjust { get; set; }

        [XmlElement("infoEstabRef")]
        public InfoEstabRef InfoEstabRef { get; set; }

        [XmlElement("infoComplObra")]
        public InfoComplObra InfoComplObra { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjResp() => !string.IsNullOrEmpty(CnpjResp);

        public bool ShouldSerializeFap() => Fap > 0;

        public bool ShouldSerializeAliqRatAjust() => AliqRatAjust > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstabRef")]
    [ComVisible(true)]
#endif
    public class InfoEstabRef
    {
        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        [XmlElement("fap")]
        public double Fap { get; set; }

        [XmlElement("aliqRatAjust")]
        public double AliqRatAjust { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFap() => Fap > 0;

        public bool ShouldSerializeAliqRatAjust() => AliqRatAjust > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplObra")]
    [ComVisible(true)]
#endif
    public class InfoComplObra
    {
        [XmlElement("indSubstPatrObra")]
        public IndicativoSubstituicaoPatronal IndSubstPatrObra { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacaoESocial5011")]
    [ComVisible(true)]
#endif
    public class IdeLotacaoESocial5011
    {
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        [XmlElement("fpas")]
        public string Fpas { get; set; }

        [XmlElement("codTercs")]
        public string CodTercs { get; set; }

        [XmlElement("codTercsSusp")]
        public string CodTercsSusp { get; set; }

        [XmlElement("infoTercSusp")]
        public List<InfoTercSusp> InfoTercSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTercSusp(InfoTercSusp item)
        {
            if (InfoTercSusp == null)
            {
                InfoTercSusp = new List<InfoTercSusp>();
            }

            InfoTercSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTercSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTercSusp</returns>
        public InfoTercSusp GetInfoTercSusp(int index)
        {
            if ((InfoTercSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoTercSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoTercSusp
        /// </summary>
        public int GetInfoTercSuspCount => (InfoTercSusp != null ? InfoTercSusp.Count : 0);
#endif

        [XmlElement("infoEmprParcial")]
        public InfoEmprParcialESocial5011 InfoEmprParcial { get; set; }

        [XmlElement("dadosOpPort")]
        public DadosOpPortESocial5011 DadosOpPort { get; set; }

        [XmlElement("basesRemun")]
        public List<BasesRemun> BasesRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesRemun(BasesRemun item)
        {
            if (BasesRemun == null)
            {
                BasesRemun = new List<BasesRemun>();
            }

            BasesRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesRemun</returns>
        public BasesRemun GetBasesRemun(int index)
        {
            if ((BasesRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesRemun
        /// </summary>
        public int GetBasesRemunCount => (BasesRemun != null ? BasesRemun.Count : 0);
#endif

        [XmlElement("basesAvNPort")]
        public BasesAvNPort BasesAvNPort { get; set; }

        [XmlElement("infoSubstPatrOpPort")]
        public InfoSubstPatrOpPortESocial5011 InfoSubstPatrOpPort { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodTercsSusp() => !string.IsNullOrEmpty(CodTercsSusp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTercSusp")]
    [ComVisible(true)]
#endif
    public class InfoTercSusp
    {
        [XmlElement("codTerc")]
        public string CodTerc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmprParcialESocial5011")]
    [ComVisible(true)]
#endif
    public class InfoEmprParcialESocial5011
    {
        [XmlElement("tpInscContrat")]
        public TpInsc TpInscContrat { get; set; }

        [XmlElement("nrInscContrat")]
        public string NrInscContrat { get; set; }

        [XmlElement("tpInscProp")]
        public TpInsc TpInscProp { get; set; }

        [XmlElement("nrInscProp")]
        public string NrInscProp { get; set; }

        [XmlElement("cnoObra")]
        public string CnoObra { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosOpPortESocial5011")]
    [ComVisible(true)]
#endif
    public class DadosOpPortESocial5011
    {
        [XmlElement("cnpjOpPortuario")]
        public string CnpjOpPortuario { get; set; }

        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        [XmlElement("fap")]
        public double Fap { get; set; }

        [XmlElement("aliqRatAjust")]
        public double AliqRatAjust { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesRemun")]
    [ComVisible(true)]
#endif
    public class BasesRemun
    {
        [XmlElement("indIncid")]
        public IndIncid IndIncid { get; set; }

        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        [XmlElement("basesCp")]
        public BasesCp BasesCp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesCp")]
    [ComVisible(true)]
#endif
    public class BasesCp
    {
        [XmlElement("vrBcCp00")]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp15")]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp20")]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp25")]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrSuspBcCp00")]
        public double VrSuspBcCp00 { get; set; }

        [XmlElement("vrSuspBcCp15")]
        public double VrSuspBcCp15 { get; set; }

        [XmlElement("vrSuspBcCp20")]
        public double VrSuspBcCp20 { get; set; }

        [XmlElement("vrSuspBcCp25")]
        public double VrSuspBcCp25 { get; set; }

        [XmlElement("vrBcCp00VA")]
        public double VrBcCp00VA { get; set; }

        [XmlElement("vrBcCp15VA")]
        public double VrBcCp15VA { get; set; }

        [XmlElement("vrBcCp20VA")]
        public double VrBcCp20VA { get; set; }

        [XmlElement("vrBcCp25VA")]
        public double VrBcCp25VA { get; set; }

        [XmlElement("vrSuspBcCp00VA")]
        public double VrSuspBcCp00VA { get; set; }

        [XmlElement("vrSuspBcCp15VA")]
        public double VrSuspBcCp15VA { get; set; }

        [XmlElement("vrSuspBcCp20VA")]
        public double VrSuspBcCp20VA { get; set; }

        [XmlElement("vrSuspBcCp25VA")]
        public double VrSuspBcCp25VA { get; set; }

        [XmlElement("vrDescSest")]
        public double VrDescSest { get; set; }

        [XmlElement("vrCalcSest")]
        public double VrCalcSest { get; set; }

        [XmlElement("vrDescSenat")]
        public double VrDescSenat { get; set; }

        [XmlElement("vrCalcSenat")]
        public double VrCalcSenat { get; set; }

        [XmlElement("vrSalFam")]
        public double VrSalFam { get; set; }

        [XmlElement("vrSalMat")]
        public double VrSalMat { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcCp00VA() => VrBcCp00VA > 0;

        public bool ShouldSerializeVrBcCp15VA() => VrBcCp15VA > 0;

        public bool ShouldSerializeVrBcCp20VA() => VrBcCp20VA > 0;

        public bool ShouldSerializeVrBcCp25VA() => VrBcCp25VA > 0;

        public bool ShouldSerializeVrSuspBcCp00VA() => VrSuspBcCp00VA > 0;

        public bool ShouldSerializeVrSuspBcCp15VA() => VrSuspBcCp15VA > 0;

        public bool ShouldSerializeVrSuspBcCp20VA() => VrSuspBcCp20VA > 0;

        public bool ShouldSerializeVrSuspBcCp25VA() => VrSuspBcCp25VA > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesAvNPort")]
    [ComVisible(true)]
#endif
    public class BasesAvNPort
    {
        [XmlElement("vrBcCp00")]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp15")]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp20")]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp25")]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp13")]
        public double VrBcCp13 { get; set; }

        [XmlElement("vrDescCP")]
        public double VrDescCP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatrOpPortESocial5011")]
    [ComVisible(true)]
#endif
    public class InfoSubstPatrOpPortESocial5011
    {
        [XmlElement("cnpjOpPortuario")]
        public string CnpjOpPortuario { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesAquis")]
    [ComVisible(true)]
#endif
    public class BasesAquis
    {
        [XmlElement("indAquis")]
        public IndAquis IndAquis { get; set; }

        [XmlElement("vlrAquis")]
        public double VlrAquis { get; set; }

        [XmlElement("vrCPDescPR")]
        public double VrCPDescPR { get; set; }

        [XmlElement("vrCPNRet")]
        public double VrCPNRet { get; set; }

        [XmlElement("vrRatNRet")]
        public double VrRatNRet { get; set; }

        [XmlElement("vrSenarNRet")]
        public double VrSenarNRet { get; set; }

        [XmlElement("vrCPCalcPR")]
        public double VrCPCalcPR { get; set; }

        [XmlElement("vrRatDescPR")]
        public double VrRatDescPR { get; set; }

        [XmlElement("vrRatCalcPR")]
        public double VrRatCalcPR { get; set; }

        [XmlElement("vrSenarDesc")]
        public double VrSenarDesc { get; set; }

        [XmlElement("vrSenarCalc")]
        public double VrSenarCalc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesComerc")]
    [ComVisible(true)]
#endif
    public class BasesComerc
    {
        [XmlElement("indComerc")]
        public IndComerc IndComerc { get; set; }

        [XmlElement("vrBcComPR")]
        public double VrBcComPR { get; set; }

        [XmlElement("vrCPSusp")]
        public double VrCPSusp { get; set; }

        [XmlElement("vrRatSusp")]
        public double VrRatSusp { get; set; }

        [XmlElement("vrSenarSusp")]
        public double VrSenarSusp { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeVrCPSusp() => VrCPSusp > 0;

        public bool ShouldSerializeVrRatSusp() => VrRatSusp > 0;

        public bool ShouldSerializeVrSenarSusp() => VrSenarSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCREstab")]
    [ComVisible(true)]
#endif
    public class InfoCREstab
    {
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        [XmlElement("vrCR")]
        public double VrCR { get; set; }

        [XmlElement("vrSuspCR")]
        public double VrSuspCR { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrSuspCR() => VrSuspCR > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRContribESocial5011")]
    [ComVisible(true)]
#endif
    public class InfoCRContribESocial5011
    {
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        [XmlElement("vrCR")]
        public double VrCR { get; set; }

        [XmlElement("vrCRSusp")]
        public double VrCRSusp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVrCRSusp() => VrCRSusp > 0;

        #endregion
    }
}
