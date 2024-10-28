#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NF3e
{
    /// <summary>
    /// NF3e - Nota Fiscal da Energia Elétrica Eletrônica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("NF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class NF3e : XMLBase
    {
        [XmlElement("infNF3e")]
        public InfNF3e InfNF3e { get; set; }

        [XmlElement("infNF3eSupl")]
        public InfNF3eSupl InfNF3eSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    #region

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.InfNF3e")]
    [ComVisible(true)]
#endif
    public class InfNF3e
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        [XmlElement("ide")]
        public IdeNF3e Ide { get; set; }

        [XmlElement("emit")]
        public EmitNF3e Emit { get; set; }

        [XmlElement("dest")]
        public DestNF3e Dest { get; set; }

        [XmlElement("acessante")]
        public Acessante Acessante { get; set; }

        [XmlElement("gSub")]
        public GSub GSub { get; set; }

        [XmlElement("gJudic")]
        public GJudic GJudic { get; set; }

        [XmlElement("gGrContrat")]
        public List<GGrContrat> GGrContrats { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GGrContrat">Elemento</param>
        public void AddGGrContrat(GGrContrat GGrContrat)
        {
            if (GGrContrats == null)
            {
                GGrContrats = new List<GGrContrat>();
            }

            GGrContrats.Add(GGrContrat);
        }

        /// <summary>
        /// Retorna o elemento da lista GGrContrat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GGrContrat</returns>
        public GGrContrat GetGGrContrat(int index)
        {
            if ((GGrContrats?.Count ?? 0) == 0)
            {
                return default;
            };

            return GGrContrats[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GGrContrat
        /// </summary>
        public int GGrContratCount => (GGrContrats != null ? GGrContrats.Count : 0);
#endif

        [XmlElement("gMed")]
        public List<GMed> GMeds { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GMed">Elemento</param>
        public void AddGMed(GMed GMed)
        {
            if (GMeds == null)
            {
                GMeds = new List<GMed>();
            }

            GMeds.Add(GMed);
        }

        /// <summary>
        /// Retorna o elemento da lista GMed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GMed</returns>
        public GMed GetGMed(int index)
        {
            if ((GMeds?.Count ?? 0) == 0)
            {
                return default;
            };

            return GMeds[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GMed
        /// </summary>
        public int GMedCount => (GMeds != null ? GMeds.Count : 0);
#endif

        [XmlElement("gSCEE")]
        public GSCEE GSCEE { get; set; }

        [XmlElement("NFdet")]
        public List<NFdet> NFdets { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="NFdet">Elemento</param>
        public void AddNFdet(NFdet NFdet)
        {
            if (NFdets == null)
            {
                NFdets = new List<NFdet>();
            }

            NFdets.Add(NFdet);
        }

        /// <summary>
        /// Retorna o elemento da lista NFdet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFdet</returns>
        public NFdet GetNFdet(int index)
        {
            if ((NFdets?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFdets[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFdet
        /// </summary>
        public int NFdetCount => (NFdets != null ? NFdets.Count : 0);
#endif

        [XmlElement("total")]
        public TotalNF3e Total { get; set; }

        [XmlElement("gFat")]
        public GFat GFat { get; set; }

        [XmlElement("gANEEL")]
        public GANEEL GANEEL { get; set; }

        [XmlElement("autXML")]
        public List<AutXMLNF3e> AutXMLNF3es { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="AutXMLNF3e">Elemento</param>
        public void AddAutXMLNF3e(AutXMLNF3e AutXMLNF3e)
        {
            if (AutXMLNF3es == null)
            {
                AutXMLNF3es = new List<AutXMLNF3e>();
            }

            AutXMLNF3es.Add(AutXMLNF3e);
        }

        /// <summary>
        /// Retorna o elemento da lista AutXMLNF3e (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AutXMLNF3e</returns>
        public AutXMLNF3e GetAutXMLNF3e(int index)
        {
            if ((AutXMLNF3es?.Count ?? 0) == 0)
            {
                return default;
            };

            return AutXMLNF3es[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AutXMLNF3e
        /// </summary>
        public int AutXMLNF3eCount => (AutXMLNF3es != null ? AutXMLNF3es.Count : 0);
#endif

        [XmlElement("infAdic")]
        public InfAdicNF3e InfAdic { get; set; }

        [XmlElement("gRespTec")]
        public GRespTec GRespTec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.IdeNF3e")]
    [ComVisible(true)]
#endif
    public class IdeNF3e
    {
        [XmlElement("cUF")]
        public UFBrasil CUF { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("mod")]
        public string Mod { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nNF")]
        public string NNF { get; set; }

        [XmlElement("cNF")]
        public string CNF { get; set; }

        [XmlElement("cDV")]
        public string CDV { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString(""); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpEmis")]
        public TipoEmissao TipoEmissao { get; set; }

        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz { get; set; }

        [XmlElement("cMunFG")]
        public string CMunFG { get; set; }

        [XmlElement("finNF3e")]
        public string FinNF3e { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset DhCont { get; set; }
#endif

        [XmlElement("dhCont")]
        public string DhContField
        {
            get => DhCont.ToString(""); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DhCont = DateTime.Parse(value);
#else
            set => DhCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xJust")]
        public string XJust { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.EmitNF3e")]
    [ComVisible(true)]
#endif
    public class EmitNF3e
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmitNF3e EnderEmit { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.EnderEmitNF3e")]
    [ComVisible(true)]
#endif
    public class EnderEmitNF3e
    {
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public string UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.DestNF3e")]
    [ComVisible(true)]
#endif
    public class DestNF3e
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("indIEDest")]
        public IndicadorIEDestinatario IndIEDest { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("cNIS")]
        public string CNIS { get; set; }

        [XmlElement("xNomeAdicional")]
        public string XNomeAdicional { get; set; }

        [XmlElement("enderDest")]
        public EnderDestNF3e EnderDest { get; set; }


    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.EnderDestNF3e")]
    [ComVisible(true)]
#endif
    public class EnderDestNF3e
    {
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public string UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.Acessante")]
    [ComVisible(true)]
#endif
    public class Acessante
    {
        [XmlElement("idAcesso")]
        public string IdAcesso { get; set; }

        [XmlElement("idCodCliente")]
        public string IdCodCliente { get; set; }

        [XmlElement("tpAcesso")]
        public string TpAcesso { get; set; }

        [XmlElement("xNomeUC")]
        public string XNomeUC { get; set; }

        [XmlElement("tpClasse")]
        public string TpClasse { get; set; }

        [XmlElement("tpSubClasse")]
        public string TpSubClasse { get; set; }

        [XmlElement("tpFase")]
        public string TpFase { get; set; }

        [XmlElement("tpGrpTensao")]
        public string TpGrpTensao { get; set; }

        [XmlElement("tpModTar")]
        public string TpModTar { get; set; }

        [XmlElement("latGPS")]
        public string LatGPS { get; set; }

        [XmlElement("longGPS")]
        public string LongGPS { get; set; }

        [XmlElement("codRoteiroLeitura")]
        public string CodRoteiroLeitura { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GSub")]
    [ComVisible(true)]
#endif
    public class GSub
    {
        [XmlElement("chNF3e")]
        public string ChNF3e { get; set; }

        [XmlElement("motSub")]
        public string MotSub { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GJudic")]
    [ComVisible(true)]
#endif
    public class GJudic
    {
        [XmlElement("chNF3e")]
        public string ChNF3e { get; set; }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GGrContrat")]
    [ComVisible(true)]
#endif
    public class GGrContrat
    {
        [XmlAttribute(AttributeName = "nContrat", DataType = "token")]
        public string NContrat { get; set; }

        [XmlElement("tpGrContrat")]
        public string TpGrContrat { get; set; }

        [XmlElement("tpPosTar")]
        public string TpPosTar { get; set; }

        [XmlElement("qUnidContrat")]
        public int QUnidContrat { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GMed")]
    [ComVisible(true)]
#endif
    public class GMed
    {
        [XmlAttribute(AttributeName = "nMed", DataType = "token")]
        public string NMed { get; set; }

        [XmlElement("idMedidor")]
        public string IdMedidor { get; set; }

        [XmlElement("dMedAnt")]
        public int DMedAnt { get; set; }

        [XmlElement("dMedAtu")]
        public int DMedAtu { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GSCEE")]
    [ComVisible(true)]
#endif
    public class GSCEE
    {
        [XmlElement("tpPartComp")]
        public string TpPartComp { get; set; }

        [XmlElement("gConsumidor")]
        public List<GConsumidor> GConsumidores { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GConsumidor">Elemento</param>
        public void AddGConsumidor(GConsumidor GConsumidor)
        {
            if (GConsumidores == null)
            {
                GConsumidores = new List<GConsumidor>();
            }

            GConsumidores.Add(GConsumidor);
        }

        /// <summary>
        /// Retorna o elemento da lista GConsumidor (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GConsumidor</returns>
        public GConsumidor GetGConsumidor(int index)
        {
            if ((GConsumidores?.Count ?? 0) == 0)
            {
                return default;
            };

            return GConsumidores[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GConsumidor
        /// </summary>
        public int GConsumidorCount => (GConsumidores != null ? GConsumidores.Count : 0);
#endif

        [XmlElement("gSaldoCred")]
        public List<GSaldoCred> GSaldoCreds { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GSaldoCred">Elemento</param>
        public void AddGSaldoCred(GSaldoCred GSaldoCred)
        {
            if (GSaldoCreds == null)
            {
                GSaldoCreds = new List<GSaldoCred>();
            }

            GSaldoCreds.Add(GSaldoCred);
        }

        /// <summary>
        /// Retorna o elemento da lista GSaldoCred (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GSaldoCred</returns>
        public GSaldoCred GetGSaldoCred(int index)
        {
            if ((GSaldoCreds?.Count ?? 0) == 0)
            {
                return default;
            };

            return GSaldoCreds[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GSaldoCred
        /// </summary>
        public int GSaldoCredCount => (GSaldoCreds != null ? GSaldoCreds.Count : 0);
#endif

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GConsumidor")]
    [ComVisible(true)]
#endif
    public class GConsumidor
    {
        [XmlElement("idAcessGer")]
        public string IdAcessGer { get; set; }

        [XmlElement("vPotInst")]
        public string vPotInst { get; set; }

        [XmlElement("tpFonteEnergia")]
        public string TpFonteEnergia { get; set; }

        [XmlElement("enerAloc")]
        public string EnerAloc { get; set; }

        [XmlElement("tpPosTar")]
        public string TpPosTar { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GSaldoCred")]
    [ComVisible(true)]
#endif
    public class GSaldoCred
    {
        [XmlElement("tpPosTar")]
        public string TpPosTar { get; set; }

        [XmlElement("vSaldAnt")]
        public string VSaldAnt { get; set; }

        [XmlElement("vCredExpirado")]
        public string VCredExpirado { get; set; }

        [XmlElement("vSaldAtual")]
        public string VSaldAtual { get; set; }

        [XmlElement("vCredExpirar")]
        public string VCredExpirar { get; set; }

        [XmlElement("CompetExpirar")]
        public string CompetExpirar { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.NFdet")]
    [ComVisible(true)]
#endif
    public class NFdet
    {
        [XmlAttribute(AttributeName = "chNF3eAnt", DataType = "token")]
        public string ChNF3eAnt { get; set; }

        [XmlAttribute(AttributeName = "mod6HashAnt", DataType = "token")]
        public string Mod6HashAnt { get; set; }

        [XmlElement("det")]
        public List<DetNF3e> Dets { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="Det">Elemento</param>
        public void AddDetNF3e(DetNF3e Det)
        {
            if (Dets == null)
            {
                Dets = new List<DetNF3e>();
            }

            Dets.Add(Det);
        }

        /// <summary>
        /// Retorna o elemento da lista DetNF3e (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetNF3e</returns>
        public DetNF3e GetDetItem(int index)
        {
            if ((Dets?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dets[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetItemAnt
        /// </summary>
        public int DetCount => (Dets != null ? Dets.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.DetNF3e")]
    [ComVisible(true)]
#endif
    public class DetNF3e
    {
        [XmlAttribute(AttributeName = "nItem", DataType = "token")]
        public string NItem { get; set; }


        [XmlElement("gAjusteNF3eAnt")]
        public GAjusteNF3eAnt GAjusteNF3eAnt { get; set; }

        [XmlElement("detItemAnt")]
        public DetItemAnt DetItemAnt { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GAjusteNF3eAnt")]
    [ComVisible(true)]
#endif
    public class GAjusteNF3eAnt
    {
        [XmlElement("tpAjuste")]
        public string TpAjuste { get; set; }

        [XmlElement("motAjuste")]
        public string MotAjuste { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.DetItemAnt")]
    [ComVisible(true)]
#endif
    public class DetItemAnt
    {
        [XmlAttribute("nItemAnt")]
        public string NItemAnt { get; set; }

        [XmlElement("vItem")]
        public string VItem { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturada { get; set; }

        [XmlElement("vProd")]
        public string VProd { get; set; }

        [XmlElement("cClass")]
        public string CClass { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }

        [XmlElement("vBCST")]
        public string VBCST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPST { get; set; }

        [XmlElement("vPIS")]
        public string VPIS { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfet { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINS { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfet { get; set; }

        [XmlElement("retTrib")]
        public RetTribNF3e RetTribNF3e { get; set; }

        [XmlElement("indDevolucao")]
        public int IndDevolucao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.RetTribNF3e")]
    [ComVisible(true)]
#endif
    public class RetTribNF3e
    {
        [XmlIgnore]
        public double VRetPIS { get; set; }

        [XmlElement("vRetPIS")]
        public string VRetPISField
        {
            get => VRetPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCOFINS { get; set; }

        [XmlElement("vRetCofins")]
        public string VRetCOFINSField
        {
            get => VRetCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCSLL { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLLField
        {
            get => VRetCSLL.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCSLL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCIRRF { get; set; }

        [XmlElement("vBCIRRF")]
        public string VBCIRRFField
        {
            get => VBCIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIRRF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIRRF { get; set; }

        [XmlElement("vIRRF")]
        public string VIRRFField
        {
            get => VIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIRRF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.TotalNF3e")]
    [ComVisible(true)]
#endif
    public class TotalNF3e
    {
        [XmlElement("vProd")]
        public string VProd { get; set; }

        [XmlElement("ICMSTot")]
        public ICMSTotNF3e ICMSTot { get; set; }

        [XmlElement("vRetTribTot")]
        public VRetTribTot VRetTribTot { get; set; }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINSEfet { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfetField
        {
            get => VCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPISEfet { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfetField
        {
            get => VPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VPISEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VNF { get; set; }

        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.ICMSTotNF3e")]
    [ComVisible(true)]
#endif
    public class ICMSTotNF3e
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCST { get; set; }
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VST { get; set; }
        [XmlElement("vST")]
        public string VSTField
        {
            get => VST.ToString("F2", CultureInfo.InvariantCulture);
            set => VST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.VRetTribTot")]
    [ComVisible(true)]
#endif
    public class VRetTribTot
    {
        [XmlIgnore]
        public double VRetPIS { get; set; }

        [XmlElement("vRetPIS")]
        public string VRetPISField
        {
            get => VRetPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCOFINS { get; set; }

        [XmlElement("vRetCofins")]
        public string VRetCOFINSField
        {
            get => VRetCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCSLL { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLLField
        {
            get => VRetCSLL.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCSLL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIRRF { get; set; }

        [XmlElement("vIRRF")]
        public string VIRRFField
        {
            get => VIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIRRF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GFAT")]
    [ComVisible(true)]
#endif
    public class GFat
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DVencFat { get; set; }
#else
        public DateTimeOffset DVencFat { get; set; }
#endif

        [XmlElement("dVencFat")]
        public string DVencFatField
        {
            get => DVencFat.ToString(""); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DVencFat = DateTime.Parse(value);
#else
            set => DVencFat = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DApresFat  { get; set; }
#else
        public DateTimeOffset DApresFat { get; set; }
#endif

        [XmlElement("dApresFat")]
        public string DApresFatField
        {
            get => DApresFat.ToString(""); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DApresFat  = DateTime.Parse(value);
#else
            set => DApresFat = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DProxLeitura  { get; set; }
#else
        public DateTimeOffset DProxLeitura { get; set; }
#endif

        [XmlElement("dProxLeitura")]
        public string DProxLeituraField
        {
            get => DProxLeitura.ToString(""); //yyyy-MM-ddTHH:mm:sszzz
#if INTEROP
            set => DProxLeitura  = DateTime.Parse(value);
#else
            set => DProxLeitura = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nFat")]
        public string NFat { get; set; }

        [XmlElement("codBarras")]
        public string CodBarras { get; set; }

        [XmlElement("codDebAuto")]
        public string CodDebAuto { get; set; }

        [XmlElement("enderCorresp")]
        public EnderNF3e EnderCorresp { get; set; }

        [XmlElement("gPIX")]
        public GPix GPix { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.EnderNF3e")]
    [ComVisible(true)]
#endif
    public class EnderNF3e
    {
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public string UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GPix")]
    [ComVisible(true)]
#endif
    public class GPix
    {
        [XmlElement("urlQRCodePIX")]
        public string UrlQRCodePIX { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GANEEL")]
    [ComVisible(true)]
#endif
    public class GANEEL
    {
        [XmlElement("gHistFat")]
        public List<GHistFat> GHistFats { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GHistFat">Elemento</param>
        public void AddGHistFat(GHistFat GHistFat)
        {
            if (GHistFats == null)
            {
                GHistFats = new List<GHistFat>();
            }

            GHistFats.Add(GHistFat);
        }

        /// <summary>
        /// Retorna o elemento da lista GHistFat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GHistFat</returns>
        public GHistFat GetGHistFat(int index)
        {
            if ((GHistFats?.Count ?? 0) == 0)
            {
                return default;
            };

            return GHistFats[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GHistFat
        /// </summary>
        public int GHistFatCount => (GHistFats != null ? GHistFats.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.AutXMLNF3e")]
    [ComVisible(true)]
#endif
    public class AutXMLNF3e
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GHistFat")]
    [ComVisible(true)]
#endif
    public class GHistFat
    {
        [XmlElement("xGrandFat")]
        public string XGrandFat { get; set; }

        [XmlElement("gGrandFat")]
        public List<GGrandFat> GGrandFats { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GGrandFat">Elemento</param>
        public void AddGGrandFat(GGrandFat GGrandFat)
        {
            if (GGrandFats == null)
            {
                GGrandFats = new List<GGrandFat>();
            }

            GGrandFats.Add(GGrandFat);
        }

        /// <summary>
        /// Retorna o elemento da lista GGrandFat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GGrandFat</returns>
        public GGrandFat GetGGrandFat(int index)
        {
            if ((GGrandFats?.Count ?? 0) == 0)
            {
                return default;
            };

            return GGrandFats[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GGrandFat
        /// </summary>
        public int GGrandFatCount => (GGrandFats != null ? GGrandFats.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GGrandFat")]
    [ComVisible(true)]
#endif
    public class GGrandFat
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlIgnore]
        public double VFat { get; set; }

        [XmlElement("vFat")]
        public string VFatField
        {
            get => VFat.ToString("F2", CultureInfo.InvariantCulture);
            set => VFat = Converter.ToDouble(value);
        }

        [XmlElement("uMed")]
        public string UMed { get; set; }

        [XmlElement("qtdDias")]
        public int QtdDias { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.InfAdicNF3e")]
    [ComVisible(true)]
#endif
    public class InfAdicNF3e
    {
        private string InfAdFiscoField;
        private string InfCplField;

        [XmlElement("infAdFisco")]
        public string InfAdFisco
        {
            get => InfAdFiscoField;
            set => InfAdFiscoField = value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(2000).Trim();
        }

        [XmlElement("infCpl")]
        public List<string> InfCpls { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="InfCpl">Elemento</param>
        public void AddInfCpl(string InfCpl)
        {
            if (InfCpls == null)
            {
                InfCpls = new List<string>();
            }

            InfCpls.Add(InfCpl);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCpl (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCpl</returns>
        public string GetInfCpl(int index)
        {
            if ((InfCpls?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCpls[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCpl
        /// </summary>
        public int InfCplCount => (InfCpls != null ? InfCpls.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.GRespTec")]
    [ComVisible(true)]
#endif
    public class GRespTec
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xContato")]
        public string XContato { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("idCSRT")]
        public string IdCSRT { get; set; }

        [XmlIgnore]
        public string HashCSRTField { get; set; }
        /// <summary>
        /// Você pode informar o conteúdo já convertido para Sha1Hash + Base64, ou pode informar somente a concatenação do CSRT + Chave de Acesso que a DLL já converte para Sha1Hash + Base64
        /// </summary>
        [XmlElement("hashCSRT")]
        public string HashCSRT { get; set; }
        //{

        //    get
        //    {
        //        if (string.IsNullOrWhiteSpace(HashCSRTField) || Converter.IsSHA1Base64(HashCSRTField))
        //        {
        //            return HashCSRTField;
        //        }
        //        else
        //        {
        //            return Converter.CalculateSHA1Hash(HashCSRTField);
        //        }
        //    }
        //    set => HashCSRTField = value;
        //}
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e.InfNF3eSupl")]
    [ComVisible(true)]
#endif
    public class InfNF3eSupl
    {
        [XmlElement("qrCodNF3e")]
        public string QrCodNF3e { get; set; }
    }

    #endregion

}
