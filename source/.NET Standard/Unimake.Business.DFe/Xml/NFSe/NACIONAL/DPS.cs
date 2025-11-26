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

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    internal static class Nfse
    {
        public const string Ns = "http://www.sped.fazenda.gov.br/nfse";
    }

    /// <summary>
    /// Emissão (Gerar) de NFS-e – Padrão Nacional (esqueleto).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.DPS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = Nfse.Ns)]
    [XmlRoot("DPS", Namespace = Nfse.Ns, IsNullable = false)]
    public class DPS : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de geração da NFS-e.
        /// </summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e a ser gerada.
        /// </summary>
        [XmlElement("infDPS", Namespace = Nfse.Ns)]
        public infDPS infDPS { get; set; } = new infDPS();

        /// <summary>
        /// Assinatura digital.
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.infDPS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infDPS", Namespace = Nfse.Ns)]
    public class infDPS
    {
        /// <summary>
        /// Id da NFS-e a ser gerada.
        /// </summary>
        [XmlAttribute("Id", DataType = "token")]
        public string Id { get; set; }

        /// <summary>
        /// Tipo de ambiente: Produção ou Homologação
        /// </summary>
        [XmlElement("tpAmb", Namespace = Nfse.Ns)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Versão do aplicativo que está emitindo a NFS-e (informado pelo contribuinte).
        /// </summary>
        [XmlElement("verAplic", Namespace = Nfse.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Número do equipamento emissor do DPS ou série do DPS
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do DPS
        /// </summary>
        [XmlElement("nDPS")]
        public int NDPS { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DCompet { get; set; }
#else
public DateTimeOffset DCompet { get; set; }
#endif
        /// <summary>
        /// Data em que se iniciou a prestação do serviço
        /// </summary>
        [XmlElement("dCompet")]
        public string DCompetField
        {
#if INTEROP
            get => DCompet.ToString("yyyy-MM-dd");
            set => DCompet = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DCompet.ToString("yyyy-MM-dd");
            set => DCompet = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>
        /// Emitente da DPS
        /// </summary>
        [XmlElement("tpEmit")]
        public TipoEmitenteNFSe TpEmit { get; set; }

        /// <summary>
        /// Motivo da Emissão da DPS pelo Tomador/Intermediário
        /// </summary>
        [XmlElement("cMotivoEmisTI")]
        public string CMotivoEmisTI { get; set; }

        /// <summary>
        /// Chave de acesso rejeitada pelo Tomador/Intermediário
        /// </summary>
        [XmlElement("chNFSeRej")]
        public string ChNFSeRej { get; set; }

        /// <summary>
        /// Código de município de emissão da NFS-e
        /// </summary>
        [XmlElement("cLocEmi")]
        public int CLocEmi { get; set; }

        /// <summary>
        /// Grupo para NFSe Substituida
        /// </summary>
        [XmlElement("subst")]
        public Subst Subst { get; set; }

        /// <summary>
        /// Dados do Prestador de Serviço
        /// </summary>
        [XmlElement("prest")]
        public Prest Prest { get; set; }

        /// <summary>
        /// Dados do Tomador de Serviço
        /// </summary>
        [XmlElement("toma")]
        public Toma Toma { get; set; }

        /// <summary>
        /// Dados do Intermediário de Serviço
        /// </summary>
        [XmlElement("interm")]
        public Interm Interm { get; set; }

        /// <summary>
        /// Grupo de informações relativos ao serviço prestado
        /// </summary>
        [XmlElement("serv")]
        public Serv Serv { get; set; }

        /// <summary>
        /// Grupo de informações relativos aos valores do serviço prestado
        /// </summary>
        [XmlElement("valores")]
        public Valores Valores { get; set; }

        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCMotivoEmisTI() => !string.IsNullOrWhiteSpace(CMotivoEmisTI);

        public bool ShouldSerializeChNFSeRej() => !string.IsNullOrWhiteSpace(ChNFSeRej);
        #endregion Should Serialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Subst")]
    [ComVisible(true)]
#endif
    public class Subst
    {
        /// <summary>
        /// Chave de Acesso da NFS-e substituída
        /// </summary>
        [XmlElement("chSubstda")]
        public string chSubstda { get; set; }

        /// <summary>
        /// Código do motivo da substituição
        /// </summary>
        [XmlElement("cMotivo")]
        public CMotivoSubs CMotivo { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Prest")]
    [ComVisible(true)]
#endif
    public class Prest
    {

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("NIF")]
        public string NIF { get; set; }

        [XmlElement("cNaoNIF")]
        public string CNaoNIF { get; set; }

        [XmlElement("CAEPF")]
        public int CAEPF { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("end")]
        public End End { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        [XmlElement("regTrib")]
        public RegTrib RegTrib { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeNIF() => !string.IsNullOrWhiteSpace(NIF);
        public bool ShouldSerializeCNaoNIF() => !string.IsNullOrWhiteSpace(CNaoNIF);
        public bool ShouldSerializeCAEPF() => CAEPF > 0;
        public bool ShouldSerializexNome() => !string.IsNullOrWhiteSpace(XNome);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        public void ValidarDocUnico()
        {
            var count = 0;
            if (!string.IsNullOrWhiteSpace(CNPJ))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(CPF))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(NIF))
            {
                count++;
            }
            if (count != 1)
            {
                throw new Exception("Informe exatamente um: CNPJ, CPF, NIF ou cNaoNIF");
            }
        }

        #endregion Should Serialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.End")]
    [ComVisible(true)]
#endif
    public class End
    {
        [XmlElement("endNac")]
        public EndNac EndNac { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        #region Should Serialize
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public void ValidarEndereco()
        {
            if ((EndNac == null && EndExt == null) || (EndNac != null && EndExt != null))
            {
                throw new Exception("Informe exatamente um endereço: endNac OU endExt");
            }
        }
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.EndNac")]
    [ComVisible(true)]
#endif
    public class EndNac
    {
        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.EndExt")]
    [ComVisible(true)]
#endif
    public class EndExt
    {
        [XmlElement("cPais")]
        public string CPais { get; set; }

        [XmlElement("cEndPost")]
        public string CEndPost { get; set; }

        [XmlElement("xCidade")]
        public string XCidade { get; set; }

        [XmlElement("xEstProvReg")]
        public string XEstProvReg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.RegTrib")]
    [ComVisible(true)]
#endif
    public class RegTrib
    {
        [XmlElement("opSimpNac")]
        public OptSimplesNacional OpSimpNac { get; set; }

        [XmlElement("regApTribSN")]
        public RegApTribSN? RegApTribSN { get; set; }

        [XmlElement("regEspTrib")]
        public RegEspTrib RegEspTrib { get; set; }

        #region Should Serialize
        public bool ShouldSerializeRegApTribSN() => RegApTribSN.HasValue;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Toma")]
    [ComVisible(true)]
#endif
    public class Toma
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("NIF")]
        public string NIF { get; set; }

        [XmlElement("cNaoNIF")]
        public string CNaoNIF { get; set; }

        [XmlElement("CAEPF")]
        public int CAEPF { get; set; }

        [XmlElement("IM")]
        public int IM { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("end")]
        public End End { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeNIF() => !string.IsNullOrWhiteSpace(NIF);
        public bool ShouldSerializeCNaoNIF() => !string.IsNullOrWhiteSpace(CNaoNIF);
        public bool ShouldSerializeCAEPF() => CAEPF > 0;
        public bool ShouldSerializexNome() => !string.IsNullOrWhiteSpace(XNome);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);
        public bool ShouldSerializeIM() => IM > 0;

        public void ValidarDocUnico()
        {
            var count = 0;
            if (!string.IsNullOrWhiteSpace(CNPJ))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(CPF))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(NIF))
            {
                count++;
            }
            if (count != 1)
            {
                throw new Exception("Informe exatamente um: CNPJ, CPF, NIF ou cNaoNIF");
            }
        }

        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Interm")]
    [ComVisible(true)]
#endif
    public class Interm : Toma { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Serv")]
    [ComVisible(true)]
#endif
    public class Serv
    {
        [XmlElement("locPrest")]
        public LocPrest LocPrest { get; set; }

        [XmlElement("cServ")]
        public CServ CServ { get; set; }

        [XmlElement("comExt")]
        public ComExt ComExt { get; set; }

        [XmlElement("lsadppu")]
        public LSADPPU LSADPPU { get; set; }

        [XmlElement("obra")]
        public Obra Obra { get; set; }

        [XmlElement("atvEvento")]
        public AtvEvento AtvEvento { get; set; }

        [XmlElement("explRod")]
        public ExplRod ExplRod { get; set; }

        [XmlElement("infoCompl")]
        public InfoCompl InfoCompl { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.LocPrest")]
    [ComVisible(true)]
#endif
    public class LocPrest
    {
        [XmlElement("cLocPrestacao")]
        public int CLocPrestacao { get; set; }

        [XmlElement("cPaisPrestacao")]
        public string CPaisPrestacao { get; set; }

        #region ShouldSerialize
        public bool ShouldSerializeCLocPrestacao() => CLocPrestacao > 0;
        public bool ShouldSerializeCPaisPrestacao() => !string.IsNullOrWhiteSpace(CPaisPrestacao);
        public void ValidarLocalPrestacao()
        {
            if ((CLocPrestacao == 0 && string.IsNullOrWhiteSpace(CPaisPrestacao)) || (CLocPrestacao > 0 && !string.IsNullOrWhiteSpace(CPaisPrestacao)))
            {
                throw new Exception("Informe exatamente um local de prestação: cLocPrestacao OU cPaisPrestacao");
            }
        }
        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.CServ")]
    [ComVisible(true)]
#endif
    public class CServ
    {
        [XmlElement("cTribNac")]
        public string CTribNac { get; set; }

        [XmlElement("cTribMun")]
        public int CTribMun { get; set; }

        [XmlElement("xDescServ")]
        public string XDescServ { get; set; }

        [XmlElement("cNBS")]
        public int CNBS { get; set; }

        [XmlElement("cIntContrib")]
        public string CIntContrib { get; set; }


        #region Should Serialize
        public bool ShouldSerializeCTribMun() => CTribMun > 0;
        public bool ShouldSerializeCIntContrib() => !string.IsNullOrWhiteSpace(CIntContrib);
        public bool ShouldSerializeCNBS() => CNBS > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ComExt")]
    [ComVisible(true)]
#endif
    public class ComExt
    {
        [XmlElement("mdPrestacao")]
        public ModoPrestacaoNFSe MdPrestacao { get; set; }

        [XmlElement("vincPrest")]
        public VinculoEntrePartes VincPrest { get; set; }

        [XmlElement("tpMoeda")]
        public string TpMoeda { get; set; }

        [XmlIgnore]
        public double vServMoeda { get; set; }

        [XmlElement("vServMoeda")]
        public string vServMoedaField
        {
            get => vServMoeda.ToString("F2", CultureInfo.InvariantCulture);
            set => vServMoeda = Converter.ToDouble(value);
        }

        [XmlElement("mecAFComexP")]
        public MecAFComexP MecAFComexP { get; set; }

        [XmlElement("mecAFComexT")]
        public MecAFComexT MecAFComexT { get; set; }

        [XmlElement("movTempBens")]
        public MovTempBens MovTempBens { get; set; }

        [XmlElement("nDI")]
        public int NDI { get; set; }

        [XmlElement("nRE")]
        public int NRE { get; set; }

        [XmlElement("mDic")]
        public int MDic { get; set; }

        #region Should Serialize
        public bool ShouldSerializeNDI() => NDI > 0;
        public bool ShouldSerializeNRE() => NRE > 0;
        public bool ShouldSerializeMDic() => MDic > 0;
        #endregion Should Serialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.LSAPPU")]
    [ComVisible(true)]
#endif
    public class LSADPPU
    {
        [XmlElement("categ")]
        public int Categ { get; set; }

        [XmlElement("objeto")]
        public int Objeto { get; set; }

        [XmlElement("extensao")]
        public int Extensao { get; set; }

        [XmlElement("nPostes")]
        public int NPostes { get; set; }

        #region Should Serialize
        public bool ShouldSerializeExtensao() => Extensao > 0;
        public bool ShouldSerializeNPostes() => NPostes > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Obra")]
    [ComVisible(true)]
#endif
    public class Obra
    {
        [XmlElement("cObra")]
        public int CObra { get; set; }

        [XmlElement("cCIB")]
        public int CCIB { get; set; }

        [XmlElement("inscImobFisc")]
        public int InscImobFisc { get; set; }

        [XmlElement("end")]
        public ServEnd End { get; set; }

        #region Should Serialize
        public bool ShouldSerializeInscImobFisc() => InscImobFisc > 0;
        public bool ShouldSerializeCObra() => CObra > 0;

        public void ValidarEscolhaCObraCCIBEnd()
        {
            int count = 0;
            if (CObra > 0) count++;
            if (CCIB > 0) count++;
            if (End != null) count++;
            if (count != 1)
                throw new Exception("Obra: informe exatamente um dos campos: cObra, cCIB ou end.");
        }
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.AtvEvento")]
    [ComVisible(true)]
#endif
    public class AtvEvento
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtIni { get; set; }
#else
public DateTimeOffset DtIni { get; set; }
#endif
        /// <summary>
        /// Data em que se iniciou a prestação do serviço
        /// </summary>
        [XmlElement("dtIni")]
        public string DtIniField
        {
#if INTEROP
            get => DtIni.ToString("yyyy-MM-dd");
            set => DtIni = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DtIni.ToString("yyyy-MM-dd");
            set => DtIni = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtFim { get; set; }
#else
public DateTimeOffset DtFim { get; set; }
#endif
        /// <summary>
        /// Data em que se iniciou a prestação do serviço
        /// </summary>
        [XmlElement("dtFim")]
        public string DtFimField
        {
#if INTEROP
            get => DtFim.ToString("yyyy-MM-dd");
            set => DtFim = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DtFim.ToString("yyyy-MM-dd");
            set => DtFim = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        [XmlElement("idAtvEvt")]
        public int IdAtvEvt { get; set; }

        [XmlElement("end")]
        public ServEnd End { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ServEnd")]
    [ComVisible(true)]
#endif
    public class ServEnd
    {
        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public void ValidarCEPEndExt()
        {
            if ((CEP == null && EndExt == null) || (CEP != null && EndExt != null))
            {
                throw new Exception("Informe exatamente um endereço: CEP OU endExt");
            }
        }
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ExplRod")]
    [ComVisible(true)]
#endif
    public class ExplRod
    {
        [XmlElement("categVeic")]
        public CategVeic CategVeic { get; set; }

        [XmlElement("nEixos")]
        public int NEixos { get; set; }

        [XmlElement("rodagem")]
        public string Rodagem { get; set; }

        [XmlElement("sentido")]
        public string Sentido { get; set; }

        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("codAcessoPed")]
        public string CodAcessoPed { get; set; }

        [XmlElement("codContrato")]
        public string CodContrato { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.InfoCompl")]
    [ComVisible(true)]
#endif
    public class InfoCompl
    {
        [XmlElement("idDocTec")]
        public int IdDocTec { get; set; }

        [XmlElement("docRef")]
        public string DocRef { get; set; }

        [XmlElement("xPed")]  // Campo novo
        public string XPed { get; set; }

        [XmlElement("gItemPed")]  // Campo novo
        public GItemPed GItemPed { get; set; }

        [XmlElement("xInfComp")]  // Renomear de xInfoComp para xInfComp
        public string XInfComp { get; set; }

        #region Should Serialize
        public bool ShouldSerializeIdDocTec() => IdDocTec > 0;
        public bool ShouldSerializeDocRef() => !string.IsNullOrWhiteSpace(DocRef);
        public bool ShouldSerializeXInfComp() => !string.IsNullOrWhiteSpace(XInfComp);
        public bool ShouldSerializeXPed() => !string.IsNullOrWhiteSpace(XPed);
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GItemPed")]
    [ComVisible(true)]
#endif
    public class GItemPed
    {
        [XmlElement("xItemPed")]
        public string XItemPed { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Valores")]
    [ComVisible(true)]
#endif
    public class Valores
    {
        [XmlElement("vServPrest")]
        public VServPrest VServPrest { get; set; }

        [XmlElement("vDescCondIncond")]
        public VDescCondIncond vDescCondIncond { get; set; }

        [XmlElement("vDedRed")]
        public VDedRed VDedRed { get; set; }

        [XmlElement("trib")]
        public Trib Trib { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.VServPrest")]
    [ComVisible(true)]
#endif
    public class VServPrest
    {
        [XmlIgnore]
        public double VReceb { get; set; }

        [XmlElement("vReceb")]
        public string VRecebField
        {
            get => VReceb.ToString("F2", CultureInfo.InvariantCulture);
            set => VReceb = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VServ { get; set; }

        [XmlElement("vServ")]
        public string VServField
        {
            get => VServ.ToString("F2", CultureInfo.InvariantCulture);
            set => VServ = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeVRecebField() => VReceb > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.VDescCondIncond")]
    [ComVisible(true)]
#endif
    public class VDescCondIncond
    {
        [XmlIgnore]
        public double VDescIncond { get; set; }

        [XmlElement("vDescIncond")]
        public string VDescIncondField
        {
            get => VDescIncond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescIncond = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDescCond { get; set; }

        [XmlElement("vDescCond")]
        public string VDescCondField
        {
            get => VDescCond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescCond = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeVDescIncondField() => VDescIncond > 0;
        public bool ShouldSerializeVDescCondField() => VDescCond > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.VDedRed")]
    [ComVisible(true)]
#endif
    public class VDedRed
    {
        [XmlIgnore]
        public double PDR { get; set; }

        [XmlElement("pDR")]
        public string PDRField
        {
            get => PDR.ToString("F2", CultureInfo.InvariantCulture);
            set => PDR = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDR { get; set; }

        [XmlElement("vDR")]
        public string VDRField
        {
            get => VDR.ToString("F2", CultureInfo.InvariantCulture);
            set => VDR = Converter.ToDouble(value);
        }

        [XmlElement("documentos")]
        public Documentos Documentos { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Documentos")]
    [ComVisible(true)]
#endif
    public class Documentos
    {
        [XmlElement("docDedRed")]
        public DocDedRed DocDedRed { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.DocDedRed")]
    [ComVisible(true)]
#endif
    public class DocDedRed
    {
        [XmlElement("chNFSe")]
        public string ChNFSe { get; set; }

        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        [XmlElement("NFSeMun")]
        public NFSeMun NFSeMun { get; set; }

        [XmlElement("NFNFS")]
        public NFNFS NFNFS { get; set; }

        [XmlElement("nDocFisc")]
        public string NDocFisc { get; set; }

        [XmlElement("nDoc")]
        public int NDoc { get; set; }

        [XmlElement("tpDedRed")]
        public TipoDeducaoReducao TpDedRed { get; set; }

        [XmlElement("xDescOutDed")]
        public string XDescOutDed { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEmiDoc { get; set; }
#else
public DateTimeOffset DtEmiDoc { get; set; }
#endif
        [XmlElement("dtEmiDoc")]
        public string DtEmiDocField
        {
#if INTEROP
            get => DtEmiDoc.ToString("yyyy-MM-dd");
            set => DtEmiDoc = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DtEmiDoc.ToString("yyyy-MM-dd");
            set => DtEmiDoc = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        [XmlIgnore]
        public double VDedutivelRedutivel { get; set; }

        [XmlElement("vDedutivelRedutivel")]
        public string VDedutivelRedutivelField
        {
            get => VDedutivelRedutivel.ToString("F2", CultureInfo.InvariantCulture);
            set => VDedutivelRedutivel = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDeducaoReducao { get; set; }

        [XmlElement("vDeducaoReducao")]
        public string VDeducaoReducaoField
        {
            get => VDeducaoReducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VDeducaoReducao = Converter.ToDouble(value);
        }

        [XmlElement("fornec")]
        public Fornec Fornec { get; set; }

        #region Should Serialize
        public bool ShouldSerializeChNFSe() => !string.IsNullOrWhiteSpace(ChNFSe);
        public bool ShouldSerializeChNFe() => !string.IsNullOrWhiteSpace(ChNFe);
        public bool ShouldSerializeXDescOutDed() => !string.IsNullOrWhiteSpace(XDescOutDed);

        public void ValidarChaves()
        {
            if ((string.IsNullOrWhiteSpace(ChNFSe) && string.IsNullOrWhiteSpace(ChNFe)) || (!string.IsNullOrWhiteSpace(ChNFSe) && !string.IsNullOrWhiteSpace(ChNFe)))
            {
                throw new Exception("Informe exatamente uma chave: chNFSe OU chNFe");
            }
        }
        public void ValidarEscolhaNota()
        {
            int count = 0;
            if (NFSeMun != null) count++;
            if (NFNFS != null) count++;
            if (count != 1)
                throw new Exception("DocDedRed: informe exatamente um dos grupos: NFSeMun ou NFNFS");
        }
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.NFSeMun")]
    [ComVisible(true)]
#endif
    public class NFSeMun
    {
        [XmlElement("cMunNFSeMun")]
        public int CMunNFSeMun { get; set; }

        [XmlElement("nNFSeMun")]
        public int NNFSeMun { get; set; }

        [XmlElement("cVerifNFSeMun")]
        public int CVerifNFSeMun { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.NFNFS")]
    [ComVisible(true)]
#endif
    public class NFNFS
    {
        [XmlElement("nNFS")]
        public int NNFS { get; set; }

        [XmlElement("modNFS")]
        public int ModNFS { get; set; }

        [XmlElement("serieNFS")]
        public int SerieNFS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Fornec")]
    [ComVisible(true)]
#endif
    public class Fornec : Toma { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Trib")]
    [ComVisible(true)]
#endif
    public class Trib
    {
        [XmlElement("tribMun")]
        public TribMun TribMun { get; set; }

        [XmlElement("tribFed")]
        public TribFed TribFed { get; set; }

        [XmlElement("totTrib")]
        public TotTrib TotTrib { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.TribMun")]
    [ComVisible(true)]
#endif
    public class TribMun
    {
        [XmlElement("tribISSQN")]
        public TribISSQN TribISSQN { get; set; }

        [XmlElement("cPaisResult")]
        public string CPaisResult { get; set; }

        [XmlElement("BM")]
        public BM BM { get; set; }

        [XmlElement("exigSusp")]
        public ExigSusp ExigSusp { get; set; }

        [XmlElement("tpImunidade")]
        public TipoImunidadeISSQN? TpImunidade { get; set; }

        [XmlElement("tpRetISSQN")]
        public TipoRetencaoISSQN TpRetISSQN { get; set; }

        [XmlIgnore]
        public double PAliq { get; set; }

        [XmlElement("pAliq")]
        public string PAliqField
        {
            get => PAliq.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliq = Converter.ToDouble(value);
        }


        #region Should Serialize
        public bool ShouldSerializeCPaisResult() => !string.IsNullOrWhiteSpace(CPaisResult);
        public bool ShouldSerializeTpImunidade() => TpImunidade.HasValue;
        public bool ShouldSerializePAliqField() => PAliq > 0;

        #endregion Should Serialize


    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.BM")]
    [ComVisible(true)]
#endif
    public class BM
    {
        [XmlElement("tpBM")]
        public int TpBM { get; set; }

        [XmlElement("nBM")]
        public int NBM { get; set; }

        [XmlIgnore]
        public double VRedBCBM { get; set; }

        [XmlElement("vRedBCBM")]
        public string VRedBCBMField
        {
            get => VRedBCBM.ToString("F2", CultureInfo.InvariantCulture);
            set => VRedBCBM = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBCBM { get; set; }

        [XmlElement("pRedBCBM")]
        public string PRedBCBMField
        {
            get => PRedBCBM.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBCBM = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ExigSusp")]
    [ComVisible(true)]
#endif
    public class ExigSusp
    {
        [XmlElement("tpSusp")]
        public TipoExigibilidadeSuspensa TpSusp { get; set; }

        [XmlElement("nProcesso")]
        public int NProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.TribFed")]
    [ComVisible(true)]
#endif
    public class TribFed
    {
        [XmlElement("piscofins")]
        public PISCOFINS PISCOFINS { get; set; }

        [XmlIgnore]
        public double VRetCP { get; set; }

        [XmlElement("vRetCP")]
        public string VRetCPField
        {
            get => VRetCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetIRRF { get; set; }

        [XmlElement("vRetIRRF")]
        public string VRetIRRFField
        {
            get => VRetIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetIRRF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCSLL { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLLField
        {
            get => VRetCSLL.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCSLL = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeVRetCPField() => VRetCP > 0;
        public bool ShouldSerializeVRetIRRFField() => VRetIRRF > 0;
        public bool ShouldSerializeVRetCSLLField() => VRetCSLL > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.PISCOFINS")]
    [ComVisible(true)]
#endif
    public class PISCOFINS
    {
        [XmlElement("CST")]
        public CSTPisCofins CST { get; set; }

        [XmlIgnore]
        public double VBCPisCofins { get; set; }

        [XmlElement("vBCPisCofins")]
        public string VBCPisCofinsField
        {
            get => VBCPisCofins.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCPisCofins = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqPis { get; set; }

        [XmlElement("pAliqPis")]
        public string PAliqPisField
        {
            get => PAliqPis.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqPis = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPis { get; set; }

        [XmlElement("vPis")]
        public string VPisField
        {
            get => VPis.ToString("F2", CultureInfo.InvariantCulture);
            set => VPis = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCofins { get; set; }

        [XmlElement("vCofins")]
        public string VCofinsField
        {
            get => VCofins.ToString("F2", CultureInfo.InvariantCulture);
            set => VCofins = Converter.ToDouble(value);
        }

        [XmlElement("tpRetPisCofins")]
        public TipoRetencaoISSQN TpRetPisCofins { get; set; }

        #region Should Serialize
        public bool ShouldSerializeVBCPisCofinsField() => VBCPisCofins > 0;
        public bool ShouldSerializePAliqPisField() => PAliqPis > 0;
        public bool ShouldSerializeVPisField() => VPis > 0;
        public bool ShouldSerializeVCofinsField() => VCofins > 0;
        public bool ShouldSerializeTpRetPisCofins() => !string.IsNullOrWhiteSpace(TpRetPisCofins.ToString());
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.TotTrib")]
    [ComVisible(true)]
#endif
    public class TotTrib
    {
        [XmlElement("vTotTrib")]
        public VTotTrib VTotTrib { get; set; }

        [XmlElement("pTotTrib")]
        public PTotTrib PTotTrib { get; set; }

        [XmlElement("indTotTrib")]
        public int IndTotTrib { get; set; }

        [XmlIgnore]
        public double PTotTribSN { get; set; }

        [XmlElement("pTotTribSN")]
        public string PTotTribSNField
        {
            get => PTotTribSN.ToString("F2", CultureInfo.InvariantCulture);
            set => PTotTribSN = Converter.ToDouble(value);
        }


        #region Should Serialize
        public bool ShouldSerializeIndTotTrib() => IndTotTrib > 0;

        public bool ShouldSerializePTotTribSNField() => PTotTribSN > 0;
        public void ValidarTotaisTributacao()
        {
            if (VTotTrib == null && PTotTrib == null)
            {
                throw new Exception("Informe ao menos um dos grupos: vTotTrib ou pTotTrib");
            }
        }

        public void ValidarPTotTribSN()
        {
            if (IndTotTrib == 1 && PTotTribSN == 0)
            {
                throw new Exception("Quando indTotTrib for igual a 1, o campo pTotTribSN deve ser informado.");
            }
            if (IndTotTrib != 1 && PTotTribSN > 0)
            {
                throw new Exception("O campo pTotTribSN só deve ser informado quando indTotTrib for igual a 1.");
            }
        }
        #endregion Should Serialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.VTotTrib")]
    [ComVisible(true)]
#endif
    public class VTotTrib
    {
        [XmlIgnore]
        public double VTotTribFed { get; set; }

        [XmlElement("vTotTribFed")]
        public string VTotTribFedField
        {
            get => VTotTribFed.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTribFed = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotTribEst { get; set; }

        [XmlElement("vTotTribEst")]
        public string VTotTribEstField
        {
            get => VTotTribEst.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTribEst = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotTribMun { get; set; }

        [XmlElement("vTotTribMun")]
        public string VTotTribMunField
        {
            get => VTotTribMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTribMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.PTotTrib")]
    [ComVisible(true)]
#endif
    public class PTotTrib
    {
        [XmlIgnore]
        public double PTotTribFed { get; set; }

        [XmlElement("pTotTribFed")]
        public string PTotTribFedField
        {
            get => PTotTribFed.ToString("F2", CultureInfo.InvariantCulture);
            set => PTotTribFed = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PTotTribEst { get; set; }

        [XmlElement("pTotTribEst")]
        public string PTotTribEstField
        {
            get => PTotTribEst.ToString("F2", CultureInfo.InvariantCulture);
            set => PTotTribEst = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PTotTribMun { get; set; }

        [XmlElement("pTotTribMun")]
        public string PTotTribMunField
        {
            get => PTotTribMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PTotTribMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.IBSCBS")]
    [ComVisible(true)]
#endif
    public class IBSCBS
    {
        [XmlElement("finNFSe")]
        public int FinNFSe { get; set; }

        [XmlElement("indFinal")]
        public int IndFinal { get; set; }

        [XmlElement("cIndOp")]
        public string CIndOp { get; set; }

        [XmlElement("tpOper")]
        public TpOperacaoGov? TpOper { get; set; }

        [XmlElement("gRefNFSe")]
        public GRefNFSe GRefNFSe { get; set; }

        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamental? TpEnteGov { get; set; }

        [XmlElement("xTpEnteGov")]
        public string XTpEnteGov { get; set; }

        [XmlElement("indDest")]
        public int IndDest { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("imovel")]
        public Imovel Imovel { get; set; }

        [XmlElement("valores")]
        public ValoresValores Valores { get; set; }

        #region Should Serialize
        public bool ShouldSerializeTpOper() => TpOper.HasValue;
        public bool ShouldSerializeTpEnteGov() => TpEnteGov.HasValue;
        public bool ShouldSerializeXTpEnteGov() => !string.IsNullOrWhiteSpace(XTpEnteGov);
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GRefNFSe")]
    [ComVisible(true)]
#endif
    public class GRefNFSe
    {
        [XmlElement("refNFSe")]
        public List<string> RefNFSe { get; set; }

#if INTEROP
        /// <summary>
        /// Adiciona uma nova referência de NFSe à lista
        /// </summary>
        /// <param name="chaveNFSe">Chave da NFS-e referenciada</param>
        public void AddRefNFSe(string chaveNFSe)
        {
            if (RefNFSe == null)
            {
                RefNFSe = new List<string>();
            }
            RefNFSe.Add(chaveNFSe);
        }

        /// <summary>
        /// Retorna uma referência de NFSe da lista pelo index informado
        /// </summary>
        /// <param name="index">Index da lista a ser retornado</param>
        /// <returns>Chave da NFSe no index passado por parâmetro</returns>
        public string GetRefNFSe(int index)
        {
            if ((RefNFSe?.Count ?? 0) == 0)
            {
                return default;
            }
            return RefNFSe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista
        /// </summary>
        public int GetRefNFSeCount => (RefNFSe != null ? RefNFSe.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Dest")]
    [ComVisible(true)]
#endif
    public class Dest : Toma { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.Imovel")]
    [ComVisible(true)]
#endif
    public class Imovel
    {
        [XmlElement("inscImobFisc")]
        public int InscImobFisc { get; set; }

        [XmlElement("cCIB")]
        public int CCIB { get; set; }

        [XmlElement("end")]
        public ImovelEnd End { get; set; }

        #region Should Serialize
        public bool ShouldSerializeInscImobFisc() => InscImobFisc > 0;
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ImovelEnd")]
    [ComVisible(true)]
#endif
    public class ImovelEnd
    {
        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("endExt")]
        public EndExt EndExt { get; set; }

        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }


        #region Should Serialize
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public void ValidarEndereco()
        {
            if ((CEP == null && EndExt == null) || (CEP != null && EndExt != null))
            {
                throw new Exception("Informe exatamente um endereço: CEP OU endExt");
            }
        }
        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ValoresValores")]
    [ComVisible(true)]
#endif
    public class ValoresValores
    {
        [XmlElement("gReeRepRes")]
        public GReeRepRes GReeRepRes { get; set; }

        [XmlElement("trib")]
        public ValoresValoresTrib Trib { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GReeRepRes")]
    [ComVisible(true)]
#endif
    public class GReeRepRes
    {
        [XmlElement("documentos")]
        public List<GReeRepResDocumentos> GReeRepResDocumentos { get; set; }

#if INTEROP

        /// <summary>
        /// Adicona novo Documento a lista
        /// </summary>
        /// <param name="item">Documentos</param>
        public void AddGReeRepResDocumentos(GReeRepResDocumentos item)
        {
            if (GReeRepResDocumentos == null)
            {
                GReeRepResDocumentos = new List<GReeRepResDocumentos>();
            }
            GReeRepResDocumentos.Add(item);
        }

        /// <summary>
        /// Retorna um Documento da lista pelo index informado
        /// </summary>
        /// <param name="index">Index da lista a ser retornado</param>
        /// <returns>Conteúdo do index passado por parâmetro</returns>
        public GReeRepResDocumentos GetGReeRepResDocumentos(int index)
        {
            if ((GReeRepResDocumentos?.Count ?? 0) == 0)
            {
                return default;
            }
            return GReeRepResDocumentos[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos na lista
        /// </summary>
        public int GetGReeRepResDocumentosCount => (GReeRepResDocumentos != null ? GReeRepResDocumentos.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GReeRepResDocumentos")]
    [ComVisible(true)]
#endif
    public class GReeRepResDocumentos
    {
        [XmlElement("dfeNacional")]
        public DfeNacional DfeNacional { get; set; }

        [XmlElement("docFiscalOutro")]
        public DocFiscalOutro DocFiscalOutro { get; set; }

        [XmlElement("docOutro")]
        public DocOutro DocOutro { get; set; }

        [XmlElement("fornec")]
        public GReeRepResFornec GReeRepResFornec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.DfeNacional")]
    [ComVisible(true)]
#endif
    public class DfeNacional
    {
        [XmlElement("tipoChaveDFe")]
        public TipoChaveDFe TipoChaveDFe { get; set; }

        [XmlElement("xTipoChaveDFe")]
        public string XTipoChaveDFe { get; set; }

        [XmlElement("chaveDFe")]
        public string Chave { get; set; }

        #region Should Serialize
        public bool ShouldSerializeXTipoChaveDFe() => !string.IsNullOrWhiteSpace(XTipoChaveDFe);
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.DocFiscalOutro")]
    [ComVisible(true)]
#endif
    public class DocFiscalOutro
    {
        [XmlElement("cMunDocFiscal")]
        public int CMunDocFiscal { get; set; }

        [XmlElement("ndocFiscal")]
        public int NDocFiscal { get; set; }

        [XmlElement("xDocFiscal")]
        public string XDocFiscal { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.DocOutro")]
    [ComVisible(true)]
#endif
    public class DocOutro
    {
        [XmlElement("nDoc")]
        public int NDoc { get; set; }

        [XmlElement("xDoc")]
        public string XDoc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GReeRepResFornec")]
    [ComVisible(true)]
#endif
    public class GReeRepResFornec
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("NIF")]
        public string NIF { get; set; }

        [XmlElement("cNaoNIF")]
        public string CNaoNIF { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEmiDoc { get; set; }
#else
public DateTimeOffset DtEmiDoc { get; set; }
#endif
        /// <summary>
        /// Data em que se iniciou a prestação do serviço
        /// </summary>
        [XmlElement("dtEmiDoc")]
        public string DtEmiDocField
        {
#if INTEROP
            get => DtEmiDoc.ToString("yyyy-MM-dd");
            set => DtEmiDoc = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DtEmiDoc.ToString("yyyy-MM-dd");
            set => DtEmiDoc = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtCompDoc { get; set; }
#else
public DateTimeOffset DtCompDoc { get; set; }
#endif
        /// <summary>
        /// Data em que se iniciou a prestação do serviço
        /// </summary>
        [XmlElement("dtCompDoc")]
        public string DtCompDocField
        {
#if INTEROP
            get => DtCompDoc.ToString("yyyy-MM-dd");
            set => DtCompDoc = DateTime.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#else
            get => DtCompDoc.ToString("yyyy-MM-dd");
            set => DtCompDoc = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture);
#endif
        }

        [XmlElement("xTpReeRepRes")]
        public string XTpReeRepRes { get; set; }

        [XmlIgnore]
        public double VlrReeRepRes { get; set; }

        [XmlElement("vlrReeRepRes")]
        public string VlrReeRepResField
        {
            get => VlrReeRepRes.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrReeRepRes = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeNIF() => !string.IsNullOrWhiteSpace(NIF);
        public bool ShouldSerializeCNaoNIF() => !string.IsNullOrWhiteSpace(CNaoNIF);
        public bool ShouldSerializeXTpReeRepRes() => !string.IsNullOrWhiteSpace(XTpReeRepRes);
        public void ValidarDocUnico()
        {
            var count = 0;
            if (!string.IsNullOrWhiteSpace(CNPJ))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(CPF))
            {
                count++;
            }
            if (!string.IsNullOrWhiteSpace(NIF))
            {
                count++;
            }
            if (count != 1)
            {
                throw new Exception("Informe exatamente um: CNPJ, CPF, NIF ou cNaoNIF");
            }
        }

        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.ValoresValoresTrib")]
    [ComVisible(true)]
#endif
    public class ValoresValoresTrib
    {
        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GIBSCBS")]
    [ComVisible(true)]
#endif
    public class GIBSCBS
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCCredPres() => ! string.IsNullOrWhiteSpace(CCredPres);

        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GTribRegular")]
    [ComVisible(true)]
#endif
    public class GTribRegular
    {
        [XmlElement("CSTReg")]
        public string CSTReg { get; set; }

        [XmlElement("cClassTribReg")]
        public string CClassTribReg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GerarNfse.GDif")]
    [ComVisible(true)]
#endif
    public class GDif
    {
        [XmlIgnore]
        public double PDifUF { get; set; }

        [XmlElement("pDifUF")]
        public string PDifUFField
        {
            get => PDifUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PDifUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PDifMun { get; set; }

        [XmlElement("pDifMun")]
        public string PDifMunField
        {
            get => PDifMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PDifMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PDifCBS { get; set; }

        [XmlElement("pDifCBS")]
        public string PDifCBSField
        {
            get => PDifCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PDifCBS = Converter.ToDouble(value);
        }
    }
}
