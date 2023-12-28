#pragma warning disable CS1591

#if INTEROP
// The result of the expression is always the same since a value of this type is never equal to 'null'
#pragma warning disable CS0472 
#endif

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnviNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("enviNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class EnviNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("idLote")]
        public string IdLote { get; set; }

        [XmlElement("indSinc")]
        public SimNao IndSinc { get; set; }

        [XmlElement("NFe")]
        public List<NFe> NFe { get; set; }

        public override XmlDocument GerarXML()
        {
            var xmlDoc = base.GerarXML();

            foreach (var nodeEnvNFe in xmlDoc.GetElementsByTagName("enviNFe"))
            {
                var elemEnvNFe = (XmlElement)nodeEnvNFe;

                foreach (var nodeNFe in elemEnvNFe.GetElementsByTagName("NFe"))
                {
                    var elemNFe = (XmlElement)nodeNFe;

                    var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
                    elemNFe.SetAttribute("xmlns", attribute.Namespace);
                }
            }

            return xmlDoc;
        }

        /// <summary>
        /// Desserializar o XML EnviNFe no objeto EnviNFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML EnviNFe</param>
        /// <returns>Objeto do EnviNFe</returns>
        public EnviNFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EnviNFe>(doc);
        }

        /// <summary>
        /// Desserializar o XML EnviNFe no objeto EnviNFe
        /// </summary>
        /// <param name="xml">string do XML EnviNFe</param>
        /// <returns>Objeto da EnviNFe</returns>
        public EnviNFe LoadFromXML(string xml) => XMLUtility.Deserializar<EnviNFe>(xml);

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nfe">Elemento</param>
        public void AddNFe(NFe nfe)
        {
            if (NFe == null)
            {
                NFe = new List<NFe>();
            }

            NFe.Add(nfe);
        }

        /// <summary>
        /// Retorna o elemento da lista NFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFe</returns>
        public NFe GetNFe(int index)
        {
            if ((NFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFe
        /// </summary>
        public int GetNFeCount => (NFe != null ? NFe.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    [XmlRoot("NFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class NFe
    {
        [XmlElement("infNFe")]
        public List<InfNFe> InfNFe { get; set; }

        [XmlElement("infNFeSupl")]
        public InfNFeSupl InfNFeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNFe">Elemento</param>
        public void AddInfNFe(InfNFe infNFe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<InfNFe>();
            }

            InfNFe.Add(infNFe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFe</returns>
        public InfNFe GetInfNFe(int index)
        {
            if ((InfNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

#endif
        /// <summary>
        /// Deserializar o XML NFe no objeto NFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML NFe</param>
        /// <returns>Objeto da NFe</returns>
        public NFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<NFe>(doc);
        }

        /// <summary>
        /// Deserializar o XML NFe no objeto NFe
        /// </summary>
        /// <param name="xml">string do XML NFe</param>
        /// <returns>Objeto da NFe</returns>
        public NFe LoadFromXML(string xml) => XMLUtility.Deserializar<NFe>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfNFe
    {
        private string IdField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>
        /// Esta TAG é de uso exclusivo do FISCO, não precisa gerar nada, só temos ela para caso de alguma necessidade de desserialização.
        /// </summary>
        [XmlElement("avulsa")]
        public Avulsa Avulsa { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("retirada")]
        public Retirada Retirada { get; set; }

        [XmlElement("entrega")]
        public Entrega Entrega { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        [XmlElement("det")]
        public List<Det> Det { get; set; }

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("transp")]
        public Transp Transp { get; set; }

        [XmlElement("cobr")]
        public Cobr Cobr { get; set; }

        [XmlElement("pag")]
        public Pag Pag { get; set; }

        [XmlElement("infIntermed")]
        public InfIntermed InfIntermed { get; set; }

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("exporta")]
        public Exporta Exporta { get; set; }

        [XmlElement("compra")]
        public Compra Compra { get; set; }

        [XmlElement("cana")]
        public Cana Cana { get; set; }

        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        [XmlElement("infSolicNFF")]
        public InfSolicNFF InfSolicNFF { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id
        {
            get
            {
                IdField = "NFe" + Chave;
                return IdField;
            }
            set => IdField = value;
        }

        private string ChaveField;

        [XmlIgnore]
        public string Chave
        {
            get
            {
                ChaveField = ((int)Ide.CUF).ToString() +
                    Ide.DhEmi.ToString("yyMM") +
                    (string.IsNullOrWhiteSpace(Emit.CNPJ) ? Emit.CPF?.PadLeft(14, '0') : Emit.CNPJ.PadLeft(14, '0')) +
                    ((int)Ide.Mod).ToString().PadLeft(2, '0') +
                    Ide.Serie.ToString().PadLeft(3, '0') +
                    Ide.NNF.ToString().PadLeft(9, '0') +
                    ((int)Ide.TpEmis).ToString() +
                    Ide.CNF.PadLeft(8, '0');

                Ide.CDV = XMLUtility.CalcularDVChave(ChaveField);

                ChaveField += Ide.CDV.ToString();

                return ChaveField;
            }
            set => throw new Exception("Não é permitido atribuir valor para a propriedade Chave. Ela é calculada automaticamente.");
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="autXML">Elemento</param>
        public void AddAutXml(AutXML autXML)
        {
            if (AutXML == null)
            {
                AutXML = new List<AutXML>();
            }

            AutXML.Add(autXML);
        }

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="det">Elemento</param>
        public void AddDet(Det det)
        {
            if (Det == null)
            {
                Det = new List<Det>();
            }

            Det.Add(det);
        }

        /// <summary>
        /// Retorna o elemento da lista AutXML (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AutXML</returns>
        public AutXML GetAutXML(int index)
        {
            if ((AutXML?.Count ?? 0) == 0)
            {
                return default;
            };

            return AutXML[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AutXML
        /// </summary>
        public int GetAutXMLCount => (AutXML != null ? AutXML.Count : 0);

        /// <summary>
        /// Retorna o elemento da lista Det (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Det</returns>
        public Det GetDet(int index)
        {
            if ((Det?.Count ?? 0) == 0)
            {
                return default;
            };

            return Det[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Det
        /// </summary>
        public int GetDetCount => (Det != null ? Det.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Ide")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Ide
    {
        private string CNFField;
        private string _natOp = "";
        private string XJustField;

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cNF")]
        public string CNF
        {
            get
            {
                string retorno;
                if (string.IsNullOrWhiteSpace(CNFField))
                {
                    if (NNF < 0)
                    {
                        throw new Exception("Defina o conteúdo da TAG <nNF>, pois a mesma é utilizada como base para calcular o código numérico.");
                    }

                    retorno = XMLUtility.GerarCodigoNumerico(NNF).ToString("00000000");
                }
                else
                {
                    retorno = CNFField;
                }

                return retorno;
            }
            set => CNFField = value;
        }

        [XmlElement("natOp")]
        public string NatOp
        {
            get => _natOp;
            set => _natOp = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nNF")]
        public int NNF { get; set; }

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

        [XmlIgnore]
#if INTEROP
        public DateTime DhSaiEnt { get; set; }
#else
        public DateTimeOffset DhSaiEnt { get; set; }
#endif

        [XmlElement("dhSaiEnt")]
        public string DhSaiEntField
        {
            get => DhSaiEnt.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhSaiEnt = DateTime.Parse(value);
#else
            set => DhSaiEnt = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpNF")]
        public TipoOperacao TpNF { get; set; }

        [XmlElement("idDest")]
        public DestinoOperacao IdDest { get; set; }

        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }

        [XmlElement("tpImp")]
        public FormatoImpressaoDANFE TpImp { get; set; }

        [XmlElement("tpEmis")]
        public TipoEmissao TpEmis { get; set; }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("finNFe")]
        public FinalidadeNFe FinNFe { get; set; }

        [XmlElement("indFinal")]
        public SimNao IndFinal { get; set; }

        [XmlElement("indPres")]
        public IndicadorPresenca IndPres { get; set; }

#if INTEROP
        /* ¯\_(ツ)_/¯
            Interop não aceita nulos, logo, passo -1 e valido no ShouldSerializeIndIntermed
        */
        [XmlElement("indIntermed")]
        public IndicadorIntermediario IndIntermed { get; set; } = (IndicadorIntermediario)(-1);
#else
        [XmlElement("indIntermed")]
        public IndicadorIntermediario? IndIntermed { get; set; }
#endif

        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi { get; set; }

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
            get => DhCont.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhCont = DateTime.Parse(value);
#else
            set => DhCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xJust")]
        public string XJust
        {
            get => XJustField;
            set => XJustField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(256).Trim());
        }

        [XmlElement("NFref")]
        public List<NFref> NFref { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        public bool ShouldSerializeIndIntermed()
        {
            var retorna = false;

            if (IndIntermed != (IndicadorIntermediario)(-1))
            {
                if (IndIntermed != null)
                {
                    if (IndPres != IndicadorPresenca.NaoSeAplica && IndPres != IndicadorPresenca.PresencialForaEstabelecimento)
                    {
                        retorna = true;
                    }
                }
            }

            return retorna;
        }

        public bool ShouldSerializeDhSaiEntField()
        {
            // ~\uninfe\doc\NFCe e NFe 3.10\NT2012.004_v1.2_NFCe.pdf
            // Página 06 item #14
            // Nota: Para a NFC-e este campo não deve existir
            if (Mod == ModeloDFe.NFCe)
            {
                return false;
            }

            return DhSaiEnt > DateTime.MinValue;
        }

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nFref">Elemento</param>
        public void AddNFref(NFref nFref)
        {
            if (NFref == null)
            {
                NFref = new List<NFref>();
            }

            NFref.Add(nFref);
        }

        /// <summary>
        /// Retorna o elemento da lista NFref (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFref</returns>
        public NFref GetNFref(int index)
        {
            if ((NFref?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFref[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFref
        /// </summary>
        public int GetNFrefCount => (NFref != null ? NFref.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFref")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class NFref
    {
        /// <summary>
        /// Referencia uma NF-e (modelo 55) emitida anteriormente, vinculada a NF-e atual, ou uma NFC-e(modelo 65)
        /// </summary>
        [XmlElement("refNFe")]
        public string RefNFe { get; set; }

        /// <summary>
        /// Referencia uma NF-e (modelo 55) emitida anteriormente pela sua Chave de Acesso com código numérico zerado, permitindo manter o sigilo da NF-e referenciada.
        /// </summary>
        [XmlElement("refNFeSig")]
        public string RefNFeSig { get; set; }

        [XmlElement("refNF")]
        public RefNF RefNF { get; set; }

        [XmlElement("refNFP")]
        public RefNFP RefNFP { get; set; }

        [XmlElement("refCTe")]
        public string RefCTe { get; set; }

        [XmlElement("refECF")]
        public RefECF RefECF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeRefNFe() => !string.IsNullOrWhiteSpace(RefNFe);

        public bool ShouldSerializeRefNFeSig() => !string.IsNullOrWhiteSpace(RefNFeSig);

        public bool ShouldSerializeRefCTe() => !string.IsNullOrWhiteSpace(RefCTe);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefNF
    {
        private string ModField;
        private string AAMMField;

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("AAMM")]
        public string AAMM
        {
            get => AAMMField;
            set
            {
                var mesesValidos = "01-02-03-04-05-06-07-08-09-10-11-12";

                if (!mesesValidos.Contains(value.Substring(2)))
                {
                    throw new Exception("Conteúdo da TAG <AAMM>, filha da TAG <refNF>, inválido! Mês informado deve estar entre 01 e 12.");
                }

                AAMMField = value;
            }
        }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set => ModField = value;
        }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nNF")]
        public int NNF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefNFP")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefNFP
    {
        private string ModField;
        private string AAMMField;

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("AAMM")]
        public string AAMM
        {
            get => AAMMField;
            set
            {
                var mesesValidos = "01-02-03-04-05-06-07-08-09-10-11-12";

                if (!mesesValidos.Contains(value.Substring(2)))
                {
                    throw new Exception("Conteúdo da TAG <AAMM>, filha da TAG <refNFP>, inválido! Mês informado deve estar entre 01 e 12.");
                }

                AAMMField = value;
            }
        }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set
            {
                if (value != "01" && value != "04")
                {
                    throw new Exception("Conteúdo da TAG <mod>, filha da TAG <refNF>, inválido! Valores aceitos: 01 e 04.");
                }

                ModField = value;
            }
        }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nNF")]
        public int NNF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefECF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefECF
    {
        private string ModField;

        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set
            {
                if (value != "2B" && value != "2C" && value != "2D")
                {
                    throw new Exception("Conteúdo da TAG <mod>, filha da TAG <refECF>, inválido! Valores aceitos: 2B, 2C e 2D.");
                }

                ModField = value;
            }
        }

        [XmlElement("nECF")]
        public int NECF { get; set; }

        [XmlElement("nCOO")]
        public int NCOO { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Emit
    {
        private string XNomeField;
        private string XFantField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xFant")]
        public string XFant
        {
            get => XFantField;
            set => XFantField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IEST")]
        public string IEST { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("CNAE")]
        public string CNAE { get; set; }

        [XmlElement("CRT")]
        public CRT CRT { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIEST() => !string.IsNullOrWhiteSpace(IEST);

        public bool ShouldSerializeCNAE() => !string.IsNullOrWhiteSpace(CNAE);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnderEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class EnderEmit
    {
        private string XLgrField;
        private string XCplField;
        private string NroField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set
            {
                if (value == null)
                {
                    XCplField = value;
                }
                else
                {
                    XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
                }
            }
        }

        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais);

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Avulsa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Avulsa
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xOrgao")]
        public string XOrgao { get; set; }

        [XmlElement("matr")]
        public string Matr { get; set; }

        [XmlElement("xAgente")]
        public string XAgente { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("nDAR")]
        public string NDAR { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        [XmlElement("vDAR")]
        public string VDAR { get; set; }

        [XmlElement("repEmi")]
        public string RepEmi { get; set; }

        [XmlIgnore]
        public DateTime DPag { get; set; }

        [XmlElement("dPag")]
        public string DPagField
        {
            get => DPag.ToString("yyyy-MM-dd");
            set => DPag = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Dest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Dest
    {
        private string XNomeField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlIgnore]
        public string CpfCnpj
        {
            set
            {
                if (value.Length <= 11)
                {
                    CPF = value;
                }
                else
                {
                    CNPJ = value;
                }
            }
        }

        [XmlElement("idEstrangeiro")]
        public string IdEstrangeiro { get; set; }

        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        [XmlElement("indIEDest")]
        public IndicadorIEDestinatario IndIEDest { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("ISUF")]
        public string ISUF { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIdEstrangeiro() => !string.IsNullOrWhiteSpace(IdEstrangeiro) || (string.IsNullOrWhiteSpace(CNPJ) && string.IsNullOrWhiteSpace(CPF) && !string.IsNullOrWhiteSpace(XNome));

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeISUF() => !string.IsNullOrWhiteSpace(ISUF);

        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnderDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class EnderDest
    {
        private string XLgrField;
        private string NroField;
        private string XCplField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set => XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region cMun

        private int _CMun;

        [XmlIgnore]
        public int CMun
        {
            get => _CMun;
            set
            {
                if (value <= 0)
                {
                    throw new Exception("Código do município do destinatário (tag <cMun> da <enderDest>) está sem conteúdo. É obrigatório informar o código IBGE do município.");
                }

                _CMun = value;
            }
        }

        [XmlElement("cMun")]
        public string CMunField
        {
            get => CMun.ToString();
            set => CMun = Convert.ToInt32(string.IsNullOrWhiteSpace(value) ? "0" : value);
        }

        #endregion

        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

    public abstract class LocalBase
    {
        private string XNomeField;
        private string XLgrField;
        private string NroField;
        private string XCplField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set => XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region cMun

        private int _CMun;

        [XmlIgnore]
        public int CMun
        {
            get => _CMun;
            set
            {
                if (value <= 0)
                {
                    throw new Exception("Código do município do local de " + GetType().Name.ToLower() + " (tag <cMun> da <" + GetType().Name.ToLower() + ">) está sem conteúdo. É obrigatório informar o código IBGE do município.");
                }

                _CMun = value;
            }
        }

        [XmlElement("cMun")]
        public string CMunField
        {
            get => CMun.ToString();
            set => CMun = Convert.ToInt32(string.IsNullOrWhiteSpace(value) ? "0" : value);
        }

        #endregion

        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCNPJ() => string.IsNullOrWhiteSpace(CPF); //Se não tiver o CPF tenho que colocar a TAG de CNPJ em branco ou gera erro de schema

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeCPais() => CPais > 0 && CPais != 1058;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && XPais.ToUpper() != "BRASIL";

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Retirada")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Retirada : LocalBase { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Entrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Entrega : LocalBase { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.AutXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class AutXML
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Det")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Det
    {
        private string InfAdProdField;

        [XmlAttribute(AttributeName = "nItem")]
        public int NItem { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        [XmlElement("impostoDevol")]
        public ImpostoDevol ImpostoDevol { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd
        {
            get => string.IsNullOrWhiteSpace(InfAdProdField) ? null : InfAdProdField;
            set => InfAdProdField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(500).Trim());
        }

        [XmlElement("obsItem")]
        public ObsItem ObsItem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Prod")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Prod
    {
        private string XProdField;

        [XmlElement("cProd")]
        public string CProd { get; set; }

        private string CEANField = "";

        [XmlElement("cEAN")]
        public string CEAN
        {
            get => CEANField;
            set => CEANField = value;
        }

        [XmlElement("cBarra")]
        public string CBarra { get; set; } = "";

        [XmlElement("xProd")]
        public string XProd
        {
            get => XProdField;
            set => XProdField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(120).Trim());
        }

        [XmlElement("NCM")]
        public string NCM { get; set; }

        [XmlElement("NVE")]
        public List<string> NVE { get; set; }

        [XmlElement("CEST")]
        public string CEST { get; set; }

        [XmlElement("indEscala")]
#if INTEROP
        public IndicadorEscalaRelevante IndEscala { get; set; } = (IndicadorEscalaRelevante)(-1);
#else
        public IndicadorEscalaRelevante? IndEscala { get; set; }
#endif

        [XmlElement("CNPJFab")]
        public string CNPJFab { get; set; }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        [XmlElement("EXTIPI")]
        public string EXTIPI { get; set; }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlElement("uCom")]
        public string UCom { get; set; }

        [XmlElement("qCom")]
        public decimal QCom { get; set; }

        [XmlElement("vUnCom")]
        public decimal VUnCom { get; set; }

        [XmlIgnore]
        public double VProd { get; set; }

        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }

        private string CEANTribField = "";

        [XmlElement("cEANTrib")]
        public string CEANTrib
        {
            get => CEANTribField;
            set => CEANTribField = value;
        }

        [XmlElement("cBarraTrib")]
        public string CBarraTrib { get; set; } = "";

        [XmlElement("uTrib")]
        public string UTrib { get; set; }

        [XmlElement("qTrib")]
        public decimal QTrib { get; set; }

        [XmlElement("vUnTrib")]
        public decimal VUnTrib { get; set; }

        [XmlIgnore]
        public double VFrete { get; set; }

        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VSeg { get; set; }

        [XmlElement("vSeg")]
        public string VSegField
        {
            get => VSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VSeg = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        [XmlElement("indTot")]
        public SimNao IndTot { get; set; }

        [XmlElement("DI")]
        public List<DI> DI { get; set; }

        [XmlElement("detExport")]
        public List<DetExport> DetExport { get; set; }

        [XmlElement("xPed")]
        public string XPed { get; set; }

        [XmlElement("nItemPed")]
        public string NItemPed { get; set; }

        [XmlElement("nFCI")]
        public string NFCI { get; set; }

        /// <remarks/>
        [XmlElement("rastro")]
        public List<Rastro> Rastro { get; set; }

        [XmlElement("infProdNFF")]
        public InfProdNFF InfProdNFF { get; set; }

        [XmlElement("infProdEmb")]
        public InfProdEmb InfProdEmb { get; set; }

        [XmlElement("veicProd")]
        public List<VeicProd> VeicProd { get; set; }

        [XmlElement("med")]
        public Med Med { get; set; }

        [XmlElement("arma")]
        public List<Arma> Arma { get; set; }

        [XmlElement("comb")]
        public List<Comb> Comb { get; set; }

        [XmlElement("nRECOPI")]
        public string NRECOPI { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNVE() => NVE != null;

        public bool ShouldSerializeCEST() => !string.IsNullOrWhiteSpace(CEST);

        public bool ShouldSerializeCNPJFab() => !string.IsNullOrWhiteSpace(CNPJFab);

        public bool ShouldSerializeCBenef() => !string.IsNullOrWhiteSpace(CBenef);

        public bool ShouldSerializeEXTIPI() => !string.IsNullOrWhiteSpace(EXTIPI);

        public bool ShouldSerializeVFreteField() => VFrete > 0;

        public bool ShouldSerializeVSegField() => VSeg > 0;

        public bool ShouldSerializeVDescField() => VDesc > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeXPed() => !string.IsNullOrWhiteSpace(XPed);

        public bool ShouldSerializeNItemPed() => NItemPed != null;

        public bool ShouldSerializeNFCI() => !string.IsNullOrWhiteSpace(NFCI);

        public bool ShouldSerializeIndEscala() => IndEscala != null && IndEscala != (IndicadorEscalaRelevante)(-1);

        public bool ShouldSerializeNRECOPI() => !string.IsNullOrWhiteSpace(NRECOPI);

        public bool ShouldSerializeCBarra() => !string.IsNullOrWhiteSpace(CBarra);

        public bool ShouldSerializeCBarraTrib() => !string.IsNullOrWhiteSpace(CBarraTrib);

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nVE">Elemento</param>
        public void AddNVE(string nVE)
        {
            if (NVE == null)
            {
                NVE = new List<string>();
            }

            NVE.Add(nVE);
        }

        /// <summary>
        /// Retorna o elemento da lista NVE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NVE</returns>
        public string GetNVE(int index)
        {
            if ((NVE?.Count ?? 0) == 0)
            {
                return default;
            };

            return NVE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NVE
        /// </summary>
        public int GetNVECount => (NVE != null ? NVE.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="di">Elemento</param>
        public void AddDI(DI di)
        {
            if (DI == null)
            {
                DI = new List<DI>();
            }

            DI.Add(di);
        }

        /// <summary>
        /// Retorna o elemento da lista DI (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DI</returns>
        public DI GetDI(int index)
        {
            if ((DI?.Count ?? 0) == 0)
            {
                return default;
            };

            return DI[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DI
        /// </summary>
        public int GetDICount => (DI != null ? DI.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="detExport">Elemento</param>
        public void AddDetExport(DetExport detExport)
        {
            if (DetExport == null)
            {
                DetExport = new List<DetExport>();
            }

            DetExport.Add(detExport);
        }

        /// <summary>
        /// Retorna o elemento da lista DetExport (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetExport</returns>
        public DetExport GetDetExport(int index)
        {
            if ((DetExport?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetExport[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetExport
        /// </summary>
        public int GetDetExportCount => (DetExport != null ? DetExport.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="rastro">Elemento</param>
        public void AddRastro(Rastro rastro)
        {
            if (Rastro == null)
            {
                Rastro = new List<Rastro>();
            }

            Rastro.Add(rastro);
        }

        /// <summary>
        /// Retorna o elemento da lista Rastro (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Rastro</returns>
        public Rastro GetRastro(int index)
        {
            if ((Rastro?.Count ?? 0) == 0)
            {
                return default;
            };

            return Rastro[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Rastro
        /// </summary>
        public int GetRastroCount => (Rastro != null ? Rastro.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="arma">Elemento</param>
        public void AddArma(Arma arma)
        {
            if (Arma == null)
            {
                Arma = new List<Arma>();
            }

            Arma.Add(arma);
        }

        /// <summary>
        /// Retorna o elemento da lista Arma (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Arma</returns>
        public Arma GetArma(int index)
        {
            if ((Arma?.Count ?? 0) == 0)
            {
                return default;
            };

            return Arma[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Arma
        /// </summary>
        public int GetArmaCount => (Arma != null ? Arma.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comb">Elemento</param>
        public void AddComb(Comb comb)
        {
            if (Comb == null)
            {
                Comb = new List<Comb>();
            }

            Comb.Add(comb);
        }

        /// <summary>
        /// Retorna o elemento da lista Comb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comb</returns>
        public Comb GetComb(int index)
        {
            if ((Comb?.Count ?? 0) == 0)
            {
                return default;
            };

            return Comb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comb
        /// </summary>
        public int GetCombCount => (Comb != null ? Comb.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="veicProd">Elemento</param>
        public void AddVeicProd(VeicProd veicProd)
        {
            if (VeicProd == null)
            {
                VeicProd = new List<VeicProd>();
            }

            VeicProd.Add(veicProd);
        }

        /// <summary>
        /// Retorna o elemento da lista VeicProd (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da VeicProd</returns>
        public VeicProd GetVeicProd(int index)
        {
            if ((VeicProd?.Count ?? 0) == 0)
            {
                return default;
            };

            return VeicProd[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista VeicProd
        /// </summary>
        public int GetVeicProdCount => (VeicProd != null ? VeicProd.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DI")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DI
    {
        [XmlElement("nDI")]
        public string NDI { get; set; }

        [XmlIgnore]
        public DateTime DDI { get; set; }

        [XmlElement("dDI")]
        public string DDIField
        {
            get => DDI.ToString("yyyy-MM-dd");
            set => DDI = DateTime.Parse(value);
        }

        [XmlElement("xLocDesemb")]
        public string XLocDesemb { get; set; }

        [XmlElement("UFDesemb")]
        public UFBrasil UFDesemb { get; set; }

        [XmlIgnore]
        public DateTime DDesemb { get; set; }

        [XmlElement("dDesemb")]
        public string DDesembField
        {
            get => DDesemb.ToString("yyyy-MM-dd");
            set => DDesemb = DateTime.Parse(value);
        }

        [XmlElement("tpViaTransp")]
        public ViaTransporteInternacional TpViaTransp { get; set; }

        [XmlIgnore]
        public double VAFRMM { get; set; }

        [XmlElement("vAFRMM")]
        public string VAFRMMField
        {
            get => VAFRMM.ToString("F2", CultureInfo.InvariantCulture);
            set => VAFRMM = Converter.ToDouble(value);
        }

        [XmlElement("tpIntermedio")]
        public FormaImportacaoIntermediacao TpIntermedio { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

#if INTEROP
        [XmlElement("UFTerceiro")]
        public UFBrasil UFTerceiro { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UFTerceiro")]
        public UFBrasil? UFTerceiro { get; set; }
#endif

        [XmlElement("cExportador")]
        public string CExportador { get; set; }

        [XmlElement("adi")]
        public List<Adi> Adi { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVAFRMM() => VAFRMM > 0;

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeUFTerceiro() => UFTerceiro != null && UFTerceiro != UFBrasil.NaoDefinido;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="adi">Elemento</param>
        public void AddAdi(Adi adi)
        {
            if (Adi == null)
            {
                Adi = new List<Adi>();
            }

            Adi.Add(adi);
        }

        /// <summary>
        /// Retorna o elemento da lista Adi (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Adi</returns>
        public Adi GetAdi(int index)
        {
            if ((Adi?.Count ?? 0) == 0)
            {
                return default;
            };

            return Adi[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Adi
        /// </summary>
        public int GetAdiCount => (Adi != null ? Adi.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Adi")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Adi
    {
        [XmlElement("nAdicao")]
        public int NAdicao { get; set; }

        [XmlElement("nSeqAdic")]
        public int NSeqAdic { get; set; }

        [XmlElement("cFabricante")]
        public string CFabricante { get; set; }

        [XmlIgnore]
        public double VDescDI { get; set; }

        [XmlElement("vDescDI")]
        public string VDescDIField
        {
            get => VDescDI.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescDI = Converter.ToDouble(value);
        }

        [XmlElement("nDraw")]
        public string NDraw { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNDraw() => !string.IsNullOrWhiteSpace(NDraw);

        public bool ShouldSerializeVDescDIField() => VDescDI > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DetExport")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DetExport
    {
        [XmlElement("nDraw")]
        public string NDraw { get; set; }

        [XmlElement("exportInd")]
        public ExportInd ExportInd { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNDraw() => !string.IsNullOrWhiteSpace(NDraw);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ExportInd")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ExportInd
    {
        [XmlElement("nRE")]
        public string NRE { get; set; }

        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        [XmlIgnore]
        public double QExport { get; set; }

        [XmlElement("qExport")]
        public string QExportField
        {
            get => QExport.ToString("F4", CultureInfo.InvariantCulture);
            set => QExport = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Rastro")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Rastro
    {
        [XmlElement("nLote")]
        public string NLote { get; set; }

        [XmlIgnore]
        public double QLote { get; set; }

        [XmlElement("qLote")]
        public string QLoteField
        {
            get => QLote.ToString("F3", CultureInfo.InvariantCulture);
            set => QLote = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime DFab { get; set; }

        [XmlElement("dFab")]
        public string DFabField
        {
            get => DFab.ToString("yyyy-MM-dd");
            set => DFab = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DVal { get; set; }

        [XmlElement("dVal")]
        public string DValField
        {
            get => DVal.ToString("yyyy-MM-dd");
            set => DVal = DateTime.Parse(value);
        }

        [XmlElement("cAgreg")]
        public string CAgreg { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCAgreg() => !string.IsNullOrWhiteSpace(CAgreg);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProdNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfProdNFF
    {
        [XmlElement("cProdFisco")]
        public string CProdFisco { get; set; }

        [XmlElement("cOperNFF")]
        public int COperNFF { get; set; }

        [XmlElement("xEmb")]
        public string XEmb { get; set; }

        [XmlIgnore]
        public double QVolEmb { get; set; }

        [XmlElement("qVolEmb")]
        public string QVolEmbField
        {
            get => QVolEmb.ToString("F2", CultureInfo.InvariantCulture);
            set => QVolEmb = Converter.ToDouble(value);
        }

        [XmlElement("uEmb")]
        public string UEmb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXEmb() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeQVolEmbField() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeUEmb() => !string.IsNullOrWhiteSpace(XEmb);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProdEmb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfProdEmb
    {
        [XmlElement("xEmb")]
        public string XEmb { get; set; }

        [XmlIgnore]
        public double QVolEmb { get; set; }

        [XmlElement("qVolEmb")]
        public string QVolEmbField
        {
            get => QVolEmb.ToString("F3", CultureInfo.InvariantCulture);
            set => QVolEmb = Converter.ToDouble(value);
        }

        [XmlElement("uEmb")]
        public string UEmb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXEmb() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeQVolEmbField() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeUEmb() => !string.IsNullOrWhiteSpace(XEmb);

        #endregion
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Arma
    {
        private string DescrField;

        [XmlElement("tpArma")]
        public TipoArma TpArma { get; set; }

        [XmlElement("nSerie")]
        public string NSerie { get; set; }

        [XmlElement("nCano")]
        public string NCano { get; set; }

        [XmlElement("descr")]
        public string Descr
        {
            get => DescrField;
            set => DescrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(256).Trim());
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Comb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Comb
    {
        [XmlElement("cProdANP")]
        public string CProdANP { get; set; }

        [XmlElement("descANP")]
        public string DescANP { get; set; }

        [XmlIgnore]
        public double PGLP { get; set; }

        [XmlElement("pGLP")]
        public string PGLPField
        {
            get => PGLP.ToString("F4", CultureInfo.InvariantCulture);
            set => PGLP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PGNn { get; set; }

        [XmlElement("pGNn")]
        public string PGNnField
        {
            get => PGNn.ToString("F4", CultureInfo.InvariantCulture);
            set => PGNn = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PGNi { get; set; }

        [XmlElement("pGNi")]
        public string PGNiField
        {
            get => PGNi.ToString("F4", CultureInfo.InvariantCulture);
            set => PGNi = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPart { get; set; }

        [XmlElement("vPart")]
        public string VPartField
        {
            get => VPart.ToString("F2", CultureInfo.InvariantCulture);
            set => VPart = Converter.ToDouble(value);
        }

        [XmlElement("CODIF")]
        public string CODIF { get; set; }

        [XmlIgnore]
        public double QTemp { get; set; }

        [XmlElement("qTemp")]
        public string QTempField
        {
            get => QTemp.ToString("F4", CultureInfo.InvariantCulture);
            set => QTemp = Converter.ToDouble(value);
        }

        [XmlElement("UFCons")]
        public UFBrasil UFCons { get; set; }

        [XmlElement("CIDE")]
        public CIDE CIDE { get; set; }

        [XmlElement("encerrante")]
        public Encerrante Encerrante { get; set; }

        /// <summary>
        /// Percentual do índice de mistura do Biodiesel (B100) no Óleo Diesel B instituído pelo órgão regulamentador
        /// </summary>
        [XmlIgnore]
        public double PBio { get; set; }

        [XmlElement("pBio")]
        public string PBioField
        {
            get => PBio.ToString("F4", CultureInfo.InvariantCulture);
            set => PBio = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo indicador da origem do combustível
        /// </summary>
        [XmlElement("origComb")]
        public List<OrigComb> OrigComb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePGLPField() => PGLP > 0;

        public bool ShouldSerializePGNnField() => PGNn > 0;

        public bool ShouldSerializePGNiField() => PGNi > 0;

        public bool ShouldSerializeVPartField() => VPart > 0;

        public bool ShouldSerializeCODIF() => !string.IsNullOrWhiteSpace(CODIF);

        public bool ShouldSerializeQTempField() => QTemp > 0;

        public bool ShouldSerializePBioField() => PBio > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOrigComb(OrigComb item)
        {
            if (OrigComb == null)
            {
                OrigComb = new List<OrigComb>();
            }

            OrigComb.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista OrigComb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da OrigComb</returns>
        public OrigComb GetOrigComb(int index)
        {
            if ((OrigComb?.Count ?? 0) == 0)
            {
                return default;
            };

            return OrigComb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista OrigComb
        /// </summary>
        public int GetOrigCombCount => (OrigComb != null ? OrigComb.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.CIDE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class CIDE
    {
        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        [XmlIgnore]
        public double VAliqProd { get; set; }

        [XmlElement("vAliqProd")]
        public string VAliqProdField
        {
            get => VAliqProd.ToString("F4", CultureInfo.InvariantCulture);
            set => VAliqProd = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCIDE { get; set; }

        [XmlElement("vCIDE")]
        public string VCIDEField
        {
            get => VCIDE.ToString("F2", CultureInfo.InvariantCulture);
            set => VCIDE = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Encerrante")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Encerrante
    {
        [XmlElement("nBico")]
        public int NBico { get; set; }

        [XmlElement("nBomba")]
        public int NBomba { get; set; }

        [XmlElement("nTanque")]
        public int NTanque { get; set; }

        [XmlIgnore]
        public double VEncIni { get; set; }

        [XmlElement("vEncIni")]
        public string VEncIniField
        {
            get => VEncIni.ToString("F3", CultureInfo.InvariantCulture);
            set => VEncIni = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VEncFin { get; set; }

        [XmlElement("vEncFin")]
        public string VEncFinField
        {
            get => VEncFin.ToString("F3", CultureInfo.InvariantCulture);
            set => VEncFin = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeNBomba() => NBomba > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.OrigComb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class OrigComb
    {
        /// <summary>
        /// Indicador de importação
        /// </summary>
        [XmlElement("indImport")]
        public IndicadorImportacao IndImport { get; set; }

        /// <summary>
        /// UF de origem do produtor ou do importado
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUFOrig { get; set; }

        [XmlElement("cUFOrig")]
        public int CUFOrigField
        {
            get => (int)CUFOrig;
            set => CUFOrig = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Percentual do índice de mistura do Biodiesel (B100) no Óleo Diesel B instituído pelo órgão regulamentador
        /// </summary>
        [XmlIgnore]
        public double POrig { get; set; }

        [XmlElement("pOrig")]
        public string POrigField
        {
            get => POrig.ToString("F4", CultureInfo.InvariantCulture);
            set => POrig = Converter.ToDouble(value);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Med")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Med
    {
        private string XMotivoIsencaoField;

        [XmlElement("cProdANVISA")]
        public string CProdANVISA { get; set; }

        [XmlElement("xMotivoIsencao")]
        public string XMotivoIsencao
        {
            get => XMotivoIsencaoField;
            set => XMotivoIsencaoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(255).Trim());
        }

        [XmlIgnore]
        public double VPMC { get; set; }

        [XmlElement("vPMC")]
        public string VPMCField
        {
            get => VPMC.ToString("F2", CultureInfo.InvariantCulture);
            set => VPMC = Converter.ToDouble(value);
        }
    }

    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class VeicProd
    {
        [XmlElement("tpOp")]
        public TipoOperacaoVeicNovo TpOp { get; set; }

        [XmlElement("chassi")]
        public string Chassi { get; set; }

        [XmlElement("cCor")]
        public string CCor { get; set; }

        [XmlElement("xCor")]
        public string XCor { get; set; }

        [XmlElement("pot")]
        public string Pot { get; set; }

        [XmlElement("cilin")]
        public string Cilin { get; set; }

        [XmlElement("pesoL")]
        public string PesoL { get; set; }

        [XmlElement("pesoB")]
        public string PesoB { get; set; }

        [XmlElement("nSerie")]
        public string NSerie { get; set; }

        [XmlElement("tpComb")]
        public string TpComb { get; set; }

        [XmlElement("nMotor")]
        public string NMotor { get; set; }

        [XmlElement("CMT")]
        public string CMT { get; set; }

        [XmlElement("dist")]
        public string Dist { get; set; }

        [XmlElement("anoMod")]
        public int AnoMod { get; set; }

        [XmlElement("anoFab")]
        public int AnoFab { get; set; }

        [XmlElement("tpPint")]
        public string TpPint { get; set; }

        [XmlElement("tpVeic")]
        public string TpVeic { get; set; }

        [XmlElement("espVeic")]
        public string EspVeic { get; set; }

        [XmlElement("VIN")]
        public CondicaoVIN VIN { get; set; }

        [XmlElement("condVeic")]
        public CondicaoVeiculo CondVeic { get; set; }

        [XmlElement("cMod")]
        public int CMod { get; set; }

        [XmlElement("cCorDENATRAN")]
        public string CCorDENATRAN { get; set; }

        [XmlElement("lota")]
        public int Lota { get; set; }

        [XmlElement("tpRest")]
        public TipoRestricaoVeiculo TpRest { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Imposto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Imposto
    {
        [XmlIgnore]
        public double VTotTrib { get; set; }

        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Converter.ToDouble(value);
        }

        [XmlElement("ICMS")]
        public ICMS ICMS { get; set; }

        [XmlElement("IPI")]
        public IPI IPI { get; set; }

        [XmlElement("II")]
        public II II { get; set; }

        [XmlElement("ISSQN")]
        public ISSQN ISSQN { get; set; }

        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        [XmlElement("PISST")]
        public PISST PISST { get; set; }

        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        [XmlElement("COFINSST")]
        public COFINSST COFINSST { get; set; }

        [XmlElement("ICMSUFDest")]
        public ICMSUFDest ICMSUFDest { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS
    {
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("ICMS02")]
        public ICMS02 ICMS02 { get; set; }

        [XmlElement("ICMS10")]
        public ICMS10 ICMS10 { get; set; }

        [XmlElement("ICMS15")]
        public ICMS15 ICMS15 { get; set; }

        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        [XmlElement("ICMS30")]
        public ICMS30 ICMS30 { get; set; }

        [XmlElement("ICMS40")]
        public ICMS40 ICMS40 { get; set; }

        [XmlElement("ICMS51")]
        public ICMS51 ICMS51 { get; set; }

        [XmlElement("ICMS53")]
        public ICMS53 ICMS53 { get; set; }

        [XmlElement("ICMS60")]
        public ICMS60 ICMS60 { get; set; }

        [XmlElement("ICMS61")]
        public ICMS61 ICMS61 { get; set; }

        [XmlElement("ICMS70")]
        public ICMS70 ICMS70 { get; set; }

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("ICMSPart")]
        public ICMSPart ICMSPart { get; set; }

        [XmlElement("ICMSSN101")]
        public ICMSSN101 ICMSSN101 { get; set; }

        [XmlElement("ICMSSN102")]
        public ICMSSN102 ICMSSN102 { get; set; }

        [XmlElement("ICMSSN201")]
        public ICMSSN201 ICMSSN201 { get; set; }

        [XmlElement("ICMSSN202")]
        public ICMSSN202 ICMSSN202 { get; set; }

        [XmlElement("ICMSSN500")]
        public ICMSSN500 ICMSSN500 { get; set; }

        [XmlElement("ICMSSN900")]
        public ICMSSN900 ICMSSN900 { get; set; }

        [XmlElement("ICMSST")]
        public ICMSST ICMSST { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS00")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS00
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "00";

        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => PFCP > 0 || VFCP > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS02")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS02
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "02";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS próprio em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto. Alíquota ad rem do ICMS, estabelecida na legislação para o produto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio - O valor do ICMS é obtido pela multiplicação da alíquota ad rem pela quantidade do produto conforme unidade de medida estabelecida na legislação.
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS10")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS10
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "10";

        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public double VBCFCP { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST { get; set; }

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPField() => VBCFCP > 0;

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => PFCP > 0;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS15")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS15
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "15";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS próprio em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade tributada sujeita a retenção - Informar a BC do ICMS sujeito a retenção em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMonoReten")]
        public decimal QBCMonoReten { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto com retenção
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSReten { get; set; }

        [XmlElement("adRemICMSReten")]
        public string AdRemICMSRetenField
        {
            get => AdRemICMSReten.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio com retenção
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoReten { get; set; }

        [XmlElement("vICMSMonoReten")]
        public string VICMSMonoRetenField
        {
            get => VICMSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução do valor da alíquota ad rem do ICMS
        /// </summary>
        [XmlIgnore]
        public double PRedAdRem { get; set; }

        [XmlElement("pRedAdRem")]
        public string PRedAdRemField
        {
            get => PRedAdRem.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAdRem = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da redução do adrem do ICMS. Só preencher se o pRedAdRem for maior que zero.
        /// </summary>
        [XmlElement("motRedAdRem")]
        public MotivoReducaoAdRem MotRedAdRem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;
        public bool ShouldSerializeQBCMonoReten() => QBCMonoReten > 0;
        public bool ShouldSerializePRedAdRemField() => PRedAdRem > 0;
        public bool ShouldSerializeMotRedAdRem() => PRedAdRem > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS20")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS20
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "20";

        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public double VBCFCP { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
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
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPField() => VBCFCP > 0;

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS30")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS30
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "30";

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST { get; set; }

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS40")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS40
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS51")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS51
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "51";

        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif


        [XmlIgnore]
        public double? PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSOp { get; set; }

        [XmlElement("vICMSOp")]
        public string VICMSOpField
        {
            get => VICMSOp?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSOp = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PDif { get; set; }

        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif?.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSDif { get; set; }

        [XmlElement("vICMSDif")]
        public string VICMSDifField
        {
            get => VICMSDif?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCP { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
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
        public double PFCPDif { get; set; }

        [XmlElement("pFCPDif")]
        public string PFCPDifField
        {
            get => PFCPDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPDif { get; set; }

        [XmlElement("vFCPDif")]
        public string VFCPDifField
        {
            get => VFCPDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPEfet { get; set; }

        [XmlElement("vFCPEfet")]
        public string VFCPEfetField
        {
            get => VFCPEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);
        public bool ShouldSerializePRedBCField() => PRedBC != null;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializPICMSeField() => PICMS != null;
        public bool ShouldSerializeVICMSOpField() => VICMSOp != null;

        public bool ShouldSerializePDifField() => PDif != null;
        public bool ShouldSerializeVICMSDifField() => VICMSDif != null;
        public bool ShouldSerializeVICMSField() => VICMS != null;

        public bool ShouldSerializeVBCFCPField() => (VBCFCP + VFCP + PFCP) > 0;
        public bool ShouldSerializePFCPField() => (VBCFCP + VFCP + PFCP) > 0;
        public bool ShouldSerializeVFCPField() => (VBCFCP + VFCP + PFCP) > 0;

        public bool ShouldSerializePFCPDifField() => PFCPDif > 0;
        public bool ShouldSerializeVFCPDifField() => VFCPDif > 0;
        public bool ShouldSerializeVFCPEfetField() => VFCPEfet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS53")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS53
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "53";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do ICMS estabelecida na legislação para o produto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da operação - O valor do ICMS é obtido pela multiplicação da alíquota ad rem pela quantidade do produto conforme unidade de medida estabelecida em legislação, como se não houvesse o diferimento
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoOp { get; set; }

        [XmlElement("vICMSMonoOp")]
        public string VICMSMonoOpField
        {
            get => VICMSMonoOp.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoOp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do diferimento - No caso de diferimento total, informar o percentual de diferimento "100".
        /// </summary>
        [XmlIgnore]
        public double PDif { get; set; }

        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoDif { get; set; }

        [XmlElement("vICMSMonoDif")]
        public string VICMSMonoDifField
        {
            get => VICMSMonoDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio devido - O valor do ICMS próprio devido é o resultado do valor do ICMS da operação menos valor do ICMS diferido.
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade tributada diferida - Informar a BC do ICMS diferido em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMonoDif")]
        public double QBCMonoDif { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto diferido
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSDif { get; set; }

        [XmlElement("adRemICMSDif")]
        public string AdRemICMSDifField
        {
            get => AdRemICMSDif.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSDif = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;
        public bool ShouldSerializeAdRemICMSField() => AdRemICMS > 0;
        public bool ShouldSerializeVICMSMonoOpField() => VICMSMonoOp > 0;
        public bool ShouldSerializePDifField() => PDif > 0;
        public bool ShouldSerializeVICMSMonoDifField() => VICMSMonoDif > 0;
        public bool ShouldSerializeVICMSMonoField() => VICMSMono > 0;
        public bool ShouldSerializeQBCMonoDif() => QBCMonoDif > 0;
        public bool ShouldSerializeAdRemICMSDifField() => AdRemICMSDif > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS60")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS60
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "60";

        [XmlIgnore]
        public double? VBCSTRet { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PST { get; set; }

        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSSTRet { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCEfet { get; set; }

        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSEfet { get; set; }

        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSEfet { get; set; }

        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCSTRetField() => VBCSTRet != null;
        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;
        public bool ShouldSerializeVICMSSTRetField() => VICMSSTRet != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS61")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS61
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "61";

        /// <summary>
        /// Quantidade tributada retida anteriormente - Informar a BC do ICMS em quantidade conforme unidade de medida estabelecida na legislação.
        /// </summary>
        [XmlElement("qBCMonoRet")]
        public decimal QBCMonoRet { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSRet { get; set; }

        [XmlElement("adRemICMSRet")]
        public string AdRemICMSRetField
        {
            get => AdRemICMSRet.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoRet { get; set; }

        [XmlElement("vICMSMonoRet")]
        public string VICMSMonoRetField
        {
            get => VICMSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoRet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMonoRet() => QBCMonoRet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS70")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS70
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "70";

        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        [XmlIgnore]
        public double? PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public double VBCFCP { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
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
#if INTEROP
        private ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        private ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMS")]
#if INTEROP
        public MotivoDesoneracaoICMS MotDesICMS { get; set; } = (MotivoDesoneracaoICMS)(-1);
#else
        public MotivoDesoneracaoICMS? MotDesICMS { get; set; }
#endif

        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public virtual bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializePRedBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && PRedBC != null;

        public virtual bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVBCFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializeVFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePMVASTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PMVAST != null;

        public virtual bool ShouldSerializePRedBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PRedBCST != null;

        public virtual bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVBCFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializePFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVICMSDesonField() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1);

        public virtual bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeMotDesICMS() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;

        public virtual bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public virtual bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS90")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS90
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; } = "90";

        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public double VBCFCP { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
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
#if INTEROP
        private ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        private ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMS")]
#if INTEROP
        public MotivoDesoneracaoICMS MotDesICMS { get; set; } = (MotivoDesoneracaoICMS)(-1);
#else
        public MotivoDesoneracaoICMS? MotDesICMS { get; set; }
#endif

        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public virtual bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializePRedBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && PRedBC > 0;

        public virtual bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVBCFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializeVFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePMVASTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PMVAST != null;

        public virtual bool ShouldSerializePRedBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PRedBCST != null;

        public virtual bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVBCFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializePFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVICMSDesonField() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1);

        public virtual bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeMotDesICMS() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;

        public virtual bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public virtual bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSPart")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSPart
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PBCOp { get; set; }

        [XmlElement("pBCOp")]
        public string PBCOpField
        {
            get => PBCOp.ToString("F4", CultureInfo.InvariantCulture);
            set => PBCOp = Converter.ToDouble(value);
        }

        [XmlElement("UFST")]
        public UFBrasil UFST { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCField() => PRedBC > 0;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN101")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN101
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "101";

        [XmlIgnore]
        public double PCredSN { get; set; }

        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredICMSSN { get; set; }

        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN102")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN102
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public virtual string CSOSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN201")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN201
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "201";

        [XmlIgnore]
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PCredSN { get; set; }

        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VCredICMSSN { get; set; }

        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializePFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializeVFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;

        public bool ShouldSerializePCredSNField() => PCredSN != null;
        public bool ShouldSerializeVCredICMSSNField() => VCredICMSSN != null || PCredSN != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN202")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN202
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public string CSOSN { get; set; }

        [XmlIgnore]
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializePFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializeVFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN500")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN500
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "500";

        [XmlIgnore]
        public double? VBCSTRet { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PST { get; set; }

        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSSTRet { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCEfet { get; set; }

        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSEfet { get; set; }

        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSEfet { get; set; }

        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCSTRetField() => VBCSTRet != null;
        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;
        public bool ShouldSerializeVICMSSTRetField() => VICMSSTRet != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN900")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN900
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "900";

        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
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
#if INTEROP
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        public ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        [XmlIgnore]
        public double? PMVAST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PRedBCST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
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
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PCredSN { get; set; }

        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VCredICMSSN { get; set; }

        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializePRedBCField() => PRedBC != null;

        public bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => VBCFCPST > 0; // Se tiver base é obrigatório ter algo nesta TAG

        public bool ShouldSerializeVFCPSTField() => VBCFCPST > 0; // Se tiver base é obrigatório ter algo nesta TAG

        public bool ShouldSerializePCredSNField() => PCredSN != null;
        public bool ShouldSerializeVCredICMSSNField() => VCredICMSSN != null || PCredSN != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSST
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBCSTRet { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PST { get; set; }

        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSTRet { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCSTDest { get; set; }

        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        [XmlElement("vBCSTDest")]
        public string VBCSTDestField
        {
            get => VBCSTDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSTDest { get; set; }

        [XmlElement("vICMSSTDest")]
        public string VICMSSTDestField
        {
            get => VICMSSTDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCEfet { get; set; }

        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSEfet { get; set; }

        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSEfet { get; set; }

        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.II")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class II
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
        public double VDespAdu { get; set; }

        [XmlElement("vDespAdu")]
        public string VDespAduField
        {
            get => VDespAdu.ToString("F2", CultureInfo.InvariantCulture);
            set => VDespAdu = Converter.ToDouble(value);
        }


        [XmlIgnore]
        public double VII { get; set; }

        [XmlElement("vII")]
        public string VIIField
        {
            get => VII.ToString("F2", CultureInfo.InvariantCulture);
            set => VII = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIOF { get; set; }

        [XmlElement("vIOF")]
        public string VIOFField
        {
            get => VIOF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIOF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPI")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPI
    {
        [XmlElement("CNPJProd")]
        public string CNPJProd { get; set; }

        [XmlElement("cSelo")]
        public string CSelo { get; set; }

        [XmlElement("qSelo")]
        public int? QSelo { get; set; }

        [XmlElement("cEnq")]
        public string CEnq { get; set; }

        [XmlElement("IPINT")]
        public IPINT IPINT { get; set; }

        [XmlElement("IPITrib")]
        public IPITrib IPITrib { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJProd() => !string.IsNullOrWhiteSpace(CNPJProd);

        public bool ShouldSerializeCSelo() => !string.IsNullOrWhiteSpace(CSelo);

        public bool ShouldSerializeQSelo() => QSelo != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPINT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPINT
    {
        [XmlElement("CST")]
        public string CST { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPITrib")]
    [ComVisible(true)]
#endif
    [System.SerializableAttribute()]
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPITrib
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PIPI { get; set; }

        [XmlElement("pIPI")]
        public string PIPIField
        {
            get => PIPI.ToString("F4", CultureInfo.InvariantCulture);
            set => PIPI = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double QUnid { get; set; }

        [XmlElement("qUnid")]
        public string QUnidField
        {
            get => QUnid.ToString("F4", CultureInfo.InvariantCulture);
            set => QUnid = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VUnid { get; set; }

        [XmlElement("vUnid")]
        public string VUnidField
        {
            get => VUnid.ToString("F4", CultureInfo.InvariantCulture);
            set => VUnid = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIPI { get; set; }

        [XmlElement("vIPI")]
        public string VIPIField
        {
            get => VIPI.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPI = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => QUnid <= 0;

        public bool ShouldSerializePIPIField() => QUnid <= 0;

        public bool ShouldSerializeQUnidField() => QUnid > 0;

        public bool ShouldSerializeVUnidField() => QUnid > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ISSQN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ISSQN
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
        public double VAliq { get; set; }

        [XmlElement("vAliq")]
        public string VAliqField
        {
            get => VAliq.ToString("F4", CultureInfo.InvariantCulture);
            set => VAliq = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VISSQN { get; set; }

        [XmlElement("vISSQN")]
        public string VISSQNField
        {
            get => VISSQN.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSQN = Converter.ToDouble(value);
        }

        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }

        [XmlElement("cListServ")]
        public ListaServicoISSQN CListServ { get; set; }

        [XmlIgnore]
        public double VDeducao { get; set; }

        [XmlElement("vDeducao")]
        public string VDeducaoField
        {
            get => VDeducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VDeducao = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

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

        [XmlIgnore]
        public double VISSRet { get; set; }

        [XmlElement("vISSRet")]
        public string VISSRetField
        {
            get => VISSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSRet = Converter.ToDouble(value);
        }

        [XmlElement("indISS")]
        public IndicadorExigibilidadeISSQN IndISS { get; set; }

        [XmlElement("cServico")]
        public string CServico { get; set; }

        [XmlElement("cMun")]
        public int CMun { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("nProcesso")]
        public string NProcesso { get; set; }

        [XmlElement("indIncentivo")]
        public SimNao12 IndIncentivo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVDeducaoField() => VDeducao > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeVDescIncondField() => VDescIncond > 0;

        public bool ShouldSerializeVDescCondField() => VDescCond > 0;

        public bool ShouldSerializeVISSRetField() => VISSRet > 0;

        public bool ShouldSerializeCServico() => string.IsNullOrWhiteSpace(CServico);

        public bool ShouldSerializeCMun() => CMun > 0;

        public bool ShouldSerializeCPais() => CPais > 0 && CPais != 1058;

        public bool ShouldSerializeNProcesso() => string.IsNullOrWhiteSpace(NProcesso);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PIS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PIS
    {
        [XmlElement("PISAliq")]
        public PISAliq PISAliq { get; set; }

        [XmlElement("PISNT")]
        public PISNT PISNT { get; set; }

        [XmlElement("PISOutr")]
        public PISOutr PISOutr { get; set; }

        [XmlElement("PISQtde")]
        public PISQtde PISQtde { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISAliq")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISAliq
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PPIS { get; set; }

        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISNT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISNT
    {
        [XmlElement("CST")]
        public string CST { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISOutr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISOutr
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double? VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PPIS { get; set; }

        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        [XmlIgnore]
        public double? VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePPISField() => PPIS != null;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVPISField() => VPIS != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISQtde")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISQtde
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "03";

        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double VAliqProd { get; set; }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISST
    {
        [XmlIgnore]
        public double? VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PPIS { get; set; }

        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }


        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        [XmlIgnore]
        public double? VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        [XmlElement("indSomaPISST")]
#if INTEROP
        public IndicaSomaPISST IndSomaPISST { get; set; } = (IndicaSomaPISST)(-1);
#else
        public IndicaSomaPISST? IndSomaPISST { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializePPISField() => PPIS != null;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVPISField() => VPIS != null;
        public bool ShouldSerializeIndSomaPISST() => IndSomaPISST != null && IndSomaPISST != (IndicaSomaPISST)(-1);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINS
    {
        [XmlElement("COFINSAliq")]
        public COFINSAliq COFINSAliq { get; set; }

        [XmlElement("COFINSNT")]
        public COFINSNT COFINSNT { get; set; }

        [XmlElement("COFINSOutr")]
        public COFINSOutr COFINSOutr { get; set; }

        [XmlElement("COFINSQtde")]
        public COFINSQtde COFINSQtde { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSAliq")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSAliq
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PCOFINS { get; set; }

        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSNT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSNT
    {
        [XmlElement("CST")]
        public string CST { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSOutr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSOutr
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double? VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PCOFINS { get; set; }

        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        [XmlIgnore]
        public double? VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializePCOFINSField() => PCOFINS != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVCOFINSField() => VCOFINS != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSQtde")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSQtde
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "03";

        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double VAliqProd { get; set; }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSST
    {
        [XmlIgnore]
        public double? VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? PCOFINS { get; set; }

        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        [XmlIgnore]
        public double? VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlElement("indSomaCOFINSST")]
#if INTEROP
        public IndicaSomaCOFINSST IndSomaCOFINSST { get; set; } = (IndicaSomaCOFINSST)(-1);
#else
        public IndicaSomaCOFINSST? IndSomaCOFINSST { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializePCOFINSField() => PCOFINS != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVCOFINSField() => VCOFINS != null;
        public bool ShouldSerializeIndSomaCOFINSST() => IndSomaCOFINSST != null && IndSomaCOFINSST != (IndicaSomaCOFINSST)(-1);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSUFDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSUFDest
    {
        [XmlIgnore]
        public double VBCUFDest { get; set; }

        [XmlElement("vBCUFDest")]
        public string VBCUFDestField
        {
            get => VBCUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPUFDest { get; set; }

        [XmlElement("vBCFCPUFDest")]
        public string VBCFCPUFDestField
        {
            get => VBCFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPUFDest { get; set; }

        [XmlElement("pFCPUFDest")]
        public string PFCPUFDestField
        {
            get => PFCPUFDest.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSUFDest { get; set; }

        [XmlElement("pICMSUFDest")]
        public string PICMSUFDestField
        {
            get => PICMSUFDest.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSInter { get; set; }

        [XmlElement("pICMSInter")]
        public string PICMSInterField
        {
            get => PICMSInter.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSInter = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSInterPart { get; set; } = 100;

        [XmlElement("pICMSInterPart")]
        public string PICMSInterPartField
        {
            get => PICMSInterPart.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSInterPart = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPUFDest { get; set; }

        [XmlElement("vFCPUFDest")]
        public string VFCPUFDestField
        {
            get => VFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFDest { get; set; }

        [XmlElement("vICMSUFDest")]
        public string VICMSUFDestField
        {
            get => VICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFRemet { get; set; }

        [XmlElement("vICMSUFRemet")]
        public string VICMSUFRemetField
        {
            get => VICMSUFRemet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFRemet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPUFDestField() => VBCFCPUFDest > 0;

        public bool ShouldSerializePFCPUFDestField() => PFCPUFDest > 0;

        public bool ShouldSerializeVFCPUFDestField() => VFCPUFDest > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ImpostoDevol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ImpostoDevol
    {
        [XmlIgnore]
        public double PDevol { get; set; }

        [XmlElement("pDevol")]
        public string PDevolField
        {
            get => PDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => PDevol = Converter.ToDouble(value);
        }

        [XmlElement("IPI")]
        public IPIDevol IPI { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsItem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsItem
    {
        [XmlElement("obsCont")]
        public List<ObsCont> ObsCont { get; set; }

        [XmlElement("obsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsCont">Elemento</param>
        public void AddObsCont(ObsCont obsCont)
        {
            if (ObsCont == null)
            {
                ObsCont = new List<ObsCont>();
            }

            ObsCont.Add(obsCont);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsCont</returns>
        public ObsCont GetObsCont(int index)
        {
            if ((ObsCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsCont
        /// </summary>
        public int GetObsContCount => (ObsCont != null ? ObsCont.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsFisco">Elemento</param>
        public void AddObsFisco(ObsFisco obsFisco)
        {
            if (ObsFisco == null)
            {
                ObsFisco = new List<ObsFisco>();
            }

            ObsFisco.Add(obsFisco);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsFisco (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsFisco</returns>
        public ObsFisco GetObsFisco(int index)
        {
            if ((ObsFisco?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsFisco[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsFisco
        /// </summary>
        public int GetObsFiscoCount => (ObsFisco != null ? ObsFisco.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPIDevol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPIDevol
    {
        [XmlIgnore]
        public double VIPIDevol { get; set; }

        [XmlElement("vIPIDevol")]
        public string VIPIDevolField
        {
            get => VIPIDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPIDevol = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Total")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Total
    {
        [XmlElement("ICMSTot")]
        public ICMSTot ICMSTot { get; set; }

        [XmlElement("ISSQNtot")]
        public ISSQNtot ISSQNtot { get; set; }

        [XmlElement("retTrib")]
        public RetTrib RetTrib { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSTot
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
        public double VFCPUFDest { get; set; }
        [XmlElement("vFCPUFDest")]
        public string VFCPUFDestField
        {
            get => VFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFDest { get; set; }
        [XmlElement("vICMSUFDest")]
        public string VICMSUFDestField
        {
            get => VICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFRemet { get; set; }
        [XmlElement("vICMSUFRemet")]
        public string VICMSUFRemetField
        {
            get => VICMSUFRemet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFRemet = Converter.ToDouble(value);
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

        [XmlIgnore]
        public double VFCPSTRet { get; set; }
        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico próprio
        /// </summary>
        [XmlIgnore]
        public double QBCMono { get; set; }

        [XmlElement("qBCMono")]
        public string QBCMonoField
        {
            get => QBCMono.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS monofásico próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        [XmlElement("qBCMonoReten")]
        public string QBCMonoRetenField
        {
            get => QBCMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoReten { get; set; }

        [XmlElement("vICMSMonoReten")]
        public string VICMSMonoRetenField
        {
            get => VICMSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoReten = Converter.ToDouble(value);
        }


        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double QBCMonoRet { get; set; }

        [XmlElement("qBCMonoRet")]
        public string QBCMonoRetField
        {
            get => QBCMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMonoRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS monofásico retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoRet { get; set; }

        [XmlElement("vICMSMonoRet")]
        public string VICMSMonoRetField
        {
            get => VICMSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VProd { get; set; }
        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFrete { get; set; }
        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VSeg { get; set; }
        [XmlElement("vSeg")]
        public string VSegField
        {
            get => VSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VSeg = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDesc { get; set; }
        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VII { get; set; }
        [XmlElement("vII")]
        public string VIIField
        {
            get => VII.ToString("F2", CultureInfo.InvariantCulture);
            set => VII = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIPI { get; set; }
        [XmlElement("vIPI")]
        public string VIPIField
        {
            get => VIPI.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPI = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIPIDevol { get; set; }
        [XmlElement("vIPIDevol")]
        public string VIPIDevolField
        {
            get => VIPIDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPIDevol = Converter.ToDouble(value);
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
        public double VCOFINS { get; set; }
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }
        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VNF { get; set; }
        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotTrib { get; set; }
        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVFCPUFDestField() => VFCPUFDest > 0;

        public bool ShouldSerializeVICMSUFDestField() => VICMSUFDest > 0;

        public bool ShouldSerializeVICMSUFRemetField() => VICMSUFRemet > 0;

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        public bool ShouldSerializeQBCMonoField() => QBCMono > 0;

        public bool ShouldSerializeVICMSMonoField() => VICMSMono > 0;

        public bool ShouldSerializeQBCMonoRetenField() => QBCMonoReten > 0;

        public bool ShouldSerializeVICMSMonoRetenField() => VICMSMonoReten > 0;

        public bool ShouldSerializeVICMSMonoRetField() => VICMSMonoRet > 0;

        public bool ShouldSerializeQBCMonoRetField() => QBCMonoRet > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ISSQNtot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ISSQNtot
    {
        [XmlIgnore]
        public double VServ { get; set; }

        [XmlElement("vServ")]
        public string VServField
        {
            get => VServ.ToString("F2", CultureInfo.InvariantCulture);
            set => VServ = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VISS { get; set; }

        [XmlElement("vISS")]
        public string VISSField
        {
            get => VISS.ToString("F2", CultureInfo.InvariantCulture);
            set => VISS = Converter.ToDouble(value);
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
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime DCompet { get; set; }

        [XmlElement("dCompet")]
        public string DCompetField
        {
            get => DCompet.ToString("yyyy-MM-dd");
            set => DCompet = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VDeducao { get; set; }

        [XmlElement("vDeducao")]
        public string VDeducaoField
        {
            get => VDeducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VDeducao = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

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

        [XmlIgnore]
        public double VISSRet { get; set; }

        [XmlElement("vISSRet")]
        public string VISSRetField
        {
            get => VISSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSRet = Converter.ToDouble(value);
        }

        [XmlElement("cRegTrib")]
        public CodigoRegimeEspecialTributacao CRegTrib { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVServField() => VServ > 0;

        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializeVISSField() => VISS > 0;

        public bool ShouldSerializeVPISField() => VPIS > 0;

        public bool ShouldSerializeVCOFINSField() => VCOFINS > 0;

        public bool ShouldSerializeVDeducaoField() => VDeducao > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeVDescIncondField() => VDescIncond > 0;

        public bool ShouldSerializeVDescCondField() => VDescCond > 0;

        public bool ShouldSerializeVISSRetField() => VISSRet > 0;

        public bool ShouldSerializeCRegTrib() => Enum.IsDefined(typeof(CodigoRegimeEspecialTributacao), CRegTrib);

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetTrib")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RetTrib
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

        [XmlElement("vRetCOFINS")]
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

        [XmlIgnore]
        public double VBCRetPrev { get; set; }

        [XmlElement("vBCRetPrev")]
        public string VBCRetPrevField
        {
            get => VBCRetPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCRetPrev = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetPrev { get; set; }

        [XmlElement("vRetPrev")]
        public string VRetPrevField
        {
            get => VRetPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPrev = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVRetPISField() => VRetPIS > 0;

        public bool ShouldSerializeVRetCOFINSField() => VRetCOFINS > 0;

        public bool ShouldSerializeVRetCSLLField() => VRetCSLL > 0;

        public bool ShouldSerializeVBCIRRFField() => VBCIRRF > 0;

        public bool ShouldSerializeVIRRFField() => VIRRF > 0;

        public bool ShouldSerializeVBCRetPrevField() => VBCRetPrev > 0;

        public bool ShouldSerializeVRetPrevField() => VRetPrev > 0;

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Transp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Transp
    {
        private string VagaoField;
        private string BalsaField;

        [XmlElement("modFrete")]
        public ModalidadeFrete ModFrete { get; set; }

        [XmlElement("transporta")]
        public Transporta Transporta { get; set; }

        [XmlElement("retTransp")]
        public RetTransp RetTransp { get; set; }

        [XmlElement("veicTransp")]
        public VeicTransp VeicTransp { get; set; }

        [XmlElement("reboque")]
        public List<Reboque> Reboque { get; set; }

        [XmlElement("vagao")]
        public string Vagao
        {
            get => VagaoField;
            set => VagaoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(20).Trim());
        }

        [XmlElement("balsa")]
        public string Balsa
        {
            get => BalsaField;
            set => BalsaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(20).Trim());
        }

        [XmlElement("vol")]
        public List<Vol> Vol { get; set; } = new List<Vol>();

        #region ShouldSerialize

        public bool ShouldSerializeVagao() => !string.IsNullOrWhiteSpace(Vagao);

        public bool ShouldSerializeBalsa() => !string.IsNullOrWhiteSpace(Balsa);

        public bool ShouldSerializeVol()
        {
            if (Vol.Count <= 0)
            {
                return false;
            }

            for (var i = 0; i < Vol.Count; i++)
            {

                if (Vol[i].QVol > 0 ||
                    !string.IsNullOrWhiteSpace(Vol[i].Esp) ||
                    !string.IsNullOrWhiteSpace(Vol[i].Marca) ||
                    !string.IsNullOrWhiteSpace(Vol[i].NVol) ||
                    Vol[i].PesoL > 0 ||
                    Vol[i].PesoB > 0)
                {
                    return true;
                }
            }

            return false;
        }

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="reboque">Elemento</param>
        public void AddReboque(Reboque reboque)
        {
            if (Reboque == null)
            {
                Reboque = new List<Reboque>();
            }

            Reboque.Add(reboque);
        }

        /// <summary>
        /// Retorna o elemento da lista Reboque (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reboque</returns>
        public Reboque GetReboque(int index)
        {
            if ((Reboque?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reboque[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reboque
        /// </summary>
        public int GetReboqueCount => (Reboque != null ? Reboque.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="vol">Elemento</param>
        public void AddVol(Vol vol)
        {
            if (Vol == null)
            {
                Vol = new List<Vol>();
            }

            Vol.Add(vol);
        }

        /// <summary>
        /// Retorna o elemento da lista Vol (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Vol</returns>
        public Vol GetVol(int index)
        {
            if ((Vol?.Count ?? 0) == 0)
            {
                return default;
            };

            return Vol[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Vol
        /// </summary>
        public int GetVolCount => (Vol != null ? Vol.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Transporta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Transporta
    {
        private string XNomeField;
        private string XEnderField;
        private string XMunField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xEnder")]
        public string XEnder
        {
            get => XEnderField;
            set => XEnderField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

#if INTEROP
        [XmlElement("UF")]
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UF")]
        public UFBrasil? UF { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeXEnder() => !string.IsNullOrWhiteSpace(XEnder);

        public bool ShouldSerializeXMun() => !string.IsNullOrWhiteSpace(XMun);

        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RetTransp
    {
        [XmlIgnore]
        public double VServ { get; set; }

        [XmlElement("vServ")]
        public string VServField
        {
            get => VServ.ToString("F2", CultureInfo.InvariantCulture);
            set => VServ = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCRet { get; set; }

        [XmlElement("vBCRet")]
        public string VBCRetField
        {
            get => VBCRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSRet { get; set; }

        [XmlElement("pICMSRet")]
        public string PICMSRetField
        {
            get => PICMSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSRet { get; set; }

        [XmlElement("vICMSRet")]
        public string VICMSRetRetField
        {
            get => VICMSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSRet = Converter.ToDouble(value);
        }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }
    }

    public abstract class VeiculoBase
    {
        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("RNTC")]
        public string RNTC { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeRNTC() => !string.IsNullOrWhiteSpace(RNTC);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.VeicTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class VeicTransp : VeiculoBase { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Reboque")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Reboque : VeiculoBase { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Vol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Vol
    {
        private string EspField;
        private string MarcaField;

        [XmlElement("qVol")]
        public double QVol { get; set; }

        [XmlElement("esp")]
        public string Esp
        {
            get => EspField;
            set => EspField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("marca")]
        public string Marca
        {
            get => MarcaField;
            set => MarcaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("nVol")]
        public string NVol { get; set; }

        [XmlIgnore]
        public double PesoL { get; set; }

        [XmlElement("pesoL")]
        public string PesoLField
        {
            get => PesoL.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PesoB { get; set; }

        [XmlElement("pesoB")]
        public string PesoBField
        {
            get => PesoB.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoB = Converter.ToDouble(value);
        }

        [XmlElement("lacres")]
        public List<Lacres> Lacres { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeEsp() => !string.IsNullOrWhiteSpace(Esp);

        public bool ShouldSerializeMarca() => !string.IsNullOrWhiteSpace(Marca);

        public bool ShouldSerializeNVol() => !string.IsNullOrWhiteSpace(NVol);

        public bool ShouldSerializeQVol() => QVol > 0;

        public bool ShouldSerializePesoLField() => PesoL > 0;

        public bool ShouldSerializePesoBField() => PesoB > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacres">Elemento</param>
        public void AddLacres(Lacres lacres)
        {
            if (Lacres == null)
            {
                Lacres = new List<Lacres>();
            }

            Lacres.Add(lacres);
        }

        /// <summary>
        /// Retorna o elemento da lista Lacres (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Lacres</returns>
        public Lacres GetLacres(int index)
        {
            if ((Lacres?.Count ?? 0) == 0)
            {
                return default;
            };

            return Lacres[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Lacres
        /// </summary>
        public int GetLacresCount => (Lacres != null ? Lacres.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Lacres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Lacres
    {
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Cobr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Cobr
    {
        [XmlElement("fat")]
        public Fat Fat { get; set; }

        [XmlElement("dup")]
        public List<Dup> Dup { get; set; } = new List<Dup>();

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="dup">Elemento</param>

        public void AddDup(Dup dup)
        {
            if (Dup == null)
            {
                Dup = new List<Dup>();
            }

            Dup.Add(dup);
        }

        /// <summary>
        /// Retorna o elemento da lista Dup (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dup</returns>
        public Dup GetDup(int index)
        {
            if ((Dup?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dup[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dup
        /// </summary>
        public int GetDupCount => (Dup != null ? Dup.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Fat")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Fat
    {
        [XmlElement("nFat")]
        public string NFat { get; set; }

        [XmlIgnore]
        public double VOrig { get; set; }

        [XmlElement("vOrig")]
        public string VOrigField
        {
            get => VOrig.ToString("F2", CultureInfo.InvariantCulture);
            set => VOrig = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VLiq { get; set; }

        [XmlElement("vLiq")]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Dup")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Dup
    {
        [XmlIgnore]
        public string NDup { get; set; }

        [XmlElement("nDup")]
        public string NDupField
        {
            get => NDup.PadLeft(3, '0');
            set => NDup = value;
        }

        [XmlIgnore]
        public DateTime DVenc { get; set; }

        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VDup { get; set; }

        [XmlElement("vDup")]
        public string VDupField
        {
            get => VDup.ToString("F2", CultureInfo.InvariantCulture);
            set => VDup = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Pag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Pag
    {
        [XmlElement("detPag")]
        public List<DetPag> DetPag { get; set; } = new List<DetPag>();

        [XmlIgnore]
        public double VTroco { get; set; }

        [XmlElement("vTroco")]
        public string VTrocoField
        {
            get => VTroco.ToString("F2", CultureInfo.InvariantCulture);
            set => VTroco = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVTrocoField() => VTroco > 0;

        #endregion ShouldSerialize

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="detPag">Elemento</param>
        public void AddDetPag(DetPag detPag)
        {
            if (DetPag == null)
            {
                DetPag = new List<DetPag>();
            }

            DetPag.Add(detPag);
        }

        /// <summary>
        /// Retorna o elemento da lista DetPag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetPag</returns>
        public DetPag GetDetPag(int index)
        {
            if ((DetPag?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetPag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetPag
        /// </summary>
        public int GetDetPagCount => (DetPag != null ? DetPag.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DetPag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DetPag
    {
        private string XPagField { get; set; }

        [XmlElement("indPag")]
#if INTEROP
        public IndicadorPagamento IndPag { get; set; } = (IndicadorPagamento)(-1);
#else
        public IndicadorPagamento? IndPag { get; set; }
#endif

        [XmlElement("tPag")]
        public MeioPagamento TPag { get; set; }

        [XmlElement("xPag")]
        public string XPag
        {
            get => XPagField;
            set => XPagField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlIgnore]
        public double VPag { get; set; }

        [XmlElement("vPag")]
        public string VPagField
        {
            get => VPag.ToString("F2", CultureInfo.InvariantCulture);
            set => VPag = Converter.ToDouble(value);
        }

        [XmlElement("card")]
        public Card Card { get; set; }

        public bool ShouldSerializeIndPag() => IndPag != null && IndPag != (IndicadorPagamento)(-1);
        public bool ShouldSerializeXPag() => !string.IsNullOrWhiteSpace(XPag);

#if INTEROP
        [ObsoleteAttribute("Este método está obsoleto e será excluído em futuras versões. Utilize a propriedade IndPag para atribuir o conteúdo desejado.", false)]
        public void SetIndPag(IndicadorPagamento indicadorPagamento) => IndPag = indicadorPagamento;
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Card")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Card
    {
        [XmlElement("tpIntegra")]
        public TipoIntegracaoPagamento TpIntegra { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("tBand")]
#if INTEROP
        public BandeiraOperadoraCartao TBand { get; set; } = (BandeiraOperadoraCartao)(-1);
#else
        public BandeiraOperadoraCartao? TBand { get; set; }
#endif

        [XmlElement("cAut")]
        public string CAut { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

#if INTEROP
        public bool ShouldSerializeTBand() => TBand != (BandeiraOperadoraCartao)(-1);
#else
        public bool ShouldSerializeTBand() => TBand != null;
#endif

        public bool ShouldSerializeCAut() => !string.IsNullOrWhiteSpace(CAut);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfIntermed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfIntermed
    {
        private string IdCadIntTranField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("idCadIntTran")]
        public string IdCadIntTran
        {
            get => IdCadIntTranField;
            set
            {
                if (value.Length < 2 || value.Length > 60)
                {
                    throw new Exception("Conteúdo da TAG <idCadIntTran> filha da TAG <infIntermed> deve ter entre 2 até 60 caracteres.");
                }

                IdCadIntTranField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfAdic")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfAdic
    {
        private string InfAdFiscoField;
        private string InfCplField;

        [XmlElement("infAdFisco")]
        public string InfAdFisco
        {
            get => InfAdFiscoField;
            set => InfAdFiscoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(2000).Trim());
        }

        [XmlElement("infCpl")]
        public string InfCpl
        {
            get => InfCplField;
            set => InfCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(5000).Trim());
        }

        [XmlElement("obsCont")]
        public List<ObsCont> ObsCont { get; set; }

        /// <summary>
        /// Uso exclusivo do fisco. Existe somente para deserialização, não utilizar.
        /// </summary>
        [XmlElement("obsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

        [XmlElement("procRef")]
        public List<ProcRef> ProcRef { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);

        public bool ShouldSerializeInfCpl() => !string.IsNullOrWhiteSpace(InfCpl);

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsCont">Elemento</param>
        public void AddObsCont(ObsCont obsCont)
        {
            if (ObsCont == null)
            {
                ObsCont = new List<ObsCont>();
            }

            ObsCont.Add(obsCont);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsCont</returns>
        public ObsCont GetObsCont(int index)
        {
            if ((ObsCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsCont
        /// </summary>
        public int GetObsContCount => (ObsCont != null ? ObsCont.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsFisco">Elemento</param>
        public void AddObsFisco(ObsFisco obsFisco)
        {
            if (ObsFisco == null)
            {
                ObsFisco = new List<ObsFisco>();
            }

            ObsFisco.Add(obsFisco);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsFisco (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsFisco</returns>
        public ObsFisco GetObsFisco(int index)
        {
            if ((ObsFisco?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsFisco[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsFisco
        /// </summary>
        public int GetObsFiscoCount => (ObsFisco != null ? ObsFisco.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="procRef">Elemento</param>
        public void AddProcRef(ProcRef procRef)
        {
            if (ProcRef == null)
            {
                ProcRef = new List<ProcRef>();
            }

            ProcRef.Add(procRef);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcRef</returns>
        public ProcRef GetProcRef(int index)
        {
            if ((ProcRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcRef
        /// </summary>
        public int GetProcRefCount => (ProcRef != null ? ProcRef.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsCont
    {
        private string XTextoField;

        [XmlElement("xTexto")]
        public string XTexto
        {
            get => XTextoField;
            set => XTextoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsFisco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsFisco
    {
        private string XTextoField;

        [XmlElement("xTexto")]
        public string XTexto
        {
            get => XTextoField;
            set => XTextoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcRef")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ProcRef
    {
        [XmlElement("nProc")]
        public string NProc { get; set; }

        [XmlElement("indProc")]
        public IndicadorOrigemProcesso IndProc { get; set; }

#if INTEROP
        [XmlElement("tpAto")]
        public TipoAtoConcessorio TpAto { get; set; } = (TipoAtoConcessorio)(-1);

#else
        [XmlElement("tpAto")]
        public TipoAtoConcessorio? TpAto { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeTpAto() => TpAto != null && TpAto != (TipoAtoConcessorio)(-1);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Exporta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Exporta
    {
        private UFBrasil UFSaidaPaisField;
        private string XLocExportaField;
        private string XLocDespachoField;

        [XmlElement("UFSaidaPais")]
        public UFBrasil UFSaidaPais
        {
            get => UFSaidaPaisField;
            set
            {
                if (value == UFBrasil.EX || value == UFBrasil.AN)
                {
                    throw new Exception("Conteúdo da TAG <UFSaidaPais> inválido. Não pode ser informado EX ou AN.");
                }
                else
                {
                    UFSaidaPaisField = value;
                }
            }
        }

        [XmlElement("xLocExporta")]
        public string XLocExporta
        {
            get => XLocExportaField;
            set => XLocExportaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("xLocDespacho")]
        public string XLocDespacho
        {
            get => XLocDespachoField;
            set => XLocDespachoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region ShouldSerialize

        public bool ShouldSerializeXLocDespacho() => !string.IsNullOrWhiteSpace(XLocDespacho);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Compra")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Compra
    {
        [XmlElement("xNEmp")]
        public string XNEmp { get; set; }

        [XmlElement("xPed")]
        public string XPed { get; set; }

        [XmlElement("xCont")]
        public string XCont { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXNEmp() => !string.IsNullOrWhiteSpace(XNEmp);

        public bool ShouldSerializeXPed() => !string.IsNullOrWhiteSpace(XPed);

        public bool ShouldSerializeXCont() => !string.IsNullOrWhiteSpace(XCont);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Cana")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Cana
    {
        [XmlElement("safra")]
        public string Safra { get; set; }

        [XmlElement("ref")]
        public string Ref { get; set; }

        [XmlElement("forDia")]
        public List<ForDia> ForDia { get; set; }

        [XmlIgnore]
        public double QTotMes { get; set; }

        [XmlElement("qTotMes")]
        public string QTotMesField
        {
            get => QTotMes.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotMes = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double QTotAnt { get; set; }

        [XmlElement("qTotAnt")]
        public string QTotAntField
        {
            get => QTotAnt.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotAnt = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double QTotGer { get; set; }

        [XmlElement("qTotGer")]
        public string QTotGerField
        {
            get => QTotGer.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotGer = Converter.ToDouble(value);
        }

        [XmlElement("deduc")]
        public List<Deduc> Deduc { get; set; }

        [XmlIgnore]
        public double VFor { get; set; }

        [XmlElement("vFor")]
        public string VForField
        {
            get => VFor.ToString("F2", CultureInfo.InvariantCulture);
            set => VFor = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotDed { get; set; }

        [XmlElement("vTotDed")]
        public string VTotDedField
        {
            get => VTotDed.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDed = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VLiqFor { get; set; }

        [XmlElement("vLiqFor")]
        public string VLiqForField
        {
            get => VLiqFor.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiqFor = Converter.ToDouble(value);
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="forDia">Elemento</param>
        public void AddForDia(ForDia forDia)
        {
            if (ForDia == null)
            {
                ForDia = new List<ForDia>();
            }

            ForDia.Add(forDia);
        }

        /// <summary>
        /// Retorna o elemento da lista ForDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ForDia</returns>
        public ForDia GetForDia(int index)
        {
            if ((ForDia?.Count ?? 0) == 0)
            {
                return default;
            };

            return ForDia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ForDia
        /// </summary>
        public int GetForDiaCount => (ForDia != null ? ForDia.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="deduc">Elemento</param>
        public void AddDeduc(Deduc deduc)
        {
            if (Deduc == null)
            {
                Deduc = new List<Deduc>();
            }

            Deduc.Add(deduc);
        }

        /// <summary>
        /// Retorna o elemento da lista Deduc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Deduc</returns>
        public Deduc GetDeduc(int index)
        {
            if ((Deduc?.Count ?? 0) == 0)
            {
                return default;
            };

            return Deduc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Deduc
        /// </summary>
        public int GetDeducCount => (Deduc != null ? Deduc.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ForDia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ForDia
    {
        [XmlIgnore]
        public double Qtde { get; set; }

        [XmlElement("qtde")]
        public string QtdeField
        {
            get => Qtde.ToString("F10", CultureInfo.InvariantCulture);
            set => Qtde = Converter.ToDouble(value);
        }

        [XmlAttribute(AttributeName = "dia")]
        public int Dia { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Deduc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Deduc
    {
        private string XDedField;

        [XmlElement("xDed")]
        public string XDed
        {
            get => XDedField;
            set => XDedField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlIgnore]
        public double VDed { get; set; }

        [XmlElement("vDed")]
        public string VDedField
        {
            get => VDed.ToString("F2", CultureInfo.InvariantCulture);
            set => VDed = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfRespTec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfRespTec
    {
        private string XContatoField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xContato")]
        public string XContato
        {
            get => XContatoField;
            set => XContatoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        [XmlElement("email")]
        public string Email { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("idCSRT")]
        public string IdCSRT { get; set; }

        [XmlElement("hashCSRT", DataType = "base64Binary")]
        public byte[] HashCSRT { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);

        public bool ShouldSerializeHashCSRT() => HashCSRT != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfSolicNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfSolicNFF
    {
        [XmlElement("xSolic")]
        public string XSolic { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfNFeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfNFeSupl
    {
        [XmlElement("qrCode")]
        public string QrCode { get; set; }

        [XmlElement("urlChave")]
        public string UrlChave { get; set; }
    }
}