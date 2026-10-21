using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>Ajustes da base de cálculo do ISSQN e documentos comprobatórios.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.VAjusteBC")]
    [ComVisible(true)]
#endif
    public class VAjusteBC
    {
        /// <summary>Percentual de ajuste da base de cálculo do ISSQN.</summary>
        [XmlIgnore]
#if INTEROP
        public double PAjusteBCISSQN { get; set; } = -1;
#else
        public double? PAjusteBCISSQN { get; set; }
#endif

        /// <summary>Representação XML de pAjusteBCISSQN.</summary>
        [XmlElement("pAjusteBCISSQN")]
        public string PAjusteBCISSQNField
        {
            get => PAjusteBCISSQNValue.ToString("F2", CultureInfo.InvariantCulture);
            set => PAjusteBCISSQN = Converter.ToDouble(value);
        }

        /// <summary>Omite pAjusteBCISSQN quando não informado, preservando zero explícito.</summary>
        public bool ShouldSerializePAjusteBCISSQNField() =>
#if INTEROP
            PAjusteBCISSQN >= 0;
#else
            PAjusteBCISSQN.HasValue;
#endif

        private double PAjusteBCISSQNValue =>
#if INTEROP
            PAjusteBCISSQN;
#else
            PAjusteBCISSQN.GetValueOrDefault();
#endif

        /// <summary>Valor de ajuste da base de cálculo do ISSQN.</summary>
        [XmlIgnore]
#if INTEROP
        public double VAjusteBCISSQN { get; set; } = -1;
#else
        public double? VAjusteBCISSQN { get; set; }
#endif

        /// <summary>Representação XML de vAjusteBCISSQN.</summary>
        [XmlElement("vAjusteBCISSQN")]
        public string VAjusteBCISSQNField
        {
            get => VAjusteBCISSQNValue.ToString("F2", CultureInfo.InvariantCulture);
            set => VAjusteBCISSQN = Converter.ToDouble(value);
        }

        /// <summary>Omite vAjusteBCISSQN quando não informado, preservando zero explícito.</summary>
        public bool ShouldSerializeVAjusteBCISSQNField() =>
#if INTEROP
            VAjusteBCISSQN >= 0;
#else
            VAjusteBCISSQN.HasValue;
#endif

        private double VAjusteBCISSQNValue =>
#if INTEROP
            VAjusteBCISSQN;
#else
            VAjusteBCISSQN.GetValueOrDefault();
#endif

        /// <summary>Documentos do ajuste, independentes da opção percentual ou valor.</summary>
        [XmlElement("documentos")]
        public DocumentosAjusteBC Documentos { get; set; }
    }

    /// <summary>Lista de documentos de ajuste da base de cálculo.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.DocumentosAjusteBC")]
    [ComVisible(true)]
#endif
    public class DocumentosAjusteBC
    {
        /// <summary>Documentos de ajuste (de 1 a 1000 ocorrências).</summary>
        [XmlElement("docAjusteBC")]
        public List<DocAjusteBC> DocAjusteBC { get; set; }

#if INTEROP
        /// <summary>Adiciona um item à lista.</summary>
        public void AddDocAjusteBC(DocAjusteBC item)
        {
            if (DocAjusteBC == null)
            {
                DocAjusteBC = new List<DocAjusteBC>();
            }
            DocAjusteBC.Add(item);
        }

        /// <summary>Obtém um item pelo índice.</summary>
        public DocAjusteBC GetDocAjusteBC(int index) => DocAjusteBC == null || DocAjusteBC.Count == 0 ? default : DocAjusteBC[index];

        /// <summary>Quantidade de itens.</summary>
        public int GetDocAjusteBCCount => DocAjusteBC?.Count ?? 0;
#endif
    }

    /// <summary>Documento comprobatório de ajuste da base de cálculo.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.DocAjusteBC")]
    [ComVisible(true)]
#endif
    public class DocAjusteBC
    {
        /// <summary>Tipo de ajuste da base de cálculo.</summary>
        [XmlElement("tpAjusteBC")]
        public TipoAjusteBCNFSe TpAjusteBC { get; set; }

        /// <summary>Descrição do tipo de ajuste.</summary>
        [XmlElement("xTpAjusteBC")]
        public string XTpAjusteBC { get; set; }

        /// <summary>Valor total do documento.</summary>
        [XmlIgnore]
        public double VTotDoc { get; set; }

        /// <summary>Representação XML de vTotDoc.</summary>
        [XmlElement("vTotDoc")]
        public string VTotDocField
        {
            get => VTotDoc.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDoc = Converter.ToDouble(value);
        }

        /// <summary>Valor de ajuste aplicado.</summary>
        [XmlIgnore]
        public double VAjusteAplic { get; set; }

        /// <summary>Representação XML de vAjusteAplic.</summary>
        [XmlElement("vAjusteAplic")]
        public string VAjusteAplicField
        {
            get => VAjusteAplic.ToString("F2", CultureInfo.InvariantCulture);
            set => VAjusteAplic = Converter.ToDouble(value);
        }

        /// <summary>Data de emissão do documento, opcional.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEmiDoc { get; set; }
#else
        public DateTimeOffset DtEmiDoc { get; set; }
#endif

        /// <summary>Representação XML de dtEmiDoc.</summary>
        [XmlElement("dtEmiDoc")]
        public string DtEmiDocField
        {
            get => DtEmiDoc.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture);
#if INTEROP
            set => DtEmiDoc = DateTime.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#else
            set => DtEmiDoc = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Omite a data não informada.</summary>
        public bool ShouldSerializeDtEmiDocField() => DtEmiDoc != default;

        /// <summary>Data de competência do documento, opcional.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtCompDoc { get; set; }
#else
        public DateTimeOffset DtCompDoc { get; set; }
#endif

        /// <summary>Representação XML de dtCompDoc.</summary>
        [XmlElement("dtCompDoc")]
        public string DtCompDocField
        {
            get => DtCompDoc.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture);
#if INTEROP
            set => DtCompDoc = DateTime.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#else
            set => DtCompDoc = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Omite a data não informada.</summary>
        public bool ShouldSerializeDtCompDocField() => DtCompDoc != default;

        /// <summary>Documento fiscal eletrônico nacional, alternativa a docFiscalOutro e docOutro.</summary>
        [XmlElement("dFeNacional")]
        public DfeNacional DfeNacional { get; set; }

        /// <summary>Outro documento fiscal, alternativa a dFeNacional e docOutro.</summary>
        [XmlElement("docFiscalOutro")]
        public DocFiscalOutro DocFiscalOutro { get; set; }

        /// <summary>Outro documento, alternativa a dFeNacional e docFiscalOutro.</summary>
        [XmlElement("docOutro")]
        public DocOutro DocOutro { get; set; }

        /// <summary>Pessoa fornecedora (TCInfoPessoa).</summary>
        [XmlElement("fornec")]
        public Fornec Fornec { get; set; }
    }

    /// <summary>Informações de bens móveis da operação.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.BensMoveis")]
    [ComVisible(true)]
#endif
    public class BensMoveis
    {
        /// <summary>Código NCM com oito dígitos, preservando zeros à esquerda.</summary>
        [XmlElement("cNCMBemMovel")]
        public string CNCMBemMovel { get; set; }

        /// <summary>Descrição do bem móvel.</summary>
        [XmlElement("xNCMBemMovel")]
        public string XNCMBemMovel { get; set; }

        /// <summary>Quantidade de bens móveis com o mesmo NCM.</summary>
        [XmlElement("qtdNCMBemMovel")]
        public int QtdNCMBemMovel { get; set; }
    }

    /// <summary>Informações da locação de imóveis.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GLocacao")]
    [ComVisible(true)]
#endif
    public class GLocacao
    {
        /// <summary>Percentual de copropriedade.</summary>
        [XmlIgnore]
        public double PCopropriedade { get; set; }

        /// <summary>Representação XML de pCopropriedade.</summary>
        [XmlElement("pCopropriedade")]
        public string PCopropriedadeField
        {
            get => PCopropriedade.ToString("F2", CultureInfo.InvariantCulture);
            set => PCopropriedade = Converter.ToDouble(value);
        }

        /// <summary>Valor total da operação.</summary>
        [XmlIgnore]
        public double VTotOper { get; set; }

        /// <summary>Representação XML de vTotOper.</summary>
        [XmlElement("vTotOper")]
        public string VTotOperField
        {
            get => VTotOper.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotOper = Converter.ToDouble(value);
        }

        /// <summary>Desconto incondicionado total.</summary>
        [XmlIgnore]
#if INTEROP
        public double VDescIncondTot { get; set; } = -1;
#else
        public double? VDescIncondTot { get; set; }
#endif

        /// <summary>Representação XML de vDescIncondTot.</summary>
        [XmlElement("vDescIncondTot")]
        public string VDescIncondTotField
        {
            get => VDescIncondTotValue.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescIncondTot = Converter.ToDouble(value);
        }

        /// <summary>Omite vDescIncondTot quando não informado, preservando zero explícito.</summary>
        public bool ShouldSerializeVDescIncondTotField() =>
#if INTEROP
            VDescIncondTot >= 0;
#else
            VDescIncondTot.HasValue;
#endif

        private double VDescIncondTotValue =>
#if INTEROP
            VDescIncondTot;
#else
            VDescIncondTot.GetValueOrDefault();
#endif

        /// <summary>Desconto condicionado total.</summary>
        [XmlIgnore]
#if INTEROP
        public double VDescCondTot { get; set; } = -1;
#else
        public double? VDescCondTot { get; set; }
#endif

        /// <summary>Representação XML de vDescCondTot.</summary>
        [XmlElement("vDescCondTot")]
        public string VDescCondTotField
        {
            get => VDescCondTotValue.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescCondTot = Converter.ToDouble(value);
        }

        /// <summary>Omite vDescCondTot quando não informado, preservando zero explícito.</summary>
        public bool ShouldSerializeVDescCondTotField() =>
#if INTEROP
            VDescCondTot >= 0;
#else
            VDescCondTot.HasValue;
#endif

        private double VDescCondTotValue =>
#if INTEROP
            VDescCondTot;
#else
            VDescCondTot.GetValueOrDefault();
#endif

        /// <summary>Data de vencimento original, opcional.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DVencOrig { get; set; }
#else
        public DateTimeOffset DVencOrig { get; set; }
#endif

        /// <summary>Representação XML de dVencOrig.</summary>
        [XmlElement("dVencOrig")]
        public string DVencOrigField
        {
            get => DVencOrig.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture);
#if INTEROP
            set => DVencOrig = DateTime.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#else
            set => DVencOrig = DateTimeOffset.ParseExact(value, "yyyy-MM-dd", CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Omite a data não informada.</summary>
        public bool ShouldSerializeDVencOrigField() => DVencOrig != default;
    }

    /// <summary>Unidade imobiliária e ajustes da base de cálculo.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GUnidImob")]
    [ComVisible(true)]
#endif
    public class GUnidImob
    {
        /// <summary>Inscrição imobiliária fiscal, preservando zeros à esquerda.</summary>
        [XmlElement("inscImobFisc")]
        public string InscImobFisc { get; set; }

        /// <summary>Cadastro imobiliário brasileiro, alternativa ao endereço.</summary>
        [XmlElement("cCIB")]
        public string CCIB { get; set; }

        /// <summary>Endereço nacional do imóvel, alternativa a cCIB.</summary>
        [XmlElement("end")]
        public EndImovel End { get; set; }

        /// <summary>Ajustes da locação (até 1000 ocorrências).</summary>
        [XmlElement("gAjusteBCLocImoveis")]
        public List<GAjusteBCLocImoveis> GAjusteBCLocImoveis { get; set; }

#if INTEROP
        /// <summary>Adiciona um item à lista.</summary>
        public void AddGAjusteBCLocImoveis(GAjusteBCLocImoveis item)
        {
            if (GAjusteBCLocImoveis == null)
            {
                GAjusteBCLocImoveis = new List<GAjusteBCLocImoveis>();
            }
            GAjusteBCLocImoveis.Add(item);
        }

        /// <summary>Obtém um item pelo índice.</summary>
        public GAjusteBCLocImoveis GetGAjusteBCLocImoveis(int index) => GAjusteBCLocImoveis == null || GAjusteBCLocImoveis.Count == 0 ? default : GAjusteBCLocImoveis[index];

        /// <summary>Quantidade de itens.</summary>
        public int GetGAjusteBCLocImoveisCount => GAjusteBCLocImoveis?.Count ?? 0;
#endif
    }

    /// <summary>Endereço nacional da unidade imobiliária (TCEnderImovel).</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.EndImovel")]
    [ComVisible(true)]
#endif
    public class EndImovel
    {
        /// <summary>CEP do imóvel.</summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>Logradouro.</summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>Número do endereço.</summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>Complemento opcional.</summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>Bairro opcional.</summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }
    }

    /// <summary>Parcela não integrante da base de cálculo do IBS/CBS na locação.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GAjusteBCLocImoveis")]
    [ComVisible(true)]
#endif
    public class GAjusteBCLocImoveis
    {
        /// <summary>Tipo da parcela não integrante da base de cálculo.</summary>
        [XmlElement("tpAjusteBCLocImoveis")]
        public TipoAjusteBCLocImoveisNFSe TpAjusteBCLocImoveis { get; set; }

        /// <summary>Descrição da parcela.</summary>
        [XmlElement("xTpAjusteBCLocImoveis")]
        public string XTpAjusteBCLocImoveis { get; set; }

        /// <summary>Valor da parcela.</summary>
        [XmlIgnore]
        public double VAjusteBCLocImoveis { get; set; }

        /// <summary>Representação XML de vAjusteBCLocImoveis.</summary>
        [XmlElement("vAjusteBCLocImoveis")]
        public string VAjusteBCLocImoveisField
        {
            get => VAjusteBCLocImoveis.ToString("F2", CultureInfo.InvariantCulture);
            set => VAjusteBCLocImoveis = Converter.ToDouble(value);
        }
    }

}

