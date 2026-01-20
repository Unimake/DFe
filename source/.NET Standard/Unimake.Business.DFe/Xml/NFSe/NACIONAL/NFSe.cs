#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe
{
    /// <summary>
    /// Nota Fiscal de Serviços Eletrônica - NFS-e (Padrão Nacional).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFSe", Namespace = Nfse.Ns, IsNullable = false)]
    public class NFSe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML da NFS-e.
        /// </summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e.
        /// </summary>
        [XmlElement("infNFSe")]
        public InfNFSe InfNFSe { get; set; }

        /// <summary>
        /// Assinatura digital.
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.InfNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class InfNFSe
    {
        private string IdField;

        /// <summary>
        /// Id da NFS-e.
        /// Informar o identificador precedido do literal 'NFS' (53 posições) conforme manual técnico.
        /// Gerado automaticamente se não for informado (quando possível) usando dados do DPS e do emitente.
        /// </summary>
        [XmlAttribute("Id", DataType = "token")]
        public string Id
        {
            get
            {
                if (string.IsNullOrWhiteSpace(IdField))
                {
                    try
                    {
                        IdField = MontarIdNFSe();
                    }
                    catch
                    {
                        return IdField;
                    }
                }
                return IdField;
            }
            set => IdField = value;
        }

        /// <summary>
        /// Descrição do código do IBGE do município emissor da NFS-e.
        /// </summary>
        [XmlElement("xLocEmi")]
        public string XLocEmi { get; set; }

        /// <summary>
        /// Descrição do local da prestação do serviço.
        /// </summary>
        [XmlElement("xLocPrestacao")]
        public string XLocPrestacao { get; set; }

        /// <summary>
        /// Número da NFS-e.
        /// </summary>
        [XmlElement("nNFSe")]
        public string NNFSe { get; set; }

        /// <summary>
        /// Código IBGE do município de incidência do ISSQN (quando aplicável).
        /// </summary>
        [XmlElement("cLocIncid")]
        public int CLocIncid { get; set; }

        /// <summary>
        /// Descrição do município de incidência do ISSQN (quando aplicável).
        /// </summary>
        [XmlElement("xLocIncid")]
        public string XLocIncid { get; set; }

        /// <summary>
        /// Descrição do código de tributação nacional.
        /// </summary>
        [XmlElement("xTribNac")]
        public string XTribNac { get; set; }

        /// <summary>
        /// Descrição do código de tributação municipal.
        /// </summary>
        [XmlElement("xTribMun")]
        public string XTribMun { get; set; }

        /// <summary>
        /// Descrição do código NBS.
        /// </summary>
        [XmlElement("xNBS")]
        public string XNBS { get; set; }

        /// <summary>
        /// Versão do aplicativo emissor.
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Ambiente gerador da NFS-e.
        /// </summary>
        [XmlElement("ambGer")]
        public TipoAmbiente AmbGer { get; set; }

        /// <summary>
        /// Tipo de emissão da NFS-e.
        /// </summary>
        [XmlElement("tpEmis")]
        public int TpEmis { get; set; }

        /// <summary>
        /// Processo de emissão: 1=Aplicativo do contribuinte.
        /// </summary>
        [XmlElement("procEmi")]
        public int ProcEmi { get; set; }

        /// <summary>
        /// Situação (status) da NFS-e.
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Data e hora de processamento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhProc { get; set; }
#else
        public DateTimeOffset DhProc { get; set; }
#endif

        [XmlElement("dhProc")]
        public string DhProcField
        {
            get => DhProc.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProc = DateTime.Parse(value);
#else
            set => DhProc = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do DFS-e.
        /// </summary>
        [XmlElement("nDFSe")]
        public string NDFSe { get; set; }

        /// <summary>
        /// Dados do emitente da NFS-e.
        /// </summary>
        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>
        /// Valores/tributos da NFS-e.
        /// </summary>
        [XmlElement("valores", Type = typeof(ValoresInfNFSe))]
        public ValoresInfNFSe Valores { get; set; }

        /// <summary>
        /// Informações IBS/CBS (quando aplicável).
        /// </summary>
        [XmlElement("IBSCBS", Type = typeof(IBSCBSNFSe))]
        public IBSCBSNFSe IBSCBS { get; set; }

        /// <summary>
        /// DPS vinculado (conteúdo do DPS utilizado na geração da NFS-e).
        /// </summary>
        [XmlElement("DPS")]
        public DPS DPS { get; set; } = new DPS();

        #region Gerar ID

        private void ValidarDadosParaGerarId()
        {
            if (DPS?.InfDPS == null)
            {
                throw new Exception("Informe o DPS (DPS.InfDPS) antes de gerar o Id da NFS-e.");
            }

            if (DPS.InfDPS.CLocEmi == 0)
            {
                throw new Exception("Informe o código IBGE do município de emissão (DPS.InfDPS.CLocEmi) antes de gerar o Id da NFS-e.");
            }

            if (Emit == null)
            {
                throw new Exception("Informe o emitente (emit) antes de gerar o Id da NFS-e.");
            }

            if (string.IsNullOrWhiteSpace(Emit.CNPJ) && string.IsNullOrWhiteSpace(Emit.CPF))
            {
                throw new Exception("Informe o CNPJ ou CPF do emitente (emit.CNPJ/emit.CPF) antes de gerar o Id da NFS-e.");
            }

            if (string.IsNullOrWhiteSpace(NNFSe))
            {
                throw new Exception("Informe o número da NFS-e (nNFSe) antes de gerar o Id da NFS-e.");
            }

            // DhProc é obrigatório no schema, mas, por segurança, valida aqui para geração do AnoMes.
#if INTEROP
            if (DhProc == default)
#else
            if (DhProc == default)
#endif
            {
                throw new Exception("Informe a data/hora de processamento (dhProc) antes de gerar o Id da NFS-e.");
            }
        }

        /// <summary>
        /// Monta o Id da NFS-e no padrão: "NFS" + cMun(7) + ambGer(1) + tpInsc(1) + inscrFed(14) + nNFSe(13) + anoMes(4) + cNum(9) + DV(1)
        /// </summary>
        private string MontarIdNFSe()
        {
            ValidarDadosParaGerarId();

            // Partes (49 dígitos numéricos)
            var cMun = DPS.InfDPS.CLocEmi.ToString("0000000");

            var ambGer = ((int)AmbGer).ToString(CultureInfo.InvariantCulture);

            var tpInsc = !string.IsNullOrWhiteSpace(Emit.CPF) ? "1" : "2";

            var inscrFed = !string.IsNullOrWhiteSpace(Emit.CPF)
                ? Emit.CPF.PadLeft(14, '0')
                : Emit.CNPJ.PadLeft(14, '0');

            var nNFSe = NNFSe.PadLeft(13, '0');

            var anoMes = $"{(DhProc.Year % 100):00}{DhProc.Month:00}";

            // Código numérico (9 dígitos) – gerado uma única vez por instância
            if (string.IsNullOrWhiteSpace(CNum))
            {
                // 9 dígitos (1..999999999)
                var rnd = new Random();
                CNum = rnd.Next(1, 1000000000).ToString("000000000");
            }

            var corpo = cMun + ambGer + tpInsc + inscrFed + nNFSe + anoMes + CNum;

            var dv = CalcularDVModulo11(corpo);
            return "NFS" + corpo + dv.ToString(CultureInfo.InvariantCulture);
        }

        /// <summary>
        /// Código numérico (9 dígitos) usado na composição do Id.
        /// É gerado automaticamente caso não seja informado.
        /// </summary>
        [XmlIgnore]
        public string CNum { get; set; }

        private static int CalcularDVModulo11(string chaveNumerica)
        {
            if (string.IsNullOrWhiteSpace(chaveNumerica))
            {
                throw new ArgumentException("Chave numérica inválida para cálculo do DV.", nameof(chaveNumerica));
            }

            // Módulo 11 (pesos 2..9 da direita para a esquerda)
            var soma = 0;
            var peso = 2;

            for (var i = chaveNumerica.Length - 1; i >= 0; i--)
            {
                var c = chaveNumerica[i];
                if (c < '0' || c > '9')
                {
                    throw new ArgumentException("A chave deve conter apenas dígitos.", nameof(chaveNumerica));
                }

                soma += (c - '0') * peso;

                peso++;
                if (peso > 9)
                {
                    peso = 2;
                }
            }

            var mod = soma % 11;
            var dv = 11 - mod;
            if (dv == 10 || dv == 11)
            {
                dv = 0;
            }
            return dv;
        }

        #endregion Gerar ID

        #region Should Serialize
        public bool ShouldSerializeCLocIncid() => CLocIncid > 0;
        public bool ShouldSerializeXLocIncid() => !string.IsNullOrWhiteSpace(XLocIncid);
        public bool ShouldSerializeXTribMun() => !string.IsNullOrWhiteSpace(XTribMun);
        public bool ShouldSerializeXNBS() => !string.IsNullOrWhiteSpace(XNBS);
        public bool ShouldSerializeProcEmi() => ProcEmi > 0;
        #endregion
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class Emit
    {
        /// <summary>
        /// CNPJ do emitente.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Municipal.
        /// </summary>
        [XmlElement("IM")]
        public string IM { get; set; }

        /// <summary>
        /// Razão social ou nome empresarial.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Nome fantasia.
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Endereço nacional do emitente.
        /// </summary>
        [XmlElement("enderNac")]
        public EnderNac EnderNac { get; set; }

        /// <summary>
        /// Telefone.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// E-mail.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        /// <summary>
        /// Valida se foi informado exatamente um documento (CNPJ ou CPF).
        /// </summary>
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
            if (count != 1)
            {
                throw new Exception("Emitente: informe exatamente um documento: CNPJ ou CPF.");
            }
        }

        #endregion Should Serialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.EnderNac")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class EnderNac
    {
        /// <summary>
        /// Logradouro.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município (IBGE).
        /// </summary>
        [XmlElement("cMun")]
        public long CMun { get; set; }

        /// <summary>
        /// Sigla da UF.
        /// </summary>
        [XmlElement("UF")]
        public string UF { get; set; }

        /// <summary>
        /// CEP.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        #region Should Serialize
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);
        #endregion
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.ValoresInfNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class ValoresInfNFSe
    {
        [XmlIgnore]
        public double VCalcDR { get; set; }

        /// <summary>
        /// Valor de dedução/redução da BC do ISSQN.
        /// </summary>
        [XmlElement("vCalcDR")]
        public string VCalcDRField
        {
            get => VCalcDR.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcDR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Tipo Benefício Municipal.
        /// </summary>
        [XmlElement("tpBM")]
        public int TpBM { get; set; }

        [XmlIgnore]
        public double VCalcBM { get; set; }

        /// <summary>
        /// Valor do cálculo do benefício municipal.
        /// </summary>
        [XmlElement("vCalcBM")]
        public string VCalcBMField
        {
            get => VCalcBM.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcBM = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Base de cálculo.
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqAplic { get; set; }

        /// <summary>
        /// Alíquota aplicada.
        /// </summary>
        [XmlElement("pAliqAplic")]
        public string PAliqAplicField
        {
            get => PAliqAplic.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqAplic = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VISSQN { get; set; }

        /// <summary>
        /// Valor do ISSQN.
        /// </summary>
        [XmlElement("vISSQN")]
        public string VISSQNField
        {
            get => VISSQN.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSQN = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotalRet { get; set; }

        /// <summary>
        /// Valor total de retenções.
        /// </summary>
        [XmlElement("vTotalRet")]
        public string VTotalRetField
        {
            get => VTotalRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotalRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VLiq { get; set; }

        /// <summary>
        /// Valor líquido da NFS-e.
        /// </summary>
        [XmlElement("vLiq")]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Converter.ToDouble(value);
        }

        /// <summary>
        /// Outras informações (uso da Administração Tributária Municipal).
        /// </summary>
        [XmlElement("xOutInf")]
        public string XOutInf { get; set; }

        #region Should Serialize
        public bool ShouldSerializeVCalcDRField() => VCalcDR > 0;
        public bool ShouldSerializeTpBM() => TpBM > 0;
        public bool ShouldSerializeVCalcBMField() => VCalcBM > 0;
        public bool ShouldSerializeVBCField() => VBC > 0;
        public bool ShouldSerializePAliqAplicField() => PAliqAplic > 0;
        public bool ShouldSerializeVISSQNField() => VISSQN > 0;
        public bool ShouldSerializeVTotalRetField() => VTotalRet > 0;
        public bool ShouldSerializeXOutInf() => !string.IsNullOrWhiteSpace(XOutInf);
        #endregion
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.IBSCBSNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class IBSCBSNFSe
    {
        /// <summary>
        /// Código da localidade de incidência.
        /// </summary>
        [XmlElement("cLocalidadeIncid")]
        public long CLocalidadeIncid { get; set; }

        /// <summary>
        /// Descrição da localidade de incidência.
        /// </summary>
        [XmlElement("xLocalidadeIncid")]
        public string XLocalidadeIncid { get; set; }

        [XmlIgnore]
        public double PRedutor { get; set; }

        /// <summary>
        /// Percentual de redução em compra governamental.
        /// </summary>
        [XmlElement("pRedutor")]
        public string PRedutorField
        {
            get => PRedutor.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedutor = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valores calculados de IBS/CBS.
        /// </summary>
        [XmlElement("valores")]
        public ValoresIBSCBS Valores { get; set; }

        /// <summary>
        /// Totalizadores de IBS/CBS.
        /// </summary>
        [XmlElement("totCIBS")]
        public TotCIBS TotCIBS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.ValoresIBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class ValoresIBSCBS
    {
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Base de cálculo.
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCalcReeRepRes { get; set; }

        /// <summary>
        /// Valor de reembolso/repasse/ressarcimento.
        /// </summary>
        [XmlElement("vCalcReeRepRes")]
        public string VCalcReeRepResField
        {
            get => VCalcReeRepRes.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcReeRepRes = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valores da UF.
        /// </summary>
        [XmlElement("uf")]
        public UF UF { get; set; }

        /// <summary>
        /// Valores do município.
        /// </summary>
        [XmlElement("mun")]
        public Mun Mun { get; set; }

        /// <summary>
        /// Valores federais.
        /// </summary>
        [XmlElement("fed")]
        public Fed Fed { get; set; }

        #region Should Serialize
        public bool ShouldSerializeVCalcReeRepResField() => VCalcReeRepRes > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.UF")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class UF
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        /// <summary>
        /// Percentual IBS UF.
        /// </summary>
        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqUF { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota UF.
        /// </summary>
        [XmlElement("pRedAliqUF")]
        public string PRedAliqUFField
        {
            get => PRedAliqUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetUF { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva UF.
        /// </summary>
        [XmlElement("pAliqEfetUF")]
        public string PAliqEfetUFField
        {
            get => PAliqEfetUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetUF = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqUFField() => PRedAliqUF > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.Mun")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class Mun
    {
        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Percentual IBS Municipal.
        /// </summary>
        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqMun { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota municipal.
        /// </summary>
        [XmlElement("pRedAliqMun")]
        public string PRedAliqMunField
        {
            get => PRedAliqMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetMun { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva municipal.
        /// </summary>
        [XmlElement("pAliqEfetMun")]
        public string PAliqEfetMunField
        {
            get => PAliqEfetMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetMun = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqMunField() => PRedAliqMun > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.Fed")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class Fed
    {
        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Percentual CBS.
        /// </summary>
        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => PCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqCBS { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota CBS.
        /// </summary>
        [XmlElement("pRedAliqCBS")]
        public string PRedAliqCBSField
        {
            get => PRedAliqCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetCBS { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva CBS.
        /// </summary>
        [XmlElement("pAliqEfetCBS")]
        public string PAliqEfetCBSField
        {
            get => PAliqEfetCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetCBS = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqCBSField() => PRedAliqCBS > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.TotCIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class TotCIBS
    {
        [XmlIgnore]
        public double VTotNF { get; set; }

        /// <summary>
        /// Valor total da nota fiscal.
        /// </summary>
        [XmlElement("vTotNF")]
        public string VTotNFField
        {
            get => VTotNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotNF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de totalizadores do IBS.
        /// </summary>
        [XmlElement("gIBS")]
        public GIBS GIBS { get; set; }

        /// <summary>
        /// Grupo de totalizadores do CBS.
        /// </summary>
        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        /// <summary>
        /// Grupo de tributação regular (opcional).
        /// </summary>
        [XmlElement("gTribRegular")]
        public GTribRegularTotCIBS GTribRegular { get; set; }

        /// <summary>
        /// Grupo de compra governamental (opcional).
        /// </summary>
        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GIBS
    {
        [XmlIgnore]
        public double VIBSTot { get; set; }

        /// <summary>
        /// Valor total do IBS.
        /// </summary>
        [XmlElement("vIBSTot")]
        public string VIBSTotField
        {
            get => VIBSTot.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSTot = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de crédito presumido IBS (opcional).
        /// </summary>
        [XmlElement("gIBSCredPres")]
        public GIBSCredPres GIBSCredPres { get; set; }

        /// <summary>
        /// Grupo de totalização IBS UF.
        /// </summary>
        [XmlElement("gIBSUFTot")]
        public GIBSUFTot GIBSUFTot { get; set; }

        /// <summary>
        /// Grupo de totalização IBS Municipal.
        /// </summary>
        [XmlElement("gIBSMunTot")]
        public GIBSMunTot GIBSMunTot { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GIBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GIBSCredPres
    {
        [XmlIgnore]
        public double PCredPresIBS { get; set; }

        /// <summary>
        /// Alíquota do crédito presumido para IBS.
        /// </summary>
        [XmlElement("pCredPresIBS")]
        public string PCredPresIBSField
        {
            get => PCredPresIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCredPresIBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresIBS { get; set; }

        /// <summary>
        /// Valor do crédito presumido para IBS.
        /// </summary>
        [XmlElement("vCredPresIBS")]
        public string VCredPresIBSField
        {
            get => VCredPresIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresIBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GIBSUFTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GIBSUFTot
    {
        [XmlIgnore]
        public double VDifUF { get; set; }

        /// <summary>
        /// Valor de diferimento UF.
        /// </summary>
        [XmlElement("vDifUF")]
        public string VDifUFField
        {
            get => VDifUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Valor IBS UF.
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GIBSMunTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GIBSMunTot
    {
        [XmlIgnore]
        public double VDifMun { get; set; }

        /// <summary>
        /// Valor de diferimento Municipal.
        /// </summary>
        [XmlElement("vDifMun")]
        public string VDifMunField
        {
            get => VDifMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Valor IBS Municipal.
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GCBS
    {
        /// <summary>
        /// Grupo de crédito presumido CBS (opcional).
        /// </summary>
        [XmlElement("gCBSCredPres")]
        public GCBSCredPres GCBSCredPres { get; set; }

        [XmlIgnore]
        public double VDifCBS { get; set; }

        /// <summary>
        /// Valor de diferimento CBS.
        /// </summary>
        [XmlElement("vDifCBS")]
        public string VDifCBSField
        {
            get => VDifCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Valor CBS.
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GCBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GCBSCredPres
    {
        [XmlIgnore]
        public double PCredPresCBS { get; set; }

        /// <summary>
        /// Alíquota do crédito presumido para CBS.
        /// </summary>
        [XmlElement("pCredPresCBS")]
        public string PCredPresCBSField
        {
            get => PCredPresCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCredPresCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCBS { get; set; }

        /// <summary>
        /// Valor do crédito presumido para CBS.
        /// </summary>
        [XmlElement("vCredPresCBS")]
        public string VCredPresCBSField
        {
            get => VCredPresCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.GTribRegularTotCIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GTribRegularTotCIBS
    {
        [XmlIgnore]
        public double PAliqEfeRegIBSUF { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular do IBS estadual.
        /// </summary>
        [XmlElement("pAliqEfeRegIBSUF")]
        public string PAliqEfeRegIBSUFField
        {
            get => PAliqEfeRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSUF { get; set; }

        /// <summary>
        /// Valor da tributação regular do IBS estadual.
        /// </summary>
        [XmlElement("vTribRegIBSUF")]
        public string VTribRegIBSUFField
        {
            get => VTribRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfeRegIBSMun { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular do IBS municipal.
        /// </summary>
        [XmlElement("pAliqEfeRegIBSMun")]
        public string PAliqEfeRegIBSMunField
        {
            get => PAliqEfeRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSMun { get; set; }

        /// <summary>
        /// Valor da tributação regular do IBS municipal.
        /// </summary>
        [XmlElement("vTribRegIBSMun")]
        public string VTribRegIBSMunField
        {
            get => VTribRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfeRegCBS { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular da CBS.
        /// </summary>
        [XmlElement("pAliqEfeRegCBS")]
        public string PAliqEfeRegCBSField
        {
            get => PAliqEfeRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        /// <summary>
        /// Valor da tributação regular da CBS.
        /// </summary>
        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => VTribRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSeGTribCompraGov")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class GTribCompraGov
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        /// <summary>
        /// Alíquota do IBS de competência do Estado.
        /// </summary>
        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS da UF calculado.
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Alíquota do IBS de competência do Município.
        /// </summary>
        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS do Município calculado.
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Alíquota da CBS.
        /// </summary>
        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => PCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Valor do Tributo da CBS calculado.
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }
}
