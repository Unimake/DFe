#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1210")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtPgtos/v_S_01_02_00", IsNullable = false)]
    public class ESocial1210 : XMLBase
    {
        [XmlElement("evtPgtos")]
        public EvtPgtos EvtPgtos { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtPgtos")]
    [ComVisible(true)]
#endif
    public class EvtPgtos
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento1210 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideBenef")]
        public IdeBenef IdeBenef { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1210")]
    [ComVisible(true)]
#endif
    public class IdeEvento1210 
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação.
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado.
        /// Validação: O preenchimento é obrigatório se indRetif = [2].
        /// Deve ser um recibo de entrega válido, correspondente ao arquivo que está sendo retificado.
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else        
        public DateTimeOffset PerApur { get; set; }
#endif

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência
        /// das informações, se indApuracao for igual a[1], ou apenas
        /// o ano(formato AAAA), se indApuracao for igual a[2].
        /// Validação: Deve ser um mês/ano ou ano válido, igual ou
        /// posterior ao início da obrigatoriedade dos eventos
        /// periódicos para o empregador.
        /// </summary>
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

        /// <summary>
        /// Indicativo do tipo de guia.
        /// Valores válidos:
        /// 1 - Documento de Arrecadação do eSocial - DAE
        /// </summary>
        [XmlElement("indGuia")]
#if INTEROP
        public IndGuia IndGuia { get; set; } = (IndGuia)(-1);
#else
        public IndGuia? IndGuia { get; set; }
#endif

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

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

#if INTEROP
        public bool ShouldSerializeIndGuia() => IndGuia != (IndGuia)(-1);
#else
        public bool ShouldSerializeIndGuia() => IndGuia != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeBenef")]
    [ComVisible(true)]
#endif
    public class IdeBenef
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("infoPgto")]
        public List<InfoPgto> InfoPgto { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPgto(InfoPgto item)
        {
            if (InfoPgto == null)
            {
                InfoPgto = new List<InfoPgto>();
            }

            InfoPgto.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPgto (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPgto</returns>
        public InfoPgto GetInfoPgto(int index)
        {
            if ((InfoPgto?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoPgto[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoPgto
        /// </summary>
        public int GetInfoPgtoCount => (InfoPgto != null ? InfoPgto.Count : 0);
#endif

        [XmlElement("infoIRComplem")]
        public InfoIRComplem InfoIRComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgto")]
    [ComVisible(true)]
#endif
    public class InfoPgto
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtPgto { get; set; }
#else
        public DateTimeOffset DtPgto { get; set; }
#endif

        [XmlElement("dtPgto")]
        public string DtPgtoField
        {
            get => DtPgto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtPgto = DateTime.Parse(value);
#else
            set => DtPgto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpPgto")]
        public TipoPagamentoESocial TpPgto { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlElement("perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Valor líquido recebido pelo trabalhador, composto pelos vencimentos e descontos, inclusive os descontos de IRRF e de pensão alimentícia (se houver).
        /// Validação: Não pode ser um valor negativo.
        /// </summary>
        [XmlIgnore]
        public double VrLiq { get; set; }

        [XmlElement("vrLiq")]
        public string VrLiqField
        {
            get => VrLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VrLiq = Converter.ToDouble(value);
        }

        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt InfoPgtoExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePaisResidExt() => !string.IsNullOrEmpty(PaisResidExt);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgtoExt")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt
    {
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        [XmlElement("frmTribut")]
        public FrmTribut FrmTribut { get; set; }

        [XmlElement("endExt")]
        public EndExt1210 EndExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNifBenef() => !string.IsNullOrEmpty(NifBenef);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EndExt1210")]
    [ComVisible(true)]
#endif
    public class EndExt1210
    {
        [XmlElement("endDscLograd")]
        public string EndDscLograd { get; set; }

        [XmlElement("endNrLograd")]
        public string EndNrLograd { get; set; }

        [XmlElement("endComplem")]
        public string EndComplem { get; set; }

        [XmlElement("endBairro")]
        public string EndBairro { get; set; }

        [XmlElement("endCidade")]
        public string EndCidade { get; set; }

        [XmlElement("endEstado")]
        public string EndEstado { get; set; }

        [XmlElement("endCodPostal")]
        public string EndCodPostal { get; set; }

        [XmlElement("telef")]
        public string Telef { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeEndDscLograd() => !string.IsNullOrEmpty(EndDscLograd);

        public bool ShouldSerializeEndNrLograd() => !string.IsNullOrEmpty(EndNrLograd);

        public bool ShouldSerializeEndComplem() => !string.IsNullOrEmpty(EndComplem);

        public bool ShouldSerializeEndBairro() => !string.IsNullOrEmpty(EndBairro);

        public bool ShouldSerializeEndCidade() => !string.IsNullOrEmpty(EndCidade);

        public bool ShouldSerializeEndEstado() => !string.IsNullOrEmpty(EndEstado);

        public bool ShouldSerializeEndCodPostal() => !string.IsNullOrEmpty(EndCodPostal);

        public bool ShouldSerializeTelef() => !string.IsNullOrEmpty(Telef);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRComplem")]
    [ComVisible(true)]
#endif
    public class InfoIRComplem
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtLaudo { get; set; }
#else
        public DateTimeOffset DtLaudo { get; set; }
#endif

        [XmlElement("dtLaudo")]
        public string DtLaudoField
        {
            get => DtLaudo.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtLaudo = DateTime.Parse(value);
#else
            set => DtLaudo = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("infoDep")]
        public List<InfoDep> InfoDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDep(InfoDep item)
        {
            if (InfoDep == null)
            {
                InfoDep = new List<InfoDep>();
            }

            InfoDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDep</returns>
        public InfoDep GetInfoDep(int index)
        {
            if ((InfoDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDep
        /// </summary>
        public int GetInfoDepCount => (InfoDep != null ? InfoDep.Count : 0);
#endif

        [XmlElement("infoIRCR")]
        public List<InfoIRCR> InfoIRCR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIRCR(InfoIRCR item)
        {
            if (InfoIRCR == null)
            {
                InfoIRCR = new List<InfoIRCR>();
            }

            InfoIRCR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIRCR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIRCR</returns>
        public InfoIRCR GetInfoIRCR(int index)
        {
            if ((InfoIRCR?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoIRCR[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoIRCR
        /// </summary>
        public int GetInfoIRCRCount => (InfoIRCR != null ? InfoIRCR.Count : 0);
#endif

        [XmlElement("planSaude")]
        public List<PlanSaude1210> PlanSaude { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPlanSaude(PlanSaude1210 item)
        {
            if (PlanSaude == null)
            {
                PlanSaude = new List<PlanSaude1210>();
            }

            PlanSaude.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PlanSaude1210 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PlanSaude</returns>
        public PlanSaude1210 GetPlanSaude(int index)
        {
            if ((PlanSaude?.Count ?? 0) == 0)
            {
                return default;
            };

            return PlanSaude[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PlanSaude
        /// </summary>
        public int GetPlanSaudeCount => (PlanSaude != null ? PlanSaude.Count : 0);
#endif

        [XmlElement("infoReembMed")]
        public List<InfoReembMed> InfoReembMed { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembMed(InfoReembMed item)
        {
            if (InfoReembMed == null)
            {
                InfoReembMed = new List<InfoReembMed>();
            }

            InfoReembMed.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembMed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembMed</returns>
        public InfoReembMed GetInfoReembMed(int index)
        {
            if ((InfoReembMed?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoReembMed[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoReembMed
        /// </summary>
        public int GetInfoReembMedCount => (InfoReembMed != null ? InfoReembMed.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDtLaudoField() => DtLaudo > DateTime.MinValue;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDep")]
    [ComVisible(true)]
#endif
    public class InfoDep
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlElement("dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nome")]
        public string Nome { get; set; }

        [XmlElement("depIRRF")]
        public string DepIRRF { get; set; }

        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);

        public bool ShouldSerializeDepIRRF() => !string.IsNullOrEmpty(DepIRRF);

#if INTEROP
        public bool ShouldSerializeTpDep() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDep() => TpDep != null;
#endif

        public bool ShouldSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRCR")]
    [ComVisible(true)]
#endif
    public class InfoIRCR
    {
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        [XmlElement("dedDepen")]
        public List<DedDepen> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepen item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepen>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepen GetDedDepen(int index)
        {
            if ((DedDepen?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedDepen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedDepen
        /// </summary>
        public int GetDedDepenCount => (DedDepen != null ? DedDepen.Count : 0);
#endif

        [XmlElement("penAlim")]
        public List<PenAlim> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlim item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlim>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlim GetPenAlim(int index)
        {
            if ((PenAlim?.Count ?? 0) == 0)
            {
                return default;
            };

            return PenAlim[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PenAlim
        /// </summary>
        public int GetPenAlimCount => (PenAlim != null ? PenAlim.Count : 0);
#endif

        [XmlElement("previdCompl")]
        public List<PrevidCompl1210> PrevidCompl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPrevidCompl(PrevidCompl1210 item)
        {
            if (PrevidCompl == null)
            {
                PrevidCompl = new List<PrevidCompl1210>();
            }

            PrevidCompl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PrevidCompl1210 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PrevidCompl</returns>
        public PrevidCompl1210 GetPrevidCompl(int index)
        {
            if ((PrevidCompl?.Count ?? 0) == 0)
            {
                return default;
            };

            return PrevidCompl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista PrevidCompl
        /// </summary>
        public int GetPrevidComplCount => (PrevidCompl != null ? PrevidCompl.Count : 0);
#endif

        [XmlElement("infoProcRet")]
        public List<InfoProcRet> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRet item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRet>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRet GetInfoProcRet(int index)
        {
            if ((InfoProcRet?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcRet[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcRet
        /// </summary>
        public int GetInfoProcRetCount => (InfoProcRet != null ? InfoProcRet.Count : 0);
#endif

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepen")]
    [ComVisible(true)]
#endif
    public class DedDepen
    {
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Preencher com o valor da dedução da base de cálculo.
        /// Validação: O valor informado neste campo deve ser menor ou igual ao valor unitário da dedução por dependente definido na legislação.
        /// Deve ser maior que 0 (zero).
        /// Em caso de inconsistência na validação, o arquivo será aceito, porém com alerta ao contribuinte.
        /// </summary>
        [XmlIgnore]
        public double VlrDedDep { get; set; }

        [XmlElement("vlrDedDep")]
        public string VlrDedDepField
        {
            get => VlrDedDep.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedDep = Converter.ToDouble(value);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PenAlim")]
    [ComVisible(true)]
#endif
    public class PenAlim
    {
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Valor relativo à dedução do rendimento tributável correspondente a pagamento de pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDedPenAlim { get; set; }

        [XmlElement("vlrDedPenAlim")]
        public string VlrDedPenAlimField
        {
            get => VlrDedPenAlim.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedPenAlim = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PrevidCompl1210")]
    [ComVisible(true)]
#endif
    public class PrevidCompl1210
    {
        [XmlElement("tpPrev")]
        public TipoDePrevidenciaComplementar TpPrev { get; set; }

        [XmlElement("cnpjEntidPC")]
        public string CnpjEntidPC { get; set; }

        /// <summary>
        /// Valor da dedução mensal relativa a previdência complementar.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDedPC { get; set; }

        [XmlElement("vlrDedPC")]
        public string VlrDedPCEField
        {
            get => VlrDedPC.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedPC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição mensal do ente público patrocinador da Fundação de Previdência Complementar do Servidor Público (Funpresp).
        /// Validação: Informação permitida apenas se tpPrev = [3].
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrPatrocFunp { get; set; }

        [XmlElement("vlrPatrocFunp")]
        public string VlrPatrocFunpField
        {
            get => VlrPatrocFunp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPatrocFunp = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrPatrocFunpField() => VlrPatrocFunp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcRet")]
    [ComVisible(true)]
#endif
    public class InfoProcRet
    {
        [XmlElement("tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlElement("nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        [XmlElement("infoValores")]
        public List<InfoValores> InfoValores { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoValores(InfoValores item)
        {
            if (InfoValores == null)
            {
                InfoValores = new List<InfoValores>();
            }

            InfoValores.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoValores (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoValores</returns>
        public InfoValores GetInfoValores(int index)
        {
            if ((InfoValores?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoValores[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoValores
        /// </summary>
        public int GetInfoValoresCount => (InfoValores != null ? InfoValores.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCodSusp() => !string.IsNullOrEmpty(CodSusp);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoValores")]
    [ComVisible(true)]
#endif
    public class InfoValores
    {
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        /// <summary>
        /// Valor da retenção que deixou de ser efetuada em função de processo administrativo ou judicial.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrNRetido { get; set; }

        [XmlElement("vlrNRetido")]
        public string VlrNRetidoField
        {
            get => VlrNRetido.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrNRetido = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do depósito judicial em função de processo administrativo ou judicial.
        /// Validação: Informação permitida apenas se indDeposito informado em S-1070 for igual a[S].
        /// Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDepJud { get; set; }

        [XmlElement("vlrDepJud")]
        public string VlrDepJudField
        {
            get => VlrDepJud.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDepJud = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da compensação relativa ao ano calendário em função de processo judicial.
        ///Validação: Informação permitida apenas se tpProcRet = [2].
        ///Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrCmpAnoCal { get; set; }

        [XmlElement("vlrCmpAnoCal")]
        public string VlrCmpAnoCalField
        {
            get => VlrCmpAnoCal.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCmpAnoCal = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da compensação relativa a anos anteriores em função de processo judicial.
        /// Validação: Informação permitida apenas se tpProcRet = [2].
        /// Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrCmpAnoAnt { get; set; }

        [XmlElement("vlrCmpAnoAnt")]
        public string VlrCmpAnoAntField
        {
            get => VlrCmpAnoAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCmpAnoAnt = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do rendimento com exigibilidade suspensa.
        /// Validação: Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrRendSusp { get; set; }

        [XmlElement("vlrRendSusp")]
        public string VlrRendSuspField
        {
            get => VlrRendSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendSusp = Converter.ToDouble(value);
        }

        [XmlElement("dedSusp")]
        public List<DedSusp> DedSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedSusp(DedSusp item)
        {
            if (DedSusp == null)
            {
                DedSusp = new List<DedSusp>();
            }

            DedSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedSusp</returns>
        public DedSusp GetDedSusp(int index)
        {
            if ((DedSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return DedSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DedSusp
        /// </summary>
        public int GetDedSuspCount => (DedSusp != null ? DedSusp.Count : 0);
#endif
        #region ShoudSerialize

        public bool ShouldSerializeVlrNRetidoField() => VlrNRetido > 0;
        public bool ShouldSerializeVlrDepJudField() => VlrDepJud > 0;
        public bool ShouldSerializeVlrCmpAnoCalField() => VlrCmpAnoCal > 0;
        public bool ShouldSerializeVlrCmpAnoAntField() => VlrCmpAnoAnt > 0;
        public bool ShouldSerializeVlrRendSuspField() => VlrRendSusp > 0;

        #endregion ShoudSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedSusp")]
    [ComVisible(true)]
#endif
    public class DedSusp
    {
        [XmlElement("indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        /// <summary>
        /// Valor da dedução da base de cálculo do imposto de renda com exigibilidade suspensa.
        /// Validação: Se indTpDeducao = [5, 7], e o grupo benefPen for preenchido, o valor informado neste campo deve ser a soma do(s) campo(s) vlrDepenSusp do grupo benefPen.
        /// Deve ser maior que 0 (zero).
        /// O não preenchimento do grupo benefPen indica que o contribuinte declarante não possui as informações detalhadas por dependente/alimentando.
        /// </summary>
        [XmlIgnore]
        public double VlrDedSusp { get; set; }

        [XmlElement("vlrDedSusp")]
        public string VlrDedSuspField
        {
            get => VlrDedSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedSusp = Converter.ToDouble(value);
        }

        [XmlElement("cnpjEntidPC")]
        public string CnpjEntidPC { get; set; }

        /// <summary>
        /// Valor da contribuição do ente público patrocinador da Fundação de Previdência Complementar do Servidor Público (Funpresp).
        /// Validação: Informação exclusiva se indTpDeducao = [4].
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrPatrocFunp { get; set; }

        [XmlElement("vlrPatrocFunp")]
        public string VlrPatrocFunpField
        {
            get => VlrPatrocFunp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPatrocFunp = Converter.ToDouble(value);
        }

        [XmlElement("benefPen")]
        public List<BenefPen> BenefPen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBenefPen(BenefPen item)
        {
            if (BenefPen == null)
            {
                BenefPen = new List<BenefPen>();
            }

            BenefPen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BenefPen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BenefPen</returns>
        public BenefPen GetBenefPen(int index)
        {
            if ((BenefPen?.Count ?? 0) == 0)
            {
                return default;
            };

            return BenefPen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BenefPen
        /// </summary>
        public int GetBenefPenCount => (BenefPen != null ? BenefPen.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVlrDedSuspField() => VlrDedSusp > 0;
        public bool ShouldSerializeCnpjEntidPC() => !string.IsNullOrEmpty(CnpjEntidPC);
        public bool ShoudSerializeVlrPatrocFunpField() => VlrPatrocFunp > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BenefPen")]
    [ComVisible(true)]
#endif
    public class BenefPen
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Valor da dedução relativa a dependentes ou a pensão alimentícia com exigibilidade suspensa.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDepenSusp { get; set; }

        [XmlElement("vlrDepenSusp")]
        public string VlrDepenSuspField
        {
            get => VlrDepenSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDepenSusp = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PlanSaude1210")]
    [ComVisible(true)]
#endif
    public class PlanSaude1210
    {
        [XmlElement("cnpjOper")]
        public string CnpjOper { get; set; }

        [XmlElement("regANS")]
        public string RegANS { get; set; }

        /// <summary>
        /// Valor relativo à dedução do rendimento tributável correspondente a pagamento a plano de saúde do titular.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// Se for igual a 0 (zero), deve haver informações em registro(s) filho(s), relativas a dependentes(infoDepSau).
        /// </summary>
        [XmlIgnore]
        public double VlrSaudeTit { get; set; }

        [XmlElement("vlrSaudeTit")]
        public string VlrSaudeTitField
        {
            get => VlrSaudeTit.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrSaudeTit = Converter.ToDouble(value);
        }

        [XmlElement("infoDepSau")]
        public List<InfoDepSau> InfoDepSau { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDepSau(InfoDepSau item)
        {
            if (InfoDepSau == null)
            {
                InfoDepSau = new List<InfoDepSau>();
            }

            InfoDepSau.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDepSau (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDepSau</returns>
        public InfoDepSau GetInfoDepSau(int index)
        {
            if ((InfoDepSau?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoDepSau[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoDepSau
        /// </summary>
        public int GetInfoDepSauCount => (InfoDepSau != null ? InfoDepSau.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeRegANS() => !string.IsNullOrEmpty(RegANS);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDepSau")]
    [ComVisible(true)]
#endif
    public class InfoDepSau
    {
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <sumary>
        /// Valor relativo a dedução do rendimento tributável correspondente a pagamento a plano de saúde do dependente.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// Se for igual a 0 (zero), vlrSaudeTit deve ser maior que 0 (zero).
        /// </sumary>
        [XmlIgnore]
        public double VlrSaudeDep { get; set; }

        [XmlElement("vlrSaudeDep")]
        public string VlrSaudeDepField
        {
            get => VlrSaudeDep.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrSaudeDep = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReembMed")]
    [ComVisible(true)]
#endif
    public class InfoReembMed
    {
        [XmlElement("indOrgReemb")]
        public IndicativoOrigemReembolso IndOrgReemb { get; set; }

        [XmlElement("cnpjOper")]
        public string CnpjOper { get; set; }

        [XmlElement("regANS")]
        public string RegANS { get; set; }

        [XmlElement("detReembTit")]
        public List<DetReembTit> DetReembTit { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetReembTit(DetReembTit item)
        {
            if (DetReembTit == null)
            {
                DetReembTit = new List<DetReembTit>();
            }

            DetReembTit.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetReembTit (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetReembTit</returns>
        public DetReembTit GetDetReembTit(int index)
        {
            if ((DetReembTit?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetReembTit[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetReembTit
        /// </summary>
        public int GetDetReembTitCount => (DetReembTit != null ? DetReembTit.Count : 0);
#endif

        [XmlElement("infoReembDep")]
        public List<InfoReembDep1210> InfoReembDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembDep(InfoReembDep1210 item)
        {
            if (InfoReembDep == null)
            {
                InfoReembDep = new List<InfoReembDep1210>();
            }

            InfoReembDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembDep</returns>
        public InfoReembDep1210 GetInfoReembDep(int index)
        {
            if ((InfoReembDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoReembDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoReembDep
        /// </summary>
        public int GetInfoReembDepCount => (InfoReembDep != null ? InfoReembDep.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCnpjOper() => !string.IsNullOrEmpty(CnpjOper);

        public bool ShouldSerializeRegANS() => !string.IsNullOrEmpty(RegANS);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetReembTit")]
    [ComVisible(true)]
#endif
    public class DetReembTit
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Valor do reembolso relativo ao ano do período indicado em perApur.
        /// Validação: Informação não obrigatória se vlrReembAnt for maior que zero.
        /// </summary>
        [XmlIgnore]
        public double VlrReemb { get; set; }

        [XmlElement("vlrReemb")]
        public string VlrReembField
        {
            get => VlrReemb.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrReemb = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do reembolso relativo a anos anteriores.
        /// Validação: Informação não obrigatória se vlrReemb for maior que zero.
        /// </summary>
        [XmlIgnore]
        public double VlrReembAnt { get; set; }

        [XmlElement("vlrReembAnt")]
        public string VlrReembAntField
        {
            get => VlrReembAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrReembAnt = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrReembField() => VlrReemb > 0;
        public bool ShouldSerializeVlrReembAntField() => VlrReembAnt > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReembDep1210")]
    [ComVisible(true)]
#endif
    public class InfoReembDep1210
    {
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        [XmlElement("detReembDep")]
        public List<DetReembDep> DetReembDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetReembDep(DetReembDep item)
        {
            if (DetReembDep == null)
            {
                DetReembDep = new List<DetReembDep>();
            }

            DetReembDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetReembDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetReembDep</returns>
        public DetReembDep GetDetReembDep(int index)
        {
            if ((DetReembDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetReembDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetReembDep
        /// </summary>
        public int GetDetReembDepCount => (DetReembDep != null ? DetReembDep.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DetReembDep")]
    [ComVisible(true)]
#endif
    public class DetReembDep : DetReembTit { }

}
