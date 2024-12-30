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
    /// <summary>
    /// S-5002 - Imposto de Renda Retido na Fonte por Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5002")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtIrrfBenef/v_S_01_02_00", IsNullable = false)]
    public class ESocial5002 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Imposto de Renda Retido na Fonte por Trabalhador
        /// </summary>
        [XmlElement("evtIrrfBenef")]
        public EvtIrrfBenef EvtIrrfBenef { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Imposto de Renda Retido na Fonte por Trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtIrrfBenef")]
    [ComVisible(true)]
#endif
    public class EvtIrrfBenef
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Identificação do evento de retorno
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento5002 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário do pagamento
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador5002 IdeTrabalhador { get; set; }
    }

    /// <summary>
    /// Identificação do evento de retorno
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5002")]
    [ComVisible(true)]
#endif
    public class IdeEvento5002
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu origem ao presente arquivo de retorno ao empregador
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência das informações
        /// </summary>
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

    /// <summary>
    /// Identificação do beneficiário do pagamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador5002")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador5002
    {
        /// <summary>
        /// Número de inscrição no Cadastro de Pessoas Físicas - CPF do beneficiário do pagamento
        /// </summary>
        [XmlElement("cpfBenef")]
        public string CpfBenef { get; set; }

        /// <summary>
        /// Informações do demonstrativo de valores devidos
        /// </summary>
        [XmlElement("dmDev")]
        public List<DmDev5002> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev5002 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev5002>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev</returns>
        public DmDev5002 GetDmDev(int index)
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
        /// Totalização dos demonstrativos de valores devidos
        /// </summary>
        [XmlElement("totInfoIR")]
        public TotInfoIR TotInfoIR { get; set; }

        /// <summary>
        /// Informações complementares para a DIRF ou para a DAA, com a legislação aplicada ao imposto de renda
        /// </summary>
        [XmlElement("infoIRComplem")]
        public InfoIRComplem5002 InfoIRComplem { get; set; }
    }

    /// <summary>
    /// Informações do demonstrativo de valores devidos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev5002")]
    [ComVisible(true)]
#endif
    public class DmDev5002
    {
        /// <summary>
        /// Período de referência das informações, no formato AAAA-MM (ou AAAA, se for relativo a 13° salário)
        /// </summary>
        [XmlElement("perRef")]
        public string PerRef { get; set; }

        /// <summary>
        /// Identificador atribuído pela fonte pagadora para o demonstrativo de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Informar o evento de origem do pagamento
        /// </summary>
        [XmlElement("tpPgto")]
        public TipoPagamentoESocial TpPgto { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtPgto { get; set; }
#else
        public DateTimeOffset DtPgto { get; set; }
#endif

        /// <summary>
        /// Informar a data de pagamento
        /// </summary>
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

        /// <summary>
        /// Preencher com o código da categoria do trabalhador, conforme Tabela 01
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Rendimentos tributáveis, deduções, isenções e retenções do IRRF
        /// </summary>
        [XmlElement("infoIR")]
        public List<InfoIR5002> InfoIR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIR(InfoIR5002 item)
        {
            if (InfoIR == null)
            {
                InfoIR = new List<InfoIR5002>();
            }

            InfoIR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIR</returns>
        public InfoIR5002 GetInfoIR(int index)
        {
            if ((InfoIR?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoIR[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoIR
        /// </summary>
        public int GetInfoIRCount => (InfoIR != null ? InfoIR.Count : 0);
#endif

        /// <summary>
        /// Totalizador de tributos com período de apuração mensal
        /// </summary>
        [XmlElement("totApurMen")]
        public List<TotApurMen5002> TotApurMen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurMen(TotApurMen5002 item)
        {
            if (TotApurMen == null)
            {
                TotApurMen = new List<TotApurMen5002>();
            }

            TotApurMen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurMen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurMen</returns>
        public TotApurMen5002 GetTotApurMen(int index)
        {
            if ((TotApurMen?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurMen[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurMen
        /// </summary>
        public int GetTotApurMenCount => (TotApurMen != null ? TotApurMen.Count : 0);
#endif

        /// <summary>
        /// Totalizador de tributos com período de apuração diário
        /// </summary>
        [XmlElement("totApurDia")]
        public List<TotApurDia5002> TotApurDia { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTotApurDia(TotApurDia5002 item)
        {
            if (TotApurDia == null)
            {
                TotApurDia = new List<TotApurDia5002>();
            }

            TotApurDia.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TotApurDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TotApurDia</returns>
        public TotApurDia5002 GetTotApurDia(int index)
        {
            if ((TotApurDia?.Count ?? 0) == 0)
            {
                return default;
            };

            return TotApurDia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TotApurDia
        /// </summary>
        public int GetTotApurDiaCount => (TotApurDia != null ? TotApurDia.Count : 0);
#endif

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA5002 InfoRRA { get; set; }

        /// <summary>
        /// Informações complementares relativas a pagamentos a residente fiscal no exterior
        /// </summary>
        [XmlElement("infoPgtoExt")]
        public InfoPgtoExt5002 InfoPgtoExt { get; set; }
    }

    /// <summary>
    /// Rendimentos tributáveis, deduções, isenções e retenções do IRRF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIR5002")]
    [ComVisible(true)]
#endif
    public class InfoIR5002
    {
        /// <summary>
        /// Consolidação dos tipos de valores relativos ao IRRF
        /// </summary>
        [XmlElement("tpInfoIR")]
        public string TpInfoIR { get; set; }

        /// <summary>
        /// Composição do valor do rendimento tributável, não tributável,
        /// retenção, dedução ou isenção do IRRF, de acordo com a
        /// classificação apresentada no campo tpInfoIR.
        /// </summary>
        [XmlIgnore]
        public double Valor { get; set; }
        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Converter.ToDouble(value);
        }

        /// <summary>
        /// Descrição do rendimento não tributável ou isento do IRRF
        /// </summary>
        [XmlElement("descRendimento")]
        public string DescRendimento { get; set; }

        /// <summary>
        /// Informações complementares - Demais rendimentos com exigibilidade suspensa decorrentes de decisão judicial aplicável à rubrica
        /// </summary>
        [XmlElement("infoProcJudRub")]
        public List<InfoProcJudRub> InfoProcJudRub { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcJudRub(InfoProcJudRub item)
        {
            if (InfoProcJudRub == null)
            {
                InfoProcJudRub = new List<InfoProcJudRub>();
            }

            InfoProcJudRub.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcJudRub (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcJudRub</returns>
        public InfoProcJudRub GetInfoProcJudRub(int index)
        {
            if ((InfoProcJudRub?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoProcJudRub[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoProcJudRub
        /// </summary>
        public int GetInfoProcJudRubCount => (InfoProcJudRub != null ? InfoProcJudRub.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeDescRendimento() => !string.IsNullOrEmpty(DescRendimento);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares - Demais rendimentos com exigibilidade suspensa decorrentes de decisão judicial aplicável à rubrica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJudRub")]
    [ComVisible(true)]
#endif
    public class InfoProcJudRub
    {
        /// <summary>
        /// Informar o número do processo judicial
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Identificação da Unidade da Federação - UF da Seção Judiciária
        /// </summary>
        [XmlElement("ufVara")]
        public UFBrasil UfVara { get; set; }

        /// <summary>
        /// Preencher com o código do município, conforme tabela do IBGE
        /// </summary>
        [XmlElement("codMunic")]
        public string CodMunic { get; set; }

        /// <summary>
        /// Código de identificação da Vara
        /// </summary>
        [XmlElement("idVara")]
        public string IdVara { get; set; }
    }

    /// <summary>
    /// Totalizador de tributos com período de apuração mensal
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotApurMen5002")]
    [ComVisible(true)]
#endif
    public class TotApurMen5002
    {
        /// <summary>
        /// Código de Receita - CR relativo ao Imposto de Renda Retido na Fonte sobre rendimentos do trabalho
        /// </summary>
        [XmlElement("CRMen")]
        public string CRMen { get; set; }

        /// <summary>
        /// Valor relativo ao Imposto de Renda Retido
        /// na Fonte sobre rendimentos do trabalho.
        /// </summary>
        [XmlIgnore]
        public double VlrRendTrib { get; set; }

        [XmlElement("vlrRendTrib")]
        public string VlrRendTribField
        {
            get => VlrRendTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao rendimento do 13º salário
        /// </summary>
        [XmlIgnore]
        public double VlrRendTrib13 { get; set; }

        [XmlElement("vlrRendTrib13")]
        public string VlrRendTrib13Field
        {
            get => VlrRendTrib13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendTrib13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo à previdência oficial sobre rendimentos do trabalho, mensal e férias
        /// </summary>
        [XmlIgnore]
        public double VlrPrevOficial { get; set; }

        [XmlElement("vlrPrevOficial")]
        public string VlrPrevOficialField
        {
            get => VlrPrevOficial.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPrevOficial = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo à previdência oficial sobre o 13° salário
        /// </summary>
        [XmlIgnore]
        public double VlrPrevOficial13 { get; set; }

        [XmlElement("vlrPrevOficial13")]
        public string VlrPrevOficial13Field
        {
            get => VlrPrevOficial13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPrevOficial13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao imposto sobre a renda retido na fonte sobre rendimentos do trabalho, mensal e férias
        /// </summary>
        [XmlIgnore]
        public double VlrCRMen { get; set; }

        [XmlElement("vlrCRMen")]
        public string VlrCRMenField
        {
            get => VlrCRMen.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCRMen = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao imposto sobre a renda retido na fonte sobre rendimentos do trabalho, 13° salário
        /// </summary>
        [XmlIgnore]
        public double VlrCRMen13 { get; set; }

        [XmlElement("vlrCRMen13")]
        public string VlrCRMen13Field
        {
            get => VlrCRMen13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCRMen13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo à parcela isenta de proventos de aposentadoria, reserva remunerada, reforma e pensão de beneficiário com 65 anos ou mais
        /// </summary>
        [XmlIgnore]
        public double VlrParcIsenta65 { get; set; }

        [XmlElement("vlrParcIsenta65")]
        public string VlrParcIsenta65Field
        {
            get => VlrParcIsenta65.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrParcIsenta65 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo à parcela isenta de proventos de aposentadoria, reserva remunerada, reforma e pensão de beneficiário com 65 anos ou mais sobre o 13º salário
        /// </summary>
        [XmlIgnore]
        public double VlrParcIsenta65Dec { get; set; }

        [XmlElement("vlrParcIsenta65Dec")]
        public string VlrParcIsenta65DecField
        {
            get => VlrParcIsenta65Dec.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrParcIsenta65Dec = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo a diárias
        /// </summary>
        [XmlIgnore]
        public double VlrDiarias { get; set; }

        [XmlElement("vlrDiarias")]
        public string VlrDiariasField
        {
            get => VlrDiarias.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDiarias = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo a ajuda de custo
        /// </summary>
        [XmlIgnore]
        public double VlrAjudaCusto { get; set; }

        [XmlElement("vlrAjudaCusto")]
        public string VlrAjudaCustoField
        {
            get => VlrAjudaCusto.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAjudaCusto = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo a indenização e rescisão de contrato, inclusive a título de PDV e acidentes de trabalho
        /// </summary>
        [XmlIgnore]
        public double VlrIndResContrato { get; set; }

        [XmlElement("vlrIndResContrato")]
        public string VlrIndResContratoField
        {
            get => VlrIndResContrato.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrIndResContrato = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao abono pecuniário
        /// </summary>
        [XmlIgnore]
        public double VlrAbonoPec { get; set; }

        [XmlElement("vlrAbonoPec")]
        public string VlrAbonoPecField
        {
            get => VlrAbonoPec.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAbonoPec = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao rendimento de beneficiário com moléstia grave ou acidente em serviço - remuneração mensal
        /// </summary>
        [XmlIgnore]
        public double VlrRendMoleGrave { get; set; }

        [XmlElement("vlrRendMoleGrave")]
        public string VlrRendMoleGraveField
        {
            get => VlrRendMoleGrave.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendMoleGrave = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao rendimento de beneficiário com moléstia grave ou acidente em serviço - 13º salário
        /// </summary>
        [XmlIgnore]
        public double VlrRendMoleGrave13 { get; set; }

        [XmlElement("vlrRendMoleGrave13")]
        public string VlrRendMoleGrave13Field
        {
            get => VlrRendMoleGrave13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendMoleGrave13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao auxílio moradia
        /// </summary>
        [XmlIgnore]
        public double VlrAuxMoradia { get; set; }

        [XmlElement("vlrAuxMoradia")]
        public string VlrAuxMoradiaField
        {
            get => VlrAuxMoradia.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAuxMoradia = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo a bolsa médico residente
        /// </summary>
        [XmlIgnore]
        public double VlrBolsaMedico { get; set; }

        [XmlElement("vlrBolsaMedico")]
        public string VlrBolsaMedicoField
        {
            get => VlrBolsaMedico.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBolsaMedico = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo a bolsa médico residente - 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VlrBolsaMedico13 { get; set; }

        [XmlElement("vlrBolsaMedico13")]
        public string VlrBolsaMedico13Field
        {
            get => VlrBolsaMedico13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBolsaMedico13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo aos juros de mora recebidos, devidos pelo atraso no pagamento de remuneração por exercício de emprego, cargo ou função
        /// </summary>
        [XmlIgnore]
        public double VlrJurosMora { get; set; }

        [XmlElement("vlrJurosMora")]
        public string VlrJurosMoraField
        {
            get => VlrJurosMora.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrJurosMora = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo aos rendimentos isentos - outros
        /// </summary>
        [XmlIgnore]
        public double VlrIsenOutros { get; set; }

        [XmlElement("vlrIsenOutros")]
        public string VlrIsenOutrosField
        {
            get => VlrIsenOutros.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrIsenOutros = Converter.ToDouble(value);
        }

        /// <summary>
        /// Descrição do rendimento não tributável ou isento do IRRF
        /// </summary>
        [XmlElement("descRendimento")]
        public string DescRendimento { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDescRendimento() => !string.IsNullOrEmpty(DescRendimento);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Totalizador de tributos com período de apuração diário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotApurDia5002")]
    [ComVisible(true)]
#endif
    public class TotApurDia5002
    {
        /// <summary>
        /// Versão do schema XML - Utilizado somente em tempo de serialização/desserialização, mas não é gerado no XML. Somente de uso interno da DLL para fazer tratamentos entre versões de schemas.
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "S_01_02_00";

        /// <summary>
        /// Retorna somente o valor inteiro da versão para facilitar comparações
        /// </summary>
        private int VersaoSchemaInt => Convert.ToInt32(VersaoSchema.Replace("S_", "").Replace("_", ""));

        /// <summary>
        /// Período de apuração diário do Código de Receita - CR
        /// </summary>
        [XmlElement("perApurDia")]
        public string PerApurDia { get; set; }

        /// <summary>
        /// Código de Receita - CR relativo ao Imposto de Renda Retido na Fonte sobre rendimentos do trabalho pagos a residente no exterior para fins fiscais
        /// </summary>
        [XmlElement("CRDia")]
        public string CRDia { get; set; }

        /// <summary>
        /// Forma de tributação, conforme opções disponíveis na Tabela 30
        /// </summary>
        [XmlElement("frmTribut")]
        public FrmTribut FrmTribut { get; set; }

        /// <summary>
        /// Código do país de residência para fins fiscais, quando no exterior, conforme Tabela 06.
        /// </summary>
        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        /// <summary>
        /// Valor pago a residente ou domiciliado no exterior
        /// </summary>
        [XmlIgnore]
        public double VlrPagoDia { get; set; }

        [XmlElement("vlrPagoDia")]
        public string VlrPagoDiaField
        {
            get => VlrPagoDia.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPagoDia = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor relativo ao Imposto de Renda Retido na Fonte
        /// sobre rendimentos do trabalho pagos a residente,
        /// para fins fiscais, no exterior.
        /// </summary>
        [XmlIgnore]
        public double VlrCRDia { get; set; }

        [XmlElement("vlrCRDia")]
        public string VlrCRDiaField
        {
            get => VlrCRDia.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCRDia = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeFrmTribut() => VersaoSchemaInt >= 10300;
        
        public bool ShouldSerializePaisResidExt() => VersaoSchemaInt >= 10300;

        public bool ShouldSerializeVlrPagoDiaField() => VersaoSchemaInt >= 10300;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA5002")]
    [ComVisible(true)]
#endif
    public class InfoRRA5002
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo
        /// </summary>
        [XmlElement("tpProcRRA")]
        public TipoProcesso TpProcRRA { get; set; }

        /// <summary>
        /// Informar o número do processo/requerimento administrativo/judicial
        /// </summary>
        [XmlElement("nrProcRRA")]
        public string NrProcRRA { get; set; }

        /// <summary>
        /// Descrição dos Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("descRRA")]
        public string DescRRA { get; set; }

        /// <summary>
        /// Número de meses relativo aos Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("qtdMesesRRA")]
        public string QtdMesesRRA { get; set; }

        /// <summary>
        /// Detalhamento das despesas com processo judicial
        /// </summary>
        [XmlElement("despProcJud")]
        public DespProcJud5002 DespProcJud { get; set; }

        /// <summary>
        /// Identificação dos advogados
        /// </summary>
        [XmlElement("ideAdv")]
        public List<IdeAdv5002> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv5002 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv5002>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv5002 GetIdeAdv(int index)
        {
            if ((IdeAdv?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeAdv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeAdv
        /// </summary>
        public int GetIdeAdvCount => (IdeAdv != null ? IdeAdv.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNrProcRRA() => !string.IsNullOrEmpty(NrProcRRA);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Detalhamento das despesas com processo judicial
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJud5002")]
    [ComVisible(true)]
#endif
    public class DespProcJud5002
    {
        /// <summary>
        /// Preencher com o valor das despesas com custas judiciais
        /// </summary>
        [XmlElement("vlrDespCustas")]
        public double VlrDespCustas { get; set; }

        /// <summary>
        /// Preencher com o valor total das despesas com advogado(s)
        /// </summary>
        [XmlElement("vlrDespAdvogados")]
        public double VlrDespAdvogados { get; set; }
    }

    /// <summary>
    /// Identificação dos advogados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdv5002")]
    [ComVisible(true)]
#endif
    public class IdeAdv5002
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do advogado
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Valor da despesa com o advogado, se houver.
        /// </summary>
        [XmlIgnore]
        public double VlrAdv { get; set; }

        [XmlElement("vlrAdv")]
        public string VlrAdvField
        {
            get => VlrAdv.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAdv = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShoulSerializeVlrAdvField() => VlrAdv > 0;

        #endregion ShouldSerialize

    }

    /// <summary>
    /// Informações complementares relativas a pagamentos a residente fiscal no exterior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPgtoExt5002")]
    [ComVisible(true)]
#endif
    public class InfoPgtoExt5002
    {
        /// <summary>
        /// Código do país de residência para fins fiscais, quando no exterior, conforme Tabela 06
        /// </summary>
        [XmlElement("paisResidExt")]
        public string PaisResidExt { get; set; }

        /// <summary>
        /// Indicativo do Número de Identificação Fiscal (NIF)
        /// </summary>
        [XmlElement("indNIF")]
        public IndicativoNIF IndNIF { get; set; }

        /// <summary>
        /// Número de Identificação Fiscal (NIF)
        /// </summary>
        [XmlElement("nifBenef")]
        public string NifBenef { get; set; }

        /// <summary>
        /// Forma de tributação, conforme opções disponíveis na Tabela 30
        /// </summary>
        [XmlElement("frmTribut")]
        public FrmTribut FrmTribut { get; set; }

        /// <summary>
        /// Endereço do beneficiário residente ou domiciliado no exterior
        /// </summary>
        [XmlElement("endExt")]
        public EndExt5002 EndExt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNifBenef() => !string.IsNullOrEmpty(NifBenef);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Endereço do beneficiário residente ou domiciliado no exterior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EndExt5002")]
    [ComVisible(true)]
#endif
    public class EndExt5002 : EndExt1210 { }

    /// <summary>
    /// Totalização dos demonstrativos de valores devidos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TotInfoIR")]
    [ComVisible(true)]
#endif
    public class TotInfoIR
    {
        /// <summary>
        /// Totalizador de valores com período de apuração mensal
        /// </summary>
        [XmlElement("consolidApurMen")]
        public ConsolidApurMen ConsolidApurMen { get; set; }
    }

    /// <summary>
    /// Totalizador de valores com período de apuração mensal
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ConsolidApurMen")]
    [ComVisible(true)]
#endif
    public class ConsolidApurMen : TotApurMen5002 { }

    /// <summary>
    /// Informações complementares para a DIRF ou para a DAA, com a legislação aplicada ao imposto de renda
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRComplem5002")]
    [ComVisible(true)]
#endif
    public class InfoIRComplem5002
    {
        /// <summary>
        /// Data da moléstia grave atribuída pelo laudo
        /// </summary>
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

        /// <summary>
        /// Identificação do evento S-1210 original e perRef cujas informações de infoIRComplem serão alteradas
        /// </summary>
        [XmlElement("perAnt")]
        public PerAnt5002 PerAnt { get; set; }

        /// <summary>
        /// Identificação dos dependentes
        /// </summary>
        [XmlElement("ideAdv")]
        public List<IdeDep5002> IdeDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeDep5002 item)
        {
            if (IdeDep == null)
            {
                IdeDep = new List<IdeDep5002>();
            }

            IdeDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeDep</returns>
        public IdeDep5002 GetIdeDep(int index)
        {
            if ((IdeDep?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeDep[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeDep
        /// </summary>
        public int GetIdeDepCount => (IdeDep != null ? IdeDep.Count : 0);
#endif

        /// <summary>
        /// Informações de Imposto de Renda, por Código de Receita - CR
        /// </summary>
        [XmlElement("infoIRCR")]
        public List<InfoIRCR5002> InfoIRCR { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoIRCR(InfoIRCR5002 item)
        {
            if (InfoIRCR == null)
            {
                InfoIRCR = new List<InfoIRCR5002>();
            }

            InfoIRCR.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoIRCR (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoIRCR</returns>
        public InfoIRCR5002 GetInfoIRCR(int index)
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

        /// <summary>
        /// Plano de saúde coletivo
        /// </summary>
        [XmlElement("planSaude")]
        public List<PlanSaude5002> PlanSaude { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPlanSaude(PlanSaude5002 item)
        {
            if (PlanSaude == null)
            {
                PlanSaude = new List<PlanSaude5002>();
            }

            PlanSaude.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PlanSaude5002 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PlanSaude</returns>
        public PlanSaude5002 GetPlanSaude(int index)
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

        /// <summary>
        /// Reembolsos de despesas médicas
        /// </summary>
        [XmlElement("infoReembMed")]
        public List<InfoReembMed5002> InfoReembMed { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoReembMed(InfoReembMed5002 item)
        {
            if (InfoReembMed == null)
            {
                InfoReembMed = new List<InfoReembMed5002>();
            }

            InfoReembMed.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoReembMed5002 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoReembMed</returns>
        public InfoReembMed5002 GetInfoReembMed(int index)
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

    }

    /// <summary>
    /// Identificação do evento S-1210 original e perRef cujas informações de infoIRComplem serão alteradas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PerAnt5002")]
    [ComVisible(true)]
#endif
    public class PerAnt5002 : PerAnt1210 { }

    /// <summary>
    /// Identificação dos dependentes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeDep5002")]
    [ComVisible(true)]
#endif
    public class IdeDep5002
    {
        /// <summary>
        /// CPF do dependente
        /// </summary>
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Este campo somente é informado em caso de dependente do trabalhador para fins de dedução 
        /// de seu rendimento tributável pelo Imposto de Renda
        /// </summary>
        [XmlElement("depIRRF")]
#if INTEROP
        public SimNaoLetra DepIRRF { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? DepIRRF { get; set; }
#endif

        /// <summary>
        /// Data de nascimento do dependente
        /// </summary>
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

        /// <summary>
        /// Nome do dependente
        /// </summary>
        [XmlElement("nome")]
        public string Nome { get; set; }

        /// <summary>
        /// Relação de dependência
        /// </summary>
        [XmlElement("tpDep")]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);
#else
        public TiposDeDependente? TpDep { get; set; }
#endif

        /// <summary>
        /// Informar a descrição da dependência
        /// </summary>
        [XmlElement("descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeDepIRRF() => DepIRRF != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeDepIRRF() => DepIRRF != null;
#endif

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);

#if INTEROP
        public bool ShouldSerializeTpDep() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDep() => TpDep != null;
#endif

        public bool ShouldSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de Imposto de Renda, por Código de Receita - CR
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIRCR5002")]
    [ComVisible(true)]
#endif
    public class InfoIRCR5002
    {
        /// <summary>
        /// Código de Receita - CR relativo ao Imposto de Renda Retido na Fonte sobre rendimentos do trabalho
        /// </summary>
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        /// <summary>
        /// Dedução do rendimento tributável relativa a dependentes
        /// </summary>
        [XmlElement("dedDepen")]
        public List<DedDepen5002> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepen5002 item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepen5002>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepen5002 GetDedDepen(int index)
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

        /// <summary>
        /// Informação dos beneficiários da pensão alimentícia
        /// </summary>
        [XmlElement("penAlim")]
        public List<PenAlim5002> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlim5002 item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlim5002>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlim5002 GetPenAlim(int index)
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

        /// <summary>
        /// Informações relativas a planos de previdência complementar
        /// </summary>
        [XmlElement("previdCompl")]
        public List<PrevidCompl5002> PrevidCompl { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPrevidCompl(PrevidCompl5002 item)
        {
            if (PrevidCompl == null)
            {
                PrevidCompl = new List<PrevidCompl5002>();
            }

            PrevidCompl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PrevidCompl5002 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PrevidCompl</returns>
        public PrevidCompl5002 GetPrevidCompl(int index)
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

        /// <summary>
        /// Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais
        /// </summary>
        [XmlElement("infoProcRet")]
        public List<InfoProcRet5002> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRet5002 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRet5002>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet5002 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRet5002 GetInfoProcRet(int index)
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

    /// <summary>
    /// Dedução do rendimento tributável relativa a dependentes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepen5002")]
    [ComVisible(true)]
#endif
    public class DedDepen5002
    {
        /// <summary>
        /// Tipo de rendimento
        /// </summary>
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        /// <summary>
        /// Informar o número de inscrição do dependente no CPF
        /// </summary>
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Preencher com o valor da dedução da base de cálculo.
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

    /// <summary>
    /// Informação dos beneficiários da pensão alimentícia
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PenAlim5002")]
    [ComVisible(true)]
#endif
    public class PenAlim5002
    {
        /// <summary>
        /// Tipo de rendimento
        /// </summary>
        [XmlElement("tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        /// <summary>
        /// Número do CPF do dependente/beneficiário da pensão alimentícia
        /// </summary>
        [XmlElement("cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Valor relativo à dedução do rendimento tributável correspondente a pagamento de pensão alimentícia
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

    /// <summary>
    /// Informações relativas a planos de previdência complementar
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PrevidCompl5002")]
    [ComVisible(true)]
#endif
    public class PrevidCompl5002 
    {
        /// <summary>
        /// Tipo de previdência complementar
        /// </summary>
        [XmlElement("tpPrev")]
        public TipoDePrevidenciaComplementar TpPrev { get; set; }

        /// <summary>
        /// Número de inscrição da entidade de previdência complementar
        /// </summary>
        [XmlElement("cnpjEntidPC")]
        public string CnpjEntidPC { get; set; }

        /// <summary>
        /// Valor da dedução mensal relativa a previdência complementar.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDedPC { get; set; }

        [XmlElement("vlrDedPC")]
        public string VlrDedPCField
        {
            get => VlrDedPC.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedPC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da dedução do 13º Salário relativa a previdência complementar
        /// </summary>
        [XmlIgnore]
        public double VlrDedPC13 { get; set; }

        [XmlElement("vlrDedPC13")]
        public string VlrDedPC13Field
        {
            get => VlrDedPC13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedPC13 = Converter.ToDouble(value);
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

        /// <summary>
        /// Valor da contribuição do 13º Salário do ente público patrocinador da Fundação de Previdência Complementar do Servidor Público (Funpresp).
        /// </summary>
        [XmlIgnore]
        public double VlrPatrocFunp13 { get; set; }

        [XmlElement("vlrPatrocFunp13")]
        public string VlrPatrocFunp13Field
        {
            get => VlrPatrocFunp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPatrocFunp13 = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVlrDedPCField() => VlrDedPC > 0;

        public bool ShouldSerializeVlrDedPC13Field() => VlrDedPC13 > 0;

        public bool ShouldSerializeVlrPatrocFunpField() => VlrPatrocFunp > 0;

        public bool ShouldSerializeVlrPatrocFunp13Field() => VlrPatrocFunp13 > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de processos relacionados a não retenção de tributos ou a depósitos judiciais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcRet5002")]
    [ComVisible(true)]
#endif
    public class InfoProcRet5002 : InfoProcRet1210 { }

    /// <summary>
    /// Plano de saúde coletivo.
    /// Identificação da(s) operadora(s) de plano privado coletivo empresarial de assistência à saúde
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PlanSaude5002")]
    [ComVisible(true)]
#endif
    public class PlanSaude5002 : PlanSaude1210 { }

    /// <summary>
    /// Informações relativas a reembolsos efetuados no período de apuração (perApur) pelo empregador ao 
    /// trabalhador referente a despesas médicas ou odontológicas pagas pelo trabalhador a prestadores de serviços de saúde
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReembMed5002")]
    [ComVisible(true)]
#endif
    public class InfoReembMed5002 : InfoReembMed1210 { }
}
