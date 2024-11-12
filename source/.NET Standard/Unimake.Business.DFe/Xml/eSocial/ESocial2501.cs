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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2501")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtContProc/v_S_01_02_00", IsNullable = false)]
    public class ESocial2501 : XMLBase
    {
        [XmlElement("evtContProc")]
        public EvtContProc EvtContProc { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtContProc")]
    [ComVisible(true)]
#endif
    public class EvtContProc
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento2501 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideProc")]
        public IdeProc IdeProc { get; set; }

        [XmlElement("ideTrab")]
        public List<IdeTrab2501> IdeTrab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeTrab(IdeTrab2501 item)
        {
            if (IdeTrab == null)
            {
                IdeTrab = new List<IdeTrab2501>();
            }

            IdeTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeTrab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeTrab</returns>
        public IdeTrab2501 GetIdeTrab(int index)
        {
            if ((IdeTrab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeTrab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeTrab
        /// </summary>
        public int GetIdeTrabCount => (IdeTrab != null ? IdeTrab.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2501")]
    [ComVisible(true)]
#endif
    public class IdeEvento2501 : IdeEvento2205 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProc")]
    [ComVisible(true)]
#endif
    public class IdeProc
    {
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime PerApurPgto { get; set; }
#else
        public DateTimeOffset PerApurPgto { get; set; }
#endif

        [XmlElement("perApurPgto")]
        public string PerApurPgtoField
        {
            get => PerApurPgto.ToString("yyyy-MM");
#if INTEROP
            set => PerApurPgto = DateTime.Parse(value);
#else
            set => PerApurPgto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("obs")]
        public string Obs { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeObs() => !string.IsNullOrEmpty(Obs);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrab2501")]
    [ComVisible(true)]
#endif
    public class IdeTrab2501
    {
        [XmlAttribute(AttributeName = "cpfTrab")]
        public string CpfTrab { get; set; }

        [XmlElement("calcTrib")]
        public List<CalcTrib> CalcTrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCalcTrib(CalcTrib item)
        {
            if (CalcTrib == null)
            {
                CalcTrib = new List<CalcTrib>();
            }

            CalcTrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CalcTrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CalcTrib</returns>
        public CalcTrib GetCalcTrib(int index)
        {
            if ((CalcTrib?.Count ?? 0) == 0)
            {
                return default;
            };

            return CalcTrib[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CalcTrib
        /// </summary>
        public int GetCalcTribCount => (CalcTrib != null ? CalcTrib.Count : 0);
#endif

        [XmlElement("infoCRIRRF")]
        public List<InfoCRIRRF> InfoCRIRRF { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRIRRF(InfoCRIRRF item)
        {
            if (InfoCRIRRF == null)
            {
                InfoCRIRRF = new List<InfoCRIRRF>();
            }

            InfoCRIRRF.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRIRRF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRIRRF</returns>
        public InfoCRIRRF GetInfoCRIRRF(int index)
        {
            if ((InfoCRIRRF?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCRIRRF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCRIRRF
        /// </summary>
        public int GetInfoCRIRRFCount => (InfoCRIRRF != null ? InfoCRIRRF.Count : 0);
#endif

        [XmlElement("infoIRComplem")]
        public InfoIRComplem2501 InfoIRComplem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CalcTrib")]
    [ComVisible(true)]
#endif
    public class CalcTrib
    {
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef { get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        [XmlAttribute(AttributeName = "perRef")]
        public string PerRefField
        {
            get => PerRef.ToString("yyyy-MM");
#if INTEROP
            set => PerRef = DateTime.Parse(value);
#else
            set => PerRef = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária
        /// sobre a remuneração mensal do trabalhador.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCpMensal { get; set; }

        [XmlAttribute(AttributeName = "vrBcCpMensal")]
        public string VrBcCpMensalField
        {
            get => VrBcCpMensal.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCpMensal = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária sobre
        /// a remuneração do trabalhador referente ao 13º salário.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrBcCp13 { get; set; }

        [XmlAttribute(AttributeName = "vrBcCp13")]
        public string VrBcCp13Field
        {
            get => VrBcCp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp13 = Converter.ToDouble(value);
        }

        [XmlElement("infoCRContrib")]
        public List<InfoCRContrib> InfoCRContrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRContrib(InfoCRContrib item)
        {
            if (InfoCRContrib == null)
            {
                InfoCRContrib = new List<InfoCRContrib>();
            }

            InfoCRContrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRContrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRContrib</returns>
        public InfoCRContrib GetInfoCRContrib(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRContrib")]
    [ComVisible(true)]
#endif
    public class InfoCRContrib
    {
        [XmlAttribute(AttributeName = "tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor correspondente ao Código de Receita - CR.
        /// Validação: Deve ser informado de acordo com a
        /// legislação em vigor na competência.
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCR { get; set; }

        [XmlAttribute(AttributeName = "vrCR")]
        public string VrCRField
        {
            get => VrCR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCR = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRIRRF")]
    [ComVisible(true)]
#endif
    public class InfoCRIRRF
    {
        [XmlAttribute(AttributeName = "tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor correspondente ao Código de Receita - CR.
        /// Validação: Deve ser informado de acordo com a
        /// legislação em vigor na competência.
        /// Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCR { get; set; }

        [XmlAttribute(AttributeName = "vrCR")]
        public string VrCRField
        {
            get => VrCR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCR = Converter.ToDouble(value);
        }

        [XmlElement("infoIR")]
        public InfoIR InfoIR { get; set; }

        [XmlElement("infoRRA")]
        public InfoRRA2501 InfoRRA { get; set; }

        [XmlElement("dedDepen")]
        public List<DedDepen2501> DedDepen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedDepen(DedDepen2501 item)
        {
            if (DedDepen == null)
            {
                DedDepen = new List<DedDepen2501>();
            }

            DedDepen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedDepen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedDepen</returns>
        public DedDepen2501 GetDedDepen(int index)
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
        public List<PenAlim2501> PenAlim { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddPenAlim(PenAlim2501 item)
        {
            if (PenAlim == null)
            {
                PenAlim = new List<PenAlim2501>();
            }

            PenAlim.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista PenAlim (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da PenAlim</returns>
        public PenAlim2501 GetPenAlim(int index)
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

        [XmlElement("infoProcRet")]
        public List<InfoProcRet2501> InfoProcRet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoProcRet(InfoProcRet2501 item)
        {
            if (InfoProcRet == null)
            {
                InfoProcRet = new List<InfoProcRet2501>();
            }

            InfoProcRet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoProcRet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoProcRet</returns>
        public InfoProcRet2501 GetInfoProcRet(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoIR")]
    [ComVisible(true)]
#endif
    public class InfoIR
    {
        /// <summary>
        /// Valor do rendimento tributável mensal do Imposto de Renda.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRendTrib { get; set; }

        [XmlAttribute(AttributeName = "vrRendTrib")]
        public string VrRendTribField
        {
            get => VrRendTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRendTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do rendimento tributável do Imposto de Renda
        /// referente ao 13º salário - Tributação exclusiva.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// Não informar se infoCRIRRF/tpCR = [188951].
        /// </summary>
        [XmlIgnore]
        public double VrRendTrib13 { get; set; }

        [XmlAttribute(AttributeName = "vrRendTrib13")]
        public string VrRendTrib13Field
        {
            get => VrRendTrib13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRendTrib13 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do rendimento isento por ser portador
        /// de moléstia grave atestada por laudo médico.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRendMoleGrave { get; set; }

        [XmlAttribute(AttributeName = "vrRendMoleGrave")]
        public string VrRendMoleGraveField
        {
            get => VrRendMoleGrave.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRendMoleGrave = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor de parcela isenta de aposentadoria
        /// para beneficiário de 65 anos ou mais.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRendIsen65 { get; set; }

        [XmlAttribute(AttributeName = "vrRendIsen65")]
        public string VrRendInsen65Field
        {
            get => VrRendIsen65.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRendIsen65 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Juros de mora recebidos, devidos pelo atraso no pagamento
        /// de remuneração por exercício de emprego, cargo ou função.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrJurosMora { get; set; }

        [XmlAttribute(AttributeName = "vrJurosMora")]
        public string VrJuroMoraField
        {
            get => VrJurosMora.ToString("F2", CultureInfo.InvariantCulture);
            set => VrJurosMora = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor de outros rendimentos isentos ou não tributáveis.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRendIsenNTrib { get; set; }

        [XmlAttribute(AttributeName = "vrRendIsenNTrib")]
        public string VrRendInsenNTribField
        {
            get => VrRendIsenNTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRendIsenNTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Descrição do rendimento isento ou não
        /// tributável informado em vrRendIsenNTrib.
        /// Validação: Somente informar se vrRendIsenNTrib > 0.
        /// </summary>
        [XmlAttribute(AttributeName = "descIsenNTrib")]
        public string DescIsenNTrib { get; set; }

        /// <summary>
        /// Valor referente à previdência oficial.
        /// Validação: Deve ser maior ou igual a 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrPrevOficial { get; set; }

        [XmlAttribute(AttributeName = "vrPrevOficial")]
        public string VrPrevOficialField
        {
            get => VrPrevOficial.ToString("F2", CultureInfo.InvariantCulture);
            set => VrPrevOficial = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrRendTribField() => VrRendTrib > 0;

        public bool ShouldSerializeVrRendTrib13Field() => VrRendTrib13 > 0;

        public bool ShouldSerializeVrRendMoleGraveField() => VrRendMoleGrave > 0;

        public bool ShouldSerializeVrRendIsen65Field() => VrRendIsen65 > 0;

        public bool ShouldSerializeVrJurosMoraField() => VrJurosMora > 0;

        public bool ShouldSerializeVrRendIsenNTribField() => VrRendIsenNTrib > 0;

        public bool ShouldSerializeDescIsenNTrib() => !string.IsNullOrEmpty(DescIsenNTrib);

        public bool ShouldSerializeVrPrevOficialField() => VrPrevOficial > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA2501")]
    [ComVisible(true)]
#endif
    public class InfoRRA2501
    {
        [XmlAttribute(AttributeName = "descRRA")]
        public string DescRRA { get; set; }

        [XmlAttribute(AttributeName = "qtdMesesRRA")]
        public string QtdMesesRRA { get; set; }

        [XmlElement("despProcJud")]
        public DespProcJud2501 DespProcJud { get; set; }

        [XmlElement("ideAdv")]
        public List<IdeAdv2501> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv2501 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv2501>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv2501 GetIdeAdv(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJud2501")]
    [ComVisible(true)]
#endif
    public class DespProcJud2501
    {
        /// <summary>
        /// Preencher com o valor das despesas com custas judiciais.
        /// </summary>
        [XmlIgnore]
        public double VlrDespCustas { get; set; }

        [XmlAttribute(AttributeName = "vlrDespCustas")]
        public string VlrDespCustasField
        {
            get => VlrDespCustas.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespCustas = Converter.ToDouble(value);

        }

        /// <summary>
        /// Preencher com o valor total das despesas com advogado(s).
        /// </summary>
        [XmlIgnore]
        public double VlrDespAdvogados { get; set; }

        [XmlAttribute(AttributeName = "vlrDespAdvogados")]
        public string VlrDespAdvogadosField
        {
            get => VlrDespAdvogados.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespAdvogados = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdv2501")]
    [ComVisible(true)]
#endif
    public class IdeAdv2501
    {
        [XmlAttribute(AttributeName = "tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlAttribute(AttributeName = "nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Valor da despesa com o advogado, se houver.
        /// </summary>
        [XmlIgnore]
        public double VlrAdv { get; set; }

        [XmlAttribute(AttributeName = "vlrAdv")]
        public string VlrAdvField
        {
            get => VlrAdv.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAdv = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShoudSerializeVlrAdvField() => VlrAdv > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedDepen2501")]
    [ComVisible(true)]
#endif
    public class DedDepen2501
    {
        [XmlAttribute(AttributeName = "tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Preencher com o valor da dedução da base de cálculo.
        /// Validação: O valor informado neste campo deve ser menor ou igual
        /// ao valor unitário da dedução por dependente definido na legislação.
        /// Deve ser maior que 0 (zero).
        /// Em caso de inconsistência na validação, o arquivo será aceito,
        /// porém com alerta ao contribuinte.
        /// </summary>
        [XmlIgnore]
        public double VlrDeducao { get; set; }

        [XmlAttribute(AttributeName = "vlrDeducao")]
        public string VlrDeducaoField
        {
            get => VlrDeducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDeducao = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.PenAlim2501")]
    [ComVisible(true)]
#endif
    public class PenAlim2501
    {
        [XmlAttribute(AttributeName = "tpRend")]
        public TipoDeRendimento TpRend { get; set; }

        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Valor relativo à dedução do rendimento tributável
        /// correspondente a pagamento de pensão alimentícia.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrPensao { get; set; }

        [XmlAttribute(AttributeName = "vlrPensao")]
        public string VlrPensaoField
        {
            get => VlrPensao.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrPensao = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcRet2501")]
    [ComVisible(true)]
#endif
    public class InfoProcRet2501
    {
        [XmlAttribute(AttributeName = "tpProcRet")]
        public TipoProcesso TpProcRet { get; set; }

        [XmlAttribute(AttributeName = "nrProcRet")]
        public string NrProcRet { get; set; }

        [XmlAttribute(AttributeName = "codSusp")]
        public string CodSusp { get; set; }

        [XmlElement("infoValores")]
        public List<InfoValores2501> InfoValores { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoValores(InfoValores2501 item)
        {
            if (InfoValores == null)
            {
                InfoValores = new List<InfoValores2501>();
            }

            InfoValores.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoValores (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoValores</returns>
        public InfoValores2501 GetInfoValores(int index)
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

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoValores2501")]
    [ComVisible(true)]
#endif
    public class InfoValores2501
    {
        [XmlAttribute(AttributeName = "indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        /// <summary>
        /// Valor da retenção que deixou de ser efetuada em 
        /// função de processo administrativo ou judicial.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrNRetido { get; set; }

        [XmlAttribute(AttributeName = "vlrNRetido")]
        public string VlrNRetidoField
        {
            get => VlrNRetido.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrNRetido = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do depósito judicial em função de processo administrativo ou judicial.
        /// Validação: Informação permitida apenas se indDeposito informado em S-1070 for igual a[S].
        ///Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDepJud { get; set; }

        [XmlAttribute(AttributeName = "vlrDepJud")]
        public string VlrDepJudField
        {
            get => VlrDepJud.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDepJud = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da compensação relativa ao ano calendário em função de processo judicial.
        /// Validação: Informação permitida apenas se tpProcRet = [2].
        /// Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrCmpAnoCal { get; set; }

        [XmlAttribute(AttributeName = "vlrCmpAnoCal")]
        public string VlrCmpAnoCalField
        {
            get => VlrCmpAnoCal.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCmpAnoAnt = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da compensação relativa a anos anteriores em função de processo judicial.
        /// Validação: Informação permitida apenas se tpProcRet = [2].
        ///Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrCmpAnoAnt { get; set; }

        [XmlAttribute(AttributeName = "vlrCmpAnoAnt")]
        public string VlrCmpAnoAntField
        {
            get => VlrCmpAnoAnt.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrCmpAnoAnt = Converter.ToDouble(value);
        }


        /// <summary>
        /// Valor do rendimento com exigibilidade suspensa.
        /// Validação: Se indApuracao = [1], não pode ser maior que vrRendTrib.
        /// Se indApuracao = [2], não pode ser maior que vrRendTrib13.
        /// Se informado, deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrRendSusp { get; set; }

        [XmlAttribute(AttributeName = "vlrRendSusp")]
        public string VlrRendSuspField
        {
            get => VlrRendSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrRendSusp = Converter.ToDouble(value);
        }

        [XmlElement("dedSusp")]
        public List<DedSusp2501> DedSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDedSusp(DedSusp2501 item)
        {
            if (DedSusp == null)
            {
                DedSusp = new List<DedSusp2501>();
            }

            DedSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DedSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DedSusp</returns>
        public DedSusp2501 GetDedSusp(int index)
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

        #region ShouldSerialize

        public bool ShouldSerializeVlrNRetidoField() => VlrNRetido > 0;

        public bool ShouldSerializeVlrDepJudField() => VlrDepJud > 0;

        public bool ShouldSerializeVlrCmpAnoCalField() => VlrCmpAnoCal > 0;

        public bool ShouldSerializeVlrCmpAnoAntField() => VlrCmpAnoAnt > 0;

        public bool ShouldSerializeVlrRendSuspField() => VlrRendSusp > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DedSusp2501")]
    [ComVisible(true)]
#endif
    public class DedSusp2501
    {
        [XmlAttribute(AttributeName = "indTpDeducao")]
        public IndicativoTipoDeducao IndTpDeducao { get; set; }

        [XmlIgnore]
        public double VlrDedSusp { get; set; }

        [XmlAttribute(AttributeName = "vlrDedSusp")]
        public string VlrDedSuspField
        {
            get => VlrDedSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDedSusp = Converter.ToDouble(value);
        }

        [XmlElement("benefPen")]
        public List<BenefPen2501> BenefPen { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBenefPen(BenefPen2501 item)
        {
            if (BenefPen == null)
            {
                BenefPen = new List<BenefPen2501>();
            }

            BenefPen.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BenefPen (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BenefPen</returns>
        public BenefPen2501 GetBenefPen(int index)
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

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BenefPen2501")]
    [ComVisible(true)]
#endif
    public class BenefPen2501
    {
        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        /// <summary>
        /// Valor da dedução relativa a dependentes ou a
        /// pensão alimentícia com exigibilidade suspensa.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VlrDepenSusp { get; set; }

        [XmlAttribute(AttributeName = "vlrDepenSusp")]
        public string VlrDepenSuspField
        {
            get => VlrDepenSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDepenSusp = Converter.ToDouble(value);
        }
    }

    public class InfoIRComplem2501
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DtLaudo { get; set; }
#else
        public DateTimeOffset DtLaudo { get; set; }
#endif

        [XmlAttribute(AttributeName = "dtLaudo")]
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
        public List<InfoDep2501> InfoDep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoDep(InfoDep2501 item)
        {
            if (InfoDep == null)
            {
                InfoDep = new List<InfoDep2501>();
            }

            InfoDep.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoDep (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoDep</returns>
        public InfoDep2501 GetInfoDep(int index)
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoDep2501")]
    [ComVisible(true)]
#endif
    public class InfoDep2501
    {
        [XmlAttribute(AttributeName = "cpfDep")]
        public string CpfDep { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtNascto { get; set; }
#else
        public DateTimeOffset DtNascto { get; set; }
#endif

        [XmlAttribute(AttributeName = "dtNascto")]
        public string DtNasctoField
        {
            get => DtNascto.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtNascto = DateTime.Parse(value);
#else
            set => DtNascto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlAttribute(AttributeName = "nome")]
        public string Nome { get; set; }

        [XmlAttribute(AttributeName = "depIRRF")]
        public string DepIRRF { get; set; }

        [XmlIgnore]
#if INTEROP
        public TiposDeDependente TpDep { get; set; } = (TiposDeDependente)(-1);

        [XmlAttribute(AttributeName = "tpDep")]
        public TiposDeDependente TpDepAux
        {
            get => TpDep;
            set => TpDep = value;
        }

#else
        public TiposDeDependente? TpDep { get; set; }

        [XmlAttribute(AttributeName = "tpDep")]
        public TiposDeDependente TpDepAux
        {
            get { return TpDep.GetValueOrDefault((TiposDeDependente)(-1)); }
            set { TpDep = value; }
        }
#endif

        [XmlAttribute(AttributeName = "descrDep")]
        public string DescrDep { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtNasctoField() => DtNascto > DateTime.MinValue;

        public bool ShouldSerializeNome() => !string.IsNullOrEmpty(Nome);

        public bool ShouldSerializeDepIRRF() => !string.IsNullOrEmpty(DepIRRF);

#if INTEROP
        public bool ShouldSerializeTpDepAux() => TpDep != (TiposDeDependente)(-1);
#else
        public bool ShouldSerializeTpDepAux()
    {
        return TpDep.HasValue;
    }
#endif

        public bool ShouldSerializeDescrDep() => !string.IsNullOrEmpty(DescrDep);

        #endregion
    }
}
