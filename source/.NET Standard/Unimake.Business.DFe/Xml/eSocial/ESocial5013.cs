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
    /// S-5013 - Informações do FGTS Consolidadas por Contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5013")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtFGTS/v_S_01_03_00", IsNullable = false)]

    public class ESocial5013 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Informações do FGTS Consolidadas por Contribuinte
        /// </summary>
        [XmlElement("evtFGTS")]
        public EvtFGTS EvtFGTS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Informações do FGTS Consolidadas por Contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtFGTS")]
    [ComVisible(true)]
#endif
    public class EvtFGTS
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
        public IdeEvento5013 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações relativas ao Fundo de Garantia do Tempo de Serviço - FGTS
        /// </summary>
        [XmlElement("infoFGTS")]
        public InfoFGTS5013 InfoFGTS { get; set; }
    }

    /// <summary>
    /// Identificação do evento de retorno
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5013")]
    [ComVisible(true)]
#endif
    public class IdeEvento5013 : IdeEvento5011 { }

    /// <summary>
    /// Informações relativas ao Fundo de Garantia do Tempo de Serviço - FGTS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoFGTS5013")]
    [ComVisible(true)]
#endif
    public class InfoFGTS5013
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu origem ao presente arquivo de retorno ao empregador
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        /// <summary>
        /// Indicativo de existência de FGTS
        /// </summary>
        [XmlElement("indExistInfo")]
        public IndExistInfo IndExistInfo { get; set; }

        /// <summary>
        /// Identificação do estabelecimento ou obra de construção civil
        /// </summary>
        [XmlElement("ideEstab")]
        public List<IdeEstab5013> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab5013 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab5013>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstab5013 GetIdeEstab(int index)
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
    }

    /// <summary>
    /// Identificação do estabelecimento ou obra de construção civil
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab5013")]
    [ComVisible(true)]
#endif
    public class IdeEstab5013
    {
        /// <summary>
        /// Valores válidos:
        /// 1 - CNPJ,
        /// 2 - CPF,
        /// 3 - CAEPF,
        /// 4 - CNO
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo ideEstab/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Identificação da lotação tributária
        /// </summary>
        [XmlElement("ideLotacao")]
        public List<IdeLotacao5013> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacao5013 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacao5013>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacao5013 GetIdeLotacao(int index)
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
    }

    /// <summary>
    /// Identificação da lotação tributária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacao5013")]
    [ComVisible(true)]
#endif
    public class IdeLotacao5013
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária
        /// </summary>
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de lotação, conforme Tabela 10
        /// </summary>
        [XmlElement("tpLotacao")]
        public TpLotacao TpLotacao { get; set; }

        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
#if INTEROP
        public TpInsc TpInsc { get; set; } = (TpInsc)(-1);
#else
        public TiposInscricao? TpInsc { get; set; }
#endif

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ, CPF, CNO) ao qual pertence a lotação tributária, conforme indicado na Tabela 10
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações referentes a bases de cálculo e valores do FGTS no estabelecimento/lotação
        /// </summary>
        [XmlElement("infoBaseFGTS")]
        public InfoBaseFGTS5013 InfoBaseFGTS { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeTpInsc() => TpInsc != (TpInsc)(-1);
#else
        public bool ShouldSerializeTpInsc() => TpInsc != null;
#endif

        public bool ShouldSerializeNrInsc() => !string.IsNullOrEmpty(NrInsc);

        #endregion
    }

    /// <summary>
    /// Informações referentes a bases de cálculo e valores do FGTS no estabelecimento/lotação
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseFGTS5013")]
    [ComVisible(true)]
#endif
    public class InfoBaseFGTS5013
    {
        /// <summary>
        /// Informações consolidadas das bases de cálculo e valores do FGTS do período de apuração e de períodos anteriores, exceto se {tpAcConv} = [E, H, I].
        /// </summary>
        [XmlElement("basePerApur")]
        public List<BasePerApur5013> BasePerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerApur(BasePerApur5013 item)
        {
            if (BasePerApur == null)
            {
                BasePerApur = new List<BasePerApur5013>();
            }

            BasePerApur.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerApur (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerApur</returns>
        public BasePerApur5013 GetBasePerApur(int index)
        {
            if ((BasePerApur?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasePerApur[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasePerApur
        /// </summary>
        public int GetBasePerApurCount => (BasePerApur != null ? BasePerApur.Count : 0);
#endif

        /// <summary>
        /// Informações referentes a bases de cálculo e valores do FGTS de períodos anteriores quando tpAcConv = [E, H, I]
        /// </summary>
        [XmlElement("infoBasePerAntE")]
        public List<InfoBasePerAntE5013> InfoBasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBasePerAntE(InfoBasePerAntE5013 item)
        {
            if (InfoBasePerAntE == null)
            {
                InfoBasePerAntE = new List<InfoBasePerAntE5013>();
            }

            InfoBasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBasePerAntE</returns>
        public InfoBasePerAntE5013 GetInfoBasePerAntE(int index)
        {
            if ((InfoBasePerAntE?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoBasePerAntE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoBasePerAntE
        /// </summary>
        public int GetInfoBasePerAntECount => (InfoBasePerAntE != null ? InfoBasePerAntE.Count : 0);
#endif

    }

    /// <summary>
    /// Informações consolidadas das bases de cálculo e valores do FGTS do período de apuração e de períodos anteriores, exceto se {tpAcConv} = [E, H, I]
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerApur5013")]
    [ComVisible(true)]
#endif
    public class BasePerApur5013
    {
        /// <summary>
        /// Tipo de valor que influi na apuração do FGTS
        /// </summary>
        [XmlElement("tpValor")]
        public TpValor TpValor { get; set; }

        /// <summary>
        /// Valores válidos:
        /// 1 - Normal (incidência de FGTS)
        /// 9 - Incidência de FGTS suspensa em decorrência de decisão judicial
        /// </summary>
        [XmlElement("indIncid")]
        public IndIncid IndIncid { get; set; }

        /// <summary>
        /// Remuneração (valor da base de cálculo) do FGTS.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double BaseFGTS { get; set; }

        [XmlElement("baseFGTS")]
        public string BaseFGTSString
        {
            get => BaseFGTS.ToString("F2", CultureInfo.InvariantCulture);
            set => BaseFGTS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado
        /// na conta vinculada do trabalhador.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrFGTS { get; set; }

        [XmlElement("vrFGTS")]
        public string VrFGTSField
        {
            get => VrFGTS.ToString("F2", CultureInfo.InvariantCulture);
            set => VrFGTS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrFGTSField() => VrFGTS > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações referentes a bases de cálculo e valores do FGTS de períodos anteriores quando tpAcConv = [E, H, I].
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBasePerAntE5013")]
    [ComVisible(true)]
#endif
    public class InfoBasePerAntE5013
    {
        /// <summary>
        /// Informar o período ao qual se refere a remuneração no formato AAAA-MM
        /// </summary>
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

        /// <summary>
        /// Tipo do instrumento ou situação ensejadora da remuneração relativa a períodos de apuração anteriores
        /// </summary>
        [XmlElement("tpAcConv")]
        public string TpAcConv { get; set; }

        /// <summary>
        /// Informações consolidadas das bases de cálculo e valores do FGTS de períodos anteriores quando tpAcConv = [E, H, I]
        /// </summary>
        [XmlElement("basePerAntE")]
        public List<BasePerAntE5013> BasePerAntE { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasePerAntE(BasePerAntE5013 item)
        {
            if (BasePerAntE == null)
            {
                BasePerAntE = new List<BasePerAntE5013>();
            }

            BasePerAntE.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasePerAntE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasePerAntE</returns>
        public BasePerAntE5013 GetBasePerAntE(int index)
        {
            if ((BasePerAntE?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasePerAntE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasePerAntE
        /// </summary>
        public int GetBasePerAntECount => (BasePerAntE != null ? BasePerAntE.Count : 0);
#endif
    }

    /// <summary>
    /// Informações consolidadas das bases de cálculo e valores do FGTS de períodos anteriores quando tpAcConv = [E, H, I]
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasePerAntE5013")]
    [ComVisible(true)]
#endif
    public class BasePerAntE5013
    {
        /// <summary>
        /// Tipo de valor que influi na apuração do FGTS
        /// </summary>
        [XmlElement("tpValorE")]
        public TpValor TpValorE { get; set; }

        /// <summary>
        /// Valores válidos:
        /// 1 - Normal (incidência de FGTS)
        /// 9 - Incidência de FGTS suspensa em decorrência de decisão judicial
        /// </summary>
        [XmlElement("indIncidE")]
        public IndIncid IndIncidE { get; set; }

        /// <summary>
        /// Remuneração (valor da base de cálculo) do FGTS.
        /// Validação: Deve ser maior que 0 (zero).
        /// Deve corresponder ao somatório dos valores informados no campo remFGTSE
        /// do evento de origem, agrupados por tpValorE e indIncidE.
        /// </summary>
        [XmlIgnore]
        public double BaseFGTSE { get; set; }

        [XmlElement("baseFGTSE")]
        public string BaseFGTSEField
        {
            get => BaseFGTSE.ToString("F2", CultureInfo.InvariantCulture);
            set => BaseFGTSE = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor histórico do FGTS a ser depositado na conta vinculada do trabalhador.
        /// Validação: Deve ser maior que 0 (zero).
        /// Deve corresponder ao somatório dos valores informados no campo
        /// dpsFGTSE do evento de origem, agrupados por tpValorE.
        /// </summary>
        [XmlIgnore]
        public double VrFGTSE { get; set; }

        [XmlElement("vrFGTSE")]
        public string VrFGTSEField
        {
            get => VrFGTSE.ToString("F2", CultureInfo.InvariantCulture);
            set => VrFGTSE = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrFGTSEField() => VrFGTSE > 0;

        #endregion ShouldSerialize
    }
}
