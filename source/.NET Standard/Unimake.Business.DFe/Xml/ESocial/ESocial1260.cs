#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1260 - Comercialização da Produção Rural Pessoa Física
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1260")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtComProd/v_S_01_02_00", IsNullable = false)]
    public class ESocial1260 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Comercialização da Produção Rural Pessoa Física
        /// </summary>
        [XmlElement("evtComProd")]
        public EvtComProd1260 EvtComProd { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

    /// <summary>
    /// Evento Comercialização da Produção Rural Pessoa Física
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtComProd1260")]
    [ComVisible(true)]
#endif
    public class EvtComProd1260
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento1260 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informação da comercialização de produção
        /// </summary>
        [XmlElement("infoComProd")]
        public InfoComProd1260 InfoComProd { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1260")]
    [ComVisible(true)]
#endif
    public class IdeEvento1260 : IdeEvento1210 { }

    /// <summary>
    /// Informação da comercialização de produção
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComProd1260")]
    [ComVisible(true)]
#endif
    public class InfoComProd1260
    {
        /// <summary>
        /// Identificação do estabelecimento que comercializou a produção
        /// </summary>
        [XmlElement("ideEstabel")]
        public IdeEstabel1260 IdeEstabel { get; set; }
    }

    /// <summary>
    /// Identificação do estabelecimento que comercializou a produção
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab1260")]
    [ComVisible(true)]
#endif
    public class IdeEstabel1260
    {
        /// <summary>
        /// Preencher com o número de inscrição no CAEPF do estabelecimento rural.
        /// Validação: Deve ser um número de inscrição válido e
        /// existente na Tabela de Estabelecimentos (S-1005).
        /// </summary>
        [XmlElement("nrInscEstabRural")]
        public string NrInscEstabRural { get; set; }

        /// <summary>
        /// Valor total da comercialização por "tipo" de comercialização.
        /// </summary>
        [XmlElement("tpComerc")]
        public List<TpComerc> TpComerc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddTpComerc(TpComerc item)
        {
            if (TpComerc == null)
            {
                TpComerc = new List<TpComerc>();
            }

            TpComerc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista TpComerc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da TpComerc</returns>
        public TpComerc GetTpComerc(int index)
        {
            if ((TpComerc?.Count ?? 0) == 0)
            {
                return default;
            };

            return TpComerc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista TpComerc
        /// </summary>
        public int GetTpComercCount => (TpComerc != null ? TpComerc.Count : 0);
#endif
    }

    /// <summary>
    /// Valor total da comercialização por "tipo" de comercialização.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.TpComerc")]
    [ComVisible(true)]
#endif
    public class TpComerc
    {
        /// <summary>
        /// Indicativo de comercialização.
        /// Valores válidos:
        /// 2 - Comercialização da produção efetuada diretamente no
        /// varejo a consumidor final ou a outro produtor rural pessoa
        /// física por produtor rural pessoa física, inclusive por
        /// segurado especial, ou por pessoa física não produtor rural
        /// 3 - Comercialização da produção por prod.rural PF/seg.
        /// especial - Vendas a PJ(exceto entidade inscrita no
        /// Programa de Aquisição de Alimentos - PAA) ou a
        /// intermediário PF
        /// 7 - Comercialização da produção isenta de acordo com a
        /// Lei 13.606/2018 efetuada diretamente no varejo a
        /// consumidor final ou a outro produtor rural pessoa física
        /// por produtor rural pessoa física, inclusive por segurado
        /// especial, ou por pessoa física não produtor rural
        /// 8 - Comercialização da produção da pessoa física/segurado especial para entidade inscrita no PAA
        /// 9 - Comercialização da produção no mercado externo
        /// </summary>
        [XmlElement("indComerc")]
        public IndComerc IndComerc { get; set; }

        /// <summary>
        /// Preencher com o valor total da comercialização.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrTotCom { get; set; }

        [XmlElement("vrTotCom")]
        public string VrTotCombField
        {
            get => VrTotCom.ToString("F2", CultureInfo.InvariantCulture);
            set => VrTotCom = Converter.ToDouble(value);
        }

        /// <summary>
        /// Identificação dos adquirentes da produção.
        /// </summary>
        [XmlElement("ideAdquir")]
        public IdeAdquir IdeAdquir { get; set; }

        /// <summary>
        /// Informações de processos judiciais com decisão/sentença
        /// favorável ao contribuinte e relativos à contribuição
        /// incidente sobre a comercialização.
        /// </summary>
        [XmlElement("infoProcJud")]
        public InfoProcJud1260 InfoProcJud { get; set; }

    }

    /// <summary>
    /// Identificação dos adquirentes da produção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdquir")]
    [ComVisible(true)]
#endif
    public class IdeAdquir
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição
        /// 1 - CNPJ
        /// 2 - CPF
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo
        /// ideAdquir/tpInsc.
        /// Validação: A inscrição informada deve ser compatível com
        /// o ideAdquir/tpInsc e diferente da inscrição do declarante.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Preencher com o valor total da comercialização.
        /// Validação: Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrComerc { get; set; }

        [XmlElement("vrComerc")]
        public string VrComercField
        {
            get => VrComerc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrComerc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Detalhamento das notas fiscais relativas à comercialização
        /// de produção com o adquirente identificado no grupo superior
        /// </summary>
        [XmlElement("nfs")]
        public Nfs Nfs { get; set; }

    }

    /// <summary>
    /// Detalhamento das notas fiscais relativas à comercialização
    /// de produção com o adquirente identificado no grupo superior
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Nfs")]
    [ComVisible(true)]
#endif
    public class Nfs
    {
        /// <summary>
        /// Informar o número de série da nota fiscal/fatura.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da nota fiscal/fatura
        /// </summary>
        [XmlElement("nrDocto")]
        public string NrDocto { get; set; }

        /// <summary>
        /// Data de emissão da nota fiscal/fatura. Validação: O mês/ano da emissão da nota fiscal deve ser
        /// igual ao mês/ano indicado no registro de abertura do arquivo
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEmisNF { get; set; }
#else
        public DateTimeOffset DtEmisNF { get; set; }
#endif

        [XmlElement("dtEmisNF")]
        public string DtEmisNFField
        {
            get => DtEmisNF.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEmisNF = DateTime.Parse(value);
#else
            set => DtEmisNF = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o valor bruto da(s) nota(s) fiscal(is).
        /// </summary>
        [XmlIgnore]
        public double VlrBruto { get; set; }

        [XmlElement("vlrBruto")]
        public string VlrBrutoField
        {
            get => VlrBruto.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrBruto = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com o valor da contribuição previdenciária descontada pelo adquirente na comercialização de
        /// produção.Se não houver informação, preencher com 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCPDescPR { get; set; }

        [XmlElement("vrCPDescPR")]
        public string VrCPDescPRField
        {
            get => VrCPDescPR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCPDescPR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao financiamento dos benefícios concedidos 
        /// em razão do grau de incidência da incapacidade laborativa decorrente dos riscos ambientais
        /// do trabalho, incidente sobre a comercialização de
        /// produção rural de produtor rural.Se não houver informação, preencher com 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRatDescPR { get; set; }

        [XmlElement("vrRatDescPR")]
        public string VrRatDescPRField
        {
            get => VrRatDescPR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRatDescPR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao SENAR, incidente sobre
        /// a comercialização de produção rural de produtor rural
        /// pessoa física/segurado especial.Se não houver
        /// informação, preencher com 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrSenarDesc { get; set; }

        [XmlElement("vrSenarDesc")]
        public string VrSenarDescField
        {
            get => VrSenarDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSenarDesc = Converter.ToDouble(value);
        }

        #region ShouldSerialize 

        public bool ShouldSerializeSerie() => !string.IsNullOrEmpty(Serie);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de processos judiciais com decisão/sentença
    /// favorável ao contribuinte e relativos à contribuição
    /// incidente sobre a comercialização.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoProcJud1260")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class InfoProcJud1260
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de processo.
        /// Valores válidos:
        /// 1 - Administrativo
        /// 2 - Judicial
        /// </summary>
        [XmlElement("tpProc")]
        public TipoProcesso TipoProcesso { get; set; }

        /// <summary>
        /// Informar um número de processo cadastrado através do evento S-1070, cujo indMatProc seja igual a [1].
        /// Validação: Deve ser um número de processo
        /// administrativo ou judicial válido e existente na Tabela de Processos(S-1070), com indMatProc = [1].
        /// </summary>
        [XmlElement("nrProc")]
        public string NrProc { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070. 
        /// Validação: A informação prestada deve estar de acordo com o que foi informado em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }

        /// <summary>
        /// Valor da contribuição previdenciária com exigibilidade suspensa. 
        /// Validação: Preenchimento obrigatório se vrRatSusp e vrSenarSusp não tiverem sido preenchidos.
        /// Deve ser um valor maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCPSusp { get; set; }

        [XmlElement("vrCPSusp")]
        public string VrCPSuspField
        {
            get => VrCPSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCPSusp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição para GILRAT com exigibilidade suspensa.
        /// Validação: Preenchimento obrigatório se vrCPSusp e vrSenarSusp não tiverem sido preenchidos.
        /// Deve ser um valor maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrRatSusp { get; set; }

        [XmlElement("vrRatSusp")]
        public string VrRatSuspField
        {
            get => VrRatSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRatSusp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição para o SENAR com exigibilidade suspensa.
        /// Validação: Preenchimento obrigatório se VrCPSusp e vrRatSusp não tiverem sido preenchidos. 
        /// Deve ser um valor maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrSenarSusp { get; set; }

        [XmlElement("vrSenarSusp")]
        public string VrSenarSuspField
        {
            get => VrSenarSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSenarSusp = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrCPSuspField() => VrCPSusp > 0;

        public bool ShouldSerializeVrRatSuspField() => VrRatSusp > 0;

        public bool ShouldSerializeVrSenarSuspField() => VrSenarSusp > 0;


        #endregion ShouldSerialize
    }
}