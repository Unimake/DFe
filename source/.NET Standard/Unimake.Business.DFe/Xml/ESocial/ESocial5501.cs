#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.GNRE;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5501")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// S-5501 - Informações Consolidadas de Tributos Decorrentes de Processo Trabalhista
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtTribProcTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial5501 : XMLBase
    {
        /// <summary>
        /// Evento Informações Consolidadas de Tributos Decorrentes de Processo Trabalhista
        /// </summary>
        [XmlElement("evtTribProcTrab")]
        public EvtTribProcTrab EvtTribProcTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtTribProcTrab")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Informações Consolidadas de Tributos Decorrentes de Processo Trabalhista
    /// </summary>
    public class EvtTribProcTrab
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Informações de identificação do evento.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial5501 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador ou do contribuinte que prestou a informação
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregadorESocial5501 IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do processo
        /// </summary>
        [XmlElement("ideProc")]
        public IdeProcESocial5501 IdeProc { get; set; }
    }

    #region IdeEvento
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5501")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do evento.
    /// </summary>
    public class IdeEventoESocial5501
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu origem ao presente arquivo de retorno.
        /// Validação: Deve ser um recibo de entrega válido,
        /// correspondente ao arquivo que deu origem ao presente
        /// arquivo de retorno(S-2501 ou S-3500).
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }
    }
    #endregion IdeEvento

    #region IdeEmpregador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEmpregadorESocial5501")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de identificação do empregador ou do contribuinte que prestou a informação.
    /// </summary>
    public class IdeEmpregadorESocial5501
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05.
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo ideEmpregador/tpInsc e conforme informado em S-1000.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }
    #endregion IdeEmpregador

    #region IdeProcESocial5501
#if INTEROP
        [ClassInterface(ClassInterfaceType.AutoDual)]
        [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProcESocial5501")]
        [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do processo.
    /// </summary>
    public class IdeProcESocial5501
    {
        /// <summary>
        /// N\u00famero do processo trabalhista, da ata ou n\u00famero de identifica\u00e7\u00e3o da concilia\u00e7\u00e3o.
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Mês/ano em que é devida a obrigação de pagar a parcela prevista no acordo/sentença.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif
        /// <summary>
        /// Mês/ano em que é devida a obrigação de pagar a parcela prevista no acordo/sentença.
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
        /// Identifica\u00e7\u00e3o do per\u00edodo e da base de c\u00e1lculo dos tributos referentes ao processo trabalhista.
        /// </summary>
        [XmlElement("infoTributos")]
        public List<InfoTributos> InfoTributos { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTributos(InfoTributos item)
        {
            if (InfoTributos == null)
            {
                InfoTributos = new List<InfoTributos>();
            }

            InfoTributos.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTributos (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTributos</returns>
        public InfoTributos GetInfoTributos(int index)
        {
            if ((InfoTributos?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoTributos[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoTributos
        /// </summary>
        public int GetInfoTributosCount => (InfoTributos != null ? InfoTributos.Count : 0);
#endif
        /// <summary>
        /// Informações de IRRF referentes ao processo trabalhista
        /// </summary>
        [XmlElement("infoCRIRRF")]
        public List<InfoCRIRRFESocial5501> InfoCRIRRF { get; set; }
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
    }
    #endregion IdeProcESocial5501

    #region InfoTributos
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTributos")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do período e da base de cálculo dos tributos referentes ao processo trabalhista.
    /// </summary>
    public class InfoTributos
    {
        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência das informações.
        /// </summary>
        [XmlElement("perRef")]
        public string PerRef { get; set; }

        /// <summary>
        ///  Informações das contribuições sociais devidas à
        /// Previdência Social e Outras Entidades e Fundos,
        /// consolidadas por perRef e por Código de Receita - CR.
        /// </summary>
        [XmlElement("infoCRContrib")]
        public List<InfoCRContribESocial5501> InfoCRContrib { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRContrib(InfoCRContribESocial5501 item)
        {
            if (InfoCRContrib == null)
            {
                InfoCRContrib = new List<InfoCRContribESocial5501>();
            }

            InfoCRContrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRContrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRContrib</returns>
        public InfoCRContribESocial5501 GetInfoCRContrib(int index)
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

    #region InfoCRContrib
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRContribESocial5501")]
    [ComVisible(true)]
#endif
    public class InfoCRContribESocial5501
    {
        [XmlElement("tpCR")]
        public string TpCR { get; set; }

        [XmlElement("vrCR")]
        public double VrCR { get; set; }
    }

    #endregion InfoCRContrib
    #endregion InfoTributos

    #region InfoCRIRRF

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRIRRFESocial5501")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de Imposto de Renda Retido na Fonte, consolidadas por Código de Receita - CR.
    /// </summary>
    public class InfoCRIRRFESocial5501
    {
        /// <summary>
        /// Código de Receita - CR relativo a Imposto de Renda Retido na Fonte.
        /// </summary>
        [XmlElement("tpCR")]
        public string TpCR { get; set; }
        /// <summary>
        /// Valor correspondente ao Código de Receita - CR.
        /// </summary>
        [XmlElement("vrCR")]
        public double VrCR { get; set; }
    }
    #endregion InfoCRIRRF
}