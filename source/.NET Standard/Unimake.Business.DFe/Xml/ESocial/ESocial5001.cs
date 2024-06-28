﻿#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Business.DFe.Xml.SNCM;
#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5001")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// S-5001 - Informações das Contribuições Sociais por Trabalhador
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtBasesTrab/v_S_01_02_00", IsNullable = false)]
    public class ESocial5001 : XMLBase
    {
        /// <summary>
        /// Evento Informações das Contribuições Sociais por Trabalhador
        /// </summary>
        [XmlElement("evtBasesTrab")]
        public EvtBasesTrab EvtBasesTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtBasesTrab")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Informações das Contribuições Sociais por Trabalhador
    /// </summary>
    public class EvtBasesTrab
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id")]
        public string Id { get; set; }

        /// <summary>
        /// Identificação do evento de retorno.
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEventoESocial5001 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador.
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador.
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhadorESocial5001 IdeTrabalhador { get; set; }

        /// <summary>
        /// Cálculo da contribuição previdenciária do segurado,
        /// incidente sobre a remuneração do período de apuração e
        /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
        /// </summary>
        [XmlElement("infoCpCalc")]
        public List<InfoCpCalc> InfoCpCalc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCpCalc(InfoCpCalc item)
        {
            if (InfoCpCalc == null)
            {
                InfoCpCalc = new List<InfoCpCalc>();
            }

            InfoCpCalc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCpCalc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCpCalc</returns>
        public InfoCpCalc GetInfoCpCalc(int index)
        {
            if ((InfoCpCalc?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCpCalc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCpCalc
        /// </summary>
        public int GetInfoCpCalcCount => (InfoCpCalc != null ? InfoCpCalc.Count : 0);
#endif

        /// <summary>
        /// Informações sobre bases e valores das contribuições sociais (opcional).
        /// </summary>
        [XmlElement("infoCp")]
        public InfoCp InfoCp { get; set; }
    }

    #region IdeEvento5001
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEventoESocial5001")]
    [ComVisible(true)]
#endif
    public class IdeEventoESocial5001
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu
        /// origem ao presente arquivo de retorno ao empregador.
        /// Validação: Deve ser um recibo de entrega válido,
        /// correspondente ao arquivo que deu origem ao presente
        /// arquivo de retorno (S-1200, S-2299, S-2399 ou S-3000).
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        /// <summary>
        /// Indicativo de período de apuração.
        /// </summary>
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

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
        public DateTime PerApur {get; set; }
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
    }
    #endregion IdeEvento5001

    #region IdeTrabalhador
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhadorESocial5001")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do trabalhador.
    /// </summary>
    public class IdeTrabalhadorESocial5001
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador.
        /// Validação: Deve ser um CPF válido
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Informações complementares do trabalhador e do contrato.
        /// </summary>
        [XmlElement("infoCompl")]
        public InfoComplESocial5001 InfoCompl { get; set; }

        /// <summary>
        /// Informações sobre processos judiciais do trabalhador
        /// com decisão favorável quanto à não incidência ou
        /// alterações na incidência de contribuição previdenciária.
        /// </summary>
        [XmlElement("procJudTrab")]
        public List<ProcJudTrabESocial5001> ProcJudTrab { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddProcJudTrab(ProcJudTrabESocial5001 item)
        {
            if (ProcJudTrab == null)
            {
                ProcJudTrab = new List<ProcJudTrabESocial5001>();
            }

            ProcJudTrab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcJudTrabESocial5001 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcJudTrab</returns>
        public ProcJudTrabESocial5001 ProcJudTrab(int index)
        {
            if ((ProcJudTrab?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcJudTrab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcJudTrab
        /// </summary>
        public int GetProcJudTrabCount => (ProcJudTrab != null ? ProcJudTrab.Count : 0);
#endif
    }

    #region InfoComplESocial5001
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplESocial5001")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações complementares do trabalhador e do contrato.
    /// </summary>
    public class InfoComplESocial5001
    {
        /// <summary>
        /// Grupo de informações da sucessão de vínculo trabalhista. Evento de origem: S-1200.
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVincESocial1200 SucessaoVinc { get; set; }

        /// <summary>
        ///  Informações relativas ao trabalho intermitente. Evento de origem: S-1200 ou S-2299.
        /// </summary>
        [XmlElement("infoInterm")]
        public List<InfoInterm> InfoInterm { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoInterm(InfoInterm item)
        {
            if (InfoInterm == null)
            {
                InfoInterm = new List<InfoInterm>();
            }

            InfoInterm.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoInterm (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoInterm</returns>
        public InfoInterm GetInfoInterm(int index)
        {
            if ((InfoInterm?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoInterm[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoInterm
        /// </summary>
        public int GetInfoIntermCount => (InfoInterm != null ? InfoInterm.Count : 0);
#endif

        /// <summary>
        /// Informações complementares contratuais do trabalhador. Evento de origem: S-1200
        /// </summary>
        [XmlElement("infoComplCont")]
        public List<InfoComplCont> InfoComplCont { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoComplCont(InfoComplCont item)
        {
            if (InfoComplCont == null)
            {
                InfoComplCont = new List<InfoComplCont>();
            }

            InfoComplCont.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoComplCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoComplCont</returns>
        public InfoComplCont GetInfoComplCont(int index)
        {
            if ((InfoComplCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoComplCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoComplCont
        /// </summary>
        public int GetInfoComplContCount => (InfoComplCont != null ? InfoComplCont.Count : 0);
#endif
    }
    #endregion InfoComplESocial5001

    #region ProcJudTrab
    /// <summary>
    /// Informações sobre processos judiciais do trabalhador
    /// com decisão favorável quanto à não incidência ou
    /// alterações na incidência de contribuição previdenciária.
    /// </summary>
    public class ProcJudTrabESocial5001
    {
        /// <summary>
        /// Informar um número de processo judicial cadastrado
        /// através do evento S-1070, cujo indMatProc seja igual a[1].
        /// Validação: Deve ser um número de processo judicial
        /// válido e existente na Tabela de Processos (S-1070), com indMatProc = [1].
        /// </summary>
        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        /// <summary>
        /// Código do indicativo da suspensão, atribuído pelo empregador em S-1070.
        /// Validação: A informação prestada deve estar de acordo
        /// com o que foi informado em S-1070.
        /// </summary>
        [XmlElement("codSusp")]
        public string CodSusp { get; set; }
    }
    #endregion ProcJudTrab
    #endregion IdeTrabalhador

    #region InfoCpCalc
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCpCalc")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Cálculo da contribuição previdenciária do segurado,
    /// incidente sobre a remuneração do período de apuração e
    /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
    /// </summary>
    public class InfoCpCalc
    {
        /// <summary>
        /// Código de Receita - CR da contribuição descontada do trabalhador.
        /// </summary>
        [XmlElement("tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor da contribuição do segurado, devida à Previdência Social,
        /// calculada segundo as regras da legislação em vigor, por CR.
        /// </summary>
        [XmlIgnore]
        public double VrCpSeg { get; set; }

        /// <summary>
        /// Valor da contribuição do segurado, devida à Previdência Social,
        /// calculada segundo as regras da legislação em vigor, por CR.
        /// </summary>
        [XmlElement("vrCpSeg")]
        public string VrCpSegField
        {
            get => VrCpSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCpSeg = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor efetivamente descontado do segurado, corresponde a TpValor = [21] do correspondente
        /// infoCpCalc/tpCR
        /// </summary>
        [XmlIgnore]
        public double VrDescSeg { get; set; }

        /// <summary>
        /// Valor efetivamente descontado do segurado, corresponde a TpValor = [21] do correspondente
        /// infoCpCalc/tpCR
        /// </summary>
        [XmlElement("vrDescSeg")]
        public string VrDescSegField
        {
            get => VrDescSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescSeg = Converter.ToDouble(value);
        }
    }
    #endregion  InfoCpCalc

    #region InfoCp
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCp")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Cálculo da contribuição previdenciária do segurado,
    /// incidente sobre a remuneração do período de apuração e
    /// de períodos anteriores informada nos eventos S-1200, S2299 e S-2399.
    /// </summary>
    public class InfoCp
    {
        /// <summary>
        /// Preencher com o código correspondente à classificação tributária
        /// do contribuinte, conforme Tabela08.
        /// Evento de origem: S-1000
        /// </summary>
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        /// <summary>
        /// Identificação do estabelecimento ou obra de construção civil e da lotação tributária.
        /// </summary>
        [XmlElement("ideEstabLot")]
        public List<IdeEstabLotESocial5001> IdeEstabLot { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstabLot(IdeEstabLotESocial5001 item)
        {
            if (IdeEstabLot == null)
            {
                IdeEstabLot = new List<IdeEstabLotESocial5001>();
            }

            IdeEstabLot.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstabLot (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstabLot</returns>
        public IdeEstabLotESocial5001 GetIdeADC(int index)
        {
            if ((IdeEstabLot?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstabLot[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstabLot
        /// </summary>
        public int GetIdeEstabLotCount => (IdeEstabLot != null ? IdeEstabLot.Count : 0);
#endif
    }

    #region IdeEstabLotESocial5001
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstabLotESocial5001")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação do estabelecimento ou obra de construção civil e da lotação tributária.
    /// </summary>
    public class IdeEstabLotESocial5001
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento
        /// Valores válidos:
        /// 1 - CNPJ;
        /// 3 - CAEPF;
        /// 4 - CNO;
        /// </summary>
        [XmlElement("tpInsc")]
        public TpInsc TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do estabelecimento do contribuinte de acordo 
        /// com o tipo de inscrição indicado no campo ideEstabLot/tpInsc.
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária.
        /// </summary>
        [XmlElement("codLotacao")]
        public string Codlotacao { get; set; }

        /// <summary>
        /// Informações relativas à matrícula e categoria do trabalhador e tipos de incidências.
        /// </summary>
        [XmlElement("infoCategIncid")]
        public List<InfoCategIncid> InfoCategIncid { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCategIncid(InfoCategIncid item)
        {
            if (InfoCategIncid == null)
            {
                InfoCategIncid = new List<InfoCategIncid>();
            }

            InfoCategIncid.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCategIncid (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCategIncid</returns>
        public InfoCategIncid GetInfoCategIncid(int index)
        {
            if ((InfoCategIncid?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCategIncid[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCategIncid
        /// </summary>
        public int GetInfoCategIncidCount => (InfoCategIncid != null ? InfoCategIncid.Count : 0);
#endif
    }
    #endregion IdeEstabLotESocial5001

    #region InfoCategIncid
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCategIncid")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações relativas à matrícula e categoria do trabalhador e tipos de incidências.
    /// </summary>
    public class InfoCategIncid
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no
        /// caso de servidor público, a matrícula constante no
        /// Sistema de Administração de Recursos Humanos do órgão.
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador,
        /// conforme Tabela 01.
        /// Validação: Se o evento de origem for S-1200, retornar o
        /// código de categoria informado nesse evento.Se o
        /// evento de origem for S-2299 ou S-2399, retornar o
        /// código de categoria existente no Registro de Eventos
        /// Trabalhistas - RET.
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Indicador de contribuição substituída.
        /// Valores válidos:
        /// 1 - Contribuição substituída integralmente
        /// 2 - Contribuição não substituída
        /// 3 - Contribuição não substituída concomitante com
        /// contribuição substituída
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("indSimples")]
#if INTEROP
        public IndSimples IndSimples { get; set; } = (IndSimples)(-1);
#else
        public IndSimples? IndSimples { get; set; }
#endif

        /// <summary>
        /// Informações sobre bases de cálculo, descontos e
        /// deduções de contribuições sociais devidas à Previdência
        /// Social e a Outras Entidades e Fundos.
        /// Evento de origem: S-1200, S-2299 ou S-2399.
        /// </summary>
        [XmlElement("infoBaseCS")]
        public List<InfoBaseCS> InfoBaseCS { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoBaseCS(InfoBaseCS item)
        {
            if (InfoBaseCS == null)
            {
                InfoBaseCS = new List<InfoBaseCS>();
            }

            InfoBaseCS.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoBaseCS (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoBaseCS</returns>
        public InfoBaseCS GetInfoBaseCS(int index)
        {
            if ((InfoBaseCS?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoBaseCS[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoBaseCS
        /// </summary>
        public int GetInfoBaseCSCount => (InfoBaseCS != null ? InfoBaseCS.Count : 0);
#endif

        /// <summary>
        /// Cálculo das contribuições sociais devidas a Outras Entidades e Fundos.
        /// </summary>
        [XmlElement("calcTerc")]
        public List<CalcTerc> CalcTerc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddCalcTerc(CalcTerc item)
        {
            if (CalcTerc == null)
            {
                CalcTerc = new List<CalcTerc>();
            }

            CalcTerc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista CalcTerc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CalcTerc</returns>
        public CalcTerc GetCalcTerc(int index)
        {
            if ((CalcTerc?.Count ?? 0) == 0)
            {
                return default;
            };

            return CalcTerc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CalcTerc
        /// </summary>
        public int GetCalcTercCount => (CalcTerc != null ? CalcTerc.Count : 0);
#endif

        /// <summary>
        /// Informações de remuneração por período de referência.
        /// </summary>
        [XmlElement("infoPerRef")]
        public List<InfoPerRef> InfoPerRef { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoPerRef(InfoPerRef item)
        {
            if (InfoPerRef == null)
            {
                InfoPerRef = new List<InfoPerRef>();
            }

            InfoPerRef.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoPerRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoPerRef</returns>
        public InfoPerRef GetInfoPerRef(int index)
        {
            if ((InfoPerRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoPerRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoPerRef
        /// </summary>
        public int GetInfoPerRefCount => (InfoPerRef != null ? InfoPerRef.Count : 0);
#endif
#region ShouldSerialize
        public bool ShouldSerializeMatriculaField() => !string.IsNullOrEmpty(Matricula);
#if INTEROP
        public bool ShouldSerializeIndSimplesField() =>  IndSimples != (IndSimples)(-1);
#else
        public bool ShouldSerializeIndSimplesField() =>  !IndSimples.IsNullOrEmpty();
#endif
        #endregion  ShouldSerialize
    }

    #region InfoBaseCS
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoBaseCS")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações sobre bases de cálculo, descontos e
    /// deduções de contribuições sociais devidas à Previdência
    /// Social e a Outras Entidades e Fundos.
    /// Evento de origem: S-1200, S-2299 ou S-2399.
    /// </summary>
    public class InfoBaseCS
    {
        /// <summary>
        /// Indicativo de 13° salário.
        /// Valores válidos:
        /// 0 - Mensal
        /// 1 - 13° salário(codIncCP em S-1010 = [12, 14, 16, 22, 26,
        /// 32, 92, 94, 96, 98])
        /// Validação: Se indApuracao = [2], preencher com[1].
        /// </summary>
        [XmlElement("ind13")]
        public string Ind13 { get; set; }

        /// <summary>
        /// Tipo de valor que influi na apuração da devida.
        /// </summary>
        [XmlElement("tpValor")]
        public string TpValor { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social devida à Previdência Social ou a Outras Entidades e Fundos, 
        /// conforme definido no campo TpValor.
        /// </summary>
        [XmlIgnore]
        public double Valor { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social devida à Previdência Social ou a Outras Entidades e Fundos, 
        /// conforme definido no campo TpValor.
        /// </summary>
        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Converter.ToDouble(value);
        }
    }
    #endregion InfoBaseCS

    #region CalcTerc
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.CalcTerc")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Cálculo das contribuições sociais devidas a Outras Entidades e Fundos.
    /// </summary>
    public class CalcTerc
    {
        /// <summary>
        /// Código de Receita - CR da contribuição descontada do
        /// trabalhador.
        /// Valores válidos:
        /// 121802 - Contribuição ao SEST, descontada do transportador autônomo
        /// 122102 - Contribuição ao SENAT, descontada do transportador autônomo
        /// </summary>
        [XmlElement("tpCR")]
        public string TpCR { get; set; }

        /// <summary>
        /// Valor da contribuição social devida a Outras Entidades ou
        /// Fundos, calculada segundo a legislação em vigor, por CR.
        /// </summary>
        [XmlIgnore]
        public double VrCsSegTerc { get; set; }

        /// <summary>
        /// Valor da contribuição social devida a Outras Entidades ou
        /// Fundos, calculada segundo a legislação em vigor, por CR.
        /// </summary>
        [XmlElement("vrCsSegTerc")]
        public string VrCsSegTercField
        {
            get => VrCsSegTerc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCsSegTerc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor efetivamente descontado do segurado,
        /// correspondente a tpValor = [22, 23], do correspondente calcTerc/tpCR.
        /// </summary>
        [XmlIgnore]
        public double VrDescTerc { get; set; }

        /// <summary>
        ///  Valor efetivamente descontado do segurado,
        /// correspondente a tpValor = [22, 23], do correspondente calcTerc/tpCR.
        /// </summary>
        [XmlElement("vrDescTerc")]
        public string VrDescTercField
        {
            get => VrDescTerc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescTerc = Converter.ToDouble(value);
        }
    }
    #endregion CalcTerc

    #region InfoPerRef
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerRef")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações de remuneração por período de referência.
    /// </summary>
    public class InfoPerRef
    {
        /// <summary>
        /// Informar o período ao qual se refere a remuneração.
        /// Origem: perApur ou campo {perRef}
        /// de S-1200/S-2299.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerRef {get; set; }
#else
        public DateTimeOffset PerRef { get; set; }
#endif

        /// <summary>
        /// Informar o período ao qual se refere a remuneração.
        /// Origem: perApur ou campo {perRef}
        /// de S-1200/S-2299.
        /// </summary>
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
        /// Identificação do instrumento ou situação ensejadora da
        /// remuneração relativa a períodos de apuração anteriores.
        /// Evento de origem: S-1200 ou S-2299 (exceto remunSuc,
        /// cujo evento de origem somente é S-1200).
        /// </summary>
        [XmlElement("ideADC")]
        public List<IdeADC> IdeADC { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeADC(IdeADC item)
        {
            if (IdeADC == null)
            {
                IdeADC = new List<IdeADC>();
            }

            IdeADC.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeADC (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeADC</returns>
        public IdeADC GetIdeADC(int index)
        {
            if ((IdeADC?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeADC[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeADC
        /// </summary>
        public int GetIdeADCCount => (IdeADC != null ? IdeADC.Count : 0);
#endif

        /// <summary>
        /// Detalhamento das informações de remuneração por
        /// período de referência.Deve ser preenchido com
        /// informações de { infoPerApur }
        /// e {infoPerAnt}
        /// do S-1200 e
        /// S-2299, e de { dmDev } do S-2399, quando houver.
        /// </summary>
        [XmlElement("detInfoPerRef")]
        public List<DetInfoPerRef> DetInfoPerRef { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDetInfoPerRef(DetInfoPerRef item)
        {
            if (DetInfoPerRef == null)
            {
                DetInfoPerRef = new List<DetInfoPerRef>();
            }

            DetInfoPerRef.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DetInfoPerRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetInfoPerRef</returns>
        public DetInfoPerRef GetDetInfoPerRef(int index)
        {
            if ((DetInfoPerRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetInfoPerRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetInfoPerRef
        /// </summary>
        public int GetDetInfoPerRefCount => (DetInfoPerRef != null ? DetInfoPerRef.Count : 0);
#endif
    }

    #region  DetInfoPerRef
    /// <summary>
    /// Detalhamento das informações de remuneração por
    /// período de referência.Deve ser preenchido com
    /// informações de { infoPerApur }
    /// e {infoPerAnt}
    /// do S-1200 e
    /// S-2299, e de { dmDev } do S-2399, quando houver.
    /// </summary>
    public class DetInfoPerRef
    {
        /// <summary>
        /// Indicativo de 13° salário.
        /// Valores válidos:
        /// 0 - Mensal
        /// 1 - 13° salário(codIncCP em S-1010 = [12, 14, 16, 22, 26,
        /// 32, 92, 94, 96, 98])
        /// Validação: Se indApuracao = [2], preencher com[1].
        /// </summary>
        [XmlElement("ind13")]
        public string Ind13 { get; set; }

        /// <summary>
        /// Tipo de valor que influi na apuração da contribuição devida.
        /// </summary>
        [XmlElement("tpVrPerRef")]
        public string TpVrPerRef { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social, conforme definido no campo tpVrPerRef.
        /// </summary>
        [XmlIgnore]
        public double VrPerRef { get; set; }

        /// <summary>
        /// Valor da base de cálculo, dedução ou desconto da contribuição
        /// social, conforme definido no campo tpVrPerRef.
        /// </summary>
        [XmlElement("vrPerRef")]
        public string VrPerRefField
        {
            get => VrPerRef.ToString("F2", CultureInfo.InvariantCulture);
            set => VrPerRef = Converter.ToDouble(value);
        }
    }
    #endregion DetInfoPerRef

    #endregion InfoPerRef

#endregion InfoCategIncid

#endregion InfoCp
}