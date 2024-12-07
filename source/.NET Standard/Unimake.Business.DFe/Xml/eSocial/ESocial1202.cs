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
    /// S-1202 - Remuneração de Servidor vinculado ao Regime Próprio de Previd. Social
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1202")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtRmnRPPS/v_S_01_02_00", IsNullable = false)]
    public class ESocial1202 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Remuneração de Servidor vinculado ao RPPS
        /// </summary>
        [XmlElement("evtRmnRPPS")]
        public EvtRmnRPPS EvtRmnRPPS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Remuneração de Servidor vinculado ao RPPS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtRmnRPPS")]
    [ComVisible(true)]
#endif
    public class EvtRmnRPPS
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento1202 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do trabalhador
        /// </summary>
        [XmlElement("ideTrabalhador")]
        public IdeTrabalhador1202 IdeTrabalhador { get; set; }

        /// <summary>
        /// Demonstrativo de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("dmDev")]
        public List<DmDev1202> DmDev { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDmDev(DmDev1202 item)
        {
            if (DmDev == null)
            {
                DmDev = new List<DmDev1202>();
            }

            DmDev.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista DmDev1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DmDev1202</returns>
        public DmDev1202 GetDmDev(int index)
        {
            if ((DmDev?.Count ?? 0) == 0)
            {
                return default;
            };

            return DmDev[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DmDev1202
        /// </summary>
        public int GetDmDevCount => (DmDev != null ? DmDev.Count : 0);
#endif
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1202")]
    [ComVisible(true)]
#endif
    public class IdeEvento1202
    {
        /// <summary>
        /// Informe [1] para arquivo original ou [2] para arquivo de retificação
        /// </summary>
        [XmlElement("indRetif")]
        public IndicativoRetificacao IndRetif { get; set; }

        /// <summary>
        /// Preencher com o número do recibo do arquivo a ser retificado
        /// </summary>
        [XmlElement("nrRecibo")]
        public string NrRecibo { get; set; }

        /// <summary>
        /// Indicativo de período de apuração
        /// </summary>
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência das informações, 
        /// se indApuracao for igual a [1], ou apenas o ano (formato AAAA), se indApuracao for igual a [2]
        /// </summary>
        [XmlElement("perApur")]
        public string PerApur { get; set; }

        /// <summary>
        /// Identificação do ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Processo de emissão do evento
        /// </summary>
        [XmlElement("procEmi")]
        public ProcEmiESocial ProcEmi { get; set; }

        /// <summary>
        /// Versão do processo de emissão do evento. Informar a versão do aplicativo emissor do evento
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrRecibo() => !string.IsNullOrEmpty(NrRecibo);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhador1202")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhador1202
    {
        /// <summary>
        /// Preencher com o número do CPF do trabalhador
        /// </summary>
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }

        /// <summary>
        /// Grupo preenchido quando o evento de remuneração se referir a trabalhador cuja categoria 
        /// não está sujeita ao evento de admissão ou ao evento TSVE - Início, bem como para informar 
        /// remuneração devida pelo órgão sucessor a servidor desligado ainda no sucedido
        /// </summary>
        [XmlElement("infoComplem")]
        public InfoComplem1202 InfoComplem { get; set; }
    }

    /// <summary>
    /// Grupo preenchido quando o evento de remuneração se referir a trabalhador cuja categoria não está 
    /// sujeita ao evento de admissão ou ao evento TSVE - Início, bem como para informar remuneração devida pelo 
    /// órgão sucessor a servidor desligado ainda no sucedido. No caso das categorias em que o evento TSVE - Início for opcional, 
    /// o preenchimento do grupo somente é exigido se não existir o respectivo evento. As informações complementares são necessárias 
    /// para correta identificação do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplem1202")]
    [ComVisible(true)]
#endif
    public class InfoComplem1202
    {
        /// <summary>
        /// Informar o nome do trabalhador
        /// </summary>
        [XmlElement("nmTrab")]
        public string NmTrab { get; set; }

        /// <summary>
        /// Preencher com a data de nascimento
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
        /// Grupo de informações da sucessão de vínculo
        /// </summary>
        [XmlElement("sucessaoVinc")]
        public SucessaoVinc SucessaoVinc { get; set; }
    }

    /// <summary>
    /// Grupo de informações da sucessão de vínculo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.SucessaoVinc")]
    [ComVisible(true)]
#endif
    public class SucessaoVinc
    {
        /// <summary>
        /// Informar o CNPJ do órgão público anterior
        /// </summary>
        [XmlElement("cnpjOrgaoAnt")]
        public string CnpjOrgaoAnt { get; set; }

        /// <summary>
        /// Matrícula do trabalhador no órgão público anterior
        /// </summary>
        [XmlElement("matricAnt")]
        public string MatricAnt { get; set; }

        /// <summary>
        /// Preencher com a data de exercício do servidor
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtExercicio { get; set; }
#else
        public DateTimeOffset DtExercicio { get; set; }
#endif

        [XmlElement("dtExercicio")]
        public string DtExercicioField
        {
            get => DtExercicio.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtExercicio = DateTime.Parse(value);
#else
            set => DtExercicio = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Observação
        /// </summary>
        [XmlElement("observacao")]
        public string Observacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeMatricAnt() => !string.IsNullOrEmpty(MatricAnt);

        public bool ShouldSerializeObservacao() => !string.IsNullOrEmpty(Observacao);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Identificação de cada um dos demonstrativos de valores devidos ao trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DmDev1202")]
    [ComVisible(true)]
#endif
    public class DmDev1202
    {
        /// <summary>
        /// Identificador atribuído pelo órgão público para o demonstrativo de valores devidos ao trabalhador
        /// </summary>
        [XmlElement("ideDmDev")]
        public string IdeDmDev { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Indicativo de Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("indRRA")]
#if INTEROP
        public SimNaoLetra IndRRA { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndRRA { get; set; }
#endif

        /// <summary>
        /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA
        /// </summary>
        [XmlElement("infoRRA")]
        public InfoRRA1202 InfoRRA { get; set; }

        /// <summary>
        /// Informações relativas ao período de apuração
        /// </summary>
        [XmlElement("infoPerApur")]
        public InfoPerApur1202 InfoPerApur { get; set; }

        /// <summary>
        /// Informações relativas a períodos anteriores	
        /// </summary>
        [XmlElement("infoPerAnt")]
        public InfoPerAnt InfoPerAnt { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndRRA() => IndRRA != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndRRA() => IndRRA != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares relativas a Rendimentos Recebidos Acumuladamente - RRA.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoRRA1202")]
    [ComVisible(true)]
#endif
    public class InfoRRA1202
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
        public int QtdMesesRRA { get; set; }

        /// <summary>
        /// Despesas com processo judicial
        /// </summary>
        [XmlElement("despProcJud")]
        public DespProcJud1202 DespProcJud { get; set; }

        /// <summary>
        /// Identificação dos advogados
        /// </summary>
        [XmlElement("ideAdv")]
        public List<IdeAdv1202> IdeAdv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeAdv(IdeAdv1202 item)
        {
            if (IdeAdv == null)
            {
                IdeAdv = new List<IdeAdv1202>();
            }

            IdeAdv.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeAdv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeAdv</returns>
        public IdeAdv1202 GetIdeAdv(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DespProcJud1202")]
    [ComVisible(true)]
#endif
    public class DespProcJud1202
    {
        /// <summary>
        /// Preencher com o valor das despesas com custas judiciais
        /// </summary>
        [XmlIgnore]
        public double VlrDespCustas { get; set; }

        [XmlElement("vlrDespCustas")]
        public string VlrDespCustasField
        {
            get => VlrDespCustas.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespCustas = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com o valor total das despesas com advogado(s)
        /// </summary>
        [XmlIgnore]
        public double VlrDespAdvogados { get; set; }

        [XmlElement("vlrDespAdvogados")]
        public string VlrDespAdvogadosField
        {
            get => VlrDespAdvogados.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrDespAdvogados = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Identificação dos advogados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeAdv1202")]
    [ComVisible(true)]
#endif
    public class IdeAdv1202
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
        /// Valor da despesa com o advogado, se houver
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

        public bool ShouldSerializeVlrAdvField() => VlrAdv > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações relativas ao período de apuração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerApur1202")]
    [ComVisible(true)]
#endif
    public class InfoPerApur1202
    {
        /// <summary>
        /// Identificação da unidade do órgão público	
        /// </summary>
        [XmlElement("ideEstab")]
        public IdeEstab1202 IdeEstab { get; set; }
    }

    /// <summary>
    /// Informações relativas à remuneração do trabalhador no período de apuração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerApur1202")]
    [ComVisible(true)]
#endif
    public class RemunPerApur1202 : RemunPerApur1200 { }

    /// <summary>
    /// Rubricas que compõem a remuneração do trabalhador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ItensRemun1202")]
    [ComVisible(true)]
#endif
    public class ItensRemun1202 : ItensRemun { }

    /// <summary>
    /// Grupo destinado às informações de:
    /// a) remuneração relativa a diferenças de vencimento provenientes de disposições legais;
    /// b) verbas de natureza salarial ou não salarial devidas após o desligamento;
    /// c) decisões administrativas ou judiciais relativas a diferenças de remuneração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPerAnt")]
    [ComVisible(true)]
#endif
    public class InfoPerAnt
    {
        /// <summary>
        /// Indicar se a remuneração é relativa a verbas de natureza salarial ou não salarial devidas pelo órgão sucessor a servidor desligado ainda no sucedido
        /// </summary>
        [XmlElement("remunOrgSuc")]
        public SimNaoOpcionalLetra RemunOrgSuc { get; set; }

        /// <summary>
        /// Identificação do período de referência da remuneração
        /// </summary>
        [XmlElement("idePeriodo")]
        public List<IdePeriodo1202> IdePeriodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdePeriodo(IdePeriodo1202 item)
        {
            if (IdePeriodo == null)
            {
                IdePeriodo = new List<IdePeriodo1202>();
            }

            IdePeriodo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdePeriodo1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdePeriodo1202</returns>
        public IdePeriodo1202 GetIdePeriodo(int index)
        {
            if ((IdePeriodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdePeriodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdePeriodo1202
        /// </summary>
        public int GetIdePeriodoCount => (IdePeriodo != null ? IdePeriodo.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação do período ao qual se referem as diferenças de remuneração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo1202")]
    [ComVisible(true)]
#endif
    public class IdePeriodo1202
    {
        /// <summary>
        /// Informar o período ao qual se refere o complemento de remuneração, no formato AAAA-MM
        /// </summary>
        [XmlIgnore]
        public DateTimeOffset PerRef { get; set; }

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
        /// Identificação da unidade do órgão público
        /// </summary>
        [XmlElement("ideEstab")]
        public List<IdeEstab1202> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab1202 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab1202>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab1202 (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab1202</returns>
        public IdeEstab1202 GetIdeEstab(int index)
        {
            if ((IdeEstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdeEstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdeEstab1202
        /// </summary>
        public int GetIdeEstabCount => (IdeEstab != null ? IdeEstab.Count : 0);
#endif
    }

    /// <summary>
    /// Identificação da unidade do órgão público na qual o servidor possui remuneração
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab1202")]
    [ComVisible(true)]
#endif
    public class IdeEstab1202
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TipoInscricaoEstabelecimento TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição da unidade do órgão público ou do estabelecimento, de acordo com o tipo de inscrição indicado no campo ideEstab/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Remuneração do trabalhador
        /// </summary>
        [XmlElement("remunPerAnt")]
        public List<RemunPerAnt> RemunPerAnt { get; set; }

        /// <summary>
        /// Informações relativas à remuneração do trabalhador no período de apuração
        /// </summary>
        [XmlElement("remunPerApur")]
        public RemunPerApur1202 RemunPerApur { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddRemunPerAnt(RemunPerAnt item)
        {
            if (RemunPerAnt == null)
            {
                RemunPerAnt = new List<RemunPerAnt>();
            }

            RemunPerAnt.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista RemunPerAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RemunPerAnt</returns>
        public RemunPerAnt GetRemunPerAnt(int index)
        {
            if ((RemunPerAnt?.Count ?? 0) == 0)
            {
                return default;
            };

            return RemunPerAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RemunPerAnt
        /// </summary>
        public int GetRemunPerAntCount => (RemunPerAnt != null ? RemunPerAnt.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas à remuneração do trabalhador em períodos anteriores
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.RemunPerAnt")]
    [ComVisible(true)]
#endif
    public class RemunPerAnt
    {
        /// <summary>
        /// Matrícula atribuída ao trabalhador pela empresa ou, no caso de servidor público, 
        /// a matrícula constante no Sistema de Administração de Recursos Humanos do órgão
        /// </summary>
        [XmlElement("matricula")]
        public string Matricula { get; set; }

        /// <summary>
        /// Itens da remuneração do trabalhador
        /// </summary>
        [XmlElement("itensRemun")]
        public List<ItensRemun1202> ItensRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddItensRemun(ItensRemun1202 item)
        {
            if (ItensRemun == null)
            {
                ItensRemun = new List<ItensRemun1202>();
            }

            ItensRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ItensRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ItensRemun</returns>
        public ItensRemun1202 GetItensRemun(int index)
        {
            if ((ItensRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return ItensRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ItensRemun1202
        /// </summary>
        public int GetItensRemunCount => (ItensRemun != null ? ItensRemun.Count : 0);
#endif
    }
}
