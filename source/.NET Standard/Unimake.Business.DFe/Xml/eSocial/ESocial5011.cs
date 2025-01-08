#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-5011 - Informações das Contribuições Sociais Consolidadas por Contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial5011")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtCS/v_S_01_02_00", IsNullable = false)]
    public class ESocial5011 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Informações das Contribuições Sociais Consolidadas por Contribuinte
        /// </summary>
        [XmlElement("evtCS")]
        public EvtCS EvtCS { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Informações das Contribuições Sociais Consolidadas por Contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtCS")]
    [ComVisible(true)]
#endif
    public class EvtCS
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
        public IdeEvento5011 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações relativas às contribuições sociais devidas à Previdência Social e a Outras Entidades e Fundos
        /// </summary>
        [XmlElement("infoCS")]
        public InfoCS InfoCS { get; set; }
    }

    /// <summary>
    /// Identificação do evento de retorno
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento5011")]
    [ComVisible(true)]
#endif
    public class IdeEvento5011
    {
        /// <summary>
        /// Indicativo de período de apuração
        /// </summary>
        [XmlElement("indApuracao")]
        public IndApuracao IndApuracao { get; set; }

        /// <summary>
        /// Informar o mês/ano (formato AAAA-MM) de referência das informações, se indApuracao for igual a [1], ou apenas o ano (formato AAAA), se indApuracao for igual a [2]
        /// </summary>
        [XmlElement("perApur")]
        public string PerApur { get; set; }
    }

    /// <summary>
    /// Informações relativas às contribuições sociais devidas à Previdência Social e a Outras Entidades e Fundos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCS")]
    [ComVisible(true)]
#endif
    public class InfoCS
    {
        /// <summary>
        /// Preencher com o número do recibo do arquivo que deu origem ao presente arquivo de retorno ao empregador
        /// </summary>
        [XmlElement("nrRecArqBase")]
        public string NrRecArqBase { get; set; }

        /// <summary>
        /// Indicativo de existência de valores de bases e de contribuições sociais
        /// </summary>
        [XmlElement("indExistInfo")]
        public IndicativoExistenciaTributos IndExistInfo { get; set; }

        /// <summary>
        /// Informações de contribuição previdenciária do segurado
        /// </summary>
        [XmlElement("infoCPSeg")]
        public InfoCPSeg InfoCPSeg { get; set; }

        [XmlElement("infoContrib")]
        public InfoContrib InfoContrib { get; set; }

        [XmlElement("ideEstab")]
        public List<IdeEstab5011> IdeEstab { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeEstab(IdeEstab5011 item)
        {
            if (IdeEstab == null)
            {
                IdeEstab = new List<IdeEstab5011>();
            }

            IdeEstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeEstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeEstab</returns>
        public IdeEstab5011 GetIdeEstab(int index)
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

        /// <summary>
        /// Informações consolidadas das contribuições sociais devidas à Previdência Social e Outras Entidades e Fundos, por Código de Receita - CR
        /// </summary>
        [XmlElement("infoCRContrib")]
        public List<InfoCRContrib5011> InfoCRContrib { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCRContrib(InfoCRContrib5011 item)
        {
            if (InfoCRContrib == null)
            {
                InfoCRContrib = new List<InfoCRContrib5011>();
            }

            InfoCRContrib.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCRContrib (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCRContrib</returns>
        public InfoCRContrib5011 GetInfoCRContrib(int index)
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

    /// <summary>
    /// Informações de contribuição previdenciária do segurado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCPSeg")]
    [ComVisible(true)]
#endif
    public class InfoCPSeg
    {
        /// <summary>
        /// Valor total da contribuição descontada dos segurados.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [21].
        /// </summary>
        [XmlIgnore]
        public double VrDescCP { get; set; }

        [XmlElement("vrDescCP")]
        public string VrDescCPField
        {
            get => VrDescCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total calculado relativo à contribuição dos segurados.
        /// Origem: campo vrCpSeg de S-5001.
        /// </summary>
        [XmlIgnore]
        public double VrCpSeg { get; set; }

        [XmlElement("vrCpSeg")]
        public string VrCpSegField
        {
            get => VrCpSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCpSeg = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações gerais do contribuinte necessárias à apuração das contribuições sociais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoContrib")]
    [ComVisible(true)]
#endif
    public class InfoContrib
    {
        /// <summary>
        /// Preencher com o código correspondente à classificação tributária do contribuinte, conforme Tabela 08
        /// </summary>
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        /// <summary>
        /// Informações complementares, exclusivas da Pessoa Jurídica
        /// </summary>
        [XmlElement("infoPJ")]
        public InfoPJ InfoPJ { get; set; }
    }

    /// <summary>
    /// Informações complementares, exclusivas da Pessoa Jurídica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoPJ")]
    [ComVisible(true)]
#endif
    public class InfoPJ
    {
        /// <summary>
        /// Indicativo de cooperativa
        /// </summary>
        [XmlElement("indCoop")]
#if INTEROP
        public IndCoop IndCoop { get; set; } = (IndCoop)(-1);
#else
        public IndCoop? IndCoop { get; set; }
#endif

        /// <summary>
        /// Indicativo de construtora
        /// </summary>
        [XmlElement("indConstr")]
        public IndConstr IndConstr { get; set; }

        /// <summary>
        /// Indicativo de substituição da contribuição previdenciária patronal
        /// </summary>
        [XmlElement("indSubstPatr")]
#if INTEROP
        public IndSubstPatr IndSubstPatr { get; set; } = (IndSubstPatr)(-1);
#else
        public IndSubstPatr? IndSubstPatr { get; set; }
#endif

        /// <summary>
        /// Percentual de redução da contribuição prevista na Lei 12.546/2011.
        /// Evento de origem: S-1280.
        /// </summary>
        [XmlIgnore]
        public double PercRedContrib { get; set; }

        [XmlElement("percRedContrib")]
        public string PercRedContribField
        {
            get => PercRedContrib.ToString("F2", CultureInfo.InvariantCulture);
            set => PercRedContrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de contribuição social - Lei 11.096/2005
        /// </summary>
        [XmlElement("percTransf")]
        public string PercTransf { get; set; }

        /// <summary>
        /// Indicador de tributação sobre a folha de pagamento - PIS e 
        /// </summary>
        [XmlElement("indTribFolhaPisCofins")]
        [Obsolete("Propriedade obsoleta, só é aceita na versão 01.02.00 do eSocial. Nas versões superiores ela foi substituída pela propriedade IndTribFolhaPisPasep.", false)]
#if INTEROP
        public SimNaoLetra IndTribFolhaPisCofins { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndTribFolhaPisCofins { get; set; }
#endif

        /// <summary>
        /// Indicador de tributação sobre a folha de pagamento - PIS e PASEP. Preenchimento exclusivo para o empregador em situação de tributação de PIS e PASEP sobre a folha de pagamento.
        /// </summary>
        [XmlElement("indTribFolhaPisPasep")]
#if INTEROP
        public SimNaoLetra IndTribFolhaPisPasep { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndTribFolhaPisPasep { get; set; }
#endif

        /// <summary>
        /// Informações prestadas por empresa enquadrada no regime de tributação Simples Nacional com tributação previdenciária substituída e não substituída
        /// </summary>
        [XmlElement("infoAtConc")]
        public InfoAtConc InfoAtConc { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndCoop() => IndCoop != (IndCoop)(-1);
#else
        public bool ShouldSerializeIndCoop() => IndCoop != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndSubstPatr() => IndSubstPatr != (IndSubstPatr)(-1);
#else
        public bool ShouldSerializeIndSubstPatr() => IndSubstPatr != null;
#endif

        public bool ShouldSerializePercRedContribField() => PercRedContrib > 0;

        public bool ShouldSerializePercTransfField() => !string.IsNullOrEmpty(PercTransf);

#if INTEROP
        public bool ShouldSerializeIndTribFolhaPisCofins() => IndTribFolhaPisCofins != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndTribFolhaPisCofins() => IndTribFolhaPisCofins != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndTribFolhaPisPasep() => IndTribFolhaPisPasep != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndTribFolhaPisPasep() => IndTribFolhaPisPasep != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações prestadas por empresa enquadrada no regime de tributação Simples Nacional com tributação previdenciária substituída e não substituída
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoAtConc")]
    [ComVisible(true)]
#endif
    public class InfoAtConc
    {
        /// <summary>
        /// Informe o fator a ser utilizado para cálculo da contribuição
        /// patronal do mês dos trabalhadores envolvidos na execução
        /// das atividades enquadradas no Anexo IV em conjunto com
        /// as dos Anexos I a III e V da Lei Complementar 123/2006.
        /// Evento de origem: S-1280.
        /// </summary>
        [XmlIgnore]
        public double FatorMes { get; set; }

        [XmlElement("fatorMes")]
        public string FatorMesField
        {
            get => FatorMes.ToString("F2", CultureInfo.InvariantCulture);
            set => FatorMes = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informe o fator a ser utilizado para cálculo da contribuição
        /// patronal do décimo terceiro dos trabalhadores envolvidos na
        /// execução das atividades enquadradas no Anexo IV em conjunto
        /// com as dos Anexos I a III e V da Lei Complementar 123/2006.
        /// Evento de origem: S-1280.
        /// </summary>
        [XmlIgnore]
        public double Fator13 { get; set; }

        [XmlElement("fator13")]
        public string Fator13Field
        {
            get => Fator13.ToString("F2", CultureInfo.InvariantCulture);
            set => Fator13 = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações de identificação do estabelecimento ou obra de construção civil
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEstab5011")]
    [ComVisible(true)]
#endif
    public class IdeEstab5011
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de inscrição, conforme Tabela 05
        /// </summary>
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        /// <summary>
        /// Informar o número de inscrição do contribuinte de acordo com o tipo de inscrição indicado no campo ideEstab/tpInsc
        /// </summary>
        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }

        /// <summary>
        /// Informações relativas a cada estabelecimento, necessárias à apuração das contribuições sociais
        /// </summary>
        [XmlElement("infoEstab")]
        public InfoEstab5011 InfoEstab { get; set; }

        /// <summary>
        /// Identificação da lotação tributária
        /// </summary>
        [XmlElement("ideLotacao")]
        public List<IdeLotacao5011> IdeLotacao { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddIdeLotacao(IdeLotacao5011 item)
        {
            if (IdeLotacao == null)
            {
                IdeLotacao = new List<IdeLotacao5011>();
            }

            IdeLotacao.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista IdeLotacao (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdeLotacao</returns>
        public IdeLotacao5011 GetIdeLotacao(int index)
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

        /// <summary>
        /// Informações de bases de cálculo relativas à aquisição de produção rural
        /// </summary>
        [XmlElement("basesAquis")]
        public List<BasesAquis> BasesAquis { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesAquis(BasesAquis item)
        {
            if (BasesAquis == null)
            {
                BasesAquis = new List<BasesAquis>();
            }

            BasesAquis.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesAquis (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesAquis</returns>
        public BasesAquis GetBasesAquis(int index)
        {
            if ((BasesAquis?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesAquis[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesAquis
        /// </summary>
        public int GetBasesAquisCount => (BasesAquis != null ? BasesAquis.Count : 0);
#endif

        /// <summary>
        /// Informações de bases de cálculo relativas à comercialização da produção rural da Pessoa Física
        /// </summary>
        [XmlElement("basesComerc")]
        public List<BasesComerc> BasesComerc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesComerc(BasesComerc item)
        {
            if (BasesComerc == null)
            {
                BasesComerc = new List<BasesComerc>();
            }

            BasesComerc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesComerc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesComerc</returns>
        public BasesComerc GetBasesComerc(int index)
        {
            if ((BasesComerc?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesComerc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesComerc
        /// </summary>
        public int GetBasesComercCount => (BasesComerc != null ? BasesComerc.Count : 0);
#endif

        /// <summary>
        /// Informações das contribuições sociais devidas à Previdência Social e Outras Entidades e Fundos, consolidadas por estabelecimento e por Código de Receita - CR.
        /// </summary>
        [XmlElement("infoCREstab")]
        public List<InfoCREstab> InfoCREstab { get; set; }

        /// <summary>
        /// Bases da contribuição do PIS/PASEP
        /// </summary>
        [XmlElement("basesPisPasep")]
        public BasesPisPasep BasesPisPasep { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoCREstab(InfoCREstab item)
        {
            if (InfoCREstab == null)
            {
                InfoCREstab = new List<InfoCREstab>();
            }

            InfoCREstab.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoCREstab (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoCREstab</returns>
        public InfoCREstab GetInfoCREstab(int index)
        {
            if ((InfoCREstab?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoCREstab[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoCREstab
        /// </summary>
        public int GetInfoCREstabCount => (InfoCREstab != null ? InfoCREstab.Count : 0);
#endif
    }

    /// <summary>
    /// Informações relativas a cada estabelecimento, necessárias à apuração das contribuições sociais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstab5011")]
    [ComVisible(true)]
#endif
    public class InfoEstab5011
    {
        /// <summary>
        /// /Preencher com o código CNAE, conforme informado em S-1005
        /// </summary>
        [XmlElement("cnaePrep")]
        public string CnaePrep { get; set; }

        /// <summary>
        /// Preencher com o CNPJ responsável pela inscrição no cadastro de obras da RFB
        /// </summary>
        [XmlElement("cnpjResp")]
        public string CnpjResp { get; set; }

        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        /// <summary>
        /// Fator Acidentário de Prevenção - FAP.
        /// Validação: Informação obrigatória e
        /// exclusiva se ideEmpregador/tpInsc = [1].
        /// </summary>
        [XmlIgnore]
        public double Fap { get; set; }

        [XmlElement("fap")]
        public string FapField
        {
            get => Fap.ToString("F4", CultureInfo.InvariantCulture);
            set => Fap = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do RAT após ajuste pelo FAP.
        /// Validação: Informação obrigatória e
        /// exclusiva se ideEmpregador/tpInsc = [1].
        /// </summary>
        [XmlIgnore]
        public double AliqRatAjust { get; set; }

        [XmlElement("aliqRatAjust")]
        public string AliqRatAjustField
        {
            get => AliqRatAjust.ToString("F4", CultureInfo.InvariantCulture);
            set => AliqRatAjust = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informações de RAT e FAP de referência, nos casos de processo administrativo ou judicial que altere a(s) alíquota(s)
        /// </summary>
        [XmlElement("infoEstabRef")]
        public InfoEstabRef InfoEstabRef { get; set; }

        /// <summary>
        /// Informações complementares relativas a obras de construção civil
        /// </summary>
        [XmlElement("infoComplObra")]
        public InfoComplObra InfoComplObra { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjResp() => !string.IsNullOrEmpty(CnpjResp);

        public bool ShouldSerializeFapField() => Fap > 0;

        public bool ShouldSerializeAliqRatAjustField() => AliqRatAjust > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de RAT e FAP de referência, nos casos de processo administrativo ou judicial que altere a(s) alíquota(s)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEstabRef")]
    [ComVisible(true)]
#endif
    public class InfoEstabRef
    {
        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        /// <summary>
        /// Fator Acidentário de Prevenção - FAP estabelecido pelo órgão governamental competente
        /// </summary>
        [XmlIgnore]
        public double Fap { get; set; }

        [XmlElement("fap")]
        public string FapField
        {
            get => Fap.ToString("F4", CultureInfo.InvariantCulture);
            set => Fap = Converter.ToDouble(value);
        }

        /// <summary>
        /// nformações de RAT e FAP de referência, nos casos de processo
        /// administrativo ou judicial que altere a(s) alíquota(s).
        /// </summary>
        [XmlIgnore]
        public double AliqRatAjust { get; set; }

        [XmlElement("aliqRatAjust")]
        public string AliqRatAjustField
        {
            get => AliqRatAjust.ToString("F4", CultureInfo.InvariantCulture);
            set => AliqRatAjust = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeFapField() => Fap > 0;

        public bool ShouldSerializeAliqRatAjustField() => AliqRatAjust > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares relativas a obras de construção civil
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoComplObra")]
    [ComVisible(true)]
#endif
    public class InfoComplObra
    {
        /// <summary>
        /// Indicativo de substituição da contribuição patronal de obra de construção civil
        /// </summary>
        [XmlElement("indSubstPatrObra")]
        public IndicativoSubstituicaoPatronal IndSubstPatrObra { get; set; }
    }

    /// <summary>
    /// Identificação da lotação tributária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeLotacao5011")]
    [ComVisible(true)]
#endif
    public class IdeLotacao5011
    {
        /// <summary>
        /// Informar o código atribuído pelo empregador para a lotação tributária
        /// </summary>
        [XmlElement("codLotacao")]
        public string CodLotacao { get; set; }

        /// <summary>
        /// Preencher com o código relativo ao FPAS
        /// </summary>
        [XmlElement("fpas")]
        public string Fpas { get; set; }

        /// <summary>
        /// Preencher com o código de Terceiros, conforme Tabela 04
        /// </summary>
        [XmlElement("codTercs")]
        public string CodTercs { get; set; }

        /// <summary>
        /// Informar o código combinado dos Terceiros para os quais o recolhimento está suspenso em virtude de processos judiciais
        /// </summary>
        [XmlElement("codTercsSusp")]
        public string CodTercsSusp { get; set; }

        /// <summary>
        /// Informações de suspensão de contribuições destinadas a Outras Entidades e Fundos (Terceiros)
        /// </summary>
        [XmlElement("infoTercSusp")]
        public List<InfoTercSusp> InfoTercSusp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfoTercSusp(InfoTercSusp item)
        {
            if (InfoTercSusp == null)
            {
                InfoTercSusp = new List<InfoTercSusp>();
            }

            InfoTercSusp.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfoTercSusp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfoTercSusp</returns>
        public InfoTercSusp GetInfoTercSusp(int index)
        {
            if ((InfoTercSusp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfoTercSusp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfoTercSusp
        /// </summary>
        public int GetInfoTercSuspCount => (InfoTercSusp != null ? InfoTercSusp.Count : 0);
#endif

        /// <summary>
        /// Informação complementar que apresenta identificação do contratante e do proprietário de obra de 
        /// construção civil contratada sob regime de empreitada parcial ou subempreitada
        /// </summary>
        [XmlElement("infoEmprParcial")]
        public InfoEmprParcial5011 InfoEmprParcial { get; set; }

        /// <summary>
        /// Informações relativas ao operador portuário
        /// </summary>
        [XmlElement("dadosOpPort")]
        public DadosOpPort5011 DadosOpPort { get; set; }

        /// <summary>
        /// Bases de cálculo da contribuição previdenciária incidente sobre remunerações, por categoria
        /// </summary>
        [XmlElement("basesRemun")]
        public List<BasesRemun> BasesRemun { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddBasesRemun(BasesRemun item)
        {
            if (BasesRemun == null)
            {
                BasesRemun = new List<BasesRemun>();
            }

            BasesRemun.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista BasesRemun (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da BasesRemun</returns>
        public BasesRemun GetBasesRemun(int index)
        {
            if ((BasesRemun?.Count ?? 0) == 0)
            {
                return default;
            };

            return BasesRemun[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista BasesRemun
        /// </summary>
        public int GetBasesRemunCount => (BasesRemun != null ? BasesRemun.Count : 0);
#endif

        /// <summary>
        /// Informações de bases de cálculo relativas à contratação de trabalhadores avulsos não portuários
        /// </summary>
        [XmlElement("basesAvNPort")]
        public BasesAvNPort BasesAvNPort { get; set; }

        /// <summary>
        /// Grupo preenchido exclusivamente pelo Órgão Gestor de Mão de Obra - OGMO
        /// </summary>
        [XmlElement("infoSubstPatrOpPort")]
        public InfoSubstPatrOpPort5011 InfoSubstPatrOpPort { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodTercsSusp() => !string.IsNullOrEmpty(CodTercsSusp);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações de suspensão de contribuições destinadas a Outras Entidades e Fundos (Terceiros)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoTercSusp")]
    [ComVisible(true)]
#endif
    public class InfoTercSusp
    {
        /// <summary>
        /// Informar o código de Terceiro
        /// </summary>
        [XmlElement("codTerc")]
        public string CodTerc { get; set; }
    }

    /// <summary>
    /// Informação complementar que apresenta identificação do contratante e do proprietário de obra de 
    /// construção civil contratada sob regime de empreitada parcial ou subempreitada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmprParcial5011")]
    [ComVisible(true)]
#endif
    public class InfoEmprParcial5011
    {
        /// <summary>
        /// Tipo de inscrição do contratante
        /// </summary>
        [XmlElement("tpInscContrat")]
        public TpInsc TpInscContrat { get; set; }

        /// <summary>
        /// Número de inscrição (CNPJ/CPF) do contratante
        /// </summary>
        [XmlElement("nrInscContrat")]
        public string NrInscContrat { get; set; }

        /// <summary>
        /// Tipo de inscrição do proprietário do CNO
        /// </summary>
        [XmlElement("tpInscProp")]
        public TpInsc TpInscProp { get; set; }

        /// <summary>
        /// Preencher com o número de inscrição (CNPJ/CPF) do proprietário do CNO
        /// </summary>
        [XmlElement("nrInscProp")]
        public string NrInscProp { get; set; }

        /// <summary>
        /// Número do CNO da obra
        /// </summary>
        [XmlElement("cnoObra")]
        public string CnoObra { get; set; }
    }

    /// <summary>
    /// Informações relativas ao operador portuário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosOpPort5011")]
    [ComVisible(true)]
#endif
    public class DadosOpPort5011
    {
        /// <summary>
        /// Preencher com o CNPJ do operador portuário
        /// </summary>
        [XmlElement("cnpjOpPortuario")]
        public string CnpjOpPortuario { get; set; }

        /// <summary>
        /// Informar a alíquota RAT.
        /// Valores válidos: 1, 2, 3
        /// </summary>
        [XmlElement("aliqRat")]
        public int AliqRat { get; set; }

        /// <summary>
        /// Fator Acidentário de Prevenção - FAP.
        /// Origem: campo dadosOpPort/fap de S-1020.
        /// </summary>
        [XmlIgnore]
        public double Fap { get; set; }

        [XmlElement("fap")]
        public string FapField
        {
            get => Fap.ToString("F4", CultureInfo.InvariantCulture);
            set => Fap = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do RAT após ajuste pelo FAP.
        /// Validação: Deve corresponder ao resultado da multiplicação
        /// dos campos dadosOpPort/aliqRat e dadosOpPort/fap.
        /// </summary>
        [XmlIgnore]
        public double AliqRatAjust { get; set; }

        [XmlElement("aliqRatAjust")]
        public string AliqRatAjustField
        {
            get => AliqRatAjust.ToString("F4", CultureInfo.InvariantCulture);
            set => AliqRatAjust = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Bases de cálculo da contribuição previdenciária incidente sobre remunerações, por categoria
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesRemun")]
    [ComVisible(true)]
#endif
    public class BasesRemun
    {
        /// <summary>
        /// Preencher com o código correspondente ao tipo de incidência para fins de apuração da contribuição previdenciária
        /// </summary>
        [XmlElement("indIncid")]
        public IndIncid IndIncid { get; set; }

        /// <summary>
        /// Preencher com o código da categoria do trabalhador, conforme definido em S-5001
        /// </summary>
        [XmlElement("codCateg")]
        public CodCateg CodCateg { get; set; }

        /// <summary>
        /// Valores correspondentes às bases, contribuições do segurado e deduções da contribuição previdenciária
        /// </summary>
        [XmlElement("basesCp")]
        public BasesCp BasesCp { get; set; }

        /// <summary>
        /// Valores correspondentes às remunerações a título de 13º salário incluídas em folha mensal - Empresas desoneradas.
        /// </summary>
        [XmlElement("basesCp13")]
        public BasesCp13 BasesCp13 { get; set; }
    }

    /// <summary>
    /// Valores correspondentes às bases, contribuições do segurado e deduções da contribuição previdenciária
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesCp")]
    [ComVisible(true)]
#endif
    public class BasesCp
    {
        /// <summary>
        /// Preencher com a base de cálculo da contribuição
        /// previdenciária sobre a remuneração.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp00")]
        public string VrBcCp00Field
        {
            get => VrBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional
        /// para o financiamento dos benefícios de aposentadoria
        /// especial após 15 anos de contribuição.
        /// Origem: campo valor de S-5001, se tpValor em S-5001 = [12, 16].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp15")]
        public string VrBcCp15Field
        {
            get => VrBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 20 anos de contribuição.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [13, 17].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp20")]
        public string VrBcCp20FIeld
        {
            get => VrBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 25 anos de contribuição.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [14, 18].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp25")]
        public string VrBcCp25Field
        {
            get => VrBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp25 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC com incidência suspensa em decorrência de decisão judicial.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [91, 95].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp00 { get; set; }

        [XmlElement("vrSuspBcCp00")]
        public string VrSuspBcCp00Field
        {
            get => VrSuspBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional
        /// correspondente a exposição a agente nocivo que dá ao trabalhador
        /// direito a aposentadoria especial aos 15 anos de trabalho, com
        /// incidência suspensa em decorrência de decisão judicial.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [92, 96].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp15 { get; set; }

        [XmlElement("vrSuspBcCp15")]
        public string VrSuspBcCp15Field
        {
            get => VrSuspBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional
        /// correspondente a exposição a agente nocivo que dá ao trabalhador
        /// expectativa de aposentadoria especial aos 20 anos de trabalho,
        /// com incidência suspensa em decorrência de decisão judicial.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [93, 97].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp20 { get; set; }

        [XmlElement("vrSuspBcCp20")]
        public string VrSuspBcCp20Field
        {
            get => VrSuspBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional correspondente
        /// a exposição a agente nocivo que dá ao trabalhador direito a aposentadoria especial
        /// aos 25 anos de trabalho, com incidência suspensa em decorrência de decisão judicial.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [94, 98].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp25 { get; set; }

        [XmlElement("vrSuspBcCp25")]
        public string VrSuspBcCp25Field
        {
            get => VrSuspBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp25 = Convert.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição previdenciária
        /// sobre a remuneração - Contrato Verde e Amarelo.
        /// Origem: somatório do campo valor de S-5001, quando tpValor em S-5001 = [41, 45].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp00VA { get; set; }

        [XmlElement("vrBcCp00VA")]
        public string VrBcCp00VAField
        {
            get => VrBcCp00VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp00VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o
        /// financiamentodos benefícios de aposentadoria especial após 15
        /// anos de contribuição - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, se tpValor em S-5001 = [42, 46].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp15VA { get; set; }

        [XmlElement("vrBcCp15VA")]
        public string VrBcCp15VAFIeld
        {
            get => VrBcCp15VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp15VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o
        /// financiamento dos benefícios de aposentadoria especial após 20
        /// anos de contribuição - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [43, 47].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp20VA { get; set; }

        [XmlElement("vrBcCp20VA")]
        public string VrBcCp20VAField
        {
            get => VrBcCp20VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp20VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o
        /// financiamento dos benefícios de aposentadoria especial após 25
        /// anos de contribuição - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [44, 48].
        /// </summary>
        [XmlIgnore]
        public double VrBcCp25VA { get; set; }

        [XmlElement("vrBcCp25VA")]
        public string VrBcCp25VAField
        {
            get => VrBcCp25VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp25VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC com incidência suspensa em decorrência
        /// de decisão judicial - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [81, 85].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp00VA { get; set; }

        [XmlElement("vrSuspBcCp00VA")]
        public string VrSuspBcCp00VAField
        {
            get => VrSuspBcCp00VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp00VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional
        /// correspondente a exposição a agente nocivo que dá ao trabalhador
        /// direito a aposentadoria especial aos 15 anos de trabalho, com
        /// incidência suspensa em decorrência de decisão judicial - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [82, 86].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp15VA { get; set; }

        [XmlElement("vrSuspBcCp15VA")]
        public string VrSuspBcCp15VAField
        {
            get => VrSuspBcCp15VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp15VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional
        /// correspondente a exposição a agente nocivo que dá ao trabalhador
        /// expectativa de aposentadoria especial aos 20 anos de trabalho, com
        /// incidência suspensa em decorrência de decisão judicial - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [83, 87].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp20VA { get; set; }

        [XmlElement("vrSuspBcCp20VA")]
        public string VrSuspBcCp20VAFIeld
        {
            get => VrSuspBcCp20VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp20VA = Converter.ToDouble(value);

        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional
        /// correspondente a exposição a agente nocivo que dá ao trabalhador
        /// direito a aposentadoria especial aos 25 anos de trabalho, com
        /// incidência suspensa em decorrência de decisão judicial - Contrato Verde e Amarelo.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [84, 88].
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp25VA { get; set; }

        [XmlElement("vrSuspBcCp25VA")]
        public string VrSuspBcCp25VAField
        {
            get => VrSuspBcCp25VA.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp25VA = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total descontado do trabalhador para recolhimento ao SEST.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [22].
        /// </summary>
        [XmlIgnore]
        public double VrDescSest { get; set; }

        [XmlElement("vrDescSest")]
        public string VrDescSestField
        {
            get => VrDescSest.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescSest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor calculado relativo à contribuição devida
        /// pelo trabalhador para recolhimento ao SEST.
        /// </summary>
        [XmlIgnore]
        public double VrCalcSest { get; set; }

        [XmlElement("vrCalcSest")]
        public string VrCalcSestField
        {
            get => VrCalcSest.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCalcSest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total descontado do trabalhador para recolhimento ao SENAT.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [23].
        /// </summary>
        [XmlIgnore]
        public double VrDescSenat { get; set; }
        [XmlElement("vrDescSenat")]
        public string VrDescSenatField
        {
            get => VrDescSenat.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescSenat = Converter.ToDouble(value);

        }

        /// <summary>
        /// Valor calculado relativo à contribuição devida
        /// pelo trabalhador para recolhimento ao SENAT.
        /// </summary>
        [XmlIgnore]
        public double VrCalcSenat { get; set; }

        [XmlElement("vrCalcSenat")]
        public string VrCalcSenatField
        {
            get => VrCalcSenat.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCalcSenat = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do salário-família para a categoria indicada.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [31].
        /// </summary>
        [XmlIgnore]
        public double VrSalFam { get; set; }

        [XmlElement("vrSalFam")]
        public string VrSalFamField
        {
            get => VrSalFam.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSalFam = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do salário-maternidade para a categoria indicada.
        /// Origem: campo valor de S-5001, quando tpValor em S-5001 = [32].
        /// </summary>
        [XmlIgnore]
        public double VrSalMat { get; set; }

        [XmlElement("vrSalMat")]
        public string VrSalMatField
        {
            get => VrSalMat.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSalMat = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrBcCp00VAField() => VrBcCp00VA > 0;

        public bool ShouldSerializeVrBcCp15VAField() => VrBcCp15VA > 0;

        public bool ShouldSerializeVrBcCp20VAField() => VrBcCp20VA > 0;

        public bool ShouldSerializeVrBcCp25VAField() => VrBcCp25VA > 0;

        public bool ShouldSerializeVrSuspBcCp00VAField() => VrSuspBcCp00VA > 0;

        public bool ShouldSerializeVrSuspBcCp15VAField() => VrSuspBcCp15VA > 0;

        public bool ShouldSerializeVrSuspBcCp20VAField() => VrSuspBcCp20VA > 0;

        public bool ShouldSerializeVrSuspBcCp25VAField() => VrSuspBcCp25VA > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Valores correspondentes às remunerações a título de 13º salário incluídas em folha mensal - Empresas desoneradas.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesCp13")]
    [ComVisible(true)]
#endif
    public class BasesCp13
    {
        /// <summary>
        /// Preencher com a base de cálculo da contribuição previdenciária - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp00")]
        public string VrBcCp00Field
        {
            get => VrBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o financiamento dos benefícios de aposentadoria especial após 15 anos de contribuição - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp15")]
        public string VrBcCp15Field
        {
            get => VrBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o financiamento dos benefícios de aposentadoria especial após 20 anos de contribuição - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp20")]
        public string VrBcCp20FIeld
        {
            get => VrBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base de cálculo da contribuição adicional para o financiamento dos benefícios de aposentadoria especial após 25 anos de contribuição - Remuneração a título de 13º salário.        
        /// </summary>
        [XmlIgnore]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp25")]
        public string VrBcCp25Field
        {
            get => VrBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp25 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo com incidência suspensa em decorrência de decisão judicial - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp00 { get; set; }

        [XmlElement("vrSuspBcCp00")]
        public string VrSuspBcCp00Field
        {
            get => VrSuspBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional correspondente a exposição a agente nocivo que dá ao trabalhador direito a aposentadoria especial aos 15 anos de trabalho, com incidência suspensa em decorrência de decisão judicial - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp15 { get; set; }

        [XmlElement("vrSuspBcCp15")]
        public string VrSuspBcCp15Field
        {
            get => VrSuspBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional correspondente a exposição a agente nocivo que dá ao trabalhador expectativa de aposentadoria especial aos 20 anos de trabalho, com incidência suspensa em decorrência de decisão judicial - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp20 { get; set; }

        [XmlElement("vrSuspBcCp20")]
        public string VrSuspBcCp20Field
        {
            get => VrSuspBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária adicional correspondente a exposição a agente nocivo que dá ao trabalhador direito a aposentadoria especial aos 25 anos de trabalho, com incidência suspensa em decorrência de decisão judicial - Remuneração a título de 13º salário.
        /// </summary>
        [XmlIgnore]
        public double VrSuspBcCp25 { get; set; }

        [XmlElement("vrSuspBcCp25")]
        public string VrSuspBcCp25Field
        {
            get => VrSuspBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspBcCp25 = Convert.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações de bases de cálculo relativas à contratação de trabalhadores avulsos não portuários
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesAvNPort")]
    [ComVisible(true)]
#endif
    public class BasesAvNPort
    {
        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária sobre
        /// a remuneração dos trabalhadores avulsos não portuários.
        /// Origem: campo vrBcCp00 de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp00 { get; set; }

        [XmlElement("vrBcCp00")]
        public string VrBcCp00FIeld
        {
            get => VrBcCp00.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp00 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o
        /// financiamento dos benefícios de aposentadoria especial
        /// após 15 anos de contribuição.
        /// Origem: campo vrBcCp15 de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp15 { get; set; }

        [XmlElement("vrBcCp15")]
        public string VrBcCp15Field
        {
            get => VrBcCp15.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp15 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 20 anos de contribuição.
        /// Origem: campo vrBcCp20 de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp20 { get; set; }

        [XmlElement("vrBcCp20")]
        public string VrBcCp20Field
        {
            get => VrBcCp20.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp20 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição adicional para o financiamento
        /// dos benefícios de aposentadoria especial após 25 anos de contribuição.
        /// Origem: campo vrBcCp25 de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp25 { get; set; }

        [XmlElement("vrBcCp25")]
        public string VrBcCp25Field
        {
            get => VrBcCp25.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp25 = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo da contribuição previdenciária sobre o 13°
        /// salário dos trabalhadores avulsos não portuários contratados.
        /// Origem: campo vrBcCp13 de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrBcCp13 { get; set; }

        [XmlElement("vrBcCp13")]
        public string VrBcCp13Field
        {
            get => VrBcCp13.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcCp13 = Converter.ToDouble(value);

        }

        /// <summary>
        /// Preencher com o valor total da contribuição descontada
        /// dos trabalhadores avulsos não portuários.
        /// Origem: campo vrDescCP de S-1270.
        /// </summary>
        [XmlIgnore]
        public double VrDescCP { get; set; }

        [XmlElement("vrDescCP")]
        public string VrDescCPField
        {
            get => VrDescCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VrDescCP = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo preenchido exclusivamente pelo Órgão Gestor de Mão de Obra - OGMO
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoSubstPatrOpPort5011")]
    [ComVisible(true)]
#endif
    public class InfoSubstPatrOpPort5011
    {
        /// <summary>
        /// Preencher com o CNPJ do operador portuário
        /// </summary>
        [XmlElement("cnpjOpPortuario")]
        public string CnpjOpPortuario { get; set; }
    }

    /// <summary>
    /// Informações de bases de cálculo relativas à aquisição de produção rural
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesAquis")]
    [ComVisible(true)]
#endif
    public class BasesAquis
    {
        /// <summary>
        /// Indicativo da aquisição
        /// </summary>
        [XmlElement("indAquis")]
        public IndAquis IndAquis { get; set; }

        /// <summary>
        ///  Valor total da aquisição de produção rural de produtor rural.
        ///  Origem: campo {vlrTotAquis} de S-1250.
        /// </summary>
        [XmlIgnore]
        public double VlrAquis { get; set; }

        [XmlElement("vlrAquis")]
        public string VlrAquisField
        {
            get => VlrAquis.ToString("F2", CultureInfo.InvariantCulture);
            set => VlrAquis = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com o valor da contribuição previdenciária descontada
        /// pelo adquirente de produção de produtor rural - sub-rogação.
        /// Origem: somatório do campo {vrCpDescPR} de S-1250.
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
        /// Valor da contribuição previdenciária que deixou de ser retida
        /// pelo declarante em decorrência de decisão/sentença judicial.
        /// </summary>
        [XmlIgnore]
        public double VrCPNRet { get; set; }

        [XmlElement("vrCPNRet")]
        public string VrCPNRetField
        {
            get => VrCPNRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCPNRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da GILRAT, incidente sobre a aquisição de produção rural de 
        /// produtor rural, cuja retenção deixou de ser efetuada em
        /// decorrência de decisão/sentença judicial.
        /// </summary>
        [XmlIgnore]
        public double VrRatNRet { get; set; }

        [XmlElement("vrRatNRet")]
        public string VrRatNRetField
        {
            get => VrRatNRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRatNRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao SENAR, incidente sobre a aquisição
        /// de produção rural de produtor rural pessoa física/segurado especial, que
        /// deixou de ser retida em decorrência de decisão/sentença judicial.
        /// </summary>
        [XmlIgnore]
        public double VrSenarNRet { get; set; }

        [XmlElement("vrSenarNRet")]
        public string VrSenarNRetField
        {
            get => VrSenarNRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSenarNRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor calculado relativo à contribuição previdenciária
        /// do produtor rural, de acordo com indAquis
        /// </summary>
        [XmlIgnore]
        public double VrCPCalcPR { get; set; }

        [XmlElement("vrCPCalcPR")]
        public string VrCPCalcPRField
        {
            get => VrCPCalcPR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCPCalcPR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao financiamento dos benefícios
        /// concedidos em razão do grau de incidência da incapacidade
        /// laborativa decorrente dos riscos ambientais do trabalho,
        /// incidente sobre a aquisição de produção rural de produtor rural.
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
        /// Valor calculado relativo à contribuição GILRAT devida
        /// pelo produtor rural, de acordo com indAquis
        /// </summary>
        [XmlIgnore]
        public double VrRatCalcPR { get; set; }

        [XmlElement("vrRatCalcPR")]
        public string VrRatCalcPRField
        {
            get => VrRatCalcPR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrRatCalcPR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao SENAR, incidente sobre a aquisição
        /// de produção rural de produtor rural pessoa física/segurado especial.
        /// </summary>
        [XmlIgnore]
        public double VrSenarDesc { get; set; }

        [XmlElement("vrSenarDesc")]
        public string VrSenarDescField
        {
            get => VrSenarDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSenarDesc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição destinada ao SENAR, incidente sobre a aquisição de produção rural de produtor rural pessoa física/segurado especial
        /// </summary>
        [XmlIgnore]
        public double VrSenarCalc { get; set; }

        [XmlElement("vrSenarCalc")]
        public string VrSenarCalcField
        {
            get => VrSenarCalc.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSenarCalc = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações de bases de cálculo relativas à comercialização da produção rural da Pessoa Física
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesComerc")]
    [ComVisible(true)]
#endif
    public class BasesComerc
    {
        /// <summary>
        /// Indicativo de comercialização
        /// </summary>
        [XmlElement("indComerc")]
        public IndComerc IndComerc { get; set; }

        /// <summary>
        /// Valor da base de cálculo da comercialização da produção rural do
        /// produtor rural PF/segurado especial a outra PF no varejo ou a
        /// outro produtor rural PF/segurado especial ou no mercado
        /// externo, conforme indComerc.
        /// Origem: campo vrTotCom de S-1260.
        /// </summary>
        [XmlIgnore]
        public double VrBcComPR { get; set; }

        [XmlElement("vrBcComPR")]
        public string VrBcComPRField
        {
            get => VrBcComPR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcComPR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da contribuição previdenciária com exigibilidade suspensa.
        /// Origem: campo vrCPSusp de S-1260.
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
        /// Origem: campo vrRatSusp de S-1260.
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
        /// Valor da contribuição para o SENAR com exigibilidade suspensa
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

    /// <summary>
    /// Informações das contribuições sociais devidas à Previdência Social e Outras Entidades e Fundos, consolidadas por estabelecimento e por Código de Receita - CR
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCREstab")]
    [ComVisible(true)]
#endif
    public class InfoCREstab
    {
        /// <summary>
        /// Código de Receita - CR relativo a contribuições sociais devidas à Previdência Social e a Outras Entidades e Fundos (Terceiros), conforme legislação em vigor na competência.
        /// </summary>
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        /// <summary>
        /// Valor correspondente ao CR apurado.
        /// Validação: Deve ser apurado de acordo com
        /// a legislação em vigor na competência.
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCR { get; set; }

        [XmlElement("vrCR")]
        public string VrCRField
        {
            get => VrCR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor suspenso correspondente ao CR apurado.
        /// Validação: Deve ser apurado de acordo com as
        /// informações de processos judiciais e administrativos.
        /// </summary>
        [XmlIgnore]
        public double VrSuspCR { get; set; }

        [XmlElement("vrSuspCR")]
        public string VrSuspCRField
        {
            get => VrSuspCR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrSuspCR = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrSuspCRField() => VrSuspCR > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações consolidadas das contribuições sociais devidas à Previdência Social e Outras Entidades e Fundos, por Código de Receita - CR
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCRContrib5011")]
    [ComVisible(true)]
#endif
    public class InfoCRContrib5011
    {
        /// <summary>
        /// Código de Receita - CR relativo a contribuições sociais devidas à Previdência Social e a Outras Entidades e Fundos (Terceiros), 
        /// conforme legislação em vigor na competência
        /// </summary>
        [XmlElement("tpCR")]
        public TpCR TpCR { get; set; }

        /// <summary>
        /// Valor correspondente ao CR apurado.
        /// Validação: Deve ser apurado de acordo com a
        /// legislação em vigor na competência.
        /// Deve ser maior que 0 (zero).
        /// </summary>
        [XmlIgnore]
        public double VrCR { get; set; }

        [XmlElement("vrCR")]
        public string VrCRField
        {
            get => VrCR.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do tributo com exigibilidade suspensa.
        /// </summary>
        [XmlIgnore]
        public double VrCRSusp { get; set; }

        [XmlElement("vrCRSusp")]
        public string VrSuspCRField
        {
            get => VrCRSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VrCRSusp = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVrCRSuspField() => VrCRSusp > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Bases da contribuição do PIS/PASEP
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.BasesPisPasep")]
    [ComVisible(true)]
#endif
    public class BasesPisPasep
    {
        /// <summary>
        /// Preencher com a base da contribuição do PIS/PASEP.
        /// </summary>
        [XmlIgnore]
        public double VrBcPisPasep { get; set; }

        [XmlElement("vrBcPisPasep")]
        public string VrBcPisPasepField
        {
            get => VrBcPisPasep.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcPisPasep = Converter.ToDouble(value);
        }

        /// <summary>
        /// Preencher com a base da contribuição do PIS/PASEP suspensa.
        /// </summary>
        [XmlIgnore]
        public double VrBcPisPasepSusp { get; set; }

        [XmlElement("vrBcPisPasepSusp")]
        public string VrBcPisPasepSuspField
        {
            get => VrBcPisPasepSusp.ToString("F2", CultureInfo.InvariantCulture);
            set => VrBcPisPasepSusp = Converter.ToDouble(value);
        }
    }
}
