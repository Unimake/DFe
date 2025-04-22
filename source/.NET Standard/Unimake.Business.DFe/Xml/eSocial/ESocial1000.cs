#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-1000 - Informações do Empregador/Contribuinte/Órgão Público
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1000")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_03_00", IsNullable = false)]
    public class ESocial1000 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Informações do Empregador 
        /// </summary>
        [XmlElement("evtInfoEmpregador")]
        public EvtInfoEmpregador EvtInfoEmpregador { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

    /// <summary>
    /// Evento Informações do Empregador 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtInfoEmpregador")]
    [ComVisible(true)]
#endif
    public class EvtInfoEmpregador
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
        public IdeEvento1000 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação da operação (inclusão, alteração ou exclusão) e das respectivas informações do empregador
        /// </summary>
        [XmlElement("infoEmpregador")]
        public InfoEmpregador InfoEmpregador { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento1000")]
    [ComVisible(true)]
#endif
    public class IdeEvento1000 : IdeEvento { }

    /// <summary>
    /// Identificação da operação (inclusão, alteração ou exclusão) e das respectivas informações do empregador.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmpregador")]
    [ComVisible(true)]
#endif
    public class InfoEmpregador
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public InclusaoE1000 Inclusao { get; set; }

        /// <summary>
        /// Alteração das informações
        /// </summary>
        [XmlElement("alteracao")]
        public Alteracao1000 Alteracao { get; set; }

        /// <summary>
        /// Exclusão das informações
        /// </summary>
        [XmlElement("exclusao")]
        public Exclucao1000 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de novas informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InclusaoE1000")]
    [ComVisible(true)]
#endif
    public class InclusaoE1000
    {
        /// <summary>
        /// Período de validade das informações
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }

        /// <summary>
        /// Detalhamento das informações do empregador
        /// </summary>
        [XmlElement("infoCadastro")]
        public InfoCadastro InfoCadastro { get; set; }
    }

    /// <summary>
    /// Período de validade das informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo")]
    [ComVisible(true)]
#endif
    public class IdePeriodo
    {
        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAAMM. Validação: Deve ser uma data válida, igual ou 
        /// posterior à data de início de obrigatoriedade deste evento para o empregador no eSocial, no formato AAAA-MM.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime IniValid { get; set; }
#else
        public DateTimeOffset IniValid { get; set; }
#endif
        /// <summary>
        /// Preencher com o mês e ano de início da validade das informações prestadas no evento, no formato AAAAMM. Validação: Deve ser uma data válida, igual ou 
        /// posterior à data de início de obrigatoriedade deste evento para o empregador no eSocial, no formato AAAA-MM.
        /// </summary>
        [XmlElement("iniValid")]
        public string IniValidField
        {
            get => IniValid.ToString("yyyy-MM");
#if INTEROP
            set => IniValid = DateTime.Parse(value);
#else
            set => IniValid = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver. Validação: Se informado, deve estar no formato AAAA-MM e ser um período igual ou posterior a IniValid.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime FimValid { get; set; }
#else
        public DateTimeOffset FimValid { get; set; }
#endif

        [XmlElement("fimValid")]
        public string FimValidField
        {
            get => FimValid.ToString("yyyy-MM");
#if INTEROP
            set => FimValid = DateTime.Parse(value);
#else
            set => FimValid = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

        public bool ShouldSerializeFimValidField() => FimValid > DateTime.MinValue;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Detalhamento das informações do empregador. 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCadastro")]
    [ComVisible(true)]
#endif
    public class InfoCadastro
    {
        /// <summary>
        /// Preencher com o código correspondente à classificação tributária do contribuinte. 
        /// </summary>
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        /// <summary>
        /// Validação: O preenchimento do campo é exclusivo e obrigatório para PJ. Somente pode ser diferente de [0] se a natureza jurídica do declarante for igual a 214-3. 
        /// </summary>
        [XmlElement("indCoop")]
#if INTEROP
        public IndCoop IndCoop { get; set; } = (IndCoop)(-1);
#else
        public IndCoop? IndCoop { get; set; }
#endif
        /// <summary>
        /// Indicativo de construtora. 
        /// </summary>
        [XmlElement("indConstr")]
#if INTEROP
        public IndConstr IndConstr { get; set; } = (IndConstr)(-1);
#else
        public IndConstr? IndConstr { get; set; }
#endif

        /// <summary>
        /// Indicativo de opção/enquadramento de desoneração da folha.
        /// </summary>
        [XmlElement("indDesFolha")]
        public IndDesFolha IndDesFolha { get; set; }

        /// <summary>
        /// Indicativo da opção pelo produtor rural pela forma de tributação da contribuição previdenciária
        /// </summary>
        [XmlElement("indOpcCP")]
#if INTEROP
        public IndicativoOpcaoProdutorRural IndOpcCP { get; set; } = (IndicativoOpcaoProdutorRural)(-1);
#else
        public IndicativoOpcaoProdutorRural? IndOpcCP { get; set; }
#endif
        /// <summary>
        /// Indicativo de microempresa - ME ou empresa de pequeno porte - EPP para permissão de acesso ao módulo simplificado
        /// </summary>
        [XmlElement("indPorte")]
#if INTEROP
        public SimNaoLetra IndPorte { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndPorte { get; set; }
#endif

        /// <summary>
        /// Indica se houve opção pelo registro eletrônico de empregados. Caso o declarante seja órgão público sem empregados regidos pela CLT, informar [0].
        /// </summary>
        [XmlElement("indOptRegEletron")]
        public IndOptRegEletron IndOptRegEletron { get; set; }

        /// <summary>
        /// CNPJ do Ente Federativo Responsável - EFR
        /// </summary>
        [XmlElement("cnpjEFR")]
        public string CnpjEFR { get; set; }

        /// <summary>
        /// Data da transformação em sociedade de fins lucrativos
        /// </summary>
        [XmlElement("dtTrans11096")]
        public string DtTrans11096 { get; set; }

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
        /// Indicador de pertencimento do IRRF.
        /// </summary>
        [XmlElement("indPertIRRF")]
#if INTEROP
        public SimNaoLetra IndPertIRRF { get; set; } = (SimNaoLetra)(-1);
#else
        public SimNaoLetra? IndPertIRRF { get; set; }
#endif

        /// <summary>
        /// Informações complementares - Empresas isentas - Dados da isenção.
        /// </summary>
        [XmlElement("dadosIsencao")]
        public DadosIsencao DadosIsencao { get; set; }

        /// <summary>
        /// Informações exclusivas de organismos internacionais e outras instituições extraterritoriais.   
        /// </summary>
        [XmlElement("infoOrgInternacional")]
        public InfoOrgInternacional InfoOrgInternacional { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndCoop() => IndCoop != (IndCoop)(-1);
#else
        public bool ShouldSerializeIndCoop() => IndCoop != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndConstr() => IndConstr != (IndConstr)(-1);
#else
        public bool ShouldSerializeIndConstr() => IndConstr != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndOpcCP() => IndOpcCP != (IndicativoOpcaoProdutorRural)(-1);
#else
        public bool ShouldSerializeIndOpcCP() => IndOpcCP != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndPorte() => IndPorte != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndPorte() => IndPorte != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndTribFolhaPisPasep() => IndTribFolhaPisPasep != (SimNaoLetra)(-1);
#else
        public bool ShouldSerializeIndTribFolhaPisPasep() => IndTribFolhaPisPasep != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndPertIRRF() => IndPertIRRF != (SimNaoLetra)(-1) && IndPertIRRF == SimNaoLetra.Sim;
#else
        public bool ShouldSerializeIndPertIRRF() => IndPertIRRF != null && IndPertIRRF == SimNaoLetra.Sim;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações complementares - Empresas isentas - Dados da isenção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosIsencao")]
    [ComVisible(true)]
#endif
    public class DadosIsencao
    {
        /// <summary>
        /// Sigla e nome do Ministério ou lei que concedeu o certificado.        
        /// </summary>
        [XmlElement("ideMinLei")]
        public string IdeMinLei { get; set; }

        /// <summary>
        /// Número do Certificado de Entidade Beneficente de Assistência Social - CEBAS, número da portaria de concessão do certificado, ou,
        /// no caso de concessão através de lei específica, o número da lei.
        /// </summary>
        [XmlElement("nrCertif")]
        public string NrCertif { get; set; }

        /// <summary>
        /// Data de emissão do certificado/publicação da lei.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEmisCertif { get; set; }
#else
        public DateTimeOffset DtEmisCertif { get; set; }
#endif

        [XmlElement("dtEmisCertif")]
        public string DtEmisCertifField
        {
            get => DtEmisCertif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEmisCertif = DateTime.Parse(value);
#else
            set => DtEmisCertif = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de vencimento do certificado.
        /// Validação: Não pode ser anterior a dtEmisCertif.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtVencCertif { get; set; }
#else
        public DateTimeOffset DtVencCertif { get; set; }
#endif

        [XmlElement("dtVencCertif")]
        public string DtVencCertifField
        {
            get => DtVencCertif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtVencCertif = DateTime.Parse(value);
#else
            set => DtVencCertif = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do protocolo do pedido de renovação.
        /// </summary>
        [XmlElement("nrProtRenov")]
        public string NrProtRenov { get; set; }

        /// <summary>
        /// Data do protocolo de renovação.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtProtRenov { get; set; }
#else
        public DateTimeOffset DtProtRenov { get; set; }
#endif

        [XmlElement("dtProtRenov")]
        public string DtProtRenovField
        {
            get => DtProtRenov.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtProtRenov = DateTime.Parse(value);
#else
            set => DtProtRenov = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de publicação no Diário Oficial da União - DOU.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtDou { get; set; }
#else
        public DateTimeOffset DtDou { get; set; }
#endif

        [XmlElement("dtDou")]
        public string DtDouField
        {
            get => DtDou.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtDou = DateTime.Parse(value);
#else
            set => DtDou = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        ///	Número da página no DOU referente à publicação do documento de concessão do certificado.
        /// </summary>
        [XmlElement("pagDou")]
        public string PagDou { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNrProtRenov() => !string.IsNullOrEmpty(NrProtRenov);
        public bool ShouldSerializeDtProtRenovField() => DtProtRenov > DateTime.MinValue;
        public bool ShouldSerializeDtDouField() => DtDou > DateTime.MinValue;
        public bool ShouldSerializePagDou() => !string.IsNullOrEmpty(PagDou);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações exclusivas de organismos internacionais e outras instituições extraterritoriais.   
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoOrgInternacional")]
    [ComVisible(true)]
#endif
    public class InfoOrgInternacional
    {
        /// <summary>
        /// Indicativo da existência de acordo internacional para isenção de multa.
        /// </summary>
        [XmlElement("indAcordoIsenMulta")]
        public IndAcordoIsenMulta IndAcordoIsenMulta { get; set; }
    }

    /// <summary>
    /// Alteração das informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Alteracao1000")]
    [ComVisible(true)]
#endif
    public class Alteracao1000
    {
        /// <summary>
        /// Período de validade das informações
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }

        /// <summary>
        /// Detalhamento das informações do empregador
        /// </summary>
        [XmlElement("infoCadastro")]
        public InfoCadastro InfoCadastro { get; set; }

        /// <summary>
        /// Informação preenchida exclusivamente em caso de alteração do 
        /// período de validade das informações, apresentando o novo período de validade
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1000 NovaValidade { get; set; }
    }

    /// <summary>
    /// Informação preenchida exclusivamente em caso de alteração do período de validade das informações, 
    /// apresentando o novo período de validade.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.NovaValidade1000")]
    [ComVisible(true)]
#endif
    public class NovaValidade1000 : NovaValidade1005 { }

    /// <summary>
    /// Exclusão das informações.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.Exclucao1000")]
    [ComVisible(true)]
#endif
    public class Exclucao1000
    {
        /// <summary>
        /// Período de validade das informações
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }
    }
}
