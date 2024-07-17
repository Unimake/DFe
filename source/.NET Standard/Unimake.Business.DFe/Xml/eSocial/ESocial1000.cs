#pragma warning disable CS1591
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
#if INTEROP
using System.Runtime.InteropServices;
#endif


namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial1000")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// Evento Informações do Empregador. eSocial - 1000
    /// </summary>
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_02_00", IsNullable = false)]
    public class ESocial1000 : XMLBase
    {
        /// <summary>
        /// Evento informações do empregador. eSocial - 1000
        /// </summary>
        [XmlElement("evtInfoEmpregador")]
        public EvtInfoEmpregador EvtInfoEmpregador { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtInfoEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Evento Informações do Empregador 
    /// </summary>
    public class EvtInfoEmpregador
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Informações do empregador 
        /// </summary>
        [XmlElement("infoEmpregador")]
        public InfoEmpregador InfoEmpregador { get; set; }
    }

    #region InfoEmpregador

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoEmpregador")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Identificação da operação (inclusão, alteração ou exclusão) e das respectivas informações do empregador.
    /// </summary>
    public class InfoEmpregador
    {
        /// <summary>
        /// Inclusão de novas informações.
        /// </summary>
        [XmlElement("inclusao")]
        public InclusaoE1000 Inclusao { get; set; }
    }

    #endregion  InfoEmpregador

    #region Inclusao


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InclusaoE1000")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Inclusão de novas informações.
    /// </summary>
    public class InclusaoE1000
    {
        /// <summary>
        /// Período de validade das informações.
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }

        /// <summary>
        /// Detalhamento das informações do empregador. 
        /// </summary>
        [XmlElement("infoCadastro")]
        public InfoCadastro InfoCadastro { get; set; }
    }



#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdePeriodo")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Período de validade das informações.
    /// </summary>
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
        /// <summary>
        /// Preencher com o mês e ano de término da validade das informações, se houver. Validação: Se informado, deve estar no formato AAAA-MM e ser um período igual ou posterior a IniValid.
        /// </summary>
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

        /// <summary>
        /// ShouldSerializeFimValid
        /// </summary>
        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValidField);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoCadastro")]
    [ComVisible(true)]
#endif

    /// <summary>
    /// Detalhamento das informações do empregador. 
    /// </summary>
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
        public IndCoop IndCoop { get; set; }

        /// <summary>
        /// Indicativo de construtora. 
        /// </summary>
        [XmlElement("indConstr")]
        public IndConstr IndConstr { get; set; }

        /// <summary>
        /// Indicativo de opção/enquadramento de desoneração da folha.
        /// </summary>
        [XmlElement("indDesFolha")]
        public IndDesFolha IndDesFolha { get; set; }

        /// <summary>
        /// Indica se houve opção pelo registro eletrônico de empregados. Caso o declarante seja órgão público sem empregados regidos pela CLT, informar [0].
        /// </summary>
        [XmlElement("indOptRegEletron")]
        public IndOptRegEletron IndOptRegEletron { get; set; }

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.DadosIsencao")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações complementares - Empresas isentas - Dados da isenção.
    /// </summary>
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

        [XmlIgnore]
#if INTEROP
        public DateTime DtEmisCertif { get; set; }
#else
        /// <summary>
        /// dtEmisCertif
        /// </summary>
        public DateTimeOffset DtEmisCertif { get; set; }
#endif
        /// <summary>
        /// dtEmisCertif
        /// </summary>
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

        [XmlIgnore]
#if INTEROP
        public DateTime DtVencCertif { get; set; }
#else
        /// <summary>
        /// DtVencCertif
        /// </summary>
        public DateTimeOffset DtVencCertif { get; set; }
#endif

        /// <summary>
        /// DtVencCertif
        /// </summary>
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
        /// nrProtRenov
        /// </summary>
        [XmlElement("nrProtRenov")]
        public string NrProtRenov { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtProtRenov { get; set; }
#else
        /// <summary>
        /// dtProtRenov
        /// </summary>
        public DateTimeOffset DtProtRenov { get; set; }
#endif

        /// <summary>
        /// dtProtRenov
        /// </summary>
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

        [XmlIgnore]
#if INTEROP
        public DateTime DtDou { get; set; }
#else
        /// <summary>
        /// dtDou
        /// </summary>
        public DateTimeOffset DtDou { get; set; }
#endif

        /// <summary>
        /// dtDou
        /// </summary>
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
        /// pagDou
        /// </summary>
        [XmlElement("pagDou")]
        public string PagDou { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoOrgInternacional")]
    [ComVisible(true)]
#endif
    /// <summary>
    /// Informações exclusivas de organismos internacionais e outras instituições extraterritoriais.   
    /// </summary>
    public class InfoOrgInternacional
    {
        /// <summary>
        /// Indicativo da existência de acordo internacional para isenção de multa.
        /// </summary>
        [XmlElement("indAcordoIsenMulta")]
        public IndAcordoIsenMulta IndAcordoIsenMulta { get; set; }
    }
#endregion Inclusao

}
