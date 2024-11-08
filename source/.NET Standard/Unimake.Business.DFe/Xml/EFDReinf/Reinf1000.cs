#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// R-1000 - Informações do contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02", IsNullable = false)]
    public class Reinf1000 : XMLBase
    {
        /// <summary>
        /// Evento de informações do contribuinte
        /// </summary>
        [XmlElement("evtInfoContri")]
        public EvtInfoContri EvtInfoContri { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento de informações do contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtInfoContri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtInfoContri : ReinfEventoBase
    {
        /// <summary>
        ///  Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do contribuinte 
        /// </summary>
        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        /// <summary>
        /// Informações do contribuinte
        /// </summary>
        [XmlElement("infoContri")]
        public InfoContri InfoContri { get; set; }
    }

    /// <summary>
    /// Informações do contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoContri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoContri
    {
        /// <summary>
        /// Inclusão de novas informações
        /// </summary>
        [XmlElement("inclusao")]
        public Inclusao1000 Inclusao { get; set; }

        [XmlElement("alteracao")]
        public Alteracao1000 Alteracao { get; set; }

        [XmlElement("exclusao")]
        public Exclusao1000 Exclusao { get; set; }
    }

    /// <summary>
    /// Inclusão de novas informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Inclusao1000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Inclusao1000
    {
        /// <summary>
        /// Período de validade das informações incluídas 
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }

        /// <summary>
        /// Informações do contribuinte 
        /// </summary>
        [XmlElement("infoCadastro")]
        public InfoCadastro InfoCadastro { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Alteracao1000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Alteracao1000 : Inclusao1000
    {
        /// <summary>
        /// Novo período de validade das informações que
        /// estão sendo alteradas
        /// </summary>
        [XmlElement("novaValidade")]
        public NovaValidade1000 NovaValidade { get; set; }
    }

    /// <summary>
    /// Novo período de validade das informações que
    /// estão sendo alteradas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NovaValidade1000")]
    [ComVisible(true)]
#endif
    public class NovaValidade1000 : NovaValidade1050 { }

    /// <summary>
    /// Exclusão de informações
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Exclusao1000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Exclusao1000
    {
        /// <summary>
        /// Período de validade das informações excluídas 
        /// </summary>
        [XmlElement("idePeriodo")]
        public IdePeriodo IdePeriodo { get; set; }
    }

    /// <summary>
    /// Período de validade das informações incluídas 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdePeriodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdePeriodo
    {
        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações do contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoCadastro")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoCadastro
    {
        [XmlElement("classTrib")]
        public ClassificacaoTributaria ClassTrib { get; set; }

        [XmlElement("indEscrituracao")]
        public IndicativoEscrituracao IndEscrituracao { get; set; }

        [XmlElement("indDesoneracao")]
        public IndicativoDesoneracao IndDesoneracao { get; set; }

        [XmlElement("indAcordoIsenMulta")]
        public IndicativoIsencaoMulta IndAcordoIsenMulta { get; set; }


        [XmlElement("indSitPJ")]
#if INTEROP
        public IndicativoSituacaoPJ IndSitPJ { get; set; } = (IndicativoSituacaoPJ)(-1);
#else
        public IndicativoSituacaoPJ? IndSitPJ { get; set; }
#endif

        [XmlElement("indUniao")]
#if INTEROP
        public IndicativoUniao IndUniao { get; set; } = (IndicativoUniao)(-1);
#else
        public IndicativoUniao? IndUniao { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DtTransfFinsLucr { get; set; }
#else
        public DateTimeOffset DtTransfFinsLucr { get; set; }
#endif

        [XmlElement("dtTransfFinsLucr")]
        public string DtTransfFinsLucrField
        {
            get => DtTransfFinsLucr.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtTransfFinsLucr = DateTime.Parse(value);
#else
            set => DtTransfFinsLucr = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DtObito { get; set; }
#else
        public DateTimeOffset DtObito { get; set; }
#endif

        [XmlElement("dtObito")]
        public string DtObitoField
        {
            get => DtObito.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtObito = DateTime.Parse(value);
#else
            set => DtObito = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Informações de contato
        /// </summary>
        [XmlElement("contato")]
        public Contato Contato { get; set; }

        /// <summary>
        /// Informações da empresa desenvolvedora da
        ///aplicação que gera os arquivos
        /// </summary>
        [XmlElement("softHouse")]
        public List<SoftHouse> SoftHouse { get; set; }
        /// <summary>
        /// Informações da Administração Pública relativas a EFR
        /// </summary>
        [XmlElement("infoEFR")]
        public InfoEFR InfoEFR { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDtObitoField() => DtObito > DateTime.MinValue;

        public bool ShouldSerializeDtTransfFinsLucrField() => DtTransfFinsLucr > DateTime.MinValue;
#if INTEROP
        public bool ShouldSerializeIndSitPJ() => IndSitPJ != (IndicativoSituacaoPJ)(-1);
#else
        public bool ShouldSerializeIndSitPJ() => IndSitPJ != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndUniao() => IndUniao != (IndicativoUniao)(-1);
#else
        public bool ShouldSerializeIndUniao() => IndUniao != null;
#endif

        #endregion ShouldSerialize

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddSoftHouse(SoftHouse item)
        {
            if (SoftHouse == null)
            {
                SoftHouse = new List<SoftHouse>();
            }

            SoftHouse.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista SoftHouse (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da SoftHouse</returns>
        public SoftHouse GetSoftHouse(int index)
        {
            if ((SoftHouse?.Count ?? 0) == 0)
            {
                return default;
            };

            return SoftHouse[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista SoftHouse
        /// </summary>
        public int GetSoftHouseCount => (SoftHouse != null ? SoftHouse.Count : 0);

#endif
    }

    /// <summary>
    /// Informações de contato
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Contato")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Contato
    {
        [XmlElement("nmCtt")]
        public string NmCtt { get; set; }

        [XmlElement("cpfCtt")]
        public string CpfCtt { get; set; }

        [XmlElement("foneFixo")]
        public string FoneFixo { get; set; }

        [XmlElement("foneCel")]
        public string FoneCel { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFoneFixo() => !string.IsNullOrEmpty(FoneFixo);

        public bool ShouldSerializeFoneCel() => !string.IsNullOrEmpty(FoneCel);

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações da empresa desenvolvedora da
    ///aplicação que gera os arquivos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.SoftHouse")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class SoftHouse
    {
        [XmlElement("cnpjSoftHouse")]
        public string CnpjSoftHouse { get; set; }

        [XmlElement("nmRazao")]
        public string NmRazao { get; set; }

        [XmlElement("nmCont")]
        public string NmCont { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);

        public bool ShouldSerializeTelefone() => !string.IsNullOrEmpty(Telefone);

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informações da Administração Pública relativas a EFR
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoEFR")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoEFR
    {
        [XmlElement("ideEFR")]
        public SimNaoLetra IdeEFR { get; set; }

        [XmlElement("cnpjEFR")]
        public string CnpjEFR { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCnpjEFR() => !string.IsNullOrEmpty(CnpjEFR);

        #endregion ShouldSerialize
    }
}
