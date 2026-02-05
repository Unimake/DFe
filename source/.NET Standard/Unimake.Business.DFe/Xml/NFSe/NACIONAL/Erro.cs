#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Retorno de erro do Sistema Nacional NFS-e.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Temp")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("temp")]
    public class Temp : XMLBase
    {
        /// <summary>
        /// Tipo de Ambiente.
        /// </summary>
        [XmlElement("tipoAmbiente")]
        public TipoAmbiente TipoAmbiente { get; set; }

        /// <summary>
        /// Versão do aplicativo.
        /// </summary>
        [XmlElement("versaoAplicativo")]
        public string VersaoAplicativo { get; set; }

        /// <summary>
        /// Data e hora do processamento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DataHoraProcessamento { get; set; }
#else
        public DateTimeOffset DataHoraProcessamento { get; set; }
#endif

        [XmlElement("dataHoraProcessamento")]
        public string DataHoraProcessamentoField
        {
            get => DataHoraProcessamento.ToString("yyyy-MM-ddTHH:mm:ss.fffffffzzz");
#if INTEROP
            set => DataHoraProcessamento = DateTime.Parse(value);
#else
            set => DataHoraProcessamento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("idDPS")]
        public string IdDPS { get; set; }

        [XmlElement("chaveAcesso")]
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Informações de erro.
        /// </summary>
        [XmlElement("erro")]
        public Erro Erro { get; set; }

        /// <summary>
        /// Informações de "Erros"
        /// </summary>
        [XmlElement("erros")]
        public Erros Erros { get; set; }

        #region ShoudlSerialize
        public bool ShouldSerializeIdDPS() => IdDPS != null;
        public bool ShouldSerializeChaveAcesso() => ChaveAcesso != null;
        #endregion ShoudlSerialize
    }

    /// <summary>
    /// Estrutura de "erro" do retorno.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Erro")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class Erro
    {
        /// <summary>
        /// Código do erro.
        /// </summary>
        [XmlElement("codigo")]
        public string Codigo { get; set; }

        /// <summary>
        /// Descrição do erro.
        /// </summary>
        [XmlElement("descricao")]
        public string Descricao { get; set; }
    }

    /// <summary>
    /// Estrutura de "erros" do retorno.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.Erros")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class Erros
    {
        /// <summary>
        /// Código do erro.
        /// </summary>
        [XmlElement("Codigo")]
        public string Codigo { get; set; }

        /// <summary>
        /// Descrição do erro.
        /// </summary>
        [XmlElement("Descricao")]
        public string Descricao { get; set; }
    }
}
