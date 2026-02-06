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
    /// Retorno da Consulta de NFSe por RPS - Padrão NACIONAL
    /// Contém chave de acesso (sucesso) OU erro (falha)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.RetConsultarNfsePorRps")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("temp")]
    public class RetConsultarNfsePorRps : XMLBase
    {
        /// <summary>
        /// Tipo de Ambiente
        /// </summary>
        [XmlIgnore]
        public TipoAmbiente TipoAmbiente { get; set; }

        /// <summary>
        /// Versão do aplicativo (presente apenas em caso de sucesso).
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

        /// <summary>
        /// Chave de acesso de 50 dígitos da NFSe consultada (presente em caso de sucesso).
        /// </summary>
        [XmlElement("chaveAcesso")]
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Informações de erro (presente em caso de falha).
        /// </summary>
        [XmlElement("erro")]
        public Erro Erro { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVersaoAplicativo() => !string.IsNullOrWhiteSpace(VersaoAplicativo);
        public bool ShouldSerializeChaveAcesso() => !string.IsNullOrWhiteSpace(ChaveAcesso);

        #endregion ShouldSerialize
    }
}
