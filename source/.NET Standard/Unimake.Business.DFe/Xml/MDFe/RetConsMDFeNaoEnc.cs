#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Retorno consulta MDFe não encerrados
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetConsMDFeNaoEnc")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsMDFeNaoEnc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class RetConsMDFeNaoEnc : XMLBase
    {
        /// <summary>
        /// Versão do processamento do retorno da consulta de MDFe não encerrados.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo de ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do aplicativo que processou o retorno da consulta de MDFe não encerrados.
        /// </summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        /// <summary>
        /// Código do status da resposta.
        /// </summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }

        /// <summary>
        /// Descrição literal do status da resposta.
        /// </summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        /// <summary>
        /// Código da UF.
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Código da UF (formato string para serialização XML).
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Informações dos MDFe não encerrados.
        /// </summary>
        [XmlElement("infMDFe")]
        public List<RetConsMDFeNaoEncInfMDFe> InfMDFe { get; set; } = new List<RetConsMDFeNaoEncInfMDFe>();

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista InfMDFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfMDFe</returns>
        public RetConsMDFeNaoEncInfMDFe GetInfMDFe(int index)
        {
            if ((InfMDFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfMDFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfMDFe.
        /// </summary>
        public int GetInfMDFeCount() => (InfMDFe != null ? InfMDFe.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetConsMDFeNaoEncInfMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class RetConsMDFeNaoEncInfMDFe
    {
        /// <summary>
        /// Chave de acesso do MDFe.
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Número do protocolo.
        /// </summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }
    }
}