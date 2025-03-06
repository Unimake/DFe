#pragma warning disable CS1591

#if INTEROP

using System.Runtime.InteropServices;

#endif

using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Retorno consulta Situação MDFe
    /// </summary>
#if INTEROP

    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetConsSitMDFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsSitMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class RetConsSitMDFe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Tipo de ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Versão do aplicativo que processou o retorno da consulta da situação do MDFe.
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
        /// Protocolo de autorização do MDFe.
        /// </summary>
        [XmlElement("protMDFe")]
        public ProtMDFe ProtMDFe { get; set; }

        /// <summary>
        /// Processamento do evento do MDFe.
        /// </summary>
        [XmlElement("procEventoMDFe")]
        public List<ProcEventoMDFe> ProcEventoMDFe { get; set; } = new List<ProcEventoMDFe>();

        /// <summary>
        /// Grupo de informações do compartilhamento do MDFe com InfraSA para geração do DTe.
        /// </summary>
        [XmlElement("procInfraSA")]
        public ProcInfraSA ProcInfraSA { get; set; }

        /// <summary>
        /// Lê o XML e deserializa para o objeto RetConsSitMDFe.
        /// </summary>
        /// <param name="document">XmlDocument contendo o XML a ser lido.</param>
        public override void ReadXml(XmlDocument document)
        {
            ProcEventoMDFe.Clear();

            var nodeListProcEventoMDFe = document.GetElementsByTagName("procEventoMDFe");

            foreach (var item in nodeListProcEventoMDFe)
            {
                ProcEventoMDFe.Add(XMLUtility.Deserializar<ProcEventoMDFe>(((XmlElement)item).OuterXml));
            }
        }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProcEventoMDFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcEventoMDFe</returns>
        public ProcEventoMDFe GetProcEventoMDFe(int index)
        {
            if ((ProcEventoMDFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcEventoMDFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcEventoMDFe.
        /// </summary>
        public int GetProcEventoMDFeCount => ProcEventoMDFe != null ? ProcEventoMDFe.Count : 0;

#endif
    }
}