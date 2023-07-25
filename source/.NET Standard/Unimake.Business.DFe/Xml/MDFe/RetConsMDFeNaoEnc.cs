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
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.RetConsMDFeNaoEnc")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsMDFeNaoEnc", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class RetConsMDFeNaoEnc: XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("infMDFe")]
        public List<RetConsMDFeNaoEncInfMDFe> InfMDFe { get; set; }

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista InfMDFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
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
        /// Retorna a quantidade de elementos existentes na lista InfMDFe
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
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        [XmlElement("nProt")]
        public string NProt { get; set; }
    }
}