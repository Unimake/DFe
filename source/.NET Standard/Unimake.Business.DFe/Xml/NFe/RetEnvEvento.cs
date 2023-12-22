#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetEnvEvento")]
    [ComVisible(true)]
#endif
    [XmlRoot("retEnvEvento", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetEnvEvento
    {
        [XmlElement("idLote", Order = 0)]
        public string IdLote { get; set; }

        [XmlElement("tpAmb", Order = 1)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic", Order = 2)]
        public string VerAplic { get; set; }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao", Order = 3)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cStat", Order = 4)]
        public int CStat { get; set; }

        [XmlElement("xMotivo", Order = 5)]
        public string XMotivo { get; set; }

        [XmlElement("retEvento", Order = 6)]
        public List<RetEvento> RetEvento { get; set; } = new List<RetEvento>();

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Desserializar o XML RetEnvEvento no objeto EnviNFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML RetEnvEvento</param>
        /// <returns>Objeto do RetEnvEvento</returns>
        public RetEnvEvento LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<RetEnvEvento>(doc);
        }

        /// <summary>
        /// Desserializar o XML EnviNFe no objeto EnviNFe
        /// </summary>
        /// <param name="xml">string do XML EnviNFe</param>
        /// <returns>Objeto da EnviNFe</returns>
        public RetEnvEvento LoadFromXML(string xml) => XMLUtility.Deserializar<RetEnvEvento>(xml);

#if INTEROP

        /// <summary>
        /// Retorna o elemento da lista ProtNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista ProtNFe)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da RetEvento</returns>
        public RetEvento GetRetEvento(int index)
        {
            if ((RetEvento?.Count ?? 0) == 0)
            {
                return default;
            }

            return RetEvento[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista RetEvento
        /// </summary>
        public int GetRetEventoCount => (RetEvento != null ? RetEvento.Count : 0);

#endif

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetEvento")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    [XmlRoot("retEvento", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetEvento : XMLBase
    {
        [XmlElement("infEvento", Order = 0)]
        public InfEventoRetEvento InfEvento { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfEventoRetEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfEventoRetEvento
    {
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        [XmlElement("tpAmb", Order = 0)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic", Order = 1)]
        public string VerAplic { get; set; }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao", Order = 2)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cStat", Order = 3)]
        public int CStat { get; set; }

        [XmlElement("xMotivo", Order = 4)]
        public string XMotivo { get; set; }

        [XmlElement("chNFe", Order = 5)]
        public string ChNFe { get; set; }

        [XmlElement("tpEvento", Order = 6)]
        public TipoEventoNFe TpEvento { get; set; }

        [XmlElement("xEvento", Order = 7)]
        public string XEvento { get; set; }

        [XmlElement("nSeqEvento", Order = 8)]
        public int NSeqEvento { get; set; }

        [XmlElement("CNPJDest", Order = 9)]
        public string CNPJDest { get; set; }

        [XmlElement("CPFDest", Order = 10)]
        public string CPFDest { get; set; }

        [XmlElement("emailDest", Order = 11)]
        public string EmailDest { get; set; }

        [XmlIgnore]
        public UFBrasil COrgaoAutor { get; set; } = UFBrasil.NaoDefinido;

        [XmlElement("cOrgaoAutor", Order = 12)]
        public int COrgaoAutorField
        {
            get => (int)COrgaoAutor;
            set => COrgaoAutor = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }        

        [XmlElement("chCTe", Order = 13)]
        public string ChCTe { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        [XmlElement("dhRegEvento", Order = 14)]
        public string DhRegEventoField
        {
            get => DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nProt", Order = 15)]
        public string NProt { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJDest() => !string.IsNullOrWhiteSpace(CNPJDest);

        public bool ShouldSerializeCPFDest() => !string.IsNullOrWhiteSpace(CPFDest);

        public bool ShouldSerializeChCTe() => !string.IsNullOrWhiteSpace(ChCTe);

        public bool ShouldSerializeCOrgaoAutorField() => COrgaoAutor != UFBrasil.NaoDefinido;

        #endregion
    }
}
