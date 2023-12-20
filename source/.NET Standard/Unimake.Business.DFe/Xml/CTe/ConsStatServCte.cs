#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// Classe para gerar o XML da Consulta Status do Serviço de CTe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ConsStatServCte")]
    [ComVisible(true)]
#endif
    [XmlRoot("consStatServCte", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ConsStatServCte : XMLBase
    {
        /// <summary>
        /// Versão do pacote de schema
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; } = "3.00";

        /// <summary>
        /// Tipo do ambiente (Homologação ou Produção)
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// UF que é para consultar o status do serviço
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Auxiliar da propriedade CUF -> Não utilizar esta diretamente, utilize a CUF
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Nome do serviço
        /// </summary>
        [XmlElement("xServ")]
        public string XServ { get; set; } = "STATUS";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se é ou não para gerar a tag cUF
        /// </summary>
        /// <returns>Retorna true se for para gerar a tag</returns>
        public bool ShouldSerializeCUFField() => Versao != "3.00";

        #endregion

        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public override XmlDocument GerarXML()
        {
            XmlDocument xml = null;

            if (Versao == "3.00")
            {
                xml = XMLUtility.Serializar(this, NameSpaces);
            }
            else
            {
                // criar uma instância de XmlRootAttribute com o novo nome do elemento
                var newRootAttribute = new XmlRootAttribute("consStatServCTe")
                {
                    Namespace = "http://www.portalfiscal.inf.br/cte",
                    IsNullable = false
                };

                xml = XMLUtility.Serializar(this, newRootAttribute, NameSpaces);
            }

            return xml;
        }

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="doc">Conteúdo do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
        public override T LerXML<T>(XmlDocument doc) => XMLUtility.Deserializar<T>(doc.OuterXml.Replace("consStatServCTe", "consStatServCte"));
    }
}