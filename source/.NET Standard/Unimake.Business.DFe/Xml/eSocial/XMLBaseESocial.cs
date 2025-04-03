#if INTEROP
using System.Runtime.InteropServices;
#endif


using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// XML Base para classes do eSocial
    /// </summary>
    public class XMLBaseESocial : XMLBase
    {
        /// <summary>
        /// Versão do Schema dos XML dos eventos
        /// </summary>
        [XmlIgnore]
        public string VersaoSchema { get; set; } = "v_S_01_03_00";

        /// <summary>
        /// Serializa o objeto (Converte o objeto para XML)
        /// </summary>
        /// <returns>Conteúdo do XML</returns>
        public override XmlDocument GerarXML() => Utility.ReplaceVersionSchema(base.GerarXML(), VersaoSchema);

        /// <summary>
        /// Desserializar XML (Converte o XML para um objeto)
        /// </summary>
        /// <typeparam name="T">Tipo do objeto</typeparam>
        /// <param name="doc">Conteúdo do XML a ser desserializado</param>
        /// <returns>Retorna o objeto com o conteúdo do XML desserializado</returns>
#if INTEROP
        [ComVisible(false)]
#endif
        public override T LerXML<T>(XmlDocument doc)
        {
            var result = base.LerXML<T>(Utility.ReplaceVersionSchema(doc, VersaoSchema));

            DefinirVersaoSchema(doc.OuterXml);

            var tipo = result.GetType();
            var propriedade = tipo.GetProperty("VersaoSchema");

            if (propriedade != null && propriedade.CanWrite)
            {
                propriedade.SetValue(result, VersaoSchema);
            }

            return result;
        }

        /// <summary>
        /// Atualizar a propriedade com a versão do Schema que veio no XML
        /// </summary>
        /// <param name="xml">string do XML</param>
        private void DefinirVersaoSchema(string xml)
        {
            if (xml.IndexOf("/v_S_") > 0)
            {
                VersaoSchema = xml.Substring(xml.IndexOf("/v_S_") + 1, 12);
            }
        }
    }
}
