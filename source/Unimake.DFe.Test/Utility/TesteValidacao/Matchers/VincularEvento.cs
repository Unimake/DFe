using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Matchers
{
    internal class VincularEvento : IVinculadorSchema
    {
        public List<(XmlNode TipoSchema, XmlNode Node)> Vincular(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode NodeIdCorreto, XmlNode eventoNode)>();

            var tagEvento = servico.SelectSingleNode("*[local-name()='TagEvento']")?.InnerText;
            if (string.IsNullOrWhiteSpace(tagEvento))
                throw new Exception("Tag de evento não configurada no serviço");

            XmlNodeList eventoXML = xml.GetElementsByTagName(tagEvento);
            if (eventoXML.Count == 0)
                throw new Exception($"Tag de evento '{tagEvento}' não encontrada no XML");

            XmlNode schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");

            foreach (XmlNode eventoNode in eventoXML)
            {
                var tpEventoNode = eventoNode.SelectSingleNode("*[local-name()='infEvento']/*[local-name()='tpEvento']");
                string tpEvento = tpEventoNode?.InnerText;

                if (string.IsNullOrWhiteSpace(tpEvento))
                    throw new Exception("Tag <tpEvento> não encontrada");

                XmlNode NodetipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, tpEvento);

                if (NodetipoCorreto is null)
                    throw new Exception($"Não existe Schema Específico configurado para o evento {tpEvento}");

                lista.Add((NodetipoCorreto, eventoNode));
            }

            return lista;
        }


        /// <summary>
        /// Localiza o nó de tipo/schema pela ID do evento (tpEvento).
        /// </summary>
        private XmlNode EncontrarTipoSchemaPorId(XmlNode schemasEspecificos, string id)
        {
            if (schemasEspecificos is null)
                throw new Exception("Configuração de SchemasEspecificos não encontrada");

            if (string.IsNullOrWhiteSpace(id))
                throw new ArgumentException("ID não pode ser nulo ou vazio", nameof(id));

            foreach (XmlNode tipo in schemasEspecificos.SelectNodes("*[local-name()='Tipo']"))
            {
                string tipoId = tipo.SelectSingleNode("*[local-name()='ID']")?.InnerText;
                if (tipoId == id)
                    return tipo;
            }

            return null;
        }
    }
}
