using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Matchers
{
    internal class VincularCTe : IVinculadorSchema
    {
        public List<(XmlNode TipoSchema, XmlNode Node)> Vincular(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode NodeIdCorreto, XmlNode NodeInfCTe)>();
            var schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");

            if (schemasEspecificos is null)
                throw new Exception("Configuração de SchemasEspecificos não encontrada para CTe");

            var nodeInfCte = xml.GetElementsByTagName("infCte");

            if (nodeInfCte is null)
                throw new Exception("tag CTe não encontrada");

            foreach (XmlElement node in nodeInfCte)
            {
                foreach (XmlElement ideNode in node.GetElementsByTagName("ide"))
                {
                        var modal = ideNode.GetElementsByTagName("modal")[0]?.InnerText;

                        if (string.IsNullOrWhiteSpace(modal))
                            throw new Exception("Tag <modal> não encontrada ou está vazia");

                        // Extrai apenas o segundo caractere do modal
                        string modalCorreto = modal.Substring(1, 1);

                        XmlNode nodeTipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, modalCorreto);

                        if (nodeTipoCorreto is null)
                            throw new Exception($"Não existe Schema Específico configurado para o modal {modalCorreto}");

                        lista.Add((nodeTipoCorreto, node));
                    }
                }

            return lista;
        }


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
