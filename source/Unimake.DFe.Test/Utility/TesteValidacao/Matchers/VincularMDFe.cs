using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.DFe.Test.Utility.TesteValidacao.Interfaces;

namespace Unimake.DFe.Test.Utility.TesteValidacao.Matchers
{
    internal class VincularMDFe : IVinculadorSchema
    {
        public List<(XmlNode TipoSchema, XmlNode Node)> Vincular(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode TipoSchema, XmlNode Node)>();
            var schemasEspecificos = servico.SelectSingleNode("*[local-name()='SchemasEspecificos']");

            if (schemasEspecificos is null)
                throw new Exception("Configuração de SchemasEspecificos não encontrada para MDFe");

            // Procura por infMDFe em qualquer nível (funciona para MDFe, enviMDFe)
            var nodeInfMDFe = xml.GetElementsByTagName("infMDFe");

            if (nodeInfMDFe is null)
                throw new Exception("Tag infMDFe não encontrada");

            foreach (XmlElement node in nodeInfMDFe)
            {

               foreach (XmlElement ideNode in node.GetElementsByTagName("ide"))
                { 
                    var modal = ideNode.GetElementsByTagName("modal")[0]?.InnerText;

                    if (string.IsNullOrEmpty(modal)) 
                    throw new Exception("Tag <modal> não encontrada ou está vazia");

                    XmlNode nodeTipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, modal);

                    if (nodeTipoCorreto is null)
                        throw new Exception($"Não foi encontrado o Schema Específico para o modal {modal}");

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
