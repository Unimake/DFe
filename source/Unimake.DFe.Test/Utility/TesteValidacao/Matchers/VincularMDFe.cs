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
            var nodosInfMDFe = xml.GetElementsByTagName("infMDFe");

            if (nodosInfMDFe.Count == 0)
                throw new Exception("Tag 'infMDFe' não encontrada no XML");

            foreach (XmlElement nodeInfMDFe in nodosInfMDFe)
            {
                var infModals = nodeInfMDFe.GetElementsByTagName("infModal");

                if (infModals.Count == 0)
                    throw new Exception("Tag <infModal> não encontrada em infMDFe");

                foreach (XmlElement infModalNode in infModals)
                {
                    var modal = infModalNode.GetElementsByTagName("modal")[0]?.InnerText;

                    if (string.IsNullOrWhiteSpace(modal))
                        throw new Exception("Tag <modal> não encontrada ou está vazia em infModal");

                    // MDFe usa modal simples (1-4), não precisa substring
                    string modalCorreto = modal.Substring(0, 1);

                    XmlNode nodeTipoCorreto = EncontrarTipoSchemaPorId(schemasEspecificos, modalCorreto);

                    if (nodeTipoCorreto is null)
                        throw new Exception($"Não existe Schema Específico configurado para o modal {modalCorreto}");

                    lista.Add((nodeTipoCorreto, nodeInfMDFe));
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
