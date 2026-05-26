using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Interfaces;
using Unimake.Business.DFe.Xml.GNRE;

namespace Unimake.Business.DFe.Xml.Validar.Matchers
{
    internal class VincularEventoESocial : IVinculadorSchema
    {

        public List<(XmlNode TipoSchema, XmlNode NodeXml)> Vincular(XmlNode servico, XmlDocument xml)
        {
            var lista = new List<(XmlNode NodeEventoCorreto, XmlNode eventoNode)>();

            XmlNodeList tipos = servico.SelectNodes("*[local-name()='SchemasEspecificos']/*[local-name()='Tipo']");

            var eventos = xml.SelectNodes("/*[local-name()='eSocial']/*[local-name()='envioLoteEventos']/*[local-name()='eventos']/*[local-name()='evento']");

            foreach (XmlNode evento in eventos)
            {
                var eSocialEvento = evento.SelectSingleNode("*[local-name()='eSocial']");

                if (eSocialEvento is null)
                {
                    throw new Exception("Não foi encontrado o evento eSocial para validação em lote");
                }

                XmlNode nodeTipo = EncontrarTipoSchemaPorEvento(eSocialEvento, tipos);

                lista.Add((nodeTipo, evento));

            }

            return lista;

        }




        public XmlNode EncontrarTipoSchemaPorEvento(XmlNode eSocial, XmlNodeList tiposEventos)
        {
            foreach (XmlNode nodeTipo in tiposEventos)
            {
                var eventoNode = nodeTipo.SelectSingleNode("*[local-name()='Evento']");

                if (eventoNode is null)
                {
                    throw new Exception("Node Evento não configurado no Schema específico");
                }

                var tagEvento = eventoNode.InnerText;

                var eventoEncontrado = eSocial.SelectSingleNode($"*[local-name()='{tagEvento}']");

                if (eventoEncontrado != null)
                {
                    return nodeTipo;
                }

            }

            throw new Exception("Não existe Schema Específico configurado para o evento encontrado");

        }

    }




}
