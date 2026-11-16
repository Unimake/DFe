using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text.Json;
using System.Xml;
using System.Xml.Schema;
using Xunit;

namespace Unimake.DFe.Test.DCe.Validacao
{
    internal static class DCeSchemasOficiaisOffline
    {
        private const string Prefixo = "Unimake.DFe.Test.DCe.Resources.AF05.";

        private static byte[] Recurso(string nome)
        {
            using var entrada = typeof(DCeSchemasOficiaisOffline).Assembly.GetManifestResourceStream(Prefixo + nome);
            Assert.NotNull(entrada);
            using var memoria = new MemoryStream();
            entrada.CopyTo(memoria);
            return memoria.ToArray();
        }

        internal static void ConferirProveniencia()
        {
            using var manifesto = JsonDocument.Parse(Recurso("proveniencia.json"));
            Assert.Equal("9530d6c15f44de07cf4acc18566196a381f11051b59af3c2e36e3c8473701a6a",
                manifesto.RootElement.GetProperty("ArchiveSha256").GetString());
            Assert.Equal(15, manifesto.RootElement.GetProperty("Files").GetArrayLength());
            foreach (var arquivo in manifesto.RootElement.GetProperty("Files").EnumerateArray())
            {
                var recurso = arquivo.GetProperty("Resource").GetString();
                var esperado = arquivo.GetProperty("Sha256").GetString();
                Assert.Equal(esperado, Convert.ToHexString(SHA256.HashData(Recurso(recurso))).ToLowerInvariant());
            }
        }

        // Os XSDs oficiais ficam somente no assembly de testes. Includes/imports são ligados
        // em memória, sem resolver URLs, usar o disco ou substituir recursos da biblioteca.
        private static XmlSchema Carregar(string contexto, string arquivo, Dictionary<string, XmlSchema> schemas)
        {
            if (contexto != "Emissao" && contexto != "Consulta") throw new ArgumentException("Contexto inválido.");
            if (Path.GetFileName(arquivo) != arquivo || arquivo.Contains(":") || arquivo.Contains("\\"))
                throw new InvalidDataException("Referência de schema externa recusada.");
            var nome = contexto + "." + arquivo;
            if (schemas.TryGetValue(nome, out var existente)) return existente;
            using var reader = XmlReader.Create(new MemoryStream(Recurso(nome)),
                new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null });
            var schema = XmlSchema.Read(reader, null);
            Assert.NotNull(schema);
            schemas.Add(nome, schema);
            foreach (XmlSchemaExternal dependencia in schema.Includes)
                if (!string.IsNullOrWhiteSpace(dependencia.SchemaLocation))
                    dependencia.Schema = Carregar(contexto, dependencia.SchemaLocation, schemas);
            return schema;
        }

        internal static List<string> Validar(XmlDocument documento, string contexto, string arquivo,
            bool exporTipoDCeConsulta = false)
        {
            var schema = Carregar(contexto, arquivo, new Dictionary<string, XmlSchema>(StringComparer.Ordinal));
            if (exporTipoDCeConsulta)
            {
                // O pacote de consulta declara TDCe sem uma raiz DCe própria.
                // Este vínculo permite testar o tipo original, sem alterar seu conteúdo.
                var vinculo = new XmlSchema { TargetNamespace = DCeCenarioOffline.Namespace, ElementFormDefault = XmlSchemaForm.Qualified };
                vinculo.Includes.Add(new XmlSchemaInclude { Schema = schema });
                vinculo.Items.Add(new XmlSchemaElement
                {
                    Name = "DCe", SchemaTypeName = new XmlQualifiedName("TDCe", DCeCenarioOffline.Namespace)
                });
                schema = vinculo;
            }
            var conjunto = new XmlSchemaSet { XmlResolver = null };
            conjunto.Add(schema);
            conjunto.Compile();
            var erros = new List<string>();
            var settings = new XmlReaderSettings
            {
                DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null, ValidationType = ValidationType.Schema,
                Schemas = conjunto, ValidationFlags = XmlSchemaValidationFlags.ProcessIdentityConstraints | XmlSchemaValidationFlags.ReportValidationWarnings
            };
            settings.ValidationEventHandler += (_, e) => erros.Add(e.Message);
            using var leitura = XmlReader.Create(new StringReader(documento.OuterXml), settings);
            while (leitura.Read()) { }
            return erros;
        }
    }
}
