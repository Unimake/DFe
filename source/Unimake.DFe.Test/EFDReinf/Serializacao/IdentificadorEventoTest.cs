using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using System.Xml.Schema;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf.Serializacao
{
    public class IdentificadorEventoTest
    {
        private const string IDNumerico = "ID1123456789012342026101012000000001";
        private const string IDAlfanumerico = "ID112ABC34501DEAB2026101012000000001";

        public static IEnumerable<object[]> Eventos()
        {
            yield return new object[] { typeof(Reinf1000), "1000_evtInfoContri", "R-1000-evtInfoContribuinte" };
            yield return new object[] { typeof(Reinf1050), "1050_evtTabLig", "R-1050-evt1050TabLig" };
            yield return new object[] { typeof(Reinf1070), "1070_evtTabProcesso", "R-1070-evtTabProcesso" };
            yield return new object[] { typeof(Reinf2010), "2010_evtServTom", "R-2010-evtTomadorServicos" };
            yield return new object[] { typeof(Reinf2020), "2020_evtServPrest", "R-2020-evtPrestadorServicos" };
            yield return new object[] { typeof(Reinf2030), "2030_evtAssocDespRec", "R-2030-evtRecursoRecebidoAssociacao" };
            yield return new object[] { typeof(Reinf2040), "2040_evtAssocDespRep", "R-2040-evtRecursoRepassadoAssociacao" };
            yield return new object[] { typeof(Reinf2050), "2050_evtComProd", "R-2050-evtInfoProdRural" };
            yield return new object[] { typeof(Reinf2055), "2055_evtAqProd", "R-2055-evt2055AquisicaoProdRural" };
            yield return new object[] { typeof(Reinf2060), "2060_evtCPRB", "R-2060-evtInfoCPRB" };
            yield return new object[] { typeof(Reinf2098), "2098_evtReabreEvPer", "R-2098-evtReabreEvPer" };
            yield return new object[] { typeof(Reinf2099), "2099_evtFechaEvPer", "R-2099-evtFechamento" };
            yield return new object[] { typeof(Reinf3010), "3010_evtEspDesportivo", "R-3010-evtEspDesportivo" };
            yield return new object[] { typeof(Reinf4010), "4010_evtRetPF", "R-4010-evt4010PagtoBeneficiarioPF" };
            yield return new object[] { typeof(Reinf4020), "4020_evtRetPJ", "R-4020-evt4020PagtoBeneficiarioPJ" };
            yield return new object[] { typeof(Reinf4040), "4040_evtBenefNId", "R-4040-evt4040PagtoBenefNaoIdentificado" };
            yield return new object[] { typeof(Reinf4080), "4080_evtRetRec", "R-4080-evt4080RetencaoRecebimento" };
            yield return new object[] { typeof(Reinf4099), "4099_evtFech", "R-4099-evt4099FechamentoDirf" };
            yield return new object[] { typeof(Reinf9000), "9000_evtExclusao", "R-9000-evtExclusao" };
            yield return new object[] { typeof(Reinf9001), "9001_evtTotal", "R-9001-evtTotal" };
            yield return new object[] { typeof(Reinf9005), "9005_evtRet", "R-9005-evtRet" };
            yield return new object[] { typeof(Reinf9011), "9011_evtTotalContrib", "R-9011-evtTotalContrib" };
            yield return new object[] { typeof(Reinf9015), "9015_evtRetCons", "R-9015-evtRetCons" };
        }

        public static IEnumerable<object[]> EventosSerializacao() => Eventos()
            .Select(item => new[] { item[0], item[1] });

        public static IEnumerable<object[]> EventosEnvio() => Eventos()
            .Where(item => int.Parse(((Type)item[0]).Name.Substring(5)) <= 9000)
            .Select(item => new[] { item[0], item[2] });

        [Theory]
        [Trait("DFe", "EFDReinf")]
        [MemberData(nameof(EventosSerializacao))]
        public void DevePreservarIDNumericoEAlfanumericoNoRoundTrip(Type tipo, string recurso)
        {
            var original = new XmlDocument();
            original.Load(@"..\..\..\EFDReinf\Resources\" + recurso + "-Reinf-evt.xml");
            var propriedade = tipo.GetProperties().Single(item => typeof(ReinfEventoBase).IsAssignableFrom(item.PropertyType));

            foreach (var id in new[] { IDNumerico, IDAlfanumerico })
            {
                var objeto = Deserializar(tipo, original);
                var evento = (ReinfEventoBase)propriedade.GetValue(objeto);
                evento.ID = id;
                var gerado = objeto.GerarXML();
                var elemento = (XmlElement)gerado.DocumentElement.SelectSingleNode("*[@id]");

                Assert.Equal(id, elemento.GetAttribute("id"));
                Assert.Equal(original.DocumentElement.NamespaceURI, gerado.DocumentElement.NamespaceURI);
                Assert.Equal(id, ((ReinfEventoBase)propriedade.GetValue(Deserializar(tipo, gerado))).ID);
            }
        }

        [Theory]
        [Trait("DFe", "EFDReinf")]
        [MemberData(nameof(EventosEnvio))]
        public void DeveAplicarNovoPatternDoIDApartirDoSchemaEmbutido(Type tipo, string schema)
        {
            var extrair = typeof(ValidarSchema).GetMethod("ExtractSchemasResource", BindingFlags.NonPublic | BindingFlags.Instance);
            var schemas = (IEnumerable<XmlSchema>)extrair.Invoke(new ValidarSchema(),
                new object[] { "EFDReinf." + schema + "-v2_01_02.xsd", PadraoNFSe.None });
            var conjunto = new XmlSchemaSet { XmlResolver = null };
            foreach (var item in schemas)
            {
                conjunto.Add(item);
            }
            conjunto.Compile();
            Assert.Equal(2, conjunto.Count);

            var ns = tipo.GetCustomAttribute<System.Xml.Serialization.XmlRootAttribute>().Namespace;
            var raiz = (XmlSchemaElement)conjunto.GlobalElements[new XmlQualifiedName("Reinf", ns)];
            var sequencia = (XmlSchemaSequence)((XmlSchemaComplexType)raiz.ElementSchemaType).ContentTypeParticle;
            var evento = (XmlSchemaElement)sequencia.Items[0];
            var atributo = (XmlSchemaAttribute)((XmlSchemaComplexType)evento.ElementSchemaType).AttributeUses[new XmlQualifiedName("id")];
            var datatype = atributo.AttributeSchemaType.Datatype;
            var nomes = new NameTable();
            var namespaces = new XmlNamespaceManager(nomes);

            // As letras nas posições finais da inscrição eram rejeitadas pelo pattern anterior.
            Assert.NotNull(datatype.ParseValue(IDAlfanumerico, nomes, namespaces));
            Assert.NotNull(datatype.ParseValue(IDNumerico, nomes, namespaces));
            Assert.Throws<XmlSchemaException>(() => datatype.ParseValue(IDAlfanumerico.ToLowerInvariant(), nomes, namespaces));
            Assert.Throws<XmlSchemaException>(() => datatype.ParseValue(IDAlfanumerico.Substring(1), nomes, namespaces));
            Assert.Throws<XmlSchemaException>(() => datatype.ParseValue(IDAlfanumerico + "0", nomes, namespaces));
            Assert.Throws<XmlSchemaException>(() => datatype.ParseValue(IDAlfanumerico.Substring(0, 17) + "A" + IDAlfanumerico.Substring(18), nomes, namespaces));
        }

        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(false)]
        [InlineData(true)]
        public void DeveAssinarEValidarIDAlfanumericoNoEventoENoLote(bool emLote)
        {
            var original = new XmlDocument();
            original.Load(@"..\..\..\EFDReinf\Resources\2098_evtReabreEvPer-Reinf-evt.xml");
            var evento = XMLUtility.Deserializar<Reinf2098>(original);
            evento.EvtReabreEvPer.ID = IDAlfanumerico;
            XmlDocument xml;
            if (emLote)
            {
                var lote = new ReinfEnvioLoteEventos
                {
                    EnvioLoteEventos = new EnvioLoteEventosReinf
                    {
                        IdeContribuinte = new IdeContribuinte { TpInsc = TiposInscricao.CNPJ, NrInsc = "12ABC345" },
                        Eventos = new EventosReinf
                        {
                            Evento = new List<EventoReinf> { new EventoReinf { ID = IDAlfanumerico, Reinf2098 = evento } }
                        }
                    }
                };
                xml = lote.GerarXML();
            }
            else
            {
                xml = evento.GerarXML();
            }

            using (var rsa = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=EFD REINF TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var resultado = new ValidarEstruturaXML().ValidarServico(xml, new Configuracao
                    {
                        TipoDFe = TipoDFe.EFDReinf,
                        CodigoUF = (int)UFBrasil.AN,
                        TipoAmbiente = TipoAmbiente.Homologacao,
                        CertificadoDigital = certificado
                    });
                    Assert.True(resultado.Validado, resultado.MensagemRetorno);
                    Assert.Equal(IDAlfanumerico, ((XmlElement)xml.SelectSingleNode("//*[@id]")).GetAttribute("id"));
                    Assert.Equal("#" + IDAlfanumerico, ((XmlElement)xml.SelectSingleNode("//*[local-name()='Reference']")).GetAttribute("URI"));
                    if (emLote)
                    {
                        var lido = XMLUtility.Deserializar<ReinfEnvioLoteEventos>(xml).EnvioLoteEventos.Eventos.Evento[0];
                        Assert.Equal(IDAlfanumerico, lido.ID);
                        Assert.Equal(IDAlfanumerico, lido.Reinf2098.EvtReabreEvPer.ID);
                    }
                }
            }
        }

        private static XMLBase Deserializar(Type tipo, XmlDocument xml)
        {
            var metodo = typeof(XMLUtility).GetMethods().Single(item => item.Name == nameof(XMLUtility.Deserializar)
                && item.IsGenericMethodDefinition && item.GetParameters().Length == 1
                && item.GetParameters()[0].ParameterType == typeof(XmlDocument));
            return (XMLBase)metodo.MakeGenericMethod(tipo).Invoke(null, new object[] { xml });
        }
    }
}
