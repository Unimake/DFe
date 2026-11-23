using System;
using System.Linq;
using System.Runtime.Loader;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    [Collection("CTe.Serializadores")]
    public class ConsStatServCteTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";
        private const string Consulta300 = "<consStatServCte versao=\"3.00\" xmlns=\"" + NamespaceCTe + "\"><tpAmb>2</tpAmb><xServ>STATUS</xServ></consStatServCte>";
        private const string Consulta400 = "<consStatServCTe versao=\"4.00\" xmlns=\"" + NamespaceCTe + "\"><tpAmb>2</tpAmb><cUF>41</cUF><xServ>STATUS</xServ></consStatServCTe>";

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData("3.00", Consulta300)]
        [InlineData("4.00", Consulta400)]
        public void GerarXmlPreservaLayoutEValidaSchema(string versao, string esperado)
        {
            var consulta = new ConsStatServCte
            {
                Versao = versao,
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR
            };

            var xml = consulta.GerarXML();
            ConferirXml(esperado, xml);

            var validador = new ValidarSchema();
            validador.Validar(xml, $"CTe.consStatServCTe_v{versao}.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "CTe")]
        public void GerarXmlPreservaCamposDaSubclasse()
        {
            var consulta = new ConsultaDerivada
            {
                Versao = "4.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.PR,
                Complemento = "controle"
            };

            const string esperado = "<consStatServCTe versao=\"4.00\" xmlns=\"" + NamespaceCTe + "\"><tpAmb>2</tpAmb><cUF>41</cUF><xServ>STATUS</xServ><complemento>controle</complemento></consStatServCTe>";
            ConferirXml(esperado, consulta.GerarXML());
        }

        [Fact]
        [Trait("DFe", "CTe")]
        public async Task PrimeiroUsoConcorrenteGeraUmUnicoAssemblyDoSerializador()
        {
            // Outra cópia da DLL tem campos estáticos próprios, mesmo após os demais testes.
            var contexto = new AssemblyLoadContext(nameof(ConsStatServCteTest), isCollectible: true);
            try
            {
                var tipo = contexto.LoadFromAssemblyPath(typeof(ConsStatServCte).Assembly.Location)
                    .GetType(typeof(ConsStatServCte).FullName, throwOnError: true);
                const int concorrentes = 8;
                var consultas = Enumerable.Range(0, concorrentes).Select(_ => CriarConsultaIsolada(tipo)).ToArray();
                var antes = ContarAssembliesDeSerializacao();
                using var barreira = new Barrier(concorrentes);
                var tarefas = consultas.Select(gerarXml => Task.Factory.StartNew(() =>
                {
                    Assert.True(barreira.SignalAndWait(TimeSpan.FromSeconds(30)));
                    return gerarXml();
                }, CancellationToken.None, TaskCreationOptions.LongRunning, TaskScheduler.Default)).ToArray();

                foreach (var xml in await Task.WhenAll(tarefas))
                {
                    ConferirXml(Consulta400, xml);
                }

                // No runtime dos testes, cada construção com XmlRootAttribute emite um assembly.
                Assert.Equal(antes + 1, ContarAssembliesDeSerializacao());

                for (var i = 0; i < 20; i++)
                {
                    ConferirXml(Consulta400, CriarConsultaIsolada(tipo)());
                }

                Assert.Equal(antes + 1, ContarAssembliesDeSerializacao());
            }
            finally
            {
                contexto.Unload();
            }
        }

        private static Func<XmlDocument> CriarConsultaIsolada(Type tipo)
        {
            var consulta = Activator.CreateInstance(tipo);
            tipo.GetProperty(nameof(ConsStatServCte.Versao)).SetValue(consulta, "4.00");
            var ambiente = tipo.GetProperty(nameof(ConsStatServCte.TpAmb));
            ambiente.SetValue(consulta, Enum.ToObject(ambiente.PropertyType, (int)TipoAmbiente.Homologacao));
            var uf = tipo.GetProperty(nameof(ConsStatServCte.CUF));
            uf.SetValue(consulta, Enum.ToObject(uf.PropertyType, (int)UFBrasil.PR));
            return tipo.GetMethod(nameof(ConsStatServCte.GerarXML)).CreateDelegate<Func<XmlDocument>>(consulta);
        }

        private static int ContarAssembliesDeSerializacao() => AppDomain.CurrentDomain.GetAssemblies()
            .Count(assembly => assembly.IsDynamic && assembly.GetName().Name == "Microsoft.GeneratedCode");

        private static void ConferirXml(string esperado, XmlDocument xml)
        {
            var declaracao = Assert.IsType<XmlDeclaration>(xml.FirstChild);
            Assert.Equal("utf-8", declaracao.Encoding);
            Assert.Equal(esperado, xml.DocumentElement.OuterXml);
        }

        [XmlRoot("consultaDerivada", Namespace = NamespaceCTe, IsNullable = false)]
        public class ConsultaDerivada : ConsStatServCte
        {
            [XmlElement("complemento")]
            public string Complemento { get; set; }
        }
    }
}
