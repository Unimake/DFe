using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    // A contagem de assemblies pertence ao processo; nenhuma outra coleção pode executar junto.
    [CollectionDefinition("CTe.Serializadores", DisableParallelization = true)]
    public class SerializadoresCTeCollection
    {
    }

    [Collection("CTe.Serializadores")]
    public class EventoSimplesNacionalSerializadoresTest
    {
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(false)]
        [InlineData(true)]
        public void ProcessamentoRepetidoNaoCriaAssembliesDeSerializacao(bool concorrente)
        {
            var original = new XmlDocument();
            original.LoadXml(File.ReadAllText(@"..\..\..\CTe\Resources\eventoCTe_412140.xml"));

            void Processar()
            {
                var modelo = XMLUtility.Deserializar<EventoCTe>(original.OuterXml);
                var detalhe = Assert.IsType<DetEventoTributacaoSN>(modelo.InfEvento.DetEvento);
                Assert.Equal(2, detalhe.GTribItemSN.Count);
                Assert.Equal(1.2345, detalhe.GTribItemSN[0].PIBSSN);
                Assert.Equal(3.58, detalhe.GTotalSN.VCBSSNTot);
                Assert.Equal(original.InnerText, modelo.GerarXML().InnerText);
            }

            // A primeira passagem inicializa os serializadores e seus assemblies.
            Processar();
            var assembliesAntes = ContarAssembliesDeSerializacao();

            if (concorrente)
            {
                Parallel.For(0, 20, _ => Processar());
            }
            else
            {
                for (var i = 0; i < 20; i++)
                {
                    Processar();
                }
            }

            Assert.Equal(assembliesAntes, ContarAssembliesDeSerializacao());
        }

        private static int ContarAssembliesDeSerializacao() => AppDomain.CurrentDomain.GetAssemblies()
            .Count(assembly => assembly.IsDynamic && assembly.GetName().Name == "Microsoft.GeneratedCode");
    }
}
