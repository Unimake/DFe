using System;
using System.Threading;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.Utility.Xml
{
    public class SerializadorComRaizTest
    {
        [Fact]
        [Trait("Utility", "Xml")]
        public void ObterSerializador_MesmoTipoEMesmaRaiz_DeveReaproveitarOSerializador()
        {
            var primeiro = XMLUtility.ObterSerializador(typeof(ConsStatServCte),
                new XmlRootAttribute("consStatServCTe") { Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false });
            var segundo = XMLUtility.ObterSerializador(typeof(ConsStatServCte),
                new XmlRootAttribute("consStatServCTe") { Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false });
            var outraRaiz = XMLUtility.ObterSerializador(typeof(ConsStatServCte),
                new XmlRootAttribute("outraRaiz") { Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false });

            Assert.Same(primeiro, segundo);
            Assert.NotSame(primeiro, outraRaiz);
        }

        /// <summary>
        /// O XmlSerializer(Type, XmlRootAttribute) carrega um assembly novo a cada construção, que o runtime nunca descarrega.
        /// Depois da primeira, as serializações com a raiz trocada (como o GerarXML do ConsStatServCte 4.00) não podem
        /// carregar nenhum. Conta só os assemblies carregados nesta thread, para outro teste em paralelo não interferir.
        /// </summary>
        [Fact]
        [Trait("Utility", "Xml")]
        public void GerarXML_ComRaizTrocada_NaoDeveCarregarAssemblyNovoACadaChamada()
        {
            var pedido = new ConsStatServCte { Versao = "4.00", TpAmb = TipoAmbiente.Homologacao, CUF = UFBrasil.PR, XServ = "STATUS" };
            var esperado = pedido.GerarXML().OuterXml;

            var thread = Environment.CurrentManagedThreadId;
            var carregados = 0;
            void Contar(object sender, AssemblyLoadEventArgs e)
            {
                if (e.LoadedAssembly.IsDynamic && Environment.CurrentManagedThreadId == thread)
                {
                    Interlocked.Increment(ref carregados);
                }
            }

            AppDomain.CurrentDomain.AssemblyLoad += Contar;
            try
            {
                for (var i = 0; i < 20; i++)
                {
                    Assert.Equal(esperado, pedido.GerarXML().OuterXml);
                }
            }
            finally
            {
                AppDomain.CurrentDomain.AssemblyLoad -= Contar;
            }

            Assert.Equal(0, carregados);
            Assert.Contains("<consStatServCTe ", esperado);
        }
    }
}
