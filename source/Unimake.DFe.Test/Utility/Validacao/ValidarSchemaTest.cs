using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Xml;
using System.Xml.Schema;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class ValidarSchemaTest
    {
        [Fact]
        public void DevePermitirValidacoesRepetidasNaMesmaInstancia()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml");
            var validar = new ValidarSchema();

            validar.Validar(
                xml,
                "CIOT.declaracaoOperacaoTransporte_v1.00.xsd",
                "http://www.antt.gov.br/ciot"
            );
            Assert.True(validar.Success, validar.ErrorMessage);

            validar.Validar(
                xml,
                "CIOT.declaracaoOperacaoTransporte_v1.00.xsd",
                "http://www.antt.gov.br/ciot"
            );
            Assert.True(validar.Success, validar.ErrorMessage);
        }

        [Fact]
        public void DeveInformarSchemaPrincipalQuandoRecursoNaoExistir()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<root />");
            var validar = new ValidarSchema();

            validar.Validar(xml, "Inexistente.schema_que_nao_existe.xsd");

            Assert.False(validar.Success);
            Assert.Contains("Inexistente.schema_que_nao_existe.xsd", validar.ErrorMessage);
        }

        [Fact]
        public void DeveResolverIncludesNaMesmaVersaoDoESocial()
        {
            var metodo = typeof(ValidarSchema).GetMethod(
                "ExtractSchemasResource",
                BindingFlags.NonPublic | BindingFlags.Instance
            );
            var validar = new ValidarSchema();
            var schemas = (IEnumerable<XmlSchema>)metodo.Invoke(
                validar,
                new object[]
                {
                    "ESocial.S_01_03_00.evtInfoEmpregador.xsd",
                    PadraoNFSe.None
                }
            );

            Assert.True(schemas.Count() >= 3);
        }
    }
}
