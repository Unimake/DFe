using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.GNRE;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Consultar uma chave do MDFe somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
    /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
    /// </summary>
    public class ConsultaConfigUFTest
    {
        /// <summary>
        /// Monta o parâmetros, de forma dinâmica, para o cenário de testes
        /// </summary>
        public static IEnumerable<object[]> Parametros => PreparaDadosCenario();

        [Theory]
        [Trait("DFe", "GNRE")]
        [MemberData(nameof(Parametros))]
        public void ConsultarConfigUFGNRETest(TipoAmbiente tipoAmbiente, UFBrasil uf, int receita, SimNaoLetra courier)
        {
            var xml = new TConsultaConfigUf
            {
                Ambiente = tipoAmbiente,
                UF = uf,
                Receita = new Receita
                {
                    Courier = courier,
                    Value = receita
                },
                TiposGnre = SimNaoLetra.Sim
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoArquivo = @"D:\projetos\Unimake_PV.pfx",
                CertificadoSenha = "12345678",
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)uf,
                Servico = Servico.GNREConsultaConfigUF
            };

            var consultaConfigUF = new ConsultaConfigUF(xml, configuracao);
            consultaConfigUF.Executar();

            Assert.True(consultaConfigUF.Result != null);
            Assert.True(consultaConfigUF.Result.Ambiente.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(consultaConfigUF.Result.SituacaoConsulta.Codigo.Equals("102") || consultaConfigUF.Result.SituacaoConsulta.Codigo.Equals("450") || consultaConfigUF.Result.SituacaoConsulta.Codigo.Equals("999"), "Código de retorno " + consultaConfigUF.Result.SituacaoConsulta.Codigo + " (" + consultaConfigUF.Result.SituacaoConsulta.Descricao + ") não esperado.");

            //Diag.Trace.WriteLine("Receita: " + receita + " Courier: " + courier.ToString() + " - Situação retorno: " + consultaConfigUF.Result.SituacaoConsulta.Codigo + "-" + consultaConfigUF.Result.SituacaoConsulta.Descricao, "Unimake.DFe.Test.GNRE.ConsultarConfigUFGNRETest()");

            //Testar a desserialização do retorno da receita
            var doc = new XmlDocument();
            doc.LoadXml(consultaConfigUF.RetornoWSString);

            var xmlRetorno = XMLUtility.Deserializar<TConfigUf>(doc);
            var doc2 = xmlRetorno.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado. (Receita: " + receita + "Courier: " + courier.ToString() + ")");
        }

        /// <summary>
        /// Preparar os dados para montagem do cenário de testes
        /// </summary>
        /// <returns>Retorna uma matriz com dados para elaboração do cenário de testes</returns>
        public static List<object[]> PreparaDadosCenario()
        {
            var dados = new List<object[]>();
            var ufs = new List<UFBrasil>();
            var receitas = new List<int>();

            ufs.Add(UFBrasil.AC);
            ufs.Add(UFBrasil.AL);
            ufs.Add(UFBrasil.AP);
            ufs.Add(UFBrasil.AM);
            ufs.Add(UFBrasil.BA);
            ufs.Add(UFBrasil.CE);
            ufs.Add(UFBrasil.DF);
            ufs.Add(UFBrasil.ES);
            ufs.Add(UFBrasil.GO);
            ufs.Add(UFBrasil.MA);
            ufs.Add(UFBrasil.MT);
            ufs.Add(UFBrasil.MS);
            ufs.Add(UFBrasil.MG);
            ufs.Add(UFBrasil.PA);
            ufs.Add(UFBrasil.PB);
            ufs.Add(UFBrasil.PR);
            ufs.Add(UFBrasil.PE);
            ufs.Add(UFBrasil.PI);
            ufs.Add(UFBrasil.RJ);
            ufs.Add(UFBrasil.RN);
            ufs.Add(UFBrasil.RS);
            ufs.Add(UFBrasil.RO);
            ufs.Add(UFBrasil.RR);
            ufs.Add(UFBrasil.SC);
            ufs.Add(UFBrasil.SP);
            ufs.Add(UFBrasil.SE);
            ufs.Add(UFBrasil.TO);

            receitas.Add(100013);
            receitas.Add(100021);
            receitas.Add(100030);
            receitas.Add(100048);
            receitas.Add(100056);
            receitas.Add(100064);
            receitas.Add(100072);
            receitas.Add(100080);
            receitas.Add(100099);
            receitas.Add(100102);
            receitas.Add(100110);
            receitas.Add(100129);
            receitas.Add(100137);
            receitas.Add(100145);
            receitas.Add(150010);
            receitas.Add(500011);
            receitas.Add(600016);

            foreach (var uf in ufs)
            {
                foreach (var receita in receitas)
                {
                    dados.Add(new object[] { TipoAmbiente.Homologacao, uf, receita, SimNaoLetra.Sim });
                    dados.Add(new object[] { TipoAmbiente.Homologacao, uf, receita, SimNaoLetra.Nao });
                    dados.Add(new object[] { TipoAmbiente.Producao, uf, receita, SimNaoLetra.Sim });
                    dados.Add(new object[] { TipoAmbiente.Producao, uf, receita, SimNaoLetra.Nao });
                }
            }

            return dados;
        }
    }
}
