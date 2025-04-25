using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Unimake.Net;
using Xunit;

namespace Unimake.DFe.Test.NF3e
{
    /// <summary>
    /// Testar o serviço de consulta recibo da NF3e
    /// </summary>
    public class RecepcaoEventoTest
    {
        /// <summary>
        /// Consultar o recibo da NF3e somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a consulta recibo</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta recibo</param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void RecepcaoEventoEstadosXml(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new XmlDocument();
            xml.Load("..\\..\\..\\NF3e\\Resources\\eventoNF3e.xml");

            var evento = new EventoNF3e();
            var eventoObjeto = evento.LerXML<EventoNF3e>(xml);

            eventoObjeto.InfEvento.COrgao = (UFBrasil)(int)ufBrasil;
            eventoObjeto.InfEvento.TpAmb = (TipoAmbiente)(int)tipoAmbiente;

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                Servico = Servico.NF3eRecepcaoEvento,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
            };

            var recepcaoEvento = new RecepcaoEvento(eventoObjeto, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());

            if (configuracao.CodigoUF.Equals((int)UFBrasil.MG) || 
                configuracao.CodigoUF.Equals((int)UFBrasil.PR) || 
                configuracao.CodigoUF.Equals((int)UFBrasil.MT))
            {
                Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }
            else
            {
                Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(UFBrasil.AC), "Webservice retornou uma UF e está diferente de " + UFBrasil.RS + " (SVRS)");
            }

            Assert.True(recepcaoEvento.Result.InfEvento.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
        }

        /// <summary>
        /// Consultar o recibo da NF3e somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a consulta recibo</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta recibo</param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        public void RecepcaoEventoObjeto(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xmlEvento = new EventoNF3e
            {
                Versao = "1.00",
                InfEvento = new InfEvento(new DetEventoCanc
                {
                    VersaoEvento = "1.00",
                    DescEvento = "Cancelamento",
                    NProt = "12345678",
                    XJust = "Erro no valor do item 3"
                })
                {
                    COrgao = ufBrasil,
                    TpAmb = tipoAmbiente,
                    CNPJ = "06117473000150",
                    ChNF3e = "12345678901234567890123456789012345678901234",
                    DhEvento = System.DateTime.Now,
                    TpEvento = TipoEventoNF3e.Cancelamento,
                    NSeqEvento = 1,
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                Servico = Servico.NF3eRecepcaoEvento,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
            };

            var recepcaoEvento = new RecepcaoEvento(xmlEvento, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            if (configuracao.CodigoUF.Equals((int)UFBrasil.MG) || configuracao.CodigoUF.Equals((int)UFBrasil.PR) || 
                configuracao.CodigoUF.Equals((int)UFBrasil.MT) || configuracao.CodigoUF.Equals((int)UFBrasil.MS))
            {
                Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }
            else
            {
                Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(UFBrasil.AC), "Webservice retornou uma UF e está diferente de " + UFBrasil.RS + " (SVRS)");
            }

            Assert.True(recepcaoEvento.Result.InfEvento.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
        }
    }
}