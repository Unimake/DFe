using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCom;
using Unimake.Business.DFe.Xml.NFCom;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    /// <summary>
    /// Testar o serviço de recepção de eventos da NFCom
    /// </summary>
    public class RecepcaoEventoTest
    {
        [Theory]
        [Trait("DFe", "NFCom")]
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
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
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
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
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
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
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
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void RecepcaoEventosEstados(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new EventoNFCom
            {
                Versao = "1.00",
                InfEvento = new InfEvento(new DetEventoCanc
                {
                    VersaoEvento = "1.00",
                    DescEvento = "Cancelamento",
                    NProt = "1234567890123456",
                    XJust = "Erro na criacao do produto"
                })
                {
                    COrgao = ufBrasil,
                    TpAmb = tipoAmbiente,
                    CNPJ = "06117473000150",
                    ChNFCom = "12345678901234567890123456789012345678901234",
                    DhEvento = System.DateTime.Now,
                    TpEvento = TipoEventoNFCom.Cancelamento,
                    NSeqEvento = 1
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                TipoEmissao = TipoEmissao.Normal,
                Servico = Servico.NFComRecepcaoEvento,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente
            };

            var recepcaoEvento = new RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());

            if (!recepcaoEvento.Result.InfEvento.COrgao.Equals(UFBrasil.AC))
            {
                Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }

            Assert.True(recepcaoEvento.Result.InfEvento.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\eventoNFCom.xml")]
        public void RecepcaoEventoEstadosXml(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var evento = new EventoNFCom();
            var eventoObjeto = evento.LerXML<EventoNFCom>(xml);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                TipoEmissao = TipoEmissao.Normal,
                Servico = Servico.NFComRecepcaoEvento,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = (TipoAmbiente)(int)eventoObjeto.InfEvento.TpAmb,
            };

            var recepcaoEvento = new RecepcaoEvento(eventoObjeto, configuracao);
            recepcaoEvento.Executar();
        }
    }
}
