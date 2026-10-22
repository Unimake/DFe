using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;
using NacionalNFSe = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe;

namespace Unimake.DFe.Test.NFSe.Serializacao
{
    public class AtualizacaoSchemaNacionalTest
    {
        private const string Ns = "http://www.sped.fazenda.gov.br/nfse";

        private static DPS LerDPS() => XMLUtility.Deserializar<DPS>(File.ReadAllText(
            @"..\..\..\NFSe\Resources\NACIONAL\1.01\DPS-AjusteBC.xml"));

        private static XmlNode Selecionar(XmlDocument xml, string caminho)
        {
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("n", Ns);
            return xml.SelectSingleNode(caminho, ns);
        }

        private static void Validar(XmlDocument xml, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "NFSe.NACIONAL." + schema, Ns, PadraoNFSe.NACIONAL);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void MassaGeracaoNacionalDeveAtenderSchemaSemNormalizacao()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFSe\Resources\NACIONAL\1.01\GerarNfseEnvio-env-loterps.xml");

            Validar(xml, "DPS_v1.01.xsd");

            var modelo = new DPS().LerXML<DPS>(xml);
            var gerado = modelo.GerarXML();
            Assert.Equal(xml.InnerText, gerado.InnerText);
            Validar(gerado, "DPS_v1.01.xsd");
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveValidarDPSComAjustesImoveisEBensMoveis()
        {
            var modelo = LerDPS();
            Assert.Equal(FinalidadeNFSeRTC.Regular, modelo.InfDPS.FinNFSe);
            var ajuste = modelo.InfDPS.Valores.VAjusteBC;
            Assert.Equal(10, ajuste.PAjusteBCISSQN);
            Assert.Equal(2, ajuste.Documentos.DocAjusteBC.Count);
            Assert.Null(ajuste.Documentos.DocAjusteBC[0].Fornec.XNome);
            Assert.Equal(TipoAjusteBCNFSe.GlosaServicosSaude, ajuste.Documentos.DocAjusteBC[1].TpAjusteBC);
            var ibs = modelo.InfDPS.IBSCBS;
            Assert.Null(ibs.CIndOp);
            Assert.Equal(2, ibs.BensMoveis.Count);
            Assert.Equal("01012100", ibs.BensMoveis[0].CNCMBemMovel);
            Assert.Equal(2, ibs.Imovel.GUnidImob.Count);
            Assert.Equal("000123", ibs.Imovel.GUnidImob[0].InscImobFisc);
            Assert.Equal(2, ibs.Imovel.GUnidImob[0].GAjusteBCLocImoveis.Count);
            Assert.Null(ibs.Valores.Trib.GIBSCBS);
            Assert.Equal("000", ibs.Valores.Trib.CST);

            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.NotNull(Selecionar(xml, "/n:DPS/n:infDPS/n:finNFSe"));
            Assert.Null(Selecionar(xml, "//n:IBSCBS/n:finNFSe"));
            Assert.Null(Selecionar(xml, "//n:gIBSCBS/n:CST"));
            Assert.Null(Selecionar(xml, "//n:vDedRed"));
            Assert.Null(Selecionar(xml, "//n:gReeRepRes"));
            var relido = new DPS().LerXML<DPS>(xml);
            Assert.Equal(xml.InnerText, relido.GerarXML().InnerText);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(2)]
        [InlineData(5)]
        [InlineData(6)]
        [InlineData(7)]
        [InlineData(8)]
        [InlineData(9)]
        [InlineData(99)]
        [InlineData(101)]
        [InlineData(102)]
        [InlineData(103)]
        [InlineData(104)]
        [InlineData(105)]
        [InlineData(199)]
        public void DeveSerializarTodosOsTiposDeAjuste(int codigo)
        {
            var modelo = LerDPS();
            modelo.InfDPS.Valores.VAjusteBC.Documentos.DocAjusteBC[0].TpAjusteBC = (TipoAjusteBCNFSe)codigo;
            var xml = modelo.GerarXML();
            Assert.Equal(codigo.ToString(), Selecionar(xml, "//n:docAjusteBC/n:tpAjusteBC").InnerText);
            Validar(xml, "DPS_v1.01.xsd");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(1, "01")]
        [InlineData(2, "02")]
        [InlineData(3, "03")]
        [InlineData(4, "04")]
        [InlineData(99, "99")]
        public void DevePreservarZerosDoTipoDeAjusteImobiliario(int codigo, string esperado)
        {
            var modelo = LerDPS();
            modelo.InfDPS.IBSCBS.Imovel.GUnidImob[0].GAjusteBCLocImoveis[0].TpAjusteBCLocImoveis = (TipoAjusteBCLocImoveisNFSe)codigo;
            var xml = modelo.GerarXML();
            Assert.Equal(esperado, Selecionar(xml, "//n:gAjusteBCLocImoveis/n:tpAjusteBCLocImoveis").InnerText);
            Validar(xml, "DPS_v1.01.xsd");
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveOmitirOpcionaisEPreservarZeroExplicito()
        {
            var modelo = LerDPS();
            modelo.InfDPS.Valores.VAjusteBC = new VAjusteBC { VAjusteBCISSQN = 0 };
            var xml = modelo.GerarXML();
            Assert.Equal("0.00", Selecionar(xml, "//n:vAjusteBC/n:vAjusteBCISSQN").InnerText);
            Assert.Null(Selecionar(xml, "//n:vAjusteBC/n:pAjusteBCISSQN"));
            Assert.Null(Selecionar(xml, "//n:vAjusteBC/n:documentos"));
            Assert.Null(Selecionar(xml, "//n:gLocacao/n:vDescCondTot"));
            Assert.Equal("0.00", Selecionar(xml, "//n:gLocacao/n:vDescIncondTot").InnerText);
            Validar(xml, "DPS_v1.01.xsd");

            modelo.InfDPS.IBSCBS = null;
            modelo.InfDPS.Valores.VAjusteBC = null;
            xml = modelo.GerarXML();
            Assert.NotNull(Selecionar(xml, "/n:DPS/n:infDPS/n:finNFSe"));
            Validar(xml, "DPS_v1.01.xsd");
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveManterAPIAntigaDosCodigosEmitindoNoNovoNivel()
        {
            var modelo = LerDPS();
            modelo.InfDPS.IBSCBS.Valores.Trib = new IBSCBSValoresTrib
            {
                GIBSCBS = new GIBSCBS { CST = "000", CClassTrib = "000001" }
            };
            var xml = modelo.GerarXML();
            Assert.Equal("000", Selecionar(xml, "//n:IBSCBS/n:valores/n:trib/n:CST").InnerText);
            Assert.Null(Selecionar(xml, "//n:gIBSCBS"));
            Validar(xml, "DPS_v1.01.xsd");
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveValidarDocumentoNacionalEComplementosDosTributos()
        {
            var modelo = LerDPS();
            var documento = modelo.InfDPS.Valores.VAjusteBC.Documentos.DocAjusteBC[0];
            documento.DocOutro = null;
            documento.DfeNacional = new DfeNacional
            {
                TipoChaveDFe = TipoChaveDFe.NFe,
                Chave = new string('1', 44)
            };
            modelo.InfDPS.IBSCBS.Valores.Trib.GIBSCBS = new GIBSCBS
            {
                CCredPres = "01",
                GTribRegular = new GTribRegular { CSTReg = "000", CClassTribReg = "000001" },
                GDif = new GDif { PDifUF = 0, PDifMun = 0, PDifCBS = 0 }
            };
            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.NotNull(Selecionar(xml, "//n:trib/n:gIBSCBS/n:cCredPres"));
            Assert.Null(Selecionar(xml, "//n:trib/n:gIBSCBS/n:CST"));
            Assert.Equal(documento.DfeNacional.Chave,
                new DPS().LerXML<DPS>(xml).InfDPS.Valores.VAjusteBC.Documentos.DocAjusteBC[0].DfeNacional.Chave);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(true)]
        [InlineData(false)]
        public void SchemaDeveRejeitarAlternativasSimultaneas(bool conflitoDeValor)
        {
            var modelo = LerDPS();
            if (conflitoDeValor)
            {
                modelo.InfDPS.Valores.VAjusteBC.VAjusteBCISSQN = 100;
            }
            else
            {
                modelo.InfDPS.IBSCBS.Imovel.GUnidImob[0].End = new EndImovel
                {
                    CEP = "80000000", XLgr = "RUA EXEMPLO", Nro = "1"
                };
            }
            var validador = new ValidarSchema();
            validador.Validar(modelo.GerarXML(), "NFSe.NACIONAL.DPS_v1.01.xsd", Ns, PadraoNFSe.NACIONAL);
            Assert.False(validador.Success);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveValidarNFSeComNovosValoresCalculados()
        {
            var modelo = LerNFSe();
            var xml = modelo.GerarXML();
            Validar(xml, "NFSe_v1.01.xsd");
            var relido = new NacionalNFSe.NFSe().LerXML<NacionalNFSe.NFSe>(xml);
            Assert.Equal(100, relido.InfNFSe.Valores.VCalcAjusteBCISSQN);
            Assert.Equal(70, relido.InfNFSe.IBSCBS.Valores.VCalcAjusteBCIBSCBS);
            Assert.Equal(30, relido.InfNFSe.IBSCBS.Valores.VCalcAjusteBCLocImoveis);
            Assert.Equal(xml.InnerText, relido.GerarXML().InnerText);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveOmitirNovosOpcionaisDaNFSe()
        {
            var modelo = LerNFSe();
            modelo.InfNFSe.IBSCBS = new NacionalNFSe.IBSCBSNFSe
            {
                TotCIBS = new NacionalNFSe.TotCIBS { VTotNF = 1000 }
            };
            var xml = modelo.GerarXML();
            Assert.Null(Selecionar(xml, "/n:NFSe/n:infNFSe/n:IBSCBS/n:cLocalidadeIncid"));
            Assert.Null(Selecionar(xml, "/n:NFSe/n:infNFSe/n:IBSCBS/n:valores"));
            Validar(xml, "NFSe_v1.01.xsd");

            modelo.InfNFSe.IBSCBS.Valores = new NacionalNFSe.ValoresIBSCBS
            {
                UF = new NacionalNFSe.UF(), Mun = new NacionalNFSe.Mun(), Fed = new NacionalNFSe.Fed()
            };
            xml = modelo.GerarXML();
            Assert.Null(Selecionar(xml, "/n:NFSe/n:infNFSe/n:IBSCBS/n:valores/n:vBC"));
            Validar(xml, "NFSe_v1.01.xsd");
            modelo.InfNFSe.IBSCBS.Valores.VBC = 0;
            xml = modelo.GerarXML();
            Assert.Equal("0.00", Selecionar(xml, "/n:NFSe/n:infNFSe/n:IBSCBS/n:valores/n:vBC").InnerText);
            Validar(xml, "NFSe_v1.01.xsd");
        }

        private static NacionalNFSe.NFSe LerNFSe()
        {
            var modelo = XMLUtility.Deserializar<NacionalNFSe.NFSe>(File.ReadAllText(
                @"..\..\..\NFSe\Resources\NACIONAL\1.01\RetornoNACIONAL.xml"));
            modelo.InfNFSe.DPS = LerDPS();
            modelo.InfNFSe.Valores.VCalcAjusteBCISSQN = 100;
            modelo.InfNFSe.IBSCBS.Valores.VCalcAjusteBCIBSCBS = 70;
            modelo.InfNFSe.IBSCBS.Valores.VCalcAjusteBCLocImoveis = 30;
            return modelo;
        }

    }
}
