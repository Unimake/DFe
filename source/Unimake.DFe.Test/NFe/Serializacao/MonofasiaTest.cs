using System;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    public class MonofasiaTest
    {
        private const string NamespaceNFe = "http://www.portalfiscal.inf.br/nfe";
        private const string RecursoNFeAssinada = @"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml";

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveSerializarDesserializarMonofasiaAdRem()
        {
            var nfe = CriarNFeBase();
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "620",
                CClassTrib = "200032",
                GIBSCBSMono = CriarMonofasiaAdRem()
            };
            var gerado = XMLUtility.Serializar(nfe);
            ValidarSchema(gerado);

            var desserializada = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(gerado);
            var monofasia = desserializada.InfNFeField.Det.Select(x => x.Imposto?.IBSCBS?.GIBSCBSMono).First(x => x != null);

            Assert.Equal(0.5, monofasia.GIBSMonoAdRem.GMonoPadrao.AdRemIBS);
            Assert.Equal(0.75, monofasia.GCBSMonoAdRem.GMonoPadrao.AdRemCBS);
            Assert.Equal(1, monofasia.GIBSMonoAdRem.GPBioDiferenca.VIBSDiferenca);
            Assert.Equal(1.5, monofasia.GCBSMonoAdRem.GPBioDiferenca.VCBSDiferenca);

        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveSerializarDesserializarMonofasiaAdValorem()
        {
            var nfe = CriarNFeBase();
            var imposto = new IBSCBS { CST = "620", CClassTrib = "200032" };
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = imposto;

            imposto.GIBSCBSMono = new GIBSCBSMono
            {
                GIBSMonoAdValorem = new GIBSMonoAdValorem
                {
                    GMonoPadrao = new GMonoPadraoIBSAdValorem
                    {
                        VBCMono = 100,
                        PAliqMonoUF = 0.1,
                        VIBSMonoUF = 0.1,
                        PAliqMonoMun = 0.2,
                        VIBSMonoMun = 0.2,
                        VIBSMono = 0.3
                    },
                    GMonoReten = new GMonoRetenIBSAdValorem
                    {
                        VBCMonoReten = 50,
                        PAliqMonoReten = 0.3,
                        VIBSMonoReten = 0.15
                    },
                    GMonoRet = new GMonoRetIBS { VIBSMonoRet = 0.05 },
                    GPBioDiferenca = new GPBioDiferencaIBS { QBCBioComb = 2, VIBSDiferenca = 0.01 }
                },
                GCBSMonoAdValorem = new GCBSMonoAdValorem
                {
                    GMonoPadrao = new GMonoPadraoCBSAdValorem
                    {
                        VBCMono = 100,
                        PAliqMonoCBS = 0.4,
                        VCBSMono = 0.4
                    },
                    GMonoReten = new GMonoRetenCBSAdValorem
                    {
                        VBCMonoReten = 50,
                        PAliqMonoReten = 0.5,
                        VCBSMonoReten = 0.25
                    },
                    GMonoRet = new GMonoRetCBS { VCBSMonoRet = 0.06 },
                    GPBioDiferenca = new GPBioDiferencaCBS { QBCBioComb = 2, VCBSDiferenca = 0.02 }
                },
                VTotIBSMonoItem = 0.51,
                VTotCBSMonoItem = 0.73
            };

            var gerado = XMLUtility.Serializar(nfe);
            ValidarSchema(gerado);

            var desserializada = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(gerado);
            var monofasia = desserializada.InfNFeField.Det.Select(x => x.Imposto?.IBSCBS?.GIBSCBSMono).First(x => x != null);
            Assert.Equal(0.2, monofasia.GIBSMonoAdValorem.GMonoPadrao.PAliqMonoMun);
            Assert.Equal(0.4, monofasia.GCBSMonoAdValorem.GMonoPadrao.PAliqMonoCBS);
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveRespeitarNovosContratosDeIeVNFTotENNF()
        {
            var nfe = CriarNFeBase();
            var semVNFTot = XMLUtility.Serializar(nfe);
            Assert.Null(semVNFTot.SelectSingleNode("//*[local-name()='total']/*[local-name()='vNFTot']"));

            nfe.InfNFeField.Total.VNFTot = 0;
            var comVNFTotZero = XMLUtility.Serializar(nfe);
            Assert.Equal("0.00", comVNFTotZero.SelectSingleNode("//*[local-name()='total']/*[local-name()='vNFTot']").InnerText);
            ValidarSchema(comVNFTotZero);

            var procNFe = new XmlDocument();
            procNFe.Load(RecursoNFeAssinada);
            var doc = new XmlDocument();
            doc.LoadXml(procNFe.GetElementsByTagName("NFe")[0].OuterXml);
            var namespaceManager = new XmlNamespaceManager(doc.NameTable);
            namespaceManager.AddNamespace("nfe", NamespaceNFe);
            doc.SelectSingleNode("/nfe:NFe/nfe:infNFeSupl/nfe:qrCode", namespaceManager).InnerText = QrCodeValido;

            var ie = doc.SelectSingleNode("/nfe:NFe/nfe:infNFe/nfe:emit/nfe:IE", namespaceManager);
            ie.ParentNode.RemoveChild(ie);
            var total = doc.SelectSingleNode("/nfe:NFe/nfe:infNFe/nfe:total", namespaceManager);
            var vNFTot = doc.CreateElement("vNFTot", NamespaceNFe);
            vNFTot.InnerText = "0.00";
            total.AppendChild(vNFTot);
            ValidarSchema(doc);

            doc.SelectSingleNode("/nfe:NFe/nfe:infNFe/nfe:ide/nfe:nNF", namespaceManager).InnerText = "0";
            var validar = new ValidarSchema();
            validar.Validar(doc, "NFe.nfe_v4.00.xsd", NamespaceNFe);
            Assert.False(validar.Success);
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void ConversorTxtDeveGerarNovaEstruturaMonofasicaAdRem()
        {
            var resultado = new NFeTxtConverter().Converter(@"..\..\..\NFe\Resources\Txt\NFe_Reforma_Tributaria_Monofasica-nfe.txt", VersaoLeiauteMonofasia.Atual);

            Assert.True(resultado.Sucesso, resultado.MensagemErro);
            var xml = new XmlDocument();
            xml.LoadXml(Assert.Single(resultado.Documentos).Xml);
            Assert.NotNull(xml.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']"));
            Assert.NotNull(xml.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']"));
            Assert.Null(xml.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
            Assert.Equal("500.0000", xml.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='qBCMono']").InnerText);
            Assert.Equal("0.1000", xml.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("50.00", xml.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='vIBSMono']").InnerText);
            Assert.Equal("500.0000", xml.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='qBCMono']").InnerText);
            Assert.Equal("0.0500", xml.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
            Assert.Equal("25.00", xml.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='vCBSMono']").InnerText);
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveConverterPropriedadesLegadasParaNovaEstruturaAdRem()
        {
            var nfe = CriarNFeBase();
#pragma warning disable CS0618
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "620",
                CClassTrib = "200032",
                GIBSCBSMono = new GIBSCBSMono
                {
                    VersaoLeiaute = VersaoLeiauteMonofasia.Atual,
                    GMonoPadrao = new GMonoPadrao
                    {
                        QBCMono = 10,
                        AdRemIBS = 0.1,
                        AdRemCBS = 0.2,
                        VIBSMono = 1,
                        VCBSMono = 2
                    },
                    VTotIBSMonoItem = 1,
                    VTotCBSMonoItem = 2
                }
            };
#pragma warning restore CS0618

            var gerado = XMLUtility.Serializar(nfe);
            ValidarSchema(gerado);
            Assert.Equal("0.1000", gerado.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("0.2000", gerado.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
            Assert.Null(gerado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveAtualizarEstruturaNovaQuandoGrupoLegadoForAlteradoInternamente()
        {
            var nfe = CriarNFeBase();
            var padrao = new GMonoPadrao
            {
                QBCMono = 10,
                AdRemIBS = 0.1,
                AdRemCBS = 0.2,
                VIBSMono = 1,
                VCBSMono = 2
            };
            var retencao = new GMonoReten
            {
                QBCMonoReten = 5,
                AdRemIBSReten = 0.3,
                AdRemCBSReten = 0.4,
                VIBSMonoReten = 1.5,
                VCBSMonoReten = 2
            };
            var retido = new GMonoRet
            {
                VIBSMonoRet = 3,
                VCBSMonoRet = 4
            };
#pragma warning disable CS0618
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "620",
                CClassTrib = "200032",
                GIBSCBSMono = new GIBSCBSMono
                {
                    VersaoLeiaute = VersaoLeiauteMonofasia.Atual,
                    GMonoPadrao = padrao,
                    GMonoReten = retencao,
                    GMonoRet = retido,
                    VTotIBSMonoItem = 1,
                    VTotCBSMonoItem = 2
                }
            };
#pragma warning restore CS0618

            XMLUtility.Serializar(nfe);

            padrao.AdRemIBS = 0.9;
            padrao.AdRemCBS = 0.8;
            retencao.AdRemIBSReten = 0.7;
            retencao.AdRemCBSReten = 0.6;
            retido.VIBSMonoRet = 5;
            retido.VCBSMonoRet = 6;

            var geradoNovamente = XMLUtility.Serializar(nfe);
            ValidarSchema(geradoNovamente);
            Assert.Equal("0.9000", geradoNovamente.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("0.8000", geradoNovamente.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
            Assert.Equal("0.7000", geradoNovamente.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoReten']/*[local-name()='adRemIBSReten']").InnerText);
            Assert.Equal("0.6000", geradoNovamente.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoReten']/*[local-name()='adRemCBSReten']").InnerText);
            Assert.Equal("5.00", geradoNovamente.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoRet']/*[local-name()='vIBSMonoRet']").InnerText);
            Assert.Equal("6.00", geradoNovamente.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoRet']/*[local-name()='vCBSMonoRet']").InnerText);
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveSerializarLeiauteLegadoPorPadrao()
        {
            var nfe = CriarNFeBase();
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "620",
                CClassTrib = "200032",
                GIBSCBSMono = new GIBSCBSMono
                {
                    GMonoPadrao = new GMonoPadrao { QBCMono = 10, AdRemIBS = 0.1, AdRemCBS = 0.2, VIBSMono = 1, VCBSMono = 2 },
                    GMonoDif = new GMonoDif { PDifIBS = 1, VIBSMonoDif = 0.01, PDifCBS = 2, VCBSMonoDif = 0.02 },
                    VTotIBSMonoItem = 1,
                    VTotCBSMonoItem = 2
                }
            };

            var gerado = XMLUtility.Serializar(nfe);
            ValidarSchema(gerado);

            Assert.NotNull(gerado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
            Assert.NotNull(gerado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoDif']"));
            Assert.Null(gerado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gIBSMonoAdRem']"));

            var roundTrip = XMLUtility.Serializar(XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(gerado));
            ValidarSchema(roundTrip);
            Assert.NotNull(roundTrip.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DevePreservarLeiauteAtualNoRoundTrip()
        {
            var nfe = CriarNFeBase();
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "620",
                CClassTrib = "200032",
                GIBSCBSMono = CriarMonofasiaAdRem()
            };

            var gerado = XMLUtility.Serializar(nfe);
            var roundTrip = XMLUtility.Serializar(XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(gerado));
            ValidarSchema(roundTrip);

            Assert.NotNull(roundTrip.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gIBSMonoAdRem']"));
            Assert.Null(roundTrip.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void DeveRejeitarMisturaDosLeiautesMonofasicos()
        {
            var doc = new XmlDocument();
            doc.LoadXml("<NFe xmlns='http://www.portalfiscal.inf.br/nfe'><gIBSCBSMono><gIBSMonoAdRem/><gMonoPadrao/></gIBSCBSMono></NFe>");

            var exception = Assert.Throws<InvalidOperationException>(() => new ValidarSchema().Validar(doc, "NFe.nfe_v4.00.xsd", NamespaceNFe));
            Assert.Contains("não pode misturar", ObterMensagemCompleta(exception));
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void GrupoAtualExplicitoDevePrevalecerSobreAdaptacaoLegada()
        {
            var grupo = new GIBSCBSMono
            {
                GMonoPadrao = new GMonoPadrao { QBCMono = 10, AdRemIBS = 1, AdRemCBS = 2, VIBSMono = 10, VCBSMono = 20 },
                VersaoLeiaute = VersaoLeiauteMonofasia.Atual,
                GIBSMonoAdRem = new GIBSMonoAdRem
                {
                    GMonoPadrao = new GMonoPadraoIBSAdRem { QBCMono = 10, AdRemIBS = 3, VIBSMono = 30 }
                },
                VTotIBSMonoItem = 30,
                VTotCBSMonoItem = 20
            };
            var nfe = CriarNFeBase();
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS { CST = "620", CClassTrib = "200032", GIBSCBSMono = grupo };

            var gerado = XMLUtility.Serializar(nfe);

            ValidarSchema(gerado);
            Assert.Equal("3.0000", gerado.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("2.0000", gerado.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
            Assert.Null(gerado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
        }

        [Fact]
        [Trait("DFe", "NFe")]
        public void ConversorTxtDevePermitirLeiautesLegadoEAtual()
        {
            const string arquivo = @"..\..\..\NFe\Resources\Txt\NFe_Reforma_Tributaria_Monofasica-nfe.txt";
            var legado = new NFeTxtConverter().Converter(arquivo);
            var atual = new NFeTxtConverter().Converter(arquivo, VersaoLeiauteMonofasia.Atual);

            Assert.True(legado.Sucesso, legado.MensagemErro);
            Assert.True(atual.Sucesso, atual.MensagemErro);
            var xmlLegado = new XmlDocument();
            var xmlAtual = new XmlDocument();
            xmlLegado.LoadXml(Assert.Single(legado.Documentos).Xml);
            xmlAtual.LoadXml(Assert.Single(atual.Documentos).Xml);
            AdicionarAssinaturaParaValidacao(xmlLegado);
            AdicionarAssinaturaParaValidacao(xmlAtual);
            ValidarSchema(xmlLegado);
            ValidarSchema(xmlAtual);
            Assert.NotNull(xmlLegado.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gMonoPadrao']"));
            Assert.NotNull(xmlAtual.SelectSingleNode("//*[local-name()='gIBSCBSMono']/*[local-name()='gIBSMonoAdRem']"));
            Assert.Equal("0.1000", xmlLegado.SelectSingleNode("//*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("0.0500", xmlLegado.SelectSingleNode("//*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
            Assert.Equal("0.1000", xmlAtual.SelectSingleNode("//*[local-name()='gIBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemIBS']").InnerText);
            Assert.Equal("0.0500", xmlAtual.SelectSingleNode("//*[local-name()='gCBSMonoAdRem']/*[local-name()='gMonoPadrao']/*[local-name()='adRemCBS']").InnerText);
        }

        private static string ObterMensagemCompleta(System.Exception exception)
        {
            var mensagem = exception.Message;
            while (exception.InnerException != null)
            {
                exception = exception.InnerException;
                mensagem += " " + exception.Message;
            }
            return mensagem;
        }

        private static void AdicionarAssinaturaParaValidacao(XmlDocument doc)
        {
            var assinado = new XmlDocument();
            assinado.Load(RecursoNFeAssinada);
            var assinatura = assinado.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0];
            doc.DocumentElement.AppendChild(doc.ImportNode(assinatura, true));
        }

        private static Business.DFe.Xml.NFe.NFe CriarNFeBase()
        {
            var doc = new XmlDocument();
            doc.Load(RecursoNFeAssinada);
            var nfe = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(doc.GetElementsByTagName("NFe")[0].OuterXml);
            nfe.InfNFeSupl.QrCode = QrCodeValido;
            return nfe;
        }

        private const string QrCodeValido = "https://www.exemplo.gov.br/qrcode?p=41250899999999999900000000000000009000000000|3|2|01|0.00|1|12345678901|YWJjZA==";

        private static GIBSCBSMono CriarMonofasiaAdRem()
        {
            return new GIBSCBSMono
            {
                GIBSMonoAdRem = new GIBSMonoAdRem
                {
                    GMonoPadrao = new GMonoPadraoIBSAdRem { QBCMono = 100, AdRemIBS = 0.5, VIBSMono = 50 },
                    GMonoReten = new GMonoRetenIBSAdRem { QBCMonoReten = 50, AdRemIBSReten = 0.25, VIBSMonoReten = 12.5 },
                    GMonoRet = new GMonoRetIBS { VIBSMonoRet = 6 },
                    GPBioDiferenca = new GPBioDiferencaIBS { QBCBioComb = 2, VIBSDiferenca = 1 }
                },
                GCBSMonoAdRem = new GCBSMonoAdRem
                {
                    GMonoPadrao = new GMonoPadraoCBSAdRem { QBCMono = 100, AdRemCBS = 0.75, VCBSMono = 75 },
                    GMonoReten = new GMonoRetenCBSAdRem { QBCMonoReten = 50, AdRemCBSReten = 0.375, VCBSMonoReten = 18.75 },
                    GMonoRet = new GMonoRetCBS { VCBSMonoRet = 9 },
                    GPBioDiferenca = new GPBioDiferencaCBS { QBCBioComb = 2, VCBSDiferenca = 1.5 }
                },
                VTotIBSMonoItem = 125,
                VTotCBSMonoItem = 180
            };
        }

        private static void ValidarSchema(XmlDocument doc)
        {
            ValidarSchema(doc, "NFe.nfe_v4.00.xsd");
        }

        private static void ValidarSchema(XmlDocument doc, string schema)
        {
            var validar = new ValidarSchema();
            validar.Validar(doc, schema, NamespaceNFe);
            Assert.True(validar.Success, validar.ErrorMessage);
        }
    }
}
