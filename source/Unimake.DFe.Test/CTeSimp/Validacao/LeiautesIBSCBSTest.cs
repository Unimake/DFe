using System;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.CTeSimp;
using Xunit;
using ModeloCTeSimp = Unimake.Business.DFe.Xml.CTeSimp.CTeSimp;

namespace Unimake.DFe.Test.CTeSimp.Validacao
{
    [Trait("DFe", "CTeSimp")]
    public class LeiautesIBSCBSTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";
        private const string SchemaCTeSimp = "CTe.cteSimp_v4.00.xsd";
        private const string SchemaProcessado = "CTe.procCTeSimp_v4.00.xsd";

        [Theory]
        [InlineData(SchemaCTeSimp)]
        [InlineData(SchemaProcessado)]
        public void DocumentoNuloRetornaFalhaDeValidacao(string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar((XmlDocument)null, schema, NamespaceCTe);

            Assert.False(validador.Success);
            Assert.Equal(1, validador.ErrorCode);
            Assert.False(string.IsNullOrWhiteSpace(validador.ErrorMessage));
        }

        [Theory]
        [InlineData(false, TipoAmbiente.Homologacao)]
        [InlineData(true, TipoAmbiente.Producao)]
        public void AutorizacaoAssinaEValidaMantendoOGrupoNaPosicaoDeOrigem(bool legado, TipoAmbiente ambiente)
        {
            var xml = CriarXml(legado);
            var assinatura = xml.DocumentElement["Signature", "http://www.w3.org/2000/09/xmldsig#"];
            xml.DocumentElement.RemoveChild(assinatura);
            xml.GetElementsByTagName("tpAmb", NamespaceCTe)[0].InnerText = ((int)ambiente).ToString();
            var modelo = XMLUtility.Deserializar<ModeloCTeSimp>(xml);
            var gruposOriginais = ObterGrupos(modelo.GerarXML()).Select(grupo => grupo.OuterXml).ToArray();

            using (var rsa = RSA.Create(2048))
            using (var certificado = new CertificateRequest("CN=CTeSimp TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1)
                .CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
            using (var autorizacao = new AutorizacaoSimp(modelo, new Configuracao { TipoDFe = TipoDFe.CTe, CertificadoDigital = certificado }))
            {
                var xmlAssinado = autorizacao.ConteudoXMLAssinado;
                Assert.Single(xmlAssinado.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#"));
                var grupos = ObterGrupos(xmlAssinado);
                Assert.Equal(legado ? 1 : 3, grupos.Length);
                Assert.All(grupos, grupo => Assert.Equal(legado ? "imp" : "det", grupo.ParentNode.LocalName));
                Assert.Equal(gruposOriginais, grupos.Select(grupo => grupo.OuterXml));
            }
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void ValidaXMLProcessadoNosDoisLeiautes(bool legado)
        {
            var cte = XMLUtility.Deserializar<ModeloCTeSimp>(CriarXml(legado));
            var proc = new CteSimpProc
            {
                Versao = "4.00",
                CTeSimp = cte,
                ProtCTe = new ProtCTe
                {
                    Versao = "4.00",
                    InfProt = new InfProt
                    {
                        TpAmb = TipoAmbiente.Homologacao,
                        VerAplic = "TESTE",
                        ChCTe = cte.InfCTe.Chave,
                        DhRecbtoField = "2026-10-15T15:00:00-03:00",
                        NProt = "123456789012345",
                        CStat = 100,
                        XMotivo = "Autorizado o uso do CT-e"
                    }
                }
            };

            var validador = new ValidarSchema();
            validador.Validar(proc.GerarXML(), SchemaProcessado, NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        public void RejeitaGrupoNoCabecalhoMisturadoComTributacaoPorPrestacao()
        {
            var xml = CriarXml(true);
            var det = (XmlElement)xml.GetElementsByTagName("det", NamespaceCTe)[0];
            det.InsertBefore(ObterGrupos(xml)[0].CloneNode(true), det["infNFe", NamespaceCTe]);

            var validador = new ValidarSchema();
            validador.Validar(xml, SchemaCTeSimp, NamespaceCTe);
            Assert.False(validador.Success);
            Assert.Contains("IBSCBS", validador.ErrorMessage);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void ICMSPrevistoDePagamentoAntecipadoPertenceAoLeiauteAtual(bool legado)
        {
            var xml = CriarXml(legado);
            var icms = xml.GetElementsByTagName("ICMS", NamespaceCTe)[0];
            var grupo = xml.CreateElement("gICMSPrevistoPagtoAntecip", NamespaceCTe);
            grupo.AppendChild(xml.CreateElement("vICMSPrevisto", NamespaceCTe)).InnerText = "1.00";
            icms.ReplaceChild(grupo, icms.FirstChild);

            var validador = new ValidarSchema();
            validador.Validar(xml, SchemaCTeSimp, NamespaceCTe);
            Assert.Equal(!legado, validador.Success);
            if (legado)
            {
                Assert.Contains("gICMSPrevistoPagtoAntecip", validador.ErrorMessage);
            }
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void TributacaoDoSimplesNacionalPertenceAoLeiauteAtual(bool legado)
        {
            var xml = CriarXml(legado);
            var ibsCbs = (XmlElement)ObterGrupos(xml)[0];
            var grupo = xml.CreateElement("gTribSN", NamespaceCTe);
            var cbsSN = grupo.AppendChild(xml.CreateElement("gCBSSN", NamespaceCTe));
            cbsSN.AppendChild(xml.CreateElement("pCBSSN", NamespaceCTe)).InnerText = "0.9000";
            cbsSN.AppendChild(xml.CreateElement("vCBSSN", NamespaceCTe)).InnerText = "0.90";
            ibsCbs.InsertBefore(grupo, ibsCbs["gEstornoCred", NamespaceCTe]);

            var validador = new ValidarSchema();
            validador.Validar(xml, SchemaCTeSimp, NamespaceCTe);
            Assert.Equal(!legado, validador.Success);
            if (legado)
            {
                Assert.Contains("gTribSN", validador.ErrorMessage);
            }
        }

        [Fact]
        public void LeiauteLegadoContinuaValidandoOsCamposDoIBSCBS()
        {
            var xml = CriarXml(true);
            ObterGrupos(xml)[0]["CST", NamespaceCTe].InnerText = "INV";

            var validador = new ValidarSchema();
            validador.Validar(xml, SchemaCTeSimp, NamespaceCTe);
            Assert.False(validador.Success);
            Assert.Contains("CST", validador.ErrorMessage);
            Assert.Contains("INV", validador.ErrorMessage);
        }

        private static XmlDocument CriarXml(bool legado)
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\CTeSimp\Resources\CTeSimp_AtualizacaoSchemas.xml");
            if (legado)
            {
                var icms = xml.GetElementsByTagName("ICMS", NamespaceCTe)[0];
                var icms45 = xml.CreateElement("ICMS45", NamespaceCTe);
                icms45.AppendChild(xml.CreateElement("CST", NamespaceCTe)).InnerText = "40";
                icms.ReplaceChild(icms45, icms.FirstChild);

                var grupos = ObterGrupos(xml);
                xml.GetElementsByTagName("imp", NamespaceCTe)[0].AppendChild(grupos[0]);
                foreach (var grupo in grupos.Skip(1))
                {
                    grupo.ParentNode.RemoveChild(grupo);
                }

                var exclusivos = xml.SelectNodes("//*[local-name()='vPrestLiq' or local-name()='vTPrestLiq' or local-name()='IBSCBSTot']");
                foreach (XmlNode elemento in exclusivos)
                {
                    elemento.ParentNode.RemoveChild(elemento);
                }
            }

            return xml;
        }

        private static XmlNode[] ObterGrupos(XmlDocument xml) =>
            xml.GetElementsByTagName("IBSCBS", NamespaceCTe).Cast<XmlNode>().ToArray();
    }
}
