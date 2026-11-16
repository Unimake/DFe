using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public partial class PreparacaoUrlsQRCodeTest
    {
        // Cada serviço/versão com QR Code precisa de uma massa própria; uma nova entrada
        // no catálogo falha explicitamente até receber cobertura de integração offline.
        public static IEnumerable<object[]> MatrizCatalogo()
        {
            var catalogo = CatalogoValidacaoXML.CriarCopia();
            foreach (XmlElement servico in catalogo.SelectNodes("/ServicosValidacao/*/Servico[GerarQrCode='true']"))
            {
                var grupo = servico["GrupoUrl"];
                var referencia = grupo.GetAttribute("referenciaDFe");
                var origem = string.IsNullOrEmpty(referencia) ? grupo : catalogo.SelectSingleNode(
                    "/ServicosValidacao/" + referencia + "/Servico[@tagRaiz='" + servico.GetAttribute("tagRaiz")
                    + "' and @versao='" + servico.GetAttribute("versao") + "']/GrupoUrl");
                var ufs = origem.SelectNodes("Grupo/UF").Cast<XmlNode>().Select(n => n.InnerText).ToArray();
                if (ufs.Length == 0) ufs = Enum.GetValues(typeof(UFBrasil)).Cast<UFBrasil>()
                    .Where(uf => (int)uf >= 11 && (int)uf <= 53).Select(uf => uf.ToString()).ToArray();
                foreach (var uf in ufs.Distinct())
                    foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
                        foreach (var qrVersao in servico.ParentNode.Name == "NFCe" ? new[] { 2, 3 } : new[] { 3 })
                            yield return new object[] { servico.ParentNode.Name, servico.GetAttribute("tagRaiz"),
                                servico.GetAttribute("versao"), uf, ambiente, qrVersao };
            }
        }

        [Theory]
        [MemberData(nameof(MatrizCatalogo))]
        public void CatalogoCompleto_DeveManterQRCodeNoResultadoFinalERevalidacao(
            string documento, string raiz, string versao, string uf, TipoAmbiente ambiente, int qrVersao)
        {
            var tipo = (TipoDFe)Enum.Parse(typeof(TipoDFe), documento);
            var arquivo = ArquivoMatriz(documento, raiz, versao);
            var xml = CriarXml(tipo, arquivo, ambiente);
            if (xml.DocumentElement.LocalName == "cteProc")
            {
                var isolado = new XmlDocument();
                isolado.AppendChild(isolado.ImportNode(xml.DocumentElement["CTe"], true));
                xml = isolado;
            }
            if (raiz == "CTeOS" && versao == "3.00")
            {
                xml.DocumentElement.SetAttribute("versao", versao);
                ((XmlElement)xml.SelectSingleNode("//*[@versao][local-name()='infCte']")).SetAttribute("versao", versao);
                // A massa atual contém campos introduzidos após 3.00; manter uma operação
                // sintética compatível com o schema antigo, sem alterar os recursos originais.
                var valorLiquido = xml.SelectSingleNode("//*[local-name()='vTPrestLiq']");
                valorLiquido.ParentNode.RemoveChild(valorLiquido);
                var icms = xml.SelectSingleNode("//*[local-name()='ICMS']");
                icms.RemoveAll();
                var icms45 = xml.CreateElement("ICMS45", icms.NamespaceURI);
                var cst = xml.CreateElement("CST", icms.NamespaceURI);
                cst.InnerText = "40";
                icms45.AppendChild(cst);
                icms.AppendChild(icms45);
            }
            if (xml.DocumentElement.LocalName != raiz)
            {
                var filho = xml.DocumentElement.ChildNodes.Cast<XmlNode>().FirstOrDefault(n => n.LocalName == raiz);
                if (filho != null)
                {
                    var isolado = new XmlDocument();
                    isolado.AppendChild(isolado.ImportNode(filho, true));
                    xml = isolado;
                }
                else
                {
                    var lote = new XmlDocument();
                    var envelope = lote.CreateElement(raiz, xml.DocumentElement.NamespaceURI);
                    lote.AppendChild(envelope);
                    envelope.SetAttribute("versao", versao);
                    var idLote = lote.CreateElement("idLote", envelope.NamespaceURI);
                    idLote.InnerText = "000000000000001";
                    envelope.AppendChild(idLote);
                    if (raiz == "enviNFe")
                    {
                        var indSinc = lote.CreateElement("indSinc", envelope.NamespaceURI);
                        indSinc.InnerText = "1";
                        envelope.AppendChild(indSinc);
                    }
                    envelope.AppendChild(lote.ImportNode(xml.DocumentElement, true));
                    xml = lote;
                }
            }
            var codigoUF = (int)(UFBrasil)Enum.Parse(typeof(UFBrasil), uf);
            xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='cUF']").InnerText = codigoUF.ToString();
            if (tipo == TipoDFe.NFeABI)
            {
                var modelo = Business.DFe.Utility.XMLUtility.Deserializar<Business.DFe.Xml.NFeABI.NFeABI>(xml);
                modelo.InfNFeABI.Id = null;
                xml = modelo.GerarXML();
            }
            Assert.Equal(raiz, xml.DocumentElement.LocalName);
            Assert.Equal(versao, xml.SelectSingleNode("//*[@versao]").Attributes["versao"].Value);
            var configuracao = CriarConfiguracao(tipo, xml, qrVersao, ambiente);
            var catalogo = CatalogoValidacaoXML.CriarCopia();
            var servico = catalogo.SelectSingleNode("/ServicosValidacao/" + documento
                + "/Servico[@tagRaiz='" + raiz + "' and @versao='" + versao + "']");
            var suplemento = servico["GrupoUrl"].GetAttribute("tagSuplementar");
            var validador = new ValidarEstruturaXML();
            var resultado = validador.ValidarServico(xml, configuracao);

            // ABI ainda não possui URL de produção. Não inventar endpoint nem copiar homologação.
            if (tipo == TipoDFe.NFeABI && ambiente == TipoAmbiente.Producao)
            {
                Assert.False(resultado.Validado);
                Assert.Contains("A URL do QR Code da NFeABI não foi configurada", resultado.MensagemRetorno);
                Assert.True(string.IsNullOrEmpty(configuracao.UrlQrCodeProducao));
                Assert.Empty(xml.GetElementsByTagName(suplemento).Cast<XmlNode>());
                return;
            }
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            if (RegistroNormalizacaoXML.DeveNormalizar(tipo, raiz))
                Assert.Contains("normalizado", resultado.MensagemRetorno);
            ConferirResultadoMatriz(xml, suplemento);
            var urlEsperada = UrlEsperadaMatriz(servico, codigoUF, ambiente);
            Assert.StartsWith(urlEsperada + "?", xml.GetElementsByTagName(suplemento)[0].FirstChild.InnerText);
            Assert.Equal(codigoUF.ToString(), xml.SelectSingleNode("//*[local-name()='ide']/*[local-name()='cUF']").InnerText);
            Assert.Equal(versao, xml.SelectSingleNode("//*[@versao]").Attributes["versao"].Value);
            var original = xml.OuterXml;
            // Uma configuração nova obriga preparar novamente as URLs com suplemento já presente.
            resultado = validador.ValidarServico(xml, CriarConfiguracao(tipo, xml, qrVersao, ambiente));
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            ConferirResultadoMatriz(xml, suplemento);
            Assert.Equal(original, xml.OuterXml);
        }

        private static string UrlEsperadaMatriz(XmlNode servico, int codigoUF, TipoAmbiente ambiente)
        {
            var grupo = servico["GrupoUrl"];
            var servicoConfiguracao = grupo.GetAttribute("servicoConfiguracao");
            if (!string.IsNullOrEmpty(servicoConfiguracao))
            {
                var controle = new Configuracao { TipoDFe = TipoDFe.BPe, CodigoUF = codigoUF,
                    TipoAmbiente = ambiente, SchemaVersao = servico.Attributes["versao"].Value };
                controle.Load(servicoConfiguracao);
                return ambiente == TipoAmbiente.Homologacao ? controle.UrlQrCodeHomologacao : controle.UrlQrCodeProducao;
            }
            var referencia = grupo.GetAttribute("referenciaDFe");
            if (!string.IsNullOrEmpty(referencia)) grupo = (XmlElement)servico.OwnerDocument.SelectSingleNode(
                "/ServicosValidacao/" + referencia + "/Servico[@tagRaiz='" + servico.Attributes["tagRaiz"].Value
                + "' and @versao='" + servico.Attributes["versao"].Value + "']/GrupoUrl");
            var uf = ((UFBrasil)codigoUF).ToString();
            return grupo.SelectSingleNode("Grupo[UF='" + uf + "']/Urls/"
                + (ambiente == TipoAmbiente.Homologacao ? "UrlQrCodeHomologacao" : "UrlQrCodeProducao")).InnerText;
        }

        private static void ConferirResultadoMatriz(XmlDocument xml, string suplemento)
        {
            var grupo = Assert.Single(xml.GetElementsByTagName(suplemento).Cast<XmlElement>());
            Assert.Equal(xml.DocumentElement.NamespaceURI, grupo.NamespaceURI);
            Assert.False(string.IsNullOrWhiteSpace(grupo.FirstChild.InnerText));
            Assert.True(Uri.TryCreate(grupo.FirstChild.InnerText, UriKind.Absolute, out var uri));
            Assert.Contains(uri.Scheme, new[] { "http", "https" });
            var assinatura = Assert.Single(xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Cast<XmlElement>());
            var signed = new SignedXml(xml);
            signed.LoadXml(assinatura);
            Assert.True(signed.CheckSignature());
        }

        private static string ArquivoMatriz(string documento, string raiz, string versao)
        {
            switch (documento)
            {
                case "NFe": case "NFCe": return @"NFe\Resources\NFe1.xml";
                case "CTe":
                    if (raiz == "CTeOS") return @"CTeOS\Resources\CTeOS_AtualizacaoSchemas.xml";
                    if (raiz == "CTeSimp") return @"CTeSimp\Resources\CTeSimp_AtualizacaoSchemas.xml";
                    return versao == "3.00" ? @"Utility\Validacao\XMLteste\CTe\99999999999999999999999999999999999999999999-procCTe.xml" : @"CTe\Resources\CTe_AtualizacaoSchemas.xml";
                case "MDFe": return @"MDFe\Resources\99999999999999999999999999999999999999999999-procMDFe.xml";
                case "NFCom": return @"NFCom\Resources\NFCom_AtualizacaoSchemas.xml";
                case "NF3e": return @"NF3e\Resources\NF3e_AtualizacaoSchemas.xml";
                case "NFGas": return @"NFGas\Resources\nfgas-atualizacao-schema.xml";
                case "DCe": return @"DCe\Resources\dce.xml";
                case "NFeABI": return @"NFeABI\Resources\NFeABI-minima.xml";
                case "BPe": return @"BPe\Resources\" + (raiz == "BPe" ? "bpe_minimo.xml" : "bpeTA_minimo.xml");
                default: throw new InvalidOperationException("Massa ausente para " + documento + "/" + raiz + "/" + versao);
            }
        }
    }
}
