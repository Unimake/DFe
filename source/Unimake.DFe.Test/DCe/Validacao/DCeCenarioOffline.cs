using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DCe;
using Xunit;
using XmlDCe = Unimake.Business.DFe.Xml.DCe.DCe;

namespace Unimake.DFe.Test.DCe.Validacao
{
    internal sealed class DCeCenarioOffline : IDisposable
    {
        internal const string Namespace = "http://www.portalfiscal.inf.br/dce";
        private readonly RSA chave = RSA.Create(2048);
        internal X509Certificate2 Certificado { get; }

        internal DCeCenarioOffline()
        {
            var pedido = new CertificateRequest("CN=AF05 DCe SINTETICO", chave,
                HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
            Certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddMinutes(-1), DateTimeOffset.UtcNow.AddHours(1));
        }

        internal Configuracao Configuracao() => new Configuracao
        {
            TipoDFe = TipoDFe.DCe,
            CodigoUF = (int)UFBrasil.PR,
            TipoAmbiente = TipoAmbiente.Homologacao,
            TipoEmissao = TipoEmissao.Normal,
            CertificadoDigital = Certificado
        };

        internal static XmlDCe Documento(bool codigoPais = true, bool nomePais = true,
            TipoEmitenteDCe tipo = TipoEmitenteDCe.EmissorProprio)
        {
            var documento = new XmlDCe
            {
                InfDCe = new InfDCe
                {
                    Versao = "1.00",
                    Ide = new Ide
                    {
                        CUF = UFBrasil.PR, CDC = "189439", Mod = ModeloDFe.DCe,
                        Serie = 59, NDC = 50,
#if INTEROP
                        DhEmi = new DateTimeOffset(2026, 11, 14, 12, 0, 0, TimeSpan.FromHours(-3)).LocalDateTime,
#else
                        DhEmi = new DateTimeOffset(2026, 11, 14, 12, 0, 0, TimeSpan.FromHours(-3)),
#endif
                        TpEmis = TipoEmissao.Normal, TpEmit = tipo, NSiteAutoriz = "0",
                        TpAmb = TipoAmbiente.Homologacao, VerProc = "AF05-OFFLINE"
                    },
                    Emit = new Emit
                    {
                        CNPJ = "00000000000199", XNome = "EMITENTE SINTETICO",
                        EnderEmit = new EnderEmit
                        {
                            XLgr = "RUA SINTETICA", Nro = "1", XBairro = "TESTE",
                            CMun = "4106902", XMun = "CURITIBA", UF = UFBrasil.PR,
                            CEP = "80010000", CPais = "1058", XPais = "BRASIL"
                        }
                    },
                    Fisco = tipo == TipoEmitenteDCe.AppFisco
                        ? new Fisco { CNPJ = "00000000000199", XOrgao = "FISCO SINTETICO", UF = UFBrasil.PR } : null,
                    Dest = new Dest
                    {
                        CPF = "12345678909", XNome = "DESTINATARIO SINTETICO",
                        EnderDest = new EnderDest
                        {
                            XLgr = "RUA SINTETICA", Nro = "2", XBairro = "TESTE",
                            CMun = "4106902", XMun = "CURITIBA", UF = UFBrasil.PR, CEP = "80010000",
                            CPais = codigoPais ? "1058" : null, XPais = nomePais ? "BRASIL" : null,
                            Email = "destinatario@example.com"
                        }
                    },
                    Det = new List<Det>
                    {
                        new Det { NItem = 1, Prod = new Prod { XProd = "PRODUTO SINTETICO", NCM = "99", QCom = 1, VUnCom = 10, VProd = 10 } }
                    },
                    Total = new Total { VDC = 10 },
                    Transp = new Transp { ModTrans = ModalidadeTransporteDCe.Correios, CNPJTransp = "00000000000199" },
                    InfDec = new InfDec { XObs1 = "DECLARACAO SINTETICA UM", XObs2 = "DECLARACAO SINTETICA DOIS" }
                }
            };
            documento.InfDCeSupl = new InfDCeSupl
            {
                QrCodDCe = "https://example.invalid/dce/consulta/validacao-offline?chDCe=" + documento.InfDCe.Chave + "&tpAmb=2",
                UrlChave = "https://example.invalid/dce/consulta"
            };
            return documento;
        }

        // Fixture DOM para confrontar os conjuntos oficiais independentemente do modelo,
        // construída antes da assinatura; nunca entra em serviço fiscal ou código distribuível.
        internal static void IncluirSolicitacaoFisco(XmlDocument documento)
        {
            Assert.Empty(documento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl));
            var grupo = documento.CreateElement("infSolicDCe", Namespace);
            var texto = documento.CreateElement("xSolic", Namespace);
            texto.InnerText = "PEDIDO SINTETICO OFFLINE";
            grupo.AppendChild(texto);
            documento.DocumentElement.InsertBefore(grupo, documento.GetElementsByTagName("infDCeSupl", Namespace)[0]);
        }

        internal XmlDocument Assinar(XmlDocument documento)
        {
            AssinaturaDigital.Assinar(documento, "DCe", "infDCe", Certificado);
            VerificarAssinatura(documento);
            return documento;
        }

        internal static XmlDocument Ler(string texto)
        {
            var documento = new XmlDocument { PreserveWhitespace = true, XmlResolver = null };
            using var reader = XmlReader.Create(new System.IO.StringReader(texto),
                new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null });
            documento.Load(reader);
            return documento;
        }

        internal static string Digest(XmlDocument documento)
        {
            var inf = (XmlElement)documento.GetElementsByTagName("infDCe", Namespace)[0];
            var ns = new XmlNamespaceManager(documento.NameTable);
            ns.AddNamespace("ds", SignedXml.XmlDsigNamespaceUrl);
            return documento.SelectSingleNode("//ds:Reference[@URI='#" + inf.GetAttribute("Id") + "']/ds:DigestValue", ns).InnerText;
        }

        internal static void VerificarAssinatura(XmlDocument documento)
        {
            var dce = (XmlElement)documento.GetElementsByTagName("DCe", Namespace)[0];
            var assinatura = Assert.IsType<XmlElement>(dce.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
            var signed = new SignedXml(dce);
            signed.LoadXml(assinatura);
            Assert.True(signed.CheckSignature(), "A assinatura sintética da DCe deve permanecer válida.");
        }

        internal static ProtDCe Protocolo(string chave, string digest, int status = 100) => new ProtDCe
        {
            Versao = "1.00",
            InfProt = new InfProt
            {
                TpAmb = TipoAmbiente.Homologacao, VerAplic = "AF05-OFFLINE", ChDCe = chave,
#if INTEROP
                DhRecbto = new DateTimeOffset(2026, 11, 14, 12, 1, 0, TimeSpan.FromHours(-3)).LocalDateTime,
#else
                DhRecbto = new DateTimeOffset(2026, 11, 14, 12, 1, 0, TimeSpan.FromHours(-3)),
#endif
                NProt = "1410000000000001", DigVal = digest, CStat = status, XMotivo = "RESPOSTA SINTETICA"
            }
        };

        internal static RetConsSitDCe RespostaConsulta(string chave, ProtDCe protocolo = null) => new RetConsSitDCe
        {
            Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, VerAplic = "AF05-OFFLINE",
            CStat = protocolo == null ? 217 : 100, XMotivo = "RESPOSTA SINTETICA", CUF = UFBrasil.PR,
#if INTEROP
            DhRecbto = new DateTimeOffset(2026, 11, 14, 12, 1, 0, TimeSpan.FromHours(-3)).LocalDateTime,
#else
            DhRecbto = new DateTimeOffset(2026, 11, 14, 12, 1, 0, TimeSpan.FromHours(-3)),
#endif
            ChDCe = chave, ProtDCe = protocolo
        };

        public void Dispose()
        {
            Certificado.Dispose();
            chave.Dispose();
        }
    }
}
