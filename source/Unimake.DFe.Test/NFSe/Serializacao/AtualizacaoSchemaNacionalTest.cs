using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Unimake.Exceptions;
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
        public void DeveValidarComercioExteriorEventoEAjusteDeBase()
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt
            {
                MdPrestacao = ModoPrestacaoNFSe.ConsumoConcomitante,
                TpMoeda = "220",
                NFatura = "000123",
                VFatura = 0,
                NDUIMP = "00BR00000000000",
                NDUE = "00BR0000000000",
                NContCambio = "000045",
                NDI = 123,
                NRE = 456,
                vServMoeda = 100
            };
            modelo.InfDPS.Serv.AtvEvento = new AtvEvento
            {
                XNome = "EVENTO TESTE",
#if INTEROP
                DtIni = new DateTime(2026, 11, 15),
                DtFim = new DateTime(2026, 11, 16),
#else
                DtIni = new DateTimeOffset(2026, 11, 15, 0, 0, 0, TimeSpan.Zero),
                DtFim = new DateTimeOffset(2026, 11, 16, 0, 0, 0, TimeSpan.Zero),
#endif
                CMun = 3550308,
                IdAtvEvt = "123"
            };
            modelo.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt = -12.34m;

            const string serv = "/n:DPS/n:infDPS/n:serv";
            const string ajuste = "/n:DPS/n:infDPS/n:valores/n:vAjusteBC/n:vAjusteBCIBSCBSComExt";
            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.Equal("5", Selecionar(xml, serv + "/n:comExt/n:mdPrestacao")?.InnerText);
            Assert.Equal("0.00", Selecionar(xml, serv + "/n:comExt/n:vFatura")?.InnerText);
            Assert.Equal("0", Selecionar(xml, serv + "/n:comExt/n:mdic")?.InnerText);
            Assert.Null(Selecionar(xml, serv + "/n:comExt/n:vServMoeda"));
            Assert.Null(Selecionar(xml, serv + "/n:comExt/n:nDI"));
            Assert.Null(Selecionar(xml, serv + "/n:comExt/n:nRE"));
            Assert.Equal("3550308", Selecionar(xml, serv + "/n:atvEvento/n:cMun")?.InnerText);
            Assert.Equal("-12.34", Selecionar(xml, ajuste)?.InnerText);

            var relido = new DPS().LerXML<DPS>(xml);
            Assert.Equal(ModoPrestacaoNFSe.ConsumoConcomitante, relido.InfDPS.Serv.ComExt.MdPrestacao);
            Assert.Equal("000123", relido.InfDPS.Serv.ComExt.NFatura);
            Assert.Equal("00BR00000000000", relido.InfDPS.Serv.ComExt.NDUIMP);
            Assert.Equal("00BR0000000000", relido.InfDPS.Serv.ComExt.NDUE);
            Assert.Equal("000045", relido.InfDPS.Serv.ComExt.NContCambio);
            Assert.Equal(-12.34m, relido.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt);

            relido.InfDPS.Serv.AtvEvento.CMun = 0;
#if INTEROP
            relido.InfDPS.Serv.ComExt.VFatura = decimal.MinValue;
            relido.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt = decimal.MinValue;
#else
            relido.InfDPS.Serv.ComExt.VFatura = null;
            relido.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt = null;
#endif
            xml = relido.GerarXML();
            Assert.Null(Selecionar(xml, serv + "/n:comExt/n:vFatura"));
            Assert.Null(Selecionar(xml, serv + "/n:atvEvento/n:cMun"));
            Assert.Null(Selecionar(xml, ajuste));
            Validar(xml, "DPS_v1.01.xsd");
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveValidarCondominioEDestinatarioNaDPSENaNFSe()
        {
            var nfse = LerNFSe();
            nfse.InfNFSe.VerCalcIBSCBS = "1.0";
            var dps = nfse.InfNFSe.DPS;
            dps.InfDPS.IndDest = IndicadorDestinatarioNFSeRTC.DestinatarioDiferenteAdquirente;
            dps.InfDPS.Dest = new Dest { CNPJ = "12345678000195", XNome = "DESTINATARIO TESTE" };
            dps.InfDPS.IBSCBS.Condominios = new Condominios
            {
#if INTEROP
                DVencOrig = new DateTime(2026, 11, 30, 14, 15, 0),
#else
                DVencOrig = new DateTimeOffset(2026, 11, 30, 14, 15, 0, TimeSpan.FromHours(-3)),
#endif
                GCobranca = new List<CobrancaCondominio>
                {
                    new CobrancaCondominio
                    {
                        TpCobranca = TipoCobrancaCondominioNFSe.CondominioOrdinario,
                        VCobranca = 100,
                        GDetCobranca = new List<DetalhamentoCobrancaCondominio>
                        {
                            new DetalhamentoCobrancaCondominio
                            {
                                TpDetCobranca = TipoDetalhamentoCobrancaCondominioNFSe.EnergiaEletrica,
                                VDetCobranca = 50
                            },
                            new DetalhamentoCobrancaCondominio
                            {
                                TpDetCobranca = TipoDetalhamentoCobrancaCondominioNFSe.Agua,
                                VDetCobranca = 50
                            }
                        }
                    },
                    new CobrancaCondominio { TpCobranca = TipoCobrancaCondominioNFSe.FundoReserva, VCobranca = 20 }
                },
                GDesconto = new List<DescontoCondominio>
                {
                    new DescontoCondominio { TpDesconto = TipoDescontoCondominioNFSe.Condicionado, VDesconto = 10, XTpDesconto = "PONTUALIDADE" }
                }
            };

            var xmlDps = dps.GerarXML();
            Validar(xmlDps, "DPS_v1.01.xsd");
            var xmlNFSe = nfse.GerarXML();
            Validar(xmlNFSe, "NFSe_v1.01.xsd");
            Assert.Equal("1.0", Selecionar(xmlNFSe, "/n:NFSe/n:infNFSe/n:verCalcIBSCBS")?.InnerText);

            const string infDps = "/n:NFSe/n:infNFSe/n:DPS/n:infDPS";
            const string cond = infDps + "/n:IBSCBS/n:condominios";
            Assert.Equal("2026-11-30", Selecionar(xmlNFSe, cond + "/n:dVencOrig")?.InnerText);
            Assert.Equal("01", Selecionar(xmlNFSe, cond + "/n:gCobranca/n:tpCobranca")?.InnerText);
            Assert.Equal("007", Selecionar(xmlNFSe, cond + "/n:gCobranca/n:gDetCobranca/n:tpDetCobranca")?.InnerText);
            Assert.Equal("2", Selecionar(xmlNFSe, cond + "/n:gDesconto/n:tpDesconto")?.InnerText);
            Assert.Null(Selecionar(xmlNFSe, infDps + "/n:IBSCBS/n:indDest"));
            Assert.Null(Selecionar(xmlNFSe, infDps + "/n:IBSCBS/n:dest"));

            var relida = new NacionalNFSe.NFSe().LerXML<NacionalNFSe.NFSe>(xmlNFSe);
            Assert.Equal("1.0", relida.InfNFSe.VerCalcIBSCBS);
            Assert.Equal(dps.InfDPS.IndDest, relida.InfNFSe.DPS.InfDPS.IndDest);
            Assert.Equal(dps.InfDPS.Dest.CNPJ, relida.InfNFSe.DPS.InfDPS.Dest.CNPJ);
#pragma warning disable CS0618 // Verifica a leitura pelas propriedades legadas durante a transição.
            Assert.Equal(dps.InfDPS.IndDest, relida.InfNFSe.DPS.InfDPS.IBSCBS.IndDest);
            Assert.Same(relida.InfNFSe.DPS.InfDPS.Dest, relida.InfNFSe.DPS.InfDPS.IBSCBS.Dest);
#pragma warning restore CS0618
            var condRelido = relida.InfNFSe.DPS.InfDPS.IBSCBS.Condominios;
            Assert.Equal(new DateTime(2026, 11, 30), condRelido.DVencOrig.Date);
            Assert.Equal(2, condRelido.GCobranca.Count);
            Assert.Equal(2, condRelido.GCobranca[0].GDetCobranca.Count);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DevePreservarCentavosNosLimitesDoSchema()
        {
            const decimal limite = 999999999999999.99m;
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt { TpMoeda = "220", VFatura = limite };
            modelo.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt = -limite;
            modelo.InfDPS.IBSCBS.Condominios = new Condominios
            {
#if INTEROP
                DVencOrig = new DateTime(2026, 11, 30),
#else
                DVencOrig = new DateTimeOffset(2026, 11, 30, 0, 0, 0, TimeSpan.Zero),
#endif
                GCobranca = new List<CobrancaCondominio>
                {
                    new CobrancaCondominio
                    {
                        TpCobranca = TipoCobrancaCondominioNFSe.CondominioOrdinario,
                        VCobranca = limite,
                        GDetCobranca = new List<DetalhamentoCobrancaCondominio>
                        {
                            new DetalhamentoCobrancaCondominio
                            {
                                TpDetCobranca = TipoDetalhamentoCobrancaCondominioNFSe.EnergiaEletrica,
                                VDetCobranca = limite
                            }
                        }
                    }
                },
                GDesconto = new List<DescontoCondominio>
                {
                    new DescontoCondominio { TpDesconto = TipoDescontoCondominioNFSe.Condicionado, VDesconto = limite, XTpDesconto = "PONTUALIDADE" }
                }
            };

            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            var relido = new DPS().LerXML<DPS>(xml);
            Assert.Equal(limite, relido.InfDPS.Serv.ComExt.VFatura);
            Assert.Equal(-limite, relido.InfDPS.Valores.VAjusteBC.VAjusteBCIBSCBSComExt);
            var cond = relido.InfDPS.IBSCBS.Condominios;
            Assert.Equal(limite, cond.GCobranca[0].VCobranca);
            Assert.Equal(limite, cond.GCobranca[0].GDetCobranca[0].VDetCobranca);
            Assert.Equal(limite, cond.GDesconto[0].VDesconto);
            Assert.Equal(xml.OuterXml, relido.GerarXML().OuterXml);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveSincronizarDestinatarioLidoDoGrupoLegado()
        {
            var xml = LerDPS().GerarXML();
            const string infDps = "/n:DPS/n:infDPS";
            var grupo = Selecionar(xml, infDps + "/n:IBSCBS");
            var indicador = Selecionar(xml, infDps + "/n:indDest");
            indicador.InnerText = "1";
            grupo.InsertBefore(indicador, Selecionar(xml, infDps + "/n:IBSCBS/n:imovel"));
            var destinatario = xml.CreateElement("dest", Ns);
            var cnpj = xml.CreateElement("CNPJ", Ns);
            cnpj.InnerText = "12345678000195";
            destinatario.AppendChild(cnpj);
            var nome = xml.CreateElement("xNome", Ns);
            nome.InnerText = "DESTINATARIO TESTE";
            destinatario.AppendChild(nome);
            grupo.InsertAfter(destinatario, indicador);

            var modelo = new DPS().LerXML<DPS>(xml);
            Assert.Equal(IndicadorDestinatarioNFSeRTC.DestinatarioDiferenteAdquirente, modelo.InfDPS.IndDest);
#pragma warning disable CS0618 // Verifica a leitura pelas propriedades legadas durante a transição.
            Assert.Equal(modelo.InfDPS.IBSCBS.IndDest, modelo.InfDPS.IndDest);
            Assert.Same(modelo.InfDPS.IBSCBS.Dest, modelo.InfDPS.Dest);
#pragma warning restore CS0618
            Assert.Equal(cnpj.InnerText, modelo.InfDPS.Dest.CNPJ);
            var gerado = modelo.GerarXML();
            Validar(gerado, "DPS_v1.01.xsd");
            Assert.Null(Selecionar(gerado, infDps + "/n:indDest"));
            Assert.Null(Selecionar(gerado, infDps + "/n:dest"));
            Assert.Equal("1", Selecionar(gerado, infDps + "/n:IBSCBS/n:indDest")?.InnerText);
            Assert.Equal(cnpj.InnerText, Selecionar(gerado, infDps + "/n:IBSCBS/n:dest/n:CNPJ")?.InnerText);

            var indicadorLido = modelo.InfDPS.IndDest;
            modelo.InfDPS.IndDest = indicadorLido;
            modelo.InfDPS.IBSCBS.IndDestSpecified = false;
            gerado = modelo.GerarXML();
            Assert.Equal("1", Selecionar(gerado, infDps + "/n:indDest")?.InnerText);
            Assert.Null(Selecionar(gerado, infDps + "/n:IBSCBS/n:indDest"));

            modelo.InfDPS.IndDestSpecified = false;
            modelo.InfDPS.IBSCBS = null;
            Assert.Null(Selecionar(modelo.GerarXML(), infDps + "/n:indDest"));
            Assert.Null(modelo.InfDPS.Dest);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveIsolarDestinatarioAoReutilizarIBSCBS()
        {
            var primeira = LerDPS();
            var segunda = LerDPS();
            var grupo = primeira.InfDPS.IBSCBS;
            var primeiroDestinatario = new Dest { CNPJ = "12345678000195", XNome = "DESTINATARIO TESTE UM" };
            var segundoDestinatario = new Dest { CNPJ = "12345678000276", XNome = "DESTINATARIO TESTE DOIS" };
            primeira.InfDPS.Dest = primeiroDestinatario;
            primeira.InfDPS.IndDest = IndicadorDestinatarioNFSeRTC.DestinatarioDiferenteAdquirente;
            segunda.InfDPS.Dest = segundoDestinatario;
            segunda.InfDPS.IndDest = IndicadorDestinatarioNFSeRTC.ProprioTomadorAdquirente;
            segunda.InfDPS.IBSCBS = grupo;

            Assert.Same(grupo, primeira.InfDPS.IBSCBS);
            Assert.NotSame(grupo, segunda.InfDPS.IBSCBS);
#pragma warning disable CS0618 // Verifica a leitura pelas propriedades legadas.
            Assert.Same(primeiroDestinatario, primeira.InfDPS.IBSCBS.Dest);
            Assert.Same(segundoDestinatario, segunda.InfDPS.IBSCBS.Dest);
            Assert.Equal(primeira.InfDPS.IndDest, primeira.InfDPS.IBSCBS.IndDest);
            Assert.Equal(segunda.InfDPS.IndDest, segunda.InfDPS.IBSCBS.IndDest);
#pragma warning restore CS0618

            const string infDps = "/n:DPS/n:infDPS";
            foreach (var modelo in new[] { primeira, segunda })
            {
                var xml = modelo.GerarXML();
                Validar(xml, "DPS_v1.01.xsd");
                Assert.Equal(modelo.InfDPS.Dest.XNome, Selecionar(xml, infDps + "/n:dest/n:xNome")?.InnerText);
                Assert.Null(Selecionar(xml, infDps + "/n:IBSCBS/n:dest"));
            }

            segunda.InfDPS.IBSCBS = null;
#pragma warning disable CS0618 // O desvínculo da segunda DPS não altera o acesso legado da primeira.
            Assert.Same(primeiroDestinatario, primeira.InfDPS.IBSCBS.Dest);
            Assert.Equal(primeira.InfDPS.IndDest, primeira.InfDPS.IBSCBS.IndDest);
#pragma warning restore CS0618
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveCopiarDestinatarioLegadoAoReutilizarIBSCBS()
        {
            var grupo = new IBSCBS
            {
                DestField = new Dest
                {
                    XNome = "DESTINATARIO TESTE",
                    End = new End { EndNac = new EndNac { CEP = "01001000" } }
                }
            };
            var primeira = new InfDPS { IBSCBS = grupo };
            var segunda = new InfDPS { IBSCBS = grupo };

            segunda.IBSCBS.DestField.XNome = "DESTINATARIO ALTERADO";
            segunda.IBSCBS.DestField.End.EndNac.CEP = "02002000";

            Assert.Equal("DESTINATARIO TESTE", primeira.Dest.XNome);
            Assert.Equal("01001000", primeira.Dest.End.EndNac.CEP);
            Assert.Equal("DESTINATARIO ALTERADO", segunda.Dest.XNome);
            Assert.Equal("02002000", segunda.Dest.End.EndNac.CEP);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(false, false, false, false)]
        [InlineData(true, false, true, false)]
        [InlineData(false, true, false, true)]
        [InlineData(true, true, true, true)]
        public void DeveEmitirSomenteATagInformadaNaPosicaoEscolhida(
            bool indicadorLegado, bool destinatarioLegado, bool indicadorNovo, bool destinatarioNovo)
        {
            const string caminho = "/n:DPS/n:infDPS";
            var xml = LerDPS().GerarXML();
            var indicador = Selecionar(xml, caminho + "/n:indDest");
            indicador.ParentNode.RemoveChild(indicador);
            var modelo = new DPS().LerXML<DPS>(xml);
            var dps = modelo.InfDPS;

#pragma warning disable CS0618 // Exercita a emissão pela API pública legada.
            if (indicadorLegado)
            {
                dps.IBSCBS.IndDest = IndicadorDestinatarioNFSeRTC.ProprioTomadorAdquirente;
            }
            if (destinatarioLegado)
            {
                dps.IBSCBS.Dest = new Dest { CNPJ = "12345678000195", XNome = "DESTINATARIO LEGADO TESTE" };
            }
#pragma warning restore CS0618
            if (indicadorNovo)
            {
                dps.IndDest = IndicadorDestinatarioNFSeRTC.DestinatarioDiferenteAdquirente;
            }
            if (destinatarioNovo)
            {
                dps.Dest = new Dest { CNPJ = "12345678000195", XNome = "DESTINATARIO NOVO TESTE" };
            }

            var gerado = modelo.GerarXML();
            Assert.Equal(indicadorLegado ? "0" : null, Selecionar(gerado, caminho + "/n:IBSCBS/n:indDest")?.InnerText);
            Assert.Equal(indicadorNovo ? "1" : null, Selecionar(gerado, caminho + "/n:indDest")?.InnerText);
            Assert.Equal(destinatarioLegado ? "DESTINATARIO LEGADO TESTE" : null,
                Selecionar(gerado, caminho + "/n:IBSCBS/n:dest/n:xNome")?.InnerText);
            Assert.Equal(destinatarioNovo ? "DESTINATARIO NOVO TESTE" : null,
                Selecionar(gerado, caminho + "/n:dest/n:xNome")?.InnerText);
            Validar(gerado, "DPS_v1.01.xsd");
            Assert.Equal(gerado.OuterXml, new DPS().LerXML<DPS>(gerado).GerarXML().OuterXml);

            var nfse = LerNFSe();
            nfse.InfNFSe.DPS = modelo;
            Validar(nfse.GerarXML(), "NFSe_v1.01.xsd");
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData((ModoPrestacaoNFSe)4, "3")] // Valor gravado em binários que usavam o membro histórico.
        [InlineData(ModoPrestacaoNFSe.ConsumoExterior, "4")]
        public void DeveEmitirModoPrestacaoComOSignificadoDoLeiauteAtual(ModoPrestacaoNFSe modo, string codigo)
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt { MdPrestacao = modo, TpMoeda = "220" };
            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.Equal(codigo, Selecionar(xml, "/n:DPS/n:infDPS/n:serv/n:comExt/n:mdPrestacao")?.InnerText);
            Assert.Equal(modo, new DPS().LerXML<DPS>(xml).InfDPS.Serv.ComExt.MdPrestacao);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(ModoPrestacaoNFSe.PresencaComercialExteriorLegada, "3", ModoPrestacaoNFSe.MovimentoTemporarioPessoasFisicas)]
        [InlineData(ModoPrestacaoNFSe.MovimentoTemporarioPessoasFisicas, "4", ModoPrestacaoNFSe.ConsumoExterior)]
        public void DeveGerarELerComercioExteriorNoLeiauteLegado(
            ModoPrestacaoNFSe modoLegado, string codigo, ModoPrestacaoNFSe modoAtual)
        {
            var modelo = LerDPS();
            var comercio = new ComExt
            {
                UsarLeiauteLegado = true,
                MdPrestacao = modoLegado,
                TpMoeda = "220",
                vServMoedaField = "999999999999999.99",
                NDI = 1234,
                NREField = "00RE5678",
                NFatura = "123",
                VFatura = 100,
                NDUIMP = "00BR00000000000",
                NDUE = "00BR0000000000",
                NContCambio = "123"
            };
            modelo.InfDPS.Serv.ComExt = comercio;
            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");

            const string comExt = "/n:DPS/n:infDPS/n:serv/n:comExt";
            var grupo = Selecionar(xml, comExt);
            var tags = new List<string>();
            foreach (XmlNode tag in grupo.ChildNodes)
            {
                tags.Add(tag.LocalName);
            }
            Assert.Equal(new[] { "mdPrestacao", "vincPrest", "tpMoeda", "vServMoeda", "mecAFComexP", "mecAFComexT",
                "movTempBens", "nDI", "nRE", "mdic" }, tags);
            Assert.Equal(codigo, Selecionar(xml, comExt + "/n:mdPrestacao")?.InnerText);
            Assert.Equal("999999999999999.99", Selecionar(xml, comExt + "/n:vServMoeda")?.InnerText);
            Assert.Equal("1234", Selecionar(xml, comExt + "/n:nDI")?.InnerText);
            Assert.Equal("00RE5678", Selecionar(xml, comExt + "/n:nRE")?.InnerText);

            var relido = new DPS().LerXML<DPS>(xml);
            var comercioRelido = relido.InfDPS.Serv.ComExt;
            Assert.True(comercioRelido.UsarLeiauteLegado);
            Assert.True(comercioRelido.UsarLeiauteLegadoNaLeitura);
            Assert.Equal(modoLegado, comercioRelido.MdPrestacao);
            Assert.Equal(1234, comercioRelido.NDI);
            Assert.Equal("00RE5678", comercioRelido.NREField);
            Assert.Equal(xml.OuterXml, relido.GerarXML().OuterXml);

            comercioRelido.UsarLeiauteLegado = false;
            Assert.Equal(modoLegado, comercioRelido.MdPrestacao);
            if (modoLegado == ModoPrestacaoNFSe.MovimentoTemporarioPessoasFisicas)
            {
                var migrado = relido.GerarXML();
                Validar(migrado, "DPS_v1.01.xsd");
                Assert.Equal("3", Selecionar(migrado, comExt + "/n:mdPrestacao")?.InnerText);
            }
            else
            {
                Assert.Throws<InvalidOperationException>(() => relido.GerarXML());
            }

            comercioRelido.UsarLeiauteLegadoNaLeitura = false;
            Assert.Equal(modoAtual, comercioRelido.MdPrestacao);
            var geradoNoAtual = relido.GerarXML();
            Validar(geradoNoAtual, "DPS_v1.01.xsd");
            Assert.Equal(codigo, Selecionar(geradoNoAtual, comExt + "/n:mdPrestacao")?.InnerText);
            Assert.Null(Selecionar(geradoNoAtual, comExt + "/n:vServMoeda"));
            Assert.Null(Selecionar(geradoNoAtual, comExt + "/n:nDI"));
            Assert.Null(Selecionar(geradoNoAtual, comExt + "/n:nRE"));

            var lidoNoAtual = new DPS().LerXML<DPS>(geradoNoAtual).InfDPS.Serv.ComExt;
            Assert.False(lidoNoAtual.UsarLeiauteLegado);
            Assert.Equal(modoAtual, lidoNoAtual.MdPrestacao);
            lidoNoAtual.UsarLeiauteLegadoNaLeitura = true;
            Assert.Equal(modoLegado, lidoNoAtual.MdPrestacao);
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveRejeitarModoDePrestacaoHistoricoSemEquivalente()
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt
            {
                MdPrestacao = (ModoPrestacaoNFSe)3, // Presença comercial no exterior no enum legado.
                TpMoeda = "220"
            };
            Assert.Throws<InvalidOperationException>(() => modelo.GerarXML());
            modelo.InfDPS.Serv.ComExt.UsarLeiauteLegado = true;
            modelo.InfDPS.Serv.ComExt.MdPrestacao = ModoPrestacaoNFSe.ConsumoExterior;
            modelo.InfDPS.Serv.ComExt.vServMoeda = 0;
            Assert.Throws<InvalidOperationException>(() => modelo.GerarXML());
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveManterModoLidoAoPreencherValorParaEmissaoLegada()
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt
            {
                MdPrestacao = ModoPrestacaoNFSe.ConsumoExterior,
                TpMoeda = "220"
            };

            var relido = new DPS().LerXML<DPS>(modelo.GerarXML());
            var comExt = relido.InfDPS.Serv.ComExt;
            comExt.UsarLeiauteLegado = true;
            comExt.vServMoedaField = "125.00";

            Assert.False(comExt.UsarLeiauteLegadoNaLeitura);
            Assert.Equal(ModoPrestacaoNFSe.ConsumoExterior, comExt.MdPrestacao);
            Assert.Throws<InvalidOperationException>(() => relido.GerarXML());
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData("nFatura", "<nFatura>")]
        [InlineData("nDUIMP", "<nDUIMP>")]
        [InlineData("vAjusteBCIBSCBSComExt", "<vAjusteBCIBSCBSComExt>")]
        [InlineData("mdPrestacao", "valor 5 somente no leiaute atual")]
        [InlineData("vServMoeda", "<vServMoeda> é obrigatória")]
        public void DeveRejeitarComercioExteriorIncompativelEmXmlPronto(string tagAlterada, string erroEsperado)
        {
            var nfse = LerNFSe();
            var dps = nfse.InfNFSe.DPS;
            dps.InfDPS.Serv.ComExt = new ComExt
            {
                UsarLeiauteLegado = true,
                MdPrestacao = ModoPrestacaoNFSe.MovimentoTemporarioPessoasFisicas,
                TpMoeda = "220",
                vServMoeda = 125,
                NDI = 1234
            };

            foreach (var xml in new[] { dps.GerarXML(), nfse.GerarXML() })
            {
                var caminho = xml.DocumentElement.LocalName == "DPS" ? "/n:DPS" : "/n:NFSe/n:infNFSe/n:DPS";
                var comExt = Selecionar(xml, caminho + "/n:infDPS/n:serv/n:comExt");
                var valor = Selecionar(xml, caminho + "/n:infDPS/n:serv/n:comExt/n:vServMoeda");
                switch (tagAlterada)
                {
                    case "nFatura":
                        var fatura = xml.CreateElement("nFatura", Ns);
                        fatura.InnerText = "00123";
                        comExt.InsertAfter(fatura, valor);
                        break;
                    case "nDUIMP":
                        var duimp = xml.CreateElement("nDUIMP", Ns);
                        duimp.InnerText = "00BR00000000000";
                        comExt.InsertBefore(duimp, comExt.LastChild);
                        break;
                    case "mdPrestacao":
                        comExt.FirstChild.InnerText = "5";
                        break;
                    case "vAjusteBCIBSCBSComExt":
                        var ajuste = xml.CreateElement("vAjusteBCIBSCBSComExt", Ns);
                        ajuste.InnerText = "-12.34";
                        Selecionar(xml, caminho + "/n:infDPS/n:valores/n:vAjusteBC").AppendChild(ajuste);
                        break;
                    case "vServMoeda":
                        comExt.RemoveChild(valor);
                        break;
                }

                Validar(xml, xml.DocumentElement.LocalName + "_v1.01.xsd");

                var configuracao = new Configuracao
                {
                    Definida = true,
                    PadraoNFSe = PadraoNFSe.NACIONAL,
                    SchemaVersao = "1.01"
                };
                var erro = Assert.Throws<ValidatorDFeException>(() =>
                    new Business.DFe.Servicos.NFSe.GerarNfse(xml, configuracao));
                Assert.Contains(erroEsperado, erro.Message);
            }
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(true)]
        [InlineData(false)]
        public void DeveExigirVServMoedaNoLegadoEAceitarZeroExplicito(bool preencherCampoXml)
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt
            {
                MdPrestacao = ModoPrestacaoNFSe.MovimentoTemporarioPessoasFisicas,
                TpMoeda = "220",
                VFatura = 125
            };

            var relido = new DPS().LerXML<DPS>(modelo.GerarXML());
            var comExt = relido.InfDPS.Serv.ComExt;
            comExt.UsarLeiauteLegado = true;
            var erro = Assert.Throws<InvalidOperationException>(() => relido.GerarXML());
            Assert.Contains("<vServMoeda>", erro.GetBaseException().Message);

            if (preencherCampoXml)
            {
                comExt.vServMoedaField = "0.00";
            }
            else
            {
                comExt.vServMoeda = 0;
            }

            var xml = relido.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.Equal("0.00", Selecionar(xml, "/n:DPS/n:infDPS/n:serv/n:comExt/n:vServMoeda")?.InnerText);
            Assert.Equal(xml.OuterXml, new DPS().LerXML<DPS>(xml).GerarXML().OuterXml);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(123456789012345.25, "123456789012345.25")]
        [InlineData(999999999999999.75, "999999999999999.75")]
        public void DevePreservarCentavosDeVServMoedaAtribuidoPelaPropriedadeLegada(double valor, string esperado)
        {
            var modelo = LerDPS();
            var comercio = new ComExt
            {
                UsarLeiauteLegado = true,
                TpMoeda = "220",
                vServMoeda = valor
            };
            modelo.InfDPS.Serv.ComExt = comercio;

            Assert.Equal(valor, comercio.vServMoeda);
            var xml = modelo.GerarXML();
            Validar(xml, "DPS_v1.01.xsd");
            Assert.Equal(esperado, Selecionar(xml, "/n:DPS/n:infDPS/n:serv/n:comExt/n:vServMoeda")?.InnerText);
            Assert.Equal(xml.OuterXml, new DPS().LerXML<DPS>(xml).GerarXML().OuterXml);
        }

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData("04")]
        [InlineData("+4")]
        [InlineData(" 4 ")]
        [InlineData("6")]
        public void DeveRejeitarMdPrestacaoForaDaEnumeracaoTextualAoLerXml(string codigo)
        {
            var modelo = LerDPS();
            modelo.InfDPS.Serv.ComExt = new ComExt
            {
                MdPrestacao = ModoPrestacaoNFSe.ConsumoExterior,
                TpMoeda = "220"
            };
            var xml = modelo.GerarXML();
            Selecionar(xml, "/n:DPS/n:infDPS/n:serv/n:comExt/n:mdPrestacao").InnerText = codigo;
            var erro = Assert.Throws<XmlException>(() => new DPS().LerXML<DPS>(xml));
            Assert.IsType<FormatException>(erro.GetBaseException());
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
        public void DeveManterEstruturaLegadaDosCodigos()
        {
            var modelo = LerDPS();
            modelo.InfDPS.IBSCBS.Valores.Trib = new IBSCBSValoresTrib
            {
                GIBSCBS = new GIBSCBS { CST = "000", CClassTrib = "000001" }
            };
            var xml = modelo.GerarXML();
            Assert.Equal("000", Selecionar(xml, "//n:IBSCBS/n:valores/n:trib/n:gIBSCBS/n:CST").InnerText);
            Assert.Equal("000001", Selecionar(xml, "//n:IBSCBS/n:valores/n:trib/n:gIBSCBS/n:cClassTrib").InnerText);
            Assert.Null(Selecionar(xml, "//n:IBSCBS/n:valores/n:trib/n:CST"));
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

        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData(true)]
        [InlineData(false)]
        public void NFSeDeveGerarCServNovoNoLeiauteDaDPSInterna(bool leiauteAtual)
        {
            var modelo = leiauteAtual
                ? LerNFSe()
                : XMLUtility.Deserializar<NacionalNFSe.NFSe>(File.ReadAllText(
                    @"..\..\..\NFSe\Resources\NACIONAL\1.01\RetornoNACIONAL.xml"));
            modelo.InfNFSe.DPS.InfDPS.Serv.CServ = new CServ
            {
                XDescServ = "SERVICO TESTE",
                CNBS = "100000000",
                CTribMun = "123",
                CTribNac = "100000",
                CIntContrib = "1"
            };

            var xmlNFSe = modelo.GerarXML();
            var cServ = Selecionar(xmlNFSe, "/n:NFSe/n:infNFSe/n:DPS/n:infDPS/n:serv/n:cServ");
            var nomes = new List<string>();
            foreach (XmlNode elemento in cServ.ChildNodes)
            {
                nomes.Add(elemento.LocalName);
            }

            Assert.Equal(leiauteAtual
                ? "cTribNac,cTribMun,cNBS,cIntContrib,xDescServ"
                : "cTribNac,cTribMun,xDescServ,cNBS,cIntContrib", string.Join(",", nomes));

            var xmlDPS = modelo.InfNFSe.DPS.GerarXML();
            Assert.Equal(cServ.InnerXml, Selecionar(xmlDPS, "/n:DPS/n:infDPS/n:serv/n:cServ").InnerXml);
            Assert.Equal(xmlNFSe.OuterXml, modelo.GerarXML().OuterXml);

            if (leiauteAtual)
            {
                Validar(xmlNFSe, "NFSe_v1.01.xsd");
            }
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
