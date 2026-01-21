using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    public class ValidarXmlNfseNacionalTest
    {
        [Theory]
        [Trait("NFSe", "ValidarXmlNfseNacional")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNfseEnvio-env-loterps.xml", "NFSe.NACIONAL.DPS_v1.00.xsd")]
        [InlineData(@"..\..\..\NFSe\Resources\NACIONAL\1.00\GerarNfseEnvio_RTC-env-loterps.xml", "NFSe.NACIONAL.DPS_v1.00.xsd")]
        public void ValidarXmlNfseNacional(string arqXML, string arqXSD)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste de obter o tipo do XML.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var validar = new ValidarSchema();
            validar.Validar(doc, arqXSD, "http://www.sped.fazenda.gov.br/nfse", PadraoNFSe.NACIONAL);

            Assert.True(validar.Success, "Ocorreu um erro na validação de SCHEMA: \n" + validar.ErrorMessage);
        }

        /// <summary>
        /// Testar a validação do XML da NFSe com indicativo Decisão Judicial
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void GerarNfseIndicativoDecisaoJudicialXMLObjeto()
        {
            var emissao = DateTimeOffset.Now;
            var nfse = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe
            {
                Versao = "1.01",
                InfNFSe = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.InfNFSe
                {
                    Id = "NFS41055082299999999999999000000000000126013568215637",
                    XLocEmi = "Cianorte",
                    XLocPrestacao = "Cianorte",
                    NNFSe = 1,
                    CLocIncid = 4105508,
                    XLocIncid = "Cianorte",
                    XTribNac = "Agenciamento, corretagem ou intermediação de bens móveis ou imóveis, não abrangidos em outros itens ou subitens, por quaisquer meios.",
                    XNBS = "Serviços de concessão de crédito não classificados em subposições anteriores",
                    VerAplic = "EmissorWeb_1.5.0.0",
                    AmbGer = TipoAmbiente.Homologacao,
                    TpEmis = 1,
                    CStat = 102,
                    DhProc = emissao,
                    NDFSe = "1",
                    Emit = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.Emit
                    {
                        CNPJ = "99999999999999",
                        XNome = "XXXXXXXXXXXXXXXXXXXXXXXXXXXX LTDA",
                        EnderNac = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.EnderNac
                        {
                            XLgr = "XXXXXXXXXXX",
                            Nro = "1111",
                            XBairro = "ZONA 02",
                            CMun = 4105508,
                            UF = "PR",
                            CEP = "87200000"
                        }
                    },
                    Valores = new Business.DFe.Xml.NFSe.NACIONAL.NFSe.ValoresInfNFSe
                    {
                        VCalcDR = 0.00,
                        VBC = 0.00,
                        PAliqAplic = 5.00,
                        VISSQN = 0.00,
                        VTotalRet = 0.00,
                        VLiq = 723.90
                    },
                    DPS = new Business.DFe.Xml.NFSe.NACIONAL.DPS
                    {
                        Versao = "1.01",
                        InfDPS = new Business.DFe.Xml.NFSe.NACIONAL.InfDPS
                        {
                            Id = "DPS410550829999999999999900901000000000000001",
                            TpAmb = TipoAmbiente.Homologacao,
                            DhEmi = emissao,
                            VerAplic = "EmissorWeb_1.4.0.26",
                            Serie = "901",
                            NDPS = "1",
                            DCompet = emissao,
                            TpEmit = TipoEmitenteNFSe.Prestador,
                            CLocEmi = 4105508,
                            Prest = new Business.DFe.Xml.NFSe.NACIONAL.Prest
                            {
                                CNPJ = "99999999999999",
                                RegTrib = new Business.DFe.Xml.NFSe.NACIONAL.RegTrib
                                {
                                    OpSimpNac = OptSimplesNacional.NaoOptante,
                                    RegEspTrib = 0
                                }
                            },
                            Toma = new Business.DFe.Xml.NFSe.NACIONAL.Toma
                            {
                                CNPJ = "11111111111111",
                                XNome = "XXXXXXXXXXXXXXXXXXXXXXXX LTDA",
                                End = new Business.DFe.Xml.NFSe.NACIONAL.End
                                {
                                    EndNac = new Business.DFe.Xml.NFSe.NACIONAL.EndNac
                                    {
                                        CMun = 4115200,
                                        CEP = "87000000"
                                    },
                                    XLgr = "XXXXXXX",
                                    Nro = "1111",
                                    XBairro = "XXXXXXXXXXXXX"
                                }
                            },
                            Serv = new Business.DFe.Xml.NFSe.NACIONAL.Serv
                            {
                                LocPrest = new Business.DFe.Xml.NFSe.NACIONAL.LocPrest
                                {
                                    CLocPrestacao = 4105508
                                },
                                CServ = new Business.DFe.Xml.NFSe.NACIONAL.CServ
                                {
                                    CTribNac = "100501",
                                    XDescServ = "SERVIÇOS NÃO TRIBUTADO MH - RECARGA BENEFÍCIO",
                                    CNBS = "109013900"
                                }
                            },
                            Valores = new Business.DFe.Xml.NFSe.NACIONAL.Valores
                            {
                                VServPrest = new Business.DFe.Xml.NFSe.NACIONAL.VServPrest
                                {
                                    VServ = 723.90
                                },
                                Trib = new Business.DFe.Xml.NFSe.NACIONAL.Trib
                                {
                                    TribMun = new Business.DFe.Xml.NFSe.NACIONAL.TribMun
                                    {
                                        TribISSQN = TribISSQN.OperacaoTributavel,
                                        ExigSusp = new Business.DFe.Xml.NFSe.NACIONAL.ExigSusp
                                        {
                                            TpSusp = TipoExigibilidadeSuspensa.ProcessoAdministrativo,
                                            NProcesso = "000000000000000000000011111111"
                                        },
                                        TpRetISSQN = TipoRetencaoISSQN.NaoRetido
                                    },
                                    TribFed = new Business.DFe.Xml.NFSe.NACIONAL.TribFed
                                    {
                                        PISCOFINS = new Business.DFe.Xml.NFSe.NACIONAL.PISCOFINS
                                        {
                                            CST = "08"
                                        }
                                    },
                                    TotTrib = new Business.DFe.Xml.NFSe.NACIONAL.TotTrib
                                    {
                                        VTotTrib = new Business.DFe.Xml.NFSe.NACIONAL.VTotTrib
                                        {
                                            VTotTribFed = 0.00,
                                            VTotTribEst = 0.00,
                                            VTotTribMun = 30.76
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var xmlGeradadoPeloObjeto = nfse.GerarXML();

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.01"
            };

            var gerarNfseIndicativoDecisaoJudicial = new GerarNfseIndicativoDecisaoJudicial(xmlGeradadoPeloObjeto.OuterXml, configuracao);
            Assert.Multiple(() => TestUtility.AnalisaResultado(gerarNfseIndicativoDecisaoJudicial));
        }
    }
}
