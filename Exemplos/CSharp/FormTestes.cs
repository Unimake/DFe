using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography.X509Certificates;
using System.Windows.Forms;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Exceptions;
using Unimake.Security.Platform;
using Unimake.Unidanfe;
using Unimake.Unidanfe.Configurations;
using DANFe = Unimake.Unidanfe;
using DFe = Unimake.Business.DFe;
using ServicoCCG = Unimake.Business.DFe.Servicos.CCG;
using ServicoCTe = Unimake.Business.DFe.Servicos.CTe;
using ServicoCTeOS = Unimake.Business.DFe.Servicos.CTeOS;
using ServicoEFDReinf = Unimake.Business.DFe.Servicos.EFDReinf;
using ServicoESocial = Unimake.Business.DFe.Servicos.ESocial;
using ServicoGNRe = Unimake.Business.DFe.Servicos.GNRE;
using ServicoMDFe = Unimake.Business.DFe.Servicos.MDFe;
using ServicoNFCe = Unimake.Business.DFe.Servicos.NFCe;
using ServicoNFe = Unimake.Business.DFe.Servicos.NFe;
using ServicoNFSe = Unimake.Business.DFe.Servicos.NFSe;
using ServicoNFCom = Unimake.Business.DFe.Servicos.NFCom;
using ServicoNF3e = Unimake.Business.DFe.Servicos.NF3e;
using XmlCCG = Unimake.Business.DFe.Xml.CCG;
using XmlCTe = Unimake.Business.DFe.Xml.CTe;
using XmlCTeOS = Unimake.Business.DFe.Xml.CTeOS;
using XmlEFDReinf = Unimake.Business.DFe.Xml.EFDReinf;
using XmlESocial = Unimake.Business.DFe.Xml.ESocial;
using XmlGNRe = Unimake.Business.DFe.Xml.GNRE;
using XmlMDFe = Unimake.Business.DFe.Xml.MDFe;
using XmlNFe = Unimake.Business.DFe.Xml.NFe;
using XmlNFCom = Unimake.Business.DFe.Xml.NFCom;
using XmlNF3e = Unimake.Business.DFe.Xml.NF3e;
using Unimake.Business.DFe.Xml.NF3e;

namespace TreinamentoDLL
{
    public partial class FormTestes : Form
    {
        #region Private Fields

        /// <summary>
        /// Field para o Certificado Selecionado
        /// </summary>
        private static X509Certificate2 CertificadoSelecionadoField;

        #endregion Private Fields

        #region Private Properties

        /// <summary>
        /// Caminho do certificado digital A1
        /// </summary>
        private static string PathCertificadoDigital { get; set; } = @"C:\Projetos\Unimake_PV.pfx";

        /// <summary>
        /// Senha de uso do certificado digital A1
        /// </summary>
        private static string SenhaCertificadoDigital { get; set; } = "12345678";

        private static X509Certificate2 CertificadoA3Selecionado;

        #endregion Private Properties

        #region Private Methods

        private void BtnConsultaCadastroContribuinte_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsCad
            {
                Versao = "2.00",
                InfCons = new XmlNFe.InfCons()
                {
                    CNPJ = "06117473000150",
                    UF = UFBrasil.PR
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaCad = new ServicoNFe.ConsultaCadastro(xml, configuracao);
            consultaCad.Executar();

            MessageBox.Show(consultaCad.RetornoWSString);
            MessageBox.Show(consultaCad.Result.InfCons.XMotivo);

            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"d:\teste\retconscad.xml");

            var retConsCad = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFe.RetConsCad>(xmlDoc);
        }

        private void BtnConsultaSituacaoMDFe_Click(object sender, EventArgs e)
        {
            var xml = new Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe
            {
                Versao = "3.00",
                TpAmb = TipoAmbiente.Homologacao,
                ChMDFe = "41201280568835000181580010000010411406004656"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaProtocolo = new Unimake.Business.DFe.Servicos.MDFe.ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            MessageBox.Show(consultaProtocolo.Result.CStat + " - " + consultaProtocolo.Result.XMotivo);
        }

        private void BtnConsultaSituacaoNFCe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsSitNFe
            {
                Versao = "4.00",
                TpAmb = TipoAmbiente.Producao,
                ChNFe = "41211206117473000150550010000710231016752423"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaProtocolo = new ServicoNFCe.ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            MessageBox.Show(consultaProtocolo.Result.CStat + " - " + consultaProtocolo.Result.XMotivo);
        }

        private void BtnConsultaSituacaoNFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsSitNFe
            {
                Versao = "4.00",
                TpAmb = TipoAmbiente.Producao,
                ChNFe = "41211206117473000150550010000710231016752423"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaProtocolo = new ServicoNFe.ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            MessageBox.Show(consultaProtocolo.Result.CStat + " - " + consultaProtocolo.Result.XMotivo);
        }

        private void BtnConsultaStatusMDFe_Click(object sender, EventArgs e)
        {
            var xml = new Unimake.Business.DFe.Xml.MDFe.ConsStatServMDFe
            {
                Versao = "3.00",
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CodigoUF = (int)UFBrasil.PR,
                CertificadoDigital = CertificadoSelecionado
            };

            var statusServico = new Unimake.Business.DFe.Servicos.MDFe.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.Result.CStat + " - " + statusServico.Result.XMotivo);
        }

        private void BtnConsultaStatusNFCe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsStatServ
            {
                Versao = "4.00",
                CUF = UFBrasil.PR,
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var statusServico = new ServicoNFCe.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.Result.CStat + " " + statusServico.Result.XMotivo);
        }

        private void BtnConsultaStatusNFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsStatServ
            {
                Versao = "4.00",
                CUF = UFBrasil.PR,
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var statusServico = new ServicoNFe.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.Result.CStat + " " + statusServico.Result.XMotivo);
        }

        private void BtnEnviarCancNFSe_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\CancelarNfseEnvio-ped-cannfse.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeCancelarNfse,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var cancelarNFSe = new ServicoNFSe.CancelarNfse(xmlDoc, configuracao);
            cancelarNFSe.Executar();

            MessageBox.Show(cancelarNFSe.RetornoWSString);
        }

        private void BtnEnviarEventoCancelamento_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento>
                {
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoCanc
                        {
                            NProt = "141190000660363",
                            Versao = "1.00",
                            XJust = "Justificativa de teste de cancelamento"
                        })
                        {
                            COrgao = UFBrasil.PR,
                            ChNFe = "41190806117473000150550010000579131943463890",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.Cancelamento,
                            NSeqEvento = 1,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoNFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result.CStat == 128) //128 = Lotem de evento processado com sucesso.
            {
                switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
                {
                    case 135: //Evento homologado
                    case 155: //Evento homologado fora do prazo permitido
                        recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                        break;

                    default:
                        //Ações necessárias
                        break;
                }
            }
        }

        private void BtnEnviarEventoCCe_Click(object sender, EventArgs e)
        {
            #region Exemplo de como criar o objeto do XML desserializando um XML já existente

            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"C:\projetos\uninfe\exemplos\NFe e NFCe 4.00\Eventos\cce26111253420477000192550550000033071213028272_01-ped-eve.xml");

            var xmlDesserializado = new XmlNFe.EnvEvento();
            xmlDesserializado = xmlDesserializado.LerXML<XmlNFe.EnvEvento>(xmlDoc);

            #endregion

            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento>
                {
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoCCE
                        {
                            Versao = "1.00",
                            XCorrecao = "CFOP errada, segue CFOP correta. teste."
                        })
                        {
                            COrgao = UFBrasil.PR,
                            ChNFe = "41191006117473000150550010000579281779843610",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.CartaCorrecao,
                            NSeqEvento = 3,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    },
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoCCE
                        {
                            Versao = "1.00",
                            XCorrecao = "Nome do transportador está errado, segue nome correto."
                        })
                        {
                            COrgao = UFBrasil.PR,
                            ChNFe = "41191006117473000150550010000579281779843610",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.CartaCorrecao,
                            NSeqEvento = 4,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado

            };

            var recepcaoEvento = new ServicoNFe.RecepcaoEvento(xml, configuracao);
            var XmlDaCCeAssinadoNoFormatoString = recepcaoEvento.ConteudoXMLAssinado.OuterXml; //Como pegar o XML da CCe no formato estring
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result.CStat == 128) //128 = Lote de evento processado com sucesso.
            {
                for (var i = 0; i < recepcaoEvento.Result.RetEvento.Count; i++)
                {
                    switch (recepcaoEvento.Result.RetEvento[i].InfEvento.CStat)
                    {
                        case 135: //Evento homologado com vinculação da respectiva NFe
                            var XmlDistribuicaoCCeFormatoString = recepcaoEvento.ProcEventoNFeResult[i].GerarXML().OuterXml; //Strig do XML de distribuição da CCe para gravar em banco de dados
                            recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe"); //Grava o XML de distribuição
                            break;

                        default:
                            //Realizar ações necessárias
                            break;
                    }

                }
            }
        }

        private void BtnEnviarMDFeSincrono_Click(object sender, EventArgs e)
        {
            #region Criar MDFe

            var xml = new XmlMDFe.MDFe
            {
                InfMDFe = new XmlMDFe.InfMDFe
                {
                    Versao = "3.00",
                    Ide = new XmlMDFe.Ide
                    {
                        CUF = UFBrasil.PR,
                        TpAmb = TipoAmbiente.Homologacao,
                        TpEmit = TipoEmitenteMDFe.PrestadorServicoTransporte,
                        Mod = ModeloDFe.MDFe,
                        Serie = 1,
                        NMDF = 861,
                        CMDF = "01722067",
                        Modal = ModalidadeTransporteMDFe.Rodoviario,
                        DhEmi = DateTime.Now,
                        TpEmis = TipoEmissao.Normal,
                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                        VerProc = "UNICO V8.0",
                        UFIni = UFBrasil.PR,
                        UFFim = UFBrasil.SP,
                        InfMunCarrega = new List<XmlMDFe.InfMunCarrega>
                        {
                            new XmlMDFe.InfMunCarrega
                            {
                                CMunCarrega = 4118402,
                                XMunCarrega = "PARANAVAI"
                            }
                        },
                        DhIniViagem = DateTime.Now,
                    },
                    Emit = new XmlMDFe.Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "9032000301",
                        XNome = "TESTE DE ENVIO DE MDFE",
                        XFant = "TESTE DE ENVIO DE MDFE",
                        EnderEmit = new XmlMDFe.EnderEmit
                        {
                            XLgr = "RUA TESTE DE SOUZA",
                            Nro = "01112",
                            XBairro = "JD. SIMARA",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87706111",
                            UF = UFBrasil.PR,
                            Fone = "04431421010",
                        },
                    },
                    InfModal = new XmlMDFe.InfModal
                    {
                        VersaoModal = "3.00",
                        Rodo = new XmlMDFe.Rodo
                        {
                            InfANTT = new XmlMDFe.InfANTT
                            {
                                RNTRC = "99778899",
                                InfContratante = new List<XmlMDFe.InfContratante>
                                {
                                    new XmlMDFe.InfContratante
                                    {
                                        CNPJ = "06117473000150"
                                    },
                                    new XmlMDFe.InfContratante
                                    {
                                        CNPJ = "06117473000150"
                                    }
                                }
                            },
                            VeicTracao = new XmlMDFe.VeicTracao
                            {
                                CInt = "ARR5555",
                                Placa = "ARR5555",
                                Tara = 0,
                                CapKG = 5000,
                                Prop = new XmlMDFe.Prop
                                {
                                    CNPJ = "06117473000150",
                                    RNTRC = "12345678",
                                    XNome = "TESTE TESTE TESTE X",
                                    IE = "1234567890",
                                    UF = UFBrasil.PR,
                                    TpProp = TipoProprietarioMDFe.Outros
                                },
                                Condutor = new List<XmlMDFe.Condutor>
                                {
                                    new XmlMDFe.Condutor
                                    {
                                        XNome = "TESTE NOME DO CONDUTOR",
                                        CPF = "00000000000"
                                    }
                                },
                                TpRod = TipoRodado.Toco,
                                TpCar = TipoCarroceriaMDFe.FechadaBau,
                                UF = UFBrasil.PR
                            },
                        }
                    },
                    InfDoc = new XmlMDFe.InfDocInfMDFe
                    {
                        InfMunDescarga = new List<XmlMDFe.InfMunDescarga>
                        {
                            new XmlMDFe.InfMunDescarga
                            {
                                CMunDescarga = 3505708,
                                XMunDescarga = "BARUERI",
                                InfCTe = new List<XmlMDFe.InfMunDescargaInfCTe>
                                {
                                    new XmlMDFe.InfMunDescargaInfCTe
                                    {
                                        ChCTe = "41200506117473000150570010000001113565666658"
                                    }
                                },
                                InfNFe = new List<XmlMDFe.InfMunDescargaInfNFe>
                                {
                                    new XmlMDFe.InfMunDescargaInfNFe
                                    {
                                        ChNFe = "12345678901234567890123456789012345678901234",
                                        InfUnidTransp = new List<XmlMDFe.InfUnidTransp>
                                        {
                                            new XmlMDFe.InfUnidTransp
                                            {
                                                IdUnidTransp = "122",
                                                TpUnidTransp = TipoUnidadeTransporte.RodoviarioReboque,
                                                LacUnidTransp = new List<XmlMDFe.LacUnidTransp>
                                                {
                                                    new XmlMDFe.LacUnidTransp
                                                    {
                                                        NLacre = "12334"
                                                    }
                                                },
                                                InfUnidCarga = new List<XmlMDFe.InfUnidCarga>
                                                {
                                                    new XmlMDFe.InfUnidCarga
                                                    {
                                                        TpUnidCarga = TipoUnidadeCarga.Container,
                                                        IdUnidCarga = "123",
                                                        LacUnidCarga = new List<XmlMDFe.LacUnidCarga>
                                                        {
                                                            new XmlMDFe.LacUnidCarga
                                                            {
                                                                NLacre = "3333333"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            },
                            new XmlMDFe.InfMunDescarga
                            {
                                CMunDescarga = 3550308,
                                XMunDescarga = "SAO PAULO",
                                InfCTe = new List<XmlMDFe.InfMunDescargaInfCTe>
                                {
                                    new XmlMDFe.InfMunDescargaInfCTe
                                    {
                                        ChCTe = "41200506117473000150570010000003335655655666"
                                    }
                                }
                            }
                        }
                    },
                    ProdPred = new XmlMDFe.ProdPred
                    {
                        TpCarga = TipoCargaMDFe.CargaGeral,
                        XProd = "TESTE DE PRODUTO PREDOMINANTE",
                        InfLotacao = new XmlMDFe.InfLotacao
                        {
                            InfLocalCarrega = new XmlMDFe.InfLocalCarrega
                            {
                                CEP = "87302080"
                            },
                            InfLocalDescarrega = new XmlMDFe.InfLocalDescarrega
                            {
                                CEP = "25650208"
                            }
                        }
                    },
                    Seg = new List<XmlMDFe.Seg>
                    {
                        new XmlMDFe.Seg
                        {
                            InfResp = new XmlMDFe.InfResp
                            {
                                RespSeg = ResponsavelSeguroMDFe.EmitenteMDFe,
                                CNPJ = "06117473000150"
                            },
                            InfSeg = new XmlMDFe.InfSeg
                            {
                                XSeg = "PORTO SEGURO",
                                CNPJ = "06117473000150"
                            },
                            NApol = "053179456362",
                            NAver = new List<string>
                            {
                                {
                                    "0000000000000000000000000000000000000000"
                                },
                                {
                                    "0000000000000000000000000000000000000001"
                                },
                            }
                        }
                    },
                    Tot = new XmlMDFe.Tot
                    {
                        QCTe = 3,
                        VCarga = 56599.09,
                        CUnid = CodigoUnidadeMedidaMDFe.KG,
                        QCarga = 2879.00
                    },
                    Lacres = new List<XmlMDFe.Lacre>
                    {
                        new XmlMDFe.Lacre
                        {
                            NLacre = "1111111"
                        },
                        new XmlMDFe.Lacre
                        {
                            NLacre = "22222"
                        }
                    },
                    InfAdic = new XmlMDFe.InfAdic
                    {
                        InfCpl = "DATA/HORA PREVISTA PARA O INICO DA VIAGEM: 10/08/2020 as 08:00"
                    },
                    InfRespTec = new XmlMDFe.InfRespTec
                    {
                        CNPJ = "06117473000150",
                        XContato = "TESTE TESTE TESTE",
                        Email = "wandrey@unimake.com.br",
                        Fone = "04431421010",
                    },
                },
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacaoSinc = new ServicoMDFe.AutorizacaoSinc(xml, configuracao);
            autorizacaoSinc.Executar();

            if (autorizacaoSinc.Result.CStat == 100)
            {
                if (autorizacaoSinc.Result.ProtMDFe.InfProt.CStat == 100)
                {
                    MessageBox.Show(autorizacaoSinc.Result.ProtMDFe.InfProt.NProt);
                    var teste = autorizacaoSinc.MDFeProcResults[xml.InfMDFe.Chave].GerarXML();
                }
            }

            #endregion Criar MDFe
        }

        private void BtnEnviarNFCeGeradaContingenciaOFFLine_Click(object sender, EventArgs e)
        {
            var arquivos = Directory.GetFiles(@"d:\testenfe\NFCeOffline");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado,
                CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG",
                CSCIDToken = 2
            };

            foreach (var item in arquivos)
            {
                var doc = new XmlDocument();
                doc.Load(item);

                var xml = new XmlNFe.EnviNFe
                {
                    IdLote = "000000000000001",
                    Versao = "4.00",
                    IndSinc = SimNao.Sim,
                    NFe = new List<XmlNFe.NFe>
                    {
                        XMLUtility.Deserializar<XmlNFe.NFe>(doc)
                    }
                };

                ServicoNFCe.Autorizacao autorizacao = null;
                try
                {
                    autorizacao = new ServicoNFCe.Autorizacao(xml, configuracao);
                    autorizacao.Executar();
                }
                catch (ValidarXMLException)
                {
                    //Retorno o erro para o usuário.
                }
                catch (Exception)
                {
                    //Entro em contingencia
                }

                if (autorizacao != null)
                {
                    //Fazer os tratamentos dos status, se autorizado ou rejeitado.
                    MessageBox.Show(autorizacao.Result.ProtNFe.InfProt.CStat.ToString() + " " +
                        autorizacao.Result.ProtNFe.InfProt.XMotivo);

                    if (autorizacao.Result.CStat == 108) //Serviço Paralisado Temporariamente
                    {
                        //Gerar novamente o XML já com as tags de contingência
                    }
                    if (autorizacao.Result.CStat == 109) //Serviço Paralisado Sem previsão de retorno
                    {
                        //Gerar novamente o XML já com as tags de contingência
                    }
                }
            }
        }

        private void BtnEnviarNFCeSincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFCe,
                                    Serie = 1,
                                    NNF = 57980,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterna,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NFCe,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },

                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Det = new List<XmlNFe.Det> {
                                    new XmlNFe.Det
                                    {
                                        NItem = 1,
                                        Prod = new XmlNFe.Prod
                                        {
                                            CProd = "01042",
                                            CEAN = "SEM GTIN",
                                            XProd = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                            NCM = "84714900",
                                            CFOP = "5101",
                                            UCom = "LU",
                                            QCom = 1.00m,
                                            VUnCom = 84.9000000000M,
                                            VProd = 84.90,
                                            CEANTrib = "SEM GTIN",
                                            UTrib = "LU",
                                            QTrib = 1.00m,
                                            VUnTrib = 84.9000000000M,
                                            IndTot = SimNao.Sim,
                                            XPed = "300474",
                                            NItemPed = "1"
                                        },
                                        Imposto = new XmlNFe.Imposto
                                        {
                                            VTotTrib = 12.63,
                                            ICMS = new XmlNFe.ICMS
                                            {
                                                ICMSSN102 = new XmlNFe.ICMSSN102
                                                {
                                                    Orig = OrigemMercadoria.Nacional,
                                                    CSOSN = "102"
                                                }
                                            },
                                            PIS = new XmlNFe.PIS
                                            {
                                                PISOutr = new XmlNFe.PISOutr
                                                {
                                                    CST = "99",
                                                    VBC = 0.00,
                                                    PPIS = 0.00,
                                                    VPIS = 0.00
                                                }
                                            },
                                            COFINS = new XmlNFe.COFINS
                                            {
                                                COFINSOutr = new XmlNFe.COFINSOutr
                                                {
                                                    CST = "99",
                                                    VBC = 0.00,
                                                    PCOFINS = 0.00,
                                                    VCOFINS = 0.00
                                                }
                                            }
                                        }
                                    }
                                },

                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.SemOcorrenciaTransporte
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                            new XmlNFe.DetPag
                                            {
                                                IndPag = IndicadorPagamento.PagamentoVista,
                                                TPag = MeioPagamento.PagamentoInstantaneo,
                                                VPag = 84.90,
                                                Card = new Card()
                                                {
                                                    TpIntegra = TipoIntegracaoPagamento.PagamentoNaoIntegrado
                                                }
                                            }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado,
                CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG",
                CSCIDToken = 2
            };

            var autorizacao = new ServicoNFCe.Autorizacao(xml, configuracao);
            autorizacao.Executar();

            if (autorizacao.Result.ProtNFe != null)
            {
                switch (autorizacao.Result.ProtNFe.InfProt.CStat)
                {
                    case 100: //Autorizado o uso da NFe
                    case 110: //Uso Denegado
                    case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                    case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                    case 301: //Uso Denegado: Irregularidade fiscal do emitente
                    case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                    case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                        autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");
                        //var docProcNFe = autorizacao.NfeProcResult.GerarXML(); //Gerar o Objeto para pegar a string e gravar em banco de dados

                        //Como é assíncrono, tenho que prever a possibilidade de ter mais de uma NFe no lote, então teremos vários XMLs com protocolos.
                        //Se no seu caso vc enviar sempre uma única nota, só vai passar uma única vez no foreach
                        foreach (var item in autorizacao.NfeProcResults.Values)
                        {
                            var docProcNFe = item.GerarXML();
                            var stringXml = docProcNFe.OuterXml;
                        }

                        MessageBox.Show(autorizacao.NfeProcResult.NomeArquivoDistribuicao);
                        break;

                    default:
                        //NF Rejeitada
                        break;
                }
            }
        }

        private void BtnEnviarNFeAssincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Nao,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57988,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 169.80,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 169.80,
                                        VTotTrib = 25.26
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 169.80,
                                        VDesc = 0,
                                        VLiq = 169.80
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 169.80
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 169.80
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
            autorizacao.Executar();

            var configSit = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            if (autorizacao.Result != null)
            {
                if (autorizacao.Result.CStat == 103) //103 = Lote Recebido com Sucesso
                {
                    #region Finalizar através da consulta do recibo.

                    var xmlRec = new XmlNFe.ConsReciNFe
                    {
                        Versao = "4.00",
                        TpAmb = TipoAmbiente.Homologacao,
                        NRec = autorizacao.Result.InfRec.NRec
                    };

                    var configRec = new Configuracao
                    {
                        TipoDFe = TipoDFe.NFe,
                        CertificadoDigital = CertificadoSelecionado
                    };

                    var retAutorizacao = new ServicoNFe.RetAutorizacao(xmlRec, configRec);
                    retAutorizacao.Executar();

                    if (retAutorizacao.Result.ProtNFe[0].InfProt.CStat == 100) //Nota autorizada
                    {
                        autorizacao.RetConsReciNFe = retAutorizacao.Result;

                        //Gravar os XMLs de distribuição em uma pasta
                        autorizacao.GravarXmlDistribuicao(@"c:\testenfe");

                        //Pegar as strings dos XMLs de distribuição para gravar em base de dados
                        var xmlDistribuicaoNFe = autorizacao.NfeProcResults[xml.NFe[0].InfNFe[0].Chave].GerarXML();
                    }
                    else //Rejeitada
                    {
                        MessageBox.Show(retAutorizacao.Result.ProtNFe[0].InfProt.CStat + " " + retAutorizacao.Result.ProtNFe[0].InfProt.XMotivo);
                    }

                    #endregion Finalizar através da consulta do recibo.
                }
            }

            #region Finalizando a nota fiscal a partir da consulta situação da NFe em caso de não ter obtido o recibo da NFe enviada

            foreach (var item in xml.NFe)
            {
                var xmlSit = new XmlNFe.ConsSitNFe
                {
                    Versao = "4.00",
                    TpAmb = TipoAmbiente.Homologacao,
                    ChNFe = item.InfNFe[0].Chave
                };

                var consultaProtocolo = new ServicoNFe.ConsultaProtocolo(xmlSit, configSit);
                consultaProtocolo.Executar();

                autorizacao.RetConsSitNFes.Add(consultaProtocolo.Result);
            }

            autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");

            #endregion Finalizando a nota fiscal a partir da consulta situação da NFe em caso de não ter obtido o recibo da NFe enviada
        }

        private void BtnEnviarNFeAssincronoLote_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Nao,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57980,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Entrega = new XmlNFe.Entrega
                                {
                                  XNome = "",
                                  XLgr = "",
                                  Nro = "",
                                  XCpl = "",
                                  XBairro = "",
                                  CEP = ""  ,
                                  CMun = 1111111,
                                  XMun = "",
                                  UF = UFBrasil.PR,
                                  Email = "teste@gmail.com",
                                  Fone = "",
                                  CNPJ = "00000000000000",
                                  IE = ""
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 169.80,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 169.80,
                                        VTotTrib = 25.26
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 169.80,
                                        VDesc = 0,
                                        VLiq = 169.80
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 169.80
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 169.80
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    },
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57980,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 169.80,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 169.80,
                                        VTotTrib = 25.26
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 169.80,
                                        VDesc = 0,
                                        VLiq = 169.80
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 169.80
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 169.80
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);

            var nfeXML = autorizacao.ConteudoXMLAssinado.OuterXml;

            autorizacao.Executar();

            var configSit = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            if (autorizacao.Result != null)
            {
                if (autorizacao.Result.CStat == 103) //103 = Lote Recebido com Sucesso
                {
                    #region Finalizar através da consulta do recibo.

                    var xmlRec = new XmlNFe.ConsReciNFe
                    {
                        Versao = "4.00",
                        TpAmb = TipoAmbiente.Homologacao,
                        NRec = autorizacao.Result.InfRec.NRec
                    };

                    var configRec = new Configuracao
                    {
                        TipoDFe = TipoDFe.NFe,
                        CertificadoDigital = CertificadoSelecionado
                    };

                    var retAutorizacao = new ServicoNFe.RetAutorizacao(xmlRec, configRec);
                    retAutorizacao.Executar();

                    autorizacao.RetConsReciNFe = retAutorizacao.Result;

                    autorizacao.GravarXmlDistribuicao(@"c:\testenfe");

                    #endregion Finalizar através da consulta do recibo.
                }
            }

            #region Finalizando a nota fiscal a partir da consulta situação da NFe em caso de não ter obtido o recibo da NFe enviada

            foreach (var item in xml.NFe)
            {
                var xmlSit = new XmlNFe.ConsSitNFe
                {
                    Versao = "4.00",
                    TpAmb = TipoAmbiente.Homologacao,
                    ChNFe = item.InfNFe[0].Chave
                };

                var consultaProtocolo = new ServicoNFe.ConsultaProtocolo(xmlSit, configSit);
                consultaProtocolo.Executar();

                autorizacao.RetConsSitNFes.Add(consultaProtocolo.Result);
            }

            autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");

            #endregion Finalizando a nota fiscal a partir da consulta situação da NFe em caso de não ter obtido o recibo da NFe enviada
        }

        private void BtnEnviarNFeSerializacao_Click(object sender, EventArgs e)
        {
            #region Deserializar XML da NFe sem o lote

            var doc = new XmlDocument();
            doc.Load(@"..\..\Recursos\NFe.xml");

            var xml = new XmlNFe.EnviNFe
            {
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                Versao = "4.00",
                NFe = new List<XmlNFe.NFe>
            {
                XMLUtility.Deserializar<XmlNFe.NFe>(doc)
            }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
            autorizacao.Executar();

            #endregion Deserializar XML da NFe sem o lote

            #region Deserializar XML da NFe com o lote

            var doc2 = new XmlDocument();
            doc2.Load(@"..\..\Recursos\EnviNFe.xml");

            var xml2 = XMLUtility.Deserializar<XmlNFe.EnviNFe>(doc2);

            var configuracao2 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao2 = new ServicoNFe.Autorizacao(xml2, configuracao2);
            autorizacao2.Executar();

            #endregion Deserializar XML da NFe com o lote
        }

        private void BtnEnviarNFeSincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 59,
                                    NNF = 1,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 80.90
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
            var xmlNFeAssinadoNoFormatoString = autorizacao.ConteudoXMLAssinado.OuterXml;

            //Gravo no meu banco de dados o xmlString
            autorizacao.Executar();


            //Gravar o arquivo do conteúdo retornado em uma pasta qualquer para ter em segurança. Pode-se também gravar na base de dados. Fica a critério de cada um.
            File.WriteAllText(@"d:\testenfe\retorno\nomearquivoretorno.xml", autorizacao.RetornoWSString);

            if (autorizacao.Result.ProtNFe != null)
            {
                switch (autorizacao.Result.ProtNFe.InfProt.CStat)
                {
                    case 100: //Autorizado o uso da NFe
                    case 110: //Uso Denegado
                    case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                    case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                    case 301: //Uso Denegado: Irregularidade fiscal do emitente
                    case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                    case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                        autorizacao.GravarXmlDistribuicao(@"D:\testenfe\");
                        var docProcNFe = autorizacao.NfeProcResult.GerarXML().OuterXml; //Gerar o Objeto para pegar a string e gravar em banco de dados
                        MessageBox.Show(autorizacao.NfeProcResult.NomeArquivoDistribuicao);
                        break;

                    default:
                        //NF Rejeitada
                        break;
                }
            }
        }

        private void BtnExecutarTelaConfigDANFe_Click(object sender, EventArgs e) => DANFe.UnidanfeServices.ShowConfigurationScreen();

        private void BtnGerarNFCeContingenciaOFFLine_Click(object sender, EventArgs e)
        {
            #region Motar o XML

            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFCe,
                                    Serie = 1,
                                    NNF = 57982,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterna,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NFCe,
                                    TpEmis = TipoEmissao.ContingenciaOffLine,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00",
                                    DhCont = DateTime.Now,
                                    XJust = "Emitido em contingência devido a problemas técnicos."
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Det = new List<XmlNFe.Det> {
                                    new XmlNFe.Det
                                    {
                                        NItem = 1,
                                        Prod = new XmlNFe.Prod
                                        {
                                            CProd = "01042",
                                            CEAN = "SEM GTIN",
                                            XProd = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                            NCM = "84714900",
                                            CFOP = "5101",
                                            UCom = "LU",
                                            QCom = 1.00m,
                                            VUnCom = 84.9000000000M,
                                            VProd = 84.90,
                                            CEANTrib = "SEM GTIN",
                                            UTrib = "LU",
                                            QTrib = 1.00m,
                                            VUnTrib = 84.9000000000M,
                                            IndTot = SimNao.Sim,
                                            XPed = "300474",
                                            NItemPed = "1"
                                        },
                                        Imposto = new XmlNFe.Imposto
                                        {
                                            VTotTrib = 12.63,
                                            ICMS = new XmlNFe.ICMS
                                            {
                                                ICMSSN102 = new XmlNFe.ICMSSN102
                                                {
                                                    Orig = OrigemMercadoria.Nacional,
                                                    CSOSN = "102"
                                                }
                                            },
                                            PIS = new XmlNFe.PIS
                                            {
                                                PISOutr = new XmlNFe.PISOutr
                                                {
                                                    CST = "99",
                                                    VBC = 0.00,
                                                    PPIS = 0.00,
                                                    VPIS = 0.00
                                                }
                                            },
                                            COFINS = new XmlNFe.COFINS
                                            {
                                                COFINSOutr = new XmlNFe.COFINSOutr
                                                {
                                                    CST = "99",
                                                    VBC = 0.00,
                                                    PCOFINS = 0.00,
                                                    VCOFINS = 0.00
                                                }
                                            }
                                        }
                                    }
                                },
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.SemOcorrenciaTransporte
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                            new XmlNFe.DetPag
                                            {
                                                IndPag = IndicadorPagamento.PagamentoVista,
                                                TPag = MeioPagamento.Dinheiro,
                                                VPag = 84.90,
                                            }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            #endregion Motar o XML

            #region Montar configuração básica

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado,
                CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG",
                CSCIDToken = 2
            };

            #endregion Montar configuração básica

            #region instanciar a classe do serviço de autorização da NFCe para pegar o conteúdo do XML assinado e 100% preparado para transmissão

            var autorizacao = new ServicoNFCe.Autorizacao(xml, configuracao);

            #endregion instanciar a classe do serviço de autorização da NFCe para pegar o conteúdo do XML assinado e 100% preparado para transmissão

            #region Salvar o XML gerado (Pode-se guardar o conteúdo em string do XML em banco de dados também para resgatar na hora de transmitir.

            StreamWriter streamWriter = null;

            var arqXMLNFCe = Path.Combine(@"D:\testenfe\NFCeOffline", autorizacao.EnviNFe.NFe[0].InfNFe[0].Chave + "-nfe.xml");

            try
            {
                streamWriter = File.CreateText(arqXMLNFCe);
                streamWriter.Write(autorizacao.ConteudoXMLAssinado.GetElementsByTagName("NFe")[0].OuterXml);
            }
            finally
            {
                if (streamWriter != null)
                {
                    streamWriter.Close();
                }
            }

            #endregion

            #region Salvar o XML gerado (Pode-se guardar o conteúdo em string do XML em banco de dados também para resgatar na hora de transmitir.

            var config = new DANFe.Configurations.UnidanfeConfiguration
            {
                Arquivo = arqXMLNFCe,
                Copias = 1,
                Visualizar = true,
                Imprimir = false,
                PastaConfiguracao = @"d:\testenfe\unidanfe"
            };

            DANFe.UnidanfeServices.Execute(config);

            #endregion
        }

        private void BtnImprimirDANFe_Click(object sender, EventArgs e)
        {
            var config = new DANFe.Configurations.UnidanfeConfiguration
            {
                Arquivo = @"C:\Users\Wandrey\Downloads\Telegram Desktop\35220639397657000170550010000001881942202322-procnfe.xml",
                Visualizar = true,
                Imprimir = false,
                EnviaEmail = false,
                Configuracao = "PAISAGEM"
            };

            DANFe.UnidanfeServices.Execute(config);
        }

        private void BtnInutilizacaoNFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.InutNFe
            {
                Versao = "4.00",
                InfInut = new XmlNFe.InutNFeInfInut
                {
                    Ano = "21",
                    CNPJ = "06117473000150",
                    CUF = UFBrasil.PR,
                    Mod = ModeloDFe.NFe,
                    NNFIni = 57919,
                    NNFFin = 57920,
                    Serie = 1,
                    TpAmb = TipoAmbiente.Homologacao,
                    XJust = "Justificativa de inutilizacao para teste"
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var inutilizacao = new ServicoNFe.Inutilizacao(xml, configuracao);
            inutilizacao.Executar();

            switch (inutilizacao.Result.InfInut.CStat)
            {
                case 102: //102 = Inutilização homologada
                    inutilizacao.GravarXmlDistribuicao(@"c:\testenfe");
                    break;

                default:
                    //Tratamentos necessários
                    break;
            }
        }

        private List<XmlNFe.Det> CriarDet()
        {
            var dets = new List<XmlNFe.Det>();

            for (var i = 0; i < 1; i++)
            {
                dets.Add(new XmlNFe.Det
                {
                    NItem = i + 1,
                    Prod = new XmlNFe.Prod
                    {
                        CProd = "01042",
                        CEAN = "SEM GTIN",
                        XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        NCM = "84714900", //84714900
                        CFOP = "6101",
                        UCom = "LU",
                        QCom = 1.00m,
                        VUnCom = 84.9000000000M,
                        VProd = 84.90,
                        CEANTrib = "SEM GTIN",
                        UTrib = "LU",
                        QTrib = 1.00m,
                        VUnTrib = 84.9000000000M,
                        IndTot = SimNao.Sim,
                        XPed = "300474",
                        NItemPed = "1",
                        Rastro = new List<Rastro>
                        {
                            new Rastro
                            {
                                CAgreg = "12345678901234",
                                DFab = DateTime.Now,
                                DVal = DateTime.Now,
                                NLote = "",
                                QLote = 0.00
                            },
                            new Rastro
                            {
                                CAgreg = "12345678901234",
                                DFab = DateTime.Now,
                                DVal = DateTime.Now,
                                NLote = "",
                                QLote = 0.00
                            },
                        },
                        Med = new Med
                        {
                            CProdANVISA = "",
                            VPMC = 0.00,
                            XMotivoIsencao = ""
                        },
                        //DI = new List<DI>
                        //{
                        //    new DI
                        //    {
                        //        Adi = new List<Adi>
                        //        {
                        //            new Adi
                        //            {
                        //                CFabricante = "",
                        //                NDraw = ""
                        //            },
                        //            new Adi
                        //            {
                        //                CFabricante = "",
                        //                NDraw = ""
                        //            }
                        //        },
                        //        CExportador = "",
                        //        CNPJ = "",
                        //        DDesemb = DateTime.Now,
                        //        DDI = DateTime.Now,
                        //        TpViaTransp = ViaTransporteInternacional.Rodoviaria,
                        //        TpIntermedio = FormaImportacaoIntermediacao.ImportacaoPorContaOrdem
                        //    },
                        //    new DI
                        //    {
                        //        Adi = new List<Adi>
                        //        {
                        //            new Adi
                        //            {
                        //                CFabricante = "",
                        //                NDraw = ""
                        //            },
                        //            new Adi
                        //            {
                        //                CFabricante = "",
                        //                NDraw = ""
                        //            }
                        //        },
                        //        CExportador = "",
                        //        CNPJ = "00000000000000",
                        //        NDI = "111",
                        //        DDesemb = DateTime.Now,
                        //        DDI = DateTime.Now,
                        //        TpViaTransp = ViaTransporteInternacional.Rodoviaria,
                        //        TpIntermedio = FormaImportacaoIntermediacao.ImportacaoPorContaOrdem
                        //    },
                        //}

                    },
                    Imposto = new XmlNFe.Imposto
                    {
                        VTotTrib = 12.63,
                        ICMS = new XmlNFe.ICMS
                        {
                            ICMSSN101 = new XmlNFe.ICMSSN101
                            {
                                Orig = OrigemMercadoria.Nacional,
                                PCredSN = 2.8255,
                                VCredICMSSN = 2.40
                            },
                            //ICMS02 = new XmlNFe.ICMS02
                            //{
                            //    Orig = OrigemMercadoria.Nacional,
                            //    CST = "02",
                            //    QBCMono = 500,
                            //    AdRemICMS = 1.1234,
                            //    VICMSMono = 9.999
                            //}
                        },
                        PIS = new XmlNFe.PIS
                        {
                            PISOutr = new XmlNFe.PISOutr
                            {
                                CST = "99",
                                VBC = 0.00,
                                PPIS = 0.00,
                                VPIS = 0.00
                            }
                        },
                        COFINS = new XmlNFe.COFINS
                        {
                            COFINSOutr = new XmlNFe.COFINSOutr
                            {
                                CST = "99",
                                VBC = 0.00,
                                PCOFINS = 0.00,
                                VCOFINS = 0.00
                            }
                        }
                    },
                    ImpostoDevol = new ImpostoDevol
                    {
                        PDevol = 100.00,
                        IPI = new IPIDevol
                        {
                            VIPIDevol = 8.27
                        }
                    }

                });
            }

            return dets;
        }

        private void Form1_Load(object sender, EventArgs e)
        {
        }

        #endregion Private Methods

        #region Public Properties

        /// <summary>
        /// Certificado digital
        /// </summary>
        public static X509Certificate2 CertificadoSelecionado
        {
            get
            {
                if (CertificadoSelecionadoField == null)
                {
                    CertificadoSelecionadoField = new CertificadoDigital().CarregarCertificadoDigitalA1(PathCertificadoDigital, SenhaCertificadoDigital);
                }

                return CertificadoSelecionadoField;
            }

            private set => throw new Exception("Não é possível atribuir um certificado digital! Somente resgate o valor da propriedade que o certificado é definido automaticamente.");
        }

        #endregion Public Properties

        #region Public Constructors

        public FormTestes() => InitializeComponent();


        #endregion Public Constructors

        private void BtnEnviarEventoCancSubstituicao_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento>
                {
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoCancSubst
                        {
                            Versao = "1.00",
                            COrgaoAutor = UFBrasil.PR,
                            TpAutor = TipoAutor.EmpresaEmitente,
                            VerAplic = "Unico ERP 9",
                            NProt = "141190000660363",
                            XJust = "Justificativa de teste de cancelamento",
                            ChNFeRef = "00000000000000000000000000000000000000000000"
                        })
                        {
                            COrgao = UFBrasil.PR,
                            ChNFe = "41190806117473000150550010000579131943463890",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.CancelamentoPorSubstituicao,
                            NSeqEvento = 1,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoNFCe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result.CStat == 128) //Lote de evento foi processado com sucesso
            {
                switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
                {
                    case 135: //Evento homologado
                    case 155: //Evento homologado fora do prazo permitido
                        recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                        break;

                    default:
                        //Devidos tratamentos em caso de rejeição
                        break;
                }
            }
        }

        private void BtnEnvioLoteRPSAssincrono_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\EnviarLoteRpsEnvio-env-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeRecepcionarLoteRps,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var recepcionarLoteRps = new ServicoNFSe.RecepcionarLoteRps(xmlDoc, configuracao);
            recepcionarLoteRps.Executar();

            MessageBox.Show(recepcionarLoteRps.RetornoWSString);
        }

        private void BtnEnvioLoteRPSSincrono_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\EnviarLoteRpsSincronoEnvio-env-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeRecepcionarLoteRpsSincrono,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var recepcionarLoteRpsSincrono = new ServicoNFSe.RecepcionarLoteRpsSincrono(xmlDoc, configuracao);
            recepcionarLoteRpsSincrono.Executar();

            MessageBox.Show(recepcionarLoteRpsSincrono.RetornoWSString);
        }

        private void BtnEnvioRPSSincrono_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\GerarNfseEnvio-env-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var gerarNfse = new ServicoNFSe.GerarNfse(xmlDoc, configuracao);
            gerarNfse.Executar();

            MessageBox.Show(gerarNfse.RetornoWSString);
        }


        private void BtnResgatarNFCeSemTagLote_Click(object sender, EventArgs e)
        {

        }

        private void BtnResgatarMDFeSemTagLote_Click(object sender, EventArgs e)
        {

        }

        private void BtnConsultarLoteRPS_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\ConsultarLoteRpsEnvio-ped-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeConsultarLoteRps,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var consultarLoteRps = new ServicoNFSe.ConsultarLoteRps(xmlDoc, configuracao);
            consultarLoteRps.Executar();

            MessageBox.Show(consultarLoteRps.RetornoWSString);
        }

        private void BtnConsultarNFSePorRPS_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\ConsultarNfseRpsEnvio-ped-sitnfserps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeConsultarNfsePorRps,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var consultarNfsePorRps = new ServicoNFSe.ConsultarNfsePorRps(xmlDoc, configuracao);
            consultarNfsePorRps.Executar();

            MessageBox.Show(consultarNfsePorRps.RetornoWSString);
        }

        private void BtnSubstituirNFSe_Click(object sender, EventArgs e)
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(@"D:\Wandrey\OneDrive\Documentos\Unimake\Treinamentos\LIVEs UniNFe\TreinamentoDLL\Recursos\SubstituirNfseEnvio-ped-substnfse.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeSubstituirNfse,
                SchemaVersao = "2.04",
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 2933307
            };

            var substituirNFse = new ServicoNFSe.SubstituirNfse(xmlDoc, configuracao);
            substituirNFse.Executar();

            MessageBox.Show(substituirNFse.RetornoWSString);
        }

        private void BtnEnviarEventoCancelamentoMDFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlMDFe.EventoMDFe
            {
                Versao = "3.00",
                InfEvento = new XmlMDFe.InfEvento(new XmlMDFe.DetEventoCanc
                {
                    NProt = "141200000007987",
                    VersaoEvento = "3.00",
                    XJust = "Teste de cancelamento do MDFe"
                })
                {
                    COrgao = UFBrasil.PR,
                    ChMDFe = "41200210859283000185570010000005671227070615",
                    CNPJ = "10859283000185",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoMDFe.Cancelamento,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoMDFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Demostrar os retornos
            MessageBox.Show(recepcaoEvento.RetornoWSString);
            MessageBox.Show(recepcaoEvento.Result.InfEvento.CStat + " - " + recepcaoEvento.Result.InfEvento.XMotivo);

            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo MDFe com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo MDFe
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo MDFe prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                    break;

                default:
                    //Evento rejeitado, fazer os devidos tratamentos.
                    break;
            }
        }

        private void BtnEnviarEventoEncerramentoMDFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlMDFe.EventoMDFe
            {
                Versao = "3.00",
                InfEvento = new XmlMDFe.InfEvento(new XmlMDFe.DetEventoEncMDFe
                {
                    VersaoEvento = "3.00",
                    NProt = "141200000007987",
                    CMun = 3106200,
                    CUF = UFBrasil.MG,
                    DtEnc = DateTime.Now
                })
                {
                    COrgao = UFBrasil.PR,
                    ChMDFe = "41200210859283000185570010000005671227070615",
                    CNPJ = "10859283000185",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoMDFe.Encerramento,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            //Definir a configuração básica
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoMDFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Demostrar os retornos
            MessageBox.Show(recepcaoEvento.RetornoWSString);
            MessageBox.Show(recepcaoEvento.Result.InfEvento.CStat + " - " + recepcaoEvento.Result.InfEvento.XMotivo);

            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo MDFecom situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo MDFe
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo MDFe prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                    break;

                default:
                    //Evento rejeitado, fazer os devidos tratamentos.
                    break;
            }
        }

        private void BtnConsultarStatusCTe_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.ConsStatServCte
            {
                Versao = "4.00",
                TpAmb = TipoAmbiente.Homologacao,
                CUF = UFBrasil.SP
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var statusServico = new ServicoCTe.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.Result.CStat + " - " + statusServico.Result.XMotivo);
        }

        private void BtnConsultarMDFeNaoEncerrado_Click(object sender, EventArgs e)
        {
            var xmlCons = new XmlMDFe.ConsMDFeNaoEnc
            {
                Versao = "3.00",
                CNPJ = "06117473000150",
                TpAmb = TipoAmbiente.Homologacao,
                XServ = "CONSULTAR NÃO ENCERRADOS"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado,
                CodigoUF = (int)UFBrasil.PR
            };

            var consultaMDFeNaoEnc = new ServicoMDFe.ConsNaoEnc(xmlCons, configuracao);
            consultaMDFeNaoEnc.Executar();

            MessageBox.Show(consultaMDFeNaoEnc.RetornoWSString);
        }

        private void BtnEnviarCTeSincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.CTe
            {
                InfCTe = new XmlCTe.InfCTe
                {
                    Versao = "4.00",
                    Ide = new XmlCTe.Ide
                    {
                        CUF = UFBrasil.PR,
                        CCT = "01234567",
                        CFOP = "6352",
                        NatOp = "PREST.SERV.TRANSP.INDUSTR",
                        Mod = ModeloDFe.CTe,
                        Serie = 1,
                        NCT = 868,
                        DhEmi = DateTime.Now,
                        TpImp = FormatoImpressaoDACTE.NormalPaisagem,
                        TpEmis = TipoEmissao.Normal,
                        TpAmb = TipoAmbiente.Homologacao,
                        TpCTe = TipoCTe.Normal,
                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                        VerProc = "UNICO V8.0",
                        CMunEnv = "4118402",
                        XMunEnv = "PARANAVAI",
                        UFEnv = UFBrasil.PR,
                        Modal = ModalidadeTransporteCTe.Rodoviario,
                        TpServ = TipoServicoCTe.Normal,
                        CMunIni = "4118402",
                        XMunIni = "PARANAVAI",
                        UFIni = UFBrasil.PR,
                        CMunFim = "3305109",
                        XMunFim = "SAO JOAO DE MERITI",
                        UFFim = UFBrasil.RJ,
                        Retira = SimNao.Nao,
                        IndIEToma = IndicadorIEDestinatario.ContribuinteICMS,
                        Toma3 = new XmlCTe.Toma3
                        {
                            Toma = TomadorServicoCTe.Remetente,
                        },
                    },
                    Emit = new XmlCTe.Emit
                    {
                        CNPJ = "00000000000000",
                        IE = "9999999999",
                        XNome = "XXXXXX XXXXXX XXXXXX",
                        XFant = "XXXXXX XXXXXX",
                        EnderEmit = new XmlCTe.EnderEmit
                        {
                            XLgr = "XXXXXXXXXXXXXXXXXXXXXXX",
                            Nro = "11111",
                            XBairro = "XXXXXXXXXXXXXX",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87700000",
                            UF = UFBrasil.PR,
                            Fone = "04433333333",
                        },
                        CRT = CRT.RegimeNormal
                    },
                    Rem = new XmlCTe.Rem
                    {
                        CNPJ = "00000000000000",
                        IE = "9999999999",
                        XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        XFant = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        Fone = "04433333333",
                        EnderReme = new XmlCTe.EnderReme
                        {
                            XLgr = "XXXXXXXXXXXXXXXXXX",
                            Nro = "9999",
                            XBairro = "XXXXXXXXXXXXXXX",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87700000",
                            UF = UFBrasil.PR,
                            CPais = 1058,
                            XPais = "BRASIL",
                        }
                    },
                    Dest = new XmlCTe.Dest
                    {
                        CNPJ = "00000000000000",
                        IE = "ISENTO",
                        XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        EnderDest = new XmlCTe.EnderDest
                        {
                            XLgr = "XXXXXXXXXXXXXXXXXXXXXXXXXXX",
                            Nro = "55",
                            XBairro = "CENTRO",
                            CMun = 3305109,
                            XMun = "SAO JOAO DE MERITI",
                            CEP = "25520570",
                            UF = UFBrasil.RJ,
                            CPais = 1058,
                            XPais = "BRASIL",
                        },
                    },
                    VPrest = new XmlCTe.VPrest
                    {
                        VTPrest = 50.00,
                        VRec = 50.00,
                        Comp = new List<XmlCTe.Comp>
                                    {
                                        new XmlCTe.Comp
                                        {
                                            XNome = "FRETE VALOR",
                                            VComp = 50.00,
                                        },
                                    },
                    },
                    Imp = new XmlCTe.Imp
                    {
                        ICMS = new XmlCTe.ICMS
                        {
                            ICMSSN = new XmlCTe.ICMSSN
                            {
                                CST = "90",
                                IndSN = SimNao.Sim,
                            }
                        }
                    },
                    InfCTeNorm = new XmlCTe.InfCTeNorm
                    {
                        InfCarga = new XmlCTe.InfCarga
                        {
                            VCarga = 6252.96,
                            ProPred = "xxxxxxx",
                            InfQ = new List<XmlCTe.InfQ>
                                        {
                                            new XmlCTe.InfQ
                                            {
                                                CUnid = CodigoUnidadeMedidaCTe.KG,
                                                TpMed ="PESO BRUTO",
                                                QCarga = 320.0000,
                                            },
                                            new XmlCTe.InfQ
                                            {
                                                CUnid = CodigoUnidadeMedidaCTe.UNIDADE,
                                                TpMed ="UNIDADE",
                                                QCarga = 1.0000,
                                            },
                                        },
                        },
                        InfDoc = new XmlCTe.InfDoc
                        {
                            InfNFe = new List<XmlCTe.InfNFe>
                                        {
                                            new XmlCTe.InfNFe
                                            {
                                                Chave = "41444444444444444444444444444444444444444441"
                                            },
                                        },
                        },
                        InfModal = new XmlCTe.InfModal
                        {
                            VersaoModal = "4.00",
                            Rodo = new XmlCTe.Rodo
                            {
                                RNTRC = "44444444",
                                Occ = new List<XmlCTe.Occ>
                                            {
                                                new XmlCTe.Occ
                                                {
                                                    NOcc = 810,
                                                    DEmi = DateTime.Now,
                                                    EmiOcc = new XmlCTe.EmiOcc
                                                    {
                                                        CNPJ = "00000000000000",
                                                        CInt = "0000000000",
                                                        IE = "9999999999",
                                                        UF = UFBrasil.PR,
                                                        Fone = "04433333333",
                                                    },
                                                },
                                            },
                            },
                        },
                    },
                    InfRespTec = new XmlCTe.InfRespTec
                    {
                        CNPJ = "00000000000000",
                        XContato = "XXXXXXXXXXXXXXXXXXXXXXX",
                        Email = "teste@gmail.com",
                        Fone = "04433333333",
                    },
                },
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacaoSinc = new ServicoCTe.AutorizacaoSinc(xml, configuracao);
            autorizacaoSinc.Executar();

            if (autorizacaoSinc.Result != null)
            {
                if (autorizacaoSinc.Result.CStat == 104 || //Lote Recebido com Sucesso
                    autorizacaoSinc.Result.CStat == 100)  //CTe Autorizado
                {
                    if (autorizacaoSinc.Result.ProtCTe.InfProt.CStat == 100) //CTe Autorizado
                    {
                        autorizacaoSinc.GravarXmlDistribuicao(@"d:\testenfe");

                        var stringXMLDistribuicaoCTe = autorizacaoSinc.CteProcResults["50230500000000000000570010000008611017220679"].GerarXML().OuterXml;
                    }
                }
            }
        }

        private void BtnEnviarCTeOSSincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTeOS.CTeOS
            {
                Versao = "4.00",
                InfCTe = new XmlCTeOS.InfCTe
                {
                    Versao = "4.00",
                    Ide = new XmlCTeOS.Ide
                    {
                        CUF = UFBrasil.PR,
                        CCT = "12356488",
                        CFOP = "6352",
                        NatOp = "PREST.SERV.TRANSP.INDUSTR",
                        Mod = ModeloDFe.CTeOS,
                        Serie = 1,
                        NCT = 861,
                        DhEmi = DateTime.Now,
                        TpImp = FormatoImpressaoDACTE.NormalPaisagem,
                        TpEmis = TipoEmissao.Normal,
                        TpAmb = TipoAmbiente.Homologacao,
                        TpCTe = TipoCTe.Normal,
                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                        VerProc = "UNICO V8.0",
                        CMunEnv = "4118402",
                        XMunEnv = "PARANAVAI",
                        UFEnv = UFBrasil.PR,
                        Modal = ModalidadeTransporteCTe.Rodoviario,
                        TpServ = TipoServicoCTeOS.TransportePessoas,
                        CMunIni = "4118402",
                        XMunIni = "PARANAVAI",
                        UFIni = UFBrasil.PR,
                        CMunFim = "3305109",
                        XMunFim = "SAO JOAO DE MERITI",
                        UFFim = UFBrasil.RJ,
                        IndIEToma = IndicadorIEDestinatario.ContribuinteICMS,
                        InfPercurso = new List<XmlCTeOS.InfPercurso>
                            {
                                new XmlCTeOS.InfPercurso
                                {
                                    UFPer = UFBrasil.SP
                                }
                            }
                    },
                    Compl = new XmlCTeOS.Compl
                    {
                        XObs = "Teste de observacao",
                        ObsCont = new List<XmlCTeOS.ObsCont>
                            {
                                new XmlCTeOS.ObsCont
                                {
                                    XCampo = "LEI DA TRANSPARENCIA",
                                    XTexto = "O valor aproximado de tributos incidentes sobre o preco deste servico e de R$ 177.33 .(0) Fonte: IBPT"
                                }
                            },
                    },
                    Emit = new XmlCTeOS.Emit
                    {
                        CNPJ = "00000000000000",
                        IE = "0000000000",
                        XNome = "XXXXXXXXXXXXXXXXXXXX",
                        XFant = "XXXXXXXXXXXXX",
                        EnderEmit = new XmlCTeOS.EnderEmit
                        {
                            XLgr = "XXXXXXXXXXXXXXXXXXXXXXX",
                            Nro = "00001",
                            XBairro = "XXXXXXXXXXXXXX",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87700000",
                            UF = UFBrasil.PR,
                            Fone = "04444444444",
                        },
                        CRT = CRT.SimplesNacional
                    },
                    Toma = new XmlCTeOS.Toma
                    {
                        CNPJ = "00000000000000",
                        IE = "0000000000",
                        XNome = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        XFant = "CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                        Fone = "04434225480",
                        EnderToma = new XmlCTeOS.EnderToma
                        {
                            XLgr = "XXXXXXXXXXXXXXXXXXXXXXX",
                            Nro = "00001",
                            XBairro = "XXXXXXXXXXXXXX",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87700000",
                            UF = UFBrasil.PR,
                            CPais = 1058,
                            XPais = "BRASIL",
                        }
                    },
                    VPrest = new XmlCTeOS.VPrest
                    {
                        VTPrest = 2845.15,
                        VRec = 2845.15,
                        Comp = new List<XmlCTeOS.Comp>
                            {
                                new XmlCTeOS.Comp
                                {
                                    XNome = "VIAGEM TURISMO",
                                    VComp = 2356.00,
                                },
                                new XmlCTeOS.Comp
                                {
                                    XNome = "PEDAGIO",
                                    VComp = 311.82,
                                },
                            },
                    },
                    Imp = new XmlCTeOS.Imp
                    {
                        ICMS = new XmlCTeOS.ICMS
                        {
                            ICMS00 = new XmlCTeOS.ICMS00
                            {
                                CST = "00",
                                VBC = 2533.33,
                                PICMS = 7.00,
                                VICMS = 177.33
                            }
                        },
                        VTotTrib = 177.33,
                        InfTribFed = new XmlCTeOS.InfTribFed
                        {
                            VPIS = 30.00,
                            VCOFINS = 3.00,
                            VIR = 3.00,
                            VINSS = 3.00,
                            VCSLL = 3.00
                        }
                    },
                    InfCTeNorm = new XmlCTeOS.InfCTeNorm
                    {
                        InfServico = new XmlCTeOS.InfServico
                        {
                            XDescServ = "TRANSPORTES DE PESSOINHAS",
                            InfQ = new XmlCTeOS.InfQ
                            {
                                QCarga = 1
                            }
                        },
                        Seg = new List<XmlCTeOS.Seg>
                            {
                                new XmlCTeOS.Seg
                                {
                                    RespSeg = ResponsavelSeguroCTeOS.EmitenteCTeOS
                                }
                            },
                        InfModal = new XmlCTeOS.InfModal
                        {
                            VersaoModal = "4.00",
                            RodoOS = new XmlCTeOS.RodoOS
                            {
                                TAF = "999999999999",
                            }
                        }
                    },
                    AutXML = new List<XmlCTeOS.AutXML>
                        {
                            new XmlCTeOS.AutXML
                            {
                             CNPJ = "99999999999999",
                            },
                            new XmlCTeOS.AutXML
                            {
                             CNPJ = "99999999999998",
                            }
                        },
                    InfRespTec = new XmlCTeOS.InfRespTec
                    {
                        CNPJ = "00000000000000",
                        XContato = "XXXXXXXXXXXXXXXXXXXXXXX",
                        Email = "teste@gmail.com",
                        Fone = "04433333333",
                    },
                },
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoCTeOS.Autorizacao(xml, configuracao);
            autorizacao.Executar();

            if (autorizacao.Result.ProtCTe != null)
            {
                if (autorizacao.Result.CStat == 103) //103 = Lote Recebido com sucesso
                {
                    if (autorizacao.Result.ProtCTe.InfProt.CStat == 100) //Autorizado
                    {
                        autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");
                    }
                }
            }
        }

        private void BtnEnviarEventoEPEC_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento> {
                        new XmlNFe.Evento
                        {
                            Versao = "1.00",
                            InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoEPEC
                            {
                                COrgaoAutor = UFBrasil.PR,
                                TpAutor = TipoAutor.EmpresaEmitente,
                                VerAplic = "1.00",
                                TpNF = TipoOperacao.Saida,
                                DhEmi = DateTime.Now,
                                IE = "9032000301",
                                Versao = "1.00",
                                Dest = new XmlNFe.DetEventoEPECDest
                                {
                                    CNPJ = "06117473000150",
                                    UF = UFBrasil.PR,
                                    VNF = 86.00,
                                    VICMS = 6.02,
                                    VST = 0.00
                                }
                            })
                            {
                                COrgao = UFBrasil.AN,
                                ChNFe = "41190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.EPEC,
                                NSeqEvento = 1,
                                VerEvento = "1.00",
                                TpAmb = TipoAmbiente.Homologacao
                            }
                        }
                    }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoNFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();


            var xmlResgatado = XMLUtility.Deserializar<XmlNFe.EnviNFe>("string do meu xml que eu guardei anteriormente");

            var autorizacao = new ServicoNFe.Autorizacao(xmlResgatado, configuracao);
            autorizacao.Executar();
        }

        private void BtnRecuperarXMLNFeDistribuicao_Click(object sender, EventArgs e)
        {
            #region Montar o XML da NFe ou qualquer outro documento (CTe, CTeOS, NFCe ou MDFe)

            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Nao,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 59903,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 84.90
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            #endregion

            #region Montar a configuração básica para o envio da NFe

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            #endregion

            #region Criar o objeto para consumir o serviço e enviar a NFe

            //Montar o objeto do serviço
            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);

            //Guardar a chave da nota gerada no banco de dados. Aqui só coloquei em uma variável para que fique registrado como pegar a chave
            var chaveNFe = xml.NFe[0].InfNFe[0].Chave;

            //Salvar o XML da nota em uma pasta ou no banco de dados antes de tentar enviar para a SEFAZ
            File.WriteAllText(@"d:\testenfe\xmlnfe-nfe.xml", autorizacao.ConteudoXMLAssinado.GetElementsByTagName("NFe")[0].OuterXml);

            //Se conseguiu salvar a nota e está tudo certo, daí sim enviar, conforma abaixo. Mas só envie se tiver garantia que o XML foi salvo acima. Eu vou fazer um teste simples que é ver se o XML existe.
            if (File.Exists(@"d:\testenfe\xmlnfe-nfe.xml"))
            {
                autorizacao.Executar();
            }

            //Agora não vou pegar o retorno para fingir que a internet caiu bem na hora, então veja que a nota foi enviada e eu não peguei o recibo ou o retorno da autorização, no caso de envio síncrono, como vou finalizar o processo?
            //Vamos para o botão da PARTE 2

            #endregion
        }

        private void BtnRecuperarXMLNFeDistribuicao2_Click(object sender, EventArgs e)
        {
            #region Resgatar o XML guardado

            var doc = new XmlDocument();
            doc.Load(@"d:\testenfe\xmlnfe-nfe.xml");

            var xmlNFe = new XmlNFe.EnviNFe
            {
                IdLote = "000000000000002",
                IndSinc = SimNao.Nao,
                Versao = "4.00",
                NFe = new List<XmlNFe.NFe>
                {
                    XMLUtility.Deserializar<XmlNFe.NFe>(doc)
                }
            };

            #endregion

            #region Criar o XML da consulta situação da NFe

            var xmlConsSit = new XmlNFe.ConsSitNFe
            {
                ChNFe = xmlNFe.NFe[0].InfNFe[0].Chave,
                TpAmb = TipoAmbiente.Homologacao,
                Versao = "4.00"
            };

            #endregion

            #region Criar uma configuração básica para efetuarmos a consulta situação da NFe pela sua chave.

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            #endregion

            #region Criar o serviço para consumir o serviço de consulta protocolo

            var consultaProtocolo = new ServicoNFe.ConsultaProtocolo(xmlConsSit, configuracao);
            consultaProtocolo.Executar();

            #endregion

            #region Criar objeto para consumir o serviço de envio da NFe para finalizar a nota gerando o arquivo de distribuição

            if (consultaProtocolo.Result.CStat == 100) //NFe Autorizada
            {
                var autorizacao = new ServicoNFe.Autorizacao(xmlNFe, configuracao)
                {
                    RetConsReciNFe = null
                };
                autorizacao.RetConsSitNFes.Add(consultaProtocolo.Result);

                autorizacao.GravarXmlDistribuicao(@"d:\testenfe");
            }

            #endregion

        }

        private void BtnCarregarA3comPIN_Click(object sender, EventArgs e)
        {
            var cert = new CertificadoDigital();
            CertificadoA3Selecionado = cert.Selecionar();

            if (ClsX509Certificate2Extension.IsA3(CertificadoA3Selecionado))
            {
                ClsX509Certificate2Extension.SetPinPrivateKey(CertificadoA3Selecionado, "12345678");
            }

            //Para funcionar o método IsA3 e SetPinPrivateKey, abaixo, tem que dar o using no name space a seguir "using Unimake.Business.DFe.Security" 
            //caso contrário não vai localizar a extensão criada para a X509Certificate2 na DLL.
            //Testa se é um certificado A3 ou não e se for vai setar o PIN, caso eu não passe o PIN, no momento do uso do certificado vai abrir aquela tela do administrador do token para informar manualmente.
            if (CertificadoA3Selecionado.IsA3())
            {
                //Setar a senha do PIN do certificado A3
                CertificadoA3Selecionado.SetPinPrivateKey("1234");
            }
        }

        private void BtnImprimirDANFeEtiqueta_Click(object sender, EventArgs e)
        {
            var config = new DANFe.Configurations.UnidanfeConfiguration
            {
                Arquivo = @"D:\testenfe\41220306117473000150550010000111111111111111-procNFe.xml",
                Visualizar = true,
                Imprimir = false,
                EnviaEmail = false,
                Configuracao = "DANFE_ETIQ" //Informar a configuração específico para o DANFe etiqueta, aqui está o segredo.
            };

            DANFe.UnidanfeServices.Execute(config);
        }

        private void BtnEventoCancelamentoCTe_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoCanc
                {
                    VersaoEvento = "4.00",
                    NProt = "141200000001111",
                    XJust = "Justificativa de teste de cancelamento"
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.Cancelamento,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnEventoCancelamentoCTeOS_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoCanc
                {
                    NProt = "141200000001111",
                    VersaoEvento = "4.00",
                    XJust = "Justificativa para cancelamento da CTe de teste"
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.Cancelamento,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnEventoCCeCTe_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoCCE
                {
                    VersaoEvento = "4.00",
                    EventoCCeCTe = new XmlCTe.EventoCCeCTe
                    {
                        InfCorrecao = new List<XmlCTe.InfCorrecao>
                        {
                            new XmlCTe.InfCorrecao
                            {
                                GrupoAlterado = "ide",
                                CampoAlterado = "cfop",
                                ValorAlterado = "6353",
                                NroItemAlterado = ""
                            },
                            new XmlCTe.InfCorrecao
                            {
                                GrupoAlterado = "ide",
                                CampoAlterado = "cfop",
                                ValorAlterado = "6353",
                                NroItemAlterado = ""
                            }
                        }
                    }
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.CartaCorrecao,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }

        }

        private void BtnEventoCCeCTeOS_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoCCE
                {
                    VersaoEvento = "4.00",
                    EventoCCeCTe = new XmlCTe.EventoCCeCTe
                    {
                        InfCorrecao = new List<XmlCTe.InfCorrecao>
                        {
                            new XmlCTe.InfCorrecao
                            {
                                GrupoAlterado = "ide",
                                CampoAlterado = "cfop",
                                ValorAlterado = "6353",
                                NroItemAlterado = ""
                            }
                        }
                    }
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.CartaCorrecao,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }

        }

        private void BtnFormasTrabalharCertificado_Click(object sender, EventArgs e)
        {
            var certificado = new CertificadoDigital();

            #region Certificado A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que está instalado no repositório do windows

            var certSel1 = certificado.AbrirTelaSelecao();

            var config1 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel1
            };

            //Você pode salvar o Thumbprint ou SerialNumber do certificado para salvar em sua base de dados para resgatar ele no futuro no repositório do windows sem precisar abrir tela para selecionar novamente.
            var thumbprint = certSel1.Thumbprint;
            var serialNumber = certSel1.SerialNumber;

            #endregion

            #region Somente certificado A1 - Carregar o certificado digital direto do arquivo .PFX.

            var certSel2 = certificado.CarregarCertificadoDigitalA1(@"d:\projetos\UnimakePV.pfx", "12345678");

            var config2 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel2
            };

            //Outra forma de configurar pegando o .PFX sem precisar carregar, ou seja, a propria configuração já carrega o A1.
            //Não recomendamos pq toda vez vai fazer acesso ao HD, melhor é carregar na forma anterior e deixar em uma variável global e só utilizar quando precisar.
            var config22 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoArquivo = @"D:\projetos\UnimakePV.pfx",
                CertificadoSenha = "12345678"
            };

            #endregion

            #region Certificado A1 e A3 - Buscar o certificado digital, instalado no repositório do windows, pelo Serial Number

            var certSel3 = certificado.BuscarCertificadoDigital(serialNumber);
            MessageBox.Show(certSel3.Subject);

            var config3 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel3
            };

            #endregion

            #region Certificado A1 e A3 - Buscar o certificado digital, instalado no repositório do windows, pelo ThumbPrint

            var certSel4 = certificado.BuscarCertificadoDigital(thumbprint);

            var config4 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel4
            };

            #endregion

            #region Somente certificado A1 - Criando uma array bytes do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.

            var certificadoByte = certificado.ToByteArray(@"d:\projetos\UnimakePV.pfx");

            //Agora você pode gravar o conteúdo da "certificadoByte" no banco de dados

            //Recuperar o certificado para uso a partir de uma array byte
            var certSel5 = certificado.CarregarCertificadoDigitalA1(certificadoByte, "12345678");

            var config5 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel5
            };

            #endregion

            #region Somente certificado A1 - Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior segurança) para resgatar o conteúdo da base de dados.

            var certificadoBase64 = certificado.ToBase64(@"d:\projetos\UnimakePV.pfx");

            //Agora você pode gravar o conteúdo da "certificadoBase64" no banco dados

            //Recuperar o certificado digital a partir de um Base64
            var certSel6 = certificado.FromBase64(certificadoBase64, "12345678");

            var config6 = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = certSel6
            };

            #endregion
        }

        private void BtnEnviarEventoCancelamentoNFCe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento>
                {
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoCanc
                        {
                            NProt = "141190000660363",
                            Versao = "1.00",
                            XJust = "Justificativa de teste de cancelamento"
                        })
                        {
                            COrgao = UFBrasil.PR,
                            ChNFe = "41190806117473000150650010000579131943463890",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.Cancelamento,
                            NSeqEvento = 1,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoNFCe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            if (recepcaoEvento.Result.CStat == 128) //Lote de evento processado com sucesso
            {
                switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
                {
                    case 135: //Evento homologado
                    case 155: //Evento homologado fora do prazo permitido
                        recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                        break;

                    default:
                        //Tratamentos necessários quando o evento é rejeitado
                        break;
                }
            }
        }

        private void BtnDistribuicaoDFe_Click(object sender, EventArgs e)
        {
            var nsu = "000000000000000"; //Começar com o NSU 0 quando não tem o ultNSU

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            PbConsultaDFe.Visible = true;
            PbConsultaDFe.Minimum = 0;
            Application.DoEvents();
            PbConsultaDFe.Refresh();

            while (true)
            {
                var xml = new XmlNFe.DistDFeInt
                {
                    Versao = "1.01",
                    TpAmb = TipoAmbiente.Homologacao,
                    CNPJ = "06117473000150",
                    CUFAutor = UFBrasil.PR,
                    DistNSU = new XmlNFe.DistNSU
                    {
                        UltNSU = nsu
                    }
                };

                var distribuicaoDFe = new ServicoNFe.DistribuicaoDFe(xml, configuracao);
                distribuicaoDFe.Executar();

                #region Atualizar ProgressBar

                if (PbConsultaDFe.Maximum != Convert.ToInt32(distribuicaoDFe.Result.MaxNSU))
                {
                    PbConsultaDFe.Maximum = Convert.ToInt32(distribuicaoDFe.Result.MaxNSU);
                }

                PbConsultaDFe.Value = Convert.ToInt32(distribuicaoDFe.Result.UltNSU);
                PbConsultaDFe.Refresh();
                Application.DoEvents();

                #endregion Atualizar ProgressBar

                if (distribuicaoDFe.Result.CStat == 138) // Documentos localizados e 137 = Não tem documentos
                {
                    var folder = @"c:\testenfe\doczip";

                    //Salvar XMLs do docZIP no HD
                    distribuicaoDFe.GravarXMLDocZIP(folder, true);
                }

                nsu = distribuicaoDFe.Result.UltNSU; //Salvar o ultNSU para usar na próxima consulta
                //Importante salvar o conteúdo de "nsu" na base de dados.

                if (Convert.ToInt64(distribuicaoDFe.Result.UltNSU) >= Convert.ToInt64(distribuicaoDFe.Result.MaxNSU))
                {
                    break;
                }

                PbConsultaDFe.Visible = false;
                Application.DoEvents();

                //Guarde o conteúdo da variável "nsu" (que tem o ultNSU retornado) em sua base para ser utilizado na próxima consulta.
                //Agora aguarde 1 hora para dar sequencia na consulta a partir do ultNSU retornado, esta é a regra para evitar Consumo indevido.
            }
        }

        private void BtnManifestacaoDestinatario_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<XmlNFe.Evento> {
                    new XmlNFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new XmlNFe.InfEvento(new XmlNFe.DetEventoManif
                        {
                            Versao = "1.00",
                            DescEvento = "Operacao nao Realizada", //Pode ser: "Ciencia da Operacao" / "Confirmacao da Operacao" / "Desconhecimento da Operacao" / "Operacao nao Realizada"
                            XJust = "Justificativa para manifestação da NFe de teste"
                        })
                        {
                            COrgao = UFBrasil.AN,
                            ChNFe = "41200211111111111111111111111111111111111115",
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.ManifestacaoOperacaoNaoRealizada,
                            NSeqEvento = 1,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Homologacao
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoNFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição do evento
            if (recepcaoEvento.Result.CStat == 128) //128 = Lote de evento processado com sucesso
            {
                switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
                {
                    case 135: //Evento homologado com vinculação da respectiva NFe
                        recepcaoEvento.GravarXmlDistribuicao(@"c:\testenfe\");
                        break;

                    default: //Evento rejeitado
                        //Executar as ações necessárias
                        break;
                }
            }
        }

        private void BtnInutilizacaoNFCe_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.InutNFe
            {
                Versao = "4.00",
                InfInut = new XmlNFe.InutNFeInfInut
                {
                    Ano = "19",
                    CNPJ = "06117473000150",
                    CUF = UFBrasil.PR,
                    Mod = ModeloDFe.NFCe,
                    NNFIni = 57919,
                    NNFFin = 57919,
                    Serie = 1,
                    TpAmb = TipoAmbiente.Homologacao,
                    XJust = "Justificativa da inutilizacao de teste"
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                CertificadoDigital = CertificadoSelecionado
            };

            var inutilizacao = new ServicoNFCe.Inutilizacao(xml, configuracao);
            inutilizacao.Executar();

            //Gravar o XML de distribuição se a inutilização foi homologada
            switch (inutilizacao.Result.InfInut.CStat)
            {
                case 102: //Inutilizacao Homologada
                    inutilizacao.GravarXmlDistribuicao(@"d:\testenfe\");
                    break;

                default:
                    //Tratar a rejeição
                    break;
            }

        }

        private void BtnNFeComTagAutXml_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57990,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                AutXML = new List<XmlNFe.AutXML>
                                {
                                    new XmlNFe.AutXML
                                    {
                                        CNPJ = "00000000000000",
                                    },
                                    new XmlNFe.AutXML
                                    {
                                        CPF = "00000000000"
                                    },
                                    new XmlNFe.AutXML
                                    {
                                        CNPJ = "11111111111111"
                                    }
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 80.90
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
            var xmlString = autorizacao.ConteudoXMLAssinado.OuterXml;
            //Gravo no meu banco de dados o xmlString

            autorizacao.Executar();

            //Gravar o arquivo do conteúdo retornado em uma pasta qualquer para ter em segurança. Pode-se também gravar na base de dados. Fica a critério de cada um.
            File.WriteAllText(@"c:\testenfe\retorno\nomearquivoretorno.xml", autorizacao.RetornoWSString);

            if (autorizacao.Result.ProtNFe != null)
            {
                switch (autorizacao.Result.ProtNFe.InfProt.CStat)
                {
                    case 100: //Autorizado o uso da NFe
                    case 110: //Uso Denegado
                    case 150: //Autorizado o uso da NF-e, autorização fora de prazo
                    case 205: //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                    case 301: //Uso Denegado: Irregularidade fiscal do emitente
                    case 302: //Uso Denegado: Irregularidade fiscal do destinatário
                    case 303: //Uso Denegado: Destinatário não habilitado a operar na UF
                        autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");
                        var docProcNFe = autorizacao.NfeProcResult.GerarXML(); //Gerar o Objeto para pegar a string e gravar em banco de dados
                        MessageBox.Show(autorizacao.NfeProcResult.NomeArquivoDistribuicao);
                        break;

                    default:
                        //NF Rejeitada
                        break;
                }
            }
        }
        private void BtnDistribuicaoDFE135_Click(object sender, EventArgs e)
        {
            var nsu = "000000000000000"; //Começar com o NSU 0 quando não tem o ultNSU

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            PbConsultaDFe.Visible = true;
            PbConsultaDFe.Minimum = 0;
            Application.DoEvents();
            PbConsultaDFe.Refresh();

            while (true)
            {
                var xml = new XmlNFe.DistDFeInt
                {
                    Versao = "1.35",
                    TpAmb = TipoAmbiente.Homologacao,
                    CNPJ = "06117473000150",
                    CUFAutor = UFBrasil.PR,
                    DistNSU = new XmlNFe.DistNSU
                    {
                        UltNSU = nsu
                    }
                };

                var distribuicaoDFe = new ServicoNFe.DistribuicaoDFe(xml, configuracao);
                distribuicaoDFe.Executar();

                #region Atualizar ProgressBar

                if (PbConsultaDFe.Maximum != Convert.ToInt32(distribuicaoDFe.Result.MaxNSU))
                {
                    PbConsultaDFe.Maximum = Convert.ToInt32(distribuicaoDFe.Result.MaxNSU);
                }

                PbConsultaDFe.Value = Convert.ToInt32(distribuicaoDFe.Result.UltNSU);
                PbConsultaDFe.Refresh();
                Application.DoEvents();

                #endregion Atualizar ProgressBar

                if (distribuicaoDFe.Result.CStat == 138) // Documentos localizados e 137 = Não tem documentos
                {
                    var folder = @"c:\testenfe\doczip";

                    //Salvar XMLs do docZIP no HD
                    distribuicaoDFe.GravarXMLDocZIP(folder, true);
                }

                nsu = distribuicaoDFe.Result.UltNSU; //Salvar o ultNSU para usar na próxima consulta
                //Importante salvar o conteúdo de "nsu" na base de dados.

                if (Convert.ToInt64(distribuicaoDFe.Result.UltNSU) >= Convert.ToInt64(distribuicaoDFe.Result.MaxNSU))
                {
                    break;
                }

                PbConsultaDFe.Visible = false;
                Application.DoEvents();

                //Guarde o conteúdo da variável "nsu" (que tem o ultNSU retornado) em sua base para ser utilizado na próxima consulta.
                //Agora aguarde 1 hora para dar sequencia na consulta a partir do ultNSU retornado, esta é a regra para evitar Consumo indevido.
            }
        }

        private void BtnDistribuicaoDFeCTe_Click(object sender, EventArgs e)
        {
            var nsu = "000000000000000"; //Começar com o NSU 0 quando não tem o ultNSU			
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            while (true)
            {
                var xml = new XmlCTe.DistDFeInt
                {
                    Versao = "1.00",
                    TpAmb = TipoAmbiente.Producao,
                    CNPJ = "06117473000150",
                    CUFAutor = UFBrasil.PR,
                    DistNSU = new XmlCTe.DistNSU
                    {
                        UltNSU = nsu
                    }
                };

                var distribuicaoDFe = new ServicoCTe.DistribuicaoDFe(xml, configuracao);
                distribuicaoDFe.Executar();


                if (distribuicaoDFe.Result.CStat.Equals(138)) //138 = Documentos localizados e 137 = Não tem documentos
                {
                    var folder = @"d:\testenfe\doczip";

                    //Salvar os XMLs do docZIP no HD
                    distribuicaoDFe.GravarXMLDocZIP(folder);
                }

                nsu = distribuicaoDFe.Result.UltNSU; //Salvar o ultNSU para usar na próxima consulta

                if (Convert.ToInt64(distribuicaoDFe.Result.UltNSU) >= Convert.ToInt64(distribuicaoDFe.Result.MaxNSU))
                {
                    break;
                }
            }

            //Guarde o conteúdo da variável "nsu" (que tem o ultNSU retornado) em sua base para ser utilizado na próxima consulta.
            //Agora aguarde 1 hora para dar sequencia na consulta a partir do ultNSU retornado, esta é a regra para evitar Consumo indevido.
        }

        private void BtnCriarXmlNFSeCSharp_Click(object sender, EventArgs e)
        {
            var xmlNFSe = new XDocument(new XDeclaration("1.0", "utf-8", null));
            XNamespace xNamespace = "http://www.prefeitura.sp.gov.br/nfe";

            #region Criar a primeira tag <PedidoEnvioRPS> do XML com o namespace

            var tagPedidoEnvioRPS = new XElement(xNamespace + "PedidoEnvioRPS");

            #endregion

            #region Criar o grupo de tag <Cabecalho>

            //Criar tag <Cabecalho>
            var tagCabecalho = new XElement("Cabecalho");

            //Criar o atributo Versao na tag <Cabecalho>
            tagCabecalho.Add(new XAttribute("Versao", "1"));

            //Criar o grupo de tag CPFCNPJRemetente
            var tagCPFCNPJRemetente = new XElement("CPFCNPJRemetente");

            //Criar a tags filhas do grupo <CPFCNPJRemetente>
            tagCPFCNPJRemetente.Add(new XElement("CNPJ", "99999997000100"));

            //Adicionar a tag <CPFCNPJRementente> dentro da tag <Cabecalho>
            tagCabecalho.Add(tagCPFCNPJRemetente);

            //Adicionar a tag <Cabelhalho> dentro da tag <PedidoEnvioRPS>
            tagPedidoEnvioRPS.Add(tagCabecalho);

            #endregion

            #region Criar o grupo de tag <RPS>

            //Criar tag <RPS>
            var tagRPS = new XElement("RPS");

            tagRPS.Add(new XElement("Assinatura", "d8Pg/jdA7t5tSaB8Il1d/CMiLGgfFAXzTL9o5stv6TNbhm9I94DIo0/ocqJpGx0KzoEeIQz4RSn99pWX4fiW/aETlNT3u5woqCAyL6U2hSyl/eQfWRYrqFu2zcdc4rsAG/wJbDjNO8y0Pz9b6rlTwkIJ+kMdLo+EWXMnB744olYE721g2O9CmUTvjtBgCfVUgvuN1MGjgzpgyussCOSkLpGbrqtM5+pYMXZsTaEVIIck1baDkoRpLmZ5Y/mcn1/Om1fMyhJVUAkgI5xBrORuotIP7e3+HLJnKgzQQPWCtLyEEyAqUk9Gq64wMayITua5FodaJsX+Eic/ie3kS5m50Q=="));

            //Criar grupo de Tag <ChaveRPS>
            var tagChaveRPS = new XElement("ChaveRPS");

            //Criar tags filhas do grupo <ChaveRPS>
            tagChaveRPS.Add(new XElement("InscricaoPrestador", "39616924"));
            tagChaveRPS.Add(new XElement("SerieRPS", "BB"));
            tagChaveRPS.Add(new XElement("NumeroRPS", "4105"));

            tagRPS.Add(tagChaveRPS);

            //Criar várias tags filhas da tag <RPS>
            tagRPS.Add(new XElement("TipoRPS", "RPS-M"));
            tagRPS.Add(new XElement("DataEmissao", "2015-01-20"));
            tagRPS.Add(new XElement("StatusRPS", "N"));
            tagRPS.Add(new XElement("TributacaoRPS", "T"));
            tagRPS.Add(new XElement("ValorServicos", "20500"));
            tagRPS.Add(new XElement("ValorDeducoes", "5000"));
            tagRPS.Add(new XElement("ValorPIS", "10"));
            tagRPS.Add(new XElement("ValorCOFINS", "10"));
            tagRPS.Add(new XElement("ValorINSS", "10"));
            tagRPS.Add(new XElement("ValorIR", "10"));
            tagRPS.Add(new XElement("ValorCSLL", "10"));
            tagRPS.Add(new XElement("CodigoServico", "7617"));
            tagRPS.Add(new XElement("AliquotaServicos", "0.05"));
            tagRPS.Add(new XElement("ISSRetido", "false"));

            //Criar grupo de tag <CPFCNPJTomador>
            var tagCPFCNPJTomador = new XElement("CPFCNPJTomador");

            //Criar tags filhas do grupo <CPFCNPJTomador>
            tagCPFCNPJTomador.Add(new XElement("CPF", "12345678909"));

            //Adicionar a tag <CPFCNPJTomador> dentro da tag <RPS>
            tagRPS.Add(tagCPFCNPJTomador);

            //Criar mais tags filhas da tag <RPS>
            tagRPS.Add(new XElement("RazaoSocialTomador", "TOMADOR PF"));

            //Criar tag <EnderecoTomador>
            var tagEnderecoTomador = new XElement("EnderecoTomador");

            //Criar tags filhas da tag <EnderecoTomador>
            tagEnderecoTomador.Add(new XElement("TipoLogradouro", "Av"));
            tagEnderecoTomador.Add(new XElement("Logradouro", "Paulista"));
            tagEnderecoTomador.Add(new XElement("NumeroEndereco", "100"));
            tagEnderecoTomador.Add(new XElement("ComplementoEndereco", "Cj 35"));
            tagEnderecoTomador.Add(new XElement("Bairro", "Bela Vista"));
            tagEnderecoTomador.Add(new XElement("Cidade", "3550308"));
            tagEnderecoTomador.Add(new XElement("UF", "SP"));
            tagEnderecoTomador.Add(new XElement("CEP", "1310100"));

            //Adicionar a tag <EnderecoTomador> dentro da tag <RPS>
            tagRPS.Add(tagEnderecoTomador);

            //Criar mais tags filhas da tag <RPS>
            tagRPS.Add(new XElement("EmailTomador", "tomador@teste.com.br"));
            tagRPS.Add(new XElement("Discriminacao", "Desenvolvimento de Web Site Pessoal."));

            #endregion 

            //Adicionar a tag <RPS> dentro da tag <PedidoEnvioRPS>
            tagPedidoEnvioRPS.Add(tagRPS);

            //Adicionar a tag <PedidoEnvioRPS> no xmlNfse
            xmlNFSe.Add(tagPedidoEnvioRPS);

            //Recuperar o conteúdo gerado acima para passar para a DLL no formato XmlDocument, que é o tipo esperado pelo método Executar da DLL
            var conteudoXML = new XmlDocument();
            conteudoXML.CreateXmlDeclaration("1.0", "utf-8", null);

            using (var xmlReader = xmlNFSe.CreateReader())
            {
                conteudoXML.Load(xmlReader);
            }

            //Pronto... só passar a conteudoXML para o método Executar para enviar para a prefeitura de São Paulo-SP.
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = CertificadoSelecionado,
                TipoAmbiente = TipoAmbiente.Producao,
                CodigoMunicipio = 3550308, //São Paulo-SP
                Servico = Servico.NFSeEnvioRps,
                SchemaVersao = "2.00"
            };

            var envioRps = new ServicoNFSe.EnvioRps(conteudoXML, configuracao);
            envioRps.Executar();
        }

        private void BtnImprimirDANFeSemValorFiscal_Click(object sender, EventArgs e)
        {
            //Gere o XML da NFe/NFCe com o conteúdo da tag <nNF> igual a ZERO, assim: <nNF>0</nNF>.
            //Com isso, ao disparar o unidanfe apontando para este XML o mesmo vai entender que é DANFE somente para conferência e sem valor fiscal
            var config = new DANFe.Configurations.UnidanfeConfiguration
            {
                Arquivo = @"D:\testenfe\41220306117473000150550010000000001111111111-procNFe.xml",
                Visualizar = true,
                Imprimir = false,
                EnviaEmail = false,
                Configuracao = "PAISAGEM"
            };

            DANFe.UnidanfeServices.Execute(config);
        }

        private void BtnValidarXML_Click(object sender, EventArgs e)
        {
            try
            {
                var validarSchema = new DFe.ValidarSchema();
                var doc = new XmlDocument();

                //Validar XML da ProcNFe - Sem falhas
                doc.Load(@"D:\testenfe\41220306117473000150550010000111111111111111-procNFe.xml");

                var schema = "NFe.procNFe_v4.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/nfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar XML da ProcNFe - com falhas
                doc.Load(@"D:\testenfe\41220306117473000150550010000000001111111111-procNFe.xml");

                schema = "NFe.procNFe_v4.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/nfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar XML da NFe
                doc.Load(@"D:\testenfe\41220306117473000150550010000111111111111111-NFe.xml");

                schema = "NFe.nfe_v4.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/nfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar uma NFCe                
                doc.Load(@"D:\testenfe\41170706117473000150550010000463191912756548-nfe.xml");

                schema = "NFe.nfe_v4.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/nfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar XML de consulta status da NFe
                doc.Load(@"D:\testenfe\20100222T222310-ped-sta.xml");

                schema = "NFe.consStatServ_v4.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/nfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar XML de CTe                
                doc.Load(@"D:\testenfe\51160624686092000173570010000000031000000020-cte.XML");

                schema = "CTe.cte_v3.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/cte");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }

                //Validar XML de consulta status da MDFe
                doc.Load(@"D:\testenfe\20170210T100210-ped-sta.xml");

                schema = "MDFe.consStatServMDFe_v3.00.xsd";
                validarSchema.Validar(doc, schema, "http://www.portalfiscal.inf.br/mdfe");

                if (!validarSchema.Success)
                {
                    MessageBox.Show("Code: " + validarSchema.ErrorCode + "\r\n\r\nMessage: " + validarSchema.ErrorMessage);
                }
                else
                {
                    MessageBox.Show("XML validado com sucesso.");
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void BtnConsultarConfigUF_Click(object sender, EventArgs e)
        {
            var xml = new XmlGNRe.TConsultaConfigUf
            {
                Ambiente = TipoAmbiente.Homologacao,
                UF = UFBrasil.RS,
                Receita = new XmlGNRe.Receita
                {
                    Courier = SimNaoLetra.Nao,
                    Value = 100064
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoArquivo = @"D:\testenfe\testegnre1234.pfx",
                CertificadoSenha = "1234"
            };

            var consultaConfigUF = new ServicoGNRe.ConsultaConfigUF(xml, configuracao);
            consultaConfigUF.Executar();
        }

        private void BtnEnviarXMLGNRe_Click(object sender, EventArgs e)
        {
            var xml = new XmlGNRe.TLoteGNRE
            {
                Guias = new XmlGNRe.Guias
                {
                    TDadosGNRE = new List<XmlGNRe.TDadosGNRE>
                    {
                        new XmlGNRe.TDadosGNRE
                        {
                            Versao = "2.00",
                            UfFavorecida = UFBrasil.PR,
                            TipoGNRE = TipoGuiaGNRE.Simples,
                            ContribuinteEmitente = new XmlGNRe.ContribuinteEmitente
                            {
                                Identificacao = new XmlGNRe.Identificacao
                                {
                                    CNPJ = "07666666000166",
                                    IE = "9335665656"
                                },
                                RazaoSocial = "TESTE EMPRESA PARA ENVIO DA GNRE",
                                Endereco = "XXX XXXXXXX XXXXX",
                                Municipio = "04808",
                                UF = UFBrasil.PR,
                                CEP= "90399899",
                                Telefone = "04456566566"
                            },

                            ItensGNRE = new XmlGNRe.ItensGNRE
                            {
                                Item = new List<XmlGNRe.Item>
                                {
                                    new XmlGNRe.Item
                                    {
                                        Receita = "100099",
                                        DocumentoOrigem = new XmlGNRe.DocumentoOrigem
                                        {
                                            Tipo = "10",
                                            Value = "41210807666666000166550010001234551123455553"
                                        },
                                        DataVencimento = DateTime.Now,
                                        Valor = new List<XmlGNRe.Valor>
                                        {
                                            new XmlGNRe.Valor
                                            {
                                                Tipo = XmlGNRe.Valor.ItemValorTipo.Item11,
                                                ValorOriginal = 116.24
                                            }
                                        },
                                        ContribuinteDestinatario = new XmlGNRe.ContribuinteDestinatario
                                        {
                                            Identificacao = new XmlGNRe.Identificacao
                                            {
                                                IE = "1236566556"
                                            },
                                        },
                                    }
                                }
                            },

                            ValorGNRE = 30.00,
                            DataPagamento = DateTime.Now
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoArquivo = @"D:\testenfe\testegnre1234.pfx",
                CertificadoSenha = "1234",
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = 41 //Paraná
            };

            var loteRecepcao = new ServicoGNRe.LoteRecepcao(xml, configuracao);
            loteRecepcao.Executar();

            if (loteRecepcao.Result.SituacaoRecepcao.Codigo == "100")
            {
                //Aguardar 30 segundos para consultar o resultado do lote, é o que solicitam no manual.
                //Thread.Sleep(30000); 

                var xmlCons = new XmlGNRe.TConsLoteGNRE
                {
                    Ambiente = TipoAmbiente.Homologacao,
                    NumeroRecibo = loteRecepcao.RetornoWSXML.GetElementsByTagName("ns1:numero")[0].InnerText,
                    IncluirPDFGuias = SimNaoLetra.Sim,
                    IncluirArquivoPagamento = SimNaoLetra.Nao
                };

                var configCons = new Configuracao
                {
                    TipoDFe = TipoDFe.GNRE,
                    TipoEmissao = TipoEmissao.Normal,
                    CertificadoArquivo = @"D:\testenfe\testegnre1234.pfx",
                    CertificadoSenha = "1234",
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    CodigoUF = 41 //Paraná
                };

                var consultaResultadoLote = new ServicoGNRe.ConsultaResultadoLote(xmlCons, configCons);
                consultaResultadoLote.Executar();

                switch (consultaResultadoLote.Result.SituacaoProcess.Codigo)
                {
                    case "400": //Lote recebido, aguardando processamento
                    case "401": //Lote em processamento
                        //Tentar consultar mais tarde
                        break;

                    case "402": //Lote processado com sucesso
                        try
                        {
                            consultaResultadoLote.GravarXmlRetorno(@"d:\testenfe", xmlCons.NumeroRecibo + "-ret-gnre.xml");
                            consultaResultadoLote.GravarPDFGuia(@"d:\testenfe", "GuiaGNRE.pdf");
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show(ex.Message);
                        }
                        break;

                    case "403": //Lote processado com pendências ou Lote com pendência de tempo de processamento. As Guias com situação 4 (campo < situacaoGuia > para a versão 2.00) podem levar em média 20 minutos, e no máximo 1 hora para serem processadas.
                        //Analisar pendencias
                        break;

                    case "404": //Erro no processamento do lote.
                        //Enviar lote novamente.
                        break;
                }
            }
        }

        private void BtnEnviarEventoPagamentoOperacaoMDFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlMDFe.EventoMDFe
            {
                Versao = "3.00",
                InfEvento = new XmlMDFe.InfEvento(new XmlMDFe.DetEventoPagtoOperMDFe
                {
                    VersaoEvento = "3.00",
                    EventoPagtoOperMDFe = new XmlMDFe.EventoPagtoOperMDFe
                    {
                        DescEvento = "Pagamento Operacao MDF-e",
                        NProt = "941190000014312",
                        InfViagens = new XmlMDFe.InfViagens
                        {
                            NroViagem = "00001",
                            QtdViagens = "00001"
                        },
                        InfPag = new List<XmlMDFe.PagtoOperMDFeInfPag>
                            {
                                new XmlMDFe.PagtoOperMDFeInfPag
                                {
                                    XNome = "TESTE TRANSPORTE E OPERACOES LTDA",
                                    CNPJ = "00000000000000",
                                    Comp = new List<XmlMDFe.Comp>
                                    {
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.Outros,
                                            VComp = 2000.00,
                                            XComp = "PAGAMENTO DE FRETE"
                                        },
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.ValePedagio,
                                            VComp = 500.00
                                        },
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.Outros,
                                            VComp = 500.00,
                                            XComp = "COMPRA DE PNEUS"
                                        }
                                    },
                                    VContrato = 3000.00,
                                    IndPag = IndicadorPagamento.PagamentoPrazo,
                                    VAdiant = 500.00,
                                    InfPrazo = new List<XmlMDFe.InfPrazo>
                                    {
                                        new XmlMDFe.InfPrazo
                                        {
                                            NParcela = "001",
                                            DVenc = DateTime.Now.AddDays(20),
                                            VParcela = 2000.00
                                        },
                                        new XmlMDFe.InfPrazo
                                        {
                                            NParcela = "002",
                                            DVenc = DateTime.Now.AddDays(40),
                                            VParcela = 500.00
                                        }
                                    },
                                    InfBanc = new XmlMDFe.InfBanc
                                    {
                                        PIX = "+5544993333223"
                                    }
                                }
                            }
                    }
                })
                {
                    COrgao = UFBrasil.PR,
                    ChMDFe = "41200380568835000181580010000007171930099252",
                    CNPJ = "80568835000181",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoMDFe.PagamentoOperacao,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoMDFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição do evento, se homologado/autorizado
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnEnviarEventoAlteracaoPagamentoServicoMDFe_Click(object sender, EventArgs e)
        {
            var xml = new XmlMDFe.EventoMDFe
            {
                Versao = "3.00",
                InfEvento = new XmlMDFe.InfEvento(new XmlMDFe.DetEventoAlteracaoPagtoServMDFe
                {
                    VersaoEvento = "3.00",

                    EventoAlteracaoPagtoServMDFe = new XmlMDFe.EventoAlteracaoPagtoServMDFe
                    {
                        DescEvento = "Alteracao Pagamento Servico MDFe",
                        NProt = "941190000014312",
                        InfPag = new List<XmlMDFe.AlteracaoPagtoServMDFeInfPag>
                            {
                                new XmlMDFe.AlteracaoPagtoServMDFeInfPag
                                {
                                    XNome = "TESTE TRANSPORTE E OPERACOES LTDA",
                                    CNPJ = "00000000000000",
                                    Comp = new List<XmlMDFe.Comp>
                                    {
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.Outros,
                                            VComp = 2000.00,
                                            XComp = "PAGAMENTO DE FRETE"
                                        },
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.ValePedagio,
                                            VComp = 500.00
                                        },
                                        new XmlMDFe.Comp
                                        {
                                            TpComp = TipoComponenteMDFe.Outros,
                                            VComp = 500.00,
                                            XComp = "COMPRA DE PNEUS"
                                        }
                                    },
                                    VContrato = 3000.00,
                                    IndPag = IndicadorPagamento.PagamentoPrazo,
                                    VAdiant = 500.00,
                                    InfPrazo = new List<XmlMDFe.InfPrazo>
                                    {
                                        new XmlMDFe.InfPrazo
                                        {
                                            NParcela = "001",
                                            DVenc = DateTime.Now.AddDays(20),
                                            VParcela = 2000.00
                                        },
                                        new XmlMDFe.InfPrazo
                                        {
                                            NParcela = "002",
                                            DVenc = DateTime.Now.AddDays(40),
                                            VParcela = 500.00
                                        }
                                    },
                                    InfBanc = new XmlMDFe.InfBanc
                                    {
                                        PIX = "+5544993333223"
                                    }
                                }
                            }
                    }
                })
                {
                    COrgao = UFBrasil.PR,
                    ChMDFe = "41200380568835000181580010000007171930099252",
                    CNPJ = "80568835000181",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoMDFe.AlteracaoPagamentoServico,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoMDFe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição do evento, se homologado/autorizado
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnConsultarResultadoLoteGNRE_Click(object sender, EventArgs e)
        {
            var xml = new XmlGNRe.TConsLoteGNRE
            {
                Ambiente = TipoAmbiente.Homologacao,
                NumeroRecibo = "1234567890",
                IncluirPDFGuias = SimNaoLetra.Sim,
                IncluirArquivoPagamento = SimNaoLetra.Nao,
                IncluirNoticias = SimNaoLetra.Nao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoArquivo = @"D:\testenfe\testegnre1234.pfx",
                CertificadoSenha = "1234",
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = 41 //Paraná
            };

            var consultaResultadoLote = new ServicoGNRe.ConsultaResultadoLote(xml, configuracao);
            consultaResultadoLote.Executar();

            switch (consultaResultadoLote.Result.SituacaoProcess.Codigo)
            {
                case "400": //Lote recebido, aguardando processamento
                case "401": //Lote em processamento
                    //Tentar consultar mais tarde
                    break;

                case "402": //Lote processado com sucesso.
                    consultaResultadoLote.GravarXmlRetorno(@"d:\testenfe", xml.NumeroRecibo + "-ret-gnre.xml");
                    consultaResultadoLote.GravarPDFGuia(@"d:\testenfe", "GuiaGNRE.pdf");
                    break;

                default:
                    //Rejeições diversas
                    break;
            }
        }

        private void BtnTratamentoExcecao_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.ConsStatServ
            {
                Versao = "3.00", //Mudar de 4.00 para 3.00
                CUF = UFBrasil.PR,
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            try
            {
                var statusServico = new ServicoNFe.StatusServico(xml, configuracao);
                statusServico.Executar();

                MessageBox.Show(statusServico.Result.CStat + " " + statusServico.Result.XMotivo);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
        }

        private void BtnTratamentoExcecao2_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFe.EnviNFe
            {
                Versao = "3.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = new List<XmlNFe.NFe>
                {
                    new XmlNFe.NFe
                    {
                        InfNFe = new List<XmlNFe.InfNFe>
                        {
                            new XmlNFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new XmlNFe.Ide
                                {
                                    CUF = UFBrasil.MG,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57990,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new XmlNFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new XmlNFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new XmlNFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new XmlNFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new XmlNFe.Total
                                {
                                    ICMSTot = new XmlNFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 84.90,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 84.90,
                                        VTotTrib = 12.63
                                    }
                                },
                                Transp = new XmlNFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<XmlNFe.Vol>
                                    {
                                        new XmlNFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new XmlNFe.Cobr()
                                {
                                    Fat = new XmlNFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 84.90,
                                        VDesc = 0,
                                        VLiq = 84.90
                                    },
                                    Dup = new List<XmlNFe.Dup>
                                    {
                                        new XmlNFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 84.90
                                        }
                                    }
                                },
                                Pag = new XmlNFe.Pag
                                {
                                    DetPag = new List<XmlNFe.DetPag>
                                    {
                                        new XmlNFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 80.90
                                        }
                                    }
                                },
                                InfAdic = new XmlNFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new XmlNFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            try
            {
                var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
                autorizacao.Executar();
            }
            catch (ValidarXMLException ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
            catch (CertificadoDigitalException ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
        }

        private void BtnTestarConexaoInternet_Click(object sender, EventArgs e)
        {
            //Testando a internet sem considerar servidor de proxy
            if (!Unimake.Net.Utility.HasInternetConnection())
            {
                MessageBox.Show("Sem internet!!!");
            }
            else
            {
                MessageBox.Show("Internet ok");
            }

            //Testando a internet considerando que tem um servidor proxy na rede
            var proxy = Unimake.Net.Utility.GetProxy("192.168.1.111", "UserTest", "PassTest", 123); //Passando o IP do servidor de proxy
            if (!Unimake.Net.Utility.HasInternetConnection(proxy))
            {
                MessageBox.Show("Sem internet!!!");
            }
            else
            {
                MessageBox.Show("Internet ok");
            }

            var proxy2 = Unimake.Net.Utility.GetProxy("", "UserTest", "PassTest", 123, true); //Detectando servidor automaticamente
            if (!Unimake.Net.Utility.HasInternetConnection(proxy2))
            {
                MessageBox.Show("Sem internet!!!");
            }
            else
            {
                MessageBox.Show("Internet ok");
            }
        }

        private void BtnGetInfCertificadoDigital_Click(object sender, EventArgs e)
        {
            var certificadoDigital = new CertificadoDigital();

            var certSel = certificadoDigital.AbrirTelaSelecao();

            var infoCertificado = "Data validade inicial: " + certSel.NotBefore + "\r\n\r\n" +
                "Data validade final: " + certSel.NotAfter + "\r\n\r\n" +
                "Sujeito/Pessoa: " + certSel.Subject + "\r\n\r\n" +
                "Emissor: " + certSel.Issuer + "\r\n\r\n" +
                "Serial Number: " + certSel.SerialNumber + "\r\n\r\n" +
                "Thumbprint: " + certSel.Thumbprint;

            MessageBox.Show(infoCertificado);
        }

        private void BtnConsultarGTIN_Click(object sender, EventArgs e)
        {
            var config = new Configuracao
            {
                TipoDFe = TipoDFe.CCG,
                CertificadoDigital = CertificadoSelecionado
            };

            var xml = new XmlCCG.ConsGTIN
            {
                Versao = "1.00",
                GTIN = "7896015516031"
            };

            try
            {
                var ccgConsGTIN = new ServicoCCG.CcgConsGTIN(xml, config);
                ccgConsGTIN.Executar();

                MessageBox.Show(ccgConsGTIN.RetornoWSString);

                switch (ccgConsGTIN.Result.CStat)
                {
                    case 9490: //Consulta realizada com sucesso
                        MessageBox.Show("GTIN:\r\n" + ccgConsGTIN.Result.GTIN + "\r\n\r\n" +
                            "NCM:\r\n" + ccgConsGTIN.Result.NCM + "\r\n\r\n" +
                            "Tipo GTIN:\r\n" + ccgConsGTIN.Result.TpGTIN.ToString() + "\r\n\r\n" +
                            "Descrição Produto:\r\n" + ccgConsGTIN.Result.XProd + "\r\n\r\n" +
                            "Motivo:\r\n" + ccgConsGTIN.Result.XMotivo + "\r\n\r\n" +
                            "CEST 1:\r\n" + (ccgConsGTIN.Result.CEST.Count >= 1 ? ccgConsGTIN.Result.CEST[0] : "SEM CEST") + "\r\n\r\n" +
                            "CEST 2:\r\n" + (ccgConsGTIN.Result.CEST.Count >= 2 ? ccgConsGTIN.Result.CEST[1] : "SEM CEST") + "\r\n\r\n" +
                            "CEST 3:\r\n" + (ccgConsGTIN.Result.CEST.Count >= 3 ? ccgConsGTIN.Result.CEST[2] : "SEM CEST"));
                        break;

                    default:
                        //Consulta foi rejeitada - Realizar tratramento necessário
                        break;
                }
            }
            catch (ValidarXMLException ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
            catch (ValidatorDFeException ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
            catch (CertificadoDigitalException ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.GetLastException().Message);
            }
        }

        private void BtnInsucessoEntregaCTe_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoInsucessoEntrega
                {
                    VersaoEvento = "4.00",
                    DescEvento = "Insucesso na Entrega do CT-e",
                    NProt = "141200000007987",
                    DhTentativaEntrega = DateTime.Now,
                    TpMotivo = TipoMotivoInsucessoEntrega.Outros,
                    XJustMotivo = "Teste da justificativa do motivo da entrega quando é outro = 4",
                    Latitude = "37.774929",
                    Longitude = "122.419418",
                    HashTentativaEntrega = "noauBnfaoS02PYxVm8ufox7OKww=",
                    DhHashTentativaEntrega = DateTime.Now,
                    InfEntrega = new List<XmlCTe.InfEntrega>
                    {
                        new XmlCTe.InfEntrega
                        {
                            ChNFe = "12345678901234567890123456789012345678901234"
                        }
                    }
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200210859283000185570010000005671227070615",
                    CNPJ = "10859283000185",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.InsucessoEntrega,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnCancInsucessoEntregaCTe_Click(object sender, EventArgs e)
        {
            var xml = new XmlCTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new XmlCTe.DetEventoCancelamentoInsucessoEntrega
                {
                    VersaoEvento = "4.00",
                    DescEvento = "Cancelamento do Insucesso de Entrega do CT-e",
                    NProt = "141200000007987",
                    NProtIE = "141200000007982"
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200210859283000185570010000005671227070615",
                    CNPJ = "10859283000185",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.CancelamentoInsucessoEntrega,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }

        }

        private void BtnDesserializandoCTeOS_Click(object sender, EventArgs e)
        {
            var doc = new XmlDocument();
            doc.Load(@"C:\projetos\uninfe\exemplos\CTe 4.00\CTeOS\35170799999999999999670000000000261309301440-cte.xml");
            var xml = XMLUtility.Deserializar<XmlCTeOS.CTeOS>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacao = new ServicoCTeOS.Autorizacao(xml, configuracao);
            autorizacao.Executar();

            if (autorizacao.Result.ProtCTe != null)
            {
                if (autorizacao.Result.CStat == 103) //103 = Lote Recebido com sucesso
                {
                    if (autorizacao.Result.ProtCTe.InfProt.CStat == 100) //Autorizado
                    {
                        autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");
                    }
                }
            }
        }

        private void BtnImprimirCCe_Click(object sender, EventArgs e)
        {
            var doc = new XmlDocument();
            doc.Load(@"D:\testenfe\35240110654122000155550010000085161700218900_110110_01-proceventonfe.xml");

            var config = new UnidanfeConfiguration
            {
                Arquivo = doc.OuterXml,
                Copias = 1,
                Visualizar = true,
                Imprimir = false,
                PastaPDF = @"d:\testenfe\pdf",
                NomePDF = "35240110654122000155550010000085161700218900_110110_01-proceventonfe.pdf",
                WaitProcess = false,
                NFe = @"D:\testenfe\41240106117473000150550300000000111905645773-procnfe.xml"
            };
            UnidanfeServices.Execute(config);
        }

        private void BtnImprimirSAT_Click(object sender, EventArgs e)
        {
            var doc = new XmlDocument();
            doc.Load(@"C:\Users\Wandrey\Downloads\Telegram Desktop\35240111111111111111599000001020007901053255-sat.xml");

            var config = new UnidanfeConfiguration
            {
                Arquivo = doc.OuterXml,
                Copias = 1,
                Visualizar = false,
                Imprimir = true,
                //WaitProcess = false
            };

            UnidanfeServices.Execute(config);
        }

        private void BtnEventoCTeEmDesacordo_Click(object sender, EventArgs e)
        {
            var xml = new Unimake.Business.DFe.Xml.CTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new XmlCTe.InfEvento(new Unimake.Business.DFe.Xml.CTe.DetEventoPrestDesacordo
                {
                    VersaoEvento = "4.00",
                    IndDesacordoOper = "1",
                    XObs = "Observação do desacordo"
                })
                {
                    COrgao = UFBrasil.PR,
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.PrestDesacordo,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnDistribuicaoDFeChNFe_Click(object sender, EventArgs e)
        {
            var chaveNFe = "41240276416890000189558950047230601397793171";

            #region Se a nota não tiver sido manifestada, pelo menos ciência, manifestar para conseguir fazer o download

            var xmlManif = new Unimake.Business.DFe.Xml.NFe.EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Unimake.Business.DFe.Xml.NFe.Evento> {
                    new Unimake.Business.DFe.Xml.NFe.Evento
                    {
                        Versao = "1.00",
                        InfEvento = new Unimake.Business.DFe.Xml.NFe.InfEvento(new Unimake.Business.DFe.Xml.NFe.DetEventoManif
                        {
                            Versao = "1.00",
                            DescEvento = "Ciencia da Operacao",
                        })
                        {
                            COrgao = UFBrasil.AN,
                            ChNFe = chaveNFe,
                            CNPJ = "06117473000150",
                            DhEvento = DateTime.Now,
                            TpEvento = TipoEventoNFe.ManifestacaoCienciaOperacao,
                            NSeqEvento = 1,
                            VerEvento = "1.00",
                            TpAmb = TipoAmbiente.Producao
                        }
                    }
                }
            };

            var configManifestacao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoArquivo = @"d:\projetos\unimake_pv.pfx",
                CertificadoSenha = "12345678"
            };

            var recepcaoEvento = new Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(xmlManif, configManifestacao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição do evento
            if (recepcaoEvento.Result.CStat == 128) //128 = Lote de evento processado com sucesso
            {
                switch (recepcaoEvento.Result.RetEvento[0].InfEvento.CStat)
                {
                    case 135: //Evento homologado com vinculação da respectiva NFe
                        recepcaoEvento.GravarXmlDistribuicao(@"c:\testenfe\");
                        break;

                    default: //Evento rejeitado
                        //Executar as ações necessárias
                        break;
                }
            }

            #endregion

            #region Fazer a consulta e o download, depois da nota já manifestada

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoArquivo = @"d:\projetos\unimake_pv.pfx",
                CertificadoSenha = "12345678"
            };

            var xmlConsulta = new XmlNFe.DistDFeInt
            {
                Versao = "1.01",
                TpAmb = TipoAmbiente.Producao,
                CNPJ = "06117473000150",
                CUFAutor = UFBrasil.PR,
                ConsChNFe = new ConsChNFe
                {
                    ChNFe = chaveNFe
                }
            };

            var distribuicaoDFe = new ServicoNFe.DistribuicaoDFe(xmlConsulta, configuracao);
            distribuicaoDFe.Executar();

            if (distribuicaoDFe.Result.CStat == 138) // Documentos localizados e 137 = Não tem documentos
            {
                var folder = @"c:\testenfe\doczip";

                //Salvar XML o XML em uma pasta
                distribuicaoDFe.GravarXMLDocZIP(folder, true);

                //String do XML Baixado, só salvar no banco de dados
                var xmlBaixado = distribuicaoDFe.ProcNFes[0].GerarXML().OuterXml;
            }

            #endregion
        }

        private void BtnEnviarReinf_Click(object sender, EventArgs e)
        {
            var xmlEnvio = new Unimake.Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos
            {
                Versao = "1.00.00",
                EnvioLoteEventos = new XmlEFDReinf.EnvioLoteEventosReinf
                {
                    IdeContribuinte = new XmlEFDReinf.IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678901234"
                    },
                    Eventos = new XmlEFDReinf.EventosReinf
                    {
                        Evento = new List<XmlEFDReinf.EventoReinf>
                        {
                            new XmlEFDReinf.EventoReinf
                            {
                                ID = "ID1000000000000002021052608080800001",
                                Reinf1000 = new XmlEFDReinf.Reinf1000
                                {
                                    EvtInfoContri = new XmlEFDReinf.EvtInfoContri
                                    {
                                        ID = "ID1000000000000002021052608080654321",
                                        IdeEvento = new XmlEFDReinf.IdeEvento
                                        {
                                            TpAmb = TipoAmbiente.Homologacao,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "150"
                                        },
                                        IdeContri = new XmlEFDReinf.IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678901234"
                                        },
                                        InfoContri = new XmlEFDReinf.InfoContri
                                        {
                                            Inclusao = new XmlEFDReinf.Inclusao1000
                                            {
                                                IdePeriodo = new XmlEFDReinf.IdePeriodo
                                                {
                                                    IniValid = "2021-05"
                                                },
                                                InfoCadastro = new XmlEFDReinf.InfoCadastro
                                                {
                                                    ClassTrib = ClassificacaoTributaria.PessoaJuridica,
                                                    IndEscrituracao = IndicativoEscrituracao.Obrigada,
                                                    IndDesoneracao = IndicativoDesoneracao.NaoAplicavel,
                                                    IndAcordoIsenMulta = IndicativoIsencaoMulta.ComAcordo,
                                                    IndSitPJ = IndicativoSituacaoPJ.Extincao,
                                                    IndUniao = IndicativoUniao.NaoAplicavel,
                                                    DtTransfFinsLucr = DateTime.Parse("2021-01-01"),
                                                    DtObito = DateTime.Parse("2021-01-01"),
                                                    Contato = new XmlEFDReinf.Contato
                                                    {
                                                        NmCtt = "NMCTT1",
                                                        CpfCtt = "12345678954",
                                                        FoneFixo = "4412347894",
                                                        FoneCel = "44912347894",
                                                        Email = "email@email.com"
                                                    },
                                                    SoftHouse = new List<XmlEFDReinf.SoftHouse>
                                                    {
                                                        new XmlEFDReinf.SoftHouse
                                                        {
                                                            CnpjSoftHouse = "12345678901234",
                                                            NmRazao = "nomeContribuinte",
                                                            NmCont = "nomeContato",
                                                            Telefone = "44123456789",
                                                            Email = "email@email.com"
                                                        }
                                                    },
                                                    InfoEFR = new XmlEFDReinf.InfoEFR
                                                    {
                                                        IdeEFR = SimNaoLetra.Sim,
                                                        CnpjEFR = "12345678901234"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = CertificadoSelecionado
            };

            var recepcionarLoteAssincReinf = new ServicoEFDReinf.RecepcionarLoteAssincrono(xmlEnvio, configuracao);
            recepcionarLoteAssincReinf.Executar();

            if (recepcionarLoteAssincReinf.Result.RetornoLoteEventosAssincrono != null)
            {
                switch (recepcionarLoteAssincReinf.Result.RetornoLoteEventosAssincrono.Status.CdResposta)
                {

                    case 1: //O lote está aguardando processamento.

                        // Guardar o protocolo para ser utilizado na consulta do lote
                        var protocolo = recepcionarLoteAssincReinf.Result.RetornoLoteEventosAssincrono.DadosRecepcaoLote.ProtocoloEnvio;

                        var configuracaoConsulta = new Configuracao
                        {
                            TipoDFe = TipoDFe.EFDReinf,
                            TipoEmissao = TipoEmissao.Normal,
                            TipoAmbiente = TipoAmbiente.Homologacao,
                            Servico = Servico.EFDReinfConsultaLoteAssincrono,
                            CertificadoDigital = CertificadoSelecionado
                        };

                        var xmlConsulta = new XmlEFDReinf.ReinfConsultaLoteAssincrono
                        {
                            Versao = "1.00.00",
                            ConsultaLoteAssincrono = new XmlEFDReinf.ConsultaLoteAssincrono
                            {
                                NumeroProtocolo = protocolo
                            }
                        };

                        var consultaLoteReinf = new ServicoEFDReinf.ConsultaLoteAssincrono(xmlConsulta, configuracaoConsulta);
                        consultaLoteReinf.Executar();

                        switch (consultaLoteReinf.Result.RetornoLoteEventosAssincrono.Status.CdResposta)
                        {
                            case 1: //O lote ainda está aguardando processamento.
                                MessageBox.Show("Aguarde alguns minutos e tente novamente!");
                                break;

                            case 2: //O lote foi processado. Todos os eventos foram processados com sucesso
                            case 3: //O lote foi processado. Possui um ou mais eventos com ocorrências
                                consultaLoteReinf.GravarXmlDistribuicao("C:\\Projetos\\Treinamentos\\C#");
                                break;

                            case 8: //Consulta não executada - Verificar ocorrências.
                                MessageBox.Show(consultaLoteReinf.RetornoWSString);
                                break;
                        }
                        break;

                    case 7: //O lote não foi recebido pois possui ocorrências a serem corrigidas
                        MessageBox.Show(recepcionarLoteAssincReinf.RetornoWSString);
                        break;

                    case 99: //Erro interno na EFD-REINF
                        MessageBox.Show("Erro interno no REINF! Aguarde alguns minutos e tente novamente!");
                        break;

                    default:
                        // Tratamentos necessários
                        break;
                }
            }
        }

        private void BtnEnviarEConsultarESocial_Click(object sender, EventArgs e)
        {
            var xmlEnvio = new XmlESocial.ESocialEnvioLoteEventos
            {
                Versao = "1.00.00",
                EnvioLoteEventos = new XmlESocial.EnvioLoteEventosESocial
                {
                    IdeEmpregador = new XmlESocial.IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678901234"
                    },

                    IdeTransmissor = new XmlESocial.IdeTransmissor
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678901234"
                    },

                    Grupo = "1231231",

                    Eventos = new XmlESocial.EventosESocial
                    {
                        Evento = new List<XmlESocial.EventoESocial>
                        {
                            new XmlESocial.EventoESocial
                            {
                                ESocial1000 = new XmlESocial.ESocial1000
                                {
                                    EvtInfoEmpregador = new XmlESocial.EvtInfoEmpregador
                                    {
                                        ID = "ID1235345346546234",
                                        IdeEvento = new XmlESocial.IdeEvento1000
                                        {
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            TpAmb = TipoAmbiente.Homologacao,
                                            VerProc = "1.00"

                                        },

                                        IdeEmpregador = new XmlESocial.IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678901234"
                                        },

                                        InfoEmpregador = new XmlESocial.InfoEmpregador
                                        {
                                            Inclusao = new XmlESocial.InclusaoE1000
                                            {
                                                InfoCadastro = new XmlESocial.InfoCadastro
                                                {
                                                    ClassTrib = ClassificacaoTributaria.SimplesNacionalTributacaoPrevidenciariaNaoSubstituida,

                                                    IndConstr = IndConstr.NaoConstrutora,

                                                    IndDesFolha = IndDesFolha.EmpresaEnquadrada,



                                                    DadosIsencao = new XmlESocial.DadosIsencao
                                                    {
                                                        NrProtRenov = "NrProtRenov",
                                                        NrCertif = "12312314125",
                                                        IdeMinLei = "123123",
                                                        PagDou = "123123123"

                                                    },
                                                },

                                                IdePeriodo = new XmlESocial.IdePeriodo
                                                {
                                                   FimValidField = "2000-10-05"
                                                }
                                            }
                                        }

                                    }
                                }
                            }
                        }
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.ESocialEnviarLoteEventos
            };

            var envioLoteESocial = new ServicoESocial.EnviarLoteEventosESocial(xmlEnvio, configuracao);
            envioLoteESocial.Executar();

            switch (envioLoteESocial.Result?.RetornoEnvioLoteEventos.Status.CdResposta)
            {
                case 201: //Lote recebido com sucesso
                    envioLoteESocial.GravarXmlDistribuicao("C:\\Projetos\\Treinamentos\\C#", "treinamento eSocial", envioLoteESocial?.RetornoWSString);

                    var protocoloEnvio = envioLoteESocial.Result.RetornoEnvioLoteEventos.DadosRecepcaoLote.ProtocoloEnvio;

                    var xmlConsultaESocial = new ConsultarLoteEventos
                    {
                        ConsultaLoteEventos = new ConsultaLoteEventos
                        {
                            ProtocoloEnvio = protocoloEnvio
                        }
                    };

                    var configuracaoConsulta = new Configuracao
                    {
                        TipoDFe = TipoDFe.ESocial,
                        TipoAmbiente = TipoAmbiente.Homologacao,
                        CertificadoDigital = CertificadoSelecionado,
                        Servico = Servico.ESocialConsultaEvts
                    };

                    var consultaLoteAssincrono = new Unimake.Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono(xmlConsultaESocial, configuracao);
                    consultaLoteAssincrono.Executar();

                    consultaLoteAssincrono.Executar();

                    switch (consultaLoteAssincrono.Result.RetornoProcessamentoLoteEventos.Status.CdResposta)
                    {
                        case 101: // Lote Aguardando Processamento.
                            MessageBox.Show("Aguarde alguns minutos e tente novamente!");
                            break;

                        case 201: // Lote Processado com Sucesso
                        case 202: // Lote Processado com Advertências.
                            consultaLoteAssincrono.GravarXmlDistribuicao("C:\\Projetos\\Treinamentos\\C#", $"{protocoloEnvio}.xml", consultaLoteAssincrono.RetornoWSString);
                            break;

                        case 301: // Erro Servidor eSocial
                            MessageBox.Show("Erro no servidor do eSocial. Aguarde alguns minutos e envie novamente!");
                            break;

                        case 401: // Lote incorreto - Erro preenchimento.
                        case 402: // Lote incorreto - Schema inválido
                        case 403: // Lote incorreto - Versão do SCHEMA não permitida
                        case 404: // Lote incorreto - Erro certificado
                        case 405: // Lote incorreto - Lote nulo ou vazio
                            MessageBox.Show($"Código: {consultaLoteAssincrono.Result.RetornoProcessamentoLoteEventos.Status.CdResposta}");
                            MessageBox.Show($"Descrição: {consultaLoteAssincrono.Result.RetornoProcessamentoLoteEventos.Status.DescResposta}");
                            break;

                        case 501: // Solicitação de Consulta Incorreta - Erro Preenchimento.
                        case 502: // Solicitação de Consulta Incorreta - Schema inválido
                        case 503: // Solicitação de Consulta Incorreta - Versão do SCHEMA não permitida
                        case 504: // Solicitação de Consulta Incorreta - Erro certificado
                        case 505: // Solicitação de Consulta Incorreta - Consulta nula ou 
                            MessageBox.Show($"Código: {consultaLoteAssincrono.Result.RetornoProcessamentoLoteEventos.Status.CdResposta}");
                            MessageBox.Show($"Descrição: {consultaLoteAssincrono.Result.RetornoProcessamentoLoteEventos.Status.DescResposta}");
                            break;

                        default:
                            MessageBox.Show("Erro não catalogado!!");
                            break;
                    }
                    break;
                case 202: // Lote recebido advertências.
                    MessageBox.Show("Lote recebido mas possui advertências, verifique!");
                    MessageBox.Show(envioLoteESocial.RetornoWSString);
                    break;

                case 203: // Lote recebido já foi recebido anteriormente e ainda está na fila de processamento.
                    MessageBox.Show("Lote recebido já foi recebido anteriormente e ainda está sendo processado");
                    break;

                case 301: // Erro no servidor do eSocial
                    MessageBox.Show("Erro no servidor do eSocial. Aguarde alguns minutos e envie novamente!");
                    break;

                case 401: // Lote incorreto - Erro preenchimento.
                case 402: // Lote incorreto - Schema inválido
                case 403: // Lote incorreto - Versão do SCHEMA não permitida
                case 404: // Lote incorreto - Erro certificado
                case 405: // Lote incorreto - Lote nulo ou vazio
                case 406: // Lote incorreto - Totalização em andamento
                case 407: // Lote incorreto - Empregador não autorizado a enviar
                    MessageBox.Show($"Código: {envioLoteESocial.Result.RetornoEnvioLoteEventos.Status.CdResposta}");
                    MessageBox.Show($"Descrição: {envioLoteESocial.Result.RetornoEnvioLoteEventos.Status.DescResposta}");
                    break;

                default:
                    MessageBox.Show("Erro não catalogado!!");
                    break;
            }

        }

        private void BtnDesserializarNFeB2B_Click(object sender, EventArgs e)
        {
            var directoryInfo = new DirectoryInfo(@"C:\Users\Wandrey\Downloads\NFeTeste\NFe");
            var files = directoryInfo.GetFiles("*.xml");
            var naoIdentificou = 0;

            foreach (var file in files)
            {
                var nfeProc = new NfeProc();

                var doc = new XmlDocument();
                doc.Load(file.FullName);

                switch (doc.LastChild.Name.ToLower())
                {
                    case "nfeproc":
                        nfeProc = XMLUtility.Deserializar<NfeProc>(doc.OuterXml);
                        break;

                    case "nfe":
                        nfeProc.NFe = XMLUtility.Deserializar<NFe>(doc.OuterXml);
                        nfeProc.ProtNFe = null;
                        break;

                    default:
                        //Não foi possível identificar o tipo do XML
                        naoIdentificou++;
                        continue;
                }

                if (nfeProc.NFe.InfNFe.Count > 0)
                {
                    if (nfeProc.NFe.InfNFe[0].Det.Count > 0)
                    {
                        foreach (var det in nfeProc.NFe.InfNFe[0].Det)
                        {
                            var descricaoProduto = det.Prod.XProd;
                        }
                    }
                }
            }

            MessageBox.Show($"Processamento encerrado! (Não identificou = {naoIdentificou} de {files.Length})");
        }

        private void BtnEventoEPECCTe_Click(object sender, EventArgs e)
        {
            var xml = new Unimake.Business.DFe.Xml.CTe.EventoCTe
            {
                Versao = "4.00",
                InfEvento = new Unimake.Business.DFe.Xml.CTe.InfEvento(new Unimake.Business.DFe.Xml.CTe.DetEventoEPEC
                {
                    VersaoEvento = "4.00",
                    XJust = "Teste de EPEC do CTE para ver se tudo está funcionando",
                    VICMS = 100,
                    VICMSST = 100,
                    VTPrest = 1000,
                    VCarga = 1000,
                    Toma4 = new Unimake.Business.DFe.Xml.CTe.EvEPECCTeToma4
                    {
                        UF = UFBrasil.PR,
                        CNPJ = "06117473000150",
                        IE = "1234567890"
                    },
                    Modal = ModalidadeTransporteCTe.Rodoviario,
                    UFIni = UFBrasil.PR,
                    UFFim = UFBrasil.PR,
                    TpCTe = TipoCTe.Normal,
                    DhEmi = DateTime.Now
                })
                {
                    COrgao = UFBrasil.RS, //Paraná quem autoriza EPEC é SVRS, então tem que ser o código do estado do Rio Grande do Sul. Se fosse SVSP a atender o PR teria que ser UFBRasil.SP.
                    ChCTe = "41200211111111111111111111111111111111111115",
                    CNPJ = "11111111111111",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.EPEC,
                    NSeqEvento = 1,
                    TpAmb = TipoAmbiente.Homologacao
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado,
                TipoEmissao = TipoEmissao.ContingenciaEPEC //EPEC o tipo de emissão tem que ser Contingência EPEC
            };

            var recepcaoEvento = new ServicoCTe.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            //Gravar o XML de distribuição se o evento foi homologada
            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
                case 135: //Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
                case 136: //Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
                    recepcaoEvento.GravarXmlDistribuicao(@"c:\testecte\");
                    break;

                default:
                    //Quando o evento é rejeitado pela Sefaz.
                    break;
            }
        }

        private void BtnAbrirTeleConfigUnidanfe_Click(object sender, EventArgs e) =>
            //Abrir a tela de configuração do UNIDANFE
            UnidanfeServices.ShowConfigurationScreen(@"d:\testenfe\unidanfe");

        private void BtnAbrirTelaLicencaUnidanfe_Click(object sender, EventArgs e) =>
            //Abrir a tela de licenças do UNIDANFE:
            UnidanfeServices.ShowLicencaScreen(@"d:\testenfe\unidanfe");

        private void BtnConsultaSituacaoCTe_Click(object sender, EventArgs e)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = CertificadoSelecionado,
                TipoEmissao = TipoEmissao.Normal
            };

            var consSitCTe = new ConsSitCTe
            {
                TpAmb = TipoAmbiente.Producao,
                ChCTe = "11111111111111111111111111111111111111111111",
                Versao = "4.00"
            };

            var consultaProtocolo = new ServicoCTe.ConsultaProtocolo(consSitCTe, configuracao);
            consultaProtocolo.Executar();

            var qq = consultaProtocolo.RetornoWSString;
        }

        private void BtnEnviarNFSeNacional_Click(object sender, EventArgs e)
        {
            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"D:\projetos\uninfe\exemplos\NFSe\NACIONAL\1.00\GerarNFSeEnvio-env-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.00",
                CodigoMunicipio = 1001058,
                PadraoNFSe = PadraoNFSe.NACIONAL
            };

            var gerarNFSe = new ServicoNFSe.GerarNfse(conteudoXML, configuracao);
            gerarNFSe.Executar();

            MessageBox.Show(gerarNFSe.RetornoWSString);
        }

        private void BtnCancelarNFSeNacional_Click(object sender, EventArgs e)
        {
            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"d:\testenfe\nacional\cancelarNFSe.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeCancelarNfse,
                SchemaVersao = "1.00",
                CodigoMunicipio = 1001058,
                PadraoNFSe = PadraoNFSe.NACIONAL
            };

            var cancelarNFSe = new ServicoNFSe.CancelarNfse(conteudoXML, configuracao);
            cancelarNFSe.Executar();

            MessageBox.Show(cancelarNFSe.RetornoWSString);
        }

        private void BtnConsultarNFSeRps_Click(object sender, EventArgs e)
        {
            // Consulta pela chave do DPS enviado
            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"d:\testenfe\nacional\consultarNFSePorRps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeConsultarNfsePorRps,
                SchemaVersao = "1.00",
                CodigoMunicipio = 1001058,
                PadraoNFSe = PadraoNFSe.NACIONAL
            };

            var consultarNfsePorRps = new ServicoNFSe.ConsultarNfsePorRps(conteudoXML, configuracao);
            consultarNfsePorRps.Executar();

            MessageBox.Show(consultarNfsePorRps.RetornoWSString);
        }

        private void BtnConsultarNFSe_Click(object sender, EventArgs e)
        {
            // Consulta pela chave da NFSe enviado
            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"d:\testenfe\nacional\consultarNFSe.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeConsultarNfse,
                SchemaVersao = "1.00",
                CodigoMunicipio = 1001058,
                PadraoNFSe = PadraoNFSe.NACIONAL
            };

            var consultarNFSe = new ServicoNFSe.ConsultarNfse(conteudoXML, configuracao);
            consultarNFSe.Executar();

            MessageBox.Show(consultarNFSe.RetornoWSString);
        }

        private void BtnConsultarPdfNFSe_Click(object sender, EventArgs e)
        {
            // Consulta do PDF da NFSe pela chave
            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"d:\testenfe\nacional\consultarPdfNFSe.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado,
                Servico = Servico.NFSeConsultarNfsePDF,
                SchemaVersao = "1.00",
                CodigoMunicipio = 1001058,
                PadraoNFSe = PadraoNFSe.NACIONAL
            };

            var consultarNfsePDF = new ServicoNFSe.ConsultarNfsePDF(conteudoXML, configuracao);
            consultarNfsePDF.Executar();

            MessageBox.Show(consultarNfsePDF.RetornoWSString);

            // Vou chamar o método para extrair o PDF do retorno
            // Parâmetros:
            // - Nome da pasta
            // - Nome do arquivo PDF
            // - Nome da tag para extração do PDF (para o NACIONAL, sempre será Base64Pdf)
            consultarNfsePDF.ExtrairPDF(@"c:\projetos\teste\nacional\pdf", "NFSe emitida dia XX/XX/XX", "Base64Pdf");
        }

        private void BtnConsultaStatusNFCom_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFCom.ConsStatServNFCom
            {
                TpAmb = TipoAmbiente.Homologacao,
                Versao = "1.00"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                CertificadoDigital = CertificadoSelecionado,
                CodigoUF = (int)UFBrasil.PR
            };

            var statusServico = new ServicoNFCom.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.RetornoWSString);
        }

        private void BtnEnviarNFComSincrono_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFCom.NFCom
            {
                InfNFCom = new XmlNFCom.InfNFCom
                {
                    Versao = "1.00",
                    Ide = new XmlNFCom.Ide
                    {
                        CUF = UFBrasil.PR,
                        TpAmb = TipoAmbiente.Homologacao,
                        Mod = ModeloDFe.NFCom,
                        Serie = 1,
                        NNF = 123,
                        CNF = "1234567",
                        DhEmi = System.DateTime.Now,
                        TpEmis = TipoEmissao.Normal,
                        NSiteAutoriz = "0",
                        CMunFG = "1234567",
                        FinNFCom = FinalidadeNFCom.Normal,
                        TpFat = TipoFaturamentoNFCom.FaturamentoNormal,
                        VerProc = "teste 1.0",
                        IndPrePago = IndicadorServicoPrePago.ServicoPrePago,
                        IndCessaoMeiosRede = IndicadorCessaoMeiosDeRede.IndicadorCessaoMeioDeRede,
                        IndNotaEntrada = IndicadorNotaEntrada.IndicaNotaEntradaAjuste
                    },
                    Emit = new XmlNFCom.Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "12345678",
                        CRT = CRT.SimplesNacional,
                        XNome = "Unimake Solucoes Corporativas",
                        XFant = "Unimake Software",
                        EnderEmit = new XmlNFCom.EnderEmit
                        {
                            XLgr = "Rua",
                            Nro = "11",
                            XCpl = "Fundos",
                            XBairro = "Vila Maria",
                            CMun = "1234567",
                            XMun = "Paranavai",
                            CEP = "12345678",
                            UF = UFBrasil.PR,
                            Fone = "12345678",
                            Email = "teste@test.com"
                        }
                    },
                    Dest = new XmlNFCom.Dest
                    {
                        XNome = "Unifake Software",
                        CNPJ = "06117473000150",
                        IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                        IE = "12345678",
                        EnderDest = new XmlNFCom.EnderDest
                        {
                            XLgr = "Rua",
                            Nro = "11",
                            XCpl = "Fundos",
                            XBairro = "Vila Maria",
                            CMun = "1234567",
                            XMun = "Paranavai",
                            CEP = "12345678",
                            UF = UFBrasil.PR,
                            CPais = "1058",
                            XPais = "BRASIL",
                            Fone = "12345678",
                            Email = "teste@test.com"
                        }
                    },
                    Assinante = new XmlNFCom.Assinante
                    {
                        ICodAssinante = "1",
                        TpAssinante = TipoAssinante.ProdutorRural,
                        TpServUtil = TipoServicoUtilizado.Outros,
                        NContrato = "123",
                        DContratoIni = System.DateTime.Today,
                        DContratoFim = System.DateTime.Today,
                        NroTermPrinc = "1234567",
                        CUFPrinc = UFBrasil.PR
                    },
                    GSub = new XmlNFCom.GSub
                    {
                        ChNFCom = "12345678901234567890123456789012345678901234",
                        MotSub = MotivoSubstituicaoNFCom.DecisaoJudicial
                    },
                    GCofat = new XmlNFCom.GCofat
                    {
                        ChNFComLocal = "12345678901234567890123456789012345678901234"
                    },
                    Det = new System.Collections.Generic.List<XmlNFCom.Det>
                    {
                        new XmlNFCom.Det
                        {
                            NItem = "1",
                            Prod = new XmlNFCom.Prod
                            {
                                CProd = "1",
                                XProd = "teste",
                                CClass = "1234567",
                                CFOP = "5120",
                                CNPJLD = "06117473000150",
                                UMed = UnidadeBasicaMedida.UN,
                                QFaturada = 1.12,
                                VItem = 1.123M,
                                VDesc = 1.11,
                                VOutro = 1.11,
                                VProd = 1234.10M,
                                DExpiracao = System.DateTime.Today,
                                IndDevolucao = IndicadorDevolucao.DevolucaoValorItem
                            },
                            Imposto = new XmlNFCom.Imposto
                            {
                                ICMS00 = new XmlNFCom.ICMS00NFCom
                                {
                                    CST = "00",
                                    VBC = 1.11,
                                    PICMS = 1.15,
                                    VICMS = 1.17,
                                    PFCP = 1.190,
                                    VFCP = 111.47
                                },
                                ICMSUFDest = new System.Collections.Generic.List<XmlNFCom.ICMSUFDest>
                                {
                                    new XmlNFCom.ICMSUFDest
                                    {
                                        CUFDest = UFBrasil.PR,
                                        VBCUFDest = 158.55,
                                        PFCPUFDest = 158.55,
                                        PICMSUFDest = 158.55,
                                        VFCPUFDest = 158.55,
                                        VICMSUFDest = 158.55,
                                        VICMSUFEmi = 158.55,
                                        CBenefUFDest = "11"
                                    }
                                },
                                PIS = new XmlNFCom.PISNFCom
                                {
                                    CST = CSTPisCofins.OperacaoComSuspensao,
                                    VBC = 1587.45,
                                    PPIS = 123.4500,
                                    VPIS = 1587.45
                                },
                                COFINS = new XmlNFCom.COFINSNFCom
                                {
                                    CST = CSTPisCofins.AliquotaBasica,
                                    VBC = 11.98,
                                    PCOFINS = 11.9800,
                                    VCOFINS = 11.98
                                },
                                FUST = new XmlNFCom.FUST
                                {
                                    VBC = 1879.88,
                                    PFUST = 132.88,
                                    VFUST = 1879.88
                                },
                                FUNTTEL = new XmlNFCom.FUNTTEL
                                {
                                    VBC = 1.47,
                                    PFUNTTEL = 1.4700,
                                    VFUNTTEL = 1.47
                                },
                                RetTribNFCom = new XmlNFCom.RetTribNFCom
                                {
                                    VRetPIS = 1444.85M,
                                    VRetCofins = 1444.85M,
                                    VRetCSLL = 1444.85M,
                                    VBCIRRF = 1444.85M,
                                    VIRRF = 1444.85M
                                }
                            },
                            GProcRef = new XmlNFCom.GProcRef
                            {
                                VItem = 123.48M,
                                QFaturada = 123.4800,
                                VProd = 123.48M,
                                VDesc = 123.48,
                                VOutro = 123.48,
                                IndDevolucao = IndicadorDevolucao.DevolucaoValorItem,
                                VBC = 123.48,
                                PICMS = 123.48,
                                VICMS = 123.48,
                                VPIS = 123.48,
                                VCOFINS = 123.48,
                                VFCP = 123.48,
                                GProc = new System.Collections.Generic.List<XmlNFCom.GProc>
                                {
                                    new XmlNFCom.GProc
                                    {
                                        TpProc = TipoProcessoNF3eNFCom.JusticaFederal,
                                        NProcesso = "12345678"
                                    }
                                }
                            },
                            GRessarc = new XmlNFCom.GRessarc
                            {
                                TpRessarc = TipoRessarcimento.CobrancaIndevida,
                                DRef = System.DateTime.Today,
                                NProcesso = "11222",
                                NProtReclama = "1111",
                                XObs = "Teste total da NFCom"
                            }
                        }
                    },
                    Total = new XmlNFCom.Total
                    {
                        VProd = 111.54,
                        ICMSTot = new XmlNFCom.ICMSTot
                        {
                            VBC = 111.54,
                            VICMS = 111.54,
                            VICMSDeson = 111.54,
                            VFCP = 111.54
                        },
                        VCOFINS = 111.54,
                        VPIS = 111.54,
                        VFUNTTEL = 111.54,
                        VFUST = 111.54,
                        VRetTribTot = new XmlNFCom.VRetTribTotNFCom
                        {
                            VRetPIS = 111.54,
                            VRetCofins = 111.54,
                            VRetCSLL = 111.54,
                            VIRRF = 111.54
                        },
                        VDesc = 111.54,
                        VOutro = 111.54,
                        VNF = 111.54
                    },
                    AutXML = new System.Collections.Generic.List<XmlNFCom.AutXMLNFCom>
                    {
                        new XmlNFCom.AutXMLNFCom
                        {
                            CNPJ = "06117473000150"
                        }
                    },
                    InfAdic = new XmlNFCom.InfAdicNFCom
                    {
                        InfAdFisco = "teste total da NFCom",
                        InfCpl = new System.Collections.Generic.List<string>
                        {
                            "Informacao 1",
                            "Informacao 2"
                        }
                    },
                    GRespTec = new XmlNFCom.GRespTecNFCom
                    {
                        CNPJ = "06117473000150",
                        XContato = "Fulano de tal",
                        Email = "email_fulando@gmail.com",
                        Fone = "12345678",
                        IdCSRT = "123",
                        HashCSRT = "oBbYbxIbKXRZhoJ2zEzYy458+YU="
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)UFBrasil.PR,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacaoSincNFCom = new ServicoNFCom.AutorizacaoSinc(xml, configuracao);
            autorizacaoSincNFCom.Executar();

            MessageBox.Show(autorizacaoSincNFCom.RetornoWSString);

            if (autorizacaoSincNFCom.Result.CStat == 100)
            {
                if (autorizacaoSincNFCom.Result.ProtNFCom.InfProt.CStat == 100)
                {
                    MessageBox.Show(autorizacaoSincNFCom.Result.ProtNFCom.InfProt.NProt);
                    var teste = autorizacaoSincNFCom.NFComProcResults[xml.InfNFCom.Chave].GerarXML();
                }
            }

        }

        private void BtnConsultaSituacaoNFCom_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFCom.ConsSitNFCom
            {
                ChNFCom = "12345678901234567890123456789012345678901234",
                TpAmb = TipoAmbiente.Homologacao,
                Versao = "1.00"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                CodigoUF = (int)UFBrasil.PR,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaProtocolo = new ServicoNFCom.ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            MessageBox.Show(consultaProtocolo.RetornoWSString);
        }

        private void BtnEnviarEventoCancelamentoNFCom_Click(object sender, EventArgs e)
        {
            var xml = new XmlNFCom.EventoNFCom
            {
                Versao = "1.00",
                InfEvento =
                new XmlNFCom.InfEvento(new XmlNFCom.DetEventoCanc
                {
                    VersaoEvento = "1.00",
                    DescEvento = "Cancelamento",
                    NProt = "1234567890123456",
                    XJust = "Erro na criacao do produto"
                })
                {
                    COrgao = UFBrasil.PR,
                    TpAmb = TipoAmbiente.Homologacao,
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
                CertificadoDigital = CertificadoSelecionado,
            };

            var recepcaoEvento = new ServicoNFCom.RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            MessageBox.Show(recepcaoEvento.RetornoWSString);
            MessageBox.Show(recepcaoEvento.Result.InfEvento.CStat + " - " + recepcaoEvento.Result.InfEvento.XMotivo);

            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: // Recebido pelo Sistema de Registro de Eventos, com vinculação do evento na NFCom com situação diferente de Autorizada
                case 135: // Recebido pelo Sistema de Registro de Eventos, com vinculação do evento na respectiva NFCom
                case 136: // Recebido pelo Sistema de Registro de Eventos – vinculação do evento à respectiva NFCom prejudicado
                    recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                    break;

                default:
                    // Evento rejeitado, fazer os devidos tratamentos.
                    break;
            }
        }

        private void BtnConsultaStatusNF3e_Click(object sender, EventArgs e)
        {
            var xml = new XmlNF3e.ConsStatServNF3e
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                CertificadoDigital = CertificadoSelecionado,
                CodigoUF = (int)UFBrasil.PR
            };

            var statusServico = new ServicoNF3e.StatusServico(xml, configuracao);
            statusServico.Executar();

            MessageBox.Show(statusServico.RetornoWSString);
        }

        private void BtnEnviarNF3eSinc_Click(object sender, EventArgs e)
        {
            var xml = new XmlNF3e.NF3e
            {
                InfNF3e = new InfNF3e
                {
                    Versao = "1.00",
                    Ide = new XmlNF3e.Ide
                    {
                        CUF = UFBrasil.PR,
                        TpAmb = TipoAmbiente.Homologacao,
                        Mod = ModeloDFe.NF3e,
                        Serie = 123,
                        NNF = 1,
                        CNF = "1489657",
                        DhEmi = DateTime.Now,
                        TpEmis = TipoEmissao.Normal,
                        NSiteAutoriz = "0",
                        CMunFG = "1234567",
                        FinNF3e = FinalidadeNF3e.Normal,
                        VerProc = "verProc1"
                    },
                    Emit = new XmlNF3e.Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "9032000301",
                        XNome = "Unimake Solucoes Corporativas",
                        XFant = "Unimake Software",
                        EnderEmit = new XmlNF3e.EnderEmit
                        {
                            XLgr = "Rua Paulo Antonio da Costa",
                            Nro = "575",
                            XBairro = "Jardim Simara",
                            CMun = "4118402",
                            XMun = "Paranavai",
                            CEP = "87777000",
                            UF = UFBrasil.PR,
                            Email = "teste@teste.com.br"
                        }
                    },
                    Dest = new XmlNF3e.Dest
                    {
                        XNome = "Empresa Teste",
                        CNPJ = "12345678901234",
                        IndIEDest = IndicadorIEDestinatario.NaoContribuinte,
                        IE = "9876543",
                        EnderDest = new XmlNF3e.EnderDest
                        {
                            XLgr = "Rua da Silva",
                            Nro = "1",
                            XBairro = "Jardim Ipe",
                            CMun = "1234567",
                            XMun = "Outro",
                            CEP = "87777001",
                            UF = UFBrasil.PR,
                            Email = "1teste1@1teste1.com.br"
                        }
                    },
                    Acessante = new Acessante
                    {
                        IdAcesso = "1",
                        TpAcesso = TipoAcessante.Cativo,
                        XNomeUC = "nome aleatorio",
                        TpClasse = TipoClasseConsumidora.ConsumoProprio,
                        TpSubClasse = TipoSubClasseConsumidora.Residencial,
                        TpFase = TipoLigacao.Monofasico,
                        TpGrpTensao = GrupoSubGrupoTensao.A1AltaTensao230kVMais,
                        TpModTar = ModalidadeTarifaria.HorariaBranca,
                        LatGPS = "45.123456",
                        LongGPS = "12.345678",
                        CodRoteiroLeitura = "85589"
                    },
                    GJudic = new GJudic
                    {
                        ChNF3e = "12345678901234567890123456789012345678901234"
                    },
                    GMed = new List<GMed>
                    {
                        new GMed
                        {
                            NMed = "01",
                            IdMedidor = "AB1234",
                            DMedAnt = DateTime.Now,
                            DMedAtu = DateTime.Now,
                        },
                        new GMed
                        {
                            NMed = "02",
                            IdMedidor = "DC98765",
                            DMedAnt = DateTime.Now,
                            DMedAtu = DateTime.Now,
                        }
                    },
                    GSCEE = new GSCEE
                    {
                        TpPartComp = TipoParticipacaoCompensacao.AutoconsumoRemoto,
                        GConsumidor = new List<GConsumidor>
                        {
                            new GConsumidor
                            {
                                IdAcessGer = "AB1234",
                                VPotInst = 12789.014,
                                TpFonteEnergia = TipoFonteEnergia.Hidraulica,
                                EnerAloc = 14.19,
                                TpPosTar = TipoPostoTarifario.Unico
                            },
                            new GConsumidor
                            {
                                IdAcessGer = "FVD9785",
                                VPotInst = 174.17,
                                TpFonteEnergia = TipoFonteEnergia.Eolica,
                                EnerAloc = 189.256,
                                TpPosTar = TipoPostoTarifario.ForaPonta
                            }
                        },
                        GSaldoCred = new List<GSaldoCred>
                        {
                            new GSaldoCred
                            {
                                TpPosTar = TipoPostoTarifario.Ponta,
                                VSaldAnt = 123.14,
                                VCredExpirado = 14.589,
                                VSaldAtual = 1.478
                            }
                        }
                    },
                    NFdet = new List<NFdet>
                    {
                        new NFdet
                        {
                            Det = new List<XmlNF3e.Det>
                            {
                                new XmlNF3e.Det
                                {
                                    NItem = "1",
                                    DetItemAnt = new DetItemAnt
                                    {
                                        NItemAnt = "12",
                                        VItem = 123456.12M,
                                        QFaturada = 123.4,
                                        VProd = 1.02M,
                                        CClass = "1234567",
                                        VBC = 1.34,
                                        PICMS = 2.3,
                                        VFCP = 2.44,
                                        VBCST = 3.45,
                                        VICMSST = 4.55,
                                        VFCPST = 2.33,
                                        VPIS = 2.3,
                                        VPISEfet = 3.4,
                                        VCOFINS = 3.22,
                                        VCOFINSEfet = 4.5,
                                        RetTribNF3e = new RetTribNF3e
                                        {
                                            VRetPIS = 123.45M,
                                            VRetCofins = 44.42M,
                                            VRetCSLL = 33M,
                                            VIRRF = 33.42M
                                        },
                                        IndDevolucao = SimNao.Sim
                                    }
                                },
                                new XmlNF3e.Det
                                {
                                    NItem = "2",
                                    DetItem = new DetItem
                                    {
                                        NItemAnt = "1",
                                        GTarif = new GTarif
                                        {
                                            DIniTarif = DateTime.Now,
                                            DFimTarif = DateTime.Now,
                                            TpAto = TipoAto.Despacho,
                                            NAto = "1342",
                                            AnoAto = "2024",
                                            TpTarif = TipoTarifa.TUSD,
                                            CPosTarif = TipoPostoTarifario.Intermediario,
                                            UMed = UnidadeMedidaEnergia.KW,
                                            VTarifHom = 0
                                        },
                                        GAdBand = new GAdBand
                                        {
                                            DIniAdBand = DateTime.Now,
                                            DFimAdBand = DateTime.Now,
                                            TpBand = TipoBandeira.EscassezHidrica,
                                            VAdBand = 33.44M,
                                            VAdBandAplic = 2.11,
                                            MotDifBand = MotivoTarifaDiferente.DescontoTarifario
                                        },
                                        Prod = new XmlNF3e.Prod
                                        {
                                            IndOrigemQtd = IndicadorOrigemQuantidadeFaturada.Medido,
                                            GMedicao = new GMedicao
                                            {
                                                NMed = "02",
                                                NContrat = "03",
                                                GMedida = new GMedida
                                                {
                                                    TpGrMed = TipoGrandezaMedida.Demanda,
                                                    CPosTarif = TipoPostoTarifario.ForaPonta,
                                                    UMed = UnidadeMedidaEnergia.KVArh,
                                                    VMedAnt = 23.33,
                                                    VMedAtu = 34.3,
                                                    VConst = 333,
                                                    VMed = 44,
                                                    PPerdaTran = 22.344,
                                                    VMedPerdaTran = 33,
                                                    VMedPerdaTec = 3455
                                                }
                                            },
                                            CProd = "12345",
                                            XProd = "produto teste",
                                            CClass = "1234567",
                                            CFOP = "1234",
                                            UMed = UnidadeMedidaEnergia.KW,
                                            QFaturada = 2345.333,
                                            VItem = 123.45M,
                                            VProd = 23,
                                            IndDevolucao = SimNao.Sim,
                                            IndPrecoACL = SimNao.Sim
                                        },
                                        Imposto = new XmlNF3e.Imposto
                                        {
                                            ICMS00 = new XmlNF3e.ICMS00
                                            {
                                                CST = "00",
                                                VBC = 123,
                                                PICMS = 12.13,
                                                VICMS = 23.33,
                                                PFCP = 22.333,
                                                VFCP = 33.4
                                            },
                                            PIS = new XmlNF3e.PIS
                                            {
                                                CST = CSTPisCofins.AliquotaBasica,
                                                VBC = 22.22,
                                                PPIS = 3,
                                                VPIS = 4.44
                                            },
                                            COFINS = new XmlNF3e.COFINS
                                            {
                                                CST = CSTPisCofins.AliquotaBasica,
                                                VBC = 22.22,
                                                PCOFINS = 5.55,
                                                VCOFINS = 33
                                            },
                                            RetTrib = new RetTribNF3e
                                            {
                                                VRetPIS = 23.13M,
                                                VRetCofins = 33M,
                                                VRetCSLL = 33,
                                                VBCIRRF = 25,
                                                VIRRF = 234.55M
                                            }
                                        },
                                        GProcRef = new GProcRef
                                        {
                                            VItem = 2.33M,
                                            QFaturada = 123.45,
                                            VProd = 23.33M,
                                            IndDevolucao = SimNao.Sim,
                                            VBC = 23.2,
                                            PICMS = 22.2,
                                            VICMS = 2,
                                            PFCP = 123.4,
                                            VFCP = 22.22,
                                            VBCST = 2.22,
                                            PICMSST = 2.222,
                                            VICMSST = 22.33,
                                            PFCPST = 22.2,
                                            VFCPST = 22.22,
                                            VPIS = 2.2,
                                            VPISEfet = 2.12,
                                            VCOFINS = 2.22,
                                            VCOFINSEfet = 2,
                                            GProc = new List<GProc>
                                            {
                                                new GProc
                                                {
                                                    TpProc = TipoProcessoNF3eNFCom.JusticaEstadual,
                                                    NProcesso = "1222"
                                                }
                                            }
                                        },
                                        InfAdProd = "TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE"
                                    }
                                }
                            }
                        }
                    },
                    Total = new XmlNF3e.Total
                    {
                        VProd = 23151515.22,
                        ICMSTot = new XmlNF3e.ICMSTot
                        {
                            VBC = 123.45,
                            VICMS = 12.22,
                            VICMSDeson = 1,
                            VFCP = 22,
                            VBCST = 123.45,
                            VST = 1234.5,
                            VFCPST = 11.1
                        },
                        VRetTribTot = new VRetTribTot
                        {
                            VRetPIS = 123.45,
                            VRetCofins = 123.45,
                            VRetCSLL = 11.11,
                            VIRRF = 11.44
                        },
                        VCOFINS = 11.1,
                        VCOFINSEfet = 11.1,
                        VPIS = 14.11,
                        VPISEfet = 14.11,
                        VNF = 111111.45
                    },
                    GFat = new GFat
                    {
                        CompetFat = "202411",
                        DVencFat = DateTime.Now,
                        DApresFat = DateTime.Now,
                        DProxLeitura = DateTime.Now,
                        CodBarras = "12345678901234567890",
                        CodDebAuto = "222",

                        EnderCorresp = new EnderCorresp
                        {
                            XLgr = "Rua da Silva",
                            Nro = "1",
                            XBairro = "Jardim Ipe",
                            CMun = "1234567",
                            XMun = "Outro",
                            CEP = "87777001",
                            UF = UFBrasil.PR,
                            Email = "1teste1@1teste1.com.br"
                        }
                    },
                    GANEEL = new GANEEL
                    {
                        GHistFat = new List<GHistFat>
                        {
                            new GHistFat
                            {
                                XGrandFat = "TESTE UNICO",
                                GGrandFat = new List<GGrandFat>
                                {
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "01"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "02"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "03"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "04"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "05"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "06"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "07"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "08"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "09"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "10"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "11"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "12"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "13"
                                    }
                                }
                            }
                        }
                    },
                    AutXML = new List<XmlNF3e.AutXML>
                    {
                        new XmlNF3e.AutXML
                        {
                            CNPJ = "06117473000150"
                        },
                        new XmlNF3e.AutXML
                        {
                            CPF = "12345678901"
                        }
                    },
                    InfAdic = new InfAdicNF3e
                    {
                        InfAdFisco = "TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE",
                        InfCpl = new List<string>
                        {
                            "TESTE do InfCpl",
                            "TESTE2 do InfCpl"
                        }
                    },
                    GRespTec = new GRespTec
                    {
                        CNPJ = "06117473000150",
                        XContato = "Wandrey Mundin Ferreria",
                        Email = "wandrey@unimake.com.br",
                        Fone = "04431414900"
                    }
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoAmbiente = TipoAmbiente.Homologacao,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = CertificadoSelecionado
            };

            var autorizacaoSincNF3e = new ServicoNF3e.AutorizacaoSinc(xml, configuracao);
            autorizacaoSincNF3e.Executar();

            MessageBox.Show(autorizacaoSincNF3e.RetornoWSString);

            if (autorizacaoSincNF3e.Result.CStat == 100)
            {
                if (autorizacaoSincNF3e.Result.ProtNF3e.InfProt.CStat == 100)
                {
                    MessageBox.Show(autorizacaoSincNF3e.Result.ProtNF3e.InfProt.NProt);
                    var teste = autorizacaoSincNF3e.NF3eProcResults[xml.InfNF3e.Chave].GerarXML();
                }
            }
        }

        private void BtnConsultaSituacaoNF3e_Click(object sender, EventArgs e)
        {
            var xml = new XmlNF3e.ConsSitNF3e
            {
                Versao = "1.00",
                ChNF3e = "12345678901234567890123456789012345678901234",
                TpAmb = TipoAmbiente.Homologacao
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CertificadoDigital = CertificadoSelecionado
            };

            var consultaProtocolo = new ServicoNF3e.ConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            MessageBox.Show(consultaProtocolo.RetornoWSString);

        }

        private void BtnEnviarEventoCancelamentoNF3e_Click(object sender, EventArgs e)
        {
            var xmlEvento = new EventoNF3e
            {
                Versao = "1.00",
                InfEvento =
                new XmlNF3e.InfEvento(new XmlNF3e.DetEventoCanc
                {
                    VersaoEvento = "1.00",
                    DescEvento = "Cancelamento",
                    NProt = "12345678",
                    XJust = "Erro no valor do item 3"
                })
                {
                    COrgao = UFBrasil.PR,
                    TpAmb = TipoAmbiente.Homologacao,
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
                CertificadoDigital = CertificadoSelecionado,
                TipoAmbiente = TipoAmbiente.Homologacao,
            };

            var recepcaoEvento = new ServicoNF3e.RecepcaoEvento(xmlEvento, configuracao);
            recepcaoEvento.Executar();

            MessageBox.Show(recepcaoEvento.RetornoWSString);
            MessageBox.Show(recepcaoEvento.Result.InfEvento.CStat + " - " + recepcaoEvento.Result.InfEvento.XMotivo);

            switch (recepcaoEvento.Result.InfEvento.CStat)
            {
                case 134: // Recebido pelo Sistema de Registro de Eventos, com vinculação do evento na respectiva NF3e com situação diferente de Autorizada
                case 135: // Recebido pelo Sistema de Registro de Eventos, com vinculação do evento na respectiva NF3e
                case 136: // Recebido pelo Sistema de Registro de Eventos – vinculação do evento à NF3e prejudicado
                    recepcaoEvento.GravarXmlDistribuicao(@"d:\testenfe");
                    break;

                default:
                    // Evento rejeitado, fazer os devidos tratamentos.
                    break;
            }
        }
    }
}