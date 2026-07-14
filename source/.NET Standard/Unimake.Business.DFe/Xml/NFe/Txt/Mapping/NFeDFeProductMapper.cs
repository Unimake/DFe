using System;
using System.Collections.Generic;
using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;
using DFeService = Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeProductMapper
    {
        private readonly NFeDFeTaxMapper taxMapper = new NFeDFeTaxMapper();
        private const string ProdutoHomologacaoNFCe = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL";

        public List<DFeNFe.Det> Mapear(NFe nota)
        {
            if (nota.det == null || nota.det.Count == 0)
            {
                return null;
            }

            return nota.det.Select(x => MapearDetalhe(nota, x)).ToList();
        }

        private DFeNFe.Det MapearDetalhe(NFe nota, Det detalhe)
        {
            var produto = detalhe.Prod;
            return new DFeNFe.Det
            {
                NItem = produto.nItem,
                Prod = new DFeNFe.Prod
                {
                    CProd = produto.cProd,
                    CEAN = produto.cEAN ?? string.Empty,
                    CBarra = VazioParaNulo(produto.cBarra),
                    XProd = nota.ide.tpAmb == DFeService.TipoAmbiente.Homologacao &&
                        nota.ide.mod == TpcnMod.modNFCe && produto.nItem == 1
                            ? ProdutoHomologacaoNFCe
                            : produto.xProd,
                    NCM = produto.NCM,
                    NVE = string.IsNullOrWhiteSpace(produto.NVE) ? null : new List<string> { produto.NVE },
                    CEST = produto.CEST > 0 ? produto.CEST.ToString("0000000") : null,
                    IndEscala = MapearIndicadorEscala(produto.indEscala),
                    CNPJFab = VazioParaNulo(produto.CNPJFab),
                    CBenef = VazioParaNulo(produto.cBenef),
                    GCred = MapearCreditosPresumidos(produto.credPresumido),
                    TpCredPresIBSZFM = MapearTipoCreditoZFM(produto.tpCredPresIBSZFM),
                    EXTIPI = VazioParaNulo(produto.EXTIPI),
                    CFOP = produto.CFOP,
                    UCom = produto.uCom,
                    QCom = produto.qCom,
                    VUnCom = produto.vUnCom,
                    VProd = produto.vProd,
                    CEANTrib = produto.cEANTrib ?? string.Empty,
                    CBarraTrib = VazioParaNulo(produto.cBarraTrib),
                    UTrib = produto.uTrib,
                    QTrib = produto.qTrib,
                    VUnTrib = produto.vUnTrib,
                    VFrete = produto.vFrete,
                    VSeg = produto.vSeg,
                    VDesc = produto.vDesc,
                    VOutro = produto.vOutro,
                    IndTot = (DFeService.SimNao)(int)produto.indTot,
                    IndBemMovelUsado = (int)produto.indBemMovelUsado,
                    XPed = VazioParaNulo(produto.xPed),
                    NItemPed = VazioParaNulo(produto.nItemPed),
                    NFCI = VazioParaNulo(produto.nFCI),
                    NRECOPI = VazioParaNulo(produto.nRECOPI),
                    DI = MapearDeclaracoesImportacao(produto.DI),
                    DetExport = MapearExportacoes(produto.detExport),
                    Rastro = MapearRastreabilidade(produto.rastro),
                    VeicProd = MapearVeiculo(produto.veicProd),
                    Med = MapearMedicamento(produto.med),
                    Arma = MapearArmas(produto.arma),
                    Comb = MapearCombustivel(produto.comb)
                },
                InfAdProd = VazioParaNulo(detalhe.infAdProd),
                VItem = detalhe.vItem,
                DFeReferenciado = MapearDFeReferenciado(detalhe.DfeReferenciado),
                Imposto = taxMapper.Mapear(nota, detalhe.Imposto),
                ImpostoDevol = taxMapper.MapearImpostoDevol(detalhe.impostoDevol),
                ObsItem = MapearObservacaoItem(detalhe.ObsItem)
            };
        }

        private static DFeNFe.ObsItem MapearObservacaoItem(ObsItem origem)
        {
            var cont = string.IsNullOrWhiteSpace(origem.ObsCont.xCampo) ? null : new List<DFeNFe.ObsCont>
            {
                new DFeNFe.ObsCont { XCampo = origem.ObsCont.xCampo, XTexto = origem.ObsCont.xTexto }
            };
            var fisco = string.IsNullOrWhiteSpace(origem.ObsFisco.xCampo) ? null : new List<DFeNFe.ObsFisco>
            {
                new DFeNFe.ObsFisco { XCampo = origem.ObsFisco.xCampo, XTexto = origem.ObsFisco.xTexto }
            };
            return cont == null && fisco == null ? null : new DFeNFe.ObsItem { ObsCont = cont, ObsFisco = fisco };
        }

        private static List<DFeNFe.DI> MapearDeclaracoesImportacao(List<DI> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.DI
            {
                NDI = x.nDI,
                DDI = x.dDI,
                XLocDesemb = x.xLocDesemb,
                UFDesemb = ParseUF(x.UFDesemb),
                DDesemb = x.dDesemb,
                TpViaTransp = (DFeService.ViaTransporteInternacional)(int)x.tpViaTransp,
                VAFRMM = x.vAFRMM,
                TpIntermedio = (DFeService.FormaImportacaoIntermediacao)(int)x.tpIntermedio,
                CNPJ = VazioParaNulo(x.CNPJ),
                CPF = VazioParaNulo(x.CPF),
                UFTerceiro = string.IsNullOrWhiteSpace(x.UFTerceiro) ? null : (DFeService.UFBrasil?)ParseUF(x.UFTerceiro),
                CExportador = x.cExportador,
                Adi = x.adi == null || x.adi.Count == 0 ? null : x.adi.Select(a => new DFeNFe.Adi
                {
                    NAdicao = a.nAdicao,
                    NSeqAdic = a.nSeqAdi,
                    CFabricante = a.cFabricante,
                    VDescDI = a.vDescDI,
                    NDraw = VazioParaNulo(a.nDraw)
                }).ToList()
            }).ToList();
        }

        private static List<DFeNFe.DetExport> MapearExportacoes(List<detExport> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.DetExport
            {
                NDraw = VazioParaNulo(x.nDraw),
                ExportInd = string.IsNullOrWhiteSpace(x.exportInd?.nRE) ? null : new DFeNFe.ExportInd
                {
                    NRE = x.exportInd.nRE,
                    ChNFe = x.exportInd.chNFe,
                    QExport = x.exportInd.qExport
                }
            }).ToList();
        }

        private static List<DFeNFe.Rastro> MapearRastreabilidade(List<Rastro> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.Rastro
            {
                NLote = x.nLote,
                QLote = x.qLote,
                DFab = x.dFab,
                DVal = x.dVal,
                CAgreg = VazioParaNulo(x.cAgreg)
            }).ToList();
        }

        private static DFeNFe.DFeReferenciado MapearDFeReferenciado(DFeReferenciado origem)
        {
            if (origem == null || string.IsNullOrWhiteSpace(origem.chaveAcesso))
            {
                return null;
            }

            return new DFeNFe.DFeReferenciado
            {
                ChaveAcesso = origem.chaveAcesso,
                NItem = origem.nItem > 0 ? origem.nItem.ToString() : null
            };
        }

        private static DFeNFe.VeicProd MapearVeiculo(veicProd origem)
        {
            if (string.IsNullOrWhiteSpace(origem.chassi))
            {
                return null;
            }

            return new DFeNFe.VeicProd
            {
                TpOp = (DFeService.TipoOperacaoVeicNovo)int.Parse(origem.tpOp),
                Chassi = origem.chassi,
                CCor = origem.cCor,
                XCor = origem.xCor,
                Pot = origem.pot,
                Cilin = origem.cilin,
                PesoL = origem.pesoL,
                PesoB = origem.pesoB,
                NSerie = origem.nSerie,
                TpComb = origem.tpComb,
                NMotor = origem.nMotor,
                CMT = origem.CMT,
                Dist = origem.dist,
                AnoMod = origem.anoMod.ToString("0000"),
                AnoFab = origem.anoFab.ToString("0000"),
                TpPint = origem.tpPint,
                TpVeic = origem.tpVeic.ToString(),
                EspVeic = origem.espVeic.ToString(),
                VIN = (DFeService.CondicaoVIN)int.Parse(origem.VIN),
                CondVeic = (DFeService.CondicaoVeiculo)int.Parse(origem.condVeic),
                CMod = int.Parse(origem.cMod),
                CCorDENATRAN = origem.cCorDENATRAN.ToString("00"),
                Lota = origem.lota,
                TpRest = (DFeService.TipoRestricaoVeiculo)origem.tpRest
            };
        }

        private static DFeNFe.Med MapearMedicamento(List<Med> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            if (origem.Count > 1)
            {
                throw new NotSupportedException("A classe NFe da Unimake.DFe aceita somente um grupo med por produto.");
            }

            var item = origem[0];
            return new DFeNFe.Med
            {
                CProdANVISA = item.cProdANVISA,
                XMotivoIsencao = VazioParaNulo(item.xMotivoIsencao),
                VPMC = item.vPMC
            };
        }

        private static List<DFeNFe.Arma> MapearArmas(List<Arma> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.Arma
            {
                TpArma = (DFeService.TipoArma)(int)x.tpArma,
                NSerie = x.nSerie,
                NCano = x.nCano,
                Descr = x.descr
            }).ToList();
        }

        private static List<DFeNFe.Comb> MapearCombustivel(Comb origem)
        {
            if (origem == null)
            {
                return null;
            }

            var combustivel = new DFeNFe.Comb
            {
                CProdANP = origem.cProdANP.ToString("000000000"),
                DescANP = origem.descANP,
                PGLP = origem.pGLP,
                PGNn = origem.pGNn,
                PGNi = origem.pGNi,
                VPart = origem.vPart,
                CODIF = VazioParaNulo(origem.CODIF),
                QTemp = origem.qTemp,
                UFCons = ParseUF(origem.UFCons),
                PBio = origem.pBio,
                CIDE = origem.CIDE.qBCprod > 0 ? new DFeNFe.CIDE
                {
                    QBCProd = origem.CIDE.qBCprod,
                    VAliqProd = origem.CIDE.vAliqProd,
                    VCIDE = origem.CIDE.vCIDE
                } : null,
                Encerrante = origem.encerrante.nBico > 0 ? new DFeNFe.Encerrante
                {
                    NBico = origem.encerrante.nBico,
                    NBomba = origem.encerrante.nBomba,
                    NTanque = origem.encerrante.nTanque,
                    VEncIni = ConverterDouble(origem.encerrante.vEncIni),
                    VEncFin = ConverterDouble(origem.encerrante.vEncFin)
                } : null,
                OrigComb = origem.origComb == null || origem.origComb.Count == 0 ? null : origem.origComb.Select(x => new DFeNFe.OrigComb
                {
                    IndImport = (DFeService.IndicadorImportacao)x.indImport,
                    CUFOrig = (DFeService.UFBrasil)x.cUFOrig,
                    POrig = x.pOrig
                }).ToList()
            };

            return new List<DFeNFe.Comb> { combustivel };
        }

        private static DFeService.IndicadorEscalaRelevante? MapearIndicadorEscala(TpcnIndicadorEscala origem)
        {
            switch (origem)
            {
                case TpcnIndicadorEscala.ieSomaTotalNFe:
                    return DFeService.IndicadorEscalaRelevante.Sim;
                case TpcnIndicadorEscala.ieNaoSomaTotalNFe:
                    return DFeService.IndicadorEscalaRelevante.Nao;
                default:
                    return null;
            }
        }

        private static List<DFeNFe.GCred> MapearCreditosPresumidos(List<CredPresumido> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.GCred
            {
                CCredPresumido = x.cCredPresumido,
                PCredPresumido = x.pCredPresumido,
                VCredPresumido = x.vCredPresumido
            }).ToList();
        }

        private static DFeService.TipoCreditoPresumidoIBSZFM? MapearTipoCreditoZFM(string origem)
        {
            if (!int.TryParse(origem, out var valor) || !Enum.IsDefined(typeof(DFeService.TipoCreditoPresumidoIBSZFM), valor))
            {
                return null;
            }

            return (DFeService.TipoCreditoPresumidoIBSZFM)valor;
        }

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;

        private static DFeService.UFBrasil ParseUF(string uf) => (DFeService.UFBrasil)Enum.Parse(typeof(DFeService.UFBrasil), uf, true);

        private static double ConverterDouble(string valor) => string.IsNullOrWhiteSpace(valor)
            ? 0
            : double.Parse(valor, System.Globalization.CultureInfo.InvariantCulture);
    }
}
