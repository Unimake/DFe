using System;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeTaxMapper
    {
        public DFeNFe.Imposto Mapear(NFe nota, Imposto origem)
        {
            var icms = MapearICMS(origem.ICMS);
            var ipi = MapearIPI(origem.IPI);
            var ii = MapearII(origem.II);
            var pis = MapearPIS(origem.PIS);
            var pisst = MapearPISST(origem.PISST);
            var cofins = MapearCOFINS(origem.COFINS);
            var cofinsst = MapearCOFINSST(origem.COFINSST);
            var issqn = MapearISSQN(origem.ISSQN);
            var icmsUfDest = MapearICMSUFDest(origem.ICMS.ICMSUFDest);
            var impostoSeletivo = MapearIS(origem.IS);
            var ibsCbs = MapearIBSCBS(nota, origem.IBSCBS);
            if (origem.vTotTrib <= 0 && icms == null && ipi == null && ii == null && pis == null && pisst == null && cofins == null && cofinsst == null && issqn == null && icmsUfDest == null && impostoSeletivo == null && ibsCbs == null)
            {
                return null;
            }

            return new DFeNFe.Imposto
            {
                VTotTrib = origem.vTotTrib,
                ICMS = icms,
                IPI = ipi,
                II = ii,
                PIS = pis,
                PISST = pisst,
                COFINS = cofins,
                COFINSST = cofinsst,
                ISSQN = issqn,
                ICMSUFDest = icmsUfDest,
                IS = impostoSeletivo,
                IBSCBS = ibsCbs
            };
        }

        private static DFeNFe.IS MapearIS(IS origem) => string.IsNullOrWhiteSpace(origem.CSTIS) ? null : new DFeNFe.IS
        {
            CSTIS = origem.CSTIS,
            CClassTribIS = origem.cClassTribIS,
            VBCIS = origem.vBCIS,
            PIS = origem.pIS,
            AdRemIS = origem.pISEspec,
            UTrib = origem.uTrib,
            QTrib = origem.qTrib,
            VIS = origem.vIS
        };

        private static DFeNFe.IBSCBS MapearIBSCBS(NFe nota, IBSCBS origem)
        {
            if (string.IsNullOrWhiteSpace(origem.CST) && string.IsNullOrWhiteSpace(origem.cClassTrib)) return null;
            return new DFeNFe.IBSCBS
            {
                CST = origem.CST,
                CClassTrib = origem.cClassTrib,
                IndDoacao = int.TryParse(origem.indDoacao, out var indDoacao) ? indDoacao : 0,
                GIBSCBS = MapearGIBSCBS(nota, origem),
                GIBSCBSMono = MapearGIBSCBSMono(origem.gIBSCBSMono),
                GTransfCred = origem.gTransfCred.vIBS + origem.gTransfCred.vCBS > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GTransfCred>(origem.gTransfCred) : null,
                GAjusteCompet = string.IsNullOrWhiteSpace(origem.gAjusteCompet.competApur) ? null : new DFeNFe.GAjusteCompet
                {
                    CompetApur = MapearCompetencia(origem.gAjusteCompet.competApur),
                    VIBS = origem.gAjusteCompet.vIBS,
                    VCBS = origem.gAjusteCompet.vCBS
                },
                GEstornoCred = origem.gEstornoCred == null ? null : NFeDFeConventionMapper.Mapear<DFeNFe.GEstornoCred>(origem.gEstornoCred),
                GCredPresOper = MapearGCredPresOper(origem.gCredPresOper),
                GCredPresIBSZFM = MapearGCredPresIBSZFM(origem.gCredPresIBSZFM)
            };
        }

        private static DFeNFe.GIBSCBSMono MapearGIBSCBSMono(GIBSCBSMono origem)
        {
            var padrao = TemDados(origem.gMonoPadrao) ? NFeDFeConventionMapper.Mapear<DFeNFe.GMonoPadrao>(origem.gMonoPadrao) : null;
            var reten = TemDados(origem.gMonoReten) ? NFeDFeConventionMapper.Mapear<DFeNFe.GMonoReten>(origem.gMonoReten) : null;
            var ret = TemDados(origem.gMonoRet) ? NFeDFeConventionMapper.Mapear<DFeNFe.GMonoRet>(origem.gMonoRet) : null;
            var dif = TemDados(origem.gMonoDif) ? NFeDFeConventionMapper.Mapear<DFeNFe.GMonoDif>(origem.gMonoDif) : null;
            if (origem.vTotIBSMonoItem <= 0 && origem.vTotCBSMonoItem <= 0 && ret == null) return null;
            return new DFeNFe.GIBSCBSMono { GMonoPadrao = padrao, GMonoReten = reten, GMonoRet = ret, GMonoDif = dif, VTotIBSMonoItem = origem.vTotIBSMonoItem, VTotCBSMonoItem = origem.vTotCBSMonoItem };
        }

        private static DFeNFe.GCredPresOper MapearGCredPresOper(GCredPresOper origem)
        {
            if (origem.vBCCredPres <= 0) return null;
            return new DFeNFe.GCredPresOper
            {
                VBCCredPres = origem.vBCCredPres,
                CCredPres = origem.cCredPres.ToString("F2", System.Globalization.CultureInfo.InvariantCulture),
                GIBSCredPres = origem.gIBSCredPres.vCredPres + origem.gIBSCredPres.vCredPresCondSus > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GIBSCredPres>(origem.gIBSCredPres) : null,
                GCBSCredPres = origem.gCBSCredPres.vCredPres + origem.gCBSCredPres.vCredPresCondSus > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GCBSCredPres>(origem.gCBSCredPres) : null
            };
        }

        private static DFeNFe.GCredPresIBSZFM MapearGCredPresIBSZFM(GCredPresIBSZFM origem)
        {
            if (string.IsNullOrWhiteSpace(origem.tpCredPresIBSZFM)) return null;
            return new DFeNFe.GCredPresIBSZFM
            {
                CompetApur = MapearCompetencia(origem.competApur),
                TpCredPresIBSZFM = (Unimake.Business.DFe.Servicos.TipoCreditoPresumidoIBSZFM)int.Parse(origem.tpCredPresIBSZFM),
                VCredPresIBSZFM = origem.vCredPresIBSZFM
            };
        }

        private static DFeNFe.GIBSCBS MapearGIBSCBS(NFe nota, IBSCBS imposto)
        {
            var origem = imposto.gIBSCBS;
            if (!TemDadosGIBSCBS(nota, imposto)) return null;
            var uf = MapearGIBSUF(nota, imposto, origem.gIBSUF);
            var mun = MapearGIBSMun(nota, imposto, origem.gIBSMun);
            var cbs = MapearGCBS(nota, imposto, origem.gCBS);
            var regular = TemDados(origem.gTribRegular) ? NFeDFeConventionMapper.Mapear<DFeNFe.GTribRegular>(origem.gTribRegular) : null;
            var compraGov = TemDados(origem.gTribCompraGov) ? NFeDFeConventionMapper.Mapear<DFeNFe.GTribCompraGov>(origem.gTribCompraGov) : null;
            return new DFeNFe.GIBSCBS
            {
                VBC = origem.vBC,
                GIBSUF = uf ?? new DFeNFe.GIBSUF { PIBSUF = origem.gIBSUF.pIBSUF, VIBSUF = origem.gIBSUF.vIBSUF },
                GIBSMun = mun ?? new DFeNFe.GIBSMun { PIBSMun = origem.gIBSMun.pIBSMun, VIBSMun = origem.gIBSMun.vIBSMun },
                VIBS = origem.vIBS,
                GCBS = cbs ?? new DFeNFe.GCBS { PCBS = origem.gCBS.pCBS, VCBS = origem.gCBS.vCBS },
                GTribRegular = regular,
                GTribCompraGov = compraGov
            };
        }

        private static DFeNFe.GIBSUF MapearGIBSUF(NFe nota, IBSCBS imposto, GIBSUF origem)
        {
            var dif = MapearGDif(origem.gDif, imposto.CST);
            var dev = origem.gDevTrib.vDevTrib > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GDevTrib>(origem.gDevTrib) : null;
            var red = MapearGRed(nota, imposto.CST, origem.gRed);
            if (origem.pIBSUF <= 0 && origem.vIBSUF <= 0 && dif == null && dev == null && red == null) return null;
            return new DFeNFe.GIBSUF { PIBSUF = origem.pIBSUF, GDif = dif, GDevTrib = dev, GRed = red, VIBSUF = origem.vIBSUF };
        }

        private static DFeNFe.GIBSMun MapearGIBSMun(NFe nota, IBSCBS imposto, GIBSMun origem)
        {
            var dif = MapearGDif(origem.gDif, imposto.CST);
            var dev = origem.gDevTrib.vDevTrib > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GDevTrib>(origem.gDevTrib) : null;
            var red = MapearGRed(nota, imposto.CST, origem.gRed);
            if (origem.pIBSMun <= 0 && origem.vIBSMun <= 0 && dif == null && dev == null && red == null) return null;
            return new DFeNFe.GIBSMun { PIBSMun = origem.pIBSMun, GDif = dif, GDevTrib = dev, GRed = red, VIBSMun = origem.vIBSMun };
        }

        private static DFeNFe.GCBS MapearGCBS(NFe nota, IBSCBS imposto, GCBS origem)
        {
            var dif = MapearGDif(origem.gDif, imposto.CST);
            var dev = origem.gDevTrib.vDevTrib > 0 ? NFeDFeConventionMapper.Mapear<DFeNFe.GDevTrib>(origem.gDevTrib) : null;
            var red = MapearGRed(nota, imposto.CST, origem.gRed);
            if (origem.pCBS <= 0 && origem.vCBS <= 0 && dif == null && dev == null && red == null) return null;
            return new DFeNFe.GCBS { PCBS = origem.pCBS, GDif = dif, GDevTrib = dev, GRed = red, VCBS = origem.vCBS };
        }

        private static DFeNFe.GDif MapearGDif(GDif origem, string cst) => origem.pDif > 0 || origem.vDif > 0 || cst == "510"
            ? NFeDFeConventionMapper.Mapear<DFeNFe.GDif>(origem) : null;

        private static DFeNFe.GRed MapearGRed(NFe nota, string cst, GRed origem)
        {
            var compraGov = Enum.IsDefined(typeof(TpcnTipoEnteGovernamental), nota.ide.gCompraGov.tpEnteGov) && cst != "510";
            return cst == "011" || cst == "200" || cst == "515" || compraGov
                ? NFeDFeConventionMapper.Mapear<DFeNFe.GRed>(origem) : null;
        }

        private static bool TemDados(GTribRegular origem) => !string.IsNullOrWhiteSpace(origem.CSTReg) || !string.IsNullOrWhiteSpace(origem.cClassTribReg) ||
            origem.pAliqEfetRegIBSUF + origem.vTribRegIBSUF + origem.pAliqEfetRegIBSMun + origem.vTribRegIBSMun + origem.pAliqEfetRegCBS + origem.vTribRegCBS > 0;

        private static bool TemDados(GTribCompraGov origem) => origem.pAliqIBSUF + origem.vTribIBSUF + origem.pAliqIBSMun + origem.vTribIBSMun + origem.pAliqCBS + origem.vTribCBS > 0;

        private static bool TemDados(GMonoPadrao origem) => origem.qBCMono + origem.adRemIBS + origem.adRemCBS + origem.vIBSMono + origem.vCBSMono > 0;
        private static bool TemDados(GMonoReten origem) => origem.qBCMonoReten + origem.adRemIBSReten + origem.vIBSMonoReten + origem.adRemCBSReten + origem.vCBSMonoReten > 0;
        private static bool TemDados(GMonoRet origem) => origem.qBCMonoRet + origem.adRemIBSRet + origem.vIBSMonoRet + origem.adRemCBSRet + origem.vCBSMonoRet > 0;
        private static bool TemDados(GMonoDif origem) => origem.pDifIBS + origem.vIBSMonoDif + origem.pDifCBS + origem.vCBSMonoDif > 0;

        private static bool TemDadosGIBSCBS(NFe nota, IBSCBS imposto)
        {
            var x = imposto.gIBSCBS;
            var compraGov = Enum.IsDefined(typeof(TpcnTipoEnteGovernamental), nota.ide.gCompraGov.tpEnteGov) && imposto.CST != "510";
            var permiteReducao = imposto.CST == "011" || imposto.CST == "200" || imposto.CST == "515" || compraGov;
            var reducao = permiteReducao && (x.gIBSUF.gRed.pRedAliq + x.gIBSUF.gRed.pAliqEfet + x.gIBSMun.gRed.pRedAliq + x.gIBSMun.gRed.pAliqEfet + x.gCBS.gRed.pRedAliq + x.gCBS.gRed.pAliqEfet > 0);
            return x.vBC > 0 || x.vIBS > 0 || imposto.CST == "510" || reducao ||
                x.gIBSUF.pIBSUF + x.gIBSUF.vIBSUF + x.gIBSUF.gDif.pDif + x.gIBSUF.gDif.vDif + x.gIBSUF.gDevTrib.vDevTrib > 0 ||
                x.gIBSMun.pIBSMun + x.gIBSMun.vIBSMun + x.gIBSMun.gDif.pDif + x.gIBSMun.gDif.vDif + x.gIBSMun.gDevTrib.vDevTrib > 0 ||
                x.gCBS.pCBS + x.gCBS.vCBS + x.gCBS.gDif.pDif + x.gCBS.gDif.vDif + x.gCBS.gDevTrib.vDevTrib > 0 ||
                TemDados(x.gTribRegular) || TemDados(x.gTribCompraGov);
        }

        private static DateTime MapearCompetencia(string valor) => DateTime.ParseExact(valor, "yyyy-MM", System.Globalization.CultureInfo.InvariantCulture);

        public DFeNFe.ImpostoDevol MapearImpostoDevol(impostoDevol origem)
        {
            if (origem == null || origem.pDevol <= 0 && origem.vIPIDevol <= 0)
            {
                return null;
            }

            return new DFeNFe.ImpostoDevol
            {
                PDevol = origem.pDevol,
                IPI = new DFeNFe.IPIDevol { VIPIDevol = origem.vIPIDevol }
            };
        }

        private static DFeNFe.ISSQN MapearISSQN(ISSQN origem)
        {
            if (origem.vBC <= 0 && origem.vAliq <= 0 && origem.vISSQN <= 0 && origem.cMunFG <= 0 && string.IsNullOrWhiteSpace(origem.cListServ))
            {
                return null;
            }

            return new DFeNFe.ISSQN
            {
                VBC = origem.vBC,
                VAliq = origem.vAliq,
                VISSQN = origem.vISSQN,
                CMunFG = origem.cMunFG,
                CListServ = (Unimake.Business.DFe.Servicos.ListaServicoISSQN)int.Parse(origem.cListServ.Replace(".", string.Empty)),
                VDeducao = origem.vDeducao,
                VOutro = origem.vOutro,
                VDescIncond = origem.vDescIncond,
                VDescCond = origem.vDescCond,
                VISSRet = origem.vISSRet,
                IndISS = (Unimake.Business.DFe.Servicos.IndicadorExigibilidadeISSQN)(int)origem.indISS,
                CServico = VazioParaNulo(origem.cServico),
                CMun = origem.cMun,
                CPais = origem.cPais,
                NProcesso = VazioParaNulo(origem.nProcesso),
                IndIncentivo = origem.indIncentivo
                    ? Unimake.Business.DFe.Servicos.SimNao12.Sim
                    : Unimake.Business.DFe.Servicos.SimNao12.Nao
            };
        }

        private static DFeNFe.ICMSUFDest MapearICMSUFDest(ICMSUFDest origem)
        {
            if (origem.vBCUFDest + origem.vBCFCPUFDest + origem.pFCPUFDest + origem.pICMSUFDest + origem.pICMSInter +
                origem.pICMSInterPart + origem.vFCPUFDest + origem.vICMSUFDest + origem.vICMSUFRemet <= 0)
            {
                return null;
            }

            return NFeDFeConventionMapper.Mapear<DFeNFe.ICMSUFDest>(origem);
        }

        private static DFeNFe.IPI MapearIPI(IPI origem)
        {
            if (string.IsNullOrWhiteSpace(origem.CST)) return null;
            var destino = new DFeNFe.IPI
            {
                CNPJProd = VazioParaNulo(origem.CNPJProd),
                CSelo = VazioParaNulo(origem.cSelo),
                QSelo = origem.qSelo > 0 ? (int?)origem.qSelo : null,
                CEnq = string.IsNullOrWhiteSpace(origem.cEnq) ? "999" : origem.cEnq
            };
            if (origem.CST == "00" || origem.CST == "49" || origem.CST == "50" || origem.CST == "99")
            {
                destino.IPITrib = NFeDFeConventionMapper.Mapear<DFeNFe.IPITrib>(origem);
            }
            else
            {
                destino.IPINT = NFeDFeConventionMapper.Mapear<DFeNFe.IPINT>(origem);
            }
            return destino;
        }

        private static DFeNFe.II MapearII(II origem) => origem.vBC + origem.vDespAdu + origem.vII + origem.vIOF > 0
            ? NFeDFeConventionMapper.Mapear<DFeNFe.II>(origem)
            : null;

        private static DFeNFe.PIS MapearPIS(PIS origem)
        {
            if (string.IsNullOrWhiteSpace(origem.CST)) return null;
            var destino = new DFeNFe.PIS();
            if (origem.CST == "01" || origem.CST == "02") destino.PISAliq = NFeDFeConventionMapper.Mapear<DFeNFe.PISAliq>(origem);
            else if (origem.CST == "03") destino.PISQtde = NFeDFeConventionMapper.Mapear<DFeNFe.PISQtde>(origem);
            else if (origem.CST == "04" || origem.CST == "05" || origem.CST == "06" || origem.CST == "07" || origem.CST == "08" || origem.CST == "09") destino.PISNT = NFeDFeConventionMapper.Mapear<DFeNFe.PISNT>(origem);
            else destino.PISOutr = CriarPISOutr(origem);
            return destino;
        }

        private static DFeNFe.PISOutr CriarPISOutr(PIS origem) => new DFeNFe.PISOutr
        {
            CST = origem.CST,
            VBC = origem.qBCProd + origem.vAliqProd > 0 ? null : (double?)origem.vBC,
            PPIS = origem.qBCProd + origem.vAliqProd > 0 ? null : (double?)origem.pPIS,
            QBCProd = origem.qBCProd + origem.vAliqProd > 0 ? (double?)origem.qBCProd : null,
            VAliqProd = origem.qBCProd + origem.vAliqProd > 0 ? (double?)origem.vAliqProd : null,
            VPIS = origem.vPIS
        };

        private static DFeNFe.PISST MapearPISST(PISST origem)
        {
            if (origem.vBC + origem.pPis + origem.qBCProd + origem.vAliqProd + origem.vPIS <= 0) return null;
            var quantidade = origem.qBCProd + origem.vAliqProd > 0;
            return new DFeNFe.PISST
            {
                VBC = quantidade ? null : (double?)origem.vBC,
                PPIS = quantidade ? null : (double?)origem.pPis,
                QBCProd = quantidade ? (double?)origem.qBCProd : null,
                VAliqProd = quantidade ? (double?)origem.vAliqProd : null,
                VPIS = origem.vPIS,
                IndSomaPISST = MapearEnumOpcional<Unimake.Business.DFe.Servicos.IndicaSomaPISST>(origem.indSomaPISST)
            };
        }

        private static DFeNFe.COFINS MapearCOFINS(COFINS origem)
        {
            if (string.IsNullOrWhiteSpace(origem.CST)) return null;
            var destino = new DFeNFe.COFINS();
            if (origem.CST == "01" || origem.CST == "02") destino.COFINSAliq = NFeDFeConventionMapper.Mapear<DFeNFe.COFINSAliq>(origem);
            else if (origem.CST == "03") destino.COFINSQtde = NFeDFeConventionMapper.Mapear<DFeNFe.COFINSQtde>(origem);
            else if (origem.CST == "04" || origem.CST == "05" || origem.CST == "06" || origem.CST == "07" || origem.CST == "08" || origem.CST == "09") destino.COFINSNT = NFeDFeConventionMapper.Mapear<DFeNFe.COFINSNT>(origem);
            else destino.COFINSOutr = CriarCOFINSOutr(origem);
            return destino;
        }

        private static DFeNFe.COFINSOutr CriarCOFINSOutr(COFINS origem) => new DFeNFe.COFINSOutr
        {
            CST = origem.CST,
            VBC = origem.qBCProd + origem.vAliqProd > 0 ? null : (double?)origem.vBC,
            PCOFINS = origem.qBCProd + origem.vAliqProd > 0 ? null : (double?)origem.pCOFINS,
            QBCProd = origem.qBCProd + origem.vAliqProd > 0 ? (double?)origem.qBCProd : null,
            VAliqProd = origem.qBCProd + origem.vAliqProd > 0 ? (double?)origem.vAliqProd : null,
            VCOFINS = origem.vCOFINS
        };

        private static DFeNFe.COFINSST MapearCOFINSST(COFINSST origem)
        {
            if (origem.vBC + origem.pCOFINS + origem.qBCProd + origem.vAliqProd + origem.vCOFINS <= 0) return null;
            var quantidade = origem.qBCProd + origem.vAliqProd > 0;
            return new DFeNFe.COFINSST
            {
                VBC = quantidade ? null : (double?)origem.vBC,
                PCOFINS = quantidade ? null : (double?)origem.pCOFINS,
                QBCProd = quantidade ? (double?)origem.qBCProd : null,
                VAliqProd = quantidade ? (double?)origem.vAliqProd : null,
                VCOFINS = origem.vCOFINS,
                IndSomaCOFINSST = MapearEnumOpcional<Unimake.Business.DFe.Servicos.IndicaSomaCOFINSST>(origem.indSomaCOFINSST)
            };
        }

        private static TEnum? MapearEnumOpcional<TEnum>(string valor) where TEnum : struct
        {
            if (!int.TryParse(valor, out var numero) || !Enum.IsDefined(typeof(TEnum), numero)) return null;
            return (TEnum)Enum.ToObject(typeof(TEnum), numero);
        }

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;

        private static DFeNFe.ICMS MapearICMS(ICMS origem)
        {
            var destino = new DFeNFe.ICMS();
            var mapeado = true;

            if (origem.ICMSPart10 > 0 || origem.ICMSPart90 > 0)
            {
                destino.ICMSPart = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSPart>(origem);
            }
            else if (origem.ICMSst > 0)
            {
                destino.ICMSST = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSST>(origem);
            }
            else if (origem.CSOSN > 0)
            {
                switch (origem.CSOSN)
                {
                    case 101: destino.ICMSSN101 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN101>(origem); break;
                    case 102:
                    case 103:
                    case 300:
                    case 400: destino.ICMSSN102 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN102>(origem); break;
                    case 201: destino.ICMSSN201 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN201>(origem); break;
                    case 202:
                    case 203: destino.ICMSSN202 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN202>(origem); break;
                    case 500:
                        destino.ICMSSN500 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN500>(origem);
                        if (origem.vBCSTRet <= 0) destino.ICMSSN500.VBCSTRet = null;
                        if (origem.pST <= 0) destino.ICMSSN500.PST = null;
                        if (origem.vICMSSubstituto <= 0) destino.ICMSSN500.VICMSSubstituto = null;
                        if (origem.vICMSSTRet <= 0) destino.ICMSSN500.VICMSSTRet = null;
                        break;
                    case 900:
                        destino.ICMSSN900 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSSN900>(origem);
                        if (origem.pRedBC <= 0) destino.ICMSSN900.PRedBC = null;
                        if (origem.pRedBCST <= 0) destino.ICMSSN900.PRedBCST = null;
                        break;
                    default: throw new NotSupportedException("CSOSN nao suportado: " + origem.CSOSN);
                }
            }
            else
            {
                switch (origem.CST)
                {
                    case "00": destino.ICMS00 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS00>(origem); break;
                    case "02": destino.ICMS02 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS02>(origem); break;
                    case "10": destino.ICMS10 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS10>(origem); break;
                    case "15": destino.ICMS15 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS15>(origem); break;
                    case "20": destino.ICMS20 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS20>(origem); break;
                    case "30": destino.ICMS30 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS30>(origem); break;
                    case "40":
                    case "41":
                    case "50":
                        destino.ICMS40 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS40>(origem);

                        // Na rotina legada, os campos de desoneraÃ§Ã£o somente eram
                        // gravados quando vICMSDeson estava preenchido. A classe da
                        // DLL possui ShouldSerialize prÃ³prio para indDeduzDeson,
                        // portanto precisamos preservar aqui a mesma condiÃ§Ã£o do
                        // TXT->XML antigo e nÃ£o emitir indDeduzDeson isoladamente.
                        if (origem.vICMSDeson <= 0)
                        {
                            destino.ICMS40.VICMSDeson = 0;
                            destino.ICMS40.MotDesICMS = default;
                            destino.ICMS40.IndDeduzDeson = null;
                        }
                        break;
                    case "51": destino.ICMS51 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS51>(origem); break;
                    case "53": destino.ICMS53 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS53>(origem); break;
                    case "60": destino.ICMS60 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS60>(origem); break;
                    case "61": destino.ICMS61 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS61>(origem); break;
                    case "70": destino.ICMS70 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS70>(origem); break;
                    case "90": destino.ICMS90 = NFeDFeConventionMapper.Mapear<DFeNFe.ICMS90>(origem); break;
                    case null:
                    case "": mapeado = false; break;
                    default: throw new NotSupportedException("CST de ICMS nao suportado: " + origem.CST);
                }
            }

            return mapeado ? destino : null;
        }
    }
}
