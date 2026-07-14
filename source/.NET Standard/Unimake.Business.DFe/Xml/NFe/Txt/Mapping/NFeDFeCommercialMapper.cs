using System;
using System.Collections.Generic;
using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;
using DFeService = Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeCommercialMapper
    {
        public DFeNFe.Transp MapearTransporte(Transp origem) => new DFeNFe.Transp
        {
            ModFrete = (DFeService.ModalidadeFrete)(int)origem.modFrete,
            Transporta = TemTransportador(origem.Transporta) ? MapearTransportador(origem.Transporta) : null,
            RetTransp = TemRetencao(origem.retTransp) ? NFeDFeConventionMapper.Mapear<DFeNFe.RetTransp>(origem.retTransp) : null,
            VeicTransp = string.IsNullOrWhiteSpace(origem.veicTransp.placa) ? null : new DFeNFe.VeicTransp { Placa = origem.veicTransp.placa, UF = ParseUF(origem.veicTransp.UF), RNTC = VazioParaNulo(origem.veicTransp.RNTC) },
            Reboque = origem.Reboque == null || origem.Reboque.Count == 0 ? null : origem.Reboque.Select(x => new DFeNFe.Reboque { Placa = x.placa, UF = ParseUF(x.UF), RNTC = VazioParaNulo(x.RNTC) }).ToList(),
            Vagao = VazioParaNulo(origem.vagao),
            Balsa = VazioParaNulo(origem.balsa),
            Vol = origem.Vol == null ? new List<DFeNFe.Vol>() : origem.Vol.Select(MapearVolume).ToList()
        };

        public DFeNFe.Cobr MapearCobranca(Cobr origem)
        {
            var temFat = !string.IsNullOrWhiteSpace(origem.Fat.nFat) || origem.Fat.vOrig + origem.Fat.vDesc + origem.Fat.vLiq > 0;
            if (!temFat && (origem.Dup == null || origem.Dup.Count == 0)) return null;
            return new DFeNFe.Cobr
            {
                Fat = temFat ? NFeDFeConventionMapper.Mapear<DFeNFe.Fat>(origem.Fat) : null,
                Dup = origem.Dup == null || origem.Dup.Count == 0 ? null : origem.Dup.Select(x => NFeDFeConventionMapper.Mapear<DFeNFe.Dup>(x)).ToList()
            };
        }

        public DFeNFe.Pag MapearPagamento(NFe nota)
        {
            if (nota.pag == null || nota.pag.Count == 0) return null;
            return new DFeNFe.Pag { VTroco = nota.vTroco, DetPag = nota.pag.Select(MapearDetalhePagamento).ToList() };
        }

        private static DFeNFe.DetPag MapearDetalhePagamento(pag origem)
        {
            var temCartao = origem.tpIntegra != 0;
            return new DFeNFe.DetPag
            {
                IndPag = origem.indPag == TpcnIndicadorPagamento.ipNone ? null : (DFeService.IndicadorPagamento?)(int)origem.indPag,
                TPag = (DFeService.MeioPagamento)(int)origem.tPag,
                XPag = origem.tPag == TpcnFormaPagamento.fpOutro ? VazioParaNulo(origem.xPag) : null,
                VPag = origem.vPag,
                DPag = origem.dPag,
                CNPJPag = VazioParaNulo(origem.CNPJPag),
                UFPag = string.IsNullOrWhiteSpace(origem.UFPag) ? default(DFeService.UFBrasil) : (DFeService.UFBrasil)Enum.Parse(typeof(DFeService.UFBrasil), origem.UFPag),
                Card = temCartao ? new DFeNFe.Card
                {
                    TpIntegra = (DFeService.TipoIntegracaoPagamento)origem.tpIntegra,
                    CNPJ = VazioParaNulo(origem.CNPJ),
                    TBand = origem.tBand == 0 ? null : (DFeService.BandeiraOperadoraCartao?)(int)origem.tBand,
                    CAut = VazioParaNulo(origem.cAut),
                    CNPJReceb = VazioParaNulo(origem.CNPJReceb),
                    IdTermPag = VazioParaNulo(origem.idTermPag)
                } : null
            };
        }

        private static DFeNFe.Vol MapearVolume(Vol origem) => new DFeNFe.Vol
        {
            QVol = origem.qVol,
            Esp = VazioParaNulo(origem.esp),
            Marca = VazioParaNulo(origem.marca),
            NVol = VazioParaNulo(origem.nVol),
            PesoL = origem.pesoL,
            PesoB = origem.pesoB,
            Lacres = origem.Lacres == null || origem.Lacres.Count == 0 ? null : origem.Lacres.Select(x => NFeDFeConventionMapper.Mapear<DFeNFe.Lacres>(x)).ToList()
        };

        private static bool TemTransportador(Transporta x) => !string.IsNullOrWhiteSpace(x.CNPJ) || !string.IsNullOrWhiteSpace(x.CPF) || !string.IsNullOrWhiteSpace(x.xNome);
        private static DFeNFe.Transporta MapearTransportador(Transporta x) => new DFeNFe.Transporta
        {
            CNPJ = VazioParaNulo(x.CNPJ), CPF = VazioParaNulo(x.CPF), XNome = VazioParaNulo(x.xNome), IE = VazioParaNulo(x.IE),
            XEnder = VazioParaNulo(x.xEnder), XMun = VazioParaNulo(x.xMun), UF = string.IsNullOrWhiteSpace(x.UF) ? null : (DFeService.UFBrasil?)ParseUF(x.UF)
        };
        private static DFeService.UFBrasil ParseUF(string uf) => (DFeService.UFBrasil)Enum.Parse(typeof(DFeService.UFBrasil), uf);
        private static bool TemRetencao(retTransp x) => x.vServ + x.vBCRet + x.pICMSRet + x.vICMSRet + x.cMunFG > 0 || !string.IsNullOrWhiteSpace(x.CFOP);
        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;
    }
}
