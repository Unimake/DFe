using System;
using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;
using DFeService = Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeAdditionalMapper
    {
        public DFeNFe.InfIntermed MapearIntermediador(InfIntermed x) => string.IsNullOrWhiteSpace(x.CNPJ) ? null : NFeDFeConventionMapper.Mapear<DFeNFe.InfIntermed>(x);

        public DFeNFe.InfAdic MapearInformacoesAdicionais(InfAdic x)
        {
            var possui = !string.IsNullOrWhiteSpace(x.infAdFisco) || !string.IsNullOrWhiteSpace(x.infCpl) || x.obsCont.Count > 0 || x.obsFisco.Count > 0 || x.procRef.Count > 0;
            if (!possui) return null;
            return new DFeNFe.InfAdic
            {
                InfAdFisco = VazioParaNulo(x.infAdFisco),
                InfCpl = VazioParaNulo(x.infCpl),
                ObsCont = x.obsCont.Count == 0 ? null : x.obsCont.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.ObsCont>(y)).ToList(),
                ObsFisco = x.obsFisco.Count == 0 ? null : x.obsFisco.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.ObsFisco>(y)).ToList(),
                ProcRef = x.procRef.Count == 0 ? null : x.procRef.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.ProcRef>(y)).ToList()
            };
        }

        public DFeNFe.Exporta MapearExportacao(Exporta x) => string.IsNullOrWhiteSpace(x.UFSaidaPais) ? null : NFeDFeConventionMapper.Mapear<DFeNFe.Exporta>(x);
        public DFeNFe.Compra MapearCompra(Compra x) => string.IsNullOrWhiteSpace(x.xNEmp) && string.IsNullOrWhiteSpace(x.xPed) && string.IsNullOrWhiteSpace(x.xCont) ? null : NFeDFeConventionMapper.Mapear<DFeNFe.Compra>(x);

        public DFeNFe.Cana MapearCana(Cana x)
        {
            if (string.IsNullOrWhiteSpace(x.safra) && string.IsNullOrWhiteSpace(x.Ref)) return null;
            return new DFeNFe.Cana
            {
                Safra = x.safra,
                Ref = x.Ref,
                ForDia = x.fordia.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.ForDia>(y)).ToList(),
                QTotMes = x.qTotMes,
                QTotAnt = x.qTotAnt,
                QTotGer = x.qTotGer,
                Deduc = x.deduc.Count == 0 ? null : x.deduc.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.Deduc>(y)).ToList(),
                VFor = x.vFor,
                VTotDed = x.vTotDed,
                VLiqFor = x.vLiqFor
            };
        }

        public DFeNFe.InfRespTec MapearResponsavel(RespTecnico x) => string.IsNullOrWhiteSpace(x.CNPJ) ? null : new DFeNFe.InfRespTec
        {
            CNPJ = x.CNPJ, XContato = x.xContato, Email = x.email, Fone = x.fone,
            IdCSRT = x.idCSRT > 0 ? x.idCSRT.ToString("00") : null,
            HashCSRT = VazioParaNulo(x.hashCSRT)
        };

        public DFeNFe.Agropecuario MapearAgropecuario(Agropecuario x)
        {
            var temGuia = !string.IsNullOrWhiteSpace(x.guiaTransito.nGuia);
            if (x.defensivo.Count == 0 && !temGuia) return null;
            return new DFeNFe.Agropecuario
            {
                Defensivo = x.defensivo.Count == 0 ? null : x.defensivo.Select(y => NFeDFeConventionMapper.Mapear<DFeNFe.Defensivo>(y)).ToList(),
                GuiaTransito = !temGuia ? null : new DFeNFe.GuiaTransito
                {
                    TpGuia = (DFeService.TipoGuiaTransito)(int)x.guiaTransito.tpGuia,
                    UFGuia = string.IsNullOrWhiteSpace(x.guiaTransito.UFGuia) ? DFeService.UFBrasil.NaoDefinido : (DFeService.UFBrasil)Enum.Parse(typeof(DFeService.UFBrasil), x.guiaTransito.UFGuia),
                    SerieGuia = VazioParaNulo(x.guiaTransito.serieGuia),
                    NGuia = x.guiaTransito.nGuia
                }
            };
        }

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;
    }
}
