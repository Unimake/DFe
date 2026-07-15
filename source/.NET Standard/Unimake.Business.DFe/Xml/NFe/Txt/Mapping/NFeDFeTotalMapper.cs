using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFeTotalMapper
    {
        public DFeNFe.Total Mapear(NFe nota)
        {
            var temIS = nota.det != null && nota.det.Any(x => !string.IsNullOrWhiteSpace(x.Imposto.IS.CSTIS));
            var temIBSCBS = nota.det != null && nota.det.Any(x => !string.IsNullOrWhiteSpace(x.Imposto.IBSCBS.CST) || !string.IsNullOrWhiteSpace(x.Imposto.IBSCBS.cClassTrib));
            return new DFeNFe.Total
            {
                ICMSTot = NFeDFeConventionMapper.Mapear<DFeNFe.ICMSTot>(nota.Total.ICMSTot),
                ISSQNtot = TemISSQN(nota.Total.ISSQNtot) ? NFeDFeConventionMapper.Mapear<DFeNFe.ISSQNtot>(nota.Total.ISSQNtot) : null,
                RetTrib = TemRetencoes(nota.Total.retTrib) ? NFeDFeConventionMapper.Mapear<DFeNFe.RetTrib>(nota.Total.retTrib) : null,
                ISTot = temIS ? NFeDFeConventionMapper.Mapear<DFeNFe.ISTot>(nota.Total.ISTot) : null,
                IBSCBSTot = temIBSCBS ? MapearIBSCBSTot(nota.Total.IBSCBSTot) : null,
                VNFTot = nota.Total.vNFTot
            };
        }

        private static DFeNFe.IBSCBSTot MapearIBSCBSTot(IBSCBSTot origem) => new DFeNFe.IBSCBSTot
        {
            VBCIBSCBS = origem.vBCIBSCBS,
            GIBS = new DFeNFe.GIBSTot
            {
                GIBSUF = NFeDFeConventionMapper.Mapear<DFeNFe.GIBSUFTot>(origem.gIBS.gIBSUF),
                GIBSMun = NFeDFeConventionMapper.Mapear<DFeNFe.GIBSMunTot>(origem.gIBS.gIBSMun),
                VIBS = origem.gIBS.vIBS,
                VCredPres = origem.gIBS.vCredPres,
                VCredPresCondSus = origem.gIBS.vCredPresCondSus
            },
            GCBS = NFeDFeConventionMapper.Mapear<DFeNFe.GCBSTot>(origem.gCBS),
            GMono = TemMono(origem.gMono) ? NFeDFeConventionMapper.Mapear<DFeNFe.GMono>(origem.gMono) : null,
            GEstornoCred = origem.gEstornoCred != null && origem.gEstornoCred.vIBSEstCred + origem.gEstornoCred.vCBSEstCred > 0
                ? NFeDFeConventionMapper.Mapear<DFeNFe.GEstornoCred>(origem.gEstornoCred) : null
        };

        private static bool TemISSQN(ISSQNtot origem) => origem.vServ + origem.vBC + origem.vISS + origem.vPIS + origem.vCOFINS > 0;
        private static bool TemRetencoes(retTrib origem) => origem.vRetPIS + origem.vRetCOFINS + origem.vRetCSLL + origem.vBCIRRF + origem.vIRRF + origem.vBCRetPrev + origem.vRetPrev > 0;
        private static bool TemMono(GMonoTot origem) => origem.vIBSMono + origem.vCBSMono + origem.vIBSMonoReten + origem.vCBSMonoReten + origem.vIBSMonoRet + origem.vCBSMonoRet > 0;
    }
}
