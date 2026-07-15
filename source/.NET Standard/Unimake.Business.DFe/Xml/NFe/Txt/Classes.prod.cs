using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Prod
    /// </summary>
    internal class Prod
    {
        public string cProd;
        public int nItem;
        public string cEAN;
        public string cBarra;
        public string xProd;
        public string NCM;
        public string NVE;
        public string tpCredPresIBSZFM;
        public string EXTIPI;
        public string CFOP;
        public string uCom;
        public decimal qCom;
        public decimal vUnCom;
        public TpcnTipoCampo vUnCom_Tipo;
        public double vProd;
        public string cEANTrib;
        public string cBarraTrib;
        public string uTrib;
        public decimal qTrib;
        public decimal vUnTrib;
        public TpcnTipoCampo vUnTrib_Tipo;
        public double vFrete;
        public double vSeg;
        public double vDesc;
        public double vOutro;
        public TpcnIndicadorTotal indTot;
        public TpcnIndicadorBemMovelUsado indBemMovelUsado;
        public string xPed;
        public string nItemPed;
        public string nFCI;
        public List<DI> DI;
        public veicProd veicProd;
        public List<Med> med;
        public List<Arma> arma;
        public Comb comb;
        public string nRECOPI;
        public List<detExport> detExport;
        public int CEST;
        public List<CredPresumido> credPresumido;
        public TpcnIndicadorEscala indEscala { get; set; }
        public string CNPJFab { get; set; }
        public string cBenef { get; set; }
        public List<Rastro> rastro { get; set; }

        public Prod()
        {
            vUnCom_Tipo = TpcnTipoCampo.tcDouble10;
            vUnTrib_Tipo = TpcnTipoCampo.tcDouble10;
            indTot = TpcnIndicadorTotal.itSomaTotalNFe;
            DI = new List<DI>();
            med = new List<Med>();
            arma = new List<Arma>();
            detExport = new List<detExport>();
            rastro = new List<Rastro>();
            credPresumido = new List<CredPresumido>();
        }
    }
}
