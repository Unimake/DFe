using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Dest
    /// </summary>
    internal class Dest
    {
        public string CNPJ;
        public string CPF;
        public string xNome;
        public enderDest enderDest;
        public string IE;
        public string IM;
        public string ISUF;
        public string email;
        public TpcnindIEDest indIEDest;

        /// <summary>
        /// NFC-e
        /// </summary>
        public string idEstrangeiro;

        public Dest()
        {
            this.idEstrangeiro = this.CNPJ = this.CPF = string.Empty;
            this.indIEDest = TpcnindIEDest.inContribuinte;
            this.enderDest = new enderDest();
        }
    }

    /// <summary>
    /// enderDest
    /// </summary>
    internal class enderDest
    {
        public string xLgr;
        public string nro;
        public string xCpl;
        public string xBairro;
        public int cMun;
        public string xMun;
        public string UF;
        public int CEP;
        public int cPais;
        public string xPais;
        public string fone;

        public enderDest()
        {
            this.fone = this.nro = this.UF = this.xBairro = this.xCpl = this.xLgr = this.xMun = this.xPais = "";
        }
    }

}
