using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// procRef
    /// </summary>
    internal class procRef
    {
        public string nProc;
        public string indProc;
        public string tpAto;
    }

    internal class protNFe
    {
        public TipoAmbiente tpAmb;
        public string verAplic;
        public string chNFe;
        public DateTime dhRecbto;
        public string nProt;
        public string digVal;
        public int cStat;
        public string xMotivo;
        public int cMsg { get; set; }
        public string xMsg { get; set; }
    }
}
