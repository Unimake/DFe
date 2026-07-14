using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal class exportInd
    {
        public string nRE;
        public string chNFe;
        public double qExport;
    }

    internal class detExport
    {
        public string nDraw;
        public exportInd exportInd;
        public detExport()
        {
            exportInd = new exportInd();
        }
    }
}
