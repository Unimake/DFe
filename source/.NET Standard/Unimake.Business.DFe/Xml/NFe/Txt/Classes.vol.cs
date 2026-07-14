using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Vol
    /// </summary>
    internal class Vol
    {
        public int qVol;
        public string esp;
        public string marca;
        public string nVol;
        public double pesoL;
        public double pesoB;
        public List<Lacres> Lacres;

        public Vol()
        {
            this.Lacres = new List<Lacres>();
        }
    }
}
