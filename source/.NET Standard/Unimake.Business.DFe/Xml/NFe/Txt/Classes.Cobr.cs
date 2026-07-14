using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Cobr
    /// </summary>
    internal class Cobr
    {
        public Fat Fat;
        public List<Dup> Dup;

        public Cobr()
        {
            Dup = new List<Dup>();
        }
    }

    /// <summary>
    /// Dup
    /// </summary>
    internal class Dup
    {
        public string nDup;
        public DateTime dVenc;
        public double vDup;
    }

    /// <summary>
    /// Fat
    /// </summary>
    internal struct Fat
    {
        public string nFat;
        public double vOrig;
        public double vDesc;
        public double vLiq;
    }

}
