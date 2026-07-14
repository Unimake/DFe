using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Det
    /// </summary>
    internal class Det
    {
        public Prod Prod;
        public Imposto Imposto;
        public impostoDevol impostoDevol;
        public ObsItem ObsItem;
        public string infAdProd;
        public double vItem;
        public DFeReferenciado DfeReferenciado;

        public Det()
        {
            Prod = new Prod();
            Imposto = new Imposto();
            impostoDevol = new impostoDevol();
            ObsItem = new ObsItem();
            DfeReferenciado = new DFeReferenciado();
        }
    }

}
