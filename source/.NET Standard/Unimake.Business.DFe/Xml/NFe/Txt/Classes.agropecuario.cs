using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal class Agropecuario
    {
        public List<Defensivo> defensivo;
        public GuiaTransito guiaTransito;

        public Agropecuario() 
        {
            defensivo = new List<Defensivo>();
            guiaTransito = new GuiaTransito();
        }
    }

    internal struct Defensivo
    {
        public string nReceituario { get; set; }
        public string CPFResptec { get; set; }
    }

    internal struct GuiaTransito
    {
        public TpcnTipoGuia tpGuia { get; set; }
        public string UFGuia { get; set; }
        public string serieGuia { get; set; }
        public string nGuia { get; set; }
    }
}
