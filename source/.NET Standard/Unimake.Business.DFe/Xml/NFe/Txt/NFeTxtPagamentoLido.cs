using System;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Dados transitórios de um pagamento lido do TXT.
    /// </summary>
    internal class NFeTxtPagamentoLido
    {
        public TpcnIndicadorPagamento indPag = TpcnIndicadorPagamento.ipNone;
        public TpcnFormaPagamento tPag = TpcnFormaPagamento.fpOutro;
        public string xPag;
        public double vPag;
        public DateTime dPag;
        public string CNPJ;
        public TpcnBandeiraCartao tBand;
        public string cAut;
        public int tpIntegra;
        public string CNPJPag;
        public string UFPag;
        public string CNPJReceb;
        public string idTermPag;

        public NFeTxtPagamentoLido()
        {
            cAut = CNPJ = string.Empty;
        }
    }
}
