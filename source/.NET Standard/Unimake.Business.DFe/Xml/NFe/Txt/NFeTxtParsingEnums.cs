namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal enum TpcnFormaPagamento
    {
        fpDinheiro = 1,
        fpCheque = 2,
        fpCartaoCredito = 3,
        fpCartaoDebito = 4,
        fpCreditoLoja = 5,
        fpValeAlimentacao = 10,
        fpValeRefeicao = 11,
        fpValePresente = 12,
        fpValeCombustivel = 13,
        fpBoletoBancario = 15,
        DepositoBancario = 16,
        PagamentoInstantaneoPIX = 17,
        TransferenciaBancaria = 18,
        ProgramaFidelidade = 19,
        fpSemPagamento = 90,
        fpOutro = 99
    }

    internal enum TpcnBandeiraCartao
    {
        bcVisa = 1,
        bcMasterCard = 2,
        bcAmericanExpress = 3,
        bcSorocred = 4,
        bcDinersClub = 5,
        bcElo = 6,
        bcHipercard = 7,
        bcAura = 8,
        bcCabal = 9,
        bcOutros = 99
    }

    internal enum TpcnTipoCampo
    {
        tcDouble2 = 2,
        tcDouble3 = 3,
        tcDouble4 = 4,
        tcDouble5 = 5,
        tcDouble6 = 6,
        tcDouble7 = 7,
        tcDouble8 = 8,
        tcDouble9 = 9,
        tcDouble10 = 10,
        tcStr,
        tcInt,
        tcDatYYYY_MM_DD,
        tcDatYYYYMMDD,
        tcHor,
        tcDatHor,
        tcDec4,
        tcDec10
    }

    internal enum TpcnIndicadorPagamento
    {
        ipNone = 100,
        ipVista = 0,
        ipPrazo = 1
    }

    internal enum ObOp
    {
        Obrigatorio,
        Opcional,
        None
    }
}
