using System;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    /// <summary>
    /// Converte o modelo preenchido pelo leitor TXT para o modelo de NFe da Unimake.DFe.
    /// O mapeamento suporta exclusivamente o layout 4.00.
    /// </summary>
    public sealed class NFeDFeMapper
    {
        private readonly NFeDFeIdentificationMapper identificationMapper = new NFeDFeIdentificationMapper();
        private readonly NFeDFePartyMapper partyMapper = new NFeDFePartyMapper();
        private readonly NFeDFeProductMapper productMapper = new NFeDFeProductMapper();
        private readonly NFeDFeTotalMapper totalMapper = new NFeDFeTotalMapper();
        private readonly NFeDFeCommercialMapper commercialMapper = new NFeDFeCommercialMapper();
        private readonly NFeDFeAdditionalMapper additionalMapper = new NFeDFeAdditionalMapper();

        internal DFeNFe.NFe Mapear(NFe origem)
        {
            if (origem == null)
            {
                throw new ArgumentNullException(nameof(origem));
            }

            if (origem.infNFe.Versao != 4m)
            {
                throw new NotSupportedException("Somente o layout 4.00 da NFe/NFCe e suportado.");
            }

            var infNFe = new DFeNFe.InfNFe
            {
                Versao = "4.00",
                Ide = identificationMapper.Mapear(origem.ide),
                Emit = partyMapper.MapearEmitente(origem.emit),
                Dest = partyMapper.MapearDestinatario(origem),
                Retirada = partyMapper.MapearRetirada(origem.retirada),
                Entrega = partyMapper.MapearEntrega(origem.entrega),
                AutXML = partyMapper.MapearAutorizados(origem.autXML),
                Det = productMapper.Mapear(origem),
                Total = totalMapper.Mapear(origem),
                Transp = commercialMapper.MapearTransporte(origem.Transp),
                Cobr = commercialMapper.MapearCobranca(origem.Cobr),
                Pag = commercialMapper.MapearPagamento(origem),
                InfIntermed = additionalMapper.MapearIntermediador(origem.InfIntermed),
                InfAdic = additionalMapper.MapearInformacoesAdicionais(origem.InfAdic),
                Exporta = additionalMapper.MapearExportacao(origem.exporta),
                Compra = additionalMapper.MapearCompra(origem.compra),
                Cana = additionalMapper.MapearCana(origem.cana),
                InfRespTec = additionalMapper.MapearResponsavel(origem.resptecnico),
                Agropecuario = additionalMapper.MapearAgropecuario(origem.agropecuario)
            };

            return new DFeNFe.NFe { InfNFeField = infNFe };
        }
    }
}
