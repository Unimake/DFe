using System;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Infraestrutura
{
    public class StatusProtocoloAutorizacaoTest
    {
        [Fact]
        public void NFeAceitaSomenteOsProtocolosDeAutorizacao()
        {
            Assert.True(StatusProtocoloAutorizacao.NFe(100));
            Assert.True(StatusProtocoloAutorizacao.NFe(120));
            Assert.True(StatusProtocoloAutorizacao.NFe(150));
            Assert.False(StatusProtocoloAutorizacao.NFe(110));
            Assert.False(StatusProtocoloAutorizacao.NFe(132));
            Assert.False(StatusProtocoloAutorizacao.NFe(999));
        }

        [Fact]
        public void MDFeAceitaSomenteOProtocolo100()
        {
            Assert.True(StatusProtocoloAutorizacao.MDFe(100));
            Assert.False(StatusProtocoloAutorizacao.MDFe(110));
            Assert.False(StatusProtocoloAutorizacao.MDFe(132));
            Assert.False(StatusProtocoloAutorizacao.MDFe(150));
            Assert.False(StatusProtocoloAutorizacao.MDFe(999));
        }

        [Fact]
        public void DemaisDocumentosAceitamSomenteProtocolos100E150()
        {
            var classificadores = new Func<int, bool>[]
            {
                StatusProtocoloAutorizacao.BPe,
                StatusProtocoloAutorizacao.CTe,
                StatusProtocoloAutorizacao.DCe,
                StatusProtocoloAutorizacao.NF3e,
                StatusProtocoloAutorizacao.NFCom,
                StatusProtocoloAutorizacao.NFGas
            };

            foreach (var classificar in classificadores)
            {
                Assert.True(classificar(100));
                Assert.True(classificar(150));
                Assert.False(classificar(110));
                Assert.False(classificar(120));
                Assert.False(classificar(132));
                Assert.False(classificar(999));
            }
        }
    }
}
