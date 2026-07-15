using System;
using System.Collections.Generic;
using System.Linq;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;
using DFeService = Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe.Txt.Mapping
{
    internal sealed class NFeDFePartyMapper
    {
        private const string NomeHomologacao = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL";

        public DFeNFe.Emit MapearEmitente(Emit origem) => new DFeNFe.Emit
        {
            CNPJ = VazioParaNulo(origem.CNPJ),
            CPF = VazioParaNulo(origem.CPF),
            XNome = origem.xNome,
            XFant = VazioParaNulo(origem.xFant),
            EnderEmit = new DFeNFe.EnderEmit
            {
                XLgr = origem.enderEmit.xLgr,
                Nro = origem.enderEmit.nro,
                XCpl = VazioParaNulo(origem.enderEmit.xCpl),
                XBairro = origem.enderEmit.xBairro,
                CMun = origem.enderEmit.cMun,
                XMun = origem.enderEmit.xMun,
                UF = ParseUF(origem.enderEmit.UF),
                CEP = origem.enderEmit.CEP > 0 ? origem.enderEmit.CEP.ToString("00000000") : null,
                CPais = origem.enderEmit.cPais,
                XPais = VazioParaNulo(origem.enderEmit.xPais),
                Fone = VazioParaNulo(origem.enderEmit.fone)
            },
            IE = origem.IE,
            IEST = VazioParaNulo(origem.IEST),
            IM = VazioParaNulo(origem.IM),
            CNAE = VazioParaNulo(origem.CNAE),
            CRT = (DFeService.CRT)(int)origem.CRT
        };

        public DFeNFe.Dest MapearDestinatario(NFe nota)
        {
            var origem = nota.dest;
            var deveGerar = nota.ide.mod == TpcnMod.modNFe ||
                !string.IsNullOrWhiteSpace(origem.CNPJ) ||
                !string.IsNullOrWhiteSpace(origem.CPF) ||
                !string.IsNullOrWhiteSpace(origem.idEstrangeiro) ||
                !string.IsNullOrWhiteSpace(origem.xNome);

            if (!deveGerar)
            {
                return null;
            }

            var destino = new DFeNFe.Dest
            {
                CNPJ = VazioParaNulo(origem.CNPJ),
                CPF = VazioParaNulo(origem.CPF),
                IdEstrangeiro = origem.idEstrangeiro == "NAO GERAR TAG" ? string.Empty : VazioParaNulo(origem.idEstrangeiro),
                XNome = nota.ide.tpAmb == DFeService.TipoAmbiente.Producao ? origem.xNome : NomeHomologacao,
                IndIEDest = (DFeService.IndicadorIEDestinatario)(int)origem.indIEDest,
                IE = VazioParaNulo(origem.IE),
                ISUF = VazioParaNulo(origem.ISUF),
                IM = VazioParaNulo(origem.IM),
                Email = VazioParaNulo(origem.email)
            };

            if (nota.ide.mod == TpcnMod.modNFe || !string.IsNullOrWhiteSpace(origem.enderDest.xLgr))
            {
                destino.EnderDest = new DFeNFe.EnderDest
                {
                    XLgr = origem.enderDest.xLgr,
                    Nro = origem.enderDest.nro,
                    XCpl = VazioParaNulo(origem.enderDest.xCpl),
                    XBairro = origem.enderDest.xBairro,
                    CMun = origem.enderDest.cMun,
                    XMun = origem.enderDest.xMun,
                    UF = ParseUF(origem.enderDest.UF),
                    CEP = origem.enderDest.CEP > 0 ? origem.enderDest.CEP.ToString("00000000") : null,
                    CPais = origem.enderDest.cPais,
                    XPais = VazioParaNulo(origem.enderDest.xPais),
                    Fone = VazioParaNulo(origem.enderDest.fone)
                };
            }

            return destino;
        }

        public DFeNFe.Retirada MapearRetirada(Retirada origem) => string.IsNullOrWhiteSpace(origem.xLgr)
            ? null
            : PreencherLocal(new DFeNFe.Retirada(), origem.CNPJ, origem.CPF, origem.xNome, origem.xLgr, origem.nro,
                origem.xCpl, origem.xBairro, origem.cMun, origem.xMun, origem.UF, origem.CEP, origem.cPais,
                origem.xPais, origem.fone, origem.email, origem.IE);

        public DFeNFe.Entrega MapearEntrega(Entrega origem) => string.IsNullOrWhiteSpace(origem.xLgr)
            ? null
            : PreencherLocal(new DFeNFe.Entrega(), origem.CNPJ, origem.CPF, origem.xNome, origem.xLgr, origem.nro,
                origem.xCpl, origem.xBairro, origem.cMun, origem.xMun, origem.UF, origem.CEP, origem.cPais,
                origem.xPais, origem.fone, origem.email, origem.IE);

        public List<DFeNFe.AutXML> MapearAutorizados(List<autXML> origem)
        {
            if (origem == null || origem.Count == 0)
            {
                return null;
            }

            return origem.Select(x => new DFeNFe.AutXML
            {
                CNPJ = VazioParaNulo(x.CNPJ),
                CPF = VazioParaNulo(x.CPF)
            }).ToList();
        }

        private static T PreencherLocal<T>(T destino, string cnpj, string cpf, string xNome, string xLgr, string nro,
            string xCpl, string xBairro, int cMun, string xMun, string uf, string cep, int cPais, string xPais,
            string fone, string email, string ie) where T : DFeNFe.LocalBase
        {
            destino.CNPJ = VazioParaNulo(cnpj);
            destino.CPF = VazioParaNulo(cpf);
            destino.XNome = VazioParaNulo(xNome);
            destino.XLgr = xLgr;
            destino.Nro = nro;
            destino.XCpl = VazioParaNulo(xCpl);
            destino.XBairro = xBairro;
            destino.CMun = cMun;
            destino.XMun = xMun;
            destino.UF = ParseUF(uf);
            destino.CEP = VazioParaNulo(cep);
            destino.CPais = cPais;
            destino.XPais = VazioParaNulo(xPais);
            destino.Fone = VazioParaNulo(fone);
            destino.Email = VazioParaNulo(email);
            destino.IE = VazioParaNulo(ie);
            return destino;
        }

        private static DFeService.UFBrasil ParseUF(string uf) => string.IsNullOrWhiteSpace(uf)
            ? DFeService.UFBrasil.NaoDefinido
            : (DFeService.UFBrasil)Enum.Parse(typeof(DFeService.UFBrasil), uf, true);

        private static string VazioParaNulo(string valor) => string.IsNullOrWhiteSpace(valor) ? null : valor;
    }
}
