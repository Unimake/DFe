using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net;
using System.Text.Json;
using System.Xml;
using Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    internal static partial class RespostaAACBS
    {
        private const string DarfGerado = "DARF_GERADO";
        private const string DadosDeEntradaInvalidos = "DADOS_DE_ENTRADA_INVALIDOS";

        internal static RetGerarDarfAdquirente DarfAdquirente(JsonElement json, HttpStatusCode status)
        {
            var objeto = new Objeto(json, "$");
            var codigo = CodigoDarf(objeto, status);
            var retorno = new RetGerarDarfAdquirente { CdRetorno = codigo };
            if (codigo != DarfGerado)
            {
                var erro = ErroDarf(json, objeto, codigo);
                retorno.Mensagem = erro.Mensagem;
                retorno.DadosInvalidos = erro.DadosInvalidos;
                return retorno;
            }

            var darf = DadosDarf(objeto);
            var pagamento = objeto.Grupo("informacoesPagamento");
            pagamento.Aceitar("dataVencimento", "dataMaximaPagamento", "valorDarfPrincipal", "valorDarfTotal");
            var data = DataPagamentoDarf(pagamento);
            retorno.Darf = new GerarDarfAdquirenteDadosDarf
            {
                ChavePIX = TextoDarf(darf, "chavePIX"),
                CodigoBarras = TextoDarf(darf, "codigoBarras"),
                LinkDarfPDF = LinkDarf(darf)
            };
            retorno.InformacoesPagamento = new GerarDarfAdquirenteInformacoesPagamento
            {
                DataMaximaPagamento = data.Maxima,
                DataVencimento = data.Vencimento,
                ValorDarfPrincipal = pagamento.Valor("valorDarfPrincipal"),
                ValorDarfTotal = pagamento.Valor("valorDarfTotal")
            };
            return retorno;
        }

        internal static RetGerarDarfContribuinte DarfContribuinte(JsonElement json, HttpStatusCode status)
        {
            var objeto = new Objeto(json, "$");
            var codigo = CodigoDarf(objeto, status);
            var retorno = new RetGerarDarfContribuinte { CdRetorno = codigo };
            if (codigo != DarfGerado)
            {
                var erro = ErroDarf(json, objeto, codigo);
                retorno.Mensagem = erro.Mensagem;
                retorno.DadosInvalidos = erro.DadosInvalidos;
                return retorno;
            }

            var darf = DadosDarf(objeto);
            var pagamento = objeto.Grupo("informacoesPagamento");
            pagamento.Aceitar("dataVencimento", "dataMaximaPagamento", "valorDarfPrincipal",
                "valorDarfMulta", "valorDarfJuros", "valorDarfTotal");
            var data = DataPagamentoDarf(pagamento);
            retorno.Darf = new GerarDarfContribuinteDadosDarf
            {
                ChavePIX = TextoDarf(darf, "chavePIX"),
                CodigoBarras = TextoDarf(darf, "codigoBarras"),
                LinkDarfPDF = LinkDarf(darf)
            };
            retorno.InformacoesPagamento = new GerarDarfContribuinteInformacoesPagamento
            {
                DataMaximaPagamento = data.Maxima,
                DataVencimento = data.Vencimento,
                ValorDarfPrincipal = pagamento.Valor("valorDarfPrincipal"),
                ValorDarfMulta = pagamento.Valor("valorDarfMulta"),
                ValorDarfJuros = pagamento.Valor("valorDarfJuros"),
                ValorDarfTotal = pagamento.Valor("valorDarfTotal")
            };
            return retorno;
        }

        private static string CodigoDarf(Objeto objeto, HttpStatusCode status)
        {
            var codigo = objeto.Texto("cdRetorno");
            switch (codigo)
            {
                case DarfGerado:
                    if (status != HttpStatusCode.OK) throw objeto.Invalido("cdRetorno");
                    break;

                case "DFE_NAO_ENCONTRADO":
                case "VALOR_PARCIAL_SUPERIOR_DFE":
                case "CNPJ_NAO_ADQUIRENTE":
                case DadosDeEntradaInvalidos:
                    if (status != HttpStatusCode.OK && !FalhaHttp(status)) throw objeto.Invalido("cdRetorno");
                    break;

                default:
                    throw objeto.Invalido("cdRetorno");
            }
            return codigo;
        }

        private static Objeto DadosDarf(Objeto objeto)
        {
            objeto.Aceitar("cdRetorno", "darf", "informacoesPagamento");
            var darf = objeto.Grupo("darf");
            darf.Aceitar("chavePIX", "codigoBarras", "linkDarfPDF");
            return darf;
        }

        private static string TextoDarf(Objeto objeto, string nome)
        {
            var texto = objeto.Texto(nome);
            if (texto.Length == 0) throw objeto.Invalido(nome);
            return texto;
        }

        private static string LinkDarf(Objeto objeto)
        {
            var link = TextoDarf(objeto, "linkDarfPDF");
            if (!Uri.TryCreate(link, UriKind.Absolute, out var uri) || !uri.IsWellFormedOriginalString())
                throw objeto.Invalido("linkDarfPDF");
            return link;
        }

        private static (string Maxima, string Vencimento) DataPagamentoDarf(Objeto pagamento)
        {
            // O HTML e o schema publicam nomes diferentes para a mesma escolha de data civil.
            var maxima = pagamento.Tem("dataMaximaPagamento");
            var vencimento = pagamento.Tem("dataVencimento");
            if (maxima == vencimento) throw pagamento.Invalido("dataVencimento");

            var nome = maxima ? "dataMaximaPagamento" : "dataVencimento";
            var data = pagamento.Texto(nome);
            if (!DateTime.TryParseExact(data, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out _))
                throw pagamento.Invalido(nome);
            if (maxima) return (data, null);
            return (null, data);
        }

        private static (string Mensagem, List<string> DadosInvalidos) ErroDarf(JsonElement json, Objeto objeto, string codigo)
        {
            objeto.Aceitar("cdRetorno", "mensagem", "dadosInvalidos");
            var mensagem = TextoDarf(objeto, "mensagem");
            if (!json.TryGetProperty("dadosInvalidos", out var invalidos))
            {
                if (codigo == DadosDeEntradaInvalidos) throw objeto.Invalido("dadosInvalidos");
                return (mensagem, null);
            }
            if (invalidos.ValueKind != JsonValueKind.Array || invalidos.GetArrayLength() == 0)
                throw objeto.Invalido("dadosInvalidos");

            var itens = new List<string>();
            foreach (var item in invalidos.EnumerateArray())
            {
                if (item.ValueKind != JsonValueKind.String)
                    throw objeto.Invalido("dadosInvalidos[]");
                string texto;
                try { texto = XmlConvert.VerifyXmlChars(item.GetString()); }
                catch (XmlException) { throw objeto.Invalido("dadosInvalidos[]"); }
                catch (InvalidOperationException) { throw objeto.Invalido("dadosInvalidos[]"); }
                if (texto.Length == 0) throw objeto.Invalido("dadosInvalidos[]");
                itens.Add(texto);
            }
            return (mensagem, itens);
        }
    }
}
