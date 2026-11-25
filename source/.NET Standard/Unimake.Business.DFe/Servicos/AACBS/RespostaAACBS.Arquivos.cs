using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Text.Json;
using Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    internal static partial class RespostaAACBS
    {
        internal static RetBaixarArquivoRetorno ArquivoCreditos(JsonElement json, HttpStatusCode status, Xml.AACBS.BaixarArquivoRetorno envio)
        {
            var obj = CabecalhoArquivo(json, status, envio, true);
            return new RetBaixarArquivoRetorno
            {
                ArquivoCreditos = new RetArquivoCreditos
                {
                    TiqueteSolicitacao = envio.TiqueteSolicitacao,
                    Ni = obj.Texto("ni"),
                    NiConsumidor = DigitosArquivo(obj, "niConsumidor", 11, 14),
                    GeradoEmField = obj.Instante("geradoEm"),
                    Apuracao = obj.Lista("apuracao", PeriodoCreditos)
                }
            };
        }

        internal static RetBaixarArquivoRetorno ArquivoPagamentos(JsonElement json, HttpStatusCode status, Xml.AACBS.BaixarArquivoRetorno envio)
        {
            var obj = CabecalhoArquivo(json, status, envio);
            return new RetBaixarArquivoRetorno
            {
                ArquivoPagamentos = new RetArquivoPagamentos
                {
                    TiqueteSolicitacao = envio.TiqueteSolicitacao,
                    Ni = envio.Cnpj,
                    NiConsumidor = DigitosArquivo(obj, "niConsumidor", 11, 14),
                    GeradoEmField = obj.Instante("geradoEm"),
                    Apuracao = obj.Lista("apuracao", DataPagamentos)
                }
            };
        }

        internal static RetBaixarArquivoRetorno ArquivoRecolhimentos(JsonElement json, HttpStatusCode status, Xml.AACBS.BaixarArquivoRetorno envio)
        {
            var obj = CabecalhoArquivo(json, status, envio);
            return new RetBaixarArquivoRetorno
            {
                ArquivoRecolhimentos = new RetArquivoRecolhimentos
                {
                    TiqueteSolicitacao = envio.TiqueteSolicitacao,
                    Ni = envio.Cnpj,
                    NiConsumidor = DigitosArquivo(obj, "niConsumidor", 11, 14),
                    GeradoEmField = obj.Instante("geradoEm"),
                    Apuracao = obj.Lista("apuracao", DataRecolhimentos)
                }
            };
        }

        private static Objeto CabecalhoArquivo(JsonElement json, HttpStatusCode status, Xml.AACBS.BaixarArquivoRetorno envio, bool niCompleto = false)
        {
            if (status != HttpStatusCode.OK) throw new CampoInvalidoException("$");
            var obj = new Objeto(json, "$");
            obj.Aceitar("tiqueteSolicitacao", "ni", "niConsumidor", "geradoEm", "apuracao");
            var tiquete = obj.Texto("tiqueteSolicitacao");
            if (tiquete.Length == 0 || !string.Equals(tiquete, envio.TiqueteSolicitacao, StringComparison.Ordinal))
                throw obj.Invalido("tiqueteSolicitacao");
            var ni = DigitosArquivo(obj, "ni", 8, niCompleto ? 14 : (int?)null);
            if (!string.Equals(ni.Substring(0, 8), envio.Cnpj, StringComparison.Ordinal))
                throw obj.Invalido("ni");
            return obj;
        }

        private static string DigitosArquivo(Objeto obj, string nome, int tamanho, int? tamanhoAlternativo = null)
        {
            var texto = obj.Texto(nome);
            if ((texto.Length != tamanho && texto.Length != tamanhoAlternativo) || texto.Any(c => c < '0' || c > '9'))
                throw obj.Invalido(nome);
            return texto;
        }

        private static string ChaveArquivo(Objeto obj, string nome)
        {
            var chave = obj.Texto(nome);
            if (chave.Length == 0 || chave.Count(c => !char.IsLowSurrogate(c)) > 100)
                throw obj.Invalido(nome);
            return chave;
        }

        private static string PaArquivo(Objeto obj)
        {
            var pa = obj.Texto("pa");
            if (!PeriodoApuracao.IsMatch(pa)) throw obj.Invalido("pa");
            return pa;
        }

        private static string DataCivilArquivo(Objeto obj, string nome)
        {
            var data = obj.Texto(nome);
            if (!DateTime.TryParseExact(data, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out _))
                throw obj.Invalido(nome);
            return data;
        }

        private static List<T> ListaObrigatoriaArquivo<T>(Objeto obj, string nome, Func<Objeto, T> ler)
        {
            var itens = obj.Lista(nome, ler);
            if (itens.Count == 0) throw obj.Invalido(nome);
            return itens;
        }

        private static int SequencialArquivo(Objeto obj)
        {
            var sequencial = obj.InteiroDoSchema("sequencial");
            if (sequencial < 1 || sequencial > 99999999) throw obj.Invalido("sequencial");
            return sequencial;
        }

        private static ApuracaoCreditos PeriodoCreditos(Objeto periodo)
        {
            periodo.Aceitar("pa", "creditos");
            return new ApuracaoCreditos
            {
                Pa = PaArquivo(periodo),
                Creditos = ListaObrigatoriaArquivo(periodo, "creditos", Credito)
            };
        }

        private static CreditoArquivo Credito(Objeto credito)
        {
            credito.Aceitar("origem", "documento", "chave", "emissao", "registro", "atualizacao", "cbs");
            var origem = credito.InteiroDoSchema("origem");
            var documento = credito.InteiroDoSchema("documento");
            if (!OrigensDebito.Contains(origem)) throw credito.Invalido("origem");
            if (!DocumentosDebito.Contains(documento)) throw credito.Invalido("documento");
            return new CreditoArquivo
            {
                Origem = origem,
                Documento = documento,
                Chave = ChaveArquivo(credito, "chave"),
                EmissaoField = credito.Instante("emissao"),
                RegistroField = credito.Instante("registro"),
                AtualizacaoField = credito.Instante("atualizacao"),
                Cbs = ValoresCredito(credito.Grupo("cbs"))
            };
        }

        private static CbsCredito ValoresCredito(Objeto cbs)
        {
            cbs.Aceitar("excedente", "apurado", "apropriacao");
            return new CbsCredito
            {
                Excedente = cbs.ValorOpcional("excedente"),
                Apurado = cbs.Valor("apurado"),
                Apropriacao = Apropriacao(cbs.Grupo("apropriacao"))
            };
        }

        private static ApropriacaoCredito Apropriacao(Objeto apropriacao)
        {
            apropriacao.Aceitar("inapropriavel", "suspenso", "prescrito", "aApropriar", "apropriado", "utilizacao");
            return new ApropriacaoCredito
            {
                Inapropriavel = apropriacao.ValorOpcional("inapropriavel"),
                Suspenso = apropriacao.ValorOpcional("suspenso"),
                Prescrito = apropriacao.ValorOpcional("prescrito"),
                AApropriar = apropriacao.ValorOpcional("aApropriar"),
                Apropriado = apropriacao.ValorOpcional("apropriado"),
                Utilizacao = Utilizacao(apropriacao.Grupo("utilizacao"))
            };
        }

        private static UtilizacaoCredito Utilizacao(Objeto utilizacao)
        {
            utilizacao.Aceitar("inutilizavel", "utilizado", "restabelecido", "naoUtilizado");
            var naoUtilizado = utilizacao.Grupo("naoUtilizado");
            naoUtilizado.Aceitar("saldoCredor", "pedidoRessarcimento");
            return new UtilizacaoCredito
            {
                Inutilizavel = utilizacao.ValorOpcional("inutilizavel"),
                Utilizado = utilizacao.ValorOpcional("utilizado"),
                Restabelecido = utilizacao.ValorOpcional("restabelecido"),
                NaoUtilizado = new NaoUtilizadoCredito
                {
                    SaldoCredor = naoUtilizado.Valor("saldoCredor"),
                    PedidoRessarcimento = naoUtilizado.ValorOpcional("pedidoRessarcimento")
                }
            };
        }

        private static ApuracaoPagamentos DataPagamentos(Objeto apuracao)
        {
            apuracao.Aceitar("dataArrecadacao", "pagamentos");
            return new ApuracaoPagamentos
            {
                DataArrecadacao = DataCivilArquivo(apuracao, "dataArrecadacao"),
                Pagamentos = ListaObrigatoriaArquivo(apuracao, "pagamentos", Pagamento)
            };
        }

        private static PagamentoArquivo Pagamento(Objeto pagamento)
        {
            pagamento.Aceitar("numeroDARF", "tipo", "niAdquirente", "niContribuinte", "composicao");
            var tipo = pagamento.InteiroDoSchema("tipo");
            if (tipo != 1 && tipo != 2) throw pagamento.Invalido("tipo");
            var rad = tipo == 1;
            var identidadeVedada = rad ? "niContribuinte" : "niAdquirente";
            if (pagamento.Tem(identidadeVedada)) throw pagamento.Invalido(identidadeVedada);
            return new PagamentoArquivo
            {
                NumeroDarf = DigitosArquivo(pagamento, "numeroDARF", 17),
                Tipo = tipo,
                NiAdquirente = rad ? DigitosArquivo(pagamento, "niAdquirente", 14) : null,
                NiContribuinte = rad ? null : DigitosArquivo(pagamento, "niContribuinte", 14),
                Composicao = ListaObrigatoriaArquivo(pagamento, "composicao", item => ComposicaoPagamento(item, rad))
            };
        }

        private static ComposicaoPagamentoArquivo ComposicaoPagamento(Objeto item, bool rad)
        {
            item.Aceitar("sequencial", "pa", "vencimento", "niContribuinte", "chaveDFE", "principal", "multa", "juros", "total");
            var temContribuinte = item.Tem("niContribuinte");
            var temChave = item.Tem("chaveDFE");
            if (!rad && temContribuinte) throw item.Invalido("niContribuinte");
            if (!rad && temChave) throw item.Invalido("chaveDFE");
            // O layout RAD aceita data civil na tabela e instante com fuso no schema.
            var vencimentoComFuso = rad && item.Texto("vencimento").IndexOf('T') >= 0;
            return new ComposicaoPagamentoArquivo
            {
                Sequencial = SequencialArquivo(item),
                Pa = PaArquivo(item),
                Vencimento = vencimentoComFuso ? item.Instante("vencimento") : DataCivilArquivo(item, "vencimento"),
                NiContribuinte = temContribuinte ? DigitosArquivo(item, "niContribuinte", 14) : null,
                ChaveDFe = temChave ? ChaveArquivo(item, "chaveDFE") : null,
                Principal = item.Valor("principal"),
                Multa = item.Valor("multa"),
                Juros = item.Valor("juros"),
                Total = item.Valor("total")
            };
        }

        private static ApuracaoRecolhimentos DataRecolhimentos(Objeto apuracao)
        {
            apuracao.Aceitar("dataArrecadacao", "pagamentos");
            return new ApuracaoRecolhimentos
            {
                DataArrecadacao = DataCivilArquivo(apuracao, "dataArrecadacao"),
                Pagamentos = ListaObrigatoriaArquivo(apuracao, "pagamentos", Recolhimento)
            };
        }

        private static RecolhimentoArquivo Recolhimento(Objeto pagamento)
        {
            pagamento.Aceitar("numeroDARF", "tipo", "composicao");
            var tipo = pagamento.InteiroDoSchema("tipo");
            if (tipo != 1) throw pagamento.Invalido("tipo");
            return new RecolhimentoArquivo
            {
                NumeroDarf = DigitosArquivo(pagamento, "numeroDARF", 17),
                Tipo = tipo,
                Composicao = ListaObrigatoriaArquivo(pagamento, "composicao", ComposicaoRecolhimento)
            };
        }

        private static ComposicaoRecolhimentoArquivo ComposicaoRecolhimento(Objeto item)
        {
            item.Aceitar("sequencial", "pa", "vencimento", "niFornecedor", "chaveDFE", "principal", "multa", "juros", "total");
            return new ComposicaoRecolhimentoArquivo
            {
                Sequencial = SequencialArquivo(item),
                Pa = PaArquivo(item),
                Vencimento = DataCivilArquivo(item, "vencimento"),
                NiFornecedor = DigitosArquivo(item, "niFornecedor", 14),
                ChaveDFe = item.Tem("chaveDFE") ? ChaveArquivo(item, "chaveDFE") : null,
                Principal = item.Valor("principal"),
                Multa = item.Valor("multa"),
                Juros = item.Valor("juros"),
                Total = item.Valor("total")
            };
        }
    }
}
