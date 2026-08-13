using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal static class EFreteMapper
    {
        internal static string CriarJsonLogin(Configuracao configuracao)
        {
            return new JObject
            {
                ["Senha"] = configuracao.EFreteSenha,
                ["Usuario"] = configuracao.EFreteUsuario,
                ["Integrador"] = configuracao.EFreteIntegrador,
                ["Versao"] = 1
            }.ToString(Newtonsoft.Json.Formatting.None);
        }

        internal static string ObterTokenLogin(string retorno)
        {
            var objeto = string.IsNullOrWhiteSpace(retorno) ? new JObject() : JObject.Parse(retorno);
            return objeto.Properties().FirstOrDefault(x => string.Equals(x.Name, "Token", StringComparison.OrdinalIgnoreCase))?.Value?.ToString();
        }

        internal static string CriarJson(XMLBase xml, Servico servico, Configuracao configuracao)
        {
            JObject payload;
            switch (servico)
            {
                case Servico.CIOTDeclaracaoOperacaoTransporte:
                    payload = CriarDeclaracao((Xml.CIOT.DeclaracaoOperacaoTransporte)xml);
                    payload["Versao"] = 8;
                    break;
                case Servico.CIOTConsultarCIOTGerado:
                    payload = CriarConsulta((Xml.CIOT.ConsultarCIOTGerado)xml);
                    payload["Versao"] = 1;
                    break;
                case Servico.CIOTCancelamentoOperacaoTransporte:
                    payload = CriarCancelamento((Xml.CIOT.CancelamentoOperacaoTransporte)xml);
                    payload["Versao"] = 1;
                    break;
                case Servico.CIOTEncerramentoOperacaoTransporte:
                    payload = CriarEncerramento((Xml.CIOT.EncerramentoOperacaoTransporte)xml);
                    payload["Versao"] = 2;
                    break;
                case Servico.CIOTConsultarSituacaoTransportador:
                    var situacao = (Xml.CIOT.ConsultarSituacaoTransportador)xml;
                    payload = CriarSituacao(situacao, situacao.PlacasConsulta);
                    payload["Versao"] = 1;
                    break;
                case Servico.CIOTConsultarFrotaTransportador:
                    var frota = (Xml.CIOT.ConsultarFrotaTransportador)xml;
                    payload = CriarSituacao(frota, frota.Placas);
                    payload["Versao"] = 1;
                    break;
                default:
                    throw new NotSupportedException("O serviço " + servico + " não possui equivalente disponível na integração eFrete.");
            }

            payload["Integrador"] = configuracao.EFreteIntegrador;
            if (!string.IsNullOrWhiteSpace(configuracao.EFreteToken))
            {
                payload["Token"] = configuracao.EFreteToken;
            }

            RemoverNulos(payload);
            return payload.ToString(Newtonsoft.Json.Formatting.None);
        }

        internal static XmlDocument NormalizarRetorno(string retorno, Servico servico)
        {
            var root = string.IsNullOrWhiteSpace(retorno) ? new JObject() : JObject.Parse(retorno);
            var sucesso = ValorBool(root, "Sucesso");
            var erro = !sucesso && Localizar(root, "Sucesso") != null;
            var codigo = Valor(root, "Codigo") ?? Valor(root, "Excecao", "Codigo");
            var mensagem = Valor(root, "Mensagem") ?? Valor(root, "Excecao", "Mensagem");

            XMLBase resultado;
            switch (servico)
            {
                case Servico.CIOTDeclaracaoOperacaoTransporte:
                    resultado = new RetDeclaracaoOperacaoTransporte
                    {
                        IdOperacaoTransporte = Valor(root, "CodigoIdentificacaoOperacao"),
                        Protocolo = Valor(root, "ProtocoloServico"),
                        Codigo = codigo,
                        Mensagem = mensagem,
                        Temp = erro ? CriarTemp(codigo, mensagem) : null
                    };
                    break;
                case Servico.CIOTConsultarCIOTGerado:
                    resultado = new RetConsultarCIOTGerado
                    {
                        CodigoIdentificacaoOperacao = Valor(root, "CodigoIdentificacaoOperacao"),
                        EstadoCIOT = Valor(root, "EstadoCiot"),
                        Protocolo = Valor(root, "ProtocoloServico"),
                        Codigo = erro ? new List<string> { codigo } : null,
                        Mensagem = erro ? new List<string> { mensagem } : null,
                        Temp = erro ? CriarTemp(codigo, mensagem) : null
                    };
                    break;
                case Servico.CIOTCancelamentoOperacaoTransporte:
                    resultado = new RetCancelamentoOperacaoTransporte
                    {
                        CodigoIdentificacaoOperacao = Valor(root, "CodigoIdentificacaoOperacao"),
                        Protocolo = Valor(root, "Protocolo"),
                        Codigo = codigo,
                        Mensagem = mensagem,
                        DataCancelamentoField = Valor(root, "Data"),
                        Temp = erro ? CriarTemp(codigo, mensagem) : null
                    };
                    break;
                case Servico.CIOTEncerramentoOperacaoTransporte:
                    resultado = new RetEncerramentoOperacaoTransporte
                    {
                        CodigoIdentificacaoOperacao = Valor(root, "CodigoIdentificacaoOperacao"),
                        Protocolo = Valor(root, "Protocolo"),
                        Codigo = codigo,
                        Mensagem = mensagem,
                        Temp = erro ? CriarTemp(codigo, mensagem) : null
                    };
                    break;
                case Servico.CIOTConsultarSituacaoTransportador:
                    resultado = CriarRetSituacao(root, erro, codigo, mensagem);
                    break;
                case Servico.CIOTConsultarFrotaTransportador:
                    resultado = CriarRetFrota(root, erro, codigo, mensagem);
                    break;
                default:
                    throw new NotSupportedException("Retorno eFrete não implementado para " + servico + ".");
            }

            return resultado.GerarXML();
        }

        private static JObject CriarDeclaracao(Xml.CIOT.DeclaracaoOperacaoTransporte xml)
        {
            var tacAgregado = xml.TipoOperacao == TipoOperacaoTransporteCIOT.TACAgregado;
            var lotacao = xml.TipoOperacao == TipoOperacaoTransporteCIOT.CargaLotacao;
            var contratado = xml.Contratado ?? new PessoaCIOT { CpfOuCnpj = xml.CpfCnpjContratado, RNTRC = xml.RNTRCContratado };
            var contratante = xml.Contratante ?? new PessoaCIOT { CpfOuCnpj = xml.CpfCnpjContratante, RNTRC = xml.RNTRCContratante };
            var destinatario = xml.Destinatario ?? (string.IsNullOrWhiteSpace(xml.CpfCnpjDestinatario) ? null : new PessoaCIOT { CpfOuCnpj = xml.CpfCnpjDestinatario });
            var payload = new JObject
            {
                ["TipoViagem"] = TipoViagem(xml.TipoOperacao),
                ["MatrizCNPJ"] = xml.MatrizCNPJ,
                ["FilialCNPJ"] = xml.FilialCNPJ,
                ["IdOperacaoCliente"] = xml.IdOperacaoCliente,
                ["DataInicioViagem"] = !tacAgregado && xml.TemDataInicioViagemEFrete() ? xml.DataInicioViagemField : null,
                ["DataFimViagem"] = xml.ShouldSerializeDataFimViagemField() ? xml.DataFimViagemField : null,
                ["CodigoNCMNaturezaCarga"] = tacAgregado ? null : xml.DadosCarga?.CodigoNaturezaCarga,
                ["PesoCarga"] = tacAgregado ? null : Numero(xml.DadosCarga?.PesoCarga),
                ["TipoEmbalagem"] = lotacao ? xml.TipoEmbalagem : null,
                ["Viagens"] = tacAgregado ? null : CriarViagens(xml.OrigemDestino),
                ["Impostos"] = xml.Impostos == null ? null : JObject.FromObject(xml.Impostos),
                ["Pagamentos"] = CriarPagamentos(xml.InfPagamento),
                ["Contratado"] = JObject.FromObject(contratado),
                ["Motorista"] = xml.Motorista == null ? null : JObject.FromObject(xml.Motorista),
                ["Destinatario"] = tacAgregado || destinatario == null ? null : JObject.FromObject(destinatario),
                ["Contratante"] = JObject.FromObject(contratante),
                ["Subcontratante"] = xml.Subcontratante == null ? null : JObject.FromObject(xml.Subcontratante),
                ["Consignatario"] = xml.Consignatario == null ? null : JObject.FromObject(xml.Consignatario),
                ["TomadorServico"] = xml.TomadorServico == null ? null : JObject.FromObject(xml.TomadorServico),
                ["Veiculos"] = xml.Veiculos == null ? null : JArray.FromObject(xml.Veiculos.Select(x => new { x.Placa })),
                ["ContratantesCargaFracionada"] = tacAgregado || xml.DadosCarga?.ContratantesCargFrac == null ? null : JArray.FromObject(xml.DadosCarga.ContratantesCargFrac),
                ["CodigoTipoCarga"] = tacAgregado || xml.DadosCarga == null || (int)xml.DadosCarga.CodigoTipoCarga == 0 ? null : new JValue((int)xml.DadosCarga.CodigoTipoCarga),
                ["AltoDesempenho"] = !tacAgregado ? (JToken)new JValue(xml.InfIndicadoresOperacionais?.IndAltoDesempenho ?? false) : null,
                ["ComposicaoVeicular"] = !tacAgregado ? (JToken)new JValue(xml.InfIndicadoresOperacionais?.ComposicaoVeicular ?? false) : null,
                ["RetornoVazio"] = !tacAgregado ? (JToken)new JValue(xml.InfIndicadoresOperacionais?.IndRetornoVazio ?? false) : null,
                ["ObservacoesAoTransportador"] = xml.ObservacoesAoTransportador == null ? null : JArray.FromObject(xml.ObservacoesAoTransportador),
                ["ObservacoesAoCredenciado"] = xml.ObservacoesAoCredenciado == null ? null : JArray.FromObject(xml.ObservacoesAoCredenciado),
                ["EntregaDocumentacao"] = xml.EntregaDocumentacao,
                ["QuantidadeSaques"] = xml.QuantidadeSaques == 0 ? null : new JValue(xml.QuantidadeSaques),
                ["QuantidadeTransferencias"] = xml.QuantidadeTransferencias == 0 ? null : new JValue(xml.QuantidadeTransferencias),
                ["TipoPagamento"] = xml.TipoPagamentoEFrete ?? MapearTipoPagamento(xml.InfPagamento?.FirstOrDefault()?.TipoPagamento)
            };
            return payload;
        }

        private static JArray CriarViagens(List<OrigemDestino> viagens)
        {
            if (viagens == null) return null;
            var result = new JArray();
            foreach (var item in viagens)
            {
                result.Add(new JObject
                {
                    ["DocumentoViagem"] = item.DocumentoViagem,
                    ["CodigoMunicipioOrigem"] = item.Origem?.CodigoMunicipioOrigem,
                    ["CodigoMunicipioDestino"] = item.Destino?.CodigoMunicipioDestino,
                    ["CepOrigem"] = item.Origem?.CepOrigem,
                    ["CepDestino"] = item.Destino?.CepDestino,
                    ["LatitudeOrigem"] = item.Origem?.LatitudeOrigem,
                    ["LongitudeOrigem"] = item.Origem?.LongitudeOrigem,
                    ["LatitudeDestino"] = item.Destino?.LatitudeDestino,
                    ["LongitudeDestino"] = item.Destino?.LongitudeDestino,
                    ["DistanciaPercorrida"] = Numero(item.DistanciaPercorrida),
                    ["Valores"] = item.Valores == null ? null : JObject.FromObject(item.Valores),
                    ["TipoPagamento"] = item.TipoPagamentoEFrete,
                    ["NotasFiscais"] = CriarNotasFiscais(item.NotasFiscais)
                });
            }
            return result;
        }

        private static JObject CriarNotasFiscais(List<NotaFiscalCIOT> notasFiscais)
        {
            if (notasFiscais == null)
            {
                return null;
            }

            var notas = new JArray();
            foreach (var notaFiscal in notasFiscais)
            {
                notas.Add(JObject.FromObject(notaFiscal));
            }

            return new JObject { ["NotaFiscal"] = notas };
        }

        private static JArray CriarPagamentos(List<InfPagamento> pagamentos)
        {
            if (pagamentos == null) return null;
            var result = new JArray();
            foreach (var item in pagamentos)
            {
                JObject banco = null;
                if (!string.IsNullOrWhiteSpace(item.CodigoInstituicaoFinanceira) || !string.IsNullOrWhiteSpace(item.NumeroAgencia) || !string.IsNullOrWhiteSpace(item.NumeroConta))
                {
                    banco = new JObject { ["InstituicaoBancaria"] = item.CodigoInstituicaoFinanceira, ["Agencia"] = item.NumeroAgencia, ["Conta"] = item.NumeroConta, ["TipoConta"] = item.TipoConta };
                }
                result.Add(new JObject
                {
                    ["IdPagamentoCliente"] = item.IdPagamentoCliente,
                    ["DataDeLiberacao"] = !string.IsNullOrWhiteSpace(item.DataDeLiberacao) ? item.DataDeLiberacao : (item.ShouldSerializeDataVencimentoField() ? item.DataVencimentoField : null),
                    ["Valor"] = item.ValorParcela,
                    ["TipoPagamento"] = item.TipoPagamentoEFrete ?? MapearTipoPagamento(item.TipoPagamento),
                    ["Categoria"] = item.Categoria,
                    ["Documento"] = item.Documento,
                    ["InformacoesBancarias"] = banco,
                    ["TipoChavePix"] = item.TipoChavePix,
                    ["ValorChavePix"] = item.ChavePix,
                    ["CpfCnpjCreditado"] = item.CpfCnpjCreditado,
                    ["IdentificadorPix"] = item.IdentificadorPix,
                    ["IndicadorPagamento"] = item.IndPagamento == IndicadorPagamentoCIOT.APrazo ? "APrazo" : "AVista",
                    ["NumeroParcela"] = item.NumeroParcela,
                    ["CodigoPagamento"] = item.CodigoPagamento,
                    ["InformacaoAdicional"] = item.InformacaoAdicional,
                    ["CnpjFilialAbastecimento"] = item.CnpjFilialAbastecimento
                });
            }
            return result;
        }

        private static JObject CriarConsulta(Xml.CIOT.ConsultarCIOTGerado xml) => new JObject { ["MatrizCNPJ"] = xml.MatrizCNPJ, ["IdOperacaoCliente"] = xml.IdOperacaoCliente };
        private static JObject CriarCancelamento(Xml.CIOT.CancelamentoOperacaoTransporte xml) => new JObject { ["CodigoIdentificacaoOperacao"] = xml.CodigoIdentificacaoOperacao, ["Motivo"] = xml.MotivoCancelamento };
        private static JObject CriarEncerramento(Xml.CIOT.EncerramentoOperacaoTransporte xml) => new JObject { ["CodigoIdentificacaoOperacao"] = xml.CodigoIdentificacaoOperacao, ["PesoCarga"] = Numero(xml.DadosCarga?.PesoTotalCarga) };
        private static JObject CriarSituacao(Xml.CIOT.ConsultarSituacaoTransportador xml, List<string> placas) => new JObject
        {
            ["InteressadoCpfOuCnpj"] = xml.CpfCnpjInteressado,
            ["TransportadorCpfOuCnpj"] = xml.CpfCnpjTransportador,
            ["TransportadorRNTRC"] = xml.RNTRCTransportador,
            ["DataPrevistaFimViagem"] = xml.DataPrevistaFimViagem,
            ["Veiculos"] = placas == null ? null : JArray.FromObject(placas.Select(x => new { Placa = x }))
        };

        private static RetConsultarSituacaoTransportador CriarRetSituacao(JObject root, bool erro, string codigo, string mensagem) => new RetConsultarSituacaoTransportador
        {
            CpfCnpjTransportador = Valor(root, "CpfOuCnpj"), RNTRCTransportador = Valor(root, "RNTRC"),
            RNTRCAtivo = ValorBool(root, "RNTRCAtivo"), EquiparadoTAC = ValorBool(root, "TACouEquiparado"),
            Protocolo = Valor(root, "ProtocoloServico"), Codigo = codigo, Mensagem = mensagem,
            Temp = erro ? CriarTemp(codigo, mensagem) : null
        };

        private static RetConsultarFrotaTransportador CriarRetFrota(JObject root, bool erro, string codigo, string mensagem)
        {
            var frota = new List<VeiculoFrota>();
            var veiculos = Localizar(root, "Veiculos") as JArray;
            if (veiculos != null)
            {
                foreach (var item in veiculos.OfType<JObject>()) frota.Add(new VeiculoFrota { PlacaVeiculo = Valor(item, "Placa"), SituacaoVeiculoFrotaTransportador = ValorBool(item, "FazParteDaFrota") });
            }
            return new RetConsultarFrotaTransportador { CpfCnpjTransportador = Valor(root, "CpfOuCnpj"), RNTRCTransportador = Valor(root, "RNTRC"), RNTRCAtivo = ValorBool(root, "RNTRCAtivo"), Frota = frota, Protocolo = Valor(root, "ProtocoloServico"), Codigo = codigo, Mensagem = mensagem, Temp = erro ? CriarTemp(codigo, mensagem) : null };
        }

        private static Temp CriarTemp(string codigo, string mensagem) => new Temp { Error = codigo ?? "EFRETE", Message = mensagem ?? "A eFrete rejeitou a solicitação." };
        private static string TipoViagem(TipoOperacaoTransporteCIOT tipo) => tipo == TipoOperacaoTransporteCIOT.CargaFracionada ? "Fracionado" : tipo == TipoOperacaoTransporteCIOT.TACAgregado ? "TAC_Agregado" : "Padrao";
        private static string MapearTipoPagamento(TipoPagamentoFreteCIOT? tipo) => tipo == TipoPagamentoFreteCIOT.InstituicaoPagamento ? "eFRETE" : "TransferenciaBancaria";
        private static JToken Numero(string value) { decimal parsed; return decimal.TryParse(value, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out parsed) ? new JValue(parsed) : null; }
        private static JToken Localizar(JObject obj, string nome) { return obj?.Properties().FirstOrDefault(x => string.Equals(x.Name, nome, StringComparison.OrdinalIgnoreCase))?.Value; }
        private static string Valor(JObject obj, string nome) => Localizar(obj, nome)?.Type == JTokenType.Null ? null : Localizar(obj, nome)?.ToString();
        private static string Valor(JObject obj, string grupo, string nome) => Valor(Localizar(obj, grupo) as JObject, nome);
        private static bool ValorBool(JObject obj, string nome) { bool value; return bool.TryParse(Valor(obj, nome), out value) && value; }
        private static void RemoverNulos(JToken token)
        {
            if (token is JObject obj) foreach (var prop in obj.Properties().ToList()) { RemoverNulos(prop.Value); if (prop.Value.Type == JTokenType.Null || (prop.Value.Type == JTokenType.String && string.IsNullOrWhiteSpace(prop.Value.ToString()))) prop.Remove(); }
            else if (token is JArray array) foreach (var item in array) RemoverNulos(item);
        }
    }
}
