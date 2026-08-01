using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Net.Http;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.DARE;
using Unimake.Business.DFe.Xml.EBoleto;
using Unimake.Business.DFe.Xml.PIX;
using Unimake.Business.DFe.Xml.UMessenger;

namespace Unimake.Business.DFe.ConsumirServico.Parsers
{
    internal sealed class ApiResponseContentParser
    {
        private readonly ApiJsonXmlExtractor _jsonXmlExtractor = new ApiJsonXmlExtractor();
        private readonly ApiResponseXmlSupport _xmlSupport = new ApiResponseXmlSupport();

        public XmlDocument Parse(ref ApiResponseContext context)
        {
            if (context.Config.Servico == Servico.EBoletoCancelar && !context.Response.IsSuccessStatusCode)
            {
                return CriarXmlRetornoBoletoCancelar(ref context);
            }

            var tipoRetorno = string.IsNullOrWhiteSpace(context.Config.ResponseMediaType)
                ? context.Response.Content.Headers.ContentType.MediaType
                : context.Config.ResponseMediaType;

            if (!context.ResponseContent.StartsWith("<") && context.Response.IsSuccessStatusCode && context.ResponseContent.StartsWith(" "))
            {
                context.ResponseContent = context.ResponseContent.Substring(1);
            }

            switch (tipoRetorno)
            {
                case "text/plain":
                case "application/xml":
                case "text/xml":
                    return ParseTextOrXml(context.ResponseContent);

                case "application/json":
                case "application/problem+json":
                    return ParseJson(ref context);

                case "text/html":
                    return ParseHtml(ref context);

                case "application/pdf":
                    return ParsePdf(ref context);

                default:
                    return _xmlSupport.CreatePdfXmlDocument(context.Response.Content.Headers.ToString());
            }
        }

        private XmlDocument ParseTextOrXml(string responseContent)
        {
            var resultadoRetorno = new XmlDocument();
            try
            {
                resultadoRetorno.LoadXml(responseContent);
            }
            catch
            {
                resultadoRetorno = _xmlSupport.StringToSerializedXml(responseContent);
            }

            return resultadoRetorno;
        }

        private XmlDocument ParseJson(ref ApiResponseContext context)
        {
            try
            {
                if (context.ResponseContent.TrimStart().StartsWith("<"))
                {
                    try
                    {
                        return ProcessXmlWithSafeEncoding(context.Response);
                    }
                    catch
                    {
                        var xml = new XmlDocument();
                        xml.LoadXml(context.ResponseContent);
                        return xml;
                    }
                }

                var config = context.Config;
                var resultadoRetorno = new XmlDocument();
                resultadoRetorno.LoadXml(_jsonXmlExtractor.ExtractXml(ref config, context.ResponseContent));
                context.Config = config;

                if (context.Config.Servico == Servico.DAREEnvio)
                {
                    if (context.ResponseContent.Contains("itensParaGeracao"))
                    {
                        var dareLote = JsonConvert.DeserializeObject<DARELoteRetorno>(context.ResponseContent);
                        return dareLote.GerarXML();
                    }

                    if (context.ResponseContent.Contains("documentoImpressao"))
                    {
                        var dareUnico = JsonConvert.DeserializeObject<DAREUnicoRetorno>(context.ResponseContent);
                        return CreateXmlDocumentDareRetorno(dareUnico);
                    }
                }

                if (context.Config.Servico == Servico.UMessengerPublish)
                {
                    return CriarXmlRetornoUMessenger(ref context);
                }

                if (EhServicoEBoleto(context.Config.Servico))
                {
                    return CriarXmlRetornoEBoleto(ref context);
                }

                if (EhServicoPIX(context.Config.Servico))
                {
                    return CriarXmlRetornoPIX(ref context);
                }

                return resultadoRetorno;
            }
            catch
            {
                if (context.Config.Servico == Servico.DAREReceita)
                {
                    return ParseDareReceitas(context.ResponseContent);
                }

                if (context.Config.Servico == Servico.UMessengerPublish)
                {
                    return CriarXmlRetornoUMessenger(ref context);
                }

                if (EhServicoEBoleto(context.Config.Servico))
                {
                    return CriarXmlRetornoEBoleto(ref context);
                }

                if (EhServicoPIX(context.Config.Servico))
                {
                    return CriarXmlRetornoPIX(ref context);
                }

                var xml = new XmlDocument();
                xml.LoadXml(context.ResponseContent);
                return xml;
            }
        }

        private XmlDocument ParseHtml(ref ApiResponseContext context)
        {
            try
            {
                var xml = new XmlDocument();
                var config = context.Config;
                xml.LoadXml(_jsonXmlExtractor.ExtractXml(ref config, context.ResponseContent));
                context.Config = config;
                return xml;
            }
            catch
            {
                return _xmlSupport.StringToSerializedXml(HtmlToPlainText(context.ResponseContent));
            }
        }

        private XmlDocument ParsePdf(ref ApiResponseContext context)
        {
            var responseString = context.ResponseContent.Replace("&lt;", "<").Replace("&gt;", ">").Replace("&amp;", "&");
            responseString = Convert.ToBase64String(Encoding.UTF8.GetBytes(responseString));

            if (context.Response.IsSuccessStatusCode)
            {
                using (var originalStream = context.Response.Content.ReadAsStreamAsync().Result)
                {
                    var memoryStream = new MemoryStream();
                    originalStream.CopyTo(memoryStream);
                    memoryStream.Position = 0;
                    context.Stream = memoryStream;
                }
            }
            else
            {
                context.Stream = null;
            }

            return _xmlSupport.CreatePdfXmlDocument(responseString);
        }

        private XmlDocument ParseDareReceitas(string responseContent)
        {
            List<ReceitaDARE> dare = null;
            try
            {
                dare = JsonConvert.DeserializeObject<List<ReceitaDARE>>(responseContent);
            }
            catch (Exception ex)
            {
                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.", ex);
            }

            if (dare == null)
            {
                throw new InvalidOperationException("Não foi possível desserializar a lista de receitas.");
            }

            var receitas = new Xml.DARE.Receitas
            {
                Receita = dare
            };

            return XMLUtility.Serializar<Xml.DARE.Receitas>(receitas);
        }

        private XmlDocument CreateXmlDocumentDareRetorno(DAREUnicoRetorno dareUnico)
        {
            var dareRetorno = new DARERetorno
            {
                DARE = dareUnico
            };

            return XMLUtility.Serializar<DARERetorno>(dareRetorno);
        }

        private XmlDocument CriarXmlRetornoUMessenger(ref ApiResponseContext context)
        {
            var root = JObject.Parse(context.ResponseContent);
            var mensagem = new retUMessengerMensagem
            {
                Status = context.Response.IsSuccessStatusCode ? 1 : 999,
                Motivo = context.Response.IsSuccessStatusCode
                    ? "Mensagem enviada com sucesso."
                    : ExtrairMotivoErroUMessenger(root),
                DLLVersao = Info.VersaoDLL
            };

            if (context.Response.IsSuccessStatusCode)
            {
                var dto = JsonConvert.DeserializeAnonymousType(context.ResponseContent, new { messageId = "" });
                mensagem.MessageID = dto?.messageId;
            }
            else
            {
                mensagem.TraceId = root.Value<string>("traceId");
                mensagem.HelpLink = root.Value<string>("helpLink");
                mensagem.ErrorType = root.Value<string>("type");
                mensagem.ErrorTitle = root.Value<string>("title");
            }

            var ret = new retUMessengerPublish();
            ret.Mensagem.Add(mensagem);
            return ret.GerarXML();
        }

        private bool EhServicoEBoleto(Servico servico)
        {
            switch (servico)
            {
                case Servico.EBoletoRegistrar:
                case Servico.EBoletoCancelar:
                case Servico.EBoletoConsultar:
                case Servico.EBoletoAlterarVencto:
                case Servico.EBoletoEnviarInstrucao:
                case Servico.EBoletoInformarPagt:
                    return true;
            }

            return false;
        }

        private XmlDocument CriarXmlRetornoEBoleto(ref ApiResponseContext context)
        {
            switch (context.Config.Servico)
            {
                case Servico.EBoletoRegistrar:
                    return CriarXmlRetornoBoletoRegistrar(ref context);

                case Servico.EBoletoConsultar:
                    return CriarXmlRetornoBoletoConsultar(ref context);

                case Servico.EBoletoCancelar:
                    return CriarXmlRetornoBoletoCancelar(ref context);

                case Servico.EBoletoAlterarVencto:
                    return CriarXmlRetornoBoletoAlterarVencto(ref context);

                case Servico.EBoletoEnviarInstrucao:
                    return CriarXmlRetornoBoletoEnviarInstrucao(ref context);

                case Servico.EBoletoInformarPagt:
                    return CriarXmlRetornoBoletoInformarPagto(ref context);
            }

            return _xmlSupport.StringToSerializedXml(context.ResponseContent);
        }

        private XmlDocument CriarXmlRetornoBoletoRegistrar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoRegistrar
            {
                DLLVersao = Info.VersaoDLL
            };

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, "Falha ao registrar boleto.").Replace("\r\n", "");
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
                return retorno.GerarXML();
            }

            retorno.Status = ResolverStatusEBoleto(root, 0, 1);
            retorno.Motivo = retorno.Status == 0
                ? "Boleto registrado"
                : ResolverMotivoEBoleto(root, retorno.Status, "Boleto registrado", "Falha ao registrar boleto.");
            retorno.CodigoBarraNumerico = ObterPrimeiroValorTexto(root, "codigoBarraNumerico", "codigoBarrasNumerico", "barcodeNumeric") ?? string.Empty;
            retorno.NumeroNoBanco = ObterPrimeiroValorTexto(root, "numeroNoBanco") ?? string.Empty;
            retorno.LinhaDigitavel = ObterPrimeiroValorTexto(root, "linhaDigitavel") ?? string.Empty;
            retorno.PdfContentSuccess = ObterPrimeiroValorBooleano(root, "pdfContentSuccess", "successPdf")
                ?? ObterPrimeiroValorBooleanoPorCaminhos(root, new[] { "pdfContent", "success" })
                ?? false;
            retorno.PdfContentMessage = ObterPrimeiroValorTexto(root, "pdfContentMessage")
                ?? ObterPrimeiroValorTextoPorCaminhos(root, new[] { "pdfContent", "message" })
                ?? string.Empty;
            retorno.PdfContentBase64 = ObterPrimeiroValorTexto(root, "pdfContentBase64")
                ?? ObterPrimeiroValorTextoPorCaminhos(root, new[] { "pdfContent", "content" }, new[] { "pdfContent", "base64" })
                ?? string.Empty;
            retorno.PdfPath = ObterPrimeiroValorTexto(root, "pdfPath", "caminhoPdf") ?? string.Empty;

            var pixPagamentoDetalhe = LocalizarTokenPorNome(root, "pixPagamentoDetalhe");
            if (pixPagamentoDetalhe != null && pixPagamentoDetalhe.Type != JTokenType.Null)
            {
                retorno.PixPagamentoDetalhe = new retBoletoRegistrarPIXPagamentoDetalhe
                {
                    DataPagamento = FormatarDataPagamentoBoletoRegistrar(LocalizarTokenPorNome(pixPagamentoDetalhe, "dataPagamento")),
                    TxId = ObterPrimeiroValorTexto(pixPagamentoDetalhe, "txId") ?? string.Empty,
                    ValorAbatimento = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorAbatimento"), true),
                    ValorDesconto = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorDesconto"), true),
                    ValorJuros = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorJuros"), true),
                    ValorLiquidado = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorLiquidado"), false),
                    ValorMulta = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorMulta"), true),
                    ValorOriginal = FormatarValorEBoleto(ObterPrimeiroValorTexto(pixPagamentoDetalhe, "valorOriginal"), false)
                };
            }

            var qrCodeImage = ObterPrimeiroValorTextoPorCaminhos(root, new[] { "qrCodeContent", "image" })
                ?? ObterPrimeiroValorTexto(root, "qrCodeImage");
            var qrCodeSuccess = ObterPrimeiroValorBooleanoPorCaminhos(root, new[] { "qrCodeContent", "success" })
                ?? ObterPrimeiroValorBooleano(root, "qrCodeSuccess");
            var qrCodeText = ObterPrimeiroValorTextoPorCaminhos(root, new[] { "qrCodeContent", "text" })
                ?? ObterPrimeiroValorTexto(root, "qrCodeText", "pixCopiaECola");

            if (!string.IsNullOrWhiteSpace(qrCodeImage) ||
                !string.IsNullOrWhiteSpace(qrCodeText) ||
                qrCodeSuccess == true)
            {
                retorno.QRCodeContent = new retBoletoRegistrarQRCodeContent
                {
                    Image = qrCodeImage ?? string.Empty,
                    Success = qrCodeSuccess.GetValueOrDefault(),
                    Text = qrCodeText ?? string.Empty
                };
            }

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoBoletoConsultar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoConsultar
            {
                DLLVersao = Info.VersaoDLL
            };

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, "Falha ao consultar boleto.");
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
                return retorno.GerarXML();
            }

            retorno.BoletoResponse.AddRange(CriarListaItensEBoleto(root));
            retorno.Status = retorno.BoletoResponse.Count > 0 ? 0 : 1;
            retorno.Motivo = retorno.BoletoResponse.Count > 0
                ? "Boletos encontrados"
                : "Nenhum boleto encontrado";

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoBoletoCancelar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoCancelar
            {
                DLLVersao = Info.VersaoDLL
            };

            PreencherRetornoEBoletoBasico(retorno, root, context.Response.IsSuccessStatusCode, "Boleto cancelado com sucesso", "Falha ao cancelar boleto.", 0, 1);

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 1;
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
            }

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoBoletoAlterarVencto(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoAlterarVencto
            {
                DLLVersao = Info.VersaoDLL
            };

            PreencherRetornoEBoletoBasico(retorno, root, context.Response.IsSuccessStatusCode, "Vencimento alterado com sucesso", "Falha ao alterar vencimento do boleto.", 0, 1);

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 1;
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
            }

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoBoletoEnviarInstrucao(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoEnviarInstrucao
            {
                DLLVersao = Info.VersaoDLL
            };

            PreencherRetornoEBoletoBasico(retorno, root, context.Response.IsSuccessStatusCode, "Instruções do boleto enviado com sucesso", "Falha ao enviar instruções do boleto.", 0, 1);
            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoBoletoInformarPagto(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retBoletoInformarPagto
            {
                DLLVersao = Info.VersaoDLL
            };

            if (context.Response.StatusCode == System.Net.HttpStatusCode.Accepted ||
                context.Response.StatusCode == System.Net.HttpStatusCode.OK)
            {
                retorno.Status = 0;
                retorno.Motivo = "Instrução para marcar o boleto como pago enviado com sucesso";
            }
            else
            {
                var codigo = ObterPrimeiroValorTexto(root, "codigo", "code");
                var mensagem = ObterPrimeiroValorTexto(root, "mensagem", "message");

                retorno.Status = 1;
                retorno.Motivo = "Não foi possível marcar o boleto como pago. Tente novamente mais tarde. (Status Code: " +
                    ((int)context.Response.StatusCode).ToString() + ")" +
                    (!string.IsNullOrWhiteSpace(codigo)
                        ? " - (Erro: " + codigo + (!string.IsNullOrWhiteSpace(mensagem) ? " - " + mensagem : "") + ")"
                        : "");
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
            }

            return retorno.GerarXML();
        }

        private void PreencherRetornoEBoletoBasico(retEBoletoRetornoBasico retorno, JObject root, bool sucessoHttp, string motivoSucesso, string motivoErro, int statusSucesso, int statusErroSemHttp)
        {
            if (!sucessoHttp)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, motivoErro);
                return;
            }

            retorno.Status = ResolverStatusEBoleto(root, statusSucesso, statusErroSemHttp);
            retorno.Motivo = ResolverMotivoEBoleto(root, retorno.Status, motivoSucesso, motivoErro);
        }

        private bool EhServicoPIX(Servico servico)
        {
            switch (servico)
            {
                case Servico.PIXCobrancaCriar:
                case Servico.PIXCobrancaConsultar:
                case Servico.PIXConsultar:
                    return true;
            }

            return false;
        }

        private XmlDocument CriarXmlRetornoPIX(ref ApiResponseContext context)
        {
            switch (context.Config.Servico)
            {
                case Servico.PIXCobrancaCriar:
                    return CriarXmlRetornoPIXCobrancaCriar(ref context);

                case Servico.PIXCobrancaConsultar:
                    return CriarXmlRetornoPIXCobrancaConsultar(ref context);

                case Servico.PIXConsultar:
                    return CriarXmlRetornoPIXConsultar(ref context);
            }

            return _xmlSupport.StringToSerializedXml(context.ResponseContent);
        }

        private XmlDocument CriarXmlRetornoPIXCobrancaCriar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retPIXCobrancaCriar
            {
                DLLVersao = Info.VersaoDLL
            };

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, "Falha ao criar cobrança PIX.");
                retorno.TraceId = ObterPrimeiroValorTexto(root, "traceId");
                retorno.PixCopiaECola = string.Empty;
                retorno.ImageQRCode = string.Empty;
                return retorno.GerarXML();
            }

            retorno.Status = ResolverStatusPIXCobrancaCriar(root);
            retorno.Motivo = ResolverMotivoPIXCobrancaCriar(retorno.Status);

            if (retorno.Status == 0)
            {
                retorno.PixCopiaECola = ObterPrimeiroValorTexto(root, "PixCopiaECola", "pixCopiaECola", "copiaECola", "pixCopiaCola", "emv");
                retorno.ImageQRCode = ObterPrimeiroValorTexto(root, "ImageQRCode", "imageQRCode", "qrCodeImagePath", "qrCodeImage", "imagemQRCode");
            }
            else
            {
                retorno.PixCopiaECola = string.Empty;
                retorno.ImageQRCode = string.Empty;
            }

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoPIXCobrancaConsultar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retPIXCobrancaConsultar
            {
                DLLVersao = Info.VersaoDLL
            };

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, "Falha ao consultar cobrança PIX.");
                return retorno.GerarXML();
            }

            var itens = CriarListaItensPIX(root);
            retorno.Items.AddRange(itens);
            retorno.Status = retorno.Items.Count > 0 ? 1 : 2;
            retorno.Motivo = retorno.Items.Count > 0
                ? "Movimentos PIX localizados."
                : "Nenhum Movimento PIX foi localizado.";

            return retorno.GerarXML();
        }

        private XmlDocument CriarXmlRetornoPIXConsultar(ref ApiResponseContext context)
        {
            var root = TryParseJsonObject(context.ResponseContent);
            var retorno = new retPIXConsultar
            {
                DLLVersao = Info.VersaoDLL
            };

            if (!context.Response.IsSuccessStatusCode)
            {
                retorno.Status = 999;
                retorno.Motivo = ExtrairMotivoErroApi(root, "Falha ao consultar PIX.");
                return retorno.GerarXML();
            }

            var item = CriarItemPIX(root);

            if (item != null && !string.IsNullOrWhiteSpace(item.TxId))
            {
                retorno.Status = 1;
                retorno.Motivo = "Movimento PIX Localizado.";
                retorno.TxId = item.TxId;
                retorno.Valor = item.Valor;
                retorno.Horario = item.Horario;
                retorno.Pagador = item.Pagador;
            }
            else
            {
                retorno.Status = 2;
                retorno.Motivo = "Nenhum Movimento PIX foi localizado.";
            }

            return retorno.GerarXML();
        }

        private JObject TryParseJsonObject(string responseContent)
        {
            if (string.IsNullOrWhiteSpace(responseContent))
            {
                return new JObject();
            }

            try
            {
                var token = JToken.Parse(responseContent);
                if (token is JObject objeto)
                {
                    return objeto;
                }

                if (token is JArray array)
                {
                    return new JObject
                    {
                        ["items"] = array
                    };
                }

                return new JObject();
            }
            catch
            {
                return new JObject();
            }
        }

        private int ResolverStatusPIXCobrancaCriar(JObject root)
        {
            var status = ObterPrimeiroValorTexto(root, "status");
            if (int.TryParse(status, out var statusNumerico))
            {
                return statusNumerico;
            }

            switch (NormalizarChave(status))
            {
                case "ATIVA":
                case "ACTIVE":
                    return 0;

                case "CONCLUIDA":
                case "CONCLUIDO":
                case "COMPLETED":
                    return 1;

                case "REMOVIDAPELOUSUARIORECEBEDOR":
                case "REMOVIDOPELOUSUARIORECEBEDOR":
                    return 2;

                case "REMOVIDAPELOPSP":
                case "REMOVIDOPELOPSP":
                    return 3;
            }

            return 0;
        }

        private string ResolverMotivoPIXCobrancaCriar(int status)
        {
            switch (status)
            {
                case 0:
                    return "PIX Ativo (Cobranca gerada)";

                case 1:
                    return "PIX Concluído (PIX ja foi pago)";

                case 2:
                    return "Cobranca do PIX removida pelo usuario recebedor";

                case 3:
                    return "Cobranca do PIX removida pelo PSP do recebedor";
            }

            return string.Empty;
        }

        private List<retPIXItem> CriarListaItensPIX(JObject root)
        {
            var lista = new List<retPIXItem>();
            var pixToken = LocalizarTokenPorNome(root, "pix");

            if (pixToken is JArray pixArray)
            {
                var indice = 1;
                foreach (var item in pixArray)
                {
                    var retornoItem = CriarItemPIX(item);
                    if (retornoItem == null)
                    {
                        continue;
                    }

                    retornoItem.Id = indice.ToString();
                    lista.Add(retornoItem);
                    indice++;
                }

                return lista;
            }

            var itemUnico = CriarItemPIX(root);
            if (itemUnico != null && !string.IsNullOrWhiteSpace(itemUnico.TxId))
            {
                itemUnico.Id = "1";
                lista.Add(itemUnico);
            }

            return lista;
        }

        private retPIXItem CriarItemPIX(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var pagadorToken = LocalizarTokenPorNome(token, "pagador", "devedor");
            var item = new retPIXItem
            {
                TxId = ObterPrimeiroValorTexto(token, "txId", "txid"),
                Valor = ObterPrimeiroValorTextoPorCaminhos(token,
                    new[] { "valor", "original" },
                    new[] { "valor" },
                    new[] { "value", "original" },
                    new[] { "value" }),
                Horario = ObterPrimeiroValorTextoPorCaminhos(token,
                    new[] { "horario", "liquidacao" },
                    new[] { "horario" },
                    new[] { "horarioLiquidacao" },
                    new[] { "dataHora" },
                    new[] { "dataHoraLiquidacao" }),
                Pagador = CriarPagadorPIX(pagadorToken ?? token)
            };

            if (string.IsNullOrWhiteSpace(item.TxId) &&
                string.IsNullOrWhiteSpace(item.Valor) &&
                string.IsNullOrWhiteSpace(item.Horario) &&
                item.Pagador == null)
            {
                return null;
            }

            return item;
        }

        private retPIXPagador CriarPagadorPIX(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var retorno = new retPIXPagador
            {
                Nome = ObterPrimeiroValorTexto(token, "nome", "name"),
                Inscricao = ObterPrimeiroValorTexto(token, "inscricao", "cpf", "cnpj", "document", "documento")
            };

            if (string.IsNullOrWhiteSpace(retorno.Nome) && string.IsNullOrWhiteSpace(retorno.Inscricao))
            {
                return null;
            }

            return retorno;
        }

        private int ResolverStatusEBoleto(JObject root, int statusSucesso, int statusErroSemHttp)
        {
            var statusTexto = ObterValorTextoDireto(root, "status");
            if (int.TryParse(statusTexto, out var statusNumerico))
            {
                return statusNumerico;
            }

            var success = ObterValorBooleanoDireto(root, "success", "sucesso");
            if (success.HasValue)
            {
                return success.Value ? statusSucesso : statusErroSemHttp;
            }

            return statusSucesso;
        }

        private string ResolverMotivoEBoleto(JObject root, int status, string motivoSucesso, string motivoErro)
        {
            var motivoExplicito = ObterValorTextoDireto(root, "motivo", "message", "mensagem", "detail");
            if (!string.IsNullOrWhiteSpace(motivoExplicito))
            {
                return motivoExplicito;
            }

            return status == 999 || status > 0
                ? motivoErro.TrimEnd('.')
                : motivoSucesso;
        }

        private List<retBoletoConsultarItem> CriarListaItensEBoleto(JObject root)
        {
            var lista = new List<retBoletoConsultarItem>();
            var boletosToken = LocalizarTokenPorNome(root, "boletos", "boletoResponse", "items", "data");

            if (boletosToken is JArray boletosArray)
            {
                foreach (var item in boletosArray)
                {
                    var retornoItem = CriarItemEBoleto(item);
                    if (retornoItem != null)
                    {
                        lista.Add(retornoItem);
                    }
                }

                return lista;
            }

            var itemUnico = CriarItemEBoleto(root);
            if (itemUnico != null)
            {
                lista.Add(itemUnico);
            }

            return lista;
        }

        private retBoletoConsultarItem CriarItemEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var item = new retBoletoConsultarItem
            {
                CodigoBarras = ObterPrimeiroValorTexto(token, "codigoBarras", "codigoBarra", "barcode") ?? string.Empty,
                DataEmissao = FormatarDataEBoleto(ObterPrimeiroValorTexto(token, "dataEmissao", "emissao")),
                DataLiquidacao = FormatarDataEBoleto(ObterPrimeiroValorTexto(token, "dataLiquidacao")),
                DataVencimento = FormatarDataEBoleto(ObterPrimeiroValorTexto(token, "dataVencimento", "vencimento")),
                NumeroNaEmpresa = ObterPrimeiroValorTexto(token, "numeroNaEmpresa", "numeroEmpresa", "seuNumero") ?? string.Empty,
                NumeroNoBanco = ObterPrimeiroValorTexto(token, "numeroNoBanco", "nossoNumero") ?? string.Empty,
                Pagador = CriarPagadorEBoleto(LocalizarTokenPorNome(token, "pagador", "payer")),
                PdfContent = CriarPdfContentEBoleto(LocalizarTokenPorNome(token, "pdfContent")) ?? new retBoletoConsultarPdfContent(),
                PIXPagamentoDetalhe = CriarPIXPagamentoDetalheEBoleto(LocalizarTokenPorNome(token, "pixPagamentoDetalhe")),
                QrCodeContent = CriarQrCodeContentEBoleto(LocalizarTokenPorNome(token, "qrCodeContent")) ?? new retBoletoConsultarQrCodeContent(),
                Situacao = ObterPrimeiroValorInteiro(token, "situacao", "statusBoleto").GetValueOrDefault(),
                TipoLiquidacao = ObterPrimeiroValorInteiro(token, "tipoLiquidacao").GetValueOrDefault(),
                Valor = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valor"), false),
                ValorAbatimento = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorAbatimento"), false),
                ValorDesconto = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorDesconto"), false),
                ValorJuros = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorJuros"), false),
                ValorLiquidado = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorLiquidado"), false),
                ValorMulta = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorMulta"), false)
            };

            if (string.IsNullOrWhiteSpace(item.NumeroNoBanco) &&
                string.IsNullOrWhiteSpace(item.NumeroNaEmpresa) &&
                string.IsNullOrWhiteSpace(item.CodigoBarras) &&
                LocalizarTokenPorNome(token,
                    "valor",
                    "dataVencimento",
                    "pagador",
                    "payer",
                    "pdfContent",
                    "pixPagamentoDetalhe",
                    "qrCodeContent") == null)
            {
                return null;
            }

            return item;
        }

        private retBoletoConsultarPagador CriarPagadorEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var retorno = new retBoletoConsultarPagador
            {
                Codigo = ObterPrimeiroValorTexto(token, "codigo") ?? string.Empty,
                Nome = ObterPrimeiroValorTexto(token, "nome", "name") ?? string.Empty,
                Inscricao = ObterPrimeiroValorTexto(token, "inscricao", "cpf", "cnpj", "documento") ?? string.Empty,
                Telefone = ObterPrimeiroValorTexto(token, "telefone", "phone") ?? string.Empty,
                Email = ObterPrimeiroValorTexto(token, "email", "mail") ?? string.Empty,
                TipoInscricao = ObterPrimeiroValorInteiro(token, "tipoInscricao").GetValueOrDefault(),
                Endereco = CriarEnderecoEBoleto(LocalizarTokenPorNome(token, "endereco", "address"))
            };

            return retorno;
        }

        private retBoletoConsultarEndereco CriarEnderecoEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var retorno = new retBoletoConsultarEndereco
            {
                Logradouro = ObterPrimeiroValorTexto(token, "logradouro", "street") ?? string.Empty,
                Numero = ObterPrimeiroValorTexto(token, "numero", "number") ?? string.Empty,
                Complemento = ObterPrimeiroValorTexto(token, "complemento", "complement") ?? string.Empty,
                Bairro = ObterPrimeiroValorTexto(token, "bairro", "district") ?? string.Empty,
                Cidade = ObterPrimeiroValorTexto(token, "cidade", "city") ?? string.Empty,
                UF = ObterPrimeiroValorTexto(token, "uf", "state") ?? string.Empty,
                CEP = ObterPrimeiroValorTexto(token, "cep", "zipCode") ?? string.Empty
            };

            return retorno;
        }

        private retBoletoConsultarPdfContent CriarPdfContentEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var retorno = new retBoletoConsultarPdfContent
            {
                Content = ObterPrimeiroValorTexto(token, "content", "base64") ?? string.Empty,
                Success = ObterPrimeiroValorBooleano(token, "success").GetValueOrDefault(),
                Message = ObterPrimeiroValorTexto(token, "message", "mensagem") ?? string.Empty
            };

            return retorno;
        }

        private retBoletoConsultarPIXPagamentoDetalhe CriarPIXPagamentoDetalheEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            return new retBoletoConsultarPIXPagamentoDetalhe
            {
                DataPagamento = FormatarDataEBoleto(ObterPrimeiroValorTexto(token, "dataPagamento")),
                ValorDesconto = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorDesconto"), true),
                ValorJuros = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorJuros"), true),
                ValorLiquidado = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorLiquidado"), false),
                ValorMulta = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorMulta"), true),
                ValorOriginal = FormatarValorEBoleto(ObterPrimeiroValorTexto(token, "valorOriginal"), false)
            };
        }

        private retBoletoConsultarQrCodeContent CriarQrCodeContentEBoleto(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return null;
            }

            var retorno = new retBoletoConsultarQrCodeContent
            {
                Text = ObterPrimeiroValorTexto(token, "text", "texto") ?? string.Empty,
                Image = ObterPrimeiroValorTexto(token, "image", "imagem") ?? string.Empty,
                Success = ObterPrimeiroValorBooleano(token, "success").GetValueOrDefault()
            };

            return retorno;
        }

        private string FormatarDataEBoleto(string valor)
        {
            if (string.IsNullOrWhiteSpace(valor))
            {
                return string.Empty;
            }

            if (DateTimeOffset.TryParse(valor, CultureInfo.InvariantCulture, DateTimeStyles.AllowWhiteSpaces, out var data) ||
                DateTimeOffset.TryParse(valor, CultureInfo.CurrentCulture, DateTimeStyles.AllowWhiteSpaces, out data))
            {
                return data.ToString("dd-MM-yyyy", CultureInfo.InvariantCulture);
            }

            return valor;
        }

        private string FormatarDataPagamentoBoletoRegistrar(JToken token)
        {
            if (token == null || token.Type == JTokenType.Null)
            {
                return string.Empty;
            }

            if (token is JValue valorData && valorData.Value is DateTimeOffset dataOffset)
            {
                return dataOffset.ToString("yyyy-MM-ddTHH:mm:ss.fff'Z'", CultureInfo.InvariantCulture);
            }

            if (token is JValue valorDataHora && valorDataHora.Value is DateTime dataHora)
            {
                return dataHora.ToString("yyyy-MM-ddTHH:mm:ss.fff'Z'", CultureInfo.InvariantCulture);
            }

            var valor = token.ToString();
            if (string.IsNullOrWhiteSpace(valor))
            {
                return string.Empty;
            }

            if (DateTimeOffset.TryParse(valor, CultureInfo.InvariantCulture, DateTimeStyles.AllowWhiteSpaces, out var data) ||
                DateTimeOffset.TryParse(valor, CultureInfo.CurrentCulture, DateTimeStyles.AllowWhiteSpaces, out data))
            {
                return data.ToString("yyyy-MM-ddTHH:mm:ss.fff'Z'", CultureInfo.InvariantCulture);
            }

            return valor;
        }

        private string FormatarValorEBoleto(string valor, bool nullable)
        {
            if (string.IsNullOrWhiteSpace(valor))
            {
                return nullable ? string.Empty : "0.00";
            }

            var culturaOrigem = valor.IndexOf(',') >= 0 && valor.IndexOf('.') < 0
                ? CultureInfo.CurrentCulture
                : CultureInfo.InvariantCulture;

            if (decimal.TryParse(valor, NumberStyles.Any, culturaOrigem, out var numero))
            {
                return numero.ToString("N2", CultureInfo.GetCultureInfo("en-US"));
            }

            return valor;
        }

        private string ExtrairMotivoErroApi(JObject root, string mensagemPadrao)
        {
            var mensagemErro = ExtrairPrimeiraMensagemErro(root["errors"]);

            if (!string.IsNullOrWhiteSpace(mensagemErro))
            {
                return mensagemErro;
            }

            var title = root.Value<string>("title");
            if (!string.IsNullOrWhiteSpace(title))
            {
                return title;
            }

            var type = root.Value<string>("type");
            if (!string.IsNullOrWhiteSpace(type))
            {
                return type;
            }

            var status = root.Value<int?>("status");
            if (status.GetValueOrDefault() > 0)
            {
                return mensagemPadrao.TrimEnd('.') + " HTTP " + status + ".";
            }

            return mensagemPadrao;
        }

        private string ObterPrimeiroValorTexto(JToken token, params string[] nomes)
        {
            var encontrado = LocalizarTokenPorNome(token, nomes);
            if (encontrado == null || encontrado.Type == JTokenType.Null)
            {
                return null;
            }

            if (encontrado.Type == JTokenType.String ||
                encontrado.Type == JTokenType.Integer ||
                encontrado.Type == JTokenType.Float ||
                encontrado.Type == JTokenType.Boolean ||
                encontrado.Type == JTokenType.Date)
            {
                return encontrado.ToString();
            }

            return null;
        }

        private int? ObterPrimeiroValorInteiro(JToken token, params string[] nomes)
        {
            var valor = ObterPrimeiroValorTexto(token, nomes);
            if (int.TryParse(valor, out var retorno))
            {
                return retorno;
            }

            return null;
        }

        private string ObterValorTextoDireto(JObject token, params string[] nomes)
        {
            if (token == null || nomes == null)
            {
                return null;
            }

            foreach (var nome in nomes)
            {
                var valor = ObterFilhoPorNome(token, nome);
                if (valor == null || valor.Type == JTokenType.Null)
                {
                    continue;
                }

                if (valor.Type == JTokenType.String ||
                    valor.Type == JTokenType.Integer ||
                    valor.Type == JTokenType.Float ||
                    valor.Type == JTokenType.Boolean ||
                    valor.Type == JTokenType.Date)
                {
                    return valor.ToString();
                }
            }

            return null;
        }

        private bool? ObterPrimeiroValorBooleano(JToken token, params string[] nomes)
        {
            var valor = ObterPrimeiroValorTexto(token, nomes);
            if (bool.TryParse(valor, out var retorno))
            {
                return retorno;
            }

            if (string.Equals(valor, "1", StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }

            if (string.Equals(valor, "0", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            return null;
        }

        private bool? ObterValorBooleanoDireto(JObject token, params string[] nomes)
        {
            var valor = ObterValorTextoDireto(token, nomes);
            if (bool.TryParse(valor, out var retorno))
            {
                return retorno;
            }

            if (string.Equals(valor, "1", StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }

            if (string.Equals(valor, "0", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            return null;
        }

        private string ObterPrimeiroValorTextoPorCaminhos(JToken token, params string[][] caminhos)
        {
            if (caminhos == null)
            {
                return null;
            }

            foreach (var caminho in caminhos)
            {
                var encontrado = LocalizarTokenPorCaminho(token, caminho);
                if (encontrado == null || encontrado.Type == JTokenType.Null)
                {
                    continue;
                }

                if (encontrado.Type == JTokenType.String ||
                    encontrado.Type == JTokenType.Integer ||
                    encontrado.Type == JTokenType.Float ||
                    encontrado.Type == JTokenType.Boolean ||
                    encontrado.Type == JTokenType.Date)
                {
                    return encontrado.ToString();
                }
            }

            return null;
        }

        private bool? ObterPrimeiroValorBooleanoPorCaminhos(JToken token, params string[][] caminhos)
        {
            var valor = ObterPrimeiroValorTextoPorCaminhos(token, caminhos);
            if (bool.TryParse(valor, out var retorno))
            {
                return retorno;
            }

            if (string.Equals(valor, "1", StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }

            if (string.Equals(valor, "0", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            return null;
        }

        private JToken LocalizarTokenPorCaminho(JToken token, string[] caminho)
        {
            if (token == null || caminho == null || caminho.Length == 0)
            {
                return null;
            }

            var atual = LocalizarTokenPorNome(token, caminho[0]);
            if (atual == null)
            {
                return null;
            }

            for (var i = 1; i < caminho.Length; i++)
            {
                atual = ObterFilhoPorNome(atual, caminho[i]);
                if (atual == null)
                {
                    return null;
                }
            }

            return atual;
        }

        private JToken ObterFilhoPorNome(JToken token, string nome)
        {
            if (token == null || string.IsNullOrWhiteSpace(nome))
            {
                return null;
            }

            if (token.Type == JTokenType.Object)
            {
                foreach (var propriedade in token.Children<JProperty>())
                {
                    if (string.Equals(propriedade.Name, nome, StringComparison.OrdinalIgnoreCase))
                    {
                        return propriedade.Value;
                    }
                }
            }

            return null;
        }

        private JToken LocalizarTokenPorNome(JToken token, params string[] nomes)
        {
            if (token == null)
            {
                return null;
            }

            if (token.Type == JTokenType.Object)
            {
                foreach (var propriedade in token.Children<JProperty>())
                {
                    foreach (var nome in nomes)
                    {
                        if (string.Equals(propriedade.Name, nome, StringComparison.OrdinalIgnoreCase))
                        {
                            return propriedade.Value;
                        }
                    }

                    var encontrado = LocalizarTokenPorNome(propriedade.Value, nomes);
                    if (encontrado != null)
                    {
                        return encontrado;
                    }
                }
            }

            if (token.Type == JTokenType.Array)
            {
                foreach (var item in token.Children())
                {
                    var encontrado = LocalizarTokenPorNome(item, nomes);
                    if (encontrado != null)
                    {
                        return encontrado;
                    }
                }
            }

            return null;
        }

        private string NormalizarChave(string valor)
        {
            return (valor ?? string.Empty)
                .Replace("_", string.Empty)
                .Replace("-", string.Empty)
                .Replace(" ", string.Empty)
                .Trim()
                .ToUpperInvariant();
        }

        private string ExtrairMotivoErroUMessenger(JObject root)
        {
            return ExtrairMotivoErroApi(root, "Falha ao publicar mensagem.");
        }

        private string ExtrairPrimeiraMensagemErro(JToken errorsToken)
        {
            if (errorsToken == null || errorsToken.Type == JTokenType.Null)
            {
                return null;
            }

            switch (errorsToken.Type)
            {
                case JTokenType.String:
                    var valor = errorsToken.Value<string>();
                    return string.IsNullOrWhiteSpace(valor) ? null : valor;

                case JTokenType.Array:
                    foreach (var item in errorsToken.Children())
                    {
                        var mensagemArray = ExtrairPrimeiraMensagemErro(item);
                        if (!string.IsNullOrWhiteSpace(mensagemArray))
                        {
                            return mensagemArray;
                        }
                    }
                    break;

                case JTokenType.Object:
                    foreach (var propriedade in errorsToken.Children<JProperty>())
                    {
                        var mensagemObjeto = ExtrairPrimeiraMensagemErro(propriedade.Value);
                        if (!string.IsNullOrWhiteSpace(mensagemObjeto))
                        {
                            return mensagemObjeto;
                        }
                    }
                    break;
            }

            return null;
        }

        private XmlDocument ProcessXmlWithSafeEncoding(HttpResponseMessage response)
        {
            using (var stream = response.Content.ReadAsStreamAsync().Result)
            {
                var settings = new XmlReaderSettings
                {
                    DtdProcessing = DtdProcessing.Prohibit,
                    CloseInput = true
                };

                using (var reader = XmlReader.Create(stream, settings))
                {
                    var xmlDoc = new XmlDocument();
                    xmlDoc.Load(reader);
                    return xmlDoc;
                }
            }
        }

        private string HtmlToPlainText(string html)
        {
            const string tagWhiteSpace = @"(>|$)(\W|\n|\r)+<";
            const string stripFormatting = @"<[^>]*(>|$)";
            const string lineBreak = @"<(br|BR)\s{0,1}\/{0,1}>";
            var lineBreakRegex = new Regex(lineBreak, RegexOptions.Multiline);
            var stripFormattingRegex = new Regex(stripFormatting, RegexOptions.Multiline);
            var tagWhiteSpaceRegex = new Regex(tagWhiteSpace, RegexOptions.Multiline);

            var text = html;
            text = System.Net.WebUtility.HtmlDecode(text);
            text = tagWhiteSpaceRegex.Replace(text, "><");
            text = lineBreakRegex.Replace(text, Environment.NewLine);
            text = stripFormattingRegex.Replace(text, string.Empty);

            return text;
        }
    }
}
