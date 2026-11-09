using System;
using System.Globalization;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class ReconhecedorRespostaNFSe
    {
        internal static EvidenciaRespostaDisponibilidade Reconhecer(Configuracao config, XmlElement raiz)
        {
            if (config.PadraoNFSe == PadraoNFSe.NACIONAL) return Nacional(config, raiz);
            var r = new EvidenciaRespostaDisponibilidade();
            if (!RaizDoServico(config.Servico, raiz.LocalName) && raiz.LocalName != config.WebTagRetorno) return r;
            var catalogada = CatalogoRespostasNFSe.Aceita(config.PadraoNFSe, raiz);
            // Alguns parsers removem namespaces. Nesse caso exigir raiz do serviço e marcadores
            // de negócio; uma raiz configurada (por vezes apenas wrapper SOAP) não basta.
            if (!catalogada) return r;
            var erros = raiz.SelectNodes("./*[local-name()='ListaMensagemRetorno' or local-name()='ListaMensagemRetornoLote']//*[local-name()='MensagemRetorno']/*[local-name()='Codigo'] | ./*[local-name()='Erro' or local-name()='MensagemRetorno' or local-name()='mensagemRetorno']/*[local-name()='Codigo' or local-name()='codigo']");
            r.Contrato = "NFSe.municipal";
            if (erros.Count > 0)
            {
                ClassificarCodigos(r, erros, config.PadraoNFSe, false);
                return r;
            }
            var situacao = raiz.SelectSingleNode("./*[local-name()='Situacao' or local-name()='stLote']");
            int valor;
            if (situacao != null && int.TryParse(situacao.InnerText, out valor) && valor >= 1 && valor <= 4)
            {
                r.Codigo = valor;
                r.Reconhecida = true; // Recebimento/processamento do lote, não autorização da nota.
                return r;
            }
            var sucesso = raiz.SelectSingleNode("./*[local-name()='Sucesso' or local-name()='sucesso'] | ./*[local-name()='Cabecalho']/*[local-name()='Sucesso']");
            if (sucesso != null)
            {
                try
                {
                    var aceito = XmlConvert.ToBoolean(sucesso.InnerText);
                    r.Reconhecida = true;
                    r.Inconclusiva = !aceito;
                    if (r.Inconclusiva) r.Limitacao = "A prefeitura recusou a operação, mas não informou código reconhecido que permita distinguir rejeição de negócio de falha técnica.";
                    return r;
                }
                catch (FormatException) { return r; }
            }
            var dados = raiz.SelectSingleNode("./*[local-name()='Protocolo' or local-name()='protocolo'] | .//*[local-name()='Nfse' or local-name()='nfse' or local-name()='InfNfse' or local-name()='DadosCadastroPrestador' or local-name()='Cadastro' or local-name()='Confirmacao' or local-name()='Confimacao' or local-name()='NfseCancelamento' or local-name()='dtCancelamento' or local-name()='ChaveNFeRPS']");
            r.Reconhecida = dados != null && !string.IsNullOrWhiteSpace(dados.InnerText);
            var consulta = config.Servico.ToString().StartsWith("NFSeConsult", StringComparison.Ordinal);
            if (!r.Reconhecida && catalogada && consulta)
                r.Reconhecida = raiz.SelectSingleNode("./*[local-name()='ListaNfse' or local-name()='listaNfse' or local-name()='ListaLinks' or local-name()='ListaDpsDisponivel']") != null;
            return r;
        }

        private static EvidenciaRespostaDisponibilidade Nacional(Configuracao config, XmlElement raiz)
        {
            var r = new EvidenciaRespostaDisponibilidade { Contrato = "NFSe.nacional" };
            const string ns = "http://www.sped.fazenda.gov.br/nfse";
            var evento = config.Servico == Servico.NFSeRecepcionarEventosDiversos || config.Servico == Servico.NFSeConsultarEventosDiversos || config.Servico == Servico.NFSeCancelarNfse;
            if (raiz.NamespaceURI == ns)
            {
                if (raiz.LocalName == "NFSe" && (config.Servico == Servico.NFSeGerarNfse || config.Servico == Servico.NFSeConsultarNfse))
                    r.Reconhecida = !string.IsNullOrWhiteSpace(raiz.SelectSingleNode("./*[local-name()='infNFSe']/*[local-name()='nNFSe']")?.InnerText);
                if (evento && raiz.LocalName == "evento")
                    r.Reconhecida = raiz.SelectSingleNode("./*[local-name()='infEvento']/*[local-name()='dhProc'] | ./*[local-name()='infEvento']/*[local-name()='verAplic']") != null;
                return r;
            }
            if (raiz.LocalName != "temp" || raiz.NamespaceURI.Length != 0) return r;
            var erros = raiz.SelectNodes("./*[local-name()='erro' or local-name()='erros' or local-name()='Erros']/*[local-name()='Codigo' or local-name()='codigo'] | ./*[local-name()='Erros']/*[local-name()='Mensagem']/*[local-name()='Codigo']");
            if (erros.Count > 0)
            {
                // Envelope nacional: não confundir um temp de JSON arbitrário com resposta da Sefin/ADN.
                if (raiz.SelectSingleNode("./*[local-name()='tipoAmbiente' or local-name()='TipoAmbiente']") == null) return r;
                ClassificarCodigos(r, erros, config.PadraoNFSe, true);
                return r;
            }
            if (config.Servico == Servico.NFSeConsultarNfsePorRps)
            {
                var chave = raiz.SelectSingleNode("./*[local-name()='chaveAcesso']");
                r.Reconhecida = chave != null && chave.InnerText.Length == 50 && ApenasDigitos(chave.InnerText);
            }
            else if (config.Servico == Servico.NFSeConsultarDistribuicaoNFSeNSU || config.Servico == Servico.NFSeConsultarEventosNFSeChaveAcesso)
            {
                var status = raiz.SelectSingleNode("./*[local-name()='StatusProcessamento']");
                r.Reconhecida = status != null && (status.InnerText == "DOCUMENTOS_LOCALIZADOS" || status.InnerText == "NENHUM_DOCUMENTO_LOCALIZADO");
                if (status != null && status.InnerText == "REJEICAO")
                {
                    r.Reconhecida = true;
                    r.Inconclusiva = true;
                    r.Limitacao = "O ADN respondeu com rejeição sem código conhecido; não é possível atribuir indisponibilidade.";
                }
            }
            else if (config.Servico == Servico.NFSeConsultarEventosDiversos)
            {
                var eventos = raiz.SelectSingleNode("./*[local-name()='eventos']");
                r.Reconhecida = eventos != null && eventos.SelectSingleNode(".//*[local-name()='evento' and namespace-uri()='" + ns + "']/*[local-name()='infEvento']") != null;
            }
            return r;
        }

        private static void ClassificarCodigos(EvidenciaRespostaDisponibilidade r, XmlNodeList erros, PadraoNFSe padrao, bool nacional)
        {
            r.Reconhecida = true;
            var quantidade = 0;
            foreach (XmlNode erro in erros)
            {
                if (++quantidade > 256) { r.Inconclusiva = true; break; }
                var codigo = erro.InnerText.Trim();
                if (codigo.Length == 0 || codigo.Length > 16) { r.Inconclusiva = true; continue; }
                if (nacional)
                {
                    if (codigo == "E1200" || codigo == "E1203" || codigo == "E1205" || codigo == "E1206" || codigo == "E1207" || codigo == "E1208" || codigo == "E1209")
                        r.TipoFalha = TipoFalhaDisponibilidade.Certificado;
                    else if (!CodigosRespostaNFSeNacional.Conhecidos.Contains(codigo)) r.Inconclusiva = true;
                }
                else
                {
                    // Códigos efetivamente publicados pelos fornecedores, sem usar faixas inferidas.
                    var publica = padrao == PadraoNFSe.PUBLICA && (codigo == "E1" || codigo == "E2" || codigo == "E3" || codigo == "E6" || codigo == "E8" || codigo == "E9");
                    var ginfes = (padrao == PadraoNFSe.GINFES || padrao == PadraoNFSe.GISSONLINE) && codigo == "E160";
                    if (!publica && !ginfes) r.Inconclusiva = true;
                }
            }
            if (r.Inconclusiva) r.Limitacao = "O contrato de resposta NFS-e foi reconhecido, mas há código sem interpretação cadastrada. Isso não confirma funcionamento normal nem indisponibilidade.";
        }

        private static bool ApenasDigitos(string texto)
        {
            foreach (var c in texto) if (c < '0' || c > '9') return false;
            return true;
        }

        private static bool RaizDoServico(Servico servico, string raiz)
        {
            var nome = servico.ToString();
            if (!nome.StartsWith("NFSe", StringComparison.Ordinal)) return false;
            var operacao = nome.Substring(4);
            var retorno = raiz.StartsWith("Retorno", StringComparison.Ordinal) ? raiz.Substring(7) : raiz;
            if (retorno.StartsWith("es", StringComparison.Ordinal)) retorno = retorno.Substring(2);
            if (retorno.EndsWith("Resposta", StringComparison.Ordinal)) retorno = retorno.Substring(0, retorno.Length - 8);
            if (retorno.EndsWith("Response", StringComparison.Ordinal)) retorno = retorno.Substring(0, retorno.Length - 8);
            if (retorno.EndsWith("Envio", StringComparison.Ordinal)) retorno = retorno.Substring(0, retorno.Length - 5);
            operacao = operacao.Replace("RecepcionarLoteRps", "EnviarLoteRps").Replace("PorRps", "Rps");
            retorno = retorno.Replace("Dps", "Rps");
            if (string.Equals(operacao, retorno, StringComparison.OrdinalIgnoreCase)) return true;
            if (operacao == "ConsultarNfse" && retorno == "ConsultarNfseServicoPrestado") return true;
            if (operacao == "ConsultarLoteRps" && retorno == "ConsultaSituacaoLote") return true;
            if ((operacao == "ConsultaNFeEmitidas" || operacao == "ConsultaNFeRecebidas" || operacao == "ConsultarNfse") && retorno == "Consulta") return true;
            if (operacao == "CancelarNfse" && retorno == "CancelamentoNFe") return true;
            if (operacao == "RecepcionarEventosDiversos" && retorno == "RecepcionarEventoCancelamento") return true;
            return false;
        }
    }
}
