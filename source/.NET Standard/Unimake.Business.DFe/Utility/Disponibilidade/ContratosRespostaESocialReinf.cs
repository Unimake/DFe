using System;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class ContratosRespostaESocialReinf
    {
        // Metadados lidos uma vez dos modelos já usados pelos serviços, sem nova desserialização
        // ou leitura de XSD no caminho fiscal. Versão de retorno não é a versão do XML de envio.
        private static readonly Contrato Envio = new Contrato(typeof(Xml.ESocial.Retorno.RetornoEnvioLote),
            "eSocial.cdResposta", "retornoEnvioLoteEventos", "status", "cdResposta");
        private static readonly Contrato Processamento = new Contrato(typeof(Xml.ESocial.Retorno.RetornoEventoProcessado),
            "eSocial.cdResposta", "retornoProcessamentoLoteEventos", "status", "cdResposta");
        private static readonly Contrato LoteReinf = new Contrato(typeof(Xml.EFDReinf.ReinfRetornoLoteAssincrono),
            "Reinf.cdResposta", "retornoLoteEventosAssincrono", "status", "cdResposta");
        private static readonly Contrato ReciboReinf = new Contrato(typeof(Xml.EFDReinf.ReinfRetornoRecibo),
            "Reinf.cdRetorno", "ideStatus", "cdRetorno");
        // Identificadores: RetornoConsultaIdentificadoresEventos-v1_0_0.xsd versionado.
        private static readonly Contrato Identificadores = new Contrato(
            "http://www.esocial.gov.br/schema/consulta/identificadores-eventos/retorno/v1_0_0",
            "eSocial.cdResposta", "retornoConsultaIdentificadoresEvts", "status", "cdResposta");
        // Download: RetornoSolicitacaoDownloadEventos-v1_0_0.xsd, pacote oficial de comunicação
        // eSocial v1.6. Evidência e hash em docsplan/diagnostico/INTERPRETACAO.md.
        private static readonly Contrato Download = new Contrato(
            "http://www.esocial.gov.br/schema/download/solicitacao/retorno/v1_0_0",
            "eSocial.cdResposta", "download", "status", "cdResposta");

        internal static XmlNode LocalizarCodigo(Configuracao config, XmlElement raiz, EvidenciaRespostaDisponibilidade evidencia)
        {
            var contrato = Selecionar(config);
            if (contrato == null || raiz.LocalName != contrato.Raiz || raiz.NamespaceURI != contrato.Namespace)
            {
                evidencia.Limitacao = "O contrato da resposta não corresponde ao serviço/operação ou à versão de retorno reconhecida.";
                return null;
            }

            var atual = raiz;
            foreach (var nome in contrato.Caminho)
            {
                atual = Filho(atual, nome, contrato.Namespace);
                if (atual == null)
                {
                    evidencia.Limitacao = "O status principal do contrato esperado está ausente ou possui estrutura/namespace incompatível.";
                    return null;
                }
            }
            evidencia.Contrato = contrato.Nome;
            return atual;
        }

        private static Contrato Selecionar(Configuracao config)
        {
            if (config.TipoDFe == TipoDFe.EFDReinf)
            {
                switch (config.Servico)
                {
                    case Servico.EFDReinfRecepcionarLoteAssincrono:
                    case Servico.EFDReinfConsultaLoteAssincrono: return LoteReinf;
                    case Servico.EFDReinfConsultaReciboEvento: return ReciboReinf;
                    default: return null;
                }
            }
            if (config.TipoDFe != TipoDFe.ESocial) return null;
            switch (config.Servico)
            {
                case Servico.ESocialEnviarLoteEventos: return Envio;
                case Servico.ESocialDownloadEvts: return Download;
                case Servico.ESocialConsultaEvts:
                    switch (config.NomeServicoObservacaoDisponibilidade)
                    {
                        case nameof(Servicos.ESocial.ConsultaLoteAssincrono): return Processamento;
                        case nameof(Servicos.ESocial.ConsultarEvtsEmpregador):
                        case nameof(Servicos.ESocial.ConsultarEvtsTabela):
                        case nameof(Servicos.ESocial.ConsultarEvtsTrabalhador): return Identificadores;
                    }
                    // Consumidores de baixo nível podem informar o wrapper SOAP sem chamar Load.
                    // Aceitar somente essa identidade explícita, nunca deduzi-la do XML recebido.
                    if (!string.IsNullOrEmpty(config.NomeServicoObservacaoDisponibilidade)) return null;
                    switch (config.WebTagRetorno)
                    {
                        case "ConsultarLoteEventosResult": return Processamento;
                        case "ConsultarIdentificadoresEventosEmpregadorResult":
                        case "ConsultarIdentificadoresEventosTabelaResult":
                        case "ConsultarIdentificadoresEventosTrabalhadorResult": return Identificadores;
                        default: return null;
                    }
                default: return null;
            }
        }

        private static XmlElement Filho(XmlElement pai, string nome, string ns)
        {
            foreach (XmlNode node in pai.ChildNodes)
            {
                var filho = node as XmlElement;
                if (filho != null && filho.LocalName == nome && filho.NamespaceURI == ns) return filho;
            }
            return null;
        }

        private sealed class Contrato
        {
            internal readonly string Raiz;
            internal readonly string Namespace;
            internal readonly string Nome;
            internal readonly string[] Caminho;

            internal Contrato(Type modelo, string nome, params string[] caminho)
            {
                var raiz = (XmlRootAttribute)Attribute.GetCustomAttribute(modelo, typeof(XmlRootAttribute));
                Raiz = raiz.ElementName;
                Namespace = raiz.Namespace;
                Nome = nome;
                Caminho = caminho;
            }

            internal Contrato(string ns, string nome, params string[] caminho)
            {
                Raiz = "eSocial";
                Namespace = ns;
                Nome = nome;
                Caminho = caminho;
            }
        }
    }
}
