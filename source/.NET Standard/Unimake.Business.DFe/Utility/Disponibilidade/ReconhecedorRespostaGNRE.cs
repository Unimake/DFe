using System.Globalization;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class ReconhecedorRespostaGNRE
    {
        // Manual do contribuinte GNRE 2.15, quadros IV–VII e anexo I.
        // Código 503 de negócio significa recibo inválido; não é HTTP 503.
        internal static EvidenciaRespostaDisponibilidade Reconhecer(Configuracao config, XmlElement raiz)
        {
            var r = new EvidenciaRespostaDisponibilidade();
            if (raiz.NamespaceURI != "http://www.gnre.pe.gov.br") return r;
            // ConsultaLoteRecepcao historicamente utiliza GNREConsultaResultadoLote na configuração.
            var recepcao = config.Servico == Servico.GNRELoteRecepcao || config.Servico == Servico.GNREConsultaLoteRecepcao ||
                config.Servico == Servico.GNREConsultaResultadoLote && raiz.LocalName == "TRetLote_GNRE";
            var resultado = config.Servico == Servico.GNREConsultaResultadoLote || config.Servico == Servico.GNREConsultaResultadoLoteConsulta;
            var grupo = recepcao && raiz.LocalName == "TRetLote_GNRE" ? "situacaoRecepcao" :
                resultado && raiz.LocalName == "TResultLote_GNRE" ? "situacaoProcess" :
                config.Servico == Servico.GNREConsultaConfigUF && raiz.LocalName == "TConfigUf" ? "situacaoConsulta" : null;
            if (grupo == null) return r;
            var node = raiz.SelectSingleNode("./*[local-name()='" + grupo + "']/*[local-name()='codigo']");
            int codigo;
            if (node == null || node.InnerText.Length != 3 || !int.TryParse(node.InnerText, NumberStyles.None, CultureInfo.InvariantCulture, out codigo)) return r;
            r.Contrato = "GNRE.codigo";
            r.Codigo = codigo;
            r.Reconhecida = true;
            if (codigo == 101) r.TipoFalha = TipoFalhaDisponibilidade.Certificado;
            else if (codigo == 102 || codigo == 154 || (resultado && codigo == 603)) r.TipoFalha = TipoFalhaDisponibilidade.Autenticacao;
            else if (codigo == 198) r.TipoFalha = TipoFalhaDisponibilidade.Configuracao;
            else if (codigo == 197 || (resultado && (codigo == 404 || codigo == 604))) r.FalhaAplicacao = true;
            else
            {
                var controle = codigo == 103 || codigo == 153 || codigo == 155 || codigo == 156 || codigo == 157 || codigo == 199 ||
                    codigo == 301 || codigo == 302 || codigo == 303;
                var conhecido = controle || (recepcao && codigo == 100) ||
                    (resultado && (codigo >= 400 && codigo <= 403 || codigo >= 501 && codigo <= 504 || codigo == 601 || codigo == 602)) ||
                    (grupo == "situacaoConsulta" && (codigo >= 450 && codigo <= 455 || codigo == 501 || codigo == 502));
                r.Inconclusiva = !conhecido;
            }
            if (r.Inconclusiva) r.Limitacao = "O portal GNRE respondeu no contrato esperado, mas o código principal ainda não possui interpretação cadastrada.";
            if (r.FalhaAplicacao) r.Limitacao = "O portal GNRE declarou falha no processamento desta operação; isso não confirma indisponibilidade geral nem a situação atual de outra UF.";
            // Não examinar rejeições antigas de guias para inferir a disponibilidade atual do portal/UF.
            return r;
        }
    }
}
