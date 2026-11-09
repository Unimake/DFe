using System;
using System.Globalization;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class ReconhecedorRespostaCIOT
    {
        internal static EvidenciaRespostaDisponibilidade Reconhecer(Configuracao config, XmlElement raiz)
        {
            var r = new EvidenciaRespostaDisponibilidade();
            var servico = config.Servico.ToString();
            if (!servico.StartsWith("CIOT", StringComparison.Ordinal) || raiz.LocalName != "Ret" + servico.Substring(4) ||
                raiz.NamespaceURI != Xml.CIOT.CIOTNamespace.PortalANTT) return r;
            r.Contrato = "CIOT.Codigo";
            var erros = raiz.SelectSingleNode("./*[local-name()='temp']/*[local-name()='error']");
            var no = raiz.SelectSingleNode("./*[local-name()='Codigo'] | ./*[local-name()='Mensagens']/*[local-name()='Mensagem']/*[local-name()='Codigo']");
            var item = no?.SelectSingleNode("./*[local-name()='Item'][1]");
            var codigo = erros?.InnerText ?? item?.InnerText ?? no?.InnerText;
            if (codigo == "USUARIO_NAO_AUTORIZADO")
            {
                r.Reconhecida = true;
                r.TipoFalha = TipoFalhaDisponibilidade.Autenticacao;
                return r;
            }
            bool sucesso;
            var noSucesso = raiz.SelectSingleNode("./*[local-name()='Sucesso']");
            var sucessoExplicito = noSucesso != null && bool.TryParse(noSucesso.InnerText, out sucesso) && sucesso;
            int numero;
            var numerico = int.TryParse(codigo, NumberStyles.None, CultureInfo.InvariantCulture, out numero) && numero >= 0;
            if (numerico) r.Codigo = numero;
            r.Reconhecida = noSucesso != null || !string.IsNullOrWhiteSpace(codigo);
            if (noSucesso != null && !bool.TryParse(noSucesso.InnerText, out sucesso)) r.Reconhecida = false;
            var erro = erros != null || (noSucesso != null && !sucessoExplicito);
            r.Inconclusiva = erro || !(sucessoExplicito || numerico && (numero == 0 || numero == 110));
            if (r.Inconclusiva) r.Limitacao = "A resposta CIOT foi recebida, mas seu erro ou código não permite confirmar funcionamento normal nem atribuir indisponibilidade ao provedor.";
            if (config.Servico == Servico.CIOTObterOperacaoTransportePdf && !erro && r.Reconhecida && !r.Inconclusiva)
            {
                var pdf = raiz.SelectSingleNode("./*[local-name()='Pdf']");
                r.Inconclusiva = pdf == null || !VerificadorRespostaPdf.Base64Completo(pdf.InnerText);
                if (r.Inconclusiva) r.Limitacao = "O retorno declara sucesso, mas o PDF recebido não possui estrutura mínima completa reconhecida.";
            }
            return r;
        }
    }
}
