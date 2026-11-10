using System;
using System.Globalization;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    // Somente marcadores do contrato, nunca mensagens, identificadores ou o corpo remoto.
    internal sealed class EvidenciaRespostaDisponibilidade
    {
        internal string Contrato = string.Empty;
        internal int Codigo;
        internal bool Reconhecida;
        internal bool FalhaAplicacao;
        internal bool Inconclusiva;
        internal TipoFalhaDisponibilidade TipoFalha;
        internal string Limitacao = string.Empty;
    }

    internal static class ReconhecedorRespostaDisponibilidade
    {
        internal static bool UsaCStat(TipoDFe tipo) => tipo == TipoDFe.NFe || tipo == TipoDFe.NFCe ||
            tipo == TipoDFe.CTe || tipo == TipoDFe.CTeOS || tipo == TipoDFe.MDFe || tipo == TipoDFe.BPe ||
            tipo == TipoDFe.NF3e || tipo == TipoDFe.NFCom || tipo == TipoDFe.NFGas || tipo == TipoDFe.DCe ||
            tipo == TipoDFe.CCG || tipo == TipoDFe.NFeABI;

        internal static bool EhFault(XmlElement raiz) => raiz != null &&
            (raiz.LocalName == "Fault" || raiz.SelectSingleNode("./*[local-name()='Fault'] | ./*[local-name()='Body']/*[local-name()='Fault']") != null);

        internal static EvidenciaRespostaDisponibilidade Reconhecer(Configuracao config, XmlDocument retorno)
        {
            var evidencia = new EvidenciaRespostaDisponibilidade();
            var raiz = retorno?.DocumentElement;
            if (raiz == null || EhFault(raiz))
                return evidencia;

            XmlNode codigo = null;
            switch (config.TipoDFe)
            {
                case TipoDFe.ESocial:
                case TipoDFe.EFDReinf:
                    codigo = ContratosRespostaESocialReinf.LocalizarCodigo(config, raiz, evidencia);
                    break;
                case TipoDFe.GNRE:
                    return ReconhecedorRespostaGNRE.Reconhecer(config, raiz);
                case TipoDFe.CIOT:
                    return ReconhecedorRespostaCIOT.Reconhecer(config, raiz);
                case TipoDFe.DARE:
                    if (config.Servico == Servico.DAREEnvio && raiz.LocalName == "DareRetorno")
                        evidencia.Reconhecida = raiz.SelectSingleNode("./*[local-name()='DARE']/*[local-name()='numeroControleDarePrincipal' or local-name()='codigoBarra44']") != null;
                    else if (config.Servico == Servico.DAREEnvio && raiz.LocalName == "DareLoteRetorno")
                        evidencia.Reconhecida = raiz.SelectSingleNode("./*[local-name()='itensParaGeracao' or local-name()='errors']/*") != null;
                    else if (config.Servico == Servico.DAREReceita && raiz.LocalName == "Receitas")
                        evidencia.Reconhecida = raiz.SelectSingleNode("./*[local-name()='consulta' or local-name()='Receita']") != null;
                    evidencia.Contrato = evidencia.Reconhecida ? "DARE.retorno" : string.Empty;
                    return evidencia;
                case TipoDFe.EBoleto:
                case TipoDFe.PIX:
                    // Raízes obtidas dos modelos públicos de retorno; não aceitar Status de um HTML/JSON genérico.
                    if (!RaizIntegracao(config.Servico, raiz.LocalName)) break;
                    codigo = raiz.SelectSingleNode("./*[local-name()='Status']");
                    evidencia.Contrato = "Unimake.Status";
                    break;
                case TipoDFe.UMessenger:
                    if (raiz.LocalName != "uMessengerResponse") break;
                    var mensagens = raiz.SelectNodes("./*[local-name()='Mensagem']");
                    if (mensagens.Count == 0) break;
                    evidencia.Contrato = "uMessenger.Status";
                    evidencia.Reconhecida = true;
                    foreach (XmlNode mensagem in mensagens)
                    {
                        var status = 0;
                        var no = mensagem.SelectSingleNode("./*[local-name()='Status']");
                        if (no == null || !int.TryParse(no.InnerText, out status)) evidencia.Reconhecida = false;
                        if (status >= 500) evidencia.FalhaAplicacao = true;
                    }
                    return evidencia;
                case TipoDFe.NFSe:
                    return ReconhecedorRespostaNFSe.Reconhecer(config, raiz);
            }
            int valor;
            if (codigo != null && int.TryParse(codigo.InnerText, NumberStyles.Integer, CultureInfo.InvariantCulture, out valor) && valor >= 0)
            {
                evidencia.Codigo = valor;
                evidencia.Reconhecida = valor > 0 || evidencia.Contrato == "Reinf.cdRetorno" || evidencia.Contrato == "Unimake.Status" || evidencia.Contrato == "CIOT.Codigo";
                evidencia.FalhaAplicacao = (evidencia.Contrato == "eSocial.cdResposta" && valor == 301) ||
                    (evidencia.Contrato == "Reinf.cdResposta" && valor == 99) ||
                    (evidencia.Contrato == "Unimake.Status" && valor >= 500);
            }
            return evidencia;
        }

        private static bool RaizIntegracao(Servico servico, string raiz)
        {
            switch (servico)
            {
                case Servico.EBoletoRegistrar: return raiz == "BoletoRegistrarResponse";
                case Servico.EBoletoAlterarVencto: return raiz == "BoletoAlterarVenctoResponse";
                case Servico.EBoletoCancelar: return raiz == "BoletoCancelarResponse";
                case Servico.EBoletoConsultar: return raiz == "BoletoConsultarResponse";
                case Servico.EBoletoEnviarInstrucao: return raiz == "BoletoEnviarInstrucaoResponse";
                case Servico.EBoletoInformarPagt: return raiz == "BoletoInformarPagtoResponse";
                case Servico.PIXCobrancaCriar: return raiz == "PIXCobrancaCreateResponse";
                case Servico.PIXCobrancaConsultar: return raiz == "PIXConsultaResponse";
                case Servico.PIXConsultar: return raiz == "PIXGetResponse";
                default: return false;
            }
        }
    }
}
