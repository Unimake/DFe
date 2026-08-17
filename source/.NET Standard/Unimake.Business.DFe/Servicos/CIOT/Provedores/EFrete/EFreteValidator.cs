using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal static class EFreteValidator
    {
        internal static void Validar(XMLBase xml, Servico servico, Configuracao configuracao)
        {
            ValidarServicoSuportado(servico);
            if (string.IsNullOrWhiteSpace(configuracao.EFreteIntegrador)) throw new ValidarXMLException("Para utilizar a eFrete, informe Configuracao.EFreteIntegrador.");

            if (servico == Servico.CIOTDeclaracaoOperacaoTransporte) ValidarDeclaracao((Xml.CIOT.DeclaracaoOperacaoTransporte)xml);
            else if (servico == Servico.CIOTConsultarCIOTGerado)
            {
                var consulta = (Xml.CIOT.ConsultarCIOTGerado)xml;
                if (string.IsNullOrWhiteSpace(consulta.MatrizCNPJ) || string.IsNullOrWhiteSpace(consulta.IdOperacaoCliente)) throw new ValidarXMLException("MatrizCNPJ e IdOperacaoCliente são obrigatórios na consulta eFrete.");
            }
            else if (servico == Servico.CIOTCancelamentoOperacaoTransporte)
            {
                var cancelamento = (Xml.CIOT.CancelamentoOperacaoTransporte)xml;
                if (string.IsNullOrWhiteSpace(cancelamento.CodigoIdentificacaoOperacao) || string.IsNullOrWhiteSpace(cancelamento.MotivoCancelamento)) throw new ValidarXMLException("CodigoIdentificacaoOperacao e MotivoCancelamento são obrigatórios no cancelamento eFrete.");
            }
            else if (servico == Servico.CIOTEncerramentoOperacaoTransporte && string.IsNullOrWhiteSpace(((Xml.CIOT.EncerramentoOperacaoTransporte)xml).CodigoIdentificacaoOperacao))
            {
                throw new ValidarXMLException("CodigoIdentificacaoOperacao é obrigatório no encerramento eFrete.");
            }
            else if (servico == Servico.CIOTConsultarSituacaoTransportador || servico == Servico.CIOTConsultarFrotaTransportador) ValidarSituacao(xml, servico);
        }

        internal static void ValidarServicoSuportado(Servico servico)
        {
            if (servico == Servico.CIOTGerarIdOperacaoTransporte || servico == Servico.CIOTRetificacaoOperacaoTransporte || servico == Servico.CIOTConsultarExcecao)
                throw new NotSupportedException("O serviço " + servico + " não é suportado pela eFrete. A emissão começa em DeclaracaoOperacaoTransporte e a retificação não está disponível na API eFrete v8.1.");
        }

        private static void ValidarDeclaracao(Xml.CIOT.DeclaracaoOperacaoTransporte declaracao)
        {
            if (string.IsNullOrWhiteSpace(declaracao.IdOperacaoCliente)) throw new ValidarXMLException("A tag IdOperacaoCliente é obrigatória para a eFrete.");
            if (string.IsNullOrWhiteSpace(declaracao.MatrizCNPJ)) throw new ValidarXMLException("A tag MatrizCNPJ é obrigatória para a eFrete.");
            if (!declaracao.ShouldSerializeDataFimViagemField()) throw new ValidarXMLException("A tag DataFimViagem é obrigatória para a eFrete.");
            if (string.IsNullOrWhiteSpace(declaracao.CpfCnpjContratado) && string.IsNullOrWhiteSpace(declaracao.Contratado?.CpfOuCnpj)) throw new ValidarXMLException("O CPF/CNPJ do Contratado é obrigatório para a eFrete.");
            if (string.IsNullOrWhiteSpace(declaracao.RNTRCContratado) && string.IsNullOrWhiteSpace(declaracao.Contratado?.RNTRC)) throw new ValidarXMLException("O RNTRC do Contratado é obrigatório para a eFrete.");
            if (declaracao.Motorista == null || string.IsNullOrWhiteSpace(declaracao.Motorista.CpfOuCnpj) || string.IsNullOrWhiteSpace(declaracao.Motorista.CNH) || declaracao.Motorista.Celular == null || string.IsNullOrWhiteSpace(declaracao.Motorista.Celular.DDD) || string.IsNullOrWhiteSpace(declaracao.Motorista.Celular.Numero)) throw new ValidarXMLException("Motorista, CPF, CNH e celular são obrigatórios para a eFrete.");
            ValidarPessoa(declaracao.Contratante, "Contratante", true);
            if (declaracao.Veiculos == null || declaracao.Veiculos.Count == 0 || declaracao.Veiculos.Count > 5) throw new ValidarXMLException("A eFrete exige de um a cinco veículos na operação.");
            if (declaracao.Impostos == null) throw new ValidarXMLException("O grupo Impostos é obrigatório para a eFrete, mesmo quando os valores forem zero.");
            if (string.IsNullOrWhiteSpace(declaracao.TipoPagamentoEFrete)) throw new ValidarXMLException("TipoPagamentoEFrete é obrigatório para a eFrete.");

            var tac = declaracao.TipoOperacao == TipoOperacaoTransporteCIOT.TACAgregado;
            if (!tac && !declaracao.TemDataInicioViagemEFrete()) throw new ValidarXMLException("DataInicioViagem é obrigatória para lotação e fracionado na eFrete.");
            if (!tac && (declaracao.OrigemDestino == null || declaracao.OrigemDestino.Count == 0)) throw new ValidarXMLException("Ao menos uma viagem em OrigemDestino é obrigatória para CIOT de lotação ou fracionado na eFrete.");
            if (!tac && (declaracao.DadosCarga == null || string.IsNullOrWhiteSpace(declaracao.DadosCarga.CodigoNaturezaCarga) || string.IsNullOrWhiteSpace(declaracao.DadosCarga.PesoCarga))) throw new ValidarXMLException("DadosCarga, NCM e peso são obrigatórios para lotação e fracionado na eFrete.");
            if (!tac) ValidarPessoa(declaracao.Destinatario, "Destinatário", false);
            if (!tac && declaracao.InfIndicadoresOperacionais == null) throw new ValidarXMLException("InfIndicadoresOperacionais é obrigatório para lotação e fracionado na eFrete.");
            if (declaracao.TipoOperacao == TipoOperacaoTransporteCIOT.CargaFracionada && (declaracao.DadosCarga?.ContratantesCargFrac == null || declaracao.DadosCarga.ContratantesCargFrac.Count == 0)) throw new ValidarXMLException("ContratantesCargFrac é obrigatório para carga fracionada na eFrete.");
            if (declaracao.TipoOperacao != TipoOperacaoTransporteCIOT.CargaFracionada && declaracao.DadosCarga?.ContratantesCargFrac?.Count > 0) throw new ValidarXMLException("ContratantesCargFrac somente pode ser informado para carga fracionada na eFrete.");
            if (tac && (declaracao.TemDataInicioViagemEFrete() || declaracao.OrigemDestino?.Count > 0 || declaracao.DadosCarga != null || declaracao.Destinatario != null || declaracao.InfIndicadoresOperacionais != null)) throw new ValidarXMLException("DataInicioViagem, viagens, carga, destinatário e indicadores não podem ser informados para TAC agregado na eFrete.");
            if (tac && string.IsNullOrWhiteSpace(declaracao.Contratante.RNTRC)) throw new ValidarXMLException("O RNTRC do Contratante é obrigatório para TAC agregado na eFrete.");
            ValidarViagens(declaracao.OrigemDestino);
            ValidarPagamentos(declaracao.InfPagamento);
        }

        private static void ValidarPagamentos(List<Xml.CIOT.InfPagamento> pagamentos)
        {
            if (pagamentos == null) return;
            foreach (var p in pagamentos)
            {
                if (string.IsNullOrWhiteSpace(p.IdPagamentoCliente) || string.IsNullOrWhiteSpace(p.DataDeLiberacao) || string.IsNullOrWhiteSpace(p.Categoria) || string.IsNullOrWhiteSpace(p.Documento) || string.IsNullOrWhiteSpace(p.CpfCnpjCreditado)) throw new ValidarXMLException("IdPagamentoCliente, DataDeLiberacao, Categoria, Documento e CpfCnpjCreditado são obrigatórios em cada pagamento eFrete.");
                var banco = !string.IsNullOrWhiteSpace(p.CodigoInstituicaoFinanceira) || !string.IsNullOrWhiteSpace(p.NumeroAgencia) || !string.IsNullOrWhiteSpace(p.NumeroConta);
                var pix = !string.IsNullOrWhiteSpace(p.TipoChavePix) || !string.IsNullOrWhiteSpace(p.ChavePix);
                if (banco && pix) throw new ValidarXMLException("Informe somente dados bancários ou dados PIX em cada InfPagamento da eFrete, nunca os dois grupos.");
                if (banco && (string.IsNullOrWhiteSpace(p.CodigoInstituicaoFinanceira) || string.IsNullOrWhiteSpace(p.NumeroAgencia) || string.IsNullOrWhiteSpace(p.NumeroConta) || string.IsNullOrWhiteSpace(p.TipoConta))) throw new ValidarXMLException("O grupo bancário da eFrete exige instituição, agência, conta e tipo de conta.");
                if (pix && (string.IsNullOrWhiteSpace(p.TipoChavePix) || string.IsNullOrWhiteSpace(p.ChavePix))) throw new ValidarXMLException("O grupo PIX da eFrete exige TipoChavePix e ChavePix.");
                var efrete = string.Equals(p.TipoPagamentoEFrete, "eFRETE", StringComparison.OrdinalIgnoreCase) || (string.IsNullOrWhiteSpace(p.TipoPagamentoEFrete) && p.TipoPagamento == TipoPagamentoFreteCIOT.InstituicaoPagamento);
                if (!efrete && !banco && !pix) throw new ValidarXMLException("Pagamentos por transferência ou depósito agendado exigem dados bancários ou PIX na eFrete.");
            }
        }

        private static void ValidarViagens(List<Xml.CIOT.OrigemDestino> viagens)
        {
            if (viagens == null) return;
            foreach (var v in viagens)
            {
                if (string.IsNullOrWhiteSpace(v.DocumentoViagem) || string.IsNullOrWhiteSpace(v.DistanciaPercorrida) || v.Valores == null || string.IsNullOrWhiteSpace(v.TipoPagamentoEFrete)) throw new ValidarXMLException("DocumentoViagem, DistanciaPercorrida, Valores e TipoPagamentoEFrete são obrigatórios em cada viagem eFrete.");
                if (v.NotasFiscais == null || v.NotasFiscais.Count == 0) throw new ValidarXMLException("Ao menos uma NotaFiscal é obrigatória em cada viagem eFrete.");
            }
        }

        private static void ValidarPessoa(Xml.CIOT.PessoaCIOT pessoa, string grupo, bool enderecoObrigatorio)
        {
            if (pessoa == null || string.IsNullOrWhiteSpace(pessoa.CpfOuCnpj)) throw new ValidarXMLException("O grupo " + grupo + " e seu CPF/CNPJ são obrigatórios para a eFrete.");
            if (enderecoObrigatorio || pessoa.Endereco != null)
            {
                var e = pessoa.Endereco;
                if (e == null || string.IsNullOrWhiteSpace(e.Bairro) || string.IsNullOrWhiteSpace(e.Rua) || string.IsNullOrWhiteSpace(e.Numero) || string.IsNullOrWhiteSpace(e.CEP) || string.IsNullOrWhiteSpace(e.CodigoMunicipio)) throw new ValidarXMLException("O endereço completo de " + grupo + " é obrigatório para a eFrete.");
            }
        }

        private static void ValidarSituacao(XMLBase xml, Servico servico)
        {
            var s = (Xml.CIOT.ConsultarSituacaoTransportador)xml;
            var placas = servico == Servico.CIOTConsultarFrotaTransportador ? ((Xml.CIOT.ConsultarFrotaTransportador)s).Placas : s.PlacasConsulta;
            if (string.IsNullOrWhiteSpace(s.CpfCnpjInteressado) || string.IsNullOrWhiteSpace(s.CpfCnpjTransportador) || string.IsNullOrWhiteSpace(s.RNTRCTransportador)) throw new ValidarXMLException("Interessado, transportador e RNTRC são obrigatórios na consulta de situação eFrete.");
            if (placas == null || placas.Count == 0) throw new ValidarXMLException("Ao menos uma placa é obrigatória na consulta de situação/frota eFrete.");
        }
    }
}
