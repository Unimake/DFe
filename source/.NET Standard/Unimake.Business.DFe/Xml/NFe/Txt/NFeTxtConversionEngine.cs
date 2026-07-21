using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using DFeNFe = Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    internal sealed class NFeTxtConversionEngine
    {
        #region --- public properties

        private Dictionary<string, string> LayoutTXT
        {
            get
            {
                if (_LayoutTXT == null)
                {
                    _LayoutTXT = NFeTxtLayoutCatalog.Criar();
                }
                return _LayoutTXT;
            }
        }

        private List<NFeTxtDocumento> documentos = null;
        private string cMensagemErro { get; set; }

        #endregion

        #region -- private proprieties

        private Dictionary<string, string> _LayoutTXT = null;
        private decimal versaoNFe = 4.0M;
        private DFeNFe.NFe nfeOficial = null;
        private DFeNFe.Ide identificacaoOficial = null;
        private List<DFeNFe.Det> detalhesOficiais = null;
        private DFeNFe.Total totalOficial = null;
        private DFeNFe.Transp transporteOficial = null;
        private DFeNFe.Dest destinatarioOficial = null;
        private DFeNFe.Retirada retiradaOficial = null;
        private DFeNFe.Entrega entregaOficial = null;
        private List<DFeNFe.AutXML> autorizadosXmlOficiais = null;
        private string FSegmento;
        private string Registro;
        private string layout;
        private string chave;
        private bool cDvInformado;
        private const string prefix = "§";
        private readonly NFeTxtConversionContext context;
        private readonly NFeTxtInitialSegmentDispatcher initialSegmentDispatcher;
        private readonly NFeTxtToNFeMapper nfeMapper;

        private readonly NFeTxtDetailSegmentDispatcher detailSegmentDispatcher;

        private readonly NFeTxtTotalSegmentDispatcher totalSegmentDispatcher;

        private readonly NFeTxtTransportSegmentDispatcher transportSegmentDispatcher;

        private readonly NFeTxtPaymentSegmentDispatcher paymentSegmentDispatcher;

        private readonly NFeTxtAdditionalInformationSegmentDispatcher additionalInformationSegmentDispatcher;

        #endregion

        internal NFeTxtConversionEngine()
        {
            this.context = new NFeTxtConversionContext();
            this.nfeMapper = new NFeTxtToNFeMapper();
            this.documentos = new List<NFeTxtDocumento>();
            this.cMensagemErro = "";
            this.initialSegmentDispatcher = new NFeTxtInitialSegmentDispatcher(new Dictionary<string, Action<int>>
            {
                { "A", _ => this.ProcessarCabecalhoInicial() },
                { "B", this.ProcessarIdentificacao },
                { "B13", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA02", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B14", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA03", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA10", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B20A", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B20D", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA13", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B20E", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA14", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA19", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B20I", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "B20J", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BA20", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BB01", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BB05", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "BC01", _ => this.ProcessarReferenciasDaIdentificacao() },
                { "C", _ => this.ProcessarEmitente() },
                { "C02", _ => this.ProcessarDocumentoEmitente() },
                { "C02A", _ => this.ProcessarCpfEmitente() },
                { "C05", _ => this.ProcessarEnderecoEmitente() },
                { "D", _ => this.ProcessarNotaAvulsa() },
                { "E", _ => this.ProcessarDestinatario() },
                { "E02", _ => this.ProcessarDocumentoDestinatario() },
                { "E03", _ => this.ProcessarCpfDestinatario() },
                { "E03A", _ => this.ProcessarIdEstrangeiroDestinatario() },
                { "E05", _ => this.ProcessarEnderecoDestinatario() },
                { "F", this.ProcessarLocalRetirada },
                { "F02", _ => this.ProcessarDocumentoRetirada() },
                { "F02A", _ => this.ProcessarCpfRetirada() },
                { "G", this.ProcessarLocalEntrega },
                { "G02", _ => this.ProcessarDocumentoEntrega() },
                { "G02A", _ => this.ProcessarCpfEntrega() },
                { "G51", _ => this.AdicionarAutorizacaoXmlCnpj() },
                { "GA02", _ => this.AdicionarAutorizacaoXmlCnpj() },
                { "G52", _ => this.AdicionarAutorizacaoXmlCpf() },
                { "GA03", _ => this.AdicionarAutorizacaoXmlCpf() }
            });
            this.detailSegmentDispatcher = new NFeTxtDetailSegmentDispatcher(new Dictionary<string, Action<int, int>>
            {
                { "I", this.ProcessarProduto },
                { "I05G", (nProd, _) => this.AdicionarCreditoPresumido(nProd) },
                { "I05A", (nProd, _) => this.ProcessarNveProduto(nProd) },
                { "I05K", (nProd, _) => this.ProcessarCreditoPresumidoIbsZfm(nProd) },
                { "I05C", this.ProcessarCestProduto },
                { "I05W", this.ProcessarCestProduto },
                { "I17", (nProd, _) => this.ProcessarBemMovelUsado(nProd) },
                { "I18", this.AdicionarDeclaracaoImportacao },
                { "I25", (nProd, _) => this.AdicionarAdicaoImportacao(nProd) },
                { "I50", (nProd, _) => this.AdicionarDetalheExportacao(nProd) },
                { "I52", (nProd, _) => this.ProcessarExportacaoIndireta(nProd) },
                { "I80", (nProd, _) => this.AdicionarRastroProduto(nProd) },
                { "IRT", (nProd, _) => this.ProcessarResponsavelTecnico() },
                { "J", (nProd, _) => this.ProcessarVeiculoNovo(nProd) },
                { "JA", (nProd, _) => this.ProcessarVeiculoNovo(nProd) },
                { "K", (nProd, _) => this.AdicionarMedicamento(nProd) },
                { "L", (nProd, _) => this.AdicionarArma(nProd) },
                { "LA", this.ProcessarCombustivel },
                { "L01", this.ProcessarCombustivel },
                { "LA1", (nProd, _) => this.ProcessarEncerranteCombustivel(nProd) },
                { "LA18", (nProd, _) => this.AdicionarOrigemCombustivel(nProd) },
                { "LA07", (nProd, _) => this.ProcessarCideCombustivel(nProd) },
                { "L105", (nProd, _) => this.ProcessarCideCombustivel(nProd) },
                { "LB", (nProd, _) => this.ProcessarRecopi(nProd) },
                { "L109", (nProd, _) => this.ProcessarRecopi(nProd) },
                { "M", (nProd, _) => this.ProcessarTotalTributosItem(nProd) },
                { "N02", (nProd, _) => this.ProcessarIcms00(nProd) },
                { "N02A", (nProd, _) => this.ProcessarIcms02(nProd) },
                { "N03", this.ProcessarIcms10 },
                { "N03A", (nProd, _) => this.ProcessarIcms15(nProd) },
                { "N04", this.ProcessarIcms20 },
                { "N05", this.ProcessarIcms30 },
                { "N06", this.ProcessarIcms40_41_50 },
                { "N07", this.ProcessarIcms51 },
                { "N07A", this.ProcessarIcms53 },
                { "N08", this.ProcessarIcms60 },
                { "N08A", this.ProcessarIcms61 },
                { "N09", this.ProcessarIcms70 },
                { "N10", this.ProcessarIcms90 },
                { "N10A", this.ProcessarIcmsPart10_90 },
                { "N10B", this.ProcessarIcmsSt },
                { "N10C", this.ProcessarIcmsSn101 },
                { "N10D", this.ProcessarIcmsSn102 },
                { "N10E", this.ProcessarIcmsSn201 },
                { "N10F", this.ProcessarIcmsSn202 },
                { "N10G", this.ProcessarIcmsSn500 },
                { "N10H", this.ProcessarIcmsSn900 },
                { "NA", this.ProcessarDiferimentoIcms },
                { "O", (nProd, _) => this.ProcessarIpi(nProd) },
                { "O07", this.ProcessarIpiTributado },
                { "O08", this.ProcessarIpiNaoTributado },
                { "O10", this.ProcessarIpiBaseAliquota },
                { "O11", this.ProcessarIpiQuantidade },
                { "P", this.ProcessarImpostoImportacao },
                { "Q02", this.ProcessarPisAliquota },
                { "Q03", this.ProcessarPisQuantidade },
                { "Q04", this.ProcessarPisNaoTributado },
                { "Q05", this.ProcessarPisOutros },
                { "Q07", this.ProcessarPisAliquotaRetencao },
                { "Q10", this.ProcessarPisQuantidadeRetencao },
                { "R", (nProd, _) => this.ProcessarPisSt(nProd) },
                { "R02", this.ProcessarPisStBase },
                { "R04", this.ProcessarPisStQuantidade },
                { "S02", this.ProcessarCofinsAliquota },
                { "S03", this.ProcessarCofinsQuantidade },
                { "S04", this.ProcessarCofinsNaoTributado },
                { "S05", this.ProcessarCofinsOutros },
                { "S07", this.ProcessarCofinsAliquotaRetencao },
                { "S09", this.ProcessarCofinsQuantidadeRetencao },
                { "T", this.ProcessarIssqn },
                { "T02", this.ProcessarIssqnValores },
                { "T04", this.ProcessarIssqnRetencao }
            });
            this.totalSegmentDispatcher = new NFeTxtTotalSegmentDispatcher(new Dictionary<string, Action<int, int>>
            {
                { "U", this.ProcessarTotalNfe },
                { "UA", this.ProcessarTotalIbsCbs },
                { "UB01", this.ProcessarTotalIbsCbsDetalhe },
                { "UB12", this.ProcessarTotalIbsCbsRegular },
                { "UB15", this.ProcessarTotalIbsCbsDiferimento },
                { "UB17", this.ProcessarTotalIbsCbsDevolucao },
                { "UB36", this.ProcessarTotalIbsCbsCreditoPresumido },
                { "UB55", this.ProcessarTotalIbsCbsReducao },
                { "UB66A", this.ProcessarGrupoAreasIncentivadasCbs },
                { "UB68", this.ProcessarTotalIbsCbsRegularCompraGov },
                { "UB82", this.ProcessarTotalIbsCbsDiferimentoCompraGov },
                { "UB84", this.ProcessarTotalIbsCbsDevolucaoCompraGov },
                { "UB85", this.ProcessarTotalIbsCbsCreditoPresumidoCompraGov },
                { "UB91", this.ProcessarTotalIbsCbsTribRegular },
                { "UB95", this.ProcessarTotalIbsCbsTribRegularCompraGov },
                { "UB100", this.ProcessarMonoDiferido },
                { "UB106", this.ProcessarTransferenciaCredito },
                { "UB112", this.ProcessarAjusteCompetencia },
                { "UB14A", this.ProcessarIndicadorDoacao },
                { "UB116", this.ProcessarEstornoCredito },
                { "UB120", this.ProcessarCreditoPresumidoOperacao },
                { "UB123", this.ProcessarCreditoPresumidoIbs },
                { "UB127", this.ProcessarCreditoPresumidoCbs },
                { "UB131", this.ProcessarCreditoPresumidoZfm },
                { "W02", this.ProcessarTotaisIcms },
                { "W04", this.ProcessarTotaisIcmsSt },
                { "W17", this.ProcessarTotaisFcp },
                { "W23", this.ProcessarTotaisIpi },
                { "W31", this.ProcessarTotaisPis },
                { "W34", this.ProcessarTotaisCofins },
                { "W36", this.ProcessarTotaisIssqn },
                { "W37", this.ProcessarTotaisRetencoes },
                { "W42", this.ProcessarTotaisTributos },
                { "W50", this.ProcessarTotaisIcmsUfDest },
                { "W57", this.ProcessarTotaisFcpUfDest },
                { "W59E", this.ProcessarTotaisFcpUfRemet },
                { "W60", this.ProcessarTotaisIbsCbs },
                { "VA02", this.ProcessarObservacaoContribuinte },
                { "VA05", this.ProcessarObservacaoFisco },
                { "VB01", this.ProcessarValorItem },
                { "VC01", this.ProcessarDfeReferenciado }
            });
            this.transportSegmentDispatcher = new NFeTxtTransportSegmentDispatcher(new Dictionary<string, Action<int, int>>
            {
                { "X", this.ProcessarTransporte },
                { "X03", this.ProcessarTransportador },
                { "X04", this.ProcessarVeiculoTransporte },
                { "X05", this.ProcessarReboque },
                { "X11", this.ProcessarVolume },
                { "X18", this.ProcessarLacre }
            });
            this.paymentSegmentDispatcher = new NFeTxtPaymentSegmentDispatcher(new Dictionary<string, Action<int, int>>
            {
                { "X22", this.ProcessarFatura },
                { "X26", this.ProcessarDuplicata },
                { "X33", this.ProcessarPagamento },
                { "Y02", this.ProcessarFormaPagamento },
                { "Y07", this.ProcessarTroco },
                { "YA", this.ProcessarCartao },
                { "YA09", this.ProcessarCnpjInstituicaoPagadora },
                { "YA04", (nProd, _) => this.ProcessarTipoIntegracaoPagamento() },
                { "YA04A", (nProd, _) => this.ProcessarTipoIntegracaoPagamento() },
                { "YB", this.ProcessarIntermediador }
            });
            this.additionalInformationSegmentDispatcher = new NFeTxtAdditionalInformationSegmentDispatcher(new Dictionary<string, Action<int, int>>
            {
                { "Z", this.ProcessarInformacoesAdicionais },
                { "Z04", this.AdicionarObservacaoContribuinte },
                { "Z07", this.AdicionarObservacaoFisco },
                { "Z10", this.AdicionarProcessoReferenciado },
                { "ZA", (nProd, _) => this.ProcessarExportacao() },
                { "ZA01", (nProd, _) => this.ProcessarExportacao() },
                { "ZB", (nProd, _) => this.ProcessarCompra() },
                { "ZC", (nProd, _) => this.ProcessarCana() },
                { "ZC01", (nProd, _) => this.ProcessarCana() },
                { "ZC04", (nProd, _) => this.AdicionarFornecimentoDiario() },
                { "ZC10", (nProd, _) => this.AdicionarDeducaoCana() },
                { "ZD", (nProd, _) => this.ProcessarResponsavelTecnicoZ() },
                { "ZF02", (nProd, _) => this.AdicionarDefensivoAgropecuario() },
                { "ZF04", (nProd, _) => this.ProcessarGuiaTransitoAgropecuario() }
            });
        }

        /// <summary>
        /// CarregarArquivo
        /// </summary>
        private bool CarregarArquivo(string cArquivo)
        {
            bool possuiCabecalho;
            string mensagemErro;
            this.context.ConteudoPorNota = new NFeTxtReader().Carregar(cArquivo, prefix, out mensagemErro, out possuiCabecalho);
            this.cMensagemErro = mensagemErro;
            if (possuiCabecalho) this.context.LinhaLida = 1;
            return this.context.ConteudoPorNota.Count > 0 && string.IsNullOrEmpty(this.cMensagemErro);
        }

        private void ReiniciarConversao()
        {
            this.context.Reiniciar();
            this.documentos.Clear();
            this.cMensagemErro = string.Empty;
            this.layout = null;
            this.chave = null;
            this.cDvInformado = false;
        }

        /// <summary>
        /// Converter
        /// </summary>
        internal NFeTxtConversaoResultado Converter(string cArquivo)
        {
            this.ReiniciarConversao();

            if (!this.CarregarArquivo(cArquivo)) return CriarResultado();

            this.context.LinhaLida = 0;

            foreach (List<string> conteudoNota in this.context.ConteudoPorNota.Values)
            {
                this.versaoNFe = 4.0M;
                this.identificacaoOficial = new DFeNFe.Ide { NFref = new List<DFeNFe.NFref>() };
                this.detalhesOficiais = new List<DFeNFe.Det>();
                this.destinatarioOficial = new DFeNFe.Dest { EnderDest = new DFeNFe.EnderDest() };
                this.retiradaOficial = new DFeNFe.Retirada();
                this.entregaOficial = new DFeNFe.Entrega();
                this.autorizadosXmlOficiais = new List<DFeNFe.AutXML>();
                this.totalOficial = this.nfeMapper.CriarTotal();
                this.transporteOficial = new DFeNFe.Transp();
                this.nfeOficial = null;
                this.cDvInformado = false;
                var houveErro = new NFeTxtParser().Processar(this.context, conteudoNota, this.LerRegistro, this.LayoutTXT,
                    () => versaoNFe,
                    () => this.layout, mensagem => this.cMensagemErro += mensagem, prefix);

                if (!houveErro && this.cMensagemErro == "")
                {
                    this.GerarXmlDaNota();
                }

                if (this.cMensagemErro != "")
                {
                    this.LimparDocumentosGerados();
                }
            }

            if (!string.IsNullOrEmpty(this.cMensagemErro))
            {
                this.cMensagemErro += "----------------------" + Environment.NewLine;
                this.cMensagemErro += "Para gerar o layout em TXT da NFe/NFCe, grave um arquivo com o nome 'uninfe-layout.txt' ou 'uninfe-layout.xml' com conteudo vazio na pasta 'geral' do Uninfe.";
                this.cMensagemErro += "----------------------" + Environment.NewLine;
            }

            return CriarResultado();
        }

        private NFeTxtConversaoResultado CriarResultado()
        {
            var resultado = new NFeTxtConversaoResultado { MensagemErro = this.cMensagemErro };
            resultado.Documentos.AddRange(this.documentos);
            return resultado;
        }

        private void GerarXmlDaNota()
        {
            try
            {
                this.documentos.Add(GerarDocumento(this.nfeOficial, this.cDvInformado));
            }
            catch (Exception ex)
            {
                this.cMensagemErro += ex.Message;
            }
        }

        private NFeTxtDocumento GerarDocumento(DFeNFe.NFe nfeOficial, bool cDvInformado)
        {
            string chave;
            if (nfeOficial == null)
            {
                throw new InvalidOperationException("Falta definir a identificação da NFe/NFCe.");
            }

            var nfe = nfeOficial;
            PrepararReferenciasDaIdentificacao();
            this.nfeMapper.Mapear(nfe, identificacaoOficial, destinatarioOficial, retiradaOficial, entregaOficial,
                autorizadosXmlOficiais, detalhesOficiais, totalOficial, transporteOficial);
            NFeTxtCompatibilityValidator.Validar(nfe, detalhesOficiais);
            chave = new NFeTxtKeyValidator().MontarEValidar(nfe.InfNFeField, cDvInformado, identificacaoOficial.CDV);
            var documento = XMLUtility.Serializar(nfe);
            NFeTxtXmlCompatibilityAdjuster.Ajustar(documento, transporteOficial.Vol);

            identificacaoOficial.CNF = chave.Substring(35, 8);
            identificacaoOficial.CDV = int.Parse(chave.Substring(43, 1));
            return new NFeTxtDocumento(documento.OuterXml, chave, identificacaoOficial.NNF, identificacaoOficial.Serie);
        }

        private void LimparDocumentosGerados()
        {
            this.documentos.Clear();
        }

#if INTEROP
        private static DateTime ConverterDataHora(string valor) =>
            DateTimeOffset.Parse(valor, CultureInfo.InvariantCulture).DateTime;

        private static T ObterEnumOpcional<T>(int codigo, T valorNaoDefinido) where T : struct =>
            Enum.IsDefined(typeof(T), codigo) ? (T)Enum.ToObject(typeof(T), codigo) : valorNaoDefinido;
#else
        private static DateTimeOffset ConverterDataHora(string valor) =>
            DateTimeOffset.Parse(valor, CultureInfo.InvariantCulture);

        private static T? ObterEnumOpcional<T>(int codigo, T valorNaoDefinido) where T : struct =>
            Enum.IsDefined(typeof(T), codigo) ? (T?)Enum.ToObject(typeof(T), codigo) : null;
#endif

        /// <summary>
        /// getDateTime
        /// </summary>
        private DateTime getDateTime(TpcnTipoCampo Tipo, string value)
        {
            if (string.IsNullOrEmpty(value))
                return DateTime.MinValue;

            try
            {
                int _ano = Convert.ToInt16(value.Substring(0, 4));
                int _mes = Convert.ToInt16(value.Substring(5, 2));
                int _dia = Convert.ToInt16(value.Substring(8, 2));
                if (Tipo == TpcnTipoCampo.tcDatHor && value.Contains(":"))
                {
                    int _hora = Convert.ToInt16(value.Substring(11, 2));
                    int _min = Convert.ToInt16(value.Substring(14, 2));
                    int _seg = Convert.ToInt16(value.Substring(17, 2));
                    return new DateTime(_ano, _mes, _dia, _hora, _min, _seg);
                }
                return new DateTime(_ano, _mes, _dia);
            }
            catch
            {
                throw new Exception("Data inválida do conteudo [" + value + "]");
            }
        }

        /// <summary>
        /// getDateTime2
        /// </summary>
        private DateTime getDate2(TpcnTipoCampo Tipo, string value)
        {
            if (string.IsNullOrEmpty(value))
                return DateTime.MinValue;

            if (value.Contains("-"))
                return this.getDateTime(Tipo, value);

            try
            {
                int _ano = Convert.ToInt16(value.Substring(0, 4));
                int _mes = Convert.ToInt16(value.Substring(4, 2));
                int _dia = Convert.ToInt16(value.Substring(6, 2));
                return new DateTime(_ano, _mes, _dia);
            }
            catch
            {
                throw new Exception("Data inválida do conteudo [" + value + "]");
            }
        }

        /// <summary>
        /// getTime
        /// </summary>
        private DateTime getTime(string value)
        {
            if (string.IsNullOrEmpty(value))
                return DateTime.MinValue;

            try
            {
                int _hora = Convert.ToInt16(value.Substring(0, 2));
                int _min = Convert.ToInt16(value.Substring(3, 2));
                int _seg = Convert.ToInt16(value.Substring(6, 2));
                return new DateTime(1, 1, 1, _hora, _min, _seg);
            }
            catch
            {
                throw new Exception("Hora inválida do conteudo [" + value + "]");
            }
        }

        /// <summary>
        /// RetornarConteudoTag
        /// </summary>
        private string RetornarConteudoTag(string TAG, bool trim, ObOp optional)
        {
            // Se a tag consultada é CNPJ, o layout define quantos pipes existem até ela.
            // Para "§B14|cUF|AAMM|CNPJ|Mod|serie|nNF|", por exemplo, são usados três pipes.
            if (string.IsNullOrEmpty(layout)) throw new Exception("Layout para o segmento '" + this.FSegmento + "' não encontrado");
            if (!layout.StartsWith(prefix)) layout = prefix + layout;
            if (!layout.EndsWith("|")) layout += "|";
            string fValue = layout.Substring(0, layout.ToUpper().IndexOf("|" + TAG.ToUpper().Trim() + "|") + 1);
            if (fValue == "")
                if (optional == ObOp.Obrigatorio)
                    throw new Exception("Segmento: " + this.FSegmento + " - Tag: " + TAG + " não encontrada");
                else
                    return "";

            string[] pipes = fValue.Split(new char[] { '|' });
            int j = pipes.GetLength(0) - 1;
            if (j >= 0)
            {
                // Localiza a posição do conteúdo no registro lido.
                string[] dados = this.Registro.Split(new char[] { '|' });
                try
                {
                    if (trim)
                        return dados[j].TrimStart().TrimEnd();
                    else
                        return dados[j];
                }
                catch
                {
                    return "";
                }
            }
            else
                return "";
        }

        /// <summary>
        /// SomenteNumeros
        /// </summary>
        private string SomenteNumeros(string entrada)
        {
            if (string.IsNullOrEmpty(entrada)) return "";

            StringBuilder saida = new StringBuilder(entrada.Length);
            foreach (char c in entrada)
            {
                if (char.IsDigit(c))
                {
                    saida.Append(c);
                }
            }
            return saida.ToString();
        }

        /// <summary>
        /// LerCampo
        /// </summary>

        private double LerDouble(TpcnTipoCampo Tipo, TpcnResources tag, ObOp optional, int maxLength, bool returnNull) =>
            (double)LerCampo(Tipo, tag, optional, 0, maxLength, true, returnNull);

        private double LerDouble(TpcnTipoCampo Tipo, TpcnResources tag, ObOp optional, int maxLength) =>
            (double)this.LerCampo(Tipo, tag, optional, 0, maxLength, true, false);
        private decimal LerDecimal(TpcnTipoCampo Tipo, TpcnResources tag, ObOp optional, int maxLength) =>
            (decimal)this.LerCampo(Tipo, tag.ToString(), optional, 0, maxLength, true, false);

        private double LerDouble(TpcnTipoCampo Tipo, TpcnResources tag, ObOp optional, int minLength, int maxLength) =>
            (double)this.LerCampo(Tipo, tag, optional, minLength, maxLength, true, false);

        private Int32 LerInt32(string tag, ObOp optional, int minLength, int maxLength) =>
            (Int32)this.LerCampo(TpcnTipoCampo.tcInt, tag, optional, minLength, maxLength, true, false);

        private Int32 LerInt32(TpcnResources tag, ObOp optional, int minLength, int maxLength) =>
            (Int32)this.LerCampo(TpcnTipoCampo.tcInt, tag, optional, minLength, maxLength, true, false);

        private Int32 LerInt32(TpcnResources tag, ObOp optional, int minLength, int maxLength, bool returnNull) =>
            (Int32)this.LerCampo(TpcnTipoCampo.tcInt, tag, optional, minLength, maxLength, true, returnNull);

        private string LerString(string tag, ObOp optional, int minLength, int maxLength) =>
            (string)this.LerCampo(TpcnTipoCampo.tcStr, tag, optional, minLength, maxLength, true, false);

        private string LerString(TpcnResources tag, ObOp optional, int minLength, int maxLength) =>
            (string)this.LerCampo(TpcnTipoCampo.tcStr, tag, optional, minLength, maxLength, true, false);

        private string LerString(TpcnResources tag, ObOp optional, int minLength, int maxLength, bool trim) =>
            (string)this.LerCampo(TpcnTipoCampo.tcStr, tag, optional, minLength, maxLength, trim, false);

        private object LerCampo(TpcnTipoCampo Tipo, TpcnResources tag, ObOp optional, int minLength, int maxLength, bool trim, bool returnNull)
            => LerCampo(Tipo, tag.ToString(), optional, minLength, maxLength, trim, returnNull);

        private object LerCampo(TpcnTipoCampo Tipo, string /*TpcnResources*/ tag, ObOp optional, int minLength, int maxLength, bool trim, bool returnNull)
        {
            int nDecimais = 0;
            string ConteudoTag = "";
            try
            {
                ConteudoTag = RetornarConteudoTag(tag.ToString(), trim, optional);

                if (ConteudoTag != "")
                    if (ConteudoTag.StartsWith(prefix))
                        ConteudoTag = "";

                if (string.IsNullOrEmpty(ConteudoTag) && (tag.ToString() == "cEAN" || tag.ToString() == "cEANTrib"))
                    return ConteudoTag = "SEM GTIN";

                int len = ConteudoTag.Length;
                if (len == 0 && (optional == ObOp.Opcional || optional == ObOp.None))
                {
                }
                else
                {
                    switch (Tipo)
                    {
                        case TpcnTipoCampo.tcHor:
                            maxLength = minLength = 8; //hh:mm:ss
                            break;
                        case TpcnTipoCampo.tcDatYYYY_MM_DD:
                            maxLength = minLength = 10; //yyyy-MM-dd
                            break;
                        case TpcnTipoCampo.tcDatYYYYMMDD:
                            maxLength = minLength = 8; //yyyyMMdd
                            break;
                        case TpcnTipoCampo.tcDatHor:
                            maxLength = minLength = 19; //aaaa-mm-dd hh:mm:ss
                            break;
                        default:
                            if (Tipo >= TpcnTipoCampo.tcDouble2 && Tipo <= TpcnTipoCampo.tcDouble10)
                            {
                                nDecimais = (int)Tipo;
                            }
                            else if (Tipo == TpcnTipoCampo.tcDec4)
                            {
                                nDecimais = 4;
                            }
                            else if (Tipo == TpcnTipoCampo.tcDec10)
                            {
                                nDecimais = 10;
                            }

                            break;
                    }

                    if (len == 0 && minLength > 0)
                    {
                        this.cMensagemErro += "Layout: " + this.layout.Replace(prefix, "") + Environment.NewLine;
                        this.cMensagemErro += string.Format("Segmento [{0}]: tag <{1}> deve ser informada.\r\n" +
                                                            "\tLinha: {2}: Conteudo do segmento: {3}",
                                                            this.FSegmento, tag.ToString(), this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
                    }
                    else
                    {
                        switch (Tipo)
                        {
                            case TpcnTipoCampo.tcDouble2:
                            case TpcnTipoCampo.tcDouble3:
                            case TpcnTipoCampo.tcDouble4:
                            case TpcnTipoCampo.tcDouble5:
                            case TpcnTipoCampo.tcDouble6:
                            case TpcnTipoCampo.tcDouble7:
                            case TpcnTipoCampo.tcDouble8:
                            case TpcnTipoCampo.tcDouble9:
                            case TpcnTipoCampo.tcDouble10:
                            case TpcnTipoCampo.tcDec4:
                            case TpcnTipoCampo.tcDec10:
                                //quando numerico do tipo double não consiste o tamanho minimo nem maximo
                                break;
                            default:
                                if ((len > maxLength || len < minLength) && (maxLength + minLength > 0))
                                {
                                    this.cMensagemErro += "Layout: " + this.layout.Replace(prefix, "") + Environment.NewLine;
                                    this.cMensagemErro += string.Format("Segmento [{0}]: tag <{1}> deve ter seu tamanho entre {2} e {3}. Conteudo: {4}" +
                                                            "\r\n\tLinha: {5}: Conteudo do segmento: {6}",
                                                            this.FSegmento, tag.ToString(), minLength, maxLength, ConteudoTag, this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
                                }
                                break;
                        }
                    }
                }

                if (optional == ObOp.Obrigatorio || ((optional == ObOp.Opcional || optional == ObOp.None) && len != 0))
                {
                    switch (Tipo)
                    {
                        case TpcnTipoCampo.tcDouble2:
                        case TpcnTipoCampo.tcDouble3:
                        case TpcnTipoCampo.tcDouble4:
                        case TpcnTipoCampo.tcDouble5:
                        case TpcnTipoCampo.tcDouble6:
                        case TpcnTipoCampo.tcDouble7:
                        case TpcnTipoCampo.tcDouble8:
                        case TpcnTipoCampo.tcDouble9:
                        case TpcnTipoCampo.tcDouble10:
                        case TpcnTipoCampo.tcDec4:
                        case TpcnTipoCampo.tcDec10:
                            {
                                int pos = ConteudoTag.IndexOf(".") + 1;
                                int ndec = (pos > 1 ? ConteudoTag.Substring(pos).Length : 0);
                                if (pos >= 1)
                                {
                                    string xdec = ConteudoTag.Substring(pos);
                                    //
                                    // ajusta o numero de casas decimais
                                    while (ndec > nDecimais)
                                    {
                                        if (xdec.Substring(ndec - 1, 1) == "0")
                                            --ndec;
                                        else
                                            break;
                                    }

                                    if (ndec > nDecimais)
                                    {
                                        this.cMensagemErro += "Layout: " + this.layout.Replace(prefix, "") + Environment.NewLine;
                                        this.cMensagemErro += string.Format("Segmento [{0}]: tag <{1}> número de casas decimais deve ser de {2} e existe(m) {3}" +
                                                                            "\r\n\tLinha: {4}: Conteudo do segmento: {5}",
                                                                            this.FSegmento, tag.ToString(), nDecimais, ndec, this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
                                    }
                                }
                                else
                                    ndec = nDecimais;

                                #region -- atribui o numero de casas decimais que serão gravadas

                                if (ndec < (int)TpcnTipoCampo.tcDouble2 || ndec > (int)TpcnTipoCampo.tcDouble10)
                                    ndec = (int)TpcnTipoCampo.tcDouble2;

                                TpcnTipoCampo tipo = (TpcnTipoCampo)ndec;

                                if (tag == TpcnResources.qTotMes.ToString())
                                {
                                }

                                if (tag == TpcnResources.qTotAnt.ToString())
                                {
                                }

                                if (tag == TpcnResources.qTotGer.ToString())
                                {
                                }

                                if (tag == TpcnResources.qtde.ToString())
                                {
                                }

                                #endregion
                            }
                            break;
                    }
                }

                switch (Tipo)
                {
                    case TpcnTipoCampo.tcDatYYYYMMDD:
                        return this.getDate2(Tipo, ConteudoTag);

                    case TpcnTipoCampo.tcDatYYYY_MM_DD:
                    case TpcnTipoCampo.tcDatHor:
                        return this.getDateTime(Tipo, ConteudoTag);

                    case TpcnTipoCampo.tcHor:
                        return this.getTime(ConteudoTag);

                    case TpcnTipoCampo.tcDouble2:
                    case TpcnTipoCampo.tcDouble3:
                    case TpcnTipoCampo.tcDouble4:
                    case TpcnTipoCampo.tcDouble5:
                    case TpcnTipoCampo.tcDouble6:
                    case TpcnTipoCampo.tcDouble7:
                    case TpcnTipoCampo.tcDouble8:
                    case TpcnTipoCampo.tcDouble9:
                    case TpcnTipoCampo.tcDouble10:
                    case TpcnTipoCampo.tcDec4:
                    case TpcnTipoCampo.tcDec10:
                        if (string.IsNullOrEmpty(ConteudoTag) && returnNull)
                        {
                            return -9.99;
                        }
                        else
                        {
                            if (Tipo == TpcnTipoCampo.tcDec4 || Tipo == TpcnTipoCampo.tcDec10)
                            {
                                return Convert.ToDecimal("0" + ConteudoTag.Replace(".", System.Globalization.NumberFormatInfo.CurrentInfo.NumberDecimalSeparator));
                            }
                            else
                            {
                                return Convert.ToDouble("0" + ConteudoTag.Replace(".", System.Globalization.NumberFormatInfo.CurrentInfo.NumberDecimalSeparator));
                            }
                        }

                    case TpcnTipoCampo.tcInt:
                        if (string.IsNullOrEmpty(ConteudoTag) && returnNull)
                            return 100;
                        else
                            return Convert.ToInt32("0" + SomenteNumeros(ConteudoTag));

                    default:
                        return (trim ? ConteudoTag.Trim() : ConteudoTag);
                }
            }
            catch (Exception ex)
            {
                if (!string.IsNullOrEmpty(layout))
                    this.cMensagemErro += "Layout: " + this.layout.Replace(prefix, "") + Environment.NewLine;
                this.cMensagemErro += string.Format("Segmento [{0}]: tag <{1}> Conteudo: {2}\r\n" +
                                                    "\tLinha: {3}: Conteudo do segmento: {4}\r\n\tMensagem de erro: {5}",
                                                    this.FSegmento, tag.ToString(), ConteudoTag, this.context.LinhaLida + 1, this.Registro.Substring(1),
                                                    ex.Message) + Environment.NewLine;
                return RetornarValorPadraoDoCampo(Tipo);
            }
        }


        private static object RetornarValorPadraoDoCampo(TpcnTipoCampo tipo)
        {
            switch (tipo)
            {
                    case TpcnTipoCampo.tcHor:
                    case TpcnTipoCampo.tcDatYYYY_MM_DD:
                    case TpcnTipoCampo.tcDatYYYYMMDD:
                    case TpcnTipoCampo.tcDatHor:
                        return DateTime.MinValue;

                    case TpcnTipoCampo.tcDouble2:
                    case TpcnTipoCampo.tcDouble3:
                    case TpcnTipoCampo.tcDouble4:
                    case TpcnTipoCampo.tcDouble5:
                    case TpcnTipoCampo.tcDouble6:
                    case TpcnTipoCampo.tcDouble7:
                    case TpcnTipoCampo.tcDouble8:
                    case TpcnTipoCampo.tcDouble9:
                    case TpcnTipoCampo.tcDouble10:
                    case TpcnTipoCampo.tcDec4:
                    case TpcnTipoCampo.tcDec10:
                        return 0.0;

                    case TpcnTipoCampo.tcInt:
                        return 0;

                    default:
                        return "";

            }
        }

        private int CasasDecimais75
        {
            get
            {
                return 7;
            }
        }
        private TpcnTipoCampo TipoCampo42
        {
            get
            {
                return TpcnTipoCampo.tcDouble4;
            }
        }


        private void AdicionarDefensivoAgropecuario()
        {
            if (this.nfeOficial.InfNFeField.Agropecuario == null) this.nfeOficial.InfNFeField.Agropecuario = new DFeNFe.Agropecuario();
            if (this.nfeOficial.InfNFeField.Agropecuario.Defensivo == null) this.nfeOficial.InfNFeField.Agropecuario.Defensivo = new List<DFeNFe.Defensivo>();
            this.nfeOficial.InfNFeField.Agropecuario.Defensivo.Add(new DFeNFe.Defensivo
            {
                NReceituario = this.LerString(TpcnResources.nReceituario, ObOp.Obrigatorio, 1, 30),
                CPFRespTec = this.LerString(TpcnResources.CPFRespTec, ObOp.Obrigatorio, 11, 11)
            });
        }

        private void ProcessarGuiaTransitoAgropecuario()
        {
            if (this.nfeOficial.InfNFeField.Agropecuario == null) this.nfeOficial.InfNFeField.Agropecuario = new DFeNFe.Agropecuario();
            this.nfeOficial.InfNFeField.Agropecuario.GuiaTransito = new DFeNFe.GuiaTransito
            {
                TpGuia = (TipoGuiaTransito)this.LerInt32(TpcnResources.tpGuia, ObOp.Obrigatorio, 1, 1),
                UFGuia = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(TpcnResources.UFGuia, ObOp.Obrigatorio, 2, 2), true),
                SerieGuia = VazioParaNulo(this.LerString(TpcnResources.serieGuia, ObOp.Opcional, 1, 9)),
                NGuia = this.LerString(TpcnResources.nGuia, ObOp.Obrigatorio, 1, 9)
            };
        }
        private void ProcessarCana()
        {
            this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana
            {
                Safra = this.LerString(TpcnResources.safra, ObOp.Obrigatorio, 4, 9),
                Ref = this.LerString(TpcnResources.Ref, ObOp.Obrigatorio, 7, 7),
                QTotMes = this.LerDouble(TpcnTipoCampo.tcDouble10, TpcnResources.qTotMes, ObOp.Obrigatorio, 11),
                QTotAnt = this.LerDouble(TpcnTipoCampo.tcDouble10, TpcnResources.qTotAnt, ObOp.Obrigatorio, 11),
                QTotGer = this.LerDouble(TpcnTipoCampo.tcDouble10, TpcnResources.qTotGer, ObOp.Obrigatorio, 11),
                VFor = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFor, ObOp.Obrigatorio, 15),
                VTotDed = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTotDed, ObOp.Obrigatorio, 15),
                VLiqFor = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vLiqFor, ObOp.Obrigatorio, 15)
            };
        }
        private void ProcessarReferenciasDaIdentificacao()
        {
            switch (this.FSegmento.ToUpper())
            {
                case "B13":
                case "BA02":
                    // Grupo da TAG <ide><NFref><refNFe>
                    #region <ide><NFref><refNFe>

                    identificacaoOficial.NFref.Add(new DFeNFe.NFref
                    {
                        RefNFe = this.LerString(TpcnResources.refNFe, ObOp.Obrigatorio, 44, 44)
                    });

                    #endregion
                    break;

                case "B14":
                case "BA03":
                    // Grupo da TAG <ide><NFref><RefNF>
                    #region <ide><NFref><RefNF>
                    {
                        identificacaoOficial.NFref.Add(new DFeNFe.NFref
                        {
                            RefNF = new DFeNFe.RefNF
                            {
                                CUF = (UFBrasil)this.LerInt32(TpcnResources.cUF, ObOp.Obrigatorio, 2, 2),
                                AAMM = this.LerString(TpcnResources.AAMM, ObOp.Obrigatorio, 4, 4),
                                CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14),
                                Mod = this.LerString(TpcnResources.mod, ObOp.Obrigatorio, 2, 2),
                                Serie = this.LerInt32(TpcnResources.serie, ObOp.Obrigatorio, 1, 3),
                                NNF = this.LerInt32(TpcnResources.nNF, ObOp.Obrigatorio, 1, 9)
                            }
                        });
                    }
                    #endregion
                    break;

                case "BA10":
                case "B20A":
                    #region B20a | BA10
                    {
                        var item = new DFeNFe.NFref
                        {
                            RefNFP = new DFeNFe.RefNFP
                            {
                                CUF = (UFBrasil)this.LerInt32(TpcnResources.cUF, ObOp.Obrigatorio, 2, 2),
                                AAMM = this.LerString(TpcnResources.AAMM, ObOp.Obrigatorio, 4, 4),
                                IE = VazioParaNulo(this.LerString(TpcnResources.IE, ObOp.Obrigatorio, 1, 14)),
                                Mod = this.LerString(TpcnResources.mod, ObOp.Obrigatorio, 2, 2),
                                Serie = this.LerInt32(TpcnResources.serie, ObOp.Obrigatorio, 1, 3),
                                NNF = this.LerInt32(TpcnResources.nNF, ObOp.Obrigatorio, 1, 9)
                            }
                        };
                        identificacaoOficial.NFref.Add(item);
                        if (FSegmento.ToUpper().Equals("BA10"))
                        {
                            var refCTe = this.LerString(TpcnResources.refCTe, ObOp.Opcional, 44, 44);
                            if (!string.IsNullOrWhiteSpace(refCTe)) identificacaoOficial.NFref.Add(new DFeNFe.NFref { RefCTe = refCTe });
                        }
                    }
                    #endregion
                    break;

                case "B20D":
                case "BA13":
                    ObterUltimaReferenciaProdutor(FSegmento.ToUpper().Equals("B20D") ? "Segmento B20d sem segmento B20A" : "Segmento BA13 sem segmento BA10").CNPJ =
                        this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
                    break;

                case "B20E":
                case "BA14":
                    ObterUltimaReferenciaProdutor(FSegmento.ToUpper().Equals("B20E") ? "Segmento B20e sem segmento B20A" : "Segmento BA14 sem segmento BA10").CPF =
                        this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11);
                    break;

                case "BA19":
                case "B20I":
                    //layout = "§BA19|refCTe"; //ok
                    identificacaoOficial.NFref.Add(new DFeNFe.NFref
                    {
                        RefCTe = LerString(TpcnResources.refCTe, ObOp.Obrigatorio, 44, 44)
                    });
                    break;

                case "B20J":
                case "BA20":
                    //layout = prefix + this.FSegmento + "|mod|nECF|nCOO"; //ok
                    {
                        identificacaoOficial.NFref.Add(new DFeNFe.NFref
                        {
                            RefECF = new DFeNFe.RefECF
                            {
                                Mod = this.LerString(TpcnResources.mod, ObOp.Obrigatorio, 2, 2),
                                NECF = this.LerInt32(TpcnResources.nECF, ObOp.Obrigatorio, 1, 3),
                                NCOO = this.LerInt32(TpcnResources.nCOO, ObOp.Obrigatorio, 1, 6)
                            }
                        });
                    }
                    break;

                case "BB01":
                    {
                        //layout = BB01|tpEnteGov|pRedutor|tpOperGov|refDFeAnt|
                        var refDFeAnt = VazioParaNulo(this.LerString(TpcnResources.refDFeAnt, ObOp.Opcional, 44, 44));
                        identificacaoOficial.GCompraGov = new DFeNFe.GCompraGov
                        {
                            TpEnteGov = (TipoEnteGovernamental)this.LerInt32(TpcnResources.tpEnteGov, ObOp.Obrigatorio, 1, 1),
                            PRedutor = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedutor, ObOp.Obrigatorio, 7),
                            TpOperGov = (TipoOperacaoEnteGovernamental)this.LerInt32(TpcnResources.tpOperGov, ObOp.Obrigatorio, 1, 1),
                            RefDFeAnt = refDFeAnt == null ? null : new List<string> { refDFeAnt }
                        };
                    }
                    break;

                case "BB05":
                    //layout = BB05|refDFeAnt|
                    if (identificacaoOficial.GCompraGov == null)
                    {
                        throw new Exception("Segmento BB05 informado sem o segmento BB01.");
                    }

                    if (identificacaoOficial.GCompraGov.RefDFeAnt == null) identificacaoOficial.GCompraGov.RefDFeAnt = new List<string>();
                    identificacaoOficial.GCompraGov.RefDFeAnt.Add(this.LerString(TpcnResources.refDFeAnt, ObOp.Obrigatorio, 44, 44));
                    break;

                case "BC01":
                    //layout = BC01|refDFe|

                    if (identificacaoOficial.GPagAntecipado == null) identificacaoOficial.GPagAntecipado = new DFeNFe.GPagAntecipado { RefDFe = new List<string>() };
                    identificacaoOficial.GPagAntecipado.RefDFe.Add(this.LerString(TpcnResources.refDFe, ObOp.Obrigatorio, 44, 44));
                    break;
            }
        }

        private DFeNFe.RefNFP ObterUltimaReferenciaProdutor(string mensagem)
        {
            for (var i = identificacaoOficial.NFref.Count - 1; i >= 0; i--)
            {
                if (identificacaoOficial.NFref[i].RefNFP != null)
                {
                    return identificacaoOficial.NFref[i].RefNFP;
                }
            }

            throw new Exception(mensagem);
        }

        private void PrepararReferenciasDaIdentificacao()
        {
            if (identificacaoOficial.NFref == null || identificacaoOficial.NFref.Count == 0)
            {
                identificacaoOficial.NFref = null;
                return;
            }

            var referencias = new List<DFeNFe.NFref>();
            referencias.AddRange(identificacaoOficial.NFref.FindAll(x => !string.IsNullOrWhiteSpace(x.RefNFe)));
            referencias.AddRange(identificacaoOficial.NFref.FindAll(x => x.RefNF != null && x.RefNF.NNF > 0));
            referencias.AddRange(identificacaoOficial.NFref.FindAll(x => x.RefNFP != null && x.RefNFP.NNF > 0));
            referencias.AddRange(identificacaoOficial.NFref.FindAll(x => !string.IsNullOrWhiteSpace(x.RefCTe) && !x.RefCTe.StartsWith("00", StringComparison.Ordinal)));
            referencias.AddRange(identificacaoOficial.NFref.FindAll(x => x.RefECF != null && x.RefECF.NCOO > 0));
            identificacaoOficial.NFref = referencias.Count == 0 ? null : referencias;
        }

        private void ProcessarCabecalhoInicial()
        {
                    double v = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.versao, ObOp.Opcional, 6);
                    this.chave = this.LerString(TpcnResources.ID, ObOp.Opcional, 0, 47);
                    // Alguns emissores informam apenas o tipo do documento (NFe ou NFCe) no campo reservado à chave.
                    if (string.Equals(this.chave, "NFe", StringComparison.OrdinalIgnoreCase) ||
                        string.Equals(this.chave, "NFCe", StringComparison.OrdinalIgnoreCase))
                    {
                        this.chave = string.Empty;
                    }
                    this.chave = NormalizarChaveDFe(this.chave);
                    if (!string.IsNullOrEmpty(this.chave) && this.chave.Length != 44)
                    {
                        throw new Exception("Chave de acesso inválida no segmento A");
                    }

                    if (Convert.ToDecimal(v) != 4.00M)
                    {
                        throw new Exception("Somente a versão 4.00 da NFe/NFCe é suportada");
                    }

                    versaoNFe = 4.00M;
        }

        private void ProcessarIdentificacao(int lenPipesRegistro)
        {
            identificacaoOficial.CUF = (UFBrasil)this.LerInt32(TpcnResources.cUF, ObOp.Obrigatorio, 2, 2);
            var codigoNumerico = this.LerInt32(TpcnResources.cNF, ObOp.Opcional, 8, 8);
            identificacaoOficial.CNF = codigoNumerico > 0 ? codigoNumerico.ToString("00000000") : null;
            identificacaoOficial.NatOp = this.LerString(TpcnResources.natOp, ObOp.Obrigatorio, 1, 60);
            identificacaoOficial.Mod = (ModeloDFe)this.LerInt32(TpcnResources.mod, ObOp.Obrigatorio, 2, 2);
            identificacaoOficial.Serie = this.LerInt32(TpcnResources.serie, ObOp.Obrigatorio, 1, 3);
            identificacaoOficial.NNF = this.LerInt32(TpcnResources.nNF, ObOp.Obrigatorio, 1, 9);

            var dataEmissao = this.LerString(TpcnResources.dhEmi, ObOp.Obrigatorio, 19, 25);
            var dataSaida = this.LerString(TpcnResources.dhSaiEnt, ObOp.Opcional, 0, 25);
            var destinoOperacao = this.LerInt32(TpcnResources.idDest, ObOp.Obrigatorio, 1, 1);
            if (string.IsNullOrEmpty(dataEmissao) || Convert.ToDateTime(dataEmissao).Year == 1 || dataEmissao.EndsWith("00:00"))
            {
                throw new Exception("Data de emissão da nota inválida");
            }
            if (!string.IsNullOrEmpty(dataSaida) && Convert.ToDateTime(dataSaida).Year == 1)
            {
                throw new Exception("Data de saida da nota inválida");
            }

            identificacaoOficial.DhEmi = ConverterDataHora(dataEmissao);
            if (identificacaoOficial.Mod == ModeloDFe.NFe && !string.IsNullOrWhiteSpace(dataSaida))
            {
                identificacaoOficial.DhSaiEnt = ConverterDataHora(dataSaida);
            }
            identificacaoOficial.IdDest = identificacaoOficial.Mod == ModeloDFe.NFCe
                ? DestinoOperacao.OperacaoInterna
                : (DestinoOperacao)destinoOperacao;
            identificacaoOficial.TpNF = (TipoOperacao)this.LerInt32(TpcnResources.tpNF, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.CMunFG = this.LerInt32(TpcnResources.cMunFG, ObOp.Obrigatorio, 7, 7);
            identificacaoOficial.CMunFGIBS = this.LerInt32(TpcnResources.cMunFGIBS, ObOp.Opcional, 7, 7);
            identificacaoOficial.TpImp = (FormatoImpressaoDANFE)this.LerInt32(TpcnResources.tpImp, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.TpEmis = (TipoEmissao)this.LerInt32(TpcnResources.tpEmis, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.CDV = this.LerInt32(TpcnResources.cDV, ObOp.Opcional, 1, 1);
            this.cDvInformado = identificacaoOficial.CDV != 0;
            identificacaoOficial.TpAmb = (TipoAmbiente)this.LerInt32(TpcnResources.tpAmb, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.FinNFe = (FinalidadeNFe)this.LerInt32(TpcnResources.finNFe, ObOp.Obrigatorio, 1, 1);

            var tipoDebito = this.LerInt32(TpcnResources.tpNFDebito, ObOp.Opcional, 2, 2, true);
            identificacaoOficial.TpNFDebito = ObterEnumOpcional(tipoDebito, (TipoNFDebito)(-1));
            var tipoCredito = this.LerInt32(TpcnResources.tpNFCredito, ObOp.Opcional, 2, 2, true);
            identificacaoOficial.TpNFCredito = ObterEnumOpcional(tipoCredito, (TipoNFCredito)(-1));
            identificacaoOficial.IndFinal = (SimNao)this.LerInt32(TpcnResources.indFinal, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.IndPres = (IndicadorPresenca)this.LerInt32(TpcnResources.indPres, ObOp.Obrigatorio, 1, 1);

            if (lenPipesRegistro >= 24)
            {
                var indicadorIntermediario = this.LerInt32(TpcnResources.indIntermed, ObOp.Opcional, 1, 1, true);
                identificacaoOficial.IndIntermed = ObterEnumOpcional(indicadorIntermediario, (IndicadorIntermediario)(-1));
            }

            identificacaoOficial.ProcEmi = (ProcessoEmissao)this.LerInt32(TpcnResources.procEmi, ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.VerProc = this.LerString(TpcnResources.verProc, ObOp.Obrigatorio, 1, 20);
            var dataContingencia = this.LerString(TpcnResources.dhCont, ObOp.Opcional, 0, 25);
            if (!string.IsNullOrWhiteSpace(dataContingencia))
            {
                identificacaoOficial.DhCont = ConverterDataHora(dataContingencia);
            }
            identificacaoOficial.XJust = VazioParaNulo(this.LerString(TpcnResources.xJust, ObOp.Opcional, 15, 256));

            if (lenPipesRegistro >= 28)
            {
                var previsaoEntrega = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dPrevEntrega, ObOp.Opcional, 8, 10, true, false);
                if (identificacaoOficial.Mod == ModeloDFe.NFe && previsaoEntrega > DateTime.MinValue)
                {
                    identificacaoOficial.DPrevEntrega = previsaoEntrega;
                }
            }

            if (!string.IsNullOrEmpty(this.chave))
            {
                if (codigoNumerico == 0)
                {
                    identificacaoOficial.CNF = this.chave.Substring(35, 8);
                }
                if (identificacaoOficial.CDV == 0)
                {
                    identificacaoOficial.CDV = Convert.ToInt32(this.chave.Substring(this.chave.Length - 1, 1));
                    this.cDvInformado = true;
                }
            }

            this.nfeOficial = new DFeNFe.NFe
            {
                InfNFeField = new DFeNFe.InfNFe
                {
                    Versao = "4.00",
                    Ide = identificacaoOficial
                }
            };
        }

        /// <summary>
        /// LerRegistro
        /// </summary>
        private void ProcessarEmitente()
        {
            this.nfeOficial.InfNFeField.Emit = new DFeNFe.Emit();
            this.nfeOficial.InfNFeField.Emit.XNome = this.LerString(TpcnResources.xNome, ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.XFant = VazioParaNulo(this.LerString(TpcnResources.xFant, ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.IE = this.LerString(TpcnResources.IE, ObOp.Opcional, 0, 14);
            this.nfeOficial.InfNFeField.Emit.IEST = VazioParaNulo(this.LerString(TpcnResources.IEST, ObOp.Opcional, 2, 14));
            this.nfeOficial.InfNFeField.Emit.IM = VazioParaNulo(this.LerString(TpcnResources.IM, ObOp.Opcional, 1, 15));
            this.nfeOficial.InfNFeField.Emit.CNAE = VazioParaNulo(this.LerString(TpcnResources.CNAE, ObOp.Opcional, 7, 7));
            this.nfeOficial.InfNFeField.Emit.CRT = (CRT)this.LerInt32(TpcnResources.CRT, ObOp.Obrigatorio, 1, 1);
            this.nfeOficial.InfNFeField.Emit.ISUFEmit = VazioParaNulo(this.LerString(TpcnResources.ISUFEmit, ObOp.Opcional, 8, 9));
        }

        private void ProcessarDocumentoEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.CPF = this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11);
        }

        private void ProcessarEnderecoEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.EnderEmit = new DFeNFe.EnderEmit();
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XLgr = this.LerString(TpcnResources.xLgr, ObOp.Obrigatorio, 1, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.Nro = this.LerString(TpcnResources.nro, ObOp.Obrigatorio, 1, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XCpl = VazioParaNulo(this.LerString(TpcnResources.xCpl, ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XBairro = this.LerString(TpcnResources.xBairro, ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CMun = this.LerInt32(TpcnResources.cMun, ObOp.Obrigatorio, 7, 7);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XMun = this.LerString(TpcnResources.xMun, ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.UF = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(TpcnResources.UF, ObOp.Obrigatorio, 2, 2), true);
            var cep = this.LerInt32(TpcnResources.CEP, ObOp.Opcional, 0, 8);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CEP = cep > 0 ? cep.ToString("00000000") : null;
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CPais = this.LerInt32(TpcnResources.cPais, ObOp.Obrigatorio, 4, 4);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XPais = VazioParaNulo(this.LerString(TpcnResources.xPais, ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.EnderEmit.Fone = VazioParaNulo(this.LerString(TpcnResources.fone, ObOp.Opcional, 6, 14));
        }
        private void ProcessarNotaAvulsa()
        {
            this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
            this.LerString(TpcnResources.xOrgao, ObOp.Obrigatorio, 1, 60);
            this.LerString(TpcnResources.matr, ObOp.Obrigatorio, 1, 60);
            this.LerString(TpcnResources.xAgente, ObOp.Obrigatorio, 1, 60);
            this.LerString(TpcnResources.fone, ObOp.Obrigatorio, 6, 14);
            this.LerString(TpcnResources.UF, ObOp.Obrigatorio, 2, 2);
            this.LerString(TpcnResources.nDAR, ObOp.Obrigatorio, 1, 60);
            this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dEmi, ObOp.Obrigatorio, 10, 10, true, false);
            this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDAR, ObOp.Obrigatorio, 15);
            this.LerString(TpcnResources.repEmi, ObOp.Obrigatorio, 1, 60);
            this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dPag, ObOp.Opcional, 10, 10, true, false);
        }
        private void ProcessarDestinatario()
        {
            destinatarioOficial.XNome = this.LerString(TpcnResources.xNome, identificacaoOficial.Mod != ModeloDFe.NFe ? ObOp.Opcional : ObOp.Obrigatorio, 2, 60);
            if (versaoNFe >= 3)
                destinatarioOficial.IndIEDest = (IndicadorIEDestinatario)this.LerInt32(TpcnResources.indIEDest, ObOp.Opcional, 0, 1);
            destinatarioOficial.IE = VazioParaNulo(this.LerString(TpcnResources.IE, ObOp.Opcional, 0, 14));
            destinatarioOficial.ISUF = VazioParaNulo(this.LerString(TpcnResources.ISUF, ObOp.Opcional, 8, 9));
            if (versaoNFe >= 3)
                destinatarioOficial.IM = VazioParaNulo(this.LerString(TpcnResources.IM, ObOp.Opcional, 1, 15));
            destinatarioOficial.Email = VazioParaNulo(this.LerString(TpcnResources.email, ObOp.Opcional, 1, 60));
        }

        private void ProcessarDocumentoDestinatario()
        {
            destinatarioOficial.CNPJ = VazioParaNulo(this.LerString(TpcnResources.CNPJ, ObOp.Opcional, 14, 14));
        }

        private void ProcessarCpfDestinatario()
        {
            if (identificacaoOficial.Mod == ModeloDFe.NFCe)
                destinatarioOficial.CPF = VazioParaNulo(this.LerString(TpcnResources.CPF, ObOp.Opcional, 11, 11));
            else
                destinatarioOficial.CPF = VazioParaNulo(this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11));
        }

        private void ProcessarIdEstrangeiroDestinatario()
        {
            destinatarioOficial.IdEstrangeiro = this.LerString(TpcnResources.idEstrangeiro, ObOp.Opcional, 5, 20);
            if (string.IsNullOrEmpty(destinatarioOficial.IdEstrangeiro) && string.IsNullOrEmpty(destinatarioOficial.CPF)) destinatarioOficial.IdEstrangeiro = "NAO GERAR TAG";
        }

        private void ProcessarEnderecoDestinatario()
        {
            destinatarioOficial.EnderDest.XLgr = this.LerString(TpcnResources.xLgr, ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.Nro = this.LerString(TpcnResources.nro, ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.XCpl = VazioParaNulo(this.LerString(TpcnResources.xCpl, ObOp.Opcional, 1, 60));
            destinatarioOficial.EnderDest.XBairro = this.LerString(TpcnResources.xBairro, ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.CMun = this.LerInt32(TpcnResources.cMun, ObOp.Obrigatorio, 7, 7);
            destinatarioOficial.EnderDest.XMun = this.LerString(TpcnResources.xMun, ObOp.Obrigatorio, 2, 60);
            destinatarioOficial.EnderDest.UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio).Value;
            var cep = this.LerInt32(TpcnResources.CEP, ObOp.Opcional, 0, 8);
            destinatarioOficial.EnderDest.CEP = cep > 0 ? cep.ToString("00000000") : null;
            destinatarioOficial.EnderDest.CPais = this.LerInt32(TpcnResources.cPais, ObOp.Obrigatorio, 2, 4);
            destinatarioOficial.EnderDest.XPais = VazioParaNulo(this.LerString(TpcnResources.xPais, ObOp.Opcional, 2, 60));
            destinatarioOficial.EnderDest.Fone = VazioParaNulo(this.LerString(TpcnResources.fone, ObOp.Opcional, 6, 14));
        }
        private void ProcessarLocalRetirada(int lenPipesRegistro)
        {
            bool novo;
            if ((novo = lenPipesRegistro == 16 || versaoNFe >= 4))
            {
                retiradaOficial.CNPJ = VazioParaNulo(this.LerString(TpcnResources.CNPJ_CPF, ObOp.Opcional, 0, 0));
                if (!string.IsNullOrEmpty(retiradaOficial.CNPJ) && retiradaOficial.CNPJ.Length == 11)
                {
                    retiradaOficial.CPF = retiradaOficial.CNPJ;
                    retiradaOficial.CNPJ = null;
                }
                retiradaOficial.XNome = VazioParaNulo(this.LerString(TpcnResources.xNome, ObOp.Opcional, 2, 60));
            }
            retiradaOficial.XLgr = this.LerString(TpcnResources.xLgr, ObOp.Obrigatorio, 1, 60);
            retiradaOficial.Nro = this.LerString(TpcnResources.nro, ObOp.Obrigatorio, 1, 60);
            retiradaOficial.XCpl = VazioParaNulo(this.LerString(TpcnResources.xCpl, ObOp.Opcional, 1, 60));
            retiradaOficial.XBairro = this.LerString(TpcnResources.xBairro, ObOp.Obrigatorio, 1, 60);
            retiradaOficial.CMun = this.LerInt32(TpcnResources.cMun, ObOp.Obrigatorio, 7, 7);
            retiradaOficial.XMun = this.LerString(TpcnResources.xMun, ObOp.Obrigatorio, 2, 60);
            retiradaOficial.UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio).Value;
            if (novo)
            {
                retiradaOficial.CEP = VazioParaNulo(this.LerString(TpcnResources.CEP, ObOp.Opcional, 8, 8));
                retiradaOficial.CPais = this.LerInt32(TpcnResources.cPais, ObOp.Opcional, 4, 4);
                retiradaOficial.XPais = VazioParaNulo(this.LerString(TpcnResources.xPais, ObOp.Opcional, 2, 60));
                retiradaOficial.Fone = VazioParaNulo(this.LerString(TpcnResources.fone, ObOp.Opcional, 6, 14));
                retiradaOficial.Email = VazioParaNulo(this.LerString(TpcnResources.email, ObOp.Opcional, 1, 60));
                retiradaOficial.IE = VazioParaNulo(this.LerString(TpcnResources.IE, ObOp.Opcional, 2, 14));
            }
        }

        private void ProcessarDocumentoRetirada()
        {
            retiradaOficial.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfRetirada()
        {
            retiradaOficial.CPF = this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11);
        }
        private void ProcessarLocalEntrega(int lenPipesRegistro)
        {
            bool novo;
            if ((novo = lenPipesRegistro == 16 || versaoNFe >= 4))
            {
                entregaOficial.CNPJ = VazioParaNulo(this.LerString(TpcnResources.CNPJ_CPF, ObOp.Opcional, 0, 0));
                if (!string.IsNullOrEmpty(entregaOficial.CNPJ) && entregaOficial.CNPJ.Length == 11)
                {
                    entregaOficial.CPF = entregaOficial.CNPJ;
                    entregaOficial.CNPJ = null;
                }
                entregaOficial.XNome = VazioParaNulo(this.LerString(TpcnResources.xNome, ObOp.Opcional, 2, 60));
            }
            entregaOficial.XLgr = this.LerString(TpcnResources.xLgr, ObOp.Obrigatorio, 1, 60);
            entregaOficial.Nro = this.LerString(TpcnResources.nro, ObOp.Obrigatorio, 1, 60);
            entregaOficial.XCpl = VazioParaNulo(this.LerString(TpcnResources.xCpl, ObOp.Opcional, 1, 60));
            entregaOficial.XBairro = this.LerString(TpcnResources.xBairro, ObOp.Obrigatorio, 1, 60);
            entregaOficial.CMun = this.LerInt32(TpcnResources.cMun, ObOp.Obrigatorio, 7, 7);
            entregaOficial.XMun = this.LerString(TpcnResources.xMun, ObOp.Obrigatorio, 2, 60);
            entregaOficial.UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio).Value;
            if (novo)
            {
                entregaOficial.CEP = VazioParaNulo(this.LerString(TpcnResources.CEP, ObOp.Opcional, 8, 8));
                entregaOficial.CPais = this.LerInt32(TpcnResources.cPais, ObOp.Opcional, 4, 4);
                entregaOficial.XPais = VazioParaNulo(this.LerString(TpcnResources.xPais, ObOp.Opcional, 2, 60));
                entregaOficial.Fone = VazioParaNulo(this.LerString(TpcnResources.fone, ObOp.Opcional, 6, 14));
                entregaOficial.Email = VazioParaNulo(this.LerString(TpcnResources.email, ObOp.Opcional, 1, 60));
                entregaOficial.IE = VazioParaNulo(this.LerString(TpcnResources.IE, ObOp.Opcional, 2, 14));
            }
        }

        private void ProcessarDocumentoEntrega()
        {
            entregaOficial.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfEntrega()
        {
            entregaOficial.CPF = this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11);
        }

        private void AdicionarAutorizacaoXmlCnpj()
        {
            autorizadosXmlOficiais.Add(new DFeNFe.AutXML
            {
                CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14)
            });
        }

        private void AdicionarAutorizacaoXmlCpf()
        {
            autorizadosXmlOficiais.Add(new DFeNFe.AutXML
            {
                CPF = this.LerString(TpcnResources.CPF, ObOp.Obrigatorio, 11, 11)
            });
        }
        private void IniciarItemDaNota(ref int nProd)
        {
            nProd = detalhesOficiais.Count;
            detalhesOficiais.Add(new DFeNFe.Det
            {
                NItem = this.LerInt32(TpcnResources.NItem, ObOp.Obrigatorio, 1, 3),
                InfAdProd = VazioParaNulo(this.LerString(TpcnResources.infAdProd, ObOp.Opcional, 0, 500, false)),
                Prod = new DFeNFe.Prod(),
                Imposto = new DFeNFe.Imposto()
            });
        }
        private void ProcessarProduto(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            produto.CProd = this.LerString(TpcnResources.cProd, ObOp.Obrigatorio, 1, 60);
            produto.CEAN = this.LerString(TpcnResources.cEAN, ObOp.Obrigatorio, 0, 14);
            if (lenPipesRegistro >= 30)
            {
                produto.CBarra = VazioParaNulo(this.LerString(TpcnResources.cBarra, ObOp.Opcional, 0, 30));
            }

            var descricao = this.LerString(TpcnResources.xProd, ObOp.Obrigatorio, 1, 120);
            produto.XProd = identificacaoOficial.TpAmb == TipoAmbiente.Homologacao &&
                identificacaoOficial.Mod == ModeloDFe.NFCe && detalhesOficiais[nProd].NItem == 1
                    ? "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
                    : descricao;
            produto.NCM = this.LerString(TpcnResources.NCM, ObOp.Obrigatorio, 2, 8);
            var nve = this.LerString(TpcnResources.NVE, ObOp.Opcional, 0, 6);
            produto.NVE = string.IsNullOrWhiteSpace(nve) ? null : new List<string> { nve };
            var cest = this.LerInt32(TpcnResources.CEST, ObOp.Opcional, 0, 7);
            produto.CEST = cest > 0 ? cest.ToString("0000000") : null;

            switch (this.LerString(TpcnResources.indEscala, ObOp.Opcional, 1, 1))
            {
                case "S":
                    produto.IndEscala = IndicadorEscalaRelevante.Sim;
                    break;
                case "N":
                    produto.IndEscala = IndicadorEscalaRelevante.Nao;
                    break;
                default:
                    produto.IndEscala = ObterEnumOpcional(-1, (IndicadorEscalaRelevante)(-1));
                    break;
            }

            produto.CNPJFab = VazioParaNulo(this.LerString(TpcnResources.CNPJFab, ObOp.Opcional, 0, 14));
            produto.CBenef = VazioParaNulo(this.LerString(TpcnResources.cBenef, ObOp.Opcional, 0, 10));
            produto.EXTIPI = VazioParaNulo(this.LerString(TpcnResources.EXTIPI, ObOp.Opcional, 2, 3));
            produto.CFOP = this.LerString(TpcnResources.CFOP, ObOp.Obrigatorio, 4, 4);
            produto.UCom = this.LerString(TpcnResources.uCom, ObOp.Obrigatorio, 1, 6);
            produto.QCom = this.LerDecimal(TpcnTipoCampo.tcDec4, TpcnResources.qCom, ObOp.Obrigatorio, 11);
            produto.VUnCom = this.LerDecimal(TpcnTipoCampo.tcDec10, TpcnResources.vUnCom, ObOp.Opcional, 21);
            produto.VProd = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vProd, ObOp.Obrigatorio, 15);
            produto.CEANTrib = this.LerString(TpcnResources.cEANTrib, ObOp.Obrigatorio, 0, 14);
            if (lenPipesRegistro >= 30)
            {
                produto.CBarraTrib = VazioParaNulo(this.LerString(TpcnResources.cBarraTrib, ObOp.Opcional, 0, 30));
            }
            produto.UTrib = this.LerString(TpcnResources.uTrib, ObOp.Obrigatorio, 1, 6);
            produto.QTrib = this.LerDecimal(TpcnTipoCampo.tcDec4, TpcnResources.qTrib, ObOp.Obrigatorio, 15);
            produto.VUnTrib = this.LerDecimal(TpcnTipoCampo.tcDec10, TpcnResources.vUnTrib, ObOp.Obrigatorio, 21);
            produto.VFrete = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFrete, ObOp.Opcional, 15);
            produto.VSeg = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vSeg, ObOp.Opcional, 15);
            produto.VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDesc, ObOp.Opcional, 15);
            produto.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vOutro, ObOp.Opcional, 15);
            produto.IndTot = (SimNao)this.LerInt32(TpcnResources.indTot, ObOp.Obrigatorio, 1, 1);
            produto.XPed = VazioParaNulo(this.LerString(TpcnResources.xPed, ObOp.Opcional, 1, 15));
            produto.NItemPed = VazioParaNulo(this.LerString(TpcnResources.nItemPed, ObOp.Opcional, 0, 6));
            produto.NFCI = VazioParaNulo(this.LerString(TpcnResources.nFCI, ObOp.Opcional, 0, 255));
        }

        private void AdicionarCreditoPresumido(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.GCred == null) produto.GCred = new List<DFeNFe.GCred>();
            produto.GCred.Add(new DFeNFe.GCred
            {
                CCredPresumido = this.LerString(TpcnResources.cCredPresumido, ObOp.Obrigatorio, 8, 10),
                PCredPresumido = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pCredPresumido, ObOp.Obrigatorio, 8),
                VCredPresumido = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresumido, ObOp.Obrigatorio, 16)
            });
        }

        private void ProcessarNveProduto(int nProd)
        {
            var nve = this.LerString(TpcnResources.NVE, ObOp.Opcional, 0, 6);
            detalhesOficiais[nProd].Prod.NVE = string.IsNullOrWhiteSpace(nve) ? null : new List<string> { nve };
        }

        private void ProcessarCreditoPresumidoIbsZfm(int nProd)
        {
            var valor = this.LerString(TpcnResources.tpCredPresIBSZFM, ObOp.Opcional, 1, 1);
            int codigo;
            detalhesOficiais[nProd].Prod.TpCredPresIBSZFM = ObterEnumOpcional(
                int.TryParse(valor, out codigo) ? codigo : -1, (TipoCreditoPresumidoIBSZFM)(-1));
        }

        private void ProcessarCestProduto(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            var cest = this.LerInt32(TpcnResources.CEST, ObOp.Opcional, 0, 7);
            produto.CEST = cest > 0 ? cest.ToString("0000000") : null;
            if (lenPipesRegistro == 4)
            {
                this.LerInt32(TpcnResources.indEscala, ObOp.Opcional, 1, 1);
                produto.IndEscala = ObterEnumOpcional(-1, (IndicadorEscalaRelevante)(-1));
                produto.CNPJFab = VazioParaNulo(this.LerString(TpcnResources.CNPJFab, ObOp.Opcional, 0, 14));
            }
        }

        private void ProcessarBemMovelUsado(int nProd)
        {
            detalhesOficiais[nProd].Prod.IndBemMovelUsado = this.LerInt32(TpcnResources.indBemMovelUsado, ObOp.Opcional, 1, 1);
        }
        private void AdicionarDeclaracaoImportacao(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.DI == null) produto.DI = new List<DFeNFe.DI>();
            var item = new DFeNFe.DI
            {
                NDI = this.LerString(TpcnResources.nDI, ObOp.Obrigatorio, 1, 15),
                DDI = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dDI, ObOp.Obrigatorio, 10, 10, true, false),
                XLocDesemb = this.LerString(TpcnResources.xLocDesemb, ObOp.Obrigatorio, 1, 60),
                UFDesemb = LerUF(TpcnResources.UFDesemb, ObOp.Obrigatorio).Value,
                DDesemb = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dDesemb, ObOp.Obrigatorio, 10, 10, true, false),
                TpViaTransp = (ViaTransporteInternacional)this.LerInt32(TpcnResources.tpViaTransp, ObOp.Opcional, 1, 2),
                VAFRMM = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vAFRMM, ObOp.Opcional, 15),
                TpIntermedio = (FormaImportacaoIntermediacao)this.LerInt32(TpcnResources.tpIntermedio, ObOp.Opcional, 1, 1),
                CNPJ = VazioParaNulo(this.LerString(TpcnResources.CNPJ, ObOp.Opcional, 14, 14))
            };
            if (lenPipesRegistro >= 13)
            {
                item.CPF = VazioParaNulo(this.LerString(TpcnResources.CPF, ObOp.Opcional, 11, 11));
            }
            var ufTerceiro = this.LerString(TpcnResources.UFTerceiro, ObOp.Opcional, 2, 2);
#if INTEROP
            item.UFTerceiro = string.IsNullOrWhiteSpace(ufTerceiro) ? UFBrasil.NaoDefinido : (UFBrasil)Enum.Parse(typeof(UFBrasil), ufTerceiro, true);
#else
            item.UFTerceiro = string.IsNullOrWhiteSpace(ufTerceiro) ? null : (UFBrasil?)Enum.Parse(typeof(UFBrasil), ufTerceiro, true);
#endif
            item.CExportador = this.LerString(TpcnResources.cExportador, ObOp.Obrigatorio, 1, 60);
            produto.DI.Add(item);
        }

        private void AdicionarAdicaoImportacao(int nProd)
        {
            var declaracoes = detalhesOficiais[nProd].Prod.DI;
            var declaracao = declaracoes[declaracoes.Count - 1];
            if (declaracao.Adi == null) declaracao.Adi = new List<DFeNFe.Adi>();
            declaracao.Adi.Add(new DFeNFe.Adi
            {
                NAdicao = this.LerInt32(TpcnResources.nAdicao, ObOp.Obrigatorio, 1, 3),
                NSeqAdic = this.LerInt32(TpcnResources.nSeqAdic, ObOp.Obrigatorio, 1, 3),
                CFabricante = this.LerString(TpcnResources.cFabricante, ObOp.Obrigatorio, 1, 60),
                VDescDI = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDescDI, ObOp.Opcional, 15),
                NDraw = VazioParaNulo(this.LerString(TpcnResources.nDraw, ObOp.Opcional, 0, 11))
            });
        }

        private void AdicionarDetalheExportacao(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.DetExport == null) produto.DetExport = new List<DFeNFe.DetExport>();
            produto.DetExport.Add(new DFeNFe.DetExport { NDraw = VazioParaNulo(this.LerString(TpcnResources.nDraw, ObOp.Opcional, 0, 11)) });
        }

        private void ProcessarExportacaoIndireta(int nProd)
        {
            var exportacoes = detalhesOficiais[nProd].Prod.DetExport;
            exportacoes[exportacoes.Count - 1].ExportInd = new DFeNFe.ExportInd
            {
                NRE = this.LerString(TpcnResources.nRE, ObOp.Opcional, 1, 12),
                ChNFe = this.LerString(TpcnResources.chNFe, ObOp.Obrigatorio, 44, 44),
                QExport = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qExport, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void AdicionarRastroProduto(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.Rastro == null) produto.Rastro = new List<DFeNFe.Rastro>();
            produto.Rastro.Add(new DFeNFe.Rastro
            {
                NLote = this.LerString(TpcnResources.nLote, ObOp.Obrigatorio, 1, 20),
                QLote = this.LerDouble(TpcnTipoCampo.tcDouble3, TpcnResources.qLote, ObOp.Obrigatorio, 11),
                DFab = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dFab, ObOp.Obrigatorio, 10, 10, true, false),
                DVal = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dVal, ObOp.Obrigatorio, 10, 10, true, false),
                CAgreg = VazioParaNulo(this.LerString(TpcnResources.cAgreg, ObOp.Opcional, 0, 20))
            });
        }
        private void ProcessarResponsavelTecnico()
        {
            var responsavel = CriarResponsavelTecnico();
            responsavel.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
            responsavel.XContato = this.LerString("xContato", ObOp.Obrigatorio, 2, 60);
            responsavel.Email = this.LerString(TpcnResources.email, ObOp.Obrigatorio, 2, 60);
            responsavel.Fone = this.LerString(TpcnResources.fone, ObOp.Obrigatorio, 6, 14);
            var idCsrt = this.LerInt32("idCSRT", ObOp.Opcional, 2, 2);
            responsavel.IdCSRT = idCsrt > 0 ? idCsrt.ToString("00") : null;
            responsavel.HashCSRT = VazioParaNulo(this.LerString("hashCSRT", ObOp.Opcional, 28, 28));
        }

        private DFeNFe.InfRespTec CriarResponsavelTecnico()
        {
            return this.nfeOficial.InfNFeField.InfRespTec = new DFeNFe.InfRespTec();
        }
        private void ProcessarVeiculoNovo(int nProd)
        {
            detalhesOficiais[nProd].Prod.VeicProd = new DFeNFe.VeicProd
            {
                TpOp = (TipoOperacaoVeicNovo)this.LerInt32(TpcnResources.tpOp, ObOp.Obrigatorio, 1, 1),
                Chassi = this.LerString(TpcnResources.chassi, ObOp.Obrigatorio, 17, 17),
                CCor = this.LerString(TpcnResources.cCor, ObOp.Obrigatorio, 1, 4),
                XCor = this.LerString(TpcnResources.xCor, ObOp.Obrigatorio, 1, 40),
                Pot = this.LerString(TpcnResources.pot, ObOp.Obrigatorio, 1, 4),
                Cilin = this.LerString(TpcnResources.cilin, ObOp.Obrigatorio, 1, 4),
                PesoL = this.LerString(TpcnResources.pesoL, ObOp.Obrigatorio, 1, 9),
                PesoB = this.LerString(TpcnResources.pesoB, ObOp.Obrigatorio, 1, 9),
                NSerie = this.LerString(TpcnResources.nSerie, ObOp.Obrigatorio, 1, 9),
                TpComb = this.LerString(TpcnResources.tpComb, ObOp.Obrigatorio, 1, 2),
                NMotor = this.LerString(TpcnResources.nMotor, ObOp.Obrigatorio, 1, 21),
                CMT = this.LerString(TpcnResources.CMT, ObOp.Obrigatorio, 1, 9),
                Dist = this.LerString(TpcnResources.dist, ObOp.Obrigatorio, 1, 4),
                AnoMod = this.LerInt32(TpcnResources.anoMod, ObOp.Obrigatorio, 4, 4).ToString("0000"),
                AnoFab = this.LerInt32(TpcnResources.anoFab, ObOp.Obrigatorio, 4, 4).ToString("0000"),
                TpPint = this.LerString(TpcnResources.tpPint, ObOp.Obrigatorio, 1, 1),
                TpVeic = this.LerInt32(TpcnResources.tpVeic, ObOp.Obrigatorio, 1, 2).ToString(),
                EspVeic = this.LerInt32(TpcnResources.espVeic, ObOp.Obrigatorio, 1, 1).ToString(),
                VIN = (CondicaoVIN)this.LerInt32(TpcnResources.VIN, ObOp.Obrigatorio, 1, 1),
                CondVeic = (CondicaoVeiculo)this.LerInt32(TpcnResources.condVeic, ObOp.Obrigatorio, 1, 1),
                CMod = this.LerInt32(TpcnResources.cMod, ObOp.Obrigatorio, 1, 6),
                CCorDENATRAN = this.LerInt32(TpcnResources.cCorDENATRAN, ObOp.Obrigatorio, 1, 2).ToString("00"),
                Lota = this.LerInt32(TpcnResources.lota, ObOp.Obrigatorio, 1, 3),
                TpRest = (TipoRestricaoVeiculo)this.LerInt32(TpcnResources.tpRest, ObOp.Obrigatorio, 1, 1)
            };
        }

        private void AdicionarMedicamento(int nProd)
        {
            if (detalhesOficiais[nProd].Prod.Med != null)
            {
                throw new NotSupportedException("A classe NFe da Unimake.DFe aceita somente um grupo med por produto.");
            }
            detalhesOficiais[nProd].Prod.Med = new DFeNFe.Med
            {
                CProdANVISA = LerString(TpcnResources.cProdANVISA, ObOp.Obrigatorio, 1, 13),
                XMotivoIsencao = VazioParaNulo(LerString("xMotivoIsencao", ObOp.Opcional, 1, 255)),
                VPMC = LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPMC, ObOp.Obrigatorio, 15)
            };
        }

        private void AdicionarArma(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.Arma == null) produto.Arma = new List<DFeNFe.Arma>();
            produto.Arma.Add(new DFeNFe.Arma
            {
                TpArma = (TipoArma)this.LerInt32(TpcnResources.tpArma, ObOp.Obrigatorio, 1, 1),
                NSerie = LerString(TpcnResources.nSerie, ObOp.Obrigatorio, 1, 15),
                NCano = LerString(TpcnResources.nCano, ObOp.Obrigatorio, 1, 15),
                Descr = LerString(TpcnResources.descr, ObOp.Obrigatorio, 1, 256)
            });
        }

        private void ProcessarCombustivel(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            var combustivel = new DFeNFe.Comb
            {
                CProdANP = this.LerInt32(TpcnResources.cProdANP, ObOp.Obrigatorio, 9, 9).ToString("000000000"),
                DescANP = this.LerString(TpcnResources.descANP, ObOp.Opcional, 2, 295),
                PGLP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pGLP, ObOp.Opcional, 16),
                PGNn = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pGNn, ObOp.Opcional, 16),
                PGNi = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pGNi, ObOp.Opcional, 16),
                VPart = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPart, ObOp.Opcional, 16),
                CODIF = VazioParaNulo(this.LerString(TpcnResources.CODIF, ObOp.Opcional, 0, 21)),
                QTemp = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qTemp, ObOp.Opcional, 16),
                UFCons = LerUF(TpcnResources.UFCons, ObOp.Obrigatorio).Value
            };

            if (lenPipesRegistro >= 11)
            {
                combustivel.PBio = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pBio, ObOp.Opcional, 15);
            }
            produto.Comb = new List<DFeNFe.Comb> { combustivel };
        }

        private void ProcessarEncerranteCombustivel(int nProd)
        {
            var combustivel = ObterCombustivel(nProd);
            combustivel.Encerrante = new DFeNFe.Encerrante
            {
                NBico = this.LerInt32(TpcnResources.nBico, ObOp.Obrigatorio, 1, 3),
                NBomba = this.LerInt32(TpcnResources.nBomba, ObOp.Opcional, 0, 3),
                NTanque = this.LerInt32(TpcnResources.nTanque, ObOp.Obrigatorio, 1, 3),
                VEncIni = double.Parse(this.LerString(TpcnResources.vEncIni, ObOp.Obrigatorio, 1, 15), CultureInfo.InvariantCulture),
                VEncFin = double.Parse(this.LerString(TpcnResources.vEncFin, ObOp.Obrigatorio, 1, 15), CultureInfo.InvariantCulture)
            };
        }

        private void AdicionarOrigemCombustivel(int nProd)
        {
            var combustivel = ObterCombustivel(nProd);
            if (combustivel.OrigComb == null) combustivel.OrigComb = new List<DFeNFe.OrigComb>();
            combustivel.OrigComb.Add(new DFeNFe.OrigComb
            {
                IndImport = (IndicadorImportacao)this.LerInt32(TpcnResources.indImport, ObOp.Obrigatorio, 1, 1),
                CUFOrig = (UFBrasil)this.LerInt32(TpcnResources.cUFOrig, ObOp.Obrigatorio, 2, 2),
                POrig = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pOrig, ObOp.Obrigatorio, 15)
            });
        }

        private void ProcessarCideCombustivel(int nProd)
        {
            var quantidade = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16);
            var aliquota = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15);
            var valor = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCIDE, ObOp.Obrigatorio, 15);
            ObterCombustivel(nProd).CIDE = quantidade > 0 ? new DFeNFe.CIDE
            {
                QBCProd = quantidade,
                VAliqProd = aliquota,
                VCIDE = valor
            } : null;
        }
        private void ProcessarRecopi(int nProd)
        {
            detalhesOficiais[nProd].Prod.NRECOPI = VazioParaNulo(this.LerString(TpcnResources.nRECOPI, ObOp.Opcional, 20, 20));
        }

        private DFeNFe.Comb ObterCombustivel(int nProd)
        {
            var combustiveis = detalhesOficiais[nProd].Prod.Comb;
            if (combustiveis == null || combustiveis.Count == 0)
            {
                throw new InvalidOperationException("Grupo de combustível não informado para o item.");
            }
            return combustiveis[combustiveis.Count - 1];
        }

        private void ProcessarTotalTributosItem(int nProd)
        {
            detalhesOficiais[nProd].Imposto.VTotTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTotTrib, ObOp.Opcional, 15);
        }
        private void ProcessarIcms00(int nProd)
        {
            var icms = new DFeNFe.ICMS00
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Obrigatorio, 15);
                icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS00 = icms };
        }

        private void ProcessarIcms02(int nProd)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS02 = new DFeNFe.ICMS02
                {
                    Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMono, ObOp.Opcional, 15),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, TpcnResources.adRemICMS, ObOp.Obrigatorio, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMono, ObOp.Obrigatorio, 13)
                }
            };
        }
        private void ProcessarIcms10(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS10
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCP, ObOp.Obrigatorio, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Obrigatorio, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15)
            };
            if (lenPipesRegistro >= 21)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTDeson, ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMSST, ObOp.Opcional, 1, 2);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS10 = icms };
        }

        private void ProcessarIcms15(int nProd)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS15 = new DFeNFe.ICMS15
                {
                    Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMono, ObOp.Opcional, 11),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, TpcnResources.adRemICMS, ObOp.Obrigatorio, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMono, ObOp.Obrigatorio, 15),
                    QBCMonoReten = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMonoReten, ObOp.Opcional, 11),
                    AdRemICMSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemICMSReten, ObOp.Obrigatorio, 15),
                    VICMSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoReten, ObOp.Obrigatorio, 15),
                    PRedAdRem = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pRedAdRem, ObOp.Obrigatorio, 15),
                    MotRedAdRem = (MotivoReducaoAdRem)this.LerInt32(TpcnResources.motRedAdRem, ObOp.Obrigatorio, 1, 1)
                }
            };
        }

        private void ProcessarIcms20(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS20
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Obrigatorio, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCP, ObOp.Obrigatorio, 15);
                icms.PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Obrigatorio, 15);
                icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Obrigatorio, 15);
            }
            icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15);
            icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMS, ObOp.Opcional, 1, 1);
            if (lenPipesRegistro >= 14) icms.IndDeduzDeson = LerSimNaoOpcional(TpcnResources.indDeduzDeson);
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS20 = icms };
        }

        private void ProcessarIcms30(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS30
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15);
            }
            icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15);
            icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMS, ObOp.Opcional, 1, 1);
            if (lenPipesRegistro >= 15) icms.IndDeduzDeson = LerSimNaoOpcional(TpcnResources.indDeduzDeson);
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS30 = icms };
        }

        private void ProcessarIcms40_41_50(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS40
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2)
            };
            if (lenPipesRegistro >= 5)
            {
                if (versaoNFe >= 3) icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15);
                else this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Opcional, 15);
                icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMS, ObOp.Opcional, 1, 1);
            }
            if (lenPipesRegistro >= 6) icms.IndDeduzDeson = LerSimNaoOpcional(TpcnResources.indDeduzDeson);
            if (icms.VICMSDeson <= 0)
            {
                icms.MotDesICMS = default(MotivoDesoneracaoICMS);
                icms.IndDeduzDeson = ObterEnumOpcional(-1, (SimNao)(-1));
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS40 = icms };
        }

        private void ProcessarIcms51(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS51
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Opcional, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Opcional, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Opcional, this.CasasDecimais75),
                VICMSOp = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSOp, ObOp.Opcional, 15),
                PDif = this.LerDouble(this.TipoCampo42, TpcnResources.pDif, ObOp.Opcional, this.CasasDecimais75),
                VICMSDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDif, ObOp.Opcional, 15),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Opcional, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCP, ObOp.Obrigatorio, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Obrigatorio, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Obrigatorio, 15)
            };
            if (lenPipesRegistro == 10 && (icms.VICMS ?? 0) == 0) icms.VICMS = Math.Max(0, (icms.VICMSOp ?? 0) - (icms.VICMSDif ?? 0));
            if (lenPipesRegistro >= 17)
            {
                icms.PFCPDif = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPDif, ObOp.Opcional, 15);
                icms.VFCPDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPDif, ObOp.Opcional, 15);
                icms.VFCPEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPEfet, ObOp.Opcional, 15);
                if (lenPipesRegistro >= 18) icms.CBenefRBC = this.LerString(TpcnResources.cBenefRBC, ObOp.Opcional, 8, 10);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS51 = icms };
        }

        private void ProcessarIcms53(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS53 = new DFeNFe.ICMS53
                {
                    Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMono, ObOp.Opcional, 15),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, TpcnResources.adRemICMS, ObOp.Obrigatorio, 15),
                    VICMSMonoOp = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoOp, ObOp.Opcional, 15),
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pDif, ObOp.Opcional, 15),
                    VICMSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoDif, ObOp.Opcional, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMono, ObOp.Opcional, 15)
                }
            };
        }

        private void ProcessarIcms60(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1);
            var cst = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2);
            double vBCSTRet = 0;
            double pST = 0;
            double vICMSSTRet = 0;
            double vICMSSubstituto = 0;
            var icms = new DFeNFe.ICMS60
            {
                Orig = origem,
                CST = cst
            };
            if (versaoNFe >= 4)
            {
                vBCSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCSTRet, ObOp.Obrigatorio, 15);
                pST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pST, ObOp.Opcional, 15);
                vICMSSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTRet, ObOp.Obrigatorio, 15);
                icms.VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPSTRet, ObOp.Opcional, 15);
                icms.PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPSTRet, ObOp.Opcional, 15);
                icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPSTRet, ObOp.Opcional, 15);
                if (lenPipesRegistro >= 13)
                {
                    icms.PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedBCEfet, ObOp.Opcional, 15);
                    icms.VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCEfet, ObOp.Opcional, 15);
                    icms.PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pICMSEfet, ObOp.Opcional, 15);
                    icms.VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSEfet, ObOp.Opcional, 15);
                }
                vICMSSubstituto = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSubstituto, ObOp.Opcional, 15);

                // A rotina histórica só gera estes quatro campos quando existe valor
                // no grupo de retenção ou quando a operação não é para consumidor final.
                var possuiGrupoIcms60 = vBCSTRet + vICMSSTRet + pST +
                    icms.VBCFCPSTRet + icms.PFCPSTRet + icms.VFCPSTRet +
                    icms.PRedBCEfet + icms.VBCEfet + icms.PICMSEfet + icms.VICMSEfet > 0 ||
                    identificacaoOficial.IndFinal == SimNao.Nao;
                var deveGerarRetidos = possuiGrupoIcms60 &&
                    (vBCSTRet + pST + vICMSSubstituto + vICMSSTRet > 0 ||
                     identificacaoOficial.IndFinal == SimNao.Nao);
                if (deveGerarRetidos)
                {
                    icms.VBCSTRet = vBCSTRet;
                    icms.PST = pST;
                    icms.VICMSSubstituto = vICMSSubstituto;
                    icms.VICMSSTRet = vICMSSTRet;
                }
            }

            // O gerador histórico escolhe a tag pelo CST efetivamente informado.
            // Quando um N08 com CST 00 complementa um N02 anterior, o ICMS00 já
            // preenchido deve ser preservado em vez de ser substituído por ICMS60.
            var grupoAtual = detalhesOficiais[nProd].Imposto.ICMS;
            if (cst == "00" && grupoAtual != null && grupoAtual.ICMS00 != null)
            {
                grupoAtual.ICMS00.Orig = origem;
                grupoAtual.ICMS00.CST = cst;
                return;
            }

            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS60 = icms };
        }

        private void ProcessarIcms61(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS61
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2)
            };
            if (versaoNFe >= 4)
            {
                icms.QBCMonoRet = this.LerDecimal(TpcnTipoCampo.tcDec4, TpcnResources.qBCMonoRet, ObOp.Obrigatorio, 15);
                icms.AdRemICMSRet = this.LerDouble(this.TipoCampo42, TpcnResources.adRemICMSRet, ObOp.Opcional, 15);
                icms.VICMSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoRet, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS61 = icms };
        }

        private void ProcessarIcms70(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS70
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Obrigatorio, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCP, ObOp.Opcional, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Opcional, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Opcional, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Opcional, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Opcional, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Opcional, 15),
                VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15),
                MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMS, ObOp.Opcional, 1, 1)
            };
            if (lenPipesRegistro >= 24)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTDeson, ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMSST, ObOp.Opcional, 1, 2);
                if (lenPipesRegistro >= 25) icms.IndDeduzDeson = LerSimNaoOpcional(TpcnResources.indDeduzDeson);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS70 = icms };
        }

        private void ProcessarIcms90(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS90
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15),
                VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15),
                MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMS, ObOp.Opcional, 1, 1),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCP, ObOp.Opcional, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCP, ObOp.Opcional, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Opcional, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Opcional, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Opcional, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Opcional, 15)
            };
            if (lenPipesRegistro >= 24)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTDeson, ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(TpcnResources.motDesICMSST, ObOp.Opcional, 1, 2);
                if (lenPipesRegistro >= 25) icms.IndDeduzDeson = LerSimNaoOpcional(TpcnResources.indDeduzDeson);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS90 = icms };
        }

        private void ProcessarIcmsPart10_90(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSPart
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15),
                PBCOp = this.LerDouble(this.TipoCampo42, TpcnResources.pBCOp, ObOp.Obrigatorio, this.CasasDecimais75),
                UFST = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(TpcnResources.UFST, ObOp.Obrigatorio, 2, 2))
            };
            if (lenPipesRegistro >= 19)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSPart = icms };
        }

        private void ProcessarIcmsSt(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMSST = new DFeNFe.ICMSST
                {
                    Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    VBCSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCSTRet, ObOp.Obrigatorio, 15),
                    VICMSSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTRet, ObOp.Obrigatorio, 15),
                    VBCSTDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCSTDest, ObOp.Obrigatorio, 15),
                    VICMSSTDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTDest, ObOp.Obrigatorio, 15),
                    PST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pST, ObOp.None, 15, true),
                    VICMSSubstituto = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSubstituto, ObOp.None, 15, true),
                    VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPSTRet, ObOp.None, 15, true),
                    PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPSTRet, ObOp.None, 15, true),
                    VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPSTRet, ObOp.None, 15, true),
                    PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedBCEfet, ObOp.None, 15, true),
                    VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCEfet, ObOp.None, 15, true),
                    PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pICMSEfet, ObOp.None, 15, true),
                    VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSEfet, ObOp.None, 15, true)
                }
            };
        }

        private void ProcessarIcmsSn101(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1);
            var csosn = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture);

            // O gerador histórico definia a tag final pelo CSOSN. Assim, mesmo
            // quando o TXT utiliza o layout N10c, os CSOSN 102/103/300/400 devem
            // ser serializados no grupo ICMSSN102.
            if (csosn == "102" || csosn == "103" || csosn == "300" || csosn == "400")
            {
                detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
                {
                    ICMSSN102 = new DFeNFe.ICMSSN102
                    {
                        Orig = origem,
                        CSOSN = csosn
                    }
                };
                return;
            }

            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMSSN101 = new DFeNFe.ICMSSN101
                {
                    Orig = origem,
                    CSOSN = csosn,
                    PCredSN = this.LerDouble(this.TipoCampo42, TpcnResources.pCredSN, ObOp.Obrigatorio, this.CasasDecimais75),
                    VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredICMSSN, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarIcmsSn102(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMSSN102 = new DFeNFe.ICMSSN102
                {
                    Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                    CSOSN = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture)
                }
            };
        }

        private void ProcessarIcmsSn201(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN201
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15);
            }
            icms.PCredSN = this.LerDouble(this.TipoCampo42, TpcnResources.pCredSN, ObOp.Obrigatorio, this.CasasDecimais75);
            icms.VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredICMSSN, ObOp.Obrigatorio, 15);
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN201 = icms };
        }

        private void ProcessarIcmsSn202(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN202
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN202 = icms };
        }

        private void ProcessarIcmsSn500(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1);
            var csosn = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture);

            // O layout N10g é utilizado por TXT legados também para os CSOSN que
            // são serializados como ICMSSN102. O gerador histórico escolhia a tag
            // final pelo CSOSN, e não apenas pelo identificador do segmento.
            if (csosn == "102" || csosn == "103" || csosn == "300" || csosn == "400")
            {
                detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
                {
                    ICMSSN102 = new DFeNFe.ICMSSN102
                    {
                        Orig = origem,
                        CSOSN = csosn
                    }
                };
                return;
            }

            var icms = new DFeNFe.ICMSSN500
            {
                Orig = origem,
                CSOSN = csosn
            };
            if (versaoNFe < 3) this.LerInt32(TpcnResources.modBCST, ObOp.Opcional, 1, 1);
            var baseRetida = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCSTRet, ObOp.Opcional, 15);
            var icmsRetido = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSTRet, ObOp.Opcional, 15);
            icms.VBCSTRet = baseRetida > 0 ? (double?)baseRetida : null;
            icms.VICMSSTRet = icmsRetido > 0 ? (double?)icmsRetido : null;
            if (versaoNFe >= 4)
            {
                var percentualSt = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pST, ObOp.Obrigatorio, 15);
                icms.PST = percentualSt > 0 ? (double?)percentualSt : null;
                icms.VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPSTRet, ObOp.Obrigatorio, 15);
                icms.PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPSTRet, ObOp.Obrigatorio, 15);
                icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPSTRet, ObOp.Obrigatorio, 15);
                if (lenPipesRegistro >= 13)
                {
                    icms.PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedBCEfet, ObOp.Opcional, 15);
                    icms.VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCEfet, ObOp.Opcional, 15);
                    icms.PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pICMSEfet, ObOp.Opcional, 15);
                    icms.VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSEfet, ObOp.Opcional, 15);
                }
                var substituto = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSSubstituto, ObOp.Opcional, 15);
                icms.VICMSSubstituto = substituto > 0 ? (double?)substituto : null;
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN500 = icms };
        }

        private void ProcessarIcmsSn900(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN900
            {
                Orig = (OrigemMercadoria)this.LerInt32(TpcnResources.orig, ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(TpcnResources.CSOSN, ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(TpcnResources.modBC, ObOp.Opcional, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Opcional, 15, true)
            };
            var reducao = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBC, ObOp.Opcional, this.CasasDecimais75);
            icms.PRedBC = reducao > 0 ? (double?)reducao : null;
            icms.PICMS = this.LerDouble(this.TipoCampo42, TpcnResources.pICMS, ObOp.Opcional, this.CasasDecimais75, true);
            icms.VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Opcional, 15, true);
            icms.ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(TpcnResources.modBCST, ObOp.Opcional, 1, 1, true);
            icms.PMVAST = this.LerDouble(this.TipoCampo42, TpcnResources.pMVAST, ObOp.Opcional, this.CasasDecimais75);
            var reducaoSt = this.LerDouble(this.TipoCampo42, TpcnResources.pRedBCST, ObOp.Opcional, this.CasasDecimais75);
            icms.PRedBCST = reducaoSt > 0 ? (double?)reducaoSt : null;
            icms.VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Opcional, 15, true);
            icms.PICMSST = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSST, ObOp.Opcional, this.CasasDecimais75, true);
            icms.VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSST, ObOp.Opcional, 15, true);
            icms.PCredSN = this.LerDouble(this.TipoCampo42, TpcnResources.pCredSN, ObOp.Opcional, this.CasasDecimais75, true);
            icms.VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredICMSSN, ObOp.Opcional, 15, true);
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPST, ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pFCPST, ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN900 = icms };
        }

        private void ProcessarDiferimentoIcms(int nProd, int lenPipesRegistro)
        {
            var imposto = new DFeNFe.ICMSUFDest
            {
                VBCUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCUFDest, ObOp.Obrigatorio, 1, 15),
                PFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pFCPUFDest, ObOp.Opcional, 1, 8),
                PICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pICMSUFDest, ObOp.Opcional, 1, 8),
                PICMSInter = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pICMSInter, ObOp.Obrigatorio, 1, 8),
                PICMSInterPart = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pICMSInterPart, ObOp.Opcional, 1, 8),
                VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPUFDest, ObOp.Opcional, 1, 15),
                VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFDest, ObOp.Obrigatorio, 1, 15),
                VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFRemet, ObOp.Opcional, 1, 15)
            };
            if (versaoNFe >= 4)
            {
                imposto.VBCFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCFCPUFDest, ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMSUFDest = imposto;
        }

        private void ProcessarIpi(int nProd)
        {
            var cnpjProdutor = this.LerString(TpcnResources.CNPJProd, ObOp.Opcional, 14, 14);
            var codigoSelo = this.LerString(TpcnResources.cSelo, ObOp.Opcional, 1, 60);
            var quantidadeSelo = this.LerInt32(TpcnResources.qSelo, ObOp.Opcional, 1, 12);
            var codigoEnquadramento = this.LerString(TpcnResources.cEnq, ObOp.Obrigatorio, 3, 3);
            detalhesOficiais[nProd].Imposto.IPI = new DFeNFe.IPI
            {
                CNPJProd = VazioParaNulo(cnpjProdutor),
                CSelo = VazioParaNulo(codigoSelo),
                QSelo = quantidadeSelo > 0 ? (int?)quantidadeSelo : null,
                CEnq = codigoEnquadramento
            };
        }

        private void ProcessarIpiTributado(int nProd, int lenPipesRegistro)
        {
            ObterIpi(nProd).IPITrib = new DFeNFe.IPITrib
            {
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                VIPI = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIPI, ObOp.Opcional, 15)
            };
        }

        private void ProcessarIpiNaoTributado(int nProd, int lenPipesRegistro)
        {
            ObterIpi(nProd).IPINT = new DFeNFe.IPINT
            {
                CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2)
            };
        }

        private void ProcessarIpiBaseAliquota(int nProd, int lenPipesRegistro)
        {
            var tributado = ObterIpiTributado(nProd);
            tributado.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
            tributado.PIPI = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pIPI, ObOp.Obrigatorio, 7);
        }

        private void ProcessarIpiQuantidade(int nProd, int lenPipesRegistro)
        {
            var tributado = ObterIpiTributado(nProd);
            tributado.QUnid = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qUnid, ObOp.Obrigatorio, 16);
            tributado.VUnid = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vUnid, ObOp.Obrigatorio, 15);
            tributado.VIPI = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vIPI, ObOp.Opcional, 15);
        }

        private void ProcessarImpostoImportacao(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.II = new DFeNFe.II
            {
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                VDespAdu = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDespAdu, ObOp.Obrigatorio, 15),
                VII = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vII, ObOp.Obrigatorio, 15),
                VIOF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIOF, ObOp.Obrigatorio, 15)
            };
        }

        private DFeNFe.IPI ObterIpi(int nProd)
        {
            if (detalhesOficiais[nProd].Imposto.IPI == null) detalhesOficiais[nProd].Imposto.IPI = new DFeNFe.IPI { CEnq = "999" };
            return detalhesOficiais[nProd].Imposto.IPI;
        }

        private DFeNFe.IPITrib ObterIpiTributado(int nProd)
        {
            var ipi = ObterIpi(nProd);
            if (ipi.IPITrib == null) ipi.IPITrib = new DFeNFe.IPITrib();
            return ipi.IPITrib;
        }

        private void ProcessarPisAliquota(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISAliq = new DFeNFe.PISAliq
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                    PPIS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pPIS, ObOp.Obrigatorio, 7),
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarPisQuantidade(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISQtde = new DFeNFe.PISQtde
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16),
                    VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15),
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarPisNaoTributado(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISNT = new DFeNFe.PISNT { CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2) }
            };
        }

        private void ProcessarPisOutros(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISOutr = new DFeNFe.PISOutr
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    VBC = 0,
                    PPIS = 0,
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Opcional, 15)
                }
            };
        }

        private void ProcessarPisAliquotaRetencao(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisOutros(nProd);
            pis.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
            pis.PPIS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pPIS, ObOp.Obrigatorio, 7);
            pis.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15);
        }

        private void ProcessarPisQuantidadeRetencao(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisOutros(nProd);
            pis.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 15);
            pis.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15);
            pis.VBC = null;
            pis.PPIS = null;
        }

        private void ProcessarPisSt(int nProd)
        {
            detalhesOficiais[nProd].Imposto.PISST = new DFeNFe.PISST
            {
                VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15)
            };
        }

        private void ProcessarPisStBase(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisSt(nProd);
            pis.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
            pis.PPIS = this.LerDouble(this.TipoCampo42, TpcnResources.pPIS, ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarPisStQuantidade(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisSt(nProd);
            pis.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16);
            pis.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15);
            pis.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15);
            pis.VBC = null;
            pis.PPIS = null;
            if (lenPipesRegistro >= 5)
            {
                var indicador = this.LerString(TpcnResources.indSomaPISST, ObOp.Opcional, 0, 1);
                int codigo;
                pis.IndSomaPISST = ObterEnumOpcional(
                    int.TryParse(indicador, out codigo) ? codigo : -1, (IndicaSomaPISST)(-1));
            }
        }

        private DFeNFe.PISOutr ObterPisOutros(int nProd)
        {
            if (detalhesOficiais[nProd].Imposto.PIS == null) detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS();
            if (detalhesOficiais[nProd].Imposto.PIS.PISOutr == null) detalhesOficiais[nProd].Imposto.PIS.PISOutr = new DFeNFe.PISOutr();
            return detalhesOficiais[nProd].Imposto.PIS.PISOutr;
        }

        private DFeNFe.PISST ObterPisSt(int nProd)
        {
            if (detalhesOficiais[nProd].Imposto.PISST == null) detalhesOficiais[nProd].Imposto.PISST = new DFeNFe.PISST();
            return detalhesOficiais[nProd].Imposto.PISST;
        }

#if INTEROP
        private SimNao LerSimNaoOpcional(TpcnResources recurso)
#else
        private SimNao? LerSimNaoOpcional(TpcnResources recurso)
#endif
        {
            var valor = this.LerString(recurso, ObOp.Opcional, 1, 1);
            int codigo;
            return ObterEnumOpcional(int.TryParse(valor, out codigo) ? codigo : -1, (SimNao)(-1));
        }

        private void ProcessarCofinsAliquota(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSAliq = new DFeNFe.COFINSAliq
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                    PCOFINS = this.LerDouble(this.TipoCampo42, TpcnResources.pCOFINS, ObOp.Obrigatorio, this.CasasDecimais75),
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsQuantidade(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSQtde = new DFeNFe.COFINSQtde
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16),
                    VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15),
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsNaoTributado(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSNT = new DFeNFe.COFINSNT { CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2) }
            };
        }

        private void ProcessarCofinsOutros(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSOutr = new DFeNFe.COFINSOutr
                {
                    CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 2, 2),
                    VBC = 0,
                    PCOFINS = 0,
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsAliquotaRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsOutros(nProd);
            cofins.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
            cofins.PCOFINS = this.LerDouble(this.TipoCampo42, TpcnResources.pCOFINS, ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarCofinsQuantidadeRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsOutros(nProd);
            cofins.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16);
            cofins.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15);
            cofins.VBC = null;
            cofins.PCOFINS = null;
        }

        private void ProcessarIssqn(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINSST = new DFeNFe.COFINSST
            {
                VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15)
            };
        }

        private void ProcessarIssqnValores(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsSt(nProd);
            cofins.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
            cofins.PCOFINS = this.LerDouble(this.TipoCampo42, TpcnResources.pCOFINS, ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarIssqnRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsSt(nProd);
            cofins.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCProd, ObOp.Obrigatorio, 16);
            cofins.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vAliqProd, ObOp.Obrigatorio, 15);
            cofins.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15);
            cofins.VBC = null;
            cofins.PCOFINS = null;
            if (lenPipesRegistro >= 5)
            {
                var indicador = this.LerString(TpcnResources.indSomaCOFINSST, ObOp.Opcional, 0, 1);
                int codigo;
                cofins.IndSomaCOFINSST = ObterEnumOpcional(
                    int.TryParse(indicador, out codigo) ? codigo : -1, (IndicaSomaCOFINSST)(-1));
            }
        }

        private DFeNFe.COFINSOutr ObterCofinsOutros(int nProd)
        {
            if (detalhesOficiais[nProd].Imposto.COFINS == null) detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS();
            if (detalhesOficiais[nProd].Imposto.COFINS.COFINSOutr == null) detalhesOficiais[nProd].Imposto.COFINS.COFINSOutr = new DFeNFe.COFINSOutr();
            return detalhesOficiais[nProd].Imposto.COFINS.COFINSOutr;
        }

        private DFeNFe.COFINSST ObterCofinsSt(int nProd)
        {
            if (detalhesOficiais[nProd].Imposto.COFINSST == null) detalhesOficiais[nProd].Imposto.COFINSST = new DFeNFe.COFINSST();
            return detalhesOficiais[nProd].Imposto.COFINSST;
        }

        private void ProcessarTotalNfe(int nProd, int lenPipesRegistro)
        {
            var issqn = new DFeNFe.ISSQN
            {
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15),
                VAliq = this.LerDouble(this.TipoCampo42, TpcnResources.vAliq, ObOp.Obrigatorio, this.CasasDecimais75),
                VISSQN = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vISSQN, ObOp.Obrigatorio, 15),
                CMunFG = this.LerInt32(TpcnResources.cMunFG, ObOp.Obrigatorio, 7, 7)
            };
            var listaServico = this.LerString(TpcnResources.cListServ, ObOp.Obrigatorio, versaoNFe >= 3 ? 5 : 3, versaoNFe >= 3 ? 5 : 4);
            issqn.CListServ = (ListaServicoISSQN)int.Parse(listaServico.Replace(".", string.Empty), CultureInfo.InvariantCulture);
            if (versaoNFe >= 3)
            {
                issqn.VDeducao = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDeducao, ObOp.Opcional, 15);
                issqn.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vOutro, ObOp.Opcional, 15);
                issqn.VDescIncond = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDescIncond, ObOp.Opcional, 15);
                issqn.VDescCond = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDescCond, ObOp.Opcional, 15);
                issqn.VISSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vISSRet, ObOp.Opcional, 15);
                issqn.IndISS = (IndicadorExigibilidadeISSQN)this.LerInt32(TpcnResources.indISS, ObOp.Obrigatorio, 1, 1);
                issqn.CServico = VazioParaNulo(this.LerString(TpcnResources.cServico, ObOp.Opcional, 1, 20));
                issqn.CMun = this.LerInt32(TpcnResources.cMun, ObOp.Opcional, 7, 7);
                issqn.CPais = this.LerInt32(TpcnResources.cPais, ObOp.Opcional, 4, 4);
                issqn.NProcesso = VazioParaNulo(this.LerString(TpcnResources.nProcesso, ObOp.Opcional, 1, 30));
                issqn.IndIncentivo = this.LerInt32(TpcnResources.indIncentivo, ObOp.Opcional, 1, 1) == 1 ? SimNao12.Sim : SimNao12.Nao;
            }
            else this.LerString(TpcnResources.cSitTrib, ObOp.Obrigatorio, 1, 1);
            detalhesOficiais[nProd].Imposto.ISSQN = issqn;
        }

        private void ProcessarTotalIbsCbs(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].ImpostoDevol = new DFeNFe.ImpostoDevol
            {
                PDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pDevol, ObOp.Opcional, 5),
                IPI = new DFeNFe.IPIDevol { VIPIDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIPIDevol, ObOp.Opcional, 5) }
            };
        }

        private void ProcessarTotalIbsCbsDetalhe(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.IS = new DFeNFe.IS
            {
                CSTIS = this.LerString(TpcnResources.CSTIS, ObOp.Obrigatorio, 1, 3),
                CClassTribIS = this.LerString(TpcnResources.cClassTribIS, ObOp.Obrigatorio, 1, 6),
                VBCIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCIS, ObOp.Opcional, 1, 15),
                PIS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pIS, ObOp.Opcional, 1, 7),
                AdRemIS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemIS, ObOp.Opcional, 1, 7),
                UTrib = this.LerString(TpcnResources.uTrib, ObOp.Opcional, 1, 6),
                QTrib = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qTrib, ObOp.Opcional, 1, 15),
                VIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIS, ObOp.Obrigatorio, 1, 15)
            };
        }

        private DFeNFe.IBSCBS ObterIBSCBS(int nProd)
        {
            var imposto = detalhesOficiais[nProd].Imposto;
            if (imposto.IBSCBS == null) imposto.IBSCBS = new DFeNFe.IBSCBS();
            return imposto.IBSCBS;
        }

        private DFeNFe.GIBSCBS ObterGIBSCBS(int nProd)
        {
            var imposto = ObterIBSCBS(nProd);
            if (imposto.GIBSCBS == null) imposto.GIBSCBS = new DFeNFe.GIBSCBS();
            return imposto.GIBSCBS;
        }

        private DFeNFe.GIBSCBSMono ObterGIBSCBSMono(int nProd)
        {
            var imposto = ObterIBSCBS(nProd);
            if (imposto.GIBSCBSMono == null) imposto.GIBSCBSMono = new DFeNFe.GIBSCBSMono();
            return imposto.GIBSCBSMono;
        }

        private DFeNFe.GCredPresOper ObterGCredPresOper(int nProd)
        {
            var imposto = ObterIBSCBS(nProd);
            if (imposto.GCredPresOper == null) imposto.GCredPresOper = new DFeNFe.GCredPresOper();
            return imposto.GCredPresOper;
        }

        private void ProcessarTotalIbsCbsRegular(int nProd, int lenPipesRegistro)
        {
            //layout = "UB12|CST|cClassTrib|"
            var imposto = ObterIBSCBS(nProd);
            imposto.CST = this.LerString(TpcnResources.CST, ObOp.Obrigatorio, 1, 3);
            imposto.CClassTrib = this.LerString(TpcnResources.cClassTrib, ObOp.Obrigatorio, 1, 6);
        }

        private void ProcessarTotalIbsCbsDiferimento(int nProd, int lenPipesRegistro)
        {
            //layout = "UB15|vBC|vIBS|"
            var imposto = ObterGIBSCBS(nProd);
            imposto.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 1, 15);
            imposto.VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBS, ObOp.Obrigatorio, 15, true);
        }

        private void ProcessarTotalIbsCbsDevolucao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB17|pIBSUF|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSUF|"
            ObterGIBSCBS(nProd).GIBSUF = new DFeNFe.GIBSUF
            {
                PIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pIBSUF, ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDif, ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedAliq, ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfet, ObOp.Opcional, 1, 7)
                },
                VIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSUF, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsCreditoPresumido(int nProd, int lenPipesRegistro)
        {
            //layout = "UB36|pIBSMun|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSMun|"
            ObterGIBSCBS(nProd).GIBSMun = new DFeNFe.GIBSMun
            {
                PIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pIBSMun, ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDif, ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedAliq, ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfet, ObOp.Opcional, 1, 7)
                },
                VIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vIBSMun, ObOp.Obrigatorio, 1, 7)
            };
        }

        private void ProcessarTotalIbsCbsReducao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB55|pCBS|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vCBS|"
            ObterGIBSCBS(nProd).GCBS = new DFeNFe.GCBS
            {
                PCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pCBS, ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDif, ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pRedAliq, ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfet, ObOp.Opcional, 1, 7)
                },
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.vCBS, ObOp.Obrigatorio, 1, 7)
            };
        }

        private DFeNFe.GDevTrib CriarGrupoDevolucaoTributos()
        {
            var percentualDevolucao = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDevTrib, ObOp.Opcional, 7, true);
            var valorDevolvido = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDevTrib, ObOp.Opcional, 15, true);
            if (percentualDevolucao == -9.99)
            {
                return null;
            }

            return new DFeNFe.GDevTrib
            {
                PDevTrib = percentualDevolucao == -9.99 ? 0 : percentualDevolucao,
                VDevTrib = valorDevolvido == -9.99 ? 0 : valorDevolvido
            };
        }

        private void ProcessarGrupoAreasIncentivadasCbs(int nProd, int lenPipesRegistro)
        {
            //layout = "UB66a|tpALCZFMCBS|nProcSuframa|pAliqEfetRegCBS|vTribRegCBS|"
            var gCBS = ObterGIBSCBS(nProd).GCBS;
            if (gCBS == null)
            {
                throw new Exception("Segmento UB66a informado sem o segmento UB55.");
            }

            gCBS.GALCZFMCBS = new DFeNFe.GALCZFMCBS
            {
                TpALCZFMCBS = (TipoAplicacaoAliquotaZeroCBS)this.LerInt32(TpcnResources.tpALCZFMCBS, ObOp.Obrigatorio, 1, 1),
                NProcSuframa = VazioParaNulo(this.LerString(TpcnResources.nProcSuframa, ObOp.Opcional, 8, 12)),
                PAliqEfetRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfetRegCBS, ObOp.Obrigatorio, 1, 7),
                VTribRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribRegCBS, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsRegularCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB68|CSTReg|cClassTribReg|pAliqEfetRegIBSUF|vTribRegIBSUF|pAliqEfetRegIBSMun|vTribRegIBSMun|pAliqEfetRegCBS|vTribRegCBS|"
            ObterGIBSCBS(nProd).GTribRegular = new DFeNFe.GTribRegular
            {
                CSTReg = this.LerString(TpcnResources.CSTReg, ObOp.Obrigatorio, 1, 3),
                CClassTribReg = this.LerString(TpcnResources.cClassTribReg, ObOp.Obrigatorio, 1, 6),
                PAliqEfetRegIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfetRegIBSUF, ObOp.Obrigatorio, 1, 7),
                VTribRegIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribRegIBSUF, ObOp.Obrigatorio, 1, 15),
                PAliqEfetRegIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfetRegIBSMun, ObOp.Obrigatorio, 1, 7),
                VTribRegIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribRegIBSMun, ObOp.Obrigatorio, 1, 15),
                PAliqEfetRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqEfetRegCBS, ObOp.Obrigatorio, 1, 7),
                VTribRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribRegCBS, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsDiferimentoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = UB82|pAliqIBSUF|vTribIBSUF|pAliqIBSMun|vTribIBSMun|pAliqCBS|vTribCBS|
            ObterGIBSCBS(nProd).GTribCompraGov = new DFeNFe.GTribCompraGov
            {
                PAliqIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqIBSUF, ObOp.Obrigatorio, 1, 7),
                VTribIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribIBSUF, ObOp.Obrigatorio, 1, 15),
                PAliqIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqIBSMun, ObOp.Obrigatorio, 1, 7),
                VTribIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribIBSMun, ObOp.Obrigatorio, 1, 15),
                PAliqCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pAliqCBS, ObOp.Obrigatorio, 1, 7),
                VTribCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTribCBS, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsDevolucaoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB84|vTotIBSMonoItem|vTotCBSMonoItem|"
            var monofasico = ObterGIBSCBSMono(nProd);
            monofasico.VTotIBSMonoItem = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTotIBSMonoItem, ObOp.Opcional, 1, 15);
            monofasico.VTotCBSMonoItem = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTotCBSMonoItem, ObOp.Opcional, 1, 15);
        }

        private void ProcessarTotalIbsCbsCreditoPresumidoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB85|qBCMono|adRemIBS|adRemCBS|vIBSMono|vCBSMono|"
            ObterGIBSCBSMono(nProd).GMonoPadrao = new DFeNFe.GMonoPadrao
            {
                QBCMono = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMono, ObOp.Opcional, 1, 15),
                AdRemIBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemIBS, ObOp.Opcional, 1, 7),
                AdRemCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemCBS, ObOp.Opcional, 1, 7),
                VIBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMono, ObOp.Opcional, 1, 15),
                VCBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMono, ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsTribRegular(int nProd, int lenPipesRegistro)
        {
            //layout = "UB91|qBCMonoReten|adRemIBSReten|vIBSMonoReten|adRemCBSReten|vCBSMonoReten|"
            ObterGIBSCBSMono(nProd).GMonoReten = new DFeNFe.GMonoReten
            {
                QBCMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMonoReten, ObOp.Opcional, 1, 15),
                AdRemIBSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemIBSReten, ObOp.Opcional, 1, 7),
                VIBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMonoReten, ObOp.Opcional, 1, 15),
                AdRemCBSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemCBSReten, ObOp.Opcional, 1, 7),
                VCBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMonoReten, ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsTribRegularCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB95|qBCMonoRet|adRemIBSRet|vIBSMonoRet|adRemCBSRet|vCBSMonoRet|"
            ObterGIBSCBSMono(nProd).GMonoRet = new DFeNFe.GMonoRet
            {
                QBCMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.qBCMonoRet, ObOp.Opcional, 1, 15),
                AdRemIBSRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemIBSRet, ObOp.Opcional, 1, 7),
                VIBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMonoRet, ObOp.Opcional, 1, 15),
                AdRemCBSRet = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.adRemCBSRet, ObOp.Opcional, 1, 7),
                VCBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMonoRet, ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotaisRetencao(int nProd, int lenPipesRegistro)
        {
            var observacao = ObterObservacaoItem(nProd);
            if (observacao.ObsCont == null) observacao.ObsCont = new List<DFeNFe.ObsCont>();
            observacao.ObsCont.Add(new DFeNFe.ObsCont
            {
                XCampo = this.LerString(TpcnResources.xCampo, ObOp.Obrigatorio, 1, 20),
                XTexto = this.LerString(TpcnResources.xTexto, ObOp.Obrigatorio, 1, 60)
            });
        }

        private void ProcessarTotaisRetencaoCbs(int nProd, int lenPipesRegistro)
        {
            var observacao = ObterObservacaoItem(nProd);
            if (observacao.ObsFisco == null) observacao.ObsFisco = new List<DFeNFe.ObsFisco>();
            observacao.ObsFisco.Add(new DFeNFe.ObsFisco
            {
                XCampo = this.LerString(TpcnResources.xCampo, ObOp.Obrigatorio, 1, 20),
                XTexto = this.LerString(TpcnResources.xTexto, ObOp.Obrigatorio, 1, 60)
            });
        }

        private void ProcessarTotaisMonofasico(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].VItem = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vItem, ObOp.Opcional, 15);
        }

        private void ProcessarTotaisMonofasicoRetido(int nProd, int lenPipesRegistro)
        {
            var chaveAcesso = this.LerString(TpcnResources.chaveAcesso, ObOp.Obrigatorio, 1, 44);
            var numeroItem = this.LerInt32(TpcnResources.NItem, ObOp.Opcional, 1, 3);
            detalhesOficiais[nProd].DFeReferenciado = new DFeNFe.DFeReferenciado
            {
                ChaveAcesso = chaveAcesso,
                NItem = numeroItem > 0 ? numeroItem.ToString() : null
            };
        }

        private DFeNFe.ObsItem ObterObservacaoItem(int nProd)
        {
            if (detalhesOficiais[nProd].ObsItem == null) detalhesOficiais[nProd].ObsItem = new DFeNFe.ObsItem();
            return detalhesOficiais[nProd].ObsItem;
        }

        private void ProcessarMonoDiferido(int nProd, int lenPipesRegistro)
        {
            //layout = "UB100|pDifIBS|vIBSMonoDif|pDifCBS|vCBSMonoDif|"
            ObterGIBSCBSMono(nProd).GMonoDif = new DFeNFe.GMonoDif
            {
                PDifIBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDifIBS, ObOp.Opcional, 1, 7),
                VIBSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMonoDif, ObOp.Opcional, 1, 15),
                PDifCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, TpcnResources.pDifCBS, ObOp.Opcional, 1, 7),
                VCBSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMonoDif, ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTransferenciaCredito(int nProd, int lenPipesRegistro)
        {
            //layout = UB106|vIBS|vCBS|
            ObterIBSCBS(nProd).GTransfCred = new DFeNFe.GTransfCred
            {
                VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBS, ObOp.Obrigatorio, 1, 15),
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBS, ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarAjusteCompetencia(int nProd, int lenPipesRegistro)
        {
            var competencia = this.LerString(TpcnResources.competApur, ObOp.Obrigatorio, 7, 7);
            ObterIBSCBS(nProd).GAjusteCompet = new DFeNFe.GAjusteCompet
            {
                CompetApur = DateTime.ParseExact(competencia, "yyyy-MM", CultureInfo.InvariantCulture),
                VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBS, ObOp.Obrigatorio, 1, 13),
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBS, ObOp.Obrigatorio, 1, 13)
            };
        }

        private void ProcessarIndicadorDoacao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB14a|indDoacao|;
            var indicador = this.LerString(TpcnResources.indDoacao, ObOp.Opcional, 1, 1);
            ObterIBSCBS(nProd).IndDoacao = int.TryParse(indicador, out var valor) ? valor : 0;
        }

        private void ProcessarEstornoCredito(int nProd, int lenPipesRegistro)
        {
            var valorCbs = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSEstCred, ObOp.None, 13, true);
            ObterIBSCBS(nProd).GEstornoCred = new DFeNFe.GEstornoCred
            {
                VCBSEstCred = valorCbs,
                VIBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSEstCred, ObOp.None, 13, true)
            };
        }

        private void ProcessarCreditoPresumidoOperacao(int nProd, int lenPipesRegistro)
        {
            var credito = ObterGCredPresOper(nProd);
            credito.VBCCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCCredPres, ObOp.Obrigatorio, 1, 13);
            credito.CCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.cCredPres, ObOp.Obrigatorio, 1, 2).ToString("F2", CultureInfo.InvariantCulture);
        }

        private void ProcessarCreditoPresumidoIbs(int nProd, int lenPipesRegistro)
        {
            ObterGCredPresOper(nProd).GIBSCredPres = new DFeNFe.GIBSCredPres
            {
                PCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pCredPres, ObOp.Opcional, 1, 4),
                VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPres, ObOp.Opcional, 1, 13),
                VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresCondSus, ObOp.Opcional, 1, 13)
            };
        }

        private void ProcessarCreditoPresumidoCbs(int nProd, int lenPipesRegistro)
        {
            ObterGCredPresOper(nProd).GCBSCredPres = new DFeNFe.GCBSCredPres
            {
                PCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.pCredPres, ObOp.Opcional, 1, 4),
                VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPres, ObOp.Opcional, 1, 13),
                VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresCondSus, ObOp.Opcional, 1, 13)
            };
        }

        private void ProcessarCreditoPresumidoZfm(int nProd, int lenPipesRegistro)
        {
            //layout = UB109|tpCredPresIBSZFM|vCredPresIBSZFM|
            var competencia = this.LerString(TpcnResources.competApur, ObOp.Obrigatorio, 7, 7);
            var tipoCredito = this.LerString(TpcnResources.tpCredPresIBSZFM, ObOp.Obrigatorio, 1, 1);
            ObterIBSCBS(nProd).GCredPresIBSZFM = new DFeNFe.GCredPresIBSZFM
            {
                CompetApur = DateTime.ParseExact(competencia, "yyyy-MM", CultureInfo.InvariantCulture),
                TpCredPresIBSZFM = (TipoCreditoPresumidoIBSZFM)int.Parse(tipoCredito),
                VCredPresIBSZFM = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresIBSZFM, ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarObservacaoContribuinte(int nProd, int lenPipesRegistro)
        {
                    this.ProcessarTotaisRetencao(nProd, lenPipesRegistro);
        }

        private void ProcessarObservacaoFisco(int nProd, int lenPipesRegistro)
        {
                    this.ProcessarTotaisRetencaoCbs(nProd, lenPipesRegistro);
        }

        private void ProcessarValorItem(int nProd, int lenPipesRegistro)
        {
                    this.ProcessarTotaisMonofasico(nProd, lenPipesRegistro);
        }

        private void ProcessarDfeReferenciado(int nProd, int lenPipesRegistro)
        {
                    this.ProcessarTotaisMonofasicoRetido(nProd, lenPipesRegistro);
        }

        private void ProcessarTotaisIcms(int nProd, int lenPipesRegistro)
        {
                    // Grupo da TAG <total><ICMSTot>.
                    #region <total><ICMSTot>
                    var icms = totalOficial.ICMSTot;
                    icms.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Obrigatorio, 15);
                    icms.VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMS, ObOp.Obrigatorio, 15);
                    icms.VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCST, ObOp.Obrigatorio, 15);
                    icms.VST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vST, ObOp.Obrigatorio, 15);
                    icms.VProd = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vProd, ObOp.Obrigatorio, 15);
                    icms.VFrete = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFrete, ObOp.Obrigatorio, 15);
                    icms.VSeg = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vSeg, ObOp.Obrigatorio, 15);
                    icms.VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDesc, ObOp.Obrigatorio, 15);
                    icms.VII = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vII, ObOp.Obrigatorio, 15);
                    icms.VIPI = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIPI, ObOp.Obrigatorio, 15);
                    icms.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Obrigatorio, 15);
                    icms.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Obrigatorio, 15);
                    icms.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vOutro, ObOp.Obrigatorio, 15);
                    icms.VNF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vNF, ObOp.Obrigatorio, 15);
                    icms.VTotTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTotTrib, ObOp.Opcional, 15);



                    icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSDeson, ObOp.Opcional, 15);

                    if (versaoNFe >= 3 && lenPipesRegistro > 17)
                    {
                        icms.VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFDest, ObOp.Opcional, 15);
                        icms.VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPUFDest, ObOp.Opcional, 15);
                        icms.VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFRemet, ObOp.Opcional, 15);

                        if (versaoNFe >= 4)
                        {
                            icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCP, ObOp.Opcional, 15);
                            icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPST, ObOp.Opcional, 15);
                            icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPSTRet, ObOp.Opcional, 15);
                            icms.VIPIDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIPIDevol, ObOp.Opcional, 15);
                            icms.QBCMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.qBCMono, ObOp.Opcional, 15);
                            icms.VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMono, ObOp.Opcional, 15);
                            icms.QBCMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.qBCMonoReten, ObOp.Opcional, 15);
                            icms.VICMSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoReten, ObOp.Opcional, 15);
                            icms.QBCMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.qBCMonoRet, ObOp.Opcional, 15);
                            icms.VICMSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSMonoRet, ObOp.Opcional, 15);
                        }
                    }
                    #endregion
        }

        private void ProcessarTotaisIcmsSt(int nProd, int lenPipesRegistro)
        {
                    //layout = prefix + this.FSegmento + "|vICMSUFDest|vICMSUFRemet|vFCPUFDest";
                    totalOficial.ICMSTot.VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFDest, ObOp.Opcional, 15);
                    totalOficial.ICMSTot.VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSUFRemet, ObOp.Opcional, 15);
                    totalOficial.ICMSTot.VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vFCPUFDest, ObOp.Opcional, 15);
        }

        private void ProcessarTotaisFcp(int nProd, int lenPipesRegistro)
        {
                    //layout = (NFe.infNFe.Versao >= 3 ?
                    //            "§W17|VServ|VBC|VISS|VPIS|VCOFINS|dCompet|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|cRegTrib" :
                    //            "§W17|VServ|VBC|VISS|VPIS|VCOFINS");
                    // Grupo da TAG <total><ISSQNtot>.
                    #region <total><ISSQNtot>
                    var issqn = totalOficial.ISSQNtot;
                    issqn.VServ = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vServ, ObOp.Opcional, 15);
                    issqn.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBC, ObOp.Opcional, 15);
                    issqn.VISS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vISS, ObOp.Opcional, 15);
                    issqn.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPIS, ObOp.Opcional, 15);
                    issqn.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCOFINS, ObOp.Opcional, 15);

                    if ((double)versaoNFe >= 3.10)
                    {
                        issqn.DCompet = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dCompet, ObOp.Opcional, 10, 10, true, false);
                        issqn.VDeducao = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDeducao, ObOp.Opcional, 15);
                        issqn.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vOutro, ObOp.Opcional, 15);
                        issqn.VDescIncond = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDescIncond, ObOp.Opcional, 15);
                        issqn.VDescCond = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDescCond, ObOp.Opcional, 15);
                        issqn.VISSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vISSRet, ObOp.Opcional, 15);
                        issqn.CRegTrib = (CodigoRegimeEspecialTributacao)this.LerInt32(TpcnResources.cRegTrib, ObOp.Opcional, 1, 1);
                    }
                    #endregion
        }

        private void ProcessarTotaisIpi(int nProd, int lenPipesRegistro)
        {
                    //layout = "§W23|VRetPIS|VRetCOFINS|VRetCSLL|VBCIRRF|VIRRF|VBCRetPrev|VRetPrev"; //ok
                    // Grupo da TAG <total><retTrib>.
                    #region <total><retTrib>
                    var retencoes = totalOficial.RetTrib;
                    retencoes.VRetPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vRetPIS, ObOp.Opcional, 15);
                    retencoes.VRetCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vRetCOFINS, ObOp.Opcional, 15);
                    retencoes.VRetCSLL = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vRetCSLL, ObOp.Opcional, 15);
                    retencoes.VBCIRRF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCIRRF, ObOp.Opcional, 15);
                    retencoes.VIRRF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIRRF, ObOp.Opcional, 15);
                    retencoes.VBCRetPrev = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCRetPrev, ObOp.Opcional, 15);
                    retencoes.VRetPrev = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vRetPrev, ObOp.Opcional, 15);
                    #endregion
        }

        private void ProcessarTotaisPis(int nProd, int lenPipesRegistro)
        {
                    //layout = "W31|vIS|"

                    totalOficial.ISTot.VIS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIS, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisCofins(int nProd, int lenPipesRegistro)
        {
                    //layout = "W34|vBCIBSCBS|"

                    totalOficial.IBSCBSTot.VBCIBSCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCIBSCBS, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisIssqn(int nProd, int lenPipesRegistro)
        {
                    //layout = "W36|vIBS|vCredPres|vCredPresCondSus|"

                    totalOficial.IBSCBSTot.GIBS.VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBS, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPres, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresCondSus, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisRetencoes(int nProd, int lenPipesRegistro)
        {
                    //layout = "W37|vDif|vDevTrib|vIBSUF|"

                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDevTrib, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSUF, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisTributos(int nProd, int lenPipesRegistro)
        {
                    //layout = "W42|vDif|vDevTrib|vIBSMun|"

                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDevTrib, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMun, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisIcmsUfDest(int nProd, int lenPipesRegistro)
        {
                    //layout = "W50|vDif|vDevTrib|vCBS|vCredPres|vCredPresCondSus|"

                    totalOficial.IBSCBSTot.GCBS.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDif, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDevTrib, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBS, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPres, ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCredPresCondSus, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisFcpUfDest(int nProd, int lenPipesRegistro)
        {
                    //layout = "W57|vIBSMono|vCBSMono|vIBSMonoReten|vCBSMonoReten|vIBSMonoRet|vCBSMonoRet|"

                    var monofasia = totalOficial.IBSCBSTot.GMono;
                    monofasia.VIBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMono, ObOp.Obrigatorio, 15);
                    monofasia.VCBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMono, ObOp.Obrigatorio, 15);
                    monofasia.VIBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMonoReten, ObOp.Obrigatorio, 15);
                    monofasia.VCBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMonoReten, ObOp.Obrigatorio, 15);
                    monofasia.VIBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSMonoRet, ObOp.Obrigatorio, 15);
                    monofasia.VCBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSMonoRet, ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisFcpUfRemet(int nProd, int lenPipesRegistro)
        {
                    totalOficial.IBSCBSTot.GEstornoCred = new DFeNFe.GEstornoCred
                    {
                        VIBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vIBSEstCred, ObOp.Opcional, 1, 13),
                        VCBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vCBSEstCred, ObOp.Opcional, 1, 13)
                    };
        }

        private void ProcessarTotaisIbsCbs(int nProd, int lenPipesRegistro)
        {
                    //layout = "W60|vNFTot|"

                    totalOficial.VNFTot = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vNFTot, ObOp.Opcional, 15);
        }

        private void ProcessarTransporte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X|modFrete"; //ok
                    // Grupo da TAG <transp>.
                    transporteOficial.ModFrete = (ModalidadeFrete)this.LerInt32(TpcnResources.modFrete, ObOp.Obrigatorio, 1, 1);
        }

        private void ProcessarTransportador(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X03|xNome|IE|xEnder|UF|xMun"; //ok - alterado em 18/3/15
                    //layout = "§X03|xNome|IE|xEnder|xMun|UF"; //ok
                    // Grupo da TAG <transp><transporta>.
                    #region <transp><TRansportadora>
                    var transportador = ObterTransportador();
                    transportador.XNome = this.LerString(TpcnResources.xNome, ObOp.Opcional, 1, 60);
                    transportador.IE = this.LerString(TpcnResources.IE, ObOp.Opcional, 0, 14);
                    transportador.XEnder = this.LerString(TpcnResources.xEnder, ObOp.Opcional, 1, 60);
                    transportador.XMun = this.LerString(TpcnResources.xMun, ObOp.Opcional, 1, 60);
#if INTEROP
                    transportador.UF = LerUF(TpcnResources.UF, ObOp.Opcional) ?? UFBrasil.NaoDefinido;
#else
                    transportador.UF = LerUF(TpcnResources.UF, ObOp.Opcional);
#endif
                    #endregion
        }

        private void ProcessarVeiculoTransporte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X04|CNPJ"; //ok

                    ObterTransportador().CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Opcional, 14, 14);
        }

        private void ProcessarReboque(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X05|CPF"; //ok

                    ObterTransportador().CPF = this.LerString(TpcnResources.CPF, ObOp.Opcional, 11, 11);
        }

        private void ProcessarVolume(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X11|VServ|VBCRet|PICMSRet|VICMSRet|CFOP|CMunFG"; //ok
                    // Grupo da TAG <transp><retTransp>.
                    #region <transp><retTransp>
                    transporteOficial.RetTransp = new DFeNFe.RetTransp
                    {
                        VServ = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vServ, ObOp.Obrigatorio, 15),
                        VBCRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vBCRet, ObOp.Obrigatorio, 15),
                        PICMSRet = this.LerDouble(this.TipoCampo42, TpcnResources.pICMSRet, ObOp.Obrigatorio, this.CasasDecimais75),
                        VICMSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vICMSRet, ObOp.Obrigatorio, 15),
                        CFOP = this.LerString(TpcnResources.CFOP, ObOp.Obrigatorio, 4, 4),
                        CMunFG = this.LerInt32(TpcnResources.cMunFG, ObOp.Obrigatorio, 7, 7)
                    };
                    #endregion
        }

        private void ProcessarLacre(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X18|Placa|UF|RNTC"; //ok
                    // Grupo da TAG <transp><veicTransp>.
                    #region <transp><veicTransp>
                    transporteOficial.VeicTransp = new DFeNFe.VeicTransp
                    {
                        Placa = this.LerString(TpcnResources.placa, ObOp.Obrigatorio, 1, 8),
#if INTEROP
                        UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio) ?? UFBrasil.NaoDefinido,
#else
                        UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio),
#endif
                        RNTC = VazioParaNulo(this.LerString(TpcnResources.RNTC, ObOp.Opcional, 1, 20))
                    };
                    #endregion
        }

        private void ProcessarFatura(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X22|Placa|UF|RNTC" + (NFe.infNFe.Versao >= 3 ? "|vagao|balsa" : "");
                    // Grupo da TAG <transp><reboque>.
                    #region <transp><reboque>
                    if (transporteOficial.Reboque == null)
                    {
                        transporteOficial.Reboque = new List<DFeNFe.Reboque>();
                    }
                    transporteOficial.Reboque.Add(new DFeNFe.Reboque
                    {
                        Placa = this.LerString(TpcnResources.placa, ObOp.Obrigatorio, 1, 8),
#if INTEROP
                        UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio) ?? UFBrasil.NaoDefinido,
#else
                        UF = LerUF(TpcnResources.UF, ObOp.Obrigatorio),
#endif
                        RNTC = VazioParaNulo(this.LerString(TpcnResources.RNTC, ObOp.Opcional, 1, 20))
                    });
                    if (versaoNFe >= 3)
                    {
                        transporteOficial.Vagao = VazioParaNulo(this.LerString(TpcnResources.vagao, ObOp.Opcional, 1, 20));
                        transporteOficial.Balsa = VazioParaNulo(this.LerString(TpcnResources.balsa, ObOp.Opcional, 1, 20));
                    }
                    #endregion
        }

        private void ProcessarDuplicata(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X26|QVol|Esp|Marca|NVol|PesoL|PesoB"; //ok
                    // Grupo da TAG <transp><vol>.
                    #region <transp><vol>
                    transporteOficial.Vol.Add(new DFeNFe.Vol
                    {
                        QVol = this.LerInt32(TpcnResources.qVol, ObOp.Obrigatorio, 1, 15),
                        Esp = VazioParaNulo(this.LerString(TpcnResources.esp, ObOp.Opcional, 1, 60)),
                        Marca = VazioParaNulo(this.LerString(TpcnResources.marca, ObOp.Opcional, 1, 60)),
                        NVol = VazioParaNulo(this.LerString(TpcnResources.nVol, ObOp.Opcional, 1, 60)),
                        PesoL = this.LerDouble(TpcnTipoCampo.tcDouble3, TpcnResources.pesoL, ObOp.Opcional, 15),
                        PesoB = this.LerDouble(TpcnTipoCampo.tcDouble3, TpcnResources.pesoB, ObOp.Opcional, 15)
                    });
                    #endregion
        }

        private void ProcessarPagamento(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X33|NLacre"; //ok
                    // Grupo da TAG <transp><vol><lacres>.
                    #region <transp><vol><lacres>
                    var volume = transporteOficial.Vol[transporteOficial.Vol.Count - 1];
                    if (volume.Lacres == null)
                    {
                        volume.Lacres = new List<DFeNFe.Lacres>();
                    }
                    volume.Lacres.Add(new DFeNFe.Lacres
                    {
                        NLacre = this.LerString(TpcnResources.nLacre, ObOp.Obrigatorio, 1, 60)
                    });
                    #endregion
        }

        private DFeNFe.Transporta ObterTransportador()
        {
            if (transporteOficial.Transporta == null)
            {
                transporteOficial.Transporta = new DFeNFe.Transporta();
            }
            return transporteOficial.Transporta;
        }

        private UFBrasil? LerUF(TpcnResources campo, ObOp obrigatoriedade)
        {
            var uf = this.LerString(campo, obrigatoriedade, 2, 2);
            return string.IsNullOrWhiteSpace(uf) ? (UFBrasil?)null : (UFBrasil)Enum.Parse(typeof(UFBrasil), uf);
        }

        private void ProcessarFormaPagamento(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Y02|NFat|VOrig|VDesc|VLiq"; //ok
                    // Grupo da TAG <cobr>.
                    #region <cobr>
                    var cobranca = CriarCobranca();
                    cobranca.Fat = new DFeNFe.Fat
                    {
                        NFat = this.LerString(TpcnResources.nFat, ObOp.Opcional, 1, 60),
                        VOrig = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vOrig, ObOp.Opcional, 15),
                        VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDesc, ObOp.None, 15, true),
                        VLiq = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vLiq, ObOp.Opcional, 15)
                    };
                    #endregion
        }

        private void ProcessarTroco(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Y07|NDup|DVenc|VDup"; //ok
                    // Grupo da TAG <cobr><dup>.
                    #region <cobr><dup>
                    var cobranca = CriarCobranca();
                    if (cobranca.Dup == null) cobranca.Dup = new List<DFeNFe.Dup>();
                    var duplicata = new DFeNFe.Dup();
                    if (DateTime.Today >= new DateTime(2018, 9, 3))
                        duplicata.NDup = this.LerString(TpcnResources.nDup, ObOp.Opcional, 1, 3);
                    else
                        duplicata.NDup = this.LerString(TpcnResources.nDup, ObOp.Opcional, 1, 60);
                    duplicata.DVenc = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dVenc, ObOp.Opcional, 10, 10, true, false);
                    duplicata.VDup = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDup, ObOp.Opcional, 15);
                    cobranca.Dup.Add(duplicata);
                    #endregion


                // NFC-e e NF-e.
        }

        private void ProcessarCartao(int nProd, int lenPipesRegistro)
        {
                    #region YA

                    //_LayoutTXT.Add("YA_9", prefix +  "YA|indPag|tPag|xPag|vPag|CNPJ|tBand|cAut|tpIntegra|");
                    //_LayoutTXT.Add("YA_14", prefix + "YA|indPag|tPag|xPag|vPag|dPag|CNPJPag|UFPag|CNPJ|tBand|cAut|tpIntegra|CNPJReceb|idTermPag|");


                    var pagamentoLegado = new NFeTxtPagamentoLido();
                    pagamentoLegado.indPag = (TpcnIndicadorPagamento)this.LerInt32(TpcnResources.indPag, ObOp.Opcional, 1, 1, true);
                    pagamentoLegado.tPag = (TpcnFormaPagamento)this.LerInt32(TpcnResources.tPag, ObOp.Obrigatorio, 2, 2);
                    pagamentoLegado.xPag = this.LerString(TpcnResources.xPag, ObOp.Opcional, 0, 60);
                    pagamentoLegado.vPag = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vPag, ObOp.Obrigatorio, 15);

                    if (lenPipesRegistro >= 14)
                    {
                        pagamentoLegado.dPag = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, TpcnResources.dPag, ObOp.Opcional, 10, 10, true, false);
                        pagamentoLegado.CNPJPag = this.LerString(TpcnResources.CNPJPag, ObOp.Opcional, 14, 14);
                        pagamentoLegado.UFPag = this.LerString(TpcnResources.UFPag, ObOp.Opcional, 1, 2);
                    }

                    pagamentoLegado.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Opcional, 14, 14);
                    pagamentoLegado.tBand = (TpcnBandeiraCartao)this.LerInt32(TpcnResources.tBand, ObOp.Opcional, 2, 2);
                    pagamentoLegado.cAut = this.LerString(TpcnResources.cAut, ObOp.Opcional, 1, 128);
                    pagamentoLegado.tpIntegra = this.LerInt32(TpcnResources.tpIntegra, ObOp.Opcional, 1, 1);

                    if (lenPipesRegistro >= 14)
                    {
                        pagamentoLegado.CNPJReceb = this.LerString(TpcnResources.CNPJReceb, ObOp.Opcional, 14, 14);
                        pagamentoLegado.idTermPag = this.LerString(TpcnResources.idTermPag, ObOp.Opcional, 0, 40);
                    }

                    var pagamentoOficial = this.nfeOficial.InfNFeField.Pag ?? (this.nfeOficial.InfNFeField.Pag = new DFeNFe.Pag());
                    pagamentoOficial.DetPag.Add(new DFeNFe.DetPag
                    {
                        IndPag = ObterEnumOpcional(
                            pagamentoLegado.indPag == TpcnIndicadorPagamento.ipNone ? -1 : (int)pagamentoLegado.indPag,
                            (IndicadorPagamento)(-1)),
                        TPag = (MeioPagamento)(int)pagamentoLegado.tPag,
                        XPag = pagamentoLegado.tPag == TpcnFormaPagamento.fpOutro ? VazioParaNulo(pagamentoLegado.xPag) : null,
                        VPag = pagamentoLegado.vPag, DPag = pagamentoLegado.dPag, CNPJPag = VazioParaNulo(pagamentoLegado.CNPJPag),
                        UFPag = string.IsNullOrWhiteSpace(pagamentoLegado.UFPag) ? default(UFBrasil) : (UFBrasil)Enum.Parse(typeof(UFBrasil), pagamentoLegado.UFPag, true),
                        Card = pagamentoLegado.tpIntegra == 0 ? null : new DFeNFe.Card { TpIntegra = (TipoIntegracaoPagamento)pagamentoLegado.tpIntegra, CNPJ = VazioParaNulo(pagamentoLegado.CNPJ), TBand = ObterEnumOpcional(pagamentoLegado.tBand == 0 ? -1 : (int)pagamentoLegado.tBand, (BandeiraOperadoraCartao)(-1)), CAut = VazioParaNulo(pagamentoLegado.cAut), CNPJReceb = VazioParaNulo(pagamentoLegado.CNPJReceb), IdTermPag = VazioParaNulo(pagamentoLegado.idTermPag) }
                    });

                    #endregion
        }

        private void ProcessarCnpjInstituicaoPagadora(int nProd, int lenPipesRegistro)
        {
                    this.nfeOficial.InfNFeField.Pag.VTroco = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vTroco, ObOp.Opcional, 15);
        }

        private void ProcessarTipoIntegracaoPagamento()
        {
            var tipoIntegracao = this.LerInt32(TpcnResources.tpIntegra, ObOp.Obrigatorio, 1, 1);
            var detalhe = this.nfeOficial.InfNFeField.Pag.DetPag[this.nfeOficial.InfNFeField.Pag.DetPag.Count - 1];
            if (detalhe.Card == null) detalhe.Card = new DFeNFe.Card();
            detalhe.Card.TpIntegra = (TipoIntegracaoPagamento)tipoIntegracao;
        }

        private DFeNFe.Cobr CriarCobranca()
        {
            if (this.nfeOficial.InfNFeField.Cobr == null) this.nfeOficial.InfNFeField.Cobr = new DFeNFe.Cobr();
            return this.nfeOficial.InfNFeField.Cobr;
        }
        private void ProcessarIntermediador(int nProd, int lenPipesRegistro)
        {
                    this.nfeOficial.InfNFeField.InfIntermed = new DFeNFe.InfIntermed
                    {
                        CNPJ = LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 1, 14, true),
                        IdCadIntTran = LerString(TpcnResources.idCadIntTran, ObOp.Obrigatorio, 1, 60, true)
                    };
        }

        private void ProcessarInformacoesAdicionais(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z|InfAdFisco|InfCpl"; //ok
                    // Grupo da TAG <infAdic>.
                    #region <InfAdic>
                    var informacoes = CriarInformacoesAdicionais();
                    informacoes.InfAdFisco += this.LerString(TpcnResources.infAdFisco, ObOp.Opcional, 1, 2000, false);
                    informacoes.InfCpl += this.LerString(TpcnResources.infCpl, ObOp.Opcional, 1, 5000, false);
                    #endregion
        }

        private void AdicionarObservacaoContribuinte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z04|XCampo|XTexto"; //ok
                    // Grupo da TAG <infAdic><obsCont>.
                    #region <infAdic><obsCont>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ObsCont == null) informacoes.ObsCont = new List<DFeNFe.ObsCont>();
                    informacoes.ObsCont.Add(new DFeNFe.ObsCont { XCampo = this.LerString(TpcnResources.xCampo, ObOp.Obrigatorio, 1, 20), XTexto = this.LerString(TpcnResources.xTexto, ObOp.Obrigatorio, 1, 60) });
                    #endregion
        }

        private void AdicionarObservacaoFisco(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z07|XCampo|XTexto"; //ok - ?
                    // Grupo da TAG <infAdic><obsFisco>.
                    #region <infAdic><obsFisco>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ObsFisco == null) informacoes.ObsFisco = new List<DFeNFe.ObsFisco>();
                    informacoes.ObsFisco.Add(new DFeNFe.ObsFisco { XCampo = this.LerString(TpcnResources.xCampo, ObOp.Obrigatorio, 1, 20), XTexto = this.LerString(TpcnResources.xTexto, ObOp.Obrigatorio, 1, 60) });
                    #endregion
        }

        private void AdicionarProcessoReferenciado(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z10|NProc|IndProc"; //ok
                    // Grupo da TAG <infAdic><procRef>.
                    #region <infAdic><procRef>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ProcRef == null) informacoes.ProcRef = new List<DFeNFe.ProcRef>();
                    var processo = new DFeNFe.ProcRef { NProc = this.LerString(TpcnResources.nProc, ObOp.Obrigatorio, 1, 60), IndProc = (IndicadorOrigemProcesso)this.LerInt32(TpcnResources.indProc, ObOp.Obrigatorio, 1, 1) };
                    if (lenPipesRegistro >= 4)
                    {
                        processo.TpAto = ObterEnumOpcional(
                            this.LerInt32(TpcnResources.tpAto, ObOp.Opcional, 2, 2),
                            (TipoAtoConcessorio)(-1));
                    }
                    informacoes.ProcRef.Add(processo);
                    #endregion
        }

        private void ProcessarExportacao()
        {
                    if (versaoNFe >= 3)
                    {
                        //layout = prefix + this.FSegmento + "|UFSaidaPais|xLocExporta|xLocDespacho"; //ok
                        // Grupo da TAG <exporta>.
                        this.nfeOficial.InfNFeField.Exporta = new DFeNFe.Exporta
                        {
                            UFSaidaPais = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(TpcnResources.UFSaidaPais, ObOp.Obrigatorio, 2, 2), true),
                            XLocExporta = this.LerString(TpcnResources.xLocExporta, ObOp.Obrigatorio, 1, 60),
                            XLocDespacho = VazioParaNulo(this.LerString(TpcnResources.xLocDespacho, ObOp.Opcional, 1, 60))
                        };
                    }
                    else
                    {
                        //layout = "§ZA|UFEmbarq|XLocEmbarq"; //ok
                        // Grupo da TAG <exporta>.
                        this.LerString(TpcnResources.UFEmbarq, ObOp.Obrigatorio, 2, 2);
                        this.LerString(TpcnResources.xLocEmbarq, ObOp.Obrigatorio, 1, 60);
                    }
        }

        private void ProcessarCompra()
        {
                    //layout = "§ZB|XNEmp|XPed|XCont"; //ok
                    // Grupo da TAG <compra>.
                    this.nfeOficial.InfNFeField.Compra = new DFeNFe.Compra
                    {
                        XNEmp = VazioParaNulo(this.LerString(TpcnResources.xNEmp, ObOp.Opcional, 1, 17)),
                        XPed = VazioParaNulo(this.LerString(TpcnResources.xPed, ObOp.Opcional, 1, 60)),
                        XCont = VazioParaNulo(this.LerString(TpcnResources.xCont, ObOp.Opcional, 1, 60))
                    };
        }

        private void AdicionarFornecimentoDiario()
        {
                    //layout = "§ZC04|dia|qtde";
                    if (this.nfeOficial.InfNFeField.Cana == null) this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana();
                    if (this.nfeOficial.InfNFeField.Cana.ForDia == null) this.nfeOficial.InfNFeField.Cana.ForDia = new List<DFeNFe.ForDia>();
                    this.nfeOficial.InfNFeField.Cana.ForDia.Add(new DFeNFe.ForDia { Dia = this.LerInt32(TpcnResources.dia, ObOp.Obrigatorio, 1, 2), Qtde = this.LerDouble(TpcnTipoCampo.tcDouble10, TpcnResources.qtde, ObOp.Obrigatorio, 11) });
        }

        private void AdicionarDeducaoCana()
        {
                    //layout = "§ZC10|xDed|vDed";
                    if (this.nfeOficial.InfNFeField.Cana == null) this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana();
                    if (this.nfeOficial.InfNFeField.Cana.Deduc == null) this.nfeOficial.InfNFeField.Cana.Deduc = new List<DFeNFe.Deduc>();
                    this.nfeOficial.InfNFeField.Cana.Deduc.Add(new DFeNFe.Deduc { XDed = this.LerString(TpcnResources.xDed, ObOp.Obrigatorio, 1, 60), VDed = this.LerDouble(TpcnTipoCampo.tcDouble2, TpcnResources.vDed, ObOp.Obrigatorio, 15) });
        }

        private void ProcessarResponsavelTecnicoZ()
        {
                    //layout = "ZD|CNPJ|xContato|email|fone|idCSRT|hashCSRT|"
                    var responsavel = CriarResponsavelTecnico();
                    responsavel.CNPJ = this.LerString(TpcnResources.CNPJ, ObOp.Obrigatorio, 14, 14);
                    responsavel.XContato = this.LerString("xContato", ObOp.Obrigatorio, 2, 60);
                    responsavel.Email = this.LerString(TpcnResources.email, ObOp.Obrigatorio, 2, 60);
                    responsavel.Fone = this.LerString(TpcnResources.fone, ObOp.Obrigatorio, 6, 14);
                    var idCsrt = this.LerInt32("idCSRT", ObOp.Opcional, 2, 2);
                    responsavel.IdCSRT = idCsrt > 0 ? idCsrt.ToString("00") : null;
                    responsavel.HashCSRT = VazioParaNulo(this.LerString("hashCSRT", ObOp.Opcional, 16, 80));
        }

        private void LerRegistro(NFeTxtSegment segmento)
        {
            if (segmento.Ignorar) return;

            int lenPipesRegistro = segmento.QuantidadePipes;
            int nProd = detalhesOficiais.Count - 1;
            this.Registro = segmento.Conteudo;
            this.FSegmento = segmento.Codigo;
#if DEBUG
            Console.WriteLine("Segmento lido: {0} - linha: {1} - Pipes: {2}", FSegmento, this.context.LinhaLida + 1, lenPipesRegistro);
#endif
            layout = segmento.Layout;

            if (this.initialSegmentDispatcher.TentarDespachar(this.FSegmento, lenPipesRegistro)) return;
            if (this.detailSegmentDispatcher.TentarDespachar(this.FSegmento, nProd, lenPipesRegistro)) return;
            if (this.totalSegmentDispatcher.TentarDespachar(this.FSegmento, nProd, lenPipesRegistro)) return;
            if (this.transportSegmentDispatcher.TentarDespachar(this.FSegmento, nProd, lenPipesRegistro)) return;
            if (this.paymentSegmentDispatcher.TentarDespachar(this.FSegmento, nProd, lenPipesRegistro)) return;
            if (this.additionalInformationSegmentDispatcher.TentarDespachar(this.FSegmento, nProd, lenPipesRegistro)) return;

            if (string.Equals(this.FSegmento, "H", StringComparison.OrdinalIgnoreCase))
            {
                this.IniciarItemDaNota(ref nProd);
            }
        }

        private DFeNFe.InfAdic CriarInformacoesAdicionais()
        {
            if (this.nfeOficial.InfNFeField.InfAdic == null) this.nfeOficial.InfNFeField.InfAdic = new DFeNFe.InfAdic();
            return this.nfeOficial.InfNFeField.InfAdic;
        }

        private static string NormalizarChaveDFe(string chave)
        {
            if (string.IsNullOrWhiteSpace(chave)) return string.Empty;

            var chaveNormalizada = chave.Trim().ToUpperInvariant();
            var prefixos = new[] { "NFCOM", "NFGAS", "MDFE", "NF3E", "NFE", "CTE", "DCE" };
            foreach (var prefixo in prefixos)
            {
                if (chaveNormalizada.StartsWith(prefixo))
                {
                    return chaveNormalizada.Substring(prefixo.Length);
                }
            }

            return chaveNormalizada;
        }

        private static string VazioParaNulo(string valor)
        {
            return string.IsNullOrWhiteSpace(valor) ? null : valor;
        }
    }
}
