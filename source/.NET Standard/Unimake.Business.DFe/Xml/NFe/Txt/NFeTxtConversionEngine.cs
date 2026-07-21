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

        private static string XmlTag<T>(string propertyName) => NFeXmlTagNameResolver.Get<T>(propertyName);

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

        private double LerDouble(TpcnTipoCampo Tipo, string tag, ObOp optional, int maxLength, bool returnNull) =>
            (double)LerCampo(Tipo, tag, optional, 0, maxLength, true, returnNull);

        private double LerDouble(TpcnTipoCampo Tipo, string tag, ObOp optional, int maxLength) =>
            (double)this.LerCampo(Tipo, tag, optional, 0, maxLength, true, false);
        private decimal LerDecimal(TpcnTipoCampo Tipo, string tag, ObOp optional, int maxLength) =>
            (decimal)this.LerCampo(Tipo, tag, optional, 0, maxLength, true, false);

        private double LerDouble(TpcnTipoCampo Tipo, string tag, ObOp optional, int minLength, int maxLength) =>
            (double)this.LerCampo(Tipo, tag, optional, minLength, maxLength, true, false);

        private Int32 LerInt32(string tag, ObOp optional, int minLength, int maxLength) =>
            (Int32)this.LerCampo(TpcnTipoCampo.tcInt, tag, optional, minLength, maxLength, true, false);

        private Int32 LerInt32(string tag, ObOp optional, int minLength, int maxLength, bool returnNull) =>
            (Int32)this.LerCampo(TpcnTipoCampo.tcInt, tag, optional, minLength, maxLength, true, returnNull);

        private string LerString(string tag, ObOp optional, int minLength, int maxLength) =>
            (string)this.LerCampo(TpcnTipoCampo.tcStr, tag, optional, minLength, maxLength, true, false);

        private string LerString(string tag, ObOp optional, int minLength, int maxLength, bool trim) =>
            (string)this.LerCampo(TpcnTipoCampo.tcStr, tag, optional, minLength, maxLength, trim, false);

        private object LerCampo(TpcnTipoCampo Tipo, string tag, ObOp optional, int minLength, int maxLength, bool trim, bool returnNull)
        {
            int nDecimais = 0;
            string ConteudoTag = "";
            try
            {
                ConteudoTag = RetornarConteudoTag(tag, trim, optional);

                if (ConteudoTag != "")
                    if (ConteudoTag.StartsWith(prefix))
                        ConteudoTag = "";

                if (string.IsNullOrEmpty(ConteudoTag) && (tag == "cEAN" || tag == "cEANTrib"))
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
                                                            this.FSegmento, tag, this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
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
                                                            this.FSegmento, tag, minLength, maxLength, ConteudoTag, this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
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
                                                                            this.FSegmento, tag, nDecimais, ndec, this.context.LinhaLida + 1, this.Registro.Substring(1)) + Environment.NewLine;
                                    }
                                }
                                else
                                    ndec = nDecimais;

                                #region -- atribui o numero de casas decimais que serão gravadas

                                if (ndec < (int)TpcnTipoCampo.tcDouble2 || ndec > (int)TpcnTipoCampo.tcDouble10)
                                    ndec = (int)TpcnTipoCampo.tcDouble2;

                                TpcnTipoCampo tipo = (TpcnTipoCampo)ndec;

                                if (tag == XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotMes)).ToString())
                                {
                                }

                                if (tag == XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotAnt)).ToString())
                                {
                                }

                                if (tag == XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotGer)).ToString())
                                {
                                }

                                if (tag == XmlTag<DFeNFe.ForDia>(nameof(DFeNFe.ForDia.Qtde)).ToString())
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
                                                    this.FSegmento, tag, ConteudoTag, this.context.LinhaLida + 1, this.Registro.Substring(1),
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
                NReceituario = this.LerString(XmlTag<DFeNFe.Defensivo>(nameof(DFeNFe.Defensivo.NReceituario)), ObOp.Obrigatorio, 1, 30),
                CPFRespTec = this.LerString(XmlTag<DFeNFe.Defensivo>(nameof(DFeNFe.Defensivo.CPFRespTec)), ObOp.Obrigatorio, 11, 11)
            });
        }

        private void ProcessarGuiaTransitoAgropecuario()
        {
            if (this.nfeOficial.InfNFeField.Agropecuario == null) this.nfeOficial.InfNFeField.Agropecuario = new DFeNFe.Agropecuario();
            this.nfeOficial.InfNFeField.Agropecuario.GuiaTransito = new DFeNFe.GuiaTransito
            {
                TpGuia = (TipoGuiaTransito)this.LerInt32(XmlTag<DFeNFe.GuiaTransito>(nameof(DFeNFe.GuiaTransito.TpGuia)), ObOp.Obrigatorio, 1, 1),
                UFGuia = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(XmlTag<DFeNFe.GuiaTransito>(nameof(DFeNFe.GuiaTransito.UFGuia)), ObOp.Obrigatorio, 2, 2), true),
                SerieGuia = VazioParaNulo(this.LerString(XmlTag<DFeNFe.GuiaTransito>(nameof(DFeNFe.GuiaTransito.SerieGuia)), ObOp.Opcional, 1, 9)),
                NGuia = this.LerString(XmlTag<DFeNFe.GuiaTransito>(nameof(DFeNFe.GuiaTransito.NGuia)), ObOp.Obrigatorio, 1, 9)
            };
        }
        private void ProcessarCana()
        {
            this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana
            {
                Safra = this.LerString(XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.Safra)), ObOp.Obrigatorio, 4, 9),
                Ref = this.LerString(NFeTxtFieldNames.Referencia, ObOp.Obrigatorio, 7, 7),
                QTotMes = this.LerDouble(TpcnTipoCampo.tcDouble10, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotMes)), ObOp.Obrigatorio, 11),
                QTotAnt = this.LerDouble(TpcnTipoCampo.tcDouble10, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotAnt)), ObOp.Obrigatorio, 11),
                QTotGer = this.LerDouble(TpcnTipoCampo.tcDouble10, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.QTotGer)), ObOp.Obrigatorio, 11),
                VFor = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.VFor)), ObOp.Obrigatorio, 15),
                VTotDed = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.VTotDed)), ObOp.Obrigatorio, 15),
                VLiqFor = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Cana>(nameof(DFeNFe.Cana.VLiqFor)), ObOp.Obrigatorio, 15)
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
                        RefNFe = this.LerString(XmlTag<DFeNFe.NFref>(nameof(DFeNFe.NFref.RefNFe)), ObOp.Obrigatorio, 44, 44)
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
                                CUF = (UFBrasil)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CUF)), ObOp.Obrigatorio, 2, 2),
                                AAMM = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.AAMM)), ObOp.Obrigatorio, 4, 4),
                                CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14),
                                Mod = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Mod)), ObOp.Obrigatorio, 2, 2),
                                Serie = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Serie)), ObOp.Obrigatorio, 1, 3),
                                NNF = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.NNF)), ObOp.Obrigatorio, 1, 9)
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
                                CUF = (UFBrasil)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CUF)), ObOp.Obrigatorio, 2, 2),
                                AAMM = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.AAMM)), ObOp.Obrigatorio, 4, 4),
                                IE = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Obrigatorio, 1, 14)),
                                Mod = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Mod)), ObOp.Obrigatorio, 2, 2),
                                Serie = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Serie)), ObOp.Obrigatorio, 1, 3),
                                NNF = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.NNF)), ObOp.Obrigatorio, 1, 9)
                            }
                        };
                        identificacaoOficial.NFref.Add(item);
                        if (FSegmento.ToUpper().Equals("BA10"))
                        {
                            var refCTe = this.LerString(XmlTag<DFeNFe.NFref>(nameof(DFeNFe.NFref.RefCTe)), ObOp.Opcional, 44, 44);
                            if (!string.IsNullOrWhiteSpace(refCTe)) identificacaoOficial.NFref.Add(new DFeNFe.NFref { RefCTe = refCTe });
                        }
                    }
                    #endregion
                    break;

                case "B20D":
                case "BA13":
                    ObterUltimaReferenciaProdutor(FSegmento.ToUpper().Equals("B20D") ? "Segmento B20d sem segmento B20A" : "Segmento BA13 sem segmento BA10").CNPJ =
                        this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
                    break;

                case "B20E":
                case "BA14":
                    ObterUltimaReferenciaProdutor(FSegmento.ToUpper().Equals("B20E") ? "Segmento B20e sem segmento B20A" : "Segmento BA14 sem segmento BA10").CPF =
                        this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11);
                    break;

                case "BA19":
                case "B20I":
                    //layout = "§BA19|refCTe"; //ok
                    identificacaoOficial.NFref.Add(new DFeNFe.NFref
                    {
                        RefCTe = LerString(XmlTag<DFeNFe.NFref>(nameof(DFeNFe.NFref.RefCTe)), ObOp.Obrigatorio, 44, 44)
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
                                Mod = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Mod)), ObOp.Obrigatorio, 2, 2),
                                NECF = this.LerInt32(XmlTag<DFeNFe.RefECF>(nameof(DFeNFe.RefECF.NECF)), ObOp.Obrigatorio, 1, 3),
                                NCOO = this.LerInt32(XmlTag<DFeNFe.RefECF>(nameof(DFeNFe.RefECF.NCOO)), ObOp.Obrigatorio, 1, 6)
                            }
                        });
                    }
                    break;

                case "BB01":
                    {
                        //layout = BB01|tpEnteGov|pRedutor|tpOperGov|refDFeAnt|
                        var refDFeAnt = VazioParaNulo(this.LerString(XmlTag<DFeNFe.GCompraGov>(nameof(DFeNFe.GCompraGov.RefDFeAnt)), ObOp.Opcional, 44, 44));
                        identificacaoOficial.GCompraGov = new DFeNFe.GCompraGov
                        {
                            TpEnteGov = (TipoEnteGovernamental)this.LerInt32(XmlTag<DFeNFe.GCompraGov>(nameof(DFeNFe.GCompraGov.TpEnteGov)), ObOp.Obrigatorio, 1, 1),
                            PRedutor = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GCompraGov>(nameof(DFeNFe.GCompraGov.PRedutor)), ObOp.Obrigatorio, 7),
                            TpOperGov = (TipoOperacaoEnteGovernamental)this.LerInt32(XmlTag<DFeNFe.GCompraGov>(nameof(DFeNFe.GCompraGov.TpOperGov)), ObOp.Obrigatorio, 1, 1),
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
                    identificacaoOficial.GCompraGov.RefDFeAnt.Add(this.LerString(XmlTag<DFeNFe.GCompraGov>(nameof(DFeNFe.GCompraGov.RefDFeAnt)), ObOp.Obrigatorio, 44, 44));
                    break;

                case "BC01":
                    //layout = BC01|refDFe|

                    if (identificacaoOficial.GPagAntecipado == null) identificacaoOficial.GPagAntecipado = new DFeNFe.GPagAntecipado { RefDFe = new List<string>() };
                    identificacaoOficial.GPagAntecipado.RefDFe.Add(this.LerString(XmlTag<DFeNFe.GPagAntecipado>(nameof(DFeNFe.GPagAntecipado.RefDFe)), ObOp.Obrigatorio, 44, 44));
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
                    double v = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.EnviNFe>(nameof(DFeNFe.EnviNFe.Versao)), ObOp.Opcional, 6);
                    this.chave = this.LerString(NFeTxtFieldNames.Id, ObOp.Opcional, 0, 47);
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
            identificacaoOficial.CUF = (UFBrasil)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CUF)), ObOp.Obrigatorio, 2, 2);
            var codigoNumerico = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CNF)), ObOp.Opcional, 8, 8);
            identificacaoOficial.CNF = codigoNumerico > 0 ? codigoNumerico.ToString("00000000") : null;
            identificacaoOficial.NatOp = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.NatOp)), ObOp.Obrigatorio, 1, 60);
            identificacaoOficial.Mod = (ModeloDFe)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Mod)), ObOp.Obrigatorio, 2, 2);
            identificacaoOficial.Serie = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.Serie)), ObOp.Obrigatorio, 1, 3);
            identificacaoOficial.NNF = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.NNF)), ObOp.Obrigatorio, 1, 9);

            var dataEmissao = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.DhEmi)), ObOp.Obrigatorio, 19, 25);
            var dataSaida = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.DhSaiEnt)), ObOp.Opcional, 0, 25);
            var destinoOperacao = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.IdDest)), ObOp.Obrigatorio, 1, 1);
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
            identificacaoOficial.TpNF = (TipoOperacao)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpNF)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.CMunFG = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CMunFG)), ObOp.Obrigatorio, 7, 7);
            identificacaoOficial.CMunFGIBS = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CMunFGIBS)), ObOp.Opcional, 7, 7);
            identificacaoOficial.TpImp = (FormatoImpressaoDANFE)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpImp)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.TpEmis = (TipoEmissao)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpEmis)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.CDV = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CDV)), ObOp.Opcional, 1, 1);
            this.cDvInformado = identificacaoOficial.CDV != 0;
            identificacaoOficial.TpAmb = (TipoAmbiente)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpAmb)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.FinNFe = (FinalidadeNFe)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.FinNFe)), ObOp.Obrigatorio, 1, 1);

            var tipoDebito = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpNFDebito)), ObOp.Opcional, 2, 2, true);
            identificacaoOficial.TpNFDebito = ObterEnumOpcional(tipoDebito, (TipoNFDebito)(-1));
            var tipoCredito = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.TpNFCredito)), ObOp.Opcional, 2, 2, true);
            identificacaoOficial.TpNFCredito = ObterEnumOpcional(tipoCredito, (TipoNFCredito)(-1));
            identificacaoOficial.IndFinal = (SimNao)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.IndFinal)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.IndPres = (IndicadorPresenca)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.IndPres)), ObOp.Obrigatorio, 1, 1);

            if (lenPipesRegistro >= 24)
            {
                var indicadorIntermediario = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.IndIntermed)), ObOp.Opcional, 1, 1, true);
                identificacaoOficial.IndIntermed = ObterEnumOpcional(indicadorIntermediario, (IndicadorIntermediario)(-1));
            }

            identificacaoOficial.ProcEmi = (ProcessoEmissao)this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.ProcEmi)), ObOp.Obrigatorio, 1, 1);
            identificacaoOficial.VerProc = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.VerProc)), ObOp.Obrigatorio, 1, 20);
            var dataContingencia = this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.DhCont)), ObOp.Opcional, 0, 25);
            if (!string.IsNullOrWhiteSpace(dataContingencia))
            {
                identificacaoOficial.DhCont = ConverterDataHora(dataContingencia);
            }
            identificacaoOficial.XJust = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.XJust)), ObOp.Opcional, 15, 256));

            if (lenPipesRegistro >= 28)
            {
                var previsaoEntrega = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.DPrevEntrega)), ObOp.Opcional, 8, 10, true, false);
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
            this.nfeOficial.InfNFeField.Emit.XNome = this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XNome)), ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.XFant = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XFant)), ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.IE = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Opcional, 0, 14);
            this.nfeOficial.InfNFeField.Emit.IEST = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.IEST)), ObOp.Opcional, 2, 14));
            this.nfeOficial.InfNFeField.Emit.IM = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.IM)), ObOp.Opcional, 1, 15));
            this.nfeOficial.InfNFeField.Emit.CNAE = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.CNAE)), ObOp.Opcional, 7, 7));
            this.nfeOficial.InfNFeField.Emit.CRT = (CRT)this.LerInt32(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.CRT)), ObOp.Obrigatorio, 1, 1);
            this.nfeOficial.InfNFeField.Emit.ISUFEmit = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.ISUFEmit)), ObOp.Opcional, 8, 9));
        }

        private void ProcessarDocumentoEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.CPF = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11);
        }

        private void ProcessarEnderecoEmitente()
        {
            this.nfeOficial.InfNFeField.Emit.EnderEmit = new DFeNFe.EnderEmit();
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XLgr = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XLgr)), ObOp.Obrigatorio, 1, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.Nro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Nro)), ObOp.Obrigatorio, 1, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XCpl = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XCpl)), ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XBairro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XBairro)), ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CMun = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CMun)), ObOp.Obrigatorio, 7, 7);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XMun = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XMun)), ObOp.Obrigatorio, 2, 60);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.UF = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio, 2, 2), true);
            var cep = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CEP)), ObOp.Opcional, 0, 8);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CEP = cep > 0 ? cep.ToString("00000000") : null;
            this.nfeOficial.InfNFeField.Emit.EnderEmit.CPais = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CPais)), ObOp.Obrigatorio, 4, 4);
            this.nfeOficial.InfNFeField.Emit.EnderEmit.XPais = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XPais)), ObOp.Opcional, 1, 60));
            this.nfeOficial.InfNFeField.Emit.EnderEmit.Fone = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Opcional, 6, 14));
        }
        private void ProcessarNotaAvulsa()
        {
            this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
            this.LerString(XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.XOrgao)), ObOp.Obrigatorio, 1, 60);
            this.LerString(XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.Matr)), ObOp.Obrigatorio, 1, 60);
            this.LerString(XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.XAgente)), ObOp.Obrigatorio, 1, 60);
            this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Obrigatorio, 6, 14);
            this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio, 2, 2);
            this.LerString(XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.NDAR)), ObOp.Obrigatorio, 1, 60);
            this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.DEmi)), ObOp.Obrigatorio, 10, 10, true, false);
            this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.VDAR)), ObOp.Obrigatorio, 15);
            this.LerString(XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.RepEmi)), ObOp.Obrigatorio, 1, 60);
            this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.DPag)), ObOp.Opcional, 10, 10, true, false);
        }
        private void ProcessarDestinatario()
        {
            destinatarioOficial.XNome = this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XNome)), identificacaoOficial.Mod != ModeloDFe.NFe ? ObOp.Opcional : ObOp.Obrigatorio, 2, 60);
            if (versaoNFe >= 3)
                destinatarioOficial.IndIEDest = (IndicadorIEDestinatario)this.LerInt32(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.IndIEDest)), ObOp.Opcional, 0, 1);
            destinatarioOficial.IE = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Opcional, 0, 14));
            destinatarioOficial.ISUF = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.ISUF)), ObOp.Opcional, 8, 9));
            if (versaoNFe >= 3)
                destinatarioOficial.IM = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.IM)), ObOp.Opcional, 1, 15));
            destinatarioOficial.Email = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.Email)), ObOp.Opcional, 1, 60));
        }

        private void ProcessarDocumentoDestinatario()
        {
            destinatarioOficial.CNPJ = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Opcional, 14, 14));
        }

        private void ProcessarCpfDestinatario()
        {
            if (identificacaoOficial.Mod == ModeloDFe.NFCe)
                destinatarioOficial.CPF = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Opcional, 11, 11));
            else
                destinatarioOficial.CPF = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11));
        }

        private void ProcessarIdEstrangeiroDestinatario()
        {
            destinatarioOficial.IdEstrangeiro = this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.IdEstrangeiro)), ObOp.Opcional, 5, 20);
            if (string.IsNullOrEmpty(destinatarioOficial.IdEstrangeiro) && string.IsNullOrEmpty(destinatarioOficial.CPF)) destinatarioOficial.IdEstrangeiro = "NAO GERAR TAG";
        }

        private void ProcessarEnderecoDestinatario()
        {
            destinatarioOficial.EnderDest.XLgr = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XLgr)), ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.Nro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Nro)), ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.XCpl = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XCpl)), ObOp.Opcional, 1, 60));
            destinatarioOficial.EnderDest.XBairro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XBairro)), ObOp.Obrigatorio, 1, 60);
            destinatarioOficial.EnderDest.CMun = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CMun)), ObOp.Obrigatorio, 7, 7);
            destinatarioOficial.EnderDest.XMun = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XMun)), ObOp.Obrigatorio, 2, 60);
            destinatarioOficial.EnderDest.UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio).Value;
            var cep = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CEP)), ObOp.Opcional, 0, 8);
            destinatarioOficial.EnderDest.CEP = cep > 0 ? cep.ToString("00000000") : null;
            destinatarioOficial.EnderDest.CPais = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CPais)), ObOp.Obrigatorio, 2, 4);
            destinatarioOficial.EnderDest.XPais = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XPais)), ObOp.Opcional, 2, 60));
            destinatarioOficial.EnderDest.Fone = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Opcional, 6, 14));
        }
        private void ProcessarLocalRetirada(int lenPipesRegistro)
        {
            bool novo;
            if ((novo = lenPipesRegistro == 16 || versaoNFe >= 4))
            {
                retiradaOficial.CNPJ = VazioParaNulo(this.LerString(NFeTxtFieldNames.CnpjCpf, ObOp.Opcional, 0, 0));
                if (!string.IsNullOrEmpty(retiradaOficial.CNPJ) && retiradaOficial.CNPJ.Length == 11)
                {
                    retiradaOficial.CPF = retiradaOficial.CNPJ;
                    retiradaOficial.CNPJ = null;
                }
                retiradaOficial.XNome = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XNome)), ObOp.Opcional, 2, 60));
            }
            retiradaOficial.XLgr = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XLgr)), ObOp.Obrigatorio, 1, 60);
            retiradaOficial.Nro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Nro)), ObOp.Obrigatorio, 1, 60);
            retiradaOficial.XCpl = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XCpl)), ObOp.Opcional, 1, 60));
            retiradaOficial.XBairro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XBairro)), ObOp.Obrigatorio, 1, 60);
            retiradaOficial.CMun = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CMun)), ObOp.Obrigatorio, 7, 7);
            retiradaOficial.XMun = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XMun)), ObOp.Obrigatorio, 2, 60);
            retiradaOficial.UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio).Value;
            if (novo)
            {
                retiradaOficial.CEP = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CEP)), ObOp.Opcional, 8, 8));
                retiradaOficial.CPais = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CPais)), ObOp.Opcional, 4, 4);
                retiradaOficial.XPais = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XPais)), ObOp.Opcional, 2, 60));
                retiradaOficial.Fone = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Opcional, 6, 14));
                retiradaOficial.Email = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.Email)), ObOp.Opcional, 1, 60));
                retiradaOficial.IE = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Opcional, 2, 14));
            }
        }

        private void ProcessarDocumentoRetirada()
        {
            retiradaOficial.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfRetirada()
        {
            retiradaOficial.CPF = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11);
        }
        private void ProcessarLocalEntrega(int lenPipesRegistro)
        {
            bool novo;
            if ((novo = lenPipesRegistro == 16 || versaoNFe >= 4))
            {
                entregaOficial.CNPJ = VazioParaNulo(this.LerString(NFeTxtFieldNames.CnpjCpf, ObOp.Opcional, 0, 0));
                if (!string.IsNullOrEmpty(entregaOficial.CNPJ) && entregaOficial.CNPJ.Length == 11)
                {
                    entregaOficial.CPF = entregaOficial.CNPJ;
                    entregaOficial.CNPJ = null;
                }
                entregaOficial.XNome = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XNome)), ObOp.Opcional, 2, 60));
            }
            entregaOficial.XLgr = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XLgr)), ObOp.Obrigatorio, 1, 60);
            entregaOficial.Nro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Nro)), ObOp.Obrigatorio, 1, 60);
            entregaOficial.XCpl = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XCpl)), ObOp.Opcional, 1, 60));
            entregaOficial.XBairro = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XBairro)), ObOp.Obrigatorio, 1, 60);
            entregaOficial.CMun = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CMun)), ObOp.Obrigatorio, 7, 7);
            entregaOficial.XMun = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XMun)), ObOp.Obrigatorio, 2, 60);
            entregaOficial.UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio).Value;
            if (novo)
            {
                entregaOficial.CEP = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CEP)), ObOp.Opcional, 8, 8));
                entregaOficial.CPais = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CPais)), ObOp.Opcional, 4, 4);
                entregaOficial.XPais = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XPais)), ObOp.Opcional, 2, 60));
                entregaOficial.Fone = VazioParaNulo(this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Opcional, 6, 14));
                entregaOficial.Email = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.Email)), ObOp.Opcional, 1, 60));
                entregaOficial.IE = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Opcional, 2, 14));
            }
        }

        private void ProcessarDocumentoEntrega()
        {
            entregaOficial.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
        }

        private void ProcessarCpfEntrega()
        {
            entregaOficial.CPF = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11);
        }

        private void AdicionarAutorizacaoXmlCnpj()
        {
            autorizadosXmlOficiais.Add(new DFeNFe.AutXML
            {
                CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14)
            });
        }

        private void AdicionarAutorizacaoXmlCpf()
        {
            autorizadosXmlOficiais.Add(new DFeNFe.AutXML
            {
                CPF = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Obrigatorio, 11, 11)
            });
        }
        private void IniciarItemDaNota(ref int nProd)
        {
            nProd = detalhesOficiais.Count;
            detalhesOficiais.Add(new DFeNFe.Det
            {
                NItem = this.LerInt32(NFeTxtFieldNames.NumeroItem, ObOp.Obrigatorio, 1, 3),
                InfAdProd = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Det>(nameof(DFeNFe.Det.InfAdProd)), ObOp.Opcional, 0, 500, false)),
                Prod = new DFeNFe.Prod(),
                Imposto = new DFeNFe.Imposto()
            });
        }
        private void ProcessarProduto(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            produto.CProd = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CProd)), ObOp.Obrigatorio, 1, 60);
            produto.CEAN = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CEAN)), ObOp.Obrigatorio, 0, 14);
            if (lenPipesRegistro >= 30)
            {
                produto.CBarra = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CBarra)), ObOp.Opcional, 0, 30));
            }

            var descricao = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.XProd)), ObOp.Obrigatorio, 1, 120);
            produto.XProd = identificacaoOficial.TpAmb == TipoAmbiente.Homologacao &&
                identificacaoOficial.Mod == ModeloDFe.NFCe && detalhesOficiais[nProd].NItem == 1
                    ? "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
                    : descricao;
            produto.NCM = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NCM)), ObOp.Obrigatorio, 2, 8);
            var nve = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NVE)), ObOp.Opcional, 0, 6);
            produto.NVE = string.IsNullOrWhiteSpace(nve) ? null : new List<string> { nve };
            var cest = this.LerInt32(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CEST)), ObOp.Opcional, 0, 7);
            produto.CEST = cest > 0 ? cest.ToString("0000000") : null;

            switch (this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.IndEscala)), ObOp.Opcional, 1, 1))
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

            produto.CNPJFab = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CNPJFab)), ObOp.Opcional, 0, 14));
            produto.CBenef = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CBenef)), ObOp.Opcional, 0, 10));
            produto.EXTIPI = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.EXTIPI)), ObOp.Opcional, 2, 3));
            produto.CFOP = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CFOP)), ObOp.Obrigatorio, 4, 4);
            produto.UCom = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.UCom)), ObOp.Obrigatorio, 1, 6);
            produto.QCom = this.LerDecimal(TpcnTipoCampo.tcDec4, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.QCom)), ObOp.Obrigatorio, 11);
            produto.VUnCom = this.LerDecimal(TpcnTipoCampo.tcDec10, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VUnCom)), ObOp.Opcional, 21);
            produto.VProd = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VProd)), ObOp.Obrigatorio, 15);
            produto.CEANTrib = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CEANTrib)), ObOp.Obrigatorio, 0, 14);
            if (lenPipesRegistro >= 30)
            {
                produto.CBarraTrib = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CBarraTrib)), ObOp.Opcional, 0, 30));
            }
            produto.UTrib = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.UTrib)), ObOp.Obrigatorio, 1, 6);
            produto.QTrib = this.LerDecimal(TpcnTipoCampo.tcDec4, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.QTrib)), ObOp.Obrigatorio, 15);
            produto.VUnTrib = this.LerDecimal(TpcnTipoCampo.tcDec10, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VUnTrib)), ObOp.Obrigatorio, 21);
            produto.VFrete = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VFrete)), ObOp.Opcional, 15);
            produto.VSeg = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VSeg)), ObOp.Opcional, 15);
            produto.VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VDesc)), ObOp.Opcional, 15);
            produto.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VOutro)), ObOp.Opcional, 15);
            produto.IndTot = (SimNao)this.LerInt32(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.IndTot)), ObOp.Obrigatorio, 1, 1);
            produto.XPed = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.XPed)), ObOp.Opcional, 1, 15));
            produto.NItemPed = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NItemPed)), ObOp.Opcional, 0, 6));
            produto.NFCI = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NFCI)), ObOp.Opcional, 0, 255));
        }

        private void AdicionarCreditoPresumido(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.GCred == null) produto.GCred = new List<DFeNFe.GCred>();
            produto.GCred.Add(new DFeNFe.GCred
            {
                CCredPresumido = this.LerString(XmlTag<DFeNFe.GCred>(nameof(DFeNFe.GCred.CCredPresumido)), ObOp.Obrigatorio, 8, 10),
                PCredPresumido = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GCred>(nameof(DFeNFe.GCred.PCredPresumido)), ObOp.Obrigatorio, 8),
                VCredPresumido = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCred>(nameof(DFeNFe.GCred.VCredPresumido)), ObOp.Obrigatorio, 16)
            });
        }

        private void ProcessarNveProduto(int nProd)
        {
            var nve = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NVE)), ObOp.Opcional, 0, 6);
            detalhesOficiais[nProd].Prod.NVE = string.IsNullOrWhiteSpace(nve) ? null : new List<string> { nve };
        }

        private void ProcessarCreditoPresumidoIbsZfm(int nProd)
        {
            var valor = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.TpCredPresIBSZFM)), ObOp.Opcional, 1, 1);
            int codigo;
            detalhesOficiais[nProd].Prod.TpCredPresIBSZFM = ObterEnumOpcional(
                int.TryParse(valor, out codigo) ? codigo : -1, (TipoCreditoPresumidoIBSZFM)(-1));
        }

        private void ProcessarCestProduto(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            var cest = this.LerInt32(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CEST)), ObOp.Opcional, 0, 7);
            produto.CEST = cest > 0 ? cest.ToString("0000000") : null;
            if (lenPipesRegistro == 4)
            {
                this.LerInt32(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.IndEscala)), ObOp.Opcional, 1, 1);
                produto.IndEscala = ObterEnumOpcional(-1, (IndicadorEscalaRelevante)(-1));
                produto.CNPJFab = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CNPJFab)), ObOp.Opcional, 0, 14));
            }
        }

        private void ProcessarBemMovelUsado(int nProd)
        {
            detalhesOficiais[nProd].Prod.IndBemMovelUsado = this.LerInt32(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.IndBemMovelUsado)), ObOp.Opcional, 1, 1);
        }
        private void AdicionarDeclaracaoImportacao(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.DI == null) produto.DI = new List<DFeNFe.DI>();
            var item = new DFeNFe.DI
            {
                NDI = this.LerString(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.NDI)), ObOp.Obrigatorio, 1, 15),
                DDI = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.DDI)), ObOp.Obrigatorio, 10, 10, true, false),
                XLocDesemb = this.LerString(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.XLocDesemb)), ObOp.Obrigatorio, 1, 60),
                UFDesemb = LerUF(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.UFDesemb)), ObOp.Obrigatorio).Value,
                DDesemb = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.DDesemb)), ObOp.Obrigatorio, 10, 10, true, false),
                TpViaTransp = (ViaTransporteInternacional)this.LerInt32(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.TpViaTransp)), ObOp.Opcional, 1, 2),
                VAFRMM = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.VAFRMM)), ObOp.Opcional, 15),
                TpIntermedio = (FormaImportacaoIntermediacao)this.LerInt32(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.TpIntermedio)), ObOp.Opcional, 1, 1),
                CNPJ = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Opcional, 14, 14))
            };
            if (lenPipesRegistro >= 13)
            {
                item.CPF = VazioParaNulo(this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Opcional, 11, 11));
            }
            var ufTerceiro = this.LerString(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.UFTerceiro)), ObOp.Opcional, 2, 2);
#if INTEROP
            item.UFTerceiro = string.IsNullOrWhiteSpace(ufTerceiro) ? UFBrasil.NaoDefinido : (UFBrasil)Enum.Parse(typeof(UFBrasil), ufTerceiro, true);
#else
            item.UFTerceiro = string.IsNullOrWhiteSpace(ufTerceiro) ? null : (UFBrasil?)Enum.Parse(typeof(UFBrasil), ufTerceiro, true);
#endif
            item.CExportador = this.LerString(XmlTag<DFeNFe.DI>(nameof(DFeNFe.DI.CExportador)), ObOp.Obrigatorio, 1, 60);
            produto.DI.Add(item);
        }

        private void AdicionarAdicaoImportacao(int nProd)
        {
            var declaracoes = detalhesOficiais[nProd].Prod.DI;
            var declaracao = declaracoes[declaracoes.Count - 1];
            if (declaracao.Adi == null) declaracao.Adi = new List<DFeNFe.Adi>();
            declaracao.Adi.Add(new DFeNFe.Adi
            {
                NAdicao = this.LerInt32(XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.NAdicao)), ObOp.Obrigatorio, 1, 3),
                NSeqAdic = this.LerInt32(XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.NSeqAdic)), ObOp.Obrigatorio, 1, 3),
                CFabricante = this.LerString(XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.CFabricante)), ObOp.Obrigatorio, 1, 60),
                VDescDI = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.VDescDI)), ObOp.Opcional, 15),
                NDraw = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.NDraw)), ObOp.Opcional, 0, 11))
            });
        }

        private void AdicionarDetalheExportacao(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.DetExport == null) produto.DetExport = new List<DFeNFe.DetExport>();
            produto.DetExport.Add(new DFeNFe.DetExport { NDraw = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Adi>(nameof(DFeNFe.Adi.NDraw)), ObOp.Opcional, 0, 11)) });
        }

        private void ProcessarExportacaoIndireta(int nProd)
        {
            var exportacoes = detalhesOficiais[nProd].Prod.DetExport;
            exportacoes[exportacoes.Count - 1].ExportInd = new DFeNFe.ExportInd
            {
                NRE = this.LerString(XmlTag<DFeNFe.ExportInd>(nameof(DFeNFe.ExportInd.NRE)), ObOp.Opcional, 1, 12),
                ChNFe = this.LerString(XmlTag<DFeNFe.ExportInd>(nameof(DFeNFe.ExportInd.ChNFe)), ObOp.Obrigatorio, 44, 44),
                QExport = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ExportInd>(nameof(DFeNFe.ExportInd.QExport)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void AdicionarRastroProduto(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.Rastro == null) produto.Rastro = new List<DFeNFe.Rastro>();
            produto.Rastro.Add(new DFeNFe.Rastro
            {
                NLote = this.LerString(XmlTag<DFeNFe.Rastro>(nameof(DFeNFe.Rastro.NLote)), ObOp.Obrigatorio, 1, 20),
                QLote = this.LerDouble(TpcnTipoCampo.tcDouble3, XmlTag<DFeNFe.Rastro>(nameof(DFeNFe.Rastro.QLote)), ObOp.Obrigatorio, 11),
                DFab = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Rastro>(nameof(DFeNFe.Rastro.DFab)), ObOp.Obrigatorio, 10, 10, true, false),
                DVal = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Rastro>(nameof(DFeNFe.Rastro.DVal)), ObOp.Obrigatorio, 10, 10, true, false),
                CAgreg = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Rastro>(nameof(DFeNFe.Rastro.CAgreg)), ObOp.Opcional, 0, 20))
            });
        }
        private void ProcessarResponsavelTecnico()
        {
            var responsavel = CriarResponsavelTecnico();
            responsavel.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
            responsavel.XContato = this.LerString("xContato", ObOp.Obrigatorio, 2, 60);
            responsavel.Email = this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.Email)), ObOp.Obrigatorio, 2, 60);
            responsavel.Fone = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Obrigatorio, 6, 14);
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
                TpOp = (TipoOperacaoVeicNovo)this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.TpOp)), ObOp.Obrigatorio, 1, 1),
                Chassi = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.Chassi)), ObOp.Obrigatorio, 17, 17),
                CCor = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.CCor)), ObOp.Obrigatorio, 1, 4),
                XCor = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.XCor)), ObOp.Obrigatorio, 1, 40),
                Pot = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.Pot)), ObOp.Obrigatorio, 1, 4),
                Cilin = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.Cilin)), ObOp.Obrigatorio, 1, 4),
                PesoL = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.PesoL)), ObOp.Obrigatorio, 1, 9),
                PesoB = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.PesoB)), ObOp.Obrigatorio, 1, 9),
                NSerie = this.LerString(XmlTag<DFeNFe.Arma>(nameof(DFeNFe.Arma.NSerie)), ObOp.Obrigatorio, 1, 9),
                TpComb = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.TpComb)), ObOp.Obrigatorio, 1, 2),
                NMotor = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.NMotor)), ObOp.Obrigatorio, 1, 21),
                CMT = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.CMT)), ObOp.Obrigatorio, 1, 9),
                Dist = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.Dist)), ObOp.Obrigatorio, 1, 4),
                AnoMod = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.AnoMod)), ObOp.Obrigatorio, 4, 4).ToString("0000"),
                AnoFab = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.AnoFab)), ObOp.Obrigatorio, 4, 4).ToString("0000"),
                TpPint = this.LerString(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.TpPint)), ObOp.Obrigatorio, 1, 1),
                TpVeic = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.TpVeic)), ObOp.Obrigatorio, 1, 2).ToString(),
                EspVeic = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.EspVeic)), ObOp.Obrigatorio, 1, 1).ToString(),
                VIN = (CondicaoVIN)this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.VIN)), ObOp.Obrigatorio, 1, 1),
                CondVeic = (CondicaoVeiculo)this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.CondVeic)), ObOp.Obrigatorio, 1, 1),
                CMod = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.CMod)), ObOp.Obrigatorio, 1, 6),
                CCorDENATRAN = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.CCorDENATRAN)), ObOp.Obrigatorio, 1, 2).ToString("00"),
                Lota = this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.Lota)), ObOp.Obrigatorio, 1, 3),
                TpRest = (TipoRestricaoVeiculo)this.LerInt32(XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.TpRest)), ObOp.Obrigatorio, 1, 1)
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
                CProdANVISA = LerString(XmlTag<DFeNFe.Med>(nameof(DFeNFe.Med.CProdANVISA)), ObOp.Obrigatorio, 1, 13),
                XMotivoIsencao = VazioParaNulo(LerString("xMotivoIsencao", ObOp.Opcional, 1, 255)),
                VPMC = LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Med>(nameof(DFeNFe.Med.VPMC)), ObOp.Obrigatorio, 15)
            };
        }

        private void AdicionarArma(int nProd)
        {
            var produto = detalhesOficiais[nProd].Prod;
            if (produto.Arma == null) produto.Arma = new List<DFeNFe.Arma>();
            produto.Arma.Add(new DFeNFe.Arma
            {
                TpArma = (TipoArma)this.LerInt32(XmlTag<DFeNFe.Arma>(nameof(DFeNFe.Arma.TpArma)), ObOp.Obrigatorio, 1, 1),
                NSerie = LerString(XmlTag<DFeNFe.Arma>(nameof(DFeNFe.Arma.NSerie)), ObOp.Obrigatorio, 1, 15),
                NCano = LerString(XmlTag<DFeNFe.Arma>(nameof(DFeNFe.Arma.NCano)), ObOp.Obrigatorio, 1, 15),
                Descr = LerString(XmlTag<DFeNFe.Arma>(nameof(DFeNFe.Arma.Descr)), ObOp.Obrigatorio, 1, 256)
            });
        }

        private void ProcessarCombustivel(int nProd, int lenPipesRegistro)
        {
            var produto = detalhesOficiais[nProd].Prod;
            var combustivel = new DFeNFe.Comb
            {
                CProdANP = this.LerInt32(XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.CProdANP)), ObOp.Obrigatorio, 9, 9).ToString("000000000"),
                DescANP = this.LerString(XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.DescANP)), ObOp.Opcional, 2, 295),
                PGLP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.PGLP)), ObOp.Opcional, 16),
                PGNn = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.PGNn)), ObOp.Opcional, 16),
                PGNi = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.PGNi)), ObOp.Opcional, 16),
                VPart = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.VPart)), ObOp.Opcional, 16),
                CODIF = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.CODIF)), ObOp.Opcional, 0, 21)),
                QTemp = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.QTemp)), ObOp.Opcional, 16),
                UFCons = LerUF(XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.UFCons)), ObOp.Obrigatorio).Value
            };

            if (lenPipesRegistro >= 11)
            {
                combustivel.PBio = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Comb>(nameof(DFeNFe.Comb.PBio)), ObOp.Opcional, 15);
            }
            produto.Comb = new List<DFeNFe.Comb> { combustivel };
        }

        private void ProcessarEncerranteCombustivel(int nProd)
        {
            var combustivel = ObterCombustivel(nProd);
            combustivel.Encerrante = new DFeNFe.Encerrante
            {
                NBico = this.LerInt32(XmlTag<DFeNFe.Encerrante>(nameof(DFeNFe.Encerrante.NBico)), ObOp.Obrigatorio, 1, 3),
                NBomba = this.LerInt32(XmlTag<DFeNFe.Encerrante>(nameof(DFeNFe.Encerrante.NBomba)), ObOp.Opcional, 0, 3),
                NTanque = this.LerInt32(XmlTag<DFeNFe.Encerrante>(nameof(DFeNFe.Encerrante.NTanque)), ObOp.Obrigatorio, 1, 3),
                VEncIni = double.Parse(this.LerString(XmlTag<DFeNFe.Encerrante>(nameof(DFeNFe.Encerrante.VEncIni)), ObOp.Obrigatorio, 1, 15), CultureInfo.InvariantCulture),
                VEncFin = double.Parse(this.LerString(XmlTag<DFeNFe.Encerrante>(nameof(DFeNFe.Encerrante.VEncFin)), ObOp.Obrigatorio, 1, 15), CultureInfo.InvariantCulture)
            };
        }

        private void AdicionarOrigemCombustivel(int nProd)
        {
            var combustivel = ObterCombustivel(nProd);
            if (combustivel.OrigComb == null) combustivel.OrigComb = new List<DFeNFe.OrigComb>();
            combustivel.OrigComb.Add(new DFeNFe.OrigComb
            {
                IndImport = (IndicadorImportacao)this.LerInt32(XmlTag<DFeNFe.OrigComb>(nameof(DFeNFe.OrigComb.IndImport)), ObOp.Obrigatorio, 1, 1),
                CUFOrig = (UFBrasil)this.LerInt32(XmlTag<DFeNFe.OrigComb>(nameof(DFeNFe.OrigComb.CUFOrig)), ObOp.Obrigatorio, 2, 2),
                POrig = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.OrigComb>(nameof(DFeNFe.OrigComb.POrig)), ObOp.Obrigatorio, 15)
            });
        }

        private void ProcessarCideCombustivel(int nProd)
        {
            var quantidade = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16);
            var aliquota = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15);
            var valor = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VCIDE)), ObOp.Obrigatorio, 15);
            ObterCombustivel(nProd).CIDE = quantidade > 0 ? new DFeNFe.CIDE
            {
                QBCProd = quantidade,
                VAliqProd = aliquota,
                VCIDE = valor
            } : null;
        }
        private void ProcessarRecopi(int nProd)
        {
            detalhesOficiais[nProd].Prod.NRECOPI = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.NRECOPI)), ObOp.Opcional, 20, 20));
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
            detalhesOficiais[nProd].Imposto.VTotTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Imposto>(nameof(DFeNFe.Imposto.VTotTrib)), ObOp.Opcional, 15);
        }
        private void ProcessarIcms00(int nProd)
        {
            var icms = new DFeNFe.ICMS00
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Obrigatorio, 15);
                icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS00 = icms };
        }

        private void ProcessarIcms02(int nProd)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS02 = new DFeNFe.ICMS02
                {
                    Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.QBCMono)), ObOp.Opcional, 15),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.AdRemICMS)), ObOp.Obrigatorio, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.VICMSMono)), ObOp.Obrigatorio, 13)
                }
            };
        }
        private void ProcessarIcms10(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS10
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCP)), ObOp.Obrigatorio, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Obrigatorio, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15)
            };
            if (lenPipesRegistro >= 21)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSSTDeson)), ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.MotDesICMSST)), ObOp.Opcional, 1, 2);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS10 = icms };
        }

        private void ProcessarIcms15(int nProd)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS15 = new DFeNFe.ICMS15
                {
                    Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.QBCMono)), ObOp.Opcional, 11),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.AdRemICMS)), ObOp.Obrigatorio, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.VICMSMono)), ObOp.Obrigatorio, 15),
                    QBCMonoReten = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.QBCMonoReten)), ObOp.Opcional, 11),
                    AdRemICMSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.AdRemICMSReten)), ObOp.Obrigatorio, 15),
                    VICMSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.VICMSMonoReten)), ObOp.Obrigatorio, 15),
                    PRedAdRem = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.PRedAdRem)), ObOp.Obrigatorio, 15),
                    MotRedAdRem = (MotivoReducaoAdRem)this.LerInt32(XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.MotRedAdRem)), ObOp.Obrigatorio, 1, 1)
                }
            };
        }

        private void ProcessarIcms20(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS20
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Obrigatorio, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCP)), ObOp.Obrigatorio, 15);
                icms.PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Obrigatorio, 15);
                icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Obrigatorio, 15);
            }
            icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15);
            icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.MotDesICMS)), ObOp.Opcional, 1, 1);
            if (lenPipesRegistro >= 14) icms.IndDeduzDeson = LerSimNaoOpcional(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.IndDeduzDeson)));
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS20 = icms };
        }

        private void ProcessarIcms30(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS30
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15);
            }
            icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15);
            icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.MotDesICMS)), ObOp.Opcional, 1, 1);
            if (lenPipesRegistro >= 15) icms.IndDeduzDeson = LerSimNaoOpcional(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.IndDeduzDeson)));
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS30 = icms };
        }

        private void ProcessarIcms40_41_50(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS40
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2)
            };
            if (lenPipesRegistro >= 5)
            {
                if (versaoNFe >= 3) icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15);
                else this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Opcional, 15);
                icms.MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.MotDesICMS)), ObOp.Opcional, 1, 1);
            }
            if (lenPipesRegistro >= 6) icms.IndDeduzDeson = LerSimNaoOpcional(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.IndDeduzDeson)));
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
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Opcional, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Opcional, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Opcional, this.CasasDecimais75),
                VICMSOp = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.VICMSOp)), ObOp.Opcional, 15),
                PDif = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PDif)), ObOp.Opcional, this.CasasDecimais75),
                VICMSDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.VICMSDif)), ObOp.Opcional, 15),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Opcional, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCP)), ObOp.Obrigatorio, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Obrigatorio, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Obrigatorio, 15)
            };
            if (lenPipesRegistro == 10 && (icms.VICMS ?? 0) == 0) icms.VICMS = Math.Max(0, (icms.VICMSOp ?? 0) - (icms.VICMSDif ?? 0));
            if (lenPipesRegistro >= 17)
            {
                icms.PFCPDif = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PFCPDif)), ObOp.Opcional, 15);
                icms.VFCPDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.VFCPDif)), ObOp.Opcional, 15);
                icms.VFCPEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.VFCPEfet)), ObOp.Opcional, 15);
                if (lenPipesRegistro >= 18) icms.CBenefRBC = this.LerString(XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.CBenefRBC)), ObOp.Opcional, 8, 10);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS51 = icms };
        }

        private void ProcessarIcms53(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMS53 = new DFeNFe.ICMS53
                {
                    Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    QBCMono = (decimal)this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.QBCMono)), ObOp.Opcional, 15),
                    AdRemICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.AdRemICMS)), ObOp.Obrigatorio, 15),
                    VICMSMonoOp = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS53>(nameof(DFeNFe.ICMS53.VICMSMonoOp)), ObOp.Opcional, 15),
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PDif)), ObOp.Opcional, 15),
                    VICMSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS53>(nameof(DFeNFe.ICMS53.VICMSMonoDif)), ObOp.Opcional, 15),
                    VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.VICMSMono)), ObOp.Opcional, 15)
                }
            };
        }

        private void ProcessarIcms60(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1);
            var cst = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2);
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
                vBCSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCSTRet)), ObOp.Obrigatorio, 15);
                pST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PST)), ObOp.Opcional, 15);
                vICMSSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSTRet)), ObOp.Obrigatorio, 15);
                icms.VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCFCPSTRet)), ObOp.Opcional, 15);
                icms.PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PFCPSTRet)), ObOp.Opcional, 15);
                icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VFCPSTRet)), ObOp.Opcional, 15);
                if (lenPipesRegistro >= 13)
                {
                    icms.PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PRedBCEfet)), ObOp.Opcional, 15);
                    icms.VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCEfet)), ObOp.Opcional, 15);
                    icms.PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PICMSEfet)), ObOp.Opcional, 15);
                    icms.VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSEfet)), ObOp.Opcional, 15);
                }
                vICMSSubstituto = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSubstituto)), ObOp.Opcional, 15);

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
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2)
            };
            if (versaoNFe >= 4)
            {
                icms.QBCMonoRet = this.LerDecimal(TpcnTipoCampo.tcDec4, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.QBCMonoRet)), ObOp.Obrigatorio, 15);
                icms.AdRemICMSRet = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.AdRemICMSRet)), ObOp.Opcional, 15);
                icms.VICMSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.VICMSMonoRet)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS61 = icms };
        }

        private void ProcessarIcms70(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS70
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Obrigatorio, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCP)), ObOp.Opcional, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Opcional, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Opcional, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Opcional, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Opcional, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Opcional, 15),
                VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15),
                MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.MotDesICMS)), ObOp.Opcional, 1, 1)
            };
            if (lenPipesRegistro >= 24)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSSTDeson)), ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.MotDesICMSST)), ObOp.Opcional, 1, 2);
                if (lenPipesRegistro >= 25) icms.IndDeduzDeson = LerSimNaoOpcional(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.IndDeduzDeson)));
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS70 = icms };
        }

        private void ProcessarIcms90(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMS90
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15),
                ModBCST = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15),
                VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15),
                MotDesICMS = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.MotDesICMS)), ObOp.Opcional, 1, 1),
                VBCFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCP)), ObOp.Opcional, 15),
                PFCP = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PFCP)), ObOp.Opcional, 15),
                VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Opcional, 15),
                VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Opcional, 15),
                PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Opcional, 15),
                VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Opcional, 15)
            };
            if (lenPipesRegistro >= 24)
            {
                icms.VICMSSTDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSSTDeson)), ObOp.Opcional, 15);
                icms.MotDesICMSST = (MotivoDesoneracaoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.MotDesICMSST)), ObOp.Opcional, 1, 2);
                if (lenPipesRegistro >= 25) icms.IndDeduzDeson = LerSimNaoOpcional(XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.IndDeduzDeson)));
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMS90 = icms };
        }

        private void ProcessarIcmsPart10_90(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSPart
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Obrigatorio, 1, 1),
                PRedBC = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Opcional, this.CasasDecimais75),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15),
                PBCOp = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMSPart>(nameof(DFeNFe.ICMSPart.PBCOp)), ObOp.Obrigatorio, this.CasasDecimais75),
                UFST = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(XmlTag<DFeNFe.ICMSPart>(nameof(DFeNFe.ICMSPart.UFST)), ObOp.Obrigatorio, 2, 2))
            };
            if (lenPipesRegistro >= 19)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSPart = icms };
        }

        private void ProcessarIcmsSt(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMSST = new DFeNFe.ICMSST
                {
                    Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    VBCSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCSTRet)), ObOp.Obrigatorio, 15),
                    VICMSSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSTRet)), ObOp.Obrigatorio, 15),
                    VBCSTDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSST>(nameof(DFeNFe.ICMSST.VBCSTDest)), ObOp.Obrigatorio, 15),
                    VICMSSTDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSST>(nameof(DFeNFe.ICMSST.VICMSSTDest)), ObOp.Obrigatorio, 15),
                    PST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PST)), ObOp.None, 15, true),
                    VICMSSubstituto = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSubstituto)), ObOp.None, 15, true),
                    VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCFCPSTRet)), ObOp.None, 15, true),
                    PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PFCPSTRet)), ObOp.None, 15, true),
                    VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VFCPSTRet)), ObOp.None, 15, true),
                    PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PRedBCEfet)), ObOp.None, 15, true),
                    VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCEfet)), ObOp.None, 15, true),
                    PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PICMSEfet)), ObOp.None, 15, true),
                    VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSEfet)), ObOp.None, 15, true)
                }
            };
        }

        private void ProcessarIcmsSn101(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1);
            var csosn = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture);

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
                    PCredSN = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.PCredSN)), ObOp.Obrigatorio, this.CasasDecimais75),
                    VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.VCredICMSSN)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarIcmsSn102(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS
            {
                ICMSSN102 = new DFeNFe.ICMSSN102
                {
                    Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                    CSOSN = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture)
                }
            };
        }

        private void ProcessarIcmsSn201(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN201
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15);
            }
            icms.PCredSN = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.PCredSN)), ObOp.Obrigatorio, this.CasasDecimais75);
            icms.VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.VCredICMSSN)), ObOp.Obrigatorio, 15);
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN201 = icms };
        }

        private void ProcessarIcmsSn202(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN202
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Obrigatorio, 1, 1),
                PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75),
                PRedBCST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75),
                VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15),
                PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Obrigatorio, this.CasasDecimais75),
                VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Obrigatorio, 15)
            };
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN202 = icms };
        }

        private void ProcessarIcmsSn500(int nProd, int lenPipesRegistro)
        {
            var origem = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1);
            var csosn = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture);

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
            if (versaoNFe < 3) this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Opcional, 1, 1);
            var baseRetida = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCSTRet)), ObOp.Opcional, 15);
            var icmsRetido = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSTRet)), ObOp.Opcional, 15);
            icms.VBCSTRet = baseRetida > 0 ? (double?)baseRetida : null;
            icms.VICMSSTRet = icmsRetido > 0 ? (double?)icmsRetido : null;
            if (versaoNFe >= 4)
            {
                var percentualSt = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PST)), ObOp.Obrigatorio, 15);
                icms.PST = percentualSt > 0 ? (double?)percentualSt : null;
                icms.VBCFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCFCPSTRet)), ObOp.Obrigatorio, 15);
                icms.PFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PFCPSTRet)), ObOp.Obrigatorio, 15);
                icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VFCPSTRet)), ObOp.Obrigatorio, 15);
                if (lenPipesRegistro >= 13)
                {
                    icms.PRedBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PRedBCEfet)), ObOp.Opcional, 15);
                    icms.VBCEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VBCEfet)), ObOp.Opcional, 15);
                    icms.PICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.PICMSEfet)), ObOp.Opcional, 15);
                    icms.VICMSEfet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSEfet)), ObOp.Opcional, 15);
                }
                var substituto = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VICMSSubstituto)), ObOp.Opcional, 15);
                icms.VICMSSubstituto = substituto > 0 ? (double?)substituto : null;
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN500 = icms };
        }

        private void ProcessarIcmsSn900(int nProd, int lenPipesRegistro)
        {
            var icms = new DFeNFe.ICMSSN900
            {
                Orig = (OrigemMercadoria)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.Orig)), ObOp.Obrigatorio, 1, 1),
                CSOSN = this.LerInt32(XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.CSOSN)), ObOp.Obrigatorio, 3, 3).ToString(CultureInfo.InvariantCulture),
                ModBC = (ModalidadeBaseCalculoICMS)this.LerInt32(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.ModBC)), ObOp.Opcional, 1, 1),
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Opcional, 15, true)
            };
            var reducao = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.PRedBC)), ObOp.Opcional, this.CasasDecimais75);
            icms.PRedBC = reducao > 0 ? (double?)reducao : null;
            icms.PICMS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.PICMS)), ObOp.Opcional, this.CasasDecimais75, true);
            icms.VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Opcional, 15, true);
            icms.ModBCSTField = (ModalidadeBaseCalculoICMSST)this.LerInt32(XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.ModBCST)), ObOp.Opcional, 1, 1, true);
            icms.PMVAST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PMVAST)), ObOp.Opcional, this.CasasDecimais75);
            var reducaoSt = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PRedBCST)), ObOp.Opcional, this.CasasDecimais75);
            icms.PRedBCST = reducaoSt > 0 ? (double?)reducaoSt : null;
            icms.VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Opcional, 15, true);
            icms.PICMSST = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PICMSST)), ObOp.Opcional, this.CasasDecimais75, true);
            icms.VICMSST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VICMSST)), ObOp.Opcional, 15, true);
            icms.PCredSN = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.PCredSN)), ObOp.Opcional, this.CasasDecimais75, true);
            icms.VCredICMSSN = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSSN101>(nameof(DFeNFe.ICMSSN101.VCredICMSSN)), ObOp.Opcional, 15, true);
            if (versaoNFe >= 4)
            {
                icms.VBCFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCFCPST)), ObOp.Obrigatorio, 15);
                icms.PFCPST = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.PFCPST)), ObOp.Obrigatorio, 15);
                icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMS = new DFeNFe.ICMS { ICMSSN900 = icms };
        }

        private void ProcessarDiferimentoIcms(int nProd, int lenPipesRegistro)
        {
            var imposto = new DFeNFe.ICMSUFDest
            {
                VBCUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VBCUFDest)), ObOp.Obrigatorio, 1, 15),
                PFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.PFCPUFDest)), ObOp.Opcional, 1, 8),
                PICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.PICMSUFDest)), ObOp.Opcional, 1, 8),
                PICMSInter = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.PICMSInter)), ObOp.Obrigatorio, 1, 8),
                PICMSInterPart = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.PICMSInterPart)), ObOp.Opcional, 1, 8),
                VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VFCPUFDest)), ObOp.Opcional, 1, 15),
                VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFDest)), ObOp.Obrigatorio, 1, 15),
                VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFRemet)), ObOp.Opcional, 1, 15)
            };
            if (versaoNFe >= 4)
            {
                imposto.VBCFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VBCFCPUFDest)), ObOp.Obrigatorio, 15);
            }
            detalhesOficiais[nProd].Imposto.ICMSUFDest = imposto;
        }

        private void ProcessarIpi(int nProd)
        {
            var cnpjProdutor = this.LerString(XmlTag<DFeNFe.IPI>(nameof(DFeNFe.IPI.CNPJProd)), ObOp.Opcional, 14, 14);
            var codigoSelo = this.LerString(XmlTag<DFeNFe.IPI>(nameof(DFeNFe.IPI.CSelo)), ObOp.Opcional, 1, 60);
            var quantidadeSelo = this.LerInt32(XmlTag<DFeNFe.IPI>(nameof(DFeNFe.IPI.QSelo)), ObOp.Opcional, 1, 12);
            var codigoEnquadramento = this.LerString(XmlTag<DFeNFe.IPI>(nameof(DFeNFe.IPI.CEnq)), ObOp.Obrigatorio, 3, 3);
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
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                VIPI = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.VIPI)), ObOp.Opcional, 15)
            };
        }

        private void ProcessarIpiNaoTributado(int nProd, int lenPipesRegistro)
        {
            ObterIpi(nProd).IPINT = new DFeNFe.IPINT
            {
                CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2)
            };
        }

        private void ProcessarIpiBaseAliquota(int nProd, int lenPipesRegistro)
        {
            var tributado = ObterIpiTributado(nProd);
            tributado.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
            tributado.PIPI = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.PIPI)), ObOp.Obrigatorio, 7);
        }

        private void ProcessarIpiQuantidade(int nProd, int lenPipesRegistro)
        {
            var tributado = ObterIpiTributado(nProd);
            tributado.QUnid = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.QUnid)), ObOp.Obrigatorio, 16);
            tributado.VUnid = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.VUnid)), ObOp.Obrigatorio, 15);
            tributado.VIPI = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.VIPI)), ObOp.Opcional, 15);
        }

        private void ProcessarImpostoImportacao(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.II = new DFeNFe.II
            {
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                VDespAdu = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.II>(nameof(DFeNFe.II.VDespAdu)), ObOp.Obrigatorio, 15),
                VII = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.II>(nameof(DFeNFe.II.VII)), ObOp.Obrigatorio, 15),
                VIOF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.II>(nameof(DFeNFe.II.VIOF)), ObOp.Obrigatorio, 15)
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
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                    PPIS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.PPIS)), ObOp.Obrigatorio, 7),
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarPisQuantidade(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISQtde = new DFeNFe.PISQtde
                {
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16),
                    VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15),
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarPisNaoTributado(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISNT = new DFeNFe.PISNT { CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2) }
            };
        }

        private void ProcessarPisOutros(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.PIS = new DFeNFe.PIS
            {
                PISOutr = new DFeNFe.PISOutr
                {
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    VBC = 0,
                    PPIS = 0,
                    VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Opcional, 15)
                }
            };
        }

        private void ProcessarPisAliquotaRetencao(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisOutros(nProd);
            pis.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
            pis.PPIS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.PPIS)), ObOp.Obrigatorio, 7);
            pis.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarPisQuantidadeRetencao(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisOutros(nProd);
            pis.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 15);
            pis.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15);
            pis.VBC = null;
            pis.PPIS = null;
        }

        private void ProcessarPisSt(int nProd)
        {
            detalhesOficiais[nProd].Imposto.PISST = new DFeNFe.PISST
            {
                VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15)
            };
        }

        private void ProcessarPisStBase(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisSt(nProd);
            pis.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
            pis.PPIS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.PPIS)), ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarPisStQuantidade(int nProd, int lenPipesRegistro)
        {
            var pis = ObterPisSt(nProd);
            pis.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16);
            pis.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15);
            pis.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15);
            pis.VBC = null;
            pis.PPIS = null;
            if (lenPipesRegistro >= 5)
            {
                var indicador = this.LerString(XmlTag<DFeNFe.PISST>(nameof(DFeNFe.PISST.IndSomaPISST)), ObOp.Opcional, 0, 1);
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
        private SimNao LerSimNaoOpcional(string recurso)
#else
        private SimNao? LerSimNaoOpcional(string recurso)
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
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                    PCOFINS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.PCOFINS)), ObOp.Obrigatorio, this.CasasDecimais75),
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsQuantidade(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSQtde = new DFeNFe.COFINSQtde
                {
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16),
                    VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15),
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsNaoTributado(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSNT = new DFeNFe.COFINSNT { CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2) }
            };
        }

        private void ProcessarCofinsOutros(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINS = new DFeNFe.COFINS
            {
                COFINSOutr = new DFeNFe.COFINSOutr
                {
                    CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 2, 2),
                    VBC = 0,
                    PCOFINS = 0,
                    VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15)
                }
            };
        }

        private void ProcessarCofinsAliquotaRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsOutros(nProd);
            cofins.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
            cofins.PCOFINS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.PCOFINS)), ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarCofinsQuantidadeRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsOutros(nProd);
            cofins.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16);
            cofins.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15);
            cofins.VBC = null;
            cofins.PCOFINS = null;
        }

        private void ProcessarIssqn(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.COFINSST = new DFeNFe.COFINSST
            {
                VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15)
            };
        }

        private void ProcessarIssqnValores(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsSt(nProd);
            cofins.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
            cofins.PCOFINS = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.PCOFINS)), ObOp.Obrigatorio, this.CasasDecimais75);
        }

        private void ProcessarIssqnRetencao(int nProd, int lenPipesRegistro)
        {
            var cofins = ObterCofinsSt(nProd);
            cofins.QBCProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.QBCProd)), ObOp.Obrigatorio, 16);
            cofins.VAliqProd = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.CIDE>(nameof(DFeNFe.CIDE.VAliqProd)), ObOp.Obrigatorio, 15);
            cofins.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15);
            cofins.VBC = null;
            cofins.PCOFINS = null;
            if (lenPipesRegistro >= 5)
            {
                var indicador = this.LerString(XmlTag<DFeNFe.COFINSST>(nameof(DFeNFe.COFINSST.IndSomaCOFINSST)), ObOp.Opcional, 0, 1);
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
                VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15),
                VAliq = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VAliq)), ObOp.Obrigatorio, this.CasasDecimais75),
                VISSQN = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VISSQN)), ObOp.Obrigatorio, 15),
                CMunFG = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CMunFG)), ObOp.Obrigatorio, 7, 7)
            };
            var listaServico = this.LerString(XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.CListServ)), ObOp.Obrigatorio, versaoNFe >= 3 ? 5 : 3, versaoNFe >= 3 ? 5 : 4);
            issqn.CListServ = (ListaServicoISSQN)int.Parse(listaServico.Replace(".", string.Empty), CultureInfo.InvariantCulture);
            if (versaoNFe >= 3)
            {
                issqn.VDeducao = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDeducao)), ObOp.Opcional, 15);
                issqn.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VOutro)), ObOp.Opcional, 15);
                issqn.VDescIncond = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDescIncond)), ObOp.Opcional, 15);
                issqn.VDescCond = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDescCond)), ObOp.Opcional, 15);
                issqn.VISSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VISSRet)), ObOp.Opcional, 15);
                issqn.IndISS = (IndicadorExigibilidadeISSQN)this.LerInt32(XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.IndISS)), ObOp.Obrigatorio, 1, 1);
                issqn.CServico = VazioParaNulo(this.LerString(XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.CServico)), ObOp.Opcional, 1, 20));
                issqn.CMun = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CMun)), ObOp.Opcional, 7, 7);
                issqn.CPais = this.LerInt32(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.CPais)), ObOp.Opcional, 4, 4);
                issqn.NProcesso = VazioParaNulo(this.LerString(XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.NProcesso)), ObOp.Opcional, 1, 30));
                issqn.IndIncentivo = this.LerInt32(XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.IndIncentivo)), ObOp.Opcional, 1, 1) == 1 ? SimNao12.Sim : SimNao12.Nao;
            }
            else this.LerString(NFeTxtFieldNames.SituacaoTributaria, ObOp.Obrigatorio, 1, 1);
            detalhesOficiais[nProd].Imposto.ISSQN = issqn;
        }

        private void ProcessarTotalIbsCbs(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].ImpostoDevol = new DFeNFe.ImpostoDevol
            {
                PDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ImpostoDevol>(nameof(DFeNFe.ImpostoDevol.PDevol)), ObOp.Opcional, 5),
                IPI = new DFeNFe.IPIDevol { VIPIDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IPIDevol>(nameof(DFeNFe.IPIDevol.VIPIDevol)), ObOp.Opcional, 5) }
            };
        }

        private void ProcessarTotalIbsCbsDetalhe(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].Imposto.IS = new DFeNFe.IS
            {
                CSTIS = this.LerString(XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.CSTIS)), ObOp.Obrigatorio, 1, 3),
                CClassTribIS = this.LerString(XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.CClassTribIS)), ObOp.Obrigatorio, 1, 6),
                VBCIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.VBCIS)), ObOp.Opcional, 1, 15),
                PIS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Imposto>(nameof(DFeNFe.Imposto.PIS)), ObOp.Opcional, 1, 7),
                AdRemIS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.AdRemIS)), ObOp.Opcional, 1, 7),
                UTrib = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.UTrib)), ObOp.Opcional, 1, 6),
                QTrib = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.QTrib)), ObOp.Opcional, 1, 15),
                VIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.VIS)), ObOp.Obrigatorio, 1, 15)
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
            imposto.CST = this.LerString(XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.CST)), ObOp.Obrigatorio, 1, 3);
            imposto.CClassTrib = this.LerString(XmlTag<DFeNFe.IBSCBS>(nameof(DFeNFe.IBSCBS.CClassTrib)), ObOp.Obrigatorio, 1, 6);
        }

        private void ProcessarTotalIbsCbsDiferimento(int nProd, int lenPipesRegistro)
        {
            //layout = "UB15|vBC|vIBS|"
            var imposto = ObterGIBSCBS(nProd);
            imposto.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 1, 15);
            imposto.VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBS>(nameof(DFeNFe.GIBSCBS.VIBS)), ObOp.Obrigatorio, 15, true);
        }

        private void ProcessarTotalIbsCbsDevolucao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB17|pIBSUF|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSUF|"
            ObterGIBSCBS(nProd).GIBSUF = new DFeNFe.GIBSUF
            {
                PIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GIBSUF>(nameof(DFeNFe.GIBSUF.PIBSUF)), ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PDif)), ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PRedAliq)), ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PAliqEfet)), ObOp.Opcional, 1, 7)
                },
                VIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSUF>(nameof(DFeNFe.GIBSUF.VIBSUF)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsCreditoPresumido(int nProd, int lenPipesRegistro)
        {
            //layout = "UB36|pIBSMun|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vIBSMun|"
            ObterGIBSCBS(nProd).GIBSMun = new DFeNFe.GIBSMun
            {
                PIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GIBSMun>(nameof(DFeNFe.GIBSMun.PIBSMun)), ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PDif)), ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PRedAliq)), ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PAliqEfet)), ObOp.Opcional, 1, 7)
                },
                VIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GIBSMun>(nameof(DFeNFe.GIBSMun.VIBSMun)), ObOp.Obrigatorio, 1, 7)
            };
        }

        private void ProcessarTotalIbsCbsReducao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB55|pCBS|pDif|vDif|pDevTrib|vDevTrib|pRedAliq|pAliqEfet|vCBS|"
            ObterGIBSCBS(nProd).GCBS = new DFeNFe.GCBS
            {
                PCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GCBS>(nameof(DFeNFe.GCBS.PCBS)), ObOp.Obrigatorio, 1, 7),
                GDif = new DFeNFe.GDif
                {
                    PDif = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS51>(nameof(DFeNFe.ICMS51.PDif)), ObOp.Opcional, 1, 7),
                    VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Opcional, 1, 15)
                },
                GDevTrib = CriarGrupoDevolucaoTributos(),
                GRed = new DFeNFe.GRed
                {
                    PRedAliq = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PRedAliq)), ObOp.Opcional, 1, 7),
                    PAliqEfet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GRed>(nameof(DFeNFe.GRed.PAliqEfet)), ObOp.Opcional, 1, 7)
                },
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GCBS>(nameof(DFeNFe.GCBS.VCBS)), ObOp.Obrigatorio, 1, 7)
            };
        }

        private DFeNFe.GDevTrib CriarGrupoDevolucaoTributos()
        {
            var percentualDevolucao = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GDevTrib>(nameof(DFeNFe.GDevTrib.PDevTrib)), ObOp.Opcional, 7, true);
            var valorDevolvido = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDevTrib>(nameof(DFeNFe.GDevTrib.VDevTrib)), ObOp.Opcional, 15, true);
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
                TpALCZFMCBS = (TipoAplicacaoAliquotaZeroCBS)this.LerInt32(XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.TpALCZFMCBS)), ObOp.Obrigatorio, 1, 1),
                NProcSuframa = VazioParaNulo(this.LerString(XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.NProcSuframa)), ObOp.Opcional, 8, 12)),
                PAliqEfetRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.PAliqEfetRegCBS)), ObOp.Obrigatorio, 1, 7),
                VTribRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.VTribRegCBS)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsRegularCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB68|CSTReg|cClassTribReg|pAliqEfetRegIBSUF|vTribRegIBSUF|pAliqEfetRegIBSMun|vTribRegIBSMun|pAliqEfetRegCBS|vTribRegCBS|"
            ObterGIBSCBS(nProd).GTribRegular = new DFeNFe.GTribRegular
            {
                CSTReg = this.LerString(XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.CSTReg)), ObOp.Obrigatorio, 1, 3),
                CClassTribReg = this.LerString(XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.CClassTribReg)), ObOp.Obrigatorio, 1, 6),
                PAliqEfetRegIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.PAliqEfetRegIBSUF)), ObOp.Obrigatorio, 1, 7),
                VTribRegIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.VTribRegIBSUF)), ObOp.Obrigatorio, 1, 15),
                PAliqEfetRegIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.PAliqEfetRegIBSMun)), ObOp.Obrigatorio, 1, 7),
                VTribRegIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GTribRegular>(nameof(DFeNFe.GTribRegular.VTribRegIBSMun)), ObOp.Obrigatorio, 1, 15),
                PAliqEfetRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.PAliqEfetRegCBS)), ObOp.Obrigatorio, 1, 7),
                VTribRegCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GALCZFMCBS>(nameof(DFeNFe.GALCZFMCBS.VTribRegCBS)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsDiferimentoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = UB82|pAliqIBSUF|vTribIBSUF|pAliqIBSMun|vTribIBSMun|pAliqCBS|vTribCBS|
            ObterGIBSCBS(nProd).GTribCompraGov = new DFeNFe.GTribCompraGov
            {
                PAliqIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.PAliqIBSUF)), ObOp.Obrigatorio, 1, 7),
                VTribIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.VTribIBSUF)), ObOp.Obrigatorio, 1, 15),
                PAliqIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.PAliqIBSMun)), ObOp.Obrigatorio, 1, 7),
                VTribIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.VTribIBSMun)), ObOp.Obrigatorio, 1, 15),
                PAliqCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.PAliqCBS)), ObOp.Obrigatorio, 1, 7),
                VTribCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GTribCompraGov>(nameof(DFeNFe.GTribCompraGov.VTribCBS)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsDevolucaoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB84|vTotIBSMonoItem|vTotCBSMonoItem|"
            var monofasico = ObterGIBSCBSMono(nProd);
            monofasico.VTotIBSMonoItem = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBSMono>(nameof(DFeNFe.GIBSCBSMono.VTotIBSMonoItem)), ObOp.Opcional, 1, 15);
            monofasico.VTotCBSMonoItem = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBSMono>(nameof(DFeNFe.GIBSCBSMono.VTotCBSMonoItem)), ObOp.Opcional, 1, 15);
        }

        private void ProcessarTotalIbsCbsCreditoPresumidoCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB85|qBCMono|adRemIBS|adRemCBS|vIBSMono|vCBSMono|"
            ObterGIBSCBSMono(nProd).GMonoPadrao = new DFeNFe.GMonoPadrao
            {
                QBCMono = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.QBCMono)), ObOp.Opcional, 1, 15),
                AdRemIBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.AdRemIBS)), ObOp.Opcional, 1, 7),
                AdRemCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.AdRemCBS)), ObOp.Opcional, 1, 7),
                VIBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.VIBSMono)), ObOp.Opcional, 1, 15),
                VCBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.VCBSMono)), ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsTribRegular(int nProd, int lenPipesRegistro)
        {
            //layout = "UB91|qBCMonoReten|adRemIBSReten|vIBSMonoReten|adRemCBSReten|vCBSMonoReten|"
            ObterGIBSCBSMono(nProd).GMonoReten = new DFeNFe.GMonoReten
            {
                QBCMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.QBCMonoReten)), ObOp.Opcional, 1, 15),
                AdRemIBSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.AdRemIBSReten)), ObOp.Opcional, 1, 7),
                VIBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.VIBSMonoReten)), ObOp.Opcional, 1, 15),
                AdRemCBSReten = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.AdRemCBSReten)), ObOp.Opcional, 1, 7),
                VCBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.VCBSMonoReten)), ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotalIbsCbsTribRegularCompraGov(int nProd, int lenPipesRegistro)
        {
            //layout = "UB95|qBCMonoRet|adRemIBSRet|vIBSMonoRet|adRemCBSRet|vCBSMonoRet|"
            ObterGIBSCBSMono(nProd).GMonoRet = new DFeNFe.GMonoRet
            {
                QBCMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.QBCMonoRet)), ObOp.Opcional, 1, 15),
                AdRemIBSRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.AdRemIBSRet)), ObOp.Opcional, 1, 7),
                VIBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.VIBSMonoRet)), ObOp.Opcional, 1, 15),
                AdRemCBSRet = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.AdRemCBSRet)), ObOp.Opcional, 1, 7),
                VCBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.VCBSMonoRet)), ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTotaisRetencao(int nProd, int lenPipesRegistro)
        {
            var observacao = ObterObservacaoItem(nProd);
            if (observacao.ObsCont == null) observacao.ObsCont = new List<DFeNFe.ObsCont>();
            observacao.ObsCont.Add(new DFeNFe.ObsCont
            {
                XCampo = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XCampo)), ObOp.Obrigatorio, 1, 20),
                XTexto = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XTexto)), ObOp.Obrigatorio, 1, 60)
            });
        }

        private void ProcessarTotaisRetencaoCbs(int nProd, int lenPipesRegistro)
        {
            var observacao = ObterObservacaoItem(nProd);
            if (observacao.ObsFisco == null) observacao.ObsFisco = new List<DFeNFe.ObsFisco>();
            observacao.ObsFisco.Add(new DFeNFe.ObsFisco
            {
                XCampo = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XCampo)), ObOp.Obrigatorio, 1, 20),
                XTexto = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XTexto)), ObOp.Obrigatorio, 1, 60)
            });
        }

        private void ProcessarTotaisMonofasico(int nProd, int lenPipesRegistro)
        {
            detalhesOficiais[nProd].VItem = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Det>(nameof(DFeNFe.Det.VItem)), ObOp.Opcional, 15);
        }

        private void ProcessarTotaisMonofasicoRetido(int nProd, int lenPipesRegistro)
        {
            var chaveAcesso = this.LerString(XmlTag<DFeNFe.DFeReferenciado>(nameof(DFeNFe.DFeReferenciado.ChaveAcesso)), ObOp.Obrigatorio, 1, 44);
            var numeroItem = this.LerInt32(NFeTxtFieldNames.NumeroItem, ObOp.Opcional, 1, 3);
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
                PDifIBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoDif>(nameof(DFeNFe.GMonoDif.PDifIBS)), ObOp.Opcional, 1, 7),
                VIBSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoDif>(nameof(DFeNFe.GMonoDif.VIBSMonoDif)), ObOp.Opcional, 1, 15),
                PDifCBS = this.LerDouble(TpcnTipoCampo.tcDouble4, XmlTag<DFeNFe.GMonoDif>(nameof(DFeNFe.GMonoDif.PDifCBS)), ObOp.Opcional, 1, 7),
                VCBSMonoDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoDif>(nameof(DFeNFe.GMonoDif.VCBSMonoDif)), ObOp.Opcional, 1, 15)
            };
        }

        private void ProcessarTransferenciaCredito(int nProd, int lenPipesRegistro)
        {
            //layout = UB106|vIBS|vCBS|
            ObterIBSCBS(nProd).GTransfCred = new DFeNFe.GTransfCred
            {
                VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBS>(nameof(DFeNFe.GIBSCBS.VIBS)), ObOp.Obrigatorio, 1, 15),
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCBS>(nameof(DFeNFe.GCBS.VCBS)), ObOp.Obrigatorio, 1, 15)
            };
        }

        private void ProcessarAjusteCompetencia(int nProd, int lenPipesRegistro)
        {
            var competencia = this.LerString(XmlTag<DFeNFe.GAjusteCompet>(nameof(DFeNFe.GAjusteCompet.CompetApur)), ObOp.Obrigatorio, 7, 7);
            ObterIBSCBS(nProd).GAjusteCompet = new DFeNFe.GAjusteCompet
            {
                CompetApur = DateTime.ParseExact(competencia, "yyyy-MM", CultureInfo.InvariantCulture),
                VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBS>(nameof(DFeNFe.GIBSCBS.VIBS)), ObOp.Obrigatorio, 1, 13),
                VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCBS>(nameof(DFeNFe.GCBS.VCBS)), ObOp.Obrigatorio, 1, 13)
            };
        }

        private void ProcessarIndicadorDoacao(int nProd, int lenPipesRegistro)
        {
            //layout = "UB14a|indDoacao|;
            var indicador = this.LerString(XmlTag<DFeNFe.IBSCBS>(nameof(DFeNFe.IBSCBS.IndDoacao)), ObOp.Opcional, 1, 1);
            ObterIBSCBS(nProd).IndDoacao = int.TryParse(indicador, out var valor) ? valor : 0;
        }

        private void ProcessarEstornoCredito(int nProd, int lenPipesRegistro)
        {
            var valorCbs = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GEstornoCred>(nameof(DFeNFe.GEstornoCred.VCBSEstCred)), ObOp.None, 13, true);
            ObterIBSCBS(nProd).GEstornoCred = new DFeNFe.GEstornoCred
            {
                VCBSEstCred = valorCbs,
                VIBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GEstornoCred>(nameof(DFeNFe.GEstornoCred.VIBSEstCred)), ObOp.None, 13, true)
            };
        }

        private void ProcessarCreditoPresumidoOperacao(int nProd, int lenPipesRegistro)
        {
            var credito = ObterGCredPresOper(nProd);
            credito.VBCCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCredPresOper>(nameof(DFeNFe.GCredPresOper.VBCCredPres)), ObOp.Obrigatorio, 1, 13);
            credito.CCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCredPresOper>(nameof(DFeNFe.GCredPresOper.CCredPres)), ObOp.Obrigatorio, 1, 2).ToString("F2", CultureInfo.InvariantCulture);
        }

        private void ProcessarCreditoPresumidoIbs(int nProd, int lenPipesRegistro)
        {
            ObterGCredPresOper(nProd).GIBSCredPres = new DFeNFe.GIBSCredPres
            {
                PCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.PCredPres)), ObOp.Opcional, 1, 4),
                VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPres)), ObOp.Opcional, 1, 13),
                VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPresCondSus)), ObOp.Opcional, 1, 13)
            };
        }

        private void ProcessarCreditoPresumidoCbs(int nProd, int lenPipesRegistro)
        {
            ObterGCredPresOper(nProd).GCBSCredPres = new DFeNFe.GCBSCredPres
            {
                PCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.PCredPres)), ObOp.Opcional, 1, 4),
                VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPres)), ObOp.Opcional, 1, 13),
                VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPresCondSus)), ObOp.Opcional, 1, 13)
            };
        }

        private void ProcessarCreditoPresumidoZfm(int nProd, int lenPipesRegistro)
        {
            //layout = UB109|tpCredPresIBSZFM|vCredPresIBSZFM|
            var competencia = this.LerString(XmlTag<DFeNFe.GAjusteCompet>(nameof(DFeNFe.GAjusteCompet.CompetApur)), ObOp.Obrigatorio, 7, 7);
            var tipoCredito = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.TpCredPresIBSZFM)), ObOp.Obrigatorio, 1, 1);
            ObterIBSCBS(nProd).GCredPresIBSZFM = new DFeNFe.GCredPresIBSZFM
            {
                CompetApur = DateTime.ParseExact(competencia, "yyyy-MM", CultureInfo.InvariantCulture),
                TpCredPresIBSZFM = (TipoCreditoPresumidoIBSZFM)int.Parse(tipoCredito),
                VCredPresIBSZFM = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCredPresIBSZFM>(nameof(DFeNFe.GCredPresIBSZFM.VCredPresIBSZFM)), ObOp.Opcional, 1, 15)
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
                    icms.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Obrigatorio, 15);
                    icms.VICMS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VICMS)), ObOp.Obrigatorio, 15);
                    icms.VBCST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VBCST)), ObOp.Obrigatorio, 15);
                    icms.VST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSTot>(nameof(DFeNFe.ICMSTot.VST)), ObOp.Obrigatorio, 15);
                    icms.VProd = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VProd)), ObOp.Obrigatorio, 15);
                    icms.VFrete = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VFrete)), ObOp.Obrigatorio, 15);
                    icms.VSeg = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VSeg)), ObOp.Obrigatorio, 15);
                    icms.VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VDesc)), ObOp.Obrigatorio, 15);
                    icms.VII = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.II>(nameof(DFeNFe.II.VII)), ObOp.Obrigatorio, 15);
                    icms.VIPI = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IPITrib>(nameof(DFeNFe.IPITrib.VIPI)), ObOp.Obrigatorio, 15);
                    icms.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Obrigatorio, 15);
                    icms.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Obrigatorio, 15);
                    icms.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VOutro)), ObOp.Obrigatorio, 15);
                    icms.VNF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSTot>(nameof(DFeNFe.ICMSTot.VNF)), ObOp.Obrigatorio, 15);
                    icms.VTotTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Imposto>(nameof(DFeNFe.Imposto.VTotTrib)), ObOp.Opcional, 15);



                    icms.VICMSDeson = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS20>(nameof(DFeNFe.ICMS20.VICMSDeson)), ObOp.Opcional, 15);

                    if (versaoNFe >= 3 && lenPipesRegistro > 17)
                    {
                        icms.VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFDest)), ObOp.Opcional, 15);
                        icms.VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VFCPUFDest)), ObOp.Opcional, 15);
                        icms.VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFRemet)), ObOp.Opcional, 15);

                        if (versaoNFe >= 4)
                        {
                            icms.VFCP = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VFCP)), ObOp.Opcional, 15);
                            icms.VFCPST = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS10>(nameof(DFeNFe.ICMS10.VFCPST)), ObOp.Opcional, 15);
                            icms.VFCPSTRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS60>(nameof(DFeNFe.ICMS60.VFCPSTRet)), ObOp.Opcional, 15);
                            icms.VIPIDevol = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IPIDevol>(nameof(DFeNFe.IPIDevol.VIPIDevol)), ObOp.Opcional, 15);
                            icms.QBCMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.QBCMono)), ObOp.Opcional, 15);
                            icms.VICMSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS02>(nameof(DFeNFe.ICMS02.VICMSMono)), ObOp.Opcional, 15);
                            icms.QBCMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.QBCMonoReten)), ObOp.Opcional, 15);
                            icms.VICMSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS15>(nameof(DFeNFe.ICMS15.VICMSMonoReten)), ObOp.Opcional, 15);
                            icms.QBCMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.QBCMonoRet)), ObOp.Opcional, 15);
                            icms.VICMSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS61>(nameof(DFeNFe.ICMS61.VICMSMonoRet)), ObOp.Opcional, 15);
                        }
                    }
                    #endregion
        }

        private void ProcessarTotaisIcmsSt(int nProd, int lenPipesRegistro)
        {
                    //layout = prefix + this.FSegmento + "|vICMSUFDest|vICMSUFRemet|vFCPUFDest";
                    totalOficial.ICMSTot.VICMSUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFDest)), ObOp.Opcional, 15);
                    totalOficial.ICMSTot.VICMSUFRemet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VICMSUFRemet)), ObOp.Opcional, 15);
                    totalOficial.ICMSTot.VFCPUFDest = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMSUFDest>(nameof(DFeNFe.ICMSUFDest.VFCPUFDest)), ObOp.Opcional, 15);
        }

        private void ProcessarTotaisFcp(int nProd, int lenPipesRegistro)
        {
                    //layout = (NFe.infNFe.Versao >= 3 ?
                    //            "§W17|VServ|VBC|VISS|VPIS|VCOFINS|dCompet|vDeducao|vOutro|vDescIncond|vDescCond|vISSRet|cRegTrib" :
                    //            "§W17|VServ|VBC|VISS|VPIS|VCOFINS");
                    // Grupo da TAG <total><ISSQNtot>.
                    #region <total><ISSQNtot>
                    var issqn = totalOficial.ISSQNtot;
                    issqn.VServ = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQNtot>(nameof(DFeNFe.ISSQNtot.VServ)), ObOp.Opcional, 15);
                    issqn.VBC = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ICMS00>(nameof(DFeNFe.ICMS00.VBC)), ObOp.Opcional, 15);
                    issqn.VISS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQNtot>(nameof(DFeNFe.ISSQNtot.VISS)), ObOp.Opcional, 15);
                    issqn.VPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.PISAliq>(nameof(DFeNFe.PISAliq.VPIS)), ObOp.Opcional, 15);
                    issqn.VCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.COFINSAliq>(nameof(DFeNFe.COFINSAliq.VCOFINS)), ObOp.Opcional, 15);

                    if ((double)versaoNFe >= 3.10)
                    {
                        issqn.DCompet = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.ISSQNtot>(nameof(DFeNFe.ISSQNtot.DCompet)), ObOp.Opcional, 10, 10, true, false);
                        issqn.VDeducao = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDeducao)), ObOp.Opcional, 15);
                        issqn.VOutro = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VOutro)), ObOp.Opcional, 15);
                        issqn.VDescIncond = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDescIncond)), ObOp.Opcional, 15);
                        issqn.VDescCond = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VDescCond)), ObOp.Opcional, 15);
                        issqn.VISSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQN>(nameof(DFeNFe.ISSQN.VISSRet)), ObOp.Opcional, 15);
                        issqn.CRegTrib = (CodigoRegimeEspecialTributacao)this.LerInt32(XmlTag<DFeNFe.ISSQNtot>(nameof(DFeNFe.ISSQNtot.CRegTrib)), ObOp.Opcional, 1, 1);
                    }
                    #endregion
        }

        private void ProcessarTotaisIpi(int nProd, int lenPipesRegistro)
        {
                    //layout = "§W23|VRetPIS|VRetCOFINS|VRetCSLL|VBCIRRF|VIRRF|VBCRetPrev|VRetPrev"; //ok
                    // Grupo da TAG <total><retTrib>.
                    #region <total><retTrib>
                    var retencoes = totalOficial.RetTrib;
                    retencoes.VRetPIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VRetPIS)), ObOp.Opcional, 15);
                    retencoes.VRetCOFINS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VRetCOFINS)), ObOp.Opcional, 15);
                    retencoes.VRetCSLL = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VRetCSLL)), ObOp.Opcional, 15);
                    retencoes.VBCIRRF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VBCIRRF)), ObOp.Opcional, 15);
                    retencoes.VIRRF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VIRRF)), ObOp.Opcional, 15);
                    retencoes.VBCRetPrev = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VBCRetPrev)), ObOp.Opcional, 15);
                    retencoes.VRetPrev = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTrib>(nameof(DFeNFe.RetTrib.VRetPrev)), ObOp.Opcional, 15);
                    #endregion
        }

        private void ProcessarTotaisPis(int nProd, int lenPipesRegistro)
        {
                    //layout = "W31|vIS|"

                    totalOficial.ISTot.VIS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IS>(nameof(DFeNFe.IS.VIS)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisCofins(int nProd, int lenPipesRegistro)
        {
                    //layout = "W34|vBCIBSCBS|"

                    totalOficial.IBSCBSTot.VBCIBSCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.IBSCBSTot>(nameof(DFeNFe.IBSCBSTot.VBCIBSCBS)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisIssqn(int nProd, int lenPipesRegistro)
        {
                    //layout = "W36|vIBS|vCredPres|vCredPresCondSus|"

                    totalOficial.IBSCBSTot.GIBS.VIBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCBS>(nameof(DFeNFe.GIBSCBS.VIBS)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPres)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPresCondSus)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisRetencoes(int nProd, int lenPipesRegistro)
        {
                    //layout = "W37|vDif|vDevTrib|vIBSUF|"

                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDevTrib>(nameof(DFeNFe.GDevTrib.VDevTrib)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSUF.VIBSUF = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSUF>(nameof(DFeNFe.GIBSUF.VIBSUF)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisTributos(int nProd, int lenPipesRegistro)
        {
                    //layout = "W42|vDif|vDevTrib|vIBSMun|"

                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDevTrib>(nameof(DFeNFe.GDevTrib.VDevTrib)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GIBS.GIBSMun.VIBSMun = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSMun>(nameof(DFeNFe.GIBSMun.VIBSMun)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisIcmsUfDest(int nProd, int lenPipesRegistro)
        {
                    //layout = "W50|vDif|vDevTrib|vCBS|vCredPres|vCredPresCondSus|"

                    totalOficial.IBSCBSTot.GCBS.VDif = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDif>(nameof(DFeNFe.GDif.VDif)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VDevTrib = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GDevTrib>(nameof(DFeNFe.GDevTrib.VDevTrib)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCBS = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GCBS>(nameof(DFeNFe.GCBS.VCBS)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCredPres = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPres)), ObOp.Obrigatorio, 15);
                    totalOficial.IBSCBSTot.GCBS.VCredPresCondSus = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GIBSCredPres>(nameof(DFeNFe.GIBSCredPres.VCredPresCondSus)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisFcpUfDest(int nProd, int lenPipesRegistro)
        {
                    //layout = "W57|vIBSMono|vCBSMono|vIBSMonoReten|vCBSMonoReten|vIBSMonoRet|vCBSMonoRet|"

                    var monofasia = totalOficial.IBSCBSTot.GMono;
                    monofasia.VIBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.VIBSMono)), ObOp.Obrigatorio, 15);
                    monofasia.VCBSMono = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoPadrao>(nameof(DFeNFe.GMonoPadrao.VCBSMono)), ObOp.Obrigatorio, 15);
                    monofasia.VIBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.VIBSMonoReten)), ObOp.Obrigatorio, 15);
                    monofasia.VCBSMonoReten = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoReten>(nameof(DFeNFe.GMonoReten.VCBSMonoReten)), ObOp.Obrigatorio, 15);
                    monofasia.VIBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.VIBSMonoRet)), ObOp.Obrigatorio, 15);
                    monofasia.VCBSMonoRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GMonoRet>(nameof(DFeNFe.GMonoRet.VCBSMonoRet)), ObOp.Obrigatorio, 15);
        }

        private void ProcessarTotaisFcpUfRemet(int nProd, int lenPipesRegistro)
        {
                    totalOficial.IBSCBSTot.GEstornoCred = new DFeNFe.GEstornoCred
                    {
                        VIBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GEstornoCred>(nameof(DFeNFe.GEstornoCred.VIBSEstCred)), ObOp.Opcional, 1, 13),
                        VCBSEstCred = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.GEstornoCred>(nameof(DFeNFe.GEstornoCred.VCBSEstCred)), ObOp.Opcional, 1, 13)
                    };
        }

        private void ProcessarTotaisIbsCbs(int nProd, int lenPipesRegistro)
        {
                    //layout = "W60|vNFTot|"

                    totalOficial.VNFTot = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Total>(nameof(DFeNFe.Total.VNFTot)), ObOp.Opcional, 15);
        }

        private void ProcessarTransporte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X|modFrete"; //ok
                    // Grupo da TAG <transp>.
                    transporteOficial.ModFrete = (ModalidadeFrete)this.LerInt32(XmlTag<DFeNFe.Transp>(nameof(DFeNFe.Transp.ModFrete)), ObOp.Obrigatorio, 1, 1);
        }

        private void ProcessarTransportador(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X03|xNome|IE|xEnder|UF|xMun"; //ok - alterado em 18/3/15
                    //layout = "§X03|xNome|IE|xEnder|xMun|UF"; //ok
                    // Grupo da TAG <transp><transporta>.
                    #region <transp><TRansportadora>
                    var transportador = ObterTransportador();
                    transportador.XNome = this.LerString(XmlTag<DFeNFe.Emit>(nameof(DFeNFe.Emit.XNome)), ObOp.Opcional, 1, 60);
                    transportador.IE = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.IE)), ObOp.Opcional, 0, 14);
                    transportador.XEnder = this.LerString(XmlTag<DFeNFe.Transporta>(nameof(DFeNFe.Transporta.XEnder)), ObOp.Opcional, 1, 60);
                    transportador.XMun = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.XMun)), ObOp.Opcional, 1, 60);
#if INTEROP
                    transportador.UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Opcional) ?? UFBrasil.NaoDefinido;
#else
                    transportador.UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Opcional);
#endif
                    #endregion
        }

        private void ProcessarVeiculoTransporte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X04|CNPJ"; //ok

                    ObterTransportador().CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Opcional, 14, 14);
        }

        private void ProcessarReboque(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X05|CPF"; //ok

                    ObterTransportador().CPF = this.LerString(XmlTag<DFeNFe.RefNFP>(nameof(DFeNFe.RefNFP.CPF)), ObOp.Opcional, 11, 11);
        }

        private void ProcessarVolume(int nProd, int lenPipesRegistro)
        {
                    //layout = "§X11|VServ|VBCRet|PICMSRet|VICMSRet|CFOP|CMunFG"; //ok
                    // Grupo da TAG <transp><retTransp>.
                    #region <transp><retTransp>
                    transporteOficial.RetTransp = new DFeNFe.RetTransp
                    {
                        VServ = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.ISSQNtot>(nameof(DFeNFe.ISSQNtot.VServ)), ObOp.Obrigatorio, 15),
                        VBCRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTransp>(nameof(DFeNFe.RetTransp.VBCRet)), ObOp.Obrigatorio, 15),
                        PICMSRet = this.LerDouble(this.TipoCampo42, XmlTag<DFeNFe.RetTransp>(nameof(DFeNFe.RetTransp.PICMSRet)), ObOp.Obrigatorio, this.CasasDecimais75),
                        VICMSRet = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.RetTransp>(nameof(DFeNFe.RetTransp.VICMSRet)), ObOp.Obrigatorio, 15),
                        CFOP = this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.CFOP)), ObOp.Obrigatorio, 4, 4),
                        CMunFG = this.LerInt32(XmlTag<DFeNFe.Ide>(nameof(DFeNFe.Ide.CMunFG)), ObOp.Obrigatorio, 7, 7)
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
                        Placa = this.LerString(XmlTag<DFeNFe.VeiculoBase>(nameof(DFeNFe.VeiculoBase.Placa)), ObOp.Obrigatorio, 1, 8),
#if INTEROP
                        UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio) ?? UFBrasil.NaoDefinido,
#else
                        UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio),
#endif
                        RNTC = VazioParaNulo(this.LerString(XmlTag<DFeNFe.VeiculoBase>(nameof(DFeNFe.VeiculoBase.RNTC)), ObOp.Opcional, 1, 20))
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
                        Placa = this.LerString(XmlTag<DFeNFe.VeiculoBase>(nameof(DFeNFe.VeiculoBase.Placa)), ObOp.Obrigatorio, 1, 8),
#if INTEROP
                        UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio) ?? UFBrasil.NaoDefinido,
#else
                        UF = LerUF(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.UF)), ObOp.Obrigatorio),
#endif
                        RNTC = VazioParaNulo(this.LerString(XmlTag<DFeNFe.VeiculoBase>(nameof(DFeNFe.VeiculoBase.RNTC)), ObOp.Opcional, 1, 20))
                    });
                    if (versaoNFe >= 3)
                    {
                        transporteOficial.Vagao = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Transp>(nameof(DFeNFe.Transp.Vagao)), ObOp.Opcional, 1, 20));
                        transporteOficial.Balsa = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Transp>(nameof(DFeNFe.Transp.Balsa)), ObOp.Opcional, 1, 20));
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
                        QVol = this.LerInt32(XmlTag<DFeNFe.Vol>(nameof(DFeNFe.Vol.QVol)), ObOp.Obrigatorio, 1, 15),
                        Esp = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Vol>(nameof(DFeNFe.Vol.Esp)), ObOp.Opcional, 1, 60)),
                        Marca = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Vol>(nameof(DFeNFe.Vol.Marca)), ObOp.Opcional, 1, 60)),
                        NVol = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Vol>(nameof(DFeNFe.Vol.NVol)), ObOp.Opcional, 1, 60)),
                        PesoL = this.LerDouble(TpcnTipoCampo.tcDouble3, XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.PesoL)), ObOp.Opcional, 15),
                        PesoB = this.LerDouble(TpcnTipoCampo.tcDouble3, XmlTag<DFeNFe.VeicProd>(nameof(DFeNFe.VeicProd.PesoB)), ObOp.Opcional, 15)
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
                        NLacre = this.LerString(XmlTag<DFeNFe.Lacres>(nameof(DFeNFe.Lacres.NLacre)), ObOp.Obrigatorio, 1, 60)
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

        private UFBrasil? LerUF(string campo, ObOp obrigatoriedade)
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
                        NFat = this.LerString(XmlTag<DFeNFe.Fat>(nameof(DFeNFe.Fat.NFat)), ObOp.Opcional, 1, 60),
                        VOrig = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Fat>(nameof(DFeNFe.Fat.VOrig)), ObOp.Opcional, 15),
                        VDesc = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.VDesc)), ObOp.None, 15, true),
                        VLiq = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Fat>(nameof(DFeNFe.Fat.VLiq)), ObOp.Opcional, 15)
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
                        duplicata.NDup = this.LerString(XmlTag<DFeNFe.Dup>(nameof(DFeNFe.Dup.NDup)), ObOp.Opcional, 1, 3);
                    else
                        duplicata.NDup = this.LerString(XmlTag<DFeNFe.Dup>(nameof(DFeNFe.Dup.NDup)), ObOp.Opcional, 1, 60);
                    duplicata.DVenc = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Dup>(nameof(DFeNFe.Dup.DVenc)), ObOp.Opcional, 10, 10, true, false);
                    duplicata.VDup = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Dup>(nameof(DFeNFe.Dup.VDup)), ObOp.Opcional, 15);
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
                    pagamentoLegado.indPag = (TpcnIndicadorPagamento)this.LerInt32(XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.IndPag)), ObOp.Opcional, 1, 1, true);
                    pagamentoLegado.tPag = (TpcnFormaPagamento)this.LerInt32(XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.TPag)), ObOp.Obrigatorio, 2, 2);
                    pagamentoLegado.xPag = this.LerString(XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.XPag)), ObOp.Opcional, 0, 60);
                    pagamentoLegado.vPag = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.VPag)), ObOp.Obrigatorio, 15);

                    if (lenPipesRegistro >= 14)
                    {
                        pagamentoLegado.dPag = (DateTime)this.LerCampo(TpcnTipoCampo.tcDatYYYY_MM_DD, XmlTag<DFeNFe.Avulsa>(nameof(DFeNFe.Avulsa.DPag)), ObOp.Opcional, 10, 10, true, false);
                        pagamentoLegado.CNPJPag = this.LerString(XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.CNPJPag)), ObOp.Opcional, 14, 14);
                        pagamentoLegado.UFPag = this.LerString(XmlTag<DFeNFe.DetPag>(nameof(DFeNFe.DetPag.UFPag)), ObOp.Opcional, 1, 2);
                    }

                    pagamentoLegado.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Opcional, 14, 14);
                    pagamentoLegado.tBand = (TpcnBandeiraCartao)this.LerInt32(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.TBand)), ObOp.Opcional, 2, 2);
                    pagamentoLegado.cAut = this.LerString(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.CAut)), ObOp.Opcional, 1, 128);
                    pagamentoLegado.tpIntegra = this.LerInt32(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.TpIntegra)), ObOp.Opcional, 1, 1);

                    if (lenPipesRegistro >= 14)
                    {
                        pagamentoLegado.CNPJReceb = this.LerString(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.CNPJReceb)), ObOp.Opcional, 14, 14);
                        pagamentoLegado.idTermPag = this.LerString(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.IdTermPag)), ObOp.Opcional, 0, 40);
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
                    this.nfeOficial.InfNFeField.Pag.VTroco = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Pag>(nameof(DFeNFe.Pag.VTroco)), ObOp.Opcional, 15);
        }

        private void ProcessarTipoIntegracaoPagamento()
        {
            var tipoIntegracao = this.LerInt32(XmlTag<DFeNFe.Card>(nameof(DFeNFe.Card.TpIntegra)), ObOp.Obrigatorio, 1, 1);
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
                        CNPJ = LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 1, 14, true),
                        IdCadIntTran = LerString(XmlTag<DFeNFe.InfIntermed>(nameof(DFeNFe.InfIntermed.IdCadIntTran)), ObOp.Obrigatorio, 1, 60, true)
                    };
        }

        private void ProcessarInformacoesAdicionais(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z|InfAdFisco|InfCpl"; //ok
                    // Grupo da TAG <infAdic>.
                    #region <InfAdic>
                    var informacoes = CriarInformacoesAdicionais();
                    informacoes.InfAdFisco += this.LerString(XmlTag<DFeNFe.InfAdic>(nameof(DFeNFe.InfAdic.InfAdFisco)), ObOp.Opcional, 1, 2000, false);
                    informacoes.InfCpl += this.LerString(XmlTag<DFeNFe.InfAdic>(nameof(DFeNFe.InfAdic.InfCpl)), ObOp.Opcional, 1, 5000, false);
                    #endregion
        }

        private void AdicionarObservacaoContribuinte(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z04|XCampo|XTexto"; //ok
                    // Grupo da TAG <infAdic><obsCont>.
                    #region <infAdic><obsCont>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ObsCont == null) informacoes.ObsCont = new List<DFeNFe.ObsCont>();
                    informacoes.ObsCont.Add(new DFeNFe.ObsCont { XCampo = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XCampo)), ObOp.Obrigatorio, 1, 20), XTexto = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XTexto)), ObOp.Obrigatorio, 1, 60) });
                    #endregion
        }

        private void AdicionarObservacaoFisco(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z07|XCampo|XTexto"; //ok - ?
                    // Grupo da TAG <infAdic><obsFisco>.
                    #region <infAdic><obsFisco>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ObsFisco == null) informacoes.ObsFisco = new List<DFeNFe.ObsFisco>();
                    informacoes.ObsFisco.Add(new DFeNFe.ObsFisco { XCampo = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XCampo)), ObOp.Obrigatorio, 1, 20), XTexto = this.LerString(XmlTag<DFeNFe.ObsCont>(nameof(DFeNFe.ObsCont.XTexto)), ObOp.Obrigatorio, 1, 60) });
                    #endregion
        }

        private void AdicionarProcessoReferenciado(int nProd, int lenPipesRegistro)
        {
                    //layout = "§Z10|NProc|IndProc"; //ok
                    // Grupo da TAG <infAdic><procRef>.
                    #region <infAdic><procRef>
                    var informacoes = CriarInformacoesAdicionais();
                    if (informacoes.ProcRef == null) informacoes.ProcRef = new List<DFeNFe.ProcRef>();
                    var processo = new DFeNFe.ProcRef { NProc = this.LerString(XmlTag<DFeNFe.ProcRef>(nameof(DFeNFe.ProcRef.NProc)), ObOp.Obrigatorio, 1, 60), IndProc = (IndicadorOrigemProcesso)this.LerInt32(XmlTag<DFeNFe.ProcRef>(nameof(DFeNFe.ProcRef.IndProc)), ObOp.Obrigatorio, 1, 1) };
                    if (lenPipesRegistro >= 4)
                    {
                        processo.TpAto = ObterEnumOpcional(
                            this.LerInt32(XmlTag<DFeNFe.ProcRef>(nameof(DFeNFe.ProcRef.TpAto)), ObOp.Opcional, 2, 2),
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
                            UFSaidaPais = (UFBrasil)Enum.Parse(typeof(UFBrasil), this.LerString(XmlTag<DFeNFe.Exporta>(nameof(DFeNFe.Exporta.UFSaidaPais)), ObOp.Obrigatorio, 2, 2), true),
                            XLocExporta = this.LerString(XmlTag<DFeNFe.Exporta>(nameof(DFeNFe.Exporta.XLocExporta)), ObOp.Obrigatorio, 1, 60),
                            XLocDespacho = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Exporta>(nameof(DFeNFe.Exporta.XLocDespacho)), ObOp.Opcional, 1, 60))
                        };
                    }
                    else
                    {
                        //layout = "§ZA|UFEmbarq|XLocEmbarq"; //ok
                        // Grupo da TAG <exporta>.
                        this.LerString(NFeTxtFieldNames.UfEmbarque, ObOp.Obrigatorio, 2, 2);
                        this.LerString(NFeTxtFieldNames.LocalEmbarque, ObOp.Obrigatorio, 1, 60);
                    }
        }

        private void ProcessarCompra()
        {
                    //layout = "§ZB|XNEmp|XPed|XCont"; //ok
                    // Grupo da TAG <compra>.
                    this.nfeOficial.InfNFeField.Compra = new DFeNFe.Compra
                    {
                        XNEmp = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Compra>(nameof(DFeNFe.Compra.XNEmp)), ObOp.Opcional, 1, 17)),
                        XPed = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Prod>(nameof(DFeNFe.Prod.XPed)), ObOp.Opcional, 1, 60)),
                        XCont = VazioParaNulo(this.LerString(XmlTag<DFeNFe.Compra>(nameof(DFeNFe.Compra.XCont)), ObOp.Opcional, 1, 60))
                    };
        }

        private void AdicionarFornecimentoDiario()
        {
                    //layout = "§ZC04|dia|qtde";
                    if (this.nfeOficial.InfNFeField.Cana == null) this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana();
                    if (this.nfeOficial.InfNFeField.Cana.ForDia == null) this.nfeOficial.InfNFeField.Cana.ForDia = new List<DFeNFe.ForDia>();
                    this.nfeOficial.InfNFeField.Cana.ForDia.Add(new DFeNFe.ForDia { Dia = this.LerInt32(XmlTag<DFeNFe.ForDia>(nameof(DFeNFe.ForDia.Dia)), ObOp.Obrigatorio, 1, 2), Qtde = this.LerDouble(TpcnTipoCampo.tcDouble10, XmlTag<DFeNFe.ForDia>(nameof(DFeNFe.ForDia.Qtde)), ObOp.Obrigatorio, 11) });
        }

        private void AdicionarDeducaoCana()
        {
                    //layout = "§ZC10|xDed|vDed";
                    if (this.nfeOficial.InfNFeField.Cana == null) this.nfeOficial.InfNFeField.Cana = new DFeNFe.Cana();
                    if (this.nfeOficial.InfNFeField.Cana.Deduc == null) this.nfeOficial.InfNFeField.Cana.Deduc = new List<DFeNFe.Deduc>();
                    this.nfeOficial.InfNFeField.Cana.Deduc.Add(new DFeNFe.Deduc { XDed = this.LerString(XmlTag<DFeNFe.Deduc>(nameof(DFeNFe.Deduc.XDed)), ObOp.Obrigatorio, 1, 60), VDed = this.LerDouble(TpcnTipoCampo.tcDouble2, XmlTag<DFeNFe.Deduc>(nameof(DFeNFe.Deduc.VDed)), ObOp.Obrigatorio, 15) });
        }

        private void ProcessarResponsavelTecnicoZ()
        {
                    //layout = "ZD|CNPJ|xContato|email|fone|idCSRT|hashCSRT|"
                    var responsavel = CriarResponsavelTecnico();
                    responsavel.CNPJ = this.LerString(XmlTag<DFeNFe.RefNF>(nameof(DFeNFe.RefNF.CNPJ)), ObOp.Obrigatorio, 14, 14);
                    responsavel.XContato = this.LerString("xContato", ObOp.Obrigatorio, 2, 60);
                    responsavel.Email = this.LerString(XmlTag<DFeNFe.Dest>(nameof(DFeNFe.Dest.Email)), ObOp.Obrigatorio, 2, 60);
                    responsavel.Fone = this.LerString(XmlTag<DFeNFe.EnderEmit>(nameof(DFeNFe.EnderEmit.Fone)), ObOp.Obrigatorio, 6, 14);
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
