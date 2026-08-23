#pragma warning disable CS1591
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>Defaults supplied by the application when converting a legacy UniNFe TXT request.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Txt.TxtConversaoContexto")]
    [ComVisible(true)]
#endif
    public sealed class TxtConversaoContexto
    {
        public TipoAmbiente TipoAmbiente { get; set; }
        public UFBrasil CodigoUF { get; set; }
        public ModeloDFe Modelo { get; set; }
        public TipoEmissao TipoEmissao { get; set; }
    }

    /// <summary>In-memory result of a legacy UniNFe TXT request conversion.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Txt.TxtConversaoResultado")]
    [ComVisible(true)]
#endif
    public sealed class TxtConversaoResultado
    {
        public string Xml { get; set; }
        public string Mensagem { get; set; }
        public bool Sucesso { get { return !string.IsNullOrWhiteSpace(Xml); } }
    }

    internal sealed class TxtCampos
    {
        private readonly List<KeyValuePair<string, string>> itens;
        private TxtCampos(List<KeyValuePair<string, string>> valores) { itens = valores; }
        internal static TxtCampos Ler(string arquivo)
        {
            if (string.IsNullOrWhiteSpace(arquivo)) throw new ArgumentException("O caminho do TXT deve ser informado.", "arquivo");
            var valores = new List<KeyValuePair<string, string>>();
            using (var leitor = new StreamReader(arquivo, Encoding.Default, true))
            {
                string linha;
                while ((linha = leitor.ReadLine()) != null)
                {
                    if (string.IsNullOrWhiteSpace(linha)) continue;
                    var p = linha.IndexOf('|');
                    if (p <= 0) throw new FormatException("Linha TXT inválida: informe campo e valor separados por '|'.");
                    valores.Add(new KeyValuePair<string, string>(linha.Substring(0, p).Trim(), linha.Substring(p + 1).Trim()));
                }
            }
            if (valores.Count == 0) throw new FormatException("O arquivo TXT não contém campos para conversão.");
            return new TxtCampos(valores);
        }
        internal string Get(string nome, string padrao = "") { var x = itens.LastOrDefault(i => string.Equals(i.Key, nome, StringComparison.OrdinalIgnoreCase)); return string.IsNullOrEmpty(x.Key) ? padrao : x.Value; }
        internal int Count(string nome) { return itens.Count(i => string.Equals(i.Key, nome, StringComparison.OrdinalIgnoreCase)); }
        internal IReadOnlyList<KeyValuePair<string, string>> Itens { get { return itens; } }
        internal static TxtCampos De(IEnumerable<KeyValuePair<string, string>> valores) { return new TxtCampos(valores.ToList()); }
    }

    internal static class LegacyTxtXml
    {
        internal const string NFeNs = "http://www.portalfiscal.inf.br/nfe";
        internal const string CTeNs = "http://www.portalfiscal.inf.br/cte";
        internal static TxtConversaoResultado Status(string a, TxtConversaoContexto c) { var f = TxtCampos.Ler(a); var d = Novo("consStatServ", NFeNs, f.Get("versao", "4.00")); Add(d, "tpAmb", f.Get("tpAmb", N(c.TipoAmbiente))); Add(d, "cUF", f.Get("cUF", N(c.CodigoUF))); Add(d, "xServ", "STATUS"); return R(d); }
        internal static TxtConversaoResultado Situacao(string a, TxtConversaoContexto c) { var f = TxtCampos.Ler(a); var d = Novo("consSitNFe", NFeNs, f.Get("versao", "4.00")); Add(d, "tpAmb", f.Get("tpAmb", N(c.TipoAmbiente))); Add(d, "xServ", "CONSULTAR"); Add(d, "chNFe", Req(f, "chNFe")); return R(d); }
        internal static TxtConversaoResultado Cadastro(string a, TxtConversaoContexto c) { var f = TxtCampos.Ler(a); var d = Novo("ConsCad", NFeNs, f.Get("versao", "2.00")); var inf = E(d, "infCons"); d.DocumentElement.AppendChild(inf); Add(d, inf, "xServ", "CONS-CAD"); Add(d, inf, "UF", f.Get("UF", c.CodigoUF.ToString())); Primeiro(d, inf, f, "IE", "CNPJ", "CPF"); return R(d); }
        internal static TxtConversaoResultado Gtin(string a) { var f = TxtCampos.Ler(a); var d = Novo("consGTIN", "http://www.portalfiscal.inf.br/ccg", f.Get("versao", "1.00")); Add(d, "GTIN", Req(f, "GTIN")); return R(d); }
        internal static TxtConversaoResultado Distribuicao(string a, TxtConversaoContexto c, string ns, bool chave) { var f = TxtCampos.Ler(a); var d = Novo("distDFeInt", ns, f.Get("versao", "1.01")); Add(d, "tpAmb", f.Get("tpAmb", N(c.TipoAmbiente))); Add(d, "cUFAutor", f.Get("cUFAutor", N(c.CodigoUF))); Primeiro(d, d.DocumentElement, f, "CNPJ", "CPF"); if (chave && !string.IsNullOrWhiteSpace(f.Get("chNFe"))) Grupo(d, "consChNFe", "chNFe", f.Get("chNFe")); else if (!string.IsNullOrWhiteSpace(f.Get("NSU"))) Grupo(d, "consNSU", "NSU", f.Get("NSU")); else Grupo(d, "distNSU", "ultNSU", f.Get("ultNSU", "000000000000000")); return R(d); }
        internal static TxtConversaoResultado Inutilizacao(string a, TxtConversaoContexto c) { var f = TxtCampos.Ler(a); var uf = f.Get("cUF", N(c.CodigoUF)); var ano = Req(f, "ano").PadLeft(2, '0'); var cnpj = Req(f, "CNPJ"); var mod = f.Get("mod", N(c.Modelo)); var ser = Req(f, "serie"); var ini = Req(f, "nNFIni"); var fim = Req(f, "nNFFin"); var d = Novo("inutNFe", NFeNs, f.Get("versao", "4.00")); var inf = E(d, "infInut"); inf.SetAttribute("Id", "ID" + uf.PadLeft(2, '0') + ano + cnpj + mod.PadLeft(2, '0') + ser.PadLeft(3, '0') + ini.PadLeft(9, '0') + fim.PadLeft(9, '0')); d.DocumentElement.AppendChild(inf); foreach (var p in new[] { new[] { "tpAmb", f.Get("tpAmb", N(c.TipoAmbiente)) }, new[] { "xServ", "INUTILIZAR" }, new[] { "cUF", uf }, new[] { "ano", ano }, new[] { "CNPJ", cnpj }, new[] { "mod", mod }, new[] { "serie", ser }, new[] { "nNFIni", ini }, new[] { "nNFFin", fim }, new[] { "xJust", Req(f, "xJust") } }) Add(d, inf, p[0], p[1]); return R(d); }
        internal static TxtConversaoResultado Evento(string arquivo, TxtConversaoContexto contexto)
        {
            var todos = TxtCampos.Ler(arquivo);
            var grupos = new List<List<KeyValuePair<string, string>>>();
            List<KeyValuePair<string, string>> atual = null;
            foreach (var campo in todos.Itens)
            {
                if (string.Equals(campo.Key, "evento", StringComparison.OrdinalIgnoreCase))
                {
                    atual = new List<KeyValuePair<string, string>>();
                    grupos.Add(atual);
                    continue;
                }
                if (atual != null) atual.Add(campo);
            }
            if (grupos.Count == 0)
                grupos.Add(todos.Itens.Where(x => !string.Equals(x.Key, "idLote", StringComparison.OrdinalIgnoreCase)).ToList());

            var documento = Novo("envEvento", NFeNs, todos.Get("versao", "1.00"));
            Add(documento, "idLote", todos.Get("idLote", DateTime.Now.ToString("yyyyMMddHHmmssfff", CultureInfo.InvariantCulture)));
            foreach (var grupo in grupos)
                AdicionarEvento(documento, TxtCampos.De(grupo), contexto);
            return R(documento);
        }

        private static void AdicionarEvento(XmlDocument documento, TxtCampos campos, TxtConversaoContexto contexto)
        {
            var tipo = Req(campos, "tpEvento");
            var chave = Req(campos, "chNFe");
            var sequencia = campos.Get("nSeqEvento", "1");
            if (tipo == "110140" || tipo == "110111" || tipo == "110112" || tipo.StartsWith("21", StringComparison.Ordinal)) sequencia = "1";
            var versao = campos.Get("verEvento", "1.00");
            var evento = E(documento, "evento");
            evento.SetAttribute("versao", versao);
            documento.DocumentElement.AppendChild(evento);
            var inf = E(documento, "infEvento");
            inf.SetAttribute("Id", campos.Get("Id", "ID" + tipo + chave + int.Parse(sequencia, CultureInfo.InvariantCulture).ToString("00", CultureInfo.InvariantCulture)));
            evento.AppendChild(inf);
            Add(documento, inf, "cOrgao", campos.Get("cOrgao", chave.Substring(0, 2)));
            Add(documento, inf, "tpAmb", campos.Get("tpAmb", N(contexto.TipoAmbiente)));
            Primeiro(documento, inf, campos, "CNPJ", "CPF");
            Add(documento, inf, "chNFe", chave);
            Add(documento, inf, "dhEvento", campos.Get("dhEvento", DateTimeOffset.Now.ToString("yyyy-MM-ddTHH:mm:sszzz", CultureInfo.InvariantCulture)));
            Add(documento, inf, "tpEvento", tipo);
            Add(documento, inf, "nSeqEvento", sequencia);
            Add(documento, inf, "verEvento", versao);
            var detalhe = E(documento, "detEvento");
            detalhe.SetAttribute("versao", versao);
            inf.AppendChild(detalhe);
            Add(documento, detalhe, "descEvento", campos.Get("descEvento", Desc(tipo)));
            AdicionarDetalheEvento(documento, detalhe, campos, tipo, chave);
        }

        private static void AdicionarDetalheEvento(XmlDocument d, XmlElement det, TxtCampos f, string tipo, string chave)
        {
            if (tipo == "110110")
            {
                Add(d, det, "xCorrecao", Req(f, "xCorrecao"));
                Add(d, det, "xCondUso", f.Get("xCondUso", "A Carta de Correcao e disciplinada pelo paragrafo 1o-A do art. 7o do Convenio S/N, de 15 de dezembro de 1970 e pode ser utilizada para regularizacao de erro ocorrido na emissao de documento fiscal, desde que o erro nao esteja relacionado com: I - as variaveis que determinam o valor do imposto tais como: base de calculo, aliquota, diferenca de preco, quantidade, valor da operacao ou da prestacao; II - a correcao de dados cadastrais que implique mudanca do remetente ou do destinatario; III - a data de emissao ou de saida."));
            }
            else if (tipo == "110111")
            {
                Add(d, det, "nProt", Req(f, "nProt")); Add(d, det, "xJust", Req(f, "xJust"));
            }
            else if (tipo == "110112")
            {
                Add(d, det, "cOrgaoAutor", f.Get("cancelamentoSubstituicao.cOrgaoAutor", chave.Substring(0, 2)));
                Add(d, det, "tpAutor", f.Get("cancelamentoSubstituicao.tpAutor", "1"));
                Add(d, det, "verAplic", f.Get("cancelamentoSubstituicao.verAplic", f.Get("verAplic", "1.0")));
                Add(d, det, "nProt", Req(f, "nProt")); Add(d, det, "xJust", Req(f, "xJust"));
                Add(d, det, "chNFeRef", ReqAlternativo(f, "cancelamentoSubstituicao.chNFeRef", "chNFeRef"));
            }
            else if (tipo == "110140") AdicionarEpec(d, det, f);
            else if (tipo == "111500" || tipo == "111501")
            {
                Add(d, det, "nProt", Req(f, "nProt"));
                foreach (var item in GruposRepetidos(f, "itemPedido.numItem"))
                {
                    var no = E(d, "itemPedido"); det.AppendChild(no);
                    no.SetAttribute("numItem", item.Get("itemPedido.numItem"));
                    Add(d, no, "qtdeItem", Req(item, "itemPedido.qtdeItem"));
                }
            }
            else if (tipo == "111502" || tipo == "111503")
            {
                Add(d, det, "idPedidoCancelado", Req(f, "idPedidoCancelado")); Add(d, det, "nProt", Req(f, "nProt"));
            }
            else if (tipo == "411500" || tipo == "411501") AdicionarRespostaProrrogacao(d, det, f);
            else if (tipo == "411502" || tipo == "411503") AdicionarRespostaCancelamentoProrrogacao(d, det, f);
            else if (tipo == "110750") AdicionarConciliacao(d, det, f);
            else if (tipo == "110751")
            {
                Add(d, det, "verAplic", Req(f, "verAplic"));
                Add(d, det, "nProtEvento", Req(f, "cancConciliacaoFinanceira.nProtEvento"));
            }
            else if (!string.IsNullOrWhiteSpace(f.Get("xJust"))) Add(d, det, "xJust", f.Get("xJust"));
        }

        private static void AdicionarRespostaProrrogacao(XmlDocument d, XmlElement det, TxtCampos f)
        {
            Add(d, det, "idPedido", Req(f, "idPedido"));
            var resposta = E(d, "respPedido"); det.AppendChild(resposta);
            Add(d, resposta, "statPrazo", Req(f, "respPedido.statPrazo"));
            foreach (var item in GruposRepetidos(f, "respPedido.itemPedido.numItem"))
            {
                var no = E(d, "itemPedido"); resposta.AppendChild(no);
                no.SetAttribute("numItem", item.Get("respPedido.itemPedido.numItem"));
                Add(d, no, "statPedido", Req(item, "respPedido.itemPedido.statPedido"));
                Add(d, no, "justStatus", Req(item, "respPedido.itemPedido.justStatus"));
                if (!string.IsNullOrWhiteSpace(item.Get("respPedido.itemPedido.justStaOutra")))
                    Add(d, no, "justStaOutra", item.Get("respPedido.itemPedido.justStaOutra"));
            }
        }

        private static void AdicionarRespostaCancelamentoProrrogacao(XmlDocument d, XmlElement det, TxtCampos f)
        {
            Add(d, det, "idPedido", Req(f, "idPedido"));
            var resposta = E(d, "respCancPedido"); det.AppendChild(resposta);
            Add(d, resposta, "statCancPedido", Req(f, "respCancPedido.statCancPedido"));
            Add(d, resposta, "justStatus", Req(f, "respCancPedido.justStatus"));
            if (!string.IsNullOrWhiteSpace(f.Get("respCancPedido.justStaOutra")))
                Add(d, resposta, "justStaOutra", f.Get("respCancPedido.justStaOutra"));
        }

        private static void AdicionarConciliacao(XmlDocument d, XmlElement det, TxtCampos f)
        {
            Add(d, det, "verAplic", Req(f, "verAplic"));
            foreach (var pagamento in GruposRepetidos(f, "detPag"))
            {
                var no = E(d, "detPag"); det.AppendChild(no);
                foreach (var nome in new[] { "indPag", "tPag", "xPag", "vPag", "dPag", "CNPJPag", "UFPag", "CNPJIF", "tBand", "cAut", "CNPJReceb", "UFReceb" })
                {
                    var valor = pagamento.Get("detPag." + nome);
                    if (!string.IsNullOrWhiteSpace(valor)) Add(d, no, nome, valor);
                }
            }
        }

        private static void AdicionarEpec(XmlDocument d, XmlElement det, TxtCampos f)
        {
            foreach (var nome in new[] { "cOrgaoAutor", "tpAutor", "verAplic", "dhEmi", "tpNF", "IE" })
                Add(d, det, nome, Req(f, "epec." + nome));
            var dest = E(d, "dest"); det.AppendChild(dest);
            Add(d, dest, "UF", Req(f, "epec.dest.UF"));
            Primeiro(d, dest, Prefixo(f, "epec.dest."), "CNPJ", "CPF", "idEstrangeiro");
            foreach (var nome in new[] { "IE", "vNF", "vICMS", "vST" })
                Add(d, dest, nome, Req(f, "epec.dest." + nome));
        }

        private static IEnumerable<TxtCampos> GruposRepetidos(TxtCampos campos, string marcador)
        {
            var grupo = new List<KeyValuePair<string, string>>();
            var prefixo = marcador.IndexOf('.') < 0 ? marcador + "." : marcador.Substring(0, marcador.LastIndexOf('.') + 1);
            foreach (var item in campos.Itens)
            {
                if (string.Equals(item.Key, marcador, StringComparison.OrdinalIgnoreCase) && grupo.Count > 0)
                {
                    yield return TxtCampos.De(grupo); grupo = new List<KeyValuePair<string, string>>();
                }
                if (string.Equals(item.Key, marcador, StringComparison.OrdinalIgnoreCase) || item.Key.StartsWith(prefixo, StringComparison.OrdinalIgnoreCase)) grupo.Add(item);
            }
            if (grupo.Count > 0) yield return TxtCampos.De(grupo);
        }

        private static TxtCampos Prefixo(TxtCampos campos, string prefixo) { return TxtCampos.De(campos.Itens.Where(x => x.Key.StartsWith(prefixo, StringComparison.OrdinalIgnoreCase)).Select(x => new KeyValuePair<string, string>(x.Key.Substring(prefixo.Length), x.Value))); }
        private static string ReqAlternativo(TxtCampos campos, string primeiro, string segundo) { var valor = campos.Get(primeiro, campos.Get(segundo)); if (string.IsNullOrWhiteSpace(valor)) throw new FormatException("Informe o campo '" + primeiro + "' no arquivo TXT."); return valor; }
        private static string Desc(string t) { switch (t) { case "110110": return "Carta de Correcao"; case "110111": return "Cancelamento"; case "110112": return "Cancelamento por substituicao"; case "210200": return "Confirmacao da Operacao"; case "210210": return "Ciencia da Operacao"; case "210220": return "Desconhecimento da Operacao"; case "210240": return "Operacao nao Realizada"; case "110140": return "EPEC"; default: return "Evento da NF-e"; } }
        private static XmlDocument Novo(string r, string ns, string v) { var d = new XmlDocument { XmlResolver = null }; var e = d.CreateElement(r, ns); e.SetAttribute("versao", v); d.AppendChild(e); return d; }
        private static XmlElement E(XmlDocument d, string n) { return d.CreateElement(n, d.DocumentElement.NamespaceURI); }
        private static void Add(XmlDocument d, string n, string v) { Add(d, d.DocumentElement, n, v); }
        private static void Add(XmlDocument d, XmlElement p, string n, string v) { var e = E(d, n); e.InnerText = v ?? ""; p.AppendChild(e); }
        private static void Grupo(XmlDocument d, string g, string n, string v) { var e = E(d, g); d.DocumentElement.AppendChild(e); Add(d, e, n, v); }
        private static void Primeiro(XmlDocument d, XmlElement p, TxtCampos f, params string[] ns) { foreach (var n in ns) { var v = f.Get(n); if (!string.IsNullOrWhiteSpace(v)) { Add(d, p, n, v); return; } } throw new FormatException("Informe IE, CNPJ ou CPF no arquivo TXT."); }
        private static string Req(TxtCampos f, string n) { var v = f.Get(n); if (string.IsNullOrWhiteSpace(v)) throw new FormatException("Informe o campo '" + n + "' no arquivo TXT."); return v; }
        private static string N(object v) { return Convert.ToInt32(v, CultureInfo.InvariantCulture).ToString(CultureInfo.InvariantCulture); }
        private static TxtConversaoResultado R(XmlDocument d) { return new TxtConversaoResultado { Xml = d.OuterXml, Mensagem = "Conversão efetuada com sucesso." }; }
    }

    internal static class LegacyTxtRetornoConverter
    {
        internal static string Autorizacao(string x) { var d = L(x); var r = F(d, "retEnviNFe"); var s = ";" + V(r, "cStat") + V(r, "xMotivo"); var p = F(r, "infProt", false); if (p != null) { var ch = T(p, "chNFe"); s += "\r\n" + Sub(ch, 6, 14) + ";" + Sub(ch, 25, 9) + ";" + V(p, "chNFe") + V(p, "dhRecbto") + Z(p, "nProt") + V(p, "digVal") + V(p, "cStat") + V(p, "xMotivo"); } return s; }
        internal static string Status(string x) { var e = F(L(x), "retConsStatServ"); return V(e, "tpAmb") + V(e, "cStat") + V(e, "xMotivo") + V(e, "cUF") + V(e, "dhRecbto") + V(e, "tMed") + "\r\n"; }
        internal static string Inutilizacao(string x) { var d = L(x); var b = new StringBuilder(); foreach (XmlElement e in d.GetElementsByTagName("infInut")) b.Append(V(e, "tpAmb")).Append(V(e, "cStat")).Append(V(e, "xMotivo")).Append(V(e, "cUF")).Append("\r\n"); return b.ToString(); }
        internal static string Situacao(string x) { var d = L(x); var b = new StringBuilder(); var protocolos = d.GetElementsByTagName("infProt"); foreach (XmlElement e in protocolos) b.Append(V(e, "tpAmb")).Append(V(e, "cStat")).Append(V(e, "xMotivo")).Append(V(e, "cUF")).Append(V(e, "dhRecbto")).Append(Z(e, "nProt")).Append("\r\n"); if (protocolos.Count == 0) { var r = F(d, "retConsSitNFe"); b.Append(V(r, "tpAmb")).Append(V(r, "cStat")).Append(V(r, "xMotivo")).Append(V(r, "cUF")).Append(V(r, "dhRecbto")).Append(Z(r, "nProt")).Append("\r\n"); } foreach (XmlElement p in d.GetElementsByTagName("procEventoNFe")) foreach (XmlElement parte in p.ChildNodes.OfType<XmlElement>().Where(e => e.LocalName == "evento" || e.LocalName == "retEvento")) { var inf = F(parte, "infEvento", false); if (inf == null) continue; b.Append('[').Append(parte.LocalName).Append("]\r\n"); foreach (XmlElement campo in inf.ChildNodes.OfType<XmlElement>()) { if (campo.LocalName == "detEvento") foreach (XmlElement det in campo.ChildNodes.OfType<XmlElement>().Where(e => e.LocalName != "xCondUso")) b.Append(det.InnerText).Append(';'); else b.Append(campo.InnerText).Append(';'); } b.Append("\r\n"); } return b.ToString(); }
        internal static string Evento(string x) { var d = L(x); var b = new StringBuilder(); foreach (XmlElement ret in d.GetElementsByTagName("retEvento")) { var e = F(ret, "infEvento", false); if (e == null) continue; var tipo = T(e, "tpEvento"); b.Append(V(e, "tpAmb")).Append(V(e, "cOrgao")).Append(V(e, "cStat")).Append(V(e, "xMotivo")).Append(V(e, "chNFe")).Append(V(e, "tpEvento")).Append(V(e, "xEvento")).Append(V(e, "nSeqEvento")); var doc = T(e, "CNPJDest"); if (string.IsNullOrEmpty(doc)) doc = T(e, "CPFDest"); b.Append(doc).Append(';').Append(V(e, "dhRegEvento")).Append(V(e, "nProt")); if (tipo == "110140") { b.Append(V(e, "cOrgaoAutor")); foreach (XmlElement pendente in e.GetElementsByTagName("chNFePend")) b.Append(pendente.InnerText).Append(';'); } else if (EhProrrogacao(tipo)) b.Append(V(e, "emailDest")); b.Append("\r\n"); } return b.ToString(); }
        internal static string Distribuicao(string x) { var d = L(x); var r = F(d, "retDistDFeInt"); var b = new StringBuilder().Append(V(r, "tpAmb")).Append(V(r, "verAplic")).Append(V(r, "cStat")).Append(V(r, "xMotivo")).Append(V(r, "dhResp")).Append(V(r, "ultNSU")).Append(V(r, "maxNSU")).Append("\r\n"); foreach (XmlElement z in d.GetElementsByTagName("docZip")) { var nsu = z.GetAttribute("NSU"); if (string.IsNullOrEmpty(nsu)) nsu = "000000000000000"; var schema = z.GetAttribute("schema"); var i = L(Compress.GZIPDecompress(z.InnerText)); string tipo = null, ch = null, extra = ""; if (schema.StartsWith("resEvento")) { tipo = "resEvento"; ch = T(i, "chNFe"); extra = Ev(i, 2); } else if (schema.StartsWith("procEventoNFe")) { tipo = "procEventoNFe"; ch = T(i, "chNFe"); extra = Ev(i, 2); } else if (schema.StartsWith("procNFe")) { tipo = "procNFe"; ch = T(i, "chNFe"); } else if (schema.StartsWith("resNFe")) { tipo = "resNFe"; ch = T(i, "chNFe"); } else if (schema.StartsWith("procEventoCTe")) { tipo = "procEventoCTe"; ch = T(i, "chCTe"); extra = Ev(i, schema.Contains("3.00") ? 2 : 3); } else if (schema.StartsWith("procCTe")) { tipo = "procCTe"; ch = T(i, "chCTe"); } if (tipo != null) b.Append(tipo).Append(';').Append(nsu).Append(';').Append(ch).Append(';').Append(extra).Append("\r\n"); } return b.ToString(); }
        internal static string Cadastro(string x) { var d = L(x); var r = F(d, "retConsCad"); var b = new StringBuilder().Append(P(T(r, "cStat"), 3)).Append(';').Append(S(T(r, "xMotivo"))).Append(';').Append(V(r, "UF")).Append(V(r, "IE")).Append(V(r, "CNPJ")).Append(V(r, "CPF")).Append(V(r, "dhCons")).Append(P(T(r, "cUF"), 2)).Append(";\r\r"); foreach (XmlElement e in d.GetElementsByTagName("infCad")) { foreach (var n in new[] { "IE", "CNPJ", "CPF", "UF", "cSit" }) b.Append(V(e, n)); b.Append(S(T(e, "xNome"))).Append(';').Append(S(T(e, "xFant"))).Append(';').Append(S(T(e, "xRegApur"))).Append(';').Append(V(e, "CNAE")).Append(V(e, "dIniAtiv")).Append(V(e, "dUltSit")).Append(S(T(e, "IEUnica"))).Append(';').Append(S(T(e, "IEAtual"))).Append(';'); var end = F(e, "ender", false); if (end != null) b.Append(S(T(end, "xLgr"))).Append(';').Append(S(T(end, "nro"))).Append(';').Append(S(T(end, "xCpl"))).Append(';').Append(S(T(end, "xBairro"))).Append(';').Append(P(T(end, "cMun"), 7)).Append(';').Append(S(T(end, "xMun"))).Append(';').Append(P(T(end, "CEP"), 8)).Append(';'); b.Append("\r\r"); } return b.ToString(); }
        internal static string Gtin(string x) { var d = L(x); var b = new StringBuilder(); foreach (var n in new[] { "cStat", "xMotivo", "GTIN", "tpGTIN", "xProd", "NCM" }) b.Append(n == "cStat" ? "CStat" : n == "xMotivo" ? "XMotivo" : n).Append('|').Append(T(d, n)).Append("\r\n"); foreach (XmlElement e in d.GetElementsByTagName("CEST")) b.Append("CEST|").Append(e.InnerText).Append("\r\n"); return b.ToString(); }
        private static string Ev(XmlDocument d, int n) { int seq; int.TryParse(T(d, "nSeqEvento"), out seq); return TipoEventoLegado(T(d, "tpEvento")) + ";" + seq.ToString(new string('0', n), CultureInfo.InvariantCulture); }
        private static bool EhProrrogacao(string t) { return t == "111500" || t == "111501" || t == "111502" || t == "111503" || t == "411500" || t == "411501" || t == "411502" || t == "411503"; }
        private static string TipoEventoLegado(string t) { switch (t) { case "110110": return "tpEvCCe"; case "110111": return "tpEvCancelamentoNFe"; case "110112": return "tpEvCancelamentoSubstituicaoNFCe"; case "210200": return "tpEvConfirmacaoOperacao"; case "210210": return "tpEvCienciaOperacao"; case "210220": return "tpEvDesconhecimentoOperacao"; case "110140": return "tpEvEPEC"; case "110113": return "tpEvEPECCTe"; case "210240": return "tpEvOperacaoNaoRealizada"; case "110114": return "tpEvInclusaoCondutor"; case "310620": return "tpEvRegistroPassagem"; case "510620": return "tpEvRegistroPassagemBRid"; case "110160": return "tpevRegMultimodal"; case "111500": return "tpEvPedProrrogacao_ICMS_1"; case "111501": return "tpEvPedProrrogacao_ICMS_2"; case "111502": return "tpEvCancPedProrrogacao_ICMS_1"; case "111503": return "tpEvCancPedProrrogacao_ICMS_2"; case "411500": return "tpEvFiscoRespPedProrrogacao_ICMS_1"; case "411501": return "tpEvFiscoRespPedProrrogacao_ICMS_2"; case "411502": return "tpEvFiscoRespCancPedProrrogacao_ICMS_1"; case "411503": return "tpEvFiscoRespCancPedProrrogacao_ICMS_2"; case "110116": return "tpEvPagamentoOperacaoMDFe"; case "110130": return "tpEvComprovanteEntregaNFe"; case "110131": return "tpEvCancelamentoComprovanteEntregaNFe"; case "110750": return "tpEvConciliacaoFinanceiraNFe"; case "110751": return "tpEvCancelamentoConciliacaoFinanceiraNFe"; default: return t; } }
        private static XmlDocument L(string x) { if (string.IsNullOrWhiteSpace(x)) throw new ArgumentException("O XML de retorno deve ser informado.", "xmlRetorno"); var d = new XmlDocument { XmlResolver = null }; d.LoadXml(x); return d; }
        private static XmlElement F(XmlNode n, string nome, bool req = true) { var d = n as XmlDocument; var l = d != null ? d.GetElementsByTagName(nome) : ((XmlElement)n).GetElementsByTagName(nome); var e = l.Count > 0 ? (XmlElement)l[0] : null; if (e == null && req) throw new FormatException("O retorno não contém o elemento '" + nome + "'."); return e; }
        private static string T(XmlNode n, string nome) { var d = n as XmlDocument; var l = d != null ? d.GetElementsByTagName(nome) : ((XmlElement)n).GetElementsByTagName(nome); return l.Count == 0 ? "" : l[0].InnerText; }
        private static string V(XmlNode n, string nome) { return T(n, nome) + ";"; }
        private static string Z(XmlNode n, string nome) { var v = T(n, nome); return (string.IsNullOrWhiteSpace(v) ? "0" : v) + ";"; }
        private static string Sub(string v, int i, int n) { return v != null && v.Length >= i + n ? v.Substring(i, n) : ""; }
        private static string S(string v) { return (v ?? "").Replace(';', ' '); }
        private static string P(string v, int n) { int i; return int.TryParse(v, out i) ? i.ToString(new string('0', n), CultureInfo.InvariantCulture) : ""; }
    }
}

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsultaCadastroTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class ConsultaCadastroTxtConverter
    {
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c)
        {
            return Txt.LegacyTxtXml.Cadastro(a, c);
        }
        public string ConverterRetorno(string x)
        {
            return Txt.LegacyTxtRetornoConverter.Cadastro(x);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsultaSituacaoTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class ConsultaSituacaoTxtConverter 
    { 
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c) 
        { 
            return Txt.LegacyTxtXml.Situacao(a, c);
        } 
        
        public string ConverterRetorno(string x) 
        { 
            return Txt.LegacyTxtRetornoConverter.Situacao(x); 
        } 
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsultaStatusTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class ConsultaStatusTxtConverter 
    { 
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c) 
        { 
            return Txt.LegacyTxtXml.Status(a, c); 
        } 
        public string ConverterRetorno(string x) 
        { 
            return Txt.LegacyTxtRetornoConverter.Status(x); 
        } 
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EventoTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class EventoTxtConverter 
    { 
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c) 
        { 
            return Txt.LegacyTxtXml.Evento(a, c); 
        } 
        
        public string ConverterRetorno(string x) 
        { 
            return Txt.LegacyTxtRetornoConverter.Evento(x); 
        } 
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InutilizacaoTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class InutilizacaoTxtConverter 
    { 
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c) 
        { 
            return Txt.LegacyTxtXml.Inutilizacao(a, c); 
        } 
        
        public string ConverterRetorno(string x) 
        { 
            return Txt.LegacyTxtRetornoConverter.Inutilizacao(x); 
        } 
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DistribuicaoDFeTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class DistribuicaoDFeTxtConverter
    {
        public Txt.TxtConversaoResultado Converter(string a, Txt.TxtConversaoContexto c)
        {
            return Txt.LegacyTxtXml.Distribuicao(a, c, Txt.LegacyTxtXml.NFeNs, true);
        }
        public string ConverterRetorno(string x)
        {
            return Txt.LegacyTxtRetornoConverter.Distribuicao(x);
        }
    }
}

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DistribuicaoDFeTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class DistribuicaoDFeTxtConverter
    {
        public NFe.Txt.TxtConversaoResultado Converter(string a, NFe.Txt.TxtConversaoContexto c)
        {
            return NFe.Txt.LegacyTxtXml.Distribuicao(a, c, NFe.Txt.LegacyTxtXml.CTeNs, false);
        }

        public string ConverterRetorno(string x)
        {
            return NFe.Txt.LegacyTxtRetornoConverter.Distribuicao(x);
        }
    }
}
namespace Unimake.Business.DFe.Xml.CCG
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CCG.ConsultaGTINTxtConverter")]
    [ComVisible(true)]
#endif
    public sealed class ConsultaGTINTxtConverter { public NFe.Txt.TxtConversaoResultado Converter(string a, NFe.Txt.TxtConversaoContexto c) { return NFe.Txt.LegacyTxtXml.Gtin(a); } public string ConverterRetorno(string x) { return NFe.Txt.LegacyTxtRetornoConverter.Gtin(x); } }
}
