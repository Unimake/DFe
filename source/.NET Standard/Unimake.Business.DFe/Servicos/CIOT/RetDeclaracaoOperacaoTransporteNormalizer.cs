using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    internal static class RetDeclaracaoOperacaoTransporteNormalizer
    {
        private const string NamespaceCIOT = CIOTNamespace.PortalANTT;

        internal static void Normalizar(XmlDocument doc)
        {
            if (doc?.DocumentElement == null || doc.DocumentElement.LocalName != "RetDeclaracaoOperacaoTransporte")
            {
                return;
            }

            var root = doc.DocumentElement;
            RemoverElementoVazio(root, "IdOperacaoTransporte");
            RemoverElementoVazio(root, "CodigoVerificador");

            if (root["temp", NamespaceCIOT] != null)
            {
                return;
            }

            var mensagens = ObterMensagens(root);
            if (mensagens.Count == 0)
            {
                return;
            }

            GarantirElementoTexto(doc, root, "Codigo", MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiroCodigo(mensagens));
            GarantirElementoTexto(doc, root, "Mensagem", MensagemDeclaracaoOperacaoTransporteHelper.ObterPrimeiraDescricao(mensagens));
            GarantirGrupoMensagens(doc, root, mensagens);
            OrdenarElementos(root);
        }

        private static List<MensagemDeclaracaoOperacaoTransporte> ObterMensagens(XmlElement root)
        {
            var grupoMensagens = root["Mensagens", NamespaceCIOT];
            var mensagensDiretas = ObterMensagensDiretas(root);

            if (mensagensDiretas.Count > 1)
            {
                return MensagemDeclaracaoOperacaoTransporteHelper.CriarMensagens(new List<string>(), mensagensDiretas);
            }

            if (grupoMensagens != null)
            {
                return LerMensagens(grupoMensagens);
            }

            var codigo = root["Codigo", NamespaceCIOT];

            return MensagemDeclaracaoOperacaoTransporteHelper.CriarMensagens(codigo?.InnerText, mensagensDiretas.Count == 1 ? mensagensDiretas[0] : null);
        }

        private static List<MensagemDeclaracaoOperacaoTransporte> LerMensagens(XmlElement grupoMensagens)
        {
            var mensagens = new List<MensagemDeclaracaoOperacaoTransporte>();

            foreach (XmlNode node in grupoMensagens.ChildNodes)
            {
                if (node.NodeType != XmlNodeType.Element)
                {
                    continue;
                }

                var mensagem = (XmlElement)node;
                var codigo = mensagem["Codigo", NamespaceCIOT];
                var descricao = mensagem["Descricao", NamespaceCIOT];

                mensagens.Add(new MensagemDeclaracaoOperacaoTransporte
                {
                    Codigo = codigo?.InnerText,
                    Descricao = descricao?.InnerText
                });
            }

            return mensagens;
        }

        private static void GarantirElementoTexto(XmlDocument doc, XmlElement root, string nome, string valor)
        {
            RemoverElementos(root, nome);

            if (string.IsNullOrWhiteSpace(valor))
            {
                return;
            }

            var elemento = doc.CreateElement(nome, NamespaceCIOT);
            elemento.InnerText = valor;
            root.AppendChild(elemento);
        }

        private static void GarantirGrupoMensagens(XmlDocument doc, XmlElement root, List<MensagemDeclaracaoOperacaoTransporte> mensagens)
        {
            RemoverElementos(root, "Mensagens");
            var grupoMensagens = doc.CreateElement("Mensagens", NamespaceCIOT);

            foreach (var mensagem in mensagens)
            {
                var item = doc.CreateElement("Mensagem", NamespaceCIOT);

                if (!string.IsNullOrWhiteSpace(mensagem.Codigo))
                {
                    var codigo = doc.CreateElement("Codigo", NamespaceCIOT);
                    codigo.InnerText = mensagem.Codigo;
                    item.AppendChild(codigo);
                }

                var descricao = doc.CreateElement("Descricao", NamespaceCIOT);
                descricao.InnerText = mensagem.Descricao ?? string.Empty;
                item.AppendChild(descricao);

                grupoMensagens.AppendChild(item);
            }

            root.AppendChild(grupoMensagens);
        }

        private static List<string> ObterMensagensDiretas(XmlElement root)
        {
            var mensagens = new List<string>();

            foreach (XmlNode node in root.ChildNodes)
            {
                if (node.NodeType == XmlNodeType.Element && node.LocalName == "Mensagem" && node.NamespaceURI == NamespaceCIOT)
                {
                    mensagens.Add(node.InnerText);
                }
            }

            return mensagens;
        }

        private static void OrdenarElementos(XmlElement root)
        {
            MoverDepois(root, "Codigo", root["Protocolo", NamespaceCIOT] ?? root["CodigoVerificador", NamespaceCIOT] ?? root["IdOperacaoTransporte", NamespaceCIOT]);
            MoverDepois(root, "Mensagem", root["Codigo", NamespaceCIOT] ?? root["Protocolo", NamespaceCIOT] ?? root["CodigoVerificador", NamespaceCIOT] ?? root["IdOperacaoTransporte", NamespaceCIOT]);
            MoverDepois(root, "Mensagens", root["Mensagem", NamespaceCIOT] ?? root["Codigo", NamespaceCIOT] ?? root["Protocolo", NamespaceCIOT] ?? root["CodigoVerificador", NamespaceCIOT] ?? root["IdOperacaoTransporte", NamespaceCIOT]);
        }

        private static void MoverDepois(XmlElement root, string nome, XmlNode anterior)
        {
            var elemento = root[nome, NamespaceCIOT];
            if (elemento != null && anterior != null)
            {
                root.InsertAfter(elemento, anterior);
            }
        }

        private static void RemoverElementoVazio(XmlElement root, string nome)
        {
            var elemento = root[nome, NamespaceCIOT];
            if (elemento != null && string.IsNullOrWhiteSpace(elemento.InnerText))
            {
                root.RemoveChild(elemento);
            }
        }

        private static void RemoverElementos(XmlElement root, string nome)
        {
            var remover = new List<XmlNode>();

            foreach (XmlNode node in root.ChildNodes)
            {
                if (node.NodeType == XmlNodeType.Element && node.LocalName == nome && node.NamespaceURI == NamespaceCIOT)
                {
                    remover.Add(node);
                }
            }

            foreach (var node in remover)
            {
                root.RemoveChild(node);
            }
        }
    }
}
