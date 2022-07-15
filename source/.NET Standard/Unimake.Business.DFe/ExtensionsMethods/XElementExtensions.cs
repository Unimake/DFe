using System;
using System.Linq;
using System.Xml.Linq;
using Unimake;

/// <summary>
/// Extensões para o tipo <see cref="XElement"/>
/// </summary>
public static class XElementExtensions
{
    #region Public Methods

    /// <summary>
    /// Retorna o valor do atributo definida em <paramref name="attributeName"/> contido no elemento <paramref name="element"/>
    /// </summary>
    /// <param name="element">Elemento que contem o valor do atributo</param>
    /// <param name="attributeName">nome do atributo</param>
    /// <returns></returns>
    public static string GetAttributeValue(this XElement element, string attributeName) => element.Attributes().FirstOrDefault(w => w.Name.LocalName == attributeName)?.Value;

    /// <summary>
    /// Retorna o elmento definido em <paramref name="elementName"/> contido no elemento <paramref name="element"/>
    /// </summary>
    /// <param name="element">Elemento que contem o valor o element</param>
    /// <param name="elementName">nome do elemento</param>
    /// <returns></returns>
    public static XElement GetElement(this XElement element, string elementName) => element.Elements().FirstOrDefault(w => w.Name.LocalName == elementName);

    /// <summary>
    /// Retorna o valor da tag definida em <paramref name="tagName"/> contida no elemento <paramref name="element"/>
    /// </summary>
    /// <param name="element">Elemento que contem o valor da tag</param>
    /// <param name="tagName">nome da tag</param>
    /// <returns></returns>
    public static string GetValue(this XElement element, string tagName) => element.Descendants().FirstOrDefault(w => w.Name.LocalName == tagName)?.Value;

    /// <summary>
    /// Retorna o valor da tag definida em <paramref name="tagName"/> contida no elemento <paramref name="element"/>.
    /// <br/> Tenta converter o valor recebido para o tipo definido em <typeparamref name="T"/>
    /// </summary>
    /// <param name="element">Elemento que contem o valor da tag</param>
    /// <param name="tagName">nome da tag</param>
    public static T GetValue<T>(this XElement element, string tagName) => UConvert.ToAny<T>(element.Descendants().FirstOrDefault(w => w.Name.LocalName == tagName)?.Value);

    /// <summary>
    /// Retorna verdadeiro se o nome for igual, ignora maiúsculas e minúsculas
    /// </summary>
    /// <param name="element">Elemento para validação do <paramref name="name"/></param>
    /// <param name="name">Nome para verificar se é igual ao do <paramref name="element"/></param>
    /// <returns></returns>
    public static bool NameEquals(this XElement element, string name) => element.Name.LocalName.Equals(name, StringComparison.InvariantCultureIgnoreCase);

    #endregion Public Methods
}