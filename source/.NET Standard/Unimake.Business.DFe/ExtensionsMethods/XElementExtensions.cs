using System;
using System.Linq;
using System.Xml.Linq;

/// <summary>
/// Extensões para o tipo <see cref="XElement"/>
/// </summary>
public static class XElementExtensions
{
    #region Public Methods

    /// <summary>
    /// Retorna o valor da tag definida em <paramref name="tagName"/> contida no elemento <paramref name="element"/>
    /// </summary>
    /// <param name="element">Elemento que contem o valor da tag</param>
    /// <param name="tagName">nome da tag</param>
    /// <returns></returns>
    public static string GetValue(this XElement element, string tagName) => element.Descendants().FirstOrDefault(w => w.Name.LocalName == tagName)?.Value;

    /// <summary>
    /// Retorna verdadeiro se o nome for igual, ignora maiúsculas e minúsculas
    /// </summary>
    /// <param name="element">Elemento para validação do <paramref name="name"/></param>
    /// <param name="name">Nome para verificar se é igual ao do <paramref name="element"/></param>
    /// <returns></returns>
    public static bool NameEquals(this XElement element, string name) => element.Name.LocalName.Equals(name, StringComparison.InvariantCultureIgnoreCase);

    #endregion Public Methods
}