using System;
using System.ComponentModel;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Extensão da Classe Enums
    /// </summary>
    public static class EnumsExtension
    {
        #region Public Methods

        /// <summary>
        /// Obter conteúdos dos atributos dos enumeradores
        /// </summary>
        /// <typeparam name="T">Tipo</typeparam>
        /// <param name="valorEnum">Valor do enumerador que é para recuperar os atributos</param>
        /// <returns>Atributos do Enum</returns>
        public static T GetAttribute<T>(this Enum valorEnum) where T : System.Attribute
        {
            var type = valorEnum.GetType();
            var memInfo = type.GetMember(valorEnum.ToString());
            var attributes = memInfo[0].GetCustomAttributes(typeof(T), false);

            return (attributes.Length > 0) ? (T)attributes[0] : null;
        }

        /// <summary>
        /// Obter o conteúdo do atributo Description do Enumerador
        /// </summary>
        /// <param name="valorEnum">Valor do Enhum que é para recuperar o conteúdo do atributo Description</param>
        /// <returns>Retorna o conteúdo do atributo Description</returns>
        public static string GetAttributeDescription(this Enum valorEnum)
        {
            return valorEnum.GetAttribute<DescriptionAttribute>().Description;
        }

        #endregion Public Methods
    }
}