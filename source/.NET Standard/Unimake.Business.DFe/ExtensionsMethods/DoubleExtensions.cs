namespace System
{
    /// <summary>
    /// Extensão da classe double
    /// </summary>
    public static class DoubleExtensions
    {
        #region Public Methods

        /// <summary>
        /// Fixar o valor das decimais do Double.
        /// 
        /// Com este método realizamos uma operação para que o valor seja mantido no seu original.
        /// Gambiarra mesmo, mas fazer o que, foi a forma que encontramos de resolver.
        /// </summary>
        /// <param name="numberDecimal">Quantidade de casas decimais</param>
        /// <param name="value">Valor que deve ser ajustado em suas casas decimais</param>
        /// <returns>Retorna o valor com as decimais corrigidas</returns>
        public static double FixValueDecimal(this string value, int numberDecimal)
        {
            if (Convert.ToDouble(value) == 0)
            {
                return 0;
            }

            return double.Parse(value) * Math.Pow(numberDecimal, -15) * Math.Pow(numberDecimal, 15);
        }

        /// <summary>
        /// Corrige o valor das decimais do Double. Resolver problema de ponto flutuante do tipo double.
        /// Dependendo da quantidade de cadas decimais informadas no tipo Double ele muda a informação, exemplo: Se informar o valor 9.9903225806 em um tipo double ele muda para 9.9903225805999991
        /// Utilizando este método ele corrige o problema.
        /// </summary>
        /// <param name="value">Valor que deve ser corrigido</param>
        /// <returns>Valor com as decimais corrigidas</returns>
        public static double Round(this double value)
        {
            if (value == 0)
            {
                return 0;
            }

            return double.Parse($"{value}") * Math.Pow(10, -15) * Math.Pow(10, 15);
        }        

        #endregion Public Methods
    }
}