#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.NF3e
{
    /// <summary>
    /// Classe base para os serviços da NF3e
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NF3e.ServicoBase")]
    [ComVisible(true)]
#endif
    public abstract class ServicoBase : Servicos.ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        protected ServicoBase() : base() { }

        /// <summary>
        /// Definir configurações
        /// </summary>
        protected override void DefinirConfiguracao() { }

        /// <summary>
        /// Validar o XML
        /// </summary>
        protected override void XmlValidar()
        {
            XmlValidarConteudo(); // Efetuar a validação antes de validar schema para evitar alguns erros que não ficam claros para o desenvolvedor.

            var validar = new ValidarSchema();
            validar.Validar(ConteudoXML, Configuracoes.TipoDFe.ToString() + "." + Configuracoes.SchemaArquivo, Configuracoes.TargetNS);

            if (!validar.Success)
            {
                throw new ValidarXMLException(validar.ErrorMessage);
            }
        }

        /// <summary>
        /// Validar, o conteúdo das tags do XML, alguns validações manuais que o schema não faz. Vamos implementando novas regras na medida da necessidade de cada serviço.
        /// </summary>
        protected override void XmlValidarConteudo() { }

        /// <summary>
        /// Executar o serviço
        /// </summary>       
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar() => base.Executar();
    }
}
