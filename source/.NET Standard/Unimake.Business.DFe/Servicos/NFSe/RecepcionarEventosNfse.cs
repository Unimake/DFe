using System;
using System.Collections.Generic;
using System.Text;

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.NFSe
{
    /// <summary>
    /// Enviar o XML de Recepção de Eventos da NFSe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.NFSe.RecepcionarEventos")]
    [ComVisible(true)]
#endif
    public class RecepcionarEventosNfse : ServicoBase
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public RecepcionarEventosNfse() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public RecepcionarEventosNfse(string conteudoXML, Configuracao configuracao) : this()
        {
            var xmlDoc = new XmlDocument();
            xmlDoc.LoadXml(conteudoXML);

            Inicializar(xmlDoc, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        public RecepcionarEventosNfse(XmlDocument conteudoXML, Configuracao configuracao) : this()
            => Inicializar(conteudoXML, configuracao);

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            if (!Configuracoes.Definida)
            {
                base.DefinirConfiguracao();
            }
        }

        /// <summary>
        /// Indica se o processamento foi bem-sucedido (retornou Evento) ou ocorreu erro (retornou Temp)
        /// </summary>
        public bool Sucesso
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    try
                    {
                        XMLUtility.Deserializar<Evento>(RetornoWSXML);
                        return true;
                    }
                    catch
                    {
                        return false;
                    }
                }
                return false;
            }
        }

        /// <summary>
        /// Resultado do processamento quando bem-sucedido.
        /// Verificar propriedade Sucesso antes de acessar.
        /// </summary>
        public Evento ResultEvento
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    try
                    {
                        return XMLUtility.Deserializar<Evento>(RetornoWSXML);
                    }
                    catch
                    {
                        return null;
                    }
                }
                return null;
            }
        }

        /// <summary>
        /// Resultado do processamento quando ocorreu erro.
        /// Verificar propriedade Sucesso antes de acessar.
        /// </summary>
        public Temp ResultTemp
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    try
                    {
                        return XMLUtility.Deserializar<Temp>(RetornoWSXML);
                    }
                    catch
                    {
                        // Se falhar ao deserializar, retorna erro genérico
                        return new Temp
                        {
                            Erro = new Erro
                            {
                                Codigo = "0",
                                Descricao = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                            }
                        };
                    }
                }

                return new Temp
                {
                    Erro = new Erro
                    {
                        Codigo = "0",
                        Descricao = "Não há retorno do servidor para processar."
                    }
                };
            }
        }

        /// <summary>
        /// Resultado do processamento do evento.
        /// [OBSOLETO] Use as propriedades Sucesso, ResultEvento e ResultTemp para melhor compatibilidade COM.
        /// Retorna Evento (sucesso) ou Temp (erro) dependendo da resposta do servidor.
        /// </summary>
        [Obsolete("Use as propriedades Sucesso, ResultEvento e ResultTemp para melhor compatibilidade COM Interop.")]
        public object Result
        {
            get
            {
                return Sucesso ? (object)ResultEvento : (object)ResultTemp;
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="conteudoXML">Conteúdo do XML que será enviado para o WebService</param>
        /// <param name="configuracao">Objeto "Configuracoes" com as propriedade necessária para a execução do serviço</param>
        [ComVisible(true)]
        public override void Executar(string conteudoXML, Configuracao configuracao)
            => base.Executar(conteudoXML, configuracao);

#endif

        /// <summary>
        /// Executar o serviço
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public override void Executar()
        {
            if (!Configuracoes.Definida)
            {
                DefinirConfiguracao();
            }

            base.Executar();
        }
    }
}