* ---------------------------------------------------------------------------------
* Gerar o XML da GNRE e Enviar para SEFAZ
* ---------------------------------------------------------------------------------
FUNCTION EnviarGNRE()         
   LOCAL oConfiguracao, oExceptionInterop
   LOCAL oTLoteGNRE, oGuias, oTDadosGNRE, oContribuinteEmitente, oIdentificacao 
   LOCAL oItensGNRE, oItem, oDocumentoOrigem, oValor
   LOCAL oLoteRecepcao, oConfigConsLote, oConsultaResultadoLote, oTConsLoteGNRE 

 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDfe = 8 && TipoDFe.GNRE
   oConfiguracao.TipoEmissao = 1 && TipoEmissao.Normal
   oConfiguracao.TipoAmbiente = 2 && TipoAmbiente.Homologacao
   oConfiguracao.CodigoUF = 41 && UFBrasil.PR
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"

 * Criar XML  
 
 * Criar grupo de tag TLoteGNRE
   oTLoteGNRE = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.TLoteGNRE")   
   
 * Criar grupo de tag Guias  
   oGuias = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.Guias")
   
 * Criar tag TDadosGNRE  
   oTDadosGNRE = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.TDadosGNRE")
   oTDadosGNRE.Versao = "2.00"
   oTDadosGNRE.UfFavorecida = 41 && UFBrasil.PR
   oTDadosGNRE.TipoGNRE = 0 && TipoGuiaGNRE.Simples
   oTDadosGNRE.ValorGNRE = 30.00
   oTDadosGNRE.DataPagamento = DATETIME()
   
 * Criar grupo de tag ContribuinteEmitente  
   oContribuinteEmitente = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.ContribuinteEmitente")
   
 * Criar grupo de tag Identificacao  
   oIdentificacao = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.Identificacao")
   oIdentificacao.CNPJ = "07666666000166"
   oIdentificacao.IE = "9335665656"
   
   oContribuinteEmitente.Identificacao = oIdentificacao && Atualizar conteúdo do grupo de tag identificacao
   
   oContribuinteEmitente.RazaoSocial = "TESTE EMPRESA PARA ENVIO DA GNRE"
   oContribuinteEmitente.Endereco = "XXX XXXXXXX XXXXX"
   oContribuinteEmitente.Municipio = "04808"
   oContribuinteEmitente.UF = 41 && UFBrasil.PR
   oContribuinteEmitente.CEP = "90399899"
   oContribuinteEmitente.Telefone = "04456566566"

   oTDadosGNRE.ContribuinteEmitente = oContribuinteEmitente && Atualizar conteúdo do grupo de tag ContribuinteEmitente   
   
 * Criar Grupo de tag ItensGNRE
   oItensGNRE = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.ItensGNRE")
   
 * Criar grupo de tag Item  
   oItem = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.Item")
   oItem.Receita = "100099"
   oItem.DataVencimento = DATETIME()
   
   oDocumentoOrigem = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.DocumentoOrigem")
   oDocumentoOrigem.Tipo = "10"
   oDocumentoOrigem.Value = "41210807666666000166550010001234551123455553"
   oItem.DocumentoOrigem = oDocumentoOrigem && Atualizar conteudo do grupo de tag Documento Origem

 * Criar grupo de tag Valor
   oValor = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.Valor")
   oValor.Tipo = 0 && ItemValorTipo.Item11
   oValor.ValorOriginal = 116.24
   
   oItem.AddValor(oValor) && Atualizar conteúdo do grupo de tag Valor
   
   oItensGNRE.AddItem(oItem) && Atualizar conteúdo do grupo de tag Item
   
   oTDadosGNRE.ItensGNRE = oItensGNRE && Atualizar conteudo do grupo de tag ItensGNRE
    
   oGuias.AddTDadosGNRE(oTDadosGNRE) && Atualizar conteúdo do grupo de tag TDadosGNRE

   oTLoteGNRE.Guias = oGuias && Atualizar o conteúdo do grupo de tag Guias

 
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
      oLoteRecepcao = CREATEOBJECT("Unimake.Business.DFe.Servicos.GNRE.LoteRecepcao")
      oLoteRecepcao.Executar(oTLoteGNRE, oConfiguracao)
      
      MESSAGEBOX(oLoteRecepcao.GetConteudoXMLAssinado())
      
      MESSAGEBOX(oLoteRecepcao.RetornoWSString)
      
      IF oLoteRecepcao.Result.SituacaoRecepcao.Codigo == "100"
       * Aguardar 30 segundos para consultar o resultado do lote, é o que solicita o manual
         
       * Criar configuraçao básica para consumir o serviço de consulta do resultado do envio da GNRE
         oConfigConsLote = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
         oConfigConsLote.TipoDfe = 8 && TipoDFe.GNRE
         oConfigConsLote.TipoEmissao = 1 && TipoEmissao.Normal
         oConfigConsLote.TipoAmbiente = 2 && TipoAmbiente.Homologacao
         oConfigConsLote.CodigoUF = 41 && UFBrasil.PR
         oConfigConsLote.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
         oConfigConsLote.CertificadoSenha = "12345678"
         
       * Criar o XML da consulta
         oTConsLoteGNRE = CREATEOBJECT("Unimake.Business.DFe.Xml.GNRE.TConsLoteGNRE")
         oTConsLoteGNRE.Ambiente = 2 && TipoAmbiente.Homologacao
         oTConsLoteGNRE.NumeroRecibo = "0000000000" && oLoteRecepcao.Result.Recibo.Numero
         oTConsLoteGNRE.IncluirPDFGuias = 1 && SimNaoLetra.Sim
         
         oConsultaResultadoLote = CREATEOBJECT("Unimake.Business.DFe.Servicos.GNRE.ConsultaResultadoLote")
         oConsultaResultadoLote.Executar(oTConsLoteGNRE , oConfigConsLote)
         
         MESSAGEBOX(oConsultaResultadoLote.RetornoWSString)
         
         DO CASE
            CASE oConsultaResultadoLote.Result.SituacaoProcess.Codigo = "400" && Lote recebido, aguardando processamento
                 * Tentar consultar mais tarde   
               
            CASE oConsultaResultadoLote.Result.SituacaoProcess.Codigo = "401" && Lote em processamento
                 * Tentar consultar mais tarde   
               
            CASE oConsultaResultadoLote.Result.SituacaoProcess.Codigo = "402" && Lote processado com sucesso
                 oConsultaResultadoLote.GravarXmlRetorno(@"d:\testenfe", xmlCons.NumeroRecibo + "-procgnre.xml");
                 oConsultaResultadoLote.GravarPDFGuia(@"d:\testenfe", "GuiaGNRE.pdf");
                 
               * Criar as configurações 
                 oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")   
                 oUnidanfeConfiguration.Arquivo = "D:\testenfe\41211207638784000127550050000001761815064891-procgnre.xml" 
                 oUnidanfeConfiguration.Visualizar = .T.
                 oUnidanfeConfiguration.Imprimir = .F.
                 oUnidanfeConfiguration.EnviaEmail = .F.    
     
               * Disparar a impressao DANFe NFe
                 oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
                 oUnidanfeServices.Execute(oUnidanfeConfiguration)  
               
            CASE oConsultaResultadoLote.Result.SituacaoProcess.Codigo = "403" && Lote processado com pendências ou Lote com pendência de tempo de processamento. As Guias com situação 4 (campo < situacaoGuia > para a versão 2.00) podem levar em média 20 minutos, e no máximo 1 hora para serem processadas.
                 * Analisar pendencias
               
            CASE oConsultaResultadoLote.Result.SituacaoProcess.Codigo = "404" && Erro no processamento do lote.
                 * Enviar novamente
                 
            OTHERWISE
                 * GNRE Rejeitada, analisar, corrigir e enviar novamente     
         ENDCASE 
       ENDIF
      
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      