* ---------------------------------------------------------------------------------
* Gerar o XML do evento de cancelamento do CTe e Enviar para SEFAZ
* ---------------------------------------------------------------------------------
FUNCTION EnviarEventoCancCTe()         
   LOCAL oConfiguracao, oExceptionInterop
   LOCAL oEventoCTe, oDetEventoCanc, oInfEvento
   LOCAL oRecepcaoEvento

 * Criar o objeto de configuração mínima
   oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao.TipoDFe = 2 && 2=CTe
   oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao.CertificadoSenha = "12345678"   

 * Criar XML 
  
 * Criar tag do lote de eventos <eventoCTe>
   oEventoCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EventoCTe")
   oEventoCTe.Versao = "3.00"

 * Criar tag <detEvento>
   oDetEventoCanc = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.DetEventoCanc")
   oDetEventoCanc.VersaoEvento = "3.00"
   oDetEventoCanc.NProt = "141190000660363"
   oDetEventoCanc.XJust = "Justificativa para cancelamento da NFe de teste"
   
 * Criar tag <infEvento>
   oInfEvento = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.InfEvento")   
   
 * Adicionar o Objeto oDetEventoCanc dentro do objeto DetEvento
   oInfEvento.DetEvento = oDetEventoCanc
   
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCanc para que funcione sem erro
   oInfEvento.COrgao = 41 && UFBrasil.PR
   oInfEvento.ChCTe = "41191006117473000150550010000579281779843610"
   oInfEvento.CNPJ = "06117473000150"
   oInfEvento.DhEvento = DATETIME()
   oInfEvento.TpEvento = 110111 && TipoEventoCTe.Cancelamento
   oInfEvento.NSeqEvento = 1
   oInfEvento.TpAmb = 2 && TipoAmbiente.Homologacao   
    
 * Adicionar a tag <infEvento> dentro da tag <eventoCTe>   
   oEventoCte.InfEvento = oInfEvento  
   
 * Resgatando alguns dados do objeto do XML do evento
   MESSAGEBOX(oEventoCTe.Versao)
   MESSAGEBOX(oEventoCTe.InfEvento.COrgao)
   MESSAGEBOX(oEventoCTe.InfEvento.CNPJ) 
   MESSAGEBOX(oEventoCTe.InfEvento.DhEvento)
 
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CREATEOBJECT("Unimake.Exceptions.ThrowHelper")

   TRY
    * Enviar evento
      oRecepcaoEvento = CREATEOBJECT("Unimake.Business.DFe.Servicos.CTe.RecepcaoEvento")
      oRecepcaoEvento.SetXMLConfiguracao(oEventoCTe, oConfiguracao)
	  
	  MESSAGEBOX(oRecepcaoEvento.GetConteudoXMLAssinado())
	  
	  oRecepcaoEvento.Executar(oEventoCTe, oConfiguracao)
	  
	  MESSAGEBOX("CStat Retornado: " + ALLTRIM(STR(oRecepcaoEvento.Result.InfEvento.CStat,6)))
	  MESSAGEBOX("XMotivo Retornado: " + oRecepcaoEvento.Result.InfEvento.XMotivo)
	  MESSAGEBOX(oRecepcaoEvento.RetornoWSString)

      DO CASE
         CASE oRecepcaoEvento.Result.InfEvento.CStat == 134 && Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
         CASE oRecepcaoEvento.Result.InfEvento.CStat == 135 && Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
         CASE oRecepcaoEvento.Result.InfEvento.CStat == 156 && Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
              oRecepcaoEvento.GravarXmlDistribuicao("tmp\testenfe") && Grava o XML de distribuição
              
         OTHERWISE
              * Evento rejeitado. Realizar as ações necessárias.
      ENDCASE
       
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      