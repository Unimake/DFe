* ---------------------------------------------------------------------------------
* Gerar o XML do evento de carta de correção do CTe e Enviar para SEFAZ
* ---------------------------------------------------------------------------------
FUNCTION EnviarEventoCCeCTe()         
   LOCAL oConfiguracao, oExceptionInterop
   LOCAL oEventoCTe, oDetEventoCCE, oEventoCCeCTe, oInfCorrecao, oInfEvento
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
   oDetEventoCCE = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.DetEventoCCE")
   oDetEventoCCE.VersaoEvento = "3.00"
   
 * Criar a tag <evCCeCTe>
   oEventoCCeCTe = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.EventoCCeCTe")
   
 * Criar a tag <infCorrecao> - Primeira correção
   oInfCorrecao = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.InfCorrecao")
   oInfCorrecao.GrupoAlterado = "ide"
   oInfCorrecao.CampoAlterado = "cfop"
   oInfCorrecao.ValorAlterado = "6353"
   oInfCorrecao.NroItemAlterado = ""   
   
   oEventoCCeCTe.AddInfCorrecao(oInfCorrecao)

 * Criar a tag <infCorrecao> - Segunda correção
   oInfCorrecao = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.InfCorrecao")
   oInfCorrecao.GrupoAlterado = "ide"
   oInfCorrecao.CampoAlterado = "cfop"
   oInfCorrecao.ValorAlterado = "6223"
   oInfCorrecao.NroItemAlterado = ""   
   
   oEventoCCeCTe.AddInfCorrecao(oInfCorrecao)
   
   oDetEventoCCe.EventoCCeCTe = oEventoCCeCTe
   
 * Criar tag <infEvento>
   oInfEvento = CREATEOBJECT("Unimake.Business.DFe.Xml.CTe.InfEvento")   
   
 * Adicionar o Objeto oDetEventoCCE dentro do objeto DetEvento
   oInfEvento.DetEvento = oDetEventoCCE
   
 * Atualizar propriedades da oInfEvento
 * IMPORTANTE: Atualização da propriedade TpEvento deve acontecer depois que o DetEvento recebeu o oDetEventoCCE para que funcione sem erro
   oInfEvento.COrgao = 41 && UFBrasil.PR
   oInfEvento.ChCTe = "41191006117473000150550010000579281779843610"
   oInfEvento.CNPJ = "06117473000150"
   oInfEvento.DhEvento = DATETIME()
   oInfEvento.TpEvento = 110110 && TipoEventoCTe.CartaCorrecao
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