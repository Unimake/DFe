* ---------------------------------------------------------------------------------
* eSocial - Consultar lote assincrono
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function eSocialConsultaLoteAssincrono()
   Local oExceptionInterop, oErro
   Local oConfiguracao
   Local oConsultarLoteEventos
   Local oConsultaLoteAssincrono

 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 12 //12=eSocial
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 70 //Servico.ESocialConsultaEvts
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
 * Criar o XML de ConsultaLoteEventos
   oConsultarLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos")
   oConsultarLoteEventos:Versao = "1.0.0"
   
   oConsultaLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaLoteEventos")
   oConsultaLoteEventos:ProtocoloEnvio = "1111111111111"
  
   oConsultarLoteEventos:ConsultaLoteEventos = oConsultaLoteEventos   
  
   Try        
    * Enviar a consulta e pegar o retorno
	  oConsultaLoteAssincrono := CreateObject("Unimake.Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono")
	  
	  Wait
      oConsultaLoteAssincrono:Executar(oConsultarLoteEventos, oConfiguracao)
	  
	  Wait
	  
	  ? oConsultaLoteAssincrono:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar o XML"
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
	  Wait
	  cls   
   End
Return 