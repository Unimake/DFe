* ---------------------------------------------------------------------------------
* Enviar Consulta Lote RPS para prefeitura
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarConsultaLoteRPS()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oConsultaLote, cArqXML, cStr

 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 5 //5=NFSe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 48 //Servico.NFSeConsultaLote
   oConfiguracao:CodigoMunicipio = 3550308 //São Paulo  
   oConfiguracao:SchemaVersao = "2.00"
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao
   
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      cArqXML := "D:\testenfe\xharbour\Unimake.DFe\ConsultaLote-ped-loterps.xml"
	  cStr := Memoread(cArqXML)
	  cStr := SubStr(cStr, 4)
     
	  ? "String do XML:"
	  ?
	  ?
	  ? cStr
	  ?
	  ?
	  wait
	  cls 
	  
	  oConsultaLote := CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultaLote")
      oConsultaLote:Executar(cStr, oConfiguracao)
	  
	  ? oConsultaLote:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar a NFSe."
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