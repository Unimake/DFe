* ---------------------------------------------------------------------------------
* Testes diversos com certificado digital no Harbour versão 3.x
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function TesteDiversoCertificadoHarbour3x()
   Local oCertificado, oCertificado2, cBase64, oConfig
   Local oErro, oExceptionInterop
   Private nConta := 0

   Cls

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")

   Try
    * Criar objeto para trabalhar com certificado digital no INTEROP
	  oCertificado := CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")
	  
    * Carregar o certificado A1 direto do .PFX
	  oCertificado:CarregarCertificadoDigitalA1("C:\Projetos\certificados\UnimakePV.pfx","12345678")	  
  
    * Demonstrar os dados do certificado digital
      ? "ID do Certificado....: ", oCertificado:GetThumbPrint()
      ? "Dados do proprietario: ", oCertificado:GetSubject()
      ? "Numero de Serie......: ", oCertificado:GetSerialNumber()
      ? "Validade Inicial.....: ", oCertificado:GetNotBefore()
      ? "Validade Final.......: ", oCertificado:GetNotAfter()
      ? "Certificado vencido?.: ", oCertificado:Vencido()
      ?
	  
	  Wait 

    * Criar objeto para trabalhar com certificado digital no INTEROP
      oCertificado2 := CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")

    * Carregar o certificado partindo da tela de sele磯 dos instalados no repositorio do windows	  
	  oCertificado2:AbrirTelaSelecao() 
	  
 	  cSerialNumber := oCertificado2:GetSerialNumber()
	  cThumbPrint   := oCertificado2:GetThumbPrint()
	  
    * Demonstrar os dados do certificado digital
	  ?
      ?
      ? "ID do Certificado....: ", oCertificado2:GetThumbPrint()
      ? "Dados do proprietario: ", oCertificado2:GetSubject()
      ? "Numero de Serie......: ", oCertificado2:GetSerialNumber()
      ? "Validade Inicial.....: ", oCertificado2:GetNotBefore()
      ? "Validade Final.......: ", oCertificado2:GetNotAfter()
      ? "Certificado vencido?.: ", oCertificado2:Vencido()
      ?
      Wait	

    * Consultar o status do servi篠da NFe - Usando a propriedade do CertificadoBase64
      //Transformar o certificado em base64
      cBase64 = oCertificado:ToBase64("C:\Projetos\certificados\UnimakePV.pfx")
	  
      oConfig                   := CreateObject("Unimake.Business.Dfe.Servicos.Configuracao")
      oConfig:TipoDfe           := 0	  
	  oConfig:CertificadoBase64 := cBase64
	  oConfig:CertificadoSenha  := "12345678"
	  
	  ? cBase64
	  
	  Wait
	  
	  ConsultaStatus(oConfig)
	  
    * Transformar o Base64 do Certificado em Certificado
	  Cls
	  ? "Opa!!! Vamos comecar?"
	  Wait 
	  
	  oCertificado3 := CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")
	  oCertificado3:FromBase64(cBase64, "12345678")
	  
      ?
      ? "ID do Certificado....: ", oCertificado3:GetThumbPrint()
      ? "Dados do proprietario: ", oCertificado3:GetSubject()
      ? "Numero de Serie......: ", oCertificado3:GetSerialNumber()
      ? "Validade Inicial.....: ", oCertificado3:GetNotBefore()
      ? "Validade Final.......: ", oCertificado3:GetNotAfter()
      ? "Certificado vencido?.: ", oCertificado3:Vencido()
      ?
      Wait		  
	  
	  
    * Consultar o status do servi篠da NFe - Usando a propriedade do arquivo .PFX do certificado
      oConfig                    := CreateObject("Unimake.Business.Dfe.Servicos.Configuracao")
      oConfig:TipoDfe            := 0
      oConfig:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
      oConfig:CertificadoSenha   := "12345678"

      ConsultaStatus(oConfig)
	  
    * Consultar o status do servi篠da NFe - Usando a propriedade do SerialNumber
      oConfig                   := CreateObject("Unimake.Business.Dfe.Servicos.Configuracao")	  
      oConfig:TipoDfe           := 0
	  oConfig:CertificadoSerialNumberOrThumbPrint := cSerialNumber

	  ConsultaStatus(oConfig)
	  
    * Consultar o status do servi篠da NFe - Usando a propriedade do Thumbprint
      oConfig                   := CreateObject("Unimake.Business.Dfe.Servicos.Configuracao")
      oConfig:TipoDfe           := 0
      oConfig:CertificadoSerialNumberOrThumbPrint := cThumbPrint

      ConsultaStatus(oConfig)	  
	
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
      ? "ERRO"
      ? "===="
      ? "Falha ao tentar pegar os dados do certificado."
      ?
      ? "Excecao DO HARBOUR"
      ? "Excecao (DESCRICAO): " + oErro:Description
      ? "Excecao (OPERACAO): " + oErro:Operation

      //Demonstrar a exceção do CSHARP
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?

      Wait
      cls
   End
Return

Static Function ConsultaStatus(oConfig)
	 Local oConsStatServ, oStatusServico

	 //Criar o XML para consultar o status da NFE
	 oConsStatServ        	  := CreateObject("Unimake.Business.Dfe.XML.Nfe.ConsStatServ")
	 oConsStatServ:Versao 	  := "4.00"
	 oConsStatServ:cUf    	  := 41
	 oConsStatServ:TpAmb  	  := 2

	 //Consultar o status do servi篠da NFe
	 oStatusServico				  := CreateObject("Unimake.Business.Dfe.Servicos.Nfe.StatusServico")
	 oStatusServico:Executar(oConsStatServ, oConfig)

	 Cls

	 //Demonstrar o retorno da SEFAZ
	 ? "Status do servico - consulta numero " + Str(++nConta,1)+"/4"
	 ?
	 ? Trim(Str(oStatusServico:Result:cStat,3))
	 ? oStatusServico:Result:XMotivo
	 ?
	 ?

	 Wait
Return