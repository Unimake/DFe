* ---------------------------------------------------------------------------------
* Testes diversos com certificado digital 
* ---------------------------------------------------------------------------------
Function TestesDiversosCertificado()
   Local oCertificado, oCertSel1, oCertSel2, oCertSel3, oCertSel4, oCertSel5, oCertA3
   Local thumbprint, serialNumber
   Local oErro, oExceptionInterop, oErro2
   
 * Criar objeto para pegar excecao do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Criar objeto para trabalhar com certificados digitais
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
       
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Certificado A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que esta instalado no repositorio do windows
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  	   MESSAGEBOX("A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que está instalado no repositorio do windows")
	   
	   oCertSel1 = oCertificado.AbrirTelaSelecao()
	   
     * Voce pode salvar o Thumbprint ou SerialNumber do certificado para salvar em sua base de dados para resgatar ele no futuro no repositório do windows sem precisar abrir tela para selecionar novamente.
	   
	   thumbPrint = oCertificado.GetThumbPrint(oCertSel1)
	   serialNumber = oCertificado.GetSerialNumber(oCertSel1)
	   
	   MESSAGEBOX("Thumbprint: " + thumbPrint)
       MESSAGEBOX("Serial Number: " + serialNumber)
	   
	   MostrarDados(oCertificado, oCertSel1)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Somente certificado A1 - Testar a senha do certificado A1
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Tentar carregar o certificado A1 do Path e pegar a exceção
	 
	   TRY
          oCertificado.CarregarCertificadoDigitalA1("c:\projetos\certificados\UnimakePV.pfx", "12345678")
	   CATCH TO oErro2
   	    * Deu algum erro na hora de carregar o certificado A1, dentre eles, se for a senha incorreta, a mensagem será bem clara para o usuário.
	   	  MessageBox(oErro2.Message) 
	   ENDTRY
	   
		   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Somente certificado A1 - Carregar o certificado digital direto do arquivo .PFX.
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   MESSAGEBOX("A1 - Carregar o certificado digital direto do arquivo .PFX.")

       oCertSel2 = oCertificado.CarregarCertificadoDigitalA1("c:\projetos\certificados\UnimakePV.pfx", "12345678")
	   
	   MostrarDados(oCertificado, oCertSel2)	   
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     * Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       MESSAGEBOX("A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number")
	   
	   oCertSel3 = oCertificado.BuscarCertificadoDigital(serialNumber)
	   
	   MostrarDados(oCertificado, oCertSel3)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   MESSAGEBOX("A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint")
	   
	   oCertSel4 = oCertificado.BuscarCertificadoDigital(thumbPrint)
	   
	   MostrarDados(oCertificado, oCertSel4)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     * Certificado A1 - Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior seguranca) para resgatar o conteudo direto da base de dados.
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   MESSAGEBOX("Transformando um arquivo de certificado A1 em Base64")
	   
	   certBase64 = oCertificado.ToBase64("C:\Projetos\certificados\UnimakePV.pfx")
	   
	   MESSAGEBOX(certBase64)
	   
	 * Agora você pode gravar o conteúdo da "certBase64" no banco de dados
	   
	 * Recuperar o certificado para uso a partir de um Base64
	   oCertSel5 = oCertificado.FromBase64(certBase64, "12345678")
	   
	   MostrarDados(oCertificado, oCertSel5)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     * Certificado A3 - Setar o PIN do A3 para não precisar informar manualmente
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  	   MESSAGEBOX("Setar o PIN do A3 para nao precisar informar manualmente")
	   
	   oCertA3 = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")
	   
	 * Ver se é um certificado A3
	   If oCertA3.IsA3(oCertSel1)
	      MESSAGEBOX("É certificado A3")
		  
		  oCertA3.SetPinPrivateKey(oCertSel1, "123456")
	   else
	      MESSAGEBOX("É certificado A1")
	   endif
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY   
Return


Function MostrarDados(oCert, oCertSel)
   Local oVerificarA3

   MESSAGEBOX("ID do Certificado"     + chr(13)+chr(10)+ oCert.GetThumbPrint(oCertSel) + chr(13)+chr(10)+chr(13)+chr(10) +;
              "Dados do proprietario" + chr(13)+chr(10)+ oCert.GetSubject(oCertSel) + chr(13)+chr(10)+chr(13)+chr(10) +;
              "Numero de Serie"       + chr(13)+chr(10)+ oCert.GetSerialNumber(oCertSel) + chr(13)+chr(10)+chr(13)+chr(10) + ;
              "Validade Inicial"      + chr(13)+chr(10)+ oCert.GetNotBefore(oCertSel) + chr(13)+chr(10)+chr(13)+chr(10) +;
              "Validade Final"        + chr(13)+chr(10)+ oCert.GetNotAfter(oCertSel) + chr(13)+chr(10)+chr(13)+chr(10) +;
              "Certificado vencido?"  + chr(13)+chr(10)+ IIF(oCert.Vencido(oCertSel), "SIM", "NÃO")) 
Return