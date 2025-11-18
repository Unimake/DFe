* ------------------------------------------------------------------
* Gerar NFSe - Padrão NACIONAL
* ------------------------------------------------------------------
FUNCTION NACIONALGerarNFSe()
   LOCAL oConfiguracao 
   LOCAL oGerarNFSe
   LOCAL oExceptionInterop
   LOCAL cXML
   LOCAL oErro, notaAssinada, cArqXML

 * Criar objeto para pegar exceção do lado do C#
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try

    * Criar objeto de configuração mínima
      oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
      oConfiguracao.TipoDFe            = 5          && 5 = NFSe
      oConfiguracao.CertificadoArquivo = "D:\projetos\certificados\DosClientes\nfse_nacional_d29m05sa.pfx"
      oConfiguracao.CertificadoSenha   = "d29m05sa"
      oConfiguracao.CodigoMunicipio    = 1001058    && Padrão Nacional
      oConfiguracao.TipoAmbiente       = 2          && Homologacao
      oConfiguracao.Servico            = 27         && NFSeGerarNFSe
      oConfiguracao.SchemaVersao       = "1.00"
		
    * Ler a string do XML	
      cArqXML = "D:\testenfe\nfse_envio_95664.xml"		
      cXML    = STRTRAN( ;
                    STRTRAN( ;
                      STRTRAN(FILETOSTR(cArqXML), CHR(13), ""), ; && remove CR
                    CHR(10), ""), ;                              && remove LF
                  CHR(9), "")                                    && remove TAB

    * Criar serviço de geração de NFSe
      oGerarNFSe = CreateObject( "Unimake.Business.DFe.Servicos.NFSe.GerarNfse" )

    * Em xHarbour normalmente podemos passar o objeto de configuração direto
      oGerarNFSe.Executar( cXML, oConfiguracao )
		
    * Gravar o xml assinado
      notaAssinada = oGerarNFSe.GetConteudoXMLAssinado()
      MESSAGEBOX(notaAssinada) && Demonstrar o XML da nota assinada na tela		
		
      DELETE FILE "d:\testenfe\nfsenacional.xml"
	  StrToFile(notaAssinada, "d:\testenfe\nfsenacional.xml", 0)	  
		
    * Gravar retorno do ambiente nacional
      DELETE FILE "d:\testenfe\nfsenacional_retorno.xml"
	  StrToFile(oGerarNFSe.RetornoWSString, "d:\testenfe\nfsenacional_retorno.xml", 0)	  
    
    * Ver se a nota foi autorizada
      If '<infNFSe Id="NFS' $ oGerarNFSe.RetornoWSString
		   
      Else
         MESSAGEBOX("XML Rejeitado!!! " + oGerarNFSe.RetornoWSString)
      Endif

   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN