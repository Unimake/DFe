// ------------------------------------------------------------------
// Gerar NFSe - Padrão NACIONAL (xHarbour)
// ------------------------------------------------------------------
FUNCTION NACIONALGerarNFSe()
   LOCAL oConfiguracao      := NIL
   LOCAL oGerarNFSe         := NIL
   LOCAL oExceptionInterop  := NIL
   LOCAL cXML               := ""
   LOCAL oError, notaAssinada, nHandle, cArqXML

   // Criar objeto para pegar exceção do lado do C#
   oExceptionInterop := CreateObject( "Unimake.Exceptions.ThrowHelper" )

   Try

      // Criar objeto de configuração mínima
      oConfiguracao := CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
      oConfiguracao:TipoDFe            := 5          // 5 = NFSe
      oConfiguracao:CertificadoArquivo := "D:\projetos\certificados\DosClientes\nfse_nacional_d29m05sa.pfx"
      oConfiguracao:CertificadoSenha   := "d29m05sa"
      oConfiguracao:CodigoMunicipio    := 1001058    // Padrão Nacional
      oConfiguracao:TipoAmbiente       := 2          // Homologacao
      oConfiguracao:Servico            := 27         // NFSeGerarNFSe
      oConfiguracao:SchemaVersao       := "1.00"
		
	 * Ler a string do XML	
		cArqXML := "D:\testenfe\nfse_envio_95664.xml"		
      cXML := StrTran(StrTran(StrTran(Memoread(cArqXML), Chr(13), ""), Chr(10), ""), "	","")

      // Criar serviço de geração de NFSe
      oGerarNFSe := CreateObject( "Unimake.Business.DFe.Servicos.NFSe.GerarNfse" )

      // Em xHarbour normalmente podemos passar o objeto de configuração direto
      oGerarNFSe:Executar( cXML, oConfiguracao )
		
    * Gravar o xml assinado
      notaAssinada := oGerarNFSe:GetConteudoXMLAssinado()
      ? notaAssinada //Demonstrar o XML da nota assinada na tela		
		
      nHandle := fCreate("d:\testenfe\nfsenacional.xml")
      fwrite(nHandle, notaAssinada)
      fClose(nHandle)		

		
		Wait
		cls
				 
    * Gravar retorno do ambiente nacional
      nHandle := fCreate("d:\testenfe\nfsenacional_retorno.xml")
      fwrite(nHandle, oGerarNFSe:RetornoWSString)
      fClose(nHandle)
		
		//Ver se a nota foi autorizada
		If [<infNFSe Id="NFS] $ oGerarNFSe:RetornoWSString
		   
		Else
		   ? "XML Rejeitado!!!"
			?
         ? "XML retornado pelo ambiente nacional: "
			?
	   	? oGerarNFSe:RetornoWSString
		   ?
		   ?
			Wait
		Endif

   Catch oError

      IF oError != NIL .AND. ValType( oError ) == "O"
         ? "Erro ao gerar NFSe: " + oError:Description
      ELSE
         ? "Erro ao gerar NFSe (sem detalhes do erro Harbour)."
      ENDIF

      // Erro detalhado vindo do lado C#
      ? oExceptionInterop:GetMessage()
      ? "Código do erro: " + LTrim( Str( oExceptionInterop:GetErrorCode() ) )
		?
		?
		Wait
   End

RETURN NIL