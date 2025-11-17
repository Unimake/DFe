// ------------------------------------------------------------------
// Gerar NFSe - Padrão NACIONAL (xHarbour)
// ------------------------------------------------------------------
FUNCTION NACIONALConsultarNFSe()
   LOCAL oConfiguracao      := NIL
   LOCAL oConsultarNfse := NIL
   LOCAL oExceptionInterop  := NIL
   LOCAL cXML               := ""
   LOCAL oError, nHandle
	
   // Criar objeto para pegar exceção do lado do C#
   oExceptionInterop := CreateObject( "Unimake.Exceptions.ThrowHelper" )
	
	Try
		oConfiguracao := CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
		oConfiguracao:TipoDFe            := 5          // 5 = NFSe
		oConfiguracao:CertificadoArquivo := "D:\projetos\certificados\DosClientes\nfse_nacional_d29m05sa.pfx"
		oConfiguracao:CertificadoSenha   := "d29m05sa"
		oConfiguracao:CodigoMunicipio    := 1001058    // Padrão Nacional
		oConfiguracao:TipoAmbiente       := 2          // Homologacao
		oConfiguracao:Servico            := 32         // NFSeConsultarNfse
		oConfiguracao:SchemaVersao       := "1.00"
		
		cXML :=;
			'<?xml version="1.0" encoding="utf-8"?>' + ;
			'<NFSe versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">' + ;
			'	<infNFSe Id="NFS42046082222441399000189000000000000125110511831204"/>' + ;
			'</NFSe>'

		oConsultarNfse := CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse")
		oConsultarNfse:Executar(cXML, oConfiguracao)

		? "XML retornado pela prefeitura:"
		?
		? oConsultarNfse:RetornoWSString
		?
		?
		Wait
		
    * Gravar retorno do ambiente nacional
      nHandle := fCreate("d:\testenfe\nfsenacional_retornoconsultanfse.xml")
      fwrite(nHandle, oConsultarNfse:RetornoWSString)
      fClose(nHandle)
		
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