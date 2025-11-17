// ------------------------------------------------------------------
// Gerar NFSe - Padrão NACIONAL (xHarbour)
// ------------------------------------------------------------------
FUNCTION NACIONALConsultarNFSeDPS()
   LOCAL oConfiguracao      := NIL
   LOCAL oConsultarNfsePorRps := NIL
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
		oConfiguracao:Servico            := 36         // NFSeConsultarNfsePorRps
		oConfiguracao:SchemaVersao       := "1.00"
		
		cXML :=;
			'<?xml version="1.0" encoding="utf-8"?>' + ;
			'<DPS versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">' + ;
			'	<infDPS Id="DPS420460822244139900018900001000000000000749"/>' + ;
			'</DPS>'

		oConsultarNfsePorRps := CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
		oConsultarNfsePorRps:Executar(cXML, oConfiguracao)

		? "XML retornado pela prefeitura:"
		?
		? oConsultarNfsePorRps:RetornoWSString
		?
		?
		Wait
		
    * Gravar retorno do ambiente nacional
      nHandle := fCreate("d:\testenfe\nfsenacional_retornoconsultadps.xml")
      fwrite(nHandle, oConsultarNfsePorRps:RetornoWSString)
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