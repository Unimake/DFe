* ------------------------------------------------------------------
* Consultar NFSe - Padrão NACIONAL
* ------------------------------------------------------------------
FUNCTION NACIONALConsultarNFSe()
   LOCAL oConfiguracao
   LOCAL oConsultarNfse
   LOCAL oExceptionInterop
   LOCAL cXML
   LOCAL oErro
	
 * Criar objeto para pegar exceção do lado do C#
   oExceptionInterop = CreateObject( "Unimake.Exceptions.ThrowHelper" )
	
   TRY
      oConfiguracao = CreateObject( "Unimake.Business.DFe.Servicos.Configuracao" )
      oConfiguracao.TipoDFe            = 5          && 5 = NFSe
      oConfiguracao.CertificadoArquivo = "D:\projetos\certificados\DosClientes\nfse_nacional_d29m05sa.pfx"
      oConfiguracao.CertificadoSenha   = "d29m05sa"
      oConfiguracao.CodigoMunicipio    = 1001058    && Padrão Nacional
      oConfiguracao.TipoAmbiente       = 2          && Homologacao
      oConfiguracao.Servico            = 32         && NFSeConsultarNfse
      oConfiguracao.SchemaVersao       = "1.00"
		
      cXML = '<?xml version="1.0" encoding="utf-8"?>'
      cXML = cXML + '<NFSe versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">'
      cXML = cXML + '	<infNFSe Id="NFS42046082222441399000189000000000000125110511831204"/>'
      cXML = cXML + '</NFSe>'
      
      oConsultarNfse = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfse")
      oConsultarNfse.Executar(cXML, oConfiguracao)

      MESSAGEBOX("XML retornado pela prefeitura:" + oConsultarNfse.RetornoWSString)
		
    * Gravar retorno do ambiente nacional
      DELETE FILE "d:\testenfe\nfsenacional_retornoconsultanfse.xml"
	  StrToFile(oConsultarNfse.RetornoWSString, "d:\testenfe\nfsenacional_retornoconsultanfe.xml", 0)	  
    
		
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN