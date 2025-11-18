* ------------------------------------------------------------------
* Consultar NFSe PDF - Padrão NACIONAL
* ------------------------------------------------------------------
FUNCTION NACIONALConsultarNFSePDF()
   LOCAL oConfiguracao
   LOCAL oConsultarNfsePDF
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
      oConfiguracao.Servico            = 37         && NFSeConsultarNfsePDF
      oConfiguracao.SchemaVersao       = "1.00"
		
      cXML = '<?xml version="1.0" encoding="utf-8"?>'
      cXML = cXML + '<NFSe versao="1.00" xmlns="http://www.sped.fazenda.gov.br/nfse">'
      cXML = cXML + '	<infNFSe Id="NFS42046082222441399000189000000000000125110511831204"/>'
      cXML = cXML + '</NFSe>'
      
      oConsultarNfsePDF = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePDF")
      oConsultarNfsePDF.Executar(cXML, oConfiguracao)

      MESSAGEBOX("XML retornado pela prefeitura:" + oConsultarNfsePDF.RetornoWSString)
		
    * Gravar retorno do ambiente nacional
      DELETE FILE "d:\testenfe\nfsenacional_retornoconsultanfsepdf.xml"
	  StrToFile(oConsultarNfsePDF.RetornoWSString, "d:\testenfe\nfsenacional_retornoconsultanfsepdf.xml", 0)	  
	  
	  oConsultarNfsePDF.ExtrairPDF("d:\testenfe\pdf", "nfsenacional_retornoconsultapdf.pdf", "Base64Pdf")
		
   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MessageBox("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MessageBox("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())
   ENDTRY
RETURN