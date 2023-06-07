* ---------------------------------------------------------------------------------
* Enviar cancelamento de NFSe padrão Betha (Município de Itapema-SC)
* ---------------------------------------------------------------------------------
FUNCTION EnviarCancPadraoBETHA()         
   LOCAL xmlCanc, oConfiguracao 
   LOCAL oErro, oCancelarNfse
      
   TRY 
    * ------------------------------------------------------   
    * Cancelar NFSe
    * ------------------------------------------------------
    * Criar o XML
      xmlCanc = ""
      xmlCanc = xmlCanc + AllTrim([<?xml version="1.0" encoding="utf-8"?>])
      xmlCanc = xmlCanc + AllTrim([<CancelarNfseEnvio xmlns="http://www.betha.com.br/e-nota-contribuinte-ws">])
      xmlCanc = xmlCanc + AllTrim([  <Pedido>])
      xmlCanc = xmlCanc + AllTrim([    <InfPedidoCancelamento Id="C7000">])
      xmlCanc = xmlCanc + AllTrim([      <IdentificacaoNfse>])
      xmlCanc = xmlCanc + AllTrim([        <Numero>58</Numero>])
      xmlCanc = xmlCanc + AllTrim([        <CpfCnpj>])
      xmlCanc = xmlCanc + AllTrim([          <Cnpj>45111111111100</Cnpj>])
      xmlCanc = xmlCanc + AllTrim([        </CpfCnpj>])
      xmlCanc = xmlCanc + AllTrim([        <InscricaoMunicipal>123498</InscricaoMunicipal>])
      xmlCanc = xmlCanc + AllTrim([        <CodigoMunicipio>4204608</CodigoMunicipio>])
      xmlCanc = xmlCanc + AllTrim([      </IdentificacaoNfse>])
      xmlCanc = xmlCanc + AllTrim([      <CodigoCancelamento>1</CodigoCancelamento>])
      xmlCanc = xmlCanc + AllTrim([    </InfPedidoCancelamento>])
      xmlCanc = xmlCanc + AllTrim([  </Pedido>])
      xmlCanc = xmlCanc + AllTrim([</CancelarNfseEnvio>])
      
   * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  2 && Produção
      oConfiguracao.CodigoMunicipio = 4208302 && Código do IBGE de Itapema-SC ###
      oConfiguracao.Servico = 24  && Servico.NFSeCancelarNfse
      oConfiguracao.SchemaVersao = "2.02"
      
      oCancelarNfse = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.CancelarNfse")
      oCancelarNfse.Executar(xmlCanc, oConfiguracao)  
      
      MESSAGEBOX(oCancelarNfse.RetornoWSString)     


   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      