* ---------------------------------------------------------------------------------
* Enviar cancelamento de NFSe padrão Paulistana (Município de São Paulo)
* ---------------------------------------------------------------------------------
FUNCTION EnviarCancPadraoPAULISTANA()         
   LOCAL xmlCanc, oConfiguracao 
   LOCAL oErro, oCancelamentoNfe
      
   TRY 
    * ------------------------------------------------------   
    * Consulta da NFSe por RPS
    * ------------------------------------------------------
    * Criar o XML
      xmlCanc = ""
      xmlCanc = xmlCanc + AllTrim([<?xml version="1.0" encoding="UTF-8"?>])
      xmlCanc = xmlCanc + AllTrim([<PedidoCancelamentoNFe xmlns="http://www.prefeitura.sp.gov.br/nfe">])
      xmlCanc = xmlCanc + AllTrim([	<Cabecalho Versao="1" xmlns="">])
      xmlCanc = xmlCanc + AllTrim([		<CPFCNPJRemetente>])
      xmlCanc = xmlCanc + AllTrim([			<CNPJ>99999997000100</CNPJ>])
      xmlCanc = xmlCanc + AllTrim([		</CPFCNPJRemetente>])
      xmlCanc = xmlCanc + AllTrim([		<transacao>true</transacao>])
      xmlCanc = xmlCanc + AllTrim([	</Cabecalho>])
      xmlCanc = xmlCanc + AllTrim([	<Detalhe xmlns="">])
      xmlCanc = xmlCanc + AllTrim([		<ChaveNFe>])
      xmlCanc = xmlCanc + AllTrim([			<InscricaoPrestador>39616924</InscricaoPrestador>])
      xmlCanc = xmlCanc + AllTrim([			<NumeroNFe>00017945</NumeroNFe>])
      xmlCanc = xmlCanc + AllTrim([		</ChaveNFe>])
      xmlCanc = xmlCanc + AllTrim([		<AssinaturaCancelamento>ZWwfEUQPeVgHhBmz/rznSelg1ZX3p+RbYeLowlgQuaLvcyPOpIK7NVziLkFa5TvqXAEDmF+r])
      xmlCanc = xmlCanc + AllTrim([VaqKgdFV6qHwRnzjI+8eAJ1Gss0DvMPDzhCl/dmQdc0WRA+RwgmBveGa9w0fYnQChCH/gXiZq2r3YsFOa616yGrNY168V4Q+NTZCWKTv6PbeVnRONuO04rQLdrZPF/agMBKr])
      xmlCanc = xmlCanc + AllTrim([10mMlG8iWWRaxOidcFkLvGHoU3sZ7UOvQVkGMWSupT0PUrtZbQfZzag8YQX1GxeHyO0jqs4Zi82jeIaofJPuVLB6EwWheuyna+DTkavPyfv6ri2qHm1zn4SlAGxQuilv1RHQ])
      xmlCanc = xmlCanc + AllTrim([o9T8uw==</AssinaturaCancelamento>])
      xmlCanc = xmlCanc + AllTrim([	</Detalhe>])
      xmlCanc = xmlCanc + AllTrim([</PedidoCancelamentoNFe>])

   * Consumir o serviço de consulta
      oConfiguracao = CREATEOBJECT("Unimake.Business.DFe.Servicos.Configuracao")
      oConfiguracao.TipoDFe = 5 && TipoDFe.NFSe
      oConfiguracao.CertificadoArquivo = "C:\Projetos\UnimakeCM.pfx"
      oConfiguracao.CertificadoSenha = "12345678"   
      oConfiguracao.TipoAmbiente =  1 && Produção
      oConfiguracao.CodigoMunicipio = 3550308 && Código do IBGE de São Paulo-SP
      oConfiguracao.Servico = 46  && Servico.NFSeCancelamentoNfe
      oConfiguracao.SchemaVersao = "2.00"
      
      oCancelamentoNfe = CREATEOBJECT("Unimake.Business.DFe.Servicos.NFSe.CancelamentoNfe")
      oCancelamentoNfe.Executar(xmlCanc, oConfiguracao)  
      
      MESSAGEBOX(oCancelamentoNfe.RetornoWSString)     


   CATCH TO oErro
    * Excecao do FOXPRO
	* Mais sobre excecao em FOXPRO
	* http://www.yaldex.com/fox_pro_tutorial/html/2344b71b-14c0-4125-b001-b5fbb7bd1f05.htm
	
	  MESSAGEBOX("FOXPRO - ErrorCode: " + ALLTRIM(STR(oErro.ErrorNo,10))+ " - Message: " + oErro.Message)
	  
    * Excecao do CSHARP
      MESSAGEBOX("CSHARP - ErrorCode: " + ALLTRIM(STR(oExceptionInterop.GetErrorCode(),20)) + " - Message: " + oExceptionInterop.GetMessage())   
   ENDTRY      
RETURN      