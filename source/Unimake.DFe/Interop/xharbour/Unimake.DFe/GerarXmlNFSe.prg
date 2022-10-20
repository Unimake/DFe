* ---------------------------------------------------------------------------------
* Gerar XML NFSe do município de São Paulo
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function GerarXmlNFSe()
   Local cConteudoXML, mArquivoTmp
   Local oConfiguracao, oExceptionInterop, oErro
   Local nHandle, oEnvioRPS
   Local cErroCodigo, cErroDescricao, lSucesso
   
   Cls
   
   mArquivoTmp  := "d:\testenfe\NFSeSP.xml"
   nHandle      := fCreate(mArquivoTmp)
   
   If nHandle < 0
      ? "Nao foi possivel criar o arquivo " + Trim(mArquivoTmp) + "!"
	  ?
	  Return 
   EndIf

   cConteudoXML := ""
   
   UniXmlTag(@cConteudoXML, [?xml version="1.0" encoding="UTF-8"?])

   UniXmlTag(@cConteudoXML, [PedidoEnvioRPS xmlns="http://www.prefeitura.sp.gov.br/nfe"])
   
     UniXmlTag(@cConteudoXML, [Cabecalho Versao="1" xmlns=""])
       UniXmlTag(@cConteudoXML, "CPFCNPJRemetente")
	     UniXmlTag(@cConteudoXML, "CNPJ", "99999997000100")
       UniXmlTag(@cConteudoXML, "/CPFCNPJRemetente")
     UniXmlTag(@cConteudoXML, "/Cabecalho")
	 
	 UniXmlTag(@cConteudoXML, [RPS xmlns=""])	 
	   UniXmlTag(@cConteudoXML, "Assinatura", "d8Pg/jdA7t5tSaB8Il1d/CMiLGgfFAXzTL9o5stv6TNbhm9I94DIo0/ocqJpGx0KzoEeIQz4RSn99pWX4fiW/aETlNT3u5woqCAyL6U2hSyl/eQfWRYrqFu2zcdc4rsAG/wJbDjNO8y0Pz9b6rlTwkIJ+kMdLo+EWXMnB744olYE721g2O9CmUTvjtBgCfVUgvuN1MGjgzpgyussCOSkLpGbrqtM5+pYMXZsTaEVIIck1baDkoRpLmZ5Y/mcn1/Om1fMyhJVUAkgI5xBrORuotIP7e3+HLJnKgzQQPWCtLyEEyAqUk9Gq64wMayITua5FodaJsX+Eic/ie3kS5m50Q==")
	   
	   UniXmlTag(@cConteudoXML, "ChaveRPS")
	     UniXmlTag(@cConteudoXML, "InscricaoPrestador", "39616924")
		 UniXmlTag(@cConteudoXML, "SerieRPS", "BB")
		 UniXmlTag(@cConteudoXML, "NumeroRPS", "4105")
	   UniXmlTag(@cConteudoXML, "/ChaveRPS")
	   
	   UniXmlTag(@cConteudoXML, "TipoRPS", "RPS-M")
	   UniXmlTag(@cConteudoXML, "DataEmissao", "2015-01-20")
	   UniXmlTag(@cConteudoXML, "StatusRPS", "N")
	   UniXmlTag(@cConteudoXML, "TributacaoRPS", "T")
	   UniXmlTag(@cConteudoXML, "ValorServicos", "20500")
	   UniXmlTag(@cConteudoXML, "ValorDeducoes", "5000")
	   UniXmlTag(@cConteudoXML, "ValorPIS", "10")
	   UniXmlTag(@cConteudoXML, "ValorCOFINS", "10")
	   UniXmlTag(@cConteudoXML, "ValorINSS", "10")
	   UniXmlTag(@cConteudoXML, "ValorIR", "10")
	   UniXmlTag(@cConteudoXML, "ValorCSLL", "10")
	   UniXmlTag(@cConteudoXML, "CodigoServico", "7617")
	   UniXmlTag(@cConteudoXML, "AliquotaServicos", "0.05")
	   UniXmlTag(@cConteudoXML, "ISSRetido", "false")
	   
	   UniXmlTag(@cConteudoXML, "CPFCNPJTomador")
	     UniXmlTag(@cConteudoXML, "CPF", "12345678909")
	   UniXmlTag(@cConteudoXML, "/CPFCNPJTomador")
	   
	   UniXmlTag(@cConteudoXML, "RazaoSocialTomador", "TOMADOR PF")
	   
	   UniXmlTag(@cConteudoXML, "EnderecoTomador")
         UniXmlTag(@cConteudoXML, "TipoLogradouro", "Av")
         UniXmlTag(@cConteudoXML, "Logradouro", "Paulista")
         UniXmlTag(@cConteudoXML, "NumeroEndereco", "100")
         UniXmlTag(@cConteudoXML, "ComplementoEndereco", "Cj 35")
         UniXmlTag(@cConteudoXML, "Bairro", "Bela Vista")
         UniXmlTag(@cConteudoXML, "Cidade", "3550308")
         UniXmlTag(@cConteudoXML, "UF", "SP")		 
         UniXmlTag(@cConteudoXML, "CEP", "1310100")		 
	   UniXmlTag(@cConteudoXML, "/EnderecoTomador")
	   
	   UniXmlTag(@cConteudoXML, "EmailTomador", "tomador@teste.com.br")
	   UniXmlTag(@cConteudoXML, "Discriminacao", "Desenvolvimento de Web Site Pessoal.")
	 UniXmlTag(@cConteudoXML, "/RPS")  
	UniXmlTag(@cConteudoXML, "/PedidoEnvioRPS")
	
  * Gravar o arquivo no HD
	FWriteLine(nHandle, cConteudoXML)
	FClose(nHandle)
	
	?
	?
	? "XML gerado com sucesso!!!"
	? mArquivoTmp
	?
	?
	?
	Wait
	
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 5 //5=NFSe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 45 //Servico.NFSeEnvioRps
   oConfiguracao:CodigoMunicipio = 3550308 //São Paulo  
   oConfiguracao:SchemaVersao = "2.00"
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao
	
 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
	  ? "String do XML:"
	  ?
	  ?
	  ? cConteudoXML
	  ?
	  ?
	  wait
	  cls 
    
	  oEnvioRPS := CreateObject("Unimake.Business.DFe.Servicos.NFSe.EnvioRps")
      oEnvioRPS:Executar(cConteudoXML, oConfiguracao)
	  
	  ? oEnvioRPS:RetornoWSString
	  ?
	  ?
	  Wait
	  
	  lSucesso = UniXmlBuscaTag(oEnvioRPS:RetornoWSString, "Sucesso", 1) == "true"
	  
	  ?
      ?
	  If ! lSucesso
	     cErroCodigo := UniXmlBuscaTag(oEnvioRPS:RetornoWSString, "Codigo", 1)
	     cErroDescricao := UniXmlBuscaTag(oEnvioRPS:RetornoWSString, "Descricao", 1)
		 
		 ? cErroCodigo
		 ? cErroDescricao
		 ?
		 ?		 
	  Else
         ? "NFSe gerada com sucesso!!!"
		 ?
		 ?
	  Endif	 
	  
	  Wait  
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar a NFSe."
      ? oErro:Description
      ? oErro:Operation
	  
      //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
	  
	  Wait
	  cls   
   End	
Return

* ------------------------------------------------------------------------------
* Função para gerar TAGS no XML
* ------------------------------------------------------------------------------
Function UniXmlTag(pConteudoXML, pTag, pConteudo, nColuna, pTagEncerramento)
   Local mConteudo

   If(nColuna   == NIL, nColuna   := 0 , .T.)

   if pTagEncerramento == NIL
      pTagEncerramento := pTag
   endif

   Do Case
      Case pTag <> NIL .And. pConteudo <> NIL
         * Cria a tag e grava o conteudo e fecha a tag ex: <QQ>teste</QQ>
           mConteudo := Space(nColuna)+"<"+Alltrim(pTag)+">"+pConteudo+"</"+Alltrim(pTagEncerramento)+">"

      Case pTag <> NIL .And. pConteudo == NIL
         * Cria ou fecha a Tag, ex: <QQ> ou </QQ>
           mConteudo := Space(nColuna)+"<"+Alltrim(pTag)+">"

      Case pTag == NIL .And. pConteudo <> NIL
         * Grava somente o conteudo. ex:
         * <VALOR>
         *  20.00     -> grava somente o conteudo.
         * <VALOR>

           mConteudo := Space(nColuna)+pConteudo

       OtherWise
           mConteudo := ""
   EndCase

   If ! Empty(mConteudo)
      pConteudoXML += mConteudo + Chr(13) + Chr(10)
   EndIf
Return

Function FWriteLine(pHandle, cConteudo)
   fwrite(pHandle, cConteudo + Chr(13) + Chr(10))
Return .T.

* Busca a tag diretamente no arquivo TXT/XML
Function UniXmlBuscaTag(cTexto, cTag, nOcorr)
  Local nPosI, nPosF, cRetorno, i

  If nOcorr == NIl
     nOcorr := 1
  Endif

  For i=1 To nOcorr
     nPosI    := At("<" + Upper(cTag) + ">",Upper(cTexto)) + Len(cTag) + 2
     nPosF    := At("</"+ Upper(cTag) + ">",Upper(cTexto)) - 1
     cRetorno := SubStr(cTexto,nPosI,nPosf-nPosI+1)
     cTexto   := Substr(cTexto,nPosF+Len(cTag))
  Next i
Return cRetorno
