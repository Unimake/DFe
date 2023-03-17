* ---------------------------------------------------------------------------------
* Enviar CTe de forma assincrona dessarializando o XML de uma string
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCTeAssincronoDesserializando2()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oEnviCTe, oCTe, cXml
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 2 //2=CTe
   oConfiguracao:CertificadoArquivo = "C:\Users\Wandrey\Downloads\Telegram Desktop\certificado enzza comercio senha 020207ab.pfx"
   oConfiguracao:CertificadoSenha = "020207ab"   
   
 * Criar a string do XML
   cXml := ""
   cXml += [<CTe xmlns="http://www.portalfiscal.inf.br/cte" >]
       cXml += '<infCte versao="3.00" Id="CTe' + '">'
           cXml += [<ide>]                                                                          // Início da TAG (ide)
               cXml += XmlTag([cUF]      , Alltrim("35"))                                        // UF do Emitente no caso SP = 35
               cXml += XmlTag([cCT]      , Strzero(1, 8))
               cXml += XmlTag([CFOP]     , [5932]) //[6352]) //
               cXml += XmlTag([natOp]    , [PREST. DE SERV. TRANSPORTE A ESTAB. COMERCIAL])
               cXml += XmlTag([mod]      , [57])
               cXml += XmlTag([serie]    , [1])                                               // Série 
               cXml += XmlTag([nCT]      , Alltrim(Str(1, 8)))  
               cXml += XmlTag([dhEmi]    , "2023-03-02T10:25:47-03:00")                             // Data Emissão Formato yyyy-mm-dd
               cXml += XmlTag([tpImp]    , [1])                                    // Tipo de Emissão      1-Normal  / 2-Contingência(FIXO)
               cXml += XmlTag([tpEmis]   , [1])
*              cXml += XmlTag([cDV]      , Right(cId, 1))                          // Dígito da Chave de Acesso
               cXml += XmlTag([tpAmb]    , Str(2, 1))                              // Ambiente de Emissão  1-Produção/ 2-Homologação
               cXml += XmlTag([tpCTe]    , [0])                                                  // Número da Nota Fiscal
               cXml += XmlTag([procEmi]  , [0])   // 0 - emissão de MDF-e com aplicativo do contribuinte;
               cXml += XmlTag([verProc]  , [Versao])
               cXml += XmlTag([cMunEnv]  , [3550308])
               cXml += XmlTag([xMunEnv]  , [SAO PAULO])
               cXml += XmlTag([UFEnv]    , [SP])
               cXml += XmlTag([modal]    , [01])
               cXml += XmlTag([tpServ]   , [0])
               cXml += XmlTag([cMunIni]  , [3550308])
               cXml += XmlTag([xMunIni]  , [SAO PAULO])
               cXml += XmlTag([UFIni]    , [SP])
               cXml += XmlTag([cMunFim]  , [3529005])
               cXml += XmlTag([xMunFim]  , [MARILIA])
               cXml += XmlTag([UFFim]    , [SP])
               cXml += XmlTag([retira]   , [0]) 
               cXml += XmlTag([indIEToma], [1]) //[9])
               cXml += [<toma3>]
                   cXml += XmlTag([toma], [0]) // Tomador do Serviço: 0-Remetente; 1-Expedidor; 2-Recebedor; 3-Destinatário 
               cXml += [</toma3>]
           cXml += [</ide>]

*           cXml += [<compl>]
*               cXml += XmlTag([xEmi], [MASTER])
*               cXml += [<fluxo />]
*               cXml += XmlTag([xObs], [NOTA FISCAL DE PRODUTOR RURAL N. 253-254-255])
*           cXml += [</compl>]

           cXml += [<emit>]
               cXml += XmlTag([CNPJ] , "06117473000150")                       // CNPJ do Emitente
               cXml += XmlTag([IE]   , "7300083417")                  // IE do Emitente
*               If parametrosiniciais->TPANFE == 2
 *                 cXml += XmlTag([xNome], [CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL])
 *              Else
                  cXml += XmlTag([xNome], Alltrim("TESTE NOME"))                // Razão Social emitente
  *             Endif
                cXml += XmlTag([xFant], Alltrim("TESTE FANTASIA"))                // Nome Fantasia Emitente

               cXml += [<enderEmit>]
                   cXml += XmlTag([xLgr]   , Alltrim("TESTE ENDERECO"))           // Endereço Emitente
                   cXml += XmlTag([nro]    , Alltrim("S/N"))            // Número do Endereco do Emitente
*                  cXml += XmlTag([xCpl]   , [CAIXA POSTAL 268])
                   cXml += XmlTag([xBairro], Alltrim("BAIRRO TESTE"))        // Bairro do Emitente
                   cXml += XmlTag([cMun]   , Alltrim("3550308"))           // Código IBGE do emitente
                   cXml += XmlTag([xMun]   , Alltrim("SAO PAULO"))           // Cidade do Emitente
	           cXml += XmlTag([CEP]    , "87707210")            // CEP do Emitente
	           cXml += XmlTag([UF]     , Alltrim("SP"))             // UF do Emitente
	           cXml += XmlTag([fone]   , Alltrim("044999999999"))           // Telefone do Emitente
               cXml += [</enderEmit>]
           cXml += [</emit>]

           cXml += [<rem>]
*	       cXml += XmlTag([CPF]  , [72304553915])
*	       cXml += XmlTag([IE]   , [ISENTO])
	       cXml += XmlTag([CNPJ] , [78408960000182])
	       cXml += XmlTag([IE]   , [251079554])

                  cXml += XmlTag([xNome], [CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL])
                  cXml += XmlTag([xFant], [CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL])

	       cXml += XmlTag([fone] , [6635651335])
                   cXml += [<enderReme>]
  	               cXml += XmlTag([xLgr]   , [GLEBA GUARIBA VI LOTE RURAL 38])
  	               cXml += XmlTag([nro]    , [SN])
*                      cXml += XmlTag([xCpl]   , [CAIXA POSTAL 1234])
  	               cXml += XmlTag([xBairro], [ZONA RURAL])
  	               cXml += XmlTag([cMun]   , [5101407])
  	               cXml += XmlTag([xMun]   , [ARIPUANA])
  	               cXml += XmlTag([CEP]    , [78325000])
  	               cXml += XmlTag([UF]     , [MT])
  	               cXml += XmlTag([cPais]  , [1058])
  	               cXml += XmlTag([xPais]  , [BRASIL])
                   cXml += [</enderReme>]
*	       cXml += XmlTag([email], [email@teste.com.br])
           cXml += [</rem>]
/*
           cXml += [<receb>]
	       cXml += XmlTag([CNPJ] , [03851469000122])
	       cXml += XmlTag([IE]   , [131952927])
                  cXml += XmlTag([xNome], [CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL])
               cXml += XmlTag([fone] , [6536342236])
                   cXml += [<enderReceb>]
    	               cXml += XmlTag([xLgr]   , [ESTRADA SOUZA LIMA])
    	               cXml += XmlTag([nro]    , [SN])
    	               cXml += XmlTag([xBairro], [SOUZA LIMA])
    	               cXml += XmlTag([cMun]   , [3529005])
    	               cXml += XmlTag([xMun]   , [MARILIA])
    	               cXml += XmlTag([CEP]    , [17533280])
    	               cXml += XmlTag([UF]     , [SP])
    	               cXml += XmlTag([cPais]  , [1058])
    	               cXml += XmlTag([xPais]  , [BRASIL])
                   cXml += [</enderReceb>]
           cXml += [</receb>]
*/
           cXml += [<dest>]
	       cXml += XmlTag([CNPJ] , [03851469000122])
	       cXml += XmlTag([IE]   , [131952927])
                  cXml += XmlTag([xNome], [CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL])
               cXml += XmlTag([fone] , [6536342236])
                   cXml += [<enderDest>]
    	               cXml += XmlTag([xLgr]   , [ESTRADA SOUZA LIMA])
    	               cXml += XmlTag([nro]    , [SN])
    	               cXml += XmlTag([xBairro], [SOUZA LIMA])
    	               cXml += XmlTag([cMun]   , [3529005])
    	               cXml += XmlTag([xMun]   , [MARILIA])
    	               cXml += XmlTag([CEP]    , [17533280])
    	               cXml += XmlTag([UF]     , [SP])
    	               cXml += XmlTag([cPais]  , [1058])
    	               cXml += XmlTag([xPais]  , [BRASIL])
                   cXml += [</enderDest>]
           cXml += [</dest>]

           cXml += [<vPrest>]
	       cXml += XmlTag([vTPrest], [50.00])
	       cXml += XmlTag([vRec]   , [50.00])
           cXml += [</vPrest>]

           cXml += [<imp>]  // tem várias situações
               cXml += [<ICMS>]
                   cXml += [<ICMSSN>]
	               cXml += XmlTag([CST]  , [90])
	               cXml += XmlTag([indSN], [1])
                   cXml += [</ICMSSN>]
               cXml += [</ICMS>]
           cXml += [</imp>]

           cXml += [<infCTeNorm>]
               cXml += [<infCarga>]
	           cXml += XmlTag([vCarga] , [79400.00])
	           cXml += XmlTag([proPred], [GADO])
*	           cXml += XmlTag([xOutCat], [GADO])
                   cXml += [<infQ>]
	               cXml += XmlTag([cUnid] , [01])
	               cXml += XmlTag([tpMed] , [KILO])
	               cXml += XmlTag([qCarga], [50.0000])
                   cXml += [</infQ>]
               cXml += [</infCarga>]

               cXml += [<infDoc>]
                   cXml += [<infOutros>]
	               cXml += XmlTag([tpDoc]     , [99])
	               cXml += XmlTag([descOutros], [NOTA FISCAL MANUAL])
	               cXml += XmlTag([nDoc]      , [123456])
	               cXml += XmlTag([dEmi]      , Transf(Dtos(Date()), [@R 9999-99-99]))
	               cXml += XmlTag([vDocFisc]  , [79400.00])
                   cXml += [</infOutros>]
               cXml += [</infDoc>]
               cXml += [<infModal versaoModal="3.00">]
                   cXml += [<rodo>]
	               cXml += XmlTag([RNTRC], [05277204])
                   cXml += [</rodo>]
               cXml += [</infModal>]
           cXml += [</infCTeNorm>]
       cXml += [</infCte>]
   cXml += [</CTe>]
   
   
   mArquivoTmp  := "d:\testenfe\CTeSemAssinatura.xml"
   nHandle      := fCreate(mArquivoTmp)
   
   If nHandle < 0
      ? "Nao foi possivel criar o arquivo " + Trim(mArquivoTmp) + "!"
      ?
      Return 
   EndIf
   
   fwrite(nHandle, cXML + Chr(13) + Chr(10))
   FClose(nHandle)
   
   ? cXML
   
   Wait

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
 * Criar grupo de tag <enviCTe> 
   oEnviCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.EnviCTe")
   oEnviCTe:Versao = "3.00"
   oEnviCTe:IdLote = "000000000000001"
   
   Try 
    * Criar grupo de tag <CTe>
      oCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.CTe")   

    * Como desserializar partindo de um arquivo
      oCTe = oCTe:LoadFromFile("C:\Users\Wandrey\Downloads\Telegram Desktop\35230310229311000180570010000000011000000018_01_SemAssinatura.xml")
      oEnviCTe:AddCTe(oCTe)
   
    * Como deserializar partindo da string do XML
*     oEnviCTe:AddCTe(oCTe:LoadFromXML(cXML))

   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
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
   
 * Resgatar alguns dados do Objeto do XML para demostrar como funciona
   oTagCTe = oEnviCTe:GetCTe(0)
 
   ? "CNPJ Emitente:", oTagCTe:InfCTe:Emit:CNPJ
   ? "Razao Emitente:", oTagCTe:InfCTe:Emit:XNome
   ? "Data Emissao:", oTagCTe:InfCTe:Ide:DhEmi
   ? "Chave do CTe:", oTagCTe:InfCTe:Chave
   ?
   ? 
   Wait
   ?
   ?
   ?
      
   Try 
    * Criar o objeto para consumir o serviço de autorização do CTe
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.Autorizacao")
      oAutorizacao:SetXMLConfiguracao(oEnviCTe, oConfiguracao)
	  
	  //O conteúdo do XML assinado deve ser gravado na base de dados para ser recuperado 
	  //caso seja necessário. Imagine que de um problema no envio do CTe e vc precise resgatar para enviar novamente.
	  ? "Demonstrar o XML do CTe assinado: "
	  ?
	  ? oAutorizacao:GetConteudoCTeAssinado(0)
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
	  
	  mArquivoTmp  := "d:\testenfe\CTeAssinado.xml"
      nHandle2     := fCreate(mArquivoTmp)
   
      If nHandle2 < 0
         ? "Nao foi possivel criar o arquivo " + Trim(mArquivoTmp) + "!"
         ?
      EndIf
   
      fwrite(nHandle2, oAutorizacao:GetConteudoCTeAssinado(0) + Chr(13) + Chr(10))
      FClose(nHandle2)   
	  
	  ? "Arquivo assinado salvo no hd"
	  ?
	  ?
	  ?
	  Wait
	  
	  oAutorizacao:Executar(oEnviCTe, oConfiguracao)   
	  
      ? "XML retornado pela SEFAZ no envio do XML de CTe:"
	  ?
	  ? oAutorizacao:RetornoWSString
	  ?
	  ?
	  Wait
	  ?
	  ?
	  ?
   
	  If oAutorizacao:Result <> NIL
	     ? "Status envio:", oAutorizacao:Result:CStat, oAutorizacao:Result:XMotivo
		 ?
		 ?
		 Wait
		 ?
		 ?
		 ?		 
		 
		 //Tratar o retorno do envio do XML do CTe
		 if oAutorizacao:Result:CStat = 103 //Lote Recebido com Sucesso
            //Fazer a consulta do Recibo para ver se a(s) nota(s) foram autorizadas
            ? "Consultando o recibo...."
            ?
			
			//Criar a configuração minima para realizar a consulta recibo
            oConfigRec = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
            oConfigRec:TipoDFe = 2 // TipoDFe.CTe
            oConfigRec:CertificadoArquivo = "C:\Users\Wandrey\Downloads\Telegram Desktop\certificado enzza comercio senha 020207ab.pfx"
            oConfigRec:CertificadoSenha = "020207ab"   

            //Criar o XML de consulta recibo
            oConsReciCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.ConsReciCTe")
            oConsReciCTe:Versao = "3.00"
            oConsReciCTe:TpAmb = 2 // TipoAmbiente.Homologacao
            oConsReciCTe:NRec = oAutorizacao:Result:InfRec:NRec
			
			//Enviar a consulta do Recibo
            oRetAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.RetAutorizacao")
            oRetAutorizacao:Executar(oConsReciCTe, oConfigRec)
			
			Wait
			
			//Tratar o retorno da consulta do recibo
            If oRetAutorizacao:Result <> NIL
			   oAutorizacao:RetConsReciCTe = oRetAutorizacao:Result
			   
             * Modelo para buscar o resultade de cada CTe do lote enviado
               ? "Codigo de Status e Motivo da Consulta Recibo"
               ? "============================================"
			   For I = 1 TO oRetAutorizacao:Result:GetProtCteCount()
			       oProtCte = oRetAutorizacao:Result:GetProtCte(I-1)
				   
				   ? AllTrim(Str(oProtCte:InfProt:CStat,5)), oProtCte:InfProt:XMotivo
				   ?
				   ?
				   Wait
				   Cls
				   
				   //Demonstrar o XML retornado
				   ? "XML retornado na consulta recibo:"
				   ?
				   ? oRetAutorizacao:RetornoWSString
				   ?
				   ?				   
				   Wait
				   Cls

				   ? "Proximo passo eh gravar o XML de distribuicao"
				   ?
				   ?				   
				   Wait
				   Cls
				   
				 * Salvar XML de distribuicao das notas enviadas na pasta informada  
				   if oProtCTe:InfProt:CStat == 100 //100=CTe Autorizada 
				      oAutorizacao:GravarXmlDistribuicao("d:\testenfe")					  
				   else
                       //CTe rejeitado, fazer devidos tratamentos
				   endif

                   ? "Fim!!!"
				   ?
				   ?
				   Wait 				 
			   Next I			   
			Endif
		 Endif
      Endif   
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
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
Function XmlTag(pTag, pConteudo)
   Local mConteudo

   mConteudo := "<" + pTag + ">" + pConteudo + "</" + pTag + ">"
Return mConteudo