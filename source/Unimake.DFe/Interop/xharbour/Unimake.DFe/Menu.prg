/*

Unimake Software
Exemplo de uso da DLL Unimake.Dfe
Data: 15/02/2022 
Autores: 
- Edson Mundin Ferreira
- Wandrey Mundin Ferreira

*/
Function Main()
   Local aOpcoes, nOpcao
   
   aOpcoes := {}
   AAdd(aOpcoes, "Consultar Status NFe")
   AAdd(aOpcoes, "Consultar Situacao NFe")
   AAdd(aOpcoes, "Enviar NFe - Modo sincrono")
   AAdd(aOpcoes, "Enviar NFe - Modo assincrono")
   AAdd(aOpcoes, "Enviar NFe - Desserializando o XML")                               
   AAdd(aOpcoes, "Testes diversos com certificado digital")  
   AAdd(aOpcoes, "Enviar Evento de Cancelamento da NFe")  
   AAdd(aOpcoes, "Enviar Evento de Cancelamento da NFe - Desserializando o XML")
   Aadd(aOpcoes, "Gerar XML de distribuicao com um nome diferente do padrao da DLL")
   Aadd(aOpcoes, "Consultar Status MDFe")
   Aadd(aOpcoes, "Enviar MDFe - Modo assincrono")
   Aadd(aOpcoes, "Enviar MDFe - Modo assincrono - Desserializando o XML")
   Aadd(aOpcoes, "Enviar NFCe - Modo sincrono")
   Aadd(aOpcoes, "Enviar NFCe - Modo sincrono - Desserializando o XML")
   Aadd(aOpcoes, "Enviar MDFe - Modo sincrono")
   
   Aadd(aOpcoes, "Validar XML")
   AAdd(aOpcoes, "Enviar Evento de Cancelamento da NFCe")  
       
   Do While .T.
      Cls

      @ 1,2 Say "Unimake.Dfe DLL for " + Version()
	  
      nOpcao := Achoice( 3, 2, 20, 80, aOpcoes)

      Cls

      do case
         case LastKey() = 27
              Exit

         case nOpcao = 1
              ConsultaStatusNfe()

         case nOpcao = 2
              ConsultaSituacaoNfe()
			  
         case nOpcao = 3
              EnviarNfeSincrono()			  
			  
         case nOpcao = 4
              EnviarNfeAssincrono()
			  
         case nOpcao = 5
              EnviarNfeDeserializando()			  			  
			  
         case nOpcao = 6
			  TesteDiversoCertificado()
			  
         case nOpcao = 7
		      CancelarNFe()
			  
         case nOpcao = 8
		      CancelarNFeDesserializando()
		 
		 case nOpcao = 9
		      GerarXmlDistribuicaoNomeDif()
			  
		 case nOpcao = 10
		      ConsultaStatusMDFe()
			  
         case nOpcao = 11
		      EnviarMDFeAssincrono()

         case nOpcao = 12
		      EnviarMDFeAssincronoDesserializando()
			  
         case nOpcao = 13
              EnviarNfceSincrono()			  
			  
         case nOpcao = 14
              EnviarNFCeSincronoDesserializando()
			  
         case nOpcao = 15
              EnviarMDFeSincrono()
			  
         case nOpcao = 16
		      ValidarXML()
			  
         case nOpcao = 17
		      CancelarNFCe()
      endcase
   EndDo
Return       