* ---------------------------------------------------------------------------------
* Imprimir DANFe NFe e NFCe, DACTe e DAMDFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ExecutarTelaUniDANFe()
   Local oDANFe, nOpcao, aOpcoes
   
   Cls
   
   aOpcoes := {}
   AAdd(aOpcoes, "Executar tela de configuracoes do UniDANFe")
   AAdd(aOpcoes, "Executar tela de gerenciamento de emails do UniDANFe")
   AAdd(aOpcoes, "Executar tela de gerenciamento de licencas do UniDANFe")

   Do While .T.   
      nOpcao := Achoice( 3, 2, 30, 80, aOpcoes)
   
      oDANFe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
	  
      Do Case
         case LastKey() = 27
              Exit
			  
	     Case nOpcao == 1
              oDANFe:ShowConfigurationScreen()
		 
	     Case nOpcao == 2
		      oDANFe:ShowEmailScreen()
		 
	     Case nOpcao == 3
		      oDANFe:ShowLicencaScreen()
      EndCase		 
   Enddo
Return