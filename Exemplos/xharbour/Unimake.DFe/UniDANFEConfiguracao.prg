* ---------------------------------------------------------------------------------
* Executar a tela de configuração do UNIDANFE
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function UniDANFEConfiguracao()
   Local oConfig
   
 * Disparar a impressão do DANFe
   oDANFe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
   oDANFe:ShowConfigurationScreen()
Return