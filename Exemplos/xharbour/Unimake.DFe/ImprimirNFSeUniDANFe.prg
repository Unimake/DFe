* ---------------------------------------------------------------------------------
* Imprimir NFSe com o UniDANFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ImprimirNFSeUniDANFe()
   Local oConfig, oDANFe   
   
 *
 * Criar as configurações para impressão da NFSe.
 *
 * Lista de parâmetros/propriedades que podem ser utilizadas:
 * https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
 *
   oConfig := CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
   oConfig:Arquivo = "C:\Users\Wandrey\OneDrive\Downloads\Telegram Desktop\Nfse_000000003615-ok.xml"
	oConfig:Configuracao = "C:\Unimake\Unimake.UniDANFe\Unimake_EXE"
   oConfig:Visualizar = .T.
   oConfig:Imprimir = .F.
   oConfig:EnviaEmail = .F.

 * Disparar a impressão do DANFe
   oDANFe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
   oDANFe:Execute(oConfig)   
Return