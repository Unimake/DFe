* ---------------------------------------------------------------------------------
* Imprimir DANFe NFe e NFCe, DACTe e DAMDFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ImprimirDANFe()
   Local oConfig, oDANFe   
   
 *
 * Criar as configurações para impressão do DANFe.
 *
 * Lista de parâmetros/propriedades que podem ser utilizadas:
 * https://wiki.unimake.com.br/index.php/UniDANFE/Integrando_o_UniDANFE_ao_ERP/Gerar_documento_auxiliar
 *
   oConfig := CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
   oConfig:Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
   oConfig:Visualizar = .T.
   oConfig:Imprimir = .F.
   oConfig:EnviaEmail = .F.

 * Disparar a impressão do DANFe
   oDANFe = CreateObject("Unimake.Unidanfe.UnidanfeServices")
   oDANFe:Execute(oConfig)   
Return