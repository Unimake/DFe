* ---------------------------------------------------------------------------------
* Verificar se a DLL está instalada no PC
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function VerificarDLLInstalada()
   Local  oConfig, lDLLInstalada

   lDLLInstalada = .F.
   
   Try
    * Testar se a DLL está instalada
	  oCertificado := CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")
      lDLLInstalada := .T.
   Catch oErro
      lDLLInstalada := .F.
   End
   
   Cls
   ?
   ?
   ?
   If lDLLInstalada
   	  ? "DLL Unimake.DFe esta instalada no PC!!!"
   Else
      ? "DLL Unimake.DFe nao esta instalada no PC!!!"
   Endif
   ?
   ?
   ?
   Wait   
Return