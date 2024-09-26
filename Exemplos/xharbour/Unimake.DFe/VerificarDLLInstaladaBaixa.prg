* ---------------------------------------------------------------------------------
* Verificar se a DLL está instalada no PC e Baixa em caso negativo
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function VerificarDLLInstaladaBaixa()
   Local oCertificado, lDLLInstalada:= .F., oInfoInterop, cArquivo, cXml, cVersao, cVersaoA
   
   Try
    * Testar se a DLL está instalada
      oCertificado := CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")
      lDLLInstalada:= .T.
   Catch oErro
      lDLLInstalada:= .F.
   End
   
   Cls
   ?
   ?
   ?
   If lDLLInstalada
      ? "DLL Unimake.DFe esta instalada no PC!!!"
      ? "Iremos Verificar se necessita de atualizacao"

      oInfoInterop:= CreateObject("Unimake.Business.DFe.Utility.InfoInterop")
      ?
      ?		  
      ? cVersao:= oInfoInterop:VersaoDLL    // 20240920.1002
      Wait

      cXml:= DownloadTexto([https://www.unimake.com.br/downloads/versaouninfe.xml])
      ?
      ? cXml
      ?
      wait
      
      cVersaoA:= UniXmlBuscaTag(cXml, "versao", 2) // 20240924.1131.28
      ?
      ? cVersaoA
      ?
      wait

      If Substr(cVersaoA, 1, 13) > Substr(cVersao, 1, 13)  
         ? "Iremos Baixar e Instalar"

         fAtualiza()
      
         /*
  
tem como não aparecer isto, pelo que entendi estou usando a dll no comando    oInfoInterop:= CreateObject("Unimake.Business.DFe.Utility.InfoInterop")

teria que retirar ele da memoria correto? vou enviar o prg e este comentário para vc analisar 
         */
      Else
         ? "DLL Unimake.DFe esta atualizada no PC!!!"
      Endif

      Wait
      cls
   Else
      ? "DLL Unimake.DFe nao esta instalada no PC!!!"
      ? "Iremos Baixar e Instalar"

      fAtualiza()
      ?
      ?
      ?
      Wait   
   Endif
 Return

****************** Rotina Genérica para Download *******************************
Function DownloadTexto(cUrl)
   Local oSoap, cRetorno:= []

   BEGIN SEQUENCE WITH __BreakBlock()
      oSoap:= Win_OleCreateObject([MSXML2.ServerXMLHTTP])
      oSoap:Open([GET], cUrl, .F.)
      oSoap:Send()
      cRetorno:= oSoap:ResponseBody()
   END SEQUENCE
   Release oSoap
Return(cRetorno)
****************** Fim Rotina Genérica para Download ***************************

* Busca a tag diretamente no arquivo TXT/XML - Retirada de GerarXmlNFSe.prg ****
Function UniXmlBuscaTag(cTexto, cTag, nOcorr)
  Local nPosI, nPosF, cRetorno, i

  hb_Default(@nOcorr, 1) // se não for informada, assume com 1

  For i:= 1 To nOcorr
     nPosI   := At("<" + Upper(cTag) + ">", Upper(cTexto)) + Len(cTag) + 2
     nPosF   := At("</"+ Upper(cTag) + ">", Upper(cTexto)) - 1
     cRetorno:= SubStr(cTexto, nPosI , nPosf - nPosI + 1)
     cTexto  := Substr(cTexto, nPosF + Len(cTag))
  Next i
  Release nPosI, nPosF, i
Return (cRetorno)

Function MyRun(cComando, lTip)
   Local oShell, nRet:= 0

   oShell:= Win_OleCreateObject("WScript.Shell")
   If lTip
      nRet:= oShell:Run("%comspec% /c " + cComando, 0, .T.)
   Else
      oShell:Exec(cComando)
   Endif
   Release oShell
Return (Iif(nRet == 0, .T., .F.))

Procedure fAtualiza()
   Local cArquivo

   If Hb_FileExists([Install_Unimake.DFe.exe])
      Ferase([Install_Unimake.DFe.exe])
   Endif

   cArquivo:= DownloadTexto([https://www.unimake.com.br/downloads/Install_Unimake.DFe.exe])

   hb_MemoWrit([Install_Unimake.DFe.exe], cArquivo)

   MyRun([Install_Unimake.DFe.exe], .T.)

   If Hb_FileExists([Install_Unimake.DFe.exe])
      Ferase([Install_Unimake.DFe.exe])
   Endif
   Release cArquivo
Return (Nil)