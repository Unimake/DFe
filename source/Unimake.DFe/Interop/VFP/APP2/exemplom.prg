PARAMETERS  otcEmail, oTcCliente, oTcFactura, oTcMensaje , otcPedido

 

* This sample shows how you can configure lots of email options, in order to determine how

* your reports will be sent by email

* Allows adding other attachments, see the cAttachments property

* Make sure to provide all the correct SMTP settings, the provided here are just samples

* Several useful email settings

* http://www.emailaddressmanager.com/tips/mail-settings.html

IF VARTYPE(_Screen.oFoxyPreviewer) <> "O"

   DO LOCFILE("FoxyPreviewer.App")

ELSE

   *WAIT WINDOW "Problemas " NOWAIT 

   *RETURN  

ENDIF

WITH _Screen.oFoxyPreviewer

.cLanguage = "SPANISH"

* REPORT FORM (_Samples + "\Solution\Reports\percent.frx") OBJECT TYPE 10 TO FILE "c:\correos\Email1.pdf"

.cEmailTo = otcEmail  && cuenta del cliente

.lEmailAuto = .T. && Automatically generates the report output file

.cEmailType = "PDF" && The file type to be used in Emails (PDF, RTF, HTML or XLS)

.nEmailMode = 4 && 1 = MAPI, 2 = CDOSYS HTML, 3 = CDOSYS TEXT, 4 = Custom procedure

* GMAIL

.cSMTPServer = "smtp.gmail.com"

.cEmailFrom =  "awbinfo23@gmail.com"

.cEmailSubject = oTcCliente + " " +oTcMensaje

.nSMTPPort = 465

*.nSMTPPort = 25

.lSMTPUseSSL = .T.

.cSMTPUserName = "sandrae...@gmail.com"

.cSMTPPassword = tclave

.lReadReceipt = .T.

.lPriority = .T.

.cAttachments = ALLTRIM(oTcFactura) && GETFILE() && Comma delimited

* Other possible properties

*!* .cEmailCC

*!* .cEmailBCC

*!* .cEmailReplyTo

.cEmailBody = "<HTML><BR>Estamos enviando a su cuenta de correo su estado de cuenta actualizdo a la fecha..<b>FoxyPreviewer</b></HTML>"

* Now we can send the file we created by email !

.SendEmailUsingCDO(ALLTRIM(oTcFactura))

ENDWITH

 

Funciona bien en modo diseño , pero al compilar y usar desde el .exe  me sale el error de :

 

.SendEmailUsingCDO()  solo en modo de red y no local

 