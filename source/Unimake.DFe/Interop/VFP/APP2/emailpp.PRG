* codigo

*m.email=ALLTRIM(thisform.text1.Value)
*m.arquivo=ALLTRIM(thisform.text2.Value)
*m.assunto=ALLTRIM(thisform.text3.Value)
*m.corpo=ALLTRIM(thisform.edit1.Value)
cArch1=m.arquivo
cArch2=m.arquivo
tCMAIL=m.email
=enviomail(cArch1,cArch2, tcMail)
PROCEDURE ENVIOMAIL
PARAMETER cArch1, cArc2, cMail
oOutLook = Createobject( "Outlook.Application" )
oEmail = oOutLook.CreateItem (0)
oEmail.Display
With oEmail
.Recipients. Add(ALLTRIM(cMail))
.Subject = m.assunto
.Importance = 1
.Body = m.corpo+chr(13)+chr(10)+CHR(13)
.Attachments. Add(cArch1)
*.Attachments. Add(cArch2)
.Send
EndWith
Release oEmail
Release oOutLook
MESSAGEBOX("Email enviado com sucesso")
Return