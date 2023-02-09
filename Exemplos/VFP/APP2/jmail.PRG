LOCAL oEmail as object
      oEmail = CREATEOBJECT("JMail.Message")
      oEmail.Logging = .T.
      oEmail.Silent = .T.
      oEmail.ReturnReceipt = .T.
      oEmail.From = "meuemail@estadao.com.br"
      oEmail.FromName = "satori"
      oEmail.AddRecipient(var_email,"Depto.Fiscal")
      oEmail.Subject = "Envio de arquivo XML ref.NF-e "+ALLTRIM(lcnf)
      oEmail.Body = "Segue o arquivo XML referente nossa NF-e de número XXX"+ ;
              CHR(13)+"Atenciosamente, "+;
              CHR(13)+" "+;
              CHR(13)+"Nome da Empresa Ltda. "+;
              CHR(13)+"E-mail: [Tens de ter uma conta e sessão iniciada para poderes visualizar este link]"
      oEmail.AddAttachment("c:\nfexml"+var_nomearq)


      oEmail.MailServerUserName = 'meuemail@estadao.com.br'
      oEmail.MailServerPassword = 'minhasenha'
     
      IF oEmail.SEND("smtp.gmail.com")

          MESSAGEBOX("Mensagem enviada!")
      ELSE
          MESSAGEBOX("Ocorreu um erro: "+;
                    CHR(13)+STR(oEmail.errorcode)+;
                    CHR(13)+oEmail.errormessage,0)                   
      ENDIF
      oEmail = null     