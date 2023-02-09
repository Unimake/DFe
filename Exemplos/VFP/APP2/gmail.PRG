SET PROCEDURE TO C:\cdo2000.prg
loMail = NEWOBJECT("Cdo2000")

TRY
  LOCAL lcSchema, loConfig, loMsg, loError, lcErr
  lcErr = ""
  lcSchema = "http://schemas.microsoft.com/cdo/configuration/"
  loConfig = CREATEOBJECT("CDO.Configuration")
  WITH loConfig.FIELDS
    .ITEM(lcSchema + "smtpserver") = "smtp.gmail.com"
    .ITEM(lcSchema + "smtpserverport") = 465 && ó 587
    .ITEM(lcSchema + "sendusing") = 2
    .ITEM(lcSchema + "smtpauthenticate") = .T.
    .ITEM(lcSchema + "smtpusessl") = .T.
    .ITEM(lcSchema + "sendusername") = "meuemail@gmail.com"
    .ITEM(lcSchema + "sendpassword") = "minhasenha"
    .UPDATE
  ENDWITH
  loMsg = CREATEOBJECT ("CDO.Message")
  WITH loMsg
    .Configuration = loConfig
    .FROM = "meuemail@gmail.com"
    .TO = "paraquemvoumandar@yahoo.com.br"
    .Subject = "Prueba desde Gmail"
    .TextBody = "Este es un mensaje de prueba con CDO con " + ;
      "autenticación y cifrado SSL desde Gmail"
    .Send()
  ENDWITH
CATCH TO loError
  lcErr = [Error: ] + STR(loError.ERRORNO) + CHR(13) + ;
    [Linea: ] + STR(loError.LINENO) + CHR(13) + ;
    [Mensaje: ] + loError.MESSAGE
FINALLY
  RELEASE loConfig, loMsg
  STORE .NULL. TO loConfig, loMsg
  IF EMPTY(lcErr)
    MESSAGEBOX("El mensaje se envió con éxito", 64, "Aviso")
  ELSE
    MESSAGEBOX(lcErr, 16 , "Error")
  ENDIF
ENDTRY