TRY
  LOCAL lcSchema, loConfig, loMsg, loError, lcErr
  lcErr = ""
  lcSchema = "http://schemas.microsoft.com/cdo/configuration/"
  loConfig = CREATEOBJECT("CDO.Configuration")
  WITH loConfig.FIELDS
    .ITEM(lcSchema + "smtpserver") = "smtp.gmail.com"
    .ITEM(lcSchema + "smtpserverport") = 465
    .ITEM(lcSchema + "sendusing") = 2
    .ITEM(lcSchema + "smtpauthenticate") = .T.
    .ITEM(lcSchema + "smtpusessl") = .T.
    .ITEM(lcSchema + "sendusername") = "awbinfo23@gmail.com" && of course your gmail address
    .ITEM(lcSchema + "sendpassword") = "@Awbinfo12"
    .UPDATE
  ENDWITH
  loMsg = CREATEOBJECT ("CDO.Message")
  WITH loMsg
    .Configuration = loConfig
    .FROM = "you@gmail.com"
    .TO = "another.mail@ofyou.com"
    .Subject = "Test of GMail"
    .TextBody = "Testing GMail smtp SSL connection via app password"
    .Send()
  ENDWITH
CATCH TO loError
  lcErr = [Error: ] + STR(loError.ERRORNO) + CHR(13) + ;
    [Line: ] + STR(loError.LINENO) + CHR(13) + ;
    [Message: ] + loError.MESSAGE
FINALLY
  RELEASE loConfig, loMsg
  STORE .NULL. TO loConfig, loMsg
  IF EMPTY(lcErr)
    MESSAGEBOX("The mail was sent", 64, "OK")
  ELSE
    MESSAGEBOX(lcErr, 16 , "Error")
  ENDIF
ENDTRY 