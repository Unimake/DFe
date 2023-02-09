Try
	Local LcSchema,LoConfig,LoMsg,LoError,LcErr
	m.LcErr    = ""
	m.LcSchema = "http://schemas.microsoft.com/cdo/configuration/"
	m.LoConfig = CreateObject("CDO.Configuration")
	With LoConfig.Fields
		.Item(m.LcSchema+"smtpserver")            = "smtp.gmail.com"
		.Item(m.LcSchema+"smtpserverport")        = 587
		.Item(m.LcSchema+"sendusing")             = 2
		.Item(m.LcSchema+"smtpauthenticate")      = .T.
		.Item(m.LcSchema+"smtpusessl")            = .F.
		.Item(m.LcSchema+"sendusername")          = "awbinfo23@gmail.com"
		.Item(m.LcSchema+"sendpassword")          = "@Awbinfo12"
		.Item(m.LcSchema+"smtpconnectiontimeout") = 60
	    .Item(m.LcSchema+"urn:schemas:mailheader:X-Priority") = 1
		.Update
	EndWith
	LoMsg = CreateObject ("CDO.Message")
	With LoMsg
		.Configuration = m.LoConfig
		.From          = "Teste <teste@teste.com.br>"
		.To            = "awbinfo23@gmail.com"
		.Subject       = "Teste"
   	    .TextBody      = "Isto é um teste."
        .HtmlBody      = "Isto é um teste"
   	    .AddAttachment("C:\Teste\arquivo.txt") && envia o anexo
		.Send()
	EndWith
Catch To m.LoError
    m.LcErr = [Não foi possível enviar o e-mail, possíveis razões:]+Chr(13)+;
              [O E-Mail do remetente pode não existir;]            +Chr(13)+;
              [A Senha pode está errada;]                          +Chr(13)+;
              [O Nome do Usuário pode estar errado;]               +Chr(13)+;
              [O Servidor de SMTP do E-Mail pode estar errado;]    +Chr(13)+;
              [A Porta do Servidor de SMTP pode estar errada;]     +Chr(13)+;
              [O requerimento da Autenticação pode está errado;]   +Chr(13)+;
              [O requerimento do protocolo SSL pode estar errado;] +Chr(13)+;
              [O E-Mail "Cc" pode não existir;]                    +Chr(13)+;
              [O E-Mail "Cco" pode não existir;]                   +Chr(13)+;
              [Pode ter passado o número de e-mails que o seu provedor aceita.]
Finally
	Release LoConfig,LoMsg
	If !Empty(m.LcErr)
		MessageBox(m.LcErr)
	EndIf
EndTry