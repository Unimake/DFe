Dim config
Dim consStatServ
Dim exceptionInterop
Dim statusServico

'----------------------------------------------------
'Criar configuração básica para consumir o serviço
'----------------------------------------------------

Set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
config.CertificadoSenha = "12345678"
config.CertificadoArquivo = "d:\projetos\UnimakePV.pfx"
config.TipoDfe = 0

'----------------------------------------------------
' Criar XML
'----------------------------------------------------
Set consStatServ = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsStatServ")
consStatServ.Versao = "4.00"
consStatServ.TpAmb  = 2 'Homologação
consStatServ.CUF    = 41 ' PR

Set exceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
MsgBox "Exceção do CSHARP: " + exceptionInterop.GetMessage()

'Consumir o serviço
Set statusServico = CreateObject("Unimake.Business.DFe.Servicos.NFe.StatusServico")

On Error Resume Next
statusServico.Executar (consStatServ), (config)

if	Err.Number <> 0 then
	MsgBox "Exceção do VBScript: " + Err.Description
	MsgBox "Exceção do CSHARP: " + exceptionInterop.GetMessage()
end if 

