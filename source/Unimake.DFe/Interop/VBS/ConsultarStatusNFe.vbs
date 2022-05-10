Dim config
Dim consStatServ
Dim exceptionInterop
Dim statusServico
On error Resume Next

'----------------------------------------------------
'Criar configuraçao básica para consumir o serviço
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

exceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
MsgBox "Exceção do CSHARP: " & exceptionInterop.GetMessage()

'Consumir o serviço
statusServico = CreateObject("Unimake.Business.DFe.Servicos.NFe.StatusServico")
statusServico.Executar consStatServ, config

MsgBox "Exceção do CSHARP: " & exceptionInterop.GetMessage()