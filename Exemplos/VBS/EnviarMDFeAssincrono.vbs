' --------------------------------------------------
' Enviar MDFe em em modo assincrono
' --------------------------------------------------

Dim oConfiguracao
Dim oEnviMDFe
Dim oCertificado
Dim oCertificadoSelecionado
Dim oAutorizacao

'Carregar o certificado digital
Set oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set oCertificadoSelecionado = oCertificado.AbrirTelaSelecao()

'Montar o objeto de configuração com informações mínimas 
'para ser utilizado na hora de consumir o serviço
Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
oConfiguracao.TipoDFe = 4 '4 = MDFe
'oConfiguracao.CertificadoSenha = "12345678"
'oConfiguracao.CertificadoArquivo = "D:\projetos\UnimakePV.pfx"
oConfiguracao.CertificadoDigital = oCertificadoSelecionado

'Criar o XML do MDFe
Set oEnviMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnviMDFe")
oEnviMDFe.Versao = "3.00"
oEnviMDFe.IdLote = "000000000000001"
oEnviMDFe.MDFe = GetFromFileMDFe() 'Ler XML de um arquivo que foi elaborado anteriormente 

'Resgatar algumas informações do objeto do XML
MsgBox "Versao schema: " + oEnviMDFe.Versao
MsgBox "Numero lote: " + oEnviMDFe.IdLote
MsgBox "Nome Emitente: " + oEnviMDFe.MDFe.InfMDFe.Emit.XNome
MsgBox "Chave MDFe: " + oEnviMDFe.MDFe.InfMDFe.Chave

'Criar serviço de autorização e enviar o XML para SEFAZ
Set oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.Autorizacao")                                 
oAutorizacao.SetXMLConfiguracao (oEnviMDFe), (oConfiguracao)
oAutorizacao.Executar (oEnviMDFe), (oConfiguracao)

'Demonstrar o XML retornado pela SEFAZ
MsgBox oAutorizacao.RetornoWSString

'----------------------------------------------------
'Serializar o XML do MDFe
'----------------------------------------------------
Function GetFromFileMDFe()
   Dim MDFe: Set MDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   Set GetFromFileMDFe = MDFe.LoadFromFile("C:\Users\Wandrey\Downloads\Telegram Desktop\31220437920833000180580010000000021000000020-mdfe.xml")
End Function
