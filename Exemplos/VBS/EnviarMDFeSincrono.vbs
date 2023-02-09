' --------------------------------------------------
' Enviar MDFe em em modo Sincrono
' --------------------------------------------------
Dim oConfiguracao
Dim oMDFe
Dim oCertificado
Dim oCertificadoSelecionado
Dim oAutorizacaoSinc
Dim oVerificarA3
Dim ehA3

'Carregar o certificado digital
Set oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set oCertificadoSelecionado = oCertificado.AbrirTelaSelecao()

'Testar se for certificado A3
Set oVerificarA3 = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")

ehA3 = oVerificarA3.IsA3 ((oCertificadoSelecionado))
If ehA3 Then 
   MsgBox "Tipo do certificado digital e A3."
   
   'Setar o PIN do A3 para não precisar informar manualmente
   oVerificarA3.SetPinPrivateKey (oCertificadoSelecionado), ("123456")
Else
   MsgBox "Tipo do certificado digital e A1."
End if

'Montar o objeto de configuração com informações mínimas 
'para ser utilizado na hora de consumir o serviço
Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
oConfiguracao.TipoDFe = 4 '4 = MDFe
oConfiguracao.CertificadoDigital = oCertificadoSelecionado

'Criar o XML do MDFe
Set oMDFe = GetFromFileMDFe() 'Ler XML que elaborado anteriormente

'Resgatar algumas informações do objeto do XML
MsgBox "Versao schema: " + oMDFe.InfMDFe.Versao
MsgBox "Nome Emitente: " + oMDFe.InfMDFe.Emit.XNome
MsgBox "Chave MDFe: " + oMDFe.InfMDFe.Chave

'Criar serviço de autorização e enviar o XML para SEFAZ
Set oAutorizacaoSinc = CreateObject("Unimake.Business.DFe.Servicos.MDFe.AutorizacaoSinc")
oAutorizacaoSinc.SetXMLConfiguracao (oMDFe), (oConfiguracao)
oAutorizacaoSinc.Executar (oMDFe), (oConfiguracao)

'Demonstrar o XML retornado pela SEFAZ
MsgBox oAutorizacaoSinc.RetornoWSString

'----------------------------------------------------
'Serializar o XML do MDFe
'----------------------------------------------------
Function GetFromFileMDFe()
   Dim MDFe: Set MDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   Set GetFromFileMDFe = MDFe.LoadFromFile("C:\Users\Wandrey\Downloads\Telegram Desktop\31220437920833000180580010000000021000000020-mdfe.xml")
End Function