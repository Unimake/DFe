' Imprimir DANFE usando UniDANFE via VBScript

Dim oUnidanfeConfiguration, oUnidanfeServices
Dim cPastaConfig

On Error Resume Next

' Criar as configurações
Set oUnidanfeConfiguration = CreateObject("Unimake.Unidanfe.Configurations.UnidanfeConfiguration")
oUnidanfeConfiguration.WaitProcess = False
oUnidanfeConfiguration.Arquivo = "D:\testenfe\41220606117473000150550010000580071051443444-procnfe.xml"
oUnidanfeConfiguration.Visualizar = True
oUnidanfeConfiguration.Imprimir = False
oUnidanfeConfiguration.EnviaEmail = False

' Pasta correta de configuração (não incluir a subpasta "dados")
cPastaConfig = "C:\Unimake\Unimake.UniDANFe\Unimake_EXE"
oUnidanfeConfiguration.PastaConfiguracao = cPastaConfig

' Criar serviço e executar impressão
Set oUnidanfeServices = CreateObject("Unimake.Unidanfe.UnidanfeServices")
oUnidanfeServices.Execute oUnidanfeConfiguration
MsgBox "Aguarde!"

' Executar telas adicionais
oUnidanfeServices.ShowConfigurationScreenPastaConfig cPastaConfig
MsgBox "Aguarde!"

oUnidanfeServices.ShowEmailScreenPastaConfig cPastaConfig
MsgBox "Aguarde!"

oUnidanfeServices.ShowLicencaScreenPastaConfig cPastaConfig
MsgBox "Aguarde!"
