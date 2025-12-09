Attribute VB_Name = "TestesCertificado"

Option Explicit

Public Sub TestesDiversosCertificado()
    Dim oCertificado
    Dim oConfiguracao

    Dim thumbPrint As String
    Dim serialNumber As String

  ' -------------------------------------------------------------
  ' Trabalhar com certificado A3
  ' -------------------------------------------------------------
    Log.ClearLog

    ' Criar objeto para trabalhar com certificados digitais
    Set oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")

    ' Abrir tela para selecionar o certificado instalado no repositorio do windows
    Call oCertificado.AbrirTelaSelecao

    ' Voce pode salvar o Thumbprint ou SerialNumber em banco para resgatar depois, será necessário um ou outro para resgatar o certificado para uso
    thumbPrint = oCertificado.GetThumbPrint()
    serialNumber = oCertificado.GetSerialNumber()

    Log.EscreveLog "Thumbprint  " & thumbPrint, False
    Log.EscreveLog "Serial Number  " & serialNumber, False
    Log.EscreveLog "Validade Inicial " & oCertificado.GetNotBefore(), False
    Log.EscreveLog "Validade Final " & oCertificado.GetNotAfter(), False
    
    ' Na hora de usar o certificado na configuração para assinar a nota, por exemplo, é esta forma que deve ser usada
    Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    oConfiguracao.TipoDFe = 15     ' 15 = NFCom
    oConfiguracao.TipoEmissao = 1  ' 1 = Normal
    ' Pode-se resgatar o certificado pelo ThumbPrint
    oConfiguracao.CertificadoSerialNumberOrThumbPrint = thumbPrint
    ' ou pelo SerialNumber usando a mesma propriedade da consiguração (Ou usa o thumbPrint ou o SerialNumber, não pode deixar os dois)
    oConfiguracao.CertificadoSerialNumberOrThumbPrint = serialNumber
    
    ' No parametro do seu sistema você terá que salvar o serialNumber ou o thumbPrint para conseguir resgatar na hora do uso
End Sub
