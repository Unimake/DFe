Imports System
Imports System.Net
Imports System.Security
Imports System.Security.Cryptography
Imports System.Security.Cryptography.X509Certificates
Imports System.Security.Cryptography.Xml
Imports System.Collections.Generic
Imports System.Text
Imports System.Xml
Imports System.Windows.Forms
Imports Microsoft.Win32
Imports Unimake.Business.DFe
Imports Unimake.Business.DFe.Servicos.Configuracao
Imports Unimake.Business.DFe.Xml.NFe
Imports Unimake.Security.Platform
Imports Unimake.Security.Exceptions


Public Class MPNSabra


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        End
    End Sub


    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        JaFeito = 2
        LocalPFX = TLocalPFX.Text
        SenhaPfx = TSenhaPFX.Text
        If CheckBox1.Checked = True Then

            CertificadoInstalado()
        Else
            ' Ler Certificado em arquivo .PFX
            Configuracao.certificadoarquivo = Trim(TLocalPFX.Text)
            CertificadoPFX()

        End If
    End Sub

    Private Sub Inutilização_Click(sender As Object, e As EventArgs) Handles Inutilização.Click
        If PorNumero.Checked = True Then
            InutilizarNumero()
        Else
            CancelarNFe()
        End If
    End Sub


    Private Sub MPNSabra_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        JaFeito = 0
    End Sub

    Private Sub MPNSabra_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        If JaFeito = 0 Then
            textotela = Me.Text
            JaFeito = 1

            LerArquivoINI("MPNSabra.ini")

            TLocalPFX.Text = LocalPFX
            TSenhaPFX.Text = SenhaPfx


            If Trim(LocalPFX) = "" Then
                CheckBox1.Checked = True
            Else
                CheckBox1.Checked = False
            End If
        End If


        ' Inicializa a Configuração

        If TipoNF = 55 Then
            Configuracao.TipoDFe = TipoDFe.NFe
            'Configuracao.TipoDFe = ModeloDFe.NFe
        Else
            Configuracao.TipoDFe = TipoDFe.NFCe
            'Configuracao.TipoDFe = ModeloDFe.NFCe
        End If

        Configuracao.CodigoUF = CUF
        Configuracao.servico = 0

        Configuracao.CertificadoSenha = Trim(TSenhaPFX.Text)

        If CheckBox1.Checked = True Then
            CertificadoInstalado()
        Else
            ' Ler Certificado em arquivo .PFX
            Configuracao.certificadoarquivo = Trim(TLocalPFX.Text)
            CertificadoPFX()
            Configuracao.CertificadoDigital = oCertificado
            Configuracao.CertificadoArquivo = Trim(TLocalPFX.Text)

        End If

        '  Dim DataInicial As String =  StringCertificado.NotBefore
        '  Dim DataFinal as string =  StringCertificado.NotAfter

    End Sub


    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        ConsultarSituacaoNF()
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        EnviarNFe()

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        AutorizarPorArquivoNFe()
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        EnviarEventoCCe()
    End Sub
End Class