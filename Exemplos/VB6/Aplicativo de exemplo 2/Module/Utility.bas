Attribute VB_Name = "Utility"
Option Explicit
Public Sub TrapException()
    Log.EscreveLog "========== ERRO ==========", False
    If Err.Number = 440 Then
        Log.EscreveLog "Os dados informados na Nota (Emitente, Destinatario, Produtos, Impostos, etc) nao estao validos para Gerar o XML.", False
    Else
        MsgBox Err.Description, vbCritical, "Erro!!!"
        Log.EscreveLog Err.Description, False
    End If
    Log.EscreveLog "==========================", False
End Sub

Public Function DirExists(ByVal path As String)
On Error GoTo erro
    If GetAttr(path) And vbDirectory Then
        DirExists = True
    Else
        DirExists = False
    End If
    
Exit Function

erro:
DirExists = False
End Function
