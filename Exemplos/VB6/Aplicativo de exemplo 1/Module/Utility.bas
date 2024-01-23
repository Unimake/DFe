Attribute VB_Name = "Utility"
Option Explicit
Public Sub TrapException()
    MsgBox Err.Description, vbCritical, "Erro!!!"
    Log.EscreveLog "========== ERRO ==========", False
    Log.EscreveLog Err.Description, False
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
