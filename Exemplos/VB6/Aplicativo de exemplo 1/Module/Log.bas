Attribute VB_Name = "Log"
Option Explicit

Public Sub EscreveLog(ByVal Log As String, ByVal isXML As Boolean)
Dim XmlDoc As New MSXML2.DOMDocument60

If isXML Then
On Error GoTo erroXML
    XmlDoc.loadXML (Log)
    PrettyPrint XmlDoc
    Log = XmlDoc.xml
End If

continue:
frmMain.EscreveLog Log
Exit Sub
erroXML:
Resume continue

End Sub

Public Sub ClearLog()
frmMain.ClearLog
End Sub


