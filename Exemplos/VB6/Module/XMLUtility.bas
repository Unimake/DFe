Attribute VB_Name = "XMLUtility"
Option Explicit

Sub PrettyPrint(Parent As IXMLDOMNode, Optional Level As Integer)
  Dim Node As IXMLDOMNode
  Dim Indent As IXMLDOMText

  If Not Parent.parentNode Is Nothing And Parent.childNodes.length > 0 Then
    For Each Node In Parent.childNodes
      Set Indent = Node.ownerDocument.createTextNode(vbNewLine & String(Level, vbTab))

      If Node.nodeType = NODE_TEXT Then
        If Trim(Node.Text) = "" Then
          Parent.removeChild Node
        End If
      ElseIf Node.previousSibling Is Nothing Then
        Parent.insertBefore Indent, Node
      ElseIf Node.previousSibling.nodeType <> NODE_TEXT Then
        Parent.insertBefore Indent, Node
      End If
    Next Node
  End If

  If Parent.childNodes.length > 0 Then
    For Each Node In Parent.childNodes
      If Node.nodeType <> NODE_TEXT Then PrettyPrint Node, Level + 1
    Next Node
  End If
End Sub
