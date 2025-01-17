Attribute VB_Name = "ImportacaoXMLNFe"
Option Explicit
Public Sub ImportarXMLNFe()
On Error GoTo erro

Log.ClearLog

Dim oNFeProc
Dim oInfNFe

Set oNFeProc = CreateObject("Unimake.Business.DFe.Xml.NFe.NfeProc")
Set oNFeProc = oNFeProc.LoadFromFile("C:\Users\Wandrey\Downloads\26241008862530000231550010005158811706174031-procNFe.xml")
Set oInfNFe = oNFeProc.NFe.GetInfNFe(0)

' Mostra a chave da NFe
MsgBox oInfNFe.Chave, vbInformation, "Chave da NFe"

' Informações do grupo de tag <ide>
MsgBox oInfNFe.Ide.NNF, vbInformation, "Número da Nota Fiscal"
MsgBox oInfNFe.Ide.DhEmi, vbInformation, "Data de Emissão"
MsgBox oInfNFe.Ide.DhEmiField, vbInformation, "Data de Emissão (Field)"
MsgBox oInfNFe.Ide.DhSaiEnt, vbInformation, "Data de Saída/Entrada"
MsgBox oInfNFe.Ide.DhSaiEntField, vbInformation, "Data de Saída/Entrada (Field)"

' Informações do emitente da nota
MsgBox oInfNFe.Emit.CNPJ, vbInformation, "CNPJ do Emitente"
MsgBox oInfNFe.Emit.XNome, vbInformation, "Razão Social do Emitente"
MsgBox oInfNFe.Emit.EnderEmit.XLgr, vbInformation, "Endereço - Logradouro"
MsgBox oInfNFe.Emit.EnderEmit.CMun, vbInformation, "Endereço - Código do Município"
MsgBox oInfNFe.Emit.EnderEmit.XMun, vbInformation, "Endereço - Nome do Município"

' Informações dos produtos da nota fiscal
Dim i As Integer
Dim oDet
For i = 1 To oInfNFe.GetDetCount() ' Quantidade de produtos no XML
    Set oDet = oInfNFe.GetDet(i - 1)
    
    MsgBox oDet.NItem, vbInformation, "Número do Item"
    MsgBox oDet.Prod.CProd, vbInformation, "Código do Produto"
    MsgBox oDet.Prod.XProd, vbInformation, "Descrição do Produto"
    MsgBox oDet.Prod.CEAN, vbInformation, "EAN"
    MsgBox oDet.Prod.CBarra, vbInformation, "Código de Barra"
    MsgBox oDet.Prod.NCM, vbInformation, "NCM"
    MsgBox oDet.Prod.CFOP, vbInformation, "CFOP"
    MsgBox oDet.Prod.QCom, vbInformation, "Quantidade Comercializada"
    MsgBox oDet.Prod.UCom, vbInformation, "Unidade Comercial"
    MsgBox oDet.Prod.VUnCom, vbInformation, "Valor Unitário"
    MsgBox oDet.Prod.VProd, vbInformation, "Valor Total"
    
    If Not oDet.Imposto.ICMS.ICMSSN101 Is Nothing Then
        MsgBox oDet.Imposto.ICMS.ICMSSN101.Orig, vbInformation, "Origem"
        MsgBox oDet.Imposto.ICMS.ICMSSN101.CSOSN, vbInformation, "CSOSN"
        MsgBox oDet.Imposto.ICMS.ICMSSN101.PCredSN, vbInformation, "Percentual Crédito SN"
        MsgBox oDet.Imposto.ICMS.ICMSSN101.VCredICMSSN, vbInformation, "Valor Crédito SN"
    End If
    
    If Not oDet.Imposto.ICMS.ICMSSN102 Is Nothing Then
        MsgBox oDet.Imposto.ICMS.ICMSSN102.Orig, vbInformation, "Origem"
        MsgBox oDet.Imposto.ICMS.ICMSSN102.CSOSN, vbInformation, "CSOSN"
    End If
    
    If Not oDet.Imposto.IPI Is Nothing Then
        MsgBox oDet.Imposto.IPI.CEnq, vbInformation, "Código Enquadramento IPI"
        
        If Not oDet.Imposto.IPI.IPINT Is Nothing Then
            MsgBox oDet.Imposto.IPI.IPINT.CST, vbInformation, "CST IPI"
        End If
    End If
Next i

' Informações das cobranças
MsgBox oInfNFe.Cobr.Fat.NFat, vbInformation, "Número da Fatura"
MsgBox oInfNFe.Cobr.Fat.VOrig, vbInformation, "Valor Original"
MsgBox oInfNFe.Cobr.Fat.VDesc, vbInformation, "Valor do Desconto"
MsgBox oInfNFe.Cobr.Fat.VLiq, vbInformation, "Valor Líquido"

' Informações das duplicatas
Dim j As Integer
Dim oDup
For j = 1 To oInfNFe.Cobr.GetDupCount() ' Quantidade de duplicatas
    Set oDup = oInfNFe.Cobr.GetDup(j - 1)
    
    MsgBox oDup.NDup, vbInformation, "Número da Duplicata"
    MsgBox oDup.DVenc, vbInformation, "Data de Vencimento"
    MsgBox oDup.DVencField, vbInformation, "Data de Vencimento (Field)"
    MsgBox oDup.VDup, vbInformation, "Valor da Duplicata"
Next j

Exit Sub
erro:
Utility.TrapException

End Sub
