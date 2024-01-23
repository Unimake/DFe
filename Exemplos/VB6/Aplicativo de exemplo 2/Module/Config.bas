Attribute VB_Name = "Config"
Global DB1 As New ADODB.Connection
Global glb_Server                           As String
Global glb_UsuarioBanco                     As String
Global glb_NomeBanco                        As String
Global glb_Senha                            As String
Global glb_Terminal                         As String

Global rsGenerico                           As New ADODB.Recordset

'Variaveis globais que sao zeradas para cada nota
Global glb_ValorTotalLiquidoProduto         As Double
Global glb_ValorTotalLiquidoNF              As Double
Global glb_ValorTotalImpostoPrevisto        As Double
Global glb_ValorTotal_Acrescimo             As Double
Global glb_ValorTotal_Desconto              As Double
''

Public Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long


Public Function InicializarConfiguracao(ByVal pTipoDFe As TipoDFe, Optional ByVal pCUF = 0)
Static flagCertificado As Boolean

If flagCertificado = False Then
    flagCertificado = True
End If

Set InicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
InicializarConfiguracao.TipoDFe = CInt(pTipoDFe)

If pCUF > 0 Then InicializarConfiguracao.CodigoUF = pCUF

InicializarConfiguracao.CertificadoSenha = fcRetornaDadosDaTabelaFARQ("CERT_ID")
InicializarConfiguracao.CertificadoArquivo = App.path & "\certificado.pfx"

End Function

Public Sub ABREDB(Optional ByVal p_blnMySQL As Boolean)
    On Error GoTo erro_
    
    glb_Terminal = "001"
    
    If p_blnMySQL Then

        glb_Server = "develop.fsbr.com.br"
        glb_UsuarioBanco = "alberto"
        glb_NomeBanco = "flex_00001"
        glb_Senha = "L4JxvhmW<$d2wcL;"
        
        DB1.Open "driver={MySQL ODBC 8.3 ANSI Driver};server=" & glb_Server & ";port=3306;uid=" & glb_UsuarioBanco & ";pwd=" & glb_Senha & ";database=" & glb_NomeBanco
        
        DB1.ConnectionString = "provider=MSDASQL;driver={MySQL ODBC 8.3 ANSI Driver};"
        DB1.ConnectionTimeout = 60
        DB1.Open , glb_NomeBanco, glb_Senha
        
        

        Set cConnection = CreateObject("ADODB.Connection")

        cConnection.Open "provider=MSDASQL;driver={FLEX};server=" & glb_Server & ";uid=" & glb_UsuarioBanco & ";pwd=" & glb_Senha & ";database=" & glb_NomeBanco

    Else
        glb_Server = "."
        glb_NomeBanco = "Papelrb2"
        glb_Senha = "!*$45937EZ"

        DB1.ConnectionString = "Driver={SQL Server};Server=" & glb_Server & ";Database = " & glb_NomeBanco & ";Persist Security Info = True;"
        DB1.ConnectionTimeout = 60
        DB1.Open , glb_NomeBanco, glb_Senha

        DB1.CommandTimeout = 300
    End If
    
    Exit Sub
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.ABREDB"
    Set DB1 = Nothing
End Sub

Public Function fcDB_OpenRecordSet(ByVal sSQL As String, Optional iTipoAbertura As Integer, Optional ByRef blnconexao As Boolean = False, Optional ByVal p_strNomeTela As String) As ADODB.Recordset
    Dim nContador As Integer
         
    On Error GoTo erro_
começo:
    ''CL''27-11-2002'' Set fcDB_OpenRecordSet = DB1.OpenRecordset(sSQL)
    Set fcDB_OpenRecordSet = New ADODB.Recordset ''CL''27-11-2002''
    
    ''CL''27-11-2002''
    Select Case iTipoAbertura
''Permite somente Leitura
        Case 1 ''visualizar todas as alterações feitas por todos os usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenDynamic, adLockReadOnly
        Case 2 ''com movimetação do registro somente para frente
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenForwardOnly, adLockReadOnly
        Case 3 ''visualizando algumas alterações feitas por alguns usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenKeyset, adLockReadOnly
        Case 4 ''com representação estática do banco
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenStatic, adLockReadOnly

''Os registros serão bloqueados somente quando você chamar o método update.
        Case 5 ''visualizar todas as alterações feitas por todos os usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenDynamic, adLockOptimistic
        Case 6 ''com movimetação do registro somente para frente
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenForwardOnly, adLockOptimistic
        Case 0, 7 ''visualizando algumas alterações feitas por alguns usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenKeyset, adLockOptimistic
        Case 8 ''com representação estática do banco
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenStatic, adLockOptimistic
    
''O registro permanece bloqueado enquanto você efetua a edição.
        Case 9 ''visualizar todas as alterações feitas por todos os usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenDynamic, adLockPessimistic
        Case 10 ''com movimetação do registro somente para frente
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenForwardOnly, adLockPessimistic
        Case 11 ''visualizando algumas alterações feitas por alguns usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenKeyset, adLockPessimistic
        Case 12 ''com representação estática do banco
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenStatic, adLockPessimistic

''Atualiza vários registros de uma só vez com o método UpdateBatch.
        Case 13 ''visualizar todas as alterações feitas por todos os usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenDynamic, adLockPessimistic
        Case 14 ''com movimetação do registro somente para frente
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenForwardOnly, adLockPessimistic
        Case 15 ''visualizando algumas alterações feitas por alguns usuários
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenKeyset, adLockPessimistic
        Case 16 ''com representação estática do banco
            fcDB_OpenRecordSet.Open sSQL, DB1, adOpenStatic, adLockPessimistic
        Case Else
            MsgBox "Passagem de parâmetro incorreto.", vbCritical, "fcDB_OpenRecordSet"
            'alt 28/09/10 60068389 WAGNER - grava log do usuário ....
            usuarioLog.sbGravarLog glb_lngSecaoUsuario, 0, False, Confirmar, "Passagem de parâmetro incorreto.", GUARDAacessoUSU, Usuario, , "mdlCadPadrao"
            ''
    End Select
    blnconexao = True
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcDB_OpenRecordSet"
End Function

Public Function fcRetornaDadosDaTabelaFARQ(Optional ByVal nflag As String = _
                        "CP = CreditoPadrao, TB = cTextoBanner, " & _
                        "RS = RazaoSocial, EN = Endereco, B = Bairro, C = Cidade " & _
                        "ES = Estado, F = Fone, ID= nId, CEP = CEP, FAX = Fax " & _
                        "INS = I.E., CGC = C.G.C., " & _
                        "IM = Inscrição Municipal, " & _
                        "IBGE = Cidade IBGE " & _
                        "CSC = Codigo Seguranca " & _
                        "TCSC = Token Codigo Seguranca" & _
                        "CERT_ID = Certificado ID", _
                         Optional ByVal p_nAcessoID As Long) As Variant
                        'alt 22/05/11 60071353 Cyber adicionando o campo Inscrição Municipal
                        'alt 01/07/18 #xxxx alberto.oliveira retornando dados, conforme ID da empresa
                        
    'JM 20mar01
    On Error GoTo erro_
    Dim rsFarq As New ADODB.Recordset
            
    'Set rsFarq = fcDB_OpenRecordSet("Select * From TABFARQ ")
    'alt 01/07/18 #xxxx alberto.oliveira retornando dados, conforme ID da empresa
    Set rsFarq = fcDB_OpenRecordSet("Select * From TABFARQ " & IIf(p_nAcessoID <> 0, " where Acesso = " & p_nAcessoID, ""))
    ''
    
    If Not rsFarq.EOF Then
        If VBA.UCase(nflag) = "CP" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!CreditoPadrao)
        ElseIf VBA.UCase(nflag) = "TB" Then 'JM 04jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!cTextoBanner)
        ElseIf VBA.UCase(nflag) = "ID" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!nId, True)
        ElseIf VBA.UCase(nflag) = "RS" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!RazaoSocial)
        ElseIf VBA.UCase(nflag) = "EN" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Endereco)
        ElseIf VBA.UCase(nflag) = "B" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Bairro)
        ElseIf VBA.UCase(nflag) = "C" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Cidade)
        ElseIf VBA.UCase(nflag) = "ES" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Estado)
        ElseIf VBA.UCase(nflag) = "F" Then 'JM 07jun01
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Telefone)
        'Cyber 13/11/3
        ElseIf VBA.UCase(nflag) = "CEP" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!CEP)
        ElseIf VBA.UCase(nflag) = "FAX" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Fax)
        ElseIf VBA.UCase(nflag) = "INS" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!Inscricao)
        ElseIf VBA.UCase(nflag) = "CGC" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!CGC)
        'alt 22/05/11 60071353 Cyber retornando a inscrição municipal
        ElseIf VBA.UCase(nflag) = "IM" Then
            fcRetornaDadosDaTabelaFARQ = Replace(Replace(Replace(Replace(CheckNull(rsFarq!IM), ".", ""), "-", ""), "/", ""), " ", "")
        ''
        ElseIf VBA.UCase(nflag) = "IBGE" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!nIBGE)
        ElseIf VBA.UCase(nflag) = "CSC" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!CSC)
        ElseIf VBA.UCase(nflag) = "TCSC" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!TCSC, True)
        ElseIf VBA.UCase(nflag) = "CERT_ID" Then
            fcRetornaDadosDaTabelaFARQ = CheckNull(rsFarq!CERT_ID, True)
        End If
    End If
    
    rsFarq.Close
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcRetornaDadosDaTabelaFARQ"
End Function


Public Function fcRetornaConteudoDaTabGeral(ByVal sCodigo As String, _
                        Optional ByVal sAcesso As String, _
                        Optional ByRef QtdRegistros As Integer, _
                        Optional ByVal blnRetornarOAcesso As Boolean = False, _
                        Optional ByVal blnRetornarOConteudo2 As Boolean = False) As Variant
    'JM 20mar01
    On Error GoTo erro_

    Dim sAND            As String
    Dim rsTABGeral      As New ADODB.Recordset
    Dim nQtdRegistros   As Integer
    Dim i               As Integer
    Dim aVetor()        As String
    
    If sAcesso <> "" Then
        sAND = " AND cAcesso = '" & sAcesso & "'"
    End If

    Set rsTABGeral = fcDB_OpenRecordSet("Select cAcesso, cConteudo1, cConteudo2  From TABGeral" & _
                    " Where cCodigo = '" & sCodigo & "'" & sAND)
    
    nQtdRegistros = 0
    If Not rsTABGeral.EOF Then
        rsTABGeral.MoveLast
        nQtdRegistros = rsTABGeral.RecordCount
        rsTABGeral.MoveFirst
    End If
    QtdRegistros = nQtdRegistros
    
    If nQtdRegistros = 1 Then
        If blnRetornarOAcesso Then
            fcRetornaConteudoDaTabGeral = rsTABGeral!cAcesso & "|" & CheckNull(rsTABGeral!cConteudo1) & IIf(blnRetornarOConteudo2, "|" & CheckNull(rsTABGeral!cConteudo2), "")
        Else
            fcRetornaConteudoDaTabGeral = CheckNull(rsTABGeral!cConteudo1) & IIf(blnRetornarOConteudo2, "|" & CheckNull(rsTABGeral!cConteudo2), "")
        End If
    ElseIf nQtdRegistros > 1 Then
        ReDim aVetor(1 To nQtdRegistros)
        For i = 1 To nQtdRegistros
            If blnRetornarOAcesso Then
                aVetor(i) = rsTABGeral!cAcesso & "|" & CheckNull(rsTABGeral!cConteudo1) & IIf(blnRetornarOConteudo2, "|" & CheckNull(rsTABGeral!cConteudo2), "")
            Else
                aVetor(i) = CheckNull(rsTABGeral!cConteudo1) & IIf(blnRetornarOConteudo2, "|" & CheckNull(rsTABGeral!cConteudo2), "")
            End If
            rsTABGeral.MoveNext
        Next i
        
        fcRetornaConteudoDaTabGeral = aVetor
        
    End If
    
    rsTABGeral.Close
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcRetornaConteudoDaTabGeral"
End Function
Public Function CheckNull(ByVal Conteudo As Variant, _
                          Optional ByVal bnlValor As Boolean = False) As Variant
    'JM 20mar01
    On Error GoTo erro_
    
    If Not IsNull(Conteudo) And Not IsEmpty(Conteudo) Then
        If Not bnlValor Then
            If Conteudo = "" Then
                CheckNull = ""
            Else
                CheckNull = Conteudo
            End If
        Else
            CheckNull = Conteudo
        End If
        Exit Function
    End If
    
    If bnlValor Then
        CheckNull = 0
    Else
        CheckNull = ""
    End If
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.CheckNull"
End Function


Public Function sbDB_AlterarNDados(ByVal sNomeTabela As String, ByVal sCampoFiltro As String, ParamArray CampoxValor()) 'FA 01out01
    Dim Rs1 As New ADODB.Recordset
    Dim lin As Integer
    
    On Error GoTo erro_
    Set Rs1 = fcDB_OpenRecordSet("SELECT * FROM " & sNomeTabela & fcWHEREouAND(sNomeTabela) & sCampoFiltro)
    If Not Rs1.EOF Then
        Do While Not Rs1.EOF 'JM 27nov01
            ''Rs1.Edit
            For lin = 0 To UBound(CampoxValor()) Step 2
                Rs1(Replace(CStr(CampoxValor(lin)), "|", "")) = CampoxValor(lin + 1)
            Next
            
            'If sNomeTabela = "TabConfCarteira" Then Rs1!cTerminal = Null
            Rs1.Update
            Rs1.MoveNext
        Loop
    End If
    Rs1.Close
    Set Rs1 = Nothing
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.sbDB_AlterarNDados"
End Function

Public Function fcWHEREouAND(ByVal sSQL As String)
    On Error GoTo erro_
    
    If UCase(sSQL) Like "*WHERE*" Then
        fcWHEREouAND = " AND "
    Else
        fcWHEREouAND = " WHERE "
    End If
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcWHEREouAND"
End Function

Public Function sbDB_ExcluiNRegistro(ByVal sNomeTabela As String, Optional ByVal sFiltro As String, _
                                     Optional ByVal nTop As String = "Todos") 'JM 27nov01 'Cyber 5/8/2
    
    Dim Rs1             As New ADODB.Recordset
    Dim cCampoPrimary   As String
    
    On Error GoTo erro_
    
    If sFiltro <> "" Then
        Set Rs1 = fcDB_OpenRecordSet("SELECT " & IIf(IsNumeric(nTop), "TOP " & nTop, "") & " * FROM " & sNomeTabela & " WHERE " & sFiltro)
        Do While Not Rs1.EOF
            Rs1.Delete
            Rs1.MoveNext
        Loop
        Rs1.Close
        Set Rs1 = Nothing
    Else
        DB1.Execute "Delete From " & sNomeTabela
    End If
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.sbDB_ExcluiNRegistro"
End Function


Public Function fcDB_IncluirNDados(ByVal sNomeTabela As String, ParamArray CampoxValor()) As Long 'FA 01out01
    Dim Rs1 As New ADODB.Recordset
    Dim lin As Integer
    Dim nValAcesso As Long
    Dim nPosAutIncrField As Long
    Dim blnTamanhoEx As Boolean 'alt 08/03/19 #2624 fernando.santos 09:17 08/03/2019 N
    On Error GoTo erro_
       
    'Set Rs1 = fcDB_OpenRecordSet("Select * From " & sNomeTabela)
    Set Rs1 = fcDB_OpenRecordSet("SELECT TOP 1 * FROM " & sNomeTabela) 'LM 11ago04
    nPosAutIncrField = fcRetornaPosAutoIncrField(, Rs1)
    
    Rs1.AddNew
    If Not IsNull(Rs1(nPosAutIncrField)) Then
        nValAcesso = Rs1(nPosAutIncrField)
    End If
    For lin = 0 To UBound(CampoxValor()) Step 2
        'alt 08/03/19 #2624 fernando.santos 09:17 08/03/2019 N
        'Rs1(Replace(CStr(CampoxValor(lin)), "|", "")) = CampoxValor(lin + 1)
        blnTamanhoEx = False
        If IsNull(CampoxValor(lin + 1)) Then
            blnTamanhoEx = False
        Else
            blnTamanhoEx = Len(Trim(CampoxValor(lin + 1))) > Rs1(Replace(CStr(CampoxValor(lin)), "|", "")).DefinedSize
        End If
        If blnTamanhoEx = True Then blnTamanhoEx = fcDB_ValorString(CStr(CampoxValor(lin)), Rs1)
        'If fcDB_ValorString(Trim(CampoxValor(lin + 1)), Rs1) = True Then
        If blnTamanhoEx = True Then
            Rs1(Replace(CStr(CampoxValor(lin)), "|", "")) = Mid(CampoxValor(lin + 1), 1, Rs1(Replace(CStr(CampoxValor(lin)), "|", "")).DefinedSize)
        Else
            Rs1(Replace(CStr(CampoxValor(lin)), "|", "")) = CampoxValor(lin + 1)
        End If
        ''
    Next
    Rs1.Update
    
    If Not IsNumeric(Rs1(nPosAutIncrField)) Then
      nValAcesso = 0
    Else
      nValAcesso = Rs1(nPosAutIncrField)
    End If

    If nValAcesso > 0 Then
        fcDB_IncluirNDados = nValAcesso
    End If
    
    Rs1.Close
    Set Rs1 = Nothing
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcDB_IncluirNDados"
End Function

Public Function fcRetornaPosAutoIncrField(Optional ByVal sTabela As String, _
                            Optional ByVal rsRecordset As ADODB.Recordset) As Long
    'JM 20set01
    On Error GoTo erro_
    Dim i As Integer
    
    fcRetornaPosAutoIncrField = 0
    
    If sTabela <> "" Then
        
    Else
        For i = 0 To rsRecordset.fields.Count - 1
            If rsRecordset.fields(i).Attributes = 16 Then
                fcRetornaPosAutoIncrField = i
                Exit For
            End If
        Next i
    End If
    
    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcRetornaPosAutoIncrField"
End Function


Public Function fcDB_ValorString(par_strCampoNome As String, ByRef par_rsAdo As ADODB.Recordset) As Boolean
    On Error GoTo erro_
    fcDB_ValorString = False
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adBSTR) 'adBSTR
    If fcDB_ValorString = True Then Exit Function
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adChapter)
    If fcDB_ValorString = True Then Exit Function
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adChar)
    If fcDB_ValorString = True Then Exit Function
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adVarChar)
    If fcDB_ValorString = True Then Exit Function
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adVarWChar)
    If fcDB_ValorString = True Then Exit Function
    fcDB_ValorString = (par_rsAdo(Replace(par_strCampoNome, "|", "")).Type = adWChar)

    Exit Function
erro_:
    MsgBox Err.Number & ", " & Err.Description, vbCritical, "Config.fcDB_ValorString"
End Function

