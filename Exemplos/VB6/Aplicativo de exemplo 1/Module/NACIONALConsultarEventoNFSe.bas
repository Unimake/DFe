Attribute VB_Name = "ConsultaEventoNFSeNacional"
' ------------------------------------------------------------------------
' Consultar Eventos NFSe - Padrão Nacional
' ------------------------------------------------------------------------

Option Explicit

Public Sub ConsultarEventoNFSeNacional()
    Dim oConfiguracao As Object
    Dim oExceptionInterop As Object
    
    Dim oConsultarEvento As Object
    Dim oConsPedRegEvento As Object
    
    Dim xmlGerado As String
    Dim xmlRetornado As String
    
    On Error GoTo TrataErro
    
    ' Criar objeto para pegar exceção do lado do C#
    Set oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
    
    ' Criar objeto de configuração mínima
    Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    oConfiguracao.TipoDFe = 5                          ' 5 = NFSe
    oConfiguracao.CertificadoArquivo = "C:\projetos\certificados\XXX.pfx"
    oConfiguracao.CertificadoSenha = "Mh26"
    oConfiguracao.CodigoMunicipio = 1001058            ' Padrão Nacional
    oConfiguracao.TipoAmbiente = 1                     ' 1 = Produção
    oConfiguracao.Servico = 92                         ' NFSeConsultarEventosDiversos
    oConfiguracao.SchemaVersao = "1.01"
    
    ' Criar objeto XML de consulta do pedido de registro de evento
    Set oConsPedRegEvento = CreateObject("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento")
    oConsPedRegEvento.Versao = "1.01"
    
    Set oConsPedRegEvento.InfConsPedRegEvento = CreateObject("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfConsPedRegEvento")
    oConsPedRegEvento.InfConsPedRegEvento.ChNFSe = "41055082234180727000110000000000017026025846920130"
    oConsPedRegEvento.InfConsPedRegEvento.TipoEvento = "101101"   ' Cancelamento
    oConsPedRegEvento.InfConsPedRegEvento.NumSeqEvento = "001"
    
    ' Gerar XML
    xmlGerado = CStr(oConsPedRegEvento.GerarXmlString)
    
    ' Exibir XML gerado
    MsgBox xmlGerado
    
    ' Gravar XML gerado no HD
    If FileExists("d:\testenfe\ConsultaEventoNFSeNacional.xml") Then
        Kill "d:\testenfe\ConsultaEventoNFSeNacional.xml"
    End If
    
    Call SalvarTextoEmArquivo("d:\testenfe\ConsultaEventoNFSeNacional.xml", xmlGerado)
    
    ' Criar objeto de consumo do serviço
    Set oConsultarEvento = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarEvento")
    
    ' Executar consulta
    oConsultarEvento.Executar xmlGerado, oConfiguracao
    
    ' Recuperar XML retornado
    xmlRetornado = CStr(oConsultarEvento.RetornoWSString)
    MsgBox "XML retornado pela prefeitura:" & vbCrLf & xmlRetornado
    
    ' Gravar XML retornado no HD
    If FileExists("d:\testenfe\ConsultaEventoNFSeNacional_retorno.xml") Then
        Kill "d:\testenfe\ConsultaEventoNFSeNacional_retorno.xml"
    End If
    
    Call SalvarTextoEmArquivo("d:\testenfe\ConsultaEventoNFSeNacional_retorno.xml", xmlRetornado)
    
    ' Verificar retorno
    If Not (oConsultarEvento.result Is Nothing) Then
        If Not (oConsultarEvento.result.erro Is Nothing) Then
            MsgBox "Algo deu errado: " & _
                   CStr(oConsultarEvento.result.erro.Descricao) & _
                   " - " & _
                   CStr(oConsultarEvento.result.erro.Codigo)
        Else
            MsgBox "Chave de acesso da NFSe: " & CStr(oConsultarEvento.result.Eventos.ChaveAcesso)
            MsgBox "Tipo Evento: " & CStr(oConsultarEvento.result.Eventos.TipoEvento)
            MsgBox "VerAplic: " & CStr(oConsultarEvento.result.Eventos.ArquivoXml.Evento.InfEvento.VerAplic)
            MsgBox "Id Evento: " & CStr(oConsultarEvento.result.Eventos.ArquivoXml.Evento.InfEvento.id)
            MsgBox "Id Pedido de registro do Evento: " & _
                   CStr(oConsultarEvento.result.Eventos.ArquivoXml.Evento.InfEvento.PedRegEvento.InfPedReg.id)
        End If
    End If
    
    Exit Sub

TrataErro:
    MsgBox "Erro ao consultar NFSe por evento: " & Err.Description
    On Error Resume Next
    MsgBox oExceptionInterop.GetMessage
    MsgBox CStr(oExceptionInterop.GetErrorCode)
End Sub

Private Function FileExists(ByVal FileName As String) As Boolean
    FileExists = (Len(Dir$(FileName)) > 0)
End Function

Private Sub SalvarTextoEmArquivo(ByVal FileName As String, ByVal Conteudo As String)
    Dim f As Integer
    f = FreeFile
    Open FileName For Binary As #f
    Put #f, , Conteudo
    Close #f
End Sub
