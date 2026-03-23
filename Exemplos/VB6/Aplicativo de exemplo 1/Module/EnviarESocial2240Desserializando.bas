Option Explicit

' ---------------------------------------------------------------------------------
' Enviar lote de eventos do eSocial - Evento 2240
' ---------------------------------------------------------------------------------
Public Function EnviarEsocial2240Desserializando() As Variant

    Dim oConfiguracao As Object
    Dim oExceptionInterop As Object
    Dim oErro As ErrObject
    
    Dim xmlString As String
    Dim nomeArqLoteEvento As String
    Dim stringXMLLoteAssinado As String
    
    Dim oESocialEnvioLoteEventos As Object
    Dim oEnviarLoteEventosESocial As Object
    
    On Error GoTo TratarErro

    ' Criar o objeto de configuração mínima
    Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

    oConfiguracao.TipoDFe = 12                  ' 12 = eSocial
    oConfiguracao.Servico = 69                  ' Servico.ESocialEnviarLoteEventos
    oConfiguracao.CertificadoArquivo = "c:\projetos\unimakecm.pfx"
    oConfiguracao.CertificadoSenha = "12345678"
    oConfiguracao.TipoAmbiente = 2              ' TipoAmbiente.Homologação

    ' Criar objeto para pegar exceção do lado do CSHARP
    Set oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")

    nomeArqLoteEvento = "C:\projetos\github\UniNFe\exemplos xml\eSocial\S_01_03_00\EnvioLoteEventos-esocial-loteevt.xml"
    
    ' ATENÇÃO:
    ' No código original em xHarbour, a variável xmlString não recebe valor antes do uso.
    ' Portanto, aqui você deve carregar o conteúdo do XML em xmlString antes de continuar.
    '
    ' Exemplo:
    ' xmlString = LerArquivoTexto(nomeArqLoteEvento)
    '
    ' Ou montar a string manualmente.
    
    Call GravarTextoEmArquivo(nomeArqLoteEvento, xmlString)

    DoEvents
    
    ' Criar objeto do lote de eventos
    Set oESocialEnvioLoteEventos = CreateObject("Unimake.Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos")

    ' Desserializar o XML de lote de eventos a partir da string do XML
    Set oESocialEnvioLoteEventos = oESocialEnvioLoteEventos.LoadFromXML(xmlString)

    ' Criar objeto para consumir o serviço de envio de lotes de eventos do eSocial
    Set oEnviarLoteEventosESocial = CreateObject("Unimake.Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial")

    ' Consumir o serviço
    Call oEnviarLoteEventosESocial.Executar(oESocialEnvioLoteEventos, oConfiguracao)

    stringXMLLoteAssinado = oEnviarLoteEventosESocial.GetConteudoXMLAssinado()

    Debug.Print "StringXMLAssinado: "; stringXMLLoteAssinado
    Debug.Print
    Debug.Print

    Call GravarTextoEmArquivo("D:\testenfe\xmlloteesocialeventosassinado.xml", stringXMLLoteAssinado)

    Debug.Print oEnviarLoteEventosESocial.RetornoWSString
    Debug.Print
    Debug.Print

    Call GravarTextoEmArquivo("D:\testenfe\xmlloteesocialeventos-ret.xml", oEnviarLoteEventosESocial.RetornoWSString)

    Set oEnviarLoteEventosESocial = Nothing
    Set oESocialEnvioLoteEventos = Nothing
    Set oExceptionInterop = Nothing
    Set oConfiguracao = Nothing

    EnviarEsocial2240Desserializando = Null
    Exit Function

TratarErro:
    ' Demonstrar exceções geradas no próprio VB6/Harbour, se existir
    Debug.Print "ERRO"
    Debug.Print "===="
    Debug.Print "Falha ao tentar consultar o status do serviço."
    Debug.Print Err.Description
    Debug.Print "Código: " & Err.Number

    ' Demonstrar a exceção do CSHARP
    Debug.Print
    On Error Resume Next
    Debug.Print "Exceção do CSHARP - Message: "; oExceptionInterop.GetMessage()
    Debug.Print "Exceção do CSHARP - Código: "; oExceptionInterop.GetErrorCode()
    Debug.Print

    Set oEnviarLoteEventosESocial = Nothing
    Set oESocialEnvioLoteEventos = Nothing
    Set oExceptionInterop = Nothing
    Set oConfiguracao = Nothing

    EnviarEsocial2240Desserializando = Null
End Function

Private Sub GravarTextoEmArquivo(ByVal CaminhoArquivo As String, ByVal Conteudo As String)
    Dim f As Integer
    
    f = FreeFile
    Open CaminhoArquivo For Output As #f
    Print #f, Conteudo;
    Close #f
End Sub

Private Function LerArquivoTexto(ByVal CaminhoArquivo As String) As String
    Dim f As Integer
    Dim Conteudo As String
    
    f = FreeFile
    Open CaminhoArquivo For Binary Access Read As #f
    Conteudo = Space$(LOF(f))
    Get #f, , Conteudo
    Close #f
    
    LerArquivoTexto = Conteudo
End Function