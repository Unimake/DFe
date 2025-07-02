Attribute VB_Name = "ServicosNFSe"
Option Explicit

Public Sub RecepcionarLoteRps() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado (arquivo SelecionarCertificado.bas)
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 28 ' Servico.NFSeRecepcionarLoteRps
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoRecepcionarLoteRps: set servicoRecepcionarLoteRps = CreateObject("Unimake.Business.DFe.Servicos.NFSe.RecepcionarLoteRps")
    Call servicoRecepcionarLoteRps.Executar(xmlPath, config)

    retornoRecepcionarLoteRps = servicoRecepcionarLoteRps.RetornoWSString
    Exit Sub

TrataErro:
	' Chamada do arquivo Utility.bas
    Utility.TrapException
End Sub

Public Sub CancelarNfse() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 24 ' Servico.NFSeCancelarNfse
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoCancelarNfse: set servicoCancelarNfse = CreateObject("Unimake.Business.DFe.Servicos.NFSe.CancelarNfse")
    Call servicoCancelarNfse.Executar(xmlPath, config)

    retornoCancelarNfse = servicoCancelarNfse.RetornoWSString
    Exit Sub

TrataErro:
    Utility.TrapException
End Sub

Public Sub ConsultarLoteRps() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 31 ' Servico.NFSeConsultarLoteRps
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoConsultarLoteRps: set servicoConsultarLoteRps = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarLoteRps")
    Call servicoConsultarLoteRps.Executar(xmlPath, config)

    retornoConsultarLoteRps = servicoConsultarLoteRps.RetornoWSString
    Exit Sub

TrataErro:
    Utility.TrapException
End Sub

Public Sub ConsultarNfsePorRps() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 36 ' Servico.NFSeConsultarNfsePorRps
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoConsultarNfsePorRps: set servicoConsultarNfsePorRps = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfsePorRps")
    Call servicoConsultarNfsePorRps.Executar(xmlPath, config)

    retornoConsultarNfsePorRps = servicoConsultarNfsePorRps.RetornoWSString
    Exit Sub

TrataErro:
    Utility.TrapException
End Sub

Public Sub ConsultarNfseServicoPrestado() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 33 ' Servico.NFSeConsultarNfseServicoPrestado
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoConsultarNfseServicoPrestado: set servicoConsultarNfseServicoPrestado = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfseServicoPrestado")
    Call servicoConsultarNfseServicoPrestado.Executar(xmlPath, config)

    retornoConsultarNfseServicoPrestado = servicoConsultarNfseServicoPrestado.RetornoWSString
    Exit Sub

TrataErro:
    Utility.TrapException
End Sub

Public Sub ConsultarNfseServicoTomado() As String
    On Error GoTo TrataErro

    Dim xmlPath As String
    xmlPath = "caminho_seu_xml"

    ' Instancia objeto de configuração
    Dim config: set config = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    config.TipoDFe = 5 ' TipoDFe.NFSe
	' Você pode utilizar outras formas de carregar o certificado
	' Se for utilizar essas outras formas, você vai informar na propriedade abaixo:
	' config.CertificadoDigital = certificado_selecionado_outra_forma
    config.CertificadoArquivo = "caminho_certificado"
	config.CertificadoSenha = "senha_certificado"
    config.Servico = 33 ' Servico.NFSeConsultarNfseServicoTomado
    config.SchemaVersao = "2.04"
    config.TipoAmbiente = 1 ' TipoAmbiente.Producao
    config.CodigoMunicipio = 3536505

    ' Instancia o serviço e executa
    Dim servicoConsultarNfseServicoTomado: set servicoConsultarNfseServicoTomado = CreateObject("Unimake.Business.DFe.Servicos.NFSe.ConsultarNfseServicoTomado")
    Call servicoConsultarNfseServicoTomado.Executar(xmlPath, config)

    retornoConsultarNfseServicoTomado = servicoConsultarNfseServicoTomado.RetornoWSString
    Exit Sub

TrataErro:
    Utility.TrapException
End Sub