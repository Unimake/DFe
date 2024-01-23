Attribute VB_Name = "ServicosMDFe"
Option Explicit

Public Sub EncerramentoMDFe()
On Error GoTo erro
Dim EventoMDFe: Set EventoMDFe = CreateObject("Unimake.Business.DFe.xml.MDFe.EventoMDFe")
Dim InfEvento: Set InfEvento = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfEvento")
Dim DetEventoEncMDFe: Set DetEventoEncMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.DetEventoEncMDFe")
Dim InfDoc: Set InfDoc = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfDoc")
Dim RecepcaoEvento: Set RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento")
Dim Configuracao: Set Configuracao = Config.InicializarConfiguracao(TipoDFe.MDFe)

With DetEventoEncMDFe
    .NProt = "141200000007987"
    .VersaoEvento = "3.00"
    .CMun = 4118402
    .CUF = UFBrasil.PR
    .DtEnc = Now
End With

With InfEvento
    Set .DetEvento = DetEventoEncMDFe
    .COrgao = UFBrasil.PR
    .ChMDFe = "41200210859283000185570010000005671227070615"
    .CNPJ = "10859283000185"
    .DhEvento = Now
    .TpEvento = TipoEventoMDFe.Encerramento
    .NSeqEvento = 1
    .TpAmb = TpAmb
End With

With EventoMDFe
    .Versao = "3.00"
    Set .InfEvento = InfEvento
End With

RecepcaoEvento.Executar (EventoMDFe), (Configuracao)
                
Log.EscreveLog RecepcaoEvento.RetornoWSString, True
Log.EscreveLog RecepcaoEvento.result.InfEvento.XMotivo, False
              
'Gravar o XML de distribuição se a inutilização foi homologada
Select Case RecepcaoEvento.result.InfEvento.CStat
    Case 134 'Recebido pelo Sistema de Registro de Eventos
    Case 135 'Recebido pelo Sistema de Registro de Eventos
    Case 136 'Recebido pelo Sistema de Registro de Eventos
        RecepcaoEvento.GravarXmlDistribuicao "D:\Temp"
        MsgBox "Evento gravado na Sefaz", vbOKOnly + vbInformation
    Case Else
        MsgBox "Evento rejeitado pela Sefaz", vbOKOnly + vbCritical
End Select

Exit Sub
erro:
Utility.TrapException

End Sub

Public Sub InclusaoDFeMDFe()
On Error GoTo erro
Dim EventoMDFe: Set EventoMDFe = CreateObject("Unimake.Business.DFe.xml.MDFe.EventoMDFe")
Dim InfEvento: Set InfEvento = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfEvento")
Dim DetEventoIncDFeMDFe: Set DetEventoIncDFeMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.DetEventoIncDFeMDFe")
Dim InfDoc: Set InfDoc = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfDoc")
Dim EventoIncDFeMDFe: Set EventoIncDFeMDFe = CreateObject("Unimake.Business.DFe.xml.MDFe.EventoIncDFeMDFe")
Dim RecepcaoEvento: Set RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento")
Dim Configuracao: Set Configuracao = Config.InicializarConfiguracao(TipoDFe.MDFe)

With EventoIncDFeMDFe
    .DescEvento = "Inclusao DF-e"
    .NProt = "941190000014312"
    .CMunCarrega = "4118402"
    .XMunCarrega = "PARANAVAI"
End With

EventoIncDFeMDFe.AddInfDoc (InfDoc)

With DetEventoIncDFeMDFe
    .VersaoEvento = "3.00"
    Set .EventoIncDFeMDFe = EventoIncDFeMDFe
End With

With InfEvento
    Set .DetEvento = DetEventoIncDFeMDFe
    .COrgao = UFBrasil.PR
    .ChMDFe = "41200380568835000181580010000007171930099252"
    .CNPJ = "80568835000181"
    .DhEvento = DateTime.Now
    .TpEvento = TipoEventoMDFe.InclusaoDFe
    .NSeqEvento = 1
    .TpAmb = TpAmb
End With

With EventoMDFe
    .Versao = "3.00"
    Set .InfEvento = InfEvento
End With

RecepcaoEvento.Executar (EventoMDFe), (Configuracao)
                
Log.EscreveLog RecepcaoEvento.RetornoWSString, True
Log.EscreveLog RecepcaoEvento.result.InfEvento.XMotivo, False
                
              
'Gravar o XML de distribuição se a inutilização foi homologada
Select Case RecepcaoEvento.result.InfEvento.CStat
    Case 134 'Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
    Case 135 'Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
    Case 136 'Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
        RecepcaoEvento.GravarXmlDistribuicao "D:\Temp"
        MsgBox "Evento gravado na Sefaz", vbOKOnly + vbInformation
    Case Else
        MsgBox "Evento rejeitado pela Sefaz", vbOKOnly + vbCritical
End Select

Exit Sub
erro:
Utility.TrapException

End Sub

Public Sub InclusaoCondutorMDFe()
On Error GoTo erro
Dim EventoMDFe: Set EventoMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.EventoMDFe")
Dim InfEvento: Set InfEvento = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfEvento")
Dim DetEventoIncCondutor: Set DetEventoIncCondutor = CreateObject("Unimake.Business.DFe.Xml.MDFe.DetEventoIncCondutor")
Dim EventoIncCondutor: Set EventoIncCondutor = CreateObject("Unimake.Business.DFe.xml.MDFe.EventoIncCondutor")
Dim CondutorMDFe: Set CondutorMDFe = CreateObject("Unimake.Business.DFe.xml.MDFe.CondutorMDFe")
Dim RecepcaoEvento: Set RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RecepcaoEvento")
Dim configExec: Set configExec = Config.InicializarConfiguracao(TipoDFe.MDFe)

CondutorMDFe.XNome = "JOSE ALMEIDA"
CondutorMDFe.CPF = "00000000191"

EventoIncCondutor.AddCondutor (CondutorMDFe)

With DetEventoIncCondutor
    .VersaoEvento = "3.00"
    .DescEvento = "Inclusao Condutor"
    Set .EventoIncCondutor = EventoIncCondutor
End With

With InfEvento
    .COrgao = UFBrasil.PR
    .ChMDFe = "41200380568835000181580010000007171930099252"
    .CNPJ = "80568835000181"
    .DhEvento = DateTime.Now
    .TpEvento = TipoEventoMDFe.InclusaoCondutor
    .NSeqEvento = 1
    .TpAmb = TpAmb
    Set .DetEvento = DetEventoIncCondutor
End With

EventoMDFe.Versao = "3.00"
Set EventoMDFe.InfEvento = InfEvento

RecepcaoEvento.Executar (EventoMDFe), (configExec)

Log.EscreveLog RecepcaoEvento.RetornoWSString, True
Log.EscreveLog RecepcaoEvento.result.InfEvento.XMotivo, False
                
'Gravar o XML de distribuição se a inutilização foi homologada
Select Case RecepcaoEvento.result.InfEvento.CStat

    Case 134 'Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respectivo CT-e com situação diferente de Autorizada.
    Case 135 'Recebido pelo Sistema de Registro de Eventos, com vinculação do evento no respetivo CTe.
    Case 136 'Recebido pelo Sistema de Registro de Eventos – vinculação do evento ao respectivo CT-e prejudicado.
        RecepcaoEvento.GravarXmlDistribuicao "D:\Temp"
        MsgBox "Evento gravado na Sefaz", vbOKOnly + vbInformation
    Case Else
        MsgBox "Evento rejeitado pela Sefaz", vbOKOnly + vbCritical
End Select

Exit Sub
erro:
Utility.TrapException

End Sub


Public Sub ConsultarSituacaoMDFe()
On Error GoTo erro
Dim ConsSitMDFe
Dim consultaProtocolo

Log.ClearLog

Set ConsSitMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe")
ConsSitMDFe.Versao = "3.00"
ConsSitMDFe.TpAmb = TpAmb
ConsSitMDFe.ChMDFe = UFBrasil.PR & "170701761135000132570010000186931903758906"

Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.MDFe.ConsultaProtocolo")
consultaProtocolo.Executar (ConsSitMDFe), (Config.InicializarConfiguracao(TipoDFe.MDFe))

Log.EscreveLog consultaProtocolo.RetornoWSString, True
Log.EscreveLog consultaProtocolo.result.XMotivo, True

Exit Sub
erro:
Utility.TrapException

End Sub

Public Sub ConsultarStatusMDFe()
On Error GoTo erro
Dim ConsStatServMDFe
Dim statusServico

Log.ClearLog

Set ConsStatServMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsStatServMDFe")
Set statusServico = CreateObject("Unimake.Business.DFe.Servicos.MDFe.StatusServico")

ConsStatServMDFe.Versao = "3.00"
ConsStatServMDFe.TpAmb = TpAmb
statusServico.Executar (ConsStatServMDFe), (Config.InicializarConfiguracao(TipoDFe.MDFe, UFBrasil.PR))

Log.EscreveLog statusServico.RetornoWSString, True
Log.EscreveLog statusServico.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException
End Sub

Public Sub EmitirUmMDFe()
On Error GoTo erro
Dim xml: Set xml = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnviMDFe")
Dim MDFe: Set MDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
Dim InfMDFe: Set InfMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMDFe")
Dim Ide: Set Ide = CreateObject("Unimake.Business.DFe.Xml.MDFe.Ide")
Dim InfMunCarrega: Set InfMunCarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunCarrega")
Dim Emit: Set Emit = CreateObject("Unimake.Business.DFe.Xml.MDFe.Emit")
Dim EnderEmit: Set EnderEmit = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnderEmit")
Dim InfModal: Set InfModal = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfModal")
Dim Rodo: Set Rodo = CreateObject("Unimake.Business.DFe.Xml.MDFe.Rodo")
Dim InfANTT: Set InfANTT = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfANTT")
Dim InfContratante: Set InfContratante = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfContratante")
Dim VeicTracao: Set VeicTracao = CreateObject("Unimake.Business.DFe.Xml.MDFe.VeicTracao")
Dim Prop: Set Prop = CreateObject("Unimake.Business.DFe.Xml.MDFe.Prop")
Dim Condutor: Set Condutor = CreateObject("Unimake.Business.DFe.Xml.MDFe.Condutor")
Dim InfMunDescarga: Set InfMunDescarga = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescarga")
Dim InfMunDescargaInfCTe: Set InfMunDescargaInfCTe = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe")
Dim InfDoc: Set InfDoc = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfDocInfMDFe")
Dim ProdPred: Set ProdPred = CreateObject("Unimake.Business.DFe.Xml.MDFe.ProdPred")
Dim InfLotacao: Set InfLotacao = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLotacao")
Dim InfLocalCarrega: Set InfLocalCarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLocalCarrega")
Dim InfLocalDescarrega: Set InfLocalDescarrega = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfLocalDescarrega")
Dim Seg: Set Seg = CreateObject("Unimake.Business.DFe.Xml.MDFe.Seg")
Dim InfResp: Set InfResp = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfResp")
Dim InfSeg: Set InfSeg = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfSeg")
Dim Tot: Set Tot = CreateObject("Unimake.Business.DFe.Xml.MDFe.Tot")
Dim InfAdic: Set InfAdic = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfAdic")
Dim InfRespTec: Set InfRespTec = CreateObject("Unimake.Business.DFe.Xml.MDFe.InfRespTec")
Dim AutorizacaoMDFe: Set AutorizacaoMDFe = CreateObject("Unimake.Business.DFe.Servicos.MDFe.Autorizacao")
Dim xmlRec: Set xmlRec = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsReciMDFe")
Dim retAutorizacao: Set retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.RetAutorizacao")
Dim xmlSit: Set xmlSit = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsSitMDFe")
Dim configSit: Set configSit = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
Dim consultaProtocolo: Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.MDFe.ConsultaProtocolo")
Dim configRec: Set configRec = Config.InicializarConfiguracao(TipoDFe.MDFe)

Log.ClearLog

With InfMunCarrega
    .CMunCarrega = 4118402
    .XMunCarrega = "PARANAVAI"
End With

With Ide
    .CUF = UFBrasil.PR
    .TpAmb = TpAmb
    .TpEmit = TipoEmitenteMDFe.PrestadorServicoTransporte
    .Mod = ModeloDFe.MDFe
    .Serie = 1
    .NMDF = 861
    .CMDF = "01722067"
    .Modal = ModalidadeTransporteMDFe.Rodoviario
    .DhEmi = Date
    .TpEmis = TipoEmissao.Normal
    .ProcEmi = ProcessoEmissao.AplicativoContribuinte
    .VerProc = "DLL Unidanfe 1.0"
    .UFIni = UFBrasil.PR
    .UFFim = UFBrasil.SP
    .DhIniViagem = Date
End With

With EnderEmit
    .XLgr = "RUA JOAQUIM F. DE SOUZA"
    .Nro = "01112"
    .XBairro = "VILA TEREZINHA"
    .CMun = 4118402
    .XMun = "PARANAVAI"
    .CEP = "87706675"
    .UF = UFBrasil.PR
    .Fone = "04434237530"
End With

With Emit
    .CNPJ = "31905001000109"
    .IE = "9079649730"
    .XNome = "EXATUS MOVEIS EIRELI"
    .XFant = "EXATUS MOVEIS"
    Set .EnderEmit = EnderEmit
End With
           
Ide.AddInfMunCarrega (InfMunCarrega)

With InfANTT
    .RNTRC = "44957333"
End With

InfContratante.CNPJ = "80568835000181"
InfANTT.AddInfContratante (InfContratante)

With Prop
    .CNPJ = "31905001000109"
    .RNTRC = "44957333"
    .XNome = "EXATUS MOVEIS EIRELI"
    .IE = "9079649730"
    .UF = UFBrasil.PR
    .TpProp = TipoProprietarioMDFe.Outros
End With
    
With Condutor
    .XNome = "ADEMILSON LOPES DE SOUZA"
    .CPF = "27056461832"
End With

With VeicTracao
    .CInt = "AXF8500"
    .Placa = "AXF8500"
    .Tara = 0
    .CapKG = 5000
    Set .Prop = Prop
    .TpRod = TipoRodado.Toco
    .TpCar = TipoCarroceriaMDFe.FechadaBau
    .UF = UFBrasil.PR
End With

VeicTracao.AddCondutor (Condutor)

With Rodo
    Set .InfANTT = InfANTT
    Set .VeicTracao = VeicTracao
End With

With InfModal
    .VersaoModal = "3.00"
    Set .Rodo = Rodo
End With

With InfMunDescargaInfCTe
    .ChCTe = "41200531905001000109570010000009551708222466"
End With

With InfMunDescarga
    .CMunDescarga = 3505708
    .XMunDescarga = "BARUERI"
End With

InfMunDescarga.AddInfCTe (InfMunDescargaInfCTe)

InfDoc.AddInfMunDescarga (InfMunDescarga)
InfLocalCarrega.CEP = "87302080"
InfLocalDescarrega.CEP = "25650208"

Set InfLotacao.InfLocalCarrega = InfLocalCarrega
Set InfLotacao.InfLocalDescarrega = InfLocalDescarrega

With ProdPred
    .TpCarga = TipoCargaMDFe.CargaGeral
    .XProd = "TESTE DE PRODUTO PREDOMINANTE"
    Set .InfLotacao = InfLotacao
End With

InfResp.RespSeg = ResponsavelSeguroMDFe.EmitenteMDFe
InfResp.CNPJ = "31905001000109"

InfSeg.XSeg = "PORTO SEGURO"
InfSeg.CNPJ = "61198164000160"

Set Seg.InfResp = InfResp
Set Seg.InfSeg = InfSeg
Seg.NApol = "053179456362"
Seg.AddNAver "0000000000000000000000000000000000000000"
Seg.AddNAver "0000000000000000000000000000000000000001"

With Tot
    .QCTe = 3
    .VCarga = 56599.09
    .CUnid = CodigoUnidadeMedidaMDFe.KG
    .QCarga = 2879
End With

InfAdic.InfCpl = "DATA/HORA PREVISTA PARA O INICO DA VIAGEM: 10/08/2020 as 08:00"

With InfRespTec
    .CNPJ = "06117473000150"
    .XContato = "Wandrey Mundin Ferreira"
    .Email = "wandrey@unimake.com.br"
    .Fone = "04431414900"
End With

With InfMDFe
    .Versao = "3.00"
    Set .Ide = Ide
    Set .Emit = Emit
    Set .InfModal = InfModal
    Set .InfDoc = InfDoc
    Set .ProdPred = ProdPred
    Set .Tot = Tot
    Set .InfAdic = InfAdic
    Set .InfRespTec = InfRespTec
End With

InfMDFe.AddSeg (Seg)

Set MDFe.InfMDFe = InfMDFe

With xml
    .Versao = "3.00"
    .IdLote = "000000000000001"
    Set .MDFe = MDFe
End With

AutorizacaoMDFe.Executar (xml), (configRec)
Log.EscreveLog AutorizacaoMDFe.RetornoWSString, True

If Not AutorizacaoMDFe.result Is Nothing Then
    MsgBox AutorizacaoMDFe.result.XMotivo
    
    If AutorizacaoMDFe.result.CStat = 103 Then '103 = Lote Recebido com Sucesso
        ' Finalizar através da consulta do recibo.
        With xmlRec
            .Versao = "3.00"
            .TpAmb = TpAmb
            .NRec = AutorizacaoMDFe.result.InfRec.NRec
        End With
        
        With configRec
            .TipoDFe = TipoDFe.MDFe
            .CertificadoDigital = configRec.CertificadoDigital
        End With
        
        retAutorizacao.Executar (xmlRec), (configRec)
        Set AutorizacaoMDFe.RetConsReciMDFe = retAutorizacao.result
        AutorizacaoMDFe.GravarXmlDistribuicao "D:\Temp\"
        
        ' Simulação da finalização do CTe através da consulta situação
        Set AutorizacaoMDFe.RetConsReciMDFe = Nothing ' Zerar para conseguir testar
        With xmlSit
            .Versao = "3.00"
            .TpAmb = TpAmb
            .ChMDFe = xml.MDFe.InfMDFe.Chave
        End With
        
        With configSit
            .TipoDFe = TipoDFe.MDFe
            .CertificadoDigital = configRec.CertificadoDigital
        End With
                        
        consultaProtocolo.Executar (xmlSit), (configSit)
        AutorizacaoMDFe.AddRetConsSitMDFe (consultaProtocolo.result)
        AutorizacaoMDFe.GravarXmlDistribuicao "D:\Temp"
    End If
End If

MsgBox "MDF-e enviado com sucesso para a sefaz.", vbOKOnly + vbInformation

Exit Sub
erro:
Utility.TrapException
End Sub
