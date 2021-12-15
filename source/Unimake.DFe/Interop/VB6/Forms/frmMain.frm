VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMain 
   Caption         =   "Unimake.DFe Interop Tests"
   ClientHeight    =   11850
   ClientLeft      =   60
   ClientTop       =   705
   ClientWidth     =   11580
   LinkTopic       =   "Form1"
   ScaleHeight     =   11850
   ScaleWidth      =   11580
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog OpenFileDialog 
      Left            =   840
      Top             =   10215
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.TextBox txtLog 
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Courier New"
         Size            =   12
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2895
      Left            =   3720
      MultiLine       =   -1  'True
      TabIndex        =   0
      Top             =   5760
      Width           =   3135
   End
   Begin VB.Label lblLog 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "LOG"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   3480
      TabIndex        =   1
      Top             =   0
      Width           =   4335
   End
   Begin VB.Line ln 
      BorderWidth     =   2
      X1              =   0
      X2              =   11535
      Y1              =   240
      Y2              =   255
   End
   Begin VB.Menu mnuNFe 
      Caption         =   "NF-e"
      Begin VB.Menu mnuNFe_Consultas 
         Caption         =   "Consultas"
         Begin VB.Menu mnuNFe_ConsultarSituacao 
            Caption         =   "Consultar Situação"
         End
         Begin VB.Menu mnuNFe_ConsultarStatus 
            Caption         =   "Consultar Status"
         End
         Begin VB.Menu mnuNFe_ConsultarRecibo 
            Caption         =   "Consultar Recibo"
         End
         Begin VB.Menu mnuNFe_ConsultarCadastro 
            Caption         =   "Consultar Cadastro"
         End
         Begin VB.Menu mnuNFe_Distribuicao 
            Caption         =   "Consultar Distribuição"
         End
      End
      Begin VB.Menu mnuNFe_Eventos 
         Caption         =   "Eventos"
         Begin VB.Menu mnuNFe_Manifestar 
            Caption         =   "Manifestação"
         End
         Begin VB.Menu mnuNFe_EvtCCe 
            Caption         =   "Evento de Carta de Correção"
         End
      End
      Begin VB.Menu mnuNFe_Autorizar 
         Caption         =   "Autorizar NF-e"
      End
      Begin VB.Menu mnuNFe_Cancelar 
         Caption         =   "Cancelar NFe"
      End
      Begin VB.Menu mnuNFe_Inutilizar 
         Caption         =   "Inutilizar Números"
      End
      Begin VB.Menu mnuNFe_AutorizarPorArquivo 
         Caption         =   "Autorizar por arquivo"
      End
      Begin VB.Menu mnuNFeValidarXML 
         Caption         =   "Validar XML"
      End
   End
   Begin VB.Menu mnuNFCe 
      Caption         =   "NFC-e"
      Begin VB.Menu mnuNFCe_Consultas 
         Caption         =   "Consultas"
         Begin VB.Menu mnuNFCe_ConsultarRecibo 
            Caption         =   "Consultar Recibo"
         End
         Begin VB.Menu mnuNFCe_ConsultarStatus 
            Caption         =   "Consultar Status"
         End
         Begin VB.Menu mnuNFCe_ConsultarSituacao 
            Caption         =   "Consultar Situação"
         End
         Begin VB.Menu mnuNFCe_ConsultarContribuinte 
            Caption         =   "Consultar Contribuinte"
         End
      End
      Begin VB.Menu mnuNFCe_ 
         Caption         =   "Eventos"
         Begin VB.Menu mnuNFCe_CCe 
            Caption         =   "Carta de Correção"
         End
      End
      Begin VB.Menu mnuNFCe_Autorizar 
         Caption         =   "Autorizar"
      End
      Begin VB.Menu mnuNFCe_Cancelamento 
         Caption         =   "Cancelamento"
      End
      Begin VB.Menu mnuNFCe_Inutilizar 
         Caption         =   "Inutilizar Números"
      End
   End
   Begin VB.Menu mnuCTe 
      Caption         =   "CT-e"
      Begin VB.Menu mnuCTe_Consultas 
         Caption         =   "Consultas"
         Begin VB.Menu mnuCTe_ConsultarSituacao 
            Caption         =   "Consultar Situação"
         End
         Begin VB.Menu mnuCTe_ConsultarStatus 
            Caption         =   "Consultar Status"
         End
         Begin VB.Menu mnuCTe_ConsultaCadastroContribuinte 
            Caption         =   "Cadastro de Contribuinte"
         End
         Begin VB.Menu mnuCTe_Distribuicao 
            Caption         =   "Distribuição"
         End
      End
      Begin VB.Menu mnuCTe_Inutilizar 
         Caption         =   "Inutilizar"
      End
   End
   Begin VB.Menu mnuMDFe 
      Caption         =   "MDF-e"
      Begin VB.Menu mnuMDFe_Consultas 
         Caption         =   "Consultas"
         Begin VB.Menu mnuMDFe_ConsultarSituacao 
            Caption         =   "Consultar Situação"
         End
         Begin VB.Menu mnuMDFe_ConsultarStatus 
            Caption         =   "Consultar Status"
         End
      End
      Begin VB.Menu mnuMDFeEvento 
         Caption         =   "Eventos"
         Begin VB.Menu mnuMDFeEventoInclusaoCondutor 
            Caption         =   "Evento de Inclusão de Condutor no MDFe"
         End
         Begin VB.Menu mnuMDFeEventoInclusaoDFe 
            Caption         =   "Evento de Inclusão de DFe no MDFe"
         End
         Begin VB.Menu mnuMDFeEventoEncerramento 
            Caption         =   "Evento de Encerramento do MDF-e"
         End
      End
      Begin VB.Menu mnuMDFe_EmitirUm 
         Caption         =   "Emitir um MDF-e"
      End
   End
   Begin VB.Menu mnuCertificado 
      Caption         =   "Certificado"
      Begin VB.Menu mnuCertificadoSelecionar 
         Caption         =   "Selecionar"
      End
      Begin VB.Menu mnuCertificadoSelecionarArquivo 
         Caption         =   "Selecionar de Arquivo"
      End
      Begin VB.Menu mnuCertificadoSelecionarBase64 
         Caption         =   "Selecionar de Base64"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Resize()
lblLog.Left = 1
txtLog.Left = 100
lblLog.Width = Width
ln.X2 = Width
txtLog.Width = Width - 300
txtLog.Top = ln.Y1 + 50
txtLog.Height = Height - txtLog.Top
End Sub

Public Sub EscreveLog(ByVal Log As String)
txtLog.Text = txtLog.Text & vbCrLf & " <<< " & Now & " >>>" & vbCrLf & Log
End Sub

Public Sub ClearLog()
txtLog.Text = ""
End Sub



Private Sub mnuCertificadoSelecionar_Click()
Dim certificado
Set certificado = SelecionarCertificado.SelecionarCertificado
MsgBox "Certificado selecionado", vbInformation + vbOKOnly, "Aviso"
End Sub

Private Sub mnuCertificadoSelecionarArquivo_Click()
On Error GoTo erro
Dim certificado
Set certificado = SelecionarCertificado.SelecionarCertificadoDeArquivo
MsgBox "Certificado selecionado", vbInformation + vbOKOnly, "Aviso"
Exit Sub
erro:
MsgBox Err.Description, vbCritical + vbOKOnly, "Erro ao selecionar o certificado"
End Sub

Private Sub mnuCertificadoSelecionarBase64_Click()
SelecionarCertificado.SelecionarDeBase64
End Sub

Private Sub mnuCTe_ConsultaCadastroContribuinte_Click()
ConsultarContribuinteCTe
End Sub

Private Sub mnuCTe_ConsultarSituacao_Click()
ConsultarSituacaoCTe
End Sub

Private Sub mnuCTe_ConsultarStatus_Click()
ConsultarStatusCTe
End Sub

Private Sub mnuCTe_Distribuicao_Click()
ConsultarDistribuicaoCTe
End Sub

Private Sub mnuCTe_Inutilizar_Click()
InutilizarNumeroCTe
End Sub

Private Sub mnuMDFe_ConsultarSituacao_Click()
ConsultarSituacaoMDFe
End Sub

Private Sub mnuMDFe_ConsultarStatus_Click()
ConsultarStatusMDFe
End Sub

Private Sub mnuMDFe_EmitirUm_Click()
EmitirUmMDFe
End Sub

Private Sub mnuMDFeEventoEncerramento_Click()
EncerramentoMDFe
End Sub

Private Sub mnuMDFeEventoInclusaoCondutor_Click()
InclusaoCondutorMDFe
End Sub

Private Sub mnuMDFeEventoInclusaoDFe_Click()
InclusaoDFeMDFe
End Sub

Private Sub mnuNFCe_Autorizar_Click()
AutorizarNFCe
End Sub

Private Sub mnuNFCe_Cancelamento_Click()
CancelarNFCe
End Sub

Private Sub mnuNFCe_CCe_Click()
EnviarEventoCCENFe
End Sub

Private Sub mnuNFCe_ConsultarContribuinte_Click()
ConsultarContribuinteNFCe
End Sub

Private Sub mnuNFCe_ConsultarRecibo_Click()
ConsultarReciboNFCe
End Sub

Private Sub mnuNFCe_ConsultarSituacao_Click()
ConsultarSituacaoNFe
End Sub

Private Sub mnuNFCe_ConsultarStatus_Click()
ConsultarStatusNFCe
End Sub

Private Sub mnuNFCe_Inutilizar_Click()
InutilizarNumeroNFCe
End Sub

Private Sub mnuNFe_Autorizar_Click()
AutorizarNFe
End Sub

Private Sub mnuNFe_AutorizarPorArquivo_Click()
AutorizarPorArquivoNFe
End Sub

Private Sub mnuNFe_Cancelar_Click()
CancelarNFe
End Sub

Private Sub mnuNFe_ConsultarCadastro_Click()
ConsultarContribuinteNFe
End Sub

Private Sub mnuNFe_ConsultarRecibo_Click()
ConsultarReciboNFe
End Sub

Private Sub mnuNFe_ConsultarSituacao_Click()
ConsultarSituacaoNFe
End Sub

Private Sub mnuNFe_ConsultarStatus_Click()
ConsultarStatus
End Sub

Private Sub mnuNFe_Distribuicao_Click()
ConsultarDistribuicaoNFe
End Sub

Private Sub mnuNFe_EvtCCe_Click()
EnviarEventoCCENFe
End Sub

Private Sub mnuNFe_Inutilizar_Click()
InutilizarNumeroNFe
End Sub

Private Sub mnuNFe_Manifestar_Click()
ManifestarNFe
End Sub

Private Sub mnuNFeValidarXML_Click()
ValidarXML
End Sub
