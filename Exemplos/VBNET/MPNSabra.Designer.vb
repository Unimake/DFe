<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MPNSabra
    Inherits System.Windows.Forms.Form

    'Descartar substituições de formulário para limpar a lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Exigido pelo Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'OBSERVAÇÃO: o procedimento a seguir é exigido pelo Windows Form Designer
    'Pode ser modificado usando o Windows Form Designer.  
    'Não o modifique usando o editor de códigos.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.TLocalPFX = New System.Windows.Forms.TextBox()
        Me.TSenhaPFX = New System.Windows.Forms.TextBox()
        Me.Inutilização = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.NumInicial = New System.Windows.Forms.TextBox()
        Me.NumFinal = New System.Windows.Forms.TextBox()
        Me.ChaveNF = New System.Windows.Forms.TextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.PorChave = New System.Windows.Forms.RadioButton()
        Me.PorNumero = New System.Windows.Forms.RadioButton()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button6 = New System.Windows.Forms.Button()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(341, 467)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 40)
        Me.Button1.TabIndex = 0
        Me.Button1.Text = "Encerrar"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'CheckBox1
        '
        Me.CheckBox1.AutoSize = True
        Me.CheckBox1.Checked = True
        Me.CheckBox1.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox1.Location = New System.Drawing.Point(183, 103)
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.Size = New System.Drawing.Size(122, 17)
        Me.CheckBox1.TabIndex = 1
        Me.CheckBox1.Text = "Certificado Instalado"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Location = New System.Drawing.Point(31, 379)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(75, 58)
        Me.Button2.TabIndex = 4
        Me.Button2.Text = "Estado do Serviço"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 148)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(153, 13)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Local e Nome do Arquivo .PFX"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 176)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(166, 13)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = "Senha do Certificado (PFX ou A3)"
        '
        'TLocalPFX
        '
        Me.TLocalPFX.Location = New System.Drawing.Point(183, 145)
        Me.TLocalPFX.Name = "TLocalPFX"
        Me.TLocalPFX.Size = New System.Drawing.Size(131, 20)
        Me.TLocalPFX.TabIndex = 7
        Me.TLocalPFX.Text = "d:\windowsdll\masimo.pfx"
        '
        'TSenhaPFX
        '
        Me.TSenhaPFX.Location = New System.Drawing.Point(183, 171)
        Me.TSenhaPFX.Name = "TSenhaPFX"
        Me.TSenhaPFX.Size = New System.Drawing.Size(131, 20)
        Me.TSenhaPFX.TabIndex = 8
        Me.TSenhaPFX.Text = "9999999999"
        '
        'Inutilização
        '
        Me.Inutilização.Location = New System.Drawing.Point(160, 380)
        Me.Inutilização.Name = "Inutilização"
        Me.Inutilização.Size = New System.Drawing.Size(75, 57)
        Me.Inutilização.TabIndex = 9
        Me.Inutilização.Text = "Inutilização"
        Me.Inutilização.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(12, 244)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(74, 13)
        Me.Label3.TabIndex = 10
        Me.Label3.Text = "Número Inicial"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(12, 277)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(69, 13)
        Me.Label4.TabIndex = 11
        Me.Label4.Text = "Número Final"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Location = New System.Drawing.Point(16, 312)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(70, 13)
        Me.Label5.TabIndex = 12
        Me.Label5.Text = "Chave da NF"
        '
        'NumInicial
        '
        Me.NumInicial.Location = New System.Drawing.Point(183, 237)
        Me.NumInicial.Name = "NumInicial"
        Me.NumInicial.Size = New System.Drawing.Size(131, 20)
        Me.NumInicial.TabIndex = 13
        Me.NumInicial.Text = "530"
        '
        'NumFinal
        '
        Me.NumFinal.Location = New System.Drawing.Point(183, 270)
        Me.NumFinal.Name = "NumFinal"
        Me.NumFinal.Size = New System.Drawing.Size(131, 20)
        Me.NumFinal.TabIndex = 14
        Me.NumFinal.Text = "532"
        '
        'ChaveNF
        '
        Me.ChaveNF.Location = New System.Drawing.Point(183, 305)
        Me.ChaveNF.Name = "ChaveNF"
        Me.ChaveNF.Size = New System.Drawing.Size(282, 20)
        Me.ChaveNF.TabIndex = 15
        Me.ChaveNF.Text = "sabra-nfe.xml"
        '
        'GroupBox1
        '
        Me.GroupBox1.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.GroupBox1.Controls.Add(Me.PorChave)
        Me.GroupBox1.Controls.Add(Me.PorNumero)
        Me.GroupBox1.Location = New System.Drawing.Point(160, 331)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(111, 49)
        Me.GroupBox1.TabIndex = 19
        Me.GroupBox1.TabStop = False
        '
        'PorChave
        '
        Me.PorChave.AutoSize = True
        Me.PorChave.Checked = True
        Me.PorChave.Location = New System.Drawing.Point(17, 32)
        Me.PorChave.Name = "PorChave"
        Me.PorChave.Size = New System.Drawing.Size(75, 17)
        Me.PorChave.TabIndex = 1
        Me.PorChave.TabStop = True
        Me.PorChave.Text = "Por Chave"
        Me.PorChave.UseVisualStyleBackColor = True
        '
        'PorNumero
        '
        Me.PorNumero.AutoSize = True
        Me.PorNumero.Location = New System.Drawing.Point(17, 9)
        Me.PorNumero.Name = "PorNumero"
        Me.PorNumero.Size = New System.Drawing.Size(81, 17)
        Me.PorNumero.TabIndex = 0
        Me.PorNumero.Text = "Por Número"
        Me.PorNumero.UseVisualStyleBackColor = True
        '
        'Button3
        '
        Me.Button3.Location = New System.Drawing.Point(297, 380)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(95, 57)
        Me.Button3.TabIndex = 20
        Me.Button3.Text = "Consultar Situação NF"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button4
        '
        Me.Button4.Location = New System.Drawing.Point(410, 382)
        Me.Button4.Name = "Button4"
        Me.Button4.Size = New System.Drawing.Size(75, 55)
        Me.Button4.TabIndex = 21
        Me.Button4.Text = "Enviar Nf"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'Button5
        '
        Me.Button5.Location = New System.Drawing.Point(509, 382)
        Me.Button5.Name = "Button5"
        Me.Button5.Size = New System.Drawing.Size(52, 55)
        Me.Button5.TabIndex = 22
        Me.Button5.Text = "Enviar XML"
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Button6
        '
        Me.Button6.Location = New System.Drawing.Point(577, 380)
        Me.Button6.Name = "Button6"
        Me.Button6.Size = New System.Drawing.Size(62, 57)
        Me.Button6.TabIndex = 23
        Me.Button6.Text = "Enviar Evento CCe"
        Me.Button6.UseVisualStyleBackColor = True
        '
        'MPNSabra
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(800, 533)
        Me.Controls.Add(Me.Button6)
        Me.Controls.Add(Me.Button5)
        Me.Controls.Add(Me.Button4)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.ChaveNF)
        Me.Controls.Add(Me.NumFinal)
        Me.Controls.Add(Me.NumInicial)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Inutilização)
        Me.Controls.Add(Me.TSenhaPFX)
        Me.Controls.Add(Me.TLocalPFX)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.CheckBox1)
        Me.Controls.Add(Me.Button1)
        Me.Name = "MPNSabra"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Exemplos de Uso da DLL Unimake      "
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Button1 As Button
    Friend WithEvents CheckBox1 As CheckBox
    Friend WithEvents Button2 As Button
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents TLocalPFX As TextBox
    Friend WithEvents TSenhaPFX As TextBox
    Friend WithEvents Inutilização As Button
    Friend WithEvents Label3 As Label
    Friend WithEvents Label4 As Label
    Friend WithEvents Label5 As Label
    Friend WithEvents NumInicial As TextBox
    Friend WithEvents NumFinal As TextBox
    Friend WithEvents ChaveNF As TextBox
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents PorChave As RadioButton
    Friend WithEvents PorNumero As RadioButton
    Friend WithEvents Button3 As Button
    Friend WithEvents Button4 As Button
    Friend WithEvents Button5 As Button
    Friend WithEvents Button6 As Button
End Class
