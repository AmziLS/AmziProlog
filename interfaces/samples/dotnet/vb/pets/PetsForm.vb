Imports amzinet

Public Class PetsForm
    Inherits System.Windows.Forms.Form

#Region " Windows Form Designer generated code "

    Public Sub New()
        MyBase.New()

        'This call is required by the Windows Form Designer.
        InitializeComponent()

        'Add any initialization after the InitializeComponent() call

    End Sub

    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing Then
            If Not (components Is Nothing) Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    Friend WithEvents GetPetButton As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents SoundTextBox As System.Windows.Forms.TextBox
    Friend WithEvents PetTextBox As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.GetPetButton = New System.Windows.Forms.Button()
        Me.SoundTextBox = New System.Windows.Forms.TextBox()
        Me.PetTextBox = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'GetPetButton
        '
        Me.GetPetButton.Location = New System.Drawing.Point(16, 80)
        Me.GetPetButton.Name = "GetPetButton"
        Me.GetPetButton.Size = New System.Drawing.Size(88, 24)
        Me.GetPetButton.TabIndex = 0
        Me.GetPetButton.Text = "Get Pet"
        '
        'SoundTextBox
        '
        Me.SoundTextBox.Location = New System.Drawing.Point(40, 40)
        Me.SoundTextBox.Name = "SoundTextBox"
        Me.SoundTextBox.Size = New System.Drawing.Size(136, 20)
        Me.SoundTextBox.TabIndex = 1
        Me.SoundTextBox.Text = ""
        '
        'PetTextBox
        '
        Me.PetTextBox.Location = New System.Drawing.Point(40, 136)
        Me.PetTextBox.Name = "PetTextBox"
        Me.PetTextBox.Size = New System.Drawing.Size(136, 20)
        Me.PetTextBox.TabIndex = 2
        Me.PetTextBox.Text = ""
        '
        'Label1
        '
        Me.Label1.Location = New System.Drawing.Point(16, 8)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(176, 32)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "What sound does your pet make? (woof, quack or meow)"
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(16, 112)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(80, 24)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Your pet is a"
        '
        'PetsForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(232, 189)
        Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.Label2, Me.Label1, Me.PetTextBox, Me.SoundTextBox, Me.GetPetButton})
        Me.Name = "PetsForm"
        Me.Text = "PetsForm"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub GetPetButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GetPetButton.Click
      Dim ls As LogicServer
        Dim term As Long

        ls = New LogicServer
      Try
         ls.Init("")
         ls.Load("pets.xpl")
         ls.AssertaStr("sound(" + SoundTextBox.Text + ")")
         term = ls.ExecStr("pet(X)")
         If (term <> 0) Then
            PetTextBox.Text = ls.GetStrArg(term, 1)
         Else
            PetTextBox.Text = "!!!ERROR!!!"
         End If
         ls.Close()
      Catch ex As LSException
            If (ex.GetExceptType() = exLSTYPE.FATAL) Then
                MsgBox(ex.GetMsg(), MsgBoxStyle.Critical, "")
            Else
                MsgBox(ex.GetMsg(), MsgBoxStyle.Exclamation, "")
            End If
        End Try

    End Sub
End Class
