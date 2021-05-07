Imports amzinet

Public Class PetsCallbackForm
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
    Friend WithEvents GoButton As System.Windows.Forms.Button
    Friend WithEvents PetText As System.Windows.Forms.TextBox
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.GoButton = New System.Windows.Forms.Button()
        Me.PetText = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'GoButton
        '
        Me.GoButton.Location = New System.Drawing.Point(96, 40)
        Me.GoButton.Name = "GoButton"
        Me.GoButton.Size = New System.Drawing.Size(104, 40)
        Me.GoButton.TabIndex = 0
        Me.GoButton.Text = "Get Pet"
        '
        'PetText
        '
        Me.PetText.Location = New System.Drawing.Point(96, 96)
        Me.PetText.Name = "PetText"
        Me.PetText.Size = New System.Drawing.Size(104, 20)
        Me.PetText.TabIndex = 1
        Me.PetText.Text = ""
        '
        'PetsCallbackForm
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.ClientSize = New System.Drawing.Size(292, 273)
        Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.PetText, Me.GoButton})
        Me.Name = "PetsCallbackForm"
        Me.Text = "Pets Callback"
        Me.ResumeLayout(False)

    End Sub

#End Region

    Private Sub GoButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GoButton.Click

      Dim ls As LogicServer
        Dim term As Long
      Dim prompt_pred As PrologPredicate
      Dim pet As String

      ls = New LogicServer
      Try
         ls.Init("")
         prompt_pred = AddressOf Prompt
         ls.AddPred("prompt", 2, prompt_pred)
         ls.Load("pets.xpl")
         term = ls.ExecStr("pet(X)")
         If (term = 0) Then
            MsgBox("Sound not found", MsgBoxStyle.Critical, "ERROR")
         Else
            pet = ls.GetStrArg(term, 1)
            PetText.Text = pet
         End If
         ls.Close()
      Catch ex As LSException
         Try
                If (ex.GetExceptType() = exLSTYPE.FATAL) Then
                    MsgBox(ex.GetMsg(), MsgBoxStyle.Critical, "ERROR")
                Else
                    MsgBox(ex.GetMsg(), MsgBoxStyle.Exclamation, "ERROR")
                End If
         Catch ex2 As Exception
            MsgBox(ex.Message, MsgBoxStyle.Exclamation, "ERROR")
         End Try
         ls.Close()
      End Try

    End Sub

    Function Prompt(ByVal ls As LogicServer) As Boolean
        Dim prompt_text As String
        Dim answer As String

        prompt_text = ls.GetStrParm(1)
        answer = InputBox(prompt_text, "Pets Callback", "")
        Return ls.UnifyStrParm(2, answer)
    End Function

End Class
