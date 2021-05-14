VERSION 5.00
Begin VB.Form PersonForm 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Person Information"
   ClientHeight    =   1890
   ClientLeft      =   1590
   ClientTop       =   3120
   ClientWidth     =   7290
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   HelpContextID   =   300
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1890
   ScaleWidth      =   7290
   Begin VB.CheckBox Single 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Single"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4560
      TabIndex        =   4
      Top             =   1440
      Width           =   855
   End
   Begin VB.Frame SexFrame 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Sex"
      ForeColor       =   &H80000008&
      Height          =   975
      Left            =   4440
      TabIndex        =   5
      Top             =   360
      Width           =   1215
      Begin VB.OptionButton Male 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         Caption         =   "Male"
         ForeColor       =   &H80000008&
         Height          =   255
         Left            =   120
         TabIndex        =   13
         Top             =   600
         Width           =   975
      End
      Begin VB.OptionButton Female 
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         Caption         =   "Female"
         ForeColor       =   &H80000008&
         Height          =   195
         Left            =   120
         TabIndex        =   12
         Top             =   360
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.CommandButton Cancel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   5880
      TabIndex        =   11
      Top             =   600
      Width           =   1215
   End
   Begin VB.CommandButton OK 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   375
      Left            =   5880
      TabIndex        =   10
      Top             =   120
      Width           =   1215
   End
   Begin VB.TextBox Person 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   0
      Top             =   120
      Width           =   3015
   End
   Begin VB.TextBox Mother 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   1
      Top             =   600
      Width           =   3015
   End
   Begin VB.TextBox Father 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   2
      Top             =   960
      Width           =   3015
   End
   Begin VB.TextBox Spouse 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   3
      Top             =   1440
      Width           =   3015
   End
   Begin VB.Label Label4 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Person:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   120
      Width           =   735
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Spouse:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   1440
      Width           =   855
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Father:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   960
      Width           =   855
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Mother:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   6
      Top             =   600
      Width           =   735
   End
End
Attribute VB_Name = "PersonForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

'''''''''''''''''''''''''''''''''''''''''''''''''''
' Tell the rest of the program the user cancelled
'
Private Sub Cancel_Click()
    PersonForm.OK.Tag = 0
    PersonForm.Hide
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''
' Make Sure All the Person Information was Entered
'
Private Sub OK_Click()

    ' Make sure all the data has been entered properly
    If Len(PersonForm.Person.Text) = 0 Then
        MsgBox "Must enter Person", 0, ""
        Exit Sub
    End If

    If Len(PersonForm.Mother.Text) = 0 Or Len(PersonForm.Father.Text) = 0 Then
        MsgBox "Must enter Mother and Father", 0, ""
        Exit Sub
    End If

    If Len(PersonForm.Spouse.Text) = 0 And PersonForm.Single.Value = False Then
        MsgBox "Must enter Spouse", 0, ""
        Exit Sub
    End If

    ' Tell the rest of the program OK was pressed
    PersonForm.OK.Tag = 1
    PersonForm.Hide
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Don't Allow a Spouse to Be Entered if Single is Ticked
'
Private Sub Single_Click()
    If PersonForm.Single.Value Then
        PersonForm.Spouse.Text = ""
        PersonForm.Spouse.Enabled = False
    Else
        PersonForm.Spouse.Enabled = True
    End If
End Sub

