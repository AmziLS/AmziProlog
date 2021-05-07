VERSION 5.00
Begin VB.Form MainForm 
   Caption         =   "COM VB Pets"
   ClientHeight    =   2340
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4095
   LinkTopic       =   "Form1"
   ScaleHeight     =   2340
   ScaleWidth      =   4095
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox PetText 
      Height          =   285
      Left            =   1320
      TabIndex        =   4
      Top             =   1440
      Width           =   2295
   End
   Begin VB.ListBox SoundList 
      Height          =   645
      ItemData        =   "vb_pets.frx":0000
      Left            =   1320
      List            =   "vb_pets.frx":000D
      TabIndex        =   1
      Top             =   120
      Width           =   2295
   End
   Begin VB.CommandButton GetPetButton 
      Caption         =   "Get Pet"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   840
      Width           =   975
   End
   Begin VB.Label PetLabel 
      Caption         =   "Your pet is a"
      Height          =   255
      Left            =   120
      TabIndex        =   3
      Top             =   1440
      Width           =   975
   End
   Begin VB.Label SoundLabel 
      Caption         =   "Pick a Sound"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   1095
   End
End
Attribute VB_Name = "MainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Get a LogicServer (enable amzicom by selecting
' Project / References and tick the box)

Dim LS As amzicom.ComLogicServer

Private Sub GetPetButton_Click()
    Dim sound As String, pet As String
    Dim term As Long
    
    On Error GoTo LSError
    
    ' Create a Logic Server
    Set LS = New amzicom.ComLogicServer
    
    ' Initialize and load our xpl file
    LS.Init ("")
    LS.Load ("\amzi\dev\a6\src\samples\com\vb_pets\pets.xpl")
    
    ' If a sound is selected get the pet
    If SoundList.ListIndex >= 0 Then
        
        ' Assert the sound
        sound = SoundList.List(SoundList.ListIndex)
        LS.AssertaStr ("sound(" + sound + ")")
        
        ' Determine the pet
        term = LS.ExecStr("pet(X)")
        pet = LS.GetStrArg(term, 1)
        
        ' And display it
        MainForm.PetText.Text = pet
    Else
        Beep
    End If
    
    GoTo CloseUp
    
LSError:
    ' On error get the message
    Call MsgBox(LS.GetExceptMsg(), vbCritical, "ERROR")
    
CloseUp:
    Call LS.Close
    
End Sub
