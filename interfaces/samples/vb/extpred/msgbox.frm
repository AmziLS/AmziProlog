VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   1785
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   1785
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton DisplayMessage 
      Caption         =   "Display Message Box"
      Height          =   495
      Left            =   1320
      TabIndex        =   0
      Top             =   600
      Width           =   2175
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub DisplayMessage_Click()
Dim tf As Integer

    Call InitLS("")
    Call AddPredLS("msgbox", 1, AddressOf PMsgBox)
    Call LoadLS(App.Path + "\msgbox")
    tf = MainLS
    Call CloseLS

End Sub

