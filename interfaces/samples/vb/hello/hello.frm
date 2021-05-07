VERSION 5.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Hello VB"
   ClientHeight    =   720
   ClientLeft      =   1485
   ClientTop       =   1470
   ClientWidth     =   2295
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
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   720
   ScaleWidth      =   2295
   Begin VB.CommandButton Command1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Greet Me!"
      Height          =   372
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   2052
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()

    ' rc is used for functions that return an error code
    ' tf is for functions that return true/false (success/failure)
    Dim rc As Integer, rc2 As Integer, tf As Integer
    Dim s As String, xpl As String, errmsg As String
    ' term is actually a pointer, but we use longs in VB
    Dim Term As Long
    
    ' Initialize the engine with the empty string
    ' Error handling is done by the cover functions in amzivb
    Call InitLS("")
    
    MsgBox "Prolog Initialized", MB_OK
    
    ' Load the XPL file
    ' The cover functions add a null onto strings passed to C/C++
    LoadLS (App.Path + "\hello.xpl")
    MsgBox "Hello.XPL Loaded", MB_OK

    ' Call a string containing a term (returns true/false)
    tf = CallStrLS(Term, "hello($Visual Basic Programmer$, X)")
    ' Loop while the term can be unified
    If (tf) Then
        ' The cover functions:
        ' (1) make room for the Amzi engine to return a string
        '     (the size is controlled by SetMaxStrLenLS) and
        ' (2) pass all such strings using ByVal
        Call GetArgLS(Term, 2, bSTR, s)
        ' Display the second argument
        MsgBox s, MB_OK
    End If

    ' Close things up before we go
    Call CloseLS

    End
End Sub

