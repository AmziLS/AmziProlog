VERSION 5.00
Begin VB.Form TestVB 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "VB Module Test"
   ClientHeight    =   4740
   ClientLeft      =   810
   ClientTop       =   1395
   ClientWidth     =   6765
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
   ScaleHeight     =   4740
   ScaleWidth      =   6765
   Begin VB.CommandButton MakeList 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Make List"
      Height          =   253
      Left            =   242
      TabIndex        =   13
      Top             =   3388
      Width           =   1342
   End
   Begin VB.ListBox List 
      Appearance      =   0  'Flat
      Height          =   810
      Left            =   1936
      TabIndex        =   12
      Top             =   968
      Width           =   1830
   End
   Begin VB.CommandButton AssertStuff 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Assert Stuff"
      Height          =   253
      Left            =   242
      TabIndex        =   11
      Top             =   3025
      Width           =   1342
   End
   Begin VB.CommandButton JustUnify 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Just Unify"
      Height          =   253
      Left            =   242
      TabIndex        =   10
      Top             =   2541
      Width           =   1342
   End
   Begin VB.CommandButton MakeUnify 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Make & Unify"
      Height          =   253
      Left            =   242
      TabIndex        =   9
      Top             =   2178
      Width           =   1342
   End
   Begin VB.CommandButton AtomTest 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Test Atom"
      Height          =   253
      Left            =   242
      TabIndex        =   8
      Top             =   1694
      Width           =   1342
   End
   Begin VB.CommandButton StringTest 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Test String"
      Height          =   253
      Left            =   242
      TabIndex        =   7
      Top             =   1331
      Width           =   1342
   End
   Begin VB.CommandButton FloatTest 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Test Float"
      Height          =   253
      Left            =   242
      TabIndex        =   6
      Top             =   968
      Width           =   1342
   End
   Begin VB.CommandButton LongTest 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Test Long"
      Height          =   253
      Left            =   242
      TabIndex        =   5
      Top             =   605
      Width           =   1342
   End
   Begin VB.CommandButton TestInt 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Test Integer"
      Height          =   253
      Left            =   242
      TabIndex        =   4
      Top             =   242
      Width           =   1342
   End
   Begin VB.TextBox Answer 
      Appearance      =   0  'Flat
      Height          =   374
      Left            =   1936
      TabIndex        =   3
      Top             =   484
      Width           =   4609
   End
   Begin VB.TextBox Version 
      Appearance      =   0  'Flat
      Height          =   450
      Left            =   1560
      TabIndex        =   0
      Top             =   4005
      Width           =   5100
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "And the Answer is..."
      ForeColor       =   &H80000008&
      Height          =   253
      Left            =   1936
      TabIndex        =   2
      Top             =   242
      Width           =   2915
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Logic Server Version"
      ForeColor       =   &H80000008&
      Height          =   450
      Left            =   240
      TabIndex        =   1
      Top             =   4005
      Width           =   1230
   End
End
Attribute VB_Name = "TestVB"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub AssertStuff_Click()
    Dim tf As Integer, Term As Long, tstr As String

    List.Clear

    Call AssertaStrLS("flower(rose)")
    Call AssertaStrLS("flower(daffodil)")
    Call AssertaStrLS("flower(gladiola)")
    Call AssertaStrLS("flower(phlox)")
    Call AssertaStrLS("flower(lily)")

    tf = CallStrLS(Term, "flower(R)")
    While tf
        List.AddItem GetStrArgLS(Term, 1)
        tf = RedoLS()
    Wend
End Sub

Private Sub AtomTest_Click()
    Dim Term As Long, tf As Integer

    tf = CallStrLS(Term, "testing(_, _, _, X, _)")
    If tf = True Then
        Answer.Text = GetStrArgLS(Term, 4)
    Else
        Answer.Text = "* failed *"
    End If

End Sub

Private Sub FloatTest_Click()
    Dim Term As Long, tf As Integer

    tf = CallStrLS(Term, "testing(_, _, X, _, _)")
    If tf = True Then
        Answer.Text = Str$(GetFloatArgLS(Term, 3))
    Else
        Answer.Text = "* failed *"
    End If

End Sub

Private Sub Form_Load()
    Call InitLS("")
    Call LoadLS(App.Path + "\testvb")
    Version.Text = GetVersionLS()
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Call CloseLS
End Sub

Private Sub JustUnify_Click()
    Dim tf As Integer, BigTrm As Long, tstr As String

    Call MakeFALS(BigTrm, "biggie", 5)
    
    tf = UnifyIntArgLS(BigTrm, 1, 42)
    tf = UnifyLongArgLS(BigTrm, 2, 1000000)
    tf = UnifyFloatArgLS(BigTrm, 3, 3.14159)
    tf = UnifyAtomArgLS(BigTrm, 4, "adamhadem")
    tf = UnifyStrArgLS(BigTrm, 5, "the tulips")

    Call TermToStrQLS(BigTrm, tstr, 200)
    Answer.Text = tstr

End Sub

Private Sub LongTest_Click()
    Dim Term As Long, tf As Integer

    tf = CallStrLS(Term, "testing(_, X, _, _, _)")
    If tf = True Then
        Answer.Text = Str$(GetLongArgLS(Term, 2))
    Else
        Answer.Text = "* failed *"
    End If

End Sub

Private Sub MakeList_Click()
    Dim ListTrm As Long, IntTrm As Long, LongTrm As Long
    Dim FloatTrm As Long, StrTrm As Long, AtomTrm As Long
    Dim tf As Integer, tstr As String

    Call MakeListLS(ListTrm)

    Call MakeIntLS(IntTrm, 99)
    Call MakeLongLS(LongTrm, 250000)
    Call MakeFloatLS(FloatTrm, 0.00001)
    Call MakeAtomLS(AtomTrm, "garden")
    Call MakeStrLS(StrTrm, "woods")

    Call PushListLS(ListTrm, IntTrm)
    Call PushListLS(ListTrm, LongTrm)
    Call PushListLS(ListTrm, FloatTrm)
    Call PushListLS(ListTrm, AtomTrm)
    Call PushListLS(ListTrm, StrTrm)

    Call TermToStrQLS(ListTrm, tstr, 200)
    Answer.Text = tstr

End Sub

Private Sub MakeUnify_Click()
    Dim BigTrm As Long, IntTrm As Long, LongTrm As Long
    Dim FloatTrm As Long, StrTrm As Long, AtomTrm As Long
    Dim tf As Integer, tstr As String

    Call MakeFALS(BigTrm, "biggie", 5)

    Call MakeIntLS(IntTrm, 33)
    Call MakeLongLS(LongTrm, 12345678)
    Call MakeFloatLS(FloatTrm, 157.234)
    Call MakeAtomLS(AtomTrm, "horse")
    Call MakeStrLS(StrTrm, "rocking")

    tf = UnifyArgLS(BigTrm, 1, bTERM, IntTrm)
    tf = UnifyArgLS(BigTrm, 2, bTERM, LongTrm)
    tf = UnifyArgLS(BigTrm, 3, bTERM, FloatTrm)
    tf = UnifyArgLS(BigTrm, 4, bTERM, AtomTrm)
    tf = UnifyArgLS(BigTrm, 5, bTERM, StrTrm)

    Call TermToStrLS(BigTrm, tstr, 200)
    Answer.Text = tstr

End Sub

Private Sub StringTest_Click()
    Dim Term As Long, tf As Integer

    tf = CallStrLS(Term, "testing(_, _, _, _, X)")
    If tf = True Then
        Answer.Text = GetStrArgLS(Term, 5)
    Else
        Answer.Text = "* failed *"
    End If

End Sub

Private Sub TestInt_Click()
    Dim Term As Long, tf As Integer

    tf = CallStrLS(Term, "testing(X, _, _, _, _)")
    If tf = True Then
        Answer.Text = Str$(GetIntArgLS(Term, 1))
    Else
        Answer.Text = "* failed *"
    End If
End Sub

