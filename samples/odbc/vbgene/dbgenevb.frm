VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form MainForm 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Amzi! ODBC Genealogical Demo"
   ClientHeight    =   3585
   ClientLeft      =   1425
   ClientTop       =   1470
   ClientWidth     =   8010
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
   HelpContextID   =   200
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3585
   ScaleWidth      =   8010
   Begin VB.CommandButton Query 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Query"
      Height          =   375
      Left            =   240
      TabIndex        =   8
      Top             =   3000
      Width           =   975
   End
   Begin VB.CommandButton Exit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Exit"
      Height          =   374
      Left            =   6840
      TabIndex        =   7
      Top             =   3000
      Width           =   979
   End
   Begin VB.CommandButton Help 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Help"
      Height          =   374
      Left            =   5280
      TabIndex        =   6
      Top             =   3000
      Width           =   979
   End
   Begin VB.ListBox RelatedPersonsList 
      Appearance      =   0  'Flat
      Height          =   2175
      Left            =   5280
      TabIndex        =   2
      Top             =   600
      Width           =   2535
   End
   Begin VB.ListBox RelationshipList 
      Appearance      =   0  'Flat
      Height          =   2175
      Left            =   3120
      TabIndex        =   1
      Top             =   600
      Width           =   1815
   End
   Begin VB.ListBox PersonList 
      Appearance      =   0  'Flat
      Height          =   2175
      Left            =   240
      TabIndex        =   0
      Top             =   600
      Width           =   2535
   End
   Begin MSComDlg.CommonDialog CMDialog 
      Left            =   7440
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   ".fam"
      Filter          =   "Family (*.fam)|*.fam|All Files (*.*)|*.*|"
   End
   Begin VB.Label RelatedTo 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "To:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   5280
      TabIndex        =   5
      Top             =   240
      Width           =   615
   End
   Begin VB.Label RelationshipLabel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Relationship:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   3120
      TabIndex        =   4
      Top             =   240
      Width           =   1335
   End
   Begin VB.Label PersonLabel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Person:"
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   240
      TabIndex        =   3
      Top             =   240
      Width           =   855
   End
End
Attribute VB_Name = "MainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Windows ODBC Genealogical Demo for Visual Basic
' Copyright (c) 1994-98 by Amzi! inc.  All Rights Reserved.
'

''''''''''''''''''''''''''''''''''''''''''''''''
' Display All the Relations for the Highlighted
' Person and Relationship
'
Private Sub DisplayRelations()
    Dim Person As String, Relationship As String
    Dim StrVal As String
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim PID As Integer
    Dim PersonID, Firstname, Midname, Surname As String

    ' First clear the list of related persons
    RelatedPersonsList.Clear

    ' If we don't have both a person and a relationship highlighted
    ' then exit this routine
    If PersonList.ListIndex < 0 Or RelationshipList.ListIndex < 0 Then
        Exit Sub
    End If

    ' Get the highlighted person and relationship
    Person = PersonList.List(PersonList.ListIndex)
    Relationship = RelationshipList.List(RelationshipList.ListIndex)

    ' Build the Prolog command "query(<relationship>(X,<person_id>), PID, Surname, Midname, Name)"
    PersonID = Mid$(Person, 1, InStr(Person, ":") - 1)
    tf = CallStrLS(Term, "query(" + Relationship + "(X, " + PersonID + "), PID, Surname, Midname, Firstname)")

    If tf = False Then
        RelatedPersonsList.AddItem "No " + Relationship + " for " + Right$(Person, Len(Person) - (InStr(Person, ":") + 1))
    End If

    ' Loop getting all the people who have that relationship
    ' and add them to the related persons list
    While (tf)
        PID = GetIntArgLS(Term, 2)
        Surname = GetStrArgLS(Term, 3)
        Midname = GetStrArgLS(Term, 4)
        Firstname = GetStrArgLS(Term, 5)
        StrVal = Format$(PID, "###") + ": " + Firstname + " " + Midname + " " + Surname
        RelatedPersonsList.AddItem StrVal
        tf = RedoLS()
    Wend
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Close Down Amzi Prolog When the Exit Button is Pressed
'
Private Sub Exit_Click()
    Dim tf As Integer
    Dim Term As Long
    
    ' Close the ODBC database
    tf = ExecStrLS(Term, "db_close")
    If tf <> True Then
        MsgBox "Unable to close ODBC database 'gene'"
    End If
    
    ' Close the Prolog runtime, releasing all resources
    CloseLS
    End
End Sub

''''''''''''''''''''''''''''''''''''''''''''
' Main Form Handler Starts up Amzi! Prolog
'
Private Sub Form_Load()
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim xplname As String

    ' Setup our xpl and help file pathnames
    xplname = App.Path + "\DBGENE.XPL"

    ' Initialize the runtime and load DBGENE.XPL, which contains
    ' all the rules and expertise for this application
    ' Also load the ODBC LSX
    InitLS (xplname)
    AddLSX ("aodbc")
    LoadLS (xplname)

    ' Turn off backslash processing in strings and atoms (so pathnames work)
    tf = CallStrLS(Term, "set_mode(string_esc, off)")
    If tf <> True Then
        MsgBox "Unable to turn of string escape processing"
    End If

    ' Open the ODBC database, gene
    tf = ExecStrLS(Term, "db_open('gene')")
    If tf <> True Then
        MsgBox "Unable to open the ODBC database 'gene'; check your ODBC setup in the control panel"
    End If

    ' Display the Persons and Relations lists
    tf = Persons()
    tf = Relations()

End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' Close Down Amzi Prolog When the Main Form Closes
'
Private Sub Form_Unload(CANCEL As Integer)
    Dim tf As Integer
    Dim Term As Long
    
    ' Close the ODBC database
    tf = ExecStrLS(Term, "db_close")
    If tf <> True Then
        MsgBox "Unable to close ODBC database 'gene'"
    End If

    ' Close the Prolog runtime, releasing all resources
    Call CloseLS
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display Help When Button is Pressed
'
Private Sub Help_Click()
    Dim retval As Integer
    retval = ShellExecute(0, ByVal "open", ByVal App.Path + "\doc.html", "", "", 1)
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display All the Family Members in the First List Box
'
Private Function Persons() As Integer
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim PID As Integer
    Dim StrVal, Firstname, Midname, Surname As String

    ' Clear the list first
    PersonList.Clear
    
    ' Issue the Prolog query:  person(X)
    tf = CallStrLS(Term, "fullname(Pid, Surname, Midname, Firstname)")

    ' Check if there are any people
    If (tf <> True) Then
        Persons = 0
        Exit Function
    End If
    
    ' Loop through all the family members, adding them to the list
    While (tf = True)
        PID = GetIntArgLS(Term, 1)
        Surname = GetStrArgLS(Term, 2)
        Midname = GetStrArgLS(Term, 3)
        Firstname = GetStrArgLS(Term, 4)
        StrVal = Format$(PID, "###") + ": " + Firstname + " " + Midname + " " + Surname
        PersonList.AddItem StrVal
        tf = RedoLS()
    Wend
    
    Persons = 1

End Function

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display Relations When the Query Button is Pressed
'
Private Sub Query_Click()
    Query.Enabled = False
    Call DisplayRelations
    Query.Enabled = True
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display All the Possible Relationships in the Second List Box
'
Private Function Relations() As Integer
    Dim rc As Integer, tf As Integer
    Dim Term As Long, TList As Long
    Dim StrVal As String

    ' Clear the list first
    RelationshipList.Clear
    
    ' Issue the Prolog query:  relations(X)
    tf = CallStrLS(Term, "relations(X)")
    
    ' Check if there are any relationships
    If (tf <> True) Then
        Relations = 0
        Return
    End If
    
    ' Loop through all the relationships adding them to the list
    Call GetArgLS(Term, 1, bTERM, TList)
    Do
        rc = PopListLS(TList, bSTR, StrVal)
        If (rc = 0) Then
            RelationshipList.AddItem StrVal
        End If
    Loop While (rc = 0)
    
    Relations = 1

End Function

