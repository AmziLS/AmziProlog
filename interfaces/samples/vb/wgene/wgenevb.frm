VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form MainForm 
   Appearance      =   0  'Flat
   BackColor       =   &H80000005&
   Caption         =   "Amzi! Prolog+Logic Server Genealogical Demo"
   ClientHeight    =   3210
   ClientLeft      =   1440
   ClientTop       =   1770
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
   ScaleHeight     =   3210
   ScaleWidth      =   8010
   Begin VB.ListBox RelatedPersonsList 
      Appearance      =   0  'Flat
      Height          =   2370
      Left            =   5280
      TabIndex        =   2
      Top             =   600
      Width           =   2535
   End
   Begin VB.ListBox RelationshipList 
      Appearance      =   0  'Flat
      Height          =   2370
      Left            =   3120
      TabIndex        =   1
      Top             =   600
      Width           =   1815
   End
   Begin VB.ListBox PersonList 
      Appearance      =   0  'Flat
      Height          =   2370
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
   Begin VB.Menu mFile 
      Caption         =   "&File"
      Begin VB.Menu mFileOpen 
         Caption         =   "&Open"
      End
      Begin VB.Menu FileSeparator1 
         Caption         =   "-"
      End
      Begin VB.Menu mFileSave 
         Caption         =   "&Save"
      End
      Begin VB.Menu mFileSaveAs 
         Caption         =   "Save &As"
      End
      Begin VB.Menu FileSeparator2 
         Caption         =   "-"
      End
      Begin VB.Menu mFileExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mPerson 
      Caption         =   "&Person"
      Begin VB.Menu mPersonAdd 
         Caption         =   "&Add"
      End
      Begin VB.Menu mPersonChange 
         Caption         =   "&Change"
      End
      Begin VB.Menu mPersonDelete 
         Caption         =   "&Delete"
      End
   End
   Begin VB.Menu mHelp 
      Caption         =   "&Help"
      Begin VB.Menu mHelpContents 
         Caption         =   "&Contents"
      End
      Begin VB.Menu HelpSeparator2 
         Caption         =   "-"
      End
      Begin VB.Menu mHelpAbout 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "MainForm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Windows Genealogical Demo for Visual Basic
' Copyright (c) 1994-2000 by Amzi! inc.  All Rights Reserved.
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

    ' Build the Prolog command <relationship>(X, <person>)
    tf = CallStrLS(Term, Relationship + "(X, '" + Person + "')")

    ' Loop getting all the people who have that relationship
    ' and add them to the related persons list
    While (tf)
        Call GetArgLS(Term, 1, bSTR, StrVal)
        RelatedPersonsList.AddItem StrVal
        tf = RedoLS()
    Wend
End Sub

''''''''''''''''''''''''''''''''''''''''''''
' Main Form Handler Starts up Amzi! Prolog
'
Private Sub Form_Load()
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim xplname As String

    ' Setup our xpl and help file pathnames
    xplname = App.Path + "\gene.xpl"
    App.HelpFile = App.Path + "\wgenevb.hlp"

    ' Initialize the runtime and load GENE.XPL, which contains
    ' all the rules and expertise for this application
    InitLS (xplname)
    LoadLS (xplname)

    ' Turn off backslash processing in strings and atoms (so pathnames work)
    tf = CallStrLS(Term, "set_mode(string_esc, off)")
    If tf <> True Then
        MsgBox "Unable to turn of string escape processing"
    End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' Close Down Amzi Prolog When the Main Form Closes
'
Private Sub Form_Unload(Cancel As Integer)
    Dim rc As Integer

    ' Close the Prolog runtime, releasing all resources
    Call CloseLS
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' Close Down Amzi! Prolog When the Main Form Closes
'
Private Sub mFileExit_Click()
    Dim rc As Integer

    ' Close the Prolog runtime, releasing all resources
    CloseLS
    End
End Sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Open a Family Database by Consulting it into Prolog
'
Private Sub mFileOpen_Click()
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Open a family file using the common dialog and check for
    ' ESC/Cancel being pressed using an error handler
    On Error GoTo OpenErrorHandler
    MainForm.CMDialog.CancelError = True
    MainForm.CMDialog.Action = 1
    On Error GoTo 0

    ' Build a Prolog consult command to load the selected family data
    CurrentFamily = MainForm.CMDialog.FileName
    tf = CallStrLS(Term, "consult('" + CurrentFamily + "')")
    If (tf <> True) Then
        MsgBox "Unable to load family", 0, ""
        CurrentFamily = ""
    End If

    ' Display the Persons and Relations lists
    tf = Persons()
    tf = Relations()

End_mFileOpen:
    Exit Sub

' Error handler for file open command dialog to capture ESC/Cancel
OpenErrorHandler:
    If Err = 32755 Then
        On Error GoTo 0
        Resume End_mFileOpen
    Else
        On Error GoTo 0
        Error Err
    End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''''
' Save the Family Database by Calling Prolog
'
Private Sub mFileSave_Click()
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Check if we have a family already opened
    ' If not, get the file name to save as using the common dialog
    If Len(CurrentFamily) = 0 Then
        On Error GoTo SaveErrorHandler
        MainForm.CMDialog.CancelError = True
        MainForm.CMDialog.Action = 2
        On Error GoTo 0
        CurrentFamily = MainForm.CMDialog.FileName
    End If

    ' Actually save the family data by issuing a Prolog command
    tf = CallStrLS(Term, "save('" + CurrentFamily + "')")
    If (tf <> True) Then
        MsgBox "Unable to save family", 0, ""
    End If

End_mFileSave:
    Exit Sub

' Error handler to capture ESC/Cancel in the file dialog box
SaveErrorHandler:
    If Err = 32755 Then
        On Error GoTo 0
        Resume End_mFileSave
    Else
        On Error GoTo 0
        Error Err
    End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''
' Save the Family Database into a New File
'
Private Sub mFileSaveAs_Click()
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    
    ' Get the name of the open family, and invoke the file dialog
    MainForm.CMDialog.FileName = CurrentFamily
    On Error GoTo SaveAsErrorHandler
    MainForm.CMDialog.CancelError = True
    MainForm.CMDialog.Action = 2
    On Error GoTo 0
    CurrentFamily = MainForm.CMDialog.FileName

    ' Build and call a Prolog command to actually save the family data
    tf = CallStrLS(Term, "save('" + CurrentFamily + "')")
    If (tf <> True) Then
        MsgBox "Unable to save family", 0, ""
    End If

End_mFileSaveAs:
    Exit Sub

SaveAsErrorHandler:
    If Err = 32755 Then
        On Error GoTo 0
        Resume End_mFileSaveAs
    Else
        On Error GoTo 0
        Error Err
    End If
End Sub

Private Sub mHelpAbout_Click()
    MsgBox "Amzi! Logic Server, " + GetVersionLS() + "-- WGENEVB, Version 1.2, Copyright (c) 1994-1995 Amzi! inc.", 0, ""
End Sub

'''''''''''''''
' Display Help
'
Private Sub mHelpContents_Click()
    Dim retval As Integer
    retval = ShellExecute(0, ByVal "open", ByVal "doc.html", "", "", 1)
End Sub

'''''''''''''''
' Display Help
'
Private Sub mHelpSearch_Click()
    Dim retval As Integer
    retval = ShellExecute(0, ByVal "open", ByVal "wgenevb.htm", "", "", 1)
End Sub

''''''''''''''''''''''''''
' Add a New Family Member
'
Private Sub mPersonAdd_Click()
    Dim s As String
    Dim tf As Integer

    ' Clear any old data in the form
    PersonForm.Person.Text = ""
    PersonForm.Female.Value = True
    PersonForm.Spouse.Text = ""
    PersonForm.Single.Value = False
    PersonForm.Mother.Text = ""
    PersonForm.Father.Text = ""

    ' Display the person information form
    PersonForm.Show 1
    
    ' If OK was selected, then add the person, and redisplay the lists
    If PersonForm.OK.Tag Then
        tf = AddPerson()
        If tf Then
            tf = Persons()
            tf = Relations()
        Else
            ' If the person didn't add, get the explanation from Prolog
            ' and display it for the user
            Call DisplayError
        End If
    End If
End Sub

'''''''''''''''''''''''''
' Change a Family Member
'
Private Sub mPersonChange_Click()
    Dim s As String, Who As String
    Dim tf As Integer

    ' Find out who were going to change
    Who = InputBox("Whom do you want to change?", "", PersonList.List(PersonList.ListIndex))
    If Len(Who) = 0 Then
        Exit Sub
    End If

    ' Get the information about the person (checking if s/he exists)
    If GetPerson(Who) = 0 Then
        MsgBox "Person " + Who + " Does Not Exist", 0, ""
        Exit Sub
    End If

    ' Display the person information and let the user change it
    PersonForm.Person.Enabled = False
    PersonForm.Show 1
    PersonForm.Person.Enabled = True
    
    ' If the user said OK then change the person and redisplay the lsits
    If PersonForm.OK.Tag Then
        tf = ChangePerson()
        If tf Then
            tf = Persons()
            tf = Relations()
        End If
    End If
End Sub

'''''''''''''''''''''''''
' Delete a Family Member
'
Private Sub mPersonDelete_Click()
    Dim s As String, Who As String
    Dim tf As Integer

    ' Find out who we're supposed to delete
    Who = InputBox("Whom do you want to delete?", "", PersonList.List(PersonList.ListIndex))
    If Len(Who) = 0 Then
        Exit Sub
    End If
    
    ' Check if the person exists
    If GetPerson(Who) = 0 Then
        MsgBox "Person " + Who + " Does Not Exist", 0, ""
        Exit Sub
    End If

    ' Delete the person and redisplay the lists
    tf = DeletePerson(Who)
    If tf Then
        tf = Persons()
        tf = Relations()
    End If
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display Relations When a New Person is Highlighted
'
Private Sub PersonList_Click()
    Call DisplayRelations
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display All the Family Members in the First List Box
'
Private Function Persons() As Integer
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim StrVal As String

    ' Clear the list first
    PersonList.Clear
    
    ' Issue the Prolog query:  person(X)
    tf = CallStrLS(Term, "person(X)")

    ' Check if there are any people
    If (tf <> True) Then
        Persons = 0
        Exit Function
    End If
    
    ' Loop through all the family members, adding them to the list
    While (tf = True)
        Call GetArgLS(Term, 1, bSTR, StrVal)
        PersonList.AddItem StrVal
        tf = RedoLS()
    Wend
    
    Persons = 1

End Function

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

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Display Relations When a New Relationship is Highlighted
'
Private Sub RelationshipList_Click()
    Call DisplayRelations
End Sub

