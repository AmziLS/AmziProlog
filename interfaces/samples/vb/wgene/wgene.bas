Attribute VB_Name = "WGENE"
'''''''''''''''''''''''''''''''''''''''''
' Some Routines to Interface with Prolog
'
Global CurrentFamily As String

Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long



''''''''''''''''''''''''''''''''''''''''''''
' Add a New Family Member by Calling Prolog
'
Function AddPerson() As Integer
    Dim s As String
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Build a Prolog query to add the person the query is of the
    ' form add_person(Name, male|female, Mother, Father, single/Spouse)
    ' AddPerson uses the data in the person information form
    s = "add_person('" + PersonForm.Person.Text + "', "
    ' Convert an OptionBox into a token
    If PersonForm.Female.Value = True Then
        s = s + "female, '"
    Else
        s = s + "male, '"
    End If

    s = s + PersonForm.Mother.Text + "', '"
    s = s + PersonForm.Father.Text + "', "

    ' Put in the single token or the name of the spouse
    If PersonForm.Single.Value = True Then
        s = s + "single)" + Chr$(0)
    Else
        s = s + "'" + PersonForm.Spouse.Text + "')"
    End If

    ' Call the query string
    tf = CallStrLS(Term, s)

    If tf = False Then
        AddPerson = 0
        Exit Function
    End If

    AddPerson = 1
End Function

'''''''''''''''''''''''''''''''''''''''''''
' Change a Family Member by Calling Prolog
'
Function ChangePerson() As Integer
    Dim s As String
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Call a Prolog command to delete the person
    ' ChangePerson works from the data in the person form
    tf = CallStrLS(Term, "delete('" + PersonForm.Person.Text + "')")
    
    ' If the Delete works, then continue
    If tf = False Then
        ChangePerson = 0
        Exit Function
    End If

    'Now build the query to add the updated person information
    ' (just like AddPerson)
    s = "add_person('" + PersonForm.Person.Text + "', "
    If PersonForm.Female.Value = True Then
        s = s + "female, '"
    Else
        s = s + "male, '"
    End If

    s = s + PersonForm.Mother.Text + "', '"
    s = s + PersonForm.Father.Text + "', "

    If PersonForm.Single.Value = True Then
        s = s + "single)"
    Else
        s = s + "'" + PersonForm.Spouse.Text + "')"
    End If

    ' Call the query string
    tf = CallStrLS(Term, s)

    If tf = False Then
        ChangePerson = 0
        Exit Function
    End If

    ChangePerson = 1
End Function

'''''''''''''''''''''''''''''''''''''''''''
' Delete a Family Member by Calling Prolog
'
Function DeletePerson(Who As String) As Integer
    Dim s As String
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Call a Prolog query to delete a person
    tf = CallStrLS(Term, "delete('" + Who + "')")
    DeletePerson = 1
    
    If tf = False Then
        DeletePerson = 0
        Exit Function
    End If
End Function

'''''''''''''''''''''''''''''''''''''
' Display an Explanation from Prolog
'
Sub DisplayError()
    Dim rc As Integer, tf As Integer
    Dim Term As Long
    Dim Msg As String

    ' Get the last error message from Prolog by building
    ' a query string, then call it
    tf = CallStrLS(Term, "message(X)")
    If tf = True Then
        ' Get the value of the 1st argument (X)
        Call GetArgLS(Term, 1, bSTR, Msg)
    End If

    ' If we couldn't get a message then display unknown
    If tf <> True Then
        Msg = "Unknown Error"
    End If

    ' Display the message
    MsgBox Msg, 48, "A Message from Amzi! Prolog"
End Sub

''''''''''''''''''''''''''''''''''''''''
' Get Information About a Family Member
'
Function GetPerson(Who As String) As Integer
    Dim s As String, Gender As String, Mother As String, Father As String, Spouse As String
    Dim rc As Integer, tf As Integer
    Dim Term As Long

    ' Get the information about the person, Who
    tf = CallStrLS(Term, "person('" + Who + "', Gender, Mother, Father, Spouse)")
        
    ' If the call worked, then extract each of the parts of the person
    ' predicate and put them in the person form
    If tf = True Then
        Call GetArgLS(Term, 2, bSTR, Gender)
        Call GetArgLS(Term, 3, bSTR, Mother)
        Call GetArgLS(Term, 4, bSTR, Father)
        Call GetArgLS(Term, 5, bSTR, Spouse)
        PersonForm.Person.Text = Who
        PersonForm.Mother.Text = Mother
        PersonForm.Father.Text = Father

        ' Convert the gender token to an option button
        If Left$(Gender, 4) = "male" Then
            PersonForm.Male.Value = True
        Else
            PersonForm.Female.Value = True
        End If

        ' Setup the single check box and spouse field
        If Left$(Spouse, 6) = "single" Then
            PersonForm.Single.Value = 1
        Else
            PersonForm.Single.Value = 0
            PersonForm.Spouse.Text = Spouse
        End If

        GetPerson = 1
    End If

    If tf = False Then
        GetPerson = 0
    End If
End Function

