VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Pets"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Sub Init(XPLFile As String)
    Call InitLS("")
    Call AddLSX("aodbc")
    LoadLS (XPLFile)
End Sub

Public Function GetPet(SoundVar As Variant) As String
Dim Term As Long
Dim Sound As String

    Sound = CStr(SoundVar)
    AssertaStrLS ("sound('" + Sound + "')")
    If ExecStrLS(Term, "pet(X)") = True Then
       GetPet = GetStrArgLS(Term, 1)
    Else
       GetPet = "ERROR"
    End If
End Function

Public Sub Done()
    Call CloseLS
End Sub


