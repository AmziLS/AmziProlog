Attribute VB_Name = "Declare"
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Windows ODBC Genealogical Demo for Visual Basic
' Copyright (c) 1994-98 by Amzi! inc.  All Rights Reserved.
'

' This declaration is not allowed by the compiler in MainForm

Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

