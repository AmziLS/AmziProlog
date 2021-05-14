Attribute VB_Name = "PMsgBoxModule"
' This is the extended predicate which can be called from Prolog
' It is passed the same EngineID argument which was passed to AddPredLS
'
Public Function PMsgBox(ByVal EngineID As Long) As Long
Dim rc As Integer, tstr As String

    tstr = Space$(255)
    rc = lsGetParm(EngineID, 1, bSTR, ByVal tstr)
    MsgBox (RTrim$(tstr))
    PMsgBox = 1
    
End Function



