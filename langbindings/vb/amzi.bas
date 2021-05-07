Attribute VB_Name = "AmziLogicServer"
' AMZI.BAS - Visual Basic DLL Logic Server API Definitions
' and VB Cover Functions
' Copyright (c)1994-2010 Amzi! inc. All Rights Reserved.

Option Explicit

' Last Error Message and Code
Global ErrorLS As String
Global ErrLS As Long

' The type of error handling to perform, 0=message boxes, 1=generate errors
' Must be type Variant--Do not change
Global ErrorMethod As Variant

' The current engine id
Dim EngineID As Long

' The maximum size of strings returned from Prolog
' Must be type Variant--Do not change
Dim MaxStrLen As Variant

' Prolog Types
Global Const pATOM = 0
Global Const pINT = 1
Global Const pSTR = 2
Global Const pFLOAT = 3
Global Const pSTRUCT = 4
Global Const pLIST = 5
Global Const pTERM = 6
Global Const pADDR = 7
Global Const pVAR = 8
Global Const pWSTR = 9
Global Const pWATOM = 10
Global Const pREAL = 11

' Basic Types
Global Const bATOM = 0
Global Const bSTR = 1
Global Const bINT = 4
Global Const bLONG = 3
Global Const bSHORT = 4
Global Const bFLOAT = 5
Global Const bDOUBLE = 6
Global Const bADDR = 7
Global Const bTERM = 8
Global Const bWSTR = 9
Global Const bWATOM = 10
Global Const bMOD = 11
Global Const bGOAL = 12

' Function Definitions

#If unicode Then

Declare Function lsInit Lib "amzi.dll" Alias "lsInitW" (ENGidPtr As Long, ByVal INIFile As String) As Long
Declare Function lsInit2 Lib "amzi.dll" Alias "lsInit2W" (ENGidPtr As Long, ByVal INIParams As String) As Long
Declare Function lsInitLSX Lib "amzi.dll" (ByVal EngID As Long, Ptr As Any) As Long
Declare Function lsAddLSX Lib "amzi.dll" Alias "lsAddLSXW" (ByVal EngID As Long, ByVal LSXFile As String, Ptr As Any) As Long
Declare Function lsAddPred Lib "amzi.dll" Alias "lsAddPredW" (ByVal EngID As Long, ByVal Predname As String, ByVal Arity As Integer, ByVal pfunc As Long, ByVal arg As Long) As Long
Declare Function lsLoad Lib "amzi.dll" Alias "lsLoadW" (ByVal EngID As Long, ByVal XPLFile As String) As Long
Declare Function lsLoadFromMemory Lib "amzi.dll" Alias "lsLoadFromMemoryW" (ByVal EngID As Long, ByVal Filename As String, ByVal Length As Long, ptr As Any) As Long
Declare Function lsMain Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsReset Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsClose Lib "amzi.dll" (ByVal EngID As Long) As Long

Declare Function lsCall Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsRedo Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsCallStr Lib "amzi.dll" Alias "lSCallStrW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsExec Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsExecStr Lib "amzi.dll" Alias "lsExecStrW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsExecStrTh Lib "amzi.dll" Alias "lsExecStrThW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsPutAction Lib "amzi.dll" Alias "lsPutActionW" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsGetActionState Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsClearCall Lib "amzi.dll" (ByVal EngID As Long) As Long

Declare Function lsAsserta Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsAssertz Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsRetract Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsAssertaStr Lib "amzi.dll" Alias "lsAssertaStrW" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsAssertzStr Lib "amzi.dll" Alias "lsAssertzStrW" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsRetractStr Lib "amzi.dll" Alias "lsRetractStrW" (ByVal EngID As Long, ByVal StrPtr As String) As Long

Declare Function lsTermToStr Lib "amzi.dll" Alias "lsTermToStrW" (ByVal EngID As Long, ByVal Term As Long, ByVal StrPtr As String, ByVal StrLen As Long) As Long
Declare Function lsTermToStrQ Lib "amzi.dll" Alias "lsTermToStrQW" (ByVal EngID As Long, ByVal Term As Long, ByVal StrPtr As String, ByVal StrLen As Long) As Long
Declare Function lsStrToTerm Lib "amzi.dll" Alias "lsStrToTermW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsStrTermLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long

Declare Function lsMakeAtom Lib "amzi.dll" Alias "lsMakeAtomW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsMakeStr Lib "amzi.dll" Alias "lsMakeStrW" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsMakeInt Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal num As Long) As Long
Declare Function lsMakeFloat Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal num As Double) As Long
Declare Function lsMakeAddr Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, Ptr As Any) As Long

Declare Function lsGetTermType Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsGetTerm Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsGetFA Lib "amzi.dll" Alias "lsGetFAW" (ByVal EngID As Long, ByVal Term As Long, ByVal Functor As String, Arity As Integer) As Long
Declare Function lsMakeFA Lib "amzi.dll" Alias "lsMakeFAW" (ByVal EngID As Long, TermPtr As Long, ByVal Functor As String, ByVal Arity As Integer) As Long
Declare Function lsGetArg Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetArgType Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long) As Integer
Declare Function lsStrArgLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long) As Long
Declare Function lsUnify Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal Term As Long) As Long
Declare Function lsUnifyArg Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal ArgNum As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsMakeList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsPushList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal Term As Long) As Long
Declare Function lsPopList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetHead Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetTail Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long

Declare Function lsGetVersion Lib "amzi.dll" Alias "lsGetVersionW" (ByVal EngID As Long, ByVal StrPtr As String) As Long

Declare Function lsGetParm Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetParmType Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long) As Long
Declare Function lsStrParmLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long) As Long
Declare Function lsUnifyParm Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsSetStream Lib "amzi.dll" (ByVal EngID As Long, ByVal Stream As Integer, ByVal Handle As Long) As Long
Declare Function lsGetStream Lib "amzi.dll" (ByVal EngID As Long, ByVal Stream As Integer) As Long

Declare Sub lsGetExceptMsg Lib "amzi.dll" Alias "lsGetExceptMsgW" (ByVal EngID As Long, ByVal StrPtr As String, ByVal StrLen As Long)
Declare Function lsGetExceptRC Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Sub lsGetExceptReadBuffer Lib "amzi.dll" Alias "lsGetExceptReadBufferW" (ByVal EngID As Long, ByVal StrPtr As String, StrLen As Long)
Declare Function lsErrRaise Lib "amzi.dll" Alias "lsErrRaiseW" (ByVal EngID As Long, ByVal StrPtr As String) As Long

#Else

Declare Function lsInit Lib "amzi.dll" Alias "lsInitA" (ENGidPtr As Long, ByVal INIFile As String) As Long
Declare Function lsInit2 Lib "amzi.dll" Alias "lsInit2A" (ENGidPtr As Long, ByVal INIParams As String) As Long
Declare Function lsInitLSX Lib "amzi.dll" (ByVal EngID As Long, Ptr As Any) As Long
Declare Function lsAddLSX Lib "amzi.dll" Alias "lsAddLSXA" (ByVal EngID As Long, ByVal LSXFile As String, Ptr As Any) As Long
Declare Function lsAddPred Lib "amzi.dll" Alias "lsAddPredA" (ByVal EngID As Long, ByVal Predname As String, ByVal Arity As Integer, ByVal pfunc As Long, ByVal arg As Long) As Long
Declare Function lsLoad Lib "amzi.dll" Alias "lsLoadA" (ByVal EngID As Long, ByVal XPLFile As String) As Long
Declare Function lsLoadFromMemory Lib "amzi.dll" Alias "lsLoadFromMemoryA" (ByVal EngID As Long, ByVal Filename As String, ByVal Length As Long, ptr As Any) As Long
Declare Function lsMain Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsReset Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsClose Lib "amzi.dll" (ByVal EngID As Long) As Long

Declare Function lsCall Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsRedo Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsCallStr Lib "amzi.dll" Alias "lsCallStrA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsExec Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsExecStr Lib "amzi.dll" Alias "lsExecStrA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsExecStrTh Lib "amzi.dll" Alias "lsExecStrThA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsPutAction Lib "amzi.dll" Alias "lsPutActionA" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsGetActionState Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Function lsClearCall Lib "amzi.dll" (ByVal EngID As Long) As Long

Declare Function lsAsserta Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsAssertz Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsRetract Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsAssertaStr Lib "amzi.dll" Alias "lsAssertaStrA" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsAssertzStr Lib "amzi.dll" Alias "lsAssertzStrA" (ByVal EngID As Long, ByVal StrPtr As String) As Long
Declare Function lsRetractStr Lib "amzi.dll" Alias "lsRetractStrA" (ByVal EngID As Long, ByVal StrPtr As String) As Long

Declare Function lsTermToStr Lib "amzi.dll" Alias "lsTermToStrA" (ByVal EngID As Long, ByVal Term As Long, ByVal StrPtr As String, ByVal StrLen As Long) As Long
Declare Function lsTermToStrQ Lib "amzi.dll" Alias "lsTermToStrQA" (ByVal EngID As Long, ByVal Term As Long, ByVal StrPtr As String, ByVal StrLen As Long) As Long
Declare Function lsStrToTerm Lib "amzi.dll" Alias "lsStrToTermA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsStrTermLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long

Declare Function lsMakeAtom Lib "amzi.dll" Alias "lsMakeAtomA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsMakeStr Lib "amzi.dll" Alias "lsMakeStrA" (ByVal EngID As Long, TermPtr As Long, ByVal StrPtr As String) As Long
Declare Function lsMakeInt Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal num As Long) As Long
Declare Function lsMakeFloat Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal num As Double) As Long
Declare Function lsMakeAddr Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, Ptr As Any) As Long

Declare Function lsGetTermType Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long
Declare Function lsGetTerm Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsGetFA Lib "amzi.dll" Alias "lsGetFAA" (ByVal EngID As Long, ByVal Term As Long, ByVal Functor As String, Arity As Integer) As Long
Declare Function lsMakeFA Lib "amzi.dll" Alias "lsMakeFAA" (ByVal EngID As Long, TermPtr As Long, ByVal Functor As String, ByVal Arity As Integer) As Long
Declare Function lsGetArg Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetArgType Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long) As Integer
Declare Function lsStrArgLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal ArgNum As Long) As Long
Declare Function lsUnify Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal Term As Long) As Long
Declare Function lsUnifyArg Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal ArgNum As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsMakeList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long) As Long
Declare Function lsPushList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal Term As Long) As Long
Declare Function lsPopList Lib "amzi.dll" (ByVal EngID As Long, TermPtr As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetHead Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetTail Lib "amzi.dll" (ByVal EngID As Long, ByVal Term As Long) As Long

Declare Function lsGetVersion Lib "amzi.dll" Alias "lsGetVersionA" (ByVal EngID As Long, ByVal StrPtr As String) As Long

Declare Function lsGetParm Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long, ByVal BType As Integer, Ptr As Any) As Long
Declare Function lsGetParmType Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long) As Long
Declare Function lsStrParmLen Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long) As Long
Declare Function lsUnifyParm Lib "amzi.dll" (ByVal EngID As Long, ByVal Param As Long, ByVal BType As Integer, Ptr As Any) As Long

Declare Function lsSetStream Lib "amzi.dll" (ByVal EngID As Long, ByVal Stream As Integer, ByVal Handle As Long) As Long
Declare Function lsGetStream Lib "amzi.dll" (ByVal EngID As Long, ByVal Stream As Integer) As Long

Declare Sub lsGetExceptMsg Lib "amzi.dll" Alias "lsGetExceptMsgA" (ByVal EngID As Long, ByVal StrPtr As String, ByVal StrLen As Long)
Declare Function lsGetExceptRC Lib "amzi.dll" (ByVal EngID As Long) As Long
Declare Sub lsGetExceptReadBuffer Lib "amzi.dll" Alias "lsGetExceptReadBufferA" (ByVal EngID As Long, ByVal StrPtr As String, StrLen As Long)
Declare Function lsErrRaise Lib "amzi.dll" Alias "lsErrRaiseA" (ByVal EngID As Long, ByVal StrPtr As String) As Long

#End If

Public Sub AssertaLS(ByVal Term As Long)
    Dim rc As Long

    rc = lsAsserta(EngineID, Term)
    If (rc <> 0) Then
        Call ErrorHandler("lsAsserta", rc)
    End If

End Sub

Public Sub AssertaStrLS(ByVal StrPtr As String)
    Dim rc As Long

    rc = lsAssertaStr(EngineID, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsAssertaStr", rc)
    End If

End Sub

Public Sub AssertzLS(ByVal Term As Long)
    Dim rc As Long

    rc = lsAssertz(EngineID, Term)
    If (rc <> 0) Then
        Call ErrorHandler("lsAssertz", rc)
    End If

End Sub

Public Sub AssertzStrLS(ByVal StrPtr As String)
    Dim rc As Long

    rc = lsAssertzStr(EngineID, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsAssertzStr", rc)
    End If

End Sub

Public Function CallLS(TermPtr As Long) As Boolean
    Dim tf As Long

    tf = lsCall(EngineID, TermPtr)
    Select Case tf
        Case 0
            CallLS = False
        Case 1
            CallLS = True
        Case Else
            Call ErrorHandler("lsCall", tf)
    End Select

End Function

Public Function CallStrLS(TermPtr As Long, ByVal StrPtr As String) As Boolean
    Dim tf As Long

    tf = lsCallStr(EngineID, TermPtr, StrPtr + Chr$(0))
    Select Case tf
        Case 0
            CallStrLS = False
        Case 1
            CallStrLS = True
        Case Else
            Call ErrorHandler("lsCallStr", tf)
    End Select

End Function

Public Sub ClearCallLS()
    Dim rc As Long

    rc = lsClearCall(EngineID)
    If (rc <> 0) Then
        Call ErrorHandler("lsClearCall", rc)
    End If

End Sub

Public Sub CloseLS()
    Dim rc As Long
    
    If EngineID <> 0 Then

        rc = lsClose(EngineID)
        If (rc <> 0) Then
            MsgBox "Error #" + str$(rc) + " calling lsClose"
        End If
    End If
    EngineID = 0

End Sub
Public Function GetExceptMsgLS() As String
    Dim tstr As String
    Dim l As Long

    tstr = Space$(MaxStrLen)
    l = MaxStrLen
    Call lsGetExceptMsg(EngineID, ByVal tstr, l)
    
    GetExceptMsgLS = StripStr(tstr)

End Function

Private Sub ErrorHandler(ByVal module As String, ByVal ErrCode As Long)
    Static errmsg As String
    Static l As Long
    Dim ans As Integer

    MaxStrLen = 32000
    errmsg = Space$(MaxStrLen)
    l = MaxStrLen
    Call lsGetExceptMsg(engineid, errmsg, l)
    errmsg = StripStr(errmsg)
        
    If ErrCode = -1 Then
        errmsg = "Amzi! engine has shutdown. Restart application to restart ARulesXL. (" & module & ")"
    End If
    
    Err.Raise Number:=vbObjectError + ErrCode, Source:="ARulesXL", Description:=errmsg
      
End Sub

Public Function GetExceptReadBufferLS() As String
    Dim StrPtr As String
    Dim l As Long
    
    StrPtr = Space$(MaxStrLen)
    l = MaxStrLen
    Call lsGetExceptReadBuffer(EngineID, ByVal StrPtr, l)
    
    GetExceptReadBufferLS = StripStr(StrPtr)

End Function

Public Function ExecLS(TermPtr As Long) As Boolean
    Dim tf As Long

    tf = lsExec(EngineID, TermPtr)
    Select Case tf
        Case 0
            ExecLS = False
        Case 1
            ExecLS = True
        Case Else
            Call ErrorHandler("lsExec", tf)
    End Select

End Function

Public Function ExecStrLS(TermPtr As Long, ByVal StrPtr) As Boolean
    Dim tf As Long

    tf = lsExecStr(EngineID, TermPtr, StrPtr + Chr$(0))
    Select Case tf
        Case 0
            ExecStrLS = False
        Case 1
            ExecStrLS = True
        Case Else
            Call ErrorHandler("lsExecStr", tf)
    End Select

End Function

Public Function ExecStrThLS(TermPtr As Long, ByVal StrPtr As String) As Boolean
    Dim tf As Long

    tf = lsExecStrTh(engineid, TermPtr, StrPtr + Chr$(0))
    Select Case tf
        Case 0
            ExecStrThLS = False
        Case 1
            ExecStrThLS = True
        Case Else
            Call ErrorHandler("lsExecStrTh", tf)
    End Select

End Function

Public Sub PutActionLS(ByVal StrPtr As String)
    Dim rc As Long

    rc = lsPutAction(engineid, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsPutAction", rc)
    End If

End Sub

Public Function GetActionStateLS() As Integer
    Dim rc As Long

    rc = lsGetActionState(engineid)
    ' rc is overloaded to return a 1 if it's safe to issue Prolog execs,
    ' or 0 if it's not
    ' 0 not ready
    ' 1 prolog trace running waiting for input
    ' 2 prolog trace done
    GetActionStateLS = rc
'    Select Case rc
'        Case 0
'            GetActionStateLS = False
'        Case 1
'            GetActionStateLS = True
'        Case Else
'            Call ErrorHandler("lsGetActionState", rc)
'    End Select

End Function

Public Sub GetArgLS(ByVal Term As Long, ByVal ArgNum As Long, ByVal BType As Integer, Ptr As Variant)
    Dim rc As Long, tstr As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double
    Dim s As String
    
    Select Case BType
        Case bSTR, bATOM
            tstr = Space$(MaxStrLen)
            rc = lsGetArg(EngineID, Term, ArgNum, BType, ByVal tstr)
            Ptr = StripStr(tstr)
        Case bLONG, bADDR, bTERM
            rc = lsGetArg(EngineID, Term, ArgNum, BType, tlong)
            Ptr = tlong
        Case bINT, bSHORT
            rc = lsGetArg(EngineID, Term, ArgNum, BType, tint)
            Ptr = tint
        Case bFLOAT
            rc = lsGetArg(EngineID, Term, ArgNum, BType, tfloat)
            Ptr = tfloat
        Case bDOUBLE
            rc = lsGetArg(EngineID, Term, ArgNum, BType, tdouble)
            Ptr = tdouble
    End Select

    If (rc <> 0) Then
        Call ErrorHandler("lsGetArg", rc)
    End If
    
End Sub

Public Function GetArgTypeLS(ByVal Term As Long, ByVal ArgNum As Long) As Integer
    GetArgTypeLS = lsGetArgType(EngineID, Term, ArgNum)
End Function

Public Function GetArityLS(ByVal Term As Long) As Integer
    Dim rc As Long, tstr As String, tint As Integer
    
    tstr = Space$(MaxStrLen)
    rc = lsGetFA(EngineID, Term, ByVal tstr, tint)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetFA", rc)
    End If

    GetArityLS = tint

End Function

Public Sub GetFALS(ByVal Term As Long, StrPtr As String, ArityPtr As Integer)
    Dim rc As Long
    
    StrPtr = Space$(MaxStrLen)
    rc = lsGetFA(EngineID, Term, ByVal StrPtr, ArityPtr)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetFA", rc)
    End If

    StrPtr = StripStr(StrPtr)

End Sub

Public Function GetFloatArgLS(ByVal Term As Long, ByVal ArgNum As Long) As Double
    Dim rc As Long, tfloat As Double

    rc = lsGetArg(EngineID, Term, ArgNum, bDOUBLE, tfloat)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetArg", rc)
    End If
    
    GetFloatArgLS = tfloat

End Function

Public Function GetFloatTermLS(ByVal Term As Long) As Double
    Dim rc As Long, tfloat As Double

    rc = lsGetTerm(EngineID, Term, bDOUBLE, tfloat)

    If (rc <> 0) Then
        Call ErrorHandler("lsGetTerm", rc)
    End If

    GetFloatTermLS = tfloat

End Function

Public Function GetFunctorLS(ByVal Term As Long) As String
    Dim rc As Long, tstr As String, tint As Integer
    
    tstr = Space$(MaxStrLen)
    rc = lsGetFA(EngineID, Term, ByVal tstr, tint)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetFA", rc)
    End If

    GetFunctorLS = StripStr(tstr)

End Function

Public Function GetIntArgLS(ByVal Term As Long, ByVal ArgNum As Long) As Integer
    Dim rc As Long, tint As Integer

    rc = lsGetArg(EngineID, Term, ArgNum, bINT, tint)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetArg", rc)
    End If
    
    GetIntArgLS = tint

End Function

Public Function GetIntTermLS(ByVal Term As Long) As Integer
    Dim rc As Long, tint As Integer

    rc = lsGetTerm(EngineID, Term, bINT, tint)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetTerm", rc)
    End If

    GetIntTermLS = tint

End Function

Public Function GetLongArgLS(ByVal Term As Long, ByVal ArgNum As Long) As Long
    Dim rc As Long, tlong As Long

    rc = lsGetArg(EngineID, Term, ArgNum, bLONG, tlong)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetArg", rc)
    End If
    
    GetLongArgLS = tlong

End Function

Public Function GetLongTermLS(ByVal Term As Long) As Long
    Dim rc As Long, tlong As Long

    rc = lsGetTerm(EngineID, Term, bLONG, tlong)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetTerm", rc)
    End If

    GetLongTermLS = tlong

End Function

Public Function GetStrArgLS(ByVal Term As Long, ByVal ArgNum As Long) As String
    Dim rc As Long, tstr As String
    Dim Length As Long

    Length = lsStrArgLen(EngineID, Term, ArgNum)
    tstr = Space$(Length + 10)

    rc = lsGetArg(engineid, Term, ArgNum, bSTR, ByVal tstr)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetArg", rc)
    End If
    
    GetStrArgLS = StripStr(tstr)

End Function


Public Function GetStreamLS(ByVal Stream As Integer) As Long
    GetStreamLS = lsGetStream(EngineID, Stream)
End Function

Public Function GetStrTermLS(ByVal Term As Long) As String
    Dim rc As Long, tstr As String

    tstr = Space$(MaxStrLen)
    rc = lsGetTerm(EngineID, Term, bSTR, ByVal tstr)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetTerm", rc)
    End If

    GetStrTermLS = StripStr(tstr)

End Function

Public Sub GetTermLS(ByVal Term As Long, ByVal BType As Integer, Ptr As Variant)
    Dim rc As Long, tstr As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Space$(MaxStrLen)
            rc = lsGetTerm(EngineID, Term, BType, ByVal tstr)
            Ptr = StripStr(tstr)
        Case bLONG, bADDR, bTERM
            rc = lsGetTerm(EngineID, Term, BType, tlong)
            Ptr = tlong
        Case bINT, bSHORT
            rc = lsGetTerm(EngineID, Term, BType, tint)
            Ptr = tint
        Case bFLOAT
            rc = lsGetTerm(EngineID, Term, BType, tfloat)
            Ptr = tfloat
        Case bDOUBLE
            rc = lsGetTerm(EngineID, Term, BType, tdouble)
            Ptr = tdouble
    End Select
    
    If (rc <> 0) Then
        Call ErrorHandler("lsGetTerm", rc)
    End If

End Sub

Public Function GetTermTypeLS(ByVal Term As Long) As Integer
    GetTermTypeLS = lsGetTermType(EngineID, Term)
End Function

Public Function GetVersionLS() As String
    Dim rc As Long, tstr As String

    tstr = Space$(MaxStrLen)
    rc = lsGetVersion(EngineID, ByVal tstr)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetVersion", rc)
    End If
    
    GetVersionLS = StripStr(tstr)

End Function

Public Sub InitLS(ByVal INIFile As String)
    Dim rc As Long

    If IsEmpty(MaxStrLen) Then
        MaxStrLen = 32000
    End If

    If IsEmpty(ErrorMethod) Then
        ErrorMethod = 0    ' not checked anyway
    End If

    If EngineID <> 0 Then Call ErrorHandler("lsInit x 2", -1)
        
    rc = lsInit(EngineID, INIFile + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsInit", rc)
    End If

End Sub

Public Sub ErrRaiseLS(ByVal StrPtr As String)
    Dim rc As Long

    rc = lsErrRaise(EngineID, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsErrRaise", rc)
    End If

End Sub

Public Sub InitLSX()
    Dim rc As Long

    rc = lsInitLSX(EngineID, 0)
    If (rc <> 0) Then
        Call ErrorHandler("lsInitLSX", rc)
    End If

End Sub

Public Sub AddLSX(ByVal LSXFile As String)
    Dim rc As Long

    rc = lsAddLSX(EngineID, LSXFile + Chr$(0), 0)
    If (rc <> 0) Then
        Call ErrorHandler("lsAddLSX", rc)
    End If

End Sub

Public Sub LoadLS(ByVal XPLFile As String)
    Dim rc As Long

    rc = lsLoad(EngineID, XPLFile + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsLoad", rc)
    End If

End Sub

Public Sub LoadFromMemoryLS(ByVal File As String, Length As Long, Bytes() As Byte)
    Dim rc As Long
    
    ' Must pass first byte of array with as any to pass the address of the array
    rc = lsLoadFromMemory(engineid, File, Length, Bytes(0))
        If (rc <> 0) Then
        Call ErrorHandler("lsLoadFromMemory", rc)
    End If
End Sub

Public Function MainLS() As Boolean
    Dim tf As Long

    tf = lsMain(EngineID)
    Select Case tf
        Case 0
            MainLS = False
        Case 1
            MainLS = True
        Case Else
            Call ErrorHandler("lsMain", tf)
    End Select

End Function

Public Sub MakeAddrLS(TermPtr As Long, Ptr As Variant)
    Dim rc As Long

    rc = lsMakeAddr(EngineID, TermPtr, Ptr)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeAddr", rc)
    End If

End Sub

Public Sub MakeAtomLS(TermPtr As Long, ByVal StrPtr As String)
    Dim rc As Long

    rc = lsMakeAtom(EngineID, TermPtr, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeAtom", rc)
    End If

End Sub

Public Sub MakeFALS(Term As Long, ByVal Functor As String, ByVal Arity As Integer)
    Dim rc As Long

    rc = lsMakeFA(EngineID, Term, Functor + Chr$(0), Arity)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeFA", rc)
    End If

End Sub

Public Sub MakeFloatLS(TermPtr As Long, ByVal FloatVal As Double)
    Dim rc As Long

    rc = lsMakeFloat(EngineID, TermPtr, FloatVal)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeFloat", rc)
    End If

End Sub

Public Sub MakeIntLS(TermPtr As Long, ByVal IntVal As Integer)
    Dim rc As Long

    rc = lsMakeInt(EngineID, TermPtr, IntVal)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeInt", rc)
    End If

End Sub

Public Sub MakeListLS(TermPtr As Long)
    Dim rc As Long

    rc = lsMakeList(EngineID, TermPtr)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeList", rc)
    End If

End Sub

Public Sub MakeLongLS(TermPtr As Long, ByVal LongVal As Long)
    Dim rc As Long

    rc = lsMakeInt(EngineID, TermPtr, LongVal)
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeInt", rc)
    End If

End Sub

Public Sub MakeStrLS(TermPtr As Long, ByVal StrPtr As String)
    Dim rc As Long

    rc = lsMakeStr(EngineID, TermPtr, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsMakeStr", rc)
    End If

End Sub

Public Function PopFloatListLS(TermPtr As Long, FloatVal As Double) As Long
    Dim rc As Long

    rc = lsPopList(EngineID, TermPtr, bDOUBLE, FloatVal)
    
    Select Case rc
        Case 0
            PopFloatListLS = rc
        Case -1
            PopFloatListLS = rc
        Case Else
            Call ErrorHandler("lsPopList", rc)
    End Select


End Function

Public Function PopIntListLS(TermPtr As Long, IntVal As Integer) As Integer
    Dim rc As Long

    rc = lsPopList(EngineID, TermPtr, bINT, IntVal)
    
    Select Case rc
        Case 0
            PopIntListLS = rc
        Case -1
            PopIntListLS = rc
        Case Else
            Call ErrorHandler("lsPopList", rc)
    End Select

End Function

Public Function PopListLS(TermPtr As Long, ByVal BType As Integer, Ptr As Variant) As Long
    Dim rc As Long, tstr As String, tstr2 As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Space$(MaxStrLen)
            rc = lsPopList(EngineID, TermPtr, BType, ByVal tstr)
            Ptr = StripStr(tstr)
        Case bLONG, bADDR, bTERM
            rc = lsPopList(EngineID, TermPtr, BType, tlong)
            Ptr = tlong
        Case bINT, bSHORT
            rc = lsPopList(EngineID, TermPtr, BType, tint)
            Ptr = tint
        Case bFLOAT
            rc = lsPopList(EngineID, TermPtr, BType, tfloat)
            Ptr = tfloat
        Case bDOUBLE
            rc = lsPopList(EngineID, TermPtr, BType, tdouble)
            Ptr = tdouble
    End Select
    
    Select Case rc
        Case 0
            PopListLS = rc
        Case -1
            PopListLS = rc
        Case Else
            Call ErrorHandler("lsPopList", rc)
    End Select

End Function

Public Function PopLongListLS(TermPtr As Long, LongVal As Long) As Long
    Dim rc As Long

    rc = lsPopList(EngineID, TermPtr, bLONG, LongVal)
    
    Select Case rc
        Case 0
            PopLongListLS = rc
        Case -1
            PopLongListLS = rc
        Case Else
            Call ErrorHandler("lsPopList", rc)
    End Select


End Function

Public Function PopStrListLS(TermPtr As Long, StrPtr As String) As Long
    Dim rc As Long

    StrPtr = Space$(MaxStrLen)
    rc = lsPopList(EngineID, TermPtr, bSTR, ByVal StrPtr)

    Select Case rc
        Case 0
            PopStrListLS = rc
        Case -1
            PopStrListLS = rc
        Case Else
            Call ErrorHandler("lsPopList", rc)
    End Select

    StrPtr = StripStr(StrPtr)

End Function

Public Function GetFloatHeadLS(ByVal Term As Long, FloatVal As Double) As Long
    Dim rc As Long

    rc = lsGetHead(EngineID, Term, bDOUBLE, FloatVal)
    
    Select Case rc
        Case 0
            GetFloatHeadLS = rc
        Case -1
            GetFloatHeadLS = rc
        Case Else
            Call ErrorHandler("lsGetHead", rc)
    End Select


End Function

Public Function GetIntHeadLS(ByVal Term As Long, IntVal As Integer) As Long
    Dim rc As Long

    rc = lsGetHead(EngineID, Term, bINT, IntVal)
    
    Select Case rc
        Case 0
            GetIntHeadLS = rc
        Case -1
            GetIntHeadLS = rc
        Case Else
            Call ErrorHandler("lsGetHead", rc)
    End Select

End Function

Public Function GetHeadLS(ByVal Term As Long, ByVal BType As Integer, Ptr As Variant) As Long
    Dim rc As Long, tstr As String, tstr2 As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Space$(MaxStrLen)
            rc = lsGetHead(EngineID, Term, BType, ByVal tstr)
            Ptr = StripStr(tstr)
        Case bLONG, bADDR, bTERM
            rc = lsGetHead(EngineID, Term, BType, tlong)
            Ptr = tlong
        Case bINT, bSHORT
            rc = lsGetHead(EngineID, Term, BType, tint)
            Ptr = tint
        Case bFLOAT
            rc = lsGetHead(EngineID, Term, BType, tfloat)
            Ptr = tfloat
        Case bDOUBLE
            rc = lsGetHead(EngineID, Term, BType, tdouble)
            Ptr = tdouble
    End Select
    
    Select Case rc
        Case 0
            GetHeadLS = rc
        Case -1
            GetHeadLS = rc
        Case Else
            Call ErrorHandler("lsGetHead", rc)
    End Select

End Function

Public Function GetLongHeadLS(ByVal Term As Long, LongVal As Long) As Long
    Dim rc As Long

    rc = lsGetHead(EngineID, Term, bLONG, LongVal)
    
    Select Case rc
        Case 0
            GetLongHeadLS = rc
        Case -1
            GetLongHeadLS = rc
        Case Else
            Call ErrorHandler("lsGetHead", rc)
    End Select


End Function

Public Function GetStrHeadLS(ByVal Term As Long, StrPtr As String) As Long
    Dim rc As Long

    StrPtr = Space$(MaxStrLen)
    rc = lsGetHead(EngineID, Term, bSTR, ByVal StrPtr)

    Select Case rc
        Case 0
            GetStrHeadLS = rc
        Case -1
            GetStrHeadLS = rc
        Case Else
            Call ErrorHandler("lsGetHead", rc)
    End Select

    StrPtr = StripStr(StrPtr)

End Function

Public Function GetTailLS(ByVal Term As Long) As Long
    GetTailLS = lsGetTail(EngineID, Term)
'    If GetTailLS = 0 Then
'        Call ErrorHandler("lsGetTail", 0)
'    End If
End Function

Public Sub PushListLS(TermPtr As Long, ByVal Term As Long)
    Dim rc As Long

    rc = lsPushList(EngineID, TermPtr, Term)
    If (rc <> 0) Then
        Call ErrorHandler("lsPushList", rc)
    End If

End Sub

Public Function RedoLS() As Boolean
    Dim tf As Long

    tf = lsRedo(EngineID)
    Select Case tf
        Case 0
            RedoLS = False
        Case 1
            RedoLS = True
        Case Else
            Call ErrorHandler("lsRedo", tf)
    End Select

End Function

Public Sub ResetLS()
    Dim rc As Long

    rc = lsReset(EngineID)
    If (rc <> 0) Then
        Call ErrorHandler("lsReset", rc)
    End If

End Sub

Public Function RetractLS(ByVal Term As Long) As Boolean
    Dim tf As Long

    tf = lsRetract(EngineID, Term)
    Select Case tf
        Case 0
            RetractLS = False
        Case 1
            RetractLS = True
        Case Else
            Call ErrorHandler("lsRectract", tf)
    End Select

End Function

Public Function RetractStrLS(ByVal StrPtr As String) As Boolean
    Dim tf As Long

    tf = lsRetractStr(EngineID, StrPtr + Chr$(0))
    Select Case tf
        Case 0
            RetractStrLS = False
        Case 1
            RetractStrLS = True
        Case Else
            Call ErrorHandler("lsRectractStr", tf)
    End Select

End Function

Public Sub SetErrorHandlerLS(ByVal EMethod As String)

    EMethod = UCase$(EMethod)

    Select Case EMethod
        Case "MESSAGEBOX"
            ErrorMethod = 2
        Case "ERRORCODE"
            ErrorMethod = 1
        Case "ERRORRAISE"
            ErrorMethod = 0
        Case Else
            ErrorMethod = 0
    End Select
              
End Sub

Public Sub SetMaxStrLenLS(ByVal num As Long)
    MaxStrLen = num
End Sub

Public Sub SetStreamLS(ByVal Stream As Integer, ByVal Handle As Long)
    Dim rc As Long

    rc = lsSetStream(EngineID, Stream, Handle)
    If (rc <> 0) Then
        Call ErrorHandler("lsSetStream", rc)
    End If

End Sub

Public Function StrArgLenLS(ByVal Term As Long, ByVal ArgNum As Long) As Long
    StrArgLenLS = lsStrArgLen(EngineID, Term, ArgNum)
End Function

Private Function StripStr(ByVal StrPtr As String)
    StrPtr = RTrim$(StrPtr)
    If Len(StrPtr) > 0 Then
        StripStr = Mid$(StrPtr, 1, Len(StrPtr) - 1)
    Else
        StripStr = ""
    End If
End Function

Public Sub StrToTermLS(TermPtr As Long, ByVal StrPtr As String)
    Dim rc As Long

    rc = lsStrToTerm(EngineID, TermPtr, StrPtr + Chr$(0))
    If (rc <> 0) Then
        Call ErrorHandler("lsStrToTerm", rc)
    End If

End Sub

Public Sub TermToStrLS(ByVal Term As Long, StrPtr As String, ByVal StrLen As Long)
    Dim rc As Long

    StrPtr = Space$(StrLen)
    rc = lsTermToStr(EngineID, Term, ByVal StrPtr, StrLen)
    If (rc <> 0) Then
        Call ErrorHandler("lsTermToStr", rc)
    End If

    StrPtr = StripStr(StrPtr)

End Sub

Public Sub TermToStrQLS(ByVal Term As Long, StrPtr As String, ByVal StrLen As Long)
    Dim rc As Long

    StrPtr = Space$(StrLen)
    rc = lsTermToStrQ(EngineID, Term, ByVal StrPtr, StrLen)
    If (rc <> 0) Then
        Call ErrorHandler("lsTermToStrQ", rc)
    End If

    StrPtr = StripStr(StrPtr)

End Sub

Public Function StringFromTermLS(ByVal term As Long) As String
    Dim rc As Long
    Dim str As String
    str = Space$(512)
    rc = lsTermToStr(engineid, term, ByVal str, 512)
    If (rc <> 0) Then
        Call ErrorHandler("StrFromTermLS", rc)
    End If
    
    StringFromTermLS = StripStr(str)
End Function

Public Function UnifyArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal BType As Integer, ByVal Ptr As Variant) As Boolean
    Dim tf As Long, tstr As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Ptr
            tf = lsUnifyArg(EngineID, TermPtr, ArgNum, BType, ByVal tstr)
        Case bLONG, bADDR, bTERM
            tlong = Ptr
            tf = lsUnifyArg(EngineID, TermPtr, ArgNum, BType, tlong)
        Case bINT, bSHORT
            tint = Ptr
            tf = lsUnifyArg(EngineID, TermPtr, ArgNum, BType, tint)
        Case bFLOAT
            tfloat = Ptr
            tf = lsUnifyArg(EngineID, TermPtr, ArgNum, BType, tfloat)
        Case bDOUBLE
            tdouble = Ptr
            tf = lsUnifyArg(EngineID, TermPtr, ArgNum, BType, tdouble)
    End Select
    
    Select Case tf
        Case 0
            UnifyArgLS = False
        Case 1
            UnifyArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function UnifyAtomArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal StrPtr As String) As Boolean
    Dim tf As Long

    tf = lsUnifyArg(EngineID, TermPtr, ArgNum, bATOM, ByVal StrPtr + Chr$(0))
    Select Case tf
        Case 0
            UnifyAtomArgLS = False
        Case 1
            UnifyAtomArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function UnifyFloatArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal FloatVal As Double) As Boolean
    Dim tf As Long

    tf = lsUnifyArg(EngineID, TermPtr, ArgNum, bDOUBLE, FloatVal)
    Select Case tf
        Case 0
            UnifyFloatArgLS = False
        Case 1
            UnifyFloatArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function UnifyIntArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal IntVal As Integer) As Boolean
    Dim tf As Long

    tf = lsUnifyArg(EngineID, TermPtr, ArgNum, bINT, IntVal)
    Select Case tf
        Case 0
            UnifyIntArgLS = False
        Case 1
            UnifyIntArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function UnifyLongArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal LongVal As Long) As Boolean
    Dim tf As Long

    tf = lsUnifyArg(EngineID, TermPtr, ArgNum, bLONG, LongVal)
    Select Case tf
        Case 0
            UnifyLongArgLS = False
        Case 1
            UnifyLongArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function UnifyLS(ByVal Term1 As Long, ByVal Term2 As Long) As Long
    Dim tf As Long

    tf = lsUnify(EngineID, Term1, Term2)
    Select Case tf
        Case 0
            UnifyLS = False
        Case 1
            UnifyLS = True
        Case Else
            Call ErrorHandler("lsUnify", tf)
    End Select

End Function

Public Function UnifyStrArgLS(TermPtr As Long, ByVal ArgNum As Long, ByVal StrPtr As String) As Boolean
    Dim tf As Long

    tf = lsUnifyArg(EngineID, TermPtr, ArgNum, bSTR, ByVal StrPtr + Chr$(0))
    Select Case tf
        Case 0
            UnifyStrArgLS = False
        Case 1
            UnifyStrArgLS = True
        Case Else
            Call ErrorHandler("lsUnifyArg", tf)
    End Select

End Function

Public Function StrTermLenLS(ByVal Term As Long) As Long
    StrTermLenLS = lsStrTermLen(EngineID, Term)
End Function

Public Sub SetCurrentEngineLS(NewEngID As Long)
    EngineID = NewEngID
End Sub

Public Function GetCurrentEngineLS() As Long
    GetCurrentEngineLS = EngineID
End Function

Public Sub AddPredLS(ByVal Predname As String, ByVal Arity As Integer, ByVal pfunc As Long)
    Dim rc As Long

    rc = lsAddPred(EngineID, Predname + Chr$(0), Arity, pfunc, EngineID)
    
    If (rc <> 0) Then
        Call ErrorHandler("lsAddPred", rc)
    End If
End Sub

Public Function UnifyParmLS(ByVal Param As Long, ByVal BType As Integer, ByVal Ptr As Variant) As Boolean
    Dim tf As Long, tstr As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Ptr
            tf = lsUnifyParm(EngineID, Param, BType, ByVal tstr)
        Case bLONG, bADDR, bTERM
            tlong = Ptr
            tf = lsUnifyParm(EngineID, Param, BType, tlong)
        Case bINT, bSHORT
            tint = Ptr
            tf = lsUnifyParm(EngineID, Param, BType, tint)
        Case bFLOAT
            tfloat = Ptr
            tf = lsUnifyParm(EngineID, Param, BType, tfloat)
        Case bDOUBLE
            tdouble = Ptr
            tf = lsUnifyParm(EngineID, Param, BType, tdouble)
    End Select
    
    Select Case tf
        Case 0
            UnifyParmLS = False
        Case 1
            UnifyParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select

End Function

Public Function UnifyStrParmLS(ByVal Param As Long, ByVal Str As String) As Boolean
    Dim tf As Long, tstr As String
    
    tf = lsUnifyParm(EngineID, Param, bSTR, ByVal Str + Chr$(0))
    Select Case tf
        Case 0
            UnifyStrParmLS = False
        Case 1
            UnifyStrParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select
    
End Function

Public Function UnifyAtomParmLS(ByVal Param As Long, ByVal Str As String) As Boolean
    Dim tf As Long, tstr As String
    
    tf = lsUnifyParm(EngineID, Param, bATOM, ByVal Str + Chr$(0))
    Select Case tf
        Case 0
            UnifyAtomParmLS = False
        Case 1
            UnifyAtomParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select
    
End Function

Public Function UnifyIntParmLS(ByVal Param As Long, ByVal IntVal As Integer) As Boolean
    Dim tf As Long

    tf = lsUnifyParm(EngineID, Param, bINT, IntVal)
    Select Case tf
        Case 0
            UnifyIntParmLS = False
        Case 1
            UnifyIntParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select

End Function

Public Function UnifyLongParmLS(ByVal Param As Long, ByVal LongVal As Long) As Boolean
    Dim tf As Long

    tf = lsUnifyParm(EngineID, Param, bLONG, LongVal)
    Select Case tf
        Case 0
            UnifyLongParmLS = False
        Case 1
            UnifyLongParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select

End Function

Public Function UnifyFloatParmLS(ByVal Param As Long, ByVal FloatVal As Single) As Boolean
    Dim tf As Long

    tf = lsUnifyParm(EngineID, Param, bFLOAT, FloatVal)
    Select Case tf
        Case 0
            UnifyFloatParmLS = False
        Case 1
            UnifyFloatParmLS = True
        Case Else
            Call ErrorHandler("lsUnifyParm", tf)
    End Select

End Function

Public Sub GetParmLS(ByVal Param As Long, ByVal BType As Integer, Ptr As Variant)
    Dim rc As Long, tstr As String
    Dim tlong As Long, tfloat As Single, tint As Integer, tdouble As Double

    Select Case BType
        Case bSTR, bATOM
            tstr = Space$(MaxStrLen)
            rc = lsGetParm(EngineID, Param, BType, ByVal tstr)
            Ptr = StripStr(tstr)
        Case bLONG, bADDR, bTERM
            rc = lsGetParm(EngineID, Param, BType, tlong)
            Ptr = tlong
        Case bINT, bSHORT
            rc = lsGetParm(EngineID, Param, BType, tint)
            Ptr = tint
        Case bFLOAT
            rc = lsGetParm(EngineID, Param, BType, tfloat)
            Ptr = tfloat
        Case bDOUBLE
            rc = lsGetParm(EngineID, Param, BType, tdouble)
            Ptr = tdouble
    End Select
    
End Sub

Public Function GetStrParmLS(ByVal Param As Long) As String
    Dim rc As Long, tstr As String
    
    tstr = Space$(MaxStrLen)
    rc = lsGetParm(EngineID, Param, bSTR, ByVal tstr)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetParm", rc)
    End If
    
    GetStrParmLS = StripStr(tstr)

End Function

Public Function GetIntParmLS(ByVal Param As Long) As Integer
    Dim rc As Long, tint As Integer
        
    rc = lsGetParm(EngineID, Param, bINT, tint)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetParm", rc)
    End If
    
    GetIntParmLS = tint

End Function

Public Function GetLongParmLS(ByVal Param As Long) As Long
    Dim rc As Long, tlong As Long
        
    rc = lsGetParm(EngineID, Param, bLONG, tlong)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetParm", rc)
    End If
    
    GetLongParmLS = tlong

End Function

Public Function GetFloatParmLS(ByVal Param As Long) As Single
    Dim rc As Long, tfloat As Single
        
    rc = lsGetParm(EngineID, Param, bFLOAT, tfloat)
    If (rc <> 0) Then
        Call ErrorHandler("lsGetParm", rc)
    End If
    
    GetFloatParmLS = tfloat

End Function

Public Function StrParmLenLS(ByVal ParmNum As Long) As Long
    StrParmLenLS = lsStrParmLen(EngineID, ParmNum)
End Function

Public Function GetParmTypeLS(ByVal Term As Long, ByVal ParmNum As Long) As Integer
    GetParmTypeLS = lsGetParmType(EngineID, ParmNum)
End Function



