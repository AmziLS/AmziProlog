{
  AMZI.PAS - Delphi DLL Logic Server API Definitions and Delphi Cover Functions

  Copyright (c) 1994-2021 Amzi! inc. All Rights Reserved.
}

unit Amzi;

interface

uses
  SysUtils, Windows, Classes;

const
  lsFalse: Integer = 0;
  lsTrue: Integer = 1;

type
  ELogicServer = class(Exception);

  { Various types used by the Logic Server API calls }
  TTerm = Pointer;  { The basic Prolog term }
  { Enumerated Prolog types and enumerated Delphi types, used for mapping
    Prolog types to Delphi types }
  TPType = (pATOM, pINT, pSTR, pFLOAT, pSTRUCT, pLIST, pTERM, pADDR, pVAR, pWSTR, pWATOM, pREAL);
  TDType = (dATOM, dSTR, dINT, dLONG, dSHORT, dFLOAT, dDOUBLE, dADDR, dTERM, dWSTR, dWATOM, dMOD, dGOAL);
  TTypeInt = Integer; { Generic type for casting types in DLL calls }
  { Enumerated stream identifier, used when redirecting Prolog I/O }
  TPStream = (CUR_IN, CUR_OUT, CUR_ERR, USER_IN, USER_OUT, USER_ERR);
  TPStreamInt = Integer; { Generic type for stream identifiers in DLL calls}
  TTFi = Integer;  { Prolog T/F or error code return code }
  TRC = Integer;  { Integer return code }
  TArity = Word;  { The arity of a functor }
  TEngID = Pointer;  { ID for Engine, only one allowed now }
  TExtPred = function (EngID: TEngID): TTFi; stdcall; { An extended predicate function }

  TPutC = procedure (P: Pointer; C: Integer); stdcall;
  TPutSA = procedure (P: Pointer; S: PAnsiChar); stdcall;
  TPutSW = procedure (P: Pointer; S: PWideChar); stdcall;
  TPutS = procedure (P: Pointer; S: PChar); stdcall;
  TGetC = function (P: Pointer): Integer; stdcall;
  TUngetC = procedure (P: Pointer); stdcall;

  TPredInitA = record
    PName: PAnsiChar;
    PArity: TArity;
    PFunc: TExtPred;
  end;
  TPredInitPtrA = ^TPredInitA;
  TPredInitW = record
    PName: PWideChar;
    PArity: TArity;
    PFunc: TExtPred;
  end;
  TPredInitPtrW = ^TPredInitW;
{$IFDEF UNICODE}
  TPredInit = TPredInitW;
  TPredInitPtr = TPredInitPtrW;
{$ELSE}
  TPredInit = TPredInitA;
  TPredInitPtr = TPredInitPtrA;
{$ENDIF}

  { The Logic Server component, a class that encapsulates all of the API
    calls as methods }
  TLSEngine = class(TComponent)
  private
    FEng: TEngID;
    FRC: TRC;
    FInitializedB, FCreatedB: BOOL;
{$IFDEF UNICODE}
    FWideBuf: array[0..100000] of WideChar;
{$ELSE}
    FAnsiBuf: array[0..100000] of AnsiChar;
{$ENDIF}
    procedure LSError(const APIname: string; RC: Integer);
  protected
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    { Main entry points to set up Prolog environment }
    procedure Init(const XPLName: string);
    procedure InitLS(const XPLName: string);
    procedure InitLSXP(P: Pointer);
    procedure InitLSX;
    procedure AddLSX(const LSXName: string);
    procedure AddPred(const PName: string; PArity: TArity; PFunc: TExtPred);
    procedure InitPreds(PIPtr: TPredInitPtr);
    procedure Load(const XPLName: string);
    procedure LoadXPL(const XPLName: string);
    function Main: Boolean;
    procedure Reset;
    procedure Close;
    procedure CloseLS;
    { Function and predicate parameters }
    procedure GetParm(N: integer; DT: TDType; P: Pointer);
    function GetPStrParm(N: Integer): string;
    function GetIntParm(N: Integer): Integer;
    function GetLongParm(N: Integer): LongInt;
    function GetShortParm(N: Integer): LongInt;
    function GetFloatParm(N: Integer): Double;
    function GetParmType(N: Integer): TPType;
    function StrParmLen(N: Integer): Integer;
    function UnifyParm(N: Integer; DT: TDType; P: Pointer): Boolean;
    function UnifyPStrParm(N: Integer; const S: string): Boolean;
    function UnifyAtomParm(N: Integer; const S: string): Boolean;
    function UnifyIntParm(N: Integer; I: Integer): Boolean;
    function UnifyLongParm(N: Integer; I: LongInt): Boolean;
    function UnifyShortParm(N: Integer; I: LongInt): Boolean;
    function UnifyFloatParm(N: Integer; F: Double): Boolean;
    { Calling Prolog from Delphi }
    function Exec(var TP: TTerm): Boolean;
    function ExecStr(var TP: TTerm; S: PChar): Boolean;
    function ExecPStr(var TP: TTerm; const S: string): Boolean;
    function Call(var TP: TTerm): Boolean;
    function CallStr(var TP: TTerm; S: PChar): Boolean;
    function CallPStr(var TP: TTerm; const S: string): Boolean;
    function Redo: Boolean;
    procedure ClearCall;
    { Asserting and retracting }
    procedure Asserta(T: TTerm);
    procedure Assertz(T: TTerm);
    procedure Retract(T: TTerm);
    procedure AssertaStr(S: PChar);
    procedure AssertzStr(S: PChar);
    procedure RetractStr(S: PChar);
    procedure AssertaPStr(const S: string);
    procedure AssertzPStr(const S: string);
    procedure RetractPStr(const S: string);
    { string/term conversion functions }
    procedure TermToStr(T: TTerm; S: PChar; N: Integer);
    procedure TermToStrQ(T: TTerm; S: PChar; N: Integer);
    procedure StrToTerm(var TP: TTerm; S: PChar);
    function TermToPStr(T: TTerm): string;
    function TermToPStrQ(T: TTerm): string;
    procedure PStrToTerm(var TP: TTerm; const S: string);
    function StrTermLen(T: TTerm): Integer;
    { Making Prolog types }
    procedure MakeAtom(var TP: TTerm; const S: string);
    procedure MakeStr(var TP: TTerm; S: PChar);
    procedure MakePStr(var TP: TTerm; const S: string);
    procedure MakeInt(var TP: TTerm; I: LongInt);
    procedure MakeFloat(var TP: TTerm; F: Double);
    procedure MakeAddr(var TP: TTerm; P: Pointer);
    { Getting C values from Prolog terms }
    function GetTermType(T: TTerm): TPType;
    procedure GetTerm(T: TTerm; DT: TDType; P: Pointer);
    function GetPStrTerm(T: TTerm): string;
    function GetIntTerm(T: TTerm): Integer;
    function GetLongTerm(T: TTerm): LongInt;
    function GetShortTerm(T: TTerm): LongInt;
    function GetFloatTerm(T: TTerm): Double;
    { Structure hacking functions }
    procedure GetFA(T: TTerm; var S: string; var AP: TArity);
    function GetFunctor(T: TTerm): string;
    function GetArity(T: TTerm): Integer;
    procedure MakeFA(var TP: TTerm; const S: string; A: TArity);
    function UnifyArg(var TP: TTerm; N: Integer; DT: TDType; P: Pointer): Boolean;
    function UnifyPStrArg(var TP: TTerm; N: Integer; const S: string): Boolean;
    function UnifyAtomArg(var TP: TTerm; N: Integer; const S: string): Boolean;
    function UnifyIntArg(var TP: TTerm; N: Integer; I: Integer): Boolean;
    function UnifyLongArg(var TP: TTerm; N: Integer; I: LongInt): Boolean;
    function UnifyShortArg(var TP: TTerm; N: Integer; I: LongInt): Boolean;
    function UnifyFloatArg(var TP: TTerm; N: Integer; F: Double): Boolean;
    procedure GetArg(T: TTerm; N: Integer; DT: TDType; P: Pointer);
    function GetPStrArg(T: TTerm; N: Integer): string;
    function GetIntArg(T: TTerm; N: Integer): integer;
    function GetLongArg(T: TTerm; N: Integer): longint;
    function GetShortArg(T: TTerm; N: Integer): longint;
    function GetFloatArg(T: TTerm; N: Integer): double;
    function GetArgType(T: TTerm; N: Integer): TPType;
    function StrArgLen(T: TTerm; I: Integer): integer;
    function Unify(T1: TTerm; T2: TTerm): Boolean;
    { List hacking functions }
    procedure MakeList(var TP: TTerm);
    procedure PushList(var TP: TTerm; T: TTerm);
    function PopList(var tp: TTerm; DT: TDType; P: Pointer): TRC;
    function PopPStrList(var TP: TTerm; var S: string): TRC;
    function PopIntList(var TP: TTerm; var I: Integer): TRC;
    function PopLongList(var TP: TTerm; var I: LongInt): TRC;
    function PopShortList(var TP: TTerm; var I: LongInt): TRC;
    function PopFloatList(var TP: TTerm; var F: Double): TRC;
    function GetHead(T: TTerm; DT: TDType; P: Pointer): TRC;
    function GetPStrHead(T: TTerm; var S: string): TRC;
    function GetIntHead(T: TTerm; var I: Integer): TRC;
    function GetLongHead(T: TTerm; var I: LongInt): TRC;
    function GetShortHead(T: TTerm; var I: LongInt): TRC;
    function GetFloatHead(T: TTerm; var F: Double): TRC;
    function GetTail(T: TTerm): TTerm;
    { Stream I/O functions }
    procedure SetStream(st: TPStream; I: Integer);
    function GetStream(st: TPStream): Integer;
    procedure SetInput(PFunc1: TGetC; PFunc2: TUngetC);
    procedure SetOutput(PFunc1: TPutC; PFunc2: TPutS);
    { Miscellaneous functions }
    procedure GetVersion(var S: string);
    function GetPVersion: string;
    { Error handling functions }
    function GetExceptRC: TRC;
    procedure GetExceptMsg(S: PChar; L: Integer);
    procedure GetExceptReadBuffer(S: PChar; L: Integer);
    procedure GetExceptCallStack(S: PChar; L: Integer);
  end;

procedure Register;

implementation

const
  AMZIDLL = 'amzi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF}

{ Defines the actual DLL entry points for the Logic Server API.
  See the file AMZI.H for the complete C header file definition. }

{ Main entry points to set up Prolog environment }
function lsInitA(var Eng: TEngID; XPLName: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsInitW(var Eng: TEngID; XPLName: PWideChar): TRC; stdcall; external AMZIDLL;
function lsInit(var Eng: TEngID; XPLName: PChar): TRC; stdcall; external AMZIDLL name 'lsInit' + AWSuffix;

function lsInit2A(var Eng: TEngID; XPLName: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsInit2W(var Eng: TEngID; XPLName: PWideChar): TRC; stdcall; external AMZIDLL;
function lsInit2(var Eng: TEngID; XPLName: PChar): TRC; stdcall; external AMZIDLL name 'lsInit2' + AWSuffix;

function lsInitLSX(Eng: TEngID; P: Pointer): TRC; stdcall; external AMZIDLL;

function lsAddLSXA(Eng: TEngID; LSXName: PAnsiChar; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsAddLSXW(Eng: TEngID; LSXName: PWideChar; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsAddLSX(Eng: TEngID; LSXName: PChar; P: Pointer): TRC; stdcall; external AMZIDLL name 'lsAddLSX' + AWSuffix;

function lsAddPredA(Eng: TEngID; PName: PAnsiChar; PArity: TArity; PFunc: TExtPred; Ptr: Pointer): TRC; stdcall; external AMZIDLL;
function lsAddPredW(Eng: TEngID; PName: PWideChar; PArity: TArity; PFunc: TExtPred; Ptr: Pointer): TRC; stdcall; external AMZIDLL;
function lsAddPred(Eng: TEngID; PName: PChar; PArity: TArity; PFunc: TExtPred; Ptr: Pointer): TRC; stdcall; external AMZIDLL name 'lsAddPred' + AWSuffix;

function lsInitPredsA(Eng: TEngID; PIPtr: TPredInitPtrA): TRC; stdcall; external AMZIDLL;
function lsInitPredsW(Eng: TEngID; PIPtr: TPredInitPtrW): TRC; stdcall; external AMZIDLL;
function lsInitPreds(Eng: TEngID; PIPtr: TPredInitPtr): TRC; stdcall; external AMZIDLL name 'lsInitPreds' + AWSuffix;

function lsLoadA(Eng: TEngID; XPLname: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsLoadW(Eng: TEngID; XPLname: PWideChar): TRC; stdcall; external AMZIDLL;
function lsLoad(Eng: TEngID; XPLname: PChar): TRC; stdcall; external AMZIDLL name 'lsLoad' + AWSuffix;

function lsMain(Eng: TEngID): TTFi; stdcall; external AMZIDLL;
function lsReset(Eng: TEngID): TRC; stdcall; external AMZIDLL;
function lsClose(Eng: TEngID): TRC; stdcall; external AMZIDLL;

{ Function and predicate parameters }
function lsGetParm(Eng: TEngID; N: Integer; DT: TTypeInt; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsGetParmType(Eng: TEngID; N: Integer): TTypeInt; stdcall; external AMZIDLL;
function lsStrParmLen(Eng: TEngID; N: Integer): Integer; stdcall; external AMZIDLL;
function lsUnifyParm(Eng: TEngID; N: Integer; DT: TTypeInt; P: Pointer): TTFi; stdcall; external AMZIDLL;

{ Calling Prolog from Delphi }
function lsExec(Eng: TEngID; var TP: TTerm): TTFi; stdcall; external AMZIDLL;

function lsExecStrA(Eng: TEngID; var TP: TTerm; S: PAnsiChar): TTFi; stdcall; external AMZIDLL;
function lsExecStrW(Eng: TEngID; var TP: TTerm; S: PWideChar): TTFi; stdcall; external AMZIDLL;
function lsExecStr(Eng: TEngID; var TP: TTerm; S: PChar): TTFi; stdcall; external AMZIDLL name 'lsExecStr' + AWSuffix;

function lsCall(Eng: TEngID; var TP: TTerm): TTFi; stdcall; external AMZIDLL;

function lsCallStrA(Eng: TEngID; var TP: TTerm; S: PAnsiChar): TTFi; stdcall; external AMZIDLL;
function lsCallStrW(Eng: TEngID; var TP: TTerm; S: PWideChar): TTFi; stdcall; external AMZIDLL;
function lsCallStr(Eng: TEngID; var TP: TTerm; S: PChar): TTFi; stdcall; external AMZIDLL name 'lsCallStr' + AWSuffix;

function lsRedo(Eng: TEngID): TTFi; stdcall; external AMZIDLL;
function lsClearCall(Eng: TEngID): TRC; stdcall; external AMZIDLL;

{ Asserting and retracting }
function lsAsserta(Eng: TEngID; T: TTerm): TRC; stdcall; external AMZIDLL;
function lsAssertz(Eng: TEngID; T: TTerm): TRC; stdcall; external AMZIDLL;
function lsRetract(Eng: TEngID; T: TTerm): TRC; stdcall; external AMZIDLL;

function lsAssertaStrA(Eng: TEngID; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsAssertaStrW(Eng: TEngID; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsAssertaStr(Eng: TEngID; S: PChar): TRC; stdcall; external AMZIDLL name 'lsAssertaStr' + AWSuffix;

function lsAssertzStrA(Eng: TEngID; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsAssertzStrW(Eng: TEngID; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsAssertzStr(Eng: TEngID; S: PChar): TRC; stdcall; external AMZIDLL name 'lsAssertzStr' + AWSuffix;

function lsRetractStrA(Eng: TEngID; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsRetractStrW(Eng: TEngID; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsRetractStr(Eng: TEngID; S: PChar): TRC; stdcall; external AMZIDLL name 'lsRetractStr' + AWSuffix;

{ string/term conversion functions }
function lsTermToStrA(Eng: TEngID; T: TTerm; S: PAnsiChar; N: Integer): TRC; stdcall; external AMZIDLL;
function lsTermToStrW(Eng: TEngID; T: TTerm; S: PWideChar; N: Integer): TRC; stdcall; external AMZIDLL;
function lsTermToStr(Eng: TEngID; T: TTerm; S: PChar; N: Integer): TRC; stdcall;  external AMZIDLL name 'lsTermToStr' + AWSuffix;

function lsTermToStrQA(Eng: TEngID; T: TTerm; S: PAnsiChar; N: Integer): TRC; stdcall; external AMZIDLL;
function lsTermToStrQW(Eng: TEngID; T: TTerm; S: PWideChar; N: Integer): TRC; stdcall; external AMZIDLL;
function lsTermToStrQ(Eng: TEngID; T: TTerm; S: PChar; N: Integer): TRC; stdcall; external AMZIDLL name 'lsTermToStrQ' + AWSuffix;

function lsStrToTermA(Eng: TEngID; var TP: TTerm; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsStrToTermW(Eng: TEngID; var TP: TTerm; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsStrToTerm(Eng: TEngID; var TP: TTerm; S: PChar): TRC; stdcall; external AMZIDLL name 'lsStrToTerm' + AWSuffix;

{ Making Prolog types }
function lsMakeAtomA(Eng: TEngID; var TP: TTerm; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsMakeAtomW(Eng: TEngID; var TP: TTerm; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsMakeAtom(Eng: TEngID; var TP: TTerm; S: PChar): TRC; stdcall; external AMZIDLL name 'lsMakeAtom' + AWSuffix;

function lsMakeStrA(Eng: TEngID; var TP: TTerm; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsMakeStrW(Eng: TEngID; var TP: TTerm; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsMakeStr(Eng: TEngID; var TP: TTerm; S: PChar): TRC; stdcall; external AMZIDLL name 'lsMakeStr' + AWSuffix;

function lsMakeInt(Eng: TEngID; var TP: TTerm; I: LongInt): TRC; stdcall; external AMZIDLL;
function lsMakeFloat(Eng: TEngID; var TP: TTerm; F: Double): TRC; stdcall; external AMZIDLL;
function lsMakeAddr(Eng: TEngID; var TP: TTerm; P: Pointer): TRC; stdcall; external AMZIDLL;

{ Getting C values from Prolog terms }
function lsGetTermType(Eng: TEngID; T: TTerm): TTypeInt; stdcall; external AMZIDLL;
function lsGetTerm(Eng: TEngID; T: TTerm; DT: TTypeInt; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsStrTermLen(Eng: TEngID; T: TTerm): Integer; stdcall; external AMZIDLL;

{ Structure hacking functions }
function lsGetFAA(Eng: TEngID; T: TTerm; S: PAnsiChar; var AP: TArity): TRC; stdcall; external AMZIDLL;
function lsGetFAW(Eng: TEngID; T: TTerm; S: PWideChar; var AP: TArity): TRC; stdcall; external AMZIDLL;
function lsGetFA(Eng: TEngID; T: TTerm; S: PChar; var AP: TArity): TRC; stdcall; external AMZIDLL name 'lsGetFA' + AWSuffix;

function lsMakeFAA(Eng: TEngID; var TP: TTerm; S: PAnsiChar; A: TArity): TRC; stdcall; external AMZIDLL;
function lsMakeFAW(Eng: TEngID; var TP: TTerm; S: PWideChar; A: TArity): TRC; stdcall; external AMZIDLL;
function lsMakeFA(Eng: TEngID; var TP: TTerm; S: PChar; A: TArity): TRC; stdcall; external AMZIDLL name 'lsMakeFA' + AWSuffix;

function lsUnifyArg(Eng: TEngID; var TP: TTerm; N: Integer; DT: TTypeInt; P: Pointer): TTFi; stdcall; external AMZIDLL;
function lsGetArg(Eng: TEngID; T: TTerm; N: Integer; DT: TTypeInt; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsGetArgType(Eng: TEngID; T: TTerm; N: Integer): TTypeInt; stdcall; external AMZIDLL;
function lsStrArgLen(Eng: TEngID; T: TTerm; I: Integer): Integer; stdcall; external AMZIDLL;
function lsUnify(Eng: TEngID; T1: TTerm; T2: TTerm): TTFi; stdcall; external AMZIDLL;

{ List hacking functions }
function lsMakeList(Eng: TEngID; var TP: TTerm): TRC; stdcall; external AMZIDLL;
function lsPushList(Eng: TEngID; var TP: TTerm; T: TTerm): TRC; stdcall; external AMZIDLL;
function lsPopList(Eng: TEngID; var TP: TTerm; DT: TTypeInt; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsGetHead(Eng: TEngID; T: TTerm; DT: TTypeInt; P: Pointer): TRC; stdcall; external AMZIDLL;
function lsGetTail(Eng: TEngID; T: TTerm): TTerm; stdcall; external AMZIDLL;

{ Stream I/O functions }
function lsSetStream(Eng: TEngID; ST: TPStreamInt; I: Integer): TRC; stdcall; external AMZIDLL;
function lsGetStream(Eng: TEngID; ST: TPStreamInt): Integer; stdcall; external AMZIDLL;
function lsSetInput(Eng: TEngID; PFunc1: TGetC; PFunc2: TUngetC): TRC; stdcall; external AMZIDLL;

function lsSetOutputA(Eng: TEngID; PFunc1: TPutC; PFunc2: TPutSA): TRC; stdcall; external AMZIDLL;
function lsSetOutputW(Eng: TEngID; PFunc1: TPutC; PFunc2: TPutSW): TRC; stdcall; external AMZIDLL;
function lsSetOutput(Eng: TEngID; PFunc1: TPutC; PFunc2: TPutS): TRC; stdcall; external AMZIDLL name 'lsSetOutput' + AWSuffix;

{ Miscellaneous functions }
function lsGetVersionA(Eng: TEngID; S: PAnsiChar): TRC; stdcall; external AMZIDLL;
function lsGetVersionW(Eng: TEngID; S: PWideChar): TRC; stdcall; external AMZIDLL;
function lsGetVersion(Eng: TEngID; S: PChar): TRC; stdcall; external AMZIDLL name 'lsGetVersion' + AWSuffix;

{ Error handling functions }
function lsGetExceptRC(Eng: TEngID): TRC; stdcall; external AMZIDLL;

procedure lsGetExceptMsgA(Eng: TEngID; S: PAnsiChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptMsgW(Eng: TEngID; S: PWideChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptMsg(Eng: TEngID; S: PChar; L: Integer) stdcall; external AMZIDLL name 'lsGetExceptMsg' + AWSuffix;

procedure lsGetExceptReadBufferA(Eng: TEngID; S: PAnsiChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptReadBufferW(Eng: TEngID; S: PWideChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptReadBuffer(Eng: TEngID; S: PChar; L: Integer) stdcall; external AMZIDLL name 'lsGetExceptReadBuffer' + AWSuffix;

procedure lsGetExceptCallStackA(Eng: TEngID; S: PAnsiChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptCallStackW(Eng: TEngID; S: PWideChar; L: Integer) stdcall; external AMZIDLL;
procedure lsGetExceptCallStack(Eng: TEngID; S: PChar; L: Integer) stdcall; external AMZIDLL name 'lsGetExceptCallStack' + AWSuffix;

constructor TLSEngine.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FCreatedB := True;
  FInitializedB := False;
end;

destructor TLSEngine.Destroy;
begin
  if FInitializedB then lsClose(FEng);
  inherited Destroy;
end;

{ The function definitions map Logic Server methods to the
  actual DLL entry points. }

{ Main entry points to set up Prolog environment }

procedure TLSEngine.Init(const XPLName: string);
begin
  InitLS(XPLName);
end;

procedure TLSEngine.InitLS(const XPLName: string);
begin
  if not FCreatedB then
    LSError('LS not created', 0);
  if FInitializedB then
    lsClose(FEng);
  FRC := lsInit(FEng, PChar(XPLName));
  if FRC <> 0 then
    LSError('lsInit', FRC);
  FInitializedB := True;
end;

procedure TLSEngine.InitLSX;
begin
  FRC := lsInitLSX(FEng, nil);
  if FRC <>  0 then LSError('lsInitLSX', FRC);
end;

procedure TLSEngine.InitLSXP(P: Pointer);
begin
  FRC := lsInitLSX(FEng, P);
  if FRC <>  0 then LSError('lsInitLSX', FRC);
end;

procedure TLSEngine.AddLSX(const LSXName: string);
begin
  FRC := lsAddLSX(FEng, PChar(LSXName), nil);
  if FRC <> 0 then LSError('lsAddLSX', FRC);
end;

procedure TLSEngine.AddPred(const PName: string; PArity: TArity; PFunc: TExtPred);
begin
  FRC := lsAddPred(FEng, PChar(PName), PArity, PFunc, Pointer(FEng));
  if FRC <> 0 then LSError('lsAddPred', FRC);
end;

procedure TLSEngine.InitPreds(PIPtr: TPredInitPtr);
begin
  FRC := lsInitPreds(FEng, PIPtr);
  if FRC <> 0 then LSError('lsInitPreds', FRC);
end;

procedure TLSEngine.Load(const XPLName: string);
begin
  LoadXPL(XPLName);
end;

procedure TLSEngine.LoadXPL(const XPLName: string);
begin
  FRC := lsLoad(FEng, PChar(XPLName));
  if FRC <> 0 then LSError('lsLoad', FRC);
end;

function TLSEngine.Main: Boolean;
begin
  Result := False;
  FRC := lsMain(FEng);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsMain', FRC);
  end;
end;

procedure TLSEngine.Reset;
begin
  FRC := lsReset(FEng);
  if FRC <> 0 then LSError('lsReset', FRC);
end;

procedure TLSEngine.Close;
begin
  CloseLS;
end;

procedure TLSEngine.CloseLS;
begin
  FRC := lsClose(FEng);
  if FRC <> 0 then LSError('lsClose', FRC);
  FInitializedB := False;
end;

{ Function and predicate parameters }

procedure TLSEngine.GetParm(N: Integer; DT: TDType; P: Pointer);
begin
  FRC := lsGetParm(FEng, N, TTypeInt(DT), P);
  if FRC <> 0 then LSError('lsGetParm', FRC);
end;

function TLSEngine.GetPStrParm(N: Integer): string;
var
  res: string;
begin
  SetLength(res, lsStrParmLen(FEng, N));
  if Length(res) > 0 then
    res[1] := #0;

{$IFDEF UNICODE}
  FRC := lsGetParm(FEng, N, TTypeInt(dWSTR), PWideChar(res));
{$ELSE}
  FRC := lsGetParm(FEng, N, TTypeInt(dSTR), PAnsiChar(res));
{$ENDIF}

  if FRC <> 0 then LSError('lsGetParm', FRC);
  Result := PChar(res);
end;

function TLSEngine.GetIntParm(N: Integer): Integer;
var
  I: Integer;
begin
  FRC := lsGetParm(FEng, N, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetParm', FRC);
  Result := I;
end;

function TLSEngine.GetLongParm(N: Integer): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetParm(FEng, N, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetParm', FRC);
  Result := I;
end;

function TLSEngine.GetShortParm(N: Integer): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetParm(FEng, N, TTypeInt(dSHORT), @I);
  if FRC <> 0 then LSError('lsGetParm', FRC);
  Result := I;
end;

function TLSEngine.GetFloatParm(N: Integer): Double;
var
  F: Double;
begin
  FRC := lsGetParm(FEng, N, TTypeInt(dDOUBLE), @F);
  if FRC <> 0 then LSError('lsGetParm', FRC);
  Result := F;
end;

function TLSEngine.GetParmType(N: Integer): TPType;
begin
  Result := TPType(lsGetParmType(FEng, N));
end;

function TLSEngine.StrParmLen(N: Integer): Integer;
begin
  Result := lsStrParmLen(FEng, N);
end;

function TLSEngine.UnifyParm(N: Integer; DT: TDType; P: Pointer): Boolean;
begin
  Result := False;
  FRC := lsUnifyParm(FEng, N, TTypeInt(DT), P);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyPStrParm(N: Integer; const S: string): Boolean;
begin
  Result := False;

{$IFDEF UNICODE}
  FRC := lsUnifyParm(FEng, N, TTypeInt(dWSTR), PWideChar(S));
{$ELSE}
  FRC := lsUnifyParm(FEng, N, TTypeInt(dSTR), PAnsiChar(S));
{$ENDIF}

  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyAtomParm(N: Integer; const S: string): Boolean;
begin
  Result := False;

{$IFDEF UNICODE}
  StrPCopy(FWideBuf, S);
  FRC := lsUnifyParm(FEng, N, TTypeInt(dWATOM), @FWideBuf);
{$ELSE}
  StrPCopy(FAnsiBuf, S);
  FRC := lsUnifyParm(FEng, N, TTypeInt(dATOM), @FAnsiBuf);
{$ENDIF}

  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyIntParm(N: Integer; I: Integer): Boolean;
begin
  Result := False;
  FRC := lsUnifyParm(FEng, N, TTypeInt(dLONG), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyLongParm(N: Integer; I: LongInt): Boolean;
begin
  Result := False;
  FRC := lsUnifyParm(FEng, N, TTypeInt(dLONG), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyShortParm(N: Integer; I: LongInt): Boolean;
begin
  Result := False;
  FRC := lsUnifyParm(FEng, N, TTypeInt(dSHORT), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;

function TLSEngine.UnifyFloatParm(N: Integer; F: Double): Boolean;
begin
  Result := False;
  FRC := lsUnifyParm(FEng, N, TTypeInt(dDOUBLE), @F);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyParm', FRC);
  end;
end;


{ Calling Prolog from Delphi }

function TLSEngine.Exec(var TP: TTerm): Boolean;
begin
  Result := False;
  FRC := lsExec(FEng, TP);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsExec', FRC);
  end;
end;

function TLSEngine.ExecStr(var TP: TTerm; S: PChar): Boolean;
begin
  Result := False;
  FRC := lsExecStr(FEng, TP, S);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', FRC);
  end;
end;

function TLSEngine.ExecPStr(var TP: TTerm; const S: string): Boolean;
begin
  Result := False;
  FRC := lsExecStr(FEng, TP, PChar(S));
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsExecStr', FRC);
  end;
end;

function TLSEngine.Call(var TP: TTerm): Boolean;
begin
  Result := False;
  FRC := lsCall(FEng, TP);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsCall', FRC);
  end;
end;

function TLSEngine.CallStr(var TP: TTerm; S: PChar): Boolean;
begin
  Result := False;
  FRC := lsCallStr(FEng, TP, S);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', FRC);
  end;
end;

function TLSEngine.CallPStr(var TP: TTerm; const S: string): Boolean;
begin
  Result := False;

  FRC := lsCallStr(FEng, TP, PChar(S));
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsCallStr', FRC);
  end;
end;

function TLSEngine.Redo: Boolean;
begin
  Result := False;
  FRC := lsRedo(FEng);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsRedo', FRC);
  end;
end;

procedure TLSEngine.ClearCall;
begin
  FRC := lsClearCall(FEng);
  if FRC <> 0 then LSError('lsClearCall', FRC);
end;

{ Asserting and retracting }

procedure TLSEngine.Asserta(T: TTerm);
begin
  FRC := lsAsserta(FEng, T);
  if FRC <> 0 then LSError('lsAsserta', FRC);
end;

procedure TLSEngine.Assertz(T: TTerm);
begin
  FRC := lsAssertz(FEng, T);
  if FRC <> 0 then LSError('lsAssertz', FRC);
end;

procedure TLSEngine.Retract(T: TTerm);
begin
  FRC := lsRetract(FEng, T);
  if FRC <> 0 then LSError('lsRetract', FRC);
end;

procedure TLSEngine.AssertaStr(S: PChar);
begin
  FRC := lsAssertaStr(FEng, S);
  if FRC <> 0 then LSError('lsAssertaStr', FRC);
end;

procedure TLSEngine.AssertzStr(S: PChar);
begin
  FRC := lsAssertzStr(FEng, S);
  if FRC <> 0 then LSError('lsAssertzStr', FRC);
end;

procedure TLSEngine.RetractStr(S: PChar);
begin
  FRC := lsRetractStr(FEng, S);
  if FRC <> 0 then LSError('lsRetractStr', FRC);
end;

procedure TLSEngine.AssertaPStr(const S: string);
begin
  FRC := lsAssertaStr(FEng, PChar(S));
  if FRC <> 0 then LSError('lsAssertaStr', FRC);
end;

procedure TLSEngine.AssertzPStr(const S: string);
begin
  FRC := lsAssertzStr(FEng, PChar(S));
  if FRC <> 0 then LSError('lsAssertzStr', FRC);
end;

procedure TLSEngine.RetractPStr(const S: string);
begin
  FRC := lsRetractStr(FEng, PChar(S));
  if FRC <> 0 then LSError('lsRetractStr', FRC);
end;

{ string/term conversion functions }

procedure TLSEngine.TermToStr(T: TTerm; S: PChar; N: Integer);
begin
  FRC := lsTermToStr(FEng, T, S, N);
  if FRC <> 0 then LSError('lsTermToStr', FRC);
end;

procedure TLSEngine.TermToStrQ(T: TTerm; S: PChar; N: Integer);
begin
  FRC := lsTermToStrQ(FEng, T, S, N);
  if FRC <> 0 then LSError('lsTermToStrQ', FRC);
end;

procedure TLSEngine.StrToTerm(var TP: TTerm; S: PChar);
begin
  FRC := lsStrToTerm(FEng, TP, S);
  if FRC <> 0 then LSError('lsStrToTerm', FRC);
end;

function TLSEngine.TermToPStr(T: TTerm): string;
var
  res: string;
begin
  SetLength(res, StrTermLen(T));
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsTermToStr(FEng, T, PChar(res), Length(res) + 1);

  if FRC <> 0 then LSError('lsTermToStr', FRC);
  Result := PChar(res);
end;

function TLSEngine.TermToPStrQ(T: TTerm): string;
var
  res: string;
begin
  SetLength(res, StrTermLen(T));
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsTermToStrQ(FEng, T, PChar(res), Length(res) + 1);

  if FRC <> 0 then LSError('lsTermToStrQ', FRC);
  Result := PChar(res);
end;

procedure TLSEngine.PStrToTerm(var TP: TTerm; const S: string);
begin
  FRC := lsStrToTerm(FEng, TP, PChar(S));
  if FRC <> 0 then LSError('lsStrToTerm', FRC);
end;

function TLSEngine.StrTermLen(T: TTerm): Integer;
begin
  Result := lsStrTermLen(FEng, T);
end;

{ Making Prolog types }

procedure TLSEngine.MakeAtom(var TP: TTerm; const S: string);
begin
  FRC := lsMakeAtom(FEng, TP, PChar(S));
  if FRC <> 0 then LSError('lsMakeAtom', FRC);
end;

procedure TLSEngine.MakeStr(var TP: TTerm; S: PChar);
begin
  FRC := lsMakeStr(FEng, TP, S);
  if FRC <> 0 then LSError('lsMakeStr', FRC);
end;

procedure TLSEngine.MakePStr(var TP: TTerm; const S: string);
begin
  FRC := lsMakeStr(FEng, TP, PChar(S));
  if FRC <> 0 then LSError('lsMakeStr', FRC);
end;

procedure TLSEngine.MakeInt(var TP: TTerm; I: LongInt);
begin
  FRC := lsMakeInt(FEng, TP, I);
  if FRC <> 0 then LSError('lsMakeInt', FRC);
end;

procedure TLSEngine.MakeFloat(var TP: TTerm; F: Double);
begin
  FRC := lsMakeFloat(FEng, TP, F);
  if FRC <> 0 then LSError('lsMakeFloat', FRC);
end;

procedure TLSEngine.MakeAddr(var TP: TTerm; P: Pointer);
begin
  FRC := lsMakeAddr(FEng, TP, P);
  if FRC <> 0 then LSError('lsMakeAddr', FRC);
end;

{ Getting C values from Prolog terms }

function TLSEngine.GetTermType(T: TTerm): TPType;
begin
  Result := TPType(lsGetTermType(FEng, T));
end;

procedure TLSEngine.GetTerm(T: TTerm; DT: TDType; P: Pointer);
begin
  FRC := lsGetTerm(FEng, T, TTypeInt(DT), P);
  if FRC <> 0 then LSError('lsGetTerm', FRC);
end;

function TLSEngine.GetPStrTerm(T: TTerm): string;
var
  res: string;
begin
  SetLength(res, lsStrTermLen(FEng, T));
  if Length(res) > 0 then
    res[1] := #0;

{$IFDEF UNICODE}
  FRC := lsGetTerm(FEng, T, TTypeInt(dWSTR), PWideChar(res));
{$ELSE}
  FRC := lsGetTerm(FEng, T, TTypeInt(dSTR), PAnsiChar(res));
{$ENDIF}

  if FRC <> 0 then LSError('lsGetTerm', FRC);
  Result := PChar(res);
end;

function TLSEngine.GetIntTerm(T: TTerm): Integer;
var
  I: Integer;
begin
  FRC := lsGetTerm(FEng, T, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetTerm', FRC);
  Result := I;
end;

function TLSEngine.GetLongTerm(T: TTerm): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetTerm(FEng, T, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetTerm', FRC);
  Result := I;
end;

function TLSEngine.GetShortTerm(T: TTerm): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetTerm(FEng, T, TTypeInt(dSHORT), @I);
  if FRC <> 0 then LSError('lsGetTerm', FRC);
  Result := I;
end;

function TLSEngine.GetFloatTerm(T: TTerm): Double;
var
  F: Double;
begin
  FRC := lsGetTerm(FEng, T, TTypeInt(dDOUBLE), @F);
  if FRC <> 0 then LSError('lsGetTerm', FRC);
  Result := F;
end;

{ Structure hacking functions }

procedure TLSEngine.GetFA(T: TTerm; var S: string; var AP: TArity);
var
  res: string;
begin
  SetLength(res, 4096);
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsGetFA(FEng, T, PChar(res), AP);

  if FRC <> 0 then LSError('lsGetFA', FRC);
  S := PChar(res);
end;

function TLSEngine.GetFunctor(T: TTerm): string;
var
  res: string;
  AP: TArity;
begin
  SetLength(res, 4096);
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsGetFA(FEng, T, PChar(res), AP);

  if FRC <> 0 then LSError('lsGetFunctor', FRC);
  Result := PChar(res);
end;

function TLSEngine.GetArity(T: TTerm): Integer;
var
  res: string;
  AP: TArity;
begin
  SetLength(res, 4096);
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsGetFA(FEng, T, PChar(res), AP);

  if FRC <> 0 then LSError('lsGetArity', FRC);
  Result := AP;
end;

procedure TLSEngine.MakeFA(var TP: TTerm; const S: string; A: TArity);
begin
  FRC := lsMakeFA(FEng, TP, PChar(S), A);
  if FRC <> 0 then LSError('lsMakeFA', FRC);
end;

function TLSEngine.UnifyArg(var TP: TTerm; N: Integer; DT: TDType; P: Pointer): Boolean;
begin
  Result := False;
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(DT), P);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyPStrArg(var TP: TTerm; N: Integer; const S: string): Boolean;
begin
  Result := False;

{$IFDEF UNICODE}
  StrPCopy(FWideBuf, S);
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dWSTR), @FWideBuf);
{$ELSE}
  StrPCopy(FAnsiBuf, S);
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dSTR), @FAnsiBuf);
{$ENDIF}

  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyAtomArg(var TP: TTerm; N: Integer; const S: string): Boolean;
begin
  Result := False;

{$IFDEF UNICODE}
  StrPCopy(FWideBuf, S);
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dWATOM), @FWideBuf);
{$ELSE}
  StrPCopy(FAnsiBuf, S);
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dATOM), @FAnsiBuf);
{$ENDIF}

  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyIntArg(var TP: TTerm; N: Integer; I: Integer): Boolean;
begin
  Result := False;
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dLONG), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyLongArg(var TP: TTerm; N: Integer; I: LongInt): Boolean;
begin
  Result := False;
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dLONG), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyShortArg(var TP: TTerm; N: Integer; I: LongInt): Boolean;
begin
  Result := False;
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dSHORT), @I);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;

function TLSEngine.UnifyFloatArg(var TP: TTerm; N: Integer; F: Double): Boolean;
begin
  Result := False;
  FRC := lsUnifyArg(FEng, TP, N, TTypeInt(dDOUBLE), @F);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnifyArg', FRC);
  end;
end;


procedure TLSEngine.GetArg(T: TTerm; N: Integer; DT: TDType; P: Pointer);
begin
  FRC := lsGetArg(FEng, T, N, TTypeInt(DT), P);
  if FRC <> 0 then LSError('lsGetArg', FRC);
end;

function TLSEngine.GetPStrArg(T: TTerm; N: Integer): string;
var
  res: string;
begin
  SetLength(res, lsStrArgLen(FEng, T, N));
  if Length(res) > 0 then
    res[1] := #0;

{$IFDEF UNICODE}
  FRC := lsGetArg(FEng, T, N, TTypeInt(dWSTR), PWideChar(res));
{$ELSE}
  FRC := lsGetArg(FEng, T, N, TTypeInt(dSTR), PAnsiChar(res));
{$ENDIF}

  if FRC <> 0 then LSError('lsGetArg', FRC);
  Result := PChar(res);
end;

function TLSEngine.GetIntArg(T: TTerm; N: Integer): Integer;
var
  I: Integer;
begin
  FRC := lsGetArg(FEng, T, N, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetArg', FRC);
  Result := I;
end;

function TLSEngine.GetLongArg(T: TTerm; N: Integer): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetArg(FEng, T, N, TTypeInt(dLONG), @I);
  if FRC <> 0 then LSError('lsGetArg', FRC);
  Result := I;
end;

function TLSEngine.GetShortArg(T: TTerm; N: Integer): LongInt;
var
  I: LongInt;
begin
  FRC := lsGetArg(FEng, T, N, TTypeInt(dSHORT), @I);
  if FRC <> 0 then LSError('lsGetArg', FRC);
  Result := I;
end;

function TLSEngine.GetFloatArg(T: TTerm; N: Integer): Double;
var
  F: Double;
begin
  FRC := lsGetArg(FEng, T, N, TTypeInt(dDOUBLE), @F);
  if FRC <> 0 then LSError('lsGetArg', FRC);
  Result := F;
end;

function TLSEngine.GetArgType(T: TTerm; N: Integer): TPType;
begin
  Result := TPType(lsGetArgType(FEng, T, N));
end;

function TLSEngine.StrArgLen(T: TTerm; I: Integer): Integer;
begin
  Result := lsStrArgLen(FEng, T, I);
end;

function TLSEngine.Unify(T1: TTerm; T2: TTerm): Boolean;
begin
  Result := False;
  FRC := lsUnify(FEng, T1, T2);
  case FRC of
    0: Result := False;
    1: Result := True;
    else LSError('lsUnify', FRC);
  end;
end;

{ List hacking functions }

procedure TLSEngine.MakeList(var TP: TTerm);
begin
  FRC := lsMakeList(FEng, TP);
  if FRC <> 0 then LSError('lsMakeList', FRC);
end;

procedure TLSEngine.PushList(var TP: TTerm; T: TTerm);
begin
  FRC := lsPushList(FEng, TP, T);
  if FRC <> 0 then LSError('lsPushList', FRC);
end;

function TLSEngine.PopList(var TP: TTerm; DT: TDType; P: Pointer): TRC;
begin
  FRC := lsPopList(FEng, TP, TTypeInt(DT), P);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.PopPStrList(var TP: TTerm; var S: string): TRC;
var
  res: string;
begin
  SetLength(res, StrTermLen(TP));
  if Length(res) > 0 then
    res[1] := #0;

{$IFDEF UNICODE}
  FRC := lsPopList(FEng, TP, TTypeInt(dWSTR), PWideChar(res));
{$ELSE}
  FRC := lsPopList(FEng, TP, TTypeInt(dSTR), PAnsiChar(res));
{$ENDIF}

  Result := FRC;
  S := PChar(res);
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.PopIntList(var TP: TTerm; var I: Integer): TRC;
begin
  FRC := lsPopList(FEng, TP, TTypeInt(dLONG), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.PopLongList(var TP: TTerm; var I: LongInt): TRC;
begin
  FRC := lsPopList(FEng, TP, TTypeInt(dLONG), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.PopShortList(var TP: TTerm; var I: LongInt): TRC;
begin
  FRC := lsPopList(FEng, TP, TTypeInt(dSHORT), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.PopFloatList(var TP: TTerm; var F: Double): TRC;
begin
  FRC := lsPopList(FEng, TP, TTypeInt(dDOUBLE), @F);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsPopList', FRC);
  end;
end;

function TLSEngine.GetHead(T: TTerm; DT: TDType; P: Pointer): TRC;
begin
  FRC := lsGetHead(FEng, T, TTypeInt(DT), P);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetPStrHead(T: TTerm; var S: string): TRC;
begin
{$IFDEF UNICODE}
  FRC := lsGetHead(FEng, T, TTypeInt(dWSTR), @FWideBuf);
  S := FWideBuf;
{$ELSE}
  FRC := lsGetHead(FEng, T, TTypeInt(dSTR), @FAnsiBuf);
  S := FAnsiBuf;
{$ENDIF}

  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetIntHead(T: TTerm; var I: Integer): TRC;
begin
  FRC := lsGetHead(FEng, T, TTypeInt(dLONG), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetLongHead(T: TTerm; var I: LongInt): TRC;
begin
  FRC := lsGetHead(FEng, T, TTypeInt(dLONG), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetShortHead(T: TTerm; var I: LongInt): TRC;
begin
  FRC := lsGetHead(FEng, T, TTypeInt(dSHORT), @I);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetFloatHead(T: TTerm; var F: Double): TRC;
begin
  FRC := lsGetHead(FEng, T, TTypeInt(dDOUBLE), @F);
  Result := FRC;
  case FRC of
    0: Result := FRC;
    -1: Result := FRC;
    else LSError('lsGetHead', FRC);
  end;
end;

function TLSEngine.GetTail(T: TTerm): TTerm;
begin
  Result := lsGetTail(FEng, T);
end;

{ Stream I/O functions }

procedure TLSEngine.SetStream(ST: TPStream; I: Integer);
begin
  FRC := lsSetStream(FEng, TPStreamInt(ST), I);
  if FRC <> 0 then LSError('lsSetStream', FRC);
end;

function TLSEngine.GetStream(ST: TPStream): Integer;
begin
  FRC := lsGetStream(FEng, TPStreamInt(ST));
  Result := FRC;
end;

procedure TLSEngine.SetInput(PFunc1: TGetC; PFunc2: TUngetC);
begin
  FRC := lsSetInput(FEng, PFunc1, PFunc2);
  if FRC <> 0 then LSError('lsSetInput', FRC);
end;

procedure TLSEngine.SetOutput(PFunc1: TPutC; PFunc2: TPutS);
begin
  FRC := lsSetOutput(FEng, PFunc1, PFunc2);
  if FRC <> 0 then LSError('lsSetOutput', FRC);
end;

{ Miscellaneous functions }

procedure TLSEngine.GetVersion(var S: string);
var
  res: string;
begin
  SetLength(res, 4096);
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsGetVersion(FEng, PChar(res));

  if FRC <> 0 then LSError('lsGetVersion', FRC);
  S := PChar(res);
end;

function TLSEngine.GetPVersion: string;
var
  res: string;
begin
  SetLength(res, 4096);
  if Length(res) > 0 then
    res[1] := #0;

  FRC := lsGetVersion(FEng, PChar(res));

  if FRC <> 0 then LSError('lsGetVersion', FRC);
  Result := PChar(res);
end;

{ Error handling functions }

procedure TLSEngine.GetExceptMsg(S: PChar; L:Integer);
begin
  lsGetExceptMsg(FEng, S, L);
end;

function TLSEngine.GetExceptRC: TRC;
begin
  Result := lsGetExceptRC(FEng);
end;

procedure TLSEngine.GetExceptReadBuffer(S: PChar; L: Integer);
begin
  lsGetExceptReadBuffer(FEng, S, L);
  if FRC <> 0 then LSError('lsGetExceptReadBuffer', FRC);
end;

procedure TLSEngine.GetExceptCallStack(S: PChar; L: Integer);
begin
  lsGetExceptCallStack(FEng, S, L);
  if FRC <> 0 then LSError('lsGetExceptCallStack', FRC);
end;

{ Non-Logic Server functions }

{ Error handling for most logic server functions.
  An exception is raised and the logic server is closed.
  This is important as it frees up all the memory allocated
  by the logic server. }

procedure TLSEngine.LSError(const APIName: string; RC: Integer);
var
  S: string;
begin
  SetLength(S, 4096);
  S[1] := #0;

  lsGetExceptMsg(FEng, PChar(S), Length(S) + 1);

  S := PChar(S);
  raise ELogicServer.Create(APIName + ': ' + IntToStr(RC) + ' ' + S);
end;

procedure Register;
begin
  RegisterComponents('Prolog', [TLSEngine]);
end;

end.
