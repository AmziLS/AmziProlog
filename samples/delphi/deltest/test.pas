unit Test;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Buttons, StdCtrls, Amzi;

function p_TFMessageBox(EngID: TEngID): TTFi; stdcall; export;
function p_InputBox(EngID: TEngID): TTFi; stdcall; export;

type
  TLogicServerTest = class(TForm)
     Hello: TButton;
    Output: TListBox;
    Siblings: TButton;
    OutLabel: TLabel;
    LSStatus: TGroupBox;
    LSOn: TRadioButton;
    LSOff: TRadioButton;
    AllSiblings: TButton;
    Children: TButton;
    TFCallback: TButton;
    YourName: TButton;
    Query: TButton;
    Assert: TButton;
    LSEng: TLSEngine;
    procedure OpenLSClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure HelloClick(Sender: TObject);
    procedure SiblingsClick(Sender: TObject);
    procedure AllSiblingsClick(Sender: TObject);
    procedure ChildrenClick(Sender: TObject);
    procedure TFCallbackClick(Sender: TObject);
    procedure YourNameClick(Sender: TObject);
    procedure QueryClick(Sender: TObject);
    procedure AssertClick(Sender: TObject);
  private
    t: TTerm;
  public
    { Public declarations }
  end;

var
  LogicServerTest: TLogicServerTest;

implementation

{$R *.DFM}

{ This is a callback function that defines an extended
  predicate.  In this case it puts up a yes/no message box
  and succeeds or fails based on the user's choice. }
function p_TFMessageBox;
var
  buf: array[0..120] of Char;
  s: string;
  yn: integer;
begin
  s := LogicServerTest.LSEng.GetPStrParm(1);
  StrPCopy(buf, s);
  yn := MessageBox(THandle(0), buf, '', MB_YESNO);
  if yn = IDYES
  then
    Result := lstrue
  else
    Result := lsfalse;
end;

{ Another callback function, this time implementing the
  Prolog predicate inputbox/2.  The first argument is
  the prompt string, and the second argument is the
  value the user entered. }
function p_InputBox;
var
  s, p: string;
begin
  p := LogicServerTest.LSEng.GetPStrParm(1);
  s := InputBox('Prolog Prompt', p, '');
  if LogicServerTest.LSEng.UnifyPStrParm(2, s)
  then Result := lstrue
  else Result := lsfalse;
end;

{ Initialize the Logic Server and load the test
  compiled Prolog program, TEST.XPL. }
procedure TLogicServerTest.OpenLSClick(Sender: TObject);
begin
  LSEng.InitLS('test');
  LSEng.AddPred('tfmessagebox', 1, p_TFMessageBox);
  LSEng.AddPred('inputbox', 2, p_InputBox);
  LSEng.LoadXPL('test');
  LSEng.Main;
  LSStatus.Color := clGreen;
end;

{ Close the Logic Server and the application. }
procedure TLogicServerTest.CloseClick(Sender: TObject);
begin
  LSEng.CloseLS;
  LSStatus.Color := clRed;
end;

{ Show the simplest call, getting one answer. }
procedure TLogicServerTest.HelloClick(Sender: TObject);
begin
  LSEng.ExecPStr(t, 'hello(delphi,X)');
  ShowMessage(LSEng.TermToPStr(t));
end;

{ Find all of Mary's siblings.  Just output the
  full query term, with variables bound, to the
  output listbox.}
procedure TLogicServerTest.SiblingsClick(Sender: TObject);
var
  tf: Boolean;
begin
  Output.items.Clear;
  OutLabel.Caption := 'Mary''s siblings';
  tf := LSEng.CallPStr(t, 'sibling(mary,X)');
  while tf do
  begin
    Output.items.add(LSEng.TermToPStr(t));
    tf := LSEng.Redo;
  end;
end;

{ Find all of the siblings.  In this case, break down
  the term, finding each of the arguments for formatting
  the output. }
procedure TLogicServerTest.AllSiblingsClick(Sender: TObject);
var
  tf: Boolean;
begin
  Output.items.Clear;
  OutLabel.Caption := 'All siblings';
  tf := LSEng.CallPStr(t, 'sibling(X,Y)');
  while tf do
  begin
    Output.items.add(LSEng.GetPStrArg(t, 1) +
                     ' is sibling of ' +
                     LSEng.GetPStrArg(t, 2));
    tf := LSEng.Redo;
  end;
end;

{ Find all of the children using findall/3 to create
  a Prolog list of the children's names.  Then get the
  3rd argument, which is that list, and walk the Prolog
  list extracting each name. Note that the general form
  of GetArg requires an address operator. }
procedure TLogicServerTest.ChildrenClick(Sender: TObject);
var
  tf: Boolean;
  tlist: TTerm;
  s: string;
begin
  Output.items.Clear;
  OutLabel.Caption := 'Children';
  tf := LSEng.ExecPStr(t, 'findall(X, parent(M, X), L)');
  if tf then
  begin
    LSEng.GetArg(t, 3, dTERM, @tlist);
    while 0 = LSEng.PopPStrList(tlist, s) do
      Output.items.add(s);
  end;
end;

procedure TLogicServerTest.TFCallbackClick(Sender: TObject);
begin
  LSEng.ExecPStr(t, 'yesno(X)');
  ShowMessage(LSEng.GetPStrArg(t, 1));
end;

procedure TLogicServerTest.YourNameClick(Sender: TObject);
begin
  LSEng.ExecPStr(t, 'yourname(X)');
  ShowMessage('Your name is ' + LSEng.GetPStrArg(t, 1));
end;

procedure TLogicServerTest.QueryClick(Sender: TObject);
var
  s: string;
  tf: Boolean;
begin
  s := InputBox('Prolog Query',
           'Enter a Prolog Query', '');
  Output.items.Clear;
  OutLabel.Caption := s;
  try
    tf := LSEng.CallPStr(t, s);
    while tf do
    begin
      Output.items.add(LSEng.TermToPStr(t));
      tf := LSEng.Redo;
    end;
  except
    on E: ELogicServer do
       ShowMessage('Query Error: ' + E.Message);
  end;
end;

procedure TLogicServerTest.AssertClick(Sender: TObject);
var
  s: string;
begin
  s := InputBox('Prolog Assertion',
           'Enter a Prolog Assertion', '');
  Output.items.Clear;
  OutLabel.Caption := s;
  try
    LSEng.AssertzPStr(s);
  except
    on E: ELogicServer do
       ShowMessage('Assertion Error: ' + E.Message);
  end;
end;

end.
