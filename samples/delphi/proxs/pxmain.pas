unit Pxmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, StdCtrls, Goald, Amzi, ShellAPI;

  function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;

{ Function definitions for extended predicates }

  function px_write(EngID: TEngID): TTFi; stdcall; export;
  function px_clear(EngID: TEngID): TTFi; stdcall; export;
  function px_ynprompt(EngID: TEngID): TTFi; stdcall; export;
  function px_menuprompt(EngID: TEngID): TTFi; stdcall; export;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    TopGoal: TMenuItem;
    OpenDialog1: TOpenDialog;
    Output: TMemo;
    Prompt: TMemo;
    Choices: TListBox;
    XSFile: TLabel;
    Help2: TMenuItem;
    Contents2: TMenuItem;
    Goal1: TMenuItem;
    How1: TMenuItem;
    Facts1: TMenuItem;
    Rules1: TMenuItem;
    Clear1: TMenuItem;
    WhyNot1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LS: TLSEngine;
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure ChoicesClick(Sender: TObject);
    procedure TopGoalClick(Sender: TObject);
    procedure Goal1Click(Sender: TObject);
    procedure How1Click(Sender: TObject);
    procedure Facts1Click(Sender: TObject);
    procedure Rules1Click(Sender: TObject);
    procedure Contents2Click(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure WhyNot1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  BChoice: Boolean;

implementation

{$R *.DFM}

{ px_write/1 is called from Prolog to write something to the
   output box. }

function px_write;
var
   t: TTERM;
begin
     with MainForm do begin
          LS.GetParm(1, dTERM, @t);
          Output.Lines.Add( LS.TermToPStr(t) );
     end;
     Result := lsTrue;
end;

{ px_clear/0 clears the output box. }

function px_clear;
begin
     MainForm.Output.Lines.Clear;
     Result := lsTrue;
end;

{ px_ynprompt presents a yes/no prompt to the user, and waits for a
   response, which is then returned to Prolog. }

function px_ynprompt;
var
   t: TTERM;
   tf: Boolean;
   i: integer;
begin
     with MainForm do begin
          Prompt.Lines.Clear;
          Choices.Items.Clear;
          BChoice := false;
          LS.GetParm(1, dTERM, @t);
          Prompt.Lines.Add( LS.TermToPStr(t) );
          Choices.Items.Add( 'yes' );
          Choices.Items.Add( 'no' );
          Choices.Items.Add( 'why' );
          Choices.Items.Add( 'quit' );
     end;

     while (BChoice = false) do
           Application.ProcessMessages;

     i := MainForm.Choices.ItemIndex;
     tf := MainForm.LS.UnifyAtomParm( 2, MainForm.Choices.Items[i] );

     MainForm.Choices.Items.Clear;
     MainForm.Prompt.Lines.Clear;

     if (tf) then Result := lsTrue
     else Result := lsFalse;
end;

{ px_menuprompt presents a menu of choices to the user and waits
   for a response, which is then returned to Prolog. }

function px_menuprompt;
var
   t: TTERM;
   s: string;
   tf: Boolean;
   i: integer;
begin
     with MainForm do begin
          Prompt.Lines.Clear;
          Choices.Items.Clear;
          BChoice := false;
          LS.GetParm(1, dTERM, @t);
          Prompt.Lines.Add( LS.TermToPStr(t) );
          LS.GetParm(2, dTERM, @t);
          while 0 = LS.PopPStrList(t, s) do
                Choices.Items.Add( s );
          Choices.Items.Add( 'why' );
          Choices.Items.Add( 'quit' );
     end;

     while (BChoice = false) do
           Application.ProcessMessages;

     i := MainForm.Choices.ItemIndex;
     tf := MainForm.LS.UnifyAtomParm( 3, MainForm.Choices.Items[i] );

     MainForm.Choices.Items.Clear;
     MainForm.Prompt.Lines.Clear;

     if (tf) then Result := lsTrue
     else Result := lsFalse;
end;

{ Initialize the Logic Server and the four extended predicates, and load
   the PROXS compiled Prolog load module. }

procedure TMainForm.FormActivate(Sender: TObject);
begin
     LS.InitLS('');
     LS.AddPred('px_write', 1, px_write);
     LS.AddPred('px_clear', 0, px_clear);
     LS.AddPred('px_ynprompt', 2, px_ynprompt);
     LS.AddPred('px_menuprompt', 3, px_menuprompt);
     LS.LoadXPL('proxs');
     Application.HelpFile := 'proxs.hlp';
end;

{ Close the Logic Server when the form deativates. }

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
     LS.CloseLS;
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
     Close;
end;

{ The various controls of the main form cause various queries to
   be posed to the PROXS engine, such as load a knowledge base,
   call a goal, pose a how question, etc. }

procedure TMainForm.Open1Click(Sender: TObject);
var
   t: TTERM;
begin
     OpenDialog1.Execute;
     LS.ExecPStr(t, 'clear');
     LS.ExecPStr(t, 'set_prolog_flag(string_esc, off)');
     LS.ExecPStr(t, Format('load_kb(''%s'')', [OpenDialog1.FileName]));
     LS.ExecPStr(t, 'set_prolog_flag(string_esc, on)');
     XSFile.Caption := OpenDialog1.FileName;
end;

procedure TMainForm.ChoicesClick(Sender: TObject);
begin
     BChoice := true;
end;

procedure TMainForm.TopGoalClick(Sender: TObject);
var
   t: TTERM;
begin
     LS.ExecPStr(t, 'solve');
end;

procedure TMainForm.Goal1Click(Sender: TObject);
var
   i: integer;
   t: TTERM;
begin
     GoalDlg.Goal := QUERY;
     GoalDlg.ShowModal;
     if (idOK = GoalDlg.ModalResult) then begin
        i := GoalDlg.GoalList.ItemIndex;
        LS.ExecPStr(t, Format('goal(%s)', [GoalDlg.GoalList.Items[i]]) );
        end;
end;

procedure TMainForm.How1Click(Sender: TObject);
var
   i: integer;
   t: TTERM;
begin
     GoalDlg.Goal := HOW;
     GoalDlg.ShowModal;

     if (mrOK = GoalDlg.ModalResult) then begin
        i := GoalDlg.GoalList.ItemIndex;
        LS.ExecPStr(t, Format('how(%s)', [GoalDlg.GoalList.Items[i]]) );
        end;
end;

procedure TMainForm.WhyNot1Click(Sender: TObject);
var
   i: integer;
   t: TTERM;
begin
     GoalDlg.Goal := WHYNOT;
     GoalDlg.ShowModal;
     if (mrOK = GoalDlg.ModalResult) then begin
        i := GoalDlg.GoalList.ItemIndex;
        LS.ExecPStr(t, Format('whynot(%s)', [GoalDlg.GoalList.Items[i]]) );
        end;
end;

procedure TMainForm.Facts1Click(Sender: TObject);
var
   t: TTERM;
   tf: Boolean;
begin
     Output.Lines.Clear;
     tf := LS.CallPStr(t, 'known(X,A,V)');
     while (tf = true) do begin
           Output.Lines.Add( LS.TermToPStr(t) );
           tf := LS.Redo;
           end;
end;

procedure TMainForm.Rules1Click(Sender: TObject);
begin
     Output.Lines.Clear;
     Output.Lines.LoadFromFile( XSFile.Caption );
end;

{ When the Help command is pressed open the HTML help file. Use the
  Windows API function ShellExecute to do this. }

procedure TMainForm.Contents2Click(Sender: TObject);
begin
     ExecuteFile('doc.html', '', '', SW_SHOWNORMAL);
end;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

procedure TMainForm.Clear1Click(Sender: TObject);
var
   t: TTERM;
begin
     LS.ExecPStr(t, 'clear');
end;


end.
