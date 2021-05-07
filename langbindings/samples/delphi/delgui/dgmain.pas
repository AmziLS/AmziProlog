unit DGmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Amzi, DelGUI;

type
  Tmainform = class(TForm)
    Test: TButton;
    LSEng: TLSEngine;
    MemoBox: TMemo;
    procedure TestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mainform: Tmainform;

implementation

{$R *.DFM}

procedure Tmainform.TestClick(Sender: TObject);
var
  t: TTerm;
begin
  LSEng.ExecPStr(t, 'test');
end;

procedure Tmainform.FormCreate(Sender: TObject);
var
  t: TTerm;
begin
  LSEng.InitLS('');
  {Insert initialization for other extended predicates here}
  DG_Init(LSEng);
  {Load a .XPL file that was linked with delgui.plm}
  LSEng.LoadXPL('dgtemp');
  LSEng.MakeFA(t, 'memobox', 2);
  LSEng.UnifyIntArg(t, 1, 1);
  LSEng.UnifyArg(t, 2, dADDR, @MemoBox);
  LSEng.Asserta(t);
end;

procedure Tmainform.FormDestroy(Sender: TObject);
begin
  LSEng.Close;
end;

end.
