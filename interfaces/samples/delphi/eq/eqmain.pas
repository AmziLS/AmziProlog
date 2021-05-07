unit Eqmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, p_draw, Amzi, ExtCtrls, Buttons, ShellAPI;

  function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;

type
  TMF = class(TForm)
    eqEditBox: TEdit;
    eqPaintBox: TPaintBox;
    Label2: TLabel;
    LSEng: TLSEngine;
    Draw: TBitBtn;
    Memo1: TMemo;
    Label1: TLabel;
    HelpButton: TButton;
    CloseButton: TButton;
    procedure eqPaintBoxPaint(Sender: TObject);
    procedure DrawClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MF: TMF;

implementation

{$R *.DFM}

function p_message(EngID: TEngID): TTFi; stdcall; export;
begin
  ShowMessage( MF.LSEng.GetPStrParm(1) );
  Result := lsTrue;
end;

{ PaintBox onPaint - When the paint box needs to be
  redrawn, the Prolog predicate paint_equation/1 is
  called.  It will use the extended graphics predicates
  to actually draw the equation in the paint box whose
  control address was passed as the single argument.  }

procedure TMF.eqPaintBoxPaint(Sender: TObject);
var
  t: TTerm;
  Can: TCanvas;
begin
  { create Prolog structure, paint_equation/1 }
  LSEng.MakeFA(t, 'paint_equation', 1);
  Can := eqPaintBox.Canvas;
  Can.Brush.Color := clWhite;
  Can.Brush.Style := bsSolid;
  Can.FillRect(Can.ClipRect);
  { unify paint_equation argument with control address }
  LSEng.UnifyArg(t, 1, dADDR, @Can);
  { call Prolog with paint_equation/1 }
  LSEng.Exec(t);
end;

{ When the user presses the 'Draw' button, read the
  edit box to get the mathematical expression.  Use it
  as the argument to the Prolog predicate set_equation/1,
  which will create the drawing commands.  Then cause
  the paint box to be repainted.  }

procedure TMF.DrawClick(Sender: TObject);
var
  s: string;
  t: TTerm;
begin
  s := eqEditBox.text;
  { use the Amzi! string interface to build the structure
    set_equation/1 with the expression as its argument, and
    call Prolog with that structure }
  LSEng.ExecPStr(t, 'set_equation($' + s + '$)');
  eqPaintBox.Repaint;
end;

{ When the form is created, initialize the Logic Server
  and load the Prolog logic base for the EQ example. }

procedure TMF.FormCreate(Sender: TObject);
begin
  LSEng.InitLS('equation');
  LSEng.AddPred('w_message', 1, p_message);
  InitDrawPreds(LSEng);
  LSEng.LoadXPL('equation');
end;

{ When the form is destroyed, close the Logic Server
  freeing the resources used by it. }

procedure TMF.FormDestroy(Sender: TObject);
begin
  LSEng.Close;
end;

procedure TMF.CloseButtonClick(Sender: TObject);
begin
     Close;
end;

{ When the Help button is pressed open the HTML help file. Use the
  Windows API function ShellExecute to do this. }

procedure TMF.HelpButtonClick(Sender: TObject);
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

end.
