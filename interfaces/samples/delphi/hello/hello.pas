unit Hello;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Amzi;

type
  TForm1 = class(TForm)
    Hello: TButton;
    ls: TLSEngine;

    procedure HelloClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.HelloClick(Sender: TObject);
var
  t: TTerm;
  s: string;
begin
  ls.InitLS('hello');  { Initialize engine }
  ls.LoadXPL('hello'); { Load .xpl file }
  { Query the Logic Server }
  ls.ExecPStr(t, 'hello($Delphi Programmer$, X)');
  { Get the second argument of the result back }
  s := ls.GetPStrArg(t, 2);
  ShowMessage(s);  { Display the result }
  ls.Close;  { Close engine }
end;

end.
