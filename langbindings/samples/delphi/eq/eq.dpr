program Eq;

uses
  Forms,
  p_draw in 'p_draw.pas',
  eqmain in 'eqmain.pas' {MF},
  amzi in '..\..\..\delphi\amzi.pas';

{$R *.RES}

begin
  Application.CreateForm(TMF, MF);
  Application.Run;
end.
