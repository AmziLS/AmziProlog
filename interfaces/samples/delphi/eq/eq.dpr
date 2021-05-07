program Eq;

uses
  Forms,
  p_draw in 'p_draw.pas',
  Eqmain in 'eqmain.pas' {MF};

{$R *.RES}

begin
  Application.CreateForm(TMF, MF);
  Application.Run;
end.
