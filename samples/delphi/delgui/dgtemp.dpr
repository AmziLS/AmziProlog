program DGtemp;

uses
  Forms,
  Amzi in '..\..\..\delphi\amzi.pas',
  DGmain in 'dgmain.pas' {mainform},
  DelGUI in 'delgui.pas';

{$R *.RES}

begin
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.
