program Ssched;

uses
  Forms,
  main in 'main.pas' {MainForm},
  amzi in '..\..\..\delphi\amzi.pas';

{$R *.RES}

begin
  Application.Title := 'Amzi! Sports Scheduler Demo';
  Application.HelpFile := 'C:\AMZIAPPS\SCHED\SSCHED.HLP';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
