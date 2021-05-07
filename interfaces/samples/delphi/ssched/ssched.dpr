program Ssched;

uses
  Forms,
  Main in 'main.pas' {MainForm};

{$R *.RES}

begin
  Application.Title := 'Amzi! Sports Scheduler Demo';
  Application.HelpFile := 'C:\AMZIAPPS\SCHED\SSCHED.HLP';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
