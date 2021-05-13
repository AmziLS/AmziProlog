program Proxs;

uses
  Forms,
  PXMAIN in 'PXMAIN.PAS' {MainForm},
  GOALD in 'GOALD.PAS' {GoalDlg},
  amzi in '..\..\..\delphi\amzi.pas';

{$R *.RES}

begin
  Application.Title := 'Bird Watcher';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TGoalDlg, GoalDlg);
  Application.Run;
end.
