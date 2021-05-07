program Proxs;

uses
  Forms,
  Pxmain in 'PXMAIN.PAS' {MainForm},
  Goald in 'GOALD.PAS' {GoalDlg};

{$R *.RES}

begin
  Application.Title := 'Bird Watcher';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TGoalDlg, GoalDlg);
  Application.Run;
end.
