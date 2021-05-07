program delphi_pets;

uses
  Forms,
  delphi_pets_unit in 'delphi_pets_unit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
