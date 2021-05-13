program Testp;

uses
  Forms,
  Amzi in '..\..\..\delphi\amzi.pas',
  Test in 'test.pas' {LogicServerTest};

{$R *.RES}

begin
  Application.CreateForm(TLogicServerTest, LogicServerTest);
  Application.Run;
end.
