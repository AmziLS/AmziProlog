program Testp;

uses
  Forms,
  Amzi,
  Test in 'test.pas' {LogicServerTest};

{$R *.RES}

begin
  Application.CreateForm(TLogicServerTest, LogicServerTest);
  Application.Run;
end.
