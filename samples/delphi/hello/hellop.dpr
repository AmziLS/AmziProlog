program Hellop;

uses
  Forms,
  hello in 'hello.pas' {Form1},
  amzi in '..\..\..\delphi\amzi.pas';

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
