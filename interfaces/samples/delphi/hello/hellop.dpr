program Hellop;

uses
  Forms,
  Hello in 'hello.pas' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
