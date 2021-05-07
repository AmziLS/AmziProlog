unit delphi_pets_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleServer, amzicom_TLB;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    SoundListBox: TListBox;
    Label2: TLabel;
    PetEdit: TEdit;
    GetPetButton: TButton;
    LS: TComLogicServer;
    procedure GetPetButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


procedure TMainForm.GetPetButtonClick(Sender: TObject);
var
   i: Integer;
   term: longint;
   pet: WideString;
begin

   { Connect to the COM Object }
   LS.Connect();

   { Initialize and load our xpl file }
   LS.Init('');
   LS.Load('c:\amzi\dev\a6\src\samples\com\delphi_pets\pets.xpl');

   { If a sound is selected get the pet }
   i := SoundListBox.ItemIndex;
   if i >= 0 then
   begin
      { Assert the sound }
      LS.AssertaStr('sound(' + SoundListBox.Items.Strings[i] +')');

      { Determine the pet }
      LS.ExecStr('pet(X)', term);
      LS.GetStrArg(term, 1, pet);
        
      { And display it }
      PetEdit.Text := pet
   end;
   
   LS.Close();
end;

end.
