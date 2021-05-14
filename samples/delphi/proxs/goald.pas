unit Goald;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Amzi, Buttons;

  type
  TGoal = (QUERY, HOW, WHYNOT);
  TGoalDlg = class(TForm)
    GoalList: TListBox;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure OKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    FGoal: TGoal;
  public
    property Goal: TGoal read FGoal write FGoal;
  end;

var
  GoalDlg: TGoalDlg;

implementation

uses
    Pxmain;
{$R *.DFM}



procedure TGoalDlg.OKClick(Sender: TObject);
begin
     Close;
end;

procedure TGoalDlg.FormActivate(Sender: TObject);
var
   t, tlist: TTERM;
   tf: Boolean;
   s: string;
begin
     GoalList.Items.Clear;
     case Goal of
     QUERY: begin
            MainForm.LS.CallPStr(t, 'xs_goals(X)');
            MainForm.LS.GetArg(t, 1, dTERM, @tlist);
            end;
     HOW: begin
          MainForm.LS.CallPStr(t, 'how_goals(X)');
          MainForm.LS.GetArg(t, 1, dTERM, @tlist);
          end;
     WHYNOT: begin
             MainForm.LS.CallPStr(t, 'whynot_goals(X)');
             MainForm.LS.GetArg(t, 1, dTERM, @tlist);
             end;
     end;
     while 0 = MainForm.LS.PopPStrList(tlist, s) do
           GoalList.Items.Add( s );
end;

end.
