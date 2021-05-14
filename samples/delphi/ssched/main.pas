unit Main;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Grids, Buttons, Spin, ExtCtrls, Gauges,
  ShellAPI, Amzi;

  function p_report(EngID: TEngID): TTFi; stdcall; export;
  function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;

{ These are the components on the main form of the application.
  The first in the list, LSEng, is the Logic Server component that
  provides access to the Prolog logic base. }

type
  TMainForm = class(TForm)
    SchedGrid: TStringGrid;
    TeamGrid: TStringGrid;
    TeamCount: TSpinEdit;
    Label2: TLabel;
    Label3: TLabel;
    CycleCount: TSpinEdit;
    Label4: TLabel;
    Setup: TButton;
    Label5: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    SaveDialog: TSaveDialog;
    Label9: TLabel;
    Label10: TLabel;
    Help: TBitBtn;
    ExitButton: TBitBtn;
    Save: TBitBtn;
    Schedule: TBitBtn;
    SchedGauge: TEdit;
    Label8: TLabel;
    LSEng: TLSEngine;
    procedure OnActivate(Sender: TObject);
    procedure ScheduleClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure SetupClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
  private
  public
  end;

var
  MainForm: TMainForm;
  NTeams: Integer;   { Number of teams }
  NGTeams: Integer;  { Even number of teams - includes bye }
  NCycles: Integer;   { Number of times each team plays each other }
  GamesPerRound: Integer;  { Number of games played each round }
  RoundsPerCycle: Integer;  { Number of rounds for everyone to play once }
  NRounds: Integer;  { Number of rounds in schedule }
  BGridsSet, BSchedDone: Boolean;  { Flags to enforce correct sequencing }
  BStopSched: Boolean;  { Flag to stop scheduler }

implementation

{$R *.DFM}

function p_report;
var
   t: TTERM;
begin
     MainForm.LSEng.GetParm(1, dTERM, @t);
     MainForm.SchedGauge.Text := MainForm.LSEng.TermToPStr(t);
     if (BStopSched = false) then
        MainForm.LSEng.UnifyPStrParm(2, 'go')
     else
         MainForm.LSEng.UnifyPStrParm(2, 'stop');
     Result := lsTrue;
end;

{ When the form is activated, the Logic Server is initialized
  and the logic base for the Sports Scheduler is loaded. }

procedure TMainForm.OnActivate(Sender: TObject);
begin
      SchedGrid.Cells[1,0] := 'Away';
      SchedGrid.Cells[2,0] := 'Home';
      SchedGrid.Cells[0,0] := 'Round';
      TeamGrid.Cells[0,0] := 'Teams/Players';
      TeamCount.Value := 0;
      LSEng.InitLS('sched');
      LSEng.AddPred('report', 2, p_report);
      LSEng.LoadXPL('sched');
      BGridsSet := false;
      BSchedDone := false;
end;

{ When the Setup button is clicked, the logic base
  is cleared, and the Delphi grids and variables are
  initialized. }
 
procedure TMainForm.SetupClick(Sender: TObject);
var
   i: integer;
   t: TTERM;
begin
     LSEng.ExecPStr(t, 'clear_schedule');
     NTeams := TeamCount.Value;
     if (NTeams mod 2 = 1) then
        NGTeams := NTeams + 1 else
        NGTeams := NTeams;
     TeamGrid.RowCount := NTeams + 1;
     for i := 1 to NTeams do
         TeamGrid.Cells[0, i] := 'Team ' + IntToStr(i);
     NCycles := CycleCount.Value;
     GamesPerRound := NGTeams div 2;
     RoundsPerCycle := NGTeams - 1;
     NRounds := NCycles * (NGTeams - 1);
     SchedGrid.RowCount := 1 + NCycles * RoundsPerCycle * GamesPerRound;
     for i := 1 to SchedGrid.RowCount do begin
         SchedGrid.Cells[0, i] := '';
         SchedGrid.Cells[1, i] := '';
         SchedGrid.Cells[2, i] := '';
         end;
     for i := 1 to NCycles*RoundsPerCycle do
         SchedGrid.Cells[0, 1+(i-1)*GamesPerRound] :=
                            'round ' + IntToStr(i);
     BGridsSet := true;
     BSchedDone := false;
     SchedGauge.Text := '';
end;

{ When the Schedule button is clicked, the logic base
  is informed of the team and round names using the Logic
  Server API call AssertzPStr.  In this case it is used
  to assert Prolog facts such as

    team(1, $Flying Squirrels$)
    team(2, $Gray Flamingos$)

  and

    round(3, $Oct 31$)

  These facts are not actually used in the scheduling
  process, but are used in the display of the final
  schedule.

  Then the logic base is called to initialize the teams
  and produce the final schedule.  The result of the
  schedule is a number of Prolog facts of the form

    round($Oct 31$, $Flying Squirrels$, $Gray Flamingos$)

  The final section of this function calls the Logic
  Server to retrieve each of those facts and display it
  in the appropriate grid cell. }
   
procedure TMainForm.ScheduleClick(Sender: TObject);
var
   t: TTerm;
   tf: Boolean;
   i, j, row: Integer;
begin
     BStopSched := false;
     if (BGridsSet = false) then
        ShowMessage('Setup grids before scheduling');
     if (BGridsSet = false) then Exit;
     Screen.Cursor := crHourglass;
     for i := 1 to NTeams do
         LSEng.AssertzPStr('team(' + IntToStr(i) + ', $'
            + TeamGrid.Cells[0,i] + '$)');
     for i := 1 to NRounds do begin
         j := 1 + (i-1) * GamesPerRound;
         LSEng.AssertzPStr('round(' + IntToStr(i) + ', $'
            + SchedGrid.Cells[0,j] + '$)');
         end;

     LSEng.ExecPStr(t, 'init_teams');

     tf := LSEng.ExecPStr(t, 'schedule(' + IntToStr(NCycles) + ')' );
     if (tf = false) then Exit;

     tf := LSEng.CallPStr(t, 'round(Round, Away, Home)');
     row := 1;
     while tf do begin
           SchedGrid.Cells[1, row] := LSEng.GetPStrArg(t, 2);
           SchedGrid.Cells[2, row] := LSEng.GetPStrArg(t, 3);
           row := row + 1;
           tf := LSEng.Redo;
           end;
     BSchedDone := true;
     BGridsSet := false;
     Screen.Cursor := crDefault;
     SchedGauge.Text := 'done';
end;

{ The Save button triggers a call to the logic base that
  causes a text report of the schedule to be written to
  the file of the user's choice. }

procedure TMainForm.SaveClick(Sender: TObject);
var
   t: TTERM;
begin
     if (BSchedDone = false) then
        ShowMessage('Run schedule before saving');
     if (BSchedDone = false) then Exit;
     SaveDialog.FileName := 'SCHED.TXT';
     if SaveDialog.Execute then
        LSEng.ExecPStr(t, 'saveas($' + SaveDialog.FileName + '$)' );
end;

{ When the Help button is pressed open the HTML help file. Use the
  Windows API function ShellExecute to do this. }

procedure TMainForm.HelpClick(Sender: TObject);
begin
     ExecuteFile('doc.html', '', '', SW_SHOWNORMAL);
end;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
     Close;
end;

{ When the form is deactivated, the Logic Server is
  closed, freeing all of its resources. }

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
     LSEng.CloseLS;
end;

procedure TMainForm.StopClick(Sender: TObject);
begin
     BStopSched := true;
end;

end.
