unit DelGUI;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Amzi, StdCtrls, ExtCtrls;

  {To use SciGraph functions add 'SciGraph' to the uses and define the
  symbol 'SCIGRAPH' with a directive}

  procedure DG_Init(LS: TLSEngine);

  function p_showmessage(EngID: TEngID): TTFi; stdcall; export;

  function p_draw_lineto(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_moveto(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_textout(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_textheight(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_textwidth(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_cliprect(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_rectangle(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_ellipse(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_pen(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_font(EngID: TEngID): TTFi; stdcall; export;
  function p_draw_brush(EngID: TEngID): TTFi; stdcall; export;

  function p_memo_add(EngID: TEngID): TTFi; stdcall; export;
  function p_memo_lines(EngID: TEngID): TTFi; stdcall; export;
  function p_memo_delete(EngID: TEngID): TTFi; stdcall; export;

  function p_edit_text(EngID: TEngID): TTFi; stdcall; export;
  function p_edit_modified(EngID: TEngID): TTFi; stdcall; export;

  function p_radio_group(EngID: TEngID): TTFi; stdcall; export;
  function p_radio_selected(EngID: TEngID): TTFi; stdcall; export;

  function p_ctrl_visible(EngID: TEngID): TTFi; stdcall; export;

  { Functions that make use of third-party components }

{$ifdef SCIGRAPH}
  function p_graph_function(EngID: TEngID): TTFi; stdcall; export;
{$endif}
var
  LSEng:  TLSEngine;
  TheCanvas:  TCanvas;
  TheMemo:  TMemo;
  TheEdit:  TEdit;
  TheControl: TControl;
{$ifdef SCIGRAPH}
  TheGraph:  TSciGraph;
{$endif}
  TheRadioGroup:  TRadioGroup;

implementation

procedure DG_Init(LS: TLSEngine);
begin
  LSEng := LS;

  LSEng.AddPred('showmessage', 1, p_showmessage);

  LSEng.AddPred('draw_lineto', 3, p_draw_lineto);
  LSEng.AddPred('draw_moveto', 3, p_draw_moveto);
  LSEng.AddPred('draw_textout', 4, p_draw_textout);
  LSEng.AddPred('draw_textheight', 3, p_draw_textheight);
  LSEng.AddPred('draw_textwidth', 3, p_draw_textwidth);
  LSEng.AddPred('draw_cliprect', 5, p_draw_cliprect);
  LSEng.AddPred('draw_rectangle', 5, p_draw_rectangle);
  LSEng.AddPred('draw_ellipse', 5, p_draw_ellipse);
  LSEng.AddPred('draw_pen', 3, p_draw_pen);
  LSEng.AddPred('draw_font', 4, p_draw_font);
  LSEng.AddPred('draw_brush', 3, p_draw_brush);

  LSEng.AddPred('memo_add', 2, p_memo_add);
  LSEng.AddPred('memo_lines', 3, p_memo_lines);
  LSEng.AddPred('memo_delete', 2, p_memo_delete);

  LSEng.AddPred('edit_text', 2, p_edit_text);
  LSEng.AddPred('edit_modified', 1, p_edit_modified);

  LSEng.AddPred('radio_group', 2, p_radio_group);
  LSEng.AddPred('radio_selected', 2, p_radio_selected);

  LSEng.AddPred('ctrl_visible', 2, p_ctrl_visible);

  { functions that make use of third-party components }

{$ifdef SCIGRAPH}
  LSEng.AddPred('graph_function', 5, p_graph_function);
{$endif}
end;

function p_showmessage;
begin
  ShowMessage( LSEng.GetPStrParm(1) );
  Result := lsTrue;
end;

function p_draw_lineto;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.LineTo(LSEng.GetIntParm(2), LSEng.GetIntParm(3));
  Result := lsTrue;
end;

function p_draw_moveto;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.MoveTo(LSEng.GetIntParm(2), LSEng.GetIntParm(3));
  Result := lsTrue;
end;

function p_draw_textout;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.TextOut(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetPStrParm(4));
  Result := lsTrue;
end;

function p_draw_textheight;
var
  N: Integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  N := TheCanvas.TextHeight(LSEng.GetPStrParm(2));
  LSEng.UnifyIntParm(3, N);
  Result := lsTrue;
end;

function p_draw_textwidth;
var
  N: Integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  N := TheCanvas.TextWidth(LSEng.GetPStrParm(2));
  LSEng.UnifyIntParm(3, N);
  Result := lsTrue;
end;

function p_draw_cliprect;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  LSEng.UnifyIntParm(2, TheCanvas.ClipRect.Top);
  LSEng.UnifyIntParm(3, TheCanvas.ClipRect.Left);
  LSEng.UnifyIntParm(4, TheCanvas.ClipRect.Bottom);
  LSEng.UnifyIntParm(5, TheCanvas.ClipRect.Right);
  Result := lsTrue;
end;

function p_draw_rectangle;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.Rectangle(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetIntParm(4), LSEng.GetIntParm(5));
  Result := lsTrue;
end;

function p_draw_ellipse;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.Ellipse(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetIntParm(4), LSEng.GetIntParm(5));
  Result := lsTrue;
end;

function p_draw_pen;
var
  ColorNum: Longint;
  ColorName: String;
  PenWidth: Integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);

  Result := lsFalse;
  ColorName := LSEng.GetPStrParm(2);
  if (not IdentToColor(ColorName, ColorNum)) then
    Exit;
  PenWidth := LSEng.GetIntParm(3);
  if (PenWidth < 1) then
    Exit;

  TheCanvas.Pen.Color := ColorNum;
  TheCanvas.Pen.Width := PenWidth;
  Result := lstrue;
end;

function p_draw_font;
var
  ColorNum: Longint;
  ColorName: String;
  Size: integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);

  if (LSEng.GetParmType(2) = pVAR) then
    LSEng.UnifyAtomParm(2, TheCanvas.Font.Name)
  else
    TheCanvas.Font.Name := LSEng.GetPStrParm(2);

  if (LSEng.GetParmType(3) = pVAR) then begin
    ColorNum := TheCanvas.Font.Color;
    ColorToIdent(ColorNum, ColorName);
    LSEng.UnifyAtomParm(3, ColorName);
    end
  else begin
    ColorName := LSEng.GetPStrParm(3);
    if (not IdentToColor(ColorName, ColorNum)) then begin
      ShowMessage('Invalid color name for font');
      Result := lsFalse;
      exit;
      end;
    TheCanvas.Font.Color := ColorNum;
    end;

  if (LSEng.GetParmType(4) = pVAR) then begin
    Size := TheCanvas.Font.Size;
    LSEng.UnifyIntParm(4, Size);
    end
  else begin
    Size := LSEng.GetIntParm(4);
    TheCanvas.Font.Size := Size;
    end;

  Result := lsTrue;
end;

function p_draw_brush;
var
  ColorNum: Longint;
  ColorName: String;
  Style: String;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);

  Result := lsFalse;
  ColorName := LSEng.GetPStrParm(2);
  if (not IdentToColor(ColorName, ColorNum)) then
    Exit;
  TheCanvas.Brush.Color := ColorNum;

  Style := LSEng.GetPStrParm(3);
  if (Style = 'bsSolid') then      TheCanvas.Brush.Style := bsSolid;
  if (Style = 'bsClear') then      TheCanvas.Brush.Style := bsClear;
  if (Style = 'bsBDiagonal') then  TheCanvas.Brush.Style := bsBDiagonal;
  if (Style = 'bsFDiagonal') then  TheCanvas.Brush.Style := bsFDiagonal;
  if (Style = 'bsCross') then      TheCanvas.Brush.Style := bsCross;
  if (Style = 'bsDiagCross') then  TheCanvas.Brush.Style := bsDiagCross;
  if (Style = 'bsHorizontal') then TheCanvas.Brush.Style := bsHorizontal;
  if (Style = 'bsVertical') then   TheCanvas.Brush.Style := bsVertical;
 
  Result := lsTrue;
end;

function p_memo_add;
begin
  LSEng.GetParm(1, dADDR, @TheMemo);
  TheMemo.Lines.Add(LSEng.GetPStrParm(2));
  Result := lsTrue;
end;

function p_memo_lines;
begin
  LSEng.GetParm(1, dADDR, @TheMemo);
  LSEng.UnifyPStrParm(3, TheMemo.Lines[LSEng.GetIntParm(2)]);
  Result := lsTrue;
end;

function p_memo_delete;
begin
  LSEng.GetParm(1, dADDR, @TheMemo);
  TheMemo.Lines.Delete(LSEng.GetIntParm(2));
  Result := lsTrue;
end;

function p_edit_text;
begin
  LSEng.GetParm(1, dADDR, @TheEdit);
  if (LSEng.GetParmType(2) = pVAR) then
    LSEng.UnifyPStrParm(2, TheEdit.Text)
  else
    TheEdit.Text := LSEng.GetPStrParm(2);
  Result := lsTrue;
end;

function p_edit_modified;
begin
  LSEng.GetParm(1, dADDR, @TheEdit);
  if (TheEdit.Modified) then
    Result := lsTrue
  else
    Result := lsFalse;
end;

function p_radio_group;
var
  ltrm:  TTerm;
  caption:  string;
  rc: integer;
begin
  LSEng.GetParm(1, dADDR, @TheRadioGroup);
  TheRadioGroup.Items.Clear;

  LSEng.GetParm(2, dTERM, @ltrm);

  repeat
    rc := LSEng.PopPStrList(ltrm, caption);
    if (rc = 0) then TheRadioGroup.Items.Add(caption);
  until (rc <> 0);

  TheRadioGroup.ItemIndex := -1;

  Result := lsTrue;
end;

function p_radio_selected;
begin
  LSEng.GetParm(1, dADDR, @TheRadioGroup);
  Result := lsTrue;

  if (LSEng.GetParmType(2) = pVAR) then
  begin
    if (TheRadioGroup.ItemIndex = -1) then
      Result := lsFalse
    else
      LSEng.UnifyIntParm(2, TheRadioGroup.ItemIndex);
  end
  else
    TheRadioGroup.ItemIndex := LSEng.GetIntParm(2);
end;

function p_ctrl_visible;
begin
  LSEng.GetParm(1, dADDR, @TheControl);
  if (LSEng.GetIntParm(2) <> 0) then
    TheControl.Visible := True
  else
    TheControl.Visible := False;
  Result := lsTrue;
end;

{ Functions that make use of third-party components }
{$ifdef SCIGRAPH}
function p_graph_function;
{
This function is used to map a Prolog symbolic
equation such as f(X) = X**2 to a scientific
graph.  It requires the SciGraph component
available from:

Pierre Mertz
phmertz@princeton.edu

WWW site: http://www.ee.princeton.edu/~phmertz/scigraph/scigraph.html
}

var
  XData, YData: array[1..100] of Double;
  ftrm, ltrm, t: TTerm;
  i, rc: Integer;
  YMax, YMin: Double;
  Start, Stop, Inc: Double;
  Func: String;
begin
  LSEng.GetParm(1, dADDR, @TheGraph);

  FillChar(XData, SizeOf(XData), 0);
  FillChar(YData, SizeOf(YData), 0);

  if (LSEng.GetParmType(2) = pINT) then
    Start := LSEng.GetIntParm(2)*1.0
  else
    Start := LSEng.GetFloatParm(2);
  if (LSEng.GetParmType(3) = pINT) then
    Stop := LSEng.GetIntParm(3)*1.0
  else
    Stop := LSEng.GetFloatParm(3);
  if (LSEng.GetParmType(4) = pINT) then
    Inc := LSEng.GetIntParm(4)*1.0
  else
    Inc := LSEng.GetFloatParm(4);

  func := format('pg$getpointlist(List, YMin, YMax, %n, %n, %n, %s)',
    [Start, Stop, Inc, LSEng.GetPStrParm(5)]);

  { Get a list of points from Prolog }
  LSEng.ExecPStr(ftrm, func);
  LSEng.GetArg(ftrm, 1, dTERM, @ltrm);

  { Get the Y min and max }
  YMin := LSEng.GetFloatArg(ftrm, 2);
  YMax := LSEng.GetFloatArg(ftrm, 3);

  i := 1;
  repeat
    rc := LSEng.PopList(ltrm, dTERM, @t);
    if (rc = 0) then
    begin
      if (LSEng.GetArgType(t, 1) = pINT) then
         XData[i] := LSEng.GetIntArg(t, 1)*1.0
      else
         XData[i] := LSEng.GetFloatArg(t, 1);

      if (LSEng.GetArgType(t, 2) = pINT) then
        YData[i] := LSEng.GetIntArg(t, 2)*1.0
      else;
        YData[i] := LSEng.GetFloatArg(t, 2);

      i := i + 1;
    end
  until (rc <> 0);

  TheGraph.Plot(YData);
  TheGraph.SetX(XData);

  Result := lsTrue;
end;
{$endif}

end.
