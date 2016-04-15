unit p_draw;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, Amzi, StdCtrls, ExtCtrls;

  procedure InitDrawPreds(LS: TLSEngine);

{ Function definitions for the Delphi functions that
  implement the various extended predicates. }

  function p_lineto(EngID: TEngID): TTFi; stdcall; export;
  function p_moveto(EngID: TEngID): TTFi; stdcall; export;
  function p_textout(EngID: TEngID): TTFi; stdcall; export;
  function p_textheight(EngID: TEngID): TTFi; stdcall; export;
  function p_textwidth(EngID: TEngID): TTFi; stdcall; export;
  function p_cliprect(EngID: TEngID): TTFi; stdcall; export;
  function p_rectangle(EngID: TEngID): TTFi; stdcall; export;
  function p_ellipse(EngID: TEngID): TTFi; stdcall; export;
  function p_pen(EngID: TEngID): TTFi; stdcall; export;
  function p_font(EngID: TEngID): TTFi; stdcall; export;
  function p_brush(EngID: TEngID): TTFi; stdcall; export;

var
  TheCanvas:  TCanvas;
  LSEng:  TLSEngine;

implementation

{ Logic Server initialization code that associates a Prolog
  predicate with a Delphi function.  For example, the first
  call to AddPred associates the Delphi function p_lineto
  with the predicate draw_lineto/3. }

procedure InitDrawPreds(LS: TLSEngine);
begin
  LSEng := LS;
  LSEng.AddPred('draw_lineto', 3, p_lineto);
  LSEng.AddPred('draw_moveto', 3, p_moveto);
  LSEng.AddPred('draw_textout', 4, p_textout);
  LSEng.AddPred('draw_textheight', 3, p_textheight);
  LSEng.AddPred('draw_textwidth', 3, p_textwidth);
  LSEng.AddPred('draw_cliprect', 5, p_cliprect);
  LSEng.AddPred('draw_rectangle', 5, p_rectangle);
  LSEng.AddPred('draw_ellipse', 5, p_ellipse);
  LSEng.AddPred('draw_pen', 3, p_pen);
  LSEng.AddPred('draw_font', 4, p_font);
  LSEng.AddPred('draw_brush', 3, p_brush);
end;

{ For each of the implementations, the first argument is the address
  of the control that is to receive the drawing commands.  The other
  arguments are then specific to the operation being performed. }

{ draw_lineto/3 expects the second and third arguments to be instantiated
  to integers, which are then retrieved from Prolog and passed to the
  Delphi LineTo function for canvases.  This same approach is used
  for many of these predicates. }

function p_lineto;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.LineTo(LSEng.GetIntParm(2), LSEng.GetIntParm(3));
  Result := lsTrue;
end;

function p_moveto;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.MoveTo(LSEng.GetIntParm(2), LSEng.GetIntParm(3));
  Result := lsTrue;
end;

function p_textout;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.TextOut(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetPStrParm(4));
  Result := lsTrue;
end;

{ draw_textheight/3 unifies the Prolog argument with the result
  of the Delphi TextHeight function.  If the Prolog argument is
  an unbound variable, this function returns the value.  If the
  Prolog argument is bound, then the predicate succeeds or fails
  depending on whether the unification succeeds or fails. }
 
function p_textheight;
var
  N: Integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  N := TheCanvas.TextHeight(LSEng.GetPStrParm(2));
  if LSEng.UnifyIntParm(3, N) then
     Result := lstrue
  else
      Result := lsfalse;
end;

function p_textwidth;
var
  N: Integer;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  N := TheCanvas.TextWidth(LSEng.GetPStrParm(2));
  if LSEng.UnifyIntParm(3, N) then
     Result := lstrue
  else
      Result := lsfalse;
end;

function p_cliprect;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  LSEng.UnifyIntParm(2, TheCanvas.ClipRect.Top);
  LSEng.UnifyIntParm(3, TheCanvas.ClipRect.Left);
  LSEng.UnifyIntParm(4, TheCanvas.ClipRect.Bottom);
  LSEng.UnifyIntParm(5, TheCanvas.ClipRect.Right);
  Result := lsTrue;
end;

function p_rectangle;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.Rectangle(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetIntParm(4), LSEng.GetIntParm(5));
  Result := lsTrue;
end;

function p_ellipse;
begin
  LSEng.GetParm(1, dADDR, @TheCanvas);
  TheCanvas.Ellipse(LSEng.GetIntParm(2), LSEng.GetIntParm(3), LSEng.GetIntParm(4), LSEng.GetIntParm(5));
  Result := lsTrue;
end;

function p_pen;
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

{ draw_font/4 checks each of the arguments to see if its bound
  or not.  If its bound, then the predicate sets the corresponding font
  property.  If its unbound,then it returns the current property.  }

function p_font;
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

{ draw_brush/3 maps Prolog atoms describing various styles to
  Delphi brush styles. }

function p_brush;
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

end.
