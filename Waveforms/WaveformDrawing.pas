{
    Copyright (C) 2025 VCC
    creation date: 30 Oct 2023   (most of the code from 2016)
    initial release date: 31 Dec 2023

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit WaveformDrawing;

interface

uses
  {$IFDEF FPC}
    {$IFDEF Windows}
      Windows,
    {$ELSE}
      LCLType, LCLIntf,
    {$ENDIF}
  {$ELSE}
    Windows,
  {$ENDIF}
  SysUtils, Classes, Graphics,
  WaveformUtils;


procedure DrawSignalWaveForm(ACanvas: TCanvas; ASignalRec: PSignalRec; CellRect: TRect;
                             NodeHeight, AWaveformDrawingWidth: Integer;
                             IsSelected, APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting, ADrawGreenZero, ADrawXes: Boolean;
                             AScrBarPos: Integer; var AMainMarkerPos: Integer;
                             AZoom: Extended);
  

implementation


uses
  Math, WaveformFuncUtils;


procedure DrawClockTransitions(ACanvas: TCanvas; ASignal: PSignal; CellRect: TRect; TransitionIndex, y0, y1, ScrBarPos: Integer; Zoom: Extended);
var
  i: Integer;
  FirstVisibleMoment, LastVisibleMoment: Integer;
  CurrentXTranMid, CurrentXTranEnd, NextXTranMid: Integer;
  XMultMid, XMultEnd, NextMultMid: Integer;
  ClockPeriod, NumberOfVisiblePeriods: Integer;  //Moment scale
begin
  FirstVisibleMoment := ASignal^.SignalEvents[TransitionIndex].Moment;
  LastVisibleMoment := ASignal^.SignalEvents[TransitionIndex + 1].Moment;

  if ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.MidTransitionOffset < 1 then
    raise Exception.Create('MidTransitionOffset must be set to a value greater than 0.');

  if ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.EndTransitionOffset < 2 then
    raise Exception.Create('EndTransitionOffset must be set to a value greater than 1.');

  ClockPeriod := ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.EndTransitionOffset;
  NumberOfVisiblePeriods := (LastVisibleMoment - FirstVisibleMoment) div ClockPeriod;

  ACanvas.Pen.Color := clOlive;

  XMultMid := ASignal^.SignalEvents[TransitionIndex].Moment + ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.MidTransitionOffset;
  CurrentXTranMid := CellRect.Left + 1 + Round((XMultMid - ScrBarPos) * Zoom);
  XMultEnd := ASignal^.SignalEvents[TransitionIndex].Moment - ClockPeriod + ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.EndTransitionOffset;
  CurrentXTranEnd := CellRect.Left + 1 + Round((XMultEnd - ScrBarPos) * Zoom);
  Line(ACanvas, CurrentXTranEnd, y1, CurrentXTranMid, y1);  //horiz

  ACanvas.Pen.Color := $0000AA00;
  Line(ACanvas, CurrentXTranEnd, y0, CurrentXTranEnd, y1); //vertical Line

  ACanvas.Pen.Color := clOlive;
  for i := 1 to NumberOfVisiblePeriods do
  begin
    //ACanvas.Pen.Color := clRed;
    XMultMid := ASignal^.SignalEvents[TransitionIndex].Moment + (i - 1) * ClockPeriod + ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.MidTransitionOffset;
    CurrentXTranMid := CellRect.Left + 1 + Round((XMultMid - ScrBarPos) * Zoom);
    Line(ACanvas, CurrentXTranMid, y0, CurrentXTranMid, y1); //vertical Line

    //ACanvas.Pen.Color := clBlue;
    XMultEnd := ASignal^.SignalEvents[TransitionIndex].Moment + (i - 1) * ClockPeriod + ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.EndTransitionOffset;
    CurrentXTranEnd := CellRect.Left + 1 + Round((XMultEnd - ScrBarPos) * Zoom);
    Line(ACanvas, CurrentXTranEnd, y0, CurrentXTranEnd, y1); //vertical Line

    NextMultMid := ASignal^.SignalEvents[TransitionIndex].Moment + (i - 0) * ClockPeriod + ASignal^.SignalEvents[TransitionIndex].ClockPatternAfterEvent.MidTransitionOffset;
    NextXTranMid := CellRect.Left + 1 + Round((NextMultMid - ScrBarPos) * Zoom);

    //ACanvas.Pen.Color := clGreen;
    Line(ACanvas, CurrentXTranMid, y0, CurrentXTranEnd, y0);  //horiz

    if i <> NumberOfVisiblePeriods then
      Line(ACanvas, CurrentXTranEnd, y1, NextXTranMid, y1);      //horiz

    if (CurrentXTranMid > CellRect.Right) or (CurrentXTranEnd > CellRect.Right) or (NextXTranMid > CellRect.Right) then
      Break;
  end;
end;  


function DrawSignalTransitionFromBinaryToClock(ACanvas: TCanvas; ASignal: PSignal; PrevXTran: Integer; CellRect: TRect; TransitionIndex, y0, y1, ScrBarPos: Integer; Zoom: Extended; DrawGreenZero: Boolean; GreenZeroColor: TColor): Integer;
var
  OldSampleBin, NewSampleBin: Byte;
  XTran: Integer;
  XTranMinus1: Integer;
  XTranPlus1: Integer;
begin
  OldSampleBin := ASignal^.SignalEvents[TransitionIndex].ValueAfterEvent.NumericValue[0];
  NewSampleBin := (not OldSampleBin) and $1;

  //XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex].Moment - ScrBarPos) * Zoom);
  XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex + 1].Moment - ScrBarPos) * Zoom);
  XTranMinus1 := XTran - 1;
  XTranPlus1 := XTran + 1;

  if InIntervalInteger(XTran, CellRect.Left, CellRect.Right) then
  begin
    ACanvas.Pen.Color := clBlack;
    Line(ACanvas, XTran, y0, XTran, y1); //vertical Line
    if ASignal^.SignalEvents[TransitionIndex].Highlighted then
    begin
      ACanvas.Pen.Color := clAqua;
      Line(ACanvas, XTranMinus1, y0, XTranMinus1, y1); //vertical Line
      Line(ACanvas, XTranPlus1, y0, XTranPlus1, y1); //vertical Line
    end;
  end;

  if TransitionIndex = 0 then
  begin
    if OldSampleBin = 0 then
      Line(ACanvas, 1, y0, XTran, y0)
    else
      Line(ACanvas, 1, y1, XTran, y1);
  end //i = 0
  else //i > 0
  begin
    if (TransitionIndex = 1) and (NewSampleBin = 1) then
    begin //debug code
      ACanvas.Pen.Color := clGreen; // $0088BBFF;
      ACanvas.Brush.Color := ACanvas.Pen.Color;
      ACanvas.Rectangle(CellRect.Left + 1, y1 + 1, PrevXTran, y0 + 1);
    end;

    if NewSampleBin = 0 then
    begin
      ACanvas.Pen.Color := GreenZeroColor;
      ACanvas.Brush.Color := ACanvas.Pen.Color;
      ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 {+ 1});
    end;

    if (OldSampleBin = 0) and (NewSampleBin = 1) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y0, XTran, y0);
          
      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTran, y1, CellRect.Right - 1, y1);

      if DrawGreenZero then
        if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        begin
          ACanvas.Pen.Color := GreenZeroColor;
          ACanvas.Brush.Color := GreenZeroColor;
          ACanvas.Rectangle(XTranPlus1, y1 + 1, CellRect.Right, y0 {+ 1});
        end;
    end;

    if (OldSampleBin = 1) and (NewSampleBin = 0) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y1, XTran, y1);

      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTranPlus1, y0, CellRect.Right - 1, y0);

      if DrawGreenZero then
      begin
        ACanvas.Pen.Color := GreenZeroColor;
        ACanvas.Brush.Color := GreenZeroColor;
        ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 {+ 1});
      end;
    end;
  end;//TransitionIndex > 0

  Result := XTran;
end;


function DrawSignalTransitionFromClockToBinary(ACanvas: TCanvas; ASignal: PSignal; PrevXTran: Integer; CellRect: TRect; TransitionIndex, y0, y1, ScrBarPos: Integer; Zoom: Extended; DrawGreenZero: Boolean; GreenZeroColor: TColor): Integer;
var
  OldSampleBin, NewSampleBin: Byte;
  XTran: Integer;
  XTranMinus1: Integer;
  XTranPlus1: Integer;
begin
  NewSampleBin := ASignal^.SignalEvents[TransitionIndex + 1].ValueAfterEvent.NumericValue[0];
  OldSampleBin := (not NewSampleBin) and $1;

  //XTran := CellRect.Left + 1 + Round((ASignal.SignalEvents[TransitionIndex].Moment - ScrBarPos) * Zoom);
  XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex + 1].Moment - ScrBarPos) * Zoom);
  XTranMinus1 := XTran - 1;
  XTranPlus1 := XTran + 1;

  if InIntervalInteger(XTran, CellRect.Left, CellRect.Right) then
  begin
    ACanvas.Pen.Color := clBlack;
    Line(ACanvas, XTran, y0, XTran, y1); //vertical Line
    if ASignal^.SignalEvents[TransitionIndex].Highlighted then
    begin
      ACanvas.Pen.Color := clAqua;
      Line(ACanvas, XTranMinus1, y0, XTranMinus1, y1); //vertical Line
      Line(ACanvas, XTranPlus1, y0, XTranPlus1, y1); //vertical Line
    end;
  end;

  if TransitionIndex = 0 then
  begin
    if OldSampleBin = 0 then
      Line(ACanvas, 1, y0, XTran, y0)
    else
      Line(ACanvas, 1, y1, XTran, y1);
  end //i = 0
  else //i > 0
  begin
    if (TransitionIndex = 1) and (NewSampleBin = 1) then
    begin //debug code
      ACanvas.Pen.Color := clGreen; // $0088BBFF;
      ACanvas.Brush.Color := ACanvas.Pen.Color;
      ACanvas.Rectangle(CellRect.Left + 1, y1 + 1, PrevXTran, y0 + 1);
    end;

    {ACanvas.Pen.Color := clYellow;
    ACanvas.Brush.Color := ACanvas.Pen.Color;
    ACanvas.Brush.Style := bsHorizontal;
    ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 + 1);
    ACanvas.Brush.Style := bsSolid;}

    DrawClockTransitions(ACanvas, ASignal, CellRect, TransitionIndex, y0, y1, ScrBarPos, Zoom);

    if (OldSampleBin = 0) and (NewSampleBin = 1) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y0, XTran, y0);

      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTran, y1, CellRect.Right - 1, y1);

      if DrawGreenZero then
        if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        begin
          ACanvas.Pen.Color := clRed;
          ACanvas.Brush.Color := clRed;
          ACanvas.Rectangle(XTranPlus1, y1 + 1, CellRect.Right, y0 + 1);
        end;
    end;

    if (OldSampleBin = 1) and (NewSampleBin = 0) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y1, XTran, y1);

      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTranPlus1, y0, CellRect.Right - 1, y0);

      if DrawGreenZero then
      begin
        ACanvas.Pen.Color := clRed;
        ACanvas.Brush.Color := clRed;
        ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 + 1);
      end;
    end;
  end;//TransitionIndex > 0

  Result := XTran;
end;


procedure DrawMultiBitTransition(ACanvas: TCanvas; XTran, PrevXTran, y0, y1, ym: Integer);
var
  PrevXTranPlus2: Integer;
  XTranMinus2: Integer;
begin
  XTranMinus2 := XTran - 2;
  PrevXTranPlus2 := PrevXTran + 2;

  Line(ACanvas, PrevXTran, ym, PrevXTranPlus2, y1); //slope              /¯¯
  Line(ACanvas, PrevXTran, ym, PrevXTranPlus2, y0); //slope              \__
  Line(ACanvas, XTranMinus2, y1, XTran, ym); //slope                       ¯¯\
  Line(ACanvas, XTranMinus2, y0, XTran, ym); //slope                       __/
  Line(ACanvas, PrevXTranPlus2, y1, XTranMinus2, y1);  //horiz
  Line(ACanvas, PrevXTranPlus2, y0, XTranMinus2, y0);  //horiz
end;  


function DrawBinarySignalTransition(ACanvas: TCanvas; ASignal: PSignal; PrevXTran: Integer; CellRect: TRect; TransitionIndex, y0, y1, ym, ScrBarPos: Integer; Zoom: Extended; DrawGreenZero: Boolean; GreenZeroColor: TColor): Integer;
var
  OldSampleBin, NewSampleBin: Byte;
  XTran: Integer;
  XTranMinus1: Integer;
  XTranPlus1, XTranPlus2: Integer;
begin
  OldSampleBin := 2;
  NewSampleBin := 2;
  
  if ASignal^.ValueType = sutNumeric then
  begin
    OldSampleBin := ASignal^.SignalEvents[TransitionIndex].ValueAfterEvent.NumericValue[0];
    NewSampleBin := ASignal^.SignalEvents[TransitionIndex + 1].ValueAfterEvent.NumericValue[0];
  end
  else
    if ASignal^.ValueType = sutSTD_LOGIC then
    begin
      if ASignal^.SignalEvents[TransitionIndex].ValueAfterEvent.STD_LOGIC_Value[0] = TSL0 then
        OldSampleBin := 0
      else
        if ASignal^.SignalEvents[TransitionIndex].ValueAfterEvent.STD_LOGIC_Value[0] = TSL1 then
          OldSampleBin := 1;

      if ASignal^.SignalEvents[TransitionIndex + 1].ValueAfterEvent.STD_LOGIC_Value[0] = TSL0 then
        NewSampleBin := 0
      else
        if ASignal^.SignalEvents[TransitionIndex + 1].ValueAfterEvent.STD_LOGIC_Value[0] = TSL1 then
          NewSampleBin := 1;    
    end;


  //XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex].Moment - ScrBarPos) * Zoom);
  XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex + 1].Moment - ScrBarPos) * Zoom);
  XTranMinus1 := XTran - 1;
  //XTranMinus2 := XTran - 2;
  XTranPlus1 := XTran + 1;
  XTranPlus2 := XTran + 2;
  //PrevXTranPlus2 := PrevXTran + 2;


  if InIntervalInteger(XTran, CellRect.Left, CellRect.Right) then
  begin
    ACanvas.Pen.Color := clBlack;
    
    if OldSampleBin <> NewSampleBin then
    begin
      if (OldSampleBin in [0, 1]) and (NewSampleBin in [0, 1]) then
      begin
        ACanvas.Pen.Color := clBlack;
        Line(ACanvas, XTran, y0, XTran, y1); //vertical Line

        if ASignal^.SignalEvents[TransitionIndex].Highlighted then
        begin
          ACanvas.Pen.Color := clAqua;
          Line(ACanvas, XTranMinus1, y0, XTranMinus1, y1); //vertical Line
          Line(ACanvas, XTranPlus1, y0, XTranPlus1, y1); //vertical Line
        end;
      end;

    end
    else
    begin  //same value, just mark the transition
      Line(ACanvas, XTran, y0, XTran, y0 - 2); //vertical short Line
      Line(ACanvas, XTran, y1, XTran, y1 + 2); //vertical short Line

      {if ASignal^.SignalEvents[TransitionIndex].Highlighted then
      begin
        ACanvas.Pen.Color := clAqua;
        Line(ACanvas, XTranMinus1, y0, XTranMinus1, y1); //vertical Line
        Line(ACanvas, XTranPlus1, y0, XTranPlus1, y1); //vertical Line
      end; }
    end;

    if not (OldSampleBin in [0, 1]) then
    begin
      ACanvas.Pen.Color := clBlack;

      DrawMultiBitTransition(ACanvas, XTran, PrevXTran, y0, y1, ym);
    end;
  end;  //InIntervalInteger

  if TransitionIndex = 0 then
  begin
    if OldSampleBin = 0 then
      Line(ACanvas, 1, y0, XTran, y0)
    else
      Line(ACanvas, 1, y1, XTran, y1);
  end //i = 0
  else //i > 0
  begin
    if (TransitionIndex = 1) and (NewSampleBin = 1) then
    begin //debug code
      ACanvas.Pen.Color := GreenZeroColor; // $0088BBFF;
      ACanvas.Brush.Color := ACanvas.Pen.Color;
      ACanvas.Rectangle(CellRect.Left + 1, y1 + 1, PrevXTran, y0 {+ 1});
    end;

    if (OldSampleBin = 0) {and (NewSampleBin = 1)} then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y0, XTran, y0);
          
      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTran, y1, CellRect.Right - 1, y1);

      if DrawGreenZero then
        if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        begin
          ACanvas.Pen.Color := GreenZeroColor;
          ACanvas.Brush.Color := GreenZeroColor;
          ACanvas.Rectangle(XTranPlus1, y1 + 1, CellRect.Right, y0 {+ 1});
        end;
    end;

    if (OldSampleBin = 1) {and (NewSampleBin = 0)} then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, PrevXTran, y1, XTran, y1);

      if TransitionIndex = Length(ASignal^.SignalEvents) - 1 then
        Line(ACanvas, XTranPlus1, y0, CellRect.Right - 1, y0);

      if DrawGreenZero then
      begin
        ACanvas.Pen.Color := GreenZeroColor;
        ACanvas.Brush.Color := GreenZeroColor;
        ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 {+ 1});
      end;
    end;

    if (OldSampleBin = 2) and (NewSampleBin = 0) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, XTran, ym, XTranPlus2, y0);
    end;

    if (OldSampleBin = 2) and (NewSampleBin = 1) then
    begin
      ACanvas.Pen.Color := clBlack;
      LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);
      Line(ACanvas, XTran, ym, XTranPlus2, y1);
    end;
    
  end;//TransitionIndex > 0

  Result := XTran;
end;


function DrawClock(ACanvas: TCanvas; ASignal: PSignal; PrevXTran: Integer; CellRect: TRect; TransitionIndex, y0, y1, ScrBarPos: Integer; Zoom: Extended; DrawGreenZero: Boolean): Integer;
var
  XTran: Integer;
  XTranMinus1: Integer;
  XTranPlus1: Integer;
begin
  //XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex].Moment - ScrBarPos) * Zoom);
  XTran := CellRect.Left + 1 + Round((ASignal^.SignalEvents[TransitionIndex + 1].Moment - ScrBarPos) * Zoom);
  XTranMinus1 := XTran - 1;
  XTranPlus1 := XTran + 1;

  //StartOfClockMoment := ASignal^.SignalEvents[TransitionIndex].Moment;
  //EndOfClockMoment := ASignal^.SignalEvents[TransitionIndex + 1].Moment;

  if InIntervalInteger(XTran, CellRect.Left, CellRect.Right) then
  begin
    ACanvas.Pen.Color := clBlack;
    Line(ACanvas, XTran, y0, XTran, y1); //vertical Line
    if ASignal^.SignalEvents[TransitionIndex].Highlighted then
    begin
      ACanvas.Pen.Color := clAqua;
      Line(ACanvas, XTranMinus1, y0, XTranMinus1, y1); //vertical Line
      Line(ACanvas, XTranPlus1, y0, XTranPlus1, y1); //vertical Line
    end;
  end;

  {ACanvas.Pen.Color := clYellow; //clMaroon;
  ACanvas.Brush.Color := ACanvas.Pen.Color;
  ACanvas.Brush.Style := bsHorizontal;
  ACanvas.Rectangle(PrevXTran + 1, y1 + 1, XTran, y0 + 1);
  ACanvas.Brush.Style := bsSolid;}

  LimitTransitionsToVisibleArea(CellRect.Left, CellRect.Right, PrevXTran, XTran);

  DrawClockTransitions(ACanvas, ASignal, CellRect, TransitionIndex, y0, y1, ScrBarPos, Zoom);

  Result := XTran;
end;


procedure DrawTransitionsOnEditing(ACanvas: TCanvas; APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting: Boolean; ATop, ABottom, AWaveformDrawingWidth, AScrBarPos: Integer; var AMainMarkerPos: Integer; AZoom: Extended);
var
  MarkerLeft, MarkerRight: Integer;
  i: Integer;
  XClk, Moment: Integer;
begin
  MarkerLeft := 0;  //can be set to a offset, e.g. vstWaveformsEditor.Header.Columns.Items[2].Left;
  MarkerRight := MarkerLeft + AWaveformDrawingWidth;  //when using offset:  //MarkerRight := MarkerLeft + vstWaveformsEditor.Header.Columns.Items[2].Width;

  if AMainMarkerPos + MarkerLeft > MarkerRight then
    AMainMarkerPos := MarkerRight - MarkerLeft;

  if APaintTransitionsOnAllZoomLevels or ((AZoom > 4) and AAllowToDrawValidTransitions) then
  begin
    XClk := 0;
    if APaintTransitionsAsInserting then
      ACanvas.Pen.Color := $0033BBCC
    else
      ACanvas.Pen.Color := $00DBB7FF;//clFuchsia;// clLime;
      
    for i := MarkerLeft to MarkerRight do
    begin
      Moment := XVstWaveToMoment(i, AZoom, AScrBarPos);

      if XClk <> Moment then
        Line(ACanvas, i, ATop, i, ABottom);

      XClk := Moment;  //This should verify the signal, for which these Lines are displayed. Then, whare are the possible transitions with respect to the synchronizing clock.
    end;
  end;
end;


procedure DrawMainMarker(ACanvas: TCanvas; ATop, ABottom, AWaveformDrawingWidth: Integer; var AMainMarkerPos: Integer);
var
  MarkerLeft, MarkerRight, ComputedMarkerPos: Integer;
begin
  MarkerLeft := 0;  //can be set to a offset, e.g. vstWaveformsEditor.Header.Columns.Items[2].Left;
  MarkerRight := MarkerLeft + AWaveformDrawingWidth;  //when using offset:  //MarkerRight := MarkerLeft + vstWaveformsEditor.Header.Columns.Items[2].Width;

  if AMainMarkerPos + MarkerLeft > MarkerRight then
    AMainMarkerPos := MarkerRight - MarkerLeft;

  ACanvas.Pen.Color := clBlue;
  ComputedMarkerPos := AMainMarkerPos + MarkerLeft;
  Line(ACanvas, ComputedMarkerPos, ATop,ComputedMarkerPos, ABottom);
end;


procedure DrawSignalWaveForm(ACanvas: TCanvas; ASignalRec: PSignalRec; CellRect: TRect;
                             NodeHeight, AWaveformDrawingWidth: Integer;
                             IsSelected, APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting, ADrawGreenZero, ADrawXes: Boolean;
                             AScrBarPos: Integer; var AMainMarkerPos: Integer;
                             AZoom: Extended);
const
  CEditingSelectedColor: TColor = $008EE203;
  CNotEditingSelectedColor: TColor = clSkyBlue;
  CEditingNotSelectedColor: TColor = $0080FFC8;
  CBackgroundColor: TColor = clWhite;
  CHighlightedSectionBaseColor = $0078D0FF;
  CHighlightedSectionHorizLineColor = $006688FF;
  CZeroColorBaseColor: TColor = $0010DF40;    //MoneyGreen = C0.DC.C0
var
  ASignal: PSignal;
  i, j: Integer;
  //TempSampleInt: Integer;
  OldSampleInt, NewSampleInt: Int64;
  OldSampleOrdinal, NewSampleOrdinal: Word;
  y0, y1, ym, ya, yb, ADrawingCellHeight: Integer;  //y0 - y of logic 0,  y1 - y of logic 1,    ym = (y1 + y0) div 2
  XTran, PrevXTran: Integer;
  //XTranTemp, PrevXTranTemp: Integer;
  XTranMinus1, XTranMinus2: Integer;
  XTranPlus1, XTranPlus2: Integer;
  PrevXTranPlus2: Integer;

  s: string;
  MinSampleValueInt64, MaxSampleValueInt64, MaxMinDiff: Int64;

  OldPatternTypeAfterEvent, NewPatternTypeAfterEvent: TPatternType;
  StartOfHighlightedSection, EndOfHighlightedSection: Integer;

  HighlightedSectionColor: TColor;
  ZeroGreenColor, HighlightedSectionZeroGreenColor, DisplayedZeroGreenColor: TColor;
const
  TestOrdinalValue: array[0..2] of string = ('one', 'two', 'three');

begin
  if not Assigned(ASignalRec) then
    raise Exception.Create('Can''t draw a signal, because it is not assigned.');

  ASignal := ASignalRec^.Signal;

  if IsSelected then   //selected node
  begin
    if ASignal^.Editing then
      ACanvas.Brush.Color := CEditingSelectedColor
    else
      ACanvas.Brush.Color := CNotEditingSelectedColor;
  end
  else
  begin
    if ASignal^.Editing then
      ACanvas.Brush.Color := CEditingNotSelectedColor
    else
      ACanvas.Brush.Color := CBackgroundColor;
  end;

  HighlightedSectionColor := AverageTwoColors(ACanvas.Brush.Color, CHighlightedSectionBaseColor);

  ACanvas.Pen.Color := clBlue;
  ACanvas.Rectangle(CellRect.Left, CellRect.Top - 1, CellRect.Right, CellRect.Bottom + 1); //blue or white  (or green if selected)         //ACanvas.Rectangle(CellRect);

  ACanvas.Brush.Color := HighlightedSectionColor;

  if (ASignal^.HighlightedSectionAfterEvent >= 0) and (ASignal^.HighlightedSectionAfterEvent < Length(ASignal^.SignalEvents)) then
  begin
    StartOfHighlightedSection := CellRect.Left + 1 + Round((ASignal^.SignalEvents[ASignal^.HighlightedSectionAfterEvent].Moment - AScrBarPos) * AZoom);

    if ASignal^.HighlightedSectionAfterEvent < Length(ASignal^.SignalEvents) - 1 then
      EndOfHighlightedSection := CellRect.Left + 1 + Round((ASignal^.SignalEvents[ASignal^.HighlightedSectionAfterEvent + 1].Moment - AScrBarPos) * AZoom)
    else
      EndOfHighlightedSection := Max(CellRect.Left + 1 + Round((ASignal^.SignalEvents[ASignal^.HighlightedSectionAfterEvent].Moment - AScrBarPos) * AZoom), CellRect.Right);

    ACanvas.Pen.Color := HighlightedSectionColor;
    ACanvas.Rectangle(StartOfHighlightedSection, CellRect.Top + 2, EndOfHighlightedSection, CellRect.Bottom - 1);

    ACanvas.Pen.Color := CHighlightedSectionHorizLineColor;
    Line(ACanvas, StartOfHighlightedSection, CellRect.Top + 1, EndOfHighlightedSection + 1, CellRect.Top + 1);
    Line(ACanvas, StartOfHighlightedSection, CellRect.Bottom - 1, EndOfHighlightedSection + 1, CellRect.Bottom - 1);
  end;

  ZeroGreenColor := AverageTwoColors(CZeroColorBaseColor, ACanvas.Brush.Color);
  HighlightedSectionZeroGreenColor := AverageTwoColors(ZeroGreenColor, HighlightedSectionColor);
  
  DrawTransitionsOnEditing(ACanvas, APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting, CellRect.Top - 1, CellRect.Bottom + 1, AWaveformDrawingWidth, AScrBarPos, AMainMarkerPos, AZoom);

  XTran := 0;
  //PrevXTran := 0;
  y0 := CellRect.Bottom - 2;
  y1 := CellRect.Top + 2;
  ym := (y1 + y0) shr 1;

  ACanvas.Pen.Color := clBlack;
  ACanvas.Font.Color := clBlack;    //font color not pen color
  ACanvas.Brush.Style := bsClear;

  OldSampleInt := 0; //just an init value - not used
  NewSampleInt := 0;
  OldSampleOrdinal := 0;
  NewSampleOrdinal := 0;

  if ASignal^.Size = 1 then   //using NumericValue[0]
  begin
    for i := 0 to Length(ASignal^.SignalEvents) - 2 do
    begin
      PrevXTran := XTran;

      OldPatternTypeAfterEvent := ASignal^.SignalEvents[i].PatternTypeAfterEvent;
      NewPatternTypeAfterEvent := ASignal^.SignalEvents[i + 1].PatternTypeAfterEvent;

      if i = ASignal^.HighlightedSectionAfterEvent then
        DisplayedZeroGreenColor := HighlightedSectionZeroGreenColor
      else
        DisplayedZeroGreenColor := ZeroGreenColor;

      if (OldPatternTypeAfterEvent = ptDiscrete) and (NewPatternTypeAfterEvent = ptDiscrete) then
        XTran := DrawBinarySignalTransition(ACanvas, ASignal, {Prev}XTran, CellRect, i, y0, y1, ym, AScrBarPos, AZoom, ADrawGreenZero, DisplayedZeroGreenColor)
      else
        if (OldPatternTypeAfterEvent = ptDiscrete) and (NewPatternTypeAfterEvent = ptClock) then
          XTran := DrawSignalTransitionFromBinaryToClock(ACanvas, ASignal, {Prev}XTran, CellRect, i, y0, y1, AScrBarPos, AZoom, ADrawGreenZero, DisplayedZeroGreenColor)
        else
          if (OldPatternTypeAfterEvent = ptClock) and (NewPatternTypeAfterEvent = ptDiscrete) then
            XTran := DrawSignalTransitionFromClockToBinary(ACanvas, ASignal, {Prev}XTran, CellRect, i, y0, y1, AScrBarPos, AZoom, ADrawGreenZero, DisplayedZeroGreenColor)
          else
            if (OldPatternTypeAfterEvent = ptClock) and (NewPatternTypeAfterEvent = ptClock) then
              XTran := DrawClock(ACanvas, ASignal, XTran, CellRect, i, y0, y1, AScrBarPos, AZoom, ADrawGreenZero);


      if ASignal^.ValueType = sutSTD_LOGIC then
      begin
        ACanvas.Brush.Style := bsClear;     
        s := ArrayOfTSTD_LOGIC_ToString(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value);
        
        //if i = 0 then
        //  ACanvas.TextOut((CellRect.Left + 2 + XTranMinus2) shr 1 - ACanvas.TextWidth(s) shr 1, (CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s)
        //else
        //  if i = Length(ASignal^.SignalEvents) - 1 then    //last transition
        //    ACanvas.TextOut((Max(XTran, CellRect.Left) + 2 + CellRect.Right - 2) shr 1 - ACanvas.TextWidth(s) shr 1, (CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s)
        //  else
            if ACanvas.TextWidth(s) <= Min(XTran, CellRect.Right) - 2 - Max(PrevXTran, CellRect.Left) - 4 then
              ACanvas.TextOut((Max(PrevXTran, CellRect.Left) + 2 + Min(XTran, CellRect.Right) - 2) shr 1 - ACanvas.TextWidth(s) shr 1,(CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s);

        ACanvas.Brush.Style := bsSolid;         
      end; //if ASignal^.ValueType = sutSTD_LOGIC
       
    end; //for

//    XTranMinus1 := XTran - 1;
    XTranPlus1 := XTran + 1;

    if (Length(ASignal^.SignalEvents) > 0) and (Length(ASignal^.SignalEvents[Length(ASignal^.SignalEvents) - 1].ValueAfterEvent.NumericValue) > 0) then //Last sample
      if ASignal^.SignalEvents[Length(ASignal^.SignalEvents) - 1].EventType = 0 then //real transition
      begin
        if ASignal^.SignalEvents[Length(ASignal^.SignalEvents) - 1].ValueAfterEvent.NumericValue[0] = 0 then
        begin
          ACanvas.Pen.Color := clBlack;
          Line(ACanvas, XTranPlus1, y0, CellRect.Right - 1, y0);  //horiz
        end
        else
          if ASignal^.SignalEvents[Length(ASignal^.SignalEvents) - 1].ValueAfterEvent.NumericValue[0] = 1 then
          begin
            ACanvas.Pen.Color := clBlack;
            Line(ACanvas, XTranPlus1, y1, CellRect.Right - 1, y1);  //horiz
          end;
      end;
  end //ASignal^.Signal.Size = 1

  else   //ASignal^.Signal.Size > 1

  begin
    ACanvas.Pen.Color := clBlack;
    
    for i := 0 to Length(ASignal^.SignalEvents) - 2 do
    begin
      case ASignal^.ValueType of
        sutNumeric:
        begin
          OldSampleInt := 0;
          for j := 0 to Length(ASignal^.SignalEvents[i].ValueAfterEvent.NumericValue) - 1 do
            OldSampleInt := OldSampleInt + ASignal^.SignalEvents[i].ValueAfterEvent.NumericValue[j] * Integer(TwoPowered(j shl 3));

          NewSampleInt := 0;
          for j := 0 to Length(ASignal^.SignalEvents[i].ValueAfterEvent.NumericValue) - 1 do
            NewSampleInt := NewSampleInt + ASignal^.SignalEvents[i + 1].ValueAfterEvent.NumericValue[j] * Integer(TwoPowered(j shl 3));
        end;

        sutSTD_LOGIC:
        begin
          OldSampleInt := 0;
          {for j := 0 to Length(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value) - 1 do
          begin
            TempSampleInt := Integer(Ord(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value[j]) - Integer(T0) );
            OldSampleInt := OldSampleInt + Int64(TempSampleInt) * TwoPoweredInt64(j);
          end;}

          NewSampleInt := 0;
          {for j := 0 to Length(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value) - 1 do
          begin
            TempSampleInt := Integer(Ord(ASignal^.SignalEvents[i + 1].ValueAfterEvent.STD_LOGIC_Value[j]) - Integer(T0) );
            NewSampleInt := NewSampleInt + Int64(TempSampleInt) * TwoPoweredInt64(j);  
          end;}
        end;

        sutOrdinal:
        begin
          OldSampleInt := ASignal^.SignalEvents[i].ValueAfterEvent.OrdinalValue;
          NewSampleInt := ASignal^.SignalEvents[i + 1].ValueAfterEvent.OrdinalValue;
        end;

        sutInt32:
        begin
          OldSampleInt := ASignal^.SignalEvents[i].ValueAfterEvent.Int32Value;
          NewSampleInt := ASignal^.SignalEvents[i + 1].ValueAfterEvent.Int32Value;
        end;

        sutInt64:
        begin
          OldSampleInt := ASignal^.SignalEvents[i].ValueAfterEvent.Int64Value;
          NewSampleInt := ASignal^.SignalEvents[i + 1].ValueAfterEvent.Int64Value;
        end;
        
        else
        begin
          OldSampleOrdinal := ASignal^.SignalEvents[i].ValueAfterEvent.Int32Value;
          NewSampleOrdinal := ASignal^.SignalEvents[i + 1].ValueAfterEvent.Int32Value;
        end;
      end; //case ASignal^.ValueType

      PrevXTran := XTran;
      PrevXTranPlus2 := PrevXTran + 2;
      //XTran := CellRect.Left + 1 + Round((Integer(ASignal^.SignalEvents[i].Moment) - AScrBarPos) * AZoom);
      XTran := CellRect.Left + 1 + Round((Integer(ASignal^.SignalEvents[i + 1].Moment) - AScrBarPos) * AZoom);

      XTranMinus1 := XTran - 1;
      XTranMinus2 := XTran - 2;
      XTranPlus2 := XTran + 2;

      case ASignal^.DrawType of
        dtNumeric:
        begin
          if i = 0 then
          begin
            Line(ACanvas, 1, y1, XTranMinus2, y1);  //horiz
            Line(ACanvas, 1, y0, XTranMinus2, y0);  //horiz
            Line(ACanvas, XTranMinus2, y1, XTran, ym); //slope     ¯¯\
            Line(ACanvas, XTranMinus2, y0, XTran, ym); //slope     __/

            if ASignal^.ValueType = sutNumeric then
              s := IntToStr(OldSampleInt);
            if ASignal^.ValueType = sutOrdinal then
              s := TestOrdinalValue[OldSampleOrdinal];
            if ASignal^.ValueType = sutSTD_LOGIC then
              //s := CSTD_LOGIC_Str[TSTD_LOGIC(OldSampleInt)];  ///////////////////////////////???????????????????????????????????????????????????????????
              s := ArrayOfTSTD_LOGIC_ToString(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value); 

            if ACanvas.TextWidth(s) <= XTranMinus2 - CellRect.Left - 4 then
              ACanvas.TextOut((CellRect.Left + 2 + XTranMinus2) shr 1 - ACanvas.TextWidth(s) shr 1, (CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s);
          end //i = 0
          else //i > 0
          begin
            if i = Length(ASignal^.SignalEvents) - 1 then    //last transition
            begin
              Line(ACanvas, XTranPlus2, y1, CellRect.Right - 1, y1);  //horiz
              Line(ACanvas, XTranPlus2, y0, CellRect.Right - 1, y0);  //horiz
              Line(ACanvas, XTran, ym, XTranPlus2, y1); //slope              /¯¯
              Line(ACanvas, XTran, ym, XTranPlus2, y0); //slope              \__

              if ASignal^.ValueType = sutNumeric then
                s := IntToStr(NewSampleInt);
              if ASignal^.ValueType = sutOrdinal then
                s := TestOrdinalValue[NewSampleOrdinal];
              if ASignal^.ValueType = sutSTD_LOGIC then
                //s := CSTD_LOGIC_Str[TSTD_LOGIC(OldSampleInt)];  ///////////////////////////////???????????????????????????????????????????????????????????
                s := ArrayOfTSTD_LOGIC_ToString(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value);  ///this was [i + 1]

              if ACanvas.TextWidth(s) <= CellRect.Right - 2 - XTran - 4 then
                ACanvas.TextOut((Max(XTran, CellRect.Left) + 2 + CellRect.Right - 2) shr 1 - ACanvas.TextWidth(s) shr 1, (CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s);
            end;

            DrawMultiBitTransition(ACanvas, XTran, PrevXTran, y0, y1, ym);

            if ASignal^.ValueType = sutNumeric then
              s := IntToStr(OldSampleInt);
            if ASignal^.ValueType = sutOrdinal then
              s := TestOrdinalValue[OldSampleOrdinal];
            if ASignal^.ValueType = sutSTD_LOGIC then
              //s := CSTD_LOGIC_Str[TSTD_LOGIC(OldSampleInt)];  ///////////////////////////////???????????????????????????????????????????????????????????
              s := ArrayOfTSTD_LOGIC_ToString(ASignal^.SignalEvents[i].ValueAfterEvent.STD_LOGIC_Value);

            if ACanvas.TextWidth(s) <= Min(XTran, CellRect.Right) - 2 - Max(PrevXTran, CellRect.Left) - 4 then
              ACanvas.TextOut((Max(PrevXTran, CellRect.Left) + 2 + Min(XTran, CellRect.Right) - 2) shr 1 - ACanvas.TextWidth(s) shr 1,(CellRect.Top + CellRect.Bottom) shr 1 - ACanvas.TextHeight(s) shr 1, s);
          end;//i > 0
        end;  //dtNumeric
      
        dtAnalog:
        begin
          case ASignal^.ValueType of
            sutNumeric, sutSTD_LOGIC, sutOrdinal:
            begin
              MinSampleValueInt64 := 0;
              for j := 0 to Length(ASignal^.NumericMinValue) - 1 do
                MinSampleValueInt64 := MinSampleValueInt64 + ASignal^.NumericMinValue[j] * TwoPoweredInt64(j shl 3);

              MaxSampleValueInt64 := 0;
              for j := 0 to Length(ASignal^.NumericMinValue) - 1 do
                MaxSampleValueInt64 := MaxSampleValueInt64 + ASignal^.NumericMaxValue[j] * TwoPoweredInt64(j shl 3);
            end;

            sutInt32:
            begin
              MinSampleValueInt64 := ASignal^.Int32MinValue;
              MaxSampleValueInt64 := ASignal^.Int32MaxValue;
            end;

            sutInt64:
            begin
              MinSampleValueInt64 := ASignal^.Int64MinValue;
              MaxSampleValueInt64 := ASignal^.Int64MaxValue;
            end;

            else
            begin
              MinSampleValueInt64 := ASignal^.Int32MinValue;
              MaxSampleValueInt64 := ASignal^.Int32MaxValue;
            end;
          end; //case

          MaxMinDiff := MaxSampleValueInt64 - MinSampleValueInt64;
          if MaxMinDiff = 0 then
            MaxMinDiff := 1;

          ADrawingCellHeight := y0 - y1;
          ya := y0 - Integer(OldSampleInt * Int64(ADrawingCellHeight) div MaxMinDiff);
          yb := y0 - Integer(NewSampleInt * Int64(ADrawingCellHeight) div MaxMinDiff);

          if ya < y1 - 2 then
          begin
            ya := y1 - 2;
            ACanvas.Pen.Color := clRed;
          end;

          if yb < y1 - 2 then
          begin
            yb := y1 - 2;
            ACanvas.Pen.Color := clRed;
          end;

          ACanvas.Pen.Color := clBlack;
          Line(ACanvas, PrevXTran, ya, XTran, yb);

          if ADrawXes then
          begin
            ACanvas.Pen.Color := clRed;
            Line(ACanvas, XTranMinus2, yb - 2, XTranPlus2, yb + 2);
            Line(ACanvas, XTranMinus2, yb + 2, XTranPlus2, yb - 2);
          end;
        end; //ASignal.AnalogicDraw
      end; //case
    end; //for  ASignal.SignalEvents
  end; //ASignal.Signal.Size > 1

  DrawMainMarker(ACanvas, CellRect.Top - 1, CellRect.Bottom + 1, AWaveformDrawingWidth, AMainMarkerPos);
end;


end.
