{
    Copyright (C) 2023 VCC
    creation date: 04 Mar 2016
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


unit WaveformUtils;

interface

uses
  SysUtils, Classes, VirtualTrees, Graphics, ComCtrls, Controls;

type
  {from IEEE std_logic_1164.vhd
                        TYPE std_ulogic IS
                       ( 'U',  -- Uninitialized
                         'X',  -- Forcing  Unknown
                         '0',  -- Forcing  0
                         '1',  -- Forcing  1
                         'Z',  -- High Impedance   
                         'W',  -- Weak     Unknown
                         'L',  -- Weak     0       
                         'H',  -- Weak     1       
                         '-'   -- Don't care
                       );
}

  TSTD_LOGIC = (TSLU, TSLX, TSL0, TSL1, TSLZ, TSLW, TSLL, TSLH, TSLMinus); //expected less than 256 possible values, because STD_LOGIC_Value is used like an array of bytes
  //TSTD_LOGIC_VECTOR = array of TSTD_LOGIC;
  TSignalUsedType = (sutNumeric, sutSTD_LOGIC, sutOrdinal, sutInt32, sutInt64);
  TSignalAddedBy = (sabUser, sabComputer);  //sabComputer = internal signal allocated by this app (this is not editable but can be managed through settings or other signals)
  TJitterType = (jtPositiveOnly, jtSymmetrical);
  TDrawType = (dtNumeric, dtAnalog, dtDutyCycleColorDensity, dtAnalogColorDensity);
  TPatternType = (ptDiscrete, ptClock);

  //TSignalOrdinalValueNames = array of string; //e.g.: 'SInitState', 'SLoadState', 'SWriteState', 'SDoneState'

  TByte_Arr = array of Byte;
  TSTD_LOGIC_Arr = array of TSTD_LOGIC;

  TSignalValueType = record
    NumericValue: TByte_Arr;
    STD_LOGIC_Value: TSTD_LOGIC_Arr;
    OrdinalValue: Cardinal; //used on FSMs. e.g.: SInitState, SLoadState, SWriteState, SDoneState
    Int32Value: Integer;
    Int64Value: Int64;
  end;

  TClockPattern = record
    //FirstValue: TSTD_LOGIC;  //maybe first value should start with a '1' every time, but the start of this clock should be set externally (by owning structure)
    MidTransitionOffset: Integer; //transition from '1' to '0' with regard to the start of clock period (the "0 to 1" transition)
    EndTransitionOffset: Integer; //end of clock period (the next "0 to 1" transition) with regard to the start of clock period
  end;
  
  TSignalEvent = record
    PatternTypeAfterEvent: TPatternType; //ptDiscrete = general purpose signal with values described by ValueAfterEvent field, ptClock = pattern described by
    ValueAfterEvent: TSignalValueType; //used when PatternTypeAfterEvent is ptDiscrete
    ClockPatternAfterEvent: TClockPattern; //used when PatternTypeAfterEvent is ptClock
    Moment: Integer;
    Jitter: Extended;
    JitterType: TJitterType; //if jtPositiveOnly, then Moment defines first possible transition and Moment + Jitter defines last transition. If jtSymmetrical, then Moment -  Jitter/2 defines first possible transition and Moment + Jitter/2 defines last transition.
    EditingEvent: Boolean;  //True only when dragging a transition (with the mouse) on the Waveform Editor
    CauseOfEvent: Byte; //0 = start of signal description (first event), 1 = use "moment" value, 2 = condition match
    EventConditionIndex: Integer; //index of condition or circuit (in a list), which describes when to change the signal on this event (used only if CauseOfEvent = 2)
    EventType: Byte;   // 0 = real transition, 1 = transition from "visible" signal to unseen pattern or constant (for graphical use), 2 = transition from unseen pattern or constant to "visible" signal (for graphical use)
    EventInfo: string; //user notes about this event
    Highlighted: Boolean; //for drawing two lines "around" the transition

    //if adding complex types (like arrays, including pointer to arrays), make sure they are properly handled by the moving functions, like MoveLastSignalEventToProperIndex
  end;

  PSignal = ^TSignal;
  TSignal = record    //Describes a signal (in time) with all its events
    SignalName: string;
    SignalValueSource: Byte; //0 = value generated by user through waveforms editor, 1 = value generated by owner circuit (waveform can't be edited), 2 = generated by other circuits (signal is connected to external circuits)
    SignalDirection: Byte; //0 = out, 1 = in, 2 = inout, 3 = internal
    OwnerCircuit: string;
    ValueType: TSignalUsedType; //0 = NumericValue (computations are made on integral numbers), 1 = STD_LOGIC_Value (computations are made on STD_LOGIC numbers), 2 = OrdinalValue
    SignalEvents: array of TSignalEvent;   //transitions or graphical events (e.g. patterns)
    HighlightedSectionAfterEvent: Integer; //index of event from SignalEvents array, after which the signal is highlighted (until next event)
    SignalOrdinalValueTypeName: string; //name of type, which contains a list of value names, stored in a TSignalOrdinalValueNames variable
    Size: Word; //number of bits
    AddedBy: TSignalAddedBy;
    Editing: Boolean;
    SynchronizedBySignal: string; //If this signal is synchronized, then this variable stores the signal's name. Asynchronous signals have this variable set to ''
    SynchronizedByCircuit: string; //If this signal is synchronized, then this variable stores the circuit name which contains the synchronization signal. Asynchronous signals have this variable set to ''
    //AllowGlitch: Boolean; // used only for signals that are described by user and synchronized by a clock which is also described by user (e.g. signals that travel through long wires) 
    DrawType: TDrawType;
    NumericMinValue: array of Byte;
    NumericMaxValue: array of Byte;
    Int32MinValue: Integer;
    Int32MaxValue: Integer;
    Int64MinValue: Int64;
    Int64MaxValue: Int64;
    MinValueColor: TColor;
    MaxValueColor: TColor;
  end;

  TPSignalArr = array of PSignal;

  PSignalRec = ^TSignalRec;
  TSignalRec = record  //for VST
    Signal: PSignal;
    //other drawing info
  end;

  TNodeHeightsArr = array of Word;


  TOnPaintAllWaveforms = procedure of object;
  TWaveformDrawing = class(TCustomControl)
  private
    FEnabledPaint: Boolean;
    FOnPaintAllWaveforms: TOnPaintAllWaveforms;

    procedure DoOnPaintAllWaveforms;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;

    property EnabledPaint: Boolean read FEnabledPaint write FEnabledPaint;
    property OnPaintAllWaveforms: TOnPaintAllWaveforms write FOnPaintAllWaveforms;

    property Canvas;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


const
  CMinNodeHeight: Word = 15;
  CPatternTypeAfterEventStr: array[TPatternType] of string = ('Discrete', 'Clock');
  CValueTypeStr: array[TSignalUsedType] of string = ('Numeric', 'STD_LOGIC', 'Ordinal', 'Int32', 'Int64');
  CSTD_LOGIC_Str: array[TSTD_LOGIC] of string = ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-');


function GetVSTNodeByIndex(AVst: TVirtualStringTree; NodeIndex: Cardinal): PVirtualNode;  
function SelectVSTNodeByIndex(AVst: TVirtualStringTree; NodeIndex: Cardinal; FocusSelectedNode: Boolean): Boolean; //returns True if index is found

function NumericSignalToInteger(Value: TSignalValueType): Integer;
function ArrayOfTSTD_LOGIC_ToString(var Value: TSTD_LOGIC_Arr): string;
function TotalNodeHeights(vst: TVirtualStringTree): Cardinal;
function LimitTransitionsToVisibleArea(CellLeft, CellRight: Integer; var PrevTran, Tran: Integer): Boolean;  //returns true when the line can be drawn
function XVstWaveToMomentExt(X: Integer; Zoom: Extended; TimePos: Integer): Extended;
function XVstWaveToMoment(X: Integer; Zoom: Extended; TimePos: Integer): Integer;

procedure ClearAllEditingEventsInASignal(ASignal: PSignal);
procedure UpdateSignalTransitionMoment(ASignal: PSignal; EventNumber: Integer; NewMoment: Integer; DragSubsequentEvents: Boolean);
function GetProperEventIndexByMoment(ASignal: PSignal; SearchMoment: Integer): Integer;
function GetEventIndexByXVst(ASignal: PSignal; X: Integer; Zoom: Extended; TimePos: Integer): Integer;
procedure AdjustEventMomentsToAvoidOverlapping(ASignal: PSignal);
function MoveLastSignalEventToProperIndex(ASignal: PSignal): Integer; //returns proper index
procedure ChangeSignalIntegerValueBeforeTransition(ASignal: PSignal; MomentOnValue: Extended; NewValue: Integer);
procedure StrToSTD_LOGIC_Arr(Value: string; var OutArr: TSTD_LOGIC_Arr);
procedure ClearArrayOfSignals(var Signals: TPSignalArr);
function AverageTwoColors(Color1, Color2: TColor): TColor;


implementation

uses
  WaveformFuncUtils;


constructor TWaveformDrawing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EnabledPaint := False;
  FOnPaintAllWaveforms := nil;
  Caption := '';
end;


procedure TWaveformDrawing.Paint;
begin
  if FEnabledPaint then
    DoOnPaintAllWaveforms;
end;


procedure TWaveformDrawing.DoOnPaintAllWaveforms;
begin
  if not Assigned(FOnPaintAllWaveforms) then
    raise Exception.Create('OnPaintAllWaveforms not assigned.');

  FOnPaintAllWaveforms();  
end;


function GetVSTNodeByIndex(AVst: TVirtualStringTree; NodeIndex: Cardinal): PVirtualNode;
var
  Node: PVirtualNode;
begin
  Result := nil;
  Node := AVst.GetFirst;
  if not Assigned(Node) then
    Exit;

  repeat
    if Node.Index = NodeIndex then
    begin
      Result := Node;
      Exit;
    end;

    Node := Node^.NextSibling;
  until Node = nil;
end;  


function SelectVSTNodeByIndex(AVst: TVirtualStringTree; NodeIndex: Cardinal; FocusSelectedNode: Boolean): Boolean;  //returns True if index is found
var
  Node: PVirtualNode;
begin
  Result := False;
  Node := GetVSTNodeByIndex(AVst, NodeIndex);
  
  if Assigned(Node) then
  begin
    AVst.Selected[Node] := True;
    
    if FocusSelectedNode then
      AVst.ScrollIntoView(Node, False);

    Result := True;  
  end;
end;  


function NumericSignalToInteger(Value: TSignalValueType): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(Value.NumericValue) - 1 do
    Result := Result + Integer(Value.NumericValue[i]) * Integer(TwoPowered(i shl 3));
end;


function ArrayOfTSTD_LOGIC_ToString(var Value: TSTD_LOGIC_Arr): string;
var
  i: Integer;
begin
  if Length(Value) = 0 then
  begin
    Result := '';
    Exit;
  end;

  if Length(Value) = 1 then
  begin
    Result := CSTD_LOGIC_Str[Value[0]];
    Exit;
  end;

  Result := '';
  for i := 0 to Length(Value) - 1 do
    Result := CSTD_LOGIC_Str[Value[i]] + Result;
end;


procedure StrToSTD_LOGIC_Arr(Value: string; var OutArr: TSTD_LOGIC_Arr);
var
  i, j, n: Integer;
begin
  n := Length(Value);
  SetLength(OutArr, n);
  for i := 1 to Length(Value) do
  begin
    for j := 0 to Length(CSTD_LOGIC_Str) - 1 do
      if Value[i] = CSTD_LOGIC_Str[TSTD_LOGIC(j)] then
      begin
        OutArr[n - i] := TSTD_LOGIC(j);
        Break;
      end;
  end;
end;


function TotalNodeHeights(vst: TVirtualStringTree): Cardinal;
var
  Node: PVirtualNode;
begin
  Result := 0;
  Node := vst.GetFirst;
  if not Assigned(Node) then
    Exit;

  repeat
    Result := Result + Node.NodeHeight; 

    //Node := Node.NextSibling;
    Node := vst.GetNext(Node);
  until Node = nil;  
end;


function LimitTransitionsToVisibleArea(CellLeft, CellRight: Integer; var PrevTran, Tran: Integer): Boolean;  //returns true when the line can be drawn
begin
  Result := False; //do not draw
  {if (PrevTran >= CellLeft) and (Tran <= CellRight) then  //inside
    Exit;   //nothing to modify }

  if (PrevTran < CellLeft) and (Tran < CellLeft) then //outside
    Exit;

  if (PrevTran > CellRight) and (Tran > CellRight) then //outside
    Exit;

  if (PrevTran < CellLeft) and (Tran > CellRight) then   //outside
  begin
    PrevTran := CellLeft;
    Tran := CellRight;
    Result := True;
    Exit;
  end;

  if (PrevTran < CellLeft) and (Tran <= CellRight) then   //right side of the line is inside the visible area
  begin
    PrevTran := CellLeft;
    Result := True;
    Exit;
  end;

  if (PrevTran >= CellLeft) and (Tran > CellRight) then   //right side of the line is inside the visible area
  begin
    Tran := CellRight;
    Result := True;
    Exit;
  end;
end;


function XVstWaveToMomentExt(X: Integer; Zoom: Extended; TimePos: Integer): Extended;
var
  WaveOriginInt: Integer;
begin
  WaveOriginInt := X - 1;

  if WaveOriginInt < 0 then
    WaveOriginInt := 0;
  Result := WaveOriginInt / Zoom + TimePos;
end;


function XVstWaveToMoment(X: Integer; Zoom: Extended; TimePos: Integer): Integer;
begin
  Result := Trunc(XVstWaveToMomentExt(X, Zoom, TimePos));
end;
            

procedure ClearAllEditingEventsInASignal(ASignal: PSignal);
var
  i: Integer;
begin
  if not Assigned(ASignal) then
    Exit;
    
  for i := 0 to Length(ASignal.SignalEvents) - 1 do
    ASignal.SignalEvents[i].EditingEvent := False;
end;


procedure UpdateSignalTransitionMoment(ASignal: PSignal; EventNumber: Integer; NewMoment: Integer; DragSubsequentEvents: Boolean);
var
  FirstEvent: Boolean;
  LastEvent: Boolean;
  PrevTran, NextTran: Integer;
  MoveDiff: Integer; //on moment scale

  procedure MoveTran;
  var
    i: Integer;
  begin
    MoveDiff := NewMoment - ASignal.SignalEvents[EventNumber].Moment;   //can be negative
    ASignal.SignalEvents[EventNumber].Moment := NewMoment;

    if DragSubsequentEvents then
      for i := EventNumber + 1 to Length(ASignal^.SignalEvents) - 1 do
        ASignal.SignalEvents[i].Moment := ASignal.SignalEvents[i].Moment + MoveDiff;
  end;
begin
  FirstEvent := EventNumber = 0;
  LastEvent := EventNumber = Length(ASignal.SignalEvents) - 1;

  if FirstEvent and LastEvent then                 //single event
  begin
    ASignal.SignalEvents[EventNumber].Moment := NewMoment;
    Exit;
  end;

  if (not FirstEvent) and (not LastEvent) then     //middle
  begin
    PrevTran := ASignal.SignalEvents[EventNumber - 1].Moment;
    NextTran := ASignal.SignalEvents[EventNumber + 1].Moment;
    if (NewMoment > PrevTran) and (NewMoment < NextTran) then
      MoveTran;
      
    Exit;
  end;

  if FirstEvent and (not LastEvent) then        //first transition
  begin
    NextTran := ASignal.SignalEvents[EventNumber + 1].Moment;
    if NewMoment < NextTran then
      MoveTran;

    Exit;
  end;

  if (not FirstEvent) and LastEvent then        //last transition
  begin
    PrevTran := ASignal.SignalEvents[EventNumber - 1].Moment;
    if (NewMoment >= PrevTran) and (NewMoment < MaxInt - 1) then
      MoveTran;
      
    Exit;
  end;
end;


function GetProperEventIndexByMoment(ASignal: PSignal; SearchMoment: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;

  if SearchMoment < 0 then
    Exit;

  if Length(ASignal.SignalEvents) = 0 then
    Exit;

  if SearchMoment < ASignal.SignalEvents[0].Moment then
  begin
    Result := 0;
    Exit;
  end;

  for i := 0 to Length(ASignal.SignalEvents) - 2 do
    if (SearchMoment >= ASignal.SignalEvents[i].Moment) and (SearchMoment < ASignal.SignalEvents[i + 1].Moment) then
    begin
      Result := i;
      Exit;
    end;

  if SearchMoment > ASignal.SignalEvents[Length(ASignal.SignalEvents) - 1].Moment then
    Result := Length(ASignal.SignalEvents) - 1; //calling function should increase array length to add this event as the last event
end;


function GetEventIndexByXVst(ASignal: PSignal; X: Integer; Zoom: Extended; TimePos: Integer): Integer;
var
  AMoment: Integer;
begin
  AMoment := XVstWaveToMoment(X, Zoom, TimePos);
  Result := GetProperEventIndexByMoment(ASignal, AMoment);
end;


procedure AdjustEventMomentsToAvoidOverlapping(ASignal: PSignal);
var
  i: Integer;
begin
  for i := 0 to Length(ASignal.SignalEvents) - 2 do
    if ASignal.SignalEvents[i].Moment >= ASignal.SignalEvents[i + 1].Moment then
      Inc(ASignal.SignalEvents[i + 1].Moment, ASignal.SignalEvents[i].Moment - ASignal.SignalEvents[i + 1].Moment + 2);
end;


function MoveLastSignalEventToProperIndex(ASignal: PSignal): Integer; //returns proper index
var
  i: Integer;
  FoundProperEventIndex: Integer;
  PhEvent: TSignalEvent;
begin
  Result := -1;
  if Length(ASignal.SignalEvents) <= 1 then
    Exit;

  FoundProperEventIndex := GetProperEventIndexByMoment(ASignal, ASignal.SignalEvents[Length(ASignal.SignalEvents) - 1].Moment);
  if (FoundProperEventIndex = Length(ASignal.SignalEvents) - 1) or (FoundProperEventIndex = -1) then
    Exit;

  PhEvent := ASignal.SignalEvents[Length(ASignal.SignalEvents) - 1];
    
  for i := Length(ASignal.SignalEvents) - 1 downto FoundProperEventIndex + 1 do
    ASignal.SignalEvents[i] := ASignal.SignalEvents[i - 1];

  ASignal.SignalEvents[FoundProperEventIndex + 1] := PhEvent;
  AdjustEventMomentsToAvoidOverlapping(ASignal);
  Result := FoundProperEventIndex;
end;


procedure ChangeSignalIntegerValueBeforeTransition(ASignal: PSignal; MomentOnValue: Extended; NewValue: Integer);
var
  i, n: Integer;
begin
  if MomentOnValue < 0 then
    Exit;

  n := Length(ASignal.SignalEvents);

  if n = 0 then
    Exit;

  if MomentOnValue < ASignal.SignalEvents[0].Moment then
  begin
    SetLength(ASignal.SignalEvents[0].ValueAfterEvent.NumericValue, 4);
    ASignal.SignalEvents[0].ValueAfterEvent.NumericValue[0] := Lo(Word(NewValue));
    ASignal.SignalEvents[0].ValueAfterEvent.NumericValue[1] := Hi(Word(NewValue));
    ASignal.SignalEvents[0].ValueAfterEvent.NumericValue[2] := Higher(NewValue);
    ASignal.SignalEvents[0].ValueAfterEvent.NumericValue[3] := Highest(NewValue);
  end
  else
    if MomentOnValue > ASignal.SignalEvents[n - 1].Moment then
    begin
      SetLength(ASignal.SignalEvents[n - 1].ValueAfterEvent.NumericValue, 4);
      ASignal.SignalEvents[n - 1].ValueAfterEvent.NumericValue[0] := Lo(Word(NewValue));
      ASignal.SignalEvents[n - 1].ValueAfterEvent.NumericValue[1] := Hi(Word(NewValue));
      ASignal.SignalEvents[n - 1].ValueAfterEvent.NumericValue[2] := Higher(NewValue);
      ASignal.SignalEvents[n - 1].ValueAfterEvent.NumericValue[3] := Highest(NewValue);
    end  
    else
      for i := 1 to n - 1 do
      begin
        if InIntervalExtended(MomentOnValue, ASignal.SignalEvents[i - 1].Moment, ASignal.SignalEvents[i].Moment) then
        begin
          SetLength(ASignal.SignalEvents[i].ValueAfterEvent.NumericValue, 4);
          ASignal.SignalEvents[i].ValueAfterEvent.NumericValue[0] := Lo(Word(NewValue));
          ASignal.SignalEvents[i].ValueAfterEvent.NumericValue[1] := Hi(Word(NewValue));
          ASignal.SignalEvents[i].ValueAfterEvent.NumericValue[2] := Higher(NewValue);
          ASignal.SignalEvents[i].ValueAfterEvent.NumericValue[3] := Highest(NewValue);
        end; //if
      end;//for
end;


procedure ClearArrayOfSignals(var Signals: TPSignalArr);
var
  i, j: Integer;
begin
  for i := 0 to Length(Signals) - 1 do
  begin
    if Assigned(Signals[i]) then
    begin
      for j := 0 to Length(Signals[i].SignalEvents) - 1 do
      begin
        SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.NumericValue, 0);
        SetLength(Signals[i].SignalEvents[j].ValueAfterEvent.STD_LOGIC_Value, 0);
      end; //for j

      SetLength(Signals[i].SignalEvents, 0);
      SetLength(Signals[i].NumericMinValue, 0);
      SetLength(Signals[i].NumericMaxValue, 0);

      Dispose(Signals[i]);
    end;
  end;
  SetLength(Signals, 0);
end;


function AverageTwoColors(Color1, Color2: TColor): TColor;
var
  R, G, B: Integer;
begin
  R := (Color1 and $FF + Color2 and $FF) shr 1;
  G := (((Color1 shr 8) and $FF) + (Color2 shr 8) and $FF) shr 1;
  B := (((Color1 shr 16) and $FF) + (Color2 shr 16) and $FF) shr 1;
  Result := B shl 16 + G shl 8 + R;
end;


end.
