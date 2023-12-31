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


unit SignalTransitionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, WaveformUtils, ImgList, ExtCtrls,
  WaveformsEditorFrame;

type
  TOnGetBooleanOption = function: Boolean of object;
  TOnGetIntegerValue = function: Integer of object;
  TOnSetIntegerValue = procedure(ANewValue: Integer) of object;

  TfrmSignalTransitions = class(TForm)
    vstTransitions: TVirtualStringTree;
    lblSignalName: TLabel;
    imglstPattern: TImageList;
    lbeMoment: TLabeledEdit;
    rdgrpSignalType: TRadioGroup;
    lbeSignalValueAfterTransitionNum: TLabeledEdit;
    lbeSignalValueAfterTransitionSTD_LOGIC: TLabeledEdit;
    lbeClockAfterTransitionMid: TLabeledEdit;
    lbeClockAfterTransitionPeriod: TLabeledEdit;
    btnAddTransition: TButton;
    btnUpdateTransition: TButton;
    btnDeleteTransition: TButton;
    rdgrpDiscreteValueType: TRadioGroup;
    tmrUpdateControlsFromClick: TTimer;
    imgDiscrete: TImage;
    imgClock: TImage;
    lblSelectedMoment: TLabel;
    tmrRepaintWaveform: TTimer;
    btnClearSelection: TButton;
    tmrRepaintVst: TTimer;
    Label1: TLabel;
    btnZoomIn: TButton;
    btnZoomOut: TButton;
    rdgrpSigValAfterTranAtSize1: TRadioGroup;
    lblSignalSize: TLabel;
    imglstClockTypeTransition: TImageList;
    imglstDiscreteTypeTransition: TImageList;
    tmrSelectTransitionByMoment: TTimer;
    pnlWaveforms: TPanel;
    procedure vstTransitionsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure vstTransitionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstTransitionsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrUpdateControlsFromClickTimer(Sender: TObject);
    procedure btnAddTransitionClick(Sender: TObject);
    procedure rdgrpSignalTypeClick(Sender: TObject);
    procedure rdgrpDiscreteValueTypeClick(Sender: TObject);
    procedure WaveformDrawingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WaveformDrawingMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WaveformDrawingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tmrRepaintWaveformTimer(Sender: TObject);
    procedure btnClearSelectionClick(Sender: TObject);
    procedure tmrRepaintVstTimer(Sender: TObject);
    procedure btnUpdateTransitionClick(Sender: TObject);
    procedure vstTransitionsGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure btnDeleteTransitionClick(Sender: TObject);
    procedure tmrSelectTransitionByMomentTimer(Sender: TObject);
    procedure btnZoomInClick(Sender: TObject);
    procedure btnZoomOutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FSignalRec: PSignalRec;
    FSignal: PSignal;
    FFrameWaveformsEditor: TFrameWaveformsEditor;
    FDrawingRect: TRect;

    FOnZoomIn: TNotifyEvent;
    FOnZoomOut: TNotifyEvent;

    FAllowToDrawValidTransitions: Boolean;
    FPaintTransitionsOnAllZoomLevels: Boolean; //if false, the transition Lines are painted only above zoom level 4
    FPaintTransitionsAsInserting: Boolean; //if true, transitions are painted in yellow (only when FPaintTransitionsOnAllZoomLevels is used)

    FDrawGreenZero: Boolean;
    FDrawXes: Boolean;

    FWaveformDrawing: TWaveformDrawing;

    FOnGetScrollBarPos: TOnGetIntegerValue;
    FOnGetMainMarkerPos: TOnGetIntegerValue;
    FOnSetMainMarkerPos: TOnSetIntegerValue;

    function DoOnGetScrollBarPos: Integer;
    function DoOnGetMainMarkerPos: Integer;
    procedure DoOnSetMainMarkerPos(ANewValue: Integer);

    procedure UpdateEventByIndexFromControls(EventIndex: Integer);
    procedure DrawLocalSignalWaveForm;
    procedure EnableEditBoxesBasedOnRadioGroups;
    procedure UpdateControlsFromSelectedTransition;
    procedure SetHighlightedSectionAfterEvent;

    procedure HandleOnPaintAllWaveforms;
  public
    { Public declarations }
    property OnZoomIn: TNotifyEvent read FOnZoomIn write FOnZoomIn;
    property OnZoomOut: TNotifyEvent read FOnZoomOut write FOnZoomOut;

    property OnGetScrollBarPos: TOnGetIntegerValue write FOnGetScrollBarPos;
    property OnGetMainMarkerPos: TOnGetIntegerValue write FOnGetMainMarkerPos;
    property OnSetMainMarkerPos: TOnSetIntegerValue write FOnSetMainMarkerPos;
  end;

var
  frmSignalTransitions: TfrmSignalTransitions;


procedure ShowSignalTransitions(ASignalRec: PSignalRec; AFrameWaveformsEditor: TFrameWaveformsEditor;
                                APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting, ADrawGreenZero, ADrawXes: Boolean;
                                AOnGetScrollBarPos, AOnGetMainMarkerPos: TOnGetIntegerValue; AOnSetMainMarkerPos: TOnSetIntegerValue; AOnZoomIn, AOnZoomOut: TNotifyEvent);

function SignalTransitionsVisibleForm: Boolean;


{ ToDo:
- Replace FFrameWaveformsEditor with events, then let their handlers call other handlers. Better, call some common code, than handlers.
- More refactoring and decoupling
}


implementation


uses
  WaveformDrawing;


{$R *.dfm}


procedure ShowSignalTransitions(ASignalRec: PSignalRec; AFrameWaveformsEditor: TFrameWaveformsEditor;
                                APaintTransitionsOnAllZoomLevels, AAllowToDrawValidTransitions, APaintTransitionsAsInserting, ADrawGreenZero, ADrawXes: Boolean;
                                AOnGetScrollBarPos, AOnGetMainMarkerPos: TOnGetIntegerValue; AOnSetMainMarkerPos: TOnSetIntegerValue; AOnZoomIn, AOnZoomOut: TNotifyEvent);
begin
  frmSignalTransitions.FSignalRec := ASignalRec;
  frmSignalTransitions.FSignal := ASignalRec.Signal;
  frmSignalTransitions.vstTransitions.RootNodeCount := Length(ASignalRec.Signal^.SignalEvents);
  frmSignalTransitions.lblSignalName.Caption := ASignalRec.Signal^.SignalName;
  frmSignalTransitions.FFrameWaveformsEditor := AFrameWaveformsEditor;

  frmSignalTransitions.FPaintTransitionsOnAllZoomLevels := APaintTransitionsOnAllZoomLevels;
  frmSignalTransitions.FAllowToDrawValidTransitions := AAllowToDrawValidTransitions;
  frmSignalTransitions.FPaintTransitionsAsInserting := APaintTransitionsAsInserting;
  frmSignalTransitions.FDrawGreenZero := ADrawGreenZero;
  frmSignalTransitions.FDrawXes := ADrawXes;
  frmSignalTransitions.FOnGetScrollBarPos := AOnGetScrollBarPos;
  frmSignalTransitions.FOnGetMainMarkerPos := AOnGetMainMarkerPos;
  frmSignalTransitions.FOnSetMainMarkerPos := AOnSetMainMarkerPos;
  frmSignalTransitions.FOnZoomIn := AOnZoomIn;
  frmSignalTransitions.FOnZoomOut := AOnZoomOut;

  frmSignalTransitions.lblSignalSize.Caption := 'Signal Size: ' + IntToStr(ASignalRec^.Signal.Size) + '-bit';
  frmSignalTransitions.Visible := True;

  Application.ProcessMessages;
  frmSignalTransitions.DrawLocalSignalWaveForm;
  frmSignalTransitions.EnableEditBoxesBasedOnRadioGroups; // a transition has to be selected to properly handle all cases
end;


function SignalTransitionsVisibleForm: Boolean;
begin
  Result := frmSignalTransitions.Visible;
end;


function TfrmSignalTransitions.DoOnGetScrollBarPos: Integer;
begin
  if not Assigned(FOnGetScrollBarPos) then
    raise Exception.Create('OnGetScrollBarPos not assigned');

  Result := FOnGetScrollBarPos;  
end;


function TfrmSignalTransitions.DoOnGetMainMarkerPos: Integer;
begin
  if not Assigned(FOnGetMainMarkerPos) then
    raise Exception.Create('OnGetMainMarkerPos not assigned');

  Result := FOnGetMainMarkerPos;
end;


procedure TfrmSignalTransitions.DoOnSetMainMarkerPos(ANewValue: Integer);
begin
  if not Assigned(FOnSetMainMarkerPos) then
    raise Exception.Create('OnSetMainMarkerPos not assigned');

  FOnSetMainMarkerPos(ANewValue);
end;


procedure TfrmSignalTransitions.btnClearSelectionClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  FSignal.HighlightedSectionAfterEvent := -1;
  Node := vstTransitions.GetFirstSelected;
  if Node = nil then
    Exit;

  vstTransitions.Selected[Node] := False;
  vstTransitions.Repaint;
  DrawLocalSignalWaveForm;
  FFrameWaveformsEditor.PaintAllWaveforms;
end;


procedure TfrmSignalTransitions.DrawLocalSignalWaveForm;
const
  CDrawingMargin = 5; //px
var
  Bmp: TBitmap;
  TempMainMarkerPos: Integer;
begin
  FDrawingRect := FWaveformDrawing.ClientRect;
  FDrawingRect.Top := CDrawingMargin;
  FDrawingRect.Bottom := FWaveformDrawing.Height - CDrawingMargin;

  Bmp := TBitmap.Create;
  try
    Bmp.Width := FWaveformDrawing.Width;
    Bmp.Height := FWaveformDrawing.Height;

    TempMainMarkerPos := DoOnGetMainMarkerPos;
    DrawSignalWaveForm(Bmp.Canvas, FSignalRec, FDrawingRect,
                           FWaveformDrawing.Height, FWaveformDrawing.Width,
                           False, FPaintTransitionsOnAllZoomLevels, FAllowToDrawValidTransitions, FPaintTransitionsAsInserting, FDrawGreenZero, FDrawXes,
                           DoOnGetScrollBarPos, TempMainMarkerPos, FFrameWaveformsEditor.Zoom);
    DoOnSetMainMarkerPos(TempMainMarkerPos);

    FWaveformDrawing.Canvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;
  end;
end;


procedure TfrmSignalTransitions.vstTransitionsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var
  HalfClockPeriod: Integer;
begin
  try
    case Column of
      2:
      begin
        ImageList := imglstPattern;
        ImageIndex := Integer(FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent);
      end;
      
      3:
      begin
        if FSignal.Size = 1 then
        begin
          case FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent of
            ptDiscrete:
            begin
              ImageList := imglstDiscreteTypeTransition;

              case NumericSignalToInteger(FSignal.SignalEvents[Node^.Index].ValueAfterEvent) of
                0: ImageIndex := 0;
                1: ImageIndex := 1;
                else
                  ImageIndex := 2;
              end; //case
            end;

            ptClock:
            begin
              ImageList := imglstClockTypeTransition;
              HalfClockPeriod := FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.EndTransitionOffset shr 1;
              if FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.MidTransitionOffset < HalfClockPeriod then
                ImageIndex := 0
              else
                if FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.MidTransitionOffset > HalfClockPeriod then
                  ImageIndex := 2
                else
                  ImageIndex := 1; // 50/50
            end;
          end;
        end
        else
        begin
          ImageList := imglstDiscreteTypeTransition;
          case FSignal.DrawType of
            dtNumeric: ImageIndex := 2;
            dtAnalog: ImageIndex := 3;
          end; //case DrawType
        end; //size
      end;
    end;
  except

  end;
end;


procedure TfrmSignalTransitions.vstTransitionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  try
    case Column of
      0: CellText := IntToStr(Node^.Index);
      1: CellText := IntToStr(FSignal.SignalEvents[Node^.Index].Moment);
      2: CellText := CPatternTypeAfterEventStr[FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent];
      3:
      begin
        case FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent of
          ptDiscrete:
            case FSignal.ValueType of
              sutNumeric: CellText := IntToStr(NumericSignalToInteger(FSignal.SignalEvents[Node^.Index].ValueAfterEvent));
              sutSTD_LOGIC: CellText := ArrayOfTSTD_LOGIC_ToString(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.STD_LOGIC_Value);
              sutOrdinal: CellText := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.OrdinalValue);
              sutInt32: CellText := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.Int32Value);
              sutInt64: CellText := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.Int64Value);
            end;

          ptClock:
            CellText := 'mid/period: ' +
              IntToStr(FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.MidTransitionOffset) +
              '/' +
              IntToStr(FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.EndTransitionOffset);
        end;
      end;
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrmSignalTransitions.vstTransitionsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  tmrUpdateControlsFromClick.Enabled := True;
end;


procedure TfrmSignalTransitions.vstTransitionsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //UpdateControlsFromSelectedTransition;
end;


procedure TfrmSignalTransitions.UpdateEventByIndexFromControls(EventIndex: Integer);
var
  Num: Integer;
begin
  FSignal.SignalEvents[EventIndex].Moment := StrToIntDef(lbeMoment.Text, 1);

  FSignal.SignalEvents[EventIndex].PatternTypeAfterEvent := TPatternType(rdgrpSignalType.ItemIndex);
  if FSignal.Size = 1 then
  begin
    SetLength(FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue, 1);
    case TPatternType(rdgrpSignalType.ItemIndex) of
      ptClock:
      begin
        FSignal.SignalEvents[EventIndex].ClockPatternAfterEvent.MidTransitionOffset := StrToIntDef(lbeClockAfterTransitionMid.Text, 1);
        FSignal.SignalEvents[EventIndex].ClockPatternAfterEvent.EndTransitionOffset := StrToIntDef(lbeClockAfterTransitionPeriod.Text, 2);
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[0] := 0;
      end;

      ptDiscrete:
      begin
        if rdgrpSigValAfterTranAtSize1.ItemIndex = -1 then
          rdgrpSigValAfterTranAtSize1.ItemIndex := 0;
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[0] := rdgrpSigValAfterTranAtSize1.ItemIndex;
      end;
    end;
  end
  else
  begin
    case TSignalUsedType(rdgrpDiscreteValueType.ItemIndex) of
      sutNumeric:
      begin
        SetLength(FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue, 4);
        Num := StrToIntDef(lbeSignalValueAfterTransitionNum.Text, 0);
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[0] := Num shr 0 and $FF;
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[1] := Num shr 8 and $FF;
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[2] := Num shr 16 and $FF;
        FSignal.SignalEvents[EventIndex].ValueAfterEvent.NumericValue[3] := Num shr 24 and $FF;
      end;

      sutSTD_LOGIC:
        StrToSTD_LOGIC_Arr(lbeSignalValueAfterTransitionSTD_LOGIC.Text, FSignal.SignalEvents[EventIndex].ValueAfterEvent.STD_LOGIC_Value);

      sutOrdinal: FSignal.SignalEvents[EventIndex].ValueAfterEvent.OrdinalValue := StrToIntDef(lbeSignalValueAfterTransitionNum.Text, 0);
      sutInt32: FSignal.SignalEvents[EventIndex].ValueAfterEvent.OrdinalValue := StrToIntDef(lbeSignalValueAfterTransitionNum.Text, 0);
      sutInt64: FSignal.SignalEvents[EventIndex].ValueAfterEvent.OrdinalValue := StrToInt64Def(lbeSignalValueAfterTransitionNum.Text, 0);
    end;
  end;
end;


procedure TfrmSignalTransitions.btnAddTransitionClick(Sender: TObject);
/////////////////////////////////////////// ToDo
//- add data validation
//[done] - Length(FSignal.SignalEvents) - 1
//[done]- fix greater than 0 case
//- test StrToSTD_LOGIC_Arr
var
  Node: PVirtualNode;
  n, NewIndex: Integer;
begin
  n := Length(FSignal.SignalEvents);
  SetLength(FSignal.SignalEvents, n + 1);

  UpdateEventByIndexFromControls(n);
  if n > 0 then
    NewIndex := MoveLastSignalEventToProperIndex(FSignal)
  else
    NewIndex := 0;

  vstTransitions.RootNodeCount := Length(FSignal.SignalEvents);

  if NewIndex > -1 then
    Node := GetVSTNodeByIndex(vstTransitions, NewIndex)
  else
    Node := vstTransitions.GetLast;

  if Assigned(Node) then
  begin
    vstTransitions.Selected[Node] := True;
    vstTransitions.ScrollIntoView(Node, True);
    vstTransitions.Repaint;
  end;
  
  FFrameWaveformsEditor.PaintAllWaveforms;
  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.btnUpdateTransitionClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstTransitions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'No transition selected. Please select one.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  UpdateEventByIndexFromControls(Node^.Index);
  AdjustEventMomentsToAvoidOverlapping(FSignal);

  vstTransitions.ScrollIntoView(Node, True);
  vstTransitions.Repaint;
  FFrameWaveformsEditor.PaintAllWaveforms;
  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.btnZoomInClick(Sender: TObject);
begin
  if Assigned(FOnZoomIn) then
    FOnZoomIn(Sender);

  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.btnZoomOutClick(Sender: TObject);
begin
  if Assigned(FOnZoomOut) then
    FOnZoomOut(Sender);

  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.btnDeleteTransitionClick(Sender: TObject);
var
  Node: PVirtualNode;
  i: Integer;
begin
  Node := vstTransitions.GetFirst;
  if Node = nil then
    Exit;
    
  Node := vstTransitions.GetFirstSelected;
  if Node = nil then
  begin
    MessageBox(Handle, 'No transition selected. Please select one.', PChar(Caption), MB_ICONERROR);
    Exit;
  end;

  for i := Node^.Index to Length(FSignal.SignalEvents) - 2 do
    FSignal.SignalEvents[i] := FSignal.SignalEvents[i + 1];

  SetLength(FSignal.SignalEvents, Length(FSignal.SignalEvents) - 1);
  
  vstTransitions.RootNodeCount := Length(FSignal.SignalEvents);

  if Node <> nil then  //Node points to the shifted transition
  begin
    vstTransitions.Selected[Node] := True;
    vstTransitions.ScrollIntoView(Node, True);
  end;

  vstTransitions.Repaint;
  FFrameWaveformsEditor.PaintAllWaveforms;
  DrawLocalSignalWaveForm;  
end;


procedure TfrmSignalTransitions.EnableEditBoxesBasedOnRadioGroups;
begin
  rdgrpDiscreteValueType.Enabled := FSignal.Size > 1;
  rdgrpSignalType.Enabled := FSignal.Size = 1;

  if FSignal.Size > 1 then
    rdgrpSignalType.ItemIndex := 0; //discrete
  
  if FSignal.Size = 1 then
  begin
    lbeSignalValueAfterTransitionNum.Enabled := False;
    lbeSignalValueAfterTransitionNum.Color := clBtnFace;
    lbeSignalValueAfterTransitionSTD_LOGIC.Enabled := False;
    lbeSignalValueAfterTransitionSTD_LOGIC.Color := clBtnFace;

    case rdgrpSignalType.ItemIndex of
      0: //discrete
      begin
        lbeClockAfterTransitionMid.Enabled := False;
        lbeClockAfterTransitionMid.Color := clBtnFace;
        lbeClockAfterTransitionPeriod.Enabled := False;
        lbeClockAfterTransitionPeriod.Color := clBtnFace;
        rdgrpSigValAfterTranAtSize1.Enabled := True;
      end;

      1:
      begin
        lbeClockAfterTransitionMid.Enabled := True;
        lbeClockAfterTransitionMid.Color := clWindow;
        lbeClockAfterTransitionPeriod.Enabled := True;
        lbeClockAfterTransitionPeriod.Color := clWindow;
        rdgrpSigValAfterTranAtSize1.Enabled := False;
      end;
    end;
  end
  else
  begin
    rdgrpSigValAfterTranAtSize1.Enabled := False;
    lbeClockAfterTransitionMid.Enabled := False;
    lbeClockAfterTransitionMid.Color := clBtnFace;
    lbeClockAfterTransitionPeriod.Enabled := False;
    lbeClockAfterTransitionPeriod.Color := clBtnFace;

    case rdgrpDiscreteValueType.ItemIndex of
      0, 2, 3, 4:
      begin
        lbeSignalValueAfterTransitionNum.Enabled := True;
        lbeSignalValueAfterTransitionNum.Color := clWindow;
        lbeSignalValueAfterTransitionSTD_LOGIC.Enabled := False;
        lbeSignalValueAfterTransitionSTD_LOGIC.Color := clBtnFace;
      end;

      1:
      begin
        lbeSignalValueAfterTransitionNum.Enabled := False;
        lbeSignalValueAfterTransitionNum.Color := clBtnFace;
        lbeSignalValueAfterTransitionSTD_LOGIC.Enabled := True;
        lbeSignalValueAfterTransitionSTD_LOGIC.Color := clWindow;
      end;
    end; //case
  end;
end;


procedure TfrmSignalTransitions.FormCreate(Sender: TObject);
begin
  FOnZoomIn := nil;
  FOnZoomOut := nil;
  FOnGetScrollBarPos := nil;
  FOnGetMainMarkerPos := nil;
  FOnSetMainMarkerPos := nil;

  FWaveformDrawing := TWaveformDrawing.Create(Self);
  FWaveformDrawing.Parent := pnlWaveforms;
  FWaveformDrawing.Left := 0;
  FWaveformDrawing.Top := 0;
  FWaveformDrawing.Height := pnlWaveforms.Height;
  FWaveformDrawing.Width := pnlWaveforms.Width;
  FWaveformDrawing.Anchors := [akLeft, akTop, akRight, akBottom];
  FWaveformDrawing.OnMouseDown := WaveformDrawingMouseDown;
  FWaveformDrawing.OnMouseMove := WaveformDrawingMouseMove;
  FWaveformDrawing.OnMouseUp := WaveformDrawingMouseUp;
  FWaveformDrawing.OnPaintAllWaveforms := HandleOnPaintAllWaveforms;
end;


procedure TfrmSignalTransitions.FormPaint(Sender: TObject);
begin
  tmrRepaintWaveform.Interval := 100;
  tmrRepaintWaveform.Enabled := True;
end;


procedure TfrmSignalTransitions.FormResize(Sender: TObject);
begin
  tmrRepaintWaveform.Enabled := True;
end;


procedure TfrmSignalTransitions.WaveformDrawingMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Moment: Integer;  
begin
  FFrameWaveformsEditor.WaveformDrawingMouseDown(Sender, Button, Shift, X, -10);

  {if FSignal.Editing then
  begin
    SelectVstNodeByIndex(vstTransitions, FFrameWaveformsEditor.EditingEventNumber);
    FSignal.HighlightedSectionAfterEvent := FFrameWaveformsEditor.EditingEventNumber;
  end;}

  Moment := XVstWaveToMoment(X, FFrameWaveformsEditor.Zoom, DoOnGetScrollBarPos);
  if (ssLeft in Shift) or (ssRight in Shift) then
  begin
    //DrawLocalSignalWaveForm;
    //tmrRepaintVst.Enabled := True;
    FSignal.HighlightedSectionAfterEvent := FFrameWaveformsEditor.EditingEventNumber;

    tmrSelectTransitionByMoment.Enabled := True;
    tmrSelectTransitionByMoment.Tag := Moment;
  end;
end;


procedure TfrmSignalTransitions.WaveformDrawingMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Moment: Integer;
begin
  FFrameWaveformsEditor.WaveformDrawingMouseMove(Sender, Shift, X, -10);

  Moment := XVstWaveToMoment(X, FFrameWaveformsEditor.Zoom, DoOnGetScrollBarPos);

  if (ssLeft in Shift) or (ssRight in Shift) then
  begin
    DrawLocalSignalWaveForm;
    tmrRepaintVst.Enabled := True;

    tmrSelectTransitionByMoment.Enabled := True;
    tmrSelectTransitionByMoment.Tag := Moment;
  end;

  lblSelectedMoment.Caption := 'Moment: ' + IntToStr(Moment);
end;


procedure TfrmSignalTransitions.WaveformDrawingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FFrameWaveformsEditor.WaveformDrawingMouseUp(Sender, Button, Shift, X, -10);
end;


procedure TfrmSignalTransitions.rdgrpDiscreteValueTypeClick(Sender: TObject);
begin
  EnableEditBoxesBasedOnRadioGroups;
end;


procedure TfrmSignalTransitions.rdgrpSignalTypeClick(Sender: TObject);
begin
  EnableEditBoxesBasedOnRadioGroups;
end;


procedure TfrmSignalTransitions.tmrRepaintVstTimer(Sender: TObject);
begin
  tmrRepaintVst.Enabled := False;
  vstTransitions.Repaint;
end;


procedure TfrmSignalTransitions.tmrRepaintWaveformTimer(Sender: TObject);
begin
  tmrRepaintWaveform.Enabled := False;
  tmrRepaintWaveform.Interval := 10;
  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.tmrSelectTransitionByMomentTimer(
  Sender: TObject);
var
  TransitionIndex: Integer;
begin
  tmrSelectTransitionByMoment.Enabled := False;
  TransitionIndex := GetProperEventIndexByMoment(FSignal, tmrSelectTransitionByMoment.Tag);

  if TransitionIndex > -1 then
    SelectVstNodeByIndex(vstTransitions, TransitionIndex, True);
end;


procedure TfrmSignalTransitions.tmrUpdateControlsFromClickTimer(
  Sender: TObject);
begin
  tmrUpdateControlsFromClick.Enabled := False;
  UpdateControlsFromSelectedTransition;
  SetHighlightedSectionAfterEvent;
  FFrameWaveformsEditor.PaintAllWaveforms; // ideally, only the editing one
  DrawLocalSignalWaveForm;
end;


procedure TfrmSignalTransitions.UpdateControlsFromSelectedTransition;
var
  Node: PVirtualNode;
begin
  Node := vstTransitions.GetFirstSelected;
  if Node = nil then
    Exit;

  lbeMoment.Text := IntToStr(FSignal.SignalEvents[Node^.Index].Moment);
  rdgrpSignalType.ItemIndex := Integer(FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent);
  rdgrpDiscreteValueType.ItemIndex := Integer(FSignal.ValueType);

  if FSignal.Size = 1 then
  begin
    lbeSignalValueAfterTransitionNum.Text := '';
    lbeSignalValueAfterTransitionSTD_LOGIC.Text := '';

    case FSignal.SignalEvents[Node^.Index].PatternTypeAfterEvent of
      ptDiscrete:
        rdgrpSigValAfterTranAtSize1.ItemIndex := NumericSignalToInteger(FSignal.SignalEvents[Node^.Index].ValueAfterEvent);

      ptClock:
      begin
        rdgrpSigValAfterTranAtSize1.ItemIndex := -1;

        lbeClockAfterTransitionMid.Text := IntToStr(FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.MidTransitionOffset);
        lbeClockAfterTransitionPeriod.Text := IntToStr(FSignal.SignalEvents[Node^.Index].ClockPatternAfterEvent.EndTransitionOffset);
      end;
    end; //case
  end
  else
  begin
    rdgrpSigValAfterTranAtSize1.ItemIndex := -1;
    lbeClockAfterTransitionMid.Text := '';
    lbeClockAfterTransitionPeriod.Text := '';

    case FSignal.ValueType of
      sutNumeric:
      lbeSignalValueAfterTransitionNum.Text := IntToStr(NumericSignalToInteger(FSignal.SignalEvents[Node^.Index].ValueAfterEvent));
      sutSTD_LOGIC: lbeSignalValueAfterTransitionSTD_LOGIC.Text := ArrayOfTSTD_LOGIC_ToString(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.STD_LOGIC_Value);
      sutOrdinal: lbeSignalValueAfterTransitionNum.Text := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.OrdinalValue);
      sutInt32: lbeSignalValueAfterTransitionNum.Text := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.Int32Value);
      sutInt64: lbeSignalValueAfterTransitionNum.Text := IntToStr(FSignal.SignalEvents[Node^.Index].ValueAfterEvent.Int64Value);
    end;
  end;

  EnableEditBoxesBasedOnRadioGroups;
end;


procedure TfrmSignalTransitions.SetHighlightedSectionAfterEvent;
var
  Node: PVirtualNode;
begin
  Node := vstTransitions.GetFirstSelected;
  if Node = nil then
  begin
    FSignal.HighlightedSectionAfterEvent := -1;
    Exit;
  end;

  FSignal.HighlightedSectionAfterEvent := Node^.Index;
end;


procedure TfrmSignalTransitions.HandleOnPaintAllWaveforms;
begin
  DrawLocalSignalWaveForm;
end;


end.
