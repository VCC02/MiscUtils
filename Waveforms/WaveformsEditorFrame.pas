{
    Copyright (C) 2025 VCC
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


unit WaveformsEditorFrame;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
    {$IFDEF Windows}
      Windows,
    {$ELSE}
      LCLIntf, LCLType,
    {$ENDIF}
  {$ELSE}
    Windows,
  {$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, ExtCtrls, Menus, ComCtrls, IniFiles,
  {$IFDEF FPC}
    {$IFDEF Windows}
      ActiveX,
    {$ELSE}
      FakeActiveX,
    {$ENDIF}
  {$ELSE}
    ActiveX,
  {$ENDIF}
  WaveformUtils;

type
  TOnMenuPopup = procedure(APopupPoint: TPoint) of object;
  TOnUpdatingPopupMenuItemsEnabledState = procedure(SignalIsEditing: Boolean) of object;
  TOnUpdateListOfTransitionsFormWithSelectedSignal = procedure(AData: PSignalRec) of object;

  TFrameWaveformsEditor = class(TFrame)
    scrboxWaveforms: TScrollBox;
    scrbarTimeWaveforms: TScrollBar;
    tmrPaintWaveforms: TTimer;
    tmrCtrlKey: TTimer;
    chkXes: TCheckBox;
    tmrMouseSelection: TTimer;
    procedure vstWaveformsEditorAfterPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure tmrPaintWaveformsTimer(Sender: TObject);
    procedure tmrCtrlKeyTimer(Sender: TObject);
    procedure vstWaveformsEditorClick(Sender: TObject);
    procedure vstWaveformsEditorColumnResize(Sender: TVTHeader;
      Column: TColumnIndex);
    procedure vstWaveformsEditorDragAllowed(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure vstWaveformsEditorDragDrop(Sender: TBaseVirtualTree;
      Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
      Shift: TShiftState; {$IFDEF FPC}const{$ENDIF} Pt: TPoint; var Effect: {$IFDEF FPC}LongWord{$ELSE}Integer{$ENDIF}; Mode: TDropMode);
    procedure vstWaveformsEditorDragOver(Sender: TBaseVirtualTree;
      Source: TObject; Shift: TShiftState; State: TDragState; {$IFDEF FPC}const{$ENDIF} Pt: TPoint;
      Mode: TDropMode; var Effect: {$IFDEF FPC}LongWord{$ELSE}Integer{$ENDIF}; var Accept: Boolean);
    procedure vstWaveformsEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstWaveformsEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstWaveformsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstWaveformsEditorMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure vstHeaderWavesColumnResize(Sender: TVTHeader;
      Column: TColumnIndex);
    procedure scrbarTimeWaveformsChange(Sender: TObject);
    procedure vstWaveformsEditorGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: {$IFDEF FPC}string{$ELSE}WideString{$ENDIF});
    procedure WaveformDrawingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WaveformDrawingMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure WaveformDrawingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstWaveformsEditorPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure tmrMouseSelectionTimer(Sender: TObject);
    procedure vstWaveformsEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FStatusBar: TStatusBar;

    FZoom: Extended;
    FMainMarkerPos: Integer;
    FMainMarkerPosXOnVst: Integer;
    FEditingSignal: PSignal;
    FEditingEventNumber: Integer;
    FMoveAllWaveforms: Boolean;
    FAllWaveformsMousePosition: Integer;
    FScrbarWavePosOnWaveDrag: Integer;
    FAllowToDrawValidTransitions: Boolean;
    FDrawGreenZero: Boolean;
    FPaintNumber: Integer;
    FAllowEditingSignalValueFromPM: Boolean;
    FMouseDownOnNode: PVirtualNode;

    FLastClickedMoment: Extended; //used for signal editing
    FLastClickedSignal: PSignal; //used for signal editing
    FSelectedNode: Integer;
    FCtrlKeyPressed: Boolean;
    FAltKeyPressed: Boolean;
    FPaintTransitionsOnAllZoomLevels: Boolean; //if false, the transition Lines are painted only above zoom level 4
    FPaintTransitionsAsInserting: Boolean; //if true, transitions are painted in yellow (only when FPaintTransitionsOnAllZoomLevels is used)
    FEnabledPaintingTransitionsAsInserting: Boolean; //only when true, FPaintTransitionsAsInserting is used  (this is required, to avoid displaying insert-transitions on readonly scope)

    FOnMenuPopup: TOnMenuPopup;
    FOnUpdatingPopupMenuItemsEnabledState: TOnUpdatingPopupMenuItemsEnabledState;
    FOnUpdateListOfTransitionsFormWithSelectedSignal: TOnUpdateListOfTransitionsFormWithSelectedSignal;

    FSampleDuration: Extended; //duration between two samples (for displaying on StatusBar and/or cursor hint)

    FWaveformDrawing: TWaveformDrawing;

    function GetDrawXes: Boolean;
    function SignalValueOnXVstWave(ASignal: PSignal; X: Integer): TSignalValueType;
    function MarkerIsCloseToATransition(ASignal: PSignal; X: Integer; var EventNumber: Integer): Boolean;
    function GetLastTransitionFromSignalsOnEditor: Integer;
    procedure HandleSpecialKeys(CtrlKeyDown, AltKeyDown: Boolean);
    //function GetMomentOnWaveformCenter: Extended;

    procedure UpdateListOfTransitionsFormWithSelectedSignal(Node: PVirtualNode);
    procedure UpdateStatusBarPanel(APanelIndex: Integer; AWaveformXPoint: Integer = 0);

    procedure HandleOnPaintAllWaveforms;

    procedure CreateRemainingUIComponents;
    procedure InitFrame;
  public
    { Public declarations }
    vstHeaderWaves: TVirtualStringTree;
    vstWaveformsEditor: TVirtualStringTree;
    pnlWaveforms: TPanel;

    constructor Create(AOwner: TComponent); override; 

    procedure LoadSettings(AIni: TMemIniFile);
    procedure SaveSettings(AIni: TMemIniFile);

    procedure AddSignalToWaveformEditorVST(ASignal: PSignal);
    procedure DeleteSignalFromWaveformEditorVST(ASignal: PSignal);
    //procedure AddSignalToWaveformEditorVSTWithCopy(ASignal: PSignal);

    procedure PaintAllWaveforms(ACanvas: TCanvas); overload;
    procedure PaintAllWaveforms;                   overload;
    function GetVstWavesVertScrollPos: Integer;
    procedure SelectNodeByIndex(NodeIndex: Cardinal);
    procedure SelectNodeByYCoord(Y: Integer);
    procedure SetWaveformEditorScrollMax;
    procedure RefreshOnColumnResize;

    procedure GetSelectedSignalRec(out AData: PSignalRec);   //returns nil if no signal is selected

    procedure EditSelectedSignal;
    procedure ExitEditingMode;

    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property Zoom: Extended read FZoom write FZoom;
    property EditingEventNumber: Integer read FEditingEventNumber;
    property MainMarkerPos: Integer read FMainMarkerPos write FMainMarkerPos;

    property LastClickedMoment: Extended read FLastClickedMoment; //used for signal editing
    property LastClickedSignal: PSignal read FLastClickedSignal; //used for signal editing

    property AllowEditingSignalValueFromPopupMenu: Boolean read FAllowEditingSignalValueFromPM;

    property SampleDuration: Extended read FSampleDuration write FSampleDuration;   //ns
    property EnabledPaintingTransitionsAsInserting: Boolean read FEnabledPaintingTransitionsAsInserting write FEnabledPaintingTransitionsAsInserting;

    property PaintTransitionsOnAllZoomLevels: Boolean read FPaintTransitionsOnAllZoomLevels;
    property AllowToDrawValidTransitions: Boolean read FAllowToDrawValidTransitions;
    property PaintTransitionsAsInserting: Boolean read FPaintTransitionsAsInserting;
    property DrawGreenZero: Boolean read FDrawGreenZero;
    property DrawXes: Boolean read GetDrawXes;

    property OnMenuPopup: TOnMenuPopup read FOnMenuPopup write FOnMenuPopup;
    property OnUpdatingPopupMenuItemsEnabledState: TOnUpdatingPopupMenuItemsEnabledState read FOnUpdatingPopupMenuItemsEnabledState write FOnUpdatingPopupMenuItemsEnabledState;
    property OnUpdateListOfTransitionsFormWithSelectedSignal: TOnUpdateListOfTransitionsFormWithSelectedSignal read FOnUpdateListOfTransitionsFormWithSelectedSignal write FOnUpdateListOfTransitionsFormWithSelectedSignal;
  end;


implementation

{$IFDEF FPC}
  {$R *.frm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  Math, MouseHintForm, WaveformFuncUtils, WaveformDrawing;


procedure TFrameWaveformsEditor.CreateRemainingUIComponents;
var
  NewColum: TVirtualTreeColumn;
begin
  pnlWaveforms := TPanel.Create(Self);
  pnlWaveforms.Parent := scrboxWaveforms;
  pnlWaveforms.FullRepaint := False;
  pnlWaveforms.Left := 0;
  pnlWaveforms.Top := 0;
  pnlWaveforms.Width := 1020;
  pnlWaveforms.Height := 319;
  pnlWaveforms.Constraints.MinWidth := 1020;
  pnlWaveforms.Constraints.MinHeight := 319;
  pnlWaveforms.Anchors := [akLeft, akTop, akRight, akBottom];
  pnlWaveforms.Color := clWhite;

  vstHeaderWaves := TVirtualStringTree.Create(Self);
  vstHeaderWaves.Parent := Self;

  vstHeaderWaves.Left := 4;
  vstHeaderWaves.Top := 2;
  vstHeaderWaves.Width := 1033;
  vstHeaderWaves.Height := 22;
  vstHeaderWaves.Anchors := [akLeft, akTop, akRight];
  vstHeaderWaves.Colors.FocusedSelectionColor := clSkyBlue;
  vstHeaderWaves.Colors.FocusedSelectionBorderColor := clMoneyGreen;
  vstHeaderWaves.DefaultNodeHeight := 19;
  vstHeaderWaves.Font.Charset := DEFAULT_CHARSET;
  vstHeaderWaves.Font.Color := clBlack;
  vstHeaderWaves.Font.Height := -11;
  vstHeaderWaves.Font.Name := 'Tahoma';
  vstHeaderWaves.Font.Style := [];
  vstHeaderWaves.Header.AutoSizeIndex := 0;
  vstHeaderWaves.Header.DefaultHeight := 21;
  vstHeaderWaves.Header.Font.Charset := DEFAULT_CHARSET;
  vstHeaderWaves.Header.Font.Color := clWindowText;
  vstHeaderWaves.Header.Font.Height := -11;
  vstHeaderWaves.Header.Font.Name := 'Tahoma';
  vstHeaderWaves.Header.Font.Style := [];
  vstHeaderWaves.Header.Height := 21;
  vstHeaderWaves.Header.Options := [hoColumnResize, hoShowSortGlyphs, hoVisible];
  vstHeaderWaves.Header.Style := hsFlatButtons;
  vstHeaderWaves.LineStyle := lsSolid;
  vstHeaderWaves.ParentFont := False;
  vstHeaderWaves.ScrollBarOptions.ScrollBars := ssNone;
  vstHeaderWaves.TabOrder := 1;
  vstHeaderWaves.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking];
  vstHeaderWaves.TreeOptions.MiscOptions := [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toFullRowDrag, toNodeHeightResize];
  vstHeaderWaves.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines];
  vstHeaderWaves.TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  vstHeaderWaves.OnColumnResize := vstHeaderWavesColumnResize;

  NewColum := vstHeaderWaves.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 0;
  NewColum.Width := 150;
  NewColum.Text := 'Index: Signal  (Size)  [Type]';

  NewColum := vstHeaderWaves.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 1;
  NewColum.Width := 70;
  NewColum.Text := 'Value';

  NewColum := vstHeaderWaves.Header.Columns.Add;
  NewColum.MinWidth := 2000;
  NewColum.MaxWidth := 2000;
  NewColum.Position := 3;
  NewColum.Width := 2000;
  NewColum.Text := 'Waveforms';

  NewColum := vstHeaderWaves.Header.Columns.Add;
  NewColum.MinWidth := 100;
  NewColum.Position := 2;
  NewColum.Width := 100;
  NewColum.Text := 'Synchronized By';

  vstWaveformsEditor := TVirtualStringTree.Create(Self);
  vstWaveformsEditor.Parent := pnlWaveforms;

  vstWaveformsEditor.Left := -1;
  vstWaveformsEditor.Top := -3;
  vstWaveformsEditor.Width := 572;
  vstWaveformsEditor.Height := 322;
  vstWaveformsEditor.Anchors := [akLeft, akTop, akBottom];
  vstWaveformsEditor.DefaultNodeHeight := 19;
  vstWaveformsEditor.Font.Charset := DEFAULT_CHARSET;
  vstWaveformsEditor.Font.Color := clBlack;
  vstWaveformsEditor.Font.Height := -11;
  vstWaveformsEditor.Font.Name := 'Tahoma';
  vstWaveformsEditor.Font.Style := [];
  vstWaveformsEditor.Header.AutoSizeIndex := 0;
  vstWaveformsEditor.Header.DefaultHeight := 21;
  vstWaveformsEditor.Header.Font.Charset := DEFAULT_CHARSET;
  vstWaveformsEditor.Header.Font.Color := clWindowText;
  vstWaveformsEditor.Header.Font.Height := -11;
  vstWaveformsEditor.Header.Font.Name := 'Tahoma';
  vstWaveformsEditor.Header.Font.Style := [];
  vstWaveformsEditor.Header.Height := 21;
  vstWaveformsEditor.Header.Options := [hoColumnResize, hoShowSortGlyphs];
  vstWaveformsEditor.Header.Style := hsXPStyle;
  vstWaveformsEditor.LineStyle := lsSolid;
  vstWaveformsEditor.ParentFont := False;
  vstWaveformsEditor.ScrollBarOptions.ScrollBars := ssNone;
  vstWaveformsEditor.TabOrder := 0;
  vstWaveformsEditor.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking];
  vstWaveformsEditor.TreeOptions.MiscOptions := [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toVariableNodeHeight, toFullRowDrag, toNodeHeightResize, toNodeHeightDblClickResize];
  vstWaveformsEditor.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages, toFullVertGridLines];
  vstWaveformsEditor.TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  vstWaveformsEditor.OnAfterPaint := vstWaveformsEditorAfterPaint;
  vstWaveformsEditor.OnClick := vstWaveformsEditorClick;
  vstWaveformsEditor.OnColumnResize := vstWaveformsEditorColumnResize;
  vstWaveformsEditor.OnDragAllowed := vstWaveformsEditorDragAllowed;
  vstWaveformsEditor.OnDragOver := vstWaveformsEditorDragOver;
  vstWaveformsEditor.OnDragDrop := vstWaveformsEditorDragDrop;
  vstWaveformsEditor.OnGetText := vstWaveformsEditorGetText;
  vstWaveformsEditor.OnPaintText := vstWaveformsEditorPaintText;
  vstWaveformsEditor.OnKeyDown := vstWaveformsEditorKeyDown;
  vstWaveformsEditor.OnKeyUp := vstWaveformsEditorKeyUp;
  vstWaveformsEditor.OnMouseDown := vstWaveformsEditorMouseDown;
  vstWaveformsEditor.OnMouseUp := vstWaveformsEditorMouseUp;
  vstWaveformsEditor.OnMouseWheel := vstWaveformsEditorMouseWheel;

  NewColum := vstWaveformsEditor.Header.Columns.Add;
  NewColum.MinWidth := 150;
  NewColum.Position := 0;
  NewColum.Width := 150;
  NewColum.Text := 'Index: Signal  (Size)  [Type]';
  NewColum.Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coFixed, coAllowFocus];

  NewColum := vstWaveformsEditor.Header.Columns.Add;
  NewColum.MinWidth := 70;
  NewColum.Position := 1;
  NewColum.Width := 70;
  NewColum.Text := 'Value';

  NewColum := vstWaveformsEditor.Header.Columns.Add;
  NewColum.MinWidth := 0;
  NewColum.MaxWidth := 10;
  NewColum.Position := 3;
  NewColum.Width := 10;
  NewColum.Text := 'Waveforms';

  NewColum := vstWaveformsEditor.Header.Columns.Add;
  NewColum.MinWidth := 100;
  NewColum.Position := 2;
  NewColum.Width := 100;
  NewColum.Text := 'Synchronized By';

  Application.CreateForm(TfrmMouseHint, frmMouseHint);
end;


constructor TFrameWaveformsEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateRemainingUIComponents;
  InitFrame;
end;


procedure TFrameWaveformsEditor.LoadSettings(AIni: TMemIniFile);
var
  Node: PVirtualNode;
  NodeData: PSignalRec;
begin
  Node := vstWaveformsEditor.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstWaveformsEditor.GetNodeData(Node);
    if Assigned(NodeData) then
      vstWaveformsEditor.NodeHeight[Node] := AIni.ReadInteger('Signals', 'NodeHeight_' + NodeData^.Signal^.SignalName, vstWaveformsEditor.DefaultNodeHeight);

    Node := vstWaveformsEditor.GetNext(Node);
  until Node = nil;
end;


procedure TFrameWaveformsEditor.SaveSettings(AIni: TMemIniFile);
var
  Node: PVirtualNode;
  NodeData: PSignalRec;
begin
  Node := vstWaveformsEditor.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstWaveformsEditor.GetNodeData(Node);
    if Assigned(NodeData) then
      AIni.WriteInteger('Signals', 'NodeHeight_' + NodeData^.Signal^.SignalName, vstWaveformsEditor.NodeHeight[Node]);

    Node := vstWaveformsEditor.GetNext(Node);
  until Node = nil;
end;


function TFrameWaveformsEditor.GetDrawXes: Boolean;
begin
  Result := chkXes.Checked;
end;


procedure TFrameWaveformsEditor.AddSignalToWaveformEditorVST(ASignal: PSignal);
var
  Data: PSignalRec; //pointer to array of string
  CurrentNode: PVirtualNode;
begin
  if not Assigned(ASignal) then
    Exit;

  CurrentNode := vstWaveformsEditor.InsertNode(vstWaveformsEditor.RootNode, amInsertAfter);
  Data := vstWaveformsEditor.GetNodeData(CurrentNode);

  Data^.Signal := ASignal;
end;


procedure TFrameWaveformsEditor.DeleteSignalFromWaveformEditorVST(ASignal: PSignal);
var
  Data: PSignalRec; //pointer to array of string
  CurrentNode: PVirtualNode;
begin
  if not Assigned(ASignal) then
    Exit;

  CurrentNode := vstWaveformsEditor.GetFirst;
  repeat
    Data := vstWaveformsEditor.GetNodeData(CurrentNode);
    if Data^.Signal = ASignal then
    begin
      vstWaveformsEditor.DeleteNode(CurrentNode);
      Exit;
    end;

    CurrentNode := CurrentNode^.NextSibling;
  until CurrentNode = nil;

  PaintAllWaveforms;
end;  


{procedure TFrameWaveformsEditor.AddSignalToWaveformEditorVSTWithCopy(ASignal: PSignal);
var
  Data: PSignalRec; //pointer to array of string
  CurrentNode: PVirtualNode;
begin
  if not Assigned(ASignal) then
    Exit; 

  CurrentNode := vstWaveformsEditor.RootNode;
  CurrentNode := vstWaveformsEditor.InsertNode(CurrentNode.LastChild, amInsertAfter);
  Data := vstWaveformsEditor.GetNodeData(CurrentNode);

  New(Data^.Signal);
  Data^.Signal^ := ASignal^;
end;}


procedure TFrameWaveformsEditor.SetWaveformEditorScrollMax;   /////////////////////////////////// not ready
var
  X: Integer;
  FMainMarkerPosXOnVst_Temp: Integer;
begin
  scrbarTimeWaveforms.Max := Max(1, GetLastTransitionFromSignalsOnEditor);
  //scrbarTimeWaveforms.PageSize := Round(scrbarTimeWaveforms.Max / scrbarTimeWaveforms.Width * 100);
  FMainMarkerPosXOnVst_Temp := FMainMarkerPosXOnVst;

  X := vstWaveformsEditor.Header.Columns.Items[2].Width shr 1; //Round(GetMomentOnWaveformCenter);
  //FMainMarkerPosXOnVst := X + vstWaveformsEditor.Header.Columns.Items[2].Left;     //.................
  FMainMarkerPosXOnVst := X;
  FMainMarkerPos := X;

  UpdateStatusBarPanel(4);
  scrbarTimeWaveforms.Position := XVstWaveToMoment(FMainMarkerPosXOnVst_Temp, FZoom, scrbarTimeWaveforms.Position);
end;


procedure TFrameWaveformsEditor.GetSelectedSignalRec(out AData: PSignalRec);
var
  Node: PVirtualNode;
begin
  Node := vstWaveformsEditor.GetFirstSelected;
  if Node = nil then
  begin
    AData := nil;
    Exit;
  end;

  AData := vstWaveformsEditor.GetNodeData(Node);
end;


procedure TFrameWaveformsEditor.UpdateListOfTransitionsFormWithSelectedSignal(Node: PVirtualNode);
var
  Data: PSignalRec;
begin
  Data := vstWaveformsEditor.GetNodeData(Node);
  if Assigned(FOnUpdateListOfTransitionsFormWithSelectedSignal) then
    FOnUpdateListOfTransitionsFormWithSelectedSignal(Data);
end;


function TFrameWaveformsEditor.SignalValueOnXVstWave(ASignal: PSignal; X: Integer): TSignalValueType;
var
  Moment: Extended;
  i, n: Integer;
begin
  Result.OrdinalValue := 0;

  Moment := XVstWaveToMomentExt(X, FZoom, scrbarTimeWaveforms.Position);
  n := Length(ASignal.SignalEvents);
  if n > 2 then
    if Moment >= ASignal.SignalEvents[n - 1].Moment then
    begin
      Result := ASignal.SignalEvents[n - 1].ValueAfterEvent;
      Exit;
    end;

  for i := 0 to n - 2 do
    if InIntervalExtended(Moment, ASignal.SignalEvents[i].Moment, ASignal.SignalEvents[i + 1].Moment) then
    begin
      Result := ASignal.SignalEvents[i].ValueAfterEvent;
      Exit;
    end;
end;


function TFrameWaveformsEditor.MarkerIsCloseToATransition(ASignal: PSignal; X: Integer; var EventNumber: Integer): Boolean;
var
  i: Integer;
  Moment: Extended;
  CurrentMoment: Extended;
  ZoomInv: Extended;
begin
  Result := False;
  Moment := XVstWaveToMomentExt(X, FZoom, scrbarTimeWaveforms.Position);
  ZoomInv := 1 / FZoom;

  for i := 0 to Length(ASignal.SignalEvents) - 1 do
  begin
    CurrentMoment := ASignal.SignalEvents[i].Moment;

    if InIntervalExtended(Moment, CurrentMoment - ZoomInv, CurrentMoment + ZoomInv) then
    begin
      Result := True;
      EventNumber := i;
      Exit;
    end;
  end;
end;


function TFrameWaveformsEditor.GetVstWavesVertScrollPos: Integer;
const
  CNodeHeight: Integer = 18;
var
  i: Integer;
  SecondNodeOffset: Integer;
  Node1, Node2: PVirtualNode;
  NodesAtPos: array of PVirtualNode;
begin
  SecondNodeOffset := 0;

  SetLength(NodesAtPos, CNodeHeight + 2 + 1);
  for i := 0 to CNodeHeight + 2 do
    NodesAtPos[i] := vstWaveformsEditor.GetNodeAt(0, i);
  
  for i := 0 to CNodeHeight + 2 do
  begin
//    Node1 := vstWaveformsEditor.GetNodeAt(0, i);
//    Node2 := vstWaveformsEditor.GetNodeAt(0, i + 1);
    Node1 := NodesAtPos[i];
    Node2 := NodesAtPos[i + 1];

    if Node1 <> Node2 then
      if Assigned(Node1) then
      begin
        SecondNodeOffset := i;
        Break;
      end;
  end;
  
  Result := Integer((vstWaveformsEditor.GetNodeAt(0, 0).Index + 1) * (vstWaveformsEditor.DefaultNodeHeight + 1)) - SecondNodeOffset - 2;
end;


procedure TFrameWaveformsEditor.SelectNodeByIndex(NodeIndex: Cardinal);
begin
  if SelectVSTNodeByIndex(vstWaveformsEditor, NodeIndex, True) then
    FSelectedNode := NodeIndex;
end;


procedure TFrameWaveformsEditor.SelectNodeByYCoord(Y: Integer);
var
  Node: PVirtualNode;
begin
  if Y < 0 then
    Exit;
    
  Node := vstWaveformsEditor.GetNodeAt(0, Y);
  if not Assigned(Node) then
    Exit;

  vstWaveformsEditor.Selected[Node] := True;
  vstWaveformsEditor.SetFocus;
  FSelectedNode := Node.Index;
end;


procedure TFrameWaveformsEditor.PaintAllWaveforms(ACanvas: TCanvas);
var
  Node: PVirtualNode;
  NodeHeight: Integer;
  Data: PSignalRec;
  CellRect: TRect;
  NodeTop: Integer;
  Bmp: TBitmap;
  DrawXes: Boolean;
begin
  Node := vstWaveformsEditor.GetFirst;
  if not Assigned(Node) then
    Exit;

  Bmp := TBitmap.Create;
  try
    Bmp.Width := FWaveformDrawing.Width;
    Bmp.Height := FWaveformDrawing.Height;

    NodeHeight := vstWaveformsEditor.NodeHeight[Node]; //.DefaultNodeHeight;
    FDrawGreenZero := True;
    DrawXes := chkXes.Checked;

    FWaveformDrawing.EnabledPaint := False;
    FWaveformDrawing.Height := TotalNodeHeights(vstWaveformsEditor) + 4; //NodeHeight * Integer(vstWaveformsEditor.RootNodeCount) + 4;
    Application.ProcessMessages;  //handle TWaveformDrawing.Paint, without actually calling PaintAllWaveforms
    FWaveformDrawing.EnabledPaint := True;

    vstWaveformsEditor.Height := FWaveformDrawing.Height;

    NodeTop := 0;

    repeat
      Data := vstWaveformsEditor.GetNodeData(Node);

      if Assigned(Data) then
      begin
        CellRect.Top := NodeTop; //(NodeHeight + 0) * Integer(Node.Index);
        CellRect.Bottom := CellRect.Top + Node^.NodeHeight; // NodeHeight;
        CellRect.Left := 0; //vstWaveformsEditor.Header.Columns.Items[2].Left;
        CellRect.Right := FWaveformDrawing.Width;

        DrawSignalWaveForm(Bmp.Canvas, Data, CellRect,
                           NodeHeight, FWaveformDrawing.Width,
                           vstWaveformsEditor.Selected[Node], FPaintTransitionsOnAllZoomLevels, FAllowToDrawValidTransitions, FPaintTransitionsAsInserting, FDrawGreenZero, DrawXes,
                           scrbarTimeWaveforms.Position, FMainMarkerPos, FZoom);
      end;

      NodeTop := NodeTop + Node^.NodeHeight;
      Node := Node^.NextSibling;
    until Node = nil;

    ACanvas.Draw(0, 0, Bmp);
  finally
    Bmp.Free;              
  end;
end;


procedure TFrameWaveformsEditor.PaintAllWaveforms;
begin
  if FWaveformDrawing <> nil then  // FWaveformDrawing is nil before initializing, but Paint can be called nonetheless
    PaintAllWaveforms(FWaveformDrawing.Canvas);
end;


function TFrameWaveformsEditor.GetLastTransitionFromSignalsOnEditor: Integer;
var
  CurrentNode: PVirtualNode;
  nn: Integer;
  Data: PSignalRec;
begin
  CurrentNode := vstWaveformsEditor.GetFirst;
  Result := -1;

  if not Assigned(CurrentNode) then
    Exit;

  repeat
    Data := vstWaveformsEditor.GetNodeData(CurrentNode);
    nn := Length(Data.Signal.SignalEvents);
    if nn > 0 then
      if Data.Signal.SignalEvents[nn - 1].Moment > Result then
        Result := Data.Signal.SignalEvents[nn - 1].Moment;

    CurrentNode := CurrentNode^.NextSibling; ///////////////////////////////////    Works only for waveforms with table structure. Doesn't work for tree structure.  No problem for children nodes containing bits from parent nodes. (Problem for e.g. subcircuits)
  until CurrentNode = nil;
end;


{function TFrameWaveformsEditor.GetMomentOnWaveformCenter: Extended;
begin
  //Result := XVstWaveToMomentExt(vstWaveformsEditor.Header.Columns.Items[2].Left + vstWaveformsEditor.Header.Columns.Items[2].Width shr 1);  //.................
  Result := XVstWaveToMomentExt(lblWaveforms.Width shr 1);
end;  }




//------------------------------------------------------------------------------

procedure TFrameWaveformsEditor.InitFrame;
begin
  vstWaveformsEditor.NodeDataSize := SizeOf(TSignalRec);
  FZoom := 1;
  FSampleDuration := 1; //ns
  UpdateStatusBarPanel(1);

  FWaveformDrawing := TWaveformDrawing.Create(Self);
  FWaveformDrawing.Parent := pnlWaveforms;
  FWaveformDrawing.Left := vstWaveformsEditor.Width + 1;
  FWaveformDrawing.Top := 0;
  FWaveformDrawing.Height := vstWaveformsEditor.Height;
  FWaveformDrawing.Width := pnlWaveforms.Width - vstWaveformsEditor.Width - 1;
  FWaveformDrawing.Anchors := [akLeft, akTop, akRight, akBottom];
  FWaveformDrawing.OnMouseDown := WaveformDrawingMouseDown;
  FWaveformDrawing.OnMouseMove := WaveformDrawingMouseMove;
  FWaveformDrawing.OnMouseUp := WaveformDrawingMouseUp;
  FWaveformDrawing.OnPaintAllWaveforms := HandleOnPaintAllWaveforms;

  FMainMarkerPos := 1;
  FMainMarkerPosXOnVst := 0;
  FEditingSignal := nil;
  FEditingEventNumber := 0;
  FMoveAllWaveforms := False;
  FAllowToDrawValidTransitions := False;
  FDrawGreenZero := True;
  FPaintNumber := 0;
  FAllowEditingSignalValueFromPM := False;
  FSelectedNode := -1;
  FCtrlKeyPressed := False;
  FAltKeyPressed := False;
  FPaintTransitionsOnAllZoomLevels := False;
  FPaintTransitionsAsInserting := False;
  FMouseDownOnNode := nil;
  FEnabledPaintingTransitionsAsInserting := True; //defaults to true, so it has to be set to false, externally
end;


procedure TFrameWaveformsEditor.WaveformDrawingMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node{, NodeOver}: PVirtualNode;
  Data: PSignalRec;
  EventNumber, n, NewIndex: Integer;
  ScrBarPos: Integer;
begin
  SelectNodeByYCoord(Y);
  ScrBarPos := scrbarTimeWaveforms.Position;

  if ssLeft in Shift then
  begin
    //if X - vstWaveformsEditor.Header.Columns.Items[2].Left > 0 then
    if X > 0 then
    begin
      //FMainMarkerPos := X - vstWaveformsEditor.Header.Columns.Items[2].Left;
      FMainMarkerPos := X;

      FMainMarkerPosXOnVst := X;

      UpdateStatusBarPanel(2, X);

      Node := vstWaveformsEditor.GetFirstSelected;
      //NodeOver := vstWaveformsEditor.GetNodeAt(X, Y - ScrollBox1.VertScrollBar.Position + 2);
      if Assigned(Node) {and Assigned(NodeOver)} then
      begin
        FSelectedNode := Node.Index;
        Data := vstWaveformsEditor.GetNodeData(Node);
        if //(Node = NodeOver) and
           InIntervalInteger(X, 0, 0 + FWaveformDrawing.Width) and
           Data.Signal.Editing then
        begin
          if FPaintTransitionsAsInserting then   //insert transition by clicking on the signal
          begin
            n := Length(Data.Signal.SignalEvents);
            SetLength(Data.Signal.SignalEvents, n + 1);

            Data.Signal.SignalEvents[n].Moment := XVstWaveToMoment(X, FZoom, ScrBarPos);
            if n = 1 then
              Data.Signal.SignalEvents[n].PatternTypeAfterEvent := ptDiscrete
            else
              Data.Signal.SignalEvents[n].PatternTypeAfterEvent := Data.Signal.SignalEvents[n - 1].PatternTypeAfterEvent;

            case Data.Signal.SignalEvents[n].PatternTypeAfterEvent of
              ptClock:
              begin
                Data.Signal.SignalEvents[n].ClockPatternAfterEvent.MidTransitionOffset := 1;
                Data.Signal.SignalEvents[n].ClockPatternAfterEvent.EndTransitionOffset := 2;
                Data.Signal.SignalEvents[n].ValueAfterEvent.NumericValue[0] := 0;
              end;

              ptDiscrete:
              begin
                SetLength(Data.Signal.SignalEvents[n].ValueAfterEvent.NumericValue, 1); //one byte
                {if n = 1 then
                  Data.Signal.SignalEvents[n].ValueAfterEvent.NumericValue[0] := 1
                else
                  Data.Signal.SignalEvents[n].ValueAfterEvent.NumericValue[0] := (not Data.Signal.SignalEvents[n - 1].ValueAfterEvent.NumericValue[0]) and 1;}

                Data.Signal.SignalEvents[n].EditingEvent := False;
                Data.Signal.SignalEvents[n].EventType := 0;
                Data.Signal.SignalEvents[n].Highlighted := False; 
              end;
            end;

            //ToDo:  ////////////////////////////////////  Handle signal size > 1 !!!!
            if n > 0 then
              NewIndex := MoveLastSignalEventToProperIndex(Data.Signal)
            else
              NewIndex := 0;

            if Data.Signal.SignalEvents[NewIndex].PatternTypeAfterEvent = ptDiscrete then
            begin
              if NewIndex = 0 then
                Data.Signal.SignalEvents[0].ValueAfterEvent.NumericValue[0] := 1
              else
                Data.Signal.SignalEvents[NewIndex].ValueAfterEvent.NumericValue[0] := (not Data.Signal.SignalEvents[NewIndex - 1].ValueAfterEvent.NumericValue[0]) and 1;
            end;

          end  //FPaintTransitionsAsInserting
          else
            if MarkerIsCloseToATransition(Data.Signal, X, EventNumber) then
            begin
              Data.Signal.SignalEvents[EventNumber].EditingEvent := True;
              FEditingSignal := Data.Signal;
              FEditingEventNumber := EventNumber;
            end;
        end;  //editing

        Data.Signal.HighlightedSectionAfterEvent := GetEventIndexByXVst(Data.Signal, X, FZoom, ScrBarPos);
        
        UpdateListOfTransitionsFormWithSelectedSignal(Node);
      end; //Assigned(Node)
    end
    else
      FMainMarkerPos := 0;

    vstWaveformsEditor.Repaint;
  end;

  if ssRight in Shift then
  begin
    FMoveAllWaveforms := True;
    FAllWaveformsMousePosition := X;
    FScrbarWavePosOnWaveDrag := ScrBarPos;


    //almost same as above  - only to make FAllowEditingSignalValueFromPM True
    //if X - vstWaveformsEditor.Header.Columns.Items[2].Left > 0 then
    if X > 0 then
    begin
      //FMainMarkerPos := X - vstWaveformsEditor.Header.Columns.Items[2].Left;
      FMainMarkerPos := X;
      FMainMarkerPosXOnVst := X;

      Node := vstWaveformsEditor.GetFirstSelected;
      //NodeOver := vstWaveformsEditor.GetNodeAt(X, Y - ScrollBox1.VertScrollBar.Position + 2);
      if Assigned(Node) {and Assigned(NodeOver)} then
      begin
        FSelectedNode := Node.Index;
        Data := vstWaveformsEditor.GetNodeData(Node);
        if //(Node = NodeOver) and
           InIntervalInteger(X, 0, 0 + FWaveformDrawing.Width) and 
           Data.Signal.Editing then
          FAllowEditingSignalValueFromPM := not MarkerIsCloseToATransition(Data.Signal, X, EventNumber);

        UpdateListOfTransitionsFormWithSelectedSignal(Node);  
      end;
    end
  end;
end;


procedure TFrameWaveformsEditor.WaveformDrawingMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Node{, NodeOver}: PVirtualNode;
  Data: PSignalRec;
  EventNumber: Integer;
  NewMoment: Integer;
  tp: TPoint;
  AComp: TControl;
  ScrBarPos: Integer;
begin
  if ssLeft in Shift then
  begin
    //if X - vstWaveformsEditor.Header.Columns.Items[2].Left > 0 then
    if X > 0 then
    begin
      FMainMarkerPosXOnVst := X;
      //FMainMarkerPos := X - vstWaveformsEditor.Header.Columns.Items[2].Left;
      FMainMarkerPos := X;
      UpdateStatusBarPanel(2, X);
    end
    else
      FMainMarkerPos := 0;
    vstWaveformsEditor.Repaint;
  end;

  ScrBarPos := scrbarTimeWaveforms.Position;

  if not (ssRight in Shift) then
    if (Sender as TWaveformDrawing).Cursor <> crDefault then
      (Sender as TWaveformDrawing).Cursor := crDefault;

  Node := vstWaveformsEditor.GetFirstSelected;
  //NodeOver := vstWaveformsEditor.GetNodeAt(X, Y);
  if Assigned(Node) {and Assigned(NodeOver)} then
  begin
    Data := vstWaveformsEditor.GetNodeData(Node);
    EventNumber := FEditingEventNumber;
    if InIntervalInteger(X, 0, 0 + FWaveformDrawing.Width) and
       Data.Signal.Editing then
    begin
      if {(Node = NodeOver) and} MarkerIsCloseToATransition(Data.Signal, X, EventNumber) then
        (Sender as TWaveformDrawing).Cursor := crHSplit;

      if ssLeft in Shift then //dragging
        if Data.Signal.SignalEvents[EventNumber].EditingEvent then
        begin
          FAllowToDrawValidTransitions := True;
          NewMoment := XVstWaveToMoment(X, FZoom, ScrBarPos);
          UpdateSignalTransitionMoment(Data.Signal, EventNumber, NewMoment, ssShift in Shift);

          GetCursorPos(tp);
          frmMouseHint.Moment := NewMoment;
          frmMouseHint.Left := tp.X + 2;
          frmMouseHint.Top := tp.Y + 10;
          frmMouseHint.Visible := True; //frmMouseHint.Show;
          //SetFocus;
          AComp := TControl(Sender);
          while (AComp.Parent <> nil) and (AComp.Owner <> Application) do
          begin
            AComp := AComp.Parent;

            if AComp is TForm then
            begin
              (AComp as TForm).SetFocus;
              Break;
            end;
          end;
        end; //EditingEvent
    end;
  end;

  UpdateStatusBarPanel(3, X);

  if ssRight in Shift then
    if FMoveAllWaveforms then
    begin
      //FAllWaveformsMousePosition := X;
      //FScrbarWavePosOnWaveDrag := ScrBarPos;
      scrbarTimeWaveforms.Position := Round(FScrbarWavePosOnWaveDrag + (FAllWaveformsMousePosition - X) / FZoom);
      (Sender as TWaveformDrawing).Cursor := crSizeAll;
    end;
end;


procedure TFrameWaveformsEditor.WaveformDrawingMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  Node: PVirtualNode;
  Data: PSignalRec;
  ScrBarPos: Integer;
begin
  ClearAllEditingEventsInASignal(FEditingSignal);
  FEditingSignal := nil;
  if FMoveAllWaveforms then
  begin
    FMoveAllWaveforms := False;
    FWaveformDrawing.Cursor := crDefault;
  end;

  if FAllWaveformsMousePosition = X then //no drag
  begin
    ScrBarPos := scrbarTimeWaveforms.Position;

    if Y < 0 then
      Node := vstWaveformsEditor.GetFirstSelected
    else
      Node := vstWaveformsEditor.GetNodeAt(X, Y);

    if Assigned(Node) then
    begin
      Data := vstWaveformsEditor.GetNodeData(Node);

      if Assigned(FOnUpdatingPopupMenuItemsEnabledState) then
        FOnUpdatingPopupMenuItemsEnabledState(Data.Signal.Editing);

      XVstWaveToMomentExt(X, FZoom, ScrBarPos);
      FLastClickedMoment := Round(XVstWaveToMomentExt(X, FZoom, ScrBarPos));
      FLastClickedSignal := Data.Signal;
    end;

    GetCursorPos(tp);

    if Assigned(FOnMenuPopup) then
      FOnMenuPopup(tp);
  end;

  frmMouseHint.Hide;
  FAllowToDrawValidTransitions := False;
  FAllowEditingSignalValueFromPM := False;

  Node := vstWaveformsEditor.GetFirstSelected;
  if Assigned(Node) then
    FSelectedNode := Node.Index;

  //PaintAllWaveforms;
end;


procedure TFrameWaveformsEditor.scrbarTimeWaveformsChange(Sender: TObject);
begin
  PaintAllWaveforms;
end;


procedure TFrameWaveformsEditor.EditSelectedSignal;
var
  Node: PVirtualNode;
  Data: PSignalRec;
begin
  Node := vstWaveformsEditor.GetFirstSelected;
  if not Assigned(Node) then
  begin
    MessageBox(Handle, 'Please select a signal to edit.', PChar(Application.Title), MB_ICONWARNING);
    Exit;
  end;
  
  Data := vstWaveformsEditor.GetNodeData(Node);
  Data.Signal.Editing := True;
  vstWaveformsEditor.RepaintNode(Node);
end;


procedure TFrameWaveformsEditor.ExitEditingMode;
var
  Node: PVirtualNode;
  Data: PSignalRec;
begin
  Node := vstWaveformsEditor.GetFirstSelected;
  if not Assigned(Node) then
  begin
    MessageBox(Handle, 'Please select a signal to edit.', PChar(Application.Title), MB_ICONWARNING);
    Exit;
  end;
  
  Data := vstWaveformsEditor.GetNodeData(Node);
  Data.Signal.Editing := False;
  vstWaveformsEditor.RepaintNode(Node);
end;


procedure TFrameWaveformsEditor.HandleSpecialKeys(CtrlKeyDown, AltKeyDown: Boolean);
var
  WillPaint: Boolean;
begin
  WillPaint := False;

  if CtrlKeyDown and not FCtrlKeyPressed then
  begin
    FCtrlKeyPressed := True;
    FPaintTransitionsOnAllZoomLevels := True;
    WillPaint := True;
  end;

  if not CtrlKeyDown and FCtrlKeyPressed then
  begin
    FCtrlKeyPressed := False;

    if not FPaintTransitionsAsInserting then
      FPaintTransitionsOnAllZoomLevels := False;
      
    WillPaint := True;
  end;

  if AltKeyDown and not FAltKeyPressed and FEnabledPaintingTransitionsAsInserting then
  begin
    FAltKeyPressed := True;
    FPaintTransitionsOnAllZoomLevels := True;
    FPaintTransitionsAsInserting := True;
    WillPaint := True;
  end;

  if not AltKeyDown and FAltKeyPressed then
  begin
    FAltKeyPressed := False;

    if not FCtrlKeyPressed then
      FPaintTransitionsOnAllZoomLevels := False;

    FPaintTransitionsAsInserting := False;
    WillPaint := True;
  end;

  if WillPaint then
    PaintAllWaveforms;
end;


procedure TFrameWaveformsEditor.tmrCtrlKeyTimer(Sender: TObject);
//var
//  CtrlKeyDown, AltKeyDown: Boolean;
begin
  //CtrlKeyDown := GetAsyncKeyState(VK_CONTROL) < 0;     //these calls to GetAsyncKeyState may be viewed as malware by various antivirus softwares
  //AltKeyDown := GetAsyncKeyState(VK_MENU) < 0;

  //HandleSpecialKeys(CtrlKeyDown, AltKeyDown);
end;


procedure TFrameWaveformsEditor.tmrMouseSelectionTimer(Sender: TObject);
var
  Node: PVirtualNode;
begin
  tmrMouseSelection.Enabled := False;

  Node := vstWaveformsEditor.GetFirstSelected;
  if FMouseDownOnNode <> nil then
  begin
    if Assigned(Node) and Assigned(FMouseDownOnNode) then
    begin
      FSelectedNode := Node.Index;
      //vstHeaderWaves.Header.Columns.Items[0].Text := IntToStr(FSelectedNode);

      UpdateListOfTransitionsFormWithSelectedSignal(Node);
    end;
  end;
end;


procedure TFrameWaveformsEditor.tmrPaintWaveformsTimer(Sender: TObject);
begin
  tmrPaintWaveforms.Enabled := False;
  PaintAllWaveforms;
end;


procedure TFrameWaveformsEditor.RefreshOnColumnResize;
var
  LastColLeft: Integer;
  LastColWidth: Integer;
begin
  vstWaveformsEditor.Header.Columns.Items[0].Width := vstHeaderWaves.Header.Columns.Items[0].Width;
  vstWaveformsEditor.Header.Columns.Items[1].Width := vstHeaderWaves.Header.Columns.Items[1].Width;
  vstWaveformsEditor.Header.Columns.Items[3].Width := vstHeaderWaves.Header.Columns.Items[3].Width;

  LastColLeft := vstWaveformsEditor.Header.Columns.Items[2].Left;
  LastColWidth := vstHeaderWaves.Header.Columns.Items[2].Width;

  vstWaveformsEditor.Width := LastColLeft;
  FWaveformDrawing.Left := LastColLeft;
  FWaveformDrawing.Width := LastColWidth;
end;


procedure TFrameWaveformsEditor.vstHeaderWavesColumnResize(Sender: TVTHeader;
  Column: TColumnIndex);
begin
  RefreshOnColumnResize;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorAfterPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas);
begin
  tmrPaintWaveforms.Enabled := True;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorClick(Sender: TObject);
begin
  tmrPaintWaveforms.Enabled := True;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorColumnResize(
  Sender: TVTHeader; Column: TColumnIndex);
begin
  if Column = 0 then
    vstHeaderWaves.Header.Columns.Items[0].Width := vstWaveformsEditor.Header.Columns.Items[0].Width;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorDragAllowed(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var Allowed: Boolean);
begin
  Allowed := Column <> 2;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorDragDrop(
  Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
  Formats: TFormatArray; Shift: TShiftState; {$IFDEF FPC}const{$ENDIF} Pt: TPoint; var Effect: {$IFDEF FPC}LongWord{$ELSE}Integer{$ENDIF};
  Mode: TDropMode);
var
  Node: PVirtualNode;
begin
  if Source <> nil then
    if Sender.DropTargetNode <> nil then
    begin
      //(Source as TVirtualStringTree).MoveTo();
      TVirtualStringTree(Source).MoveTo(TVirtualStringTree(Source).FocusedNode, Sender.DropTargetNode, {amAddChildLast} amInsertBefore, False);
      Node := vstWaveformsEditor.GetFirstSelected;
      if Assigned(Node) then
        FSelectedNode := Node.Index;
      //TVirtualStringTree(Source).Expanded[Sender.DropTargetNode] := True;
    end
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorDragOver(
  Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; {$IFDEF FPC}const{$ENDIF} Pt: TPoint; Mode: TDropMode; var Effect: {$IFDEF FPC}LongWord{$ELSE}Integer{$ENDIF};
  var Accept: Boolean);
begin
  Accept := Sender = vstWaveformsEditor;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: {$IFDEF FPC}string{$ELSE}WideString{$ENDIF});

  function GetSignalTypeStr(ASignal: PSignal): string;
  begin
    Result := CValueTypeStr[ASignal^.ValueType];
    if (ASignal.ValueType = sutSTD_LOGIC) and (ASignal^.Size > 1) then
      Result := Result + '_VECTOR';
  end;

var
  Data: PSignalRec;
  SignalValue: TSignalValueType;
begin
  if not Assigned(Node) then
    Exit;

  Data := Sender.GetNodeData(Node);

  if not Assigned(Data) then
    Exit;

  case Column of   //Index: Signal  (Size)  [Type]
    0:
      CellText := IntToStr(Node^.Index) + ':  ' + Data.Signal^.SignalName + '  (' + IntToStr(Data.Signal^.Size) + ')   [' + GetSignalTypeStr(Data.Signal) + ']';

    1:
    begin
      SignalValue := SignalValueOnXVstWave(Data.Signal, FMainMarkerPosXOnVst);
      case Data.Signal.ValueType of
        sutNumeric: CellText := IntToStr(NumericSignalToInteger(SignalValue));
        sutSTD_LOGIC: CellText := ArrayOfTSTD_LOGIC_ToString(SignalValue.STD_LOGIC_Value);
        sutOrdinal: CellText := IntToStr(SignalValue.OrdinalValue);
        sutInt32: CellText := IntToStr(SignalValue.Int32Value);
        sutInt64: CellText := IntToStr(SignalValue.Int64Value);
      end;
    end;

    3:
      CellText := Data.Signal.SynchronizedByCircuit + '.' + Data.Signal.SynchronizedBySignal;

    else
      CellText := '';
  end;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  HandleSpecialKeys(ssCtrl in Shift, ssAlt in Shift);
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_NEXT, VK_PRIOR, VK_HOME, VK_END] then
  begin
    Node := vstWaveformsEditor.GetFirstSelected;
    if Assigned(Node) then
    begin
      FSelectedNode := Node.Index;
      UpdateListOfTransitionsFormWithSelectedSignal(Node);
    end;

    PaintAllWaveforms;
  end;

  HandleSpecialKeys(ssCtrl in Shift, ssAlt in Shift);
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownOnNode := vstWaveformsEditor.GetNodeAt(X, Y);
  tmrMouseSelection.Enabled := True;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
begin
  Node := vstWaveformsEditor.GetFirstSelected;
  if Assigned(Node) then
    FSelectedNode := Node.Index;
  PaintAllWaveforms;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Extended;
  VerticalScrolling: Boolean;
  KeyToSimulate: Word;
begin
  VerticalScrolling := True;

  if ssCtrl in Shift then
  begin
    if WheelDelta > 0 then
      Factor := WheelDelta div 48
    else
      Factor := 1 / (WheelDelta div 48);
      
    FZoom := FZoom * Abs(Factor);
    FStatusBar.Panels.Items[1].Text := 'Zoom level: ' + FloatToStr(FZoom);
    //scrbarTimeWaveforms.Max := //FZoom LastTransitionOnVST   scrbarTimeWaveforms.Position := ... accordingly

    vstWaveformsEditor.Repaint;
    VerticalScrolling := False;
  end;

  if ssShift in Shift then
  begin
    scrbarTimeWaveforms.Position := scrbarTimeWaveforms.Position - WheelDelta div 24;
    VerticalScrolling := False;
  end;

  if VerticalScrolling then
  begin
    if ssMiddle in Shift then
    begin
      if WheelDelta < 0 then
        KeyToSimulate := VK_DOWN
      else
        KeyToSimulate := VK_UP;

      SendMessage(vstWaveformsEditor.Handle, WM_KEYDOWN, KeyToSimulate, 0);
      Sleep(10);
      SendMessage(vstWaveformsEditor.Handle, WM_KEYUP, KeyToSimulate, 0);
    end
    else
      scrboxWaveforms.VertScrollBar.Position := scrboxWaveforms.VertScrollBar.Position - WheelDelta div 12;
  end;
end;


procedure TFrameWaveformsEditor.vstWaveformsEditorPaintText(
  Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Node.NodeHeight < CMinNodeHeight then
    Node.NodeHeight := CMinNodeHeight
end;


function GetScaledMeasurementUnit(var ATime: Extended): string;
begin
  Result := 'ns.';

  if ATime > 1000000000 then
  begin
    Result := 's.';
    ATime := ATime / 1000000000;
  end
  else
    if ATime > 1000000 then
    begin
      Result := 'ms.';
      ATime := ATime / 1000000;
    end
    else
      if ATime > 1000 then
      begin
        Result := 'us.';
        ATime := ATime / 1000;
      end;
end;


procedure TFrameWaveformsEditor.UpdateStatusBarPanel(APanelIndex: Integer; AWaveformXPoint: Integer = 0);
var
  ATime: Extended;
  MeasurementUnit, TimeStr: string;
begin
  if not Assigned(FStatusBar) then
    Exit;

  if APanelIndex in [2, 3] then
  begin
    ATime := XVstWaveToMomentExt(AWaveformXPoint, FZoom, scrbarTimeWaveforms.Position) * FSampleDuration;    //ns
    MeasurementUnit := GetScaledMeasurementUnit(ATime);
    TimeStr := FloatToStrF(ATime, ffFixed, 15, 7) + MeasurementUnit;
  end;

  case APanelIndex of
    1:
      FStatusBar.Panels.Items[1].Text := 'Zoom level: ' + FloatToStrF(FZoom, ffFixed, 15, 4);
      
    2:
      FStatusBar.Panels.Items[2].Text := 'Main cursor time: ' + TimeStr;

    3:
      FStatusBar.Panels.Items[3].Text := 'Mouse time: ' + TimeStr;

    4:
      FStatusBar.Panels.Items[4].Text := 'Max scroll: ' + IntToStr(scrbarTimeWaveforms.Max);

    else
      ;
  end;
end;


procedure TFrameWaveformsEditor.HandleOnPaintAllWaveforms;
begin
  PaintAllWaveforms;
end;


end.
