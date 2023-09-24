{
    Copyright (C) 2023 VCC
    creation date: Sep 2023 (06)
    initial release date: 07 Sep 2023

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


unit AutoCompleteForm;

{$mode Delphi}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, VirtualTrees;

type

  { TfrmAutoComplete }

  //TfrmAutoComplete = class(TForm)
  TfrmAutoComplete = class(THintWindow)
    lblSizeInfo: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure vstIdentifiersDblClick(Sender: TObject);
    procedure vstIdentifiersDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const AText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstIdentifiersGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure vstIdentifiersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstIdentifiersKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstIdentifiersPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FSelected: string;
    FSearchWord: string;
    FEdit: TCustomEdit;

    FListOfVars: TStringList;
    FListOfFuncs: TStringList;

    vstIdentifiers: TVirtualStringTree;
    pnlDragCorner: TPanel;
    tmrDeactivate: TTimer;
    tmrStartup: TTimer;

    FHold: Boolean;
    FInitialWidth: Integer;
    FInitialHeight: Integer;
    FInitialX: Integer;
    FInitialY: Integer;

    procedure pnlDragCornerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlDragCornerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlDragCornerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure FormDeactivate(Sender: TObject);
    procedure tmrDeactivateTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);

    procedure SearchText;
    procedure CreateRemainingUIComponents;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmAutoComplete: TfrmAutoComplete;

procedure ShowAutoComplete(AEdit: TCustomEdit; AListOfVars, AListOfFuncs: TStrings);  //should be called once / CloseAutoComplete call
function CloseAutoComplete: string;
function AutoCompleteVisible: Boolean;


{ ToDo:
 - add only the missing string part, at Caret position
 - Highlight ASearchWord in list of identifiers
 - Find a way to detect defocusing
}


implementation

{$R *.frm}


function ExtractSearchWord(AStr: string; ACurrentCaretPos: Integer): string;
var
  i: Integer;
begin
  AStr := Copy(AStr, 1, ACurrentCaretPos);
  Result := AStr;

  for i := ACurrentCaretPos - 1 downto 1 do
    if not (AStr[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) then
    begin
      Result := Copy(AStr, i + 1, MaxInt);
      Break;
    end;
end;


procedure ShowAutoComplete(AEdit: TCustomEdit; AListOfVars, AListOfFuncs: TStrings);
var
  CreatedHere: Boolean;
  EditRect: TRect;
begin
  CreatedHere := False;
  if frmAutoComplete = nil then
  begin
    //Application.CreateForm(TfrmAutoComplete, frmAutoComplete);
    frmAutoComplete := TfrmAutoComplete.Create(Application.MainForm);
    frmAutoComplete.Width := 416;
    frmAutoComplete.Height := 313;
    frmAutoComplete.FormStyle := fsSystemStayOnTop;
    frmAutoComplete.BorderStyle := bsNone;
    frmAutoComplete.Constraints.MinWidth := frmAutoComplete.Width;
    frmAutoComplete.Constraints.MinHeight := frmAutoComplete.Height;

    frmAutoComplete.OnCreate := frmAutoComplete.FormCreate;
    frmAutoComplete.OnClose := frmAutoComplete.FormClose;
    frmAutoComplete.OnDeactivate := frmAutoComplete.FormDeactivate;

    CreatedHere := True;
  end;

  GetWindowRect(AEdit.Handle, EditRect);

  frmAutoComplete.Left := EditRect.Left + 10;
  frmAutoComplete.Top := EditRect.Bottom + 15;

  if frmAutoComplete.BoundsRect.Bottom > Screen.Height then
    frmAutoComplete.Top := EditRect.Top - frmAutoComplete.Height - 20;

  if frmAutoComplete.Top < 40 then
    frmAutoComplete.Top := 40;

  frmAutoComplete.FSearchWord := ExtractSearchWord(AEdit.Text, AEdit.CaretPos.X);
  frmAutoComplete.FEdit := AEdit;

  if not frmAutoComplete.Visible then
  begin
    frmAutoComplete.Show;
    frmAutoComplete.vstIdentifiers.SetFocus;
    frmAutoComplete.FSelected := '';
  end;

  if CreatedHere then
  begin
    frmAutoComplete.FListOfVars.AddStrings(AListOfVars);
    frmAutoComplete.FListOfFuncs.AddStrings(AListOfFuncs);
    try
      frmAutoComplete.vstIdentifiers.RootNodeCount := AListOfVars.Count + AListOfFuncs.Count;
    except
    end;
  end;

  frmAutoComplete.SearchText;
  frmAutoComplete.BringToFront;
  AEdit.SetFocus;
end;


function CloseAutoComplete: string;
begin
  if frmAutoComplete <> nil then
  begin
    Result := frmAutoComplete.FSelected;
    frmAutoComplete.Close;
  end;
end;


function AutoCompleteVisible: Boolean;
begin
  Result := Assigned(frmAutoComplete) and frmAutoComplete.Visible;
end;


constructor TfrmAutoComplete.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FListOfVars := TStringList.Create;
  FListOfFuncs := TStringList.Create;
  FSelected := '';
  FHold := False;

  CreateRemainingUIComponents;
  tmrStartup.Enabled := True;
end;


destructor TfrmAutoComplete.Destroy;
begin
  FListOfVars.Free;
  FListOfFuncs.Free;
  inherited Destroy;
end;


procedure TfrmAutoComplete.FormDeactivate(Sender: TObject);
begin
  tmrDeactivate.Enabled := True;
end;


procedure TfrmAutoComplete.tmrDeactivateTimer(Sender: TObject);
begin
  tmrDeactivate.Enabled := False;
  if not FEdit.Focused then
    Close;
end;


procedure TfrmAutoComplete.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  pnlDragCorner.Left := ClientWidth - pnlDragCorner.Width;
  pnlDragCorner.Top := ClientHeight - pnlDragCorner.Height;
end;


procedure TfrmAutoComplete.SearchText;
var
  Node, FirstVisible: PVirtualNode;
  TempIsVisible: Boolean;
begin
  Node := vstIdentifiers.GetFirst;
  if Node = nil then
    Exit;

  FirstVisible := nil;
  FSearchWord := Trim(FSearchWord);
  repeat
    TempIsVisible := (FSearchWord = '') or
    ((Integer(Node^.Index) < FListOfVars.Count) and (Pos(FSearchWord, FListOfVars.Strings[Node^.Index]) > 0)) or
    ((Integer(Node^.Index) >= FListOfVars.Count) and (Pos(FSearchWord, FListOfFuncs.Strings[Node^.Index - FListOfVars.Count]) > 0));

    vstIdentifiers.IsVisible[Node] := TempIsVisible;
    if TempIsVisible and (FirstVisible = nil) then
      FirstVisible := Node;

    Node := Node^.NextSibling;
  until Node = nil;

  if FirstVisible <> nil then //there is at least one visible node
    if vstIdentifiers.VisibleCount > 0 then
      vstIdentifiers.Selected[FirstVisible] := True;
end;


procedure TfrmAutoComplete.vstIdentifiersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  case Column of
    0:
    begin
      if Integer(Node^.Index) < FListOfVars.Count then
        CellText := 'Var'
      else
        CellText := 'Func';
    end;

    1:
    begin
      if Integer(Node^.Index) < FListOfVars.Count then
        CellText := FListOfVars.Strings[Node^.Index]
      else
        CellText := FListOfFuncs.Strings[Node^.Index - FListOfVars.Count];
    end;
  end;
end;


procedure TfrmAutoComplete.vstIdentifiersDblClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  FSelected := '';

  Node := vstIdentifiers.GetFirstSelected;
  if Node <> nil then
  begin
    if Integer(Node^.Index) < FListOfVars.Count then
      FSelected := FListOfVars.Strings[Node^.Index]
    else
      FSelected := FListOfFuncs.Strings[Node^.Index - FListOfVars.Count];
  end;

  Close;
end;


procedure TfrmAutoComplete.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try                         //////////////////////////////////////////////// ToDo: add only the missing part, at Caret position
    if FSelected <> '' then
      FEdit.Text := FEdit.Text + FSelected;
  except
    //the editbox might not be available
  end;
end;


procedure TfrmAutoComplete.CreateRemainingUIComponents;
var
  NewColum: TVirtualTreeColumn;
begin
  vstIdentifiers := TVirtualStringTree.Create(Self);
  vstIdentifiers.Parent := Self;

  vstIdentifiers.Left := 0;
  vstIdentifiers.Top := 0;
  vstIdentifiers.Height := Height {- 20};
  vstIdentifiers.Width := Width;
  vstIdentifiers.Anchors := [akTop, akLeft, akRight, akBottom];
  vstIdentifiers.Colors.UnfocusedColor := clMedGray;
  vstIdentifiers.DefaultNodeHeight := 16;
  vstIdentifiers.DefaultText := 'Node';
  vstIdentifiers.Font.Height := -13;
  vstIdentifiers.Font.Name := 'Courier New';
  vstIdentifiers.Header.AutoSizeIndex := 0;
  vstIdentifiers.Header.DefaultHeight := 17;
  vstIdentifiers.Indent := 4;
  vstIdentifiers.ParentFont := False;
  vstIdentifiers.TabOrder := 0;
  vstIdentifiers.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  vstIdentifiers.TreeOptions.SelectionOptions := [toFullRowSelect];
  vstIdentifiers.ScrollBarOptions.AlwaysVisible := True;
  vstIdentifiers.OnDblClick := vstIdentifiersDblClick;
  vstIdentifiers.OnDrawText := vstIdentifiersDrawText;
  vstIdentifiers.OnGetText := vstIdentifiersGetText;
  vstIdentifiers.OnPaintText := vstIdentifiersPaintText;
  vstIdentifiers.OnKeyDown := vstIdentifiersKeyDown;
  vstIdentifiers.OnKeyUp := vstIdentifiersKeyUp;;

  NewColum := vstIdentifiers.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 0;
  NewColum.Width := 50;
  NewColum.Text := 'Type';

  NewColum := vstIdentifiers.Header.Columns.Add;
  NewColum.MinWidth := 500;
  NewColum.Position := 1;
  NewColum.Width := 500;
  NewColum.Text := 'Definition';

  pnlDragCorner := TPanel.Create(Self);
  pnlDragCorner.Parent := Self;
  pnlDragCorner.Width := 19;
  pnlDragCorner.Height := 19;
  pnlDragCorner.Left := ClientWidth - pnlDragCorner.Width;
  pnlDragCorner.Top := ClientHeight - pnlDragCorner.Height;
  pnlDragCorner.Cursor := crSizeNWSE;
  pnlDragCorner.Font.Style := [fsBold, fsItalic];
  pnlDragCorner.Font.Color := clGray;
  pnlDragCorner.Caption := '/.';
  pnlDragCorner.Anchors := [akLeft, akTop];  //yes, left and top
  pnlDragCorner.OnMouseDown := pnlDragCornerMouseDown;
  pnlDragCorner.OnMouseMove := pnlDragCornerMouseMove;
  pnlDragCorner.OnMouseUp := pnlDragCornerMouseUp;
  pnlDragCorner.BringToFront;

  tmrDeactivate := TTimer.Create(Self);
  tmrDeactivate.Interval := 100;
  tmrDeactivate.OnTimer := tmrDeactivateTimer;
  tmrDeactivate.Enabled := False;

  tmrStartup := TTimer.Create(Self);
  tmrStartup.Interval := 100;
  tmrStartup.OnTimer := tmrStartupTimer;
  tmrStartup.Enabled := False;             //Enabled later
end;


procedure TfrmAutoComplete.FormCreate(Sender: TObject);
begin
  //does not execute on THintWindow, unless manually assigned
end;

procedure TfrmAutoComplete.FormHide(Sender: TObject);
begin
  //does not execute on THintWindow, unless manually assigned
end;


procedure TfrmAutoComplete.vstIdentifiersDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const AText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;


procedure TfrmAutoComplete.vstIdentifiersKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  if Key in [VK_RETURN, VK_SPACE] then
  begin
    Node := vstIdentifiers.GetFirstSelected;

    if Node <> nil then
    begin
      if Integer(Node^.Index) < FListOfVars.Count then
        FSelected := FListOfVars.Strings[Node^.Index]
      else
        FSelected := FListOfFuncs.Strings[Node^.Index - FListOfVars.Count];
    end;
  end;

  if Key in [VK_ESCAPE, VK_RETURN, VK_SPACE] then
    Close
  else
    if not (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR]) then
      FEdit.SetFocus;
end;


procedure TfrmAutoComplete.vstIdentifiersKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //
end;


procedure TfrmAutoComplete.vstIdentifiersPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Column = 0 then
  begin
    if Integer(Node^.Index) < FListOfVars.Count then
    begin  //vars
      if vstIdentifiers.Focused then
      begin
        if not vstIdentifiers.Selected[Node] then
          TargetCanvas.Font.Color := clMaroon
        else
          TargetCanvas.Font.Color := $00CCCCFF;
      end
      else
        TargetCanvas.Font.Color := clMaroon;
    end
    else
    begin  //functions
      if vstIdentifiers.Focused then
      begin
        if not vstIdentifiers.Selected[Node] then
          TargetCanvas.Font.Color := clGreen
        else
          TargetCanvas.Font.Color := $00CCFFC;
      end
      else
        TargetCanvas.Font.Color := clGreen;
    end;
  end;
end;


procedure TfrmAutoComplete.pnlDragCornerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    FHold := True;
    FInitialWidth := Width;
    FInitialHeight := Height;
    FInitialX := X;
    FInitialY := Y;
    pnlDragCorner.Hide;
  end;
end;


procedure TfrmAutoComplete.pnlDragCornerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FHold then
  begin
    Width := FInitialWidth + X - FInitialX;
    Height := FInitialHeight + Y - FInitialY;
  end;
end;


procedure TfrmAutoComplete.pnlDragCornerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
  pnlDragCorner.Left := ClientWidth - pnlDragCorner.Width;
  pnlDragCorner.Top := ClientHeight - pnlDragCorner.Height;
  pnlDragCorner.Show;
end;


initialization
  frmAutoComplete := nil;

end.

