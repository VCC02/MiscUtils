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
  ComCtrls, StdCtrls, ExtCtrls, Menus, VirtualTrees;

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
    procedure vstIdentifiersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstIdentifiersPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    FSelected: string;
    FSearchWord: string;
    FEdit: TCustomEdit;

    FListOfVars: TStringList;
    FListOfFuncs: TStringList;
    FListOfVarsDesc: TStringList;
    FListOfFuncsDesc: TStringList;

    vstIdentifiers: TVirtualStringTree;
    pnlDragCorner: TPanel;
    tmrDeactivate: TTimer;
    tmrStartup: TTimer;

    pmVST: TPopupMenu;
    MenuItem_CopySelectedItemToClipboard: TMenuItem;

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

    procedure MenuItem_CopySelectedItemToClipboardClick(Sender: TObject);

    procedure FormDeactivate(Sender: TObject);
    procedure tmrDeactivateTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);

    procedure SearchText;
    procedure CreateRemainingUIComponents;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure SetIdentifiersHint;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmAutoComplete: TfrmAutoComplete;

procedure ShowAutoComplete(AEdit: TCustomEdit; AListOfVars, AListOfFuncs, AListOfVarsDesc, AListOfFuncsDesc: TStrings);  //should be called once / CloseAutoComplete call
function CloseAutoComplete: string;
procedure UpdateAutoComplete(AEdit: TCustomEdit);
function AutoCompleteVisible: Boolean;


{ ToDo:
 - add only the missing string part, at Caret position
 - Highlight ASearchWord in list of identifiers
}


implementation


//{$R *.frm}


uses
  IniFiles, Clipbrd, Math;


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


procedure ShowAutoComplete(AEdit: TCustomEdit; AListOfVars, AListOfFuncs, AListOfVarsDesc, AListOfFuncsDesc: TStrings);
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
    if AListOfVars <> nil then
      frmAutoComplete.FListOfVars.AddStrings(AListOfVars);

    if AListOfFuncs <> nil then
      frmAutoComplete.FListOfFuncs.AddStrings(AListOfFuncs);

    if AListOfVarsDesc <> nil then
      frmAutoComplete.FListOfVarsDesc.AddStrings(AListOfVarsDesc);

    if AListOfFuncsDesc <> nil then
      frmAutoComplete.FListOfFuncsDesc.AddStrings(AListOfFuncsDesc);

    try
      frmAutoComplete.vstIdentifiers.RootNodeCount := AListOfVars.Count + AListOfFuncs.Count;
    except
    end;
  end;

  frmAutoComplete.SearchText;
  frmAutoComplete.BringToFront;

  if frmAutoComplete.Left + frmAutoComplete.Width > Screen.Width then
    frmAutoComplete.Left := EditRect.Left - frmAutoComplete.Width - 10;

  if frmAutoComplete.Top + frmAutoComplete.Width > Screen.Height then
    frmAutoComplete.Top := EditRect.Top - frmAutoComplete.Height - 15;

  if frmAutoComplete.Top < 0 then
    frmAutoComplete.Top := 0;

  try  //using try..except, because the editbox might be a dangling pointer if it is destroyed
    if AEdit.Visible then
      AEdit.SetFocus;
  except
  end;
end;


function CloseAutoComplete: string;
begin
  Result := '';

  if frmAutoComplete <> nil then
  begin
    Result := frmAutoComplete.FSelected;
    frmAutoComplete.Close;
  end;
end;


procedure UpdateAutoComplete(AEdit: TCustomEdit); //similar to ShowAutoComplete, without setting focus
begin
  frmAutoComplete.FSearchWord := ExtractSearchWord(AEdit.Text, AEdit.CaretPos.X);
  frmAutoComplete.FEdit := AEdit;
  frmAutoComplete.SearchText;
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
  FListOfVarsDesc := TStringList.Create;
  FListOfFuncsDesc := TStringList.Create;
  FListOfVars.LineBreak := #13#10;
  FListOfFuncs.LineBreak := #13#10;
  FListOfVarsDesc.LineBreak := #13#10;
  FListOfFuncsDesc.LineBreak := #13#10;
  FSelected := '';
  FHold := False;

  pmVST := TPopupMenu.Create(Self);
  MenuItem_CopySelectedItemToClipboard := TMenuItem.Create(Self);
  MenuItem_CopySelectedItemToClipboard.Caption := 'Copy selected item to clipboard';
  MenuItem_CopySelectedItemToClipboard.OnClick := MenuItem_CopySelectedItemToClipboardClick;
  pmVST.Items.Add(MenuItem_CopySelectedItemToClipboard);

  CreateRemainingUIComponents;
  tmrStartup.Enabled := True;
end;


destructor TfrmAutoComplete.Destroy;
begin
  FListOfVars.Free;
  FListOfFuncs.Free;

  try
    SaveSettings;
  except
  end;

  inherited Destroy;
end;


procedure TfrmAutoComplete.LoadSettings;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AutoComplete.ini'); //every application, which uses autocomplete will have this file
  try
    Width := Ini.ReadInteger('AutoComplete', 'Width', Width);
    Height := Ini.ReadInteger('AutoComplete', 'Height', Height);

    vstIdentifiers.ShowHint := True;
    SetIdentifiersHint;
  finally
    Ini.Free;
  end;
end;


procedure TfrmAutoComplete.SaveSettings;
var
  Ini: TMemIniFile;
  TempWidth, TempHeight: Integer;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AutoComplete.ini'); //every application, which uses autocomplete will have this file
  try
    TempWidth := Ini.ReadInteger('AutoComplete', 'Width', 50);
    TempHeight := Ini.ReadInteger('AutoComplete', 'Height', 50);

    if (TempWidth <> Width) or
       (TempHeight <> Height) then
    begin
      Ini.WriteInteger('AutoComplete', 'Width', Width);
      Ini.WriteInteger('AutoComplete', 'Height', Height);
      Ini.UpdateFile;  //save only if modified
    end;
  finally
    Ini.Free;
  end;
end;


procedure TfrmAutoComplete.FormDeactivate(Sender: TObject);
begin
  tmrDeactivate.Enabled := True;
end;


procedure TfrmAutoComplete.tmrDeactivateTimer(Sender: TObject);
var
  FEditFocused: Boolean;
begin
  tmrDeactivate.Enabled := False;

  try
    FEditFocused := FEdit.Focused;
  except
    FEditFocused := False; //if FEdit is not available
  end;

  if not FEditFocused then
    Close;
end;


procedure TfrmAutoComplete.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  LoadSettings;
  pnlDragCorner.Left := ClientWidth - pnlDragCorner.Width;
  pnlDragCorner.Top := ClientHeight - pnlDragCorner.Height;
end;


procedure TfrmAutoComplete.MenuItem_CopySelectedItemToClipboardClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstIdentifiers.GetFirstSelected;
  if Node = nil then
    Exit;

  if Integer(Node^.Index) < FListOfVars.Count then
    Clipboard.AsText := FListOfVars.Strings[Node^.Index] + '  ' + FListOfVarsDesc.Strings[Node^.Index]
  else
    Clipboard.AsText := FListOfFuncs.Strings[Node^.Index - FListOfVars.Count] + '  ' + FListOfFuncsDesc.Strings[Node^.Index - FListOfVars.Count];
end;


function NegPos(ASub, s: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (ASub = '') or (s = '') then
    Exit;

  if Length(ASub) > Length(s) then
    Exit;

  for i := Length(s) - Length(ASub) + 1 downto 1 do
    if s[i] = ASub[1] then
      if CompareMem(@s[i], @ASub[1], Length(ASub)) then
      begin
        Result := i;
        Exit;
      end;
end;


function CropToTheLeft(s: string): string;      //  crop to the left, until the beginning of the string or up to a ' '.
var
  LastBlankPos: Integer;
begin
  LastBlankPos := Max(NegPos(' ', s), NegPos('=', s));
  if LastBlankPos = Length(s) then // ' ' is the last character (useless)
    Result := s
  else
    Result := Copy(s, LastBlankPos + 1, MaxInt);
end;


function CropToTheLeft_AllSymbols(s: string): string;      //  crop to the left, until the beginning of the string or up to a ' ', '=', or '$'.
var
  LastBlankPos: Integer;
begin
  LastBlankPos := Max(Max(NegPos(' ', s), NegPos('=', s)), NegPos('$', s));
  if LastBlankPos = Length(s) then // ' ' is the last character (useless)
    Result := '' //s
  else
    Result := Copy(s, LastBlankPos + 1, MaxInt);
end;


procedure TfrmAutoComplete.SearchText;
var
  Node, FirstVisible: PVirtualNode;
  TempIsVisible: Boolean;
  UpperCaseSearchWord: string;
begin
  Node := vstIdentifiers.GetFirst;
  if Node = nil then
    Exit;

  FirstVisible := nil;
  FSearchWord := Trim(CropToTheLeft_AllSymbols(FSearchWord));

  UpperCaseSearchWord := UpperCase(FSearchWord);
  repeat
    TempIsVisible := (FSearchWord = '') or
    ((Integer(Node^.Index) < FListOfVars.Count) and (Pos(UpperCaseSearchWord, UpperCase(FListOfVars.Strings[Node^.Index])) > 0)) or
    ((Integer(Node^.Index) >= FListOfVars.Count) and (Pos(UpperCaseSearchWord, UpperCase(FListOfFuncs.Strings[Node^.Index - FListOfVars.Count])) > 0));

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

    2:
    begin
      try
        if Integer(Node^.Index) < FListOfVars.Count then
          CellText := FListOfVarsDesc.Strings[Node^.Index]
        else
          CellText := FListOfFuncsDesc.Strings[Node^.Index - FListOfVars.Count];
      except
        CellText := '*** indexing bug ***';
      end;
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
var
  LastPart: string;
begin
  try
    if FSelected <> '' then
    begin
      if FEdit.Text = '' then
        FEdit.Text := FSelected
      else
      begin
        LastPart := CropToTheLeft(FEdit.Text);   //everything after ' ', '$' or '='

        if Pos(LastPart, FSelected) = 1 then
          FEdit.Text := FEdit.Text + Copy(FSelected, Length(LastPart) + 1, MaxInt)
        else
          if (Pos('$', LastPart) = 0) or (Pos(LastPart, Copy(FSelected, 1, Pos('=', FSelected))) = 1) then
          begin
            FEdit.Text := Copy(FEdit.Text, 1, Pos(LastPart, FEdit.Text) - 1);
            FEdit.Text := FEdit.Text + FSelected;       //the LastPart is deleted from FEdit.Text, then completly replaced by FSelected
          end
          else
            FEdit.Text := FEdit.Text + FSelected;     //e.g. FEdit.Text = '$Desktop_Width$=1920',  FSelected = '$Screen_Width$=1920'
      end;

      FEdit.SelStart := Length(FEdit.Text);
      FEdit.SelLength := 1;
    end;
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
  vstIdentifiers.Caption := 'VST with list of variables and functions'; //used by UI tests, to identify the component
  vstIdentifiers.Colors.UnfocusedColor := clMedGray;
  vstIdentifiers.DefaultNodeHeight := 16;
  vstIdentifiers.DefaultText := 'Node';
  vstIdentifiers.Font.Height := -13;
  vstIdentifiers.Font.Name := 'Courier New';
  vstIdentifiers.Header.AutoSizeIndex := 0;
  vstIdentifiers.Header.DefaultHeight := 17;
  vstIdentifiers.Indent := 4;
  vstIdentifiers.ParentFont := False;
  vstIdentifiers.PopupMenu := pmVST;
  vstIdentifiers.TabOrder := 0;
  vstIdentifiers.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages];
  vstIdentifiers.TreeOptions.SelectionOptions := [toFullRowSelect];
  vstIdentifiers.ScrollBarOptions.AlwaysVisible := True;
  vstIdentifiers.OnDblClick := vstIdentifiersDblClick;
  vstIdentifiers.OnDrawText := vstIdentifiersDrawText;
  vstIdentifiers.OnGetText := vstIdentifiersGetText;
  vstIdentifiers.OnPaintText := vstIdentifiersPaintText;
  vstIdentifiers.OnKeyDown := vstIdentifiersKeyDown;
  vstIdentifiers.OnKeyUp := vstIdentifiersKeyUp;
  vstIdentifiers.OnMouseUp := vstIdentifiersMouseUp;

  NewColum := vstIdentifiers.Header.Columns.Add;
  NewColum.MinWidth := 50;
  NewColum.Position := 0;
  NewColum.Width := 50;
  NewColum.Text := 'Type';

  NewColum := vstIdentifiers.Header.Columns.Add;
  NewColum.MinWidth := 390;
  NewColum.Position := 1;
  NewColum.Width := 390;
  NewColum.Text := 'Definition';

  NewColum := vstIdentifiers.Header.Columns.Add;
  NewColum.MinWidth := 8500;
  NewColum.Position := 2;
  NewColum.Width := 10500;
  NewColum.Text := 'Description';

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


procedure TfrmAutoComplete.SetIdentifiersHint;
var
  Node: PVirtualNode;
begin
  vstIdentifiers.Hint := 'Width: ' + IntToStr(Width) + '  Height: ' + IntToStr(Height);

  Node := vstIdentifiers.GetFirstSelected;
  if Node = nil then
    Exit;

  if Integer(Node^.Index) < FListOfVars.Count then
    vstIdentifiers.Hint := vstIdentifiers.Hint + #13#10#13#10 + FListOfVars.Strings[Node^.Index] + #13#10 + FListOfVarsDesc.Strings[Node^.Index]
  else
    vstIdentifiers.Hint := vstIdentifiers.Hint + #13#10#13#10 + FListOfFuncs.Strings[Node^.Index - FListOfVars.Count] + #13#10 + FListOfFuncsDesc.Strings[Node^.Index - FListOfVars.Count];
end;


procedure TfrmAutoComplete.vstIdentifiersDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const AText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
end;


procedure TfrmAutoComplete.vstIdentifiersKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  //
end;


procedure TfrmAutoComplete.vstIdentifiersKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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
    begin
      try
        if Assigned(FEdit) then
          FEdit.SetFocus;
      except
        //the editbox might not be available
      end;
    end;

  if Key in [VK_HOME, VK_END, VK_PRIOR, VK_NEXT, VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN] then
    SetIdentifiersHint;
end;


procedure TfrmAutoComplete.vstIdentifiersMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetIdentifiersHint;
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

