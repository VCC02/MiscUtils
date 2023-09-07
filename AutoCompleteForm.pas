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

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  VirtualTrees;

type

  { TfrmAutoComplete }

  TfrmAutoComplete = class(TForm)
    StatusBar1: TStatusBar;
    vstIdentifiers: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure vstIdentifiersDblClick(Sender: TObject);
    procedure vstIdentifiersDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const AText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure vstIdentifiersExit(Sender: TObject);
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

    procedure SearchText;
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
    Application.CreateForm(TfrmAutoComplete, frmAutoComplete);
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

  if CreatedHere then
  begin
    frmAutoComplete.FListOfVars.AddStrings(AListOfVars);
    frmAutoComplete.FListOfFuncs.AddStrings(AListOfFuncs);
    frmAutoComplete.vstIdentifiers.RootNodeCount := AListOfVars.Count + AListOfFuncs.Count;
  end;

  if not frmAutoComplete.Visible then
  begin
    frmAutoComplete.Show;
    frmAutoComplete.vstIdentifiers.SetFocus;
    frmAutoComplete.FSelected := '';
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
end;


destructor TfrmAutoComplete.Destroy;
begin
  FListOfVars.Free;
  FListOfFuncs.Free;
  inherited Destroy;
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


procedure TfrmAutoComplete.vstIdentifiersExit(Sender: TObject);
begin
  Close;
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


initialization
  frmAutoComplete := nil;

end.

