{
    Copyright (C) 2024 VCC
    creation date: 24 May 2024
    initial release date: 25 May 2024

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


unit InMemFileSystemBrowserForm;

{$mode Delphi}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  VirtualTrees, InMemFileSystem;

type

  { TfrmInMemFileSystemBrowser }

  TfrmInMemFileSystemBrowser = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    imgPreview: TImage;
    pnlHorizSplitterResults: TPanel;
    pnlImg: TPanel;
    pnlToolbar: TPanel;
    pnlPreview: TPanel;
    pnlFileList: TPanel;
    scrboxPreview: TScrollBox;
    tmrStartup: TTimer;
    vstFiles: TVirtualStringTree;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pnlHorizSplitterResultsMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterResultsMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHorizSplitterResultsMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstFilesClick(Sender: TObject);
    procedure vstFilesDblClick(Sender: TObject);
    procedure vstFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FInMemFS: TInMemFileSystem;
    FCachedListOfFiles: TMemFileArr;

    FHoldResults: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    procedure PreviewSelectedFile(AIndex: Integer); overload;
    procedure PreviewSelectedFile; overload;
    procedure ResizeFrameSectionsBySplitterResults(NewLeft: Integer);

    procedure scrboxPreviewMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public

  end;


function BrowseInMemFSFile(AInMemFS: TInMemFileSystem): string;


implementation

{$R *.frm}


function BrowseInMemFSFile(AInMemFS: TInMemFileSystem): string;
var
  frmInMemFileSystemBrowser: TfrmInMemFileSystemBrowser;
  Node: PVirtualNode;
  AllFiles: TMemFileArr;
begin
  Result := '';
  Application.CreateForm(TfrmInMemFileSystemBrowser, frmInMemFileSystemBrowser);
  frmInMemFileSystemBrowser.FInMemFS := AInMemFS;
  AInMemFS.ListMemFiles(frmInMemFileSystemBrowser.FCachedListOfFiles);
  frmInMemFileSystemBrowser.vstFiles.RootNodeCount := Length(frmInMemFileSystemBrowser.FCachedListOfFiles);

  frmInMemFileSystemBrowser.ShowModal;

  Node := frmInMemFileSystemBrowser.vstFiles.GetFirstSelected;

  if (frmInMemFileSystemBrowser.Tag = 1) and (Node <> nil) then
  begin
    AInMemFS.ListMemFiles(AllFiles);
    try
      Result := AllFiles[Node^.Index].Name;
    finally
      SetLength(AllFiles, 0);
    end;
  end;
end;


{ TfrmInMemFileSystemBrowser }

procedure TfrmInMemFileSystemBrowser.vstFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  case Column of
    0:
      CellText := FCachedListOfFiles[Node^.Index].Name;

    1:
      CellText := IntToStr(FCachedListOfFiles[Node^.Index].Size);

    2:
      CellText := FCachedListOfFiles[Node^.Index].Hash;
  end;
end;


procedure TfrmInMemFileSystemBrowser.vstFilesKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Tag := 0;
    Close;
  end;
end;


procedure TfrmInMemFileSystemBrowser.ResizeFrameSectionsBySplitterResults(NewLeft: Integer);
begin
  if NewLeft < pnlFileList.Constraints.MinWidth then
    NewLeft := pnlFileList.Constraints.MinWidth;

  if NewLeft > Width - 320 then
    NewLeft := Width - 320;

  pnlHorizSplitterResults.Left := NewLeft;

  pnlPreview.Left := pnlHorizSplitterResults.Left + pnlHorizSplitterResults.Width;
  pnlPreview.Width := Self.Width - pnlPreview.Left;
  pnlFileList.Width := pnlHorizSplitterResults.Left;
end;


procedure TfrmInMemFileSystemBrowser.FormCreate(Sender: TObject);
begin
  SetLength(FCachedListOfFiles, 0);
  FHoldResults := False;
  tmrStartup.Enabled := True;
end;


procedure TfrmInMemFileSystemBrowser.FormResize(Sender: TObject);
var
  NewLeft: Integer;
begin
  NewLeft := pnlHorizSplitterResults.Left;
  ResizeFrameSectionsBySplitterResults(NewLeft);
end;


procedure TfrmInMemFileSystemBrowser.pnlHorizSplitterResultsMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldResults then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlHorizSplitterResults.Left;
    FHoldResults := True;
  end;
end;


procedure TfrmInMemFileSystemBrowser.pnlHorizSplitterResultsMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHoldResults then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitterResults(NewLeft);
end;


procedure TfrmInMemFileSystemBrowser.pnlHorizSplitterResultsMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FHoldResults := False;
end;


procedure TfrmInMemFileSystemBrowser.tmrStartupTimer(Sender: TObject);
var
  tp: TPoint;
begin
  tmrStartup.Enabled := False;
  scrboxPreview.OnMouseWheel := scrboxPreviewMouseWheel;

  if vstFiles.RootNodeCount > 0 then
  begin
    vstFiles.Selected[vstFiles.GetFirst] := True;
    PreviewSelectedFile;
  end;

  GetCursorPos(tp);
  Left := tp.X;
  Top := tp.Y;

  if Left + 100 < Screen.Width then
    Left := Left - 100;

  if Top + 100 < Screen.Height then
    Top := Top - 100;

  vstFiles.SetFocus;
end;


procedure TfrmInMemFileSystemBrowser.PreviewSelectedFile(AIndex: Integer);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    MemStream.SetSize(FCachedListOfFiles[AIndex].Size);
    FInMemFS.LoadFileFromMem(FCachedListOfFiles[AIndex].Name, MemStream.Memory, AIndex);

    MemStream.Position := 0;
    imgPreview.Picture.Bitmap.LoadFromStream(MemStream);
    imgPreview.Width := imgPreview.Picture.Bitmap.Width;
    imgPreview.Height := imgPreview.Picture.Bitmap.Height;
    pnlImg.Width := imgPreview.Width;
    pnlImg.Height := imgPreview.Height;
  finally
    MemStream.Free;
  end;
end;


procedure TfrmInMemFileSystemBrowser.PreviewSelectedFile;
var
  Node: PVirtualNode;
begin
  Node := vstFiles.GetFirstSelected;
  if Node = nil then
  begin
    imgPreview.Picture.Clear;
    Exit;
  end;

  PreviewSelectedFile(Node^.Index);
end;


procedure TfrmInMemFileSystemBrowser.vstFilesClick(Sender: TObject);
begin
  PreviewSelectedFile;
end;


procedure TfrmInMemFileSystemBrowser.vstFilesDblClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmInMemFileSystemBrowser.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmInMemFileSystemBrowser.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;


procedure TfrmInMemFileSystemBrowser.scrboxPreviewMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Factor: Integer;
begin
  if ssCtrl in Shift then
    Factor := 1
  else
    Factor := 3;

  if ssShift in Shift then
    scrboxPreview.HorzScrollBar.Position := scrboxPreview.HorzScrollBar.Position - WheelDelta div Factor
  else
    scrboxPreview.VertScrollBar.Position := scrboxPreview.VertScrollBar.Position - WheelDelta div Factor;

  Handled := True;
end;

end.

