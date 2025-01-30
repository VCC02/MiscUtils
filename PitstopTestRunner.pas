{
    Copyright (C) 2025 VCC
    creation date: 30 Jan 2025
    initial release date: 30 Jan 2025

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


unit PitstopTestRunner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, VirtualTrees, fpcunit, ImgList;

type
  TTestStatus = (tsInit, tsFailed, tsPassed, tsRunning, tsPaused);

  TTestNodeRec = record
    Test: TTest;
    IsRootTest: Boolean;
    Status: TTestStatus;
    TestResult: string;
  end;
  PTestNodeRec = ^TTestNodeRec;

  { TfrmPitstopTestRunner }

  TfrmPitstopTestRunner = class(TForm)
    imglstTestStatus: TImageList;
    memLog: TMemo;
    memTestResult: TMemo;
    pnlToolbar: TPanel;
    spdbtnStop: TSpeedButton;
    spdbtnRunAll: TSpeedButton;
    spdbtnPause: TSpeedButton;
    spdbtnRunSelectedTest: TSpeedButton;
    tmrStartup: TTimer;
    vstTests: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure spdbtnPauseClick(Sender: TObject);
    procedure spdbtnRunAllClick(Sender: TObject);
    procedure spdbtnRunSelectedTestClick(Sender: TObject);
    procedure spdbtnStopClick(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstTestsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer);
    procedure vstTestsGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: boolean; var ImageIndex: integer;
      var ImageList: TCustomImageList);
    procedure vstTestsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FPaused: Boolean;
    FStopping: Boolean;

    procedure InitStatusOnAllTests;
    function RunTest(ANode: PVirtualNode): Boolean;
    function RunCategory(ACatNode: PVirtualNode): Boolean;
  public
    procedure AddToLog(s: string);
  end;

var
  frmPitstopTestRunner: TfrmPitstopTestRunner;

{ToDo:
- Get test result and display it when clicked on failed test.
- Implement checkboxes on test nodes.
- Load/Save window settings and test settings from/to ini.
- Add progressbar.
- Set "AllTests" category status to failed if at least one category fails.
- Measure test duration
- [nice to have] - report test results (xml or ini)
}

implementation

{$R *.frm}

uses
  testregistry;

{ TfrmPitstopTestRunner }

procedure TfrmPitstopTestRunner.FormCreate(Sender: TObject);
begin
  FPaused := False;
  FStopping := False;
  vstTests.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
  vstTests.NodeDataSize := SizeOf(TTestNodeRec);
  tmrStartup.Enabled := True;
end;


procedure TfrmPitstopTestRunner.spdbtnPauseClick(Sender: TObject);
begin
  FPaused := not FPaused;
  if FPaused then
  begin
    spdbtnPause.Enabled := False;
    spdbtnPause.Repaint;
  end;
end;


procedure TfrmPitstopTestRunner.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + ': ' + s);
end;


procedure TfrmPitstopTestRunner.spdbtnRunAllClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  vstTests.Selected[Node] := True;
  spdbtnRunSelectedTest.Click;
end;


procedure TfrmPitstopTestRunner.InitStatusOnAllTests;
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  vstTests.BeginUpdate;
  try
    repeat
      NodeData := vstTests.GetNodeData(Node);
      if NodeData <> nil then
        NodeData^.Status := tsInit;

      Node := vstTests.GetNext(Node);
    until Node = nil;
  finally
    vstTests.EndUpdate;
  end;
end;


function TfrmPitstopTestRunner.RunTest(ANode: PVirtualNode): Boolean; //ANode is expected to be a leaf
var
  NodeData: PTestNodeRec;
  TestRes: TTestResult;
begin
  Result := True;

  NodeData := vstTests.GetNodeData(ANode);
  NodeData^.Status := tsRunning;
  vstTests.Expanded[ANode] := True;
  vstTests.InvalidateNode(ANode);

  if FPaused then
  begin
    NodeData^.Status := tsPaused;
    vstTests.InvalidateNode(ANode);
    spdbtnPause.Enabled := True;
    spdbtnPause.Repaint;
    try
      repeat
        Application.ProcessMessages;
        Sleep(1);
      until not FPaused;
    finally
      NodeData^.Status := tsRunning;
      vstTests.InvalidateNode(ANode);
    end;
  end;

  NodeData^.TestResult := 'Not executed yet';

  if FStopping then
  begin
    NodeData^.Status := tsInit;
    vstTests.InvalidateNode(ANode);
    Exit;
  end;

  TestRes := TTestResult.Create;
  try
    NodeData^.Test.Run(TestRes);
  finally
    if TestRes.WasSuccessful then
    begin
      NodeData^.Status := tsPassed;
      NodeData^.TestResult := '';
    end
    else
    begin
      NodeData^.Status := tsFailed;
      NodeData^.TestResult := 'ToDo '; //TestRes.Errors.;  //TestRes.Errors.Count,  TestRes.Failures.Count,  TestRes.Listeners.
      Result := False;
    end;

    vstTests.InvalidateNode(ANode);
    TestRes.Free;
  end;
end;


function TfrmPitstopTestRunner.RunCategory(ACatNode: PVirtualNode): Boolean;
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  NodeData := vstTests.GetNodeData(ACatNode);
  NodeData^.Status := tsRunning;
  vstTests.InvalidateNode(ACatNode);
  Result := True;

  try
    Node := ACatNode^.FirstChild;
    if Node = nil then  //Leaf
    begin
      Result := RunTest(ACatNode);
      Exit;
    end;

    repeat
      vstTests.Expanded[Node] := True;
      vstTests.Repaint;
      Result := RunCategory(Node);

      if FStopping then
        Break;

      Application.ProcessMessages;
      Node := Node^.NextSibling;
    until Node = nil;
  finally
    NodeData := vstTests.GetNodeData(ACatNode);
    if Result then
      NodeData^.Status := tsPassed
    else
      NodeData^.Status := tsFailed;

    vstTests.InvalidateNode(ACatNode);
  end;
end;


procedure TfrmPitstopTestRunner.spdbtnRunSelectedTestClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  FStopping := False;

  Node := vstTests.GetFirstSelected;
  if Node = nil then
    AddToLog('No test selected.')
  else
  begin
    spdbtnRunAll.Enabled := False;
    spdbtnRunSelectedTest.Enabled := False;
    spdbtnPause.Enabled := True;
    spdbtnStop.Enabled := True;
    InitStatusOnAllTests;

    try
      vstTests.Expanded[Node] := True;
      vstTests.Repaint;
      RunCategory(Node);
    finally
      spdbtnRunAll.Enabled := True;
      spdbtnRunSelectedTest.Enabled := True;
      spdbtnPause.Enabled := False;
      spdbtnStop.Enabled := False;
    end;
  end;
end;


procedure TfrmPitstopTestRunner.spdbtnStopClick(Sender: TObject);
begin
  FPaused := False;
  FStopping := True;
  spdbtnStop.Enabled := False;
  spdbtnPause.Enabled := False;
end;


procedure TfrmPitstopTestRunner.tmrStartupTimer(Sender: TObject);
var
  AllTests: TTestSuite;
  i, j: Integer;
  AllTestsNode, CatNode, TestNode: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  tmrStartup.Enabled := False;
  AllTests := GetTestRegistry;

  vstTests.BeginUpdate;
  try
    AllTestsNode := vstTests.InsertNode(vstTests.RootNode, amInsertAfter);
    NodeData := vstTests.GetNodeData(AllTestsNode);
    NodeData^.Test := AllTests;
    NodeData^.IsRootTest := True;
    NodeData^.Status := tsInit;

    for i := 0 to AllTests.ChildTestCount - 1 do
    begin
      CatNode := vstTests.InsertNode(AllTestsNode, amAddChildLast);
      NodeData := vstTests.GetNodeData(CatNode);
      NodeData^.Test := AllTests.Test[i];
      NodeData^.IsRootTest := False;
      NodeData^.Status := tsInit;

      for j := 0 to AllTests.Test[i].CountTestCases - 1 do
      begin
        TestNode := vstTests.InsertNode(CatNode, amAddChildLast);
        NodeData := vstTests.GetNodeData(TestNode);
        NodeData^.Test := AllTests.Test[i].GetChildTest(j);
        NodeData^.IsRootTest := False;
        NodeData^.Status := tsInit;
      end;
    end;
  finally
    vstTests.EndUpdate;
  end;
end;


procedure TfrmPitstopTestRunner.vstTestsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: integer);
begin
  //
end;


procedure TfrmPitstopTestRunner.vstTestsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: integer;
  var ImageList: TCustomImageList);
var
  NodeData: PTestNodeRec;
begin
  NodeData := vstTests.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  ImageIndex := Ord(NodeData^.Status);
  ImageList := imglstTestStatus;
end;


procedure TfrmPitstopTestRunner.vstTestsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PTestNodeRec;
begin
  try
    NodeData := vstTests.GetNodeData(Node);
    if NodeData = nil then
      Exit;

    if NodeData^.IsRootTest then
      CellText := 'All Tests'
    else
      CellText := NodeData^.Test.TestName;
  except
    CellText := 'bug';
  end;
end;

end.

