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
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, VirtualTrees, fpcunit, ImgList, ComCtrls;

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

  TfrmPitstopTestRunner = class(TForm, ITestListener)
    imglstTestStatus: TImageList;
    memLog: TMemo;
    memTestResult: TMemo;
    pnlToolbar: TPanel;
    prbTestProgress: TProgressBar;
    spdbtnRunAllSelectedTests: TSpeedButton;
    spdbtnStop: TSpeedButton;
    spdbtnRunAll: TSpeedButton;
    spdbtnPause: TSpeedButton;
    spdbtnRunSelectedTest: TSpeedButton;
    tmrStartup: TTimer;
    vstTests: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure spdbtnPauseClick(Sender: TObject);
    procedure spdbtnRunAllClick(Sender: TObject);
    procedure spdbtnRunAllSelectedTestsClick(Sender: TObject);
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
    procedure vstTestsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstTestsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FPaused: Boolean;
    FStopping: Boolean;

    function GetIniNameNoExt: string;
    procedure GetListOfSelectedTests(AList: TStringList);         //selected doesn't mean checked
    procedure GetSelectedNodes(var ASelectedNodes: TNodeArray);  //selected doesn't mean checked
    procedure SelectTestsByName(AList: TStringList);
    function GetNodeByTest(ATest: TTest): PVirtualNode;
    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;
    procedure DisplayTestResultFromSelectedTest;

    procedure InitStatusOnAllTests;
    function RunTest(ANode: PVirtualNode): Boolean;
    function RunCategory(ACatNode: PVirtualNode): Boolean;

    procedure AddFailure(ATest: TTest; AFailure: TTestFailure);
    procedure AddError(ATest: TTest; AError: TTestFailure);
    procedure StartTest(ATest: TTest);
    procedure EndTest(ATest: TTest);
    procedure StartTestSuite(ATestSuite: TTestSuite);
    procedure EndTestSuite(ATestSuite: TTestSuite);
  public
    procedure AddToLog(s: string);
  end;

var
  frmPitstopTestRunner: TfrmPitstopTestRunner;

{ToDo:
- Implement checkboxes on test nodes.
- Load/Save window settings and test settings from/to ini.
- Set "AllTests" category status to failed if at least one category fails.
- Measure test duration
- [nice to have] - report test results (xml or ini)
}

implementation

{$R *.frm}

uses
  testregistry, IniFiles;

{ TfrmPitstopTestRunner }

procedure TfrmPitstopTestRunner.FormCreate(Sender: TObject);
begin
  FPaused := False;
  FStopping := False;
  vstTests.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
  vstTests.NodeDataSize := SizeOf(TTestNodeRec);
  tmrStartup.Enabled := True;
end;


procedure TfrmPitstopTestRunner.FormDestroy(Sender: TObject);
begin
  try
    SaveSettingsToIni;
  except
  end;
end;


function TfrmPitstopTestRunner.GetIniNameNoExt: string;
var
  Ext: string;
begin
  Result := ExtractFileName(ParamStr(0));
  Ext := ExtractFileExt(Result);
  if Ext <> '' then
    Delete(Result, Length(Result) - Length(Ext) + 1, Length(Ext));

  Result := ExtractFilePath(ParamStr(0)) + Result + '.fpcunit.ini';
end;


procedure TfrmPitstopTestRunner.GetListOfSelectedTests(AList: TStringList);
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  AList.Clear;

  vstTests.BeginUpdate;
  try
    repeat
      if vstTests.Selected[Node] then
      begin
        NodeData := vstTests.GetNodeData(Node);
        if NodeData <> nil then
          AList.Add(NodeData^.Test.TestName);
      end;

      Node := vstTests.GetNext(Node);
    until Node = nil;
  finally
    vstTests.EndUpdate;
  end;
end;


procedure TfrmPitstopTestRunner.GetSelectedNodes(var ASelectedNodes: TNodeArray);
var
  Node: PVirtualNode;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  SetLength(ASelectedNodes, 0);

  vstTests.BeginUpdate;
  try
    repeat
      if vstTests.Selected[Node] then
      begin
        SetLength(ASelectedNodes, Length(ASelectedNodes) + 1);
        ASelectedNodes[Length(ASelectedNodes) - 1] := Node;
      end;

      Node := vstTests.GetNext(Node);
    until Node = nil;
  finally
    vstTests.EndUpdate;
  end;
end;


procedure TfrmPitstopTestRunner.SelectTestsByName(AList: TStringList);
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  vstTests.ClearSelection;

  vstTests.BeginUpdate;
  try
    repeat
      NodeData := vstTests.GetNodeData(Node);
      if NodeData <> nil then
        if AList.IndexOf(NodeData^.Test.TestName) > -1 then
        begin
          vstTests.Selected[Node] := True;
          vstTests.ScrollIntoView(Node, False);
        end;

      Node := vstTests.GetNext(Node);
    until Node = nil;
  finally
    vstTests.EndUpdate;
  end;
end;


function TfrmPitstopTestRunner.GetNodeByTest(ATest: TTest): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Result := nil;
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  vstTests.BeginUpdate;
  try
    repeat
      NodeData := vstTests.GetNodeData(Node);
      if NodeData <> nil then
        if NodeData^.Test = ATest then
        begin
          Result := Node;
          Exit;
        end;

      Node := vstTests.GetNext(Node);
    until Node = nil;
  finally
    vstTests.EndUpdate;
  end;
end;


procedure TfrmPitstopTestRunner.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
  Fnm: string;
  SelectedTests: TStringList;
  i, n: Integer;
begin
  Fnm := GetIniNameNoExt;
  if not FileExists(Fnm) then
    Exit;

  Ini := TMemIniFile.Create(Fnm);
  try
    Left := Ini.ReadInteger('PitstopTestRunner.Window', 'Left', Left);
    Top := Ini.ReadInteger('PitstopTestRunner.Window', 'Top', Top);
    Width := Ini.ReadInteger('PitstopTestRunner.Window', 'Width', Width);
    Height := Ini.ReadInteger('PitstopTestRunner.Window', 'Height', Height);

    SelectedTests := TStringList.Create;
    try
      n := Ini.ReadInteger('SelectedTests', 'Count', 0);
      for i := 0 to n - 1 do
        SelectedTests.Add(Ini.ReadString('SelectedTests', 'Test_' + IntToStr(i), 'UnknownTestName'));

      SelectTestsByName(SelectedTests);
    finally
      SelectedTests.Free;
    end;
  finally
    Ini.Free;
  end;
end;


procedure TfrmPitstopTestRunner.SaveSettingsToIni;
var
  Ini: TMemIniFile;
  Fnm: string;
  SelectedTests: TStringList;
  i: Integer;
begin
  Fnm := GetIniNameNoExt;
  if not FileExists(Fnm) then
    Exit;

  Ini := TMemIniFile.Create(Fnm);
  try
    Ini.WriteInteger('PitstopTestRunner.Window', 'Left', Left);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Top', Top);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Width', Width);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Height', Height);

    SelectedTests := TStringList.Create;
    try
      GetListOfSelectedTests(SelectedTests);
      Ini.EraseSection('SelectedTests');

      Ini.WriteInteger('SelectedTests', 'Count', SelectedTests.Count);
      for i := 0 to SelectedTests.Count - 1 do
        Ini.WriteString('SelectedTests', 'Test_' + IntToStr(i), SelectedTests.Strings[i]);  //this should contain the full test name (i.e. with parent categories)
    finally
      SelectedTests.Free;
    end;

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
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


procedure TfrmPitstopTestRunner.AddFailure(ATest: TTest; AFailure: TTestFailure);
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := GetNodeByTest(ATest);
  if Node = nil then
    Exit;

  NodeData := vstTests.GetNodeData(Node);
  if NodeData <> nil then
  begin
    NodeData^.TestResult := DateTimeToStr(Now) + #13#10 +
                            AFailure.ExceptionMessage + #13#10 +
                            '(' + AFailure.ExceptionClassName + ') at ' + AFailure.LocationInfo;
    memTestResult.Lines.Text := NodeData^.TestResult;

    if vstTests.GetNodeLevel(Node) = 2 then
    begin
      Node := Node^.Parent;
      if Node <> nil then
      begin
        NodeData := vstTests.GetNodeData(Node);
        if NodeData <> nil then
          NodeData^.Status := tsFailed;
      end;
    end;
  end;
end;


procedure TfrmPitstopTestRunner.AddError(ATest: TTest; AError: TTestFailure);
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := GetNodeByTest(ATest);
  if Node = nil then
    Exit;

  NodeData := vstTests.GetNodeData(Node);
  if NodeData <> nil then
  begin
    NodeData^.TestResult := DateTimeToStr(Now) + #13#10 +
                            AError.ExceptionMessage + #13#10 +
                            '(' + AError.ExceptionClassName + ') at ' + AError.LocationInfo;
    memTestResult.Lines.Text := NodeData^.TestResult;

    if vstTests.GetNodeLevel(Node) = 2 then
    begin
      Node := Node^.Parent;
      if Node <> nil then
      begin
        NodeData := vstTests.GetNodeData(Node);
        if NodeData <> nil then
          NodeData^.Status := tsFailed;
      end;
    end;
  end;
end;


procedure TfrmPitstopTestRunner.StartTest(ATest: TTest);
begin
  // UI update if needed
end;


procedure TfrmPitstopTestRunner.EndTest(ATest: TTest);
begin
  // UI update if needed
end;


procedure TfrmPitstopTestRunner.StartTestSuite(ATestSuite: TTestSuite);
begin
  //
end;


procedure TfrmPitstopTestRunner.EndTestSuite(ATestSuite: TTestSuite);
begin
  //
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
  vstTests.ScrollIntoView(ANode, True);

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
    TestRes.AddListener(Self);
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
      Result := False;
    end;

    vstTests.InvalidateNode(ANode);
    TestRes.Free;
  end;
end;


function TfrmPitstopTestRunner.RunCategory(ACatNode: PVirtualNode): Boolean;
var
  Node, CntNode: PVirtualNode;
  NodeData: PTestNodeRec;
  CategoryProgress: Integer;
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

    prbTestProgress.Max := 0;
    CntNode := Node;
    repeat
      prbTestProgress.Max := prbTestProgress.Max + 1;
      CntNode := CntNode^.NextSibling;
    until CntNode = nil;

    CategoryProgress := 0;
    repeat
      vstTests.Expanded[Node] := True;
      vstTests.Repaint;

      prbTestProgress.Position := CategoryProgress;
      Inc(CategoryProgress);  //after setting the progress bar
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
    prbTestProgress.Position := prbTestProgress.Max;
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
    spdbtnRunAllSelectedTests.Enabled := False;
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
      spdbtnRunAllSelectedTests.Enabled := True;
      spdbtnPause.Enabled := False;
      spdbtnStop.Enabled := False;
    end;
  end;
end;


procedure TfrmPitstopTestRunner.spdbtnRunAllSelectedTestsClick(Sender: TObject);
var
  Node: PVirtualNode;
  SelectedTests: TNodeArray;
  i: Integer;
begin
  FStopping := False;

  Node := vstTests.GetFirstSelected;
  if Node = nil then
    AddToLog('No test selected.')
  else
  begin
    spdbtnRunAll.Enabled := False;
    spdbtnRunSelectedTest.Enabled := False;
    spdbtnRunAllSelectedTests.Enabled := False;
    spdbtnPause.Enabled := True;
    spdbtnStop.Enabled := True;
    InitStatusOnAllTests;

    GetSelectedNodes(SelectedTests);

    try
      for i := 0 to Length(SelectedTests) - 1 do
      begin
        vstTests.Expanded[SelectedTests[i]] := True;
        vstTests.Repaint;
        RunCategory(SelectedTests[i]);

        if FStopping then //The tests are skipped if FStopping is True. But then, their status is updated as passed, although they are skipped.
          Break;
      end;
    finally
      spdbtnRunAll.Enabled := True;
      spdbtnRunSelectedTest.Enabled := True;
      spdbtnRunAllSelectedTests.Enabled := True;
      spdbtnPause.Enabled := False;
      spdbtnStop.Enabled := False;

      SetLength(SelectedTests, 0);
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

  LoadSettingsFromIni;
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


procedure TfrmPitstopTestRunner.vstTestsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    DisplayTestResultFromSelectedTest;
end;


procedure TfrmPitstopTestRunner.DisplayTestResultFromSelectedTest;
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirstSelected;
  if Node = nil then
    Exit;

  NodeData := vstTests.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  memTestResult.Lines.Text := NodeData^.TestResult;
end;


procedure TfrmPitstopTestRunner.vstTestsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DisplayTestResultFromSelectedTest;
end;

end.

