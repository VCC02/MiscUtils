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
  Buttons, VirtualTrees, fpcunit, ImgList, ComCtrls, Menus;

type
  TTestStatus = (tsInit, tsFailed, tsPassed, tsRunning, tsPaused);

  TTestNodeRec = record
    Test: TTest;
    IsRootTest: Boolean;
    Status: TTestStatus;
    TestResult: string;
    ExtraResult: string; //can be set in test code
  end;
  PTestNodeRec = ^TTestNodeRec;

  TSettingsHandler = TNotifyEvent; //procedure of object;

  TSettingsHandlerRec = record
    SettingsCategory: string;
    ParentCaption: string;
    MenuCaption: string;
    Handler: TSettingsHandler;
    IsAutoCheck: Boolean;
    IsRadioItem: Boolean;
    IsChecked: Boolean;
  end;

  TSettingsHandlerRecArr = array of TSettingsHandlerRec;

  { TfrmPitstopTestRunner }

  TfrmPitstopTestRunner = class(TForm, ITestListener)
    imglstTestStatus: TImageList;
    memLog: TMemo;
    memTestResult: TMemo;
    MenuItemRunUntilFails: TMenuItem;
    MenuItem_CopyTestNameToClipboard: TMenuItem;
    pnlToolbar: TPanel;
    pmTests: TPopupMenu;
    pmRun: TPopupMenu;
    pmSettings: TPopupMenu;
    prbTestProgress: TProgressBar;
    spdbtnExtraRunSelectedTest: TSpeedButton;
    spdbtnRunAllSelectedTests: TSpeedButton;
    spdbtnSettings: TSpeedButton;
    spdbtnStop: TSpeedButton;
    spdbtnRunAll: TSpeedButton;
    spdbtnPause: TSpeedButton;
    spdbtnRunSelectedTest: TSpeedButton;
    tmrStartup: TTimer;
    vstTests: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemRunUntilFailsClick(Sender: TObject);
    procedure MenuItem_CopyTestNameToClipboardClick(Sender: TObject);
    procedure spdbtnExtraRunSelectedTestClick(Sender: TObject);
    procedure spdbtnPauseClick(Sender: TObject);
    procedure spdbtnRunAllClick(Sender: TObject);
    procedure spdbtnRunAllSelectedTestsClick(Sender: TObject);
    procedure spdbtnRunSelectedTestClick(Sender: TObject);
    procedure spdbtnSettingsClick(Sender: TObject);
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
    FPersistentTestSettings: TStringList;
    FSettingsHandlers: TSettingsHandlerRecArr;

    procedure CreateAllMenuCategories;
    procedure NoUserSettingsConfiguredClick(Sender: TObject);

    function GetIniNameNoExt: string;
    procedure GetListOfSelectedTests(AList: TStringList; ATestNameContainsParent: Boolean = False);         //selected doesn't mean checked
    procedure GetSelectedNodes(var ASelectedNodes: TNodeArray);  //selected doesn't mean checked
    procedure SelectTestsByName(AList: TStringList; ATestNameContainsParent: Boolean = False);
    function GetNodeByTest(ATest: TTest): PVirtualNode;
    function GetNodeByName(AName: string): PVirtualNode;
    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;
    procedure DisplayTestResultFromSelectedTest;
    procedure CopySelectedTestNamesToClipboard;

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
    function RunCategoryByName(ACatName: string; out AResponse: string): Boolean;
    procedure SetExtraTestResult(ATest: TTest; AExtraResult: string);
    procedure GetPersistentTestSettings(ASettings: TStringList);
    procedure SetValueToPersistentTestSettings(AVarName, AValue: string);

    procedure RegisterTestSettings(ASettingsCategory, AParentCaption, AMenuEntryCaption: string; ASettingsHandler: TSettingsHandler; AIsAutoCheck, AIsRadioItem, AIsChecked: Boolean);
    procedure UpdateTestSettingsItemCheckedState(ASettingsCategory, AParentCaption, AMenuEntryCaption: string; AIsChecked: Boolean);

    procedure PauseTests;
    procedure ContinueTests;
    procedure StopTests;
  end;


const
  CTestStatusStr: array[TTestStatus] of string = ('tsInit', 'tsFailed', 'tsPassed', 'tsRunning', 'tsPaused');

var
  frmPitstopTestRunner: TfrmPitstopTestRunner;

{ToDo:
- Implement checkboxes on test nodes.
- Set "AllTests" category status to failed if at least one category fails.
- Measure test duration
- [nice to have] - report test results (xml or ini)
}

implementation

{$R *.frm}

uses
  testregistry, IniFiles, Clipbrd, PitstopTestUtils;

{ TfrmPitstopTestRunner }

procedure TfrmPitstopTestRunner.FormCreate(Sender: TObject);
begin
  FPaused := False;
  FStopping := False;
  vstTests.Colors.UnfocusedSelectionColor := clGradientInactiveCaption;
  vstTests.NodeDataSize := SizeOf(TTestNodeRec);
  FPersistentTestSettings := TStringList.Create;
  SetLength(FSettingsHandlers, 0);

  tmrStartup.Enabled := True;
end;


procedure TfrmPitstopTestRunner.FormDestroy(Sender: TObject);
begin
  try
    SaveSettingsToIni;
  except
  end;

  FreeAndNil(FPersistentTestSettings);
  SetLength(FSettingsHandlers, 0);
end;


procedure TfrmPitstopTestRunner.MenuItemRunUntilFailsClick(Sender: TObject);
var
  tk: QWord;
  Cnt: Integer;
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirstSelected;
  if Node = nil then
  begin
    MessageBoxFunction('Please select a test to be run until it fails.', 'Test runner', 0);
    Exit;
  end;

  NodeData := vstTests.GetNodeData(Node);
  if NodeData = nil then
  begin
    AddToLog('Error getting node data.');
    Exit;
  end;

  vstTests.BeginUpdate;
  MenuItemRunUntilFails.Enabled := False;
  try
    AddToLog('Running selected test until fails: ' + NodeData^.Test.TestName);
    Cnt := 1;
    repeat
      AddToLog('Run #' + IntToStr(Cnt));
      Inc(Cnt);
      spdbtnRunAllSelectedTests.Click;

      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(1);
      until (GetTickCount64 - tk > 1000) or FStopping or (NodeData^.Status = tsFailed);
    until FStopping or (NodeData^.Status = tsFailed);
  finally
    vstTests.EndUpdate;
    MenuItemRunUntilFails.Enabled := True;
  end;

  AddToLog('');
end;


procedure TfrmPitstopTestRunner.CopySelectedTestNamesToClipboard;
var
  ListOfTests: TStringList;
  s: string;
begin
  ListOfTests := TStringList.Create;
  try
    ListOfTests.LineBreak := #13#10;
    GetListOfSelectedTests(ListOfTests);
    s := ListOfTests.Text;

    if Length(s) > 0 then
      Delete(s, Length(s) - 1, 2);

    Clipboard.AsText := s;
  finally
    ListOfTests.Free;
  end;
end;


procedure TfrmPitstopTestRunner.MenuItem_CopyTestNameToClipboardClick(
  Sender: TObject);
begin
  CopySelectedTestNamesToClipboard;
end;


procedure TfrmPitstopTestRunner.spdbtnExtraRunSelectedTestClick(Sender: TObject);
begin
  pmRun.PopUp;
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


procedure TfrmPitstopTestRunner.GetListOfSelectedTests(AList: TStringList; ATestNameContainsParent: Boolean = False);
var
  Node, ParentNode: PVirtualNode;
  NodeData, ParentNodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  Node := vstTests.GetNext(Node);   //move away from the first node
  if Node = nil then
    Exit;

  AList.Clear;

  vstTests.BeginUpdate;
  try
    repeat
      if not ATestNameContainsParent then
      begin
        if vstTests.Selected[Node] then
        begin
          NodeData := vstTests.GetNodeData(Node);
          if NodeData <> nil then
            AList.Add(NodeData^.Test.TestName);
        end;
      end
      else
      begin
        if (Node^.Parent <> nil) and (vstTests.Selected[Node]) then
        begin
          ParentNode := Node^.Parent;
          NodeData := vstTests.GetNodeData(Node);
          ParentNodeData := vstTests.GetNodeData(ParentNode);

          if (NodeData <> nil) and (ParentNodeData <> nil) then
            AList.Add(ParentNodeData^.Test.TestName + '.' + NodeData^.Test.TestName);
        end;
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


procedure TfrmPitstopTestRunner.SelectTestsByName(AList: TStringList; ATestNameContainsParent: Boolean = False);
var
  Node, ParentNode: PVirtualNode;
  NodeData, ParentNodeData: PTestNodeRec;
begin
  Node := vstTests.GetFirst;
  if Node = nil then
    Exit;

  Node := vstTests.GetNext(Node);   //move away from the first node
  if Node = nil then
    Exit;

  vstTests.ClearSelection;

  vstTests.BeginUpdate;
  try
    repeat
      if not ATestNameContainsParent then
      begin
        NodeData := vstTests.GetNodeData(Node);
        if NodeData <> nil then
          if AList.IndexOf(NodeData^.Test.TestName) > -1 then
          begin
            vstTests.Selected[Node] := True;
            vstTests.ScrollIntoView(Node, False);
          end;
      end
      else
        if Node^.Parent <> nil then
        begin
          ParentNode := Node^.Parent;
          NodeData := vstTests.GetNodeData(Node);
          ParentNodeData := vstTests.GetNodeData(ParentNode);

          if (NodeData <> nil) and (ParentNodeData <> nil) then
            if AList.IndexOf(ParentNodeData^.Test.TestName + '.' + NodeData^.Test.TestName) > -1 then
            begin
              vstTests.Selected[Node] := True;
              vstTests.ScrollIntoView(Node, False);
            end;
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


function TfrmPitstopTestRunner.GetNodeByName(AName: string): PVirtualNode;   //For now, it works only if the test names are unique, or a category is searched for (as a test)
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
        if NodeData^.Test.TestName = AName then
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

    for i := 0 to vstTests.Header.Columns.Count - 1 do
      vstTests.Header.Columns.Items[i].Width := Ini.ReadInteger('PitstopTestRunner.Window', 'ColWidth', vstTests.Header.Columns.Items[i].Width);

    SelectedTests := TStringList.Create;
    try
      n := Ini.ReadInteger('SelectedTests', 'Count', 0);
      for i := 0 to n - 1 do
        SelectedTests.Add(Ini.ReadString('SelectedTests', 'Test_' + IntToStr(i), 'UnknownTestName'));

      SelectTestsByName(SelectedTests, True);
    finally
      SelectedTests.Free;
    end;

    Ini.ReadSectionRaw('PersistentTestSettings', FPersistentTestSettings);
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

  Ini := TMemIniFile.Create(Fnm);
  try
    Ini.WriteInteger('PitstopTestRunner.Window', 'Left', Left);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Top', Top);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Width', Width);
    Ini.WriteInteger('PitstopTestRunner.Window', 'Height', Height);

    for i := 0 to vstTests.Header.Columns.Count - 1 do
      Ini.WriteInteger('PitstopTestRunner.Window', 'ColWidth', vstTests.Header.Columns.Items[i].Width);

    SelectedTests := TStringList.Create;
    try
      GetListOfSelectedTests(SelectedTests, True);
      Ini.EraseSection('SelectedTests');

      Ini.WriteInteger('SelectedTests', 'Count', SelectedTests.Count);
      for i := 0 to SelectedTests.Count - 1 do
        Ini.WriteString('SelectedTests', 'Test_' + IntToStr(i), SelectedTests.Strings[i]);  //this should contain the full test name (i.e. with parent categories)
    finally
      SelectedTests.Free;
    end;

    for i := 0 to FPersistentTestSettings.Count - 1 do
      Ini.WriteString('PersistentTestSettings', FPersistentTestSettings.Names[i], FPersistentTestSettings.ValueFromIndex[i]);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmPitstopTestRunner.spdbtnPauseClick(Sender: TObject);
begin
  FPaused := not FPaused;
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
  FPaused := False;

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


procedure TfrmPitstopTestRunner.CreateAllMenuCategories;
var
  i: Integer;
  CategoryMenuItem: TMenuItem;
begin
  for i := 0 to Length(FSettingsHandlers) - 1 do
  begin
    CategoryMenuItem := pmSettings.Items.Find(FSettingsHandlers[i].SettingsCategory);
    if (CategoryMenuItem = nil) or (FSettingsHandlers[i].SettingsCategory = '-') then
    begin
      CategoryMenuItem := TMenuItem.Create(Self);
      CategoryMenuItem.Caption := FSettingsHandlers[i].SettingsCategory;
      pmSettings.Items.Add(CategoryMenuItem);
    end;
  end;
end;


procedure TfrmPitstopTestRunner.NoUserSettingsConfiguredClick(Sender: TObject);
begin
  MessageBoxFunction('You may have to run a "BeforeAll" test first, or other configuration procedure, in order to generate the menu.', PChar(Caption), MB_ICONINFORMATION);
end;


procedure TfrmPitstopTestRunner.spdbtnSettingsClick(Sender: TObject);
var
  CategoryMenuItem, ParentMenuItem, MenuItem: TMenuItem;
  i: Integer;
begin
  pmSettings.Items.Clear;
  if Length(FSettingsHandlers) = 0 then
  begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := 'No user settings configured.';
    MenuItem.OnClick := @NoUserSettingsConfiguredClick;
    pmSettings.Items.Add(MenuItem);
  end;

  CreateAllMenuCategories;

  for i := 0 to Length(FSettingsHandlers) - 1 do
    if FSettingsHandlers[i].SettingsCategory <> '-' then
    begin
      MenuItem := TMenuItem.Create(Self);
      MenuItem.Caption := FSettingsHandlers[i].MenuCaption;
      MenuItem.OnClick := FSettingsHandlers[i].Handler;
      MenuItem.Bitmap := nil; //
      MenuItem.AutoCheck := FSettingsHandlers[i].IsAutoCheck;
      MenuItem.RadioItem := FSettingsHandlers[i].IsRadioItem;
      MenuItem.Checked := FSettingsHandlers[i].IsChecked;

      CategoryMenuItem := pmSettings.Items.Find(FSettingsHandlers[i].SettingsCategory);
      if CategoryMenuItem = nil then   //if CategoryMenuItem is nil, it means that pmSettings.Items.Find is not reliable
      begin
        CategoryMenuItem := TMenuItem.Create(Self);
        CategoryMenuItem.Caption := FSettingsHandlers[i].SettingsCategory;
        pmSettings.Items.Add(CategoryMenuItem);
      end;

      if FSettingsHandlers[i].ParentCaption = '' then
        CategoryMenuItem.Add(MenuItem)
      else
      begin
        ParentMenuItem := CategoryMenuItem.Find(FSettingsHandlers[i].ParentCaption);
        if ParentMenuItem <> nil then
          ParentMenuItem.Add(MenuItem);
      end;
    end;

  pmSettings.PopUp;
end;


procedure TfrmPitstopTestRunner.spdbtnRunAllSelectedTestsClick(Sender: TObject);
var
  Node: PVirtualNode;
  SelectedTests: TNodeArray;
  i: Integer;
begin
  FStopping := False;
  FPaused := False;

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
  StopTests;
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

    case Column of
      0:
      begin
        if NodeData^.IsRootTest then
          CellText := 'All Tests'
        else
          CellText := NodeData^.Test.TestName;
      end;

      1:
        CellText := StringReplace(NodeData^.TestResult, #13#10, '  ', [rfReplaceAll]);
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrmPitstopTestRunner.vstTestsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
    DisplayTestResultFromSelectedTest;

  if Key = Ord('C') then
    if ssCtrl in Shift then
      CopySelectedTestNamesToClipboard;
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


function TfrmPitstopTestRunner.RunCategoryByName(ACatName: string; out AResponse: string): Boolean;
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  Result := False;
  Node := GetNodeByName(ACatName);
  if Node = nil then
  begin
    AResponse := 'Category not found';
    Exit;
  end;

  FStopping := False;
  FPaused := False;

  AResponse := '';
  try
    spdbtnRunAll.Enabled := False;
    spdbtnRunSelectedTest.Enabled := False;
    spdbtnRunAllSelectedTests.Enabled := False;
    spdbtnPause.Enabled := True;
    spdbtnStop.Enabled := True;
    InitStatusOnAllTests;

    try
      vstTests.Expanded[Node] := True;
      vstTests.Repaint;
      Result := RunCategory(Node);
    finally
      spdbtnRunAll.Enabled := True;
      spdbtnRunSelectedTest.Enabled := True;
      spdbtnRunAllSelectedTests.Enabled := True;
      spdbtnPause.Enabled := False;
      spdbtnStop.Enabled := False;
    end;
  finally
    Node := Node^.FirstChild;
    repeat
      NodeData := vstTests.GetNodeData(Node);
      AResponse := AResponse + NodeData^.Test.TestName + '=' + CTestStatusStr[NodeData^.Status] + CFirstSeparator +
                                                               NodeData^.TestResult + CSecondSeparator +
                                                               NodeData^.ExtraResult +
                                                               #4#5;

      Node := Node^.NextSibling;
    until Node = nil;
  end;
end;


procedure TfrmPitstopTestRunner.SetExtraTestResult(ATest: TTest; AExtraResult: string);
var
  Node: PVirtualNode;
  NodeData: PTestNodeRec;
begin
  if ATest = nil then
  begin
    AddToLog('Nil "Test" argument when setting extra result.');
    Exit;
  end;

  Node := GetNodeByTest(ATest);
  if Node = nil then
  begin
    try
      AddToLog('Test "' + ATest.TestName + '" not found when setting extra result.' { ATest = ' + IntToStr(QWord(Pointer(ATest)))});
    except
      AddToLog('Invalid "Test" argument when setting extra result.' { ATest = ' + IntToStr(QWord(Pointer(ATest)))});
    end;

    Exit;
  end;

  AddToLog('Setting extra test result of ' + ATest.TestName + ' to ' + AExtraResult);
  NodeData := vstTests.GetNodeData(Node);
  NodeData^.ExtraResult := AExtraResult;
end;


procedure TfrmPitstopTestRunner.GetPersistentTestSettings(ASettings: TStringList);
begin
  ASettings.Assign(FPersistentTestSettings);
end;


procedure TfrmPitstopTestRunner.SetValueToPersistentTestSettings(AVarName, AValue: string);
begin
  FPersistentTestSettings.Values[AVarName] := AValue;
end;


procedure TfrmPitstopTestRunner.RegisterTestSettings(ASettingsCategory, AParentCaption, AMenuEntryCaption: string; ASettingsHandler: TSettingsHandler; AIsAutoCheck, AIsRadioItem, AIsChecked: Boolean);
begin
  SetLength(FSettingsHandlers, Length(FSettingsHandlers) + 1);
  FSettingsHandlers[Length(FSettingsHandlers) - 1].SettingsCategory := ASettingsCategory;
  FSettingsHandlers[Length(FSettingsHandlers) - 1].ParentCaption := AParentCaption;
  FSettingsHandlers[Length(FSettingsHandlers) - 1].MenuCaption := AMenuEntryCaption;

  FSettingsHandlers[Length(FSettingsHandlers) - 1].Handler := ASettingsHandler;
  FSettingsHandlers[Length(FSettingsHandlers) - 1].IsAutoCheck := AIsAutoCheck;
  FSettingsHandlers[Length(FSettingsHandlers) - 1].IsRadioItem := AIsRadioItem;
  FSettingsHandlers[Length(FSettingsHandlers) - 1].IsChecked := AIsChecked;
end;


procedure TfrmPitstopTestRunner.UpdateTestSettingsItemCheckedState(ASettingsCategory, AParentCaption, AMenuEntryCaption: string; AIsChecked: Boolean);
var
  i: Integer;
  TargetItemIsRadio: Boolean;
begin
  TargetItemIsRadio := False;
  for i := 0 to Length(FSettingsHandlers) - 1 do
    if (FSettingsHandlers[i].SettingsCategory = ASettingsCategory) and
       (FSettingsHandlers[i].MenuCaption = AMenuEntryCaption) and
       (FSettingsHandlers[i].ParentCaption = AParentCaption) then
    begin
      FSettingsHandlers[i].IsChecked := AIsChecked;
      TargetItemIsRadio := FSettingsHandlers[i].IsRadioItem;
      Break;
    end;

  if AIsChecked then
  begin
    for i := 0 to Length(FSettingsHandlers) - 1 do
      if (FSettingsHandlers[i].SettingsCategory = ASettingsCategory) and
         (FSettingsHandlers[i].MenuCaption <> AMenuEntryCaption) and
         (FSettingsHandlers[i].ParentCaption = AParentCaption) then
        if FSettingsHandlers[i].IsRadioItem and TargetItemIsRadio then
          FSettingsHandlers[i].IsChecked := False;  //uncheck all the other radio items
  end;
end;


procedure TfrmPitstopTestRunner.PauseTests;
begin
  FPaused := True;
  AddToLog('Pausing...');
end;


procedure TfrmPitstopTestRunner.ContinueTests;
begin
  FPaused := False;
  AddToLog('Continuing...');
end;


procedure TfrmPitstopTestRunner.StopTests;
begin
  FPaused := False;
  FStopping := True;
  spdbtnStop.Enabled := False;
  spdbtnPause.Enabled := False;
  AddToLog('Stopping...');
end;

end.

