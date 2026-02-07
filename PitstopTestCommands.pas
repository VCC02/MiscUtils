{
    Copyright (C) 2026 VCC
    creation date: 30 Jan 2026
    initial release date: 30 Jan 2026

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


unit PitstopTestCommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


procedure CreatePitstopCommandServer;
procedure DestroyPitstopCommandServer;


var
  TestVars: TStringList; //created / destroyed by initialization / finalization sections

implementation


uses
  PitstopTestRunner, PitstopTestUtils,
  IdHTTPServer, IdCustomHTTPServer, IdContext, IdSync;

type
  TServerHandlers = class
  private
    FAuthString: string;

    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

  TSyncCmd = (scAddToLog, scRunCategory, scSetTestVars, scPauseTests, scContinueTests, scStopTests);
  TSyncObj = class(TIdSync)
  private
    FCmd: TSyncCmd;
    FMsg: string;
    FCategory: string;
    FTestVars: TStringList;
    FResponse: string;
    FStoppingNow: Boolean;
  protected
    procedure DoSynchronize; override;
  end;


var
  IdHTTPServer1: TIdHTTPServer;
  ServerHandlers: TServerHandlers;


procedure TSyncObj.DoSynchronize;
begin
  case FCmd of
    scAddToLog:
      frmPitstopTestRunner.AddToLog(FMsg);

    scRunCategory:
      frmPitstopTestRunner.RunCategoryByName(FCategory, FResponse);

    scSetTestVars:
      TestVars.Assign(FTestVars);

    scPauseTests:
      frmPitstopTestRunner.PauseTests;

    scContinueTests:
      frmPitstopTestRunner.ContinueTests;

    scStopTests:
      frmPitstopTestRunner.StopTests(FStoppingNow);
  end;
end;


procedure AddToLog(AMsg: string);
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FMsg := AMsg;
    SyncObj.FCmd := scAddToLog;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


function RunCategory(ACategory: string): string;
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := scRunCategory;
    SyncObj.FCategory := ACategory;
    SyncObj.Synchronize;
    Result := SyncObj.FResponse;
  finally
    SyncObj.Free;
  end;
end;


procedure SetTestVars(ATestVars: TStrings);
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := scSetTestVars;
    SyncObj.FTestVars := TStringList.Create;
    try
      SyncObj.FTestVars.Assign(ATestVars);
      SyncObj.Synchronize;
    finally
      SyncObj.FTestVars.Free;
    end;
  finally
    SyncObj.Free;
  end;
end;


procedure PauseTests;
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := scPauseTests;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


procedure ContinueTests;
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := scContinueTests;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


procedure StopTests(AStoppingNow: Boolean);
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := scStopTests;
    SyncObj.FStoppingNow := AStoppingNow;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


procedure TServerHandlers.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Cmd: string;
  CategoryName: string;
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if ARequestInfo.Params.Values[CPitstopCmd_Param_Auth] <> ServerHandlers.FAuthString then  //empty string is allowed if this is how the app is started
  begin
    AddToLog('Forbidden: ' + Cmd);
    AResponseInfo.ContentText := 'Forbidden';
    Exit;
  end;

  if Cmd = '/' + CPitstopCmd_AddToLog then
  begin
    AddToLog(ARequestInfo.Params.Values['Log']);
    AResponseInfo.ContentText := 'Done';
  end;

  if Cmd = '/' + CPitstopCmd_RunCategory then
  begin
    CategoryName := ARequestInfo.Params.Values['Category'];
    AddToLog('Running category: ' + CategoryName);
    AResponseInfo.ContentText := RunCategory(CategoryName);
    Exit;
  end;

  if Cmd = '/' + CPitstopCmd_SetTestVars then
  begin
    SetTestVars(ARequestInfo.Params);
    AResponseInfo.ContentText := 'Done';
  end;

  if Cmd = '/' + CPitstopCmd_PauseTests then
  begin
    PauseTests;
    AResponseInfo.ContentText := 'Done';
  end;

  if Cmd = '/' + CPitstopCmd_ContinueTests then
  begin
    ContinueTests;
    AResponseInfo.ContentText := 'Done';
  end;

  if Cmd = '/' + CPitstopCmd_StopTests then
  begin
    StopTests(ARequestInfo.Params.Values[CPitstopCmd_Param_StoppingNow] <> 'False');
    AResponseInfo.ContentText := 'Done';
  end;
end;


procedure CreatePitstopCommandServer;
var
  i: Integer;
begin
  if IdHTTPServer1 = nil then
  begin
    IdHTTPServer1 := TIdHTTPServer.Create;
    IdHTTPServer1.DefaultPort := 7472;

    ServerHandlers := TServerHandlers.Create;
    ServerHandlers.FAuthString := '';

    for i := 1 to ParamCount - 1 do
      if ParamStr(i) = '--Auth' then
      begin
        ServerHandlers.FAuthString := ParamStr(i + 1);
        Break;
      end;
  end;

  try
    IdHTTPServer1.OnCommandGet := @ServerHandlers.IdHTTPServer1CommandGet;

    frmPitstopTestRunner.AddToLog('Starting server module in PitstopTestRunner.');
    IdHTTPServer1.Active := True;
    frmPitstopTestRunner.AddToLog('PitstopTestRunner is listening on port ' + IntToStr(IdHTTPServer1.DefaultPort));
  except
    on E: EIdHTTPServerError do
    begin
      frmPitstopTestRunner.AddToLog('PitstopTestRunner can''t listen on port ' + IntToStr(IdHTTPServer1.DefaultPort) + '. ' + E.Message);
      raise;
    end;
  end;
end;


procedure DestroyPitstopCommandServer;
begin
  if IdHTTPServer1 = nil then
    Exit;

  try
    IdHTTPServer1.Active := False;
  finally
    ServerHandlers.Free;
    FreeAndNil(IdHTTPServer1);
  end;
end;


function GetTestVars: TStringList;
begin
  Result := TestVars;
end;


initialization
  TestVars := TStringList.Create;
  IdHTTPServer1 := nil;

finalization
  FreeAndNil(TestVars);
end.

