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
  TestVars: TStringList; //created / destroyed by CreatePitstopCommandServer / DestroyPitstopCommandServer

implementation


uses
  PitstopTestRunner,
  IdHTTPServer, IdCustomHTTPServer, IdContext, IdSync;

type
  TServerHandlers = class
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;

  TSyncCmd = (scAddToLog, scRunCategory, scSetTestVars);
  TSyncObj = class(TIdSync)
  private
    FCmd: TSyncCmd;
    FMsg: string;
    FCategory: string;
    FTestVars: TStringList;
    FResponse: string;
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


procedure TServerHandlers.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Cmd: string;
  CategoryName: string;
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + 'AddToLog' then
  begin
    AddToLog(ARequestInfo.Params.Values['Log']);
    AResponseInfo.ContentText := 'Done';
  end;

  if Cmd = '/' + 'RunCategory' then
  begin
    CategoryName := ARequestInfo.Params.Values['Category'];
    AddToLog('Running category: ' + CategoryName);
    AResponseInfo.ContentText := RunCategory(CategoryName);
    Exit;
  end;

  if Cmd = '/' + 'SetTestVars' then
  begin
    SetTestVars(ARequestInfo.Params);
    AResponseInfo.ContentText := 'Done';
  end;
end;


procedure CreatePitstopCommandServer;
begin
  IdHTTPServer1 := TIdHTTPServer.Create;
  IdHTTPServer1.DefaultPort := 7472;
  ServerHandlers := TServerHandlers.Create;
  TestVars := TStringList.Create;
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
  try
    IdHTTPServer1.Active := False;
  finally
    ServerHandlers.Free;
    IdHTTPServer1.Free;
    TestVars.Free;
  end;
end;


function GetTestVars: TStringList;
begin
  Result := TestVars;
end;

end.

