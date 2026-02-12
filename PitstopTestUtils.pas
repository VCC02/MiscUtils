{
    Copyright (C) 2026 VCC
    creation date: 03 Feb 2026
    initial release date: 03 Feb 2026

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


unit PitstopTestUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


type
  TTestStatus = (tsInit, tsFailed, tsPassed, tsRunning, tsPaused);

const
  CTestStatusStr: array[TTestStatus] of string = ('tsInit', 'tsFailed', 'tsPassed', 'tsRunning', 'tsPaused');

  CPitstopCmd_AddToLog = 'AddToLog';
  CPitstopCmd_RunCategory = 'RunCategory';
  CPitstopCmd_RunTest = 'RunTest';
  CPitstopCmd_SetTestVars = 'SetTestVars';
  CPitstopCmd_PauseTests = 'PauseTests';
  CPitstopCmd_ContinueTests = 'ContinueTests';
  CPitstopCmd_StopTests = 'StopTests';

  CPitstopCmd_Param_Auth = 'Auth';
  CPitstopCmd_Param_StoppingNow = 'StoppingNow';
  CPitstopCmd_Param_Category = 'Category';
  CPitstopCmd_Param_Test = 'Test';

  CFirstSeparator = '(::)';
  CSecondSeparator = '(:.:)';
  CRunInfoPrefix = 'Run info: ';


procedure ParseTestResult(ATestLine: string; out ATestName, ATestResult, AErrorMessage, ARunInfo: string);
function TestStatusAsStringToStatus(ATestStatusStr: string): TTestStatus;


implementation


procedure ParseTestResult(ATestLine: string; out ATestName, ATestResult, AErrorMessage, ARunInfo: string);
var
  TestResultAndInfo, TestInfo: string;
begin
  ATestName := Copy(ATestLine, 1, Pos('=', ATestLine) - 1);
  TestResultAndInfo := Copy(ATestLine, Pos('=', ATestLine) + 1, MaxInt);
  ATestResult := Copy(TestResultAndInfo, 1, Pos(CFirstSeparator, TestResultAndInfo) - 1);
  TestInfo := Copy(TestResultAndInfo, Pos(CFirstSeparator, TestResultAndInfo) + Length(CFirstSeparator), MaxInt);

  AErrorMessage := Copy(TestInfo, 1, Pos(CSecondSeparator, TestInfo) - 1);
  AErrorMessage := StringReplace(AErrorMessage, #13#10, ', ', [rfReplaceAll]);

  ARunInfo := Copy(TestInfo, Pos(CSecondSeparator, TestInfo) + Length(CSecondSeparator), MaxInt);

  if Pos(CRunInfoPrefix, ARunInfo) > 0 then
    ARunInfo := Copy(ARunInfo, Pos(CRunInfoPrefix, ARunInfo) + Length(CRunInfoPrefix), MaxInt);

  ARunInfo := StringReplace(ARunInfo, #13#10, ', ', [rfReplaceAll]);
end;


function TestStatusAsStringToStatus(ATestStatusStr: string): TTestStatus;
var
  i: TTestStatus;
begin
  Result := tsInit;
  for i := Low(TTestStatus) to High(TTestStatus) do
    if ATestStatusStr = CTestStatusStr[i] then
    begin
      Result := i;
      Break;
    end;
end;

end.

