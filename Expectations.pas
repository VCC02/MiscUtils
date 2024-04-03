{
    Copyright (C) 2024 VCC
    creation date: Nov 2021
    initial release date: 25 Aug 2022

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

unit Expectations;

//{$mode ObjFPC}{$H+}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}

// When using "ConvExMsg", compiler directive (at project-level),
// the messages from raised exceptions will have ASCII 0 characters (#0), replaced with ASCII 1 characters (#1),
// so they can be visible on various window components (e.g. TMemo, TListBox etc.)

// When displayed string values are too long, to prevent UI glitching, the "TruncateDiffMsg" compiler directive can be defined.
// This will truncate the values to 255 characters.

interface

uses
  Classes, SysUtils;


type
  TStrArr = array[0..0] of string;
  PStrArr = ^TStrArr;

  TConstByteArr = array[0..0] of Byte;
  PConstByteArr = ^TConstByteArr;

  TStringCallbackFunc = function: string;
  TIntegerCallbackFunc = function: Integer;
  TDWordCallbackFunc = function: DWord;
  TBooleanCallbackFunc = function: Boolean;

  TExpectDataType = (edtString, edtInteger, edtDWord, edtBoolean); //couldn't use TTypeKind, because Integer and DWord resolve to the same type

  EExp = class(Exception)
  end;

  TExpect = class;


  TExpectedValue = class
  private
    FActualValue: string;
  public
    constructor Create;
    procedure OfValue(ExpectedValue: string; ExtraMessage: string = '');
    procedure DifferentThanValue(ExpectedValue: string; ExtraMessage: string = '');
    procedure ToContain(ExpectedValue: string; ExtraMessage: string = ''); overload;
  end;


  TExpect = class
  private
    FActualValue: string;
    FActualValueInt: Integer;
    FActualValueDWord: Integer;
    FActualValueStringList: TStringList;
    FActualValuePointer: Pointer;
    FActualValueBoolean: Boolean;
    FActualValueConstByteArr: TBytes;
  public
    constructor Create;

    procedure ToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: PConstByteArr; ExtraMessage: string = ''); overload;
    procedure ToBePointer(ExpectedValue: Pointer; ExtraMessage: string = ''); overload;
    procedure ToBeGreaterThan(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeGreaterThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeLessThan(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeLessThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeAssigned(ExtraMessage: string = ''); overload;  //verifies FActualValueStringList
    procedure ToMatchContentOfStringArray(ExpectedValue: PStrArr; ExpectedLength: Integer; ExtraMessage: string = '');
    procedure NotToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
    procedure ToContain(ExpectedValue: string; ExtraMessage: string = ''); overload;
    function WithItem(ExpectedItemName: string; ExtraMessage: string = ''): TExpectedValue; overload;
  end;


  TLoopedExpect = class
  private
    FTimeout: Integer;

    FActualValue: PString;
    FActualValueDWord: PDWord;
    FActualValueInt: PInteger;
    FActualValueBoolean: PBoolean;

    FActualValueStringCallbackFunc: TStringCallbackFunc;
    FActualValueIntegerCallbackFunc: TIntegerCallbackFunc;
    FActualValueDWordCallbackFunc: TDWordCallbackFunc;
    FActualValueBooleanCallbackFunc: TBooleanCallbackFunc;

    procedure UntypedValidator(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
    procedure UntypedValidatorDifferent(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
    procedure UntypedToBe(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
    procedure UntypedNotToBe(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
  public
    constructor Create;

    procedure ToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;

    procedure NotToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;
  end;


function GetTextFileContent(APath: string): string;

function Expect(ActualValue: string): TExpect; overload;
function Expect(ActualValue: Integer): TExpect; overload;
function Expect(ActualValue: DWord): TExpect; overload;
function Expect(ActualValue: Boolean): TExpect; overload;
function Expect(ActualValue: TStringList): TExpect; overload;
function Expect(ActualValue: Pointer): TExpect; overload;
function Expect(ActualValue: PConstByteArr; ALen: Integer): TExpect; overload;


function LoopedExpect(ActualValue: PString; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: PInteger; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: PDWord; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: PBoolean; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: TStringCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: TIntegerCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
function LoopedExpect(ActualValue: TBooleanCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;

implementation


uses
  Forms, Math;


function FastReplace0To1(s: string): string;
var
  i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
    if Result[i] = #0 then
      Result[i] := #1;
end;


function GetTextFileContent(APath: string): string;
var
  AFileContent: TMemoryStream;
begin
  if not FileExists(APath) then
    Result := 'File not found.'
  else
  begin
    try
      AFileContent := TMemoryStream.Create;
      try
        AFileContent.LoadFromFile(APath);
        SetLength(Result, AFileContent.Size);
        AFileContent.Position := 0;
        AFileContent.Read(Result[1], AFileContent.Size);
      finally
        AFileContent.Free;
      end;
    except
      on E: Exception do
        Result := 'Exception reading file: ' + E.Message;
    end;
  end;
end;


procedure ExpectInt(ActualValue, ExpectedValue: Integer; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise EExp.Create('Expected ' + IntToStr(ExpectedValue) + ', but is was ' + IntToStr(ActualValue) + '.  ' + ExtraMsg);
end;


procedure ExpectDWord(ActualValue, ExpectedValue: DWord; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise EExp.Create('Expected ' + IntToStr(ExpectedValue) + ', but is was ' + IntToStr(ActualValue) + '.  ' + ExtraMsg);
end;


procedure ExpectStr(ActualValue, ExpectedValue: string; ExtraMsg: string = '');
var
  i, DiffAt: Integer;
begin
  if ExpectedValue <> ActualValue then
  begin
    {$IFDEF ConvExMsg}
      ActualValue := FastReplace0To1(ActualValue);
      ExpectedValue := FastReplace0To1(ExpectedValue);
    {$ENDIF}

    if (ActualValue = '') or (ExpectedValue = '') then
      DiffAt := 0
    else
      for i := 1 to Min(Length(ActualValue), Length(ExpectedValue)) do
        if ExpectedValue[i] <> ActualValue[i] then
        begin
          DiffAt := i;
          Break
        end;

    {$IFDEF TruncateDiffMsg}
      ActualValue := Copy(ActualValue, 1, 255);
      ExpectedValue := Copy(ExpectedValue, 1, 255);
    {$ENDIF}

    raise EExp.Create('Expected "' + ExpectedValue + '", but is was "' + ActualValue + '".  Diff at index ' + IntToStr(DiffAt) + '.  ' + ExtraMsg);
  end;
end;


procedure ExpectAssigned(ActualValue: Pointer; ExtraMsg: string = '');
const
  CDigitCount: array[Boolean] of Byte = (8, 16);
var
  DigitCount: Byte;
begin
  DigitCount := CDigitCount[SizeOf(Pointer) = 8];
  if not Assigned(ActualValue) then
    raise EExp.Create('Expected pointer/object at ' + IntToHex(Int64(ActualValue), DigitCount) + ', to be assigned.  ' + ExtraMsg);
end;


procedure ExpectBoolean(ActualValue, ExpectedValue: Boolean; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise EExp.Create('Expected ' + BoolToStr(ExpectedValue, 'True', 'False') + ', but is was ' + BoolToStr(ActualValue, 'True', 'False') + '.  ' + ExtraMsg);
end;


procedure ExpectPointer(ActualValue, ExpectedValue: Pointer; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise EExp.Create('Expected $' + IntToHex(QWord(ExpectedValue), 8) + ', but is was $' + IntToHex(QWord(ActualValue), 8) + '.  ' + ExtraMsg);
end;


procedure ExpectConstByteArr(var ActualValue: TBytes; ExpectedValue: PConstByteArr; ExtraMsg: string = '');
var
  i: Integer;
  s: string;
begin
  s := '';

  for i := 0 to Length(ActualValue) - 1 do
    if ExpectedValue^[i] <> ActualValue[i] then
    begin
      if s <> '' then
        s := s + #13#10;

      s := s + 'Expected ' + IntToStr(ExpectedValue^[i]) + ', but is was ' + IntToStr(ActualValue[i]) + ' at index ' + IntToStr(i) + '.  ' + ExtraMsg;
    end;

  if s <> '' then
    raise EExp.Create(s);
end;


procedure ExpectIntDifferent(ActualValue, ExpectedValue: Integer; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
    raise EExp.Create('Expected ' + IntToStr(ExpectedValue) + ' to be different than ' + IntToStr(ActualValue) + ', but they are the same.  ' + ExtraMsg);
end;


procedure ExpectDWordDifferent(ActualValue, ExpectedValue: DWord; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
    raise EExp.Create('Expected ' + IntToStr(ExpectedValue) + ' to be different than ' + IntToStr(ActualValue) + ', but they are the same.  ' + ExtraMsg);
end;


procedure ExpectStrDifferent(ActualValue, ExpectedValue: string; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
  begin
    {$IFDEF ConvExMsg}
      ActualValue := FastReplace0To1(ActualValue);
      ExpectedValue := FastReplace0To1(ExpectedValue);
    {$ENDIF}

    {$IFDEF TruncateDiffMsg}
      ActualValue := Copy(ActualValue, 1, 255);
      ExpectedValue := Copy(ExpectedValue, 1, 255);
    {$ENDIF}

    raise EExp.Create('Expected "' + ExpectedValue + '" to be different than "' + ActualValue + '", but they are the same.  ' + ExtraMsg);
  end;
end;


procedure ExpectBooleanDifferent(ActualValue, ExpectedValue: Boolean; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
    raise EExp.Create('Expected "' + BoolToStr(ExpectedValue, 'True', 'False') + '" to be different than "' + BoolToStr(ActualValue, 'True', 'False') + '", but they are the same.  ' + ExtraMsg);
end;


constructor TExpect.Create;
begin
  FActualValue := '';
  FActualValueInt := -22; //some unusual, undefault value
  FActualValueStringList := nil;
  FActualValuePointer := nil;
  FActualValueBoolean := False;
  SetLength(FActualValueConstByteArr, 0);
end;


procedure TExpect.ToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
begin
  try
    ExpectStr(FActualValue, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
begin
  try
    ExpectInt(FActualValueInt, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
begin
  try
    ExpectDWord(FActualValueDWord, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;
begin
  try
    ExpectBoolean(FActualValueBoolean, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBe(ExpectedValue: PConstByteArr; ExtraMessage: string = ''); overload;
begin
  try
    ExpectConstByteArr(FActualValueConstByteArr, ExpectedValue, ExtraMessage);
  finally
    SetLength(FActualValueConstByteArr, 0);
    Free;
  end;
end;


procedure TExpect.ToBePointer(ExpectedValue: Pointer; ExtraMessage: string = ''); overload;
begin
  try
    ExpectPointer(FActualValuePointer, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;



procedure TExpect.ToBeGreaterThan(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    if not (FActualValueInt > ExpectedValue) then
      raise EExp.Create('Expected ' + IntToStr(FActualValueInt) + ' to be greater than ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeGreaterThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    if not (FActualValueInt >= ExpectedValue) then
      raise EExp.Create('Expected ' + IntToStr(FActualValueInt) + ' to be greater than or equal to ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeLessThan(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    if not (FActualValueInt < ExpectedValue) then
      raise EExp.Create('Expected ' + IntToStr(FActualValueInt) + ' to be less than ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeLessThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    if not (FActualValueInt <= ExpectedValue) then
      raise EExp.Create('Expected ' + IntToStr(FActualValueInt) + ' to be less than or equal to ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeAssigned(ExtraMessage: string = ''); overload;
begin
  try
    ExpectAssigned(FActualValueStringList, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToMatchContentOfStringArray(ExpectedValue: PStrArr; ExpectedLength: Integer; ExtraMessage: string = '');
var
  i: Integer;
begin
  ExpectInt(FActualValueStringList.Count, ExpectedLength, 'List length (' +
                                                          IntToStr(FActualValueStringList.Count) +
                                                          ') does not match expected array length (' +
                                                          IntToStr(ExpectedLength) +
                                                          ').  ' +
                                                          ExtraMessage);

  for i := 0 to ExpectedLength - 1 do
    if FActualValueStringList.Strings[i] <> ExpectedValue^[i] then
      raise EExp.Create('Expected "' + ExpectedValue^[i] + '", but is was "' + FActualValueStringList.Strings[i] + '"  at index ' + IntToStr(i) + '  ' + ExtraMessage);
end;


procedure TExpect.NotToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
begin
  try
    ExpectStrDifferent(FActualValue, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.NotToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
begin
  try
    ExpectIntDifferent(FActualValueInt, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.NotToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
begin
  try
    ExpectDWordDifferent(FActualValueInt, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToContain(ExpectedValue: string; ExtraMessage: string = ''); overload;
begin
  try
    ExpectInt(Ord(Pos(ExpectedValue, FActualValue) > 0), 1, ExtraMessage + '  Expected "' + ExpectedValue + '" to be a substring of ' + '"' + FActualValue + '"');
  finally
    Free;
  end;
end;


function TExpect.WithItem(ExpectedItemName: string; ExtraMessage: string = ''): TExpectedValue; overload;
begin
  try
    Expect(FActualValueStringList).ToBeAssigned(ExtraMessage + '  Also, make sure you pass a TStringList to Expect(), not a string.');
    Expect(FActualValueStringList.IndexOfName(ExpectedItemName)).ToBeGreaterThan(-1, ExtraMessage + '  Expected "' + ExpectedItemName + '" to be an item of list.');

    Result := TExpectedValue.Create;
    Result.FActualValue := FActualValueStringList.Values[ExpectedItemName];
  finally
    Free;
  end;
end;


constructor TExpectedValue.Create;
begin
  FActualValue := 'some non-initialized string';
end;


procedure TExpectedValue.OfValue(ExpectedValue: string; ExtraMessage: string = '');
begin
  try
    ExpectStr(FActualValue, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectedValue.DifferentThanValue(ExpectedValue: string; ExtraMessage: string = '');
begin
  try
    ExpectStrDifferent(FActualValue, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectedValue.ToContain(ExpectedValue: string; ExtraMessage: string = ''); overload;
begin
  try
    ExpectInt(Ord(Pos(ExpectedValue, FActualValue) > 0), 1, ExtraMessage + '  Expected "' + ExpectedValue + '" to be a substring of ' + '"' + FActualValue + '"');
  finally
    Free;
  end;
end;



{TLoopedExpect}
constructor TLoopedExpect.Create;
begin
  inherited Create;
  FTimeout := 1000;

  FActualValue := nil;
  FActualValueInt := nil;
  FActualValueDWord := nil;
  FActualValueBoolean := nil;
  FActualValueStringCallbackFunc := nil;
  FActualValueIntegerCallbackFunc := nil;
  FActualValueDWordCallbackFunc := nil;
  FActualValueBooleanCallbackFunc := nil;
end;


procedure TLoopedExpect.UntypedValidator(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
const
  CValidPointerExpectedMessage = 'Expecting a valid pointer when calling LoopedExpect with ';
begin
  case AExpectDataType of
    edtString:
    begin
      if FActualValue <> nil then
        ExpectStr(FActualValue^, PString(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueStringCallbackFunc <> nil then
          ExpectStr(FActualValueStringCallbackFunc(), PString(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'string.  ' + ExtraMessage);
    end;

    edtInteger:
    begin
      if FActualValueInt <> nil then
        ExpectInt(FActualValueInt^, PInteger(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueIntegerCallbackFunc <> nil then
          ExpectInt(FActualValueIntegerCallbackFunc(), PInteger(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'Integer.  ' + ExtraMessage);
    end;

    edtDWord:
    begin
      if FActualValueDWord <> nil then
        ExpectDWord(FActualValueDWord^, PDWord(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueDWordCallbackFunc <> nil then
          ExpectDWord(FActualValueDWordCallbackFunc(), PDWord(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'DWord.  ' + ExtraMessage);
          //When calling LoopedExpect with Integer, and the DWord (overloaded) version of ToBe is used (by compiler), then please typecast the argument passed to ToBe, to DWord, in order to avoid the EExp.
    end;

    edtBoolean:
    begin
      if FActualValueBoolean <> nil then
        ExpectBoolean(FActualValueBoolean^, PBoolean(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueBooleanCallbackFunc <> nil then
          ExpectBoolean(FActualValueBooleanCallbackFunc(), PBoolean(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'Boolean.  ' + ExtraMessage);
    end;
    else
      raise EExp.Create('Unhandled datatype in UntypedValidator');
  end;
end;


procedure TLoopedExpect.UntypedValidatorDifferent(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
const
  CValidPointerExpectedMessage = 'Expecting a valid pointer when calling LoopedExpect with ';
begin
  case AExpectDataType of
    edtString:
    begin
      if FActualValue <> nil then
        ExpectStrDifferent(FActualValue^, PString(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueStringCallbackFunc <> nil then
          ExpectStrDifferent(FActualValueStringCallbackFunc(), PString(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'string.  ' + ExtraMessage);
    end;

    edtInteger:
    begin
      if FActualValueInt <> nil then
        ExpectIntDifferent(FActualValueInt^, PInteger(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueIntegerCallbackFunc <> nil then
          ExpectIntDifferent(FActualValueIntegerCallbackFunc(), PInteger(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'Integer.  ' + ExtraMessage);
    end;

    edtDWord:
    begin
      if FActualValueDWord <> nil then
        ExpectDWordDifferent(FActualValueDWord^, PDWord(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueDWordCallbackFunc <> nil then
          ExpectDWordDifferent(FActualValueDWordCallbackFunc(), PDWord(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'DWord.  ' + ExtraMessage);
          //When calling LoopedExpect with Integer, and the DWord (overloaded) version of ToBe is used (by compiler), then please typecast the argument passed to ToBe, to DWord, in order to avoid the EExp.
    end;

    edtBoolean:
    begin
      if FActualValueBoolean <> nil then
        ExpectBooleanDifferent(FActualValueBoolean^, PBoolean(ExpectedValue)^, ExtraMessage)
      else
        if FActualValueBooleanCallbackFunc <> nil then
          ExpectBooleanDifferent(FActualValueBooleanCallbackFunc(), PBoolean(ExpectedValue)^, ExtraMessage)
        else
          raise EExp.Create(CValidPointerExpectedMessage + 'Boolean.  ' + ExtraMessage);
    end;
    else
      raise EExp.Create('Unhandled datatype in UntypedValidatorDifferent');
  end;
end;


procedure TLoopedExpect.UntypedToBe(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
var
  tk: DWord;
begin
  try
    tk := GetTickCount64;
    repeat
      try
        UntypedValidator(ExpectedValue, AExpectDataType, ExtraMessage);
        Break;
      except
        if GetTickCount64 - tk > FTimeout then
          raise;
      end;

      Application.ProcessMessages;
      Sleep(1);
    until False;
  finally
    Free;
  end;
end;


procedure TLoopedExpect.UntypedNotToBe(ExpectedValue: Pointer; AExpectDataType: TExpectDataType; ExtraMessage: string = '');
var
  tk: DWord;
begin
  try
    tk := GetTickCount64;
    repeat
      Application.ProcessMessages;
      Sleep(1);
    until GetTickCount64 - tk > FTimeout;

    UntypedValidatorDifferent(ExpectedValue, AExpectDataType, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TLoopedExpect.ToBe(ExpectedValue: string; ExtraMessage: string = '');
begin
  UntypedToBe(@ExpectedValue, edtString, ExtraMessage);
end;


procedure TLoopedExpect.ToBe(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  UntypedToBe(@ExpectedValue, edtInteger, ExtraMessage);
end;


procedure TLoopedExpect.ToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
begin
  UntypedToBe(@ExpectedValue, edtDWord, ExtraMessage);
end;


procedure TLoopedExpect.ToBe(ExpectedValue: Boolean; ExtraMessage: string = '');
begin
  UntypedToBe(@ExpectedValue, edtBoolean, ExtraMessage);
end;


procedure TLoopedExpect.NotToBe(ExpectedValue: string; ExtraMessage: string = '');
begin
  UntypedNotToBe(@ExpectedValue, edtString, ExtraMessage);
end;


procedure TLoopedExpect.NotToBe(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  UntypedNotToBe(@ExpectedValue, edtInteger, ExtraMessage);
end;


procedure TLoopedExpect.NotToBe(ExpectedValue: DWord; ExtraMessage: string = ''); overload;
begin
  UntypedNotToBe(@ExpectedValue, edtDWord, ExtraMessage);
end;


procedure TLoopedExpect.NotToBe(ExpectedValue: Boolean; ExtraMessage: string = '');
begin
  UntypedNotToBe(@ExpectedValue, edtBoolean, ExtraMessage);
end;



function Expect(ActualValue: string): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValue := ActualValue;
  Result.FActualValueInt := StrToIntDef(ActualValue, MaxLongint);
  Result.FActualValueDWord := Result.FActualValueInt;
  Result.FActualValueBoolean := Result.FActualValueInt = $FF;
end;


function Expect(ActualValue: Integer): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValueInt := ActualValue;
  Result.FActualValueDWord := ActualValue;
  Result.FActualValue := IntToStr(ActualValue);
  Result.FActualValueBoolean := ActualValue = $FF;
end;


function Expect(ActualValue: DWord): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValueInt := ActualValue;
  Result.FActualValueDWord := ActualValue;
  Result.FActualValue := IntToStr(ActualValue);
  Result.FActualValueBoolean := ActualValue = $FF;
end;


function Expect(ActualValue: Boolean): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValueBoolean := ActualValue;
  Result.FActualValueInt := Integer(ActualValue);
  Result.FActualValueDWord := DWord(ActualValue);;
  Result.FActualValue := IntToStr(Ord(ActualValue));
end;


function Expect(ActualValue: TStringList): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValue := 'Expected "ToMatchContentOfStringArray" to be used.';
  Result.FActualValueStringList := ActualValue;
end;


function Expect(ActualValue: Pointer): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValuePointer := ActualValue;
  Result.FActualValue := '0x' + IntToHex(Int64(ActualValue));
end;


function Expect(ActualValue: PConstByteArr; ALen: Integer): TExpect; overload;
begin
  Result := TExpect.Create;

  if ActualValue = nil then
    raise EExp.Create('Actual pointer to array is nil.');

  SetLength(Result.FActualValueConstByteArr, ALen);
  Move(ActualValue^[0], Result.FActualValueConstByteArr[0], Length(Result.FActualValueConstByteArr));
end;


function LoopedExpect(ActualValue: PString; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValue := ActualValue;
end;


function LoopedExpect(ActualValue: PInteger; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueInt := ActualValue;
end;


function LoopedExpect(ActualValue: PDWord; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueDWord := ActualValue;
end;


function LoopedExpect(ActualValue: PBoolean; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueBoolean := ActualValue;
end;


function LoopedExpect(ActualValue: TStringCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueStringCallbackFunc := ActualValue;
end;


function LoopedExpect(ActualValue: TIntegerCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueIntegerCallbackFunc := ActualValue;
end;


function LoopedExpect(ActualValue: TDWordCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueDWordCallbackFunc := ActualValue;
end;


function LoopedExpect(ActualValue: TBooleanCallbackFunc; Timeout: Integer = 1000): TLoopedExpect; overload;
begin
  Result := TLoopedExpect.Create;
  Result.FTimeout := Timeout;

  Result.FActualValueBooleanCallbackFunc := ActualValue;
end;

end.

