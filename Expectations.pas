{
    Copyright (C) 2022 VCC
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

interface

uses
  Classes, SysUtils;


type
  TStrArr = array[0..0] of string;
  PStrArr = ^TStrArr;

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
    FActualValueStringList: TStringList;
    FActualValuePointer: Pointer;
    FActualValueBoolean: Boolean;
  public
    constructor Create;

    procedure ToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure ToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;
    procedure ToBeGreaterThan(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeGreaterThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeLessThan(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeLessThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
    procedure ToBeAssigned(ExtraMessage: string = ''); overload;  //verifies FActualValueStringList
    procedure ToMatchContentOfStringArray(ExpectedValue: PStrArr; ExpectedLength: Integer; ExtraMessage: string = '');
    procedure NotToBe(ExpectedValue: string; ExtraMessage: string = ''); overload;
    procedure NotToBe(ExpectedValue: Integer; ExtraMessage: string = ''); overload;
    procedure ToContain(ExpectedValue: string; ExtraMessage: string = ''); overload;
    function WithItem(ExpectedItemName: string; ExtraMessage: string = ''): TExpectedValue; overload;
  end;




function GetTextFileContent(APath: string): string;

function Expect(ActualValue: string): TExpect; overload;
function Expect(ActualValue: Integer): TExpect; overload;
function Expect(ActualValue: Boolean): TExpect; overload;
function Expect(ActualValue: TStringList): TExpect; overload;
function Expect(ActualValue: Pointer): TExpect; overload;


implementation


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
    raise Exception.Create('Expected ' + IntToStr(ExpectedValue) + ', but is was ' + IntToStr(ActualValue) + '.  ' + ExtraMsg);
end;


procedure ExpectStr(ActualValue, ExpectedValue: string; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise Exception.Create('Expected "' + ExpectedValue + '", but is was "' + ActualValue + '".  ' + ExtraMsg);
end;


procedure ExpectAssigned(ActualValue: Pointer; ExtraMsg: string = '');
const
  CDigitCount: array[Boolean] of Byte = (8, 16);
var
  DigitCount: Byte;
begin
  DigitCount := CDigitCount[SizeOf(Pointer) = 8];
  if not Assigned(ActualValue) then
    raise Exception.Create('Expected pointer/object at' + IntToHex(Int64(ActualValue), DigitCount) + ', to be assigned.  ' + ExtraMsg);
end;


procedure ExpectBoolean(ActualValue, ExpectedValue: Boolean; ExtraMsg: string = '');
begin
  if ExpectedValue <> ActualValue then
    raise Exception.Create('Expected ' + BoolToStr(ExpectedValue, 'True', 'False') + ', but is was ' + BoolToStr(ActualValue, 'True', 'False') + '.  ' + ExtraMsg);
end;


procedure ExpectIntDifferent(ActualValue, ExpectedValue: Integer; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
    raise Exception.Create('Expected ' + IntToStr(ExpectedValue) + ' to be different than ' + IntToStr(ActualValue) + ', but they are the same.  ' + ExtraMsg);
end;


procedure ExpectStrDifferent(ActualValue, ExpectedValue: string; ExtraMsg: string = '');
begin
  if ExpectedValue = ActualValue then
    raise Exception.Create('Expected "' + ExpectedValue + '" to be different than "' + ActualValue + '", but they are the same.  ' + ExtraMsg);
end;


constructor TExpect.Create;
begin
  FActualValue := '';
  FActualValueInt := -22; //some unusual, undefault value
  FActualValueStringList := nil;
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


procedure TExpect.ToBe(ExpectedValue: Boolean; ExtraMessage: string = ''); overload;
begin
  try
    ExpectBoolean(FActualValueBoolean, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeGreaterThan(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    ExpectInt(Ord(FActualValueInt > ExpectedValue), 1, IntToStr(FActualValueInt) + ' is not greater than ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeGreaterThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    ExpectInt(Ord(FActualValueInt >= ExpectedValue), 1, IntToStr(FActualValueInt) + ' is not greater than or equal to ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeLessThan(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    ExpectInt(Ord(FActualValueInt < ExpectedValue), 1, IntToStr(FActualValueInt) + ' is not less than ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpect.ToBeLessThanOrEqualTo(ExpectedValue: Integer; ExtraMessage: string = '');
begin
  try
    ExpectInt(Ord(FActualValueInt <= ExpectedValue), 1, IntToStr(FActualValueInt) + ' is not less than or equal to ' + IntToStr(ExpectedValue) + '.  ' + ExtraMessage);
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
      raise Exception.Create('Expected "' + ExpectedValue^[i] + '", but is was "' + FActualValueStringList.Strings[i] + '"  at index ' + IntToStr(i) + '  ' + ExtraMessage);
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
    Expect(FActualValueStringList).ToBeAssigned(ExtraMessage);
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


function Expect(ActualValue: string): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValue := ActualValue;
  Result.FActualValueInt := StrToIntDef(ActualValue, MaxLongint);
  Result.FActualValueBoolean := Result.FActualValueInt = $FF;
end;


function Expect(ActualValue: Integer): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValueInt := ActualValue;
  Result.FActualValue := IntToStr(ActualValue);
  Result.FActualValueBoolean := ActualValue = $FF;
end;


function Expect(ActualValue: Boolean): TExpect; overload;
begin
  Result := TExpect.Create;
  Result.FActualValueBoolean := ActualValue;
  Result.FActualValueInt := Integer(ActualValue);
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


end.

