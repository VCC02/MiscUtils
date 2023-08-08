{
    Copyright (C) 2022 VCC
    creation date: Aug 2023
    initial release date: 08 Aug 2023

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

unit ExpectationsDynArrays;

//{$mode ObjFPC}{$H+}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
interface

uses
  Classes, SysUtils, Expectations, DynArrays;

type
  TExpectDynArr = class
  private
    FActualValue1DByte: TDynArrayOfByte;
    FActualValue2DByte: TDynArrayOfTDynArrayOfByte;
    FActualValue3DByte: TDynArrayOfPDynArrayOfTDynArrayOfByte;
    FActualValueDWord: TDynArrayOfDWord;
    FActualValuePtrUInt: TDynArrayOfPtrUInt;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ToMatchContentOf(var ExpectedValue: TDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchContentOf(var ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchContentOf(var ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchContentOf(var ExpectedValue: TDynArrayOfDWord; ExtraMessage: string = ''); overload;
    procedure ToMatchContentOf(var ExpectedValue: TDynArrayOfPtrUInt; ExtraMessage: string = ''); overload;

    procedure ToMatchLengthOf(var ExpectedValue: TDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchLengthOf(var ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchLengthOf(var ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMessage: string = ''); overload;
    procedure ToMatchLengthOf(var ExpectedValue: TDynArrayOfDWord; ExtraMessage: string = ''); overload;
    procedure ToMatchLengthOf(var ExpectedValue: TDynArrayOfPtrUInt; ExtraMessage: string = ''); overload;
  end;


function Expect(var ActualValue: TDynArrayOfByte): TExpectDynArr; overload;
function Expect(var ActualValue: TDynArrayOfTDynArrayOfByte): TExpectDynArr; overload;
function Expect(var ActualValue: TDynArrayOfPDynArrayOfTDynArrayOfByte): TExpectDynArr; overload;
function Expect(var ActualValue: TDynArrayOfDWord): TExpectDynArr; overload;
function Expect(var ActualValue: TDynArrayOfPtrUInt): TExpectDynArr; overload;


implementation


procedure Expect1DLengthMatch(var ActualValue, ExpectedValue: TDynArrayOfByte; ExtraMsg: string = '');
begin
  if ExpectedValue.Len <> ActualValue.Len then
    raise Exception.Create('Expected (1D) array length to be "' + IntToStr(ExpectedValue.Len) + '", but is was "' + IntToStr(ActualValue.Len) + '".  ' + ExtraMsg);
end;


procedure Expect2DLengthMatch(var ActualValue, ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMsg: string = '');
begin
  if ExpectedValue.Len <> ActualValue.Len then
    raise Exception.Create('Expected (2D) array length to be "' + IntToStr(ExpectedValue.Len) + '", but is was "' + IntToStr(ActualValue.Len) + '".  ' + ExtraMsg);
end;


procedure Expect3DLengthMatch(var ActualValue, ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMsg: string = '');
begin
  if ExpectedValue.Len <> ActualValue.Len then
    raise Exception.Create('Expected (3D) array length to be "' + IntToStr(ExpectedValue.Len) + '", but is was "' + IntToStr(ActualValue.Len) + '".  ' + ExtraMsg);
end;


procedure ExpectDWordLengthMatch(var ActualValue, ExpectedValue: TDynArrayOfDWord; ExtraMsg: string = '');
begin
  if ExpectedValue.Len <> ActualValue.Len then
    raise Exception.Create('Expected (1D) array length to be "' + IntToStr(ExpectedValue.Len) + '", but is was "' + IntToStr(ActualValue.Len) + '".  ' + ExtraMsg);
end;


procedure ExpectPtrUIntLengthMatch(var ActualValue, ExpectedValue: TDynArrayOfPtrUInt; ExtraMsg: string = '');
begin
  if ExpectedValue.Len <> ActualValue.Len then
    raise Exception.Create('Expected (1D) array length to be "' + IntToStr(ExpectedValue.Len) + '", but is was "' + IntToStr(ActualValue.Len) + '".  ' + ExtraMsg);
end;

//---------------

procedure Expect1DByteMatch(var ActualValue, ExpectedValue: TDynArrayOfByte; ExtraMsg: string = '');
begin
  Expect1DLengthMatch(ActualValue, ExpectedValue, ExtraMsg);
  Expect(@ActualValue.Content^, ActualValue.Len).ToBe(@ExpectedValue.Content^, ExtraMsg);
end;


procedure Expect2DByteMatch(var ActualValue, ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMsg: string = '');
var
  i: Integer;
begin
  Expect2DLengthMatch(ActualValue, ExpectedValue, ExtraMsg);

  for i := 0 to LongInt(ExpectedValue.Len) - 1 do
    Expect1DByteMatch(ActualValue.Content^[i]^, ExpectedValue.Content^[i]^, ExtraMsg + '(at index ' + IntToStr(i) + ')');
end;


procedure Expect3DByteMatch(var ActualValue, ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMsg: string = '');
var
  i: Integer;
begin
  Expect3DLengthMatch(ActualValue, ExpectedValue, ExtraMsg);

  for i := 0 to LongInt(ExpectedValue.Len) - 1 do
    Expect2DByteMatch(ActualValue.Content^[i]^, ExpectedValue.Content^[i]^, ExtraMsg + '(at index ' + IntToStr(i) + ')');
end;


procedure ExpectDWordMatch(var ActualValue, ExpectedValue: TDynArrayOfDWord; ExtraMsg: string = '');
begin
  ExpectDWordLengthMatch(ActualValue, ExpectedValue, ExtraMsg);
  Expect(@ActualValue.Content^, ActualValue.Len shl 2).ToBe(@ExpectedValue.Content^, ExtraMsg);
end;


procedure ExpectPtrUIntMatch(var ActualValue, ExpectedValue: TDynArrayOfPtrUInt; ExtraMsg: string = '');
begin
  ExpectPtrUIntLengthMatch(ActualValue, ExpectedValue, ExtraMsg);
  Expect(@ActualValue.Content^, ActualValue.Len shl CArchBitShift).ToBe(@ExpectedValue.Content^, ExtraMsg);
end;

//---------------

constructor TExpectDynArr.Create;
begin
  inherited Create;

  InitDynArrayToEmpty(FActualValue1DByte);
  InitDynOfDynOfByteToEmpty(FActualValue2DByte);
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(FActualValue3DByte);
  InitDynArrayOfDWordToEmpty(FActualValueDWord);
  InitDynArrayOfPtrUIntToEmpty(FActualValuePtrUInt);
end;


destructor TExpectDynArr.Destroy;
begin
  FreeDynArray(FActualValue1DByte);
  FreeDynOfDynOfByteArray(FActualValue2DByte);
  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(FActualValue3DByte);
  FreeDynArrayOfDWord(FActualValueDWord);
  FreeDynArrayOfPtrUInt(FActualValuePtrUInt);

  inherited Destroy;
end;


procedure TExpectDynArr.ToMatchContentOf(var ExpectedValue: TDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect1DByteMatch(FActualValue1DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchContentOf(var ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect2DByteMatch(FActualValue2DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchContentOf(var ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect3DByteMatch(FActualValue3DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchContentOf(var ExpectedValue: TDynArrayOfDWord; ExtraMessage: string = ''); overload;
begin
  try
    ExpectDWordMatch(FActualValueDWord, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchContentOf(var ExpectedValue: TDynArrayOfPtrUInt; ExtraMessage: string = ''); overload;
begin
  try
    ExpectPtrUIntMatch(FActualValuePtrUInt, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;

//---------------

procedure TExpectDynArr.ToMatchLengthOf(var ExpectedValue: TDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect1DLengthMatch(FActualValue1DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchLengthOf(var ExpectedValue: TDynArrayOfTDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect2DLengthMatch(FActualValue2DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchLengthOf(var ExpectedValue: TDynArrayOfPDynArrayOfTDynArrayOfByte; ExtraMessage: string = '');
begin
  try
    Expect3DLengthMatch(FActualValue3DByte, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchLengthOf(var ExpectedValue: TDynArrayOfDWord; ExtraMessage: string = '');
begin
  try
    ExpectDWordLengthMatch(FActualValueDWord, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;


procedure TExpectDynArr.ToMatchLengthOf(var ExpectedValue: TDynArrayOfPtrUInt; ExtraMessage: string = '');
begin
  try
    ExpectPtrUIntLengthMatch(FActualValuePtrUInt, ExpectedValue, ExtraMessage);
  finally
    Free;
  end;
end;

//---------------

function Expect(var ActualValue: TDynArrayOfByte): TExpectDynArr;
begin
  Result := TExpectDynArr.Create;
  ConcatDynArrays(Result.FActualValue1DByte, ActualValue);
end;


function Expect(var ActualValue: TDynArrayOfTDynArrayOfByte): TExpectDynArr;
begin
  Result := TExpectDynArr.Create;
  ConcatDynOfDynOfByteArrays(Result.FActualValue2DByte, ActualValue);
end;


function Expect(var ActualValue: TDynArrayOfPDynArrayOfTDynArrayOfByte): TExpectDynArr;
begin
  Result := TExpectDynArr.Create;
  ConcatDynArraysOfPDynArrayOfTDynArrayOfByte(Result.FActualValue3DByte, ActualValue);
end;


function Expect(var ActualValue: TDynArrayOfDWord): TExpectDynArr;
begin
  Result := TExpectDynArr.Create;
  ConcatDynArraysOfDWord(Result.FActualValueDWord, ActualValue);
end;


function Expect(var ActualValue: TDynArrayOfPtrUInt): TExpectDynArr;
begin
  Result := TExpectDynArr.Create;
  ConcatDynArraysOfPtrUInt(Result.FActualValuePtrUInt, ActualValue);
end;

end.

