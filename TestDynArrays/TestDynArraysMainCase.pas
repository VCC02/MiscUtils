{
    Copyright (C) 2023 VCC
    creation date: Apr 2023
    initial release date: 23 Apr 2023

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


unit TestDynArraysMainCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  DynArrays, Expectations;

type

  TTestDynArrays= class(TTestCase)
  published
    procedure TestSimpleAllocation;
    procedure TestWritingToArray;
    procedure TestReallocationToLargerArray;
    procedure TestReallocationToSmallerArray;

    procedure TestConcatDynArrays_HappyFlow;
    procedure TestConcatDynArray_WithEmpty;
    procedure TestConcatEmptyDynArray_WithValid;
    procedure TestConcatEmptyDynArray_WithEmpty;

    procedure Test_CallDynLength_WithoutInitDynArray;
    procedure Test_CallSetDynLength_WithoutInitDynArray;
    procedure Test_CallConcatDynArrays_WithoutFirstInitDynArray;
    procedure Test_CallConcatDynArrays_WithoutSecondInitDynArray;

    procedure TestDoubleFree;
  end;


implementation


const
  CUninitializedDynArrayErrMsg = 'The DynArray is not initialized. Please call InitDynArrayToEmpty before working with DynArray functions.';


procedure TTestDynArrays.TestSimpleAllocation;
var
  Arr: TDynArrayOfByte;
  AllocationResult: Boolean;
begin
  InitDynArrayToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);
  finally
    FreeDynArray(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynArrays.TestWritingToArray;
var
  Arr: TDynArrayOfByte;
begin
  InitDynArrayToEmpty(Arr);
  SetDynLength(Arr, 20);
  try
    Arr.Content^[17] := 80;
    Expect(Arr.Content^[17]).ToBe(80);
  finally
    FreeDynArray(Arr);
  end;
end;


procedure TTestDynArrays.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayToEmpty(Arr);
  SetDynLength(Arr, 20);

  for i := 0 to DynLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  SetDynLength(Arr, 30);
  try
    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(i * 10);
  finally
    FreeDynArray(Arr);
  end;
end;


procedure TTestDynArrays.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayToEmpty(Arr);
  SetDynLength(Arr, 20);

  for i := 0 to DynLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  SetDynLength(Arr, 10);
  try
    for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(i * 10);
  finally
    FreeDynArray(Arr);
  end;
end;


procedure TTestDynArrays.TestConcatDynArrays_HappyFlow;
var
  Arr1, Arr2: TDynArrayOfByte;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayToEmpty(Arr1);
  InitDynArrayToEmpty(Arr2);

  try
    AllocationResult := SetDynLength(Arr1, 20);
    for i := 0 to DynLength(Arr1) - 1 do
      Arr1.Content^[i] := i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'First allocation Result is overwritten.');

    AllocationResult := SetDynLength(Arr2, 15);
    for i := 0 to DynLength(Arr2) - 1 do
      Arr2.Content^[i] := i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Second allocation Result is overwritten.');

    AllocationResult := ConcatDynArrays(Arr1, Arr2);

    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Concat Result is overwritten.');
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(35);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(i * 10);

    for i := 20 to 35 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe((i - 20) * 10);
  finally
    FreeDynArray(Arr1);
    FreeDynArray(Arr2);
  end;
end;


procedure TTestDynArrays.TestConcatDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfByte;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayToEmpty(Arr1);
  InitDynArrayToEmpty(Arr2);

  try
    AllocationResult := SetDynLength(Arr1, 20);
    for i := 0 to DynLength(Arr1) - 1 do
      Arr1.Content^[i] := i * 10;

    AllocationResult := ConcatDynArrays(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(20);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(i * 10);
  finally
    FreeDynArray(Arr1);
    FreeDynArray(Arr2);
  end;
end;


procedure TTestDynArrays.TestConcatEmptyDynArray_WithValid;
var
  Arr1, Arr2: TDynArrayOfByte;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayToEmpty(Arr1);
  InitDynArrayToEmpty(Arr2);

  try
    AllocationResult := SetDynLength(Arr2, 15);
    for i := 0 to DynLength(Arr2) - 1 do
      Arr2.Content^[i] := i * 10;

    AllocationResult := ConcatDynArrays(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(15);

    for i := 0 to 15 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(i * 10);
  finally
    FreeDynArray(Arr1);
    FreeDynArray(Arr2);
  end;
end;


procedure TTestDynArrays.TestConcatEmptyDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfByte;
  AllocationResult: Boolean;
begin
  InitDynArrayToEmpty(Arr1);
  InitDynArrayToEmpty(Arr2);

  try
    AllocationResult := ConcatDynArrays(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(0);
  finally
    FreeDynArray(Arr1);
    FreeDynArray(Arr2);
  end;
end;


procedure TTestDynArrays.Test_CallDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfByte;
begin
  try
    DynLength(Arr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArrays.Test_CallSetDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfByte;
begin
  try
    SetDynLength(Arr, 3);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArrays.Test_CallConcatDynArrays_WithoutFirstInitDynArray;
var
  Arr1, Arr2: TDynArrayOfByte;
begin
  InitDynArrayToEmpty(Arr2);
  SetDynLength(Arr2, 3);

  try
    ConcatDynArrays(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArray(Arr2);
end;


procedure TTestDynArrays.Test_CallConcatDynArrays_WithoutSecondInitDynArray;
var
  Arr1, Arr2: TDynArrayOfByte;
begin
  InitDynArrayToEmpty(Arr1);
  SetDynLength(Arr1, 3);

  try
    ConcatDynArrays(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArray(Arr1);
end;


procedure TTestDynArrays.TestDoubleFree;
var
  Arr: TDynArrayOfByte;
begin
  InitDynArrayToEmpty(Arr);
  SetDynLength(Arr, 3);

  FreeDynArray(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynArray(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


initialization

  RegisterTest(TTestDynArrays);
end.

