{
    Copyright (C) 2024 VCC
    creation date: 18 Apr 2024
    initial release date: 18 Apr 2024

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


unit TestDynArraysOfPtrUIntCase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, //Windows,
  DynArrays, Expectations;

type

  TTestDynArraysOfPtrUIntCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
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

    procedure TestDeleteFirstPtrUInts_ZeroLength;
    procedure TestDeleteFirstPtrUInts_LessThanLength;
    procedure TestDeleteFirstPtrUInts_LessThanLength_MoreItems;
    procedure TestDeleteFirstPtrUInts_SameAsLength;
    procedure TestDeleteFirstPtrUInts_GreaterThanLength;

    procedure TestCopyFromDynArray_HappyFlow;
    procedure TestCopyFromDynArray_0Length;
    procedure TestCopyFromDynArray_PartialOutOfContent;
    procedure TestCopyFromDynArray_CompletelyOutOfContent;
    procedure TestCopyFromDynArray_EmptySource;

    procedure TestDeleteFromDynArray_EmptyArray;
    procedure TestDeleteFromDynArray_IndexOutOfBounds;
    procedure TestDeleteFromDynArray_HappyFlow1;
    procedure TestDeleteFromDynArray_HappyFlow2;

    procedure TestDoubleFree;
  end;


implementation

{$IFDEF UsingDynTFT}
  uses
    MemManager;
{$ELSE}
  {$IFDEF UsingMPMM} //mP's memoy manager
    __Lib_MemManager  //users may still want to use a different flavor of the same memoy manager, without the DynTFT dependencies
  {$ELSE}
    //this is FP's memory manager
  {$ENDIF}
{$ENDIF}


const
  CUninitializedDynArrayErrMsg = 'The DynArray is not initialized. Please call InitDynArrayOfPtrUIntToEmpty before working with DynArray functions.';


procedure TTestDynArraysOfPtrUIntCase.SetUp;
begin
  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  {$IFDEF UsingMPMM}
    MM_Init;
  {$ENDIF}
end;


procedure TTestDynArraysOfPtrUIntCase.TearDown;
begin

end;


procedure TTestDynArraysOfPtrUIntCase.TestSimpleAllocation;
var
  Arr: TDynArrayOfPtrUInt;
  AllocationResult: Boolean;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynOfPtrUIntLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);
  finally
    FreeDynArrayOfPtrUInt(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestWritingToArray;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 20);
  try
    Arr.Content^[17] := 80;
    Expect(Arr.Content^[17]).ToBe(80);
  finally
    FreeDynArrayOfPtrUInt(Arr);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfPtrUInt;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  Expect(SetDynOfPtrUIntLength(Arr, 20)).ToBe(True);

  for i := 0 to DynOfPtrUIntLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  Expect(SetDynOfPtrUIntLength(Arr, 30)).ToBe(True, 'expecting successful reallocation');
  try
    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfPtrUInt(Arr);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfPtrUInt;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 20);

  for i := 0 to DynOfPtrUIntLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  SetDynOfPtrUIntLength(Arr, 10);
  try
    for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfPtrUInt(Arr);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestConcatDynArrays_HappyFlow;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr1);
  InitDynArrayOfPtrUIntToEmpty(Arr2);

  try
    AllocationResult := SetDynOfPtrUIntLength(Arr1, 20);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_20 should succeed.');
    for i := 0 to DynOfPtrUIntLength(Arr1) - 1 do
      Arr1.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'First allocation Result is overwritten.');

    AllocationResult := SetDynOfPtrUIntLength(Arr2, 15);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_15 should succeed.');
    for i := 0 to DynOfPtrUIntLength(Arr2) - 1 do
      Arr2.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Second allocation Result is overwritten.');

    AllocationResult := ConcatDynArraysOfPtrUInt(Arr1, Arr2);

    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Concat Result is overwritten or memory is full.');
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(35);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + i * 10));

    for i := 20 to 35 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + (i - 20) * 10));
  finally
    FreeDynArrayOfPtrUInt(Arr1);
    FreeDynArrayOfPtrUInt(Arr2);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestConcatDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr1);
  InitDynArrayOfPtrUIntToEmpty(Arr2);

  try
    AllocationResult := SetDynOfPtrUIntLength(Arr1, 20);
    for i := 0 to DynOfPtrUIntLength(Arr1) - 1 do
      Arr1.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfPtrUInt(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(20);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfPtrUInt(Arr1);
    FreeDynArrayOfPtrUInt(Arr2);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestConcatEmptyDynArray_WithValid;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr1);
  InitDynArrayOfPtrUIntToEmpty(Arr2);

  try
    AllocationResult := SetDynOfPtrUIntLength(Arr2, 15);
    for i := 0 to DynOfPtrUIntLength(Arr2) - 1 do
      Arr2.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfPtrUInt(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(15);

    for i := 0 to 15 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfPtrUInt(Arr1);
    FreeDynArrayOfPtrUInt(Arr2);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.TestConcatEmptyDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
  AllocationResult: Boolean;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr1);
  InitDynArrayOfPtrUIntToEmpty(Arr2);

  try
    AllocationResult := ConcatDynArraysOfPtrUInt(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(0);
  finally
    FreeDynArrayOfPtrUInt(Arr1);
    FreeDynArrayOfPtrUInt(Arr2);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.Test_CallDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfPtrUInt;
begin
  try
    DynOfPtrUIntLength(Arr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.Test_CallSetDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfPtrUInt;
begin
  try
    SetDynOfPtrUIntLength(Arr, 3);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfPtrUIntCase.Test_CallConcatDynArrays_WithoutFirstInitDynArray;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr2);
  SetDynOfPtrUIntLength(Arr2, 3);

  try
    ConcatDynArraysOfPtrUInt(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfPtrUInt(Arr2);
end;


procedure TTestDynArraysOfPtrUIntCase.Test_CallConcatDynArrays_WithoutSecondInitDynArray;
var
  Arr1, Arr2: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr1);
  SetDynOfPtrUIntLength(Arr1, 3);

  try
    ConcatDynArraysOfPtrUInt(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfPtrUInt(Arr1);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFirstPtrUInts_ZeroLength;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartPtrUIntsFromDynArray(0, Arr);

  Expect(Arr.Len).ToBe(3);
  Expect(Arr.Content^[0]).ToBe(30);
  Expect(Arr.Content^[1]).ToBe(40);
  Expect(Arr.Content^[2]).ToBe(50);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFirstPtrUInts_LessThanLength;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartPtrUIntsFromDynArray(2, Arr);

  Expect(Arr.Len).ToBe(1);
  Expect(Arr.Content^[0]).ToBe(50);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFirstPtrUInts_LessThanLength_MoreItems;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 8);
  Arr.Content^[0] := 2030;
  Arr.Content^[1] := 2040;
  Arr.Content^[2] := 2050;
  Arr.Content^[3] := 2060;
  Arr.Content^[4] := 2070;
  Arr.Content^[5] := 2080;
  Arr.Content^[6] := 2090;
  Arr.Content^[7] := 20100;

  RemoveStartPtrUIntsFromDynArray(2, Arr);

  Expect(Arr.Len).ToBe(6);
  Expect(Arr.Content^[0]).ToBe(2050);
  Expect(Arr.Content^[1]).ToBe(2060);
  Expect(Arr.Content^[2]).ToBe(2070);
  Expect(Arr.Content^[3]).ToBe(2080);
  Expect(Arr.Content^[4]).ToBe(2090);
  Expect(Arr.Content^[5]).ToBe(20100);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFirstPtrUInts_SameAsLength;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartPtrUIntsFromDynArray(3, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFirstPtrUInts_GreaterThanLength;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 3);
  RemoveStartPtrUIntsFromDynArray(7, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfPtrUInt(Arr);
end;


const
  //CSrcString_16bit = '01 23 45 67 89 AB CD EF GH IJ KL MN OP QR ST UV WX YZ ab cd ef gh ij kl mn op qr st uv wx yz';
  //CSrcString_32bit = '0123 4567 89AB CDEF GHIJ KLMN OPQR STUV WXYZ abcd efgh ijkl mnop qrst uvwx yz';
  //CSrcString_64bit = '01234567 89ABCDEF GHIJKLMN OPQRSTUV WXYZabcd efghijkl mnopqrst uvwxyz';

  CSrcString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

procedure TTestDynArraysOfPtrUIntCase.TestCopyFromDynArray_HappyFlow;
var
  Src, Dest: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Src);
  Expect(StringToDynArrayOfPtrUInt(CSrcString, Src)).ToBe(True);

  case SizeOf(PtrUInt) of
    2:  // 2 bytes    //PIC24 / dsPIC
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 3, 7);
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('6789ABCDEFGHIJ');
    end;

    4:  // 4 bytes
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 3, 7);
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('CDEFGHIJKLMNOPQRSTUVWXYZabcd');
    end;

    8:  // 8 bytes
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 3, 4);  //4 instead of 7, because the string is already too long
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('OPQRSTUVWXYZabcdefghijklmnopqrst');
    end;
  end;

  FreeDynArrayOfPtrUInt(Src);
  FreeDynArrayOfPtrUInt(Dest);
end;


procedure TTestDynArraysOfPtrUIntCase.TestCopyFromDynArray_0Length;
var
  Src, Dest: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Src);
  Expect(StringToDynArrayOfPtrUInt(CSrcString, Src)).ToBe(True);
  CopyFromDynArrayOfPtrUInt(Dest, Src, 3, 0);

  Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('');
  FreeDynArrayOfPtrUInt(Src);
  FreeDynArrayOfPtrUInt(Dest);
end;


procedure TTestDynArraysOfPtrUIntCase.TestCopyFromDynArray_PartialOutOfContent;
var
  Src, Dest: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Src);
  Expect(StringToDynArrayOfPtrUInt(CSrcString, Src)).ToBe(True);

  case SizeOf(PtrUInt) of
    2:  // 2 bytes   //PIC24 / dsPIC
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 20, 50);
      Expect(Dest.Len).ToBe(11); //  CopyFromDynArrayOfPtrUInt doesn't care about the content.
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('efghijklmnopqrstuvwxyz');
    end;

    4:  // 4 bytes
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 7, 20);
      Expect(Dest.Len).ToBe(9); //  CopyFromDynArrayOfPtrUInt doesn't care about the content.
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('STUVWXYZabcdefghijklmnopqrstuvwxyz');
    end;

    8:  // 8 bytes
    begin
      CopyFromDynArrayOfPtrUInt(Dest, Src, 3, 20);  //4 instead of 7, because the string is already too long
      Expect(Dest.Len).ToBe(5); //  CopyFromDynArrayOfPtrUInt doesn't care about the content.
      Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('OPQRSTUV WXYZabcd efghijkl mnopqrst uvwxyz');
    end;
  end;

  FreeDynArrayOfPtrUInt(Src);
  FreeDynArrayOfPtrUInt(Dest);
end;


procedure TTestDynArraysOfPtrUIntCase.TestCopyFromDynArray_CompletelyOutOfContent;
var
  Src, Dest: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Src);
  Expect(StringToDynArrayOfPtrUInt(CSrcString, Src)).ToBe(True);
  CopyFromDynArrayOfPtrUInt(Dest, Src, 50, 20);

  Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('');
  FreeDynArrayOfPtrUInt(Src);
  FreeDynArrayOfPtrUInt(Dest);
end;


procedure TTestDynArraysOfPtrUIntCase.TestCopyFromDynArray_EmptySource;
var
  Src, Dest: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Src);
  CopyFromDynArrayOfPtrUInt(Dest, Src, 0, 20);

  Expect(DynArrayOfPtrUIntToString(Dest)).ToBe('');
  FreeDynArrayOfPtrUInt(Src);
  FreeDynArrayOfPtrUInt(Dest);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFromDynArray_EmptyArray;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  DeleteItemFromDynArrayOfPtrUInt(Arr, 0);
  Expect(Arr.Len).ToBe(0);
  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFromDynArray_IndexOutOfBounds;
var
  Arr: TDynArrayOfPtrUInt;
  FoundException: Boolean;
begin
  FoundException := False;
  InitDynArrayOfPtrUIntToEmpty(Arr);
  try
    SetDynOfPtrUIntLength(Arr, 3);
    DeleteItemFromDynArrayOfPtrUInt(Arr, 3);
  except
    on E: Exception do
    begin
      FoundException := True;
      Expect(E.Message).ToBe('Delete index out of bounds in DeleteItemFromDynArrayOfPtrUInt.');
    end;
  end;

  Expect(FoundException).ToBe(True);
  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFromDynArray_HappyFlow1;
var
  Arr: TDynArrayOfPtrUInt;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);

  SetDynOfPtrUIntLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfPtrUInt(Arr, 3);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to 2 do
    Expect(Arr.Content^[i]).ToBe(i + 20);

  for i := 3 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDeleteFromDynArray_HappyFlow2;
var
  Arr: TDynArrayOfPtrUInt;
  i: Integer;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);

  SetDynOfPtrUIntLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfPtrUInt(Arr, 0);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfPtrUInt(Arr);
end;


procedure TTestDynArraysOfPtrUIntCase.TestDoubleFree;
var
  Arr: TDynArrayOfPtrUInt;
begin
  InitDynArrayOfPtrUIntToEmpty(Arr);
  SetDynOfPtrUIntLength(Arr, 3);

  FreeDynArrayOfPtrUInt(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynArrayOfPtrUInt(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


initialization

  RegisterTest(TTestDynArraysOfPtrUIntCase);
end.


