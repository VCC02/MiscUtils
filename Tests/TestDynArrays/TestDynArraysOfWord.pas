{
    Copyright (C) 2024 VCC
    creation date: 25 Feb 2024
    initial release date: 25 Feb 2024

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


unit TestDynArraysOfWord;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, //Windows,
  DynArrays, Expectations;

type

  TTestDynArraysOfWord = class(TTestCase)
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

    procedure TestDeleteFirstBytes_ZeroLength;
    procedure TestDeleteFirstBytes_LessThanLength;
    procedure TestDeleteFirstBytes_SameAsLength;
    procedure TestDeleteFirstBytes_GreaterThanLength;

    procedure TestCopyFromDynArray_HappyFlow;
    procedure TestCopyFromDynArray_0Length;
    procedure TestCopyFromDynArray_PartialOutOfContent;
    procedure TestCopyFromDynArray_CompletelyOutOfContent;
    procedure TestCopyFromDynArray_EmptySource;

    procedure TestDeleteFromDynArray_EmptyArray;
    procedure TestDeleteFromDynArray_IndexOutOfBounds;
    procedure TestDeleteFromDynArray_HappyFlow1;
    procedure TestDeleteFromDynArray_HappyFlow2;

    procedure TestCreateUniqueWord_HappyFlow1;
    procedure TestCreateUniqueWord_HappyFlow2;
    procedure TestCreateUniqueWord_HappyFlow3;
    procedure TestCreateUniqueWord_HappyFlow4;
    procedure TestCreateUniqueWord_HappyFlow5;
    procedure TestCreateUniqueWord_HappyFlow6;
    procedure TestCreateUniqueWord_FullArray1;
    procedure TestCreateUniqueWord_FullArray2;
    procedure TestCreateUniqueWord_FullArray3;
    procedure TestCreateUniqueWord_FullArray4;

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
  CUninitializedDynArrayErrMsg = 'The DynArray is not initialized. Please call InitDynArrayOfWordToEmpty before working with DynArray functions.';


procedure TTestDynArraysOfWord.SetUp;
begin
  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  {$IFDEF UsingMPMM}
    MM_Init;
  {$ENDIF}
end;


procedure TTestDynArraysOfWord.TearDown;
begin

end;


procedure TTestDynArraysOfWord.TestSimpleAllocation;
var
  Arr: TDynArrayOfWord;
  AllocationResult: Boolean;
begin
  InitDynArrayOfWordToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynOfWordLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);
  finally
    FreeDynArrayOfWord(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynArraysOfWord.TestWritingToArray;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 20);
  try
    Arr.Content^[17] := 80;
    Expect(Arr.Content^[17]).ToBe(80);
  finally
    FreeDynArrayOfWord(Arr);
  end;
end;


procedure TTestDynArraysOfWord.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);
  Expect(SetDynOfWordLength(Arr, 20)).ToBe(True);

  for i := 0 to DynOfWordLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  Expect(SetDynOfWordLength(Arr, 30)).ToBe(True, 'expecting successful reallocation');
  try
    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfWord(Arr);
  end;
end;


procedure TTestDynArraysOfWord.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 20);

  for i := 0 to DynOfWordLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  SetDynOfWordLength(Arr, 10);
  try
    for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfWord(Arr);
  end;
end;


procedure TTestDynArraysOfWord.TestConcatDynArrays_HappyFlow;
var
  Arr1, Arr2: TDynArrayOfWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr1);
  InitDynArrayOfWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfWordLength(Arr1, 20);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_20 should succeed.');
    for i := 0 to DynOfWordLength(Arr1) - 1 do
      Arr1.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'First allocation Result is overwritten.');

    AllocationResult := SetDynOfWordLength(Arr2, 15);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_15 should succeed.');
    for i := 0 to DynOfWordLength(Arr2) - 1 do
      Arr2.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Second allocation Result is overwritten.');

    AllocationResult := ConcatDynArraysOfWord(Arr1, Arr2);

    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Concat Result is overwritten or memory is full.');
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(35);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + i * 10));

    for i := 20 to 35 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + (i - 20) * 10));
  finally
    FreeDynArrayOfWord(Arr1);
    FreeDynArrayOfWord(Arr2);
  end;
end;


procedure TTestDynArraysOfWord.TestConcatDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr1);
  InitDynArrayOfWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfWordLength(Arr1, 20);
    for i := 0 to DynOfWordLength(Arr1) - 1 do
      Arr1.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(20);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfWord(Arr1);
    FreeDynArrayOfWord(Arr2);
  end;
end;


procedure TTestDynArraysOfWord.TestConcatEmptyDynArray_WithValid;
var
  Arr1, Arr2: TDynArrayOfWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr1);
  InitDynArrayOfWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfWordLength(Arr2, 15);
    for i := 0 to DynOfWordLength(Arr2) - 1 do
      Arr2.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(15);

    for i := 0 to 15 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfWord(Arr1);
    FreeDynArrayOfWord(Arr2);
  end;
end;


procedure TTestDynArraysOfWord.TestConcatEmptyDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfWord;
  AllocationResult: Boolean;
begin
  InitDynArrayOfWordToEmpty(Arr1);
  InitDynArrayOfWordToEmpty(Arr2);

  try
    AllocationResult := ConcatDynArraysOfWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(0);
  finally
    FreeDynArrayOfWord(Arr1);
    FreeDynArrayOfWord(Arr2);
  end;
end;


procedure TTestDynArraysOfWord.Test_CallDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfWord;
begin
  try
    DynOfWordLength(Arr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfWord.Test_CallSetDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfWord;
begin
  try
    SetDynOfWordLength(Arr, 3);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfWord.Test_CallConcatDynArrays_WithoutFirstInitDynArray;
var
  Arr1, Arr2: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr2);
  SetDynOfWordLength(Arr2, 3);

  try
    ConcatDynArraysOfWord(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfWord(Arr2);
end;


procedure TTestDynArraysOfWord.Test_CallConcatDynArrays_WithoutSecondInitDynArray;
var
  Arr1, Arr2: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr1);
  SetDynOfWordLength(Arr1, 3);

  try
    ConcatDynArraysOfWord(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfWord(Arr1);
end;


procedure TTestDynArraysOfWord.TestDeleteFirstBytes_ZeroLength;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartWordsFromDynArray(0, Arr);

  Expect(Arr.Len).ToBe(3);
  Expect(Arr.Content^[0]).ToBe(30);
  Expect(Arr.Content^[1]).ToBe(40);
  Expect(Arr.Content^[2]).ToBe(50);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFirstBytes_LessThanLength;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartWordsFromDynArray(2, Arr);

  Expect(Arr.Len).ToBe(1);
  Expect(Arr.Content^[0]).ToBe(50);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFirstBytes_SameAsLength;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartWordsFromDynArray(3, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFirstBytes_GreaterThanLength;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 3);
  RemoveStartWordsFromDynArray(7, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCopyFromDynArray_HappyFlow;
var
  Src, Dest: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Src);
  Expect(StringToDynArrayOfWord('0123456789ABCDEFGHIJKL', Src)).ToBe(True);
  CopyFromDynArrayOfWord(Dest, Src, 3, 7);

  Expect(DynArrayOfWordToString(Dest)).ToBe('6789ABCDEFGHIJ');
  FreeDynArrayOfWord(Src);
  FreeDynArrayOfWord(Dest);
end;


procedure TTestDynArraysOfWord.TestCopyFromDynArray_0Length;
var
  Src, Dest: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Src);
  Expect(StringToDynArrayOfWord('0123456789ABCDEF', Src)).ToBe(True);
  CopyFromDynArrayOfWord(Dest, Src, 3, 0);

  Expect(DynArrayOfWordToString(Dest)).ToBe('');
  FreeDynArrayOfWord(Src);
  FreeDynArrayOfWord(Dest);
end;


procedure TTestDynArraysOfWord.TestCopyFromDynArray_PartialOutOfContent;
var
  Src, Dest: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Src);
  Expect(StringToDynArrayOfWord('0123456789ABCDEF', Src)).ToBe(True);
  CopyFromDynArrayOfWord(Dest, Src, 5, 20);
  Expect(Dest.Len).ToBe(3); //  CopyFromDynArrayOfWord doesn't care about the content.

  Expect(DynArrayOfWordToString(Dest)).ToBe('ABCDEF');
  FreeDynArrayOfWord(Src);
  FreeDynArrayOfWord(Dest);
end;


procedure TTestDynArraysOfWord.TestCopyFromDynArray_CompletelyOutOfContent;
var
  Src, Dest: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Src);
  Expect(StringToDynArrayOfWord('0123456789ABCDEF', Src)).ToBe(True);
  CopyFromDynArrayOfWord(Dest, Src, 50, 20);

  Expect(DynArrayOfWordToString(Dest)).ToBe('');
  FreeDynArrayOfWord(Src);
  FreeDynArrayOfWord(Dest);
end;


procedure TTestDynArraysOfWord.TestCopyFromDynArray_EmptySource;
var
  Src, Dest: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Src);
  CopyFromDynArrayOfWord(Dest, Src, 0, 20);

  Expect(DynArrayOfWordToString(Dest)).ToBe('');
  FreeDynArrayOfWord(Src);
  FreeDynArrayOfWord(Dest);
end;


procedure TTestDynArraysOfWord.TestDeleteFromDynArray_EmptyArray;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  DeleteItemFromDynArrayOfWord(Arr, 0);
  Expect(Arr.Len).ToBe(0);
  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFromDynArray_IndexOutOfBounds;
var
  Arr: TDynArrayOfWord;
  FoundException: Boolean;
begin
  FoundException := False;
  InitDynArrayOfWordToEmpty(Arr);
  try
    SetDynOfWordLength(Arr, 3);
    DeleteItemFromDynArrayOfWord(Arr, 3);
  except
    on E: Exception do
    begin
      FoundException := True;
      Expect(E.Message).ToBe('Delete index out of bounds in DeleteItemFromDynArrayOfWord.');
    end;
  end;

  Expect(FoundException).ToBe(True);
  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFromDynArray_HappyFlow1;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  SetDynOfWordLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfWord(Arr, 3);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to 2 do
    Expect(Arr.Content^[i]).ToBe(i + 20);

  for i := 3 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDeleteFromDynArray_HappyFlow2;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  SetDynOfWordLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfWord(Arr, 0);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow1;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);

  //SetDynOfWordLength(Arr, 0);
  Expect(CreateUniqueWord(Arr)).ToBe(0);
  Expect(Arr.Len).ToBe(1);
  Expect(Arr.Content^[0]).ToBe(0);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow2;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);

  SetDynOfWordLength(Arr, 1);
  Arr.Content^[0] := 0;
  Expect(CreateUniqueWord(Arr)).ToBe(1);
  Expect(Arr.Len).ToBe(2);
  Expect(Arr.Content^[0]).ToBe(0);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow3;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);

  SetDynOfWordLength(Arr, 2);
  Arr.Content^[0] := 0;
  Arr.Content^[1] := 2;
  Expect(CreateUniqueWord(Arr)).ToBe(3);
  Expect(Arr.Len).ToBe(3);

  Expect(Arr.Content^[0]).ToBe(0);
  Expect(Arr.Content^[1]).ToBe(2);
  Expect(Arr.Content^[2]).ToBe(3);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow4;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);

  SetDynOfWordLength(Arr, 3);
  Arr.Content^[0] := 0;
  Arr.Content^[1] := 2;
  Arr.Content^[2] := 3;
  Expect(CreateUniqueWord(Arr)).ToBe(4);
  Expect(Arr.Len).ToBe(4);

  Expect(Arr.Content^[0]).ToBe(0);
  Expect(Arr.Content^[1]).ToBe(2);
  Expect(Arr.Content^[2]).ToBe(3);
  Expect(Arr.Content^[3]).ToBe(4);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow5;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65534)).ToBe(True, 'Increase MaxMM value in case of an error.');
  for i := 0 to 65534 - 1 do
    Arr.Content^[i] := i;

  Expect(CreateUniqueWord(Arr)).ToBe(65534);
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to 65534 - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_HappyFlow6;  //the search wraps around to find a free number
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65534)).ToBe(True);
  for i := 0 to 10 - 1 do
    Arr.Content^[i] := i;

  for i := 10 to 65534 - 1 do
    Arr.Content^[i] := i + 1;

  Expect(CreateUniqueWord(Arr)).ToBe(10);
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to 10 - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  for i := 10 to 65534 - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 1);

  Expect(Arr.Content^[65534]).ToBe(10);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_FullArray1;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65535)).ToBe(True);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i;

  Expect(CreateUniqueWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_FullArray2;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65535)).ToBe(True);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := 0;

  Expect(CreateUniqueWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(0);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_FullArray3;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65540)).ToBe(True);  //The array has more numbers than the max value CreateUniqueWord can create.
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i;

  Expect(CreateUniqueWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65540);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i and $FFFF);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestCreateUniqueWord_FullArray4;
var
  Arr: TDynArrayOfWord;
  i: Integer;
begin
  InitDynArrayOfWordToEmpty(Arr);

  Expect(SetDynOfWordLength(Arr, 65540)).ToBe(True); //The array has more numbers than the max value CreateUniqueWord can create.
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := 3;

  Expect(CreateUniqueWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65540);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(3);

  FreeDynArrayOfWord(Arr);
end;


procedure TTestDynArraysOfWord.TestDoubleFree;
var
  Arr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(Arr);
  SetDynOfWordLength(Arr, 3);

  FreeDynArrayOfWord(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynArrayOfWord(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


initialization

  RegisterTest(TTestDynArraysOfWord);
end.


