{
    Copyright (C) 2024 VCC
    creation date: 21 Apr 2024  - copied from TestDynArraysOfWord.pas
    initial release date: 21 Apr 2024

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


unit TestDynArraysOfDWord;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, //Windows,
  DynArrays, Expectations;

type

  TTestDynArraysOfDWordCase = class(TTestCase)
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

    procedure TestDeleteFirstDWords_ZeroLength;
    procedure TestDeleteFirstDWords_LessThanLength;
    procedure TestDeleteFirstDWords_LessThanLength_MoreItems;
    procedure TestDeleteFirstDWords_SameAsLength;
    procedure TestDeleteFirstDWords_GreaterThanLength;

    procedure TestCopyFromDynArray_HappyFlow;
    procedure TestCopyFromDynArray_0Length;
    procedure TestCopyFromDynArray_PartialOutOfContent;
    procedure TestCopyFromDynArray_PartialOutOfContent2;
    procedure TestCopyFromDynArray_CompletelyOutOfContent;
    procedure TestCopyFromDynArray_EmptySource;

    procedure TestDeleteFromDynArray_EmptyArray;
    procedure TestDeleteFromDynArray_IndexOutOfBounds;
    procedure TestDeleteFromDynArray_HappyFlow1;
    procedure TestDeleteFromDynArray_HappyFlow2;

    procedure TestCreateUniqueDWord_HappyFlow1;
    procedure TestCreateUniqueDWord_HappyFlow2;
    procedure TestCreateUniqueDWord_HappyFlow3;
    procedure TestCreateUniqueDWord_HappyFlow4;
    procedure TestCreateUniqueDWord_HappyFlow5;
    procedure TestCreateUniqueDWord_HappyFlow6;
    procedure TestCreateUniqueDWord_FullArray1;
    procedure TestCreateUniqueDWord_FullArray2;
    procedure TestCreateUniqueDWord_FullArray3;
    procedure TestCreateUniqueDWord_FullArray4;

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
  CUninitializedDynArrayErrMsg = 'The DynArray is not initialized. Please call InitDynArrayOfDWordToEmpty before working with DynArray functions.';


procedure TTestDynArraysOfDWordCase.SetUp;
begin
  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  {$IFDEF UsingMPMM}
    MM_Init;
  {$ENDIF}
end;


procedure TTestDynArraysOfDWordCase.TearDown;
begin

end;


procedure TTestDynArraysOfDWordCase.TestSimpleAllocation;
var
  Arr: TDynArrayOfDWord;
  AllocationResult: Boolean;
begin
  InitDynArrayOfDWordToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynOfDWordLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);
  finally
    FreeDynArrayOfDWord(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynArraysOfDWordCase.TestWritingToArray;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 20);
  try
    Arr.Content^[17] := 80;
    Expect(Arr.Content^[17]).ToBe(80);
  finally
    FreeDynArrayOfDWord(Arr);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfDWord;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  Expect(SetDynOfDWordLength(Arr, 20)).ToBe(True);

  for i := 0 to DynOfDWordLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  Expect(SetDynOfDWordLength(Arr, 30)).ToBe(True, 'expecting successful reallocation');
  try
    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfDWord(Arr);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfDWord;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 20);

  for i := 0 to DynOfDWordLength(Arr) - 1 do
    Arr.Content^[i] := i * 10;

  SetDynOfDWordLength(Arr, 10);
  try
    for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfDWord(Arr);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestConcatDynArrays_HappyFlow;
var
  Arr1, Arr2: TDynArrayOfDWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr1);
  InitDynArrayOfDWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfDWordLength(Arr1, 20);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_20 should succeed.');
    for i := 0 to DynOfDWordLength(Arr1) - 1 do
      Arr1.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'First allocation Result is overwritten.');

    AllocationResult := SetDynOfDWordLength(Arr2, 15);
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Allocation_15 should succeed.');
    for i := 0 to DynOfDWordLength(Arr2) - 1 do
      Arr2.Content^[i] := 1234 + i * 10;
    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Second allocation Result is overwritten.');

    AllocationResult := ConcatDynArraysOfDWord(Arr1, Arr2);

    Expect(Byte(AllocationResult)).ToBe(Byte(True), 'Concat Result is overwritten or memory is full.');
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(35);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + i * 10));

    for i := 20 to 35 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(1234 + (i - 20) * 10));
  finally
    FreeDynArrayOfDWord(Arr1);
    FreeDynArrayOfDWord(Arr2);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestConcatDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfDWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr1);
  InitDynArrayOfDWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfDWordLength(Arr1, 20);
    for i := 0 to DynOfDWordLength(Arr1) - 1 do
      Arr1.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfDWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(20);

    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfDWord(Arr1);
    FreeDynArrayOfDWord(Arr2);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestConcatEmptyDynArray_WithValid;
var
  Arr1, Arr2: TDynArrayOfDWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr1);
  InitDynArrayOfDWordToEmpty(Arr2);

  try
    AllocationResult := SetDynOfDWordLength(Arr2, 15);
    for i := 0 to DynOfDWordLength(Arr2) - 1 do
      Arr2.Content^[i] := i * 10;

    AllocationResult := ConcatDynArraysOfDWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(15);

    for i := 0 to 15 - 1 do  //test up to the old length, as this content has to be valid only
      Expect(Arr1.Content^[i]).ToBe(DWord(i * 10));
  finally
    FreeDynArrayOfDWord(Arr1);
    FreeDynArrayOfDWord(Arr2);
  end;
end;


procedure TTestDynArraysOfDWordCase.TestConcatEmptyDynArray_WithEmpty;
var
  Arr1, Arr2: TDynArrayOfDWord;
  AllocationResult: Boolean;
begin
  InitDynArrayOfDWordToEmpty(Arr1);
  InitDynArrayOfDWordToEmpty(Arr2);

  try
    AllocationResult := ConcatDynArraysOfDWord(Arr1, Arr2);
    Expect(AllocationResult).ToBe(True);
    Expect(Arr1.Len).ToBe(0);
  finally
    FreeDynArrayOfDWord(Arr1);
    FreeDynArrayOfDWord(Arr2);
  end;
end;


procedure TTestDynArraysOfDWordCase.Test_CallDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfDWord;
begin
  try
    DynOfDWordLength(Arr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfDWordCase.Test_CallSetDynLength_WithoutInitDynArray;
var
  Arr: TDynArrayOfDWord;
begin
  try
    SetDynOfDWordLength(Arr, 3);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;
end;


procedure TTestDynArraysOfDWordCase.Test_CallConcatDynArrays_WithoutFirstInitDynArray;
var
  Arr1, Arr2: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr2);
  SetDynOfDWordLength(Arr2, 3);

  try
    ConcatDynArraysOfDWord(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfDWord(Arr2);
end;


procedure TTestDynArraysOfDWordCase.Test_CallConcatDynArrays_WithoutSecondInitDynArray;
var
  Arr1, Arr2: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr1);
  SetDynOfDWordLength(Arr1, 3);

  try
    ConcatDynArraysOfDWord(Arr1, Arr2);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg);
  end;

  FreeDynArrayOfDWord(Arr1);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFirstDWords_ZeroLength;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartDWordsFromDynArray(0, Arr);

  Expect(Arr.Len).ToBe(3);
  Expect(Arr.Content^[0]).ToBe(30);
  Expect(Arr.Content^[1]).ToBe(40);
  Expect(Arr.Content^[2]).ToBe(50);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFirstDWords_LessThanLength;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartDWordsFromDynArray(2, Arr);

  Expect(Arr.Len).ToBe(1);
  Expect(Arr.Content^[0]).ToBe(50);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFirstDWords_LessThanLength_MoreItems;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 8);
  Arr.Content^[0] := 2030;
  Arr.Content^[1] := 2040;
  Arr.Content^[2] := 2050;
  Arr.Content^[3] := 2060;
  Arr.Content^[4] := 2070;
  Arr.Content^[5] := 2080;
  Arr.Content^[6] := 2090;
  Arr.Content^[7] := 20100;

  RemoveStartDWordsFromDynArray(2, Arr);

  Expect(Arr.Len).ToBe(6);
  Expect(Arr.Content^[0]).ToBe(2050);
  Expect(Arr.Content^[1]).ToBe(2060);
  Expect(Arr.Content^[2]).ToBe(2070);
  Expect(Arr.Content^[3]).ToBe(2080);
  Expect(Arr.Content^[4]).ToBe(2090);
  Expect(Arr.Content^[5]).ToBe(20100);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFirstDWords_SameAsLength;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 3);
  Arr.Content^[0] := 30;
  Arr.Content^[1] := 40;
  Arr.Content^[2] := 50;

  RemoveStartDWordsFromDynArray(3, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFirstDWords_GreaterThanLength;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 3);
  RemoveStartDWordsFromDynArray(7, Arr);

  Expect(Arr.Len).ToBe(0);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_HappyFlow;
var
  Src, Dest: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Src);
  Expect(StringToDynArrayOfDWord('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdef', Src)).ToBe(True);
  CopyFromDynArrayOfDWord(Dest, Src, 3, 7);

  Expect(DynArrayOfDWordToString(Dest)).ToBe('CDEFGHIJKLMNOPQRSTUVWXYZabcd');
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_0Length;
var
  Src, Dest: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Src);
  Expect(StringToDynArrayOfDWord('0123456789ABCDEF', Src)).ToBe(True);
  CopyFromDynArrayOfDWord(Dest, Src, 3, 0);

  Expect(DynArrayOfDWordToString(Dest)).ToBe('');
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_PartialOutOfContent;
var
  Src, Dest: TDynArrayOfDWord;
begin
  //  0123 4567 89AB CDEF GHIJ KLMN OPQR STUV WXYZ abcd efgh ijkl mno
  InitDynArrayOfDWordToEmpty(Src);
  Expect(StringToDynArrayOfDWord('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmno', Src)).ToBe(True);
  Expect(Src.Len).ToBe(13, 'src len');
  Expect(Src.Content^[12]).ToBe(Ord('m') shl 0 + Ord('n') shl 8 + Ord('o') shl 16 + 0 shl 24, 'src content');  //6F6E6D (onm)

  CopyFromDynArrayOfDWord(Dest, Src, 5, 20);
  Expect(Dest.Len).ToBe(8); //  CopyFromDynArrayOfDWord doesn't care about the content.

  Expect(DynArrayOfDWordToString(Dest)).ToBe('KLMNOPQRSTUVWXYZabcdefghijklmno' + #0);
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_PartialOutOfContent2;
var
  Src, Dest: TDynArrayOfDWord;
begin
  //  0123 4567 89AB CDEF GHIJ KLMN OPQR STUV WXYZ abcd efgh ijkl m
  InitDynArrayOfDWordToEmpty(Src);
  Expect(StringToDynArrayOfDWord('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklm', Src)).ToBe(True);
  Expect(Src.Len).ToBe(13, 'src len');
  Expect(Src.Content^[12]).ToBe(Ord('m') shl 0 + 0 shl 8 + 0 shl 16 + 0 shl 24, 'src content');  //00006D (onm)

  CopyFromDynArrayOfDWord(Dest, Src, 5, 20);
  Expect(Dest.Len).ToBe(8); //  CopyFromDynArrayOfDWord doesn't care about the content.

  Expect(DynArrayOfDWordToString(Dest)).ToBe('KLMNOPQRSTUVWXYZabcdefghijklm' + #0#0#0);
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_CompletelyOutOfContent;
var
  Src, Dest: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Src);
  Expect(StringToDynArrayOfDWord('0123456789ABCDEF', Src)).ToBe(True);
  CopyFromDynArrayOfDWord(Dest, Src, 50, 20);

  Expect(DynArrayOfDWordToString(Dest)).ToBe('');
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestCopyFromDynArray_EmptySource;
var
  Src, Dest: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Src);
  CopyFromDynArrayOfDWord(Dest, Src, 0, 20);

  Expect(DynArrayOfDWordToString(Dest)).ToBe('');
  FreeDynArrayOfDWord(Src);
  FreeDynArrayOfDWord(Dest);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFromDynArray_EmptyArray;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  DeleteItemFromDynArrayOfDWord(Arr, 0);
  Expect(Arr.Len).ToBe(0);
  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFromDynArray_IndexOutOfBounds;
var
  Arr: TDynArrayOfDWord;
  FoundException: Boolean;
begin
  FoundException := False;
  InitDynArrayOfDWordToEmpty(Arr);
  try
    SetDynOfDWordLength(Arr, 3);
    DeleteItemFromDynArrayOfDWord(Arr, 3);
  except
    on E: Exception do
    begin
      FoundException := True;
      Expect(E.Message).ToBe('Delete index out of bounds in DeleteItemFromDynArrayOfDWord.');
    end;
  end;

  Expect(FoundException).ToBe(True);
  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFromDynArray_HappyFlow1;
var
  Arr: TDynArrayOfDWord;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  SetDynOfDWordLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfDWord(Arr, 3);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to 2 do
    Expect(Arr.Content^[i]).ToBe(i + 20);

  for i := 3 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDeleteFromDynArray_HappyFlow2;
var
  Arr: TDynArrayOfDWord;
  i: Integer;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  SetDynOfDWordLength(Arr, 7);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i + 20;

  DeleteItemFromDynArrayOfDWord(Arr, 0);

  Expect(Arr.Len).ToBe(6);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 20 + 1);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow1;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  //SetDynOfDWordLength(Arr, 0);
  Expect(CreateUniqueDWord(Arr)).ToBe(0);
  Expect(Arr.Len).ToBe(1);
  Expect(Arr.Content^[0]).ToBe(0);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow2;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  SetDynOfDWordLength(Arr, 1);
  Arr.Content^[0] := 0;
  Expect(CreateUniqueDWord(Arr)).ToBe(1);
  Expect(Arr.Len).ToBe(2);
  Expect(Arr.Content^[0]).ToBe(0);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow3;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  SetDynOfDWordLength(Arr, 2);
  Arr.Content^[0] := 0;
  Arr.Content^[1] := 2;
  Expect(CreateUniqueDWord(Arr)).ToBe(3);
  Expect(Arr.Len).ToBe(3);

  Expect(Arr.Content^[0]).ToBe(0);
  Expect(Arr.Content^[1]).ToBe(2);
  Expect(Arr.Content^[2]).ToBe(3);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow4;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  SetDynOfDWordLength(Arr, 3);
  Arr.Content^[0] := 0;
  Arr.Content^[1] := 2;
  Arr.Content^[2] := 3;
  Expect(CreateUniqueDWord(Arr)).ToBe(4);
  Expect(Arr.Len).ToBe(4);

  Expect(Arr.Content^[0]).ToBe(0);
  Expect(Arr.Content^[1]).ToBe(2);
  Expect(Arr.Content^[2]).ToBe(3);
  Expect(Arr.Content^[3]).ToBe(4);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow5;
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65534)).ToBe(True, 'Increase MaxMM value in case of an error.');
  for i := 0 to 65534 - 1 do
    Arr.Content^[i] := i;

  Expect(CreateUniqueDWord(Arr)).ToBe(65534, 'created item');
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to 65534 - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_HappyFlow6;  //the search wraps around to find a free number
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65534)).ToBe(True);
  for i := 0 to 10 - 1 do
    Arr.Content^[i] := i;

  for i := 10 to 65534 - 1 do
    Arr.Content^[i] := (i + 1) and $FFFF;

  Expect(CreateUniqueDWord(Arr)).ToBe(10, 'created item');
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to 10 - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  for i := 10 to 65534 - 1 do
    Expect(Arr.Content^[i]).ToBe(i + 1);

  Expect(Arr.Content^[65534]).ToBe(10, 'content');

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_FullArray1;
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65535)).ToBe(True);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i;

  Expect(CreateUniqueDWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_FullArray2;
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65535)).ToBe(True);
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := 0;

  Expect(CreateUniqueDWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65535);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(0);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_FullArray3;
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65540)).ToBe(True);  //The array has more numbers than the max value CreateUniqueDWord can create.
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := i;   //this is different than array of Word, because the array of DWord can store items, with values greater than 65535

  Expect(CreateUniqueDWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65540);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(i {and $FFFF});

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestCreateUniqueDWord_FullArray4;
var
  Arr: TDynArrayOfDWord;
  i: LongInt;
begin
  InitDynArrayOfDWordToEmpty(Arr);

  Expect(SetDynOfDWordLength(Arr, 65540)).ToBe(True); //The array has more numbers than the max value CreateUniqueDWord can create.
  for i := 0 to Arr.Len - 1 do
    Arr.Content^[i] := 3;

  Expect(CreateUniqueDWord(Arr)).ToBe(65535); //65535 is an error indicator, not a valid number
  Expect(Arr.Len).ToBe(65540);

  for i := 0 to Arr.Len - 1 do
    Expect(Arr.Content^[i]).ToBe(3);

  FreeDynArrayOfDWord(Arr);
end;


procedure TTestDynArraysOfDWordCase.TestDoubleFree;
var
  Arr: TDynArrayOfDWord;
begin
  InitDynArrayOfDWordToEmpty(Arr);
  SetDynOfDWordLength(Arr, 3);

  FreeDynArrayOfDWord(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynArrayOfDWord(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


initialization

  RegisterTest(TTestDynArraysOfDWordCase);
end.


