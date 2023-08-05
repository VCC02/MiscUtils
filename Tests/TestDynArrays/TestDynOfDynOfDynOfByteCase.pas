{
    Copyright (C) 2023 VCC
    creation date: May 2023
    initial release date: 04 Aug 2023

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


unit TestDynOfDynOfDynOfByteCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  DynArrays, Expectations;

type

  TTestDynOfDynOfDynOfByteCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleAllocation;
    procedure TestWritingToArray;
    procedure TestReallocationToLargerArray;
    procedure TestReallocationToSmallerArray;
    procedure Test_AddDynArrayOfByteToDynOfDynOfByte_HappyFlow1;
    procedure Test_AddDynArrayOfByteToDynOfDynOfByte_HappyFlow2;
    procedure Test_AddDynArrayOfByteToDynOfDynOfByte_WithoutFirstInitDynArray;
    procedure Test_AddDynArrayOfByteToDynOfDynOfByte_WithoutSecondInitDynArray;
    procedure TestDoubleFree;

    procedure TestDeleteItem_FromEmptyArray;
    procedure TestDeleteItem_FromOneItemArray_NoContent;
    procedure TestDeleteItem_FromOneItemArray_WithContent;

    procedure TestDeleteFirstItem_FromTwoItemArray;
    procedure TestDeleteSecondItem_FromTwoItemArray;

    procedure TestDeleteFirstItem_FromThreeItemArray;
    procedure TestDeleteSecondItem_FromThreeItemArray;
    procedure TestDeleteThirdItem_FromThreeItemArray;
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
  CUninitializedDynArrayErrMsg1 = 'The DynArray is not initialized. Please call InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty before working with DynArray functions.';
  CUninitializedDynArrayErrMsg2 = 'The DynArray is not initialized. Please call InitDynOfDynOfByteToEmpty before working with DynArray functions.';  //The underlying init functions are from PtrUInt.



procedure TTestDynOfDynOfDynOfByteCase.SetUp;
begin
  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  {$IFDEF UsingMPMM}
    MM_Init;
  {$ENDIF}
end;


procedure TTestDynOfDynOfDynOfByteCase.TearDown;
begin

end;


procedure TTestDynOfDynOfDynOfByteCase.TestSimpleAllocation;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);

    for i := 0 to Arr.Len - 1 do
      CheckInitializedDynOfDynArray(Arr.Content^[i]^);    //the inner arrays are initialized automatically by SetDynOfPDynArrayOfTDynArrayOfByteLength
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.TestWritingToArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i, k: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);

  try
    for k := 0 to Arr.Len - 1 do
    begin
      SetDynOfDynOfByteLength(Arr.Content^[k]^, 20);
      try
        for i := 0 to Arr.Content^[k]^.Len - 1 do
        begin
          SetDynLength(Arr.Content^[k]^.Content^[i]^, 3);
          Arr.Content^[k]^.Content^[i]^.Content^[2] := 80 + i;
          Expect(Arr.Content^[k]^.Content^[i]^.Content^[2]).ToBe(DWord(80 + i));
        end;
      finally
        FreeDynOfDynOfByteArray(Arr.Content^[k]^);
      end;
    end;
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i, j, k: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);

  try
    for k := 0 to Arr.Len - 1 do
    begin
      SetDynOfDynOfByteLength(Arr.Content^[k]^, 20);

      for i := 0 to DynOfDynOfByteLength(Arr.Content^[k]^) - 1 do
      begin
        Expect(SetDynLength(Arr.Content^[k]^.Content^[i]^, 3)).ToBe(True, 'Internal array allocation');

        for j := 0 to 2 do
          Arr.Content^[k]^.Content^[i]^.Content^[j] := i * 10 + j;
      end;

      Expect(SetDynOfDynOfByteLength(Arr.Content^[k]^, 30)).ToBe(True, 'External array re-allocation');
      try
        for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
          for j := 0 to 2 do
            Expect(Arr.Content^[k]^.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));
      finally
        FreeDynOfDynOfByteArray(Arr.Content^[k]^);
      end;
    end;
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i, j, k: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);

  try
    for k := 0 to Arr.Len - 1 do
    begin
      SetDynOfDynOfByteLength(Arr.Content^[k]^, 20);

      for i := 0 to DynOfDynOfByteLength(Arr.Content^[k]^) - 1 do
      begin
        SetDynLength(Arr.Content^[k]^.Content^[i]^, 3);

        for j := 0 to 2 do
          Arr.Content^[k]^.Content^[i]^.Content^[j] := i * 10 + j;
      end;

      SetDynOfDynOfByteLength(Arr.Content^[k]^, 10);
      try
        for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
          for j := 0 to 2 do
            Expect(Arr.Content^[k]^.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));
      finally
        FreeDynOfDynOfByteArray(Arr.Content^[k]^);
      end;
    end;
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.Test_AddDynArrayOfByteToDynOfDynOfByte_HappyFlow1;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  NewArr: TDynArrayOfByte;
  i, j, k: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);
  try
    for k := 0 to Arr.Len - 1 do
    begin
      Expect(SetDynOfDynOfByteLength(Arr.Content^[k]^, 20)).ToBe(True);

      for i := 0 to DynOfDynOfByteLength(Arr.Content^[k]^) - 1 do
      begin
        Expect(SetDynLength(Arr.Content^[k]^.Content^[i]^, 3)).ToBe(True);

        for j := 0 to 2 do
          Arr.Content^[k]^.Content^[i]^.Content^[j] := i * 10 + j;
      end;

      InitDynArrayToEmpty(NewArr);
      Expect(SetDynLength(NewArr, 7)).ToBe(True);
      for j := 0 to NewArr.Len - 1 do
        NewArr.Content^[j] := 200 + j;

      try
        Expect(AddDynArrayOfByteToDynOfDynOfByte(Arr.Content^[k]^, NewArr)).ToBe(True);
      finally
        FreeDynArray(NewArr);
      end;

      for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
        for j := 0 to 2 do
          Expect(Arr.Content^[k]^.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));

      Expect(Arr.Content^[k]^.Len).ToBe(21);
      Expect(Arr.Content^[k]^.Content^[20]^.Content).ToBe(@[200, 201, 202, 203, 204, 205, 206]);

      FreeDynOfDynOfByteArray(Arr.Content^[k]^); //freeing only if successfully allocated
    end;
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.Test_AddDynArrayOfByteToDynOfDynOfByte_HappyFlow2;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  NewArr: TDynArrayOfTDynArrayOfByte;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 1);
  try
    Expect(AddStringToDynOfDynArrayOfByte('First', Arr.Content^[0]^)).ToBe(True);
    Expect(AddStringToDynOfDynArrayOfByte('Second', Arr.Content^[0]^)).ToBe(True);

    InitDynOfDynOfByteToEmpty(NewArr);

    AddStringToDynOfDynArrayOfByte('one', NewArr);
    AddStringToDynOfDynArrayOfByte('two', NewArr);
    AddStringToDynOfDynArrayOfByte('three', NewArr);

    Expect(AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(Arr, NewArr)).ToBe(True);
    Expect(Arr.Len).ToBe(2);

    FreeDynOfDynOfByteArray(NewArr);

    Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 5).ToBe(@['First']);
    Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 6).ToBe(@['Second']);

    Expect(@Arr.Content^[1]^.Content^[0]^.Content^, 3).ToBe(@['one']);
    Expect(@Arr.Content^[1]^.Content^[1]^.Content^, 3).ToBe(@['two']);
    Expect(@Arr.Content^[1]^.Content^[2]^.Content^, 4).ToBe(@['three']);
  finally
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.Test_AddDynArrayOfByteToDynOfDynOfByte_WithoutFirstInitDynArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  NewArr: TDynArrayOfTDynArrayOfByte;
begin
  InitDynOfDynOfByteToEmpty(NewArr);
  SetDynOfDynOfByteLength(NewArr, 3);

  try
    AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(Arr, NewArr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg1);
  end;

  FreeDynOfDynOfByteArray(NewArr);
end;


procedure TTestDynOfDynOfDynOfByteCase.Test_AddDynArrayOfByteToDynOfDynOfByte_WithoutSecondInitDynArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  NewArr: TDynArrayOfTDynArrayOfByte;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 3);

  try
    AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(Arr, NewArr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg2);
  end;

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDoubleFree;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 3);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteItem_FromEmptyArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  ErrMsg: string;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);

  ErrMsg := 'no err';
  try
    Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 0)).ToBe(False);
  except
    on E: Exception do
      ErrMsg := E.Message;
  end;

  Expect(ErrMsg).ToBe('Index out of range when deleting item from DynArrayOfPDynArrayOfTDynArrayOfByte.');
  Expect(Arr.Len).ToBe(0, 'no action');

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteItem_FromOneItemArray_NoContent;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 1);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(0, 'successful deletion');
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteItem_FromOneItemArray_WithContent;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 1);

  InitDynOfDynOfByteToEmpty(Arr.Content^[0]^);
  Expect(AddStringToDynOfDynArrayOfByte('First', Arr.Content^[0]^)).ToBe(True);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(0, 'successful deletion');
end;


procedure AddToItemsToBigArray(var Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
var
  i: Integer;
begin
  for i := 0 to Arr.Len - 1 do
  begin
    InitDynOfDynOfByteToEmpty(Arr.Content^[i]^);
    Expect(AddStringToDynOfDynArrayOfByte('First' + IntToStr(i), Arr.Content^[i]^)).ToBe(True);
    Expect(AddStringToDynOfDynArrayOfByte('Second' + IntToStr(i), Arr.Content^[i]^)).ToBe(True);
  end;
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteFirstItem_FromTwoItemArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);

  AddToItemsToBigArray(Arr);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(1, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 6).ToBe(@['First1']);
  Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 7).ToBe(@['Second1']);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteSecondItem_FromTwoItemArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 2);

  AddToItemsToBigArray(Arr);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 1)).ToBe(True);
  Expect(Arr.Len).ToBe(1, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 6).ToBe(@['First0']);
  Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 7).ToBe(@['Second0']);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteFirstItem_FromThreeItemArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 3);

  AddToItemsToBigArray(Arr);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 6).ToBe(@['First1']);
  Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 7).ToBe(@['Second1']);

  Expect(@Arr.Content^[1]^.Content^[0]^.Content^, 6).ToBe(@['First2']);
  Expect(@Arr.Content^[1]^.Content^[1]^.Content^, 7).ToBe(@['Second2']);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteSecondItem_FromThreeItemArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 3);

  AddToItemsToBigArray(Arr);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 1)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 6).ToBe(@['First0']);
  Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 7).ToBe(@['Second0']);

  Expect(@Arr.Content^[1]^.Content^[0]^.Content^, 6).ToBe(@['First2']);
  Expect(@Arr.Content^[1]^.Content^[1]^.Content^, 7).ToBe(@['Second2']);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


procedure TTestDynOfDynOfDynOfByteCase.TestDeleteThirdItem_FromThreeItemArray;
var
  Arr: TDynArrayOfPDynArrayOfTDynArrayOfByte;
  i: Integer;
begin
  InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(Arr);
  SetDynOfPDynArrayOfTDynArrayOfByteLength(Arr, 3);

  AddToItemsToBigArray(Arr);

  Expect(DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(Arr, 2)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^[0]^.Content^, 6).ToBe(@['First0']);
  Expect(@Arr.Content^[0]^.Content^[1]^.Content^, 7).ToBe(@['Second0']);

  Expect(@Arr.Content^[1]^.Content^[0]^.Content^, 6).ToBe(@['First1']);
  Expect(@Arr.Content^[1]^.Content^[1]^.Content^, 7).ToBe(@['Second1']);

  FreeDynArrayOfPDynArrayOfTDynArrayOfByte(Arr);
end;


initialization

  RegisterTest(TTestDynOfDynOfDynOfByteCase);
end.

