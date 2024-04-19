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


unit TestDynOfDynOfWordCase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  DynArrays, Expectations;

type

  TTestDynOfDynOfWordCase = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleAllocation;
    procedure TestWritingToArray;
    procedure TestReallocationToLargerArray;
    procedure TestReallocationToSmallerArray;
    procedure Test_AddDynArrayOfWordToDynOfDynOfWord_HappyFlow;
    procedure Test_AddDynArrayOfWordToDynOfDynOfWord_WithoutFirstInitDynArray;
    procedure Test_AddDynArrayOfWordToDynOfDynOfWord_WithoutSecondInitDynArray;
    procedure TestDoubleFree;

    procedure TestDeleteItem_FromEmptyArray;
    procedure TestDeleteItem_FromOneItemArray;

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
  CUninitializedDynArrayErrMsg1 = 'The DynArray is not initialized. Please call InitDynOfDynOfWordToEmpty before working with DynArray functions.';
  CUninitializedDynArrayErrMsg2 = 'The DynArray is not initialized. Please call InitDynArrayOfWordToEmpty before working with DynArray functions.';


procedure TTestDynOfDynOfWordCase.SetUp;
begin
  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  {$IFDEF UsingMPMM}
    MM_Init;
  {$ENDIF}
end;


procedure TTestDynOfDynOfWordCase.TearDown;
begin

end;


procedure TTestDynOfDynOfWordCase.TestSimpleAllocation;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  AllocationResult: Boolean;
  i: Integer;
begin
  InitDynOfDynOfWordToEmpty(Arr);  //this is what Delphi and FP do automatically

  AllocationResult := SetDynOfDynOfWordLength(Arr, 7);
  try
    Expect(AllocationResult).ToBe(True, 'Expected a successful allocation.');
    Expect(Byte(AllocationResult)).ToBe(Byte(True));
    Expect(Arr.Len).ToBe(7);

    for i := 0 to Arr.Len - 1 do
      CheckInitializedDynArrayOfWord(Arr.Content^[i]^);    //the inner arrays are initialized automatically by SetDynOfDynOfByteLength
  finally
    FreeDynOfDynOfWordArray(Arr);  //the array has to be manually freed, because there is no reference counting
  end;
end;


procedure TTestDynOfDynOfWordCase.TestWritingToArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  i: Integer;
begin
  InitDynOfDynOfWordToEmpty(Arr);
  SetDynOfDynOfWordLength(Arr, 20);
  try
    for i := 0 to Arr.Len - 1 do
    begin
      SetDynOfWordLength(Arr.Content^[i]^, 3);
      Arr.Content^[i]^.Content^[2] := 80 + i;
      Expect(Arr.Content^[i]^.Content^[2]).ToBe(DWord(80 + i));
    end;
  finally
    FreeDynOfDynOfWordArray(Arr);
  end;
end;


procedure TTestDynOfDynOfWordCase.TestReallocationToLargerArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  i, j: Integer;
begin
  InitDynOfDynOfWordToEmpty(Arr);
  SetDynOfDynOfWordLength(Arr, 20);

  for i := 0 to DynOfDynOfWordLength(Arr) - 1 do
  begin
    Expect(SetDynOfWordLength(Arr.Content^[i]^, 3)).ToBe(True, 'Internal array allocation');

    for j := 0 to 2 do
      Arr.Content^[i]^.Content^[j] := i * 10 + j;
  end;

  Expect(SetDynOfDynOfWordLength(Arr, 30)).ToBe(True, 'External array re-allocation');
  try
    for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
      for j := 0 to 2 do
        Expect(Arr.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));
  finally
    FreeDynOfDynOfWordArray(Arr);
  end;
end;


procedure TTestDynOfDynOfWordCase.TestReallocationToSmallerArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  i, j: Integer;
begin
  InitDynOfDynOfWordToEmpty(Arr);
  SetDynOfDynOfWordLength(Arr, 20);

  for i := 0 to DynOfDynOfWordLength(Arr) - 1 do
  begin
    SetDynOfWordLength(Arr.Content^[i]^, 3);

    for j := 0 to 2 do
      Arr.Content^[i]^.Content^[j] := i * 10 + j;
  end;

  SetDynOfDynOfWordLength(Arr, 10);
  try
    for i := 0 to 10 - 1 do  //test up to the old length, as this content has to be valid only
      for j := 0 to 2 do
        Expect(Arr.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));
  finally
    FreeDynOfDynOfWordArray(Arr);
  end;
end;


procedure TTestDynOfDynOfWordCase.Test_AddDynArrayOfWordToDynOfDynOfWord_HappyFlow;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  NewArr: TDynArrayOfWord;
  i, j: Integer;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(SetDynOfDynOfWordLength(Arr, 20)).ToBe(True);

  for i := 0 to DynOfDynOfWordLength(Arr) - 1 do
  begin
    Expect(SetDynOfWordLength(Arr.Content^[i]^, 3)).ToBe(True);

    for j := 0 to 2 do
      Arr.Content^[i]^.Content^[j] := i * 10 + j;
  end;

  InitDynArrayOfWordToEmpty(NewArr);
  Expect(SetDynOfWordLength(NewArr, 7)).ToBe(True);
  for j := 0 to NewArr.Len - 1 do
    NewArr.Content^[j] := 200 + j;

  try
    Expect(AddDynArrayOfWordToDynOfDynOfWord(Arr, NewArr)).ToBe(True);
  finally
    FreeDynArrayOfWord(NewArr);
  end;

  for i := 0 to 20 - 1 do  //test up to the old length, as this content has to be valid only
    for j := 0 to 2 do
      Expect(Arr.Content^[i]^.Content^[j]).ToBe(DWord(i * 10 + j), ' at i = ' + IntToStr(i) + '  j = ' + IntToStr(j));

  Expect(Arr.Len).ToBe(21);
  Expect(@Arr.Content^[20]^.Content^, 7 shl 1).ToBe(@[200, 0, 201, 0, 202, 0, 203, 0, 204, 0, 205, 0, 206, 0]);

  FreeDynOfDynOfWordArray(Arr); //freeing only if successfully allocated
end;


procedure TTestDynOfDynOfWordCase.Test_AddDynArrayOfWordToDynOfDynOfWord_WithoutFirstInitDynArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  NewArr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(NewArr);
  SetDynOfWordLength(NewArr, 3);

  try
    AddDynArrayOfWordToDynOfDynOfWord(Arr, NewArr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg1);
  end;

  FreeDynArrayOfWord(NewArr);
end;


procedure TTestDynOfDynOfWordCase.Test_AddDynArrayOfWordToDynOfDynOfWord_WithoutSecondInitDynArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  NewArr: TDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);
  SetDynOfDynOfWordLength(Arr, 3);

  try
    AddDynArrayOfWordToDynOfDynOfWord(Arr, NewArr);
  except
    on E: Exception do
      Expect(E.Message).ToBe(CUninitializedDynArrayErrMsg2);
  end;

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDoubleFree;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);
  SetDynOfDynOfWordLength(Arr, 3);

  FreeDynOfDynOfWordArray(Arr);
  Expect(Arr.Len).ToBe(0);
  Expect(Arr.Content).ToBe(nil);

  try                            //Free again. The structure should stay the same. No exception is expected.
    FreeDynOfDynOfWordArray(Arr);
    Expect(Arr.Len).ToBe(0);
    Expect(Arr.Content).ToBe(nil);
  except
    on E: Exception do
      Expect(E.Message).ToBe('No exception is expected!');
  end;
end;


procedure TTestDynOfDynOfWordCase.TestDeleteItem_FromEmptyArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
  ErrMsg: string;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  ErrMsg := 'no err';
  try
    Expect(DeleteItemFromDynOfDynOfWord(Arr, 0)).ToBe(False);
  except
    on E: Exception do
      ErrMsg := E.Message;
  end;

  Expect(ErrMsg).ToBe('Index out of range when deleting item from DynOfDynArrayOfWord.');
  Expect(Arr.Len).ToBe(0, 'no action');

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteItem_FromOneItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(0, 'successful deletion');

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteFirstItem_FromTwoItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Second', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(1, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^, 6).ToBe(@['Second']);

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteSecondItem_FromTwoItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Second', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 1)).ToBe(True);
  Expect(Arr.Len).ToBe(1, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^, 5).ToBe(@['First']);

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteFirstItem_FromThreeItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Second', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Third', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 0)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^, 6).ToBe(@['Second']);
  Expect(@Arr.Content^[1]^.Content^, 5).ToBe(@['Third']);

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteSecondItem_FromThreeItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Second', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Third', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 1)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^, 5).ToBe(@['First']);
  Expect(@Arr.Content^[1]^.Content^, 5).ToBe(@['Third']);

  FreeDynOfDynOfWordArray(Arr);
end;


procedure TTestDynOfDynOfWordCase.TestDeleteThirdItem_FromThreeItemArray;
var
  Arr: TDynArrayOfTDynArrayOfWord;
begin
  InitDynOfDynOfWordToEmpty(Arr);

  Expect(AddStringToDynOfDynArrayOfWord('First', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Second', Arr)).ToBe(True);
  Expect(AddStringToDynOfDynArrayOfWord('Third', Arr)).ToBe(True);

  Expect(DeleteItemFromDynOfDynOfWord(Arr, 2)).ToBe(True);
  Expect(Arr.Len).ToBe(2, 'successful deletion');
  Expect(@Arr.Content^[0]^.Content^, 5).ToBe(@['First']);
  Expect(@Arr.Content^[1]^.Content^, 6).ToBe(@['Second']);

  FreeDynOfDynOfWordArray(Arr);
end;

initialization

  RegisterTest(TTestDynOfDynOfWordCase);
end.


