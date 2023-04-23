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


unit DynArrays;

{$IFDEF FPC}
  {$mode ObjFPC}{$H+}
{$ELSE}

{$ENDIF}

//Dependencies:  MemManager  (see DynTFT repo for mP)

{  How to use:
  - Before calling any of the DynLength, SetDynLength or ConcatDynArrays function, users have to call InitDynArrayToEmpty.
    This ensures that the array is properly initialized. The reason is that there is no reference counting or automatic initialization.
    The FP implementation has an "Initialized" field, on every array, which allows raising an exception if calling one of the array functions
    on unitialized arrays. Thus, it is recommended to validate the code, as much as possible, using FP, before moving to mP.

  - Arrays are allocated in heap, using a memory manager. The SetDynLength and ConcatDynArrays return True if successfully allocated.
    Once an array is no longer needed, it has to be freed, by calling SetDynLength(<Arr>, 0).

  - Setting a new length to an array, which already has elements, will require at least the same amount of memory to be available.
    If the new length is greater than the current length, more memory is required. See implementation.

  - Accessing array elements can be done as Arr.Content^[<index>] := <value>;
}

interface

const
  CMaxDynArrayLength = 65536;  //bytes

type
  TDynArrayOfByteContent = array[0..CMaxDynArrayLength - 1] of Byte;
  PDynArrayOfByteContent = ^TDynArrayOfByteContent;

  TDynArrayLength = DWord; // {$IFDEF FPC} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP

  TDynArrayOfByte = record
    Len: TDynArrayLength;
    Content: PDynArrayOfByteContent;
    {$IFDEF FPC}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfByte = ^TDynArrayOfByte;


procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.


implementation


{$IFDEF FPC}
  uses
    SysUtils;
{$ENDIF}


procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF FPC}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


{$IFDEF FPC}
  //This check is not available in mP, but is is useful as a debugging means on Desktop.
  procedure CheckInitializedArray(var AArr: TDynArrayOfByte);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayToEmpty before working with DynArray functions.');
  end;
{$ENDIF}


function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
begin
  {$IFDEF FPC}
    CheckInitializedArray(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: PIntPtr;
begin
  {$IFDEF FPC}
    CheckInitializedArray(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
      Freemem(AArr.Content, AArr.Len);

    Exit;
  end;

  OldPointer := PIntPtr(AArr.Content);

  {$IFnDEF FPC}
    GetMem(AArr.Content, ANewLength);
    if MM_error then
    begin
      Result := False;
      Exit;
    end;

    memcpy.....
    if ANewLength < AArr.Len then
      MemMove(OldPointer^, AArr.Content^, ANewLength)   //the new array is smaller
    else
      MemMove(OldPointer^, AArr.Content^, AArr.Len);    //the new array is larger  - the rest of the content is not initialized
  {$ELSE}
    try
      GetMem(AArr.Content, ANewLength);

      if ANewLength < AArr.Len then
        Move(OldPointer^, AArr.Content^, ANewLength)   //the new array is smaller
      else
        Move(OldPointer^, AArr.Content^, AArr.Len);    //the new array is larger  - the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
    Freemem(OldPointer, AArr.Len);

  AArr.Len := ANewLength;
end;


function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  NewLen: DWord;
  NewPointer: PIntPtr;
  OldArr1Len: DWord;
begin
  {$IFDEF FPC}
    CheckInitializedArray(AArr1);
    CheckInitializedArray(AArr2);
  {$ENDIF}

  if AArr2.Len = 0 then
  begin
    Result := True;
    Exit;
  end;

  NewLen := DWord(AArr1.Len) + DWord(AArr2.Len);
  if NewLen > 65535 then
  begin
    Result := False;
    Exit;
  end;

  OldArr1Len := AArr1.Len;
  Result := SetDynLength(AArr1, NewLen);

  {$IFnDEF FPC}
    memcpy.....
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len));  //NewPointer := @AArr1.Content^[OldArr1Len];
    Move(AArr2.Content^, NewPointer^, AArr2.Len);
  {$ENDIF}
end;

end.

