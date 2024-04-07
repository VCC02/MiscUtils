{
    Copyright (C) 2024 VCC
    creation date: 06 Apr 2024
    initial release date: 07 Apr 2024

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


unit DynArrayFIFO;

{$IFDEF FPC}
  {$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF UNIX}
    LCLIntf, LCLType,
  {$ELSE}
    Windows,
  {$ENDIF}
  Classes, SysUtils, DynArrays;

type
  TDynArrayOfByteFIFO = class
  private
    FFIFO: TDynArrayOfTDynArrayOfByte;
    FCritSec: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Put(var AArr: TDynArrayOfByte);
    procedure PutMultiple(var AArrays: TDynArrayOfTDynArrayOfByte);
    function Pop(var AArr: TDynArrayOfByte): Boolean; //Returns True for valid output and False if there was no item in FIFO. The function calls InitDynArrayToEmpty(AArr), so the AArr should already be empty.
    procedure PopAll(var AArrays: TDynArrayOfTDynArrayOfByte);  //AArrays shold be initialized. The FIFO content is concatenated to AArrays.
    function PopAllAsString: string;
    function GetLength: Integer;
  end;


{$IFnDEF FPC}
  function GetTickCount64: DWORD; stdcall; external kernel32 name 'GetTickCount64';  //available on Vista and newer, according to SysUtils from FP
{$ENDIF}


implementation


uses
  Forms;

const
  COutOfMemoryMsg = 'Out of memory in DynArray FIFO.';


constructor TDynArrayOfByteFIFO.Create;
begin
  inherited Create;
  InitDynOfDynOfByteToEmpty(FFIFO);

  {$IFDEF FPC}
    InitCriticalSection(FCritSec);
  {$ELSE}
    InitializeCriticalSection(FCritSec);
  {$ENDIF}
end;


destructor TDynArrayOfByteFIFO.Destroy;
var
  RawCount: Integer;
  FoundLock: Boolean;
  tk: UInt64;
begin
  tk := GetTickCount64;

  {$IFnDEF UNIX}
    repeat
      FoundLock := False;
      RawCount := (-1 - FCritSec.LockCount) shr 2 + FCritSec.RecursionCount;  ////////////////////// ToDo: look for FCritSec fields in Linux if they can be used here
      if RawCount > 0 then
        FoundLock := True;

      Application.ProcessMessages;
      Sleep(2);
    until not FoundLock or (GetTickCount64 - tk > 5000);
  {$ENDIF}

  {$IFDEF FPC}
    DoneCriticalSection(FCritSec);
  {$ELSE}
    DeleteCriticalSection(FCritSec);
  {$ENDIF}

  FreeDynOfDynOfByteArray(FFIFO);

  inherited Destroy;
end;


procedure TDynArrayOfByteFIFO.Put(var AArr: TDynArrayOfByte);
begin
  EnterCriticalSection(FCritSec);
  try
    if not AddDynArrayOfByteToDynOfDynOfByte(FFIFO, AArr) then
      raise Exception.Create(COutOfMemoryMsg);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TDynArrayOfByteFIFO.PutMultiple(var AArrays: TDynArrayOfTDynArrayOfByte);
begin
  EnterCriticalSection(FCritSec);
  try
    if not ConcatDynOfDynOfByteArrays(FFIFO, AArrays) then    //Concats AArr1 with AArr2. Places new array in AArr1.
      raise Exception.Create(COutOfMemoryMsg);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TDynArrayOfByteFIFO.Pop(var AArr: TDynArrayOfByte): Boolean; //Returns True for valid output and False if there was no item in FIFO
var
  BufferPointer: PDynArrayOfByte;
begin
  EnterCriticalSection(FCritSec);
  try
    Result := FFIFO.Len > 0;

    if Result then
    begin
      InitDynArrayToEmpty(AArr);
      BufferPointer := FFIFO.Content^[0];

      if not CopyFromDynArray(AArr, BufferPointer^, 0, BufferPointer^.Len) then
        raise Exception.Create(COutOfMemoryMsg);

      if not DeleteItemFromDynOfDynOfByte(FFIFO, 0) then
        raise Exception.Create(COutOfMemoryMsg);
    end;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TDynArrayOfByteFIFO.PopAll(var AArrays: TDynArrayOfTDynArrayOfByte);  //AArrays shold be initialized. The FIFO content is concatenated to AArrays.
begin
  EnterCriticalSection(FCritSec);
  try
    if FFIFO.Len = 0 then
      Exit;

    if not ConcatDynOfDynOfByteArrays(AArrays, FFIFO) then   //Concats AArr1 with AArr2. Places new array in AArr1.
      raise Exception.Create(COutOfMemoryMsg);

    FreeDynOfDynOfByteArray(FFIFO);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TDynArrayOfByteFIFO.PopAllAsString: string;
var
  i: Integer;
begin
  EnterCriticalSection(FCritSec);
  try
    Result := '';
    for i := 0 to FFIFO.Len - 1 do
      Result := Result + DynArrayOfByteToString(FFIFO.Content^[i]^);
    FreeDynOfDynOfByteArray(FFIFO);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TDynArrayOfByteFIFO.GetLength: Integer;
begin
  EnterCriticalSection(FCritSec);
  try
    Result := FFIFO.Len;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;

end.
