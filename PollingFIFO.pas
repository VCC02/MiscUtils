{
    Copyright (C) 2023 VCC
    creation date: Jul 2022
    initial release date: 26 Jul 2022

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

unit PollingFIFO;

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
  Classes, SysUtils;

type
  TPollingFIFO = class
  private
    FFIFO: TStringList;
    FCritSec: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Put(AString: string);
    procedure PutMultiple(AListOfFiles: TStringList);
    function Pop(out AString: string): Boolean; //Returns True for valid output and False if there was no item in FIFO
    procedure PopAll(OutStrings: TStringList);
    function PopAllAsString: string;
    function GetLength: Integer;
  end;

  
{$IFnDEF FPC}
  function GetTickCount64: DWORD; stdcall; external kernel32 name 'GetTickCount64';  //available on Vista and newer, according to SysUtils from FP
{$ENDIF}


implementation


uses
  Forms;


constructor TPollingFIFO.Create;
begin
  inherited Create;
  FFIFO := TStringList.Create;

  {$IFDEF FPC}
    InitCriticalSection(FCritSec);
  {$ELSE}
    InitializeCriticalSection(FCritSec);
  {$ENDIF}
end;


destructor TPollingFIFO.Destroy;
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

  FreeAndNil(FFIFO);

  inherited Destroy;
end;


procedure TPollingFIFO.Put(AString: string);
begin
  EnterCriticalSection(FCritSec);
  try
    FFIFO.Add(AString);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TPollingFIFO.PutMultiple(AListOfFiles: TStringList);
begin
  EnterCriticalSection(FCritSec);
  try
    FFIFO.AddStrings(AListOfFiles);
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TPollingFIFO.Pop(out AString: string): Boolean; //Returns True for valid output and False if there was no item in FIFO
begin
  EnterCriticalSection(FCritSec);
  try
    Result := FFIFO.Count > 0;

    if Result then
    begin
      AString := FFIFO.Strings[0];
      FFIFO.Delete(0);
    end;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


procedure TPollingFIFO.PopAll(OutStrings: TStringList);
begin
  EnterCriticalSection(FCritSec);
  try
    if FFIFO.Count = 0 then
      Exit;

    OutStrings.AddStrings(FFIFO);

    FFIFO.Clear;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TPollingFIFO.PopAllAsString: string;
begin
  EnterCriticalSection(FCritSec);
  try
    Result := FFIFO.Text;
    FFIFO.Clear;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;


function TPollingFIFO.GetLength: Integer;
begin
  EnterCriticalSection(FCritSec);
  try
    Result := FFIFO.Count;
  finally
    LeaveCriticalSection(FCritSec);
  end;
end;

end.

