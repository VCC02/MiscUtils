{
    Copyright (C) 2023 VCC
    creation date: 03 Oct 2023
    initial release date: 04 Oct 2023

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


//Currently, the library is not thread safe.  It would require some critical sections.


unit SimpleCOM;

{$IFDEF FPC}
  {$mode Delphi}{$H+}
{$ENDIF}

{$IFnDEF FPC}  //e.g. Delphi
  {$IFnDEF UNIX} //still a FPC directive
    {$DEFINE Windows}
  {$ENDIF}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  {$IFDEF Windows}
    Windows
  {$ELSE}
    LCLType, LCLIntf, BaseUnix, Unix, termio
  {$ENDIF}
{$ELSE}
  Windows //WinTypes
{$ENDIF}
  , SysUtils, Classes

{$IFDEF Windows}
  , Registry
{$ELSE}

{$ENDIF}
  ;


function ConnectToCOM(AComName: string; ABaudRate: Cardinal; ARxBufSize, ATxBufSize: Integer): Cardinal;  //Returns a handle, greater than 0 for success. Returns 0, in case of error.
procedure DisconnectFromCOM(AComName: string);
procedure ListExistentCOMPorts(AList: TStrings; AIncludeNonExistent: Boolean = False);

function COMIsConnected(AComName: string): Boolean; overload;
function COMIsConnected(AConnHandle: THandle): Boolean; overload;

function SendDataToCOM(AConnHandle: THandle; var ABuf; AByteCount: Integer): Integer;
function ReceiveDataFromCOM(AConnHandle: THandle; out ABuf; AByteCount: Integer): Integer;

//Use this when ConnectToCOM returns 0 (i.e. no valid handle).
//This function returns 0 either if no COM port is found by that name, or there is no error.
function GetCOMError(AComName: string): Cardinal;

//Returns additional info. Not all errors will have additional info.
function GetCOMExtraError(AComName: string): string;

//Returns number of bytes available to read with ReceiveDataFromCOM. Returns -1 in case of an error. Use GetCOMError to get error code.
function GetReceivedByteCount(AConnHandle: THandle): Integer;


const
  CComPortPrefix = {$IFDEF Windows} '\\.\'; {$ELSE}'/dev/'; {$ENDIF}


implementation


type
  TComPortInfo = record
    Name: string;
    ConnHandle: Cardinal;  //0 means not connected. Greater than 0 means connected.
    Err: Cardinal;  //use SysErrorMessage on this field, to get a string
    ErrLoc: string;
  end;
  //PComPortInfo = ^TComPortInfo;

  TComPortInfoArr = array of TComPortInfo;


var
  ComConnections: TComPortInfoArr;


function GetCOMIndexByName(AComName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(ComConnections) - 1 do
    if ComConnections[i].Name = AComName then
    begin
      Result := i;
      Break;
    end;
end;


function GetCOMIndexByHandle(AHandle: THandle): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(ComConnections) - 1 do
    if ComConnections[i].ConnHandle = AHandle then
    begin
      Result := i;
      Break;
    end;
end;


function BaudRateToConst(ABaudRate: Cardinal): Cardinal;
begin
  {$IFDEF UNIX}
    case ABaudRate of
      0:       Result := B0;
      50:      Result := B50;
      75:      Result := B75;
      110:     Result := B110;
      134:     Result := B134;
      150:     Result := B150;
      200:     Result := B200;
      300:     Result := B300;
      600:     Result := B600;
      1200:    Result := B1200;
      1800:    Result := B1800;
      2400:    Result := B2400;
      4800:    Result := B4800;
      9600:    Result := B9600;
      19200:   Result := B19200;
      38400:   Result := B38400;
      57600:   Result := B57600;
      115200:  Result := B115200;
      230400:  Result := B230400;
      460800:  Result := B460800;
      500000:  Result := B500000;
      576000:  Result := B576000;
      921600:  Result := B921600;
      1000000: Result := B1000000;
      1152000: Result := B1152000;
      1500000: Result := B1500000;
      2000000: Result := B2000000;
      2500000: Result := B2500000;
      3000000: Result := B3000000;
      3500000: Result := B3500000;
      4000000: Result := B4000000;
      else
        Result := B230400;
    end;

  {$ELSE}
    Result := 230400;  //some default value
  {$ENDIF}
end;


function ConnectToCOM(AComName: string; ABaudRate: Cardinal; ARxBufSize, ATxBufSize: Integer): Cardinal;  //Returns a handle, greater than 0 for success. Returns 0, in case of error.
var
  Idx: Integer;
  {$IFDEF Windows}
    DCB: TDCB;
    CommTimeouts: TCommTimeouts;
  {$ELSE}
    tios: Termios;
  {$ENDIF}  
begin
  Idx := GetCOMIndexByName(AComName);
  if Idx > -1 then
  begin
    if ComConnections[Idx].ConnHandle > 0 then
    begin
      Result := ComConnections[Idx].ConnHandle;
      Exit;
    end
    else //an item has already been allocated, on a previous call, so use it
      ;
  end
  else
  begin
    SetLength(ComConnections, Length(ComConnections) + 1);
    Idx := Length(ComConnections) - 1;
  end;

  ComConnections[Idx].Name := AComName;

  {$IFDEF Windows}
    SetLastError(0);
  {$ELSE}
    fpseterrno(0);
    //from FPC help:
    //O_RDWR: File is opened Read-Write
    //O_SYNC: The file is opened for synchronous IO. Any write operation on the file will not return until the data is physically written to disk.
    //see also FpOpen(CComPortPrefix + ComConnections[Idx].Name, O_RDWR + O_SYNC);
  {$ENDIF}
  ComConnections[Idx].ErrLoc := '_';
  Result := FileOpen(CComPortPrefix + ComConnections[Idx].Name, fmOpenReadWrite or fmShareExclusive);

  if Result = $FFFFFFFF then //if Result = THandle(-1) then
  begin
    ComConnections[Idx].ConnHandle := 0;
    ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
    ComConnections[Idx].ErrLoc := 'FileOpen';
    Result := 0;
    Exit;
  end
  else
  begin
    ComConnections[Idx].ConnHandle := Result;
    ComConnections[Idx].Err := 0;
    ComConnections[Idx].ErrLoc := '1';
  end;

  {$IFDEF Windows}
    if not GetCommState(Result, DCB) then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'GetCommState';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;

    DCB.BaudRate := ABaudRate;
    DCB.ByteSize := 8;
    DCB.Parity := NOPARITY;
    DCB.StopBits := ONESTOPBIT;

    if not SetCommState(Result, DCB) then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'SetCommState';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;

    if not SetupComm(Result, ARxBufSize, ATxBufSize) then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'SetupComm';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;

    CommTimeouts.ReadIntervalTimeout := MAXDWORD;
    CommTimeouts.ReadTotalTimeoutMultiplier := 0;
    CommTimeouts.ReadTotalTimeoutConstant := 0;
    CommTimeouts.WriteTotalTimeoutMultiplier := 10;
    CommTimeouts.WriteTotalTimeoutConstant := 10;

    if not SetCommTimeouts(ComConnections[Idx].ConnHandle, CommTimeouts) then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'SetCommTimeouts';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;
  {$ELSE}
    if TCGetAttr(ComConnections[Idx].ConnHandle, tios) = -1 then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'TCGetAttr tios';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;

    //see StackOverflow answer 19440268
    //FpIOCtl(ComConnections[Idx].ConnHandle, TIOCGSERIAL, @Ser);
    //tios.c_ispeed := BaudRateToConst(ABaudRate);
    //tios.c_ospeed := BaudRateToConst(ABaudRate);

    CFSetISpeed(tios, BaudRateToConst(ABaudRate));
    CFSetOSpeed(tios, BaudRateToConst(ABaudRate));

    tios.c_cflag := tios.c_cflag or CS8; //byte is 8-bits wide

    if tios.c_cflag and PARENB = PARENB then
      tios.c_cflag := tios.c_cflag xor PARENB; //disable parity

    tios.c_cflag := tios.c_cflag or CSTOPB;
    //More flags need to be set here

    if TCSetAttr(ComConnections[Idx].ConnHandle, TCSANOW, tios) = -1 then
    begin
      ComConnections[Idx].ConnHandle := 0;
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
      ComConnections[Idx].ErrLoc := 'TCSetAttr tios';
      DisconnectFromCOM(ComConnections[Idx].Name);
      Result := 0;
      Exit;
    end;
  {$ENDIF}
end;


procedure RemoveCOMFromArr(AIndex: Integer);
var
  i: Integer;
begin
  for i := AIndex to Length(ComConnections) - 2 do
    ComConnections[i] := ComConnections[i + 1];

  SetLength(ComConnections, Length(ComConnections) - 1);
end;


procedure DisconnectFromCOMByIndex(AIndex: Integer);
begin
  if ComConnections[AIndex].ConnHandle > 0 then  //an item in ComConnections, with Handle=0, means a failed connection
    FileClose(ComConnections[AIndex].ConnHandle);

  RemoveCOMFromArr(AIndex);
end;


procedure DisconnectFromCOM(AComName: string);
var
  Idx: Integer;
begin
  Idx := GetCOMIndexByName(AComName);
  if Idx > -1 then
    DisconnectFromCOMByIndex(Idx);
end;


procedure ListPortsByFilter(AList: TStrings; APrefix, AFilter: string);
var
  DirRec: TSearchRec;
  Res: Integer;
begin
  Res := FindFirst(APrefix + AFilter + '*', faAnyFile, DirRec);
  try
    while Res = 0 do
    begin
      AList.Add(DirRec.Name);
      Res := FindNext(DirRec);
    end;
  finally
    SysUtils.FindClose(DirRec);
  end;
end;


procedure GetAllCOMPorts(AList: TStrings);
var
  i: Integer;
begin
  {$IFDEF Windows}
    for i := 1 to 255 do
      AList.Add('COM' + IntToStr(i));
  {$ELSE}
    {$IFDEF UNIX}
      for i := 0 to 31 do
        AList.Add('ttyUSB' + IntToStr(i));

      for i := 0 to 31 do
        AList.Add('ttyACM' + IntToStr(i));

      for i := 0 to 31 do
        AList.Add('ttyS' + IntToStr(i));
    {$ENDIF}
  {$ENDIF}
end;


procedure ListExistentCOMPorts(AList: TStrings; AIncludeNonExistent: Boolean = False);
{$IFDEF Windows}
  var
    i: Integer;
    AReg: TRegistry;
    ValueNames: TStringList;

    {$IFDEF OpenComFilter}
      Res: Cardinal;
    {$ENDIF}
{$ENDIF}
begin
  AList.Clear;

  if AIncludeNonExistent then
    GetAllCOMPorts(AList)
  else
  begin
    {$IFDEF Windows}
      AReg := TRegistry.Create;
      try
        AReg.RootKey := HKEY_LOCAL_MACHINE;

        if AReg.OpenKeyReadOnly('\HARDWARE\DEVICEMAP\SERIALCOMM') then
        begin
          try
            ValueNames := TStringList.Create;
            try
              AReg.GetValueNames(ValueNames);      //a.k.a. subkeys

              for i := 0 to ValueNames.Count - 1 do
                AList.Add(AReg.ReadString(ValueNames[i]));
            finally
              ValueNames.Free;
            end;
          finally
            AReg.CloseKey;
          end;
        end
        else
          GetAllCOMPorts(AList);
      finally
        AReg.Free;
      end;

      {$IFDEF OpenComFilter}
        for i := AList.Count - 1 downto 0 do
        begin
          Res := FileOpen(CComPortPrefix + AList.Strings[i], fmOpenRead or fmShareDenyNone);

          if Res <> $FFFFFFFF then
            FileClose(Res)
          else
            AList.Delete(i);
        end;  
      {$ENDIF}
    {$ELSE}
      {$IFDEF UNIX}
        ListPortsByFilter(AList, CComPortPrefix, 'ttyUSB');
        ListPortsByFilter(AList, CComPortPrefix, 'ttyACM');
        ListPortsByFilter(AList, CComPortPrefix, 'ttyS');
      {$ENDIF}
    {$ENDIF}
  end;
end;


function COMIsConnected(AComName: string): Boolean;
var
  Idx: Integer;
begin
  Result := False;

  Idx := GetCOMIndexByName(AComName);
  if Idx > -1 then
    Result := ComConnections[Idx].ConnHandle > 0;
end;


function COMIsConnected(AConnHandle: THandle): Boolean;
var
  Idx: Integer;
begin
  Result := False;

  Idx := GetCOMIndexByHandle(AConnHandle);
  if Idx > -1 then
    Result := ComConnections[Idx].ConnHandle > 0;
end;


//Returns the number of sent bytes. In case of error, it returns -1 and sets the internal error code, available to GetCOMError function (if available).
function SendDataToCOM(AConnHandle: THandle; var ABuf; AByteCount: Integer): Integer;
var
  Idx: Integer;
begin
  Result := FileWrite(AConnHandle, ABuf, AByteCount);

  if Result = -1 then
  begin
    Idx := GetCOMIndexByHandle(AConnHandle);
    if Idx > -1 then
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
  end;
end;


function ReceiveDataFromCOM(AConnHandle: THandle; out ABuf; AByteCount: Integer): Integer;
var
  Idx: Integer;
begin
  Result := FileRead(AConnHandle, ABuf, AByteCount);

  if Result = -1 then
  begin
    Idx := GetCOMIndexByHandle(AConnHandle);
    if Idx > -1 then
      ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}
  end;
end;


function GetCOMError(AComName: string): Cardinal;
var
  Idx: Integer;
begin
  Result := 0;

  Idx := GetCOMIndexByName(AComName);
  if Idx > -1 then
    Result := ComConnections[Idx].Err;
end;


//Returns additional info. Not all errors will have additional info.
function GetCOMExtraError(AComName: string): string;
var
  Idx: Integer;
begin
  Result := '';

  Idx := GetCOMIndexByName(AComName);
  if Idx > -1 then
    Result := ComConnections[Idx].ErrLoc;
end;

{$IFDEF Windows}
  function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): BOOL; stdcall; external kernel32 name 'GetFileSizeEx';
{$ENDIF}


//Returns number of bytes available to read with ReceiveDataFromCOM. Returns -1 in case of an error. Use GetCOMError to get error code.
function GetReceivedByteCount(AConnHandle: THandle): Integer;
var
  {$IFDEF Windows}
    Stat: TComStat;
    Errors: Cardinal;
  {$ELSE}
    Res: Integer;
  {$ENDIF}
  Idx: Integer;
begin
  Result := -1;  //defaults to error
  {$IFDEF Windows}
    if ClearCommError(AConnHandle, Errors, @Stat) then
      Result := Stat.cbInQue
    else
  {$ELSE}
    Result := FpIoCtl(AConnHandle, FIONREAD, @Res);
    if Result = 0 then    //see https://man7.org for details
      Result := Res
    else
      if Result = -1 then
  {$ENDIF}
    begin
      Idx := GetCOMIndexByHandle(AConnHandle);
      if Idx > -1 then
        ComConnections[Idx].Err := {$IFDEF FPC} GetLastOSError; {$ELSE} GetLastError; {$ENDIF}   //see also fpgeterrno
    end;
end;


procedure CloseAllConnections;
var
  i: Integer;
begin
  for i := Length(ComConnections) - 1 downto 0 do
    DisconnectFromCOMByIndex(i);
end;


initialization
  SetLength(ComConnections, 0);

finalization
  SetLength(ComConnections, 0);

end.

