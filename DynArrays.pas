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

{$IFNDEF IsMCU}
  {$DEFINE IsDesktop}
{$ENDIF}


{$IFDEF FPC}
  {$mode ObjFPC}{$H+}
{$ELSE}

{$ENDIF}

//Dependencies:  MemManager  (see DynTFT repo for mP) if UsingDynTFT or IsMCU, directives are defined.
//MemManager requirements:  open __Lib_MemManager.mpas (or MemManager.pas) and uncomment the header of MM_error function from the interface.
//Whe using __Lib_MemManager.mpas (not MemManager.pas), the NR_FREE_BLOCKS may have to be moved to interface or set to a different value
//Define MMFreeBlocks, for Memory Manager (requires MMFreeBlocks.inc). All the main tests should pass with a minimum value of 6 blocks (the library comes with a default value of 20).
//reate MaxMM.inc, to define the heap size.

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

//ToDo:
  - test with MM(.mpas) and on various cases of fragmentation error and memory full error.
    In case of fragmentation errors, the memory manager becomes unusable, so the allocated data will be lost
  - test on mP that for loops on empty arrays, behave as expected, when TDynArrayLength is DWord instead of LongInt. If not, this has to be changed.
}

{$IFnDEF IsMCU}
  interface
{$ENDIF}


//Desktop, with mP_MM (DynTFT types)  /  with FP_MM
//MCU    , with mP_MM (DynTFT types)  /  with mP_MM (Built-in types)

{$IFDEF IsMCU}
  uses
    {$IFDEF UsingDynTFT}
      DynTFTTypes
      ;
    {$ELSE}
       __Lib_MemManager
       ;
    {$ENDIF}
{$ELSE}
  //Desktop:
  {$IFDEF UsingDynTFT}    //UsingDynTFT should be defined in all DynTFT projects (MCU or Desktop), which will include this unit (DynArrays).
    uses
      MemManager          //there is only the DynTFT type of MM for MCU
      {$IFDEF UsingDynTFT}
      , DynTFTTypes
      ;
      {$ENDIF}
  {$ELSE}
    {$IFDEF UsingMPMM} //mP's memoy manager
      __Lib_MemManager  //users may still want to use the a different flavor of the same memoy manager, without the DynTFT dependencies
    {$ELSE}
      //this is FP's memory manager
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

const
  CMaxDynArrayLength = 65536;  //bytes
  CMaxDynArrayOfDWordLength = 255;  //DWords  ///255 should be enough

type
  //array of byte
  TDynArrayOfByteContent = array[0..CMaxDynArrayLength - 1] of Byte;
  PDynArrayOfByteContent = ^TDynArrayOfByteContent;

  TDynArrayLength = DWord; // {$IFDEF IsDesktop} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP

  TDynArrayOfByte = record
    Len: TDynArrayLength;
    Content: PDynArrayOfByteContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfByte = ^TDynArrayOfByte;


  //array of array of byte
  TDynArrayOfTDynArrayOfByteContent = array[0..CMaxDynArrayLength - 1] of PDynArrayOfByte;
  PDynArrayOfTDynArrayOfByteContent = ^TDynArrayOfTDynArrayOfByteContent;

  TDynArrayOfTDynArrayOfByte = record
    Len: TDynArrayLength;
    Content: PDynArrayOfTDynArrayOfByteContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfTDynArrayOfByte = ^TDynArrayOfTDynArrayOfByte;


  //array of DWord
  TDynArrayOfDWordContent = array[0..CMaxDynArrayOfDWordLength - 1] of DWord;
  PDynArrayOfDWordContent = ^TDynArrayOfDWordContent;

  TDynArrayOfDWordLength = DWord; // {$IFDEF IsDesktop} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP

  TDynArrayOfDWord = record
    Len: TDynArrayOfDWordLength;
    Content: PDynArrayOfDWordContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfDWord = ^TDynArrayOfDWord;


//directives section, copied from DynTFTTypes.pas:
{$IFnDEF IsMCU}
  {$DEFINE IsDesktop}

  {$IFDEF CPU64} // works on FP
    {$IFNDEF AppArch64}
      {$DEFINE AppArch64}
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF AppArch64}
    {$IFNDEF AppArch32}
      {$DEFINE AppArch32}
    {$ENDIF}
  {$ENDIF}

{$ELSE}
  {$IFnDEF UsingDynTFT}
    {$IFDEF AppArch32}     //PIC32
      {$UNDEF AppArch32}
    {$ENDIF}

    {$IFDEF AppArch16}     //dsPIC / PIC24
      {$UNDEF AppArch16}
    {$ENDIF}

    {$IFDEF P30}
      {$DEFINE AppArch16}
    {$ENDIF}

    {$IFDEF P33}
      {$DEFINE AppArch16}
    {$ENDIF}

    {$IFDEF P24}
      {$DEFINE AppArch16}
    {$ENDIF}

    {$IFNDEF AppArch16}    //16-bit not defined,
      {$DEFINE AppArch32}  //then it must be 32-bit !
    {$ENDIF}

    type
      //PByte = ^Byte;
      {$IFDEF AppArch16}
        PByte = ^far const code Byte;
      {$ELSE}
        PByte = ^const code Byte;
      {$ENDIF}
  {$ENDIF}
{$ENDIF}
// end of copied section

{$IFDEF IsMCU}
  PIntPtr = PByte;
{$ENDIF}


procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArray(var AArr: TDynArrayOfByte);
function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddByteToDynArray(AByte: Byte; var AArr: TDynArrayOfByte): Boolean;


procedure InitDynOfDynOfByteToEmpty(var AArr: TDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte): TDynArrayLength;
function SetDynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynOfDynOfByteArray(var AArr: TDynArrayOfTDynArrayOfByte);
function AddDynArrayOfByteToDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; var ANewArr: TDynArrayOfByte): Boolean;
function DeleteItemFromDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; ADelIndex: LongInt): Boolean;


procedure InitDynArrayOfDWordToEmpty(var AArr: TDynArrayOfDWord); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDWordLength(var AArr: TDynArrayOfDWord): TDynArrayOfDWordLength;
function SetDynOfDWordLength(var AArr: TDynArrayOfDWord; ANewLength: TDynArrayOfDWordLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfDWord(var AArr: TDynArrayOfDWord);
function ConcatDynArraysOfDWord(var AArr1, AArr2: TDynArrayOfDWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDWordToDynArraysOfDWord(var AArr: TDynArrayOfDWord; ANewDWord: DWord): Boolean;


{$IFDEF IsDesktop}
  //This check is not available in mP, but is is useful as a debugging means on Desktop.
  procedure CheckInitializedDynArray(var AArr: TDynArrayOfByte);
  procedure CheckInitializedDynOfDynArray(var AArr: TDynArrayOfTDynArrayOfByte);
  procedure CheckInitializedDynArrayOfDWord(var AArr: TDynArrayOfDWord);
{$ENDIF}

function StringToDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfByte): Boolean;   //assumes ADest is initialized
procedure DynArrayOfByteToString(var AArr: TDynArrayOfByte; var ADestStr: string); {$IFDEF IsDesktop} overload; {$ENDIF}
function AddStringToDynOfDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AStr: string; var ADest: TDynArrayOfTDynArrayOfByte): Boolean;

{$IFDEF IsDesktop}
  function DynArrayOfByteToString(var AArr: TDynArrayOfByte): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  function DynOfDynArrayOfByteToString(var AArr: TDynArrayOfTDynArrayOfByte; ASeparator: string = #13#10): string;
{$ENDIF}


implementation


{$IFDEF IsDesktop}
  uses
    SysUtils, Math;

  //These checks are not available in mP, but are useful as a debugging means on Desktop.
  procedure CheckInitializedDynArray(var AArr: TDynArrayOfByte);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynOfDynArray(var AArr: TDynArrayOfTDynArrayOfByte);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynArrayOfDWord(var AArr: TDynArrayOfDWord);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayToEmpty before working with DynArray functions.');
  end;
{$ENDIF}


function StringToDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfByte): Boolean;   //assumes ADest is initialized
var
  TempLen: TDynArrayLength;
begin
  TempLen := TDynArrayLength(Length(AString));
  Result := SetDynLength(ADest, TempLen);

  if not Result then
    Exit;

  {$IFDEF IsDesktop}
    Move(AString[1], ADest.Content^[0], TempLen);
  {$ELSE}
    MemMove(PByte(ADest.Content), PByte(@AString[0]), TempLen);
  {$ENDIF}
end;


procedure DynArrayOfByteToString(var AArr: TDynArrayOfByte; var ADestStr: string);  {$IFDEF IsDesktop} overload; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr);
    SetLength(ADestStr, AArr.Len);
    Move(AArr.Content^[0], ADestStr[1], AArr.Len);
  {$ELSE}
    MemMove(PByte(@ADestStr[0]), PByte(AArr.Content), AArr.Len);
  {$ENDIF}
end;


function AddStringToDynOfDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AStr: string; var ADest: TDynArrayOfTDynArrayOfByte): Boolean;
var
  TempArr: TDynArrayOfByte;
begin
  InitDynArrayToEmpty(TempArr);
  Result := StringToDynArrayOfByte(AStr, TempArr);
  Result := Result and AddDynArrayOfByteToDynOfDynOfByte(ADest, TempArr);
  FreeDynArray(TempArr);
end;


{$IFDEF IsDesktop}
  function DynArrayOfByteToString(var AArr: TDynArrayOfByte): string;  {$IFDEF IsDesktop} overload; {$ENDIF}
  begin
    Result := 'no string content';
    DynArrayOfByteToString(AArr, Result);
  end;


  function DynOfDynArrayOfByteToString(var AArr: TDynArrayOfTDynArrayOfByte; ASeparator: string = #13#10): string;
  var
    i: Integer;
    TempStr: string;
  begin
    Result := '';
    for i := 0 to LongInt(AArr.Len) - 1 do
    begin
      TempStr := 'some init value';
      DynArrayOfByteToString(AArr.Content^[i]^, TempStr);
      Result := Result + TempStr + ASeparator;
    end;
  end;
{$ENDIF}


//array of byte

procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      {$IFDEF UsingDynTFT}
        {$IFDEF IsMCU}
          FreeMem(AArr.Content, AArr.Len);
        {$ELSE}
          FreeMem(TPtrRec(AArr.Content), AArr.Len);
        {$ENDIF}
      {$ELSE}
        FreeMem(AArr.Content, AArr.Len);
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  OldPointer := PIntPtr(AArr.Content);

  {$IFnDEF IsDesktop}
    GetMem(AArr.Content, ANewLength);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    MemMove(AArr.Content, OldPointer, Min(ANewLength, AArr.Len));   //OldPointer = src, AArr.Content = dest
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength);
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength);
      {$ENDIF}

      //AArr.Len is still the old array length. Only the Content field points somewhere else.
      Move(OldPointer^, AArr.Content^, Min(ANewLength, AArr.Len))   // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFDEF UsingDynTFT}
      {$IFDEF IsMCU}
        FreeMem(TPtrRec(OldPointer), AArr.Len);
      {$ELSE}
        FreeMem(TPtrRec(OldPointer), AArr.Len);
      {$ENDIF}
    {$ELSE}
      FreeMem(OldPointer, AArr.Len);
    {$ENDIF}
  end;

  AArr.Len := ANewLength;
end;


procedure FreeDynArray(var AArr: TDynArrayOfByte);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr);
  {$ENDIF}

  SetDynLength(AArr, 0);
end;


function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  NewLen: DWord;
  NewPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
  OldArr1Len: DWord;
begin
  Result := False;
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr1);
    CheckInitializedDynArray(AArr2);
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
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len;
    MemMove(NewPointer, AArr2.Content, AArr2.Len);
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len));  //NewPointer := @AArr1.Content^[OldArr1Len];
    Move(AArr2.Content^, NewPointer^, AArr2.Len);
  {$ENDIF}

  Result := True;
end;


function AddByteToDynArray(AByte: Byte; var AArr: TDynArrayOfByte): Boolean;
begin
  Result := SetDynLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := AByte;
end;


// array of array of byte

procedure InitDynOfDynOfByteToEmpty(var AArr: TDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: PDynArrayOfTDynArrayOfByteContent;
  i, MaxCopyIdx: LongInt;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      for i := 0 to LongInt(AArr.Len) - 1 do
      begin
        FreeDynArray(AArr.Content^[i]^);

        {$IFnDEF IsDesktop}
          Freemem(AArr.Content^[i], SizeOf(TDynArrayOfByte));
        {$ELSE}
          Dispose(AArr.Content^[i]);
        {$ENDIF}
      end;

      {$IFDEF UsingDynTFT}
        {$IFDEF IsMCU}
          FreeMem(AArr.Content, AArr.Len * SizeOf(PDynArrayOfByte));
        {$ELSE}
          FreeMem(TPtrRec(AArr.Content), AArr.Len * SizeOf(PDynArrayOfByte));
        {$ENDIF}
      {$ELSE}
        FreeMem(AArr.Content, AArr.Len * SizeOf(PDynArrayOfByte));
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(AArr.Content);
  {$ELSE}
    OldPointer := PDynArrayOfTDynArrayOfByteContent(AArr.Content);
  {$ENDIF}

  {$IFnDEF IsDesktop}
    GetMem(AArr.Content, ANewLength * SizeOf(PDynArrayOfByte));    //SizeOf pointer
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    for i := 0 to ANewLength - 1 do
    begin
      GetMem(AArr.Content^[i], SizeOf(TDynArrayOfByte));           //SizeOf struct
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    end;
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength * SizeOf(PDynArrayOfByte)); //SizeOf pointer
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength * SizeOf(PDynArrayOfByte)); //SizeOf pointer
      {$ENDIF}
                                              /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.
      for i := 0 to ANewLength - 1 do
        New(AArr.Content^[i]);           //SizeOf struct    //Using New instead of GetMem, to properly initialize the "Initialized" field (string). GetMem doesn't do any form of initialization to the allocated structure.
    except
      Result := False;
    end;
  {$ENDIF}

  MaxCopyIdx := LongInt(Min(AArr.Len, ANewLength)) - 1;
  for i := 0 to MaxCopyIdx do
  begin
    InitDynArrayToEmpty(AArr.Content^[i]^);
    if not ConcatDynArrays(AArr.Content^[i]^, OldPointer^[i]^) then   /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.
    begin
      Result := False;
      Exit;
    end;
  end;

  if AArr.Len > 0 then
  begin
    for i := 0 to LongInt(AArr.Len) - 1 do
    begin
      FreeDynArray(OldPointer^[i]^);    /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.

      {$IFnDEF IsDesktop}
        Freemem(OldPointer^[i], SizeOf(TDynArrayOfByte));
      {$ELSE}
        Dispose(OldPointer^[i]);
      {$ENDIF}
    end;

    {$IFDEF UsingDynTFT}
      {$IFDEF IsMCU}
        FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
      {$ELSE}
        FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfByte));
      {$ENDIF}
    {$ELSE}
      FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
    {$ENDIF}
  end;

  if ANewLength > AArr.Len then
    for i := AArr.Len to ANewLength - 1 do
      InitDynArrayToEmpty(AArr.Content^[i]^);

  AArr.Len := ANewLength;
end;


function AddDynArrayOfByteToDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; var ANewArr: TDynArrayOfByte): Boolean;
begin
  Result := SetDynOfDynOfByteLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  Result := ConcatDynArrays(AArr.Content^[AArr.Len - 1]^, ANewArr);
end;


function DeleteItemFromDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; ADelIndex: LongInt): Boolean;
var
  i: Integer;
  OldPointer: PDynArrayOfTDynArrayOfByteContent;
  NewLen: DWord;
begin
  Result := False;

  if (ADelIndex < 0) or (ADelIndex > AArr.Len - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynOfDynArrayOfByte.');
    {$ELSE}
      Exit;
    {$ENDIF}
  end;

  for i := ADelIndex to AArr.Len - 2 do
  begin
    FreeDynArray(AArr.Content^[i]^);

    {$IFnDEF IsDesktop}
      Freemem(AArr.Content^[i], SizeOf(TDynArrayOfByte));
    {$ELSE}
      Dispose(AArr.Content^[i]);
    {$ENDIF}

    {$IFnDEF IsDesktop}
      GetMem(AArr.Content^[i], SizeOf(TDynArrayOfByte));           //SizeOf struct
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    {$ELSE}
      New(AArr.Content^[i]);
    {$ENDIF}

    InitDynArrayToEmpty(AArr.Content^[i]^);
    Result := ConcatDynArrays(AArr.Content^[i]^, AArr.Content^[i + 1]^);
    if not Result then
      Exit;
  end;

  FreeDynArray(AArr.Content^[AArr.Len - 1]^);

  {$IFnDEF IsDesktop}
    Freemem(AArr.Content^[AArr.Len - 1], SizeOf(TDynArrayOfByte));
  {$ELSE}
    Dispose(AArr.Content^[AArr.Len - 1]);
  {$ENDIF}

  NewLen := AArr.Len - 1;
  OldPointer := AArr.Content;

  if NewLen > 0 then
  begin
    {$IFnDEF IsDesktop}
      GetMem(AArr.Content, NewLen * SizeOf(PDynArrayOfByte));    //SizeOf pointer
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    {$ELSE}
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), NewLen * SizeOf(PDynArrayOfByte)); //SizeOf pointer
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, NewLen * SizeOf(PDynArrayOfByte)); //SizeOf pointer
      {$ENDIF}
    {$ENDIF}
  end;

  {$IFnDEF IsDesktop}
    MemMove(AArr.Content, OldPointer, NewLen * SizeOf(PDynArrayOfByte));
  {$ELSE}
    Move(OldPointer^, AArr.Content^, NewLen * SizeOf(PDynArrayOfByte));
  {$ENDIF}

  {$IFDEF UsingDynTFT}
    {$IFDEF IsMCU}
      FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
    {$ELSE}
      FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfByte));
    {$ENDIF}
  {$ELSE}
    FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
  {$ENDIF}

  AArr.Len := AArr.Len - 1;
  Result := True;
end;


procedure FreeDynOfDynOfByteArray(var AArr: TDynArrayOfTDynArrayOfByte);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr);
  {$ENDIF}

  SetDynOfDynOfByteLength(AArr, 0);
end;


//array of DWord

procedure InitDynArrayOfDWordToEmpty(var AArr: TDynArrayOfDWord); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfDWordLength(var AArr: TDynArrayOfDWord): TDynArrayOfDWordLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfDWordLength(var AArr: TDynArrayOfDWord; ANewLength: TDynArrayOfDWordLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      {$IFDEF UsingDynTFT}
        {$IFDEF IsMCU}
          FreeMem(AArr.Content, AArr.Len shl 2);
        {$ELSE}
          FreeMem(TPtrRec(AArr.Content), AArr.Len shl 2);
        {$ENDIF}
      {$ELSE}
        FreeMem(AArr.Content, AArr.Len shl 2);
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  OldPointer := PIntPtr(AArr.Content);

  {$IFnDEF IsDesktop}
    GetMem(AArr.Content, ANewLength shl 2);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    MemMove(AArr.Content, OldPointer, Min(ANewLength, AArr.Len) shl 2);  //OldPointer = src, AArr.Content = dest
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength shl 2);
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength shl 2);
      {$ENDIF}

      //AArr.Len is still the old array length. Only the Content field points somewhere else.
      Move(OldPointer^, AArr.Content^, Min(ANewLength, AArr.Len) shl 2);   // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFDEF UsingDynTFT}
      {$IFDEF IsMCU}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl 2);
      {$ELSE}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl 2);
      {$ENDIF}
    {$ELSE}
      FreeMem(OldPointer, AArr.Len shl 2);
    {$ENDIF}
  end;

  AArr.Len := ANewLength;
end;


procedure FreeDynArrayOfDWord(var AArr: TDynArrayOfDWord);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  SetDynOfDWordLength(AArr, 0);
end;


function ConcatDynArraysOfDWord(var AArr1, AArr2: TDynArrayOfDWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  NewLen: DWord;
  NewPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
  OldArr1Len: DWord;
begin
  Result := False;
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr1);
    CheckInitializedDynArrayOfDWord(AArr2);
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
  Result := SetDynOfDWordLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len shl 2;
    MemMove(NewPointer, AArr2.Content, AArr2.Len shl 2);
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len shl 2));  //NewPointer := @AArr1.Content^[OldArr1Len shl 2];
    Move(AArr2.Content^, NewPointer^, AArr2.Len shl 2);
  {$ENDIF}

  Result := True;
end;


function AddDWordToDynArraysOfDWord(var AArr: TDynArrayOfDWord; ANewDWord: DWord): Boolean;
begin
  Result := SetDynOfDWordLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := ANewDWord;
end;


end.
