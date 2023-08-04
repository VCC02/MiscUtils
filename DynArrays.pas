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
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
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

//directives section, copied from DynTFTTypes.pas:
{$IFnDEF IsMCU}
  //{$DEFINE IsDesktop}  //already done, at the beginning of the unit

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

    //type
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
{$ELSE}
  {$IFDEF ExtraType}
    PIntPtr = PByte;
  {$ENDIF}
{$ENDIF}


{$IFDEF DefineDWord}
  DWord = Cardinal;
{$ENDIF}


{$IFDEF ExtraType}
  {$IFDEF AppArch64}
    {$IFnDEF FPC}
      QWord = Int64; //If QWord is available in newer Delphi versions, this definition might cause further incompatibilities.
    {$ENDIF}

    PtrUInt = QWord;
  {$ENDIF}
  
  {$IFDEF AppArch32}
    PtrUInt = DWord;
  {$ENDIF}

  {$IFDEF AppArch16}
    PtrUInt = Word;
  {$ENDIF}
{$ELSE}
  {$IFDEF IsMCU}
    {$IFDEF AppArch32}
      PtrUInt = DWord;
    {$ENDIF}

    {$IFDEF AppArch16}
      PtrUInt = Word;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

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


  //array of Pointer (PtrUInt)
  TDynArrayOfPtrUIntContent = array[0..CMaxDynArrayOfDWordLength - 1] of PtrUInt;
  PDynArrayOfPtrUIntContent = ^TDynArrayOfPtrUIntContent;

  TDynArrayOfPtrUIntLength = DWord; // {$IFDEF IsDesktop} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP

  TDynArrayOfPtrUInt = record
    Len: TDynArrayOfPtrUIntLength;
    Content: PDynArrayOfPtrUIntContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfPtrUInt = ^TDynArrayOfPtrUInt;


  //array of PDynArrayOfTDynArrayOfByte   -  array of pointers to array of array of byte
  TDynArrayOfPDynArrayOfTDynArrayOfByteContent = array[0..CMaxDynArrayOfDWordLength - 1] of PDynArrayOfTDynArrayOfByte;
  PDynArrayOfPDynArrayOfTDynArrayOfByteContent = ^TDynArrayOfPDynArrayOfTDynArrayOfByteContent;

  TDynArrayOfPDynArrayOfTDynArrayOfByteLength = DWord; // {$IFDEF IsDesktop} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP

  TDynArrayOfPDynArrayOfTDynArrayOfByte = record
    Len: TDynArrayOfPDynArrayOfTDynArrayOfByteLength;
    Content: PDynArrayOfPDynArrayOfTDynArrayOfByteContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfPDynArrayOfTDynArrayOfByte = ^TDynArrayOfPDynArrayOfTDynArrayOfByte;

const
  {$IFDEF AppArch64}
    CArchBitShift = 4;   //shl 4 means  multiply by SizeOf(Pointer)
  {$ENDIF}

  {$IFDEF AppArch32}
    CArchBitShift = 2;   //shl 2 means  multiply by SizeOf(Pointer)
  {$ENDIF}

  {$IFDEF AppArch16}
    CArchBitShift = 1;   //shl 1 means  multiply by SizeOf(Pointer)
  {$ENDIF}



procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArray(var AArr: TDynArrayOfByte);
function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddByteToDynArray(AByte: Byte; var AArr: TDynArrayOfByte): Boolean;
function RemoveStartBytesFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfByte): Boolean;


procedure InitDynOfDynOfByteToEmpty(var AArr: TDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte): TDynArrayLength;
function SetDynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynOfDynOfByteArray(var AArr: TDynArrayOfTDynArrayOfByte);
function ConcatDynOfDynOfByteArrays(var AArr1, AArr2: TDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDynArrayOfByteToDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; var ANewArr: TDynArrayOfByte): Boolean;  //adds the new array to the outer array
function DeleteItemFromDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; ADelIndex: LongInt): Boolean;


procedure InitDynArrayOfDWordToEmpty(var AArr: TDynArrayOfDWord); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDWordLength(var AArr: TDynArrayOfDWord): TDynArrayOfDWordLength;
function SetDynOfDWordLength(var AArr: TDynArrayOfDWord; ANewLength: TDynArrayOfDWordLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfDWord(var AArr: TDynArrayOfDWord);
function ConcatDynArraysOfDWord(var AArr1, AArr2: TDynArrayOfDWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDWordToDynArraysOfDWord(var AArr: TDynArrayOfDWord; ANewDWord: DWord): Boolean;


procedure InitDynArrayOfPtrUIntToEmpty(var AArr: TDynArrayOfPtrUInt); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt): TDynArrayOfPtrUIntLength;
function SetDynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt; ANewLength: TDynArrayOfPtrUIntLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
function ConcatDynArraysOfPtrUInt(var AArr1, AArr2: TDynArrayOfPtrUInt): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddPtrUIntToDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ANewPtrUInt: PtrUInt): Boolean;
//function DeleteItemFromPDynArraysOfPtrUInt(AArr: PDynArrayOfPtrUInt; ADelIndex: LongInt): Boolean;
function DeleteItemFromDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ADelIndex: LongInt): Boolean;


procedure InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte): TDynArrayOfPDynArrayOfTDynArrayOfByteLength;
function SetDynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayOfPDynArrayOfTDynArrayOfByteLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
function ConcatDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr1, AArr2: TDynArrayOfPDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; var ANewDynArrayOfTDynArrayOfByte: TDynArrayOfTDynArrayOfByte): Boolean;
function DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ADelIndex: LongInt): Boolean;


{$IFDEF IsDesktop}
  //This check is not available in mP, but is is useful as a debugging means on Desktop.
  procedure CheckInitializedDynArray(var AArr: TDynArrayOfByte);
  procedure CheckInitializedDynOfDynArray(var AArr: TDynArrayOfTDynArrayOfByte);
  procedure CheckInitializedDynArrayOfDWord(var AArr: TDynArrayOfDWord);
  procedure CheckInitializedDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
  procedure CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
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
      raise Exception.Create('The DynArray is not initialized. Please call InitDynOfDynOfByteToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynArrayOfDWord(var AArr: TDynArrayOfDWord);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayOfDWordToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayOfPtrUIntToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty before working with DynArray functions.');
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


function RemoveStartBytesFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfByte): Boolean;
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  Result := True;

  if ACount > AArr.Len then
  begin
    FreeDynArray(AArr);
    Exit;
  end;

  if ACount = 0 then
    Exit;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(AArr.Content) + ACount;
    MemMove(AArr.Content, OldPointer, ACount);
  {$ELSE}
    OldPointer := Pointer(PtrUInt(AArr.Content) + PtrUInt(ACount));
    Move(OldPointer^, AArr.Content^, ACount);
  {$ENDIF}

  SetDynLength(AArr, AArr.Len - ACount);
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
  if (ADelIndex < 0) or (ADelIndex > LongInt(AArr.Len) - 1) then
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


function ConcatDynOfDynOfByteArrays(var AArr1, AArr2: TDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  i: LongInt;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr1);
    CheckInitializedDynOfDynArray(AArr2);
  {$ENDIF}

  Result := True;
  for i := 0 to LongInt(AArr2.Len) - 1 do
  begin
    Result := Result and AddDynArrayOfByteToDynOfDynOfByte(AArr1, AArr2.Content^[i]^);
    if not Result then
      Exit;
  end;
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


//array of PtrUInt

procedure InitDynArrayOfPtrUIntToEmpty(var AArr: TDynArrayOfPtrUInt); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt): TDynArrayOfPtrUIntLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt; ANewLength: TDynArrayOfPtrUIntLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      {$IFDEF UsingDynTFT}
        {$IFDEF IsMCU}
          FreeMem(AArr.Content, AArr.Len shl CArchBitShift); 
        {$ELSE}
          FreeMem(TPtrRec(AArr.Content), AArr.Len shl CArchBitShift);
        {$ENDIF}
      {$ELSE}
        FreeMem(AArr.Content, AArr.Len shl CArchBitShift);
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  OldPointer := PIntPtr(AArr.Content);

  {$IFnDEF IsDesktop}
    GetMem(AArr.Content, ANewLength shl CArchBitShift);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    MemMove(AArr.Content, OldPointer, Min(ANewLength, AArr.Len) shl CArchBitShift);  //OldPointer = src, AArr.Content = dest
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength shl CArchBitShift);
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength shl CArchBitShift);
      {$ENDIF}

      //AArr.Len is still the old array length. Only the Content field points somewhere else.
      Move(OldPointer^, AArr.Content^, Min(ANewLength, AArr.Len) shl CArchBitShift);   // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFDEF UsingDynTFT}
      {$IFDEF IsMCU}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl CArchBitShift);
      {$ELSE}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl CArchBitShift);
      {$ENDIF}
    {$ELSE}
      FreeMem(OldPointer, AArr.Len shl CArchBitShift);
    {$ENDIF}
  end;

  AArr.Len := ANewLength;
end;


procedure FreeDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr);
  {$ENDIF}

  SetDynOfPtrUIntLength(AArr, 0);
end;


function ConcatDynArraysOfPtrUInt(var AArr1, AArr2: TDynArrayOfPtrUInt): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  NewLen: DWord;
  NewPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF} //////////////////////////////////////////////// is it OK to be DWord in 16-bit ?????
  OldArr1Len: DWord;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr1);
    CheckInitializedDynArrayOfPtrUInt(AArr2);
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
  Result := SetDynOfPtrUIntLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len shl CArchBitShift;
    MemMove(NewPointer, AArr2.Content, AArr2.Len shl CArchBitShift);
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len shl CArchBitShift));  //NewPointer := @AArr1.Content^[OldArr1Len shl CArchBitShift];
    Move(AArr2.Content^, NewPointer^, AArr2.Len shl CArchBitShift);
  {$ENDIF}

  Result := True;
end;


function AddPtrUIntToDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ANewPtrUInt: PtrUInt): Boolean;
begin
  Result := SetDynOfPtrUIntLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := ANewPtrUInt;
end;


function DeleteItemFromDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ADelIndex: LongInt): Boolean;
var
  DelPointer, NextPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  if (ADelIndex < 0) or (ADelIndex > LongInt(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynArrayOfPtrUInt.');
    {$ELSE}
      Exit;
    {$ENDIF}
  end;

  if AArr.Len = 1 then
  begin
    FreeDynArrayOfPtrUInt(AArr);
    Result := True;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    DelPointer := DWord(AArr.Content) + ADelIndex shl CArchBitShift;
    NextPointer := DWord(AArr.Content) + (ADelIndex + 1) shl CArchBitShift;
    MemMove(DelPointer, NextPointer, (AArr.Len - ADelIndex - 1) shl CArchBitShift);
  {$ELSE}
    DelPointer := Pointer(PtrUInt(AArr.Content) + PtrUInt(ADelIndex shl CArchBitShift));  //NewPointer := @AArr.Content^[ADelIndex shl CArchBitShift];
    NextPointer := Pointer(PtrUInt(AArr.Content) + PtrUInt((ADelIndex + 1) shl CArchBitShift));   //NextPointer := DelPointer + SizeOf(Pointer);
    Move(NextPointer^, DelPointer^, (AArr.Len - ADelIndex - 1) shl CArchBitShift);
  {$ENDIF}

  Result := SetDynOfPtrUIntLength(AArr, AArr.Len - 1);
end;


////////// array of TDynArrayOfTDynArrayOfByte

procedure InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil;

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}

  //InitDynArrayOfPtrUIntToEmpty is not called here
end;


function DynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte): TDynArrayOfPDynArrayOfTDynArrayOfByteLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayOfPDynArrayOfTDynArrayOfByteLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldLen: TDynArrayOfPDynArrayOfTDynArrayOfByteLength;
  i: LongInt;
  {$IFDEF IsMCU}
    TempDynArrayOfPtrUInt: TDynArrayOfPtrUInt;
  {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
  {$ENDIF}

  Result := False;
  OldLen := AArr.Len;

  if ANewLength = OldLen then
  begin
    Result := True;
    Exit;
  end;

  if ANewLength < OldLen then
  begin
    for i := ANewLength to OldLen - 1 do
    begin
      FreeDynOfDynOfByteArray(AArr.Content^[i]^);

      {$IFnDEF MCU}
        Dispose(AArr.Content^[i]);
      {$ELSE}
        FreeMem(AArr.Content^[i], SizeOf(TDynArrayOfTDynArrayOfByte));
      {$ENDIF}
    end;
  end;

  {$IFnDEF IsMCU}
    Result := SetDynOfPtrUIntLength(TDynArrayOfPtrUInt(AArr), ANewLength);
  {$ELSE}
    TempDynArrayOfPtrUInt.Len := AArr.Len;
    TempDynArrayOfPtrUInt.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr.Content));
    Result := SetDynOfPtrUIntLength(TempDynArrayOfPtrUInt, ANewLength);
    AArr.Len := TempDynArrayOfPtrUInt.Len;
    AArr.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
  {$ENDIF}

  if not Result then
    Exit;

  for i := OldLen to ANewLength - 1 do
  begin
    {$IFnDEF MCU}
      New(AArr.Content^[i]);  //AArr.Content^[i] is a pointer to a PDynArrayOfTDynArrayOfByte
    {$ELSE}
      GetMem(AArr.Content^[i], SizeOf(TDynArrayOfTDynArrayOfByte));
    {$ENDIF}

    InitDynOfDynOfByteToEmpty(AArr.Content^[i]^);
  end;

  Result := True;
end;


procedure FreeDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
  {$ENDIF}

  SetDynOfPDynArrayOfTDynArrayOfByteLength(AArr, 0);
end;


function ConcatDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr1, AArr2: TDynArrayOfPDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
{$IFDEF IsMCU}
  var
    TempDynArrayOfPtrUInt1: TDynArrayOfPtrUInt;
    TempDynArrayOfPtrUInt2: TDynArrayOfPtrUInt;
{$ENDIF}
begin
  {$IFnDEF IsMCU}
    Result := ConcatDynArraysOfPtrUInt(TDynArrayOfPtrUInt(AArr1), TDynArrayOfPtrUInt(AArr2));
  {$ELSE}
    TempDynArrayOfPtrUInt1.Len := AArr1.Len;
    TempDynArrayOfPtrUInt2.Len := AArr2.Len;
    TempDynArrayOfPtrUInt1.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr1.Content));
    TempDynArrayOfPtrUInt2.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr2.Content));

    Result := ConcatDynArraysOfPtrUInt(TempDynArrayOfPtrUInt1, TempDynArrayOfPtrUInt2);

    AArr1.Len := TempDynArrayOfPtrUInt1.Len;
    AArr2.Len := TempDynArrayOfPtrUInt2.Len;
    AArr1.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt1.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
    AArr2.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt2.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
  {$ENDIF}
end;


function AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; var ANewDynArrayOfTDynArrayOfByte: TDynArrayOfTDynArrayOfByte): Boolean;
var
{$IFDEF IsMCU}
  TempDynArrayOfPtrUInt: TDynArrayOfPtrUInt;
{$ENDIF}
  TempNewPDynArrayOfTDynArrayOfByte: PDynArrayOfTDynArrayOfByte;
begin
  Result := True;

  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
    CheckInitializedDynOfDynArray(ANewDynArrayOfTDynArrayOfByte);
  {$ENDIF}

  {$IFnDEF IsMCU}
    New(TempNewPDynArrayOfTDynArrayOfByte);
  {$ELSE}
    GetMem(TempNewPDynArrayOfTDynArrayOfByte, SizeOf(TempNewPDynArrayOfTDynArrayOfByte^));
  {$ENDIF}

  InitDynOfDynOfByteToEmpty(TempNewPDynArrayOfTDynArrayOfByte^);

  Result := ConcatDynOfDynOfByteArrays(TempNewPDynArrayOfTDynArrayOfByte^, ANewDynArrayOfTDynArrayOfByte);
  if not Result then
    Exit;

  {$IFnDEF IsMCU}
    Result := AddPtrUIntToDynArraysOfPtrUInt(TDynArrayOfPtrUInt(AArr), PtrUInt(TempNewPDynArrayOfTDynArrayOfByte));
  {$ELSE}
    TempDynArrayOfPtrUInt.Len := AArr.Len;
    TempDynArrayOfPtrUInt.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr.Content));
    Result := AddPtrUIntToDynArraysOfPtrUInt(TempDynArrayOfPtrUInt, PtrUInt(TempNewPDynArrayOfTDynArrayOfByte));
    AArr.Len := TempDynArrayOfPtrUInt.Len;
    AArr.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
  {$ENDIF}
end;


function DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ADelIndex: LongInt): Boolean;
{$IFDEF IsMCU}
  var
    TempDynArrayOfPtrUInt: TDynArrayOfPtrUInt;
{$ENDIF}
begin
  if (ADelIndex < 0) or (ADelIndex > LongInt(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynArrayOfPDynArrayOfTDynArrayOfByte.');
    {$ELSE}
      Exit;
    {$ENDIF}
  end;

  if AArr.Len = 1 then
  begin
    FreeDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
    Result := True;
    Exit;
  end;

  FreeDynOfDynOfByteArray(AArr.Content^[ADelIndex]^);

  {$IFnDEF IsMCU}
    Result := DeleteItemFromDynArraysOfPtrUInt(TDynArrayOfPtrUInt(AArr), ADelIndex);
  {$ELSE}
    TempDynArrayOfPtrUInt.Len := AArr.Len;
    TempDynArrayOfPtrUInt.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr.Content));
    Result := DeleteItemFromDynArraysOfPtrUInt(TempDynArrayOfPtrUInt, ADelIndex);
    AArr.Len := TempDynArrayOfPtrUInt.Len;
    AArr.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
  {$ENDIF}
end;

end.
