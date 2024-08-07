{
    Copyright (C) 2024 VCC
    creation date: 23 Apr 2023
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
{$ENDIF}

//Dependencies:  MemManager  (see DynTFT repo for mP) if UsingDynTFT or IsMCU, directives are defined.
//MemManager requirements:  open __Lib_MemManager.mpas (or MemManager.pas) and uncomment the header of MM_error function from the interface.
//Whe using __Lib_MemManager.mpas (not MemManager.pas), the NR_FREE_BLOCKS may have to be moved to interface or set to a different value
//Define MMFreeBlocks, for Memory Manager (requires MMFreeBlocks.inc). All the main tests should pass with a minimum value of 6 blocks (the library comes with a default value of 20).
//Create/Update MaxMM.inc file, to define the heap size.

{  How to use:
  - Before calling any of the DynLength, SetDynLength or ConcatDynArrays functions, users have to call InitDynArrayToEmpty.
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
  - The implementations of TDynArrayOfByte, TDynArrayOfWord and TDynArrayOfDWord might share a common implementation, where a data size is passed to each function, for allocation / deallocation.
    This should decrease the code size at the expense of a stack level. The complez arrays (array of array of..) might also require small changes.
}

{$IFnDEF IsMCU}
  {$IFDEF FPC}
    {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
  {$ENDIF}
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
      __Lib_MemManager  //users may still want to use a different flavor of the same memoy manager, without the DynTFT dependencies
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
        PByte = ^far const code Byte;   //this is how DynTFT defines PByte
        //PByte = ^Byte;
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

  PPtrUInt = ^PtrUInt;
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


  {$IFDEF AppArch64}
    TDynArrayLength = QWord;
    TDynArrayLengthSig = Int64;
  {$ELSE}
    TDynArrayLength = DWord; // {$IFDEF IsDesktop} SmallInt {$ELSE} Integer {$ENDIF}; //16-bit on mP
    TDynArrayLengthSig = LongInt;
  {$ENDIF}

  //-------------------------------------
  //array of Byte
  TDynArrayOfByteContent = array[0..CMaxDynArrayLength - 1] of Byte;
  PDynArrayOfByteContent = ^TDynArrayOfByteContent;

  TDynArrayOfByte = record
    Len: TDynArrayLength;
    Content: PDynArrayOfByteContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfByte = ^TDynArrayOfByte;


  //-------------------------------------
  //array of array of Byte
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


  //-------------------------------------
  //array of Word
  TDynArrayOfWordContent = array[0..CMaxDynArrayLength - 1] of Word;
  PDynArrayOfWordContent = ^TDynArrayOfWordContent;

  TDynArrayOfWord = record
    Len: TDynArrayLength;
    Content: PDynArrayOfWordContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfWord = ^TDynArrayOfWord;


  //-------------------------------------
  //array of array of Word
  TDynArrayOfTDynArrayOfWordContent = array[0..CMaxDynArrayLength - 1] of PDynArrayOfWord;
  PDynArrayOfTDynArrayOfWordContent = ^TDynArrayOfTDynArrayOfWordContent;

  TDynArrayOfTDynArrayOfWord = record
    Len: TDynArrayLength;
    Content: PDynArrayOfTDynArrayOfWordContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfTDynArrayOfWord = ^TDynArrayOfTDynArrayOfWord;


  //-------------------------------------
  //array of DWord
  TDynArrayOfDWordContent = array[0..CMaxDynArrayOfDWordLength - 1] of DWord;
  PDynArrayOfDWordContent = ^TDynArrayOfDWordContent;


  TDynArrayOfDWord = record
    Len: TDynArrayLength;
    Content: PDynArrayOfDWordContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfDWord = ^TDynArrayOfDWord;


  //-------------------------------------
  //array of Pointer (PtrUInt)
  TDynArrayOfPtrUIntContent = array[0..CMaxDynArrayOfDWordLength - 1] of PtrUInt;
  PDynArrayOfPtrUIntContent = ^TDynArrayOfPtrUIntContent;


  TDynArrayOfPtrUInt = record
    Len: TDynArrayLength;
    Content: PDynArrayOfPtrUIntContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfPtrUInt = ^TDynArrayOfPtrUInt;


  //-------------------------------------
  //array of PDynArrayOfTDynArrayOfByte   -  array of pointers to array of array of byte
  TDynArrayOfPDynArrayOfTDynArrayOfByteContent = array[0..CMaxDynArrayOfDWordLength - 1] of PDynArrayOfTDynArrayOfByte;
  PDynArrayOfPDynArrayOfTDynArrayOfByteContent = ^TDynArrayOfPDynArrayOfTDynArrayOfByteContent;


  TDynArrayOfPDynArrayOfTDynArrayOfByte = record
    Len: TDynArrayLength;
    Content: PDynArrayOfPDynArrayOfTDynArrayOfByteContent;
    {$IFDEF IsDesktop}
      Initialized: string; //strings are automatically initialized to empty in FP
    {$ENDIF}
  end;

  PDynArrayOfPDynArrayOfTDynArrayOfByte = ^TDynArrayOfPDynArrayOfTDynArrayOfByte;

const
  {$IFDEF AppArch64}
    CArchBitShift = 3;   //shl 3 means  multiply by SizeOf(Pointer)
    CArchBitMask = 7;
  {$ENDIF}

  {$IFDEF AppArch32}
    CArchBitShift = 2;   //shl 2 means  multiply by SizeOf(Pointer)
    CArchBitMask = 3;
  {$ENDIF}

  {$IFDEF AppArch16}
    CArchBitShift = 1;   //shl 1 means  multiply by SizeOf(Pointer)
    CArchBitMask = 1;
  {$ENDIF}



procedure InitDynArrayToEmpty(var AArr: TDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynLength(var AArr: TDynArrayOfByte): TDynArrayLength;
function SetDynLength(var AArr: TDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArray(var AArr: TDynArrayOfByte);
function ConcatDynArrays(var AArr1, AArr2: TDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddByteToDynArray(AByte: Byte; var AArr: TDynArrayOfByte): Boolean;
function RemoveStartBytesFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfByte): Boolean;
function CopyFromDynArray(var ADestArr, ASrcArr: TDynArrayOfByte; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
function DeleteItemFromDynArray(var AArr: TDynArrayOfByte; ADelIndex: TDynArrayLength): Boolean;


procedure InitDynOfDynOfByteToEmpty(var AArr: TDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte): TDynArrayLength;
function SetDynOfDynOfByteLength(var AArr: TDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynOfDynOfByteArray(var AArr: TDynArrayOfTDynArrayOfByte);
function ConcatDynOfDynOfByteArrays(var AArr1, AArr2: TDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDynArrayOfByteToDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; var ANewArr: TDynArrayOfByte): Boolean;  //adds the new array to the outer array
function DeleteItemFromDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; ADelIndex: TDynArrayLengthSig): Boolean;


procedure InitDynArrayOfWordToEmpty(var AArr: TDynArrayOfWord); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfWordLength(var AArr: TDynArrayOfWord): TDynArrayLength;
function SetDynOfWordLength(var AArr: TDynArrayOfWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfWord(var AArr: TDynArrayOfWord);
function ConcatDynArraysOfWord(var AArr1, AArr2: TDynArrayOfWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddWordToDynArraysOfWord(var AArr: TDynArrayOfWord; ANewWord: Word): Boolean;
function RemoveStartWordsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfWord): Boolean;
function CopyFromDynArrayOfWord(var ADestArr, ASrcArr: TDynArrayOfWord; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
function IndexOfWordInArrayOfWord(var AArr: TDynArrayOfWord; AWordToFind: Word): TDynArrayLengthSig; //returns -1 if not found
function DeleteItemFromDynArrayOfWord(var AArr: TDynArrayOfWord; ADelIndex: TDynArrayLength): Boolean;
function CreateUniqueWordWithStart(var AArr: TDynArrayOfWord; AStartAt: Word): Word;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
function CreateUniqueWord(var AArr: TDynArrayOfWord): Word;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.


procedure InitDynOfDynOfWordToEmpty(var AArr: TDynArrayOfTDynArrayOfWord); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDynOfWordLength(var AArr: TDynArrayOfTDynArrayOfWord): TDynArrayLength;
function SetDynOfDynOfWordLength(var AArr: TDynArrayOfTDynArrayOfWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynOfDynOfWordArray(var AArr: TDynArrayOfTDynArrayOfWord);
function ConcatDynOfDynOfWordArrays(var AArr1, AArr2: TDynArrayOfTDynArrayOfWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDynArrayOfWordToDynOfDynOfWord(var AArr: TDynArrayOfTDynArrayOfWord; var ANewArr: TDynArrayOfWord): Boolean;  //adds the new array to the outer array
function DeleteItemFromDynOfDynOfWord(var AArr: TDynArrayOfTDynArrayOfWord; ADelIndex: TDynArrayLengthSig): Boolean;


procedure InitDynArrayOfDWordToEmpty(var AArr: TDynArrayOfDWord); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfDWordLength(var AArr: TDynArrayOfDWord): TDynArrayLength;
function SetDynOfDWordLength(var AArr: TDynArrayOfDWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfDWord(var AArr: TDynArrayOfDWord);
function ConcatDynArraysOfDWord(var AArr1, AArr2: TDynArrayOfDWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDWordToDynArraysOfDWord(var AArr: TDynArrayOfDWord; ANewDWord: DWord): Boolean;
function RemoveStartDWordsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfDWord): Boolean;
function CopyFromDynArrayOfDWord(var ADestArr, ASrcArr: TDynArrayOfDWord; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
function IndexOfDWordInArrayOfDWord(var AArr: TDynArrayOfDWord; ADWordToFind: DWord): TDynArrayLengthSig; //returns -1 if not found
function DeleteItemFromDynArrayOfDWord(var AArr: TDynArrayOfDWord; ADelIndex: TDynArrayLength): Boolean;
function CreateUniqueDWordWithStart(var AArr: TDynArrayOfDWord; AStartAt: DWord): DWord;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
function CreateUniqueDWord(var AArr: TDynArrayOfDWord): DWord;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.


procedure InitDynArrayOfPtrUIntToEmpty(var AArr: TDynArrayOfPtrUInt); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt): TDynArrayLength;
function SetDynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
function ConcatDynArraysOfPtrUInt(var AArr1, AArr2: TDynArrayOfPtrUInt): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddPtrUIntToDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ANewPtrUInt: PtrUInt): Boolean;
function RemoveStartPtrUIntsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfPtrUInt): Boolean;
function CopyFromDynArrayOfPtrUInt(var ADestArr, ASrcArr: TDynArrayOfPtrUInt; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
function DeleteItemFromDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ADelIndex: TDynArrayLengthSig): Boolean;


procedure InitDynArrayOfPDynArrayOfTDynArrayOfByteToEmpty(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte); //do not call this on an array, which is already allocated, because it results in memory leaks
function DynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte): TDynArrayLength;
function SetDynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
procedure FreeDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
function ConcatDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr1, AArr2: TDynArrayOfPDynArrayOfTDynArrayOfByte): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
function AddDynArrayOfTDynArrayOfByteToDynArraysOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; var ANewDynArrayOfTDynArrayOfByte: TDynArrayOfTDynArrayOfByte): Boolean;
function DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ADelIndex: TDynArrayLengthSig): Boolean;


{$IFDEF IsDesktop}
  //This check is not available in mP, but is is useful as a debugging means on Desktop.
  procedure CheckInitializedDynArray(var AArr: TDynArrayOfByte);
  procedure CheckInitializedDynOfDynArray(var AArr: TDynArrayOfTDynArrayOfByte);
  procedure CheckInitializedDynArrayOfDWord(var AArr: TDynArrayOfDWord);
  procedure CheckInitializedDynArrayOfWord(var AArr: TDynArrayOfWord);
  procedure CheckInitializedDynOfDynArrayOfWord(var AArr: TDynArrayOfTDynArrayOfWord);
  procedure CheckInitializedDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt);
  procedure CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte);
{$ENDIF}

function StringToDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfByte): Boolean;   //assumes ADest is initialized
function StringToDynArrayOfWord({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfWord): Boolean;   //assumes ADest is initialized
function StringToDynArrayOfDWord({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfDWord): Boolean;   //assumes ADest is initialized
function StringToDynArrayOfPtrUInt({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfPtrUInt): Boolean;   //assumes ADest is initialized
procedure DynArrayOfByteToString(var AArr: TDynArrayOfByte; var ADestStr: string); {$IFDEF IsDesktop} overload; {$ENDIF}
procedure DynArrayOfWordToString(var AArr: TDynArrayOfWord; var ADestStr: string); {$IFDEF IsDesktop} overload; {$ENDIF}
procedure DynArrayOfDWordToString(var AArr: TDynArrayOfDWord; var ADestStr: string); {$IFDEF IsDesktop} overload; {$ENDIF}
procedure DynArrayOfPtrUIntToString(var AArr: TDynArrayOfPtrUInt; var ADestStr: string); {$IFDEF IsDesktop} overload; {$ENDIF}

function AddStringToDynOfDynArrayOfByte({$IFnDEF IsDesktop} var {$ENDIF} AStr: string; var ADest: TDynArrayOfTDynArrayOfByte): Boolean;
function AddStringToDynOfDynArrayOfWord({$IFnDEF IsDesktop} var {$ENDIF} AStr: string; var ADest: TDynArrayOfTDynArrayOfWord): Boolean;
function CopyBufferToDynArrayOfByte(ABuf: {$IFDEF IsDesktop} Pointer; {$ELSE} ^DWord; {$ENDIF} ALen: TDynArrayLength; var ADest: TDynArrayOfByte): Boolean; //overwrites ADest
function AddBufferToDynArrayOfByte(ABuf: {$IFDEF IsDesktop} Pointer; {$ELSE} ^DWord; {$ENDIF} ALen: TDynArrayLength; var ADest: TDynArrayOfByte): Boolean;  //concatenates with ADest

{$IFDEF IsDesktop}
  function DynArrayOfByteToString(var AArr: TDynArrayOfByte): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  function DynArrayOfWordToString(var AArr: TDynArrayOfWord): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  function DynArrayOfDWordToString(var AArr: TDynArrayOfDWord): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  function DynArrayOfPtrUIntToString(var AArr: TDynArrayOfPtrUInt): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  function DynOfDynArrayOfByteToString(var AArr: TDynArrayOfTDynArrayOfByte; ASeparator: string = #13#10): string;
  function ArrayOfByteToDynArrayOfByte(var AArray: array of Byte; var ADest: TDynArrayOfByte): Boolean;   //assumes ADest is initialized

  //for compatibility with mP
  procedure MemMove(ADest, ASrc: Pointer; ACount: LongInt);
{$ELSE}
  {$IFDEF RedefineMemMove}
    procedure MemMove32(ADest, ASrc: PByte; ACount: LongInt);
  {$ENDIF}
{$ENDIF}
  function Min32(a, b: LongInt): LongInt;


{$IFDEF IsDesktop}
  {$IFDEF UsingDynTFT}
    {$IFDEF LogMem}
      type
        TOnAfterGetMem = procedure(AAllocatedAddress, ARequestedSize: DWord);
        TOnAfterFreeMem = procedure(AAllocatedAddress, ARequestedSize: DWord);

      var
        OnAfterGetMem: TOnAfterGetMem;
        OnAfterFreeMem: TOnAfterFreeMem;

      procedure DoOnAfterGetMem(AAllocatedAddress, ARequestedSize: DWord);
      procedure DoOnAfterFreeMem(AAllocatedAddress, ARequestedSize: DWord);
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


implementation


{$IFDEF IsDesktop}
  uses
    SysUtils
    {$IFnDEF FPC}, Windows {$ENDIF}
    ;

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

  procedure CheckInitializedDynArrayOfWord(var AArr: TDynArrayOfWord);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynArrayOfWordToEmpty before working with DynArray functions.');
  end;

  procedure CheckInitializedDynOfDynArrayOfWord(var AArr: TDynArrayOfTDynArrayOfWord);
  begin
    if AArr.Initialized = '' then
      raise Exception.Create('The DynArray is not initialized. Please call InitDynOfDynOfWordToEmpty before working with DynArray functions.');
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


{$IFDEF IsDesktop}
  {$IFDEF UsingDynTFT}
    var
      MMCritSec: TRTLCriticalSection;

    procedure GetMem(var P: TPtrRec; Size: DWord);
    {$IFDEF RoundAlloc}
      var
        Remainder: Byte;
    {$ENDIF}
    begin
      EnterCriticalSection(MMCritSec);
      try
        {$IFDEF RoundAlloc} //this will save some FreeMem blocks in MemManager, by allocating multiple by pointer size.
          Remainder := {$IFDEF CPU64} (8 - Byte(Size) and 7) and 7; {$ELSE} (4 - Byte(Size) and 3) and 3; {$ENDIF}
          Size := Size + Remainder;
        {$ENDIF}

        MemManager.GetMem(P, Size);

        {$IFDEF LogMem}
          DoOnAfterGetMem(P, Size);
        {$ENDIF}
      finally
        LeaveCriticalSection(MMCritSec);
      end;
    end;


    procedure FreeMem(var P: TPtrRec; Size: DWord);
    {$IFDEF LogMem}
      var
        TempPointer: TPtrRec;
    {$ENDIF}
    {$IFDEF RoundAlloc}
      var
        Remainder: Byte;
    {$ENDIF}
    begin
      if P = 0 then
        Exit;

      EnterCriticalSection(MMCritSec);
      try
        {$IFDEF LogMem}
          TempPointer := P;
        {$ENDIF}

        {$IFDEF RoundAlloc} //this will save some FreeMem blocks in MemManager, by allocating multiple by pointer size.
          Remainder := {$IFDEF CPU64} (8 - Size and 7) and 7; {$ELSE} (4 - Size and 3) and 3; {$ENDIF}
          Size := Size + Remainder;
        {$ENDIF}

        MemManager.FreeMem(P, Size);

        {$IFDEF LogMem}
          DoOnAfterFreeMem(TempPointer, Size);
        {$ENDIF}
      finally
        LeaveCriticalSection(MMCritSec);
      end;
    end;
  {$ENDIF}
{$ELSE}
  {$IFDEF RoundAlloc}
    procedure GetMemRound(var P: {$IFDEF UsingDynTFT} TPtrRec; {$ELSE} ^Byte; {$ENDIF} Size: DWord);
    var
      Remainder: Byte;
    begin
      {$IFDEF AppArch32}
        Remainder := (4 - Byte(Size) and 3) and 3;
      {$ENDIF}
      {$IFDEF AppArch16}
        Remainder := (2 - Byte(Size) and 1) and 1;
      {$ENDIF}

      Size := Size + Remainder;
      GetMem(P, Size);
    end;

    procedure FreeMemRound(var P: {$IFDEF UsingDynTFT} TPtrRec; {$ELSE} ^Byte; {$ENDIF} Size: DWord);
    var
      Remainder: Byte;
    begin
      {$IFDEF AppArch32}
        Remainder := (4 - Byte(Size) and 3) and 3;
      {$ENDIF}
      {$IFDEF AppArch16}
        Remainder := (2 - Byte(Size) and 1) and 1;
      {$ENDIF}

      Size := Size + Remainder;
      FreeMem(P, Size);
    end;
  {$ENDIF}
{$ENDIF}


{$IFnDEF IsDesktop}
  function OrdBool(B: Boolean): Byte;
  begin
    Result := Byte(B) and 1;
  end;
{$ENDIF}

{$IFnDEF IsDesktop}
  type
    PPtrUInt = ^PtrUInt;
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
    MemMove(ADest.Content, @AString[1], TempLen);
  {$ELSE}
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(ADest.Content), DWord(@AString[0]), TempLen);
  {$ENDIF}
end;


function StringToDynArrayOfWord({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfWord): Boolean;   //assumes ADest is initialized
var
  TempLen: TDynArrayLength;
begin
  TempLen := TDynArrayLength(Length(AString));

  if (TempLen and 1) = 1 then
    Inc(TempLen);  //add one more item if odd

  TempLen := TempLen shr 1;

  Result := SetDynOfWordLength(ADest, TempLen);

  if not Result then
    Exit;

  {$IFDEF IsDesktop}
    MemMove(ADest.Content, @AString[1], TempLen shl 1);
  {$ELSE}
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(ADest.Content), DWord(@AString[0]), TempLen shl 1);
  {$ENDIF}
end;


function StringToDynArrayOfDWord({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfDWord): Boolean;   //assumes ADest is initialized
const
  CZeroLen = 1 shl 2;
var
  TempLen, ZeroPaddingSize: TDynArrayLength;
  IsPartial: Byte;
  i: Byte;
  ZeroString: string {$IFnDEF IsDesktop}[CZeroLen]{$ENDIF};
begin
  TempLen := TDynArrayLength(Length(AString));

  IsPartial := {$IFnDEF IsDesktop} OrdBool {$ELSE} Ord {$ENDIF}((TempLen and 3) <> 0);
  TempLen := (TempLen shr 2) + IsPartial;  //add one more item if odd or "misaligned"

  Result := SetDynOfDWordLength(ADest, TempLen);

  if not Result then
    Exit;

  {$IFDEF IsDesktop}
    SetLength(ZeroString, CZeroLen);
  {$ENDIF}

  {$IFDEF IsDesktop}
    for i := 1 to CZeroLen do
  {$ELSE}
    for i := 0 to CZeroLen - 1 do
  {$ENDIF}
      ZeroString[i] := #0;

  {$IFDEF IsDesktop}
    MemMove(ADest.Content, @AString[1], TempLen shl 2);
  {$ELSE}
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(ADest.Content), DWord(@AString[0]), TempLen shl 2);
  {$ENDIF}

  if IsPartial > 0 then
  begin
    ZeroPaddingSize := (TempLen shl 2) - Length(AString);
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(PPtrUInt(PtrUint(@ADest.Content^[TempLen - 1]) + DWord(4) - ZeroPaddingSize), @ZeroString[{$IFDEF IsDesktop}1{$ELSE}0{$ENDIF}], ZeroPaddingSize);
  end;
end;


function StringToDynArrayOfPtrUInt({$IFnDEF IsDesktop} var {$ENDIF} AString: string; var ADest: TDynArrayOfPtrUInt): Boolean;   //assumes ADest is initialized
const
  CZeroLen = 1 shl CArchBitShift;
var
  TempLen, ZeroPaddingSize: TDynArrayLength;
  IsPartial: Byte;
  i: Byte;
  ZeroString: string {$IFnDEF IsDesktop}[CZeroLen]{$ENDIF};
begin
  TempLen := TDynArrayLength(Length(AString));

  IsPartial := {$IFnDEF IsDesktop} OrdBool {$ELSE} Ord {$ENDIF}((TempLen and CArchBitMask) <> 0);
  TempLen := (TempLen shr CArchBitShift) + IsPartial;  //add one more item if odd or "misaligned"

  Result := SetDynOfPtrUIntLength(ADest, TempLen);

  if not Result then
    Exit;

  {$IFDEF IsDesktop}
    SetLength(ZeroString, CZeroLen);
  {$ENDIF}

  {$IFDEF IsDesktop}
    for i := 1 to CZeroLen do
  {$ELSE}
    for i := 0 to CZeroLen - 1 do
  {$ENDIF}
      ZeroString[i] := #0;

  {$IFDEF IsDesktop}
    MemMove(ADest.Content, @AString[1], TempLen shl CArchBitShift);
  {$ELSE}
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(ADest.Content), DWord(@AString[0]), TempLen shl CArchBitShift);
  {$ENDIF}

  if IsPartial > 0 then
  begin
    ZeroPaddingSize := (TempLen shl CArchBitShift) - Length(AString);
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(PPtrUInt(PtrUint(@ADest.Content^[TempLen - 1]) + DWord(CZeroLen) - ZeroPaddingSize), @ZeroString[{$IFDEF IsDesktop}1{$ELSE}0{$ENDIF}], ZeroPaddingSize);
  end;
end;


procedure DynArrayOfByteToString(var AArr: TDynArrayOfByte; var ADestStr: string);  {$IFDEF IsDesktop} overload; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(AArr);
    SetLength(ADestStr, AArr.Len);

    if AArr.Len = 0 then
      Exit;

    MemMove(@ADestStr[1], AArr.Content, AArr.Len);
  {$ELSE}
    if AArr.Len = 0 then
    begin
      ADestStr[0] := #0;
      Exit;
    end;

    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(@ADestStr[0]), DWord(AArr.Content), AArr.Len);
    ADestStr[AArr.Len] := #0;
  {$ENDIF}
end;


procedure DynArrayOfWordToString(var AArr: TDynArrayOfWord; var ADestStr: string);  {$IFDEF IsDesktop} overload; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
    SetLength(ADestStr, AArr.Len shl 1);

    if AArr.Len = 0 then
      Exit;

    MemMove(@ADestStr[1], AArr.Content, AArr.Len shl 1);
  {$ELSE}
    if AArr.Len = 0 then
    begin
      ADestStr[0] := #0;
      Exit;
    end;

    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(@ADestStr[0]), DWord(AArr.Content), AArr.Len shl 1);
    ADestStr[AArr.Len shl 1] := #0;
  {$ENDIF}
end;


procedure DynArrayOfDWordToString(var AArr: TDynArrayOfDWord; var ADestStr: string);  {$IFDEF IsDesktop} overload; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
    SetLength(ADestStr, AArr.Len shl 2);

    if AArr.Len = 0 then
      Exit;

    MemMove(@ADestStr[1], AArr.Content, AArr.Len shl 2);
  {$ELSE}
    if AArr.Len = 0 then
    begin
      ADestStr[0] := #0;
      Exit;
    end;

    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(@ADestStr[0]), DWord(AArr.Content), AArr.Len shl 2);
    ADestStr[AArr.Len shl 2] := #0;
  {$ENDIF}
end;


procedure DynArrayOfPtrUIntToString(var AArr: TDynArrayOfPtrUInt; var ADestStr: string);  {$IFDEF IsDesktop} overload; {$ENDIF}
{$IFDEF IsDesktop}
  var
    LastNonZeroIndex: LongInt;
{$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr);
    SetLength(ADestStr, AArr.Len shl CArchBitShift);

    if AArr.Len = 0 then
      Exit;

    MemMove(@ADestStr[1], AArr.Content, AArr.Len shl CArchBitShift);

    if Length(ADestStr) > 0 then  //delete trailing null characters
    begin
      LastNonZeroIndex := Length(ADestStr);
      while ADestStr[LastNonZeroIndex] = #0 do
        Dec(LastNonZeroIndex);

      if LastNonZeroIndex > 0 then
        ADestStr := Copy(ADestStr, 1, LastNonZeroIndex);
    end;
  {$ELSE}
    if AArr.Len = 0 then
    begin
      ADestStr[0] := #0;
      Exit;
    end;

    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(@ADestStr[0]), DWord(AArr.Content), AArr.Len shl CArchBitShift);
    ADestStr[AArr.Len shl CArchBitShift] := #0;
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


function AddStringToDynOfDynArrayOfWord({$IFnDEF IsDesktop} var {$ENDIF} AStr: string; var ADest: TDynArrayOfTDynArrayOfWord): Boolean;
var
  TempArr: TDynArrayOfWord;
begin
  InitDynArrayOfWordToEmpty(TempArr);
  Result := StringToDynArrayOfWord(AStr, TempArr);
  Result := Result and AddDynArrayOfWordToDynOfDynOfWord(ADest, TempArr);
  FreeDynArrayOfWord(TempArr);
end;


function CopyBufferToDynArrayOfByte(ABuf: {$IFDEF IsDesktop} Pointer; {$ELSE} ^DWord; {$ENDIF} ALen: TDynArrayLength; var ADest: TDynArrayOfByte): Boolean; //overwrites ADest
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArray(ADest);
  {$ENDIF}

  Result := SetDynLength(ADest, ALen);
  if Result and (ALen > 0) then
    {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(ADest.Content, ABuf, ALen);
end;


function AddBufferToDynArrayOfByte(ABuf: {$IFDEF IsDesktop} Pointer; {$ELSE} ^DWord; {$ENDIF} ALen: TDynArrayLength; var ADest: TDynArrayOfByte): Boolean;  //concatenates with ADest
var
  OldLen: TDynArrayLength;
begin
  if ALen = 0 then
  begin
    Result := True;
    Exit;
  end;

  {$IFDEF IsDesktop}
    CheckInitializedDynArray(ADest);
  {$ENDIF}

  OldLen := ADest.Len;
  Result := SetDynLength(ADest, OldLen + ALen);
  if Result then
    {$IFDEF IsDesktop}
      MemMove(PPtrUInt(PtrUInt(@ADest.Content^[OldLen])), ABuf, ALen);
    {$ELSE}
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(PtrUInt(@ADest.Content^[OldLen])), ABuf, ALen);
    {$ENDIF}
end;


{$IFDEF IsDesktop}
  function DynArrayOfByteToString(var AArr: TDynArrayOfByte): string;  {$IFDEF IsDesktop} overload; {$ENDIF}
  begin
    Result := 'no string content';
    DynArrayOfByteToString(AArr, Result);
  end;


  function DynArrayOfWordToString(var AArr: TDynArrayOfWord): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  begin
    Result := 'no string content';
    DynArrayOfWordToString(AArr, Result);
  end;


  function DynArrayOfDWordToString(var AArr: TDynArrayOfDWord): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  begin
    Result := 'no string content';
    DynArrayOfDWordToString(AArr, Result);
  end;


  function DynArrayOfPtrUIntToString(var AArr: TDynArrayOfPtrUInt): string; {$IFDEF IsDesktop} overload; {$ENDIF}
  begin
    Result := 'no string content';
    DynArrayOfPtrUIntToString(AArr, Result);
  end;


  function DynOfDynArrayOfByteToString(var AArr: TDynArrayOfTDynArrayOfByte; ASeparator: string = #13#10): string;
  var
    i: Integer;
    TempStr: string;
  begin
    Result := '';
    for i := 0 to TDynArrayLengthSig(AArr.Len) - 1 do
    begin
      TempStr := 'some init value';
      DynArrayOfByteToString(AArr.Content^[i]^, TempStr);
      Result := Result + TempStr + ASeparator;
    end;
  end;
  
  
  function ArrayOfByteToDynArrayOfByte(var AArray: array of Byte; var ADest: TDynArrayOfByte): Boolean;   //assumes ADest is initialized
  var
    TempLen: TDynArrayLength;
  begin
    TempLen := TDynArrayLength(Length(AArray));
    Result := SetDynLength(ADest, TempLen);

    if not Result then
      Exit;

    {$IFDEF IsDesktop}
      MemMove(ADest.Content, @AArray[1], TempLen);
    {$ELSE}
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DWord(ADest.Content), DWord(@AArray[0]), TempLen);
    {$ENDIF}
  end;


  //for compatibility with mP
  procedure MemMove(ADest, ASrc: Pointer; ACount: LongInt);
  begin
    {$IFDEF UsingDynTFT}
      EnterCriticalSection(MMCritSec);
      try
    {$ENDIF}
        Move(ASrc^, ADest^, ACount); //(src, dst, cnt);
    {$IFDEF UsingDynTFT}
      finally
        LeaveCriticalSection(MMCritSec);
      end;
    {$ENDIF}
  end;
{$ELSE}
  {$IFDEF RedefineMemMove}
    procedure MemMove32(ADest, ASrc: PByte; ACount: LongInt);
    type
      TZeroArr = array[0..0] of Byte;
      PZeroArr = ^TZeroArr;
    var
      i, ACountM1: LongInt;
    begin
      if ADest = ASrc then
        Exit;

      ACountM1 := ACount - 1;
      if ADest > ASrc then   //this ensures that the item is being read before being overwritten
      begin
        for i := ACountM1 downto 0 do
          {$IFDEF AppArch16}
            PZeroArr(DWord(ADest))^[i] := PZeroArr(DWord(ASrc))^[i];
          {$ELSE}
            PZeroArr(ADest)^[i] := PZeroArr(ASrc)^[i];
          {$ENDIF}
      end
      else
      begin
        for i := 0 to ACountM1 do
          {$IFDEF AppArch16}
            PZeroArr(DWord(ADest))^[i] := PZeroArr(DWord(ASrc))^[i];
          {$ELSE}
            PZeroArr(ADest)^[i] := PZeroArr(ASrc)^[i];
          {$ENDIF}
      end;
    end;
  {$ENDIF}
{$ENDIF}

function Min32(a, b: LongInt): LongInt;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

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
      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len);
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len);
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len);
        {$ENDIF}
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  if AArr.Len = 0 then
    OldPointer := nil
  else
    OldPointer := PIntPtr(AArr.Content);

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength);
    if MM_error or (AArr.Content = nil) then    //If compilers report that "MM_error" was not declared, then open MemManager library and uncomment MM_error header.
    begin
      Result := False;
      Exit;
    end;

    if OldPointer <> nil then
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len));   //OldPointer = src, AArr.Content = dest
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

      if OldPointer <> nil then
        MemMove(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len))   // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len);
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len);
      {$ELSE}
        FreeMem(OldPointer, AArr.Len);
      {$ENDIF}
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

  OldArr1Len := AArr1.Len;
  Result := SetDynLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len;
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len));  //NewPointer := @AArr1.Content^[OldArr1Len];
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(NewPointer, AArr2.Content, AArr2.Len);

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
  PartialArr: TDynArrayOfByte;
  NewLen: TDynArrayLength;
  {$IFDEF AppArch16} Dummy: TDynArrayLength; {$ENDIF}
begin
  Result := True;

  if ACount >= AArr.Len then
  begin
    FreeDynArray(AArr);
    Exit;
  end;

  if ACount = 0 then
    Exit;

  InitDynArrayToEmpty(PartialArr);
  NewLen := AArr.Len - ACount;
  
  if not CopyFromDynArray(PartialArr, AArr, {$IFDEF AppArch16} Dummy, {$ENDIF} ACount, NewLen) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, PartialArr.Content, NewLen);
  FreeDynArray(PartialArr);

  Result := SetDynLength(AArr, NewLen);
end;


function CopyFromDynArray(var ADestArr, ASrcArr: TDynArrayOfByte; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  Result := True;
  InitDynArrayToEmpty(ADestArr);

  if ACount = 0 then
    Exit;

  if ASrcArr.Len = 0 then
  begin
    Result := False;
    Exit;
  end;

  if AIndex > ASrcArr.Len - 1 then
  begin
    Result := False;
    Exit;
  end;

  if ACount > ASrcArr.Len - AIndex then
    ACount := ASrcArr.Len - AIndex;

  if not SetDynLength(ADestArr, ACount) then
  begin
    Result := False;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(ASrcArr.Content) + AIndex;
  {$ELSE}
    OldPointer := Pointer(PtrUInt(ASrcArr.Content) + PtrUInt(AIndex));
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(ADestArr.Content, OldPointer, ACount);
end;


function DeleteItemFromDynArray(var AArr: TDynArrayOfByte; ADelIndex: TDynArrayLength): Boolean;
var
  i, Dest: TDynArrayLengthSig;
begin
  Result := True;

  if AArr.Len = 0 then
  begin
    Result := False;
    Exit;
  end;

  if ADelIndex > AArr.Len - 1 then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Delete index out of bounds in DeleteItemFromDynArray.');
    {$ENDIF}

    Result := False;
    Exit;
  end;

  Dest := AArr.Len - 2;
  for i := ADelIndex to Dest do
    AArr.Content^[i] := AArr.Content^[i + 1];

  Result := SetDynLength(AArr, AArr.Len - 1);
end;


// array of array of Byte

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
  i, MaxCopyIdx: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      for i := 0 to TDynArrayLengthSig(AArr.Len) - 1 do
      begin
        FreeDynArray(AArr.Content^[i]^);

        {$IFnDEF IsDesktop}
          {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfByte));
        {$ELSE}
          Dispose(AArr.Content^[i]);
        {$ENDIF}
      end;

      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len * SizeOf(PDynArrayOfByte));
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len * SizeOf(PDynArrayOfByte));
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len * SizeOf(PDynArrayOfByte));
        {$ENDIF}
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

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength * SizeOf(PDynArrayOfByte));    //SizeOf pointer
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    for i := 0 to ANewLength - 1 do
    begin
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfByte));           //SizeOf struct
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

  MaxCopyIdx := TDynArrayLengthSig(Min32(AArr.Len, ANewLength)) - 1;
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
    for i := 0 to TDynArrayLengthSig(AArr.Len) - 1 do
    begin
      FreeDynArray(OldPointer^[i]^);    /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.

      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer^[i], SizeOf(TDynArrayOfByte));
      {$ELSE}
        Dispose(OldPointer^[i]);
      {$ENDIF}
    end;


    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfByte));
      {$ELSE}
        FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
      {$ENDIF}
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


function DeleteItemFromDynOfDynOfByte(var AArr: TDynArrayOfTDynArrayOfByte; ADelIndex: TDynArrayLengthSig): Boolean;
var
  i: TDynArrayLengthSig;
  OldPointer: PDynArrayOfTDynArrayOfByteContent;
  NewLen: DWord;
begin
  if (ADelIndex < 0) or (ADelIndex > TDynArrayLengthSig(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynOfDynArrayOfByte.');
    {$ELSE}
      Result := False;
      Exit;
    {$ENDIF}
  end;

  if AArr.Len = 0 then  //verify again, in case the above typecasting doesn't work
  begin
    Result := True;
    Exit;
  end;

  for i := ADelIndex to AArr.Len - 2 do
  begin
    FreeDynArray(AArr.Content^[i]^);

    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfByte));
    {$ELSE}
      Dispose(AArr.Content^[i]);
    {$ENDIF}

    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfByte));           //SizeOf struct
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
    {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[AArr.Len - 1], SizeOf(TDynArrayOfByte));
  {$ELSE}
    Dispose(AArr.Content^[AArr.Len - 1]);
  {$ENDIF}

  NewLen := AArr.Len - 1;
  OldPointer := AArr.Content;

  if NewLen > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, NewLen * SizeOf(PDynArrayOfByte));    //SizeOf pointer
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

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, NewLen * SizeOf(PDynArrayOfByte));

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
  {$ELSE}
    {$IFDEF UsingDynTFT}
      FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfByte));
    {$ELSE}
      FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfByte));
    {$ENDIF}
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
  i: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArray(AArr1);
    CheckInitializedDynOfDynArray(AArr2);
  {$ENDIF}

  Result := True;

  if AArr2.Len = 0 then
    Exit;

  for i := 0 to TDynArrayLengthSig(AArr2.Len) - 1 do
  begin
    Result := Result and AddDynArrayOfByteToDynOfDynOfByte(AArr1, AArr2.Content^[i]^);
    if not Result then
      Exit;
  end;
end;


//---------------------------------------
//array of Word

procedure InitDynArrayOfWordToEmpty(var AArr: TDynArrayOfWord); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfWordLength(var AArr: TDynArrayOfWord): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfWordLength(var AArr: TDynArrayOfWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len shl 1);
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len shl 1);
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len shl 1);
        {$ENDIF}
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  if AArr.Len = 0 then
    OldPointer := nil
  else
    OldPointer := PIntPtr(AArr.Content);

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength shl 1);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    if OldPointer <> nil then
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl 1);  //OldPointer = src, AArr.Content = dest
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength shl 1);
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength shl 1);
      {$ENDIF}

      if OldPointer <> nil then
      //AArr.Len is still the old array length. Only the Content field points somewhere else.
        MemMove(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl 1);    // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len shl 1);
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl 1);
      {$ELSE}
        FreeMem(OldPointer, AArr.Len shl 1);
      {$ENDIF}
    {$ENDIF}
  end;

  AArr.Len := ANewLength;
end;


procedure FreeDynArrayOfWord(var AArr: TDynArrayOfWord);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
  {$ENDIF}

  SetDynOfWordLength(AArr, 0);
end;


function ConcatDynArraysOfWord(var AArr1, AArr2: TDynArrayOfWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  NewLen: DWord;
  NewPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
  OldArr1Len: DWord;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr1);
    CheckInitializedDynArrayOfWord(AArr2);
  {$ENDIF}

  if AArr2.Len = 0 then
  begin
    Result := True;
    Exit;
  end;

  NewLen := DWord(AArr1.Len) + DWord(AArr2.Len);

  OldArr1Len := AArr1.Len;
  Result := SetDynOfWordLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len shl 1;
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len shl 1));  //NewPointer := @AArr1.Content^[OldArr1Len shl 1];
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(NewPointer, AArr2.Content, AArr2.Len shl 1);

  Result := True;
end;


function AddWordToDynArraysOfWord(var AArr: TDynArrayOfWord; ANewWord: Word): Boolean;
begin
  Result := SetDynOfWordLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := ANewWord;
end;


function RemoveStartWordsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfWord): Boolean;
var
  PartialArr: TDynArrayOfWord;
  NewLen: TDynArrayLength;
  {$IFDEF AppArch16} Dummy: TDynArrayLength; {$ENDIF}
begin
  Result := True;

  if ACount >= AArr.Len then
  begin
    FreeDynArrayOfWord(AArr);
    Exit;
  end;

  if ACount = 0 then
    Exit;

  InitDynArrayOfWordToEmpty(PartialArr);
  NewLen := AArr.Len - ACount;
  if not CopyFromDynArrayOfWord(PartialArr, AArr, {$IFDEF AppArch16} Dummy, {$ENDIF} ACount, NewLen) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, PartialArr.Content, NewLen shl 1);
  FreeDynArrayOfWord(PartialArr);

  Result := SetDynOfWordLength(AArr, NewLen);
end;


function CopyFromDynArrayOfWord(var ADestArr, ASrcArr: TDynArrayOfWord; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  Result := True;
  InitDynArrayOfWordToEmpty(ADestArr);

  if ACount = 0 then
    Exit;

  if AIndex > ASrcArr.Len then
  begin
    Result := False;
    Exit;
  end;

  if ACount > ASrcArr.Len - AIndex then
    ACount := ASrcArr.Len - AIndex;

  if not SetDynOfWordLength(ADestArr, ACount) then
  begin
    Result := False;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(ASrcArr.Content) + AIndex shl 1;
  {$ELSE}
    OldPointer := Pointer(PtrUInt(ASrcArr.Content) + PtrUInt(AIndex shl 1));
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(ADestArr.Content, OldPointer, ACount shl 1);
end;


function IndexOfWordInArrayOfWord(var AArr: TDynArrayOfWord; AWordToFind: Word): TDynArrayLengthSig; //returns -1 if not found
var
  i, Dest: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
  {$ENDIF}

  Result := -1;
  Dest := AArr.Len - 1;
  for i := 0 to Dest do
    if AArr.Content^[i] = AWordToFind then
    begin
      Result := i;
      Break;
    end;
end;


function DeleteItemFromDynArrayOfWord(var AArr: TDynArrayOfWord; ADelIndex: TDynArrayLength): Boolean;
var
  i, Dest: TDynArrayLengthSig;
begin
  Result := True;

  if AArr.Len = 0 then
  begin
    Result := False;
    Exit;
  end;

  if ADelIndex > AArr.Len - 1 then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Delete index out of bounds in DeleteItemFromDynArrayOfWord.');
    {$ENDIF}

    Result := False;
    Exit;
  end;

  Dest := AArr.Len - 2;
  for i := ADelIndex to Dest do
    AArr.Content^[i] := AArr.Content^[i + 1];

  Result := SetDynOfWordLength(AArr, AArr.Len - 1);
end;


function CreateUniqueWordWithStart(var AArr: TDynArrayOfWord; AStartAt: Word): Word;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
var
  TempNumber: TDynArrayLength;  //using a DWord, instead of a Word, because the array length might already be greater than 65535.
  //TempNumber: Word;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfWord(AArr);
  {$ENDIF}

  if AStartAt = $FFFF then
    TempNumber := AArr.Len   //Start with AArr.Len
  else
    TempNumber := AStartAt;

  if (AArr.Len >= 65535) or (TempNumber = $FFFF) then
  begin
    Result := $FFFF;
    Exit;
  end;

  Result := 0; //init here. When it becomes $FFFF (err) and TempNumber is also $FFFF, then exit.
  repeat
    if IndexOfWordInArrayOfWord(AArr, TempNumber) = -1 then //Found
    begin
      if not AddWordToDynArraysOfWord(AArr, TempNumber) then
      begin
        Result := $FFFF; //Error: Out of memory.
        Exit;
      end;

      Result := TempNumber;
      Exit;
    end;

    Inc(TempNumber);
    if TempNumber = $FFFF then
    begin
      if Result = $FFFF then
        Exit;

      Inc(TempNumber);  //Jump past $FFFF, so it should become 0, to verify the other part of the array.
      if TempNumber > $FFFF then
        TempNumber := 0;

      Result := $FFFF;  //Mark as wrapped around.
    end
    else
      if Result = $FFFF then //already wrapped around
        if TempNumber = AArr.Len then //back to first attempted value
          Exit;
  until False;
end;


function CreateUniqueWord(var AArr: TDynArrayOfWord): Word;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
begin
  Result := CreateUniqueWordWithStart(AArr, $FFFF);
end;


//-------------------------
// array of array of Word

procedure InitDynOfDynOfWordToEmpty(var AArr: TDynArrayOfTDynArrayOfWord); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfDynOfWordLength(var AArr: TDynArrayOfTDynArrayOfWord): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArrayOfWord(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfDynOfWordLength(var AArr: TDynArrayOfTDynArrayOfWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldPointer: PDynArrayOfTDynArrayOfWordContent;
  i, MaxCopyIdx: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArrayOfWord(AArr);
  {$ENDIF}

  Result := True;

  if ANewLength = 0 then
  begin
    if AArr.Len > 0 then
    begin
      for i := 0 to TDynArrayLengthSig(AArr.Len) - 1 do
      begin
        FreeDynArrayOfWord(AArr.Content^[i]^);

        {$IFnDEF IsDesktop}
          {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfWord));
        {$ELSE}
          Dispose(AArr.Content^[i]);
        {$ENDIF}
      end;

      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len* SizeOf(PDynArrayOfWord));
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len* SizeOf(PDynArrayOfWord));
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len* SizeOf(PDynArrayOfWord));
        {$ENDIF}
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(AArr.Content);
  {$ELSE}
    OldPointer := PDynArrayOfTDynArrayOfWordContent(AArr.Content);
  {$ENDIF}

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength * SizeOf(PDynArrayOfWord));    //SizeOf pointer
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    for i := 0 to ANewLength - 1 do
    begin
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfWord));           //SizeOf struct
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    end;
  {$ELSE}
    try
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), ANewLength * SizeOf(PDynArrayOfWord)); //SizeOf pointer
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, ANewLength * SizeOf(PDynArrayOfWord)); //SizeOf pointer
      {$ENDIF}
                                              /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.
      for i := 0 to ANewLength - 1 do
        New(AArr.Content^[i]);           //SizeOf struct    //Using New instead of GetMem, to properly initialize the "Initialized" field (string). GetMem doesn't do any form of initialization to the allocated structure.
    except
      Result := False;
    end;
  {$ENDIF}

  MaxCopyIdx := TDynArrayLengthSig(Min32(AArr.Len, ANewLength)) - 1;
  for i := 0 to MaxCopyIdx do
  begin
    InitDynArrayOfWordToEmpty(AArr.Content^[i]^);
    if not ConcatDynArraysOfWord(AArr.Content^[i]^, OldPointer^[i]^) then   /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.
    begin
      Result := False;
      Exit;
    end;
  end;

  if AArr.Len > 0 then
  begin
    for i := 0 to TDynArrayLengthSig(AArr.Len) - 1 do
    begin
      FreeDynArrayOfWord(OldPointer^[i]^);    /////////////////////// as an optimization, the inner arrays do not have to be freed and reallocated. Only the pointers to these arrays have to be updated.

      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer^[i], SizeOf(TDynArrayOfWord));
      {$ELSE}
        Dispose(OldPointer^[i]);
      {$ENDIF}
    end;

    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len * SizeOf(PDynArrayOfWord));
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfWord));
      {$ELSE}
        FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfWord));
      {$ENDIF}
    {$ENDIF}
  end;

  if ANewLength > AArr.Len then
    for i := AArr.Len to ANewLength - 1 do
      InitDynArrayOfWordToEmpty(AArr.Content^[i]^);

  AArr.Len := ANewLength;
end;


function AddDynArrayOfWordToDynOfDynOfWord(var AArr: TDynArrayOfTDynArrayOfWord; var ANewArr: TDynArrayOfWord): Boolean;
begin
  Result := SetDynOfDynOfWordLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  Result := ConcatDynArraysOfWord(AArr.Content^[AArr.Len - 1]^, ANewArr);
end;


function DeleteItemFromDynOfDynOfWord(var AArr: TDynArrayOfTDynArrayOfWord; ADelIndex: TDynArrayLengthSig): Boolean;
var
  i: Integer;
  OldPointer: PDynArrayOfTDynArrayOfWordContent;
  NewLen: DWord;
begin
  if (ADelIndex < 0) or (ADelIndex > TDynArrayLengthSig(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynOfDynArrayOfWord.');
    {$ELSE}
      Result := False;
      Exit;
    {$ENDIF}
  end;

  for i := ADelIndex to AArr.Len - 2 do
  begin
    FreeDynArrayOfWord(AArr.Content^[i]^);

    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfWord));
    {$ELSE}
      Dispose(AArr.Content^[i]);
    {$ENDIF}

    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfWord));           //SizeOf struct
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    {$ELSE}
      New(AArr.Content^[i]);
    {$ENDIF}

    InitDynArrayOfWordToEmpty(AArr.Content^[i]^);
    Result := ConcatDynArraysOfWord(AArr.Content^[i]^, AArr.Content^[i + 1]^);
    if not Result then
      Exit;
  end;

  FreeDynArrayOfWord(AArr.Content^[AArr.Len - 1]^);

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[AArr.Len - 1], SizeOf(TDynArrayOfWord));
  {$ELSE}
    Dispose(AArr.Content^[AArr.Len - 1]);
  {$ENDIF}

  NewLen := AArr.Len - 1;
  OldPointer := AArr.Content;

  if NewLen > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, NewLen * SizeOf(PDynArrayOfWord));    //SizeOf pointer
      if MM_error or (AArr.Content = nil) then
      begin
        Result := False;
        Exit;
      end;
    {$ELSE}
      {$IFDEF UsingDynTFT}
        GetMem(TPtrRec(AArr.Content), NewLen * SizeOf(PDynArrayOfWord)); //SizeOf pointer
        if MM_error or (AArr.Content = nil) then
        begin
          Result := False;
          Exit;
        end;
      {$ELSE}
        GetMem(AArr.Content, NewLen * SizeOf(PDynArrayOfWord)); //SizeOf pointer
      {$ENDIF}
    {$ENDIF}
  end;

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, NewLen * SizeOf(PDynArrayOfWord));

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len * SizeOf(PDynArrayOfWord));
  {$ELSE}
    {$IFDEF UsingDynTFT}
      FreeMem(TPtrRec(OldPointer), AArr.Len * SizeOf(PDynArrayOfWord));
    {$ELSE}
      FreeMem(OldPointer, AArr.Len * SizeOf(PDynArrayOfWord));
    {$ENDIF}
  {$ENDIF}

  AArr.Len := AArr.Len - 1;
  Result := True;
end;


procedure FreeDynOfDynOfWordArray(var AArr: TDynArrayOfTDynArrayOfWord);
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArrayOfWord(AArr);
  {$ENDIF}

  SetDynOfDynOfWordLength(AArr, 0);
end;


function ConcatDynOfDynOfWordArrays(var AArr1, AArr2: TDynArrayOfTDynArrayOfWord): Boolean; //Concats AArr1 with AArr2. Places new array in AArr1.
var
  i: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynOfDynArrayOfWord(AArr1);
    CheckInitializedDynOfDynArrayOfWord(AArr2);
  {$ENDIF}

  Result := True;
  for i := 0 to TDynArrayLengthSig(AArr2.Len) - 1 do
  begin
    Result := Result and AddDynArrayOfWordToDynOfDynOfWord(AArr1, AArr2.Content^[i]^);
    if not Result then
      Exit;
  end;
end;


//----------------------------
//array of DWord

procedure InitDynArrayOfDWordToEmpty(var AArr: TDynArrayOfDWord); //do not call this on an array, which is already allocated, because it results in memory leaks
begin
  AArr.Len := 0;       //this is required when allocating a new array
  AArr.Content := nil; //probably, not needed, since Len is set to 0

  {$IFDEF IsDesktop}
    AArr.Initialized := 'init';  //some string, different than ''
  {$ENDIF}
end;


function DynOfDWordLength(var AArr: TDynArrayOfDWord): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfDWordLength(var AArr: TDynArrayOfDWord; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
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
      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len shl 2);
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len shl 2);
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len shl 2);
        {$ENDIF}
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  if AArr.Len = 0 then
    OldPointer := nil
  else
    OldPointer := PIntPtr(AArr.Content);

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength shl 2);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    if OldPointer <> nil then
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl 2);  //OldPointer = src, AArr.Content = dest
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

      if OldPointer <> nil then
      //AArr.Len is still the old array length. Only the Content field points somewhere else.
        MemMove(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl 2);    // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len shl 2);
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl 2);
      {$ELSE}
        FreeMem(OldPointer, AArr.Len shl 2);
      {$ENDIF}
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

  OldArr1Len := AArr1.Len;
  Result := SetDynOfDWordLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len shl 2;
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len shl 2));  //NewPointer := @AArr1.Content^[OldArr1Len shl 2];
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(NewPointer, AArr2.Content, AArr2.Len shl 2);

  Result := True;
end;


function AddDWordToDynArraysOfDWord(var AArr: TDynArrayOfDWord; ANewDWord: DWord): Boolean;
begin
  Result := SetDynOfDWordLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := ANewDWord;
end;


function RemoveStartDWordsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfDWord): Boolean;
var
  PartialArr: TDynArrayOfDWord;
  NewLen: TDynArrayLength;
  {$IFDEF AppArch16} Dummy: TDynArrayLength; {$ENDIF}
begin
  Result := True;

  if ACount >= AArr.Len then
  begin
    FreeDynArrayOfDWord(AArr);
    Exit;
  end;

  if ACount = 0 then
    Exit;

  InitDynArrayOfDWordToEmpty(PartialArr);
  NewLen := AArr.Len - ACount;
  if not CopyFromDynArrayOfDWord(PartialArr, AArr, {$IFDEF AppArch16} Dummy, {$ENDIF} ACount, NewLen) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, PartialArr.Content, NewLen shl 2);
  FreeDynArrayOfDWord(PartialArr);

  Result := SetDynOfDWordLength(AArr, NewLen);
end;


function CopyFromDynArrayOfDWord(var ADestArr, ASrcArr: TDynArrayOfDWord; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  Result := True;
  InitDynArrayOfDWordToEmpty(ADestArr);

  if ACount = 0 then
    Exit;

  if AIndex > ASrcArr.Len then
  begin
    Result := False;
    Exit;
  end;

  if ACount > ASrcArr.Len - AIndex then
    ACount := ASrcArr.Len - AIndex;

  if not SetDynOfDWordLength(ADestArr, ACount) then
  begin
    Result := False;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(ASrcArr.Content) + AIndex shl 2;
  {$ELSE}
    OldPointer := Pointer(PtrUInt(ASrcArr.Content) + PtrUInt(AIndex shl 2));
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(ADestArr.Content, OldPointer, ACount shl 2);
end;


function IndexOfDWordInArrayOfDWord(var AArr: TDynArrayOfDWord; ADWordToFind: DWord): TDynArrayLengthSig; //returns -1 if not found
var
  i, Dest: TDynArrayLengthSig;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  Result := -1;
  Dest := AArr.Len - 1;
  for i := 0 to Dest do
    if AArr.Content^[i] = ADWordToFind then
    begin
      Result := i;
      Break;
    end;
end;


function DeleteItemFromDynArrayOfDWord(var AArr: TDynArrayOfDWord; ADelIndex: TDynArrayLength): Boolean;
var
  i, Dest: TDynArrayLengthSig;
begin
  Result := True;

  if AArr.Len = 0 then
  begin
    Result := False;
    Exit;
  end;

  if ADelIndex > AArr.Len - 1 then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Delete index out of bounds in DeleteItemFromDynArrayOfDWord.');
    {$ENDIF}

    Result := False;
    Exit;
  end;

  Dest := AArr.Len - 2;
  for i := ADelIndex to Dest do
    AArr.Content^[i] := AArr.Content^[i + 1];

  Result := SetDynOfDWordLength(AArr, AArr.Len - 1);
end;


//function CreateUniqueDWord(var AArr: TDynArrayOfDWord): DWord;  //Returns $FFFFFFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFFFFFF is reserved as an error message.
//var           //TempNumber is used below as DWord (for controlled overflow), but for length comparison, it should be QWord (or Int64)
//  TempNumber: TDynArrayLength;  //using a DWord, instead of a Word, because the array length might already be greater than $FFFFFFFF.
//  //TempNumber: Word;
//begin
//  {$IFDEF IsDesktop}
//    CheckInitializedDynArrayOfDWord(AArr);
//  {$ENDIF}
//
//  TempNumber := AArr.Len;   //Start with AArr.Len
//  if (AArr.Len >= $FFFFFFFF) or (TempNumber = $FFFFFFFF) then
//  begin
//    Result := $FFFFFFFF;
//    Exit;
//  end;
//
//  Result := 0; //init here. When it becomes $FFFFFFFF (err) and TempNumber is also $FFFFFFFF, then exit.
//  repeat
//    if IndexOfDWordInArrayOfDWord(AArr, TempNumber) = -1 then //Found
//    begin
//      if not AddDWordToDynArraysOfDWord(AArr, TempNumber) then
//      begin
//        Result := $FFFFFFFF; //Error: Out of memory.
//        Exit;
//      end;
//
//      Result := TempNumber;
//      Exit;
//    end;
//
//    Inc(TempNumber);
//    if TempNumber = $FFFFFFFF then
//    begin
//      if Result = $FFFFFFFF then
//        Exit;
//
//      Inc(TempNumber);  //Jump past $FFFFFFFF, so it should become 0, to verify the other part of the array.
//      Result := $FFFFFFFF;  //Mark as wrapped around.
//    end
//    else
//      if Result = $FFFFFFFF then //already wrapped around
//        if TempNumber = AArr.Len then //back to first attempted value
//          Exit;
//  until False;
//end;

function CreateUniqueDWordWithStart(var AArr: TDynArrayOfDWord; AStartAt: DWord): DWord;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
var
  TempNumber: TDynArrayLength;  //using a DWord, instead of a Word, because the array length might already be greater than 65535.
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfDWord(AArr);
  {$ENDIF}

  if AStartAt = $FFFF then
    TempNumber := AArr.Len   //Start with AArr.Len
  else
    TempNumber := AStartAt;

  if (AArr.Len >= 65535) or (TempNumber = $FFFF) then
  begin
    Result := $FFFF;
    Exit;
  end;

  Result := 0; //init here. When it becomes $FFFF (err) and TempNumber is also $FFFF, then exit.
  repeat
    if IndexOfDWordInArrayOfDWord(AArr, TempNumber) = -1 then //Found
    begin
      if not AddDWordToDynArraysOfDWord(AArr, TempNumber) then
      begin
        Result := $FFFF; //Error: Out of memory.
        Exit;
      end;

      Result := TempNumber;
      Exit;
    end;

    Inc(TempNumber);
    if TempNumber = $FFFF then
    begin
      if Result = $FFFF then
        Exit;

      Inc(TempNumber);  //Jump past $FFFF, so it should become 0, to verify the other part of the array.
      if TempNumber > $FFFF then
        TempNumber := 0;

      Result := $FFFF;  //Mark as wrapped around.
    end
    else
      if Result = $FFFF then //already wrapped around
        if TempNumber = AArr.Len then //back to first attempted value
          Exit;
  until False;
end;


function CreateUniqueDWord(var AArr: TDynArrayOfDWord): DWord;  //Returns $FFFF if can't find a new number to add or the array is already full (with or without duplicates). This means $FFFF is reserved as an error message.
begin
  Result := CreateUniqueDWordWithStart(AArr, $FFFF);
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


function DynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPtrUInt(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfPtrUIntLength(var AArr: TDynArrayOfPtrUInt; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
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
      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content, AArr.Len shl CArchBitShift);
      {$ELSE}
        {$IFDEF UsingDynTFT}
          FreeMem(TPtrRec(AArr.Content), AArr.Len shl CArchBitShift);
        {$ELSE}
          FreeMem(AArr.Content, AArr.Len shl CArchBitShift);
        {$ENDIF}
      {$ENDIF}
    end;

    AArr.Len := 0;
    Exit;
  end;

  if AArr.Len = 0 then
    OldPointer := nil
  else
    OldPointer := PIntPtr(AArr.Content);

  if (OldPointer = nil) and (AArr.Len > 0) then
    Exit;

  {$IFnDEF IsDesktop}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content, ANewLength shl CArchBitShift);
    if MM_error or (AArr.Content = nil) then
    begin
      Result := False;
      Exit;
    end;

    if OldPointer <> nil then
      {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl CArchBitShift);  //OldPointer = src, AArr.Content = dest
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

      if OldPointer <> nil then
      //AArr.Len is still the old array length. Only the Content field points somewhere else.
        MemMove(AArr.Content, OldPointer, Min32(ANewLength, AArr.Len) shl CArchBitShift);  //OldPointer = src, AArr.Content = dest   // the rest of the content is not initialized
    except
      Result := False;
    end;
  {$ENDIF}

  if AArr.Len > 0 then
  begin
    {$IFnDEF IsDesktop}
      {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(OldPointer, AArr.Len shl CArchBitShift);
    {$ELSE}
      {$IFDEF UsingDynTFT}
        FreeMem(TPtrRec(OldPointer), AArr.Len shl CArchBitShift);
      {$ELSE}
        FreeMem(OldPointer, AArr.Len shl CArchBitShift);
      {$ENDIF}
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

  OldArr1Len := AArr1.Len;
  Result := SetDynOfPtrUIntLength(AArr1, NewLen);
  if not Result then
    Exit;

  {$IFnDEF IsDesktop}
    NewPointer := DWord(AArr1.Content) + OldArr1Len shl CArchBitShift;
  {$ELSE}
    NewPointer := Pointer(PtrUInt(AArr1.Content) + PtrUInt(OldArr1Len shl CArchBitShift));  //NewPointer := @AArr1.Content^[OldArr1Len shl CArchBitShift];
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(NewPointer, AArr2.Content, AArr2.Len shl CArchBitShift);

  Result := True;
end;


function AddPtrUIntToDynArraysOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ANewPtrUInt: PtrUInt): Boolean;
begin
  Result := SetDynOfPtrUIntLength(AArr, AArr.Len + 1);
  if not Result then
    Exit;

  AArr.Content^[AArr.Len - 1] := ANewPtrUInt;
end;


function RemoveStartPtrUIntsFromDynArray(ACount: TDynArrayLength; var AArr: TDynArrayOfPtrUInt): Boolean;
var
  PartialArr: TDynArrayOfPtrUInt;
  NewLen: TDynArrayLength;
  {$IFDEF AppArch16} Dummy: TDynArrayLength; {$ENDIF}
begin
  Result := True;

  if ACount >= AArr.Len then
  begin
    FreeDynArrayOfPtrUInt(AArr);
    Exit;
  end;

  if ACount = 0 then
    Exit;

  InitDynArrayOfPtrUIntToEmpty(PartialArr);
  NewLen := AArr.Len - ACount;
  if not CopyFromDynArrayOfPtrUInt(PartialArr, AArr, {$IFDEF AppArch16} Dummy, {$ENDIF} ACount, NewLen) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(AArr.Content, PartialArr.Content, NewLen shl CArchBitShift);
  FreeDynArrayOfPtrUInt(PartialArr);

  Result := SetDynOfPtrUIntLength(AArr, NewLen);
end;


function CopyFromDynArrayOfPtrUInt(var ADestArr, ASrcArr: TDynArrayOfPtrUInt; {$IFDEF AppArch16} ADummy, {$ENDIF} AIndex, ACount: TDynArrayLength): Boolean;  //ADestArr should be empty, because it is initialized here
var
  OldPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  Result := True;
  InitDynArrayOfPtrUIntToEmpty(ADestArr);

  if ACount = 0 then
    Exit;

  if AIndex > ASrcArr.Len then
  begin
    Result := False;
    Exit;
  end;

  if ACount > ASrcArr.Len - AIndex then
    ACount := ASrcArr.Len - AIndex;

  if not SetDynOfPtrUIntLength(ADestArr, ACount) then
  begin
    Result := False;
    Exit;
  end;

  {$IFnDEF IsDesktop}
    OldPointer := DWord(ASrcArr.Content) + AIndex shl CArchBitShift;
  {$ELSE}
    OldPointer := Pointer(PtrUInt(ASrcArr.Content) + PtrUInt(AIndex shl CArchBitShift));
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(ADestArr.Content, OldPointer, ACount shl CArchBitShift);
end;


function DeleteItemFromDynArrayOfPtrUInt(var AArr: TDynArrayOfPtrUInt; ADelIndex: TDynArrayLengthSig): Boolean;
var
  DelPointer, NextPointer: {$IFDEF IsDesktop} PIntPtr; {$ELSE} DWord; {$ENDIF}
begin
  if AArr.Len = 0 then
  begin
    Result := False;
    Exit;
  end;

  if (ADelIndex < 0) or (ADelIndex > TDynArrayLengthSig(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Delete index out of bounds in DeleteItemFromDynArrayOfPtrUInt.');
    {$ELSE}
      Result := False;
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
  {$ELSE}
    DelPointer := Pointer(PtrUInt(AArr.Content) + PtrUInt(ADelIndex shl CArchBitShift));  //NewPointer := @AArr.Content^[ADelIndex shl CArchBitShift];
    NextPointer := Pointer(PtrUInt(AArr.Content) + PtrUInt((ADelIndex + 1) shl CArchBitShift));   //NextPointer := DelPointer + SizeOf(Pointer);
  {$ENDIF}
  {$IFDEF RedefineMemMove} MemMove32 {$ELSE} MemMove {$ENDIF}(DelPointer, NextPointer, (TDynArrayLengthSig(AArr.Len) - ADelIndex - 1) shl CArchBitShift);

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


function DynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte): TDynArrayLength;
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
  {$ENDIF}

  Result := AArr.Len;
end;


function SetDynOfPDynArrayOfTDynArrayOfByteLength(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ANewLength: TDynArrayLength): Boolean; //returns True if successful, or False if it can't allocate memory
var
  OldLen: TDynArrayLength;
  i: TDynArrayLengthSig;
  {$IFDEF IsMCU}
    TempDynArrayOfPtrUInt: TDynArrayOfPtrUInt;
  {$ENDIF}
begin
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
  {$ENDIF}

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

      {$IFnDEF IsDesktop}
        {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfTDynArrayOfByte));
      {$ELSE}
        Dispose(AArr.Content^[i]);
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
      {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(AArr.Content^[i], SizeOf(TDynArrayOfTDynArrayOfByte));
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
  {$IFDEF IsDesktop}
    CheckInitializedDynArrayOfPDynArrayOfTDynArrayOfByte(AArr);
    CheckInitializedDynOfDynArray(ANewDynArrayOfTDynArrayOfByte);
  {$ENDIF}

  {$IFnDEF IsMCU}
    New(TempNewPDynArrayOfTDynArrayOfByte);
  {$ELSE}
    {$IFDEF RoundAlloc}GetMemRound{$ELSE}GetMem{$ENDIF}(TempNewPDynArrayOfTDynArrayOfByte, SizeOf(TempNewPDynArrayOfTDynArrayOfByte^));
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


function DeleteItemFromDynArrayOfPDynArrayOfTDynArrayOfByte(var AArr: TDynArrayOfPDynArrayOfTDynArrayOfByte; ADelIndex: TDynArrayLengthSig): Boolean;
{$IFDEF IsMCU}
  var
    TempDynArrayOfPtrUInt: TDynArrayOfPtrUInt;
{$ENDIF}
begin
  if (ADelIndex < 0) or (ADelIndex > TDynArrayLengthSig(AArr.Len) - 1) then
  begin
    {$IFDEF IsDesktop}
      raise Exception.Create('Index out of range when deleting item from DynArrayOfPDynArrayOfTDynArrayOfByte.');
    {$ELSE}
      Result := False;
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
    Dispose(AArr.Content^[ADelIndex]);
    Result := DeleteItemFromDynArrayOfPtrUInt(TDynArrayOfPtrUInt(AArr), ADelIndex);
  {$ELSE}
    {$IFDEF RoundAlloc}FreeMemRound{$ELSE}FreeMem{$ENDIF}(AArr.Content^[ADelIndex], SizeOf(TDynArrayOfTDynArrayOfByte));

    TempDynArrayOfPtrUInt.Len := AArr.Len;
    TempDynArrayOfPtrUInt.Content := PDynArrayOfPtrUIntContent(PtrUInt(AArr.Content));
    Result := DeleteItemFromDynArrayOfPtrUInt(TempDynArrayOfPtrUInt, ADelIndex);
    AArr.Len := TempDynArrayOfPtrUInt.Len;
    AArr.Content := PDynArrayOfPDynArrayOfTDynArrayOfByteContent(PtrUInt(TempDynArrayOfPtrUInt.Content));  //This is required, because SetDynOfPtrUIntLength modifies the pointer.
  {$ENDIF}
end;


{$IFDEF IsDesktop}
  {$IFDEF UsingDynTFT}
    {$IFDEF LogMem}
      procedure DoOnAfterGetMem(AAllocatedAddress, ARequestedSize: DWord);
      begin
        if Assigned(OnAfterGetMem) then
          OnAfterGetMem(AAllocatedAddress, ARequestedSize);
      end;

      procedure DoOnAfterFreeMem(AAllocatedAddress, ARequestedSize: DWord);
      begin
        if Assigned(OnAfterFreeMem) then
          OnAfterFreeMem(AAllocatedAddress, ARequestedSize);
      end;
    {$ENDIF}

      initialization
        {$IFDEF FPC}
          InitCriticalSection(MMCritSec);
        {$ELSE}
          InitializeCriticalSection(MMCritSec);
        {$ENDIF}

        {$IFDEF LogMem}
          OnAfterGetMem := nil;
          OnAfterFreeMem := nil;
        {$ENDIF}

      finalization
        {$IFDEF FPC}
          DoneCriticalSection(MMCritSec);
        {$ELSE}
          DeleteCriticalSection(MMCritSec);
        {$ENDIF}

        {$IFDEF LogMem}
          OnAfterGetMem := nil;
          OnAfterFreeMem := nil;
        {$ENDIF}
  {$ENDIF}
{$ENDIF}


end.