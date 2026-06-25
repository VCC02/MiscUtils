{
    Copyright (C) 2026 VCC
    creation date: 17 Jun 2026
    initial release date: 22 Jun 2026

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

Extra header:
    This unit is a rewrite of the DynMemLib.pas library, found on GitHub.
    Portions of this code may be copied from there.
    It is also another implementation of the old BTMemoryModule library.

    As in the initial library, exception handling in 64-bit dll is still not properly handled.
}


unit DynLibMemLoader;


{$IFDEF FPC}
  {$mode delphi}
  {$H+}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
  {$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}


interface


uses
  Windows, Classes, SysUtils;

type
  {$IFnDEF FPC}
    IMAGE_EXPORT_DIRECTORY = record
      Characteristics: DWord;
      TimeDateStamp: DWord;
      MajorVersion: Word;
      MinorVersion: Word;
      Name: DWord;
      Base: DWord;
      NumberOfFunctions: DWord;
      NumberOfNames: DWord;
      AddressOfFunctions: DWord;    //pointer to array of DWord
      AddressOfNames: DWord;        //pointer to array of DWord
      AddressOfNameOrdinals: DWord; //pointer to array of Word
    end;
    PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;
  {$ENDIF}

  TLibSection = record
    Address: Pointer;
    Size: DWord;
  end;

  TLibSectionArr = array of TLibSection;

  TExportedFunction = record
    Address: Pointer;
    Name: string;
    Ordinal: Integer;
  end;

  TExportedFunctionArr = array of TExportedFunction;

  TDynLibMemLoader = class
  private
    FLibSections: TLibSectionArr;
    FSuccessfullyInitialized: Boolean;
    FImageNtHeaders: PImageNtHeaders;
    FBaseCodeAddress: UInt64;
    FImportedLibsAddress: Pointer;
    FImportedLibsCount: Integer;

    FProtectDiscardableSection: Boolean;
    FProtectNonDiscardableSection: Boolean;
    FExportedFunctions: TExportedFunctionArr;

    procedure SetTableSections(ALibraryContent: Pointer; AExistingHeaders: TImageNtHeaders);
    procedure RelocateImageBase(ARelocationAmount: UInt64);
    procedure LoadImportedLibraries;
    procedure ProtectTableSections;

    procedure GetAllExportedFunctions;
    function FindProcIndexByName(AFuncName: PChar): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure SetProtectionFlags(AProtectDiscardableSection, AProtectNonDiscardableSection: Boolean);
    function LoadLibrary(ALibraryContent: Pointer; AAttachLibraryOnLoad: Boolean = True): Boolean;
    function GetProcAddress(AFuncName: PChar; ARaiseExceptionIfNotFound: Boolean = False): Pointer; overload;
    function GetProcAddress(AFuncName: string; ARaiseExceptionIfNotFound: Boolean = False): Pointer; overload;
    procedure ListExportedFunctions(var AFunctions: TExportedFunctionArr);
    procedure HandleTLS(AReason: DWord); //Can be called with DLL_THREAD_ATTACH and DLL_THREAD_DETACH when the app using this library, creates a new thread / destroys it.
    procedure FreeLibrary;

    property SuccessfullyInitialized: Boolean read FSuccessfullyInitialized;
  end;


implementation


{$IFnDEF FPC}
  type
    {$IFDEF CPUX64}
      PtrUInt = UInt64;
      PtrInt = Int64;
    {$ELSE} //CPUX32
      PtrUInt = Cardinal;
      PtrInt = LongInt;
    {$ENDIF}

    PUInt64 = ^UInt64;
    PPtrUInt = ^PtrUInt;
{$ENDIF}


const
  IMAGE_SIZEOF_BASE_RELOCATION: DWord = 8;

  IMAGE_REL_BASED_HIGHLOW {: DWord} = 3;
  IMAGE_REL_BASED_DIR64 {: DWord} = 10;

  {$IFnDEF FPC}
    IMAGE_ORDINAL_FLAG32: DWord = $80000000;  //if not defined in Delphi
    IMAGE_ORDINAL_FLAG64: UInt64 = UInt64($8000000000000000);  //if not defined in Delphi
  {$ENDIF}

type
  IMAGE_BASE_RELOCATION = record
    VirtualAddress: DWord;
    SizeOfBlock: DWord;
  end;

  PIMAGE_BASE_RELOCATION = ^IMAGE_BASE_RELOCATION;

  {$IFnDEF FPC}
    IMAGE_IMPORT_DESCRIPTOR = record  //missing from Delphi
      OriginalFirstThunk: DWord;
      TimeDateStamp: DWord;
      ForwarderChain: DWord;
      Name: DWord;
      FirstThunk: DWord;
    end;
    PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;

    //ToDo: IMAGE_TLS_DIRECTORY for Delphi
  {$ENDIF}

  {$IFDEF CPUX64} //Delphi only
    IMAGE_RUNTIME_FUNCTION_ENTRY = record
      BeginAddress: DWord;
      EndAddress: DWord;
      ExceptionHandler: Pointer;
      HandlerData: Pointer;
      PrologEndAddress: DWord;
    end;

    PIMAGE_RUNTIME_FUNCTION_ENTRY = ^IMAGE_RUNTIME_FUNCTION_ENTRY;
  {$ENDIF}

  IMAGE_IMPORT_BY_NAME = record
    Hint: Word;
    Name: array[Byte] of AnsiChar;
  end;

  TWideCharArray = array[0..2047] of WideChar;

  //This is also called DllMain.
  TLibEntryProc = function(AInst: HINST; AReason: DWord; AReserved: Pointer): Bool; stdcall;

  TDWordArray = array[0..0] of DWord;
  PDWordArray = ^TDWordArray;

  TWordArray = array[0..0] of Word;
  PWordArray = ^TWordArray;


function GetFirstSection(AHeader: PImageNtHeaders): PIMAGESECTIONHEADER;
var
  Is64Bit: Boolean;
begin
  //Result := PIMAGESECTIONHEADER(UInt64(@AHeader^.OptionalHeader) + UInt64(AHeader^.FileHeader.SizeOfOptionalHeader));  //the loader has to match the library bitness

  Is64Bit := AHeader^.OptionalHeader.Magic = $20B;
  if Is64Bit then
    Result := PIMAGESECTIONHEADER(UInt64(AHeader) + SizeOf(IMAGE_NT_HEADERS64))
  else
    Result := PIMAGESECTIONHEADER(UInt64(AHeader) + SizeOf(IMAGE_NT_HEADERS32));
end;


constructor TDynLibMemLoader.Create;
begin
  inherited Create;
  FSuccessfullyInitialized := False;
  FImageNtHeaders := nil;
  FBaseCodeAddress := 0;
  FImportedLibsAddress := nil;
  FImportedLibsCount := 0;
  FProtectDiscardableSection := True;
  FProtectNonDiscardableSection := True;

  SetLength(FLibSections, 0);
  SetLength(FExportedFunctions, 0);
end;


destructor TDynLibMemLoader.Destroy;
begin
  SetLength(FLibSections, 0);
  SetLength(FExportedFunctions, 0);
  inherited Destroy;
end;


procedure TDynLibMemLoader.SetProtectionFlags(AProtectDiscardableSection, AProtectNonDiscardableSection: Boolean);
begin
  FProtectDiscardableSection := AProtectDiscardableSection;
  FProtectNonDiscardableSection := AProtectNonDiscardableSection;
end;


procedure TDynLibMemLoader.SetTableSections(ALibraryContent: Pointer; AExistingHeaders: TImageNtHeaders);
var
  TempSection: PIMAGESECTIONHEADER;
  i, n: Integer;
  CommitSize: DWord;
  SrcPointer, DestPointer, VAPointer: Pointer;
begin
  TempSection := GetFirstSection(FImageNtHeaders);
  n := FImageNtHeaders^.FileHeader.NumberOfSections;
  SetLength(FLibSections, n);

  for i := 0 to n - 1 do
  begin
    VAPointer := Pointer(FBaseCodeAddress + UInt64(TempSection^.VirtualAddress));

    if TempSection^.SizeOfRawData = 0 then
    begin
      CommitSize := AExistingHeaders.OptionalHeader.SectionAlignment;
      if CommitSize > 0 then
      begin
        if CommitSize < TempSection^.Misc.VirtualSize then
          CommitSize := TempSection^.Misc.VirtualSize;

        DestPointer := VirtualAlloc(VAPointer, PtrUInt(CommitSize), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        FLibSections[i].Address := DestPointer;
        FLibSections[i].Size := CommitSize;
        TempSection^.Misc.PhysicalAddress := UInt64(DestPointer);
        ZeroMemory(DestPointer, CommitSize);
      end;
    end
    else
    begin
      DestPointer := VirtualAlloc(VAPointer, PtrUInt(TempSection^.SizeOfRawData), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
      FLibSections[i].Address := DestPointer;
      FLibSections[i].Size := TempSection^.SizeOfRawData;
      SrcPointer := Pointer(UInt64(ALibraryContent) + UInt64(TempSection^.PointerToRawData));

      Move(SrcPointer^, DestPointer^, TempSection^.SizeOfRawData);
      TempSection^.Misc.PhysicalAddress := UInt64(DestPointer);
    end;

    TempSection := Pointer(UInt64(TempSection) + UInt64(SizeOf(TIMAGESECTIONHEADER)));
  end;
end;


procedure TDynLibMemLoader.RelocateImageBase(ARelocationAmount: UInt64);
var
  ImageDataDir: PImageDataDirectory;
  BaseRelocationAddress: PIMAGE_BASE_RELOCATION;
  DestPointer, DestPointerWithOffset: Pointer;
  RelocationInfo: PWord;
  i, ChunksCount: PtrInt;
  RelocationType: DWord;
  RelocationOffset: Integer;
begin
  ImageDataDir := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];

  if ImageDataDir^.Size > 0 then
  begin
    BaseRelocationAddress := Pointer(FBaseCodeAddress + UInt64(ImageDataDir^.VirtualAddress));

    while BaseRelocationAddress^.VirtualAddress > 0 do
    begin
      DestPointer := Pointer(FBaseCodeAddress + UInt64(BaseRelocationAddress^.VirtualAddress));
      RelocationInfo := Pointer(UInt64(BaseRelocationAddress) + UInt64(IMAGE_SIZEOF_BASE_RELOCATION));
      ChunksCount := (BaseRelocationAddress^.SizeOfBlock - IMAGE_SIZEOF_BASE_RELOCATION) shr 1;

      for i := 0 to ChunksCount - 1 do
      begin
        RelocationType := RelocationInfo^ shr 12;
        RelocationOffset := RelocationInfo^ and $0FFF;
        DestPointerWithOffset := Pointer(UInt64(DestPointer) + UInt64(RelocationOffset));

        case RelocationType of
          IMAGE_REL_BASED_HIGHLOW:
          begin
            //maybe handle differently between 32 and 64

            if ARelocationAmount <= $FFFFFFFF then
              PCardinal(DestPointerWithOffset)^ := PCardinal(DestPointerWithOffset)^ + Cardinal(ARelocationAmount)  //limit to 32-bit
            else
              PUInt64(DestPointerWithOffset)^ := PUInt64(DestPointerWithOffset)^ + ARelocationAmount;   //old code for both 32-bit and 64-bit
          end;

          IMAGE_REL_BASED_DIR64:
          begin
            //64-bit only
            PUInt64(DestPointerWithOffset)^ := PUInt64(DestPointerWithOffset)^ + ARelocationAmount;
          end;

          else
            ;
        end; //case

        Inc(RelocationInfo); //next (pointer) location
      end;  //for

      BaseRelocationAddress := Pointer(UInt64(BaseRelocationAddress) + UInt64(BaseRelocationAddress^.SizeOfBlock));
    end;
  end;
end;


procedure ConvertPAnsiCharToPChar(AAnsi: Pointer; var AWide: TWideCharArray);
begin
  {$IFDEF UNICODE}
    Move(StringToOleStr(PAnsiChar(AAnsi))[0], AWide[0], (High(TWideCharArray) + 1) shl 1);
  {$ELSE}
    Move(PAnsiChar(AAnsi)[0], AWide[0], High(TWideCharArray) + 1);
  {$ENDIF}
end;


function GetExportRange(ADllHandle: HINST; out AExportVirtualAddress, AExportSize: UInt64): Boolean;
var
  DosHeader: IMAGE_DOS_HEADER;
  TempImageNtHeaders: PImageNtHeaders;
  ImageDataDir: PImageDataDirectory;
begin
  Result := False;
  AExportVirtualAddress := 0;
  AExportSize := 0;

  if ADllHandle = 0 then
    Exit;

  Move(Pointer(ADllHandle)^, DosHeader, SizeOf(IMAGE_DOS_HEADER));
  if DosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;

  TempImageNtHeaders := PImageNtHeaders(UInt64(ADllHandle) + DosHeader._lfanew);
  if TempImageNtHeaders^.Signature <> IMAGE_NT_SIGNATURE then
    Exit;

  ImageDataDir := @TempImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT]; //EXPORT this time
  if ImageDataDir^.Size = 0 then
    Exit;

  AExportVirtualAddress := UInt64(ADllHandle) + ImageDataDir^.VirtualAddress;
  AExportSize := ImageDataDir^.Size;
  Result := True;
end;


function GetForwarderProcAddress(AFwd: string): Pointer;
var
  DotPs: Integer;
  FwdDll, FwdFunc: string;
  DllHandle: HINST;
  FuncOrdinal: Int64;
begin
  Result := nil;
  DotPs := Pos('.', AFwd);
  if DotPs = 0 then
    raise Exception.Create('Forwarder export string doesn''t have the expected format: ' + AFwd);

  FwdDll := Copy(AFwd, 1, DotPs);
  FwdFunc := Copy(AFwd, DotPs + 1, MaxInt);

  DllHandle := Windows.LoadLibrary(PChar(FwdDll));
  if DllHandle = 0 then
    raise Exception.Create('Unable to load forwarder dll: ' + FwdDll);

  if (FwdFunc <> '') and (FwdFunc[1] = '#') then
  begin   //"#<ordinal>" format
    Delete(FwdFunc, 1, 1);
    FuncOrdinal := StrToInt64Def(FwdFunc, -1);
    if FuncOrdinal > -1 then
      Result := Windows.GetProcAddress(DllHandle, PAnsiChar(FuncOrdinal and $0000FFFF));
  end
  else    //"<name>" format
    Result := Windows.GetProcAddress(DllHandle, PAnsiChar(FwdFunc));
end;


procedure TDynLibMemLoader.LoadImportedLibraries;
const
  COrdinalMask: UInt64 = $0000FFFF;
var
  ImageDataDir: PImageDataDirectory;
  ImportDescriptor: PIMAGE_IMPORT_DESCRIPTOR;
  ImportedLibName: TWideCharArray;
  ImportedLibNameAddress: Pointer;
  ImportedLibHandle: HModule;
  Temp64: UInt64;
  ThunkRef, IATAddress: PPtrUInt;
  FuncNamePtr: Pointer;
  FuncNameStr, ImportedLibNameStr: AnsiString;
  ThunkData: IMAGE_IMPORT_BY_NAME;
  TempImportedLibsAddress: Pointer;
  MaxImportRVA, ThunkRVA, ThunkValue: UInt64;
  Is64Bit: Boolean;
  ExportVirtualAddress, ExportSize: UInt64;
  IsValidExportRange: Boolean;
  DepAddress: Pointer;
begin
  ImageDataDir := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];

  if ImageDataDir^.Size = 0 then
    Exit;

  if ImageDataDir^.VirtualAddress = 0 then
    Exit;

  Is64Bit := FImageNtHeaders^.OptionalHeader.Magic = $20B;
  MaxImportRVA := UInt64(ImageDataDir^.VirtualAddress) + UInt64(ImageDataDir^.Size);
  ImportDescriptor := PIMAGE_IMPORT_DESCRIPTOR(FBaseCodeAddress + UInt64(ImageDataDir^.VirtualAddress)); //The first ImportDescriptor starts at VirtualAddress.

  while UInt64(ImportDescriptor) - FBaseCodeAddress < MaxImportRVA do
  begin
    if ImportDescriptor^.Name = 0 then
      Break;

    ImportedLibNameAddress := Pointer(FBaseCodeAddress + UInt64(ImportDescriptor^.Name));

    try
      ConvertPAnsiCharToPChar(ImportedLibNameAddress, ImportedLibName);
      ImportedLibNameStr := AnsiString(PAnsiChar(ImportedLibNameAddress)); //AnsiString(ImportedLibName);
    except
      raise Exception.Create('Cannot convert library name: ' + ImportedLibNameStr);
    end;

    try
      ImportedLibHandle := Windows.LoadLibrary(@ImportedLibName[0]);   //External AV when ImportedLibName is 'cmdlg32.dll', imported by a custom dll, only after loading Kernel32.dll, for using 'Sleep'. This happens if the custom dll uses the Application variable in an exported function. (The exported function initializes the global Application variable and creates a form, and this export is not even called by the loader app).
      //ImportedLibHandle := Windows.LoadLibrary(PAnsiChar(ImportedLibNameAddress));  //a simple call, if the library name is not unicode
    except
      ImportedLibHandle := 0;
    end;

    if ImportedLibHandle = 0 then
      raise Exception.Create('Cannot load library or library not found: ' + ImportedLibNameStr);

    if ImportDescriptor^.OriginalFirstThunk <> 0 then
      ThunkRef := Pointer(FBaseCodeAddress + UInt64(ImportDescriptor^.OriginalFirstThunk))
    else
      ThunkRef := Pointer(FBaseCodeAddress + UInt64(ImportDescriptor^.FirstThunk));

    if ImportDescriptor^.OriginalFirstThunk <> 0 then
      ThunkRVA := ImportDescriptor^.OriginalFirstThunk
    else
      ThunkRVA := ImportDescriptor^.FirstThunk;

    IATAddress := Pointer(FBaseCodeAddress + UInt64(ImportDescriptor^.FirstThunk));

    while True do
    begin
      if Is64Bit then
        ThunkValue := PUInt64(FBaseCodeAddress + ThunkRVA)^
      else
        ThunkValue := PUInt32(FBaseCodeAddress + ThunkRVA)^;

      if ThunkValue = 0 then
        Break;

      IsValidExportRange := GetExportRange(ImportedLibHandle, ExportVirtualAddress, ExportSize);

      if    (Is64Bit and (ThunkValue and IMAGE_ORDINAL_FLAG64 <> 0)) or
        (not Is64Bit and (ThunkValue and IMAGE_ORDINAL_FLAG32 <> 0)) then
      begin
        //IATAddress^ := UInt64(Windows.GetProcAddress(ImportedLibHandle, PAnsiChar(ThunkValue and COrdinalMask))) //old code, which does not use forwarders
        DepAddress := Pointer(Windows.GetProcAddress(ImportedLibHandle, PAnsiChar(ThunkValue and COrdinalMask)))
      end
      else
      begin
        FuncNamePtr := Pointer(FBaseCodeAddress + ThunkValue);
        Move(FuncNamePtr^, ThunkData, SizeOf(IMAGE_IMPORT_BY_NAME));
        FuncNameStr := AnsiString(PAnsiChar(@ThunkData.Name));
        //IATAddress^ := UInt64(Windows.GetProcAddress(ImportedLibHandle, PAnsiChar(FuncNameStr)));  //old code, which does not use forwarders
        DepAddress := Pointer(Windows.GetProcAddress(ImportedLibHandle, PAnsiChar(FuncNameStr)));
      end;

      //if IATAddress^ = 0 then
      if DepAddress = nil then
        raise Exception.Create('GetProcAddress cannot get address while creating import table, at ModuleIndex ' + IntToStr(FImportedLibsCount) + ' and function ' + string(@ThunkData.Name) + '.');

      if IsValidExportRange then
      begin
        if (UInt64(DepAddress) >= ExportVirtualAddress) and (UInt64(DepAddress) < ExportVirtualAddress + ExportSize) then // in range
        begin
          //ToDo: requires more testing
          //MessageBox(0, 'in range', '', 0);  //debug code
          DepAddress := GetForwarderProcAddress(AnsiString(PAnsiChar(DepAddress)));    //ToDo: This call (and there is one more) loads the new libray, which has to be freed later.
        end;            //Ideally, there should be a recursion on solving forwarders, but for now, one level should be fine.
      end;

      IATAddress^ := UInt64(DepAddress);

      Inc(IATAddress);
      Inc(ThunkRef);

      if Is64Bit then
        Inc(ThunkRVA, 8)
      else
        Inc(ThunkRVA, 4);
    end; //while

    ReAllocMem(FImportedLibsAddress, PtrUInt((FImportedLibsCount + 1) shl 3));

    if FImportedLibsAddress = nil then
      raise Exception.Create('ReAllocMem cannot allocate memory for the library modules. ModulesCount is ' + IntToStr(FImportedLibsCount));

    Temp64 := FImportedLibsCount shl 3;
    TempImportedLibsAddress := Pointer(UInt64(FImportedLibsAddress) + Temp64);
    UInt64(TempImportedLibsAddress^) := ImportedLibHandle;

    Inc(FImportedLibsCount);

    ImportDescriptor := PIMAGE_IMPORT_DESCRIPTOR(UInt64(ImportDescriptor) + UInt64(SizeOf(IMAGE_IMPORT_DESCRIPTOR)));
  end;
end;


function GetSectionProtectionInfo(ACharacteristics: DWord): DWord;
begin
  Result := 0;
  if ACharacteristics and IMAGE_SCN_MEM_NOT_CACHED > 0 then
    Result := Result or PAGE_NOCACHE;

  if ACharacteristics and IMAGE_SCN_MEM_EXECUTE > 0 then
  begin
    //with execute flag
    if ACharacteristics and IMAGE_SCN_MEM_READ > 0 then
    begin
      //with read flag
      if ACharacteristics and IMAGE_SCN_MEM_WRITE > 0 then
        Result := Result or PAGE_EXECUTE_READWRITE
      else
        Result := Result or PAGE_EXECUTE_READ
    end
    else
    begin
      //without read flag
      if ACharacteristics and IMAGE_SCN_MEM_WRITE > 0 then
        Result := Result or PAGE_EXECUTE_WRITECOPY
      else
        Result := Result or PAGE_EXECUTE
    end
  end
  else
  begin
    //without execute flag
    if ACharacteristics and IMAGE_SCN_MEM_READ > 0 then
    begin
      //with read flag
      if ACharacteristics and IMAGE_SCN_MEM_WRITE > 0 then
        Result := Result or PAGE_READWRITE
      else
        Result := Result or PAGE_READONLY;
    end
    else
    begin
      //without read flag
      if ACharacteristics and IMAGE_SCN_MEM_WRITE > 0 then
        Result := Result or PAGE_WRITECOPY
      else
        Result := Result or PAGE_NOACCESS;
    end;
  end;
end;


procedure TDynLibMemLoader.ProtectTableSections;
var
  WorkSection: PIMAGESECTIONHEADER;
  i, n: Integer;
  ProtectionInfo, OldProtectionInfo, DataSize: DWord;
begin
  WorkSection := GetFirstSection(FImageNtHeaders);
  n := FImageNtHeaders^.FileHeader.NumberOfSections;

  for i := 0 to n - 1 do
  begin
    if FProtectDiscardableSection and (WorkSection^.Characteristics and IMAGE_SCN_MEM_DISCARDABLE > 0) then
    begin
      VirtualFree(Pointer(WorkSection^.Misc.PhysicalAddress), PtrUInt(WorkSection^.SizeOfRawData), MEM_DECOMMIT);
      WorkSection := Pointer(UInt64(WorkSection) + UInt64(SizeOf(TIMAGESECTIONHEADER)));
      Continue;
    end;

    if FProtectNonDiscardableSection and (WorkSection^.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = 0) then
    begin
      ProtectionInfo := GetSectionProtectionInfo(WorkSection^.Characteristics);
      if WorkSection^.Characteristics and IMAGE_SCN_MEM_NOT_CACHED > 0 then
        ProtectionInfo := ProtectionInfo or PAGE_NOCACHE;

      DataSize := WorkSection^.SizeOfRawData;
      if DataSize = 0 then
      begin
        if WorkSection^.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA > 0 then
          DataSize := FImageNtHeaders^.OptionalHeader.SizeOfInitializedData
        else
          if WorkSection^.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA > 0 then
            DataSize := FImageNtHeaders^.OptionalHeader.SizeOfUninitializedData;

        if DataSize > 0 then
          if not VirtualProtect(Pointer(WorkSection^.Misc.PhysicalAddress), WorkSection^.SizeOfRawData, ProtectionInfo, OldProtectionInfo) then //OldProtectionInfo has to be a valid var
          begin
            //Exit;//raise Exception.Create('VirtualProtect cannot set protection info at section ' + IntToStr(i) + ' of ' + IntToStr(n) + '.');
          end;
      end;
    end;

    WorkSection := Pointer(UInt64(WorkSection) + UInt64(SizeOf(TIMAGESECTIONHEADER)));
  end;
end;


{$IFDEF CPUX64}
  function RtlAddFunctionTable(FunctionTable: PIMAGE_RUNTIME_FUNCTION_ENTRY; EntryCount: DWord; BaseAddress: QWord): Bool; external 'kernel32' name 'RtlAddFunctionTable';
{$ENDIF}

function TDynLibMemLoader.LoadLibrary(ALibraryContent: Pointer; AAttachLibraryOnLoad: Boolean = True): Boolean;
var
  DosHeader: TIMAGEDOSHEADER;
  OldHeader: TImageNtHeaders;
  HeaderAddress, CodeAddress: Pointer;
  LocationAmount: UInt64;
  LibEntryProc: TLibEntryProc;
  {$IFDEF CPUX64}
    ImageDataDir: PImageDataDirectory;
    ImageRuntime: PIMAGE_RUNTIME_FUNCTION_ENTRY;
    Count: DWord;
  {$ENDIF}
begin
  Result := False;

  try
    Move(ALibraryContent^, DosHeader, SizeOf(IMAGE_DOS_HEADER));
    if DosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
      raise Exception.Create('Invalid DOS header in library.');

    HeaderAddress := Pointer(UInt64(ALibraryContent) + UInt64(DosHeader._lfanew));
    Move(HeaderAddress^, OldHeader, SizeOf(IMAGE_NT_HEADERS));

    if OldHeader.Signature <> IMAGE_NT_SIGNATURE then
      raise Exception.Create('Invalid IMAGE_NT_SIGNATURE value.');

    CodeAddress := VirtualAlloc(Pointer(OldHeader.OptionalHeader.ImageBase),
                                PtrUInt(OldHeader.OptionalHeader.SizeOfImage),
                                MEM_RESERVE,
                                PAGE_EXECUTE_READWRITE);

    if CodeAddress = nil then        //If can't reserve memory at ImageBase, then let the system find an available area.
      CodeAddress := VirtualAlloc(nil,
                                  PtrUInt(OldHeader.OptionalHeader.SizeOfImage),
                                  MEM_RESERVE,
                                  PAGE_EXECUTE_READWRITE);

    if CodeAddress = nil then
      raise Exception.Create('Cannot allocate memory with VirtualAlloc for library code.');

    FBaseCodeAddress := UInt64(CodeAddress);

    VirtualAlloc(CodeAddress, PtrUInt(OldHeader.OptionalHeader.SizeOfImage), MEM_COMMIT, PAGE_EXECUTE_READWRITE);   //Check result?
    HeaderAddress := VirtualAlloc(CodeAddress, PtrUInt(OldHeader.OptionalHeader.SizeOfHeaders), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    Move(ALibraryContent^, HeaderAddress^, UInt64(DosHeader._lfanew) + UInt64(OldHeader.OptionalHeader.SizeOfHeaders));

    FImageNtHeaders := PImageNtHeaders(UInt64(HeaderAddress) + UInt64(DosHeader._lfanew));
    FImageNtHeaders^.OptionalHeader.ImageBase := PtrUInt(CodeAddress);
    SetTableSections(ALibraryContent, OldHeader);

    LocationAmount := UInt64(CodeAddress) - UInt64(OldHeader.OptionalHeader.ImageBase);
    if LocationAmount <> 0 then
      RelocateImageBase(LocationAmount);

    {$IFDEF CPUX64}
      ImageDataDir := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXCEPTION];
      ImageRuntime := Pointer(UInt64(CodeAddress) + UInt64(ImageDataDir^.VirtualAddress));
      Count := ImageDataDir^.Size div SizeOf(IMAGE_RUNTIME_FUNCTION_ENTRY) {- 1};

      if Count > 0 then
        if not RtlAddFunctionTable(ImageRuntime, Count, UInt64(CodeAddress)) then
          raise Exception.Create('Cannot add function to table.');
    {$ENDIF}

    try
      LoadImportedLibraries;
    except
      on E: Exception do
        raise Exception.Create('Cannot load library imports. ' + E.Message);
    end;

    ProtectTableSections;
    HandleTLS(DLL_PROCESS_ATTACH);

    if FImageNtHeaders^.OptionalHeader.AddressOfEntryPoint > 0 then
    begin
      if AAttachLibraryOnLoad then
      begin
        @LibEntryProc := Pointer(UInt64(CodeAddress) + UInt64(FImageNtHeaders^.OptionalHeader.AddressOfEntryPoint));
        if @LibEntryProc = nil then
          raise Exception.Create('Cannot get library entry point.');

        if not LibEntryProc(UInt64(CodeAddress), DLL_PROCESS_ATTACH, nil) then
          raise Exception.Create('Cannot attach library.');
      end;
    end;

    GetAllExportedFunctions;
    FSuccessfullyInitialized := True;
    Result := True;
  except
    on E: Exception do
    begin
      FreeLibrary;
      raise
    end;
  end;
end;


procedure TDynLibMemLoader.GetAllExportedFunctions;
var
  ImageDataDir: PImageDataDirectory;
  ExportsDir: PIMAGE_EXPORT_DIRECTORY;
  i, TempOrdinal: Integer;

  ComputedAddressOfFunctions: PDWordArray;
  ComputedAddressOfNames: PDWordArray;
  ComputedAddressOfNameOrdinals: PWordArray;
  FuncName: AnsiString;
begin
  ImageDataDir := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  if ImageDataDir^.Size = 0 then
    Exit;

  ExportsDir := Pointer(FBaseCodeAddress + UInt64(ImageDataDir^.VirtualAddress));
  if (ExportsDir^.NumberOfNames = 0) or (ExportsDir^.NumberOfFunctions = 0) then
    Exit;

  SetLength(FExportedFunctions, ExportsDir^.NumberOfNames);  //NumberOfNames may also happen to be less than NumberOfFunctions

  ComputedAddressOfNames := Pointer(FBaseCodeAddress + UInt64(ExportsDir^.AddressOfNames));
  ComputedAddressOfNameOrdinals := Pointer(FBaseCodeAddress + UInt64(ExportsDir^.AddressOfNameOrdinals));

  for i := 0 to Length(FExportedFunctions) - 1 do
  begin
    FuncName := AnsiString(PAnsiChar(FBaseCodeAddress + ComputedAddressOfNames^[i]));
    FExportedFunctions[i].Name := FuncName;
    ComputedAddressOfFunctions := Pointer(FBaseCodeAddress + UInt64(ExportsDir^.AddressOfFunctions));

    TempOrdinal := ComputedAddressOfNameOrdinals^[i];
    FExportedFunctions[i].Ordinal := TempOrdinal;
    FExportedFunctions[i].Address := Pointer(FBaseCodeAddress + UInt64(ComputedAddressOfFunctions^[TempOrdinal]));
  end;
end;


function TDynLibMemLoader.FindProcIndexByName(AFuncName: PChar): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Length(FExportedFunctions) - 1 do
    if FExportedFunctions[i].Name = string(AFuncName) then
    begin
      Result := i;
      Break;
    end;
end;



function TDynLibMemLoader.GetProcAddress(AFuncName: PChar; ARaiseExceptionIfNotFound: Boolean = False): Pointer;
var
  ProcIndex: Integer;
  ImageDataDir: PImageDataDirectory;
  ExportsDir: PIMAGE_EXPORT_DIRECTORY;
  CurrentAddress, ExportVirtualAddress, ExportSize: UInt64;
begin
  Result := nil;
  if AFuncName = nil then
    Exit;

  ImageDataDir := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  if ImageDataDir^.Size = 0 then
    if ARaiseExceptionIfNotFound then
      raise Exception.Create('No export table found.')
    else
      Exit;

  ExportsDir := Pointer(FBaseCodeAddress + UInt64(ImageDataDir^.VirtualAddress));
  if (ExportsDir^.NumberOfNames = 0) or (ExportsDir^.NumberOfFunctions = 0) then
    if ARaiseExceptionIfNotFound then
      raise Exception.Create('No exports found.')
    else
      Exit;

  ProcIndex := FindProcIndexByName(AFuncName);
  if ProcIndex > -1 then
  begin
    CurrentAddress := UInt64(FExportedFunctions[ProcIndex].Address);
    ExportVirtualAddress := UInt64(ExportsDir);
    ExportSize := ImageDataDir^.Size;

    if (CurrentAddress >= ExportVirtualAddress) and (CurrentAddress < ExportVirtualAddress + ExportSize) then
      Result := GetForwarderProcAddress(AnsiString(PAnsiChar(CurrentAddress)))
    else
      Result := FExportedFunctions[ProcIndex].Address;

    //No forwarders handling:
    //Result := FExportedFunctions[ProcIndex].Address;
  end;
end;


function TDynLibMemLoader.GetProcAddress(AFuncName: string; ARaiseExceptionIfNotFound: Boolean = False): Pointer;
begin
  Result := nil;
  if AFuncName = '' then
    Exit;

  Result := GetProcAddress(PChar(@AFuncName[1]), ARaiseExceptionIfNotFound);
end;


procedure TDynLibMemLoader.HandleTLS(AReason: DWord);
var
  TLSDirEntry: PImageDataDirectory;
  TLSVirtualAddress: UInt64;
  CallbacksVirtualAddress: UInt64;
  Is64bit: Boolean;
  TLSDir64: PIMAGE_TLS_DIRECTORY64;
  TLSDir32: PIMAGE_TLS_DIRECTORY32;
  i: Integer;
  CallbackProc: TLibEntryProc;
begin
  TLSDirEntry := @FImageNtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_TLS];
  if (TLSDirEntry^.Size = 0) or (TLSDirEntry^.VirtualAddress = 0) then
    Exit; //no TLS

  TLSVirtualAddress := FBaseCodeAddress + TLSDirEntry^.VirtualAddress;
  Is64Bit := FImageNtHeaders^.OptionalHeader.Magic = $20B;

  if Is64Bit then
  begin
    TLSDir64 := PIMAGE_TLS_DIRECTORY64(TLSVirtualAddress);
    CallbacksVirtualAddress := TLSDir64^.AddressOfCallBacks;
  end
  else
  begin
    TLSDir32 := PIMAGE_TLS_DIRECTORY32(TLSVirtualAddress);
    CallbacksVirtualAddress := TLSDir32^.AddressOfCallBacks;
  end;

  if CallbacksVirtualAddress = 0 then
    Exit;

  for i := 0 to 1023 do
  begin
    //ToDo: verify pointer arithmetic:  - if wrong, then change Pointer(FBaseCodeAddress ..) to UInt64(..)
    //Eventually, replace this with ^array
    CallbackProc := PPointer(Pointer(FBaseCodeAddress + CallbacksVirtualAddress) + i * SizeOf(Pointer))^;  //ToDo: verify if SizeOf(Pointer) or (Ord(Is64Bit) + 1) * 4
    if not Assigned(CallbackProc) then
      Break;

    CallbackProc(FBaseCodeAddress, AReason, nil);
  end;
end;


procedure TDynLibMemLoader.ListExportedFunctions(var AFunctions: TExportedFunctionArr);
var
  i: Integer;
begin
  SetLength(AFunctions, Length(FExportedFunctions));
  for i := 0 to Length(FExportedFunctions) - 1 do
    AFunctions[i] := FExportedFunctions[i];
end;


procedure TDynLibMemLoader.FreeLibrary;
var
  i: PtrInt;
  LibEntryProc: TLibEntryProc;
  Temp64: UInt64;
  TempImportedLibsAddress: Pointer;
begin
  if not FSuccessfullyInitialized then
    Exit;

  if FImageNtHeaders^.OptionalHeader.AddressOfEntryPoint > 0 then  //not all libraries implement this entry point function
  begin
    HandleTLS(DLL_PROCESS_DETACH);

    @LibEntryProc := Pointer(FBaseCodeAddress + UInt64(FImageNtHeaders^.OptionalHeader.AddressOfEntryPoint));
    if @LibEntryProc = nil then
      raise Exception.Create('Cannot get library entry point.');

    try
      //This call is limited to 32-bit because it crashes on 64-bit.
      //{$IFnDEF CPUX64}   // Verified on 64-bit. It crashed randomly. Possible memory corruption or bad exception handling.
        LibEntryProc(FBaseCodeAddress, DLL_PROCESS_DETACH, nil); //Do not verify the result, because the library sets it.
      //{$ENDIF}
    except
      on E: Exception do
        raise Exception.Create('Cannot call library entry proc for detaching. ' + E.Message);
    end;
  end;

  FSuccessfullyInitialized := False;

  for i := 0 to FImportedLibsCount - 1 do
  begin
    Temp64 := i shl 3;  //Allocated as multiple by 8 in LoadImportedLibraries.

    TempImportedLibsAddress := Pointer(UInt64(FImportedLibsAddress) + Temp64);
    if UInt64(TempImportedLibsAddress^) <> INVALID_HANDLE_VALUE then
      Windows.FreeLibrary(UInt64(TempImportedLibsAddress^));
  end;

  FreeMemory(FImportedLibsAddress);

  if FBaseCodeAddress <> 0 then
    VirtualFree(Pointer(FBaseCodeAddress), 0, MEM_RELEASE);

  FImportedLibsAddress := nil;

  for i := 0 to Length(FLibSections) - 1 do
    if FLibSections[i].Address <> nil then
      VirtualFree(FLibSections[i].Address, 0, MEM_RELEASE);
end;

end.
