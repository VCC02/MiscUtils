{
    Copyright (C) 2026 VCC
    creation date: 19 Jun 2026
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
}


unit TestLoader;

//{$mode ObjFPC}{$H+}
{$mode Delphi}{$H+}
{$WARN 4055 off : Conversion between ordinals and pointers is not portable}
{$WARN 4056 off : Conversion between ordinals and pointers is not portable}

interface

uses
  Classes, SysUtils, fpcunit, DynLibMemLoader, Expectations;

type
  TTestLoader = class(TTestCase)
  private
    FFirstLoader: TDynLibMemLoader;  //1st inst
    FFirstLoader2: TDynLibMemLoader; //2nd inst
    FSecondLoader: TDynLibMemLoader; //2nd dll

    //UIClicker plugins:
    FDistPluginLoader: TDynLibMemLoader;
    FBrokerParamsPluginLoader: TDynLibMemLoader;

    function LoadDll(APath: string; AAttachLibraryOnLoad: Boolean = True): TDynLibMemLoader;
    procedure UnloadDll(var ALoader: TDynLibMemLoader);

    procedure SetAddrOfFirstProcs;
    procedure SetAddrOfFirst2Procs;
    procedure SetAddrOfSecondProcs;

    procedure SetAddrOfDistProcs;
    procedure SetAddrOfBrokerParamsProcs;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute;

    procedure LoadUnload_SingleDll_WithoutCallingFunctions;
    procedure LoadUnload_TwoDll_WithoutCallingFunctions;
    procedure LoadUnload_SingleDll_GetAllFunctions;
    procedure LoadUnload_SingleDll_VerifyDefaultGlobalVar_NoMain;
    procedure LoadUnload_SingleDll_VerifyAllFunctionAddresses;
    procedure LoadUnload_SingleDll_VerifyDefaultGlobalVar;
    procedure LoadUnload_SingleDll_VerifySettingGlobalVar;
    procedure LoadUnload_SingleDll_VerifyExceptionHandlingInsideDll;

    procedure LoadUnload_SameDllTwice_VerifySettingGlobalVar;
    procedure LoadUnload_SingleDll_VerifyExceptionHandlingInsideSameDllTwice;
    procedure SeparateLoadingAndUnloading_TwoDlls_ThenLoadingAndUnloadingFirst;

    procedure SeparateLoadingAndUnloading_TwoDlls_ThenLoadingAndUnloadingFirst_DistAndBrokerParams;
    procedure LoadSleepFromKernel32;

    procedure AfterAll_AlwaysExecute;
  end;


{ToDo:
- see Dist verification of loading the dll the second time and having the same global var values
}


implementation


uses
  testregistry;


const
  {$IFnDEF CPU64}
    CLibDirByBitness = 'i386-win32';
  {$ELSE}
    CLibDirByBitness = 'x86_64-win64';
  {$ENDIF}


type
  TSetAGlobalVarStdcall = procedure(AInt: Integer); stdcall;
  TSetAGlobalVarCdecl = procedure(AInt: Integer); cdecl;
  TGetAGlobalVar = function: Integer; stdcall;
  THandleAnException = procedure(ADummyValue: Integer); cdecl;

  TSetTwoGlobalVarsStdcall = procedure(AIntA, AIntB: Integer); stdcall;
  TSetTwoGlobalVarsCdecl = procedure(AIntA, AIntB: Integer); cdecl;
  TGetAGlobalVarFromTwo = function(AIndex: Byte): Integer; stdcall;
  THandleAnotherException = procedure(ADummyValue: Integer); cdecl;


  TOnActionPlugin_UpdatePropertyIcons = procedure(APluginReference: Pointer; AStreamContent: Pointer; AStreamSize: Int64); cdecl;
  //Two UIClicker plugin functions
  TGetAPIVersion = function: DWord; cdecl;
  TGetListOfProperties = procedure(APluginReference: Pointer;                 //UIClicker passes the plugin reference to the plugin, then the plugin calls some callbacks with that reference
                                   AListOfProperties: Pointer;
                                   AListOfPropertiesLen: PDWord;

                                   AOnActionPlugin_UpdatePropertyIcons: TOnActionPlugin_UpdatePropertyIcons); cdecl;

var
  SetAGlobalVarStdcall: TSetAGlobalVarStdcall;
  SetAGlobalVarCdecl: TSetAGlobalVarCdecl;
  GetAGlobalVar: TGetAGlobalVar;
  HandleAnException: THandleAnException;

  SetAGlobalVarStdcall2: TSetAGlobalVarStdcall;
  SetAGlobalVarCdecl2: TSetAGlobalVarCdecl;
  GetAGlobalVar2: TGetAGlobalVar;
  HandleAnException2: THandleAnException;

  SetTwoGlobalVarsStdcall: TSetTwoGlobalVarsStdcall;
  SetTwoGlobalVarsCdecl: TSetTwoGlobalVarsCdecl;
  GetAGlobalVarFromTwo: TGetAGlobalVarFromTwo;
  HandleAnotherException: THandleAnotherException;

  GetAPIVersion_Dist: TGetAPIVersion;
  GetListOfProperties_Dist: TGetListOfProperties;

  GetAPIVersion_BrokerParams: TGetAPIVersion;
  GetListOfProperties_BrokerParams: TGetListOfProperties;


constructor TTestLoader.Create;
begin
  inherited Create;
end;


procedure TTestLoader.SetAddrOfFirstProcs;
begin
  @SetAGlobalVarStdcall := FFirstLoader.GetProcAddress('SetAGlobalVarStdcall');
  @SetAGlobalVarCdecl := FFirstLoader.GetProcAddress('SetAGlobalVarCdecl');
  @GetAGlobalVar := FFirstLoader.GetProcAddress('GetAGlobalVar');
  @HandleAnException := FFirstLoader.GetProcAddress('HandleAnException');
end;


procedure TTestLoader.SetAddrOfFirst2Procs;
begin
  @SetAGlobalVarStdcall2 := FFirstLoader2.GetProcAddress('SetAGlobalVarStdcall');
  @SetAGlobalVarCdecl2 := FFirstLoader2.GetProcAddress('SetAGlobalVarCdecl');
  @GetAGlobalVar2 := FFirstLoader2.GetProcAddress('GetAGlobalVar');
  @HandleAnException2 := FFirstLoader2.GetProcAddress('HandleAnException');
end;


procedure TTestLoader.SetAddrOfSecondProcs;
begin
  @SetTwoGlobalVarsStdcall := FSecondLoader.GetProcAddress('SetTwoGlobalVarsStdcall');
  @SetTwoGlobalVarsCdecl := FSecondLoader.GetProcAddress('SetTwoGlobalVarsCdecl');
  @GetAGlobalVarFromTwo := FSecondLoader.GetProcAddress('GetAGlobalVarFromTwo');
  @HandleAnotherException := FSecondLoader.GetProcAddress('HandleAnotherException');
end;


procedure TTestLoader.SetAddrOfDistProcs;
begin
  @GetAPIVersion_Dist := FDistPluginLoader.GetProcAddress('GetAPIVersion');
  @GetListOfProperties_Dist := FDistPluginLoader.GetProcAddress('GetListOfProperties');
end;


procedure TTestLoader.SetAddrOfBrokerParamsProcs;
begin
  @GetAPIVersion_BrokerParams := FBrokerParamsPluginLoader.GetProcAddress('GetAPIVersion');
  @GetListOfProperties_BrokerParams := FBrokerParamsPluginLoader.GetProcAddress('GetListOfProperties');
end;


function TTestLoader.LoadDll(APath: string; AAttachLibraryOnLoad: Boolean = True): TDynLibMemLoader;
var
  Content: TMemoryStream;
begin
  Result := nil;
  Expect(FileExists(APath)).ToBe(True, 'The dll must exist at ' + APath);

  Result := TDynLibMemLoader.Create;
  Content := TMemoryStream.Create;
  try
    Content.LoadFromFile(APath);
    Expect(Result.LoadLibrary(Content.Memory, AAttachLibraryOnLoad)).ToBe(True, 'The dll must be loaded.');
  finally
    Content.Free;
  end;
end;


procedure TTestLoader.UnloadDll(var ALoader: TDynLibMemLoader);
begin
  Expect(Integer(ALoader)).NotToBe(0, 'The loader was not successfully created.');

  ALoader.FreeLibrary;
  FreeAndNil(ALoader);
end;


procedure TTestLoader.SetUp;
begin
  inherited SetUp;
end;


procedure TTestLoader.TearDown;
begin
  inherited TearDown;
end;


procedure TTestLoader.BeforeAll_AlwaysExecute;
begin
  //
end;


procedure TTestLoader.LoadUnload_SingleDll_WithoutCallingFunctions;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  UnloadDll(FFirstLoader);
end;


procedure TTestLoader.LoadUnload_TwoDll_WithoutCallingFunctions;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  FSecondLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\Second\lib\' + CLibDirByBitness + '\Second.dll');

  UnloadDll(FSecondLoader);
  UnloadDll(FFirstLoader);
end;


procedure TTestLoader.LoadUnload_SingleDll_GetAllFunctions;
var
  Functions: TExportedFunctionArr;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    FFirstLoader.ListExportedFunctions(Functions);
    Expect(Integer(Length(Functions))).ToBe(4);
    Expect(Functions[0].Name).ToBe('GetAGlobalVar');
    Expect(Functions[1].Name).ToBe('HandleAnException');
    Expect(Functions[2].Name).ToBe('SetAGlobalVarCdecl');
    Expect(Functions[3].Name).ToBe('SetAGlobalVarStdcall');
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifyAllFunctionAddresses;
var
  Functions: TExportedFunctionArr;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    FFirstLoader.ListExportedFunctions(Functions);
    Expect(Integer(Length(Functions))).ToBe(4);

    SetAddrOfFirstProcs;
    Expect(Functions[0].Address).ToBe(@GetAGlobalVar);
    Expect(Functions[1].Address).ToBe(@HandleAnException);
    Expect(Functions[2].Address).ToBe(@SetAGlobalVarCdecl);
    Expect(Functions[3].Address).ToBe(@SetAGlobalVarStdcall);
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifyDefaultGlobalVar_NoMain;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll', False);
  try
    SetAddrOfFirstProcs;

    Expect(GetAGlobalVar).ToBe(0);  //The main function of the dll is not executed, so the variable is not initialized by code. It is initialized by the compiler to 0.
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifyDefaultGlobalVar;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;

    Expect(GetAGlobalVar).ToBe(1234567890);
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifySettingGlobalVar;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;

    SetAGlobalVarStdcall(321);
    Expect(GetAGlobalVar).ToBe(321);

    SetAGlobalVarCdecl(765);
    Expect(GetAGlobalVar).ToBe(765);
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifyExceptionHandlingInsideDll;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;

    HandleAnException(200);
    Expect(GetAGlobalVar).ToBe(-1);
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure TTestLoader.LoadUnload_SameDllTwice_VerifySettingGlobalVar;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  FFirstLoader2 := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;
    SetAddrOfFirst2Procs;

    SetAGlobalVarStdcall(321);
    Expect(GetAGlobalVar).ToBe(321);
    Expect(GetAGlobalVar2).ToBe(1234567890); //the second instance shouldn't be affected

    SetAGlobalVarCdecl(765);
    Expect(GetAGlobalVar).ToBe(765);
    Expect(GetAGlobalVar2).ToBe(1234567890); //the second instance shouldn't be affected

    SetAGlobalVarStdcall2(9876);
    Expect(GetAGlobalVar).ToBe(765);   //the first instance shouldn't be affected
    Expect(GetAGlobalVar2).ToBe(9876);

    SetAGlobalVarCdecl2(6543);
    Expect(GetAGlobalVar).ToBe(765);   //the first instance shouldn't be affected
    Expect(GetAGlobalVar2).ToBe(6543);
  finally
    UnloadDll(FFirstLoader);
    UnloadDll(FFirstLoader2);
  end;
end;


procedure TTestLoader.LoadUnload_SingleDll_VerifyExceptionHandlingInsideSameDllTwice;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  FFirstLoader2 := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;
    SetAddrOfFirst2Procs;

    HandleAnException(200);
    Expect(GetAGlobalVar).ToBe(-1);
    Expect(GetAGlobalVar2).ToBe(1234567890); //the second instance shouldn't be affected

    HandleAnException2(300);
    Expect(GetAGlobalVar2).ToBe(-1);
  finally
    UnloadDll(FFirstLoader);
    UnloadDll(FFirstLoader2);
  end;
end;


procedure TTestLoader.SeparateLoadingAndUnloading_TwoDlls_ThenLoadingAndUnloadingFirst;
begin
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;
    Expect(GetAGlobalVar).ToBe(1234567890);
    SetAGlobalVarStdcall(568);
    Expect(GetAGlobalVar).ToBe(568);
  finally
    UnloadDll(FFirstLoader);
  end;

  FSecondLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\Second\lib\' + CLibDirByBitness + '\Second.dll');
  try
    SetAddrOfSecondProcs;
    Expect(GetAGlobalVarFromTwo(0)).ToBe(1234567898);
    Expect(GetAGlobalVarFromTwo(1)).ToBe(1234567897);
    Expect(GetAGlobalVarFromTwo(2)).ToBe(-1);

    SetTwoGlobalVarsStdcall(580, 590);
    Expect(GetAGlobalVarFromTwo(0)).ToBe(580);
    Expect(GetAGlobalVarFromTwo(1)).ToBe(590);
    Expect(GetAGlobalVarFromTwo(2)).ToBe(-1);
    SetTwoGlobalVarsCdecl(583, 593);
    Expect(GetAGlobalVarFromTwo(0)).ToBe(583);
    Expect(GetAGlobalVarFromTwo(1)).ToBe(593);
    Expect(GetAGlobalVarFromTwo(2)).ToBe(-1);

    HandleAnotherException(8);
    Expect(GetAGlobalVarFromTwo(0)).ToBe(-1);
    Expect(GetAGlobalVarFromTwo(1)).ToBe(-2);
    Expect(GetAGlobalVarFromTwo(2)).ToBe(-1);
  finally
    UnloadDll(FSecondLoader);
  end;

  //Second loading of the first dll, after loading and unloading another one.
  FFirstLoader := LoadDll(ExtractFilePath(ParamStr(0)) + 'TestFiles\First\lib\' + CLibDirByBitness + '\First.dll');
  try
    SetAddrOfFirstProcs;
    Expect(GetAGlobalVar).ToBe(1234567890);
    SetAGlobalVarStdcall(321);
    Expect(GetAGlobalVar).ToBe(321);
  finally
    UnloadDll(FFirstLoader);
  end;
end;


procedure DoOnActionPlugin_UpdatePropertyIcons(APluginReference: Pointer; AStreamContent: Pointer; AStreamSize: Int64); cdecl;
begin
  Expect(Integer(APluginReference)).ToBeGreaterThan(0);
  Expect(Integer(AStreamContent)).ToBeGreaterThan(0);
  Expect(Integer(AStreamSize)).ToBeGreaterThan(0);
end;


procedure TTestLoader.SeparateLoadingAndUnloading_TwoDlls_ThenLoadingAndUnloadingFirst_DistAndBrokerParams;
const
  CMaxSharedStringLength = 10 * 1048576;
var
  DistProperties, BrokerParamsProperties: string;
  Len: DWord;
begin
  FDistPluginLoader := LoadDll(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\lib\' + CLibDirByBitness + '\UIClickerDistFindSubControl.dll');
  try
    SetAddrOfDistProcs;
    Expect(GetAPIVersion_Dist).ToBe(10);

    SetLength(DistProperties, CMaxSharedStringLength);
    GetListOfProperties_Dist(@Self, @DistProperties[1], @Len, @DoOnActionPlugin_UpdatePropertyIcons);
    SetLength(DistProperties, Len);
    Expect(Copy(DistProperties, 1, 14)).ToBe('FindSubControl');
  finally
    UnloadDll(FDistPluginLoader);
  end;

  FBrokerParamsPluginLoader := LoadDll(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\BrokerParams\lib\' + CLibDirByBitness + '\BrokerParams.dll');
  try
    SetAddrOfBrokerParamsProcs;
    Expect(GetAPIVersion_BrokerParams).ToBe(10);

    SetLength(BrokerParamsProperties, CMaxSharedStringLength);
    GetListOfProperties_BrokerParams(@Self, @BrokerParamsProperties[1], @Len, @DoOnActionPlugin_UpdatePropertyIcons);
    SetLength(BrokerParamsProperties, Len);
    Expect(Copy(BrokerParamsProperties, 1, 9)).ToBe('Reserved=');
  finally
    UnloadDll(FBrokerParamsPluginLoader);
  end;

  //Second loading of the first dll, after loading and unloading another one.
  FDistPluginLoader := LoadDll(ExtractFilePath(ParamStr(0)) + '..\..\..\UIClickerDistFindSubControlPlugin\lib\' + CLibDirByBitness + '\UIClickerDistFindSubControl.dll');
  try
    SetAddrOfDistProcs;
    Expect(GetAPIVersion_Dist).ToBe(10);

    SetLength(DistProperties, CMaxSharedStringLength);
    GetListOfProperties_Dist(@Self, @DistProperties[1], @Len, @DoOnActionPlugin_UpdatePropertyIcons);
    SetLength(DistProperties, Len);
    Expect(Copy(DistProperties, 1, 14)).ToBe('FindSubControl');
  finally
    UnloadDll(FDistPluginLoader);
  end;
end;


procedure TTestLoader.LoadSleepFromKernel32;
type
  TSleep = procedure(dWMilliseconds: DWord); stdcall;
var
  tk: UInt64;
  SleepProc: TSleep;
  Loader: TDynLibMemLoader;
begin
  Loader := LoadDll('C:\Windows\System32\' + KernelDLL + '.dll'); //assume 'C:\Windows\System32\'
  try
    @SleepProc := Loader.GetProcAddress('Sleep', True);

    tk := GetTickCount64;
    SleepProc(100);
    Expect(Integer(GetTickCount64 - tk)).ToBeGreaterThan(100);
  finally
    UnloadDll(Loader);
  end;
end;


procedure TTestLoader.AfterAll_AlwaysExecute;
begin
  //
end;


initialization
  RegisterTest(TTestLoader);

end.

