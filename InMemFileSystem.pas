{
    Copyright (C) 2024 VCC
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

unit InMemFileSystem;

{$IFDEF FPC}
  //{$MODE Delphi}
{$ENDIF}


{
This is a simple file system, for when updating every change to disk, is expensive, e.g. heavy logging or heavy creating/deleting files (like in testing).
It has no tree structure (i.e. no directories), so it is expected to handle only a few files.
Filenames can contain full paths, so they can be uniquely identified.
Files can have additional user-defined info attached to them.
}

interface

uses
  Windows, SysUtils, Classes;

const
  CDefaultInMemFileNameHashSeparator = #1#4;

type
  TFileLocation = (flDisk, flMem, flDiskThenMem, flMemThenDisk);

  TOnComputeInMemFileHash = function(AFileContent: Pointer; AFileSize: Int64): string of object;

  TMemFile = record
    Name: string;
    Size: Int64;
    Content: Pointer;
    Hash: string; //Must be manually updated if used. See OnComputeInMemFileHash event. This is useful to verify if a file has to be updated.
    AdditionalFileInfo: Pointer;  //user-defined file annotations (can be a comment, a timestamp, or other file-specific info)
  end;

  TMemFileArr = array of TMemFile;    //Plain array of files, no tree, no folders. Linear access.

  TInMemFileSystem = class(TObject)
  private
    FListOfFiles: TMemFileArr;
    FSystemCriticalSection: TRTLCriticalSection;
    FFileNameHashSeparator: string;
    FOnComputeInMemFileHash: TOnComputeInMemFileHash;

    function GetTotalFileSize: Int64;
    function GetTotalFileCount: Integer;

    function GetFileIndexByName(AFileName: string): Integer;
    procedure UpdateFileContentByIndexToInMemDisk(AIndex: Integer; NewContent: Pointer; ASize: Int64);
    procedure AddFileByName(AFileName: string; AContent: Pointer; ASize: Int64);
    procedure DeleteFileFromMemByIndex(AFileIndex: Integer);

    function DoOnComputeInMemFileHash(AFileContent: Pointer; AFileSize: Int64): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveFileToMem(AName: string; AContent: Pointer; ASize: Int64);
    procedure LoadFileFromMem(AName: string; AContent: Pointer; AvailableIndex: Integer = -1); //size should be queried in advance, using GetFileSize. If the destination is a TMemoryStream, it should preallocate memory before calling LoadFileFromMem.
    procedure LoadFileFromMemToStream(AName: string; AMemStream: TMemoryStream);  //SetSize is called internally if the new size does not match.
    function GetFileSize(AName: string): Int64;
    procedure ListMemFiles(var AList: TMemFileArr); overload;
    procedure ListMemFiles(AStringList: TStringList); overload;
    function ListMemFilesAsString: string;
    function ListMemFilesWithHashAsString: string;
    procedure UpdateListOfMemFiles(FileNames: TStringList); //creates in-mem files if the provided names are not found
    function FileExistsInMem(AFileName: string): Boolean;
    function FileExistsInMemWithHash(AFileName, AHash: string): Boolean; overload;
    function FileExistsInMemWithHash(AFileNameWithHash: string): Boolean; overload;
    procedure DeleteFileFromMem(AFileName: string);
    function DuplicateFile(AFileName: string): string;   //returns new name
    procedure RenameFile(AFileName, NewFileName: string);
    procedure AttachAdditionalInfoToFile(AFileName: string; AFileInfo: Pointer);
    function GetAdditionalFileInfo(AFileName: string): Pointer;
    function GetHashByFileName(AFileName: string): string;
    procedure Clear;

    property FileNameHashSeparator: string read FFileNameHashSeparator write FFileNameHashSeparator;
    property TotalFileSize: Int64 read GetTotalFileSize;
    property TotalFileCount: Integer read GetTotalFileCount;

    property OnComputeInMemFileHash: TOnComputeInMemFileHash read FOnComputeInMemFileHash write FOnComputeInMemFileHash;
  end;


function FileExistsInDiskOrMem(AFileName: string; AInMemFileSystem: TInMemFileSystem): Boolean;
function FileExistsInDiskOrMemWithPriority(AFileName: string; AInMemFileSystem: TInMemFileSystem = nil; AFileLocation: TFileLocation = flDisk): Boolean;
procedure TMemFileArrToStringList(var AListOfMemFiles: TMemFileArr; AListOfFileNames: TStringList);
procedure ExtractNonExistentFiles(AInitialListOfFiles, ANonExistentFiles: TStringList; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);


const
  CFileLocationStr: array[TFileLocation] of string = ('flDisk', 'flMem', 'flDiskThenMem', 'flMemThenDisk');

implementation


function FileExistsInDiskOrMem(AFileName: string; AInMemFileSystem: TInMemFileSystem): Boolean;
begin
  Result := AInMemFileSystem.FileExistsInMem(AFileName) or FileExists(AFileName);
end;


function FileExistsInDiskOrMemWithPriority(AFileName: string; AInMemFileSystem: TInMemFileSystem = nil; AFileLocation: TFileLocation = flDisk): Boolean;
const
  CFileSystemMustExist = 'In-mem file system does not exist.';
begin
  case AFileLocation of
    flDisk:
      Result := FileExists(AFileName);

    flMem:
    begin
      if AInMemFileSystem = nil then
        raise Exception.Create(CFileSystemMustExist);

      Result := AInMemFileSystem.FileExistsInMem(AFileName);
    end;

    flDiskThenMem:
    begin
      Result := FileExistsInDiskOrMemWithPriority(AFileName, AInMemFileSystem, flDisk);
      if not Result then
        Result := FileExistsInDiskOrMemWithPriority(AFileName, AInMemFileSystem, flMem);
    end;

    flMemThenDisk:
    begin
      Result := FileExistsInDiskOrMemWithPriority(AFileName, AInMemFileSystem, flMem);
      if not Result then
        Result := FileExistsInDiskOrMemWithPriority(AFileName, AInMemFileSystem, flDisk);
    end;
  end;
end;


procedure TMemFileArrToStringList(var AListOfMemFiles: TMemFileArr; AListOfFileNames: TStringList);
var
  i: Integer;
begin
  for i := 0 to Length(AListOfMemFiles) - 1 do
    AListOfFileNames.Add(AListOfMemFiles[i].Name);
end;


procedure ExtractNonExistentFiles(AInitialListOfFiles, ANonExistentFiles: TStringList; AFileLocation: TFileLocation = flDisk; AInMemFileSystem: TInMemFileSystem = nil);
var
  i: Integer;
begin
  for i := 0 to AInitialListOfFiles.Count - 1 do
    if not FileExistsInDiskOrMemWithPriority(AInitialListOfFiles.Strings[i], AInMemFileSystem, AFileLocation) then
      ANonExistentFiles.Add(AInitialListOfFiles.Strings[i]);
end;


constructor TInMemFileSystem.Create;
begin
  inherited Create;
  SetLength(FListOfFiles, 0);
  InitializeCriticalSection(FSystemCriticalSection);

  FFileNameHashSeparator := CDefaultInMemFileNameHashSeparator;
  FOnComputeInMemFileHash := nil;
end;


destructor TInMemFileSystem.Destroy;
begin
  try
    Clear;
  finally
    DeleteCriticalSection(FSystemCriticalSection);
  end;
  
  inherited Destroy;
end;


function TInMemFileSystem.GetFileIndexByName(AFileName: string): Integer;
var
  i: Integer;
  UpperCaseFileName: string;
begin
  UpperCaseFileName := UpperCase(AFileName);
  Result := -1;

  EnterCriticalSection(FSystemCriticalSection);
  try
    for i := 0 to Length(FListOfFiles) - 1 do
      if UpperCase(FListOfFiles[i].Name) = UpperCaseFileName then
      begin
        Result := i;
        Break;
      end;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.UpdateFileContentByIndexToInMemDisk(AIndex: Integer; NewContent: Pointer; ASize: Int64);
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    if (AIndex < 0) or (AIndex > Length(FListOfFiles) - 1) then
      raise Exception.Create('Index out of bounds when updating in-mem file.  FileIndex: ' + IntToStr(AIndex) + '  FileCount: ' + IntToStr(Length(FListOfFiles)));

    if FListOfFiles[AIndex].Size <> ASize then
    begin
      if FListOfFiles[AIndex].Content <> nil then
        FreeMem(FListOfFiles[AIndex].Content, FListOfFiles[AIndex].Size);

      try
        GetMem(FListOfFiles[AIndex].Content, ASize);                      //ReallocMem ???
      except
        on E: EOutOfMemory do
          raise Exception.Create('Out of in-mem disk space.');
      end;

      FListOfFiles[AIndex].Size := ASize;
    end;

    Move(NewContent^, FListOfFiles[AIndex].Content^, ASize);
    FListOfFiles[AIndex].Hash := DoOnComputeInMemFileHash(FListOfFiles[AIndex].Content, FListOfFiles[AIndex].Size);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.AddFileByName(AFileName: string; AContent: Pointer; ASize: Int64);
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    SetLength(FListOfFiles, Length(FListOfFiles) + 1);
    FListOfFiles[Length(FListOfFiles) - 1].Name := AFileName;
    FListOfFiles[Length(FListOfFiles) - 1].Size := 0;
    FListOfFiles[Length(FListOfFiles) - 1].AdditionalFileInfo := nil;
    UpdateFileContentByIndexToInMemDisk(Length(FListOfFiles) - 1, AContent, ASize);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.SaveFileToMem(AName: string; AContent: Pointer; ASize: Int64);
var
  FileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);   //required, because FileIndex should stay valid for the UpdateFileContentByIndexToInMemDisk call
  try
    FileIndex := GetFileIndexByName(AName);

    if FileIndex = -1 then
      AddFileByName(AName, AContent, ASize)
    else
      UpdateFileContentByIndexToInMemDisk(FileIndex, AContent, ASize);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.LoadFileFromMem(AName: string; AContent: Pointer; AvailableIndex: Integer = -1);
var
  FileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);   //required, because FileIndex should stay valid for the Move call
  try
    if AvailableIndex > -1 then
      FileIndex := AvailableIndex
    else
      FileIndex := GetFileIndexByName(AName);

    if (FileIndex < 0) or (FileIndex > Length(FListOfFiles) - 1) then
      raise Exception.Create('Requested file can''t be found by name: ' + AName);

    Move(FListOfFiles[FileIndex].Content^, AContent^, FListOfFiles[FileIndex].Size);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.LoadFileFromMemToStream(AName: string; AMemStream: TMemoryStream);    //SetSize is called internally if the new size does not match.
var
  CurrentFileSize: Int64;
  TempPointer: Pointer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    CurrentFileSize := GetFileSize(AName);
    if AMemStream.Size <> CurrentFileSize then
      AMemStream.SetSize(CurrentFileSize);

    TempPointer := AMemStream.Memory;
    LoadFileFromMem(AName, TempPointer);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.ListMemFiles(var AList: TMemFileArr); //overload;
var
  i: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    SetLength(AList, Length(FListOfFiles));
    for i := 0 to Length(FListOfFiles) - 1 do
      AList[i] := FListOfFiles[i];
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.ListMemFiles(AStringList: TStringList); //overload;
var
  i: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AStringList.Clear;
    for i := 0 to Length(FListOfFiles) - 1 do
      AStringList.Add(FListOfFiles[i].Name);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.ListMemFilesAsString: string;
var
  i: Integer;
begin
  Result := '';
  EnterCriticalSection(FSystemCriticalSection);
  try
    for i := 0 to Length(FListOfFiles) - 1 do
      Result := Result + FListOfFiles[i].Name + #13#10;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.ListMemFilesWithHashAsString: string;
var
  i: Integer;
begin
  Result := '';
  EnterCriticalSection(FSystemCriticalSection);
  try
    for i := 0 to Length(FListOfFiles) - 1 do
      Result := Result + FListOfFiles[i].Name + FFileNameHashSeparator + FListOfFiles[i].Hash + #13#10;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.GetFileSize(AName: string): Int64;
var
  FileIndex: Integer;
begin
  Result := -1;
  EnterCriticalSection(FSystemCriticalSection);
  try
    FileIndex := GetFileIndexByName(AName);
    if FileIndex = -1 then
      raise Exception.Create('Requested file can''t be found by name: ' + AName);

    Result := FListOfFiles[FileIndex].Size;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.UpdateListOfMemFiles(FileNames: TStringList); //creates in-mem files
var
  i: Integer;
  TempPointer: Pointer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    for i := 0 to FileNames.Count - 1 do
      if GetFileIndexByName(FileNames.Strings[i]) = -1 then
      begin
        GetMem(TempPointer, 0);  //maybe not needed, then call  AddFileByName with nil.
        AddFileByName(FileNames.Strings[i], TempPointer, 0);
      end;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.FileExistsInMem(AFileName: string): Boolean;
begin
  Result := GetFileIndexByName(AFileName) > -1;
end;



function TInMemFileSystem.FileExistsInMemWithHash(AFileName, AHash: string): Boolean; overload;
var
  FileIndex: Integer;
begin
  if AHash = '' then
  begin
    Result := False;
    Exit;
  end;

  EnterCriticalSection(FSystemCriticalSection);
  try
    FileIndex := GetFileIndexByName(AFileName);
    Result := (FileIndex > -1) and (FListOfFiles[FileIndex].Hash = AHash);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.FileExistsInMemWithHash(AFileNameWithHash: string): Boolean; overload;
var
  Fnm, Hash: string;
begin
  Fnm := Copy(AFileNameWithHash, 1, Pos(FFileNameHashSeparator, AFileNameWithHash) - 1);
  Hash := Copy(AFileNameWithHash, Pos(FFileNameHashSeparator, AFileNameWithHash) + Length(FFileNameHashSeparator), MaxInt);
  Result := FileExistsInMemWithHash(Fnm, Hash);
end;


procedure TInMemFileSystem.DeleteFileFromMemByIndex(AFileIndex: Integer);
var
  i: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    FreeMem(FListOfFiles[AFileIndex].Content, FListOfFiles[AFileIndex].Size);

    for i := AFileIndex to Length(FListOfFiles) - 2 do
      FListOfFiles[i] := FListOfFiles[i + 1];

    SetLength(FListOfFiles, Length(FListOfFiles) - 1);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.DeleteFileFromMem(AFileName: string);
var
  AFileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AFileIndex := GetFileIndexByName(AFileName);
    if AFileIndex > -1 then
    begin
      try
        DeleteFileFromMemByIndex(AFileIndex);
      except
        on E: Exception do
          raise Exception.Create('Can''t properly delete file. "' + AFileName + '"' + #13#10 + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.DuplicateFile(AFileName: string): string;      //additional info has to be manually duplicated externally, because this class is not content-aware
var
  AMemStream: TMemoryStream;
  NewName, FileNameNoExt: string;
  AFileExt: string;
  CopyNumber: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AMemStream := TMemoryStream.Create;
    try
      AFileExt := ExtractFileExt(AFileName);
      FileNameNoExt := Copy(AFileName, 1, Length(AFileName) - Length(AFileExt));
      NewName := FileNameNoExt + ' - Copy' + AFileExt;

      CopyNumber := 1;
      while FileExistsInMem(NewName) do
      begin
        NewName := FileNameNoExt + ' - Copy ' + IntToStr(CopyNumber) + AFileExt;
        Inc(CopyNumber);
      end;

      LoadFileFromMemToStream(AFileName, AMemStream);
      SaveFileToMem(NewName, AMemStream.Memory, AMemStream.Size);

      Result := NewName;
    finally
      AMemStream.Free;
    end;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.RenameFile(AFileName, NewFileName: string);
var
  AFileIndex: Integer;
begin
  if AFileName = NewFileName then
    Exit;

  EnterCriticalSection(FSystemCriticalSection);
  try
    AFileIndex := GetFileIndexByName(NewFileName);
    if AFileIndex > -1 then
      raise Exception.Create('New file already exists on renaming. "' + AFileName + '"');

    AFileIndex := GetFileIndexByName(AFileName);
    if AFileIndex > -1 then
    begin
      try
        FListOfFiles[AFileIndex].Name := NewFileName;
      except
        on E: Exception do
          raise Exception.Create('Can''t properly rename file. "' + AFileName + '"' + #13#10 + E.Message);
      end;
    end
    else
      raise Exception.Create('Can''t find file to rename. "' + AFileName + '"');
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.AttachAdditionalInfoToFile(AFileName: string; AFileInfo: Pointer);
var
  AFileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AFileIndex := GetFileIndexByName(AFileName);
    if AFileIndex = -1 then
      raise Exception.Create('File not found on adding info. "' + AFileName + '"');

    FListOfFiles[AFileIndex].AdditionalFileInfo := AFileInfo;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.GetAdditionalFileInfo(AFileName: string): Pointer;
var
  AFileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AFileIndex := GetFileIndexByName(AFileName);
    if AFileIndex = -1 then
      raise Exception.Create('File not found on getting info. "' + AFileName + '"');

    Result := FListOfFiles[AFileIndex].AdditionalFileInfo;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.DoOnComputeInMemFileHash(AFileContent: Pointer; AFileSize: Int64): string;
begin
  if Assigned(FOnComputeInMemFileHash) then
    Result := FOnComputeInMemFileHash(AFileContent, AFileSize)
  else
    Result := '';
end;


function TInMemFileSystem.GetHashByFileName(AFileName: string): string;
var
  AFileIndex: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    AFileIndex := GetFileIndexByName(AFileName);
    if AFileIndex = -1 then
      Result := ''
    else
      Result := FListOfFiles[AFileIndex].Hash;
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


procedure TInMemFileSystem.Clear;
var
  i: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    for i := 0 to Length(FListOfFiles) - 1 do
      FreeMem(FListOfFiles[i].Content, FListOfFiles[i].Size);

    SetLength(FListOfFiles, 0);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.GetTotalFileSize: Int64;
var
  i: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    Result := 0;
    for i := 0 to Length(FListOfFiles) - 1 do
      Inc(Result, FListOfFiles[i].Size);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;


function TInMemFileSystem.GetTotalFileCount: Integer;
begin
  EnterCriticalSection(FSystemCriticalSection);
  try
    Result := Length(FListOfFiles);
  finally
    LeaveCriticalSection(FSystemCriticalSection);
  end;
end;

end.
