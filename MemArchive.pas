{
    Copyright (C) 2026 VCC
    creation date: 18 May 2024
    initial release date: 19 May 2024

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


unit MemArchive;

{$IFDEF FPC}
  {$mode ObjFPC}{$H+}
  {$WARN 4056 off : Conversion between ordinals and pointers is not portable}
{$ENDIF}
interface

uses
  Classes, SysUtils;

//encrypted archive starts with:
//4  bytes - padding size (for proper encryption)
//32 bytes - hash of decrypted archive - used for checking proper decryption (either before decompression, or parsing)

//structure of uncompressed archive  (plain data)
//header
//64 bytes - reserved for 2x32 fields
//8  bytes - total files size (size of next block)  - concatenated files    may not be updated for now
//8  bytes - reserved - 0s for now

//start of data
//4 bytes - length(filename1)
//# bytes - filename1 (can be ANSI or Unicode,  does not matter)
//8 bytes - file1 size
//# bytes - file1 content
//4 bytes - length(filename2)
//# bytes - filename2 (can be ANSI or Unicode,  does not matter)
//8 bytes - file2 size
//# bytes - file2 content
//...
//------------------------------------------


const
  CHashSize = 32;

type
  TArr32OfByte = array[0..CHashSize - 1] of Byte;   //has to match CHashSize
  TOnEncryptArchive = procedure(AArchiveStream: TMemoryStream) of object;
  TOnDecryptArchive = procedure(AArchiveStream: TMemoryStream) of object;
  TOnGetKeyFromPassword = procedure(APassword: string; var ArcKey: TArr32OfByte) of object;
  TOnComputeArchiveHash = procedure(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '') of object;
  TOnInitEncryption = procedure(var AArcKey: TArr32OfByte) of object;
  TOnCompress = procedure(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer) of object;  //compresses APlainStream, results AArchiveStream
  TOnDecompress = function(AArchiveStream, APlainStream: TMemoryStream): Boolean of object;                    //decompresses AArchiveStream, results APlainStream
  TOnEncryptionCleanup = procedure of object;


  TMemArchive = class(TObject)
  private
    FPlainStream: TMemoryStream;
    FArchiveStream: TMemoryStream;
    FPassword: string;
    FCompressionLevel: Integer;
    FShouldUpdateArchiveOnClose: Boolean;

    FKey: TArr32OfByte; //use it from here, to avoid computing it twice per archive (on open and close)

    FMaxExpectedFileCount: Integer;
    FMaxExpectedTotalFileSize: Int64;
    FMaxExpectedFilenameLength: Integer;

    FOnEncryptArchive: TOnEncryptArchive;
    FOnDecryptArchive: TOnDecryptArchive;
    FOnGetKeyFromPassword: TOnGetKeyFromPassword;
    FOnComputeArchiveHash: TOnComputeArchiveHash;
    FOnInitEncryption: TOnInitEncryption;
    FOnCompress: TOnCompress;
    FOnDecompress: TOnDecompress;
    FOnEncryptionCleanup: TOnEncryptionCleanup;

    procedure SetMaxExpectedFilenameLength(Value: Integer);

    procedure DoOnEncryptArchive(AArchiveStream: TMemoryStream);
    procedure DoOnDecryptArchive(AArchiveStream: TMemoryStream);
    procedure DoOnGetKeyFromPassword(APassword: string; var AArcKey: TArr32OfByte);
    procedure DoOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
    procedure DoOnInitEncryption(var AArcKey: TArr32OfByte);
    procedure DoOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
    function DoOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;
    procedure DoOnEncryptionCleanup;

    procedure EncryptArchive;
    procedure DecryptArchive;
    function GetPositionOfFileSizeInPlainStream(FileName: string; AInitPos: Int64 = -1): Int64; //points to the 8-byte file size
    procedure AddHeaderToEmptyArchive(AContent: TMemoryStream);
    procedure GetKeyFromPassword(var AArcKey: TArr32OfByte);
    procedure PurgeKey;
    procedure AddPadding;
    procedure CheckHashOfDecryptedArchive;
    function SetHashOfDecryptedArchive: string;
    procedure CheckMaxValues;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenArchive(AArchiveStream: TMemoryStream; CreateNewArchive: Boolean);
    function CloseArchive: string;

    procedure AddFromStream(AFileName: string; AContent: TMemoryStream; UseBuffer: Boolean = True); overload;
    procedure AddFromStream(AFileName: string; AContent: TMemoryStream; AContentCustomSize: Int64; UseBuffer: Boolean = True); overload; //use -1 for AContentCustomSize, to leave stream position as it is
    procedure AddFromString(AFileName: string; AContent: string);
    procedure ExtractToStream(AFileName: string; AContent: TMemoryStream);
    procedure ExtractToString(AFileName: string; var AContent: string);
    function FindFirst(AFileMask: string; var SearchState: Int64): Boolean;
    procedure GetListOfFiles(AListOfFiles: TStringList);

    property MaxExpectedFileCount: Integer read FMaxExpectedFileCount write FMaxExpectedFileCount; //When this is set to a positive value, an exception is raised when opening an archive with too many files or when adding too many files.
    property MaxExpectedTotalFileSize: Int64 read FMaxExpectedTotalFileSize write FMaxExpectedTotalFileSize; //sum of all file sizes. Does not include filenames.
    property MaxExpectedFilenameLength: Integer read FMaxExpectedFilenameLength write SetMaxExpectedFilenameLength;

    property Password: string read FPassword write FPassword; //Should be set only once, before calling OpenArchive (if archive encryption is desired). There is no support for changing archive password.
    property CompressionLevel: Integer read FCompressionLevel write FCompressionLevel; //Compression specific value.

    property OnEncryptArchive: TOnEncryptArchive write FOnEncryptArchive;              //Handler required only when using an archive password.
    property OnDecryptArchive: TOnDecryptArchive write FOnDecryptArchive;              //Handler required only when using an archive password.
    property OnGetKeyFromPassword: TOnGetKeyFromPassword write FOnGetKeyFromPassword;  //Handler required only when using an archive password. //A 32-byte key is supported only.
    property OnComputeArchiveHash: TOnComputeArchiveHash write FOnComputeArchiveHash;  //Handler required only when opening an existing archive. //A 32-byte hash is expected.
    property OnInitEncryption: TOnInitEncryption write FOnInitEncryption;              //Handler required only when using an archive password. //A 32-byte key is supported only.
    property OnCompress: TOnCompress write FOnCompress;                                //Handler required only when using compression (i.e. CompressionLevel > 0).  //compresses APlainStream, results AArchiveStream
    property OnDecompress: TOnDecompress write FOnDecompress;                          //Handler required only when using compression (i.e. CompressionLevel > 0).  //decompresses AArchiveStream, results APlainStream
    property OnEncryptionCleanup: TOnEncryptionCleanup write FOnEncryptionCleanup;     //Handler required only when using an archive password.
  end;


implementation


uses
  Math;


const
  CPaddingSizeField = 4;
  CPaddingAndHashSizeField = CPaddingSizeField + CHashSize;
  CReservedFieldsSizeField = 64;
  CTotalFileSizeField = 8; //may not be updated for now
  CReservedAfterTotalFileSizeField = 8;
  CHeaderSizeField = CReservedFieldsSizeField + CTotalFileSizeField + CReservedAfterTotalFileSizeField;

  CFilenameLengthSizeField = 4;
  CFileContentSizeField = 8;

  {$IFDEF CPUX64}
    CMaxCompressedFileSize = 16 * 1073741824; //16GB
    CMaxDecompressedFileSize = 32 * 1073741824; //32GB
  {$ELSE}
    CMaxCompressedFileSize = 1 * 1073741824; //1GB
    CMaxDecompressedFileSize = 2 * 1073741824; //2GB
  {$ENDIF}

  CPaddingTo = 32; //Verify any calculations, which does padding to 32 byte, if changing this value.  For example: "PaddingSize := CompressedSize - (CompressedSize shr 5) shl 5;"


constructor TMemArchive.Create;
begin
  FPassword := '';
  FPlainStream := TMemoryStream.Create;
  FCompressionLevel := 9;
  FShouldUpdateArchiveOnClose := False;
  FMaxExpectedFileCount := -1; //no limit by default
  FMaxExpectedTotalFileSize := -1; //no limit by default, although TMemoryStream might have some 2GB limitations
  FMaxExpectedFilenameLength := 1024;

  FOnEncryptArchive := nil;
  FOnDecryptArchive := nil;
  FOnGetKeyFromPassword := nil;
  FOnComputeArchiveHash := nil;
  FOnInitEncryption := nil;
  FOnCompress := nil;
  FOnDecompress := nil;
  FOnEncryptionCleanup := nil;
end;


destructor TMemArchive.Destroy;
begin
  FreeAndNil(FPlainStream);

  if FPassword > '' then
    FillChar(FPassword[1], Length(FPassword), Random(256));

  FPassword := '';
end;


procedure TMemArchive.SetMaxExpectedFilenameLength(Value: Integer);
begin
  FMaxExpectedFilenameLength := Min(Max(Value, 255), 8192);
end;


procedure TMemArchive.DoOnEncryptArchive(AArchiveStream: TMemoryStream);
var
  OldSize: Int64;
begin
  if Assigned(FOnEncryptArchive) then
  begin
    OldSize := FArchiveStream.Size;
    FOnEncryptArchive(FArchiveStream);

    if FArchiveStream.Size <> OldSize then
      raise Exception.Create('Archive size modified by OnEncryptArchive.');
  end
  else
    raise Exception.Create('OnEncryptArchive is not assigned.');
end;


procedure TMemArchive.DoOnDecryptArchive(AArchiveStream: TMemoryStream);
var
  OldSize: Int64;
begin
  if Assigned(FOnDecryptArchive) then
  begin
    OldSize := FArchiveStream.Size;
    FOnDecryptArchive(FArchiveStream);

    if FArchiveStream.Size <> OldSize then
      raise Exception.Create('Archive size modified by OnDecryptArchive.');
  end
  else
    raise Exception.Create('OnDecryptArchive is not assigned.');
end;


procedure TMemArchive.DoOnGetKeyFromPassword(APassword: string; var AArcKey: TArr32OfByte);
begin
  if Assigned(FOnGetKeyFromPassword) then
    FOnGetKeyFromPassword(APassword, AArcKey)
  else
    raise Exception.Create('OnGetKeyFromPassword is not assigned.');
end;


procedure TMemArchive.DoOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
begin
  if Assigned(FOnComputeArchiveHash) then
    FOnComputeArchiveHash(AArchiveStream, AArchiveStreamSize, AResultedHash, AAdditionalInfo)
  else
    raise Exception.Create('OnComputeArchiveHash is not assigned.');
end;


procedure TMemArchive.DoOnInitEncryption(var AArcKey: TArr32OfByte);
begin
  if Assigned(FOnInitEncryption) then
    FOnInitEncryption(AArcKey)
  else
    raise Exception.Create('OnInitEncryption is not assigned.');
end;


procedure TMemArchive.DoOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
begin
  if Assigned(FOnCompress) then
    FOnCompress(APlainStream, AArchiveStream, ACompressionLevel)
  else
    raise Exception.Create('OnCompress is not assigned.');
end;


function TMemArchive.DoOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;
begin
  if Assigned(FOnDecompress) then
  begin
    Result := FOnDecompress(AArchiveStream, APlainStream);
    if APlainStream.Size > CMaxDecompressedFileSize then
      raise Exception.Create('Decompressed size exceeded limit.');
  end
  else
    raise Exception.Create('OnDecompress is not assigned.');
end;


procedure TMemArchive.DoOnEncryptionCleanup;
begin
  if Assigned(FOnEncryptionCleanup) then
    FOnEncryptionCleanup()
  else
    ; //do nothing if not assigned
end;


procedure TMemArchive.EncryptArchive;
begin
  DoOnEncryptArchive(FArchiveStream);
end;


procedure TMemArchive.DecryptArchive;
begin
  DoOnDecryptArchive(FArchiveStream);
end;


function TMemArchive.GetPositionOfFileSizeInPlainStream(FileName: string; AInitPos: Int64 = -1): Int64; //points to the 8-byte file size
var
  //APosition: Int64;
  TempFileNameLen: Integer;
  InitPos: Int64;
  DecodedFileName: string;
  FileContentSize: Int64;
begin
  Result := -1; //file not found

  if FPlainStream.Size = 0 then
    Exit;

  if FPlainStream.Size <= CHeaderSizeField then
    Exit;

  //jump from file to file, until match
  if AInitPos = -1 then
    InitPos := FPlainStream.Position
  else
    InitPos := AInitPos;

  try
    FPlainStream.Position := CHeaderSizeField;  //start of files
    repeat
      //read filename string length
      if Int64(CFilenameLengthSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename size does not fit into the next section of the archive.');

      FPlainStream.Read(TempFileNameLen, CFilenameLengthSizeField);

      if (TempFileNameLen < 1) or (TempFileNameLen > FMaxExpectedFilenameLength) then
        raise Exception.Create('The filename is empty or too long.');

      if Int64(TempFileNameLen) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename does not fit into the next section of the archive.');

      //read string
      SetLength(DecodedFileName, TempFileNameLen);
      FPlainStream.Read(DecodedFileName[1], TempFileNameLen);

      //read file size
      if Int64(CFileContentSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filesize info does not fit into the next section of the archive.');

      FPlainStream.Read(FileContentSize, CFileContentSizeField);

      if FileContentSize > CMaxCompressedFileSize then
        raise Exception.Create('The filesize is greater than the allowed limit.');

      if (FileContentSize < 0) or (FPlainStream.Position + FileContentSize > FPlainStream.Size) then
        raise Exception.Create('The file content does not fit into the next section of the archive.');

      if DecodedFileName = FileName then //found   //////////////////////////////////////////////////////////////////  there is a special comparison compatible to MAC OS, for Unicode strings. Simple string comparison will not work on all strings. See FreePascal docs on strings.
      begin
        Result := FPlainStream.Position - CFileContentSizeField;
        Exit;
      end;

      // go past the file content
      FPlainStream.Position := Min(FPlainStream.Position + FileContentSize, FPlainStream.Size);
    until FPlainStream.Position >= FPlainStream.Size;   //Position can be at most the same as Size  (limited by TStream)
  finally
    FPlainStream.Position := InitPos;
  end;
end;


procedure TMemArchive.AddHeaderToEmptyArchive(AContent: TMemoryStream);
var
  a: array of Byte;
  i: Integer;
begin
  SetLength(a, CHeaderSizeField);
  for i := 0 to Length(a) - 1 do
    a[i] := 0;

  AContent.Position := 0;
  AContent.Write(a[0], Length(a));
end;


procedure TMemArchive.GetKeyFromPassword(var AArcKey: TArr32OfByte);
begin
  DoOnGetKeyFromPassword(FPassword, AArcKey);
end;


procedure TMemArchive.PurgeKey;
var
  i: Integer;
begin
  Randomize;
  for i := 0 to 32 - 1 do
    FKey[i] := Random(256);
end;


procedure TMemArchive.AddPadding;
var
  PaddingSize, CompressedSize, i: LongInt;
  Padding: array of Byte;
begin
  CompressedSize := FArchiveStream.Size;  //even if CompressedSize is negative, PaddingSize is >= 0
  PaddingSize := CompressedSize - (CompressedSize shr 5) shl 5;      //modulo without division
  if PaddingSize > 0 then
    PaddingSize := LongInt(CPaddingTo) - PaddingSize;              //padding to 32 bytes   (padding to CPaddingTo bytes)

  FArchiveStream.Position := 0;
  FArchiveStream.Write(PaddingSize, SizeOf(PaddingSize));

  Randomize;
  SetLength(Padding, PaddingSize);
  try
    for i := 0 to PaddingSize - 1 do
      Padding[i] := Random(256);

    FArchiveStream.Position := FArchiveStream.Size;
    FArchiveStream.Write(Padding[0], PaddingSize);
  finally
    SetLength(Padding, 0);
  end;
end;


function ArrOfByteToHex(var AArr: TArr32OfByte): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AArr) - 1 do
    Result := Result + IntToHex(AArr[i], 2);
end;


procedure TMemArchive.CheckHashOfDecryptedArchive;
var
  ExpectedHash, CurrentHash: TArr32OfByte;
  i: Integer;
begin
  if FArchiveStream.Size = 0 then
    raise Exception.Create('Archive is invalid. There is no content.');

  FArchiveStream.Position := CPaddingSizeField; //point to hash
  FArchiveStream.Read(ExpectedHash[0], CHashSize);

  DoOnComputeArchiveHash(Pointer(UInt64(FArchiveStream.Memory) + CPaddingAndHashSizeField), FArchiveStream.Size - CPaddingAndHashSizeField, CurrentHash, 'CheckHashOfDecryptedArchive');

  for i := 0 to CHashSize - 1 do
    if CurrentHash[i] <> ExpectedHash[i] then
      raise Exception.Create('Archive is invalid. Hash mismatch. ' + ArrOfByteToHex(CurrentHash) + '   ' + ArrOfByteToHex(ExpectedHash));
end;


function TMemArchive.SetHashOfDecryptedArchive: string;
var
  CurrentHash: TArr32OfByte;
begin
  FArchiveStream.Position := CPaddingAndHashSizeField;

  DoOnComputeArchiveHash(Pointer(UInt64(FArchiveStream.Memory) + CPaddingAndHashSizeField), FArchiveStream.Size - CPaddingAndHashSizeField, CurrentHash, 'SetHashOfDecryptedArchive');

  FArchiveStream.Position := CPaddingSizeField; //point to hash
  FArchiveStream.Write(CurrentHash[0], CHashSize);

  Result := ArrOfByteToHex(CurrentHash);
end;


procedure GenerateEmptyHash(var AHash: TArr32OfByte);
var
  i: Integer;
begin
  for i := 0 to CHashSize - 1 do
    AHash[i] := 0;
end;


procedure TMemArchive.OpenArchive(AArchiveStream: TMemoryStream; CreateNewArchive: Boolean);
var
  PaddingSize: LongInt;
  EmptyHash: TArr32OfByte;
  TempArchiveStream: TMemoryStream;
begin
  if Password > '' then
  begin
    GetKeyFromPassword(FKey);
    DoOnInitEncryption(FKey);
  end;

  FArchiveStream := AArchiveStream;   //assign to provided stream

  if not CreateNewArchive then
  begin   //use existing archive content
    if Password > '' then
      DecryptArchive;     //padding will be truncated automatically because of the for loop

    CheckHashOfDecryptedArchive;

    FArchiveStream.Position := 0;
    FArchiveStream.Read(PaddingSize, CPaddingSizeField);  //Position becomes 4

    if (PaddingSize < 0) or (PaddingSize > CPaddingTo - 1) then
      raise Exception.Create('Invalid padding size (' + IntToStr(PaddingSize) + ').');

    if PaddingSize > FArchiveStream.Size then
      raise Exception.Create('The padding size exceeds archive size.');

    FArchiveStream.SetSize(FArchiveStream.Size - PaddingSize); //discard padding
    FArchiveStream.Position := CPaddingAndHashSizeField; //padding size + hash of decrypted

    if FCompressionLevel > 0 then
    begin
      TempArchiveStream := TMemoryStream.Create;
      try
        TempArchiveStream.CopyFrom(FArchiveStream, FArchiveStream.Size - CPaddingAndHashSizeField);
        TempArchiveStream.Position := 0;
        FPlainStream.Position := 0;

        if not DoOnDecompress(TempArchiveStream, FPlainStream) then
          raise Exception.Create('Error while decompressing.');

        FillChar(TempArchiveStream.Memory^, TempArchiveStream.Size, 0);
      finally
        TempArchiveStream.Free;
      end;
    end
    else
    begin
      FPlainStream.Position := 0;
      FPlainStream.CopyFrom(FArchiveStream, FArchiveStream.Size - CPaddingAndHashSizeField);
    end;

    CheckMaxValues;
  end
  else
  begin   //create from scratch
    FShouldUpdateArchiveOnClose := True;
    AddHeaderToEmptyArchive(FPlainStream);

    PaddingSize := 0; //assume 0 at this point
    FArchiveStream.Position := 0;
    FArchiveStream.Write(PaddingSize, SizeOf(PaddingSize));

    GenerateEmptyHash(EmptyHash);
    FArchiveStream.Write(EmptyHash[0], CHashSize); //hash of decrypted
  end;
end;


function TMemArchive.CloseArchive: string;
var
  TempArchiveStream: TMemoryStream;
begin
  Result := '';

  if FShouldUpdateArchiveOnClose then
  begin
    FArchiveStream.Position := CPaddingAndHashSizeField; //  position of compressed content (which comes after the padding info, a.k.a. 4)  + size of hash

    if FArchiveStream.Size <> CPaddingAndHashSizeField then
      raise Exception.Create('================= Archive size mismatch on setting position.');

    if FCompressionLevel > 0 then
    begin
      TempArchiveStream := TMemoryStream.Create;
      try
        FPlainStream.Position := 0;
        DoOnCompress(FPlainStream, TempArchiveStream, FCompressionLevel);

        TempArchiveStream.Position := 0;
        FArchiveStream.CopyFrom(TempArchiveStream, TempArchiveStream.Size);
        FillChar(TempArchiveStream.Memory^, TempArchiveStream.Size, 0);

        if FArchiveStream.Size <> TempArchiveStream.Size + CPaddingAndHashSizeField then
          raise Exception.Create('================= Archive size mismatch on close.');
      finally
        TempArchiveStream.Free;
      end;
    end
    else
    begin
      FPlainStream.Position := 0;
      FArchiveStream.Position := CPaddingAndHashSizeField;  //size of padding field + size of hash
      FArchiveStream.CopyFrom(FPlainStream, FPlainStream.Size);
    end;

    //  add padding, hardcoded to 32 bytes
    AddPadding;

    {Result := } SetHashOfDecryptedArchive;

    if Password > '' then
    begin
      try
        EncryptArchive;
      finally
        PurgeKey;
        DoOnEncryptionCleanup;
      end;
    end;
  end;

  FillChar(FPlainStream.Memory^, FPlainStream.Size, 0);
  FPlainStream.Clear;
end;


procedure TMemArchive.AddFromStream(AFileName: string; AContent: TMemoryStream; AContentCustomSize: Int64; UseBuffer: Boolean = True);
var
  FnmLen: LongInt;
  StreamSize, FilePosition: Int64;
begin
  if AFileName = '' then
    raise Exception.Create('Cannot add a file to archive if it has an empty name.');

  FilePosition := GetPositionOfFileSizeInPlainStream(AFileName);
  if FilePosition <> -1 then
    raise Exception.Create('Another file with the exact same name is already in archive.' + '"' + AFileName + '".');

  FShouldUpdateArchiveOnClose := True;

  FnmLen := Length(AFileName);
  StreamSize := AContent.Size;

  FPlainStream.Position := FPlainStream.Size;          //go to the end of the archive

  FPlainStream.Write(FnmLen, 4);                       //string length
  FPlainStream.Write(AFileName[1], Length(AFileName)); //filename itself

  if AContent.Position < 0 then
    AContent.Position := 0;

  if AContentCustomSize = -1 then
    AContent.Position := 0
  else  //leave position as it is, but set the size to user-provided value
  begin
    if AContentCustomSize > AContent.Size then
      AContentCustomSize := AContent.Size;

    StreamSize := Max(0, AContentCustomSize - AContent.Position);
  end;

  FPlainStream.Write(StreamSize, {SizeOf(StreamSize)} 8);

  if StreamSize > 0 then
    FPlainStream.CopyFrom(AContent, StreamSize);  //FPlainStream will grow in size after this

  CheckMaxValues;  //The valid filecount is obtained after adding the file. That's why this check is placed at the end of AddFromStream procedure.
end;


procedure TMemArchive.AddFromStream(AFileName: string; AContent: TMemoryStream; UseBuffer: Boolean = True);
begin
  AddFromStream(AFileName, AContent, -1, UseBuffer);
end;


procedure TMemArchive.AddFromString(AFileName: string; AContent: string);
var
  ContentStream: TMemoryStream;
begin
  ContentStream := TMemoryStream.Create;
  try
    ContentStream.Write(AContent[1], Length(AContent));
    AddFromStream(AFileName, ContentStream, False);
    FillChar(ContentStream.Memory^, ContentStream.Size, 0);
  finally
    ContentStream.Free;
  end;
end;


procedure TMemArchive.ExtractToStream(AFileName: string; AContent: TMemoryStream);
var
  AFilePos: Int64;
  FileContentSize: Int64;
begin
  AFilePos := GetPositionOfFileSizeInPlainStream(AFileName);

  if AFilePos = -1 then
    raise Exception.Create('The archive does not contain this file: ' + '"' + AFileName + '"');

  FPlainStream.Position := AFilePos;

  if Int64(CFileContentSizeField) > FPlainStream.Size - FPlainStream.Position then
    raise Exception.Create('The filesize info does not fit into the next section of the archive.');

  FPlainStream.Read(FileContentSize, CFileContentSizeField); //this moves position to file content

  if (FileContentSize > 0) and (FileContentSize < CMaxCompressedFileSize) then
  begin
    if FileContentSize > FPlainStream.Size - FPlainStream.Position then
      raise Exception.Create('The current archive does not have enough content, to match the given file size.');

    AContent.Position := 0;
    AContent.CopyFrom(FPlainStream, FileContentSize);  //this enlarges AContent, to fit FileContentSize
  end
  else
    AContent.Clear;
end;


procedure TMemArchive.ExtractToString(AFileName: string; var AContent: string);
var
  ContentStream: TMemoryStream;
begin
  ContentStream := TMemoryStream.Create;
  try
    ExtractToStream(AFileName, ContentStream);

    SetLength(AContent, ContentStream.Size);
    ContentStream.Position := 0;
    ContentStream.Read(AContent[1], ContentStream.Size);
  finally
    ContentStream.Free;
  end;
end;


function TMemArchive.FindFirst(AFileMask: string; var SearchState: Int64): Boolean;
begin
  SearchState := GetPositionOfFileSizeInPlainStream(AFileMask, SearchState);
  Result := SearchState <> -1;
end;


procedure TMemArchive.GetListOfFiles(AListOfFiles: TStringList);
var
  TempFileNameLen: Integer;
  InitPos: Int64;
  DecodedFileName: string;
  FileContentSize: Int64;
begin
  if FPlainStream.Size = 0 then
    Exit;

  if FPlainStream.Size <= CHeaderSizeField then
    Exit;

  //jump from file to file, until match
  InitPos := FPlainStream.Position;        //GetListOfFiles is used for debugging, so the position depends on the caller, i.e. CheckMaxValues

  try
    FPlainStream.Position := CHeaderSizeField;  //start of files
    repeat
      //read filename string length
      if Int64(CFilenameLengthSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename size does not fit into the next section of the archive.');

      FPlainStream.Read(TempFileNameLen, CFilenameLengthSizeField);

      if Int64(TempFileNameLen) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename does not fit into the next section of the archive.');

      if (TempFileNameLen < 1) or (TempFileNameLen > FMaxExpectedFilenameLength) then
        raise Exception.Create('The filename is empty or too long.');

      //read string
      SetLength(DecodedFileName, TempFileNameLen);
      FPlainStream.Read(DecodedFileName[1], TempFileNameLen);
      AListOfFiles.Add(DecodedFileName);

      //read file size
      if Int64(CFileContentSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filesize info does not fit into the next section of the archive.');

      FPlainStream.Read(FileContentSize, CFileContentSizeField);

      if FileContentSize > CMaxCompressedFileSize then
        raise Exception.Create('The filesize is greater than the allowed limit.');

      if (FileContentSize < 0) or (FPlainStream.Position + FileContentSize > FPlainStream.Size) then
        raise Exception.Create('The file content does not fit into the next section of the archive.');

      // go past the file content
      FPlainStream.Position := Min(FPlainStream.Position + FileContentSize, FPlainStream.Size);
    until FPlainStream.Position >= FPlainStream.Size;   //Position can be at most the same as Size  (limited by TStream)
  finally
    FPlainStream.Position := InitPos;  //Restore
  end;
end;


procedure TMemArchive.CheckMaxValues;         //////////////////////////// ToDo:  max archive size  (maybe in OpenArchive)
var
  TempFileNameLen: Integer;
  InitPos: Int64;
  FileContentSize: Int64;
  FileCount: Integer;
  TotalFileSize: Int64;
  {$IFDEF DebugLimits}
    ListOfFiles: TStringList;
    ListOfFileSizes: TStringList;
    DbgFileName: string;
  {$ENDIF}
  s: string;
begin
  if FPlainStream.Size = 0 then
    Exit;

  if FPlainStream.Size <= CHeaderSizeField then
    Exit;

  //jump from file to file, until match
  InitPos := FPlainStream.Position;

  {$IFDEF DebugLimits}
    ListOfFiles := TStringList.Create;
    ListOfFileSizes := TStringList.Create;
  {$ENDIF}

  try
    FileCount := 0;
    TotalFileSize := 0;
    FPlainStream.Position := CHeaderSizeField;  //start of files
    repeat
      //read filename string length
      if Int64(CFilenameLengthSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename size does not fit into the next section of the archive.');

      FPlainStream.Read(TempFileNameLen, CFilenameLengthSizeField);

      {$IFDEF DebugLimits}
        ListOfFileSizes.Add('FileNameLen = ' + IntToStr(AFileNameLen));
      {$ENDIF}

      if (TempFileNameLen < 1) or (TempFileNameLen > FMaxExpectedFilenameLength) then
      begin
        s := 'The filename is empty or too long.' {$IFDEF DebugLimits} + IntToStr(AFileNameLen) {$ENDIF};
        {$IFDEF DebugLimits}
          SetLength(DbgFileName, AFileNameLen);

          ///// reading filename
          FPlainStream.Read(DbgFileName[1], AFileNameLen);
          FPlainStream.Position := FPlainStream.Position - AFileNameLen;  //restore position  (although not needed)
          s := s + ' <' + StringReplace(DbgFileName, #0, #1, [rfReplaceAll]) + '>';
          ///// done reading filename

          GetListOfFiles(ListOfFiles);
          s := s + #13#10 + IntToStr(AFileNameLen) + ' vs. ' + IntToStr(FMaxExpectedFilenameLength) + #13#10 + ListOfFiles.Text + #13#10 + ListOfFileSizes.Text;
        {$ENDIF}
        raise Exception.Create(s);
      end;

      if Int64(TempFileNameLen) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filename does not fit into the next section of the archive.');

      FPlainStream.Position := FPlainStream.Position + TempFileNameLen;    //advance past filename
      Inc(FileCount);

      if (FMaxExpectedFileCount > 0) and (FileCount > FMaxExpectedFileCount) then
      begin
        s := 'The archive containts too many files.';
        {$IFDEF DebugLimits}
          GetListOfFiles(ListOfFiles);
          s := s + #13#10 + ListOfFiles.Text + #13#10 + ListOfFileSizes.Text;
        {$ENDIF}
        raise Exception.Create(s);
      end;

      //read file size
      if Int64(CFileContentSizeField) > FPlainStream.Size - FPlainStream.Position then
        raise Exception.Create('The filesize info does not fit into the next section of the archive.');

      FPlainStream.Read(FileContentSize, CFileContentSizeField);

      if FileContentSize > CMaxCompressedFileSize then
        raise Exception.Create('The filesize is greater than the allowed limit.');

      if (FileContentSize < 0) or (FPlainStream.Position + FileContentSize > FPlainStream.Size) then
        raise Exception.Create('The file content does not fit into the next section of the archive.');

      Inc(TotalFileSize, FileContentSize);

      {$IFDEF DebugLimits}
        ListOfFileSizes.Add('FileSize = ' + IntToStr(AFileNameLen));
      {$ENDIF}

      if (FMaxExpectedTotalFileSize > 0) and (TotalFileSize > FMaxExpectedTotalFileSize) then
      begin
        s := 'The total sum of filesizes is greater than allowed. ' + IntToStr(TotalFileSize);
        {$IFDEF DebugLimits}
          GetListOfFiles(ListOfFiles);
          s := s + #13#10 + ListOfFiles.Text + #13#10 + ListOfFileSizes.Text;
        {$ENDIF}
        raise Exception.Create(s);
      end;

      // go past the file content
      FPlainStream.Position := Min(FPlainStream.Position + FileContentSize, FPlainStream.Size);
    until FPlainStream.Position >= FPlainStream.Size;  //Position can be at most the same as Size  (limited by TStream)
  finally
    FPlainStream.Position := InitPos;

    {$IFDEF DebugLimits}
      ListOfFiles.Free;
      ListOfFileSizes.Free;
    {$ENDIF}
  end;
end;

end.

