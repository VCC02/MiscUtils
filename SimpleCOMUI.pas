{
    Copyright (C) 2025 VCC
    creation date: 07 Nov 2025
    initial release date: 07 Nov 2025

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


unit SimpleCOMUI;

{$IFDEF FPC}
  {$MODE Delphi}{$H+}
{$ENDIF}

interface

uses
  //{$IFDEF UNIX}
  //  LCLIntf, LCLType,
  //{$ELSE}
  //  Windows,
  //{$ENDIF}
  Messages, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics;

type
  TOnConnectionToCOM = procedure of object;
  TOnDisconnectionFromCOM = procedure of object;
  TOnExecuteCOMThread = procedure(ATerminated: PBoolean) of object;

  TfrSimpleCOMUI = class;

  TComThread = class(TThread)
  private
    FOwnerFrame: TfrSimpleCOMUI;
  protected
    procedure Execute; override;
  end;

  { TfrSimpleCOMUI }

  TfrSimpleCOMUI = class(TFrame)
    btnConnect: TButton;
    btnDisconnect: TButton;
    chkShowAll: TCheckBox;
    cmbBaud: TComboBox;
    cmbCOMPort: TComboBox;
    lblBaudRate: TLabel;
    lblCOMNumber: TLabel;
    lblCOMStatus: TLabel;
    lblStatusMsg: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure cmbCOMPortDropDown(Sender: TObject);
  private
    FConnHandle: THandle;
    FCOMName: string;
    FBaudRate: Integer;
    FTh: TComThread;

    FOnConnectionToCOM: TOnConnectionToCOM;
    FOnDisconnectionFromCOM: TOnDisconnectionFromCOM;
    FOnExecuteCOMThread: TOnExecuteCOMThread;

    procedure SetCOMName(Value: string);
    procedure SetBaudRate(Value: Integer);

    procedure DoOnConnectionToCOM;
    procedure DoOnDisconnectionFromCOM;
    procedure DoOnExecuteCOMThread(ATerminated: PBoolean);

    procedure CreateTh;
    procedure TerminateTh;

    {$IFnDEF UNIX}
      procedure DetectDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
    {$ENDIF}

    procedure DisplayExtraErrorMessage(AMsg: string; AExtraInfo: string = '');
    procedure ClearExtraErrorMessage;

    procedure ConnectToCOMPortUI;
    procedure DisconnectFromCOMPortUI;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateListOfCOMPorts; //should be called in user code on app startup
    function GetCurrentCOMName: string; //Returns selected COM name or empty string if none selected. The COMName property can be set to a non-existent COM name, but this function returns only from the list.

    property COMName: string read FCOMName write SetCOMName;
    property BaudRate: Integer read FBaudRate write SetBaudRate;
    property ConnHandle: THandle read FConnHandle;

    property OnConnectionToCOM: TOnConnectionToCOM write FOnConnectionToCOM;
    property OnDisconnectionFromCOM: TOnDisconnectionFromCOM write FOnDisconnectionFromCOM;
    property OnExecuteCOMThread: TOnExecuteCOMThread write FOnExecuteCOMThread;
  end;


implementation

{$IFDEF FPC}
  {$R *.frm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
  SimpleCOM;


procedure TComThread.Execute;
begin
  try
    FOwnerFrame.DoOnExecuteCOMThread(@Terminated);
  except
  end;
end;


{ TfrSimpleCOMUI }


constructor TfrSimpleCOMUI.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCOMName := '';
  FTh := nil;
  FBaudRate := 256000;

  FOnConnectionToCOM := nil;
  FOnDisconnectionFromCOM := nil;
  FOnExecuteCOMThread := nil;
end;


destructor TfrSimpleCOMUI.Destroy;
begin
  try
    TerminateTh;

    try
      if COMIsConnected(FComName) then
        DisconnectFromCOM(FComName);
    except
    end;
  finally
    inherited Destroy;
  end;
end;


procedure TfrSimpleCOMUI.DoOnConnectionToCOM;
begin
  if Assigned(FOnConnectionToCOM) then
    FOnConnectionToCOM();

  if not Assigned(FOnExecuteCOMThread) then
    raise Exception.Create('The OnExecuteCOMThread event is not assigned to a handler.');
end;


procedure TfrSimpleCOMUI.DoOnDisconnectionFromCOM;
begin
  if Assigned(FOnDisconnectionFromCOM) then
    FOnDisconnectionFromCOM();
end;


procedure TfrSimpleCOMUI.DoOnExecuteCOMThread(ATerminated: PBoolean);
begin
  //The Assigned(FOnExecuteCOMThread) verification is done once in DoOnConnectionToCOM.
  FOnExecuteCOMThread(ATerminated);
end;


procedure TfrSimpleCOMUI.SetCOMName(Value: string);
begin
  if FCOMName <> Value then
  begin
    FCOMName := Value;
    cmbCOMPort.ItemIndex := cmbCOMPort.Items.IndexOf(FComName);
  end;
end;


procedure TfrSimpleCOMUI.SetBaudRate(Value: Integer);
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := Value;
    cmbBaud.ItemIndex := cmbBaud.Items.IndexOf(IntToStr(FBaudRate));
  end;
end;


procedure TfrSimpleCOMUI.CreateTh;
begin
  if FTh <> nil then
    TerminateTh;

  FTh := TComThread.Create(True);
  FTh.FreeOnTerminate := False;
  FTh.FOwnerFrame := Self;

  {$IFDEF VER180}
    FTh.Resume;
  {$ELSE}
    FTh.Start;
  {$ENDIF}
end;


procedure TfrSimpleCOMUI.TerminateTh;
var
  tk: Int64;
begin
  if FTh = nil then
    Exit;

  try
    FTh.Terminate;

    tk := GetTickCount64;
    repeat
      Application.ProcessMessages;
      Sleep(1);
    until FTh.Terminated or (GetTickCount64 - tk > 1000);
  except
  end;

  try
    FreeAndNil(FTh);
  except
  end;
end;


procedure TfrSimpleCOMUI.ConnectToCOMPortUI;
begin
  lblCOMStatus.Font.Color := $00006800;
  lblCOMStatus.Caption := 'Status: Connected';
  lblCOMStatus.Hint := '';

  ClearExtraErrorMessage;

  btnConnect.Enabled := False;
  btnDisconnect.Enabled := True;
  cmbCOMPort.Enabled := False;
  cmbBaud.Enabled := False;

  CreateTh;
end;


procedure TfrSimpleCOMUI.DisconnectFromCOMPortUI;
begin
  TerminateTh;
  DisconnectFromCOM(FCOMName);  //set internal library info to "disconnected"

  try
    lblCOMStatus.Hint := SysErrorMessage(GetCOMError(FCOMName));
  except
    //GetComErrors may throw an AV if the COM port is closed  :(
  end;

  lblCOMStatus.Font.Color := clMaroon;
  lblCOMStatus.Caption := 'Status: Disconnected';

  ClearExtraErrorMessage;

  btnConnect.Enabled := True;
  btnDisconnect.Enabled := False;
  cmbCOMPort.Enabled := True;
  cmbBaud.Enabled := True;

  DoOnDisconnectionFromCOM;
end;


{$IFnDEF UNIX}
  procedure TfrSimpleCOMUI.DetectDeviceChange(var Msg: TMessage);
  begin
    Sleep(100);
    UpdateListOfCOMPorts;

    if cmbCOMPort.Items.IndexOf(FCOMName) = -1 then  //current com is disconnected
      if COMIsConnected(FCOMName) then
        DisconnectFromCOMPortUI;
  end;
{$ENDIF}


procedure TfrSimpleCOMUI.DisplayExtraErrorMessage(AMsg: string;
  AExtraInfo: string);
begin
  lblStatusMsg.Caption := 'Msg: ' + AMsg;
  lblStatusMsg.Font.Color := clMaroon;
  lblStatusMsg.Font.Style := [fsBold];
  lblStatusMsg.Hint := AExtraInfo;

  {$IFDEF UNIX}
    if AMsg = 'Permission denied' then
      lblStatusMsg.Hint := lblStatusMsg.Hint + #13#10 +
                           'Type this into a terminal, then relogin: ' + #13#10 +
                           'sudo usermod -a -G dialout $USER';
  {$ENDIF}
end;


procedure TfrSimpleCOMUI.ClearExtraErrorMessage;
begin
  lblStatusMsg.Caption := 'Msg';
  lblStatusMsg.Font.Color := clWindowText;
  lblStatusMsg.Font.Style := [];
end;


procedure TfrSimpleCOMUI.UpdateListOfCOMPorts;
var
  OldSelection: string;
  NewSelectionIndex: Integer;
  AllCOMs: TStringList;
begin
  AllCOMs := TStringList.Create;
  try
    ListExistentCOMPorts(AllCOMs, Visible and chkShowAll.Checked);

    if cmbCOMPort.Items.Text <> AllCOMs.Text then
    begin
      OldSelection := '';
      if cmbCOMPort.ItemIndex <> -1 then
        OldSelection := cmbCOMPort.Items.Strings[cmbCOMPort.ItemIndex];

      cmbCOMPort.Items.Text := AllCOMs.Text;

      if OldSelection <> '' then
      begin
        NewSelectionIndex := cmbCOMPort.Items.IndexOf(OldSelection);
        if NewSelectionIndex <> -1 then
          cmbCOMPort.ItemIndex := NewSelectionIndex;
      end;
    end;
  finally
    AllCOMs.Free;
  end;
end;


function TfrSimpleCOMUI.GetCurrentCOMName: string;
begin
  if cmbCOMPort.ItemIndex = -1 then
  begin
    Result := '';
    Exit;
  end;

  Result := cmbCOMPort.Items.Strings[cmbCOMPort.ItemIndex];
end;


procedure TfrSimpleCOMUI.cmbCOMPortDropDown(Sender: TObject);
begin
  UpdateListOfCOMPorts;
end;


procedure TfrSimpleCOMUI.btnConnectClick(Sender: TObject);
var
  BaudStr: string;
begin
  FCOMName := GetCurrentCOMName;
  if FCOMName = '' then
  begin
    DisplayExtraErrorMessage('No COM is selected.');
    Exit;
  end;

  if cmbBaud.ItemIndex = -1 then
    BaudStr := '256000'
  else
    BaudStr := cmbBaud.Items.Strings[cmbBaud.ItemIndex];

  FConnHandle := ConnectToCOM(FCOMName, StrToIntDef(BaudStr, 256000), {Parity_none, 8, ONESTOPBIT,} 4096, 4096);
  if FConnHandle > 0 then
  begin
    ConnectToCOMPortUI;
    DoOnConnectionToCOM;
  end
  else
  begin
    try
      DisplayExtraErrorMessage(SysErrorMessage(GetCOMError(FCOMName)), GetCOMExtraError(FCOMName)); //SysErrorMessage(GetLastError);
    except
      //GetComErrors may throw an AV if the COM port is closed  :(
    end;
  end;
end;


procedure TfrSimpleCOMUI.btnDisconnectClick(Sender: TObject);
begin
  DisconnectFromCOMPortUI;
end;

end.

