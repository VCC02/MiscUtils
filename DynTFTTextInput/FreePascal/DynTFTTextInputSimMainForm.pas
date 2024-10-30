{   DynTFT  - graphic components for microcontrollers
    Copyright (C) 2024 VCC
    release date: 30 Oct 2024
    author: VCC

    This file is part of DynTFT project.

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

unit DynTFTTextInputSimMainForm;

{$IFNDEF IsMCU}
  {$DEFINE IsDesktop}
{$ENDIF}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls;

type

  { TfrmDynTFTTextInputSimMain }

  TfrmDynTFTTextInputSimMain = class(TForm)
    lblAllocatedMemory: TLabel;
    lstLog: TListBox;
    pnlRunning: TPanel;
    btnSimulate: TButton;
    btnStopSimulator: TButton;
    btnDisplayVirtualScreen: TButton;
    prbAllocatedMemory: TProgressBar;
    tmrStartup: TTimer;
    tmrBlinkCaret: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure lstLogKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnDisplayVirtualScreenClick(Sender: TObject);
    procedure btnSimulateClick(Sender: TObject);
    procedure btnStopSimulatorClick(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure tmrBlinkCaretTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure DynTFTUILoop;
  public
    { Public declarations }
  end;

var
  frmDynTFTTextInputSimMain: TfrmDynTFTTextInputSimMain;

implementation

{$R *.frm}


uses
  {$IFDEF UseSmallMM}
    DynTFTSmallMM,
  {$ELSE}
    MemManager,
  {$ENDIF}

  TFT, DynTFTTextInputGUI, DynTFTControls, DynTFTBaseDrawing, DynTFTUtils, DynTFTMessageBox,
  IniFiles, DynTFTTextInputSimScreenForm, ClipBrd;


{TfrmDynTFTTextInputSimMain}


procedure TfrmDynTFTTextInputSimMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + Application.Title + '.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);
  finally
    Ini.Free;
  end;
end;


procedure TfrmDynTFTTextInputSimMain.lstLogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = Ord('C') then
    if ssCtrl in Shift then
    begin
      if ssShift in Shift then
        Clipboard.AsText := lstLog.Items.Text
      else
        if lstLog.ItemIndex > -1 then
          Clipboard.AsText := lstLog.Items.Strings[lstLog.ItemIndex];
    end;
end;


procedure TfrmDynTFTTextInputSimMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + Application.Title + '.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmDynTFTTextInputSimMain.tmrBlinkCaretTimer(Sender: TObject);
begin
  DynTFTGBlinkingCaretStatus := not DynTFTGBlinkingCaretStatus;
end;


procedure TfrmDynTFTTextInputSimMain.DynTFTUILoop;
var
  AllocatedMemSize: Integer;
begin
  try
    DynTFT_GUI_LoopIteration;

    AllocatedMemSize := {$IFDEF UseSmallMM} MaxMM {$ELSE} HEAP_SIZE {$ENDIF} - MM_TotalFreeMemSize;
    if prbAllocatedMemory.Position <> AllocatedMemSize then
    begin
      prbAllocatedMemory.Position := AllocatedMemSize;
      lblAllocatedMemory.Caption := 'Allocated Memory: ' + IntToStr(AllocatedMemSize) + ' B    (' + IntToStr(AllocatedMemSize shr 10) + ' KB)';
    end;
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Exception while executing DynTFT_GUI_LoopIteration: ' + E.Message);
      lstLog.Color := $E0E0FF;
    end;
  end;
end;


procedure TfrmDynTFTTextInputSimMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  UseTFTTrueColor := True;
  prbAllocatedMemory.Max := {$IFDEF UseSmallMM} MaxMM {$ELSE} HEAP_SIZE {$ENDIF};

  LoadSettingsFromIni;
  DynTFT_AssignDebugConsole(lstLog);
end;


procedure TfrmDynTFTTextInputSimMain.btnDisplayVirtualScreenClick(Sender: TObject);
begin
  frmDynTFTTextInputSimScreen.Show;
end;


procedure HandleMessageBox(AMessageBox: PDynTFTMessageBox);
begin
  repeat
    frmDynTFTTextInputSimMain.DynTFTUILoop;
    Application.ProcessMessages;
    Sleep(1);
  until AMessageBox^.Done or not frmDynTFTTextInputSimMain.btnSimulate.Enabled;
end;


procedure TfrmDynTFTTextInputSimMain.btnSimulateClick(Sender: TObject);
begin
  pnlRunning.Color := clLime;
  pnlRunning.Tag := 1; //Set the color to whatever you want, but leave this tag alone!

  {$IFNDEF IsDesktop}
    {$IFNDEF IsMCU}
       raise Exception.Create('Neither "IsDesktop" nor "IsMCU" found.'); //IsDesktop is required when assigning DynTFTMessageBoxMainLoopHandler
    {$ENDIF}
  {$ENDIF}

  btnSimulate.Enabled := False;
  lstLog.Clear;
  MM_Init;

  GCanvas.Pen.Color := $0088FF;
  GCanvas.Brush.Color := clSilver;
  GCanvas.Rectangle(0, 0, frmDynTFTTextInputSimScreen.imgScreen.Width, frmDynTFTTextInputSimScreen.imgScreen.Height);

  DynTFT_Init(frmDynTFTTextInputSimScreen.trbScreenWidth.Position, frmDynTFTTextInputSimScreen.trbScreenHeight.Position);

  try
    New(DynTFTMessageBoxMainLoopHandler);
    DynTFT_GUI_Start;
    DynTFTSetScreenSizeForMessageBox(TFT_DISP_WIDTH, TFT_DISP_HEIGHT);
    {$IFDEF IsDesktop}
      DynTFTMessageBoxMainLoopHandler^ := HandleMessageBox;
    {$ELSE}
      DynTFTMessageBoxMainLoopHandler := @HandleMessageBox;
    {$ENDIF}

    tmrBlinkCaret.Enabled := True;
    lstLog.Color := clWindow;
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Exception in DynTFT_GUI_Start: ' + E.Message);
      lstLog.Color := $E0E0FF;
    end;
  end;

  repeat
    DynTFTUILoop;
    Application.ProcessMessages;
    Sleep(1);
  until btnSimulate.Enabled;  //Can be enabled by Stop button or by closing the app.
end;


procedure TfrmDynTFTTextInputSimMain.btnStopSimulatorClick(Sender: TObject);
begin
  btnSimulate.Enabled := True;

  try
    Dispose(DynTFTMessageBoxMainLoopHandler);
    DynTFTMessageBoxMainLoopHandler := nil;
  except
    on E: Exception do
      DynTFT_DebugConsole('Exception on disposing DynTFTMessageBoxMainLoopHandler: ' + E.Message);
  end;

  if pnlRunning.Tag = 1 then
  begin
    pnlRunning.Tag := 0;
    try
      DynTFTFreeComponentTypeRegistration;
    except
      on E: Exception do
        DynTFT_DebugConsole('Exception in FreeComponentTypeRegistration: ' + E.Message);
    end;
  end;

  pnlRunning.Color := clGreen;
  tmrBlinkCaret.Enabled := False;
end;


procedure TfrmDynTFTTextInputSimMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  try
    btnStopSimulator.Click;  //some cleanup before closing
  except
  end;

  try
    SaveSettingsToIni;
  except
  end;
end;


procedure TfrmDynTFTTextInputSimMain.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
  lstLog.Hint := 'Press Ctrl-C to copy selected line to clipboard.' + #13#10 +
                 'Press Ctrl-Shift-C to copy the entire log.';
end;

end.
