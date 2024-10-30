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

unit DynTFTTextInputSimScreenForm;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TfrmDynTFTTextInputSimScreen = class(TForm)
    imgScreen: TImage;
    pnlCoords: TPanel;
    tmrStartup: TTimer;
    trbScreenWidth: TTrackBar;
    lblScreenWidth: TLabel;
    lblScreenHeight: TLabel;
    trbScreenHeight: TTrackBar;
    lblWidth: TLabel;
    lblHeight: TLabel;
    chkShowWidthLine: TCheckBox;
    chkShowHeightLine: TCheckBox;
    procedure imgScreenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgScreenMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrStartupTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure chkShowWidthLineClick(Sender: TObject);
    procedure chkShowHeightLineClick(Sender: TObject);
    procedure trbScreenWidthChange(Sender: TObject);
    procedure trbScreenHeightChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;
  public
    { Public declarations }
  end;

var
  frmDynTFTTextInputSimScreen: TfrmDynTFTTextInputSimScreen;

implementation

{$R *.frm}

uses
  IniFiles, TFT, DynTFTBaseDrawing;
  

procedure TfrmDynTFTTextInputSimScreen.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + Application.Title + '.ini');
  try
    Left := Ini.ReadInteger('ScreenWindow', 'Left', Left);
    Top := Ini.ReadInteger('ScreenWindow', 'Top', Top);
    Width := Ini.ReadInteger('ScreenWindow', 'Width', Width);
    Height := Ini.ReadInteger('ScreenWindow', 'Height', Height);

    chkShowWidthLine.Checked := Ini.ReadBool('ScreenOptions', 'chkShowWidthLine.Checked', chkShowWidthLine.Checked);
    chkShowHeightLine.Checked := Ini.ReadBool('ScreenOptions', 'chkShowHeightLine.Checked', chkShowHeightLine.Checked);
    trbScreenWidth.Position := Ini.ReadInteger('ScreenOptions', 'trbScreenWidth.Position', 320);
    trbScreenHeight.Position := Ini.ReadInteger('ScreenOptions', 'trbScreenHeight.Position', 240);
  finally
    Ini.Free;
  end;
end;


procedure TfrmDynTFTTextInputSimScreen.SaveSettingsToIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + Application.Title + '.ini');
  try
    Ini.WriteInteger('ScreenWindow', 'Left', Left);
    Ini.WriteInteger('ScreenWindow', 'Top', Top);
    Ini.WriteInteger('ScreenWindow', 'Width', Width);
    Ini.WriteInteger('ScreenWindow', 'Height', Height);

    Ini.WriteBool('ScreenOptions', 'chkShowWidthLine.Checked', chkShowWidthLine.Checked);
    Ini.WriteBool('ScreenOptions', 'chkShowHeightLine.Checked', chkShowHeightLine.Checked);
    Ini.WriteInteger('ScreenOptions', 'trbScreenWidth.Position', trbScreenWidth.Position);
    Ini.WriteInteger('ScreenOptions', 'trbScreenHeight.Position', trbScreenHeight.Position);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmDynTFTTextInputSimScreen.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  imgScreen.Picture.Bitmap := TBitmap.Create;
  imgScreen.Picture.Bitmap.Width := imgScreen.Width;
  imgScreen.Picture.Bitmap.Height := imgScreen.Height;

  lblWidth.Width := 1;
  lblWidth.Height := imgScreen.Height;
  lblWidth.Top := imgScreen.Top;

  lblHeight.Height := 1;
  lblHeight.Width := imgScreen.Width;
  lblHeight.Left := imgScreen.Left;

  Show;

  LoadSettingsFromIni;

  lblWidth.Visible := chkShowWidthLine.Checked;
  lblHeight.Visible := chkShowHeightLine.Checked;

  lblWidth.Left := trbScreenWidth.Position + imgScreen.Left;
  lblHeight.Top := trbScreenHeight.Position + imgScreen.Top;

  GCanvas := frmDynTFTTextInputSimScreen.imgScreen.Canvas;

  DoubleBuffered := True;
end;


procedure TfrmDynTFTTextInputSimScreen.trbScreenHeightChange(Sender: TObject);
begin
  lblHeight.Top := trbScreenHeight.Position + imgScreen.Top;
  lblScreenHeight.Caption := 'Screen Height: ' + IntToStr(trbScreenHeight.Position);
end;


procedure TfrmDynTFTTextInputSimScreen.trbScreenWidthChange(Sender: TObject);
begin
  lblWidth.Left := trbScreenWidth.Position + imgScreen.Left;
  lblScreenWidth.Caption := 'Screen Width: ' + IntToStr(trbScreenWidth.Position);
end;


procedure TfrmDynTFTTextInputSimScreen.chkShowHeightLineClick(Sender: TObject);
begin
  lblHeight.Visible := chkShowHeightLine.Checked;
end;


procedure TfrmDynTFTTextInputSimScreen.chkShowWidthLineClick(Sender: TObject);
begin
  lblWidth.Visible := chkShowWidthLine.Checked;
end;


procedure TfrmDynTFTTextInputSimScreen.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
end;


procedure TfrmDynTFTTextInputSimScreen.FormDestroy(Sender: TObject);
begin
  try
    SaveSettingsToIni;
  except
  end;
end;


procedure TfrmDynTFTTextInputSimScreen.imgScreenMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DynTFTMCU_OldXMouse := DynTFTMCU_XMouse;
  DynTFTMCU_OldYMouse := DynTFTMCU_YMouse;
  DynTFTMCU_XMouse := X;
  DynTFTMCU_YMouse := Y;
  DynTFTReceivedMouseDown := True;
end;


procedure TfrmDynTFTTextInputSimScreen.imgScreenMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  pnlCoords.Caption := IntToStr(X) + ' : ' + IntToStr(Y);

  if ssLeft in Shift then
  begin
    DynTFTMCU_OldXMouse := DynTFTMCU_XMouse;
    DynTFTMCU_OldYMouse := DynTFTMCU_YMouse;
    DynTFTMCU_XMouse := X;
    DynTFTMCU_YMouse := Y;
    DynTFTReceivedMouseDown := True;
  end;
end;


procedure TfrmDynTFTTextInputSimScreen.imgScreenMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DynTFTMCU_XMouse := X;
  DynTFTMCU_YMouse := Y;
  DynTFTReceivedMouseUp := True;
end;

end.
