object frmClickerZoomPreview: TfrmClickerZoomPreview
  Left = 244
  Top = 227
  BorderStyle = bsNone
  Caption = 'Clicker Zoom'
  ClientHeight = 242
  ClientWidth = 242
  Color = clLime
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object imgZoom: TImage
    Left = 1
    Top = 1
    Width = 240
    Height = 240
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 82
    Top = 44
  end
end
