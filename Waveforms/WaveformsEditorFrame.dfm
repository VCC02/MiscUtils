object FrameWaveformsEditor: TFrameWaveformsEditor
  Left = 0
  Top = 0
  Width = 1040
  Height = 382
  TabOrder = 0
  TabStop = True
  DesignSize = (
    1040
    382)
  object scrboxWaveforms: TScrollBox
    Left = 2
    Top = 24
    Width = 1035
    Height = 324
    HorzScrollBar.Visible = False
    VertScrollBar.Tracking = True
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
  end
  object scrbarTimeWaveforms: TScrollBar
    Left = 3
    Top = 354
    Width = 974
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    Max = 1
    PageSize = 0
    TabOrder = 1
    OnChange = scrbarTimeWaveformsChange
  end
  object chkXes: TCheckBox
    Left = 991
    Top = 353
    Width = 97
    Height = 17
    Hint = 'Display red x-es on analog samples.'
    Anchors = [akRight, akBottom]
    Caption = 'Xes'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object tmrPaintWaveforms: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrPaintWaveformsTimer
    Left = 950
    Top = 317
  end
  object tmrCtrlKey: TTimer
    Interval = 10
    OnTimer = tmrCtrlKeyTimer
    Left = 982
    Top = 317
  end
  object tmrMouseSelection: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrMouseSelectionTimer
    Left = 912
    Top = 312
  end
end
