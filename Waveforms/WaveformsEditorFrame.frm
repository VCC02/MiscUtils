object FrameWaveformsEditor: TFrameWaveformsEditor
  Left = 0
  Height = 382
  Top = 0
  Width = 1040
  ClientHeight = 382
  ClientWidth = 1040
  TabOrder = 0
  TabStop = True
  DesignLeft = 86
  DesignTop = 85
  object scrboxWaveforms: TScrollBox
    Left = 2
    Height = 324
    Top = 24
    Width = 1035
    HorzScrollBar.Page = 1
    HorzScrollBar.Visible = False
    VertScrollBar.Page = 1
    VertScrollBar.Tracking = True
    Anchors = [akTop, akLeft, akRight, akBottom]
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
  end
  object scrbarTimeWaveforms: TScrollBar
    Left = 3
    Height = 16
    Top = 354
    Width = 974
    Anchors = [akLeft, akRight, akBottom]
    Max = 1
    PageSize = 0
    TabOrder = 1
    OnChange = scrbarTimeWaveformsChange
  end
  object chkXes: TCheckBox
    Left = 991
    Height = 19
    Hint = 'Display red x-es on analog samples.'
    Top = 351
    Width = 36
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
