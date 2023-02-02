object frObjectInspector: TfrObjectInspector
  Left = 0
  Height = 240
  Top = 0
  Width = 397
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 240
  ClientWidth = 397
  Color = 7993855
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlvstOI: TPanel
    Left = 0
    Height = 214
    Top = 0
    Width = 397
    Anchors = [akTop, akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlvstOI'
    Color = 181757
    ParentColor = False
    TabOrder = 0
    Visible = False
  end
  object imgDownArrow: TImage
    Left = 216
    Height = 5
    Top = 224
    Width = 11
    AutoSize = True
    Picture.Data = {
      07544269746D6170EA000000424DEA0000000000000036000000280000000B00
      0000050000000100180000000000B40000000000000000000000000000000000
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF39841AFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFF
      FFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39
      841AFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF39841A39841A39841A39841A
      39841A39841A39841AFFFFFFFFFFFF000000FFFFFF39841A39841A39841A3984
      1A39841A39841A39841A39841A39841AFFFFFF000000
    }
    Visible = False
  end
  object imglstOIColorIcons: TImageList
    Left = 268
    Top = 108
  end
  object tmrEditingProperty: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrEditingPropertyTimer
    Left = 272
    Top = 168
  end
  object tmrColCmbDropped: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrColCmbDroppedTimer
    Left = 128
    Top = 168
  end
  object tmrSetEditBox: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSetEditBoxTimer
    Left = 128
    Top = 112
  end
end
