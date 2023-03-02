object frObjectInspector: TfrObjectInspector
  Left = 0
  Height = 240
  Top = 0
  Width = 360
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 240
  ClientWidth = 360
  Color = 7993855
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object pnlvstOI: TPanel
    Left = 0
    Height = 216
    Top = 0
    Width = 360
    Anchors = [akTop, akLeft, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlvstOI'
    Color = 181757
    ParentColor = False
    TabOrder = 0
    Visible = False
  end
  object imgDownArrow: TImage
    Left = 336
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
  object edtSearch: TEdit
    Left = 0
    Height = 23
    Hint = 'The tree does not work properly.'#13#10'After searching for a string, the vertical scrollbar does not scroll to the last item.'
    Top = 216
    Width = 320
    Anchors = [akLeft, akRight, akBottom]
    OnChange = edtSearchChange
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'Search'
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
  object tmrSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrSearchTimer
    Left = 272
    Top = 32
  end
  object pmOI: TPopupMenu
    Left = 128
    Top = 32
    object MenuItem_ShowHideSearchBox: TMenuItem
      Caption = 'Show / Hide SearchBox'
      OnClick = MenuItem_ShowHideSearchBoxClick
    end
  end
end
