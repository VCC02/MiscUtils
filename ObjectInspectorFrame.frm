object frObjectInspector: TfrObjectInspector
  Left = 0
  Height = 240
  Top = 0
  Width = 360
  Anchors = [akTop, akLeft, akRight, akBottom]
  ClientHeight = 240
  ClientWidth = 360
  Color = clDefault
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
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    Visible = False
  end
  object imgDownArrow: TImage
    Left = 336
    Height = 9
    Top = 224
    Width = 15
    AutoSize = True
    Picture.Data = {
      07544269746D6170E6010000424DE60100000000000036000000280000000F00
      0000090000000100180000000000B00100000000000000000000000000000000
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFF1DE6B5EAD9991DE6B5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1DE6B5EAD99939841AEAD9991DE6B5
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFF1DE6
      B5EAD99939841A39841A39841AEAD9991DE6B5FFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFF1DE6B5EAD99939841A39841A39841A39841A39841A
      EAD9991DE6B5FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFF1DE6B5EAD9993984
      1A39841A39841A39841A39841A39841A39841AEAD9991DE6B5FFFFFFFFFFFF00
      0000FFFFFF1DE6B5EAD99939841A39841A39841A39841A39841A39841A39841A
      39841A39841AEAD9991DE6B5FFFFFF000000FFFFFFEAD999EAD999EAD999EAD9
      99EAD999EAD999EAD999EAD999EAD999EAD999EAD999EAD999EAD999FFFFFF00
      0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
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
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    TextHint = 'Search'
    Visible = False
    OnChange = edtSearchChange
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
