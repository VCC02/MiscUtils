object frmAutoComplete: TfrmAutoComplete
  Left = 387
  Height = 313
  Top = 43
  Width = 416
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Autocomplete'
  ClientHeight = 313
  ClientWidth = 416
  FormStyle = fsStayOnTop
  OnClose = FormClose
  Position = poDesktopCenter
  LCLVersion = '7.5'
  object vstIdentifiers: TVirtualStringTree
    Left = 0
    Height = 288
    Top = 0
    Width = 416
    Anchors = [akTop, akLeft, akRight, akBottom]
    Colors.UnfocusedColor = clMedGray
    DefaultNodeHeight = 16
    DefaultText = 'Node'
    Font.Height = -13
    Font.Name = 'Courier New'
    Header.AutoSizeIndex = 0
    Header.Columns = <    
      item
        MinWidth = 50
        Position = 0
        Text = 'Type'
      end    
      item
        MinWidth = 500
        Position = 1
        Text = 'Definition'
        Width = 500
      end>
    Header.DefaultHeight = 17
    Indent = 4
    ParentFont = False
    TabOrder = 0
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnDblClick = vstIdentifiersDblClick
    OnDrawText = vstIdentifiersDrawText
    OnExit = vstIdentifiersExit
    OnGetText = vstIdentifiersGetText
    OnPaintText = vstIdentifiersPaintText
    OnKeyDown = vstIdentifiersKeyDown
    OnKeyUp = vstIdentifiersKeyUp
  end
  object StatusBar1: TStatusBar
    Left = 0
    Height = 23
    Top = 290
    Width = 416
    Panels = <>
  end
end
