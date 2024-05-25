object frmInMemFileSystemBrowser: TfrmInMemFileSystemBrowser
  Left = 373
  Height = 240
  Top = 185
  Width = 737
  Caption = 'In-Mem FileSystem Browser'
  ClientHeight = 240
  ClientWidth = 737
  Constraints.MinHeight = 240
  Constraints.MinWidth = 737
  LCLVersion = '8.2'
  OnCreate = FormCreate
  OnResize = FormResize
  object pnlFileList: TPanel
    Left = 0
    Height = 242
    Top = 0
    Width = 320
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'pnlFileList'
    ClientHeight = 242
    ClientWidth = 320
    Constraints.MinHeight = 100
    Constraints.MinWidth = 100
    TabOrder = 0
    object vstFiles: TVirtualStringTree
      Left = 0
      Height = 240
      Top = 0
      Width = 320
      Anchors = [akTop, akLeft, akRight, akBottom]
      DefaultText = 'Node'
      Header.AutoSizeIndex = 0
      Header.Columns = <      
        item
          MinWidth = 270
          Position = 0
          Text = 'Name'
          Width = 270
        end      
        item
          MinWidth = 100
          Position = 1
          Text = 'Size [B]'
          Width = 100
        end      
        item
          MinWidth = 300
          Position = 2
          Text = 'Hash'
          Width = 300
        end>
      Header.Height = 21
      Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Header.Style = hsFlatButtons
      TabOrder = 0
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale, toDisableAutoscrollOnEdit]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnClick = vstFilesClick
      OnDblClick = vstFilesDblClick
      OnGetText = vstFilesGetText
      OnKeyUp = vstFilesKeyUp
    end
  end
  object pnlPreview: TPanel
    Left = 330
    Height = 242
    Top = 0
    Width = 406
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'pnlPreview'
    ClientHeight = 242
    ClientWidth = 406
    Color = 12582911
    ParentBackground = False
    ParentColor = False
    TabOrder = 1
    object pnlToolbar: TPanel
      Left = 0
      Height = 26
      Top = 0
      Width = 406
      Anchors = [akTop, akLeft, akRight]
      ClientHeight = 26
      ClientWidth = 406
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      object btnOK: TButton
        Left = 244
        Height = 25
        Top = 0
        Width = 75
        Anchors = [akTop, akRight]
        Caption = 'OK'
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 323
        Height = 25
        Top = 0
        Width = 75
        Anchors = [akTop, akRight]
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = btnCancelClick
      end
    end
    object scrboxPreview: TScrollBox
      Left = 0
      Height = 216
      Top = 24
      Width = 406
      HorzScrollBar.Page = 396
      VertScrollBar.Page = 208
      Anchors = [akTop, akLeft, akRight, akBottom]
      ClientHeight = 212
      ClientWidth = 402
      ParentBackground = False
      TabOrder = 1
      object pnlImg: TPanel
        Left = 0
        Height = 208
        Top = 0
        Width = 396
        Caption = 'Select a file to be displayed.'
        ClientHeight = 208
        ClientWidth = 396
        ParentBackground = False
        ParentColor = False
        TabOrder = 0
        object imgPreview: TImage
          Left = 0
          Height = 200
          Top = 0
          Width = 392
        end
      end
    end
  end
  object pnlHorizSplitterResults: TPanel
    Cursor = crHSplit
    Left = 320
    Height = 268
    Top = 0
    Width = 10
    Anchors = [akTop, akLeft, akBottom]
    Caption = 'ResultsSplitter'
    Color = 13041606
    Font.Color = 13041606
    Font.Height = -11
    Font.Name = 'Tahoma'
    ParentBackground = False
    ParentColor = False
    ParentFont = False
    TabOrder = 2
    OnMouseDown = pnlHorizSplitterResultsMouseDown
    OnMouseMove = pnlHorizSplitterResultsMouseMove
    OnMouseUp = pnlHorizSplitterResultsMouseUp
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 151
    Top = 66
  end
end
