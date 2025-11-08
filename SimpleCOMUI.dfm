object frSimpleCOMUI: TfrSimpleCOMUI
  Left = 0
  Top = 0
  Width = 310
  Height = 90
  TabOrder = 0
  TabStop = True
  object lblCOMNumber: TLabel
    Left = 8
    Top = 0
    Width = 63
    Height = 13
    Caption = 'COM Number'
  end
  object lblCOMStatus: TLabel
    Left = 88
    Top = 48
    Width = 119
    Height = 13
    Hint = 'Status is updated on connect/disconnect.'
    Caption = 'Status: Disconnected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object lblBaudRate: TLabel
    Left = 103
    Top = 0
    Width = 47
    Height = 13
    Caption = 'Baud rate'
  end
  object lblStatusMsg: TLabel
    Left = 8
    Top = 70
    Width = 23
    Height = 13
    Caption = 'Msg:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object cmbCOMPort: TComboBox
    Left = 8
    Top = 16
    Width = 89
    Height = 22
    Style = csOwnerDrawFixed
    DropDownCount = 10
    ItemHeight = 16
    TabOrder = 0
    OnDropDown = cmbCOMPortDropDown
  end
  object btnConnect: TButton
    Left = 224
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 224
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 2
    OnClick = btnDisconnectClick
  end
  object cmbBaud: TComboBox
    Left = 104
    Top = 16
    Width = 82
    Height = 22
    Style = csOwnerDrawFixed
    DropDownCount = 10
    ItemHeight = 16
    ItemIndex = 20
    TabOrder = 3
    Text = '256000'
    Items.Strings = (
      '75'
      '110'
      '134'
      '150'
      '300'
      '600'
      '1200'
      '1800'
      '2400'
      '4800'
      '7200'
      '9600'
      '14400'
      '19200'
      '38400'
      '56000'
      '57600'
      '115200'
      '128000'
      '230400'
      '256000'
      '460800'
      '921600')
  end
  object chkShowAll: TCheckBox
    Left = 8
    Top = 45
    Width = 64
    Height = 19
    Hint = 'Shows all possible COM numbers'
    Caption = 'Show All'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
end
