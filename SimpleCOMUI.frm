object frSimpleCOMUI: TfrSimpleCOMUI
  Left = 0
  Height = 90
  Top = 0
  Width = 310
  ClientHeight = 90
  ClientWidth = 310
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object lblCOMNumber: TLabel
    Left = 8
    Height = 15
    Top = 0
    Width = 75
    Caption = 'COM Number'
  end
  object lblCOMStatus: TLabel
    Left = 88
    Height = 13
    Hint = 'Status is updated on connect/disconnect.'
    Top = 48
    Width = 119
    Caption = 'Status: Disconnected'
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
    Height = 15
    Top = 0
    Width = 50
    Caption = 'Baud rate'
  end
  object lblStatusMsg: TLabel
    Left = 8
    Height = 13
    Top = 70
    Width = 23
    Caption = 'Msg:'
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object cmbCOMPort: TComboBox
    Left = 8
    Height = 22
    Top = 16
    Width = 89
    AutoSize = False
    DropDownCount = 10
    ItemHeight = 16
    Style = csOwnerDrawFixed
    TabOrder = 0
    OnDropDown = cmbCOMPortDropDown
  end
  object btnConnect: TButton
    Left = 224
    Height = 25
    Top = 16
    Width = 75
    Caption = 'Connect'
    TabOrder = 1
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 224
    Height = 25
    Top = 47
    Width = 75
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 2
    OnClick = btnDisconnectClick
  end
  object cmbBaud: TComboBox
    Left = 104
    Height = 22
    Top = 16
    Width = 82
    AutoSize = False
    DropDownCount = 10
    ItemHeight = 16
    ItemIndex = 20
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
      '921600'
    )
    Style = csOwnerDrawFixed
    TabOrder = 3
    Text = '256000'
  end
  object chkShowAll: TCheckBox
    Left = 8
    Height = 19
    Hint = 'Shows all possible COM numbers'
    Top = 45
    Width = 64
    Caption = 'Show All'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
  end
end
