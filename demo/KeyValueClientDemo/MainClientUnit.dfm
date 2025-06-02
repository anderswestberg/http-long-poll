object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Key-Value Client Demo'
  ClientHeight = 441
  ClientWidth = 715
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 105
    Width = 715
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 89
    ExplicitWidth = 624
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 715
    Height = 105
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelKey: TLabel
      Left = 8
      Top = 16
      Width = 22
      Height = 15
      Caption = 'Key:'
    end
    object LabelValue: TLabel
      Left = 168
      Top = 16
      Width = 31
      Height = 15
      Caption = 'Value:'
    end
    object LabelPort: TLabel
      Left = 8
      Top = 45
      Width = 25
      Height = 15
      Caption = 'Port:'
    end
    object LabelClientId: TLabel
      Left = 168
      Top = 45
      Width = 48
      Height = 15
      Caption = 'Client ID:'
    end
    object LabelLogLevel: TLabel
      Left = 531
      Top = 15
      Width = 53
      Height = 15
      Caption = 'Log Level:'
    end
    object LabelStatus: TLabel
      Left = 8
      Top = 70
      Width = 35
      Height = 15
      Caption = 'Status:'
    end
    object LabelInstanceInfo: TLabel
      Left = 280
      Top = 70
      Width = 68
      Height = 15
      Caption = 'Instance Info'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object EditKey: TEdit
      Left = 35
      Top = 13
      Width = 121
      Height = 23
      TabOrder = 0
      Text = 'P15.PressureOffset'
    end
    object EditValue: TEdit
      Left = 206
      Top = 13
      Width = 121
      Height = 23
      TabOrder = 1
    end
    object EditPort: TEdit
      Left = 41
      Top = 42
      Width = 115
      Height = 23
      TabOrder = 2
      Text = '8868'
    end
    object EditClientId: TEdit
      Left = 221
      Top = 42
      Width = 106
      Height = 23
      TabOrder = 3
    end
    object BtnWrite: TButton
      Left = 341
      Top = 39
      Width = 69
      Height = 25
      Caption = 'Write'
      Enabled = False
      TabOrder = 4
      OnClick = BtnWriteClick
    end
    object BtnRead: TButton
      Left = 341
      Top = 11
      Width = 69
      Height = 25
      Caption = 'Read'
      Enabled = False
      TabOrder = 5
      OnClick = BtnReadClick
    end
    object BtnConnect: TButton
      Left = 525
      Top = 41
      Width = 89
      Height = 25
      Caption = 'Connect'
      TabOrder = 6
      OnClick = BtnConnectClick
    end
    object BtnDisconnect: TButton
      Left = 620
      Top = 41
      Width = 69
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 7
      OnClick = BtnDisconnectClick
    end
    object CheckBoxLogging: TCheckBox
      Left = 695
      Top = 18
      Width = 12
      Height = 17
      TabOrder = 8
      OnClick = CheckBoxLoggingClick
    end
    object ComboBoxLogLevel: TComboBox
      Left = 591
      Top = 12
      Width = 98
      Height = 23
      Style = csDropDownList
      TabOrder = 9
      OnChange = ComboBoxLogLevelChange
    end
    object BtnStartStressTest: TButton
      Left = 525
      Top = 72
      Width = 89
      Height = 25
      Caption = 'Start Stress Test'
      Enabled = False
      TabOrder = 10
      OnClick = BtnStartStressTestClick
    end
    object BtnStopStressTest: TButton
      Left = 620
      Top = 72
      Width = 69
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 11
      OnClick = BtnStopStressTestClick
    end
  end
  object MemoLog: TMemo
    Left = 0
    Top = 108
    Width = 715
    Height = 333
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitTop = 111
  end
end
