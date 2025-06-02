object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Key-Value Client Demo'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 89
    Width = 624
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 41
    ExplicitWidth = 447
  end
  object MemoLog: TMemo
    Left = 0
    Top = 92
    Width = 624
    Height = 349
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object LabelKey: TLabel
      Left = 16
      Top = 16
      Width = 19
      Height = 13
      Caption = 'Key'
    end
    object LabelValue: TLabel
      Left = 16
      Top = 43
      Width = 26
      Height = 13
      Caption = 'Value'
    end
    object LabelStatus: TLabel
      Left = 16
      Top = 70
      Width = 31
      Height = 13
      Caption = 'Status'
    end
    object LabelInstanceInfo: TLabel
      Left = 280
      Top = 70
      Width = 71
      Height = 13
      Caption = 'Instance Info'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object EditKey: TEdit
      Left = 56
      Top = 13
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object EditValue: TEdit
      Left = 56
      Top = 40
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object BtnWrite: TButton
      Left = 183
      Top = 11
      Width = 75
      Height = 25
      Caption = 'Write'
      Enabled = False
      TabOrder = 2
      OnClick = BtnWriteClick
    end
    object BtnRead: TButton
      Left = 183
      Top = 38
      Width = 75
      Height = 25
      Caption = 'Read'
      Enabled = False
      TabOrder = 3
      OnClick = BtnReadClick
    end
    object BtnStartStressTest: TButton
      Left = 280
      Top = 11
      Width = 89
      Height = 25
      Caption = 'Start Stress Test'
      Enabled = False
      TabOrder = 4
      OnClick = BtnStartStressTestClick
    end
    object BtnStopStressTest: TButton
      Left = 280
      Top = 38
      Width = 89
      Height = 25
      Caption = 'Stop Stress Test'
      Enabled = False
      TabOrder = 5
      OnClick = BtnStopStressTestClick
    end
  end
end
