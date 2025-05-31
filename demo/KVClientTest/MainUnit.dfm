object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'KV Client Test'
  ClientHeight = 387
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 115
    Width = 532
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitTop = 77
  end
  object MemoLog: TMemo
    Left = 0
    Top = 120
    Width = 532
    Height = 267
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 532
    Height = 115
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 75
    object LabelKey: TLabel
      Left = 16
      Top = 16
      Width = 22
      Height = 13
      Caption = 'Key:'
    end
    object LabelValue: TLabel
      Left = 200
      Top = 16
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object EditKey: TEdit
      Left = 44
      Top = 13
      Width = 120
      Height = 21
      TabOrder = 0
      Text = 'P15.PressureOffset'
    end
    object EditValue: TEdit
      Left = 240
      Top = 13
      Width = 160
      Height = 21
      TabOrder = 1
    end
    object BtnWrite: TButton
      Left = 320
      Top = 40
      Width = 80
      Height = 25
      Caption = 'Write Key/Val'
      TabOrder = 2
      OnClick = BtnWriteClick
    end
    object BtnRead: TButton
      Left = 420
      Top = 40
      Width = 80
      Height = 25
      Caption = 'Read Key'
      TabOrder = 3
      OnClick = BtnReadClick
    end
    object BtnStartStressTest: TButton
      Left = 44
      Top = 71
      Width = 120
      Height = 25
      Caption = 'Start Stress Test'
      TabOrder = 4
      OnClick = BtnStartStressTestClick
    end
    object BtnStopStressTest: TButton
      Left = 180
      Top = 71
      Width = 120
      Height = 25
      Caption = 'Stop Stress Test'
      Enabled = False
      TabOrder = 5
      OnClick = BtnStopStressTestClick
    end
  end
  object StressTestTimer: TTimer
    Enabled = False
    OnTimer = StressTestTimerTimer
    Left = 480
    Top = 8
  end
end
