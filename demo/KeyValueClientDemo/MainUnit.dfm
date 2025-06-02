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
    Top = 337
    Width = 624
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 0
    ExplicitWidth = 0
  end
  object MemoLog: TMemo
    Left = 0
    Top = 340
    Width = 624
    Height = 101
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 337
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object LabelKey: TLabel
      Left = 16
      Top = 16
      Width = 18
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
      Top = 80
      Width = 585
      Height = 13
      AutoSize = False
      Caption = 'Status: Disconnected'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object EditKey: TEdit
      Left = 56
      Top = 13
      Width = 545
      Height = 21
      TabOrder = 0
    end
    object EditValue: TEdit
      Left = 56
      Top = 40
      Width = 545
      Height = 21
      TabOrder = 1
    end
    object BtnWrite: TButton
      Left = 56
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Write'
      Enabled = False
      TabOrder = 2
      OnClick = BtnWriteClick
    end
    object BtnRead: TButton
      Left = 137
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Read'
      Enabled = False
      TabOrder = 3
      OnClick = BtnReadClick
    end
    object BtnStartStressTest: TButton
      Left = 56
      Top = 152
      Width = 75
      Height = 25
      Caption = 'Start Test'
      Enabled = False
      TabOrder = 4
      OnClick = BtnStartStressTestClick
    end
    object BtnStopStressTest: TButton
      Left = 137
      Top = 152
      Width = 75
      Height = 25
      Caption = 'Stop Test'
      Enabled = False
      TabOrder = 5
      OnClick = BtnStopStressTestClick
    end
  end
end
