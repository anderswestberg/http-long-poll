object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Key/Value Store Server'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object LabelKey: TLabel
    Left = 8
    Top = 371
    Width = 21
    Height = 15
    Caption = 'Key:'
  end
  object LabelValue: TLabel
    Left = 168
    Top = 371
    Width = 32
    Height = 15
    Caption = 'Value:'
  end
  object LabelLogLevel: TLabel
    Left = 328
    Top = 371
    Width = 54
    Height = 15
    Caption = 'Log Level:'
  end
  object MemoLog: TMemo
    Left = 8
    Top = 8
    Width = 608
    Height = 353
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object BtnStart: TButton
    Left = 8
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = BtnStartClick
  end
  object BtnStop: TButton
    Left = 89
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = BtnStopClick
  end
  object EditKey: TEdit
    Left = 35
    Top = 367
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EditValue: TEdit
    Left = 206
    Top = 367
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object BtnWriteKV: TButton
    Left = 170
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Write K/V'
    TabOrder = 5
    OnClick = BtnWriteKVClick
  end
  object Button1: TButton
    Left = 251
    Top = 408
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 6
    OnClick = Button1Click
  end
  object CheckBoxLogging: TCheckBox
    Left = 388
    Top = 408
    Width = 97
    Height = 17
    Caption = 'Enable Logging'
    TabOrder = 7
    OnClick = CheckBoxLoggingClick
  end
  object ComboBoxLogLevel: TComboBox
    Left = 388
    Top = 367
    Width = 145
    Height = 23
    Style = csDropDownList
    TabOrder = 8
    OnChange = ComboBoxLogLevelChange
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 576
    Top = 400
  end
end
