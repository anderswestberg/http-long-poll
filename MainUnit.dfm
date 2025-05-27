object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'TestLongPolling'
  ClientHeight = 380
  ClientWidth = 420
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
  object LabelKey: TLabel
    Left = 16
    Top = 225
    Width = 22
    Height = 13
    Caption = 'Key:'
  end
  object LabelValue: TLabel
    Left = 200
    Top = 225
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object MemoLog: TMemo
    Left = 16
    Top = 16
    Width = 384
    Height = 200
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object EditKey: TEdit
    Left = 44
    Top = 222
    Width = 140
    Height = 21
    TabOrder = 1
  end
  object EditValue: TEdit
    Left = 240
    Top = 222
    Width = 160
    Height = 21
    TabOrder = 2
  end
  object BtnWriteKV: TButton
    Left = 320
    Top = 252
    Width = 80
    Height = 25
    Caption = 'Write Key/Val'
    TabOrder = 3
    OnClick = BtnWriteKVClick
  end
  object BtnStart: TButton
    Left = 16
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 4
    OnClick = BtnStartClick
  end
  object BtnStop: TButton
    Left = 104
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = BtnStopClick
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 200
    Top = 272
  end
end
