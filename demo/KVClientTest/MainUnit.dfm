object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'KV Client Test'
  ClientHeight = 330
  ClientWidth = 611
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
  object MemoLog: TMemo
    Left = 16
    Top = 88
    Width = 577
    Height = 220
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object EditKey: TEdit
    Left = 44
    Top = 13
    Width = 120
    Height = 21
    TabOrder = 1
  end
  object EditValue: TEdit
    Left = 240
    Top = 13
    Width = 160
    Height = 21
    TabOrder = 2
  end
  object BtnWrite: TButton
    Left = 320
    Top = 40
    Width = 80
    Height = 25
    Caption = 'Write Key/Val'
    TabOrder = 3
    OnClick = BtnWriteClick
  end
  object BtnRead: TButton
    Left = 420
    Top = 40
    Width = 80
    Height = 25
    Caption = 'Read Key'
    TabOrder = 4
    OnClick = BtnReadClick
  end
  object BtnStartLongPoll: TButton
    Left = 44
    Top = 40
    Width = 120
    Height = 25
    Caption = 'Start Long Poll'
    TabOrder = 5
    OnClick = BtnStartLongPollClick
  end
  object BtnStopLongPoll: TButton
    Left = 180
    Top = 40
    Width = 120
    Height = 25
    Caption = 'Stop Long Poll'
    TabOrder = 6
    OnClick = BtnStopLongPollClick
  end
end
