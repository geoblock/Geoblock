object fmFileEditGridPars: TfmFileEditGridPars
  Left = 295
  Top = 191
  Caption = 'Grid Parameters'
  ClientHeight = 308
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    436
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object EditFileName: TEdit
    Left = 0
    Top = 0
    Width = 436
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
    Text = 'EditFileName'
  end
  object MemoFileContents: TMemo
    Left = 0
    Top = 16
    Width = 436
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 267
    Width = 436
    Height = 22
    Align = alBottom
    TabOrder = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 289
    Width = 436
    Height = 19
    Panels = <>
  end
end
