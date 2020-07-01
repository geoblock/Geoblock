object fmViewHorizon: TfmViewHorizon
  Left = 197
  Top = 199
  HelpContext = 511
  BorderStyle = bsDialog
  Caption = 'Horizon'
  ClientHeight = 265
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 120
  TextHeight = 16
  object PanelOKCancelHelp: TPanel
    Left = 0
    Top = 212
    Width = 597
    Height = 53
    Align = alBottom
    TabOrder = 0
    object OKBtn: TButton
      Left = 238
      Top = 10
      Width = 92
      Height = 31
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 350
      Top = 10
      Width = 92
      Height = 31
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object ButtonHelp: TButton
      Left = 462
      Top = 10
      Width = 92
      Height = 31
      HelpContext = 510
      Caption = '&Help'
      TabOrder = 2
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 41
    Width = 597
    Height = 171
    Align = alClient
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -13
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object PanelNavigator: TPanel
    Left = 0
    Top = 0
    Width = 597
    Height = 41
    Align = alTop
    TabOrder = 2
    object DBNavigator1: TDBNavigator
      Left = 8
      Top = 8
      Width = 240
      Height = 25
      TabOrder = 0
    end
    object Panel1: TPanel
      Left = 256
      Top = 1
      Width = 340
      Height = 39
      Align = alRight
      Caption = 'Horizon'
      TabOrder = 1
    end
  end
end
