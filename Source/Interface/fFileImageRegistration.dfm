inherited fmFileImageRegistration: TfmFileImageRegistration
  Left = 284
  Top = 129
  Caption = 'Image Registration'
  ClientHeight = 485
  OldCreateOrder = True
  ExplicitHeight = 514
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 193
    ExplicitHeight = 193
    object DBGrid1: TDBGrid
      Left = 40
      Top = 48
      Width = 441
      Height = 120
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  inherited PanelMiddle: TPanel
    Top = 193
    Height = 244
    ExplicitTop = 193
    ExplicitHeight = 244
    object Image: TImage
      Left = 1
      Top = 1
      Width = 565
      Height = 242
      Align = alClient
      Center = True
    end
  end
  inherited PanelBottom: TPanel
    Top = 437
    ExplicitTop = 437
    object SpeedButtonPlus: TSpeedButton [0]
      Left = 48
      Top = 16
      Width = 23
      Height = 22
      Caption = '+'
    end
    object SpeedButtonMinus: TSpeedButton [1]
      Left = 80
      Top = 16
      Width = 23
      Height = 22
      Caption = '-'
    end
  end
end
