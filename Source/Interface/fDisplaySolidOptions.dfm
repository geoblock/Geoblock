inherited fmDisplaySolidsOptions: TfmDisplaySolidsOptions
  Left = 529
  Top = 128
  HelpContext = 460
  Caption = 'Solids Options'
  ClientHeight = 455
  ExplicitWidth = 320
  ExplicitHeight = 484
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelMiddle: TPanel
    Height = 154
    ExplicitHeight = 154
    object GroupBoxFeature: TGroupBox
      Left = 1
      Top = 73
      Width = 383
      Height = 80
      Align = alClient
      Caption = 'Show'
      TabOrder = 2
      object ButtonContours: TButton
        Left = 48
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 495
        TabOrder = 0
      end
      object CheckBoxContours: TCheckBox
        Left = 24
        Top = 32
        Width = 17
        Height = 17
        Hint = 'Show contours'
        TabOrder = 1
        OnClick = CheckBoxContoursClick
      end
      object ButtonVectors: TButton
        Left = 232
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 498
        TabOrder = 2
      end
      object CheckBoxVectors: TCheckBox
        Left = 208
        Top = 32
        Width = 17
        Height = 17
        Hint = 'Show vectors'
        HelpContext = 400
        TabOrder = 3
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 407
    ExplicitTop = 407
  end
end
