inherited fmDisplayPoints3DOptions: TfmDisplayPoints3DOptions
  Left = 519
  Top = 73
  HelpContext = 430
  Caption = 'Points 3D Options'
  ClientHeight = 473
  ClientWidth = 393
  ExplicitWidth = 399
  ExplicitHeight = 502
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 393
    ExplicitWidth = 393
    inherited GroupBoxAttribute: TGroupBox
      Width = 391
      ExplicitWidth = 391
    end
    inherited GroupBoxModel: TGroupBox
      Width = 391
      ExplicitWidth = 391
      inherited StaticTextModel: TStaticText
        Width = 387
        Height = 20
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 265
    Width = 393
    Height = 160
    ExplicitTop = 265
    ExplicitWidth = 393
    ExplicitHeight = 160
    inherited RadioGroupSize: TRadioGroup
      Width = 391
      Height = 80
      ExplicitWidth = 391
      ExplicitHeight = 80
    end
  end
  inherited PanelBottom: TPanel
    Top = 425
    Width = 393
    ExplicitTop = 425
    ExplicitWidth = 393
    inherited ButtonOK: TButton
      Left = 52
      ExplicitLeft = 52
    end
    inherited ButtonCancel: TButton
      Left = 173
      ExplicitLeft = 173
    end
    inherited ButtonHelp: TButton
      Left = 287
      ExplicitLeft = 287
    end
  end
  inherited RadioGroupDetails: TRadioGroup
    Width = 393
    Height = 96
    Items.Strings = (
      'Point')
    ExplicitWidth = 393
    ExplicitHeight = 96
  end
  inherited CheckBoxAsCylinder: TCheckBox
    Top = 240
    ExplicitTop = 240
  end
  inherited CheckBoxAsSphere: TCheckBox
    Top = 240
    ExplicitTop = 240
  end
end
