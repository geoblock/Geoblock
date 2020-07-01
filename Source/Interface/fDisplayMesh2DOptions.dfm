inherited fmDisplayMesh2DOptions: TfmDisplayMesh2DOptions
  Left = 522
  Top = 124
  HelpContext = 490
  Caption = 'Mesh 2D Options'
  ClientHeight = 473
  ClientWidth = 387
  ExplicitWidth = 393
  ExplicitHeight = 502
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 387
    ExplicitWidth = 387
    inherited GroupBoxAttribute: TGroupBox
      Width = 385
      ExplicitWidth = 385
    end
    inherited GroupBoxModel: TGroupBox
      Width = 385
      ExplicitWidth = 385
      inherited StaticTextModel: TStaticText
        Width = 381
        Height = 20
        ExplicitWidth = 381
        ExplicitHeight = 20
      end
    end
  end
  inherited PanelMiddle: TPanel
    Width = 387
    Height = 172
    ExplicitWidth = 387
    ExplicitHeight = 172
    inherited RadioGroupSize: TRadioGroup
      Width = 385
      ExplicitWidth = 385
    end
    object GroupBoxFeature: TGroupBox
      Left = 1
      Top = 73
      Width = 385
      Height = 98
      Align = alClient
      Caption = 'Show'
      TabOrder = 2
      object ButtonContours: TButton
        Left = 48
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 495
        Caption = 'Isolines...'
        TabOrder = 0
      end
      object CheckBoxContours: TCheckBox
        Left = 24
        Top = 32
        Width = 17
        Height = 17
        TabOrder = 1
      end
      object ButtonVectors: TButton
        Left = 231
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 498
        Caption = 'Vectors 2D...'
        TabOrder = 2
      end
      object CheckBoxVectors: TCheckBox
        Left = 208
        Top = 32
        Width = 17
        Height = 17
        HelpContext = 498
        TabOrder = 3
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 425
    Width = 387
    ExplicitTop = 425
    ExplicitWidth = 387
    inherited ButtonOK: TButton
      Left = 46
      ExplicitLeft = 46
    end
    inherited ButtonCancel: TButton
      Left = 167
      ExplicitLeft = 167
    end
    inherited ButtonHelp: TButton
      Left = 281
      ExplicitLeft = 281
    end
  end
  inherited RadioGroupDetails: TRadioGroup
    Width = 387
    ItemIndex = 2
    ExplicitWidth = 387
  end
end
