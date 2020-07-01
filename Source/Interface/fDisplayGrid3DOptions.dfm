inherited fmDisplayGrid3DOptions: TfmDisplayGrid3DOptions
  Left = 460
  HelpContext = 480
  Caption = '3D Grid Options'
  ClientHeight = 504
  ClientWidth = 389
  ExplicitWidth = 395
  ExplicitHeight = 533
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 389
    Height = 177
    ExplicitWidth = 389
    ExplicitHeight = 177
    inherited GroupBoxAttribute: TGroupBox
      Width = 387
      Height = 135
      ExplicitWidth = 387
      ExplicitHeight = 135
      inherited CheckBoxTextAttributes: TCheckBox
        TabOrder = 3
      end
      object CheckBoxByVertices: TCheckBox
        Left = 144
        Top = 115
        Width = 129
        Height = 17
        Caption = 'By vertices'
        TabOrder = 2
        Visible = False
      end
    end
    inherited GroupBoxModel: TGroupBox
      Width = 387
      ExplicitWidth = 387
      inherited StaticTextModel: TStaticText
        Width = 383
        Height = 20
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 261
    Width = 389
    Height = 195
    ExplicitTop = 261
    ExplicitWidth = 389
    ExplicitHeight = 195
    inherited RadioGroupSize: TRadioGroup
      Width = 387
      ExplicitWidth = 387
    end
    object GroupBoxFeature: TGroupBox
      Left = 1
      Top = 73
      Width = 387
      Height = 121
      Align = alClient
      Caption = 'Show'
      TabOrder = 2
      object CheckBoxIsosurface: TCheckBox
        Left = 24
        Top = 56
        Width = 17
        Height = 17
        TabOrder = 0
        OnClick = CheckBoxIsosurfaceClick
      end
      object ButtonVectors: TButton
        Left = 231
        Top = 48
        Width = 137
        Height = 25
        HelpContext = 498
        Caption = 'Vectors 3D...'
        TabOrder = 1
      end
      object CheckBoxVectors: TCheckBox
        Left = 208
        Top = 56
        Width = 17
        Height = 17
        Hint = 'Features of vectors'
        HelpContext = 400
        Caption = '&Vector...'
        Enabled = False
        TabOrder = 2
        OnClick = CheckBoxVectorsClick
      end
      object ButtonIsosurface: TButton
        Left = 47
        Top = 48
        Width = 137
        Height = 25
        HelpContext = 496
        Caption = 'Isosurfaces...'
        TabOrder = 3
        OnClick = ButtonIsosurfaceClick
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 456
    Width = 389
    ExplicitTop = 456
    ExplicitWidth = 389
    inherited ButtonOK: TButton
      Left = 48
      ExplicitLeft = 48
    end
    inherited ButtonCancel: TButton
      Left = 169
      ExplicitLeft = 169
    end
    inherited ButtonHelp: TButton
      Left = 283
      ExplicitLeft = 283
    end
  end
  inherited RadioGroupDetails: TRadioGroup
    Top = 177
    Width = 389
    ExplicitTop = 177
    ExplicitWidth = 389
  end
end
