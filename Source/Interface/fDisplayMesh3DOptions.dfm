inherited fmDisplayMesh3DOptions: TfmDisplayMesh3DOptions
  Left = 537
  Top = 155
  HelpContext = 492
  Caption = 'Mesh 3D Options'
  ClientHeight = 445
  ExplicitHeight = 474
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    inherited GroupBoxModel: TGroupBox
      inherited StaticTextModel: TStaticText
        Width = 379
        Height = 20
        ExplicitWidth = 379
        ExplicitHeight = 20
      end
    end
  end
  inherited PanelMiddle: TPanel
    Height = 144
    ExplicitHeight = 144
    object GroupBoxFeature: TGroupBox
      Left = 1
      Top = 73
      Width = 383
      Height = 70
      Align = alClient
      Caption = 'Show'
      TabOrder = 2
      object CheckBoxIsosurface: TCheckBox
        Left = 24
        Top = 32
        Width = 17
        Height = 17
        TabOrder = 0
        OnClick = CheckBoxIsosurfaceClick
      end
      object ButtonIsosurface: TButton
        Left = 48
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 496
        Caption = 'Isosurfaces...'
        TabOrder = 3
        OnClick = ButtonIsosurfaceClick
      end
      object CheckBoxVectors: TCheckBox
        Left = 208
        Top = 32
        Width = 17
        Height = 17
        Hint = 'Features of vectors'
        HelpContext = 400
        Caption = '&Vector...'
        Enabled = False
        TabOrder = 2
        OnClick = CheckBoxVectorsClick
      end
      object ButtonVectors: TButton
        Left = 232
        Top = 24
        Width = 137
        Height = 25
        HelpContext = 498
        Caption = 'Vectors 3D...'
        TabOrder = 1
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 397
    ExplicitTop = 397
  end
end
