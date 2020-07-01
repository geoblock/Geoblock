inherited fmDisplayTinOptions: TfmDisplayTinOptions
  Left = 487
  Top = 186
  HelpContext = 450
  Caption = 'Tin Options'
  ClientHeight = 474
  ClientWidth = 390
  ExplicitWidth = 396
  ExplicitHeight = 503
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 390
    Height = 201
    ExplicitWidth = 390
    ExplicitHeight = 201
    inherited GroupBoxAttribute: TGroupBox
      Width = 388
      Height = 159
      ExplicitWidth = 388
      ExplicitHeight = 159
      object CheckBoxByVertices: TCheckBox
        Left = 152
        Top = 128
        Width = 129
        Height = 17
        Caption = 'By vertices'
        TabOrder = 3
        Visible = False
        OnClick = CheckBoxByVerticesClick
      end
    end
    inherited GroupBoxModel: TGroupBox
      Width = 388
      ExplicitWidth = 388
    end
  end
  inherited PanelMiddle: TPanel
    Top = 297
    Width = 390
    Height = 129
    ExplicitTop = 297
    ExplicitWidth = 390
    ExplicitHeight = 129
    inherited RadioGroupSize: TRadioGroup
      Width = 388
      ExplicitWidth = 388
    end
    inherited SpinEditFactor: TSpinEdit
      Left = 96
      ExplicitLeft = 96
    end
    object GroupBoxFeature: TGroupBox
      Left = 1
      Top = 61
      Width = 388
      Height = 67
      Align = alBottom
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
        OnClick = ButtonContoursClick
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
        Caption = 'Vectors 3D...'
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
        OnClick = CheckBoxVectorsClick
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 426
    Width = 390
    ExplicitTop = 426
    ExplicitWidth = 390
    inherited ButtonOK: TButton
      Left = 49
      ExplicitLeft = 49
    end
    inherited ButtonCancel: TButton
      Left = 170
      ExplicitLeft = 170
    end
    inherited ButtonHelp: TButton
      Left = 284
      ExplicitLeft = 284
    end
  end
  inherited RadioGroupDetails: TRadioGroup
    Top = 201
    Width = 390
    Height = 96
    ExplicitTop = 201
    ExplicitWidth = 390
    ExplicitHeight = 96
  end
  inherited CheckBoxAsCylinder: TCheckBox
    Top = 264
    ExplicitTop = 264
  end
  inherited CheckBoxAsSphere: TCheckBox
    Top = 264
    Visible = False
    ExplicitTop = 264
  end
end
