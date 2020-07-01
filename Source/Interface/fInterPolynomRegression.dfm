inherited fmInterPolynomialRegression: TfmInterPolynomialRegression
  Left = 289
  HelpContext = 3145
  Caption = 'Polynomial Regression Options'
  ClientHeight = 199
  ClientWidth = 547
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 553
  ExplicitHeight = 228
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 547
    Height = 1
    ExplicitWidth = 547
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 547
    Height = 150
    ExplicitTop = 1
    ExplicitWidth = 547
    ExplicitHeight = 150
    object RadioGroupSurface: TRadioGroup
      Left = 1
      Top = 1
      Width = 545
      Height = 148
      Align = alClient
      Caption = 'Surface'
      ItemIndex = 0
      Items.Strings = (
        'Planar'
        'Bi-linear'
        'Quadratic'
        'Cubic')
      TabOrder = 0
    end
    object PanelPlanar: TPanel
      Left = 176
      Top = 16
      Width = 353
      Height = 25
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = 'z(x,y) - A + Bx + Cy'
      TabOrder = 1
    end
    object PanelBiLinear: TPanel
      Left = 176
      Top = 48
      Width = 353
      Height = 25
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = 'z(x,y) - A + Bx + Cy + Dxy'
      TabOrder = 2
    end
    object PanelQuadratic: TPanel
      Left = 176
      Top = 80
      Width = 353
      Height = 25
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = 'z(x,y) - A + Bx + Cy + Dxi + Exy + Fyi'
      TabOrder = 3
    end
    object Panel1: TPanel
      Left = 176
      Top = 112
      Width = 353
      Height = 25
      Alignment = taLeftJustify
      BevelOuter = bvLowered
      Caption = 
        'z(x,y) - A + Bx + Cy + Dxi + Exy + Fyi + Gxi + Hxiy + Ixyi + Jyi' +
        ' '
      TabOrder = 4
    end
  end
  inherited PanelBottom: TPanel
    Top = 151
    Width = 547
    ExplicitTop = 151
    ExplicitWidth = 547
    inherited ButtonOK: TButton
      Left = 185
      ExplicitLeft = 185
    end
    inherited ButtonCancel: TButton
      Left = 306
      ExplicitLeft = 306
    end
    inherited ButtonHelp: TButton
      Left = 428
      ExplicitLeft = 428
    end
  end
end
