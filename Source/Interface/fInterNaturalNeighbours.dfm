inherited fmInterNaturalNeighbours: TfmInterNaturalNeighbours
  Left = 382
  Top = 238
  HelpContext = 3144
  Caption = 'Natural Neighbors Options'
  ClientHeight = 260
  ClientWidth = 418
  OldCreateOrder = True
  ExplicitWidth = 424
  ExplicitHeight = 289
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 418
    Height = 1
    ExplicitWidth = 418
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 418
    Height = 211
    ExplicitTop = 1
    ExplicitWidth = 418
    ExplicitHeight = 211
    object RadioGroupNodeFunction: TRadioGroup
      Left = 1
      Top = 1
      Width = 216
      Height = 128
      Align = alLeft
      Caption = 'Node function'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Constant'
        'Gradient '
        'Quadratic ')
      ParentFont = False
      TabOrder = 0
    end
    object RadioGroupCoefficient: TRadioGroup
      Left = 217
      Top = 1
      Width = 224
      Height = 128
      Align = alLeft
      Caption = 'Coefficient'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Neighbors'
        'Near points'
        'All points')
      ParentFont = False
      TabOrder = 1
    end
    object GroupBoxFrame: TGroupBox
      Left = 1
      Top = 129
      Width = 416
      Height = 81
      Align = alBottom
      Caption = 'Frame'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      object LabelOverConvexHull: TLabel
        Left = 16
        Top = 40
        Width = 98
        Height = 16
        Caption = 'Over convex hull'
      end
      object SpinEdit1: TSpinEdit
        Left = 208
        Top = 32
        Width = 65
        Height = 26
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object StaticTextPercent: TStaticText
        Left = 288
        Top = 40
        Width = 16
        Height = 20
        Caption = '%'
        TabOrder = 1
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 212
    Width = 418
    ExplicitTop = 212
    ExplicitWidth = 418
    inherited ButtonOK: TButton
      Left = 72
      ExplicitLeft = 72
    end
    inherited ButtonCancel: TButton
      Left = 185
      ExplicitLeft = 185
    end
    inherited ButtonHelp: TButton
      Left = 299
      ExplicitLeft = 299
    end
  end
end
