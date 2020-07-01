inherited fmInterInverseDistance: TfmInterInverseDistance
  Left = 336
  Top = 234
  HelpContext = 3140
  Caption = 'Inverse Distance Options'
  ClientHeight = 190
  ClientWidth = 468
  OldCreateOrder = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 474
  ExplicitHeight = 219
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 468
    Height = 145
    BevelOuter = bvNone
    ExplicitWidth = 468
    ExplicitHeight = 145
    object GroupBoxMode: TGroupBox
      Left = 0
      Top = 0
      Width = 184
      Height = 145
      Align = alLeft
      Caption = 'Mode'
      TabOrder = 0
      object LabelPower: TLabel
        Left = 10
        Top = 32
        Width = 38
        Height = 16
        Caption = 'Power'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object SpinEditPower: TSpinEdit
        Left = 112
        Top = 29
        Width = 49
        Height = 26
        Hint = 'Power'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 3
        MinValue = 1
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Value = 1
      end
    end
    object RadioGroupSearch: TRadioGroup
      Left = 190
      Top = 0
      Width = 275
      Height = 111
      Caption = 'Search'
      ItemIndex = 0
      Items.Strings = (
        'All points'
        'Nearest points'
        'Points in sector')
      TabOrder = 1
      OnClick = RadioGroupSearchClick
    end
    object CheckBoxUseTriangulation: TCheckBox
      Left = 198
      Top = 117
      Width = 201
      Height = 17
      Caption = 'Use triangulation'
      TabOrder = 2
    end
    object SpinEditLocalPoints: TSpinEdit
      Left = 356
      Top = 77
      Width = 66
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object SpinEditGlobalPoints: TSpinEdit
      Left = 356
      Top = 45
      Width = 66
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 4
      Value = 0
    end
  end
  inherited PanelMiddle: TPanel
    Top = 145
    Width = 468
    Height = 0
    ExplicitTop = 145
    ExplicitWidth = 468
    ExplicitHeight = 0
  end
  inherited PanelBottom: TPanel
    Top = 145
    Width = 468
    Height = 45
    ExplicitTop = 145
    ExplicitWidth = 468
    ExplicitHeight = 45
    inherited ButtonOK: TButton
      Left = 147
      Top = 6
      OnClick = ButtonOKClick
      ExplicitLeft = 147
      ExplicitTop = 6
    end
    inherited ButtonCancel: TButton
      Left = 251
      Top = 6
      ExplicitLeft = 251
      ExplicitTop = 6
    end
    inherited ButtonHelp: TButton
      Left = 356
      Top = 6
      ExplicitLeft = 356
      ExplicitTop = 6
    end
    object ButtonReset: TButton
      Left = 25
      Top = 6
      Width = 96
      Height = 30
      Caption = 'Reset'
      Default = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = ButtonResetClick
    end
  end
end
