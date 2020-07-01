inherited fmOptionDialog: TfmOptionDialog
  Tag = 1
  Left = 500
  Top = 144
  ClientHeight = 458
  ClientWidth = 385
  OldCreateOrder = True
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 391
  ExplicitHeight = 487
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 385
    Height = 169
    ExplicitWidth = 385
    ExplicitHeight = 169
    object GroupBoxAttribute: TGroupBox
      Left = 1
      Top = 41
      Width = 383
      Height = 127
      Align = alClient
      Caption = 'Attribute'
      TabOrder = 0
      object LabelNumeric: TLabel
        Left = 16
        Top = 24
        Width = 50
        Height = 16
        Caption = 'Numeric'
      end
      object SpeedButtonLegend: TSpeedButton
        Left = 160
        Top = 16
        Width = 23
        Height = 22
        Hint = 'Legend'
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777700000007777777770EEEEE07770777770EEEEE077077777700000007770
          0777777777777777777770000000777777777099999077077077709999907700
          707770000000770770777777777777777777700000007777777770BBBBB07770
          777770BBBBB07770077770000000777077777777777777777777}
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButtonLegendClick
      end
      object SpeedButtonFont: TSpeedButton
        Left = 344
        Top = 16
        Width = 23
        Height = 22
        Hint = 'Font'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7777777770666607777777777706607777777777777007770177777777700777
          7777777777700777077777777770077701777777777007777017777777700770
          7087777777700778087777777770077777777077777007777707700777700777
          7007706666666666660770000000000000077777777777777777}
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButtonFontClick
      end
      object ListBoxNumericAttributes: TListBox
        Left = 16
        Top = 40
        Width = 169
        Height = 73
        TabOrder = 0
        OnClick = ListBoxNumericAttributesClick
      end
      object ListBoxTextAttributes: TListBox
        Left = 200
        Top = 40
        Width = 169
        Height = 73
        TabOrder = 1
        OnClick = ListBoxTextAttributesClick
      end
      object CheckBoxTextAttributes: TCheckBox
        Left = 204
        Top = 24
        Width = 104
        Height = 17
        Caption = 'Text'
        TabOrder = 2
        OnClick = CheckBoxTextAttributesClick
      end
    end
    object GroupBoxModel: TGroupBox
      Left = 1
      Top = 1
      Width = 383
      Height = 40
      Align = alTop
      Caption = 'Model'
      TabOrder = 1
      object StaticTextModel: TStaticText
        Left = 2
        Top = 18
        Width = 4
        Height = 4
        Align = alClient
        BorderStyle = sbsSunken
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 253
    Width = 385
    Height = 157
    AutoSize = True
    ExplicitTop = 253
    ExplicitWidth = 385
    ExplicitHeight = 157
    object RadioGroupSize: TRadioGroup
      Left = 1
      Top = 1
      Width = 383
      Height = 72
      Align = alTop
      Caption = 'Size'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Factor'
        'Value')
      TabOrder = 0
      OnClick = RadioGroupSizeClick
    end
    object SpinEditFactor: TSpinEdit
      Left = 112
      Top = 24
      Width = 49
      Height = 26
      MaxValue = 10
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
  end
  inherited PanelBottom: TPanel
    Top = 410
    Width = 385
    ExplicitTop = 410
    ExplicitWidth = 385
    DesignSize = (
      385
      48)
    inherited ButtonOK: TButton
      Left = 44
      ExplicitLeft = 44
    end
    inherited ButtonCancel: TButton
      Left = 165
      ExplicitLeft = 165
    end
    inherited ButtonHelp: TButton
      Left = 279
      ExplicitLeft = 279
    end
  end
  object RadioGroupDetails: TRadioGroup
    Left = 0
    Top = 169
    Width = 385
    Height = 84
    Align = alTop
    Caption = 'Details'
    Columns = 3
    Ctl3D = True
    ItemIndex = 0
    Items.Strings = (
      'Point'
      'Line'
      'Fill')
    ParentCtl3D = False
    TabOrder = 3
    OnClick = RadioGroupDetailsClick
  end
  object CheckBoxAsCylinder: TCheckBox
    Left = 136
    Top = 232
    Width = 105
    Height = 17
    Caption = 'As cylinder'
    TabOrder = 4
    Visible = False
  end
  object CheckBoxAsSphere: TCheckBox
    Left = 8
    Top = 232
    Width = 105
    Height = 17
    Caption = 'As sphere'
    TabOrder = 5
  end
end
