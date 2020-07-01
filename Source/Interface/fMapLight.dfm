inherited fmMapLighting: TfmMapLighting
  Left = 436
  Top = 303
  HelpContext = 405
  Caption = 'Lighting'
  ClientHeight = 221
  ClientWidth = 499
  OldCreateOrder = True
  ExplicitWidth = 505
  ExplicitHeight = 250
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 499
    Height = 1
    ExplicitWidth = 499
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 499
    Height = 172
    ExplicitTop = 1
    ExplicitWidth = 499
    ExplicitHeight = 172
    object GroupBoxPosition: TGroupBox
      Left = 1
      Top = 1
      Width = 241
      Height = 170
      Align = alLeft
      Caption = 'Position'
      TabOrder = 0
      object Label2: TLabel
        Left = 56
        Top = 40
        Width = 8
        Height = 16
        Caption = 'X'
      end
      object Label3: TLabel
        Left = 56
        Top = 80
        Width = 9
        Height = 16
        Caption = 'Y'
      end
      object Label5: TLabel
        Left = 56
        Top = 120
        Width = 8
        Height = 16
        Caption = 'Z'
      end
      object SpinEditX: TSpinEdit
        Left = 96
        Top = 32
        Width = 73
        Height = 26
        MaxValue = 1000000000
        MinValue = -1000000000
        TabOrder = 0
        Value = 0
      end
      object SpinEditY: TSpinEdit
        Left = 96
        Top = 72
        Width = 73
        Height = 26
        MaxValue = 1000000000
        MinValue = -1000000000
        TabOrder = 1
        Value = 0
      end
      object SpinEditZ: TSpinEdit
        Left = 96
        Top = 112
        Width = 73
        Height = 26
        MaxValue = 1000000000
        MinValue = -1000000000
        TabOrder = 2
        Value = 0
      end
    end
    object GroupBoxReflection: TGroupBox
      Left = 249
      Top = 1
      Width = 249
      Height = 170
      Align = alRight
      Caption = 'Reflection'
      TabOrder = 1
      object LabelAmbient: TLabel
        Left = 32
        Top = 43
        Width = 49
        Height = 16
        Caption = 'Ambient'
      end
      object LabelDifusion: TLabel
        Left = 32
        Top = 83
        Width = 51
        Height = 16
        Caption = 'Diffusion'
      end
      object LabelSpecular: TLabel
        Left = 32
        Top = 123
        Width = 54
        Height = 16
        Caption = 'Specular'
      end
      object PanelAmbient: TPanel
        Left = 136
        Top = 35
        Width = 65
        Height = 25
        BevelInner = bvRaised
        BevelOuter = bvNone
        Color = clSilver
        TabOrder = 0
        OnEnter = PanelReflectionEnter
        OnMouseUp = PanelReflectionMouseUp
      end
      object PanelDiffuse: TPanel
        Left = 136
        Top = 75
        Width = 65
        Height = 25
        BevelInner = bvRaised
        BevelOuter = bvNone
        Color = clSilver
        TabOrder = 1
        OnEnter = PanelReflectionEnter
        OnMouseUp = PanelReflectionMouseUp
      end
      object PanelSpecular: TPanel
        Left = 136
        Top = 115
        Width = 65
        Height = 25
        BevelInner = bvRaised
        BevelOuter = bvNone
        Color = clSilver
        TabOrder = 2
        OnEnter = PanelReflectionEnter
        OnMouseUp = PanelReflectionMouseUp
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 173
    Width = 499
    ExplicitTop = 173
    ExplicitWidth = 499
    inherited ButtonOK: TButton
      Left = 177
      ExplicitLeft = 177
    end
    inherited ButtonCancel: TButton
      Left = 282
      ExplicitLeft = 282
    end
    inherited ButtonHelp: TButton
      Left = 388
      ExplicitLeft = 388
    end
    object CheckBoxLightColor: TCheckBox
      Left = 16
      Top = 18
      Width = 129
      Height = 17
      Caption = 'Light color'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
end
