inherited fmInterLinear: TfmInterLinear
  Left = 407
  Top = 191
  HelpContext = 3142
  Caption = 'Linear Options'
  ClientHeight = 317
  ClientWidth = 374
  OldCreateOrder = True
  ExplicitWidth = 380
  ExplicitHeight = 346
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 374
    Height = 1
    ExplicitWidth = 374
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 374
    Height = 268
    ExplicitTop = 1
    ExplicitWidth = 374
    ExplicitHeight = 268
    object GroupBoxAnisotropy: TGroupBox
      Left = 1
      Top = 1
      Width = 372
      Height = 266
      Align = alClient
      Caption = 'Anisotropy'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object GroupBox1: TGroupBox
        Left = 64
        Top = 18
        Width = 233
        Height = 95
        Caption = 'Factors'
        TabOrder = 0
        object LabelRatio1: TLabel
          Left = 16
          Top = 32
          Width = 39
          Height = 16
          Caption = 'Ratio1'
        end
        object LabelRatio2: TLabel
          Left = 16
          Top = 65
          Width = 39
          Height = 16
          Caption = 'Ratio2'
        end
        object SpinEdit1: TSpinEdit
          Left = 128
          Top = 24
          Width = 65
          Height = 26
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
        object SpinEdit2: TSpinEdit
          Left = 128
          Top = 55
          Width = 65
          Height = 26
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object GroupBox2: TGroupBox
        Left = 64
        Top = 120
        Width = 233
        Height = 129
        Caption = 'Angles'
        TabOrder = 1
        object LabelAzimuth: TLabel
          Left = 16
          Top = 32
          Width = 46
          Height = 16
          Caption = 'Azimuth'
        end
        object LabelDip: TLabel
          Left = 16
          Top = 64
          Width = 21
          Height = 16
          Caption = 'Dip'
        end
        object LabelStrike: TLabel
          Left = 16
          Top = 96
          Width = 34
          Height = 16
          Caption = 'Strike'
        end
        object EditAzimuth: TEdit
          Left = 128
          Top = 24
          Width = 65
          Height = 24
          TabOrder = 0
          Text = '0'
        end
        object EditDip: TEdit
          Left = 128
          Top = 56
          Width = 65
          Height = 24
          TabOrder = 1
          Text = '0'
        end
        object EditStrike: TEdit
          Left = 128
          Top = 88
          Width = 65
          Height = 24
          TabOrder = 2
          Text = '0'
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 269
    Width = 374
    ExplicitTop = 269
    ExplicitWidth = 374
    inherited ButtonOK: TButton
      Left = 28
      ExplicitLeft = 28
    end
    inherited ButtonCancel: TButton
      Left = 141
      ExplicitLeft = 141
    end
    inherited ButtonHelp: TButton
      Left = 255
      ExplicitLeft = 255
    end
  end
end
