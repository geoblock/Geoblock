inherited fmDrawObjectDepth: TfmDrawObjectDepth
  Left = 364
  Top = 254
  HelpContext = 630
  Caption = 'Drawing Depth'
  ClientHeight = 187
  ClientWidth = 384
  OldCreateOrder = True
  ExplicitWidth = 390
  ExplicitHeight = 216
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 384
    Height = 1
    ExplicitWidth = 384
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 384
    Height = 138
    ExplicitTop = 1
    ExplicitWidth = 384
    ExplicitHeight = 138
    object GroupBoxDepth: TGroupBox
      Left = 1
      Top = 1
      Width = 382
      Height = 136
      Align = alClient
      Caption = 'Depth'
      TabOrder = 0
      object RadioButtonAverage: TRadioButton
        Left = 40
        Top = 40
        Width = 153
        Height = 17
        Caption = 'Average'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonAverageClick
      end
      object RadioButtonSpecified: TRadioButton
        Left = 40
        Top = 80
        Width = 137
        Height = 17
        Caption = 'Specified'
        TabOrder = 1
        OnClick = RadioButtonSpecifiedClick
      end
      object EditValueDepth: TGBEditValue
        Left = 200
        Top = 72
        Width = 121
        Height = 24
        Enabled = False
        TabOrder = 2
        Text = '0'
        Parent = GroupBoxDepth
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 139
    Width = 384
    ExplicitTop = 139
    ExplicitWidth = 384
    inherited ButtonOK: TButton
      Left = 22
      ExplicitLeft = 22
    end
    inherited ButtonCancel: TButton
      Left = 143
      ExplicitLeft = 143
    end
    inherited ButtonHelp: TButton
      Left = 265
      ExplicitLeft = 265
    end
  end
end
