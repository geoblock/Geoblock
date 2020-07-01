inherited fmMethodTransformation: TfmMethodTransformation
  Left = 358
  Top = 20
  HelpContext = 312
  Caption = 'Transformation'
  ClientHeight = 641
  ExplicitHeight = 680
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 169
    ExplicitHeight = 169
    inherited GroupBoxInput: TGroupBox
      Height = 159
      ExplicitHeight = 159
      inherited GroupBoxRealAttribute: TGroupBox
        Height = 75
        ExplicitHeight = 75
        inherited ListBoxRealAttribute: TListBox
          Height = 55
          Enabled = False
          ExtendedSelect = False
          MultiSelect = False
          ExplicitHeight = 55
        end
      end
      inherited GroupBoxModel: TGroupBox
        Height = 75
        ExplicitHeight = 75
        inherited ListBoxInputNames: TListBox
          Height = 55
          ExplicitHeight = 55
        end
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 169
    Height = 426
    ExplicitTop = 169
    ExplicitHeight = 426
    inherited GroupBoxOutput: TGroupBox
      Top = 367
      ExplicitTop = 367
    end
    object RadioGroupMode: TRadioGroup
      Left = 5
      Top = 5
      Width = 544
      Height = 44
      Align = alTop
      Caption = 'Mode'
      Columns = 2
      ItemIndex = 1
      Items.Strings = (
        'Matrix'
        'Command')
      TabOrder = 1
      OnClick = RadioGroupModeClick
    end
    object GroupBoxMatrix: TGroupBox
      Left = 5
      Top = 49
      Width = 544
      Height = 176
      HelpType = htKeyword
      Align = alTop
      Caption = 'Matrix'
      TabOrder = 2
      object Label1: TLabel
        Tag = 1000
        Left = 48
        Top = 36
        Width = 7
        Height = 16
        Caption = '1'
      end
      object Label2: TLabel
        Tag = 1000
        Left = 48
        Top = 68
        Width = 7
        Height = 16
        Caption = '2'
      end
      object Label3: TLabel
        Tag = 1000
        Left = 48
        Top = 104
        Width = 7
        Height = 16
        Caption = '3'
      end
      object Label4: TLabel
        Tag = 1000
        Left = 48
        Top = 139
        Width = 7
        Height = 16
        Caption = '4'
      end
      object Label5: TLabel
        Tag = 1000
        Left = 90
        Top = 16
        Width = 7
        Height = 16
        Caption = '1'
      end
      object Label6: TLabel
        Tag = 1000
        Left = 178
        Top = 16
        Width = 7
        Height = 16
        Caption = '2'
      end
      object Label7: TLabel
        Tag = 1000
        Left = 266
        Top = 16
        Width = 7
        Height = 16
        Caption = '3'
      end
      object Label8: TLabel
        Tag = 1000
        Left = 354
        Top = 16
        Width = 7
        Height = 16
        Caption = '4'
      end
      object GBEditValue00: TGBEditValue
        Left = 79
        Top = 34
        Width = 57
        Height = 24
        TabOrder = 0
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue01: TGBEditValue
        Left = 160
        Top = 36
        Width = 57
        Height = 24
        TabOrder = 1
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue02: TGBEditValue
        Left = 232
        Top = 36
        Width = 57
        Height = 24
        TabOrder = 2
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue03: TGBEditValue
        Left = 313
        Top = 35
        Width = 57
        Height = 24
        TabOrder = 3
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue10: TGBEditValue
        Left = 80
        Top = 66
        Width = 57
        Height = 24
        TabOrder = 4
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue11: TGBEditValue
        Left = 160
        Top = 68
        Width = 57
        Height = 24
        TabOrder = 5
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue12: TGBEditValue
        Left = 232
        Top = 68
        Width = 57
        Height = 24
        TabOrder = 6
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue13: TGBEditValue
        Left = 313
        Top = 66
        Width = 57
        Height = 24
        TabOrder = 7
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue20: TGBEditValue
        Left = 80
        Top = 101
        Width = 57
        Height = 24
        TabOrder = 8
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue21: TGBEditValue
        Left = 160
        Top = 101
        Width = 57
        Height = 24
        TabOrder = 9
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue22: TGBEditValue
        Left = 232
        Top = 101
        Width = 57
        Height = 24
        TabOrder = 10
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue23: TGBEditValue
        Left = 313
        Top = 99
        Width = 57
        Height = 24
        TabOrder = 11
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue30: TGBEditValue
        Left = 80
        Top = 136
        Width = 57
        Height = 24
        TabOrder = 12
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue31: TGBEditValue
        Left = 160
        Top = 136
        Width = 57
        Height = 24
        TabOrder = 13
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue32: TGBEditValue
        Left = 232
        Top = 136
        Width = 57
        Height = 24
        TabOrder = 14
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValue33: TGBEditValue
        Left = 314
        Top = 136
        Width = 57
        Height = 24
        TabOrder = 15
        Text = '0'
        Parent = GroupBoxMatrix
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
    end
    object GroupBoxCommand: TGroupBox
      Left = 5
      Top = 225
      Width = 544
      Height = 142
      Align = alClient
      Caption = 'Command'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 3
      object LabelAngle: TLabel
        Left = 436
        Top = 43
        Width = 35
        Height = 16
        Caption = 'Angle'
      end
      object LabelX: TLabel
        Tag = 1000
        Left = 186
        Top = 16
        Width = 8
        Height = 16
        Caption = 'X'
      end
      object LabelY: TLabel
        Tag = 1000
        Left = 250
        Top = 16
        Width = 9
        Height = 16
        Caption = 'Y'
      end
      object LabelZ: TLabel
        Tag = 1000
        Left = 314
        Top = 16
        Width = 8
        Height = 16
        Caption = 'Z'
      end
      object ButtonTransfer: TButton
        Left = 16
        Top = 32
        Width = 129
        Height = 25
        Caption = 'Transfer'
        TabOrder = 0
        OnClick = ButtonTransferClick
      end
      object ButtonRotate: TButton
        Left = 16
        Top = 67
        Width = 129
        Height = 24
        Caption = 'Rotate'
        TabOrder = 1
        OnClick = ButtonRotateClick
      end
      object ButtonScale: TButton
        Left = 16
        Top = 99
        Width = 129
        Height = 25
        Caption = 'Scale'
        TabOrder = 2
        OnClick = ButtonScaleClick
      end
      object GBEditValueTransferX: TGBEditValue
        Left = 160
        Top = 32
        Width = 57
        Height = 24
        TabOrder = 3
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueTransferY: TGBEditValue
        Left = 235
        Top = 32
        Width = 57
        Height = 24
        TabOrder = 4
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueTransferZ: TGBEditValue
        Left = 313
        Top = 34
        Width = 57
        Height = 24
        TabOrder = 5
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueRotateX: TGBEditValue
        Left = 161
        Top = 62
        Width = 57
        Height = 24
        TabOrder = 6
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueRotateY: TGBEditValue
        Left = 235
        Top = 64
        Width = 57
        Height = 24
        TabOrder = 7
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueRotateZ: TGBEditValue
        Left = 313
        Top = 66
        Width = 57
        Height = 24
        TabOrder = 8
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueScaleX: TGBEditValue
        Left = 161
        Top = 94
        Width = 57
        Height = 24
        TabOrder = 9
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueScaleY: TGBEditValue
        Left = 235
        Top = 94
        Width = 57
        Height = 24
        TabOrder = 10
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueScaleZ: TGBEditValue
        Left = 314
        Top = 96
        Width = 57
        Height = 24
        TabOrder = 11
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
      object GBEditValueRotateAngle: TGBEditValue
        Left = 425
        Top = 65
        Width = 57
        Height = 24
        TabOrder = 12
        Text = '0'
        Parent = GroupBoxCommand
        EnabledConvert = True
        AsInteger = 0
        ValueType = vtDouble
        Error = False
        Precision = 0
      end
    end
    object ButtonReset: TButton
      Left = 441
      Top = 20
      Width = 88
      Height = 24
      Caption = 'Reset'
      TabOrder = 4
      OnClick = ButtonResetClick
    end
  end
  inherited PanelBottom: TPanel
    Top = 595
    ExplicitTop = 595
    inherited ButtonHelp: TButton
      HelpContext = 312
    end
  end
end
