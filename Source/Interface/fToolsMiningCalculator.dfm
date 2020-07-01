inherited fmToolsMiningCalculator: TfmToolsMiningCalculator
  Left = 195
  Top = 149
  HelpContext = 814
  Caption = 'Mining Calculator'
  ClientHeight = 491
  ClientWidth = 779
  ExplicitWidth = 785
  ExplicitHeight = 520
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 779
    ExplicitWidth = 314
  end
  inherited PanelMiddle: TPanel
    Width = 779
    Height = 433
    ExplicitWidth = 314
    ExplicitHeight = 153
    inherited PageControl: TPageControl
      Width = 769
      Height = 423
      ActivePage = TabSheetCutoffGrade
      ExplicitWidth = 304
      ExplicitHeight = 143
      object TabSheetCutoffGrade: TTabSheet
        Caption = 'Cutoff Grade'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 296
        ExplicitHeight = 112
        object LabelOreCosts: TLabel
          Left = 8
          Top = 72
          Width = 93
          Height = 32
          Caption = 'Costs of mining 1t ore'
          WordWrap = True
        end
        object LabelWasteCosts: TLabel
          Left = 368
          Top = 72
          Width = 134
          Height = 32
          Caption = 'Costs of processing 1t waste'
          WordWrap = True
        end
        object LabelRecovery: TLabel
          Left = 8
          Top = 136
          Width = 182
          Height = 32
          Caption = 'Recovery of valuable element to product'
          WordWrap = True
        end
        object LabelDilution: TLabel
          Left = 368
          Top = 144
          Width = 139
          Height = 32
          Caption = 'Dilution of ore in mining operations'
          WordWrap = True
        end
        object LabelStrippingCoefficient: TLabel
          Left = 8
          Top = 208
          Width = 82
          Height = 16
          Caption = 'Stripping ratio'
          WordWrap = True
        end
        object LabelProductPrice: TLabel
          Left = 368
          Top = 208
          Width = 79
          Height = 16
          Caption = 'Product price'
          WordWrap = True
        end
        object LabelCutoffGrade: TLabel
          Left = 208
          Top = 288
          Width = 74
          Height = 16
          Caption = 'Cutoff Grade'
          WordWrap = True
        end
        object PanelCutoffGrade: TPanel
          Left = 0
          Top = 0
          Width = 761
          Height = 41
          Align = alTop
          BevelInner = bvLowered
          Caption = 'Cutoff Grade'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 296
        end
        object Edit1: TEdit
          Left = 304
          Top = 80
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 1
          Text = '$'
        end
        object Edit2: TEdit
          Left = 304
          Top = 208
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 2
          Text = 't/t'
        end
        object Edit3: TEdit
          Left = 688
          Top = 208
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 3
          Text = '$'
        end
        object Edit4: TEdit
          Left = 688
          Top = 77
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 4
          Text = '$'
        end
        object Edit5: TEdit
          Left = 304
          Top = 144
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 5
          Text = 'u'
        end
        object Edit6: TEdit
          Left = 688
          Top = 144
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 6
          Text = 'u'
        end
        object Edit7: TEdit
          Left = 472
          Top = 280
          Width = 25
          Height = 24
          Color = clBtnFace
          TabOrder = 7
          Text = '%'
        end
        object GBEditValue1: TGBEditValue
          Left = 224
          Top = 80
          Width = 74
          Height = 24
          TabOrder = 8
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValueOreRecovery: TGBEditValue
          Left = 224
          Top = 144
          Width = 74
          Height = 24
          TabOrder = 9
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValueStripRatio: TGBEditValue
          Left = 224
          Top = 205
          Width = 74
          Height = 24
          TabOrder = 10
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValue4: TGBEditValue
          Left = 608
          Top = 77
          Width = 74
          Height = 24
          TabOrder = 11
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValueOreDilution: TGBEditValue
          Left = 608
          Top = 144
          Width = 74
          Height = 24
          TabOrder = 12
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValueProductPrice: TGBEditValue
          Left = 608
          Top = 208
          Width = 74
          Height = 24
          TabOrder = 13
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
        object GBEditValueCutoffGrade: TGBEditValue
          Left = 392
          Top = 280
          Width = 74
          Height = 24
          TabOrder = 14
          Text = '0'
          Parent = TabSheetCutoffGrade
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
      end
      object TabSheetStrippingRatio: TTabSheet
        Caption = 'Stripping Ratio'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Panel5: TPanel
          Left = 0
          Top = 0
          Width = 754
          Height = 41
          Align = alTop
          BevelInner = bvLowered
          Caption = 'Stripping Ratio'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -20
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
        end
        object GBEditValueStrippingRatio: TGBEditValue
          Left = 320
          Top = 104
          Width = 121
          Height = 24
          TabOrder = 1
          Text = '0'
          Parent = TabSheetStrippingRatio
          EnabledConvert = True
          AsInteger = 0
          ValueType = vtDouble
          Error = False
          Precision = 0
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 444
    Width = 779
    ExplicitTop = 164
    ExplicitWidth = 314
    inherited ButtonOK: TButton
      Left = 411
      Align = alCustom
      ExplicitLeft = 411
    end
    inherited ButtonCancel: TButton
      Left = 531
      Align = alCustom
      ExplicitLeft = 531
    end
    inherited ButtonHelp: TButton
      Left = 653
      Align = alCustom
      ExplicitLeft = 653
    end
  end
end
