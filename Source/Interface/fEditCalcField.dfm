inherited fmEditCalcField: TfmEditCalcField
  Left = 283
  Top = 204
  HelpContext = 233
  Caption = 'Calculate Field'
  ClientHeight = 486
  ClientWidth = 554
  OldCreateOrder = True
  ExplicitWidth = 560
  ExplicitHeight = 515
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 554
    Height = 1
    ExplicitWidth = 554
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 554
    Height = 437
    ExplicitTop = 1
    ExplicitWidth = 554
    ExplicitHeight = 437
    object LabelResultField: TLabel
      Left = 16
      Top = 240
      Width = 66
      Height = 16
      Caption = 'Result field'
    end
    object LabelFormula: TLabel
      Left = 221
      Top = 262
      Width = 49
      Height = 16
      Caption = 'Formula'
      OnMouseDown = LabelFormulaMouseDown
    end
    object LabelAvailableFields: TLabel
      Left = 168
      Top = 8
      Width = 92
      Height = 16
      Caption = 'Available fields'
    end
    object lbAvailableFields: TListBox
      Left = 168
      Top = 32
      Width = 233
      Height = 185
      TabOrder = 0
      OnClick = lbAvailableFieldsClick
      OnKeyDown = lbAvailableFieldsKeyDown
    end
    object bt8: TButton
      Tag = 666
      Left = 64
      Top = 32
      Width = 41
      Height = 41
      Caption = '8'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt9: TButton
      Tag = 666
      Left = 112
      Top = 32
      Width = 41
      Height = 41
      Caption = '9'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt7: TButton
      Tag = 666
      Left = 16
      Top = 32
      Width = 41
      Height = 41
      Caption = '7'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btPlus: TButton
      Tag = 666
      Left = 416
      Top = 32
      Width = 41
      Height = 41
      Caption = '+'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt4: TButton
      Tag = 666
      Left = 16
      Top = 80
      Width = 41
      Height = 41
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt5: TButton
      Tag = 666
      Left = 64
      Top = 80
      Width = 41
      Height = 41
      Caption = '5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 6
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt6: TButton
      Tag = 666
      Left = 112
      Top = 80
      Width = 41
      Height = 41
      Caption = '6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 7
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btMinus: TButton
      Tag = 666
      Left = 472
      Top = 33
      Width = 41
      Height = 41
      Caption = '-'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt1: TButton
      Tag = 666
      Left = 16
      Top = 128
      Width = 41
      Height = 41
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 9
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt2: TButton
      Tag = 666
      Left = 64
      Top = 128
      Width = 41
      Height = 41
      Caption = '2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt3: TButton
      Tag = 666
      Left = 112
      Top = 128
      Width = 41
      Height = 41
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 11
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btMul: TButton
      Tag = 666
      Left = 472
      Top = 80
      Width = 41
      Height = 41
      Caption = '*'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 12
      TabStop = False
      OnClick = ButtonSKClick
    end
    object bt0: TButton
      Tag = 666
      Left = 16
      Top = 176
      Width = 41
      Height = 41
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 13
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btPoint: TButton
      Tag = 666
      Left = 64
      Top = 176
      Width = 41
      Height = 41
      Caption = '.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btSk: TButton
      Tag = 666
      Left = 112
      Top = 176
      Width = 41
      Height = 41
      Caption = '(  )'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 15
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btDiv: TButton
      Tag = 666
      Left = 416
      Top = 80
      Width = 41
      Height = 41
      Caption = '/'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 16
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btGt: TButton
      Tag = 666
      Left = 472
      Top = 128
      Width = 41
      Height = 33
      Caption = '>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 17
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btLt: TButton
      Tag = 666
      Left = 416
      Top = 128
      Width = 41
      Height = 33
      Caption = '<'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 18
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btEq: TButton
      Tag = 666
      Left = 416
      Top = 168
      Width = 41
      Height = 33
      Caption = '='
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 19
      TabStop = False
      OnClick = ButtonSKClick
    end
    object btWhere: TButton
      Tag = 666
      Left = 448
      Top = 257
      Width = 81
      Height = 25
      Caption = 'Where'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 20
      TabStop = False
      OnClick = ButtonSQLClick
    end
    object btNULL: TButton
      Tag = 666
      Left = 474
      Top = 167
      Width = 39
      Height = 34
      Caption = 'NULL'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 21
      TabStop = False
      OnClick = ButtonSQLClick
    end
    object pnEqual: TPanel
      Left = 181
      Top = 287
      Width = 25
      Height = 24
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = '='
      Color = clWhite
      TabOrder = 22
    end
    object EditFormula: TEdit
      Left = 216
      Top = 288
      Width = 313
      Height = 24
      TabOrder = 23
      OnChange = EditFormulaChange
    end
    object btClear: TButton
      Left = 448
      Top = 318
      Width = 81
      Height = 28
      Caption = '&Clear'
      TabOrder = 24
      OnClick = btClearClick
    end
    object lbResultFields: TListBox
      Left = 8
      Top = 264
      Width = 161
      Height = 113
      TabOrder = 25
    end
    object cbIncRecord: TCheckBox
      Left = 188
      Top = 329
      Width = 117
      Height = 17
      Caption = 'Incrementation'
      TabOrder = 26
    end
    object ButtonAddField: TButton
      Left = 48
      Top = 394
      Width = 97
      Height = 25
      Caption = 'Add field...'
      TabOrder = 27
    end
  end
  inherited PanelBottom: TPanel
    Top = 438
    Width = 554
    ExplicitTop = 438
    ExplicitWidth = 554
    inherited ButtonOK: TButton
      Left = 192
      Enabled = False
      ExplicitLeft = 192
    end
    inherited ButtonCancel: TButton
      Left = 313
      ExplicitLeft = 313
    end
    inherited ButtonHelp: TButton
      Left = 435
      ExplicitLeft = 435
    end
  end
end
