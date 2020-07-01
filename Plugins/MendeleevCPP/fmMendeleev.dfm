object fmMain: TfmMain
  Left = 266
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Periodic Table'
  ClientHeight = 480
  ClientWidth = 651
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelTable: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 480
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object SpeedButtonUn: TSpeedButton
      Tag = 110
      Left = 241
      Top = 260
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Un'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonH: TSpeedButton
      Tag = 1
      Left = 7
      Top = 59
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'H'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = False
      OnClick = SpeedButtonElementClick
    end
    object LabelIA: TLabel
      Left = 13
      Top = 39
      Width = 9
      Height = 13
      Caption = 'Ia'
    end
    object LabelIIA: TLabel
      Left = 39
      Top = 39
      Width = 12
      Height = 13
      Caption = 'IIa'
    end
    object LabelIIIB: TLabel
      Left = 62
      Top = 40
      Width = 15
      Height = 13
      Caption = 'IIIb'
    end
    object LabelIIIA: TLabel
      Left = 319
      Top = 39
      Width = 15
      Height = 13
      Caption = 'IIIa'
    end
    object LabelVIIIA: TLabel
      Left = 449
      Top = 39
      Width = 22
      Height = 13
      Caption = 'VIIIa'
    end
    object LabelVIIA: TLabel
      Left = 423
      Top = 39
      Width = 19
      Height = 13
      Caption = 'VIIa'
    end
    object LabelLanthanide: TLabel
      Left = 13
      Top = 267
      Width = 53
      Height = 13
      Caption = 'Lanthanide'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object LabelActinide: TLabel
      Left = 13
      Top = 293
      Width = 38
      Height = 13
      Caption = 'Actinide'
    end
    object LabelIVB: TLabel
      Left = 88
      Top = 40
      Width = 16
      Height = 13
      Caption = 'IVb'
    end
    object LabelVB: TLabel
      Left = 114
      Top = 40
      Width = 13
      Height = 13
      Caption = 'Vb'
    end
    object Label1VIB: TLabel
      Left = 140
      Top = 40
      Width = 16
      Height = 13
      Caption = 'VIb'
    end
    object LabelVIIB: TLabel
      Left = 166
      Top = 40
      Width = 19
      Height = 13
      Caption = 'VIIb'
    end
    object LabelIIB: TLabel
      Left = 296
      Top = 40
      Width = 12
      Height = 13
      Caption = 'IIb'
    end
    object Label1IB: TLabel
      Left = 270
      Top = 40
      Width = 9
      Height = 13
      Caption = 'Ib'
    end
    object LabelIVA: TLabel
      Left = 345
      Top = 39
      Width = 16
      Height = 13
      Caption = 'IVa'
    end
    object LabelVA: TLabel
      Left = 371
      Top = 39
      Width = 13
      Height = 13
      Caption = 'Va'
    end
    object LabelVIA: TLabel
      Left = 397
      Top = 39
      Width = 16
      Height = 13
      Caption = 'VIa'
    end
    object SpeedButtonLi: TSpeedButton
      Tag = 3
      Left = 7
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Li'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNa: TSpeedButton
      Tag = 11
      Left = 7
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Na'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonK: TSpeedButton
      Tag = 19
      Left = 7
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRb: TSpeedButton
      Tag = 37
      Left = 7
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Rb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCs: TSpeedButton
      Tag = 55
      Left = 7
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Cs'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonFr: TSpeedButton
      Tag = 87
      Left = 7
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Fr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clAqua
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBe: TSpeedButton
      Tag = 4
      Left = 33
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Be'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonMg: TSpeedButton
      Tag = 12
      Left = 33
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Mg'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCa: TSpeedButton
      Tag = 20
      Left = 33
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ca'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSr: TSpeedButton
      Tag = 38
      Left = 33
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Sr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBa: TSpeedButton
      Tag = 56
      Left = 33
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ba'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRa: TSpeedButton
      Tag = 88
      Left = 33
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ra'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSc: TSpeedButton
      Tag = 21
      Left = 59
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Sc'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonY: TSpeedButton
      Tag = 39
      Left = 59
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonLa: TSpeedButton
      Tag = 57
      Left = 59
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'La'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAc: TSpeedButton
      Tag = 89
      Left = 59
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ac'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTi: TSpeedButton
      Tag = 22
      Left = 85
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ti'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonZr: TSpeedButton
      Tag = 40
      Left = 85
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Zr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonHf: TSpeedButton
      Tag = 72
      Left = 85
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Hf'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonDb: TSpeedButton
      Tag = 104
      Left = 85
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Db'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonV: TSpeedButton
      Tag = 23
      Left = 111
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'V'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNb: TSpeedButton
      Tag = 41
      Left = 111
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Nb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTa: TSpeedButton
      Tag = 73
      Left = 111
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ta'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonJl: TSpeedButton
      Tag = 105
      Left = 111
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Jl'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCr: TSpeedButton
      Tag = 24
      Left = 137
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Cr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonMo: TSpeedButton
      Tag = 42
      Left = 137
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Mo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonW: TSpeedButton
      Tag = 74
      Left = 137
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'W'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonMn: TSpeedButton
      Tag = 25
      Left = 163
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Mn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTc: TSpeedButton
      Tag = 43
      Left = 163
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Tc'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRe: TSpeedButton
      Tag = 75
      Left = 163
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Re'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonFe: TSpeedButton
      Tag = 26
      Left = 189
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Fe'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRu: TSpeedButton
      Tag = 44
      Left = 189
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ru'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonOs: TSpeedButton
      Tag = 76
      Left = 189
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Os'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCo: TSpeedButton
      Tag = 27
      Left = 215
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Co'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRh: TSpeedButton
      Tag = 45
      Left = 215
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Rh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonIr: TSpeedButton
      Tag = 77
      Left = 215
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNi: TSpeedButton
      Tag = 28
      Left = 241
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ni'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPd: TSpeedButton
      Tag = 46
      Left = 241
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Pd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPt: TSpeedButton
      Tag = 78
      Left = 241
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Pt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCu: TSpeedButton
      Tag = 29
      Left = 267
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Cu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAg: TSpeedButton
      Tag = 47
      Left = 267
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ag'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAu: TSpeedButton
      Tag = 79
      Left = 267
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Au'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonZn: TSpeedButton
      Tag = 30
      Left = 293
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Zn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCd: TSpeedButton
      Tag = 48
      Left = 293
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Cd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonHg: TSpeedButton
      Tag = 80
      Left = 293
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Hg'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonB: TSpeedButton
      Tag = 5
      Left = 319
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAl: TSpeedButton
      Tag = 13
      Left = 319
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Al'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonGa: TSpeedButton
      Tag = 31
      Left = 319
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ga'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonIn: TSpeedButton
      Tag = 49
      Left = 319
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'In'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTl: TSpeedButton
      Tag = 81
      Left = 319
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Tl'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonC: TSpeedButton
      Tag = 6
      Left = 345
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSi: TSpeedButton
      Tag = 14
      Left = 345
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Si'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonGe: TSpeedButton
      Tag = 32
      Left = 345
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ge'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSn: TSpeedButton
      Tag = 50
      Left = 345
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Sn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPb: TSpeedButton
      Tag = 82
      Left = 345
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Pb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonN: TSpeedButton
      Tag = 7
      Left = 371
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'N'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonP: TSpeedButton
      Tag = 15
      Left = 371
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'P'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAs: TSpeedButton
      Tag = 33
      Left = 371
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'As'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSb: TSpeedButton
      Tag = 51
      Left = 371
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Sb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBi: TSpeedButton
      Tag = 83
      Left = 371
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Bi'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonO: TSpeedButton
      Tag = 8
      Left = 397
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'O'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonS: TSpeedButton
      Tag = 16
      Left = 397
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSe: TSpeedButton
      Tag = 34
      Left = 397
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Se'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTe: TSpeedButton
      Tag = 52
      Left = 397
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Te'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPo: TSpeedButton
      Tag = 84
      Left = 397
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Po'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonF: TSpeedButton
      Tag = 9
      Left = 423
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'F'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCl: TSpeedButton
      Tag = 17
      Left = 423
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Cl'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBr: TSpeedButton
      Tag = 35
      Left = 423
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Br'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonI: TSpeedButton
      Tag = 53
      Left = 423
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'I'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAt: TSpeedButton
      Tag = 85
      Left = 423
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'At'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonHe: TSpeedButton
      Tag = 2
      Left = 449
      Top = 59
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'He'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNe: TSpeedButton
      Tag = 10
      Left = 449
      Top = 85
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ne'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAr: TSpeedButton
      Tag = 18
      Left = 449
      Top = 111
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Ar'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonKr: TSpeedButton
      Tag = 36
      Left = 449
      Top = 137
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Kr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonXe: TSpeedButton
      Tag = 54
      Left = 449
      Top = 163
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Xe'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRn: TSpeedButton
      Tag = 86
      Left = 449
      Top = 189
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Rn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCe: TSpeedButton
      Tag = 58
      Left = 85
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Ce'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTh: TSpeedButton
      Tag = 90
      Left = 85
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Th'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPr: TSpeedButton
      Tag = 59
      Left = 111
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Pr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPa: TSpeedButton
      Tag = 91
      Left = 111
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Pa'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNd: TSpeedButton
      Tag = 60
      Left = 137
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Nd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonU: TSpeedButton
      Tag = 92
      Left = 137
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'U'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPm: TSpeedButton
      Tag = 61
      Left = 163
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Pm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNp: TSpeedButton
      Tag = 93
      Left = 163
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Np'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonSm: TSpeedButton
      Tag = 62
      Left = 189
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Sm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonPu: TSpeedButton
      Tag = 94
      Left = 189
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Pu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonEu: TSpeedButton
      Tag = 63
      Left = 215
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Eu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonAm: TSpeedButton
      Tag = 95
      Left = 215
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Am'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonGd: TSpeedButton
      Tag = 64
      Left = 241
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Gd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCm: TSpeedButton
      Tag = 96
      Left = 241
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Cm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTb: TSpeedButton
      Tag = 65
      Left = 267
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Tb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBk: TSpeedButton
      Tag = 97
      Left = 267
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Bk'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonDy: TSpeedButton
      Tag = 66
      Left = 293
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Dy'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonCf: TSpeedButton
      Tag = 98
      Left = 293
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Cf'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonHo: TSpeedButton
      Tag = 67
      Left = 319
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Ho'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonEs: TSpeedButton
      Tag = 99
      Left = 319
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Es'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonEr: TSpeedButton
      Tag = 68
      Left = 345
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Er'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonFm: TSpeedButton
      Tag = 100
      Left = 345
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Fm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonTm: TSpeedButton
      Tag = 69
      Left = 371
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Tm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonMd: TSpeedButton
      Tag = 101
      Left = 371
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Md'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonYb: TSpeedButton
      Tag = 70
      Left = 397
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Yb'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonNo: TSpeedButton
      Tag = 102
      Left = 397
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'No'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonLu: TSpeedButton
      Tag = 71
      Left = 423
      Top = 260
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Lu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonLr: TSpeedButton
      Tag = 103
      Left = 423
      Top = 286
      Width = 27
      Height = 28
      GroupIndex = 1
      Caption = 'Lr'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clOlive
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonRf: TSpeedButton
      Tag = 106
      Left = 137
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Rf'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonBh: TSpeedButton
      Tag = 107
      Left = 163
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Bh'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonHn: TSpeedButton
      Tag = 108
      Left = 189
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Hn'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object SpeedButtonMt: TSpeedButton
      Tag = 109
      Left = 214
      Top = 215
      Width = 27
      Height = 27
      GroupIndex = 1
      Caption = 'Mt'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clPurple
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = SpeedButtonElementClick
    end
    object GroupBoxVIII: TGroupBox
      Left = 195
      Top = 39
      Width = 72
      Height = 14
      Caption = 'VIII'
      TabOrder = 0
    end
    object PanelBottom: TPanel
      Left = 1
      Top = 440
      Width = 483
      Height = 39
      HelpContext = 702
      Align = alBottom
      TabOrder = 1
      object ButtonCancel: TButton
        Left = 269
        Top = 4
        Width = 80
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object ButtonOK: TButton
        Left = 159
        Top = 4
        Width = 81
        Height = 25
        Caption = 'OK'
        ModalResult = 1
        TabOrder = 1
      end
      object ButtonPrint: TButton
        Left = 18
        Top = 7
        Width = 72
        Height = 25
        Caption = 'Print...'
        TabOrder = 2
        OnClick = ButtonPrintClick
      end
      object ButtonHelp: TButton
        Left = 380
        Top = 4
        Width = 79
        Height = 25
        Caption = '&Help'
        TabOrder = 3
        OnClick = ButtonHelpClick
      end
    end
    object PageControlLegend: TPageControl
      Left = 2
      Top = 361
      Width = 484
      Height = 77
      ActivePage = TabSheetChemistry
      Align = alCustom
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      OnChange = PageControlLegendChange
      object TabSheetChemistry: TTabSheet
        Caption = 'Chemistry'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object LabelAlkaliMetals: TLabel
          Left = 14
          Top = 6
          Width = 73
          Height = 13
          Caption = 'Alkali Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clAqua
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelRareEarthMetals: TLabel
          Left = 14
          Top = 19
          Width = 103
          Height = 13
          Caption = 'Rare Earth Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clOlive
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelTransitionMetals: TLabel
          Left = 14
          Top = 32
          Width = 98
          Height = 13
          Caption = 'Transition Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clPurple
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelAlkaliEarthMetals: TLabel
          Left = 189
          Top = 6
          Width = 107
          Height = 13
          Caption = 'Alkali Earth Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clTeal
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelOtherMetals: TLabel
          Left = 189
          Top = 19
          Width = 73
          Height = 13
          Caption = 'Other Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelOtherNonmetals: TLabel
          Left = 189
          Top = 32
          Width = 95
          Height = 13
          Caption = 'Other Nonmetals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelHalogens: TLabel
          Left = 345
          Top = 19
          Width = 54
          Height = 13
          Caption = 'Halogens'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clLime
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelNobleGases: TLabel
          Left = 345
          Top = 32
          Width = 73
          Height = 13
          Caption = 'Noble Gases'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clYellow
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object TabSheetMetallurgy: TTabSheet
        Caption = 'Metallurgy'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object LabelFerrousMetals: TLabel
          Left = 13
          Top = 20
          Width = 84
          Height = 13
          Caption = 'Ferrous Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelNonFerrousMetals: TLabel
          Left = 13
          Top = 33
          Width = 108
          Height = 13
          Caption = 'Non-ferrous Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelRareElements: TLabel
          Left = 306
          Top = 20
          Width = 83
          Height = 13
          Caption = 'Rare Elements'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clOlive
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
        end
        object LabelPreciousMetals: TLabel
          Left = 150
          Top = 20
          Width = 91
          Height = 13
          Caption = 'Precious Metals'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelRadioactiveElements: TLabel
          Left = 150
          Top = 33
          Width = 124
          Height = 13
          Caption = 'Radioactive Elements'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clFuchsia
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object LabelNonmetallicElements: TLabel
          Left = 306
          Top = 33
          Width = 126
          Height = 13
          Caption = 'Non-metallic Elements'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -12
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
    end
    object PanelMendeleevTable: TPanel
      Left = 1
      Top = 1
      Width = 483
      Height = 26
      Align = alTop
      Caption = 'Mendeleev Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object Panel1: TPanel
    Left = 485
    Top = 0
    Width = 166
    Height = 480
    Align = alRight
    BevelOuter = bvLowered
    TabOrder = 1
    object LabelDensity: TLabel
      Left = 7
      Top = 202
      Width = 90
      Height = 13
      Caption = 'Density, grams/cc)'
    end
    object LabelBoilingPoint: TLabel
      Left = 7
      Top = 176
      Width = 80
      Height = 13
      Caption = 'Boiling point,  *C'
    end
    object LabelMeltingPoint: TLabel
      Left = 7
      Top = 150
      Width = 84
      Height = 13
      Caption = 'Melting point,  *C'
    end
    object LabelState: TLabel
      Left = 7
      Top = 124
      Width = 26
      Height = 13
      Caption = 'State'
    end
    object LabelValence: TLabel
      Left = 7
      Top = 98
      Width = 37
      Height = 13
      Caption = 'Valence'
    end
    object LabelAtomicWeight: TLabel
      Left = 7
      Top = 72
      Width = 67
      Height = 13
      Caption = 'Atomic weight'
    end
    object LabelAtomicNumber: TLabel
      Left = 7
      Top = 46
      Width = 71
      Height = 13
      Caption = 'Atomic number'
    end
    object LabelName: TLabel
      Left = 7
      Top = 20
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object LabelDescription: TLabel
      Left = 1
      Top = 292
      Width = 164
      Height = 13
      Align = alBottom
      Caption = 'Description'
      ExplicitWidth = 53
    end
    object LabelMainMinerals: TLabel
      Left = 1
      Top = 394
      Width = 164
      Height = 13
      Align = alBottom
      Caption = 'Main minerals'
      ExplicitWidth = 64
    end
    object DBEditName: TDBEdit
      Left = 59
      Top = 11
      Width = 98
      Height = 21
      BiDiMode = bdRightToLeft
      Color = clBtnFace
      DataField = 'NAME'
      DataSource = DataSourceElements
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentBiDiMode = False
      ParentFont = False
      TabOrder = 0
    end
    object DBEditAtomicNumber: TDBEdit
      Left = 111
      Top = 38
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'ATOMNUMBER'
      DataSource = DataSourceElements
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object DBEditWeight: TDBEdit
      Left = 111
      Top = 64
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'ATOMMASS'
      DataSource = DataSourceElements
      TabOrder = 2
    end
    object DBEditValence: TDBEdit
      Left = 111
      Top = 90
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'VALENCE'
      DataSource = DataSourceElements
      TabOrder = 3
    end
    object DBEditState: TDBEdit
      Left = 72
      Top = 117
      Width = 85
      Height = 21
      BiDiMode = bdRightToLeft
      Color = clBtnFace
      DataField = 'STATE'
      DataSource = DataSourceElements
      ParentBiDiMode = False
      TabOrder = 4
    end
    object DBEditMeltingPoint: TDBEdit
      Left = 111
      Top = 143
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'MELTING'
      DataSource = DataSourceElements
      TabOrder = 5
    end
    object DBEditBoilingPoint: TDBEdit
      Left = 111
      Top = 169
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'BOILING'
      DataSource = DataSourceElements
      TabOrder = 6
    end
    object DBEditDensity: TDBEdit
      Left = 111
      Top = 195
      Width = 46
      Height = 21
      Color = clBtnFace
      DataField = 'DENSITY'
      DataSource = DataSourceElements
      TabOrder = 7
    end
    object DBListBoxMainMinerals: TDBListBox
      Left = 1
      Top = 407
      Width = 164
      Height = 72
      Align = alBottom
      DataSource = DataSourceMinerals
      ItemHeight = 13
      TabOrder = 8
    end
    object DBRichEditDescription: TDBRichEdit
      Left = 1
      Top = 305
      Width = 164
      Height = 89
      Align = alBottom
      Color = clBtnFace
      DataSource = DataSourceElements
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ReadOnly = True
      TabOrder = 9
      Zoom = 100
    end
  end
  object DataSourceElements: TDataSource
    DataSet = TableElements
    Left = 536
    Top = 336
  end
  object DataSourceMinerals: TDataSource
    DataSet = TableMainMinerals
    Left = 224
    Top = 80
  end
  object TableElements: TTable
    TableName = 'Elements'
    Left = 136
    Top = 80
  end
  object TableMainMinerals: TTable
    Left = 536
    Top = 416
  end
end
