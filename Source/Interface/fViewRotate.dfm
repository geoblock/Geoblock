inherited fmViewRotate: TfmViewRotate
  Left = 434
  Top = 196
  HelpContext = 527
  Caption = 'Rotate'
  ClientHeight = 318
  ClientWidth = 387
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 393
  ExplicitHeight = 347
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 387
    Height = 1
    ExplicitWidth = 387
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 387
    Height = 269
    ExplicitTop = 1
    ExplicitWidth = 387
    ExplicitHeight = 269
    object LabelDistance: TLabel
      Left = 64
      Top = 224
      Width = 53
      Height = 16
      Caption = 'Distance'
    end
    object GroupBoxAzimuth: TGroupBox
      Left = 1
      Top = 1
      Width = 385
      Height = 97
      Align = alTop
      Caption = 'Azimuth'
      TabOrder = 0
      object lbMinBearing: TLabel
        Left = 48
        Top = 64
        Width = 24
        Height = 16
        Caption = '0.00'
      end
      object lbMaxBearing: TLabel
        Left = 216
        Top = 64
        Width = 38
        Height = 16
        Caption = '360.00'
      end
      object ScrollBarAzimuth: TScrollBar
        Left = 47
        Top = 38
        Width = 202
        Height = 20
        Max = 360
        PageSize = 0
        TabOrder = 0
        OnChange = ScrollBarAzimuthChange
      end
      object MaskEditAzimuth: TMaskEdit
        Left = 263
        Top = 35
        Width = 74
        Height = 24
        EditMask = '999;1; '
        MaxLength = 3
        TabOrder = 1
        Text = '   '
        OnChange = MaskEditAzimuthChange
      end
    end
    object GroupBoxSlope: TGroupBox
      Left = 1
      Top = 98
      Width = 385
      Height = 89
      Align = alTop
      Caption = 'Slope'
      TabOrder = 1
      object lbMinSlope: TLabel
        Left = 48
        Top = 64
        Width = 24
        Height = 16
        Caption = '0.00'
      end
      object lbMaxSlope: TLabel
        Left = 216
        Top = 64
        Width = 38
        Height = 16
        Caption = '360.00'
      end
      object ScrollBarSlope: TScrollBar
        Left = 48
        Top = 32
        Width = 201
        Height = 20
        Max = 360
        PageSize = 0
        TabOrder = 0
        OnChange = ScrollBarSlopeChange
      end
      object MaskEditSlope: TMaskEdit
        Left = 263
        Top = 29
        Width = 74
        Height = 24
        EditMask = '999;1; '
        MaxLength = 3
        TabOrder = 1
        Text = '   '
        OnChange = MaskEditSlopeChange
      end
    end
    object EditDistance: TEdit
      Left = 216
      Top = 216
      Width = 121
      Height = 24
      TabOrder = 2
    end
  end
  inherited PanelBottom: TPanel
    Top = 270
    Width = 387
    ExplicitTop = 270
    ExplicitWidth = 387
    inherited ButtonOK: TButton
      Left = 26
      ExplicitLeft = 26
    end
    inherited ButtonCancel: TButton
      Left = 146
      ExplicitLeft = 146
    end
    inherited ButtonHelp: TButton
      Left = 268
      ExplicitLeft = 268
    end
  end
end
