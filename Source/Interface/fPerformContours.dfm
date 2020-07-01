inherited fmPerformContours: TfmPerformContours
  Tag = 1
  Left = 377
  Top = 190
  HelpContext = 495
  Caption = 'Features of Contours'
  ClientHeight = 315
  ClientWidth = 489
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 495
  ExplicitHeight = 344
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 489
    Height = 1
    ExplicitWidth = 489
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 489
    Height = 266
    ExplicitTop = 1
    ExplicitWidth = 489
    ExplicitHeight = 266
    object RadioGroupShowAs: TRadioGroup
      Left = 1
      Top = 1
      Width = 487
      Height = 105
      Align = alTop
      Caption = 'Show as'
      ItemIndex = 0
      Items.Strings = (
        'Line segments'
        'Spline smoothing')
      TabOrder = 1
    end
    object GroupBoxMark: TGroupBox
      Left = 1
      Top = 106
      Width = 487
      Height = 159
      Align = alClient
      Caption = 'Mark'
      TabOrder = 0
      object LabelBetweenMarks: TLabel
        Left = 40
        Top = 104
        Width = 92
        Height = 16
        Caption = 'Between marks'
      end
      object LabelMarkEvery: TLabel
        Left = 280
        Top = 104
        Width = 67
        Height = 16
        Caption = 'Mark every'
      end
      object CheckBoxPlaceMarks: TCheckBox
        Left = 24
        Top = 40
        Width = 145
        Height = 25
        Caption = 'Place marks'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 0
        OnClick = CheckBoxPlaceMarksClick
      end
      object PanelFont: TPanel
        Left = 200
        Top = 16
        Width = 97
        Height = 41
        BevelWidth = 3
        BorderStyle = bsSingle
        Caption = '- 123 -'
        ParentColor = True
        TabOrder = 1
        OnClick = PanelFontClick
      end
      object SpinEditMarkEvery: TSpinEdit
        Left = 400
        Top = 96
        Width = 49
        Height = 26
        MaxValue = 100
        MinValue = 1
        TabOrder = 2
        Value = 5
      end
      object SpinEditExpandMarks: TSpinEdit
        Left = 200
        Top = 96
        Width = 57
        Height = 26
        MaxValue = 1000
        MinValue = 20
        TabOrder = 3
        Value = 100
      end
    end
    object StaticTextOutline: TStaticText
      Left = 232
      Top = 32
      Width = 82
      Height = 20
      Caption = 'Outline every'
      TabOrder = 2
    end
    object SpinEditOutline: TSpinEdit
      Left = 392
      Top = 24
      Width = 57
      Height = 26
      MaxValue = 10
      MinValue = 1
      TabOrder = 3
      Value = 1
    end
  end
  inherited PanelBottom: TPanel
    Top = 267
    Width = 489
    BevelInner = bvRaised
    BevelOuter = bvNone
    ExplicitTop = 267
    ExplicitWidth = 489
    inherited ButtonOK: TButton
      Left = 127
      OnClick = ButtonOKClick
      ExplicitLeft = 127
    end
    inherited ButtonCancel: TButton
      Left = 248
      ExplicitLeft = 248
    end
    inherited ButtonHelp: TButton
      Left = 370
      ExplicitLeft = 370
    end
  end
end
