inherited fmViewScale: TfmViewScale
  Tag = 1
  Left = 338
  Top = 193
  HelpContext = 510
  Caption = 'Scale'
  ClientHeight = 341
  ClientWidth = 381
  OldCreateOrder = True
  ExplicitWidth = 387
  ExplicitHeight = 370
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 381
    Height = 1
    ExplicitWidth = 381
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 381
    Height = 292
    ExplicitTop = 1
    ExplicitWidth = 381
    ExplicitHeight = 292
    object RadioGroupScale: TRadioGroup
      Left = 1
      Top = 1
      Width = 379
      Height = 290
      Align = alClient
      Columns = 2
      ItemIndex = 2
      Items.Strings = (
        '200'
        '500'
        '1000'
        '2000'
        '5000'
        '10000'
        '20000'
        '50000'
        '100000'
        ' ')
      TabOrder = 0
    end
    object EditScale: TEdit
      Left = 212
      Top = 242
      Width = 85
      Height = 24
      AutoSize = False
      TabOrder = 1
      OnEnter = EditScaleEnter
      OnExit = EditScaleExit
    end
  end
  inherited PanelBottom: TPanel
    Top = 293
    Width = 381
    ExplicitTop = 293
    ExplicitWidth = 381
    inherited ButtonOK: TButton
      Left = 20
      ExplicitLeft = 20
    end
    inherited ButtonCancel: TButton
      Left = 140
      ExplicitLeft = 140
    end
    inherited ButtonHelp: TButton
      Left = 262
      ExplicitLeft = 262
    end
  end
end
