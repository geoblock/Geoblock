inherited fmInfoDialog: TfmInfoDialog
  Left = 317
  Top = 194
  HelpContext = 115
  BorderIcons = [biSystemMenu, biMinimize, biMaximize]
  Caption = 'Information'
  ClientHeight = 302
  ClientWidth = 500
  OldCreateOrder = True
  OnClose = FormClose
  ExplicitWidth = 506
  ExplicitHeight = 331
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 500
    Height = 1
    ExplicitWidth = 500
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 500
    Height = 253
    ExplicitTop = 1
    ExplicitWidth = 500
    ExplicitHeight = 253
    object RichEdit: TRichEdit
      Left = 1
      Top = 1
      Width = 498
      Height = 251
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      Zoom = 100
    end
  end
  inherited PanelBottom: TPanel
    Top = 254
    Width = 500
    ExplicitTop = 254
    ExplicitWidth = 500
    inherited ButtonOK: TButton
      Left = 139
      ExplicitLeft = 139
    end
    inherited ButtonCancel: TButton
      Left = 259
      ExplicitLeft = 259
    end
    inherited ButtonHelp: TButton
      Left = 381
      HelpContext = 115
      ExplicitLeft = 381
    end
  end
end
