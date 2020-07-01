inherited fmFileOpenText: TfmFileOpenText
  Left = 349
  Top = 204
  HelpContext = 115
  BorderIcons = [biSystemMenu, biMinimize, biMaximize]
  BorderStyle = bsSizeable
  Caption = 'Text'
  ClientHeight = 385
  ClientWidth = 565
  FormStyle = fsMDIChild
  OldCreateOrder = True
  Visible = True
  OnClose = FormClose
  ExplicitWidth = 581
  ExplicitHeight = 424
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 565
    Height = 1
    ExplicitWidth = 565
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 565
    Height = 336
    ExplicitTop = 1
    ExplicitWidth = 565
    ExplicitHeight = 336
    object RichEdit: TRichEdit
      Left = 1
      Top = 1
      Width = 563
      Height = 334
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
    Top = 337
    Width = 565
    ExplicitTop = 337
    ExplicitWidth = 565
    inherited ButtonOK: TButton
      OnClick = ButtonOKClick
    end
    inherited ButtonCancel: TButton
      OnClick = ButtonCancelClick
    end
    inherited ButtonHelp: TButton
      HelpContext = 115
    end
  end
end
