object fmInitialDialog: TfmInitialDialog
  Left = 308
  Top = 155
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 448
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  Scaled = False
  TextHeight = 16
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 567
    Height = 41
    Align = alTop
    TabOrder = 0
  end
  object PanelMiddle: TPanel
    Left = 0
    Top = 41
    Width = 567
    Height = 359
    Align = alClient
    TabOrder = 1
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 400
    Width = 567
    Height = 48
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      567
      48)
    object ButtonOK: TButton
      Left = 206
      Top = 10
      Width = 98
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 326
      Top = 10
      Width = 99
      Height = 31
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object ButtonHelp: TButton
      Left = 448
      Top = 10
      Width = 96
      Height = 31
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 2
      OnClick = ButtonHelpClick
    end
  end
end
