inherited fmFileEditWhittlePars: TfmFileEditWhittlePars
  Left = 298
  Top = 152
  Caption = 'Parameters'
  ClientHeight = 365
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitHeight = 394
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 25
    ExplicitHeight = 25
    object EditFileName: TEdit
      Left = -94
      Top = 0
      Width = 661
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 0
      Text = 'EditFileName'
      OnChange = EditFileNameChange
    end
  end
  inherited PanelMiddle: TPanel
    Top = 25
    Height = 267
    ExplicitTop = 25
    ExplicitHeight = 267
    object MemoFileContents: TMemo
      Left = 1
      Top = 1
      Width = 565
      Height = 265
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnChange = MemoFileContentsChange
      OnKeyDown = MemoFileContentsKeyUp
      OnKeyUp = MemoFileContentsKeyUp
      OnMouseDown = MemoFileContentsMouseDown
      OnMouseMove = MemoFileContentsMouseMove
    end
  end
  inherited PanelBottom: TPanel
    Top = 292
    Height = 73
    ExplicitTop = 292
    ExplicitHeight = 73
    inherited ButtonOK: TButton
      OnClick = ButtonOKClick
    end
    inherited ButtonCancel: TButton
      OnClick = ButtonCancelClick
    end
    object StatusBar: TStatusBar
      Left = 1
      Top = 53
      Width = 565
      Height = 19
      Panels = <>
    end
  end
end
