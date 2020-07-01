inherited fmEditRenField: TfmEditRenField
  Left = 302
  Top = 201
  Caption = 'Rename Field'
  ClientHeight = 316
  ClientWidth = 378
  OldCreateOrder = True
  OnCreate = FormCreate
  ExplicitWidth = 384
  ExplicitHeight = 345
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 378
    Height = 185
    ExplicitWidth = 378
    ExplicitHeight = 185
    object LabelFieldName: TLabel
      Left = 1
      Top = 1
      Width = 376
      Height = 16
      Align = alTop
      Caption = 'Field name'
      ExplicitWidth = 67
    end
    object ListBoxFieldName: TListBox
      Left = 1
      Top = 17
      Width = 376
      Height = 167
      Align = alClient
      TabOrder = 0
      OnClick = ListBoxFieldNameClick
    end
  end
  inherited PanelMiddle: TPanel
    Top = 185
    Width = 378
    Height = 83
    ExplicitTop = 185
    ExplicitWidth = 378
    ExplicitHeight = 83
    object LabelNewName: TLabel
      Left = 8
      Top = 17
      Width = 64
      Height = 16
      Caption = 'New name'
    end
    object EditNewName: TEdit
      Left = 9
      Top = 41
      Width = 344
      Height = 24
      TabOrder = 0
      OnChange = EditNewNameChange
    end
  end
  inherited PanelBottom: TPanel
    Top = 268
    Width = 378
    ExplicitTop = 268
    ExplicitWidth = 378
    inherited ButtonOK: TButton
      Left = 17
      ExplicitLeft = 17
    end
    inherited ButtonCancel: TButton
      Left = 137
      ExplicitLeft = 137
    end
    inherited ButtonHelp: TButton
      Left = 259
      ExplicitLeft = 259
    end
  end
end
