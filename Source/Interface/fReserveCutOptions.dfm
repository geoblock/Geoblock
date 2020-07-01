inherited fmReserveCutOptions: TfmReserveCutOptions
  Left = 337
  Top = 202
  HelpContext = 713
  Caption = 'Cutting Options'
  ClientHeight = 263
  ClientWidth = 473
  OldCreateOrder = True
  ExplicitWidth = 479
  ExplicitHeight = 292
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 473
    Height = 1
    ExplicitWidth = 473
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 473
    Height = 214
    ExplicitTop = 1
    ExplicitWidth = 473
    ExplicitHeight = 214
    object GroupBoxMode: TGroupBox
      Left = 1
      Top = 1
      Width = 471
      Height = 212
      Align = alClient
      Caption = 'Mode'
      TabOrder = 0
      object Label1: TLabel
        Left = 25
        Top = 64
        Width = 51
        Height = 16
        Caption = 'Ore type'
      end
      object ListBoxOreTypes: TListBox
        Left = 25
        Top = 82
        Width = 200
        Height = 74
        Sorted = True
        TabOrder = 0
      end
      object ListBoxOreSorts: TListBox
        Left = 265
        Top = 82
        Width = 192
        Height = 74
        Sorted = True
        TabOrder = 1
      end
      object CheckBoxOreTypeAndSort: TCheckBox
        Left = 265
        Top = 62
        Width = 192
        Height = 17
        Caption = 'Ore type and sort'
        TabOrder = 2
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 215
    Width = 473
    ExplicitTop = 215
    ExplicitWidth = 473
    inherited ButtonOK: TButton
      Left = 110
      Default = True
      ExplicitLeft = 110
    end
    inherited ButtonCancel: TButton
      Left = 231
      ExplicitLeft = 231
    end
    inherited ButtonHelp: TButton
      Left = 353
      ExplicitLeft = 353
    end
  end
end
