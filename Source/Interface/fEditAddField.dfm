inherited fmEditAddField: TfmEditAddField
  Left = 306
  Top = 179
  Caption = 'Add Field'
  ClientHeight = 368
  ClientWidth = 568
  OldCreateOrder = True
  OnCreate = FormCreate
  ExplicitWidth = 574
  ExplicitHeight = 397
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 568
    Height = 97
    ExplicitWidth = 568
    ExplicitHeight = 97
    object LabelName: TLabel
      Left = 30
      Top = 20
      Width = 37
      Height = 16
      Caption = 'Name'
    end
    object LabelSize: TLabel
      Left = 418
      Top = 20
      Width = 26
      Height = 16
      Caption = 'Size'
    end
    object EditFieldName: TEdit
      Left = 30
      Top = 47
      Width = 379
      Height = 24
      TabOrder = 0
      Text = 'NUMBER'
    end
    object SpinEditSize: TSpinEdit
      Left = 418
      Top = 47
      Width = 66
      Height = 26
      MaxValue = 32
      MinValue = 1
      TabOrder = 1
      Value = 32
    end
  end
  inherited PanelMiddle: TPanel
    Top = 97
    Width = 568
    Height = 223
    ExplicitTop = 97
    ExplicitWidth = 568
    ExplicitHeight = 223
    object RadioGroupType: TRadioGroup
      Left = 1
      Top = 1
      Width = 566
      Height = 221
      Align = alClient
      Caption = 'Type'
      Columns = 3
      ItemIndex = 12
      Items.Strings = (
        'Alpha'
        'Autoincrement'
        'BCD'
        'Binary'
        'Bytes'
        'Date'
        'Fmemo'
        'Graphic'
        'Logical'
        'Long'
        'Memo'
        'Money'
        'Number'
        'OLE  '
        'Short'
        'Time'
        'Timestamp')
      TabOrder = 0
      OnClick = RadioGroupTypeClick
    end
  end
  inherited PanelBottom: TPanel
    Top = 320
    Width = 568
    ExplicitTop = 320
    ExplicitWidth = 568
    inherited ButtonOK: TButton
      Left = 207
      OnClick = ButtonOKClick
      ExplicitLeft = 207
    end
    inherited ButtonCancel: TButton
      Left = 327
      ExplicitLeft = 327
    end
    inherited ButtonHelp: TButton
      Left = 449
      ExplicitLeft = 449
    end
  end
end
