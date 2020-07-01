inherited fmRecordEditor: TfmRecordEditor
  Left = 535
  Top = 283
  HelpContext = 20
  Caption = 'Record Editor'
  ClientHeight = 358
  ClientWidth = 422
  OldCreateOrder = True
  ExplicitWidth = 428
  ExplicitHeight = 387
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 422
    Height = 57
    ExplicitWidth = 422
    ExplicitHeight = 57
    object LabelModel: TLabel
      Left = 16
      Top = 8
      Width = 38
      Height = 16
      Caption = 'Model'
    end
    object StaticTextModel: TStaticText
      Left = 16
      Top = 24
      Width = 385
      Height = 20
      AutoSize = False
      BorderStyle = sbsSunken
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object StaticTextModelIndex: TStaticText
      Left = 69
      Top = 8
      Width = 44
      Height = 16
      AutoSize = False
      Caption = '1'
      TabOrder = 1
    end
  end
  inherited PanelMiddle: TPanel
    Top = 57
    Width = 422
    Height = 253
    BorderWidth = 20
    ExplicitTop = 57
    ExplicitWidth = 422
    ExplicitHeight = 253
    object HeaderControl: THeaderControl
      Left = 21
      Top = 21
      Width = 380
      Height = 20
      Sections = <
        item
          Alignment = taCenter
          ImageIndex = -1
          MaxWidth = 200
          MinWidth = 200
          Text = 'Attribute'
          Width = 200
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          MaxWidth = 150
          MinWidth = 150
          Text = 'Value'
          Width = 150
        end>
    end
    object StringGrid: TStringGrid
      Left = 21
      Top = 41
      Width = 380
      Height = 191
      Align = alClient
      BorderStyle = bsNone
      Color = clWhite
      ColCount = 2
      DefaultRowHeight = 20
      RowCount = 10
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      ScrollBars = ssVertical
      TabOrder = 1
      OnGetEditText = StringGridGetEditText
      ColWidths = (
        199
        149)
      RowHeights = (
        20
        20
        20
        20
        20
        20
        20
        20
        20
        20)
    end
  end
  inherited PanelBottom: TPanel
    Top = 310
    Width = 422
    ExplicitTop = 310
    ExplicitWidth = 422
    inherited ButtonOK: TButton
      Left = 60
      OnClick = ButtonOKClick
      ExplicitLeft = 60
    end
    inherited ButtonCancel: TButton
      Left = 181
      ExplicitLeft = 181
    end
    inherited ButtonHelp: TButton
      Left = 303
      ExplicitLeft = 303
    end
  end
  object TableEdit: TTable
    Left = 184
    Top = 8
  end
end
