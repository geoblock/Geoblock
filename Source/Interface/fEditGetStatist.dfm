inherited fmEditGetStatist: TfmEditGetStatist
  Left = 207
  Top = 312
  HelpContext = 720
  BorderIcons = [biSystemMenu, biMinimize, biMaximize]
  BorderStyle = bsSizeable
  Caption = 'Statistics'
  ClientHeight = 341
  ClientWidth = 745
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 761
  ExplicitHeight = 380
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 745
    Height = 273
    Align = alClient
    ExplicitWidth = 745
    ExplicitHeight = 273
    object StringGrid: TStringGrid
      Left = 1
      Top = 18
      Width = 743
      Height = 254
      Align = alClient
      ColCount = 7
      DefaultColWidth = 77
      DefaultRowHeight = 20
      FixedCols = 0
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goTabs]
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 0
      ColWidths = (
        77
        77
        77
        77
        77
        77
        77)
      RowHeights = (
        20
        20
        20
        20
        20)
    end
    object HeaderControl: THeaderControl
      Left = 1
      Top = 1
      Width = 743
      Height = 17
      FullDrag = False
      Sections = <
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Field'
          Width = 120
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Minimum'
          Width = 100
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Maximum'
          Width = 100
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Average'
          Width = 100
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Number'
          Width = 80
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Sum'
          Width = 120
        end
        item
          Alignment = taCenter
          ImageIndex = -1
          Text = 'Type'
          Width = 120
        end>
      Style = hsFlat
      OnSectionResize = HeaderControlSectionResize
    end
  end
  inherited PanelMiddle: TPanel
    Top = 273
    Width = 745
    Height = 20
    Align = alBottom
    ExplicitTop = 273
    ExplicitWidth = 745
    ExplicitHeight = 20
    object StatusBar: TStatusBar
      Left = 1
      Top = 1
      Width = 743
      Height = 18
      Panels = <>
    end
  end
  inherited PanelBottom: TPanel
    Top = 293
    Width = 745
    ExplicitTop = 293
    ExplicitWidth = 745
    inherited ButtonOK: TButton
      Left = 887
      OnClick = ButtonOKClick
      ExplicitLeft = 887
    end
    inherited ButtonCancel: TButton
      Left = 1007
      OnClick = ButtonCancelClick
      ExplicitLeft = 1007
    end
    inherited ButtonHelp: TButton
      Left = 1129
      ExplicitLeft = 1129
    end
    object ButtonSaveAs: TButton
      Left = 9
      Top = 12
      Width = 112
      Height = 26
      Hint = 'Save one or more  table'
      HelpContext = 121
      Caption = 'Save As...'
      Default = True
      TabOrder = 3
      OnClick = ButtonSaveAsClick
    end
  end
  object Query: TQuery
    Left = 352
    Top = 81
  end
end
