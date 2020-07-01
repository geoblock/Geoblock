inherited fmFileOpenDialog: TfmFileOpenDialog
  Left = 422
  Top = 224
  BorderWidth = 5
  Caption = 'File Open Dialog'
  ClientHeight = 416
  ClientWidth = 540
  OldCreateOrder = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 556
  ExplicitHeight = 455
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 540
    Height = 1
    BevelOuter = bvNone
    BorderWidth = 5
    ExplicitWidth = 540
    ExplicitHeight = 1
  end
  inherited PanelMiddle: TPanel
    Top = 1
    Width = 540
    Height = 364
    BevelOuter = bvNone
    BorderWidth = 5
    ExplicitTop = 1
    ExplicitWidth = 540
    ExplicitHeight = 364
    DesignSize = (
      540
      364)
    object SpeedButton1: TSpeedButton
      Left = 458
      Top = 26
      Width = 24
      Height = 24
      Hint = 'Browse|Browse the directory up one level'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButtonBrowseClick
    end
    object SpeedButton2: TSpeedButton
      Left = 500
      Top = 26
      Width = 24
      Height = 24
      Hint = 'Delete|Delete the current table with associated files'
      Anchors = [akTop, akRight]
      Enabled = False
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        040000000000800000007A120000701200001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDDDDDDDDDDDDDDDD0DDDDDDDDDD0DDD000DDDDDDDDDDDDD0000DDDDDD
        D0DDDDD000DDDDDD0DDDDDDD000DDDD00DDDDDDDD000DD00DDDDDDDDDD00000D
        DDDDDDDDDDD000DDDDDDDDDDDD00000DDDDDDDDDD000DD00DDDDDDD0000DDDD0
        0DDDDD0000DDDDDD00DDDD000DDDDDDDDD0DDDDDDDDDDDDDDDDD}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButtonDeleteClick
    end
    object PageControl: TPageControl
      Left = 5
      Top = 5
      Width = 530
      Height = 84
      Align = alTop
      Images = fmInitialForm.ImageListInterface
      MultiLine = True
      TabOrder = 0
      OnChange = PageControlChange
    end
    object PanelDirectory: TPanel
      Left = 5
      Top = 89
      Width = 530
      Height = 49
      Align = alTop
      TabOrder = 1
      DesignSize = (
        530
        49)
      object SpeedButtonBrowse: TSpeedButton
        Left = 465
        Top = 19
        Width = 25
        Height = 24
        Hint = 'Browse|Browse the directory'
        Anchors = [akTop, akRight]
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButtonBrowseClick
      end
      object SpeedButtonDelete: TSpeedButton
        Left = 496
        Top = 19
        Width = 24
        Height = 24
        Hint = 'Delete|Delete the current table with associated files'
        Anchors = [akTop, akRight]
        Enabled = False
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          040000000000800000007A120000701200001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDDDDDDDDDDDDDDDDDDD0DDDDDDDDDD0DDD000DDDDDDDDDDDDD0000DDDDDD
          D0DDDDD000DDDDDD0DDDDDDD000DDDD00DDDDDDDD000DD00DDDDDDDDDD00000D
          DDDDDDDDDDD000DDDDDDDDDDDD00000DDDDDDDDDD000DD00DDDDDDD0000DDDD0
          0DDDDD0000DDDDDD00DDDD000DDDDDDDDD0DDDDDDDDDDDDDDDDD}
        ParentShowHint = False
        ShowHint = True
        OnClick = SpeedButtonDeleteClick
      end
      object LabelPath: TLabel
        Left = 8
        Top = 24
        Width = 27
        Height = 16
        Caption = 'Path'
      end
      object PanelInputPath: TPanel
        Left = 60
        Top = 15
        Width = 393
        Height = 24
        Alignment = taLeftJustify
        Anchors = [akTop, akRight]
        BevelOuter = bvLowered
        TabOrder = 0
      end
    end
    object ListView: TListView
      Left = 5
      Top = 138
      Width = 530
      Height = 167
      Align = alClient
      AllocBy = 100
      Columns = <
        item
          Caption = 'Name'
          Width = 300
        end
        item
          Caption = 'Size'
          Width = 100
        end
        item
          Caption = 'Changed'
          Width = 100
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      IconOptions.AutoArrange = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      SortType = stText
      TabOrder = 2
      ViewStyle = vsReport
      OnChange = ListViewChange
      OnDblClick = ListViewDblClick
    end
    object GroupBoxOutput: TGroupBox
      Left = 5
      Top = 305
      Width = 530
      Height = 54
      Align = alBottom
      Caption = 'Output'
      TabOrder = 3
      object ToolBarShowAs: TToolBar
        Left = 2
        Top = 18
        Width = 526
        Height = 22
        AutoSize = True
        Color = clBtnFace
        EdgeOuter = esNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Images = fmInitialForm.ImageListInterface
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        DesignSize = (
          526
          22)
        object ToolButton3: TToolButton
          Left = 0
          Top = 0
          Width = 8
          Style = tbsSeparator
        end
        object ToolButtonMap: TToolButton
          Left = 8
          Top = 0
          Hint = 'Map|Show the result as map'
          Caption = 'Show as map'
          Grouped = True
          ImageIndex = 138
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ToolButtonShowAsClick
        end
        object ToolButtonTable: TToolButton
          Tag = 1
          Left = 31
          Top = 0
          Hint = 'Table|Show the result as table'
          Caption = 'Show as table'
          Grouped = True
          ImageIndex = 139
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ToolButtonShowAsClick
        end
        object ToolButtonGraph: TToolButton
          Tag = 2
          Left = 54
          Top = 0
          Hint = 'Graph|Show the result as Vcl.Vcl.CheckLst,'
          Caption = 'Show as graph'
          Grouped = True
          ImageIndex = 140
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ToolButtonShowAsClick
        end
        object EditOutName: TEdit
          Left = 77
          Top = 0
          Width = 136
          Height = 22
          Anchors = [akTop, akBottom]
          AutoSelect = False
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 365
    Width = 540
    Height = 51
    BevelOuter = bvNone
    ExplicitTop = 365
    ExplicitWidth = 540
    ExplicitHeight = 51
    DesignSize = (
      540
      51)
    inherited ButtonOK: TButton
      Left = 230
      Top = 9
      Enabled = False
      ExplicitLeft = 230
      ExplicitTop = 9
    end
    inherited ButtonCancel: TButton
      Left = 334
      ExplicitLeft = 334
    end
    inherited ButtonHelp: TButton
      Left = 439
      ExplicitLeft = 439
    end
    object PanelLeft: TPanel
      Left = 0
      Top = 0
      Width = 16
      Height = 51
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
end
