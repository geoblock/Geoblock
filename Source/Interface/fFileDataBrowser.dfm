inherited fmFileDataBrowser: TfmFileDataBrowser
  HelpContext = 1100
  BorderIcons = [biSystemMenu, biHelp]
  BorderStyle = bsSizeToolWin
  Caption = 'Data Browser'
  ClientHeight = 447
  ClientWidth = 330
  UseDockManager = True
  DefaultMonitor = dmMainForm
  FormStyle = fsStayOnTop
  PopupMenu = PopupMenu
  Position = poDesigned
  ExplicitWidth = 346
  ExplicitHeight = 486
  PixelsPerInch = 96
  TextHeight = 16
  object PanelTop: TPanel [0]
    Left = 0
    Top = 0
    Width = 330
    Height = 46
    Align = alTop
    BorderWidth = 10
    TabOrder = 0
    DesignSize = (
      330
      46)
    object LabelPath: TLabel
      Left = 8
      Top = 24
      Width = 27
      Height = 16
      Caption = 'Path'
    end
    object SpeedButtonBrowse: TSpeedButton
      Left = 268
      Top = 16
      Width = 25
      Height = 24
      Hint = 'Browse|Browse the directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      ExplicitLeft = 370
    end
    object SpeedButtonDelete: TSpeedButton
      Left = 295
      Top = 14
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
      ExplicitLeft = 397
    end
    object PanelInputPath: TPanel
      Left = 104
      Top = 16
      Width = 144
      Height = 24
      Alignment = taLeftJustify
      Anchors = [akTop, akRight]
      BevelOuter = bvLowered
      TabOrder = 0
    end
  end
  object PanelMiddle: TPanel [1]
    Left = 0
    Top = 46
    Width = 330
    Height = 344
    Align = alClient
    BorderWidth = 10
    TabOrder = 1
    object pgDatabase: TPageControl
      Left = 11
      Top = 11
      Width = 308
      Height = 322
      ActivePage = tsExploring
      Align = alClient
      Style = tsButtons
      TabOrder = 0
      object tsExploring: TTabSheet
        Caption = 'Exploring'
        object TreeView: TTreeView
          Left = 0
          Top = 0
          Width = 300
          Height = 288
          Align = alClient
          AutoExpand = True
          Indent = 35
          PopupMenu = PopupMenu
          RowSelect = True
          ShowButtons = False
          TabOrder = 0
          OnClick = TreeViewClick
        end
      end
      object tsModeling: TTabSheet
        Caption = 'Modeling'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object TreeView1: TTreeView
          Left = 0
          Top = 0
          Width = 300
          Height = 288
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object tsReference: TTabSheet
        Caption = 'Reference'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object TreeView2: TTreeView
          Left = 0
          Top = 0
          Width = 300
          Height = 288
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
    end
  end
  object PanelBottom: TPanel [2]
    Left = 0
    Top = 390
    Width = 330
    Height = 57
    Align = alBottom
    BorderWidth = 10
    TabOrder = 2
    object ToolBarShowAs: TToolBar
      Left = 11
      Top = 11
      Width = 308
      Height = 22
      AutoSize = True
      Color = clBtnFace
      EdgeOuter = esNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Images = ImageListInterface
      ParentColor = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
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
        ImageIndex = 138
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
      end
      object ToolButtonTable: TToolButton
        Tag = 1
        Left = 31
        Top = 0
        Hint = 'Table|Show the result as table'
        Caption = 'Show as table'
        ImageIndex = 139
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
      end
      object ToolButtonGraph: TToolButton
        Tag = 2
        Left = 54
        Top = 0
        Hint = 'Graph|Show the result as Vcl.Vcl.CheckLst,'
        Caption = 'Show as graph'
        ImageIndex = 140
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
      end
      object ToolButton1: TToolButton
        Left = 77
        Top = 0
        Width = 132
        Caption = 'ToolButton1'
        ImageIndex = 141
        Style = tbsSeparator
      end
      object tbCollapse: TToolButton
        Left = 209
        Top = 0
        Hint = 'Collapse|Full Collapse'
        Caption = 'Collapse'
        ImageIndex = 133
        ParentShowHint = False
        ShowHint = True
        OnClick = tbCollapseClick
      end
      object tbExpand: TToolButton
        Left = 232
        Top = 0
        Hint = 'Expand|Full Expand'
        Caption = 'Expand'
        ImageIndex = 132
        ParentShowHint = False
        ShowHint = True
        Style = tbsCheck
        OnClick = tbExpandClick
      end
    end
  end
  inherited ImageListInterface: TImageList
    Left = 51
    Top = 72
  end
  inherited ImageListPictures: TImageList
    Left = 35
    Top = 187
  end
  object PopupMenu: TPopupMenu
    Left = 48
    Top = 128
    object Cut1: TMenuItem
      Caption = 'Cut'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
    object Delete1: TMenuItem
      Caption = 'Delete'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Rename1: TMenuItem
      Caption = 'Rename '
    end
    object New1: TMenuItem
      Caption = 'New...'
    end
  end
end
