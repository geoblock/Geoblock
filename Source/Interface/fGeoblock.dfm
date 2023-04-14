object fmGeoblock: TfmGeoblock
  Left = 17
  Top = 26
  Anchors = []
  Caption = 'Geoblock '
  ClientHeight = 651
  ClientWidth = 997
  Color = clAppWorkSpace
  TransparentColorValue = clActiveCaption
  Constraints.MinWidth = 112
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Times New Roman'
  Font.Style = []
  FormStyle = fsMDIForm
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF004444
    4444444444444444444444444444444444444444444444444444444444444444
    4444444444444444444444444444488888888888888888888884444444444800
    00000000000000000088444444444801111111111111110BB008844444444801
    11111111111110BBB010844444444801111111110BBBBBBBB000884444444801
    1111110BBBBBBBB000B108844444480111110BBBBBBBB00660B0108844444801
    110BBBBBBB066666600B11084444480110BBBBBB06666666606B110884444801
    0BBBB00666666666606BB11088444800BBB0666666666666606BBB110844480B
    B0666666666666666066BB110844480B06666666666666666066BBB108444806
    666666666666660EE0660BB108444806666666666666EEEEE0E66BBB08444806
    66666666660EEEEEE0E660BB08444800000000000000000000EE66BB08444080
    6666666666666EEEEE0E660B08444448006666666666660EEEE0666B08444444
    88000006666666666EEE0660084444444880BBBBBBB066666666006608444444
    44880B000BBBBB66666660660844444444488001110BBBB06666660608444444
    4444888011110BBB066666600844444444444888000000000000000008444444
    4444448888888888888888888844444444444444444444444444444444444444
    4444444444444444444444444444444444444444444444444444444444440000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000400000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  PrintScale = poPrintToFit
  Scaled = False
  ShowHint = True
  WindowState = wsMaximized
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 19
  object StatusBar: TStatusBar
    Left = 0
    Top = 625
    Width = 997
    Height = 26
    AutoHint = True
    Panels = <
      item
        Width = 500
      end
      item
        Text = 'X:'
        Width = 100
      end
      item
        Text = 'Y:'
        Width = 100
      end
      item
        Text = 'Z:'
        Width = 100
      end>
    SimplePanel = True
  end
  object ControlBarTop: TControlBar
    Left = 0
    Top = 30
    Width = 997
    Height = 66
    Align = alTop
    AutoDrag = False
    BevelEdges = [beTop, beBottom]
    BevelKind = bkFlat
    Color = clBtnFace
    Constraints.MinHeight = 20
    DragMode = dmAutomatic
    ParentColor = False
    ParentShowHint = False
    RowSize = 30
    ShowHint = False
    TabOrder = 1
    TabStop = True
    object PanelModelPalette: TPanel
      Left = 480
      Top = 2
      Width = 718
      Height = 56
      BevelOuter = bvNone
      BiDiMode = bdLeftToRight
      Caption = 'Model Palette'
      Constraints.MaxHeight = 90
      Constraints.MinHeight = 54
      Ctl3D = True
      DockSite = True
      DragKind = dkDock
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      object PageControlModels: TPageControl
        Left = 0
        Top = 0
        Width = 718
        Height = 56
        ActivePage = TabSheetSolids
        Constraints.MinHeight = 54
        Constraints.MinWidth = 20
        DragKind = dkDock
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Style = tsFlatButtons
        TabOrder = 0
        object TabSheetHoles: TTabSheet
          Caption = 'Dholes'
          ImageIndex = 32
          object ActionToolBarDrillholes: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 22
            ActionManager = ActionManager
            Align = alClient
            Caption = 'Dholes'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetPoints2D: TTabSheet
          Tag = 1
          Caption = 'Points 2D'
          ImageIndex = 38
          object ActionToolBarPoints2D: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 22
            ActionManager = ActionManager
            Align = alClient
            Caption = 'Points 2D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetPoints3D: TTabSheet
          Tag = 2
          Caption = 'Points 3D'
          ImageIndex = 41
          object ActionToolBarPoints3D: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Points 3D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetPolygons: TTabSheet
          Tag = 3
          Caption = 'Polygons'
          ImageIndex = 44
          object ActionToolBarPolygons: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Polygons'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetTins: TTabSheet
          Tag = 4
          Caption = 'Tin'
          ImageIndex = 48
          object ActionToolBarTin: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Tin'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetSolids: TTabSheet
          Tag = 5
          Caption = 'Solids'
          ImageIndex = 66
          object ActionToolBarSolids: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Solids'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetGrids2D: TTabSheet
          Tag = 6
          Caption = 'Grid 2D'
          ImageIndex = 55
          object ToolBarGrids2D: TToolBar
            Left = 471
            Top = 0
            Width = 239
            Height = 22
            Align = alRight
            AutoSize = True
            ButtonHeight = 26
            ButtonWidth = 27
            Caption = 'Grids 2D'
            Constraints.MinHeight = 20
            Constraints.MinWidth = 20
            DragKind = dkDock
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            object StaticTextGrid2DRow: TStaticText
              Left = 0
              Top = 0
              Width = 47
              Height = 26
              Alignment = taCenter
              AutoSize = False
              BevelInner = bvNone
              Caption = 'Row'
              TabOrder = 0
            end
            object SpinEditGrid2DRow: TSpinEdit
              Left = 47
              Top = 0
              Width = 63
              Height = 26
              MaxValue = 100000
              MinValue = 1
              TabOrder = 3
              Value = 1
            end
            object ToolButton2: TToolButton
              Left = 110
              Top = 0
              Width = 8
              Caption = 'ToolButton2'
              Enabled = False
              ImageIndex = 33
              Style = tbsSeparator
            end
            object StaticTextGrid2DColumn: TStaticText
              Left = 118
              Top = 0
              Width = 58
              Height = 26
              Alignment = taCenter
              AutoSize = False
              BevelInner = bvNone
              Caption = 'Column'
              TabOrder = 1
            end
            object SpinEditGrid2DColumn: TSpinEdit
              Left = 176
              Top = 0
              Width = 63
              Height = 26
              MaxValue = 100000
              MinValue = 1
              TabOrder = 2
              Value = 1
            end
          end
          object ActionToolBarGrid2D: TActionToolBar
            Left = 0
            Top = 0
            Width = 471
            Height = 22
            ActionManager = ActionManager
            Align = alClient
            Caption = 'Grid 2D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetGrids3D: TTabSheet
          Tag = 7
          Caption = 'Grid 3D'
          ImageIndex = 60
          object ToolBarGrids3D: TToolBar
            Left = 369
            Top = 0
            Width = 341
            Height = 22
            Align = alRight
            AutoSize = True
            ButtonHeight = 26
            ButtonWidth = 25
            Caption = 'Grids 3D'
            Constraints.MinHeight = 20
            Constraints.MinWidth = 20
            DragKind = dkDock
            List = True
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            object StaticTextGrid3DRow: TStaticText
              Left = 0
              Top = 0
              Width = 47
              Height = 26
              Alignment = taCenter
              AutoSize = False
              BevelInner = bvNone
              Caption = 'Row'
              TabOrder = 4
            end
            object SpinEditGrid3DRow: TSpinEdit
              Left = 47
              Top = 0
              Width = 63
              Height = 26
              MaxValue = 100000
              MinValue = 1
              TabOrder = 2
              Value = 1
            end
            object StaticTextGrid3DColumn: TStaticText
              Left = 110
              Top = 0
              Width = 58
              Height = 26
              Alignment = taCenter
              AutoSize = False
              BevelInner = bvNone
              Caption = 'Column'
              TabOrder = 3
            end
            object SpinEditGrid3DColumn: TSpinEdit
              Left = 168
              Top = 0
              Width = 63
              Height = 26
              MaxValue = 100000
              MinValue = 1
              TabOrder = 1
              Value = 1
            end
            object StaticTextGrid3DLayer: TStaticText
              Left = 231
              Top = 0
              Width = 47
              Height = 26
              Alignment = taCenter
              AutoSize = False
              BevelInner = bvNone
              Caption = 'Layer'
              TabOrder = 5
            end
            object SpinEditGrid3DLayer: TSpinEdit
              Left = 278
              Top = 0
              Width = 63
              Height = 26
              MaxValue = 100000
              MinValue = 1
              TabOrder = 0
              Value = 1
            end
          end
          object ActionToolBarGrid3D: TActionToolBar
            Left = 0
            Top = 0
            Width = 177
            Height = 25
            ActionManager = ActionManager
            Align = alNone
            Caption = 'Grid 3D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetMeshes2D: TTabSheet
          Tag = 8
          Caption = 'Mesh 2D'
          ImageIndex = 69
          object ActionToolBarMesh2D: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Mesh 2D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetMeshes3D: TTabSheet
          Tag = 9
          Caption = 'Mesh 3D'
          ImageIndex = 73
          object ActionToolBarMesh3D: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Mesh 3D'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetDrawings: TTabSheet
          Caption = 'Drawings'
          ImageIndex = 163
          object ActionToolBarDrawing: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 26
            ActionManager = ActionManager
            Caption = 'Drawings'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            DragKind = dkDrag
            DragMode = dmAutomatic
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            HorzSeparator = False
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            Spacing = 0
          end
        end
        object TabSheetPolylines: TTabSheet
          Caption = 'Polylines'
          ImageIndex = 112
          object ActionToolBarPolylines: TActionToolBar
            Left = 0
            Top = 0
            Width = 710
            Height = 29
            Caption = 'Polylines'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Spacing = 0
          end
        end
      end
    end
    object ActionToolBarStandard: TActionToolBar
      Left = 11
      Top = 2
      Width = 158
      Height = 30
      ActionManager = ActionManager
      Caption = 'Standard'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
    object ActionToolBarEdit: TActionToolBar
      Left = 185
      Top = 2
      Width = 88
      Height = 26
      ActionManager = ActionManager
      Caption = 'Edit'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
    object ActionToolBarMap: TActionToolBar
      Left = 286
      Top = 2
      Width = 89
      Height = 26
      ActionManager = ActionManager
      Caption = 'Map'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
    object ActionToolBarView: TActionToolBar
      Left = 11
      Top = 32
      Width = 360
      Height = 26
      ActionManager = ActionManager
      Caption = 'View'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
    object ActionToolBarAnalyse: TActionToolBar
      Left = 388
      Top = 2
      Width = 62
      Height = 26
      ActionManager = ActionManager
      Caption = 'Analyse'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
    object ActionToolBarMethod: TActionToolBar
      Left = 384
      Top = 32
      Width = 81
      Height = 26
      ActionManager = ActionManager
      Caption = 'Method'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Spacing = 0
    end
  end
  object ControlBarBottom: TControlBar
    Left = 0
    Top = 600
    Width = 997
    Height = 25
    Align = alBottom
    AutoDrag = False
    BevelEdges = [beTop]
    BevelKind = bkFlat
    Color = clBtnFace
    DragMode = dmAutomatic
    ParentColor = False
    RowSize = 32
    TabOrder = 2
    Visible = False
  end
  object ControlBarRight: TControlBar
    Left = 928
    Top = 96
    Width = 69
    Height = 504
    Align = alRight
    Anchors = [akRight]
    AutoDrag = False
    BevelEdges = [beLeft]
    BevelInner = bvLowered
    BevelOuter = bvRaised
    BevelKind = bkFlat
    Color = clBtnFace
    DragMode = dmAutomatic
    ParentColor = False
    RowSnap = False
    TabOrder = 3
    Visible = False
    object ActionToolBarObserve: TActionToolBar
      Left = 14
      Top = 2
      Width = 25
      Height = 239
      ActionManager = ActionManager
      Align = alClient
      Caption = 'Observe'
      Color = clMenuBar
      ColorMap.DisabledFontColor = 7171437
      ColorMap.HighlightColor = clWhite
      ColorMap.BtnSelectedFont = clBlack
      ColorMap.UnusedColor = clWhite
      Constraints.MinHeight = 25
      Constraints.MinWidth = 25
      DragKind = dkDrag
      DragMode = dmAutomatic
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      HorzSeparator = False
      ParentFont = False
      Spacing = 0
    end
  end
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 997
    Height = 30
    UseSystemFont = False
    ActionManager = ActionManager
    AnimationStyle = asNone
    Caption = 'Main Menu'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 96
    Width = 289
    Height = 504
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    ParentBackground = False
    TabOrder = 5
    object PanelBottom: TPanel
      Left = 1
      Top = 446
      Width = 287
      Height = 57
      Align = alBottom
      BorderWidth = 10
      TabOrder = 0
      object ToolBarShowAs: TToolBar
        Left = 11
        Top = 11
        Width = 265
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
        end
      end
    end
    object PanelTop: TPanel
      Left = 1
      Top = 1
      Width = 287
      Height = 46
      Align = alTop
      BorderWidth = 10
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Times New Roman'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      DesignSize = (
        287
        46)
      object LabelPath: TLabel
        Left = 8
        Top = 24
        Width = 24
        Height = 15
        Caption = 'Path'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Times New Roman'
        Font.Style = []
        ParentFont = False
      end
      object SpeedButtonBrowse: TSpeedButton
        Left = 202
        Top = 16
        Width = 25
        Height = 24
        Hint = 'Browse|Browse the directory'
        Anchors = [akTop, akRight]
        Caption = '...'
        ParentShowHint = False
        ShowHint = True
      end
      object SpeedButtonDelete: TSpeedButton
        Left = 243
        Top = 16
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
      end
      object PanelInputPath: TPanel
        Left = 80
        Top = 16
        Width = 116
        Height = 24
        Alignment = taLeftJustify
        Anchors = [akTop, akRight]
        BevelOuter = bvLowered
        TabOrder = 0
      end
    end
    object PanelMiddle: TPanel
      Left = 1
      Top = 47
      Width = 287
      Height = 399
      Align = alClient
      BorderWidth = 10
      TabOrder = 2
      object pgDatabase: TPageControl
        Left = 11
        Top = 11
        Width = 265
        Height = 377
        ActivePage = tsReference
        Align = alClient
        Style = tsButtons
        TabOrder = 0
        object tsExploring: TTabSheet
          Caption = 'Database'
          object TreeView: TTreeView
            Left = 0
            Top = 0
            Width = 257
            Height = 340
            Align = alClient
            AutoExpand = True
            Indent = 35
            RowSelect = True
            ShowButtons = False
            TabOrder = 0
          end
        end
        object tsModeling: TTabSheet
          Caption = 'Models'
          ImageIndex = 1
          object TreeView1: TTreeView
            Left = 0
            Top = 0
            Width = 257
            Height = 340
            Align = alClient
            Indent = 19
            TabOrder = 0
          end
        end
        object tsReference: TTabSheet
          Caption = 'Catalogs'
          ImageIndex = 2
          object TreeView2: TTreeView
            Left = 0
            Top = 0
            Width = 257
            Height = 340
            Align = alClient
            Indent = 19
            TabOrder = 0
          end
        end
      end
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        ContextItems = <
          item
            Items = <
              item
                Caption = '&ActionClientItem0'
              end>
            Caption = '&ActionClientItem0'
          end>
        Items = <
          item
            Caption = 'F&ile'
          end
          item
            Items = <
              item
                Action = EditCopy
                Caption = '&Copy'
                ImageIndex = 168
                ShortCut = 16451
              end
              item
                Action = EditPaste
                Caption = '&Paste'
                ImageIndex = 169
                ShortCut = 16470
              end
              item
                Action = EditSelectAll
                Caption = '&Select All'
                ShortCut = 16449
              end
              item
                Action = EditUndo
                Caption = '&Undo'
                ImageIndex = 170
                ShortCut = 16474
              end
              item
                Action = EditDelete
                Caption = '&Delete'
                ImageIndex = 171
                ShortCut = 46
              end>
            Caption = '&Edit'
          end
          item
            Caption = '&Tools'
          end
          item
            Items = <
              item
                Action = WindowClose
                Caption = '&Close'
              end
              item
                Action = WindowCascade
                Caption = 'C&ascade'
                ImageIndex = 178
              end
              item
                Action = WindowTileHorizontal
                Caption = '&Tile Horizontally'
                ImageIndex = 179
              end
              item
                Action = WindowTileVertical
                Caption = 'T&ile Vertically'
                ImageIndex = 180
              end
              item
                Action = WindowMinimizeAll
                Caption = '&Minimize All'
              end
              item
                Action = WindowArrange
                Caption = 'A&rrange'
              end>
            Caption = '&Window'
          end
          item
            Caption = '&Help'
          end>
        Visible = False
      end
      item
      end
      item
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Items = <
              item
                Action = FileOpenProject
                Caption = '&Open Project...'
                ImageIndex = 1
                ShortCut = 16506
              end
              item
                Action = FileOpenModel
                Caption = 'Op&en Model...'
                ImageIndex = 4
                ShortCut = 114
              end
              item
                Action = FileOpenImage
                Caption = 'Open &Image...'
              end
              item
                Action = FileOpenText
                Caption = 'Open &Text...'
              end
              item
                Caption = '-'
              end
              item
                Action = FileSave
                Caption = '&Save'
                ImageIndex = 6
                ShortCut = 113
              end
              item
                Action = FileSaveAs
                Caption = 'S&ave As...'
                ImageIndex = 161
                ShortCut = 16467
              end
              item
                Action = FileSaveProjectAs
                Caption = 'Sa&ve Project As...'
              end
              item
                Action = FileClose
                Caption = '&Close'
              end
              item
                Action = FileCloseAll
                Caption = 'C&lose All'
              end
              item
                Caption = '-'
              end
              item
                Action = FileImport
                Caption = 'I&mport...'
              end
              item
                Action = FileExport
                Caption = 'E&xport...'
              end
              item
                Caption = '-'
              end
              item
                Action = FilePrint
                Caption = '&Print...'
                ImageIndex = 7
                ShortCut = 16464
              end
              item
                Action = FilePrintPreview
                Caption = 'Print Previe&w...'
              end
              item
                Caption = '-'
              end
              item
                Action = FileExit
                ImageIndex = 162
                ShortCut = 32856
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = EditUndo
                Caption = '&Undo'
                ImageIndex = 155
                ShortCut = 16474
              end
              item
                Action = EditCut
                Caption = '&Cut'
                ImageIndex = 11
                ShortCut = 16472
              end
              item
                Action = EditCopy
                Caption = 'C&opy'
                ImageIndex = 12
                ShortCut = 16451
              end
              item
                Action = EditPaste
                Caption = '&Paste'
                ImageIndex = 13
                ShortCut = 16470
              end
              item
                Action = EditSelectAll
                Caption = '&Select All'
                ShortCut = 16449
              end
              item
                Action = EditDelete
                Caption = '&Delete'
                ImageIndex = 14
                ShortCut = 46
              end
              item
                Caption = '-'
              end
              item
                Action = EditFind
                Caption = '&Find...'
                ShowShortCut = False
              end>
            Caption = '&Edit'
          end
          item
            Items = <
              item
                Items = <
                  item
                    Action = CompositingSampleContacts
                    Caption = '&Sample Contacts...'
                    ImageIndex = 21
                  end
                  item
                    Action = CompositingCenters
                    Caption = '&Coordinates of Centers...'
                    ImageIndex = 22
                  end
                  item
                    Visible = False
                    Action = CompositingInsideHorizons
                    Caption = '&Inside Horizons...'
                    ImageIndex = 26
                  end
                  item
                    Action = CompositingOreIntervals
                    Caption = '&Ore Intervals...'
                    ImageIndex = 23
                  end
                  item
                    Action = CompositingOreSorting
                    Caption = 'O&re Sorting...'
                    ImageIndex = 25
                  end
                  item
                    Action = CompositingLinearReserves
                    Caption = '&Linear Reserves...'
                    ImageIndex = 24
                  end>
                Caption = '&Compositing'
              end
              item
                Action = MethodGridGeneration
                Caption = '&Grid Generation...'
                ImageIndex = 15
              end
              item
                Action = MethodTriangulation
                Caption = '&Triangulation...'
                ImageIndex = 16
              end
              item
                Action = MethodInterpolation
                Caption = '&Interpolation...'
                ImageIndex = 17
              end
              item
                Caption = '-'
              end
              item
                Action = MethodPrediction
                Caption = '&Dressing Prediction...'
              end
              item
                Action = MethodBlockEvaluation
                Caption = '&Block Evaluation...'
              end
              item
                Action = MethodPitOptimization
                Caption = '&Pit Optimization...'
                ImageIndex = 18
              end
              item
                Caption = '-'
              end
              item
                Action = MethodTransformation
                Caption = 'T&ransformation...'
              end
              item
                Action = MethodConversion
                Caption = 'C&onversion...'
              end
              item
                Action = MethodSetOperations
                Caption = '&Set Operations...'
              end>
            Caption = '&Method'
          end
          item
            Items = <
              item
                Action = MapOptions
                Caption = '&Options...'
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Items = <
                      item
                        Action = DrillholesOptions
                        Caption = '&Drillholes...'
                        ImageIndex = 32
                      end
                      item
                        Action = DrillholesSelectDrillhole
                        Caption = '&Select Drillhole'
                        ImageIndex = 33
                      end
                      item
                        Action = DrillholesSelectContact
                        Caption = 'S&elect Contact'
                        ImageIndex = 34
                      end
                      item
                        Action = DrillholesSelectSegment
                        Caption = 'Se&lect Segment'
                        ImageIndex = 35
                      end
                      item
                        Action = DrillholesCreateDrillhole
                        Caption = '&Create Drillhole'
                        ImageIndex = 36
                      end
                      item
                        Action = DrillholesCreateSegment
                        Caption = 'C&reate Segment'
                        ImageIndex = 37
                      end>
                    Caption = '&Drillholes'
                  end
                  item
                    Items = <
                      item
                        Action = Points2DOptions
                        Caption = '&Points 2D...'
                        ImageIndex = 38
                      end
                      item
                        Action = Points2DSelectPoint
                        Caption = '&Select Point'
                        ImageIndex = 39
                      end
                      item
                        Action = Points2DCreatePoint
                        Caption = '&Create Point'
                        ImageIndex = 40
                      end>
                    Caption = '&Points2D'
                  end
                  item
                    Items = <
                      item
                        Action = Points3DOptions
                        Caption = '&Points 3D...'
                        ImageIndex = 41
                      end
                      item
                        Action = Points3DSelectPoint
                        Caption = '&Select Point 3D'
                        ImageIndex = 42
                      end
                      item
                        Action = Points3DCreatePoint
                        Caption = '&Create Point 3D'
                        ImageIndex = 43
                      end>
                    Caption = 'P&oints3D'
                  end
                  item
                    Items = <
                      item
                        Action = PolygonsOptions
                        Caption = '&Polygons...'
                        ImageIndex = 44
                      end
                      item
                        Action = PolygonsSelectPolygon
                        Caption = '&Select Polygon'
                        ImageIndex = 45
                      end
                      item
                        Action = PolygonsSelectVertex
                        Caption = 'S&elect Vertex'
                        ImageIndex = 46
                      end
                      item
                        Action = PolygonsCreatePolygon
                        Caption = '&Create Polygon'
                        ImageIndex = 47
                      end
                      item
                        Action = PolygonsSelectType
                        Caption = 'Se&lect Type'
                      end
                      item
                        Action = PolygonsLinkToSolid
                        Caption = 'L&ink To Solid'
                        ImageIndex = 83
                      end>
                    Caption = 'Po&lygons'
                  end
                  item
                    Items = <
                      item
                        Action = TinOptions
                        Caption = '&Tin...'
                        ImageIndex = 48
                      end
                      item
                        Action = TinVertex
                        Caption = '&Select Vertex'
                        ImageIndex = 49
                      end
                      item
                        Action = TinSelectEdge
                        Caption = 'S&elect Edge'
                        ImageIndex = 50
                      end
                      item
                        Action = TinSelectTriangle
                        Caption = 'Se&lect Triangle'
                        ImageIndex = 51
                      end
                      item
                        Action = TinCreateVertex
                        Caption = '&Create Vertex'
                        ImageIndex = 52
                      end
                      item
                        Action = TinCreateTriangle
                        Caption = 'C&reate Triangle'
                        ImageIndex = 53
                      end
                      item
                        Action = TinSwapDiagonals
                        Caption = 'S&wap Diagonals'
                        ImageIndex = 54
                      end>
                    Caption = '&Tin'
                  end
                  item
                    Items = <
                      item
                        Action = SolidOptions
                        Caption = '&Solids...'
                        ImageIndex = 66
                      end
                      item
                        Action = SolidSelect
                        Caption = 'S&elect Solid'
                        ImageIndex = 67
                      end
                      item
                        Action = SolidCreate
                        Caption = '&Create Solid'
                        ImageIndex = 68
                      end>
                    Caption = '&Solids'
                  end
                  item
                    Items = <
                      item
                        Action = Grid2DOptions
                        Caption = '&Grid 2D...'
                        ImageIndex = 55
                      end
                      item
                        Action = Grid2DSelectCell
                        Caption = '&Select Cell'
                        ImageIndex = 56
                      end
                      item
                        Action = Grid2DSelectRow
                        Caption = 'S&elect Row'
                        ImageIndex = 57
                      end
                      item
                        Action = Grid2DSelectCol
                        Caption = 'Se&lect Column'
                        ImageIndex = 58
                      end
                      item
                        Action = Grid2DCreateCell
                        Caption = '&Create Cell'
                        ImageIndex = 59
                      end>
                    Caption = '&Grid2D'
                  end
                  item
                    Items = <
                      item
                        Action = Grid3DOptions
                        Caption = '&Grid 3D...'
                        ImageIndex = 60
                      end
                      item
                        Action = Grid3DSelectCell
                        Caption = '&Select Cell'
                        ImageIndex = 61
                      end
                      item
                        Action = Grid3DSelectRow
                        Caption = 'S&elect Row'
                        ImageIndex = 62
                      end
                      item
                        Action = Grid3DSelectCol
                        Caption = 'Se&lect Column'
                        ImageIndex = 63
                      end
                      item
                        Action = Grid3DSelectLay
                        Caption = 'Sele&ct Layer'
                        ImageIndex = 64
                      end
                      item
                        Action = Grid3DCreateCell
                        Caption = 'C&reate Cell'
                        ImageIndex = 65
                      end>
                    Caption = 'G&rid3D'
                  end
                  item
                    Items = <
                      item
                        Action = Mesh2DOptions
                        Caption = '&Mesh 2D...'
                        ImageIndex = 69
                      end
                      item
                        Action = Mesh2DSelectNode
                        Caption = '&Select Node'
                        ImageIndex = 70
                      end
                      item
                        Action = Mesh2DSelectCell
                        Caption = 'S&elect Cell'
                        ImageIndex = 71
                      end
                      item
                        Action = Mesh2DCreateCell
                        Caption = '&Create Cell'
                        ImageIndex = 72
                      end>
                    Caption = '&Mesh2D'
                  end
                  item
                    Items = <
                      item
                        Action = Mesh3DOptions
                        Caption = '&Mesh 3D...'
                        ImageIndex = 73
                      end
                      item
                        Action = Mesh3DSelectNode
                        Caption = '&Select Node'
                        ImageIndex = 74
                      end
                      item
                        Action = Mesh3DSelectCell
                        Caption = 'S&elect Cell'
                        ImageIndex = 75
                      end
                      item
                        Action = Mesh3DCreateNode
                        Caption = '&Create Node'
                        ImageIndex = 76
                      end
                      item
                        Action = Mesh3DCreateCell
                        Caption = 'C&reate Cell'
                        ImageIndex = 76
                      end>
                    Caption = 'M&esh3D'
                  end>
                Caption = '&Display'
              end
              item
                Items = <
                  item
                    Action = ShowSection
                    Caption = '&Section...'
                    ImageIndex = 80
                  end
                  item
                    Action = ShowContours
                    Caption = '&Contours...'
                    ImageIndex = 78
                  end
                  item
                    Action = ShowIsosurface
                    Caption = '&Isosurface...'
                    ImageIndex = 79
                  end
                  item
                    Action = ShowVectors
                    Caption = '&Vectors...'
                    ImageIndex = 81
                  end
                  item
                    Action = ShowFilm
                    Caption = '&Film...'
                    ImageIndex = 82
                  end>
                Caption = '&Perform'
              end
              item
                Caption = '-'
              end
              item
                Action = MapLegend
                Caption = '&Legend...'
                ImageIndex = 29
              end
              item
                Action = MapMaterial
                Caption = '&Materials...'
                ImageIndex = 30
              end
              item
                Action = MapLighting
                Caption = 'L&ighting...'
                ImageIndex = 31
              end
              item
                Caption = '-'
              end
              item
                Action = MapBackColor
                Caption = '&Back Color...'
              end>
            Caption = 'M&ap'
          end
          item
            Items = <
              item
                Action = ViewProjectManager
                Caption = '&Project Manager...'
                ImageIndex = 105
                ShortCut = 49274
              end
              item
                Visible = False
                Action = ViewGeoscene
                Caption = '&Geoscene...'
              end
              item
                Caption = '-'
              end
              item
                Action = ViewScale
                Caption = '&Scale...'
                ImageIndex = 107
              end
              item
                Action = ViewSelect
                Caption = 'S&elect'
                ImageIndex = 108
              end
              item
                Action = ViewScroll
                Caption = 'S&croll'
                ImageIndex = 99
              end
              item
                Action = ViewPan
                Caption = 'P&an'
                ImageIndex = 97
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = aZoomInOut
                    Caption = '&Zoom InOut'
                    ImageIndex = 106
                  end
                  item
                    Action = aZoomToProject
                    Caption = 'Zoo&m to Project'
                    ImageIndex = 94
                  end
                  item
                    Action = aZoomToComplete
                    Caption = 'Zoom &to Complete'
                    ImageIndex = 95
                  end
                  item
                    Action = aZoomToModel
                    Caption = 'Zoom to Mo&del'
                    ImageIndex = 96
                  end
                  item
                    Action = aZoomToRectangle
                    Caption = 'Z&oom Rectangle'
                    ImageIndex = 98
                  end>
                Caption = '&Zoom'
              end
              item
                Caption = '-'
              end
              item
                Action = ViewPlaneXY
                Caption = 'P&lane XY'
                ImageIndex = 100
              end
              item
                Action = ViewPlaneXZ
                Caption = 'Pla&ne XZ'
                ImageIndex = 101
              end
              item
                Action = ViewPlaneYZ
                Caption = 'Plane &YZ'
                ImageIndex = 102
              end
              item
                Action = ViewVolumeXYZ
                Caption = '&Volume XYZ'
                ImageIndex = 103
              end
              item
                Action = ViewStatusline
                Caption = 'S&tatusLine'
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Items = <
                  item
                    Action = StyleText
                    Caption = '&Text...'
                    ImageIndex = 118
                  end
                  item
                    Action = StyleSymbol
                    Caption = '&Symbol...'
                    ImageIndex = 119
                  end
                  item
                    Action = StyleLine
                    Caption = '&Line...'
                    ImageIndex = 120
                  end
                  item
                    Action = StyleFill
                    Caption = '&Fill...'
                    ImageIndex = 121
                  end>
                Caption = '&Style'
              end
              item
                Action = DrawText
                Caption = '&Text'
                ImageIndex = 109
              end
              item
                Action = DrawSymbol
                Caption = 'S&ymbol'
                ImageIndex = 110
              end
              item
                Action = DrawPolyline
                Caption = '&Polyline'
                ImageIndex = 112
              end
              item
                Action = DrawPolygon
                Caption = 'P&olygon'
                ImageIndex = 113
              end
              item
                Action = DrawEllipse
                Caption = '&Ellipse'
                ImageIndex = 114
              end
              item
                Action = DrawRectangle
                Caption = '&Rectangle'
                ImageIndex = 115
              end
              item
                Action = DrawAddNode
                Caption = '&Add Node'
                ImageIndex = 117
              end
              item
                Action = DrawReshape
                Caption = 'Res&hape'
                ImageIndex = 116
              end
              item
                Action = DrawSolid
                Caption = 'So&lid'
                ImageIndex = 123
              end
              item
                Caption = '-'
              end
              item
                Action = DrawDepth
                Caption = '&Drawing Depth...'
              end>
            Caption = '&Draw'
          end
          item
            Items = <
              item
                Action = acAnalyseProblemBook
                Caption = '&Problem Book...'
                ImageIndex = 105
              end
              item
                Visible = False
                Action = acAnalyseVolumeCalculation
                Caption = '&Volume Calculation...'
                ImageIndex = 151
              end
              item
                Action = acAnalyseReserveCalculation
                Caption = '&Reserve Calculation...'
                ImageIndex = 129
              end
              item
                Caption = '-'
              end
              item
                Action = acAnalyseBaseStatistics
                Caption = '&Basic Statistics...'
              end
              item
                Visible = False
                Action = acAnalyseFactorAnalysis
              end
              item
                Action = acAnalyseVariograms
                Caption = '&Variograms...'
              end>
            Caption = 'A&nalyse'
          end
          item
            Items = <
              item
                Action = ToolsConfiguration
                Caption = '&Configuration...'
                ImageIndex = 130
              end
              item
                Action = ToolsCustomize
                Caption = 'Cu&stomize...'
              end
              item
                Action = ToolsStandardStyle
                Caption = 'S&tandard Style'
              end
              item
                Action = ToolsXPStyle
                Caption = '&XP Style'
              end
              item
                Action = ToolsUnitsConverter
                Caption = '&Units Converter...'
              end
              item
                Caption = '-'
              end
              item
                Items = <
                  item
                    Action = CalculatorGeology
                    Caption = '&Geology...'
                  end
                  item
                    Action = CalculatorGeometry
                    Caption = 'G&eometry...'
                  end
                  item
                    Action = CalculatorMining
                    Caption = '&Mining...'
                  end
                  item
                    Action = CalculatorSurvey
                    Caption = '&Survey...'
                  end>
                Caption = 'C&alculator'
              end>
            Caption = '&Tools'
          end
          item
            Items = <
              item
                Action = WindowClose
                Caption = '&Close'
              end
              item
                Caption = '-'
              end
              item
                Action = WindowCascade
                Caption = 'C&ascade'
                ImageIndex = 132
              end
              item
                Action = WindowTileHorizontal
                Caption = '&Tile Horizontally'
                ImageIndex = 133
              end
              item
                Action = WindowTileVertical
                Caption = 'T&ile Vertically'
                ImageIndex = 134
              end
              item
                Action = WindowMinimizeAll
                Caption = '&Minimize All'
              end
              item
                Action = WindowArrange
                Caption = 'A&rrange'
              end
              item
                Action = WindowRedraw
                Caption = 'R&edraw'
              end>
            Caption = '&Window'
          end
          item
            Items = <
              item
                Action = HelpContents
                Caption = '&Contents'
                ImageIndex = 135
                ShortCut = 112
              end
              item
                Action = HelpGlossary
                Caption = '&Glossary...'
                ImageIndex = 136
              end
              item
                Action = HelpAbout
                Caption = '&About...'
              end>
            Caption = '&Help'
          end>
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = FileOpenProject
            Caption = 'O&pen Project...'
            ImageIndex = 1
            ShortCut = 16506
          end
          item
            Action = FileDataBase
            Caption = '&Data Base...'
            ImageIndex = 3
          end
          item
            Action = FileOpenModel
            Caption = 'Open &Model...'
            ImageIndex = 4
            ShortCut = 114
          end>
        ActionBar = ActionToolBarStandard
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = EditCut
            Caption = '&Cut'
            ImageIndex = 11
            ShortCut = 16472
          end
          item
            Action = EditCopy
            Caption = 'C&opy'
            ImageIndex = 12
            ShortCut = 16451
          end
          item
            Action = EditPaste
            Caption = '&Paste'
            ImageIndex = 13
            ShortCut = 16470
          end>
        ActionBar = ActionToolBarEdit
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = MapLegend
            Caption = '&Legend...'
            ImageIndex = 29
          end
          item
            Action = MapMaterial
            Caption = '&Materials...'
            ImageIndex = 30
          end
          item
            Action = MapLighting
            Caption = 'L&ighting...'
            ImageIndex = 31
          end>
        ActionBar = ActionToolBarMap
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Caption = '-'
          end>
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = ViewProjectManager
            Caption = '&Project Manager...'
            ImageIndex = 105
            ShortCut = 49274
          end
          item
            Action = ViewPlaneXY
            Caption = 'Pla&ne XY'
            ImageIndex = 100
          end
          item
            Action = ViewPlaneXZ
            Caption = 'Plan&e XZ'
            ImageIndex = 101
          end
          item
            Caption = '-'
          end
          item
            Action = aZoomToComplete
            Caption = 'Z&oom to Complete'
            ImageIndex = 95
          end
          item
            Action = aZoomToModel
            Caption = 'Zoo&m to Model'
            ImageIndex = 96
          end
          item
            Caption = '-'
          end
          item
            Action = ViewSelect
            Caption = 'Se&lect'
            ImageIndex = 108
          end
          item
            Action = ViewScroll
            Caption = 'Sc&roll'
            ImageIndex = 99
          end
          item
            Action = ViewPan
            Caption = 'P&an'
            ImageIndex = 97
          end
          item
            Action = aZoomInOut
            Caption = 'Zoom &InOut'
            ImageIndex = 106
          end>
        ActionBar = ActionToolBarView
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = acAnalyseProblemBook
            Caption = '&Problem Book...'
            ImageIndex = 128
          end
          item
            Action = acAnalyseReserveCalculation
            Caption = '&Reserve Calculation...'
            ImageIndex = 129
          end>
        ActionBar = ActionToolBarAnalyse
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = ObserveTop
            Caption = '&Top'
            ImageIndex = 86
          end
          item
            Action = ObserveBottom
            Caption = '&Bottom'
            ImageIndex = 87
          end
          item
            Action = ObserveRight
            Caption = '&Right'
            ImageIndex = 90
          end
          item
            Action = ObserveLeft
            Caption = '&Left'
            ImageIndex = 91
          end
          item
            Action = ObserveBack
            Caption = 'B&ack'
            ImageIndex = 89
          end
          item
            Action = ObserveFront
            Caption = '&Front'
            ImageIndex = 88
          end
          item
            Action = Observe3D
            Caption = '&3D'
            ImageIndex = 92
          end
          item
            Action = ObserveRotate
            Caption = 'R&otate Options...'
            ImageIndex = 93
          end>
        ActionBar = ActionToolBarObserve
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Points2DOptions
            Caption = '&Points 2D...'
            ImageIndex = 38
          end
          item
            Caption = '-'
          end
          item
            Action = Points2DSelectPoint
            Caption = '&Select Point'
            ImageIndex = 39
          end
          item
            Action = Points2DCreatePoint
            Caption = '&Create Point'
            ImageIndex = 40
          end>
        ActionBar = ActionToolBarPoints2D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = DrillholesOptions
            Caption = '&Drillholes...'
            ImageIndex = 32
          end
          item
            Caption = '-'
          end
          item
            Action = DrillholesSelectDrillhole
            Caption = '&Select Drillhole'
            ImageIndex = 33
          end
          item
            Action = DrillholesSelectContact
            Caption = 'S&elect Contact'
            ImageIndex = 34
          end
          item
            Action = DrillholesSelectSegment
            Caption = 'Se&lect Segment'
            ImageIndex = 35
          end
          item
            Action = DrillholesCreateDrillhole
            Caption = '&Create Drillhole'
            ImageIndex = 36
          end
          item
            Action = DrillholesCreateSegment
            Caption = 'C&reate Segment'
            ImageIndex = 37
          end>
        ActionBar = ActionToolBarDrillholes
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Points3DOptions
            Caption = '&Points 3D...'
            ImageIndex = 41
          end
          item
            Caption = '-'
          end
          item
            Action = Points3DSelectPoint
            Caption = '&Select Point 3D'
            ImageIndex = 42
          end
          item
            Action = Points3DCreatePoint
            Caption = '&Create Point 3D'
            ImageIndex = 43
          end>
        ActionBar = ActionToolBarPoints3D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = PolygonsOptions
            Caption = '&Polygons...'
            ImageIndex = 44
          end
          item
            Caption = '-'
          end
          item
            Action = PolygonsSelectPolygon
            Caption = '&Select Polygon'
            ImageIndex = 45
          end
          item
            Action = PolygonsSelectVertex
            Caption = 'S&elect Vertex'
            ImageIndex = 46
          end
          item
            Action = PolygonsCreatePolygon
            Caption = '&Create Polygon'
            ImageIndex = 47
          end
          item
            Caption = '-'
          end
          item
            Action = PolygonsLinkToSolid
            Caption = '&Link To Solid'
            ImageIndex = 83
          end>
        ActionBar = ActionToolBarPolygons
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Grid2DOptions
            Caption = '&Grid 2D...'
            ImageIndex = 55
          end
          item
            Caption = '-'
          end
          item
            Action = Grid2DSelectCell
            Caption = '&Select Cell'
            ImageIndex = 56
          end
          item
            Action = Grid2DSelectCol
            Caption = 'S&elect Column'
            ImageIndex = 58
          end>
        ActionBar = ActionToolBarGrid2D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Grid3DOptions
            Caption = '&Grid 3D...'
            ImageIndex = 60
          end
          item
            Caption = '-'
          end
          item
            Action = Grid3DSelectCell
            Caption = '&Select Cell'
            ImageIndex = 61
          end
          item
            Action = Grid3DSelectRow
            Caption = 'S&elect Row'
            ImageIndex = 62
          end
          item
            Action = Grid3DSelectCol
            Caption = 'Se&lect Column'
            ImageIndex = 63
          end
          item
            Action = Grid3DSelectLay
            Caption = 'Sele&ct Layer'
            ImageIndex = 64
          end>
        ActionBar = ActionToolBarGrid3D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = TinOptions
            Caption = '&Tin...'
            ImageIndex = 48
          end
          item
            Caption = '-'
          end
          item
            Action = TinVertex
            Caption = '&Select Vertex'
            ImageIndex = 49
          end
          item
            Action = TinSelectEdge
            Caption = 'S&elect Edge'
            ImageIndex = 50
          end
          item
            Action = TinSelectTriangle
            Caption = 'Se&lect Triangle'
            ImageIndex = 51
          end
          item
            Action = TinCreateVertex
            Caption = '&Create Vertex'
            ImageIndex = 52
          end
          item
            Action = TinCreateTriangle
            Caption = 'C&reate Triangle'
            ImageIndex = 53
          end
          item
            Action = TinSwapDiagonals
            Caption = 'S&wap Diagonals'
            ImageIndex = 54
          end
          item
            Caption = '-'
          end>
        ActionBar = ActionToolBarTin
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Caption = '-'
          end
          item
            Action = SolidSelect
            Caption = 'S&elect Solid'
            ImageIndex = 67
          end
          item
            Action = SolidCreate
            Caption = '&Create Solid'
            ImageIndex = 68
          end>
        ActionBar = ActionToolBarSolids
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Mesh2DOptions
            Caption = '&Mesh 2D...'
            ImageIndex = 69
          end
          item
            Caption = '-'
          end
          item
            Action = Mesh2DSelectNode
            Caption = '&Select Node'
            ImageIndex = 70
          end
          item
            Action = Mesh2DSelectCell
            Caption = 'S&elect Cell'
            ImageIndex = 71
          end
          item
            Action = Mesh2DCreateCell
            Caption = '&Create Cell'
            ImageIndex = 72
          end
          item
            Caption = '-'
          end>
        ActionBar = ActionToolBarMesh2D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = Mesh3DOptions
            Caption = '&Mesh 3D...'
            ImageIndex = 73
          end
          item
            Caption = '-'
          end
          item
            Action = Mesh3DSelectNode
            Caption = '&Select Node'
            ImageIndex = 74
          end
          item
            Action = Mesh3DSelectCell
            Caption = 'S&elect Cell'
            ImageIndex = 75
          end
          item
            Action = Mesh3DCreateNode
            Caption = '&Create Node'
            ImageIndex = 76
          end>
        ActionBar = ActionToolBarMesh3D
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Caption = '-'
          end
          item
            Action = DrawText
            Caption = '&Text'
            ImageIndex = 109
          end
          item
            Action = DrawSymbol
            Caption = '&Symbol'
            ImageIndex = 110
          end
          item
            Action = DrawPolyline
            Caption = '&Polyline'
            ImageIndex = 112
          end
          item
            Action = DrawPolygon
            Caption = 'P&olygon'
            ImageIndex = 113
          end
          item
            Action = DrawEllipse
            Caption = '&Ellipse'
            ImageIndex = 114
          end
          item
            Action = DrawRectangle
            Caption = '&Rectangle'
            ImageIndex = 115
          end
          item
            Caption = '-'
          end
          item
            Action = DrawSolid
            Caption = 'So&lid'
            ImageIndex = 123
          end
          item
            Action = DrawReshape
            Caption = 'Res&hape'
            ImageIndex = 116
          end
          item
            Action = DrawAddNode
            Caption = '&Add Node'
            ImageIndex = 117
          end
          item
            Caption = '-'
          end
          item
            Action = StyleText
            Caption = 'Te&xt...'
            ImageIndex = 118
          end
          item
            Action = StyleSymbol
            Caption = 'S&ymbol...'
            ImageIndex = 119
          end
          item
            Action = StyleLine
            Caption = 'L&ine...'
            ImageIndex = 120
          end
          item
            Action = StyleFill
            Caption = '&Fill...'
            ImageIndex = 121
          end>
        ActionBar = ActionToolBarDrawing
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = MethodGridGeneration
            Caption = '&Grid Generation...'
            ImageIndex = 15
          end
          item
            Action = MethodInterpolation
            Caption = '&Interpolation...'
            ImageIndex = 17
          end
          item
            Action = MethodTriangulation
            Caption = '&Triangulation...'
            ImageIndex = 16
          end>
        ActionBar = ActionToolBarMethod
      end
      item
        Items = <
          item
            ChangesAllowed = []
            Items = <
              item
                Caption = '-'
              end
              item
                Action = FileOpenProject
                Caption = '&Open Project...'
                ImageIndex = 1
                ShortCut = 16506
              end
              item
                Action = FileDataBase
                Caption = '&Data Base...'
              end
              item
                Action = FileOpenModel
                Caption = 'Op&en Model...'
                ImageIndex = 4
                ShortCut = 114
              end
              item
                Action = FileOpenReport
                Caption = 'Open &Report...'
                ImageIndex = 3
              end
              item
                Action = FileOpenImage
                Caption = 'Open &Image...'
              end
              item
                Action = FileOpenText
                Caption = 'Open &Text...'
              end
              item
                Caption = '-'
              end
              item
                Action = FileSave
                Caption = '&Save'
                ImageIndex = 6
                ShortCut = 113
              end
              item
                Action = FileSaveAs
                Caption = 'S&ave As...'
                ImageIndex = 161
                ShortCut = 16467
              end
              item
                Action = FileSaveProjectAs
                Caption = 'Sa&ve Project As...'
              end
              item
                Action = FileClose
                Caption = '&Close'
              end
              item
                Action = FileCloseAll
                Caption = 'C&lose All'
              end
              item
                Caption = '-'
              end
              item
                Action = FileImport
                Caption = 'I&mport...'
              end
              item
                Action = FileExport
                Caption = 'E&xport...'
              end
              item
                Caption = '-'
              end
              item
                Action = FilePrint
                Caption = '&Print...'
                ImageIndex = 7
                ShortCut = 16464
              end
              item
                Action = FilePrintPreview
                Caption = 'Print Previe&w...'
              end
              item
                Caption = '-'
              end
              item
                Action = FileExit
                ImageIndex = 162
                ShortCut = 32856
              end>
            Caption = '&File'
            ShowShortCut = False
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Action = EditUndo
                Caption = '&Undo'
                ImageIndex = 155
                ShortCut = 16474
              end
              item
                Caption = '-'
              end
              item
                Action = EditCut
                Caption = '&Cut'
                ImageIndex = 11
                ShortCut = 16472
              end
              item
                Action = EditCopy
                Caption = 'C&opy'
                ImageIndex = 12
                ShortCut = 16451
              end
              item
                Action = EditPaste
                Caption = '&Paste'
                ImageIndex = 13
                ShortCut = 16470
              end
              item
                Action = EditSelectAll
                Caption = '&Select All'
                ShortCut = 16449
              end
              item
                Action = EditDelete
                Caption = '&Delete'
                ImageIndex = 14
                ShortCut = 46
              end
              item
                Caption = '-'
              end
              item
                Action = EditFind
                Caption = '&Find...'
              end>
            Caption = '&Edit'
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Items = <
                  item
                    Action = CompositingSampleContacts
                    Caption = '&Sample Contacts...'
                    ImageIndex = 21
                  end
                  item
                    Action = CompositingCenters
                    Caption = '&Coordinates of Centers...'
                    ImageIndex = 22
                  end
                  item
                    Visible = False
                    Action = CompositingInsideHorizons
                    Caption = '&Inside Horizons...'
                    ImageIndex = 26
                  end
                  item
                    Action = CompositingOreIntervals
                    Caption = '&Ore Intervals...'
                    ImageIndex = 23
                  end
                  item
                    Action = CompositingOreSorting
                    Caption = 'O&re Sorting...'
                    ImageIndex = 25
                  end
                  item
                    Action = CompositingLinearReserves
                    Caption = '&Linear Reserves...'
                    ImageIndex = 24
                  end>
                Caption = 'C&ompositing'
                UsageCount = 1
              end
              item
                Action = MethodTriangulation
                Caption = '&Triangulation...'
                ImageIndex = 16
              end
              item
                Action = MethodGridGeneration
                Caption = '&Grid Generation...'
                ImageIndex = 15
              end
              item
                Action = MethodInterpolation
                Caption = '&Interpolation...'
                ImageIndex = 17
              end
              item
                Action = MethodSetOperations
                Caption = '&Set Operations...'
              end
              item
                Caption = '-'
              end
              item
                Action = MethodVarModeling
                Caption = '&Variogram Modelling...'
              end
              item
                Action = MethodPrediction
                Caption = '&Dressing Prediction...'
              end
              item
                Action = MethodBlockEvaluation
                Caption = '&Block Evaluation...'
                ImageIndex = 150
              end
              item
                Action = MethodPitOptimization
                Caption = '&Pit Optimization...'
                ImageIndex = 18
              end
              item
                Caption = '-'
              end
              item
                Action = MethodSimulation
                Caption = 'Si&mulation...'
                ImageIndex = 38
              end
              item
                Action = OctreeConstruction
                Caption = 'Octr&ee Construction...'
              end
              item
                Action = MethodTransformation
                Caption = 'T&ransformation...'
              end
              item
                Action = MethodConversion
                Caption = '&Conversion...'
              end>
            Caption = '&Method'
          end
          item
            Items = <
              item
                Action = MapScenery
                Caption = '&Scenery...'
                ImageIndex = 27
              end
              item
                Visible = False
                Action = ViewMapWindow
              end
              item
                Action = MapOptions
                Caption = '&Options...'
              end
              item
                Action = MapLegend
                Caption = '&Legend...'
                ImageIndex = 29
              end
              item
                Action = MapMaterial
                Caption = '&Materials...'
                ImageIndex = 30
              end
              item
                Action = MapLighting
                Caption = 'L&ighting...'
                ImageIndex = 31
              end
              item
                Action = MapBackColor
                Caption = '&Back Color...'
              end>
            Caption = 'M&ap'
          end
          item
            Items = <
              item
                Action = ViewProjectManager
                Caption = '&Project Manager...'
                ImageIndex = 105
                ShortCut = 49274
              end
              item
                Visible = False
                Action = ViewGeoscene
                Caption = '&Geoscene...'
              end
              item
                Caption = '-'
              end
              item
                Action = ViewScale
                Caption = 'S&cale...'
                ImageIndex = 107
              end
              item
                Action = ViewSelect
                Caption = 'S&elect'
                ImageIndex = 108
              end
              item
                Action = ViewScroll
                Caption = 'Sc&roll'
                ImageIndex = 99
              end
              item
                Action = ViewPan
                Caption = 'P&an'
                ImageIndex = 97
              end
              item
                Action = ViewPlaneXY
                Caption = 'P&lane XY'
                ImageIndex = 100
              end
              item
                Action = ViewPlaneXZ
                Caption = 'Pla&ne XZ'
                ImageIndex = 101
              end
              item
                Action = ViewPlaneYZ
                Caption = 'Plane &YZ'
                ImageIndex = 102
              end
              item
                Action = ViewVolumeXYZ
                Caption = '&Volume XYZ'
                ImageIndex = 103
              end
              item
                Action = ViewStatusline
                Caption = 'S&tatusLine'
              end>
            Caption = 'V&iew'
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Items = <
                  item
                    Action = StyleText
                    Caption = '&Text...'
                    ImageIndex = 118
                  end
                  item
                    Action = StyleSymbol
                    Caption = '&Symbol...'
                    ImageIndex = 119
                  end
                  item
                    Action = StyleLine
                    Caption = '&Line...'
                    ImageIndex = 120
                  end
                  item
                    Action = StyleFill
                    Caption = '&Fill...'
                    ImageIndex = 121
                  end>
                Caption = 'St&yle'
                UsageCount = 1
              end
              item
                Action = DrawText
                Caption = '&Text'
                ImageIndex = 109
              end
              item
                Action = DrawSymbol
                Caption = '&Symbol'
                ImageIndex = 110
              end
              item
                Action = DrawPolyline
                Caption = '&Polyline'
                ImageIndex = 112
              end
              item
                Action = DrawPolygon
                Caption = 'Po&lygon'
                ImageIndex = 113
              end
              item
                Action = DrawEllipse
                Caption = '&Ellipse'
                ImageIndex = 114
              end
              item
                Action = DrawRectangle
                Caption = '&Rectangle'
                ImageIndex = 115
              end
              item
                Action = DrawSolid
                Caption = 'Sol&id'
                ImageIndex = 123
              end
              item
                Action = DrawReshape
                Caption = 'Res&hape'
                ImageIndex = 116
              end
              item
                Action = DrawAddNode
                Caption = '&Add Node'
                ImageIndex = 117
              end
              item
                Caption = '-'
              end
              item
                Action = DrawDepth
                Caption = '&Drawing Depth...'
              end>
            Caption = '&Draw'
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Action = acAnalyseProblemBook
                Caption = '&Problem Book...'
                ImageIndex = 128
              end
              item
                Visible = False
                Action = acAnalyseVolumeCalculation
                Caption = '&Volume Calculation...'
                ImageIndex = 151
              end
              item
                Action = acAnalyseReserveCalculation
                Caption = '&Reserve Calculation...'
                ImageIndex = 129
              end
              item
                Caption = '-'
              end
              item
                Action = acAnalyseBaseStatistics
                Caption = '&Basic Statistics...'
              end
              item
                Action = acAnalyseFactorAnalysis
                Caption = '&Factor Analysis...'
              end
              item
                Action = acAnalyseVariograms
                Caption = '&Variograms...'
              end>
            Caption = 'A&nalyse'
          end
          item
            Items = <
              item
                Action = ToolsConfiguration
                Caption = '&Configuration...'
                ImageIndex = 130
              end
              item
                Action = ToolsCustomize
                Caption = 'C&ustomize...'
              end
              item
                Action = ToolsStandardStyle
                Caption = '&Standard Style'
              end
              item
                Action = ToolsXPStyle
                Caption = '&XP Style'
              end
              item
                Action = ToolsUnitsConverter
                Caption = 'U&nits Converter...'
              end
              item
                Items = <
                  item
                    Action = CalculatorGeology
                    Caption = '&Geology...'
                  end
                  item
                    Action = CalculatorGeometry
                    Caption = 'G&eometry...'
                  end
                  item
                    Action = CalculatorMining
                    Caption = '&Mining...'
                  end
                  item
                    Action = CalculatorSurvey
                    Caption = '&Survey...'
                  end>
                Caption = 'C&alculator'
                UsageCount = 1
              end>
            Caption = '&Tools'
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Action = WindowClose
                Caption = '&Close'
              end
              item
                Caption = '-'
              end
              item
                Action = WindowCascade
                Caption = 'C&ascade'
                ImageIndex = 132
              end
              item
                Action = WindowTileHorizontal
                Caption = '&Tile Horizontally'
                ImageIndex = 133
              end
              item
                Action = WindowTileVertical
                Caption = 'T&ile Vertically'
                ImageIndex = 134
              end
              item
                Action = WindowMinimizeAll
                Caption = '&Minimize All'
              end
              item
                Action = WindowArrange
                Caption = 'A&rrange'
              end
              item
                Action = WindowRedraw
                Caption = 'R&edraw'
              end>
            Caption = '&Window'
          end
          item
            ChangesAllowed = []
            Items = <
              item
                Action = HelpContents
                Caption = '&Contents'
                ImageIndex = 135
                ShortCut = 112
              end
              item
                Action = HelpGlossary
                Caption = '&Glossary...'
                ImageIndex = 136
              end
              item
                Caption = '-'
              end
              item
                Action = HelpAbout
                Caption = '&About...'
              end>
            Caption = '&Help'
          end>
        ActionBar = ActionMainMenuBar
      end
      item
      end
      item
      end>
    Images = fmInitialForm.ImageListInterface
    Left = 328
    Top = 192
    StyleName = 'Standard'
    object MethodGridGeneration: TAction
      Category = 'Method'
      Caption = 'Grid Generation...'
      HelpContext = 311
      Hint = 'Generate a grid'
      ImageIndex = 15
      OnExecute = MethodGridGenerationExecute
    end
    object MethodTriangulation: TAction
      Category = 'Method'
      Caption = 'Triangulation...'
      HelpContext = 312
      Hint = 'Triangulate data'
      ImageIndex = 16
      OnExecute = MethodTriangulationExecute
    end
    object MethodInterpolation: TAction
      Category = 'Method'
      Caption = 'Interpolation...'
      HelpContext = 314
      Hint = 'Interpolate points to a grid or mesh'
      ImageIndex = 17
      OnExecute = MethodInterpolationExecute
    end
    object MethodVarModeling: TAction
      Category = 'Method'
      Caption = 'Variogram Modelling...'
      HelpContext = 320
      OnExecute = MethodVarModelingExecute
    end
    object MethodPrediction: TAction
      Category = 'Method'
      Caption = 'Dressing Prediction...'
      HelpContext = 315
      Hint = 'Prediction|Predict ore dressing parameters'
      OnExecute = MethodPredictionExecute
    end
    object MethodBlockEvaluation: TAction
      Category = 'Method'
      Caption = 'Block Evaluation...'
      HelpContext = 316
      Hint = 'Evaluation|Evaluate block values'
      ImageIndex = 150
      OnExecute = MethodBlockEvaluationExecute
    end
    object MethodPitOptimization: TAction
      Category = 'Method'
      Caption = 'Pit Optimization...'
      HelpContext = 317
      Hint = 'Optimization|Optimize open pit limits'
      ImageIndex = 18
      OnExecute = MethodPitOptimizationExecute
    end
    object MethodTransformation: TAction
      Category = 'Method'
      Caption = 'Transformation...'
      HelpContext = 234
      Hint = 'Transformation|Transform a model in space, rotate and scale'
      OnExecute = MethodTransformationExecute
    end
    object MethodConversion: TAction
      Category = 'Method'
      Caption = 'Conversion...'
      HelpContext = 132
      Hint = 'Conversion|Convert datasets from one type into other'
      OnExecute = MethodConversionExecute
    end
    object MethodSetOperations: TAction
      Category = 'Method'
      Caption = 'Set Operations...'
      HelpContext = 318
      Hint = 'Set operations between two selected objects'
      OnExecute = MethodSetOperationsExecute
    end
    object ShowSection: TAction
      Category = 'Perform'
      Caption = 'Section...'
      Enabled = False
      HelpContext = 497
      Hint = 'Features of cuttings'
      ImageIndex = 80
      OnExecute = ShowSectionExecute
    end
    object ShowContours: TAction
      Category = 'Perform'
      AutoCheck = True
      Caption = 'Contours...'
      Enabled = False
      HelpContext = 495
      Hint = 'Features of contours '
      ImageIndex = 78
      OnExecute = ShowContoursExecute
    end
    object ShowIsosurface: TAction
      Category = 'Perform'
      Caption = 'Isosurface...'
      Enabled = False
      HelpContext = 496
      Hint = 'Features of isosurfaces'
      ImageIndex = 79
      OnExecute = ShowIsosurfaceExecute
    end
    object ShowVectors: TAction
      Category = 'Perform'
      AutoCheck = True
      Caption = 'Vectors...'
      Enabled = False
      HelpContext = 498
      Hint = 'Features of vectors'
      ImageIndex = 81
      OnExecute = ShowVectorsExecute
    end
    object ShowFilm: TAction
      Category = 'Perform'
      Caption = 'Film...'
      Enabled = False
      HelpContext = 499
      Hint = 'Features of films'
      ImageIndex = 82
      OnExecute = ShowFilmExecute
    end
    object DrillholesOptions: TAction
      Category = 'Dholes'
      Caption = 'Drillholes...'
      Enabled = False
      HelpContext = 410
      Hint = 'Drillholes options|Set options of drillholes'
      ImageIndex = 32
      OnExecute = MapOptionsExecute
    end
    object DrillholesSelectDrillhole: TAction
      Category = 'Dholes'
      AutoCheck = True
      Caption = 'Select Drillhole'
      Enabled = False
      GroupIndex = 100
      HelpContext = 400
      Hint = 'Select a drillhole'
      ImageIndex = 33
      OnExecute = DrillholesSelectDrillholeExecute
    end
    object DrillholesSelectContact: TAction
      Category = 'Dholes'
      AutoCheck = True
      Caption = 'Select Contact'
      Enabled = False
      GroupIndex = 100
      HelpContext = 400
      Hint = 'Select a contact of samples'
      ImageIndex = 34
      OnExecute = DrillholesSelectContactExecute
    end
    object DrillholesSelectSegment: TAction
      Category = 'Dholes'
      AutoCheck = True
      Caption = 'Select Segment'
      Enabled = False
      GroupIndex = 100
      HelpContext = 400
      Hint = 'Select a sample segment'
      ImageIndex = 35
      OnExecute = DrillholesSelectSegmentExecute
    end
    object DrillholesCreateDrillhole: TAction
      Category = 'Dholes'
      AutoCheck = True
      Caption = 'Create Drillhole'
      Enabled = False
      GroupIndex = 100
      HelpContext = 400
      Hint = 'Create a drillhole'
      ImageIndex = 36
      OnExecute = DrillholesCreateDrillholeExecute
    end
    object DrillholesCreateSegment: TAction
      Category = 'Dholes'
      AutoCheck = True
      Caption = 'Create Segment'
      Enabled = False
      GroupIndex = 100
      HelpContext = 400
      Hint = 'Create a sample segment'
      ImageIndex = 37
      OnExecute = DrillholesCreateSegmentExecute
    end
    object Points2DOptions: TAction
      Category = 'Points2D'
      Caption = 'Points 2D...'
      Enabled = False
      HelpContext = 420
      Hint = 'Points 2D options|Set options of points 2D'
      ImageIndex = 38
      OnExecute = MapOptionsExecute
    end
    object Points2DSelectPoint: TAction
      Category = 'Points2D'
      Caption = 'Select Point'
      Enabled = False
      GroupIndex = 200
      HelpContext = 400
      Hint = 'Select a point'
      ImageIndex = 39
      OnExecute = Points2DSelectPointExecute
    end
    object Points2DCreatePoint: TAction
      Category = 'Points2D'
      Caption = 'Create Point'
      Enabled = False
      GroupIndex = 200
      HelpContext = 400
      Hint = 'Create a point on surface'
      ImageIndex = 40
      OnExecute = Points2DCreatePointExecute
    end
    object Points3DOptions: TAction
      Category = 'Points3D'
      Caption = 'Points 3D...'
      Enabled = False
      HelpContext = 430
      Hint = 'Points 3D options|Set options of points 3D'
      ImageIndex = 41
      OnExecute = MapOptionsExecute
    end
    object Points3DSelectPoint: TAction
      Category = 'Points3D'
      Caption = 'Select Point 3D'
      Enabled = False
      GroupIndex = 300
      HelpContext = 400
      Hint = 'Select a point'
      ImageIndex = 42
      OnExecute = Points3DSelectPointExecute
    end
    object Points3DCreatePoint: TAction
      Category = 'Points3D'
      Caption = 'Create Point 3D'
      Enabled = False
      GroupIndex = 300
      HelpContext = 400
      Hint = 'Create a point'
      ImageIndex = 43
      OnExecute = Points3DSelectPointExecute
    end
    object PolygonsOptions: TAction
      Category = 'Polygons'
      Caption = 'Polygons...'
      Enabled = False
      HelpContext = 440
      Hint = 'Polygon options|Set options of polygons'
      ImageIndex = 44
      OnExecute = MapOptionsExecute
    end
    object PolygonsSelectPolygon: TAction
      Category = 'Polygons'
      Caption = 'Select Polygon'
      Enabled = False
      GroupIndex = 400
      HelpContext = 440
      Hint = 'Select a polygon'
      ImageIndex = 45
    end
    object PolygonsSelectVertex: TAction
      Category = 'Polygons'
      Caption = 'Select Vertex'
      Enabled = False
      GroupIndex = 400
      HelpContext = 400
      Hint = 'Select a vertex'
      ImageIndex = 46
    end
    object PolygonsCreatePolygon: TAction
      Category = 'Polygons'
      Caption = 'Create Polygon'
      Enabled = False
      GroupIndex = 400
      HelpContext = 400
      Hint = 'Create a polygon'
      ImageIndex = 47
    end
    object PolygonsSelectType: TAction
      Category = 'Polygons'
      Caption = 'Select Type'
      GroupIndex = 400
      Hint = 'Select polygon type'
      OnExecute = PolygonsSelectTypeExecute
    end
    object PolygonsLinkToSolid: TAction
      Category = 'Polygons'
      Caption = 'Link To Solid'
      Enabled = False
      GroupIndex = 400
      Hint = 'Link polygons to a solid'
      ImageIndex = 83
      OnExecute = PolygonsLinkToSolidExecute
    end
    object TinOptions: TAction
      Category = 'Tin'
      Caption = 'Tin...'
      Enabled = False
      HelpContext = 450
      Hint = 'Tins options|Set options of tins'
      ImageIndex = 48
      OnExecute = MapOptionsExecute
    end
    object TinVertex: TAction
      Category = 'Tin'
      Caption = 'Select Vertex'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Select a vertex'
      ImageIndex = 49
    end
    object TinSelectEdge: TAction
      Category = 'Tin'
      Caption = 'Select Edge'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Select an edge'
      ImageIndex = 50
    end
    object TinSelectTriangle: TAction
      Category = 'Tin'
      Caption = 'Select Triangle'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Select a triangle'
      ImageIndex = 51
    end
    object TinCreateVertex: TAction
      Category = 'Tin'
      Caption = 'Create Vertex'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Create a vertex'
      ImageIndex = 52
    end
    object TinCreateTriangle: TAction
      Category = 'Tin'
      Caption = 'Create Triangle'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Create a triangle'
      ImageIndex = 53
    end
    object TinSwapDiagonals: TAction
      Category = 'Tin'
      Caption = 'Swap Diagonals'
      Enabled = False
      GroupIndex = 500
      HelpContext = 400
      Hint = 'Swap diagonals of the quadrilateral'
      ImageIndex = 54
    end
    object SolidOptions: TAction
      Category = 'Solids'
      Caption = 'Solids...'
      Enabled = False
      HelpContext = 460
      Hint = 'Set options of solids'
      ImageIndex = 66
    end
    object SolidSelect: TAction
      Category = 'Solids'
      Caption = 'Select Solid'
      Enabled = False
      GroupIndex = 600
      HelpContext = 460
      Hint = 'Select a solid'
      ImageIndex = 67
    end
    object SolidCreate: TAction
      Category = 'Solids'
      Caption = 'Create Solid'
      Enabled = False
      GroupIndex = 600
      HelpContext = 460
      Hint = 'Create a solid'
      ImageIndex = 68
    end
    object Grid2DOptions: TAction
      Category = 'Grid2D'
      Caption = 'Grid 2D...'
      Enabled = False
      HelpContext = 470
      Hint = 'Grids 2D options|Set options of grids 2D'
      ImageIndex = 55
      OnExecute = MapOptionsExecute
    end
    object Grid2DSelectCell: TAction
      Category = 'Grid2D'
      Caption = 'Select Cell'
      Enabled = False
      GroupIndex = 700
      HelpContext = 400
      Hint = 'Select a cell'
      ImageIndex = 56
      OnExecute = Grid2DSelectCellExecute
    end
    object Grid2DSelectRow: TAction
      Category = 'Grid2D'
      Caption = 'Select Row'
      Enabled = False
      GroupIndex = 700
      HelpContext = 400
      Hint = 'Select a I row'
      ImageIndex = 57
      OnExecute = Grid2DSelectRowExecute
    end
    object Grid2DSelectCol: TAction
      Category = 'Grid2D'
      Caption = 'Select Column'
      Enabled = False
      GroupIndex = 700
      HelpContext = 400
      Hint = 'Select a J column'
      ImageIndex = 58
      OnExecute = Grid2DSelectColExecute
    end
    object Grid2DCreateCell: TAction
      Category = 'Grid2D'
      Caption = 'Create Cell'
      Enabled = False
      GroupIndex = 700
      HelpContext = 400
      Hint = 'Create a cell'
      ImageIndex = 59
    end
    object Grid3DOptions: TAction
      Category = 'Grid3D'
      Caption = 'Grid 3D...'
      Enabled = False
      HelpContext = 480
      Hint = 'Grids 3D options|Set options of grids 3D'
      ImageIndex = 60
      OnExecute = MapOptionsExecute
    end
    object Grid3DSelectCell: TAction
      Category = 'Grid3D'
      Caption = 'Select Cell'
      Enabled = False
      GroupIndex = 800
      HelpContext = 400
      Hint = 'Select a cell'
      ImageIndex = 61
    end
    object Grid3DSelectRow: TAction
      Category = 'Grid3D'
      Caption = 'Select Row'
      Enabled = False
      GroupIndex = 800
      HelpContext = 400
      Hint = 'Select a I layer'
      ImageIndex = 62
    end
    object Grid3DSelectCol: TAction
      Category = 'Grid3D'
      Caption = 'Select Column'
      Enabled = False
      GroupIndex = 800
      HelpContext = 400
      Hint = 'Select a J layer'
      ImageIndex = 63
    end
    object Grid3DSelectLay: TAction
      Category = 'Grid3D'
      Caption = 'Select Layer'
      Enabled = False
      GroupIndex = 800
      HelpContext = 400
      Hint = 'Select a K layer'
      ImageIndex = 64
    end
    object Grid3DCreateCell: TAction
      Category = 'Grid3D'
      Caption = 'Create Cell'
      Enabled = False
      GroupIndex = 800
      HelpContext = 400
      Hint = 'Create a cell'
      ImageIndex = 65
    end
    object Mesh2DOptions: TAction
      Category = 'Mesh2D'
      Caption = 'Mesh 2D...'
      Enabled = False
      HelpContext = 490
      Hint = 'Set options of meshes 2D'
      ImageIndex = 69
      OnExecute = MapOptionsExecute
    end
    object Mesh2DSelectNode: TAction
      Category = 'Mesh2D'
      Caption = 'Select Node'
      Enabled = False
      GroupIndex = 900
      HelpContext = 400
      Hint = 'Select a node'
      ImageIndex = 70
    end
    object Mesh2DSelectCell: TAction
      Category = 'Mesh2D'
      Caption = 'Select Cell'
      Enabled = False
      GroupIndex = 900
      HelpContext = 400
      Hint = 'Select a cell'
      ImageIndex = 71
    end
    object Mesh2DCreateCell: TAction
      Category = 'Mesh2D'
      Caption = 'Create Cell'
      Enabled = False
      GroupIndex = 900
      HelpContext = 400
      Hint = 'Create a cell'
      ImageIndex = 72
    end
    object Mesh3DOptions: TAction
      Category = 'Mesh3D'
      Caption = 'Mesh 3D...'
      Enabled = False
      HelpContext = 492
      Hint = 'Set options of meshes 3D'
      ImageIndex = 73
      OnExecute = MapOptionsExecute
    end
    object Mesh3DSelectNode: TAction
      Category = 'Mesh3D'
      Caption = 'Select Node'
      Enabled = False
      GroupIndex = 1000
      HelpContext = 400
      Hint = 'Select a node'
      ImageIndex = 74
    end
    object Mesh3DSelectCell: TAction
      Category = 'Mesh3D'
      Caption = 'Select Cell'
      Enabled = False
      GroupIndex = 1000
      HelpContext = 400
      Hint = 'Select a cell'
      ImageIndex = 75
    end
    object Mesh3DCreateNode: TAction
      Category = 'Mesh3D'
      Caption = 'Create Node'
      Enabled = False
      GroupIndex = 1000
      HelpContext = 400
      Hint = 'Create a node'
      ImageIndex = 76
    end
    object Mesh3DCreateCell: TAction
      Category = 'Mesh3D'
      Caption = 'Create Cell'
      Enabled = False
      GroupIndex = 1000
      HelpContext = 400
      Hint = 'Create a cell'
      ImageIndex = 76
    end
    object aZoomInOut: TAction
      Category = 'Zoom'
      AutoCheck = True
      Caption = 'Zoom InOut'
      Enabled = False
      GroupIndex = 11
      HelpContext = 512
      Hint = 'Zoom|Zoom in or zoom out'
      ImageIndex = 106
      OnExecute = aZoomInOutExecute
    end
    object aZoomToProject: TAction
      Category = 'Zoom'
      AutoCheck = True
      Caption = 'Zoom to Project'
      Enabled = False
      GroupIndex = 12
      HelpContext = 512
      Hint = 'Zoom to project|Zoom map to the project bounds'
      ImageIndex = 94
      OnExecute = aZoomToProjectExecute
    end
    object aZoomToComplete: TAction
      Category = 'Zoom'
      AutoCheck = True
      Caption = 'Zoom to Complete'
      Enabled = False
      GroupIndex = 12
      HelpContext = 514
      Hint = 'Zoom to complete|Zoom to the complete bounds'
      ImageIndex = 95
      OnExecute = aZoomToCompleteExecute
    end
    object aZoomToModel: TAction
      Category = 'Zoom'
      AutoCheck = True
      Caption = 'Zoom to Model'
      Enabled = False
      GroupIndex = 12
      HelpContext = 513
      Hint = 'Zoom to model|Zoom to the active model bounds'
      ImageIndex = 96
      OnExecute = aZoomToModelExecute
    end
    object aZoomToRectangle: TAction
      Category = 'Zoom'
      AutoCheck = True
      Caption = 'Zoom Rectangle'
      Enabled = False
      GroupIndex = 12
      HelpContext = 551
      Hint = 'Zoom rectangle bounds'
      ImageIndex = 98
      OnExecute = aZoomToRectangleExecute
    end
    object ViewProjectManager: TAction
      Category = 'View'
      Caption = 'Project Manager...'
      HelpContext = 501
      Hint = 'Open manager|Open the project manager browser'
      ImageIndex = 105
      ShortCut = 49274
      OnExecute = ViewProjectManagerExecute
    end
    object MapScenery: TAction
      Category = 'Map'
      Caption = 'Scenery...'
      HelpContext = 400
      ImageIndex = 27
      OnExecute = MapSceneryExecute
    end
    object ViewMapWindow: TAction
      Category = 'Map'
      Caption = 'Map Window...'
      Visible = False
    end
    object ViewTableWindow: TAction
      Category = 'View'
      Caption = 'Table Window...'
      Visible = False
      OnExecute = ViewTableWindowExecute
    end
    object ViewGeoscene: TAction
      Category = 'View'
      Caption = 'Geoscene...'
      Enabled = False
      HelpContext = 502
      Hint = 'View geoscene to inspect space'
      Visible = False
    end
    object ViewScale: TAction
      Category = 'View'
      Caption = 'Scale...'
      Enabled = False
      HelpContext = 510
      Hint = 'Map scale|Set scale for the map'
      ImageIndex = 107
      OnExecute = ViewScaleExecute
    end
    object ViewSelect: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Select'
      Checked = True
      Enabled = False
      GroupIndex = 11
      HelpContext = 550
      Hint = 'Select an object'
      ImageIndex = 108
      OnExecute = ViewSelectExecute
    end
    object ViewScroll: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Scroll'
      Enabled = False
      GroupIndex = 11
      HelpContext = 551
      Hint = 'Scroll|Scroll the map'
      ImageIndex = 99
      OnExecute = ViewScrollExecute
    end
    object ViewPan: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Pan'
      Enabled = False
      GroupIndex = 11
      HelpContext = 527
      Hint = 'Pan|Rotate the map'
      ImageIndex = 97
      OnExecute = ViewPanExecute
    end
    object ViewPlaneXY: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Plane XY'
      Enabled = False
      GroupIndex = 22
      HelpContext = 530
      Hint = 'View XY|View XY plane'
      ImageIndex = 100
      OnExecute = ViewPlaneXYExecute
    end
    object ViewPlaneXZ: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Plane XZ'
      Enabled = False
      GroupIndex = 22
      HelpContext = 531
      Hint = 'View XZ|View XZ plane'
      ImageIndex = 101
      OnExecute = ViewPlaneXZExecute
    end
    object ViewPlaneYZ: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Plane YZ'
      Enabled = False
      GroupIndex = 22
      HelpContext = 532
      Hint = 'View YZ|View YZ plane'
      ImageIndex = 102
      OnExecute = ViewPlaneYZExecute
    end
    object ViewVolumeXYZ: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Volume XYZ'
      Checked = True
      Enabled = False
      GroupIndex = 22
      HelpContext = 526
      Hint = 'View XYZ|View XYZ volume'
      ImageIndex = 103
      OnExecute = ViewVolumeXYZExecute
    end
    object ViewStatusline: TAction
      Category = 'View'
      Caption = 'StatusLine'
      Checked = True
      HelpContext = 500
      Hint = 'Show or hide status line'
      OnExecute = ViewStatuslineExecute
    end
    object DrawText: TAction
      Category = 'Draw'
      Caption = 'Text'
      Enabled = False
      HelpContext = 610
      Hint = 'Text tool'
      ImageIndex = 109
      OnExecute = DrawTextExecute
    end
    object DrawSymbol: TAction
      Category = 'Draw'
      Caption = 'Symbol'
      Enabled = False
      HelpContext = 620
      Hint = 'Symbol tool'
      ImageIndex = 110
      OnExecute = DrawSymbolExecute
    end
    object MapOptions: TAction
      Category = 'Map'
      Caption = 'Options...'
      Enabled = False
      HelpContext = 400
      Hint = 'Set a dataset options'
      OnExecute = MapOptionsExecute
    end
    object MapLegend: TAction
      Category = 'Map'
      Caption = 'Legend...'
      Enabled = False
      HelpContext = 403
      Hint = 'Select levels|Set a scale of values for the scalar attribute'
      ImageIndex = 29
      OnExecute = MapLegendExecute
    end
    object MapMaterial: TAction
      Category = 'Map'
      Caption = 'Materials...'
      Enabled = False
      HelpContext = 404
      Hint = 'Select materials|Select materials for an integer attribute'
      ImageIndex = 30
      OnExecute = MapMaterialExecute
    end
    object MapLighting: TAction
      Category = 'Map'
      Caption = 'Lighting...'
      Enabled = False
      HelpContext = 405
      Hint = 'Set lighting|Set options of lighting'
      ImageIndex = 31
      OnExecute = MapLightingExecute
    end
    object MapBackColor: TAction
      Category = 'Map'
      Caption = 'Back Color...'
      Enabled = False
      HelpContext = 409
      Hint = 'Set a background color '
      OnExecute = MapBackColorExecute
    end
    object DrawPolyline: TAction
      Category = 'Draw'
      Caption = 'Polyline'
      Enabled = False
      HelpContext = 630
      Hint = 'Polyline tool'
      ImageIndex = 112
      OnExecute = DrawPolylineExecute
    end
    object DrawPolygon: TAction
      Category = 'Draw'
      Caption = 'Polygon'
      Enabled = False
      HelpContext = 640
      Hint = 'Polygon tool'
      ImageIndex = 113
      OnExecute = DrawPolygonExecute
    end
    object DrawEllipse: TAction
      Category = 'Draw'
      Caption = 'Ellipse'
      Enabled = False
      HelpContext = 600
      Hint = 'Ellipse tool'
      ImageIndex = 114
      OnExecute = DrawEllipseExecute
    end
    object DrawRectangle: TAction
      Category = 'Draw'
      Caption = 'Rectangle'
      Enabled = False
      HelpContext = 640
      Hint = 'Rectangle tool'
      ImageIndex = 115
      OnExecute = DrawRectangleExecute
    end
    object DrawSolid: TAction
      Category = 'Draw'
      Caption = 'Solid'
      Enabled = False
      Hint = 'Solid tool'
      ImageIndex = 123
      OnExecute = DrawSolidExecute
    end
    object DrawReshape: TAction
      Category = 'Draw'
      Caption = 'Reshape'
      Enabled = False
      HelpContext = 630
      ImageIndex = 116
      OnExecute = DrawReshapeExecute
    end
    object DrawAddNode: TAction
      Category = 'Draw'
      Caption = 'Add Node'
      Enabled = False
      HelpContext = 600
      ImageIndex = 117
      OnExecute = DrawAddNodeExecute
    end
    object DrawDepth: TAction
      Category = 'Draw'
      Caption = 'Drawing Depth...'
      Enabled = False
      HelpContext = 600
      OnExecute = DrawDepthExecute
    end
    object FileOpenProject: TAction
      Category = 'File'
      Caption = 'Open Project...'
      HelpContext = 102
      Hint = 'Open a project|Open a project file as text or map'
      ImageIndex = 1
      ShortCut = 16506
      OnExecute = FileOpenProjectExecute
    end
    object FileDataBase: TAction
      Category = 'File'
      Caption = 'Data Base...'
      HelpContext = 1100
      Hint = 'Browser|Data browser'
      ImageIndex = 3
      OnExecute = FileDataBaseExecute
    end
    object FileOpenModel: TAction
      Category = 'File'
      Caption = 'Open Model...'
      HelpContext = 111
      Hint = 'Open a spatial model|Open a model table'
      ImageIndex = 4
      ShortCut = 114
      OnExecute = FileOpenModelExecute
    end
    object FileOpenReport: TAction
      Category = 'File'
      Caption = 'Open Report...'
      HelpContext = 113
      Hint = 'Open a report table'
      ImageIndex = 3
      OnExecute = FileOpenReportExecute
    end
    object EditUndo: TEditUndo
      Category = 'Edit'
      Caption = 'Undo'
      HelpContext = 201
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 155
      ShortCut = 16474
    end
    object EditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cut'
      HelpContext = 202
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 11
      ShortCut = 16472
    end
    object EditCopy: TEditCopy
      Category = 'Edit'
      Caption = 'Copy'
      HelpContext = 203
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 12
      ShortCut = 16451
      OnExecute = EditCopyExecute
    end
    object EditPaste: TEditPaste
      Category = 'Edit'
      Caption = 'Paste'
      Enabled = False
      HelpContext = 204
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 13
      ShortCut = 16470
      OnExecute = EditPasteExecute
    end
    object EditSelectAll: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select All'
      HelpContext = 206
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
      OnExecute = EditSelectAllExecute
    end
    object EditDelete: TEditDelete
      Category = 'Edit'
      Caption = 'Delete'
      HelpContext = 205
      Hint = 'Delete|Erases the selection'
      ImageIndex = 14
      ShortCut = 46
    end
    object EditFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      Enabled = False
      HelpContext = 200
      Hint = 'Find an object by identifier'
      OnExecute = EditFindExecute
    end
    object FileOpenImage: TAction
      Category = 'File'
      Caption = 'Open Image...'
      HelpContext = 100
      Hint = 'Open an image'
      OnExecute = FileOpenImageExecute
    end
    object FileOpenText: TAction
      Category = 'File'
      Caption = 'Open Text...'
      HelpContext = 100
      Hint = 'Open a text file'
      OnExecute = FileOpenTextExecute
    end
    object FileSave: TAction
      Category = 'File'
      Caption = 'Save'
      Enabled = False
      HelpContext = 120
      Hint = 'Save one or more table'
      ImageIndex = 6
      ShortCut = 113
    end
    object FileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      Enabled = False
      HelpContext = 121
      Hint = 'Save one or more table'
      ImageIndex = 161
      ShortCut = 16467
      OnExecute = FileSaveAsExecute
    end
    object FileSaveProjectAs: TAction
      Category = 'File'
      Caption = 'Save Project As...'
      HelpContext = 122
      Hint = 'Save the collector datasets in a superfile'
      OnExecute = FileSaveProjectAsExecute
    end
    object FileClose: TAction
      Category = 'File'
      Caption = 'Close'
      Enabled = False
      HelpContext = 123
      Hint = 'Close the open table'
      OnExecute = FileCloseExecute
    end
    object FileCloseAll: TAction
      Category = 'File'
      Caption = 'Close All'
      Enabled = False
      HelpContext = 124
      Hint = 'Close all windows and tables'
      OnExecute = FileCloseAllExecute
    end
    object FileImport: TAction
      Category = 'File'
      Caption = 'Import...'
      HelpContext = 130
      Hint = 'Import data from another formats'
      OnExecute = FileImportExecute
    end
    object FileExport: TAction
      Category = 'File'
      Caption = 'Export...'
      Enabled = False
      HelpContext = 131
      Hint = 'Export the active data table'
      OnExecute = FileExportExecute
    end
    object FilePrint: TAction
      Category = 'File'
      Caption = 'Print...'
      Enabled = False
      HelpContext = 140
      Hint = 'Print a current open window'
      ImageIndex = 7
      ShortCut = 16464
      OnExecute = FilePrintExecute
    end
    object FilePrintPreview: TAction
      Category = 'File'
      Caption = 'Print Preview...'
      Enabled = False
      HelpContext = 140
      Hint = 'Print a preview '
      OnExecute = FilePrintPreviewExecute
    end
    object acAnalyseProblemBook: TAction
      Category = 'Analyse'
      Caption = 'Problem Book...'
      HelpContext = 702
      Hint = 'Problem book|Open a problem book'
      ImageIndex = 128
      OnExecute = acAnalyseProblemBookExecute
    end
    object CompositingSampleContacts: TAction
      Category = 'Compositing'
      Caption = 'Sample Contacts...'
      HelpContext = 301
      Hint = 'Calculate coordinates of sample contacts'
      ImageIndex = 21
      OnExecute = CompositingSampleContactsExecute
    end
    object CompositingCenters: TAction
      Category = 'Compositing'
      Caption = 'Coordinates of Centers...'
      HelpContext = 302
      Hint = 'Define sample or interval centers'
      ImageIndex = 22
      OnExecute = CompositingCentersExecute
    end
    object CompositingInsideHorizons: TAction
      Category = 'Compositing'
      Caption = 'Inside Horizons...'
      HelpContext = 302
      Hint = 'Compose samples inside horizons'
      ImageIndex = 26
      Visible = False
      OnExecute = CompositingInsideHorizonsExecute
    end
    object CompositingOreIntervals: TAction
      Category = 'Compositing'
      Caption = 'Ore Intervals...'
      HelpContext = 304
      Hint = 'Define ore intervals'
      ImageIndex = 23
      OnExecute = CompositingOreIntervalsExecute
    end
    object CompositingOreSorting: TAction
      Category = 'Compositing'
      Caption = 'Ore Sorting...'
      HelpContext = 303
      Hint = 'Classify samples by ore sorts'
      ImageIndex = 25
      OnExecute = CompositingOreSortingExecute
    end
    object CompositingLinearReserves: TAction
      Category = 'Compositing'
      Caption = 'Linear Reserves...'
      HelpContext = 305
      Hint = 'Calculate linear reserves'
      ImageIndex = 24
      OnExecute = CompositingLinearReservesExecute
    end
    object acAnalyseVolumeCalculation: TAction
      Category = 'Analyse'
      Caption = 'Volume Calculation...'
      Enabled = False
      HelpContext = 712
      Hint = 
        'Volume calculation|Calculate the volume of an current object on ' +
        'map'
      ImageIndex = 151
      Visible = False
      OnExecute = acAnalyseVolumeCalculationExecute
    end
    object acAnalyseReserveCalculation: TAction
      Category = 'Analyse'
      Caption = 'Reserve Calculation...'
      HelpContext = 713
      Hint = 'Reserve Calculation|Calculate reserves with different methods'
      ImageIndex = 129
      OnExecute = acAnalyseReserveCalculationExecute
    end
    object acAnalyseBaseStatistics: TAction
      Category = 'Analyse'
      Caption = 'Basic Statistics...'
      Enabled = False
      HelpContext = 720
      Hint = 'Calculate basic statistics'
      OnExecute = acAnalyseBaseStatisticsExecute
    end
    object acAnalyseFactorAnalysis: TAction
      Category = 'Analyse'
      Caption = 'Factor Analysis...'
      Enabled = False
      HelpContext = 721
      Hint = 'Carry out factor analysis'
    end
    object acAnalyseVariograms: TAction
      Category = 'Analyse'
      Caption = 'Variograms...'
      HelpContext = 722
      Hint = 'Define experimental variograms'
      OnExecute = acAnalyseVariogramsExecute
    end
    object ObserveTop: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Top'
      GroupIndex = 1
      HelpContext = 520
      Hint = 'View top|Set a top view'
      ImageIndex = 86
      OnExecute = ObserveTopExecute
    end
    object ObserveBottom: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Bottom'
      GroupIndex = 1
      HelpContext = 521
      Hint = 'View bottom|Set a bottom view'
      ImageIndex = 87
      OnExecute = ObserveBottomExecute
    end
    object ObserveRight: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Right'
      GroupIndex = 1
      HelpContext = 525
      Hint = 'View right|Set a right view'
      ImageIndex = 90
      OnExecute = ObserveRightExecute
    end
    object ObserveLeft: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Left'
      GroupIndex = 1
      HelpContext = 524
      Hint = 'View left|Set a left view'
      ImageIndex = 91
      OnExecute = ObserveLeftExecute
    end
    object ObserveBack: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Back'
      GroupIndex = 1
      HelpContext = 523
      Hint = 'View back|Set a back view'
      ImageIndex = 89
      OnExecute = ObserveBackExecute
    end
    object ObserveFront: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = 'Front'
      GroupIndex = 1
      HelpContext = 522
      Hint = 'View front|Set a front view'
      ImageIndex = 88
      OnExecute = ObserveFrontExecute
    end
    object Observe3D: TAction
      Category = 'Observe'
      AutoCheck = True
      Caption = '3D'
      GroupIndex = 1
      HelpContext = 526
      Hint = 'View 3D|Set 3D oblique view'
      ImageIndex = 92
      OnExecute = Observe3DExecute
    end
    object ObserveRotate: TAction
      Category = 'Observe'
      Caption = 'Rotate Options...'
      HelpContext = 525
      Hint = 'Rotate map|Rotate and zoom options for map'
      ImageIndex = 93
      OnExecute = ObserveRotateExecute
    end
    object ObservePerspective: TAction
      Category = 'Observe'
      Caption = 'Perspective'
      Checked = True
      HelpContext = 528
      Hint = 'Perspective or orthogonal projection'
      OnExecute = ObservePerspectiveExecute
    end
    object HelpContents: TAction
      Category = 'Help'
      Caption = 'Contents'
      HelpContext = 991
      Hint = 'Display the help table of contents'
      ImageIndex = 135
      ShortCut = 112
      OnExecute = HelpContentsExecute
    end
    object HelpGlossary: TAction
      Category = 'Help'
      Caption = 'Glossary...'
      HelpContext = 992
      Hint = 'Show the help glossary'
      ImageIndex = 136
      OnExecute = HelpGlossaryExecute
    end
    object HelpAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      HelpContext = 999
      Hint = 
        'About|Show the program information, version number and the copyr' +
        'ight'
      OnExecute = HelpAboutExecute
    end
    object WindowClose: TWindowClose
      Category = 'Window'
      Caption = 'Close'
      Enabled = False
      Hint = 'Close'
    end
    object WindowCascade: TWindowCascade
      Category = 'Window'
      Caption = 'Cascade'
      Enabled = False
      Hint = 'Cascade'
      ImageIndex = 132
    end
    object WindowTileHorizontal: TWindowTileHorizontal
      Category = 'Window'
      Caption = 'Tile Horizontally'
      Enabled = False
      Hint = 'Tile horizontal'
      ImageIndex = 133
    end
    object WindowTileVertical: TWindowTileVertical
      Category = 'Window'
      Caption = 'Tile Vertically'
      Enabled = False
      Hint = 'Tile vertical'
      ImageIndex = 134
    end
    object WindowMinimizeAll: TWindowMinimizeAll
      Category = 'Window'
      Caption = 'Minimize All'
      Enabled = False
      Hint = 'Minimize All'
    end
    object WindowArrange: TWindowArrange
      Category = 'Window'
      Caption = 'Arrange'
      Enabled = False
      Hint = 'Arrange all icons'
    end
    object StyleText: TAction
      Category = 'Style'
      Caption = 'Text...'
      HelpContext = 611
      Hint = 'Text style|Set a text style for new or selected text'
      ImageIndex = 118
      OnExecute = StyleTextExecute
    end
    object StyleSymbol: TAction
      Category = 'Style'
      Caption = 'Symbol...'
      HelpContext = 613
      Hint = 'Symbol style|Set a symbol style for new or selected objects'
      ImageIndex = 119
      OnExecute = StyleSymbolExecute
    end
    object StyleLine: TAction
      Category = 'Style'
      Caption = 'Line...'
      HelpContext = 615
      Hint = 'Line style|Set a line style for new or selected objects'
      ImageIndex = 120
      OnExecute = StyleLineExecute
    end
    object StyleFill: TAction
      Category = 'Style'
      Caption = 'Fill...'
      HelpContext = 617
      Hint = 'Fill style|Set a fill style for new or selected objects'
      ImageIndex = 121
      OnExecute = StyleFillExecute
    end
    object CalculatorGeology: TAction
      Category = 'Calculator'
      Caption = 'Geology...'
      HelpContext = 812
      Hint = 'Geology Calculator'
      OnExecute = CalculatorGeologyExecute
    end
    object CalculatorGeometry: TAction
      Category = 'Calculator'
      Caption = 'Geometry...'
      HelpContext = 811
      Hint = 'Geometry Calculator'
      OnExecute = CalculatorGeometryExecute
    end
    object ToolsConfiguration: TAction
      Category = 'Tools'
      Caption = 'Configuration...'
      HelpContext = 801
      Hint = 'Configuration|Configurate program parameters'
      ImageIndex = 130
      OnExecute = ToolsConfigurationExecute
    end
    object ToolsCustomize: TCustomizeActionBars
      Category = 'Tools'
      Caption = 'Customize...'
      HelpContext = 801
      Hint = 'Customize|Customize user interface'
      CustomizeDlg.StayOnTop = True
    end
    object ToolsStandardStyle: TAction
      Category = 'Tools'
      Caption = 'Standard Style'
      Checked = True
      GroupIndex = 1
      HelpContext = 800
      Hint = 'Standard Style|Standard Winows style'
      OnExecute = ToolsStandardStyleExecute
    end
    object ToolsXPStyle: TAction
      Category = 'Tools'
      Caption = 'XP Style'
      GroupIndex = 1
      HelpContext = 800
      Hint = 'XP Style|Winows XP style'
      OnExecute = ToolsXPStyleExecute
    end
    object WindowRedraw: TAction
      Category = 'Window'
      Caption = 'Redraw'
      Hint = 'Redraw'
      OnExecute = WindowRedrawExecute
    end
    object ToolsUnitsConverter: TAction
      Category = 'Tools'
      Caption = 'Units Converter...'
      HelpContext = 815
      Hint = 'Convert units'
      OnExecute = ToolsUnitsConverterExecute
    end
    object CalculatorMining: TAction
      Category = 'Calculator'
      Caption = 'Mining...'
      HelpContext = 814
      Hint = 'Mining calculator'
      OnExecute = CalculatorMiningExecute
    end
    object CalculatorSurvey: TAction
      Category = 'Calculator'
      Caption = 'Survey...'
      HelpContext = 813
      Hint = 'Survey calculator'
      OnExecute = CalculatorSurveyExecute
    end
    object FileExit: TFileExit
      Category = 'File'
      Caption = 'Exit'
      HelpContext = 199
      Hint = 'Exit|Quits Geoblock'
      ImageIndex = 162
      ShortCut = 32856
    end
    object Action1: TAction
      Category = 'Display'
      Caption = 'Option'
    end
    object MethodSimulation: TAction
      Category = 'Method'
      Caption = 'Simulation...'
      ImageIndex = 38
      OnExecute = MethodSimulationExecute
    end
    object OctreeConstruction: TAction
      Category = 'Method'
      Caption = 'Octree Construction...'
      OnExecute = OctreeConstructionExecute
    end
  end
  object XPColorMap: TXPColorMap
    HighlightColor = 15660791
    BtnSelectedColor = clBtnFace
    UnusedColor = 15660791
    Left = 792
    Top = 184
  end
  object CustomizeDlg: TCustomizeDlg
    ActionManager = ActionManager
    StayOnTop = True
    Left = 792
    Top = 256
  end
end
