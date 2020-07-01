inherited fmFileNew: TfmFileNew
  Left = 282
  Top = 178
  HelpContext = 101
  Caption = 'Simulation of Points'
  ClientHeight = 652
  ClientWidth = 755
  ExplicitWidth = 761
  ExplicitHeight = 681
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 755
    Height = 17
    ExplicitLeft = -26
    ExplicitTop = -15
    ExplicitWidth = 755
    ExplicitHeight = 17
  end
  inherited PanelMiddle: TPanel
    Top = 17
    Width = 755
    Height = 588
    ExplicitTop = 17
    ExplicitWidth = 755
    ExplicitHeight = 588
    inherited PageControl: TPageControl
      Width = 745
      Height = 519
      ActivePage = tsSurfaces
      Images = fmInitialForm.ImageListPictures
      Style = tsButtons
      OnChange = PageControlChange
      OnChanging = PageControlChanging
      ExplicitWidth = 745
      ExplicitHeight = 519
      object tsLines: TTabSheet
        BorderWidth = 10
        Caption = 'Lines'
        object rgLines: TRadioGroup
          Left = 55
          Top = 1
          Width = 610
          Height = 46
          Caption = 'Outlines'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'Segments'
            'Polylines'
            'Polygons')
          TabOrder = 0
          OnClick = rgLinesClick
        end
      end
      object tsAreas: TTabSheet
        BorderWidth = 10
        Caption = 'Areas'
        ImageIndex = 1
        object rgAreas: TRadioGroup
          Left = 55
          Top = 1
          Width = 610
          Height = 46
          Caption = 'Outlines'
          Columns = 3
          ItemIndex = 0
          Items.Strings = (
            'Rectangle'
            'Circle'
            'Triangle')
          TabOrder = 0
          OnClick = rgAreasClick
        end
      end
      object tsSurfaces: TTabSheet
        Caption = 'Surfaces'
        ImageIndex = 4
        object rgSurfaces: TRadioGroup
          Left = 65
          Top = 13
          Width = 610
          Height = 46
          Caption = 'Outlines'
          Columns = 4
          ItemIndex = 0
          Items.Strings = (
            'Box'
            'Sphere'
            'Cylinder'
            'Cone')
          TabOrder = 0
          OnClick = rgSurfacesClick
        end
      end
      object tsVolumes: TTabSheet
        Caption = 'Volumes'
        ImageIndex = 2
        object rgVolumes: TRadioGroup
          Left = 65
          Top = 13
          Width = 610
          Height = 46
          Caption = 'Outlines'
          Columns = 4
          ItemIndex = 0
          Items.Strings = (
            'Box'
            'Sphere'
            'Cylinder'
            'Cone')
          TabOrder = 0
          OnClick = rgVolumesClick
        end
      end
    end
    object GroupBoxStart: TGroupBox
      Left = 30
      Top = 175
      Width = 161
      Height = 151
      Caption = 'Start'
      TabOrder = 1
      object stXO: TStaticText
        Tag = 1000
        Left = 16
        Top = 28
        Width = 22
        Height = 20
        Caption = 'XO'
        TabOrder = 0
      end
      object stYO: TStaticText
        Tag = 1000
        Left = 15
        Top = 54
        Width = 23
        Height = 20
        Caption = 'YO'
        TabOrder = 1
      end
      object stZO: TStaticText
        Tag = 1000
        Left = 16
        Top = 88
        Width = 22
        Height = 20
        Caption = 'ZO'
        TabOrder = 2
      end
      object stGO: TStaticText
        Tag = 1000
        Left = 14
        Top = 114
        Width = 24
        Height = 20
        Caption = 'GO'
        TabOrder = 3
      end
      object evXO: TEdit
        Left = 44
        Top = 18
        Width = 81
        Height = 24
        TabOrder = 4
        Text = '-512'
      end
      object evYO: TEdit
        Left = 44
        Top = 50
        Width = 81
        Height = 24
        TabOrder = 5
        Text = '-512'
      end
      object evZO: TEdit
        Left = 44
        Top = 80
        Width = 81
        Height = 24
        TabOrder = 6
        Text = '-512'
      end
      object evGO: TEdit
        Left = 44
        Top = 110
        Width = 81
        Height = 24
        TabOrder = 7
        Text = '0'
      end
    end
    object GroupBoxEnd: TGroupBox
      Left = 197
      Top = 175
      Width = 180
      Height = 151
      Caption = 'End'
      TabOrder = 2
      object stYE: TStaticText
        Tag = 1000
        Left = 16
        Top = 56
        Width = 22
        Height = 20
        Caption = 'YE'
        TabOrder = 0
      end
      object stXE: TStaticText
        Tag = 1000
        Left = 17
        Top = 30
        Width = 21
        Height = 20
        Caption = 'XE'
        TabOrder = 1
      end
      object stZE: TStaticText
        Tag = 1000
        Left = 17
        Top = 85
        Width = 21
        Height = 20
        Caption = 'ZE'
        TabOrder = 2
      end
      object stGE: TStaticText
        Tag = 1000
        Left = 17
        Top = 111
        Width = 23
        Height = 20
        Caption = 'GE'
        TabOrder = 3
      end
      object evXE: TEdit
        Left = 60
        Top = 18
        Width = 85
        Height = 24
        TabOrder = 4
        Text = '512'
      end
      object evYE: TEdit
        Left = 60
        Top = 46
        Width = 85
        Height = 24
        TabOrder = 5
        Text = '512'
      end
      object evZE: TEdit
        Left = 60
        Top = 76
        Width = 85
        Height = 24
        TabOrder = 6
        Text = '512'
      end
      object evGE: TEdit
        Left = 62
        Top = 106
        Width = 85
        Height = 24
        TabOrder = 7
        Text = '100'
      end
    end
    object seNPoints: TSpinEdit
      Left = 259
      Top = 129
      Width = 74
      Height = 26
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 3
      Value = 1000
    end
    object StaticTextNumberOfPoints: TStaticText
      Left = 38
      Top = 129
      Width = 105
      Height = 20
      Caption = 'Number of points'
      TabOrder = 4
    end
    object GroupBoxOutput: TGroupBox
      Left = 5
      Top = 524
      Width = 745
      Height = 59
      Align = alBottom
      Caption = 'Output'
      TabOrder = 5
      object ToolBarShowAs: TToolBar
        Left = 2
        Top = 18
        Width = 741
        Height = 39
        Align = alClient
        AutoSize = True
        ButtonHeight = 25
        Caption = 'Show as'
        Color = clBtnFace
        EdgeOuter = esNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -14
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        HotImages = fmInitialForm.ImageListInterface
        Images = fmInitialForm.ImageListInterface
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        DesignSize = (
          741
          39)
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
          Caption = 'Map'
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
          Caption = 'Table'
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
          Hint = 'Graph|Show the result as graph'
          Caption = 'Graph'
          Grouped = True
          ImageIndex = 140
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ToolButtonShowAsClick
        end
        object PanelOutPath: TPanel
          AlignWithMargins = True
          Left = 77
          Top = 0
          Width = 476
          Height = 25
          Align = alLeft
          Alignment = taLeftJustify
          BevelOuter = bvLowered
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object EditOutputName: TEdit
          Left = 553
          Top = 0
          Width = 146
          Height = 25
          Anchors = [akTop, akBottom]
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object SpeedButtonOutputBrowse: TSpeedButton
          Left = 699
          Top = 0
          Width = 23
          Height = 25
          Hint = 'Browse'
          Anchors = [akTop, akRight]
          Caption = '...'
          ParentShowHint = False
          ShowHint = True
          Visible = False
        end
      end
    end
    object rgDistribution: TRadioGroup
      Left = 30
      Top = 344
      Width = 347
      Height = 102
      Caption = 'Distribution of values'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Uniform'
        'Normal'
        'Log Normal'
        'Exponential'
        'Poisson'
        'Empirical')
      TabOrder = 6
      OnClick = rgDistributionClick
    end
    object GroupBox: TGroupBox
      Left = 417
      Top = 175
      Width = 312
      Height = 343
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Classes'
      TabOrder = 7
      object StringGridClasses: TStringGrid
        Left = 2
        Top = 35
        Width = 308
        Height = 306
        Align = alClient
        ColCount = 4
        RowCount = 12
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        ColWidths = (
          64
          64
          64
          64)
        RowHeights = (
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24
          24)
      end
      object HeaderControlGrid: THeaderControl
        Left = 2
        Top = 18
        Width = 308
        Height = 17
        Sections = <
          item
            Alignment = taCenter
            ImageIndex = -1
            Text = 'No.'
            Width = 50
          end
          item
            Alignment = taCenter
            ImageIndex = -1
            Text = '>='
            Width = 90
          end
          item
            Alignment = taCenter
            ImageIndex = -1
            Text = '<'
            Width = 90
          end
          item
            Alignment = taCenter
            ImageIndex = -1
            Text = 'Kind'
            Width = 50
          end>
      end
    end
    object SpinEditClasses: TSpinEdit
      Left = 615
      Top = 129
      Width = 57
      Height = 26
      MaxValue = 12
      MinValue = 2
      TabOrder = 8
      Value = 12
      OnChange = SpinEditClassesChange
    end
    object StaticTextNumberOfClasses: TStaticText
      Left = 458
      Top = 135
      Width = 116
      Height = 20
      Caption = 'Number of classes'
      TabOrder = 9
    end
  end
  inherited PanelBottom: TPanel
    Top = 605
    Width = 755
    ExplicitTop = 605
    ExplicitWidth = 755
    inherited ButtonOK: TButton
      Left = 417
      OnClick = ButtonOKClick
      ExplicitLeft = 417
    end
    inherited ButtonCancel: TButton
      Left = 530
      ExplicitLeft = 530
    end
    inherited ButtonHelp: TButton
      Left = 644
      ExplicitLeft = 644
    end
    object ProgressBar: TProgressBar
      Left = 9
      Top = 12
      Width = 176
      Height = 18
      TabOrder = 3
    end
  end
  object rgSpacing: TRadioGroup
    Left = 488
    Top = 8
    Width = 196
    Height = 65
    Caption = 'Spacing'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Random'
      'Regular')
    TabOrder = 3
  end
end
