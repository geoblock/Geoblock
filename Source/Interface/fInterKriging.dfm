inherited fmInterKriging: TfmInterKriging
  Left = 286
  Top = 116
  HelpContext = 3141
  Caption = 'Kriging Options'
  ClientHeight = 457
  ClientWidth = 714
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 720
  ExplicitHeight = 486
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Left = 153
    Width = 561
    Height = 409
    Align = alClient
    BevelOuter = bvNone
    ExplicitLeft = 153
    ExplicitWidth = 561
    ExplicitHeight = 409
    object GroupBoxVariogram: TGroupBox
      Left = 264
      Top = 0
      Width = 297
      Height = 161
      Align = alClient
      Caption = 'Variogram model'
      TabOrder = 0
      object TreeViewVariogram: TTreeView
        Left = 2
        Top = 18
        Width = 293
        Height = 141
        Align = alClient
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnClick = TreeViewVariogramClick
      end
    end
    object GroupBoxsearchOptions: TGroupBox
      Left = 0
      Top = 0
      Width = 264
      Height = 161
      Align = alLeft
      Caption = 'Search options'
      TabOrder = 1
      object LabelPointsNumber: TLabel
        Left = 12
        Top = 31
        Width = 101
        Height = 16
        Caption = 'Number of points'
      end
      object LabelMin: TLabel
        Left = 160
        Top = 11
        Width = 21
        Height = 16
        Caption = 'Min'
      end
      object LabelMax: TLabel
        Left = 214
        Top = 11
        Width = 25
        Height = 16
        Caption = 'Max'
      end
      object LabelSearchRadius: TLabel
        Left = 12
        Top = 127
        Width = 141
        Height = 16
        Caption = 'Maximum search radius'
      end
      object EditMinPoints: TEdit
        Left = 159
        Top = 28
        Width = 41
        Height = 24
        TabOrder = 0
        Text = '1'
      end
      object EditMaxPoints: TEdit
        Left = 214
        Top = 28
        Width = 44
        Height = 24
        TabOrder = 1
        Text = '20'
      end
      object GroupBoxSearchType: TGroupBox
        Left = 6
        Top = 53
        Width = 252
        Height = 68
        Caption = 'Search type'
        TabOrder = 2
        object RadioButtonNormalSearch: TRadioButton
          Left = 17
          Top = 17
          Width = 113
          Height = 17
          Caption = 'Normal'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButtonOctantSearch: TRadioButton
          Left = 17
          Top = 40
          Width = 136
          Height = 17
          Caption = 'Points per octant'
          TabOrder = 1
        end
        object EditPointsPerOctant: TEdit
          Left = 206
          Top = 38
          Width = 43
          Height = 24
          TabOrder = 2
          Text = '4'
        end
      end
      object EditRadius: TEdit
        Left = 192
        Top = 124
        Width = 66
        Height = 24
        TabOrder = 3
        Text = '100'
      end
    end
    object DBChart: TDBChart
      Left = 0
      Top = 161
      Width = 561
      Height = 248
      AllowPanning = pmNone
      BackWall.Brush.Style = bsClear
      Title.Text.Strings = (
        'TDBChart')
      BottomAxis.Title.Caption = 'Lag'
      DepthAxis.Automatic = False
      DepthAxis.AutomaticMaximum = False
      DepthAxis.AutomaticMinimum = False
      DepthAxis.Maximum = -4.630000000000002000
      DepthAxis.Minimum = -5.630000000000002000
      LeftAxis.Title.Caption = 'Variogram'
      Legend.Visible = False
      RightAxis.Automatic = False
      RightAxis.AutomaticMaximum = False
      RightAxis.AutomaticMinimum = False
      View3D = False
      View3DWalls = False
      Zoom.Allow = False
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
      object Series2: TLineSeries
        Legend.Visible = False
        SeriesColor = clGreen
        ShowInLegend = False
        Brush.BackColor = clDefault
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
  inherited PanelMiddle: TPanel
    Left = 680
    Top = 441
    Width = 34
    Height = 16
    Align = alNone
    Enabled = False
    Visible = False
    ExplicitLeft = 680
    ExplicitTop = 441
    ExplicitWidth = 34
    ExplicitHeight = 16
  end
  inherited PanelBottom: TPanel
    Top = 409
    Width = 714
    BevelOuter = bvLowered
    ExplicitTop = 409
    ExplicitWidth = 714
    object LabelMaxLag: TLabel [0]
      Left = 220
      Top = 16
      Width = 47
      Height = 16
      Caption = 'Max lag'
    end
    inherited ButtonOK: TButton
      Left = 360
      OnClick = ButtonOKClick
      ExplicitLeft = 360
    end
    inherited ButtonCancel: TButton
      Left = 481
      ExplicitLeft = 481
    end
    inherited ButtonHelp: TButton
      Left = 603
      ExplicitLeft = 603
    end
    object ButtonNewModel: TButton
      Left = 123
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Model'
      TabOrder = 3
      OnClick = ButtonNewModelClick
    end
    object EditMax: TEdit
      Left = 279
      Top = 16
      Width = 54
      Height = 24
      TabOrder = 4
      Text = '100'
      OnExit = EditMaxChange
      OnKeyDown = EditMaxKeyDown
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 409
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
    object GroupBoxType: TGroupBox
      Left = 0
      Top = 0
      Width = 153
      Height = 409
      Align = alClient
      Caption = 'Type'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object RadioButtonSimple: TRadioButton
        Left = 8
        Top = 32
        Width = 134
        Height = 17
        Caption = 'Simple'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object RadioButtonOrdinary: TRadioButton
        Left = 8
        Top = 55
        Width = 134
        Height = 23
        Caption = 'Ordinary'
        TabOrder = 1
      end
      object GroupBoxDrift: TGroupBox
        Left = 2
        Top = 87
        Width = 149
        Height = 320
        Align = alBottom
        Caption = 'Drift'
        TabOrder = 2
        object GroupBox1: TGroupBox
          Left = 2
          Top = 18
          Width = 145
          Height = 95
          Align = alTop
          Caption = 'Linear'
          TabOrder = 0
          object CheckBoxLinearX: TCheckBox
            Left = 10
            Top = 22
            Width = 100
            Height = 17
            Caption = 'X'
            TabOrder = 0
          end
          object CheckBoxLinearY: TCheckBox
            Left = 10
            Top = 45
            Width = 100
            Height = 17
            Caption = 'Y'
            TabOrder = 1
          end
          object CheckBoxLinearZ: TCheckBox
            Left = 10
            Top = 68
            Width = 100
            Height = 17
            Caption = 'Z'
            TabOrder = 2
          end
        end
        object GroupBox2: TGroupBox
          Left = 2
          Top = 113
          Width = 145
          Height = 104
          Align = alTop
          Caption = 'Quadratic'
          TabOrder = 1
          object CheckBoxQuadraticX: TCheckBox
            Left = 10
            Top = 24
            Width = 100
            Height = 17
            Caption = 'X'
            TabOrder = 0
          end
          object CheckBoxQuadraticY: TCheckBox
            Left = 10
            Top = 70
            Width = 100
            Height = 17
            Caption = 'Y'
            TabOrder = 1
          end
          object CheckBoxQuadraticZ: TCheckBox
            Left = 10
            Top = 47
            Width = 100
            Height = 17
            Caption = 'Z'
            TabOrder = 2
          end
        end
        object GroupBox3: TGroupBox
          Left = 2
          Top = 217
          Width = 145
          Height = 101
          Align = alClient
          Caption = 'Cross-quadratic'
          TabOrder = 2
          object CheckBoxQuadraticXY: TCheckBox
            Left = 10
            Top = 23
            Width = 100
            Height = 17
            Caption = 'XY'
            TabOrder = 0
          end
          object CheckBoxQuadraticXZ: TCheckBox
            Left = 10
            Top = 46
            Width = 100
            Height = 17
            Caption = 'XZ'
            TabOrder = 1
          end
          object CheckBoxQuadraticYZ: TCheckBox
            Left = 10
            Top = 69
            Width = 100
            Height = 17
            Caption = 'YZ'
            TabOrder = 2
          end
        end
      end
    end
  end
  object Query: TQuery
    Left = 640
    Top = 120
  end
  object QueryData: TQuery
    Left = 608
    Top = 120
  end
  object Table: TTable
    Left = 640
    Top = 88
  end
end
