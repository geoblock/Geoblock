inherited fmMethodVarioModeller: TfmMethodVarioModeller
  HelpContext = 320
  Caption = 'Variogram Modelling'
  ClientHeight = 544
  ClientWidth = 784
  OnClose = FormClose
  OnCreate = FormCreate
  ExplicitWidth = 790
  ExplicitHeight = 573
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 784
    Height = 305
    ExplicitWidth = 784
    ExplicitHeight = 305
    object DBChart: TDBChart
      Left = 1
      Top = 1
      Width = 782
      Height = 303
      BackWall.Brush.Style = bsClear
      Title.Text.Strings = (
        '')
      Title.Visible = False
      BottomAxis.Title.Caption = 'Lag'
      DepthAxis.Automatic = False
      DepthAxis.AutomaticMaximum = False
      DepthAxis.AutomaticMinimum = False
      DepthAxis.Maximum = -4.600000000000001000
      DepthAxis.Minimum = -5.600000000000001000
      LeftAxis.Title.Caption = 'Variogram'
      RightAxis.Automatic = False
      RightAxis.AutomaticMaximum = False
      RightAxis.AutomaticMinimum = False
      View3D = False
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
      object Series1: TLineSeries
        Marks.Callout.Length = 20
        Brush.BackColor = clDefault
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
      object Series2: TLineSeries
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
    Top = 305
    Width = 784
    Height = 191
    ExplicitTop = 305
    ExplicitWidth = 784
    ExplicitHeight = 191
    object LabelExpVar: TLabel
      Left = 16
      Top = 10
      Width = 3
      Height = 16
    end
    object GroupBoxVariogramModel: TGroupBox
      Left = 257
      Top = 1
      Width = 526
      Height = 189
      Align = alClient
      Caption = 'Variogram model'
      TabOrder = 0
      object LabelModelFunction: TLabel
        Left = 187
        Top = 23
        Width = 86
        Height = 16
        Caption = 'Model function'
      end
      object LabelNugget: TLabel
        Left = 187
        Top = 57
        Width = 44
        Height = 16
        Caption = 'Nugget'
      end
      object LabelContribution: TLabel
        Left = 187
        Top = 79
        Width = 70
        Height = 16
        Caption = 'Contribution'
      end
      object LabelAnis1: TLabel
        Left = 187
        Top = 123
        Width = 33
        Height = 16
        Caption = 'Anis1'
      end
      object LabelAnis2: TLabel
        Left = 187
        Top = 145
        Width = 33
        Height = 16
        Caption = 'Anis2'
      end
      object LabelAzimuth: TLabel
        Left = 380
        Top = 57
        Width = 46
        Height = 16
        Caption = 'Azimuth'
      end
      object LabelDip: TLabel
        Left = 380
        Top = 79
        Width = 21
        Height = 16
        Caption = 'Dip'
      end
      object LabelPlunge: TLabel
        Left = 380
        Top = 101
        Width = 42
        Height = 16
        Caption = 'Plunge'
      end
      object LabelRange: TLabel
        Left = 187
        Top = 101
        Width = 41
        Height = 16
        Caption = 'Range'
      end
      object ComboBoxModelFunction: TComboBox
        Left = 360
        Top = 20
        Width = 163
        Height = 24
        Style = csDropDownList
        TabOrder = 0
        OnChange = ModelPropertyChange
        Items.Strings = (
          'Spherical'
          'Exponential'
          'Gaussian'
          'Power')
      end
      object EditNugget: TEdit
        Left = 299
        Top = 52
        Width = 64
        Height = 24
        TabOrder = 1
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditContribution: TEdit
        Left = 299
        Top = 74
        Width = 64
        Height = 24
        TabOrder = 2
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditRange: TEdit
        Left = 299
        Top = 96
        Width = 64
        Height = 24
        TabOrder = 3
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditAzimuth: TEdit
        Left = 465
        Top = 52
        Width = 57
        Height = 24
        TabOrder = 4
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditDip: TEdit
        Left = 465
        Top = 74
        Width = 57
        Height = 24
        TabOrder = 5
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditPlunge: TEdit
        Left = 465
        Top = 96
        Width = 57
        Height = 24
        TabOrder = 6
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditAnis1: TEdit
        Left = 299
        Top = 118
        Width = 64
        Height = 24
        TabOrder = 7
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object EditAnis2: TEdit
        Left = 299
        Top = 140
        Width = 64
        Height = 24
        TabOrder = 8
        OnExit = ModelPropertyChange
        OnKeyDown = ModelPropertyKeyDown
      end
      object GroupBoxNestedStructures: TGroupBox
        Left = 4
        Top = 25
        Width = 163
        Height = 143
        Caption = 'Nested structures'
        TabOrder = 9
        object LabelNestedStructuesNumber: TLabel
          Left = 3
          Top = 24
          Width = 48
          Height = 16
          Caption = 'Number'
        end
        object ListBoxNestedStructures: TListBox
          Left = 2
          Top = 56
          Width = 151
          Height = 84
          TabOrder = 0
          OnClick = ListBoxNestedStructuresClick
        end
        object SpinEditNestedStructuresNumber: TSpinEdit
          Left = 81
          Top = 24
          Width = 56
          Height = 26
          MaxValue = 0
          MinValue = 1
          TabOrder = 1
          Value = 0
          OnChange = SpinEditNestedStructuresNumberChange
        end
      end
      object ButtonAutoFit: TButton
        Left = 432
        Top = 136
        Width = 89
        Height = 25
        Caption = 'Fitting'
        TabOrder = 10
        OnClick = ButtonAutoFitClick
      end
    end
    object GroupBoxExpVariogram: TGroupBox
      Left = 1
      Top = 1
      Width = 256
      Height = 189
      Align = alLeft
      Caption = 'Experimental variogram'
      TabOrder = 1
      object TreeViewVariogram: TTreeView
        Left = 2
        Top = 18
        Width = 252
        Height = 169
        Align = alClient
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnClick = TreeViewVariogramClick
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 496
    Width = 784
    ExplicitTop = 496
    ExplicitWidth = 784
    inherited ButtonOK: TButton
      Left = 914
      OnClick = ButtonOKClick
      ExplicitLeft = 914
    end
    inherited ButtonCancel: TButton
      Left = 1034
      ExplicitLeft = 1034
    end
    inherited ButtonHelp: TButton
      Left = 1156
      HelpContext = 320
      ExplicitLeft = 1156
    end
    object ButtonNewVariogram: TButton
      Left = 16
      Top = 14
      Width = 113
      Height = 25
      Hint = 'Define experimental variograms'
      HelpContext = 722
      Caption = 'Calculation...'
      TabOrder = 3
      OnClick = ButtonNewVariogramClick
    end
  end
  object Query: TQuery
    Left = 360
    Top = 56
  end
end
