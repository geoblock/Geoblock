inherited fmMainForm: TfmMainForm
  Left = 22
  Top = 40
  Caption = 'Geochronology'
  ClientHeight = 639
  ClientWidth = 960
  OldCreateOrder = True
  OnCreate = FormCreate
  ExplicitWidth = 966
  ExplicitHeight = 668
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 960
    Height = 121
    Align = alNone
    ExplicitWidth = 960
    ExplicitHeight = 121
    object GroupBoxTimeScales: TGroupBox
      Left = 1
      Top = 1
      Width = 958
      Height = 119
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object PanelTimeScale: TPanel
        Left = 2
        Top = 18
        Width = 954
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Time scale'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
      end
      object PanelUniverseScale: TPanel
        Left = 2
        Top = 41
        Width = 954
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object StaticText1: TStaticText
          Left = 943
          Top = 0
          Width = 11
          Height = 20
          Align = alRight
          Caption = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object StaticText2: TStaticText
          Left = 0
          Top = 0
          Width = 90
          Height = 20
          Align = alLeft
          Caption = '13 700 000 000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
      object TrackBarScale: TTrackBar
        Left = 2
        Top = 62
        Width = 954
        Height = 25
        Align = alTop
        Max = 100
        TabOrder = 2
        ThumbLength = 10
        TickMarks = tmTopLeft
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 136
    Width = 960
    Height = 455
    Align = alBottom
    ExplicitTop = 136
    ExplicitWidth = 960
    ExplicitHeight = 455
    object Splitter1: TSplitter
      Left = 274
      Top = 1
      Width = 15
      Height = 453
    end
    object GroupBoxHistoryTree: TGroupBox
      Left = 1
      Top = 1
      Width = 273
      Height = 453
      Align = alLeft
      Caption = 'History tree'
      TabOrder = 0
      object TreeView: TTreeView
        Left = 2
        Top = 18
        Width = 269
        Height = 433
        Align = alClient
        AutoExpand = True
        Indent = 19
        TabOrder = 0
      end
    end
    object GroupBoxLegend: TGroupBox
      Left = 289
      Top = 1
      Width = 527
      Height = 453
      Align = alClient
      Caption = 'Legend'
      TabOrder = 1
      object DBGrid: TDBGrid
        Left = 281
        Top = 18
        Width = 244
        Height = 433
        Align = alRight
        Ctl3D = True
        DataSource = DataSourceAge
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
        ParentCtl3D = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'AGE'
            Title.Alignment = taCenter
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SYMBOL'
            Visible = True
          end>
      end
      object DBGridEpoch: TDBGrid
        Left = 24
        Top = 18
        Width = 257
        Height = 433
        Align = alRight
        DataSource = DataSourceEpoch
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -13
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'EPOCH'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SYMBOL'
            Width = 78
            Visible = True
          end>
      end
    end
    object GroupBoxDescription: TGroupBox
      Left = 816
      Top = 1
      Width = 143
      Height = 453
      Align = alRight
      Caption = 'Description'
      TabOrder = 2
      object DBRichEditDescription: TDBRichEdit
        Left = 2
        Top = 18
        Width = 139
        Height = 433
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        TabOrder = 0
        Zoom = 100
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 591
    Width = 960
    ExplicitTop = 591
    ExplicitWidth = 960
    inherited ButtonOK: TButton
      Left = 599
      ExplicitLeft = 599
    end
    inherited ButtonCancel: TButton
      Left = 719
      ExplicitLeft = 719
    end
    inherited ButtonHelp: TButton
      Left = 841
      OnClick = nil
      ExplicitLeft = 841
    end
  end
  object DataSourceAge: TDataSource
    DataSet = TableAge
    Left = 597
    Top = 522
  end
  object TableAge: TTable
    Left = 589
    Top = 474
  end
  object DataSourceEpoch: TDataSource
    DataSet = TableEpoch
    Left = 381
    Top = 506
  end
  object TableEpoch: TTable
    Left = 373
    Top = 466
  end
end
