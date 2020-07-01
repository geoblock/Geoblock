inherited fmViewVariogram: TfmViewVariogram
  HelpContext = 722
  Caption = 'View variogram'
  ClientHeight = 508
  ClientWidth = 766
  OnCreate = FormCreate
  OnShow = FormShow
  ExplicitWidth = 772
  ExplicitHeight = 537
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 766
    Height = 465
    ExplicitWidth = 766
    ExplicitHeight = 465
    object GroupBoxVariogram: TGroupBox
      Left = 1
      Top = 1
      Width = 216
      Height = 463
      Align = alLeft
      Caption = 'Experimental variogram'
      TabOrder = 0
      object TreeViewVariogram: TTreeView
        Left = 2
        Top = 18
        Width = 212
        Height = 443
        Align = alClient
        AutoExpand = True
        Indent = 19
        TabOrder = 0
        OnClick = TreeViewVariogramClick
      end
    end
    object PageControlViewMode: TPageControl
      Left = 217
      Top = 1
      Width = 548
      Height = 463
      ActivePage = TabSheetGraph
      Align = alClient
      TabOrder = 1
      object TabSheetTable: TTabSheet
        Caption = 'Table'
        object DBGrid: TDBGrid
          Left = 0
          Top = 0
          Width = 540
          Height = 432
          Align = alClient
          DataSource = DataSource
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -13
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
        end
      end
      object TabSheetGraph: TTabSheet
        Caption = 'Graph'
        ImageIndex = 1
        object DBChart: TDBChart
          Left = 0
          Top = 0
          Width = 540
          Height = 432
          BackWall.Brush.Style = bsClear
          Title.Text.Strings = (
            '')
          Title.Visible = False
          Legend.Visible = False
          View3D = False
          View3DWalls = False
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 13
          object Series1: TLineSeries
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
    end
  end
  inherited PanelMiddle: TPanel
    Top = 465
    Width = 766
    Height = 17
    Visible = False
    ExplicitTop = 465
    ExplicitWidth = 766
    ExplicitHeight = 17
  end
  inherited PanelBottom: TPanel
    Top = 460
    Width = 766
    ExplicitTop = 460
    ExplicitWidth = 766
    inherited ButtonOK: TButton
      Left = 397
      Align = alCustom
      ExplicitLeft = 397
    end
    inherited ButtonCancel: TButton
      Left = 517
      Align = alCustom
      ExplicitLeft = 517
    end
    inherited ButtonHelp: TButton
      Left = 639
      Align = alCustom
      ExplicitLeft = 639
    end
  end
  object Query: TQuery
    Left = 40
    Top = 104
  end
  object DataSource: TDataSource
    DataSet = Query
    Left = 96
    Top = 104
  end
end
