inherited fmToolsUnitsConverter: TfmToolsUnitsConverter
  Tag = 1
  Left = 356
  Top = 187
  HelpContext = 815
  Caption = 'Units Converter'
  ClientHeight = 428
  ClientWidth = 597
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  ExplicitWidth = 603
  ExplicitHeight = 457
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 597
    Height = 9
    ExplicitWidth = 597
    ExplicitHeight = 9
  end
  inherited PanelMiddle: TPanel
    Top = 9
    Width = 597
    Height = 372
    ExplicitTop = 9
    ExplicitWidth = 597
    ExplicitHeight = 372
    inherited PageControl: TPageControl
      Width = 587
      Height = 362
      ActivePage = TabSheetTemperature
      OnChange = PageControlChange
      ExplicitWidth = 587
      ExplicitHeight = 362
      object TabSheetAngle: TTabSheet
        Caption = 'Angle'
        ImageIndex = 2
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetArea: TTabSheet
        Tag = 1
        Caption = 'Area'
        ImageIndex = 5
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetDensity: TTabSheet
        Tag = 2
        Caption = 'Density'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetEnergy: TTabSheet
        Tag = 3
        Caption = 'Energy'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetFlow: TTabSheet
        Tag = 4
        Caption = 'Flow'
        ImageIndex = 10
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetForce: TTabSheet
        Tag = 5
        Caption = 'Force'
        ImageIndex = 6
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetLength: TTabSheet
        Tag = 6
        Caption = 'Length'
        ImageIndex = 9
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetMass: TTabSheet
        Tag = 7
        Caption = 'Mass'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetTemperature: TTabSheet
        Tag = 8
        Caption = 'Temperature'
        ImageIndex = 8
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetTime: TTabSheet
        Tag = 9
        Caption = 'Time'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetVolume: TTabSheet
        Tag = 10
        Caption = 'Volume'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
    object DBGridUnits: TDBGrid
      Left = 48
      Top = 72
      Width = 465
      Height = 240
      DataSource = DataSourceUnits
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnKeyPress = DBGridUnitsKeyPress
    end
  end
  inherited PanelBottom: TPanel
    Top = 381
    Width = 597
    ExplicitTop = 381
    ExplicitWidth = 597
    inherited ButtonOK: TButton
      Align = alCustom
    end
    inherited ButtonCancel: TButton
      Align = alCustom
    end
    inherited ButtonHelp: TButton
      Align = alCustom
    end
  end
  object TableUnits: TTable
    AfterPost = TableUnitsAfterPost
    StoreDefs = True
    TableName = 'Units'
    Left = 313
    Top = 150
  end
  object DataSourceUnits: TDataSource
    DataSet = TableUnits
    Left = 177
    Top = 150
  end
end
