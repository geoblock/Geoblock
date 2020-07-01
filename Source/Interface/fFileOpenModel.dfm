inherited fmFileOpenModel: TfmFileOpenModel
  Tag = 1
  Top = 181
  HelpContext = 111
  Caption = 'Open Model'
  ClientHeight = 401
  ClientWidth = 631
  ExplicitWidth = 647
  ExplicitHeight = 440
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 631
    ExplicitWidth = 631
  end
  inherited PanelMiddle: TPanel
    Width = 631
    Height = 349
    ExplicitWidth = 631
    ExplicitHeight = 349
    inherited SpeedButton1: TSpeedButton
      Left = 785
      ExplicitLeft = 762
    end
    inherited SpeedButton2: TSpeedButton
      Left = 827
      ExplicitLeft = 804
    end
    inherited PageControl: TPageControl
      Width = 621
      Height = 94
      ActivePage = TabSheetHoles
      ExplicitWidth = 621
      ExplicitHeight = 94
      object TabSheetHoles: TTabSheet
        Caption = 'Dholes'
        ImageIndex = 32
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetPoints2D: TTabSheet
        Tag = 1
        Caption = 'Points 2D'
        ImageIndex = 38
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetPoints3D: TTabSheet
        Tag = 2
        Caption = 'Points 3D'
        ImageIndex = 41
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetPolygons: TTabSheet
        Tag = 3
        Caption = 'Polygons'
        ImageIndex = 44
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetTins: TTabSheet
        Tag = 4
        Caption = 'Tins'
        ImageIndex = 48
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetSolids: TTabSheet
        Tag = 5
        Caption = 'Solids'
        ImageIndex = 66
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetGrids2D: TTabSheet
        Tag = 6
        Caption = 'Grid 2D'
        ImageIndex = 55
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetGrids3D: TTabSheet
        Tag = 7
        Caption = 'Grid 3D'
        ImageIndex = 60
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetMeshes2D: TTabSheet
        Tag = 8
        Caption = 'Meshes 2D'
        ImageIndex = 69
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
      object TabSheetMeshes3D: TTabSheet
        Tag = 9
        Caption = 'Mesh 3D'
        ImageIndex = 73
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
    inherited PanelDirectory: TPanel
      Top = 99
      Width = 621
      Height = 85
      ExplicitTop = 99
      ExplicitWidth = 621
      ExplicitHeight = 85
      inherited SpeedButtonBrowse: TSpeedButton
        Left = 485
        Top = 32
        Height = 27
        ExplicitLeft = 485
        ExplicitTop = 32
        ExplicitHeight = 27
      end
      inherited SpeedButtonDelete: TSpeedButton
        Left = 516
        Top = 30
        Height = 29
        ExplicitLeft = 516
        ExplicitTop = 30
        ExplicitHeight = 29
      end
      inherited LabelPath: TLabel
        Left = 6
        Top = 40
        ExplicitLeft = 6
        ExplicitTop = 40
      end
      inherited PanelInputPath: TPanel
        Left = 79
        Top = 35
        ExplicitLeft = 79
        ExplicitTop = 35
      end
    end
    inherited ListView: TListView
      Top = 184
      Width = 621
      Height = 106
      Columns = <
        item
          Caption = 'Name'
          Width = 320
        end
        item
          Caption = 'Size'
          Width = 100
        end
        item
          Caption = 'Changed'
          Width = 100
        end>
      ExplicitTop = 184
      ExplicitWidth = 621
      ExplicitHeight = 106
    end
    inherited GroupBoxOutput: TGroupBox
      Top = 290
      Width = 621
      ExplicitTop = 290
      ExplicitWidth = 621
      inherited ToolBarShowAs: TToolBar
        Width = 617
        ExplicitWidth = 617
        inherited ToolButtonGraph: TToolButton
          Caption = '`'
        end
        object ToolButton4: TToolButton
          Left = 213
          Top = 0
          Caption = '`'
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 350
    Width = 631
    ExplicitTop = 350
    ExplicitWidth = 631
    inherited ButtonOK: TButton
      Left = 304
      Top = 6
      ExplicitLeft = 304
      ExplicitTop = 6
    end
    inherited ButtonCancel: TButton
      Left = 416
      Top = 6
      Height = 32
      ExplicitLeft = 416
      ExplicitTop = 6
      ExplicitHeight = 32
    end
    inherited ButtonHelp: TButton
      Left = 532
      Top = 6
      Height = 33
      ExplicitLeft = 532
      ExplicitTop = 6
      ExplicitHeight = 33
    end
  end
end
