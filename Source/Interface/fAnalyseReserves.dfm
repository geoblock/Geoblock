inherited fmAnalyseReserves: TfmAnalyseReserves
  Tag = 1
  Left = 119
  Top = 128
  HelpContext = 713
  Caption = 'Reserve Calculation'
  ClientHeight = 423
  ClientWidth = 787
  ExplicitWidth = 815
  ExplicitHeight = 480
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 787
    ExplicitWidth = 787
    inherited GroupBoxInput: TGroupBox
      Width = 777
      ExplicitWidth = 777
      inherited PanelInputPath: TPanel
        Width = 773
        Hint = '|'
        ExplicitWidth = 773
      end
      inherited PanelInputButtons: TPanel
        Width = 773
        ExplicitWidth = 773
        inherited ToolBarInput: TToolBar
          Width = 364
          AutoSize = False
          ExplicitWidth = 364
          inherited ToolButtonHoles: TToolButton
            Visible = False
          end
          inherited ToolButtonPoints2D: TToolButton
            Visible = False
          end
          inherited ToolButtonPoints3D: TToolButton
            Visible = False
          end
          inherited ToolButtonPolygons: TToolButton
            DropdownMenu = DropdownMenuPoly
          end
          inherited ToolButtonSolids: TToolButton
            Visible = False
          end
          inherited ToolButtonMeshes2D: TToolButton
            Visible = False
          end
          inherited ToolButtonMeshes3D: TToolButton
            Visible = False
          end
        end
        inherited ToolBarRight: TToolBar
          Left = 713
          ExplicitLeft = 713
        end
      end
      inherited GroupBoxRealAttribute: TGroupBox
        Left = 369
        Width = 406
        ExplicitLeft = 369
        ExplicitWidth = 406
        inherited ListBoxRealAttribute: TListBox
          Width = 402
          Enabled = False
          ExtendedSelect = False
          MultiSelect = False
          ExplicitWidth = 402
        end
      end
      inherited GroupBoxModel: TGroupBox
        Width = 367
        ExplicitWidth = 367
        inherited ListBoxInputNames: TListBox
          Width = 363
          ExplicitWidth = 363
        end
      end
    end
  end
  inherited PanelMiddle: TPanel
    Width = 787
    Height = 130
    ExplicitWidth = 787
    ExplicitHeight = 130
    inherited GroupBoxOutput: TGroupBox
      Top = 71
      Width = 777
      ExplicitTop = 71
      ExplicitWidth = 777
      inherited ToolBarShowAs: TToolBar
        Width = 773
        ExplicitWidth = 773
        inherited ToolButtonMap: TToolButton
          Enabled = False
        end
        inherited ToolButtonTable: TToolButton
          Down = True
        end
        inherited PanelOutPath: TPanel
          Width = 424
          ExplicitWidth = 424
        end
        inherited EditOutName: TEdit
          Left = 513
          Width = 199
          ExplicitLeft = 513
          ExplicitWidth = 199
        end
        inherited SpeedButtonOutputBrowse: TSpeedButton
          Left = 712
          ExplicitLeft = 712
        end
      end
    end
    object RadioGroupDensity: TRadioGroup
      Left = 5
      Top = 5
      Width = 372
      Height = 66
      Align = alLeft
      Caption = 'Density'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Constant'
        'Value')
      TabOrder = 1
    end
    object RadioGroupMoisture: TRadioGroup
      Left = 377
      Top = 5
      Width = 405
      Height = 66
      Align = alClient
      Caption = 'Moisture'
      Columns = 2
      ItemIndex = 0
      Items.Strings = (
        'Constant'
        'Value')
      TabOrder = 2
    end
    object GBEditValueDensity: TGBEditValue
      Left = 112
      Top = 33
      Width = 52
      Height = 24
      TabOrder = 3
      Text = '0'
      Parent = PanelMiddle
      EnabledConvert = True
      AsInteger = 0
      ValueType = vtDouble
      Error = False
      Precision = 0
    end
    object GBEditValueMoisture: TGBEditValue
      Left = 480
      Top = 33
      Width = 52
      Height = 24
      TabOrder = 4
      Text = '0'
      Parent = PanelMiddle
      EnabledConvert = True
      AsInteger = 0
      ValueType = vtDouble
      Error = False
      Precision = 0
    end
  end
  inherited PanelBottom: TPanel
    Top = 377
    Width = 787
    ExplicitTop = 377
    ExplicitWidth = 787
    inherited ProgressBar: TProgressBar
      Width = 280
      ExplicitWidth = 280
    end
    inherited ButtonOK: TButton
      Left = 473
      ExplicitLeft = 473
    end
    inherited ButtonCancel: TButton
      Left = 583
      ExplicitLeft = 583
    end
    inherited ButtonHelp: TButton
      Left = 687
      ExplicitLeft = 687
    end
  end
  object DropdownMenuPoly: TPopupMenu
    Left = 64
    Top = 114
    object MenuItemParallelProfiles: TMenuItem
      Caption = 'Parallel Profiles'
      GroupIndex = 1
      Hint = 
        'Parallel profiles|Reserve calculation using method of parallel s' +
        'ections'
      ImageIndex = 85
      OnClick = MenuItemParallelProfilesClick
    end
    object MenuItemVoronoiPolygons: TMenuItem
      Tag = 1
      Caption = 'Voronoi Polygons'
      GroupIndex = 1
      Hint = 
        'Boldyrev regions|Reserve calculation using method of Voronoi pol' +
        'ygons'
      ImageIndex = 72
      OnClick = MenuItemVoronoiPolygonsClick
    end
  end
  object PopupMenu: TPopupMenu
    Left = 246
    Top = 114
    object MenuItemHoles: TMenuItem
      Caption = 'Holes'
      Hint = 'Weighted arithmetics'
      ImageIndex = 32
    end
    object MenuItemPoints2D: TMenuItem
      Caption = 'Points 2D'
      Hint = 'Average arithmetics'
      ImageIndex = 38
    end
    object MenuItemPoints3D: TMenuItem
      Caption = 'Points 3D'
      Hint = 'Average arithmetics'
      ImageIndex = 41
    end
    object MenuItemPolygons: TMenuItem
      Caption = 'Polygons'
      Hint = 
        'Parallel profiles|Reserve calculation using method of parallel s' +
        'ections'
      ImageIndex = 44
    end
    object MenuItemTins: TMenuItem
      Caption = 'Tins'
      Hint = 'Triangle prizms'
      ImageIndex = 48
    end
    object MenuItemSolids: TMenuItem
      Caption = 'Solids'
      Hint = 'Solids'
      ImageIndex = 66
    end
    object MenuItemGrids2D: TMenuItem
      Caption = 'Grids 2D'
      Hint = 'Sobolevskiy grid'
      ImageIndex = 55
    end
    object MenuItemGrids3D: TMenuItem
      Caption = 'Grids 3D'
      Hint = 'Regular blocks'
      ImageIndex = 60
    end
    object MenuItemMeshes2D: TMenuItem
      Caption = 'Meshes 2D'
      Hint = 'Polygonal prizms'
      ImageIndex = 69
    end
    object MenuItemMeshes3D: TMenuItem
      Caption = 'Meshes 3D'
      Hint = 'Tetrahedron cells'
      ImageIndex = 73
    end
  end
end
