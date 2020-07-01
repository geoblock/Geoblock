inherited fmMethodDualDialog: TfmMethodDualDialog
  Left = 195
  Top = 122
  ClientHeight = 479
  ClientWidth = 782
  ExplicitWidth = 798
  ExplicitHeight = 518
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 782
    Height = 218
    ExplicitWidth = 782
    ExplicitHeight = 218
    inherited GroupBoxInput: TGroupBox
      Width = 389
      Height = 208
      Align = alLeft
      Caption = 'Input A'
      ExplicitWidth = 389
      ExplicitHeight = 208
      inherited PanelInputPath: TPanel
        Width = 385
        ExplicitWidth = 385
      end
      inherited PanelInputButtons: TPanel
        Width = 385
        ExplicitWidth = 385
        inherited ToolBarInput: TToolBar
          Width = 246
          HotImages = ImageListInput
          ExplicitWidth = 246
          inherited ToolButton1: TToolButton
            Caption = ''
          end
          inherited ToolButton2: TToolButton
            Width = 8
            Caption = ''
            ExplicitWidth = 8
          end
        end
        inherited ToolBarRight: TToolBar
          Left = 320
          Width = 60
          ExplicitLeft = 320
          ExplicitWidth = 60
          DesignSize = (
            60
            30)
        end
      end
      inherited GroupBoxRealAttribute: TGroupBox
        Left = 188
        Width = 199
        Height = 124
        ExplicitLeft = 188
        ExplicitWidth = 199
        ExplicitHeight = 124
        inherited ListBoxRealAttribute: TListBox
          Width = 195
          Height = 104
          ExplicitWidth = 195
          ExplicitHeight = 104
        end
      end
      inherited GroupBoxModel: TGroupBox
        Width = 186
        Height = 124
        Caption = 'Model A'
        ExplicitWidth = 186
        ExplicitHeight = 124
        inherited ListBoxInputNames: TListBox
          Width = 182
          Height = 104
          ExplicitWidth = 182
          ExplicitHeight = 104
        end
      end
    end
    object GroupBoxInputB: TGroupBox
      Left = 393
      Top = 5
      Width = 384
      Height = 208
      Align = alRight
      Caption = 'Input B'
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
      object PanelInputPathB: TPanel
        Left = 2
        Top = 58
        Width = 380
        Height = 25
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvLowered
        BorderWidth = 5
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object PanelInputButtonsB: TPanel
        Left = 2
        Top = 18
        Width = 380
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 5
        TabOrder = 1
        object ToolBarInputB: TToolBar
          Left = 5
          Top = 5
          Width = 249
          Height = 30
          Align = alLeft
          AutoSize = True
          Caption = 'Input model B'
          EdgeInner = esNone
          EdgeOuter = esNone
          Images = ImageListInput
          TabOrder = 0
          object ToolButton5: TToolButton
            Left = 0
            Top = 0
            Width = 8
            ImageIndex = 76
            Style = tbsSeparator
          end
          object ToolButtonHolesB: TToolButton
            Left = 8
            Top = 0
            Hint = 'Drillholes'
            Grouped = True
            ImageIndex = 0
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonPoints2DB: TToolButton
            Tag = 1
            Left = 31
            Top = 0
            Hint = 'Points 2D'
            Grouped = True
            ImageIndex = 1
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonPoints3DB: TToolButton
            Tag = 2
            Left = 54
            Top = 0
            Hint = 'Points 3D'
            Grouped = True
            ImageIndex = 2
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonPolygonsB: TToolButton
            Tag = 3
            Left = 77
            Top = 0
            Hint = 'Polygons'
            Grouped = True
            ImageIndex = 3
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonTinsB: TToolButton
            Tag = 4
            Left = 100
            Top = 0
            Hint = 'Tin'
            Grouped = True
            ImageIndex = 4
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonSolidsB: TToolButton
            Tag = 5
            Left = 123
            Top = 0
            Hint = 'Solids'
            Grouped = True
            ImageIndex = 5
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonGrids2DB: TToolButton
            Tag = 6
            Left = 146
            Top = 0
            Hint = 'Grid 2D'
            Grouped = True
            ImageIndex = 6
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonGrids3DB: TToolButton
            Tag = 7
            Left = 169
            Top = 0
            Hint = 'Grid 3D'
            Caption = '``'
            Grouped = True
            ImageIndex = 7
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonMeshes2DB: TToolButton
            Tag = 8
            Left = 192
            Top = 0
            Hint = 'Mesh 2D'
            Grouped = True
            ImageIndex = 8
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButtonMeshes3DB: TToolButton
            Tag = 9
            Left = 215
            Top = 0
            Hint = 'Mesh 3D'
            Grouped = True
            ImageIndex = 9
            ParentShowHint = False
            ShowHint = True
            Style = tbsCheck
            OnClick = ToolButtonInputBClick
          end
          object ToolButton16: TToolButton
            Left = 238
            Top = 0
            Width = 11
            ImageIndex = 76
            Style = tbsSeparator
          end
        end
        object ToolBarBRight: TToolBar
          Left = 319
          Top = 5
          Width = 56
          Height = 30
          Align = alRight
          EdgeInner = esNone
          EdgeOuter = esNone
          TabOrder = 1
          DesignSize = (
            56
            30)
          object SpeedButtonInputInfoB: TSpeedButton
            Left = 0
            Top = 0
            Width = 23
            Height = 22
            Hint = 'Statistics'
            Anchors = [akTop, akRight]
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              7777777770000777777777770666E0777777777706608F07777777777E660077
              7777777770E687777777777777E6607777777777700E6877777777770F806607
              7777777770E6660777777777770E607777777777777007777777777777770077
              777777777770FF07777777777770000777777777777777777777}
            ParentShowHint = False
            ShowHint = True
            OnClick = SpeedButtonInputBInfoClick
          end
          object SpeedButtonInputBrowseB: TSpeedButton
            Left = 23
            Top = 0
            Width = 23
            Height = 22
            Hint = 'Browse|Browse the directory'
            Anchors = [akTop, akRight]
            Caption = '...'
            Margin = 5
            ParentShowHint = False
            ShowHint = True
            OnClick = SpeedButtonInputBrowseBClick
          end
        end
      end
      object GroupBoxModelB: TGroupBox
        Left = 2
        Top = 83
        Width = 189
        Height = 123
        Align = alLeft
        Caption = 'Model B'
        TabOrder = 2
        object ListBoxInputNamesB: TListBox
          Left = 2
          Top = 18
          Width = 185
          Height = 103
          Align = alClient
          ExtendedSelect = False
          TabOrder = 0
          OnClick = ListBoxInputNamesBClick
          OnKeyUp = ListBoxInputNamesKeyUp
        end
      end
      object GroupBoxRealAttributeB: TGroupBox
        Left = 191
        Top = 83
        Width = 191
        Height = 123
        Align = alClient
        Caption = 'Attribute'
        TabOrder = 3
        object ListBoxRealAttributeB: TListBox
          Left = 2
          Top = 18
          Width = 187
          Height = 103
          Align = alClient
          MultiSelect = True
          TabOrder = 0
          OnClick = ListBoxRealAttributeBClick
        end
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 218
    Width = 782
    Height = 209
    ExplicitTop = 218
    ExplicitWidth = 782
    ExplicitHeight = 209
    inherited GroupBoxOutput: TGroupBox
      Top = 148
      Width = 772
      Height = 56
      ExplicitTop = 148
      ExplicitWidth = 772
      ExplicitHeight = 56
      inherited ToolBarShowAs: TToolBar
        Width = 768
        ExplicitWidth = 768
        DesignSize = (
          768
          22)
        inherited PanelOutPath: TPanel
          Width = 432
          ExplicitWidth = 432
        end
        inherited EditOutName: TEdit
          Left = 509
          Width = 201
          ExplicitLeft = 509
          ExplicitWidth = 201
        end
        inherited SpeedButtonOutputBrowse: TSpeedButton
          Left = 710
          ExplicitLeft = 710
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 427
    Width = 782
    Height = 52
    ExplicitTop = 427
    ExplicitWidth = 782
    ExplicitHeight = 52
    DesignSize = (
      782
      52)
    inherited ProgressBar: TProgressBar
      Top = 18
      Width = 288
      ExplicitTop = 18
      ExplicitWidth = 288
    end
    inherited ButtonOK: TButton
      Left = 449
      Top = 12
      ExplicitLeft = 449
      ExplicitTop = 12
    end
    inherited ButtonCancel: TButton
      Left = 558
      Top = 12
      ExplicitLeft = 558
      ExplicitTop = 12
    end
    inherited ButtonHelp: TButton
      Left = 672
      Top = 12
      ExplicitLeft = 672
      ExplicitTop = 12
    end
  end
end
