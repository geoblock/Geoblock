inherited fmFileImport: TfmFileImport
  Left = 326
  Top = 265
  Caption = 'Import'
  ClientHeight = 170
  ClientWidth = 639
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 645
  ExplicitHeight = 199
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 639
    Height = 57
    ExplicitWidth = 639
    ExplicitHeight = 57
    object GroupBoxInput: TGroupBox
      Left = 1
      Top = 1
      Width = 637
      Height = 55
      Align = alClient
      Caption = 'Input'
      TabOrder = 0
      object PanelInFile: TPanel
        Left = 2
        Top = 18
        Width = 633
        Height = 23
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvLowered
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 57
    Width = 639
    Height = 65
    ExplicitTop = 57
    ExplicitWidth = 639
    ExplicitHeight = 65
    object GroupBoxOutput: TGroupBox
      Left = 1
      Top = 1
      Width = 637
      Height = 63
      Align = alClient
      Caption = 'Output'
      TabOrder = 0
      object ToolBarShowAs: TToolBar
        Left = 2
        Top = 18
        Width = 633
        Height = 22
        AutoSize = True
        Caption = 'Show as'
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
        DesignSize = (
          633
          22)
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
          Hint = 'Graph|Show the result as Vcl.Vcl.CheckLst,'
          Caption = 'Graph'
          Grouped = True
          ImageIndex = 140
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ToolButtonShowAsClick
        end
        object ToolButton4: TToolButton
          Left = 77
          Top = 0
          Width = 12
          Caption = 'ToolButton4'
          ImageIndex = 145
          Style = tbsSeparator
        end
        object PanelOutputPath: TPanel
          Left = 89
          Top = 0
          Width = 344
          Height = 22
          Align = alLeft
          Alignment = taLeftJustify
          BevelOuter = bvLowered
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object EditOutputName: TEdit
          Left = 433
          Top = 0
          Width = 160
          Height = 22
          Anchors = [akTop, akBottom]
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnChange = EditOutputNameChange
        end
        object SpeedButtonOutputBrowse: TSpeedButton
          Left = 593
          Top = 0
          Width = 23
          Height = 22
          Hint = 'Browse'
          Anchors = [akTop, akRight]
          Caption = '...'
          ParentShowHint = False
          ShowHint = True
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 122
    Width = 639
    ExplicitTop = 122
    ExplicitWidth = 639
    inherited ButtonOK: TButton
      Left = 278
      OnClick = ButtonOKClick
      ExplicitLeft = 278
    end
    inherited ButtonCancel: TButton
      Left = 398
      ExplicitLeft = 398
    end
    inherited ButtonHelp: TButton
      Left = 520
      OnClick = nil
      ExplicitLeft = 520
    end
    object ProgressBar: TProgressBar
      Left = 9
      Top = 19
      Width = 240
      Height = 19
      TabOrder = 3
    end
  end
end
