inherited fmEditQuery: TfmEditQuery
  Left = 229
  Top = 183
  HelpContext = 210
  Caption = 'Select by Query'
  ClientHeight = 356
  ClientWidth = 546
  OldCreateOrder = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  ExplicitWidth = 552
  ExplicitHeight = 385
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 546
    Height = 33
    ExplicitWidth = 546
    ExplicitHeight = 33
    object ToolBar: TToolBar
      Left = 1
      Top = 1
      Width = 544
      Height = 29
      TabOrder = 0
      ExplicitTop = -1
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object ToolButtonOpen: TToolButton
        Left = 8
        Top = 0
        Hint = 'Open'
        ImageIndex = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = ToolButtonOpenClick
      end
      object ToolButtonSave: TToolButton
        Left = 31
        Top = 0
        Hint = 'Save'
        ImageIndex = 6
        ParentShowHint = False
        ShowHint = True
        OnClick = ToolButtonSaveClick
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 33
    Width = 546
    Height = 275
    ExplicitTop = 33
    ExplicitWidth = 546
    ExplicitHeight = 275
    object GroupBoxQuery: TGroupBox
      Left = 1
      Top = 1
      Width = 544
      Height = 209
      Align = alClient
      Caption = 'Query'
      TabOrder = 0
      object MemoSql: TMemo
        Left = 2
        Top = 18
        Width = 540
        Height = 189
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -18
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'SELECT [DISTINCT] * | column_list'
          'FROM table_reference'
          '[WHERE predicates]'
          '[GROUP BY group_list]'
          '[ORDER BY order_list]'
          '[HAVING having_condition]')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        OnChange = MemoSqlChange
      end
    end
    object GroupBoxOutput: TGroupBox
      Left = 1
      Top = 210
      Width = 544
      Height = 64
      Align = alBottom
      Caption = 'Output'
      TabOrder = 1
      object Splitter1: TSplitter
        Left = 489
        Top = 18
        Width = 2
        Height = 44
        AutoSnap = False
        Beveled = True
        MinSize = 50
        ResizeStyle = rsUpdate
      end
      object PanelLeft: TPanel
        Left = 2
        Top = 18
        Width = 487
        Height = 44
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          487
          44)
        object PanelOutputPath: TPanel
          Left = 9
          Top = 9
          Width = 456
          Height = 24
          Alignment = taLeftJustify
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelOuter = bvLowered
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
      end
      object PanelRight: TPanel
        Left = 491
        Top = 18
        Width = 51
        Height = 44
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          51
          44)
        object EditOutputName: TEdit
          Left = 24
          Top = 9
          Width = 0
          Height = 24
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          OnChange = EditOutputNameChange
          OnKeyUp = EditOutputNameKeyUp
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 308
    Width = 546
    ExplicitTop = 308
    ExplicitWidth = 546
    inherited ButtonOK: TButton
      Left = 552
      OnClick = ButtonOKClick
      ExplicitLeft = 552
    end
    inherited ButtonCancel: TButton
      Left = 672
      ExplicitLeft = 672
    end
    inherited ButtonHelp: TButton
      Left = 794
      ExplicitLeft = 794
    end
  end
end
