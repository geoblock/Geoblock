inherited fmEditLookupField: TfmEditLookupField
  Left = 379
  Top = 183
  HelpContext = 200
  Caption = 'Lookup field'
  ClientHeight = 364
  ClientWidth = 625
  OldCreateOrder = True
  OnCreate = FormCreate
  ExplicitWidth = 631
  ExplicitHeight = 393
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 625
    Height = 241
    ExplicitWidth = 625
    ExplicitHeight = 241
    object GroupBoxLookupField: TGroupBox
      Left = 1
      Top = 1
      Width = 209
      Height = 239
      Align = alLeft
      Caption = 'Lookup'
      TabOrder = 0
      object ListBoxLookup: TListBox
        Left = 2
        Top = 18
        Width = 205
        Height = 219
        Align = alClient
        TabOrder = 0
      end
    end
    object GroupBoxLinkField: TGroupBox
      Left = 210
      Top = 1
      Width = 219
      Height = 239
      Align = alClient
      Caption = 'Link'
      TabOrder = 1
      object ListBoxLink: TListBox
        Left = 2
        Top = 18
        Width = 215
        Height = 219
        Align = alClient
        TabOrder = 0
      end
    end
    object GroupBoxViewField: TGroupBox
      Left = 429
      Top = 1
      Width = 195
      Height = 239
      Align = alRight
      Caption = 'View'
      TabOrder = 2
      object ListBoxView: TListBox
        Left = 2
        Top = 18
        Width = 191
        Height = 219
        Align = alClient
        TabOrder = 0
      end
    end
  end
  inherited PanelMiddle: TPanel
    Top = 241
    Width = 625
    Height = 75
    ExplicitTop = 241
    ExplicitWidth = 625
    ExplicitHeight = 75
    object GroupBoxLookupTable: TGroupBox
      Left = 1
      Top = 1
      Width = 623
      Height = 64
      Align = alTop
      Caption = 'Lookup table'
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 369
        Top = 18
        Height = 44
        AutoSnap = False
        Beveled = True
        MinSize = 80
        ResizeStyle = rsUpdate
      end
      object PanelLeft: TPanel
        Left = 2
        Top = 18
        Width = 367
        Height = 44
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          367
          44)
        object PanelLookupPath: TPanel
          Left = 8
          Top = 9
          Width = 351
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
        Left = 372
        Top = 18
        Width = 249
        Height = 44
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          249
          44)
        object EditLookupName: TEdit
          Left = 8
          Top = 9
          Width = 188
          Height = 24
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
        end
        object Panel2: TPanel
          Left = 208
          Top = 0
          Width = 41
          Height = 44
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object SpeedButtonLookupName: TSpeedButton
            Left = 8
            Top = 10
            Width = 23
            Height = 22
            Hint = 'Browse'
            Caption = '...'
            ParentShowHint = False
            ShowHint = True
            OnClick = SpeedButtonLookupNameClick
          end
        end
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 316
    Width = 625
    ExplicitTop = 316
    ExplicitWidth = 625
    inherited ButtonOK: TButton
      Left = 295
      ExplicitLeft = 295
    end
    inherited ButtonCancel: TButton
      Left = 400
      ExplicitLeft = 400
    end
    inherited ButtonHelp: TButton
      Left = 506
      ExplicitLeft = 506
    end
  end
  object Table: TTable
    Left = 97
    Top = 73
  end
  object TableLookup: TTable
    StoreDefs = True
    TableName = 'default'
    Left = 264
    Top = 72
    object TableLookupee: TIntegerField
      FieldName = 'ee'
    end
  end
end
