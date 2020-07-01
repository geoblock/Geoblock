inherited fmDrawSymbolStyle: TfmDrawSymbolStyle
  Left = 268
  Top = 219
  Caption = 'Symbol Style'
  ClientHeight = 309
  OnCreate = FormCreate
  ExplicitHeight = 338
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 9
    ExplicitHeight = 9
  end
  inherited PanelMiddle: TPanel
    Top = 9
    Height = 252
    ExplicitTop = 9
    ExplicitHeight = 252
    object GroupBoxOptions: TGroupBox
      Left = 1
      Top = 1
      Width = 565
      Height = 250
      Align = alClient
      Caption = 'Options'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 0
      object LabelFont: TLabel
        Left = 6
        Top = 39
        Width = 26
        Height = 16
        Caption = 'Font'
      end
      object LabelSymbol: TLabel
        Left = 6
        Top = 70
        Width = 46
        Height = 16
        Caption = 'Symbol'
      end
      object LabelColor: TLabel
        Left = 8
        Top = 189
        Width = 32
        Height = 16
        Caption = 'Color'
      end
      object LabelSize: TLabel
        Left = 326
        Top = 39
        Width = 26
        Height = 16
        Caption = 'Size'
      end
      object StringGridSymbols: TStringGrid
        Left = 70
        Top = 62
        Width = 237
        Height = 107
        ColCount = 10
        DefaultColWidth = 16
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 25
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
        ParentFont = False
        TabOrder = 1
        OnSelectCell = StringGridSymbolsSelectCell
        ColWidths = (
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16)
        RowHeights = (
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16
          16)
      end
      object ComboBoxFont: TComboBox
        Left = 70
        Top = 30
        Width = 235
        Height = 24
        Style = csDropDownList
        DropDownCount = 5
        TabOrder = 0
        OnChange = ComboBoxFontChange
      end
      object PanelColor: TPanel
        Left = 70
        Top = 183
        Width = 235
        Height = 24
        BevelOuter = bvLowered
        BorderWidth = 1
        BorderStyle = bsSingle
        TabOrder = 2
        TabStop = True
        OnEnter = PanelColorEnter
        object SpeedButtonColor: TSpeedButton
          Left = 209
          Top = 0
          Width = 21
          Height = 20
          Glyph.Data = {
            96000000424D9600000000000000760000002800000008000000080000000100
            0400000000002000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777770077777000077700000077777777777777777}
          OnClick = ImageColorClick
        end
        object ImageColor: TImage
          Left = 1
          Top = 1
          Width = 208
          Height = 17
          OnClick = ImageColorClick
        end
      end
      object GroupBoxExample: TGroupBox
        Left = 320
        Top = 64
        Width = 177
        Height = 145
        Caption = 'Example'
        TabOrder = 3
        object ImageExample: TImage
          Left = 2
          Top = 18
          Width = 173
          Height = 125
          Align = alClient
        end
      end
      object SpinEditSize: TSpinEdit
        Left = 424
        Top = 32
        Width = 65
        Height = 26
        Increment = 2
        MaxValue = 100
        MinValue = 2
        TabOrder = 4
        Value = 8
        OnChange = SpinEditSizeChange
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 261
    ExplicitTop = 261
  end
end
