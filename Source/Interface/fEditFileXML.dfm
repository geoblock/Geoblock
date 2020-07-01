inherited fmEditFileXML: TfmEditFileXML
  Left = 362
  Top = 228
  Caption = 'Editor of XML'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 17
    ExplicitHeight = 17
  end
  inherited PanelMiddle: TPanel
    Top = 17
    Height = 383
    ExplicitTop = 17
    ExplicitHeight = 383
    object DBGrid: TDBGrid
      Left = 1
      Top = 1
      Width = 565
      Height = 381
      Align = alClient
      DataSource = DataSource
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -13
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object Button: TButton
      Left = 376
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Button'
      TabOrder = 1
      OnClick = ButtonClick
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 64
    Top = 104
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 168
    Top = 104
  end
end
