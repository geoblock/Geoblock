inherited fmPageDialog: TfmPageDialog
  Caption = 'Page Dialog'
  Constraints.MinWidth = 320
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Height = 11
    BevelOuter = bvNone
    ExplicitHeight = 11
  end
  inherited PanelMiddle: TPanel
    Top = 11
    Height = 390
    BevelOuter = bvNone
    BorderWidth = 5
    ExplicitTop = 11
    ExplicitHeight = 390
    object PageControl: TPageControl
      Left = 5
      Top = 5
      Width = 557
      Height = 380
      Align = alClient
      MultiLine = True
      TabOrder = 0
    end
  end
  inherited PanelBottom: TPanel
    Top = 401
    Height = 47
    BevelOuter = bvNone
    ExplicitTop = 401
    ExplicitHeight = 47
    DesignSize = (
      567
      47)
  end
end
