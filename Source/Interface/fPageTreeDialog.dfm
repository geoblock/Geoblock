inherited fmPageTreeDialog: TfmPageTreeDialog
  Caption = 'Page Tree Dialog'
  ClientHeight = 419
  ClientWidth = 610
  ExplicitWidth = 616
  ExplicitHeight = 448
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 610
    Height = 13
    ExplicitWidth = 610
    ExplicitHeight = 13
  end
  inherited PanelMiddle: TPanel
    Top = 13
    Width = 610
    Height = 359
    ExplicitTop = 13
    ExplicitWidth = 610
    ExplicitHeight = 359
    inherited PageControl: TPageControl
      Left = 200
      Width = 405
      Height = 349
      ExplicitLeft = 200
      ExplicitWidth = 405
      ExplicitHeight = 349
    end
    object TreeView: TTreeView
      Left = 5
      Top = 5
      Width = 195
      Height = 349
      Align = alLeft
      Indent = 19
      TabOrder = 1
    end
  end
  inherited PanelBottom: TPanel
    Top = 372
    Width = 610
    ExplicitTop = 372
    ExplicitWidth = 610
    DesignSize = (
      610
      47)
    inherited ButtonOK: TButton
      Left = 502
      ExplicitLeft = 502
    end
    inherited ButtonCancel: TButton
      Left = 622
      ExplicitLeft = 622
    end
    inherited ButtonHelp: TButton
      Left = 744
      ExplicitLeft = 744
    end
  end
end
