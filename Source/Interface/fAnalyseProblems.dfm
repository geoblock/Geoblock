inherited fmAnalyseProblems: TfmAnalyseProblems
  Tag = 1
  Left = 258
  Top = 190
  HelpContext = 702
  Caption = 'Problem Book'
  ClientHeight = 363
  ClientWidth = 604
  Constraints.MinWidth = 0
  ExplicitWidth = 610
  ExplicitHeight = 392
  PixelsPerInch = 96
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 604
    ExplicitWidth = 604
  end
  inherited PanelMiddle: TPanel
    Width = 604
    Height = 305
    ExplicitWidth = 604
    ExplicitHeight = 305
    inherited PageControl: TPageControl
      Width = 594
      Height = 295
      ExplicitWidth = 594
      ExplicitHeight = 295
    end
  end
  inherited PanelBottom: TPanel
    Top = 316
    Width = 604
    ExplicitTop = 316
    ExplicitWidth = 604
    inherited ButtonOK: TButton
      Left = 249
      ModalResult = 0
      OnClick = ButtonOKClick
      ExplicitLeft = 249
    end
    inherited ButtonCancel: TButton
      Left = 369
      ExplicitLeft = 369
    end
    inherited ButtonHelp: TButton
      Left = 491
      ExplicitLeft = 491
    end
  end
end
