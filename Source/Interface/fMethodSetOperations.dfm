inherited fmMethodSetOperations: TfmMethodSetOperations
  Left = 206
  Top = 119
  HelpContext = 318
  Caption = 'Set Operations'
  ClientHeight = 477
  ClientWidth = 761
  ExplicitWidth = 789
  ExplicitHeight = 534
  TextHeight = 16
  inherited PanelTop: TPanel
    Width = 761
    ExplicitWidth = 761
    inherited GroupBoxInput: TGroupBox
      Width = 372
      ExplicitWidth = 372
      inherited PanelInputPath: TPanel
        Width = 368
        ExplicitWidth = 368
      end
      inherited PanelInputButtons: TPanel
        Width = 368
        ExplicitWidth = 368
        inherited ToolBarRight: TToolBar
          Left = 319
          Width = 44
          AutoSize = True
          ExplicitLeft = 319
          ExplicitWidth = 44
        end
      end
      inherited GroupBoxRealAttribute: TGroupBox
        Width = 182
        ExplicitWidth = 182
        inherited ListBoxRealAttribute: TListBox
          Width = 178
          ExtendedSelect = False
          MultiSelect = False
          ExplicitWidth = 178
        end
      end
    end
    inherited GroupBoxInputB: TGroupBox
      Left = 387
      Width = 369
      ExplicitLeft = 387
      ExplicitWidth = 369
      inherited PanelInputPathB: TPanel
        Width = 365
        ExplicitWidth = 365
      end
      inherited PanelInputButtonsB: TPanel
        Width = 365
        ExplicitWidth = 365
        inherited ToolBarInputB: TToolBar
          Width = 251
          ButtonHeight = 23
          ExplicitWidth = 251
          inherited ToolButton16: TToolButton
            Width = 13
            ExplicitWidth = 13
          end
        end
        inherited ToolBarBRight: TToolBar
          Left = 314
          Width = 46
          AutoSize = True
          ExplicitLeft = 314
          ExplicitWidth = 46
        end
      end
      inherited GroupBoxRealAttributeB: TGroupBox
        Width = 176
        ExplicitWidth = 176
        inherited ListBoxRealAttributeB: TListBox
          Width = 172
          ExtendedSelect = False
          MultiSelect = False
          ExplicitWidth = 172
        end
      end
    end
  end
  inherited PanelMiddle: TPanel
    Width = 761
    Height = 207
    ExplicitWidth = 761
    ExplicitHeight = 207
    inherited GroupBoxOutput: TGroupBox
      Top = 146
      Width = 751
      ExplicitTop = 146
      ExplicitWidth = 751
      inherited ToolBarShowAs: TToolBar
        Width = 747
        ExplicitWidth = 747
        inherited PanelOutPath: TPanel
          Width = 408
          ExplicitWidth = 408
        end
        inherited EditOutName: TEdit
          Left = 497
          ExplicitLeft = 497
        end
        inherited SpeedButtonOutputBrowse: TSpeedButton
          Left = 698
          ExplicitLeft = 698
        end
      end
    end
    object GroupBoxOperation: TGroupBox
      Left = 5
      Top = 5
      Width = 751
      Height = 141
      Align = alClient
      Caption = 'Operation'
      TabOrder = 1
      object RadioGroupOperation: TRadioGroup
        Left = 304
        Top = 16
        Width = 137
        Height = 111
        Hint = 'Operation'
        ItemIndex = 2
        Items.Strings = (
          'Union'
          'Difference'
          'Intersection')
        TabOrder = 0
        OnClick = RadioGroupOperationClick
      end
    end
  end
  inherited PanelBottom: TPanel
    Top = 425
    Width = 761
    ExplicitTop = 425
    ExplicitWidth = 761
    inherited ProgressBar: TProgressBar
      Width = 352
      ExplicitWidth = 352
    end
    inherited ButtonOK: TButton
      Left = 428
      ExplicitLeft = 428
    end
    inherited ButtonCancel: TButton
      Left = 537
      ExplicitLeft = 537
    end
    inherited ButtonHelp: TButton
      Left = 651
      ExplicitLeft = 651
    end
  end
end
