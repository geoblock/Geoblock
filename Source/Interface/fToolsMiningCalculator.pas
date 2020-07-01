//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The Mining Calculator  }

unit fToolsMiningCalculator;

interface

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  
  fPageDialog,
  GBEditValue;

type
  TfmToolsMiningCalculator = class(TfmPageDialog)
    TabSheetCutoffGrade: TTabSheet;
    PanelCutoffGrade: TPanel;
    LabelOreCosts: TLabel;
    LabelWasteCosts: TLabel;
    LabelRecovery: TLabel;
    LabelDilution: TLabel;
    LabelStrippingCoefficient: TLabel;
    LabelProductPrice: TLabel;
    LabelCutoffGrade: TLabel;
    TabSheetStrippingRatio: TTabSheet;
    Panel5: TPanel;
    Edit1:  TEdit;
    Edit2:  TEdit;
    Edit3:  TEdit;
    Edit4:  TEdit;
    Edit5:  TEdit;
    Edit6:  TEdit;
    Edit7:  TEdit;
    GBEditValue1: TGBEditValue;
    GBEditValueOreRecovery: TGBEditValue;
    GBEditValueStripRatio: TGBEditValue;
    GBEditValue4: TGBEditValue;
    GBEditValueOreDilution: TGBEditValue;
    GBEditValueProductPrice: TGBEditValue;
    GBEditValueCutoffGrade: TGBEditValue;
    GBEditValueStrippingRatio: TGBEditValue;
  private
     
  public
     
  end;

var
  fmToolsMiningCalculator: TfmToolsMiningCalculator;

implementation

{$R *.dfm}

end.
