//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The dialog to put parameters of natural neighbore method
}

unit fInterNaturalNeighbours;

interface

uses
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.Samples.Spin, 
  Vcl.ExtCtrls,

  
  fInitialDialog;

type
  TfmInterNaturalNeighbours = class(TfmInitialDialog)
    RadioGroupNodeFunction: TRadioGroup;
    RadioGroupCoefficient: TRadioGroup;
    GroupBoxFrame: TGroupBox;
    LabelOverConvexHull: TLabel;
    SpinEdit1:     TSpinEdit;
    StaticTextPercent: TStaticText;
  private
     
  public
     
  end;

var
  fmInterNaturalNeighbours: TfmInterNaturalNeighbours;

implementation

{$R *.DFM}

end.
