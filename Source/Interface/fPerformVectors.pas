//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{!  Show features of vectors

  History :
     08/02/2015 - PW - Removed unnecessary units from uses clause
     01/08/97 - Pavel Vassiliev - Created;
}

unit fPerformVectors;

interface

uses
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.Samples.Spin, 
  Vcl.ExtCtrls,

  
  fInitialDialog, System.Classes;

type
  TfmPerformVectors = class(TfmInitialDialog)
    LabelShowEvery:    TLabel;
    SpinEditDisplayEvery: TSpinEdit;
    SpinEditLengthFactor: TSpinEdit;
    LabelLengthFactor: TLabel;
  private
     
  public
     
  end;

var
  fmPerformVectors: TfmPerformVectors;

implementation

{$R *.DFM}

end.
