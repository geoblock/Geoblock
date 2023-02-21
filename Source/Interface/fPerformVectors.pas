//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(* Show options for vectors *)

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
