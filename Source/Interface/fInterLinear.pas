 //------------------------------------------------------------------------------
 // The modeling system Geoblock http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------
(* The dialog to put parameters of linear interpolation by TIN method *)

unit fInterLinear;

interface

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
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
  TfmInterLinear = class(TfmInitialDialog)
    GroupBoxAnisotropy: TGroupBox;
    GroupBox1:    TGroupBox;
    LabelRatio1:  TLabel;
    LabelRatio2:  TLabel;
    SpinEdit1:    TSpinEdit;
    SpinEdit2:    TSpinEdit;
    GroupBox2:    TGroupBox;
    LabelAzimuth: TLabel;
    LabelDip:     TLabel;
    LabelStrike:  TLabel;
    EditAzimuth:  TEdit;
    EditDip:      TEdit;
    EditStrike:   TEdit;
  private
     
  public
     
  end;

var
  fmInterLinear: TfmInterLinear;

implementation

uses
  uLinearByTIN;

{$R *.DFM}

end.
