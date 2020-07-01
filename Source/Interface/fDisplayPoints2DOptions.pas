//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{
  The dialog to display options for POINTS 2D models
}

unit fDisplayPoints2DOptions;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.CheckLst, 
  Vcl.ExtCtrls, 
  Vcl.Buttons, 
  Vcl.Samples.Spin,

  
  fOptionDialog;

type
  TfmDisplayPoints2DOptions = class(TfmOptionDialog)
  private
     
  public
     
  end;

var
  fmDisplayPoints2DOptions: TfmDisplayPoints2DOptions;

implementation

{$R *.DFM}

end.
