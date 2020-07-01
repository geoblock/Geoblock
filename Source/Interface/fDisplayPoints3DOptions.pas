//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The display dialog for grid 2d models}


unit fDisplayPoints3DOptions;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.CheckLst, 
  Vcl.Buttons, 
  Vcl.Samples.Spin,

  
  fOptionDialog;

type
  TfmDisplayPoints3DOptions = class(TfmOptionDialog)
  private
     
  public
     
  end;

var
  fmDisplayPoints3DOptions: TfmDisplayPoints3DOptions;

implementation

{$R *.DFM}

end.
