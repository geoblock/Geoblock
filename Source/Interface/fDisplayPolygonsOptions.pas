//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{
  The display dialog for POLYGON models
}


unit fDisplayPolygonsOptions;

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

  
  fOptionDialog,
  uModels;

type
  TfmDisplayPolygonsOptions = class(TfmOptionDialog)
  private
     
  public
  end;

var
  fmDisplayPolygonsOptions: TfmDisplayPolygonsOptions;

implementation

{$R *.DFM}

{ TfmDisplayPolygonsOptions }

end.
