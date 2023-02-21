//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The form for image registration *)

unit fFileImageRegistration;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.ExtCtrls, 
  Vcl.Grids, 
  Vcl.DBGrids, 
  Vcl.StdCtrls, 
  Vcl.Buttons,
  Data.DB,
  
  fInitialDialog;

type
  TfmFileImageRegistration = class(TfmInitialDialog)
    DBGrid1: TDBGrid;
    Image:   TImage;
    SpeedButtonPlus: TSpeedButton;
    SpeedButtonMinus: TSpeedButton;
  private
     
  public
     
  end;

var
  fmFileImageRegistration: TfmFileImageRegistration;

implementation

{$R *.DFM}

end.
