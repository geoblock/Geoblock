//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{
  The dialog to display options for SOLIDS models
}

unit fDisplaySolidOptions;

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
  TfmDisplaySolidsOptions = class(TfmOptionDialog)
    GroupBoxFeature:  TGroupBox;
    ButtonContours:   TButton;
    CheckBoxContours: TCheckBox;
    ButtonVectors:    TButton;
    CheckBoxVectors:  TCheckBox;
    procedure CheckBoxContoursClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmDisplaySolidsOptions: TfmDisplaySolidsOptions;

//===========================================================================
implementation
//===========================================================================

{$R *.DFM}

procedure TfmDisplaySolidsOptions.CheckBoxContoursClick(Sender: TObject);
begin
  CheckBoxContours.Checked := ButtonContours.Enabled;
  CheckBoxVectors.Checked  := ButtonVectors.Enabled;
end;

end.
