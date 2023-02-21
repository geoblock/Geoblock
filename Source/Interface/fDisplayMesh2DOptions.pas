//------------------------------------------------------------------------------
//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
//------------------------------------------------------------------------------
(* The dialog to display options for MESH 2D models *)

unit fDisplayMesh2DOptions;

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
  Vcl.CheckLst, 
  Vcl.ExtCtrls, 
  Vcl.Buttons, 
  Vcl.Samples.Spin,

  fOptionDialog;


type
  TfmDisplayMesh2DOptions = class(TfmOptionDialog)
    GroupBoxFeature:  TGroupBox;
    ButtonContours:   TButton;
    CheckBoxContours: TCheckBox;
    ButtonVectors:    TButton;
    CheckBoxVectors:  TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonVectorsClick(Sender: TObject);
    procedure ButtonContoursClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmDisplayMesh2DOptions: TfmDisplayMesh2DOptions;

//==========================================================================
implementation
//==========================================================================

{$R *.DFM}

procedure TfmDisplayMesh2DOptions.FormCreate(Sender: TObject);
begin
  inherited;
  ButtonContours.Enabled := CheckBoxContours.Checked;
  ButtonVectors.Enabled  := CheckBoxVectors.Checked;
end;

procedure TfmDisplayMesh2DOptions.ButtonContoursClick(Sender: TObject);
begin
  ButtonContours.Enabled := CheckBoxContours.Checked;
end;

procedure TfmDisplayMesh2DOptions.ButtonVectorsClick(Sender: TObject);
begin
  ButtonVectors.Enabled := CheckBoxVectors.Checked;
end;

end.
