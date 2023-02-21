//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
(*
   The Dialog to display 3D Grid Options
*)

unit fDisplayGrid3DOptions;

interface

uses
  System.SysUtils, 
  System.Classes,
  Vcl.CheckLst,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.Samples.Spin,

  fOptionDialog,
  fPerformIsosurfaces;

type
  TfmDisplayGrid3DOptions = class(TfmOptionDialog)
    CheckBoxByVertices: TCheckBox;
    GroupBoxFeature: TGroupBox;
    CheckBoxIsosurface: TCheckBox;
    ButtonVectors: TButton;
    CheckBoxVectors: TCheckBox;
    ButtonIsosurface: TButton;
    procedure CheckBoxIsosurfaceClick(Sender: TObject);
    procedure CheckBoxVectorsClick(Sender: TObject);
    procedure ButtonIsosurfaceClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmDisplayGrid3DOptions: TfmDisplayGrid3DOptions;

//==========================================================================
implementation
//==========================================================================

{$R *.DFM}

procedure TfmDisplayGrid3DOptions.ButtonIsosurfaceClick(Sender: TObject);
begin
  fmPerformIsosurfaces := TfmPerformIsosurfaces.Create(Self);
  if fmPerformIsosurfaces.ShowModal = mrOk then
  begin
    //Calculate Isosurface
  end;
  fmPerformIsosurfaces.Free;
end;

procedure TfmDisplayGrid3DOptions.CheckBoxIsosurfaceClick(Sender: TObject);
begin
  ButtonIsosurface.Enabled := CheckBoxIsosurface.Checked;
end;

procedure TfmDisplayGrid3DOptions.CheckBoxVectorsClick(Sender: TObject);
begin
  ButtonVectors.Enabled := CheckBoxVectors.Checked;
end;

end.
