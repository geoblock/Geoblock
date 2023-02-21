//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
(* The dialog to display options of Mesh 3D models *)

unit fDisplayMesh3DOptions;

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


  fOptionDialog,
  fPerformIsosurfaces;


type
  TfmDisplayMesh3DOptions = class(TfmOptionDialog)
    GroupBoxFeature: TGroupBox;
    CheckBoxIsosurface: TCheckBox;
    ButtonVectors: TButton;
    CheckBoxVectors: TCheckBox;
    ButtonIsosurface: TButton;
    procedure CheckBoxIsosurfaceClick(Sender: TObject);
    procedure CheckBoxVectorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonIsosurfaceClick(Sender: TObject);
  private
     
  public
     
  end;

var
  fmDisplayMesh3DOptions: TfmDisplayMesh3DOptions;

//==========================================================================
implementation
//==========================================================================

{$R *.DFM}

procedure TfmDisplayMesh3DOptions.ButtonIsosurfaceClick(Sender: TObject);
begin
  fmPerformIsosurfaces := TfmPerformIsosurfaces.Create(Self);
  if fmPerformIsosurfaces.ShowModal = mrOk then
  begin
    //Calculate Isosurface
  end;
  fmPerformIsosurfaces.Free;
end;

procedure TfmDisplayMesh3DOptions.CheckBoxIsosurfaceClick(Sender: TObject);
begin
  ButtonIsosurface.Enabled := CheckBoxIsosurface.Checked;
end;

procedure TfmDisplayMesh3DOptions.CheckBoxVectorsClick(Sender: TObject);
begin
  ButtonVectors.Enabled := CheckBoxVectors.Checked;
end;

procedure TfmDisplayMesh3DOptions.FormCreate(Sender: TObject);
begin
  inherited;
  CheckBoxVectors.Checked := ButtonVectors.Enabled;
  CheckBoxIsosurface.Checked := ButtonIsosurface.Enabled;
end;

end.
