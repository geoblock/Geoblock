//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(*  The dialog to display TIN models *)

unit fDisplayTinOptions;

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

  GLS.Isolines,

  fOptionDialog,
  fPerformContours,
  uModels;

type
  TfmDisplayTinOptions = class(TfmOptionDialog)
    GroupBoxFeature:    TGroupBox;
    ButtonContours:     TButton;
    CheckBoxContours:   TCheckBox;
    ButtonVectors:      TButton;
    CheckBoxVectors:    TCheckBox;
    CheckBoxByVertices: TCheckBox;
    procedure CheckBoxByVerticesClick(Sender: TObject);
    procedure CheckBoxContoursClick(Sender: TObject);
    procedure CheckBoxVectorsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonContoursClick(Sender: TObject);
  private
    procedure AssignFromTIN(Source: TGBTin);
    procedure AssignToTIN(Dest: TGBTin);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;


var
  fmDisplayTinOptions: TfmDisplayTinOptions;

//=========================================================================
implementation
//=========================================================================

uses
  fMapWindow;

{$R *.DFM}

procedure TfmDisplayTinOptions.FormCreate(Sender: TObject);
begin
  inherited;
  ButtonContours.Enabled := CheckBoxContours.Checked;
  ButtonVectors.Enabled  := CheckBoxVectors.Checked;
end;

procedure TfmDisplayTinOptions.Assign(Source: TPersistent);
begin
  if Source is TGBTin then
    AssignFromTIN(TGBTin(Source))
  else
    inherited;
end;

procedure TfmDisplayTinOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TGBTin then
    AssignToTin(TGBTin(Dest))
  else
    inherited;
end;

procedure TfmDisplayTinOptions.AssignToTIN(Dest: TGBTin);
begin
  Dest.ShowByVertices := CheckBoxByVertices.Checked;
  Dest.ShowContours   := CheckBoxContours.Checked;
  Dest.ShowVectors    := CheckBoxVectors.Checked;
  AssignToModel(Dest);
end;

procedure TfmDisplayTinOptions.ButtonContoursClick(Sender: TObject);
begin
  with TfmPerformContours do
  try

  finally

  end;

end;

procedure TfmDisplayTinOptions.AssignFromTIN(Source: TGBTin);
begin
  AssignFromModel(Source);
  CheckBoxByVertices.Checked := Source.ShowByVertices;
  CheckBoxContours.Checked   := Source.ShowContours;
  CheckBoxVectors.Checked    := Source.ShowVectors;
end;

procedure TfmDisplayTinOptions.CheckBoxContoursClick(Sender: TObject);
begin
  ButtonContours.Enabled := CheckBoxContours.Checked;
end;

procedure TfmDisplayTinOptions.CheckBoxVectorsClick(Sender: TObject);
begin
  ButtonVectors.Enabled := CheckBoxVectors.Checked;
end;

procedure TfmDisplayTinOptions.CheckBoxByVerticesClick(Sender: TObject);
begin
  AssignTo(fmMapWindow.Model);
  fmMapWindow.Repaint;
end;


end.
