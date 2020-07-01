//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{!  The dialog to display DHoles options

  History :
     21/03/99 - Pavel Vassiliev - Inserted the DrawOnChange ListBox;
     01/08/97 - Pavel Vassiliev - Creation;
}


unit fDisplayHolesOptions;

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
  TfmDisplayHolesOptions = class(TfmOptionDialog)
    CheckBoxDrawTextOnChange: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxDrawTextOnChangeClick(Sender: TObject);
    procedure CheckBoxTextAttributesClick(Sender: TObject);
    procedure RadioGroupDetailsClick(Sender: TObject);
  private
    procedure AssignFromHoles(Source: TGBHoles);
    procedure AssignToHoles(Dest: TGBHoles);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

var
  fmDisplayHolesOptions: TfmDisplayHolesOptions;

implementation

uses 
  fMapWindow;

{$R *.DFM}

{ TfmDisplayHolesOptions }

procedure TfmDisplayHolesOptions.Assign(Source: TPersistent);
begin
  if Source is TGBHoles then
    AssignFromHoles(TGBHoles(Source))
  else
    inherited;
end;

procedure TfmDisplayHolesOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TGBHoles then
    AssignToHoles(TGBHoles(Dest))
  else
    inherited;
end;

procedure TfmDisplayHolesOptions.AssignFromHoles(Source: TGBHoles);
begin
  AssignFromModel(Source);
  CheckBoxDrawTextOnChange.Checked := Source.DrawTextOnChange;
end;

procedure TfmDisplayHolesOptions.AssignToHoles(Dest: TGBHoles);
begin
  Dest.DrawTextOnChange := CheckBoxDrawTextOnChange.Checked;
  AssignToModel(Dest);
end;

procedure TfmDisplayHolesOptions.FormCreate(Sender: TObject);
begin
  inherited;
  CheckBoxDrawTextOnChange.Enabled := CheckBoxTextAttributes.Checked;
end;

procedure TfmDisplayHolesOptions.CheckBoxTextAttributesClick(Sender: TObject);
begin
  inherited;
  CheckBoxDrawTextOnChange.Enabled := CheckBoxTextAttributes.Checked;
  ListBoxTextAttributesClick(Sender);
end;

procedure TfmDisplayHolesOptions.CheckBoxDrawTextOnChangeClick(Sender: TObject);
begin
  AssignTo(fmMapWindow.Model);
  fmMapWindow.Repaint;
end;


procedure TfmDisplayHolesOptions.RadioGroupDetailsClick(Sender: TObject);
begin
  inherited;
  if RadioGroupDetails.ItemIndex = 0 then
    CheckBoxDrawTextOnChange.Enabled := False
  else
    CheckBoxDrawTextOnChange.Enabled := True;
end;

end.
