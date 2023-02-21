//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The form to rename fields *)

unit fEditRenField;

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
  
  fInitialDialog ;

type
  TfmEditRenField = class(TfmInitialDialog)
    LabelFieldName:   TLabel;
    ListBoxFieldName: TListBox;
    EditNewName:      TEdit;
    LabelNewName:     TLabel;
    procedure EditNewNameChange(Sender: TObject);
    procedure ListBoxFieldNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
    function Execute: boolean;
  end;

var
  fmEditRenField: TfmEditRenField;

implementation

{$R *.DFM}

procedure TfmEditRenField.EditNewNameChange(Sender: TObject);
begin
  ButtonOK.Enabled := (EditNewName.Text <> '') and
    (ListBoxFieldName.Items.IndexOf(EditNewName.Text) < 0);
end;

procedure TfmEditRenField.ListBoxFieldNameClick(Sender: TObject);
begin
  try
    EditNewName.Text := ListBoxFieldName.Items[ListBoxFieldName.ItemIndex];
  except
  end;
end;

procedure TfmEditRenField.FormCreate(Sender: TObject);
begin
  inherited;
  EditNewNameChange(EditNewName);
end;

function TfmEditRenField.Execute: boolean;
begin
  Hide;
  Result := ShowModal = mrOk;
end;

end.
