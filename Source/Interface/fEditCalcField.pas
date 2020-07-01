//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The dialog to calculate values in a field }

unit fEditCalcField;

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
  Vcl.ExtCtrls,

  fInitialDialog;


type
  TfmEditCalcField = class(TfmInitialDialog)
    LabelResultField: TLabel;
    LabelFormula: TLabel;
    LabelAvailableFields: TLabel;
    lbAvailableFields: TListBox;
    bt8:      TButton;
    bt9:      TButton;
    bt7:      TButton;
    btPlus: TButton;
    bt4:      TButton;
    bt5:      TButton;
    bt6:      TButton;
    btMinus: TButton;
    bt1:      TButton;
    bt2:      TButton;
    bt3:      TButton;
    btMul:    TButton;
    bt0:      TButton;
    btPoint:  TButton;
    btSk:     TButton;
    btDiv:    TButton;
    btGt: TButton;
    btLt: TButton;
    btEq: TButton;
    btWhere: TButton;
    btNULL: TButton;
    pnEqual:  TPanel;
    EditFormula: TEdit;
    btClear: TButton;
    lbResultFields: TListBox;
    cbIncRecord: TCheckBox;
    ButtonAddField: TButton;
    procedure btClearClick(Sender: TObject);
    procedure lbAvailableFieldsKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure LabelFormulaMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ButtonSQLClick(Sender: TObject);
    procedure ButtonSKClick(Sender: TObject);
    procedure EditFormulaChange(Sender: TObject);
    procedure lbAvailableFieldsClick(Sender: TObject);
  private
    S : String;
  end;

var
  fmEditCalcField: TfmEditCalcField;

//========================================================================
implementation
//========================================================================

{$R *.DFM}

procedure TfmEditCalcField.btClearClick(Sender: TObject);
begin
  EditFormula.Text := '';
end;

procedure TfmEditCalcField.lbAvailableFieldsKeyDown(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    lbAvailableFields.OnDblClick(Sender);
end;

procedure TfmEditCalcField.LabelFormulaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  EditFormula.SetFocus;
end;

procedure TfmEditCalcField.lbAvailableFieldsClick(Sender: TObject);
var
  I: integer;
begin
  //if Mouse Double click on ItemIndex then add Item in EditFormula.Text
  S := ' ' + lbAvailableFields.Items[lbAvailableFields.ItemIndex] + ' ';
  for I := 1 to Length(S) do
    PostMessage(EditFormula.Handle, WM_CHAR, Ord(S[I]), 0);
end;

procedure TfmEditCalcField.ButtonSQLClick(Sender: TObject);
var
  I: integer;
begin
  S := ' ' + TButton(Sender).Caption + ' ';
  for I := 1 to Length(S) do
    PostMessage(EditFormula.Handle, WM_CHAR, Ord(S[I]), 0);
end;

procedure TfmEditCalcField.ButtonSKClick(Sender: TObject);
begin
  PostMessage(EditFormula.Handle, WM_CHAR,
    Ord(TButton(Sender).Caption[1]), 0);
  if Sender = btSk then
    PostMessage(EditFormula.Handle, WM_CHAR, Ord(')'), 0);
end;


procedure TfmEditCalcField.EditFormulaChange(Sender: TObject);
begin
  ButtonOK.Enabled := (fmEditCalcField.EditFormula.Text <> '');
end;

end.
