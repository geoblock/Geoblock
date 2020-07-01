//------------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The dialog for adding field types }

unit fEditAddField;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Samples.Spin,

  
  fInitialDialog;

type
  TfmEditAddField = class(TfmInitialDialog)
    LabelName:      TLabel;
    EditFieldName:  TEdit;
    SpinEditSize:   TSpinEdit;
    LabelSize:      TLabel;
    RadioGroupType: TRadioGroup;
    procedure ButtonOKClick(Sender: TObject);
    procedure RadioGroupTypeClick(Sender: TObject);
  private
     
  public
     
    SQLString: string;
  end;

var
  fmEditAddField: TfmEditAddField;

implementation


{$R *.dfm}

(*
SQL Syntax  BDE Logical          Paradox          dBASE

SMALLINT  fldINT16          Short          Number (6,10)
INTEGER          fldINT32          Long Integer  Number (20,4)
DECIMAL(x,y)  fldBCD                  BCD          N/A
NUMERIC(x,y)  fldFLOAT          Number          Number (x,y)
FLOAT(x,y)  fldFLOAT          Number          Float (x,y)
CHARACTER(n)  fldZSTRING          Alpha          Character
VARCHAR(n)  fldZSTRING          Alpha          Character
DATE          fldDATE                  Date          Date
BOOLEAN          fldBOOL                  Logical          Logical
BLOB(n,1)  fldstMEMO          Memo          Memo
BLOB(n,2)  fldstBINARY          Binary          Binary
BLOB(n,3)  fldstFMTMEMO          Formatted memo  N/A
BLOB(n,4)  fldstOLEOBJ          OLE          OLE
BLOB(n,5)  fldstGRAPHIC          Graphic          N/A
TIME          fldTIME                  Time          N/A
TIMESTAMP  fldTIMESTAMP          Timestamp  N/A
MONEY          fldFLOAT, fldstMONEY  Money          Number (20,4)
AUTOINC          fldINT32, fldstAUTOINC  Autoincrement  N/A
BYTES(n)  fldBYTES(n)          Bytes          N/A
*)


procedure TfmEditAddField.ButtonOKClick(Sender: TObject);
begin
  EditFieldName.Text := '"' + EditFieldName.Text + '"';
  case RadioGroupType.ItemIndex of
    0: SQLString  := EditFieldName.Text + ' VARCHAR(' + SpinEditSize.Text + ')';
    1: SQLString  := EditFieldName.Text + ' AUTOINC';
    2: SQLString  := EditFieldName.Text + ' DECIMAL(' + SpinEditSize.Text + ',2)';
    3: SQLString  := EditFieldName.Text + ' BLOB(' + SpinEditSize.Text + ',2)';
    4: SQLString  := EditFieldName.Text + ' BYTES(' + SpinEditSize.Text + ')';
    5: SQLString  := EditFieldName.Text + ' DATE';
    6: SQLString  := EditFieldName.Text + ' BLOB(' + SpinEditSize.Text + ',3)';
    7: SQLString  := EditFieldName.Text + ' BLOB(' + SpinEditSize.Text + ',5)';
    8: SQLString  := EditFieldName.Text + ' BOOLEAN';
    9: SQLString  := EditFieldName.Text + ' INTEGER';
    10: SQLString := EditFieldName.Text + ' BLOB(' + SpinEditSize.Text + ',1)';
    11: SQLString := EditFieldName.Text + ' MONEY';
    12: SQLString := EditFieldName.Text + ' FLOAT(' + SpinEditSize.Text + ',2)';
    13: SQLString := EditFieldName.Text + ' BLOB(' + SpinEditSize.Text + ',4)';
    14: SQLString := EditFieldName.Text + ' SMALLINT';
    15: SQLString := EditFieldName.Text + ' TIME';
    16: SQLString := EditFieldName.Text + ' TIMESTAMP';
  end;
end;

procedure TfmEditAddField.RadioGroupTypeClick(Sender: TObject);
begin
  case RadioGroupType.ItemIndex of
    0: EditFieldName.Text  := RadioGroupType.Items[0];
    1: EditFieldName.Text  := RadioGroupType.Items[1];
    2: EditFieldName.Text  := RadioGroupType.Items[2];
    3: EditFieldName.Text  := RadioGroupType.Items[3];
    4: EditFieldName.Text  := RadioGroupType.Items[4];
    5: EditFieldName.Text  := RadioGroupType.Items[5];
    6: EditFieldName.Text  := RadioGroupType.Items[6];
    7: EditFieldName.Text  := RadioGroupType.Items[7];
    8: EditFieldName.Text  := RadioGroupType.Items[8];
    9: EditFieldName.Text  := RadioGroupType.Items[9];
    10: EditFieldName.Text := RadioGroupType.Items[10];
    11: EditFieldName.Text := RadioGroupType.Items[11];
    12: EditFieldName.Text := RadioGroupType.Items[12];
    13: EditFieldName.Text := RadioGroupType.Items[13];
    14: EditFieldName.Text := RadioGroupType.Items[14];
    15: EditFieldName.Text := RadioGroupType.Items[15];
    16: EditFieldName.Text := RadioGroupType.Items[16];
  end;
  EditFieldName.Text := UpperCase(EditFieldName.Text);
end;

end.
