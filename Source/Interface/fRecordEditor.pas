//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! The record editor }

unit fRecordEditor;

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
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.ComCtrls,
  //DB
  Data.DB,
  Bde.DBTables,

  
  fInitialDialog;

type
  TfmRecordEditor = class(TfmInitialDialog)
    HeaderControl: THeaderControl;
    StringGrid: TStringGrid;
    LabelModel: TLabel;
    StaticTextModel: TStaticText;
    TableEdit: TTable;
    StaticTextModelIndex: TStaticText;
    procedure ButtonOKClick(Sender: TObject);
    procedure StringGridGetEditText(Sender: TObject; ACol, ARow: integer;
      var Value: string);
  private
     
  public
     
    procedure UpdateStringGrid;
  end;

var
  fmRecordEditor: TfmRecordEditor;

implementation

uses
  uCommon,
  cGlobals;

{$R *.DFM}

procedure TfmRecordEditor.UpdateStringGrid;
var
  I: integer;
begin
  StringGrid.RowCount := TableEdit.FieldCount;
  for I := 0 to TableEdit.FieldCount - 1 do
  begin
    StringGrid.Cells[0, I] := TableEdit.Fields[I].FieldName;
    if TableEdit.Fields[I].IsNull then
      StringGrid.Cells[1, I] := ''
    else
      StringGrid.Cells[1, I] := TableEdit.Fields[I].AsVariant;
  end;
end;

procedure TfmRecordEditor.StringGridGetEditText(Sender: TObject;
  ACol, ARow: integer; var Value: string);
begin
  Value := AnsiLowerCase(Value);
end;

procedure TfmRecordEditor.ButtonOKClick(Sender: TObject);
var
  I: integer;
  S: string;
begin
  try
    TableEdit.Edit;
    for I := 0 to TableEdit.FieldCount - 1 do //not StringGrid.RowCount-1!
    begin
      S := StringGrid.Cells[1, I];
      S := Trim(S);
      if (S = '') then
        TableEdit.Fields[I].Clear
      else
        TableEdit.Fields[I].AsVariant := S;
    end;
  finally
    TableEdit.Post;
  end;
end;

end.
