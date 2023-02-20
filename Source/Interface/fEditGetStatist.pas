//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
{!  The form to show main statistics }

unit fEditGetStatist;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.Math,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.ComCtrls, 
  Vcl.Grids,
  Data.DB, 
  Bde.DBTables,

  
  fInitialDialog,
  dBase;

const
  ColumnCount = 6;

type
  TColumnRange = 0..ColumnCount;
  TColumnOrder = array[TColumnRange] of integer;
  PPointer     = ^Pointer;

type
  TfmEditGetStatist = class(TfmInitialDialog)
    ButtonSaveAs: TButton;
    Query:      TQuery;
    StringGrid: TStringGrid;
    HeaderControl: THeaderControl;
    StatusBar:  TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonSaveAsClick(Sender: TObject);
    procedure StringGridMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure HeaderControlSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FColumnOrder: TColumnOrder;
    FSelfPointer: PPointer;//pointer to the current copy of TfmGetDBTableInfo
    FonChange:    TNotifyEvent;
    FTableName:   TFileName;
    function GetTableName: TFileName;
    procedure SetTableName(const Value: TFileName);
    procedure Change;
    procedure SetSelfPointer(const Value: PPointer);
    procedure SaveStatistTable;
    function GetColumnOrder(Index: TColumnRange): integer;
    procedure SetColumnOrder(Index: TColumnRange; const Value: integer);
     
  public
     
    destructor Destroy; override;
    property TableName: TFileName Read GetTableName Write SetTableName;
    property onChange: TNotifyEvent Read FonChange Write FonChange;
    property SelfPointer: PPointer Read FSelfPointer Write SetSelfPointer;
    property CO[Index: TColumnRange]: integer Read GetColumnOrder Write SetColumnOrder;
  end;

function isFieldTypeNumber(Value: TFieldType): boolean;

var
  fmEditGetStatist: TfmEditGetStatist;

implementation

uses
  dDialogs,
  uCommon,
  cGlobals,
  cProfuns;

{$R *.dfm}

{ TfmEditGetStatist }

function isFieldTypeNumber(Value: TFieldType): boolean;
begin
  Result := Value in [ftAutoInc, ftLargeint, ftSmallint, ftInteger,
    ftWord, ftBCD, ftFloat, ftCurrency];
end;

function isFieldTypeDateTime(Value: TFieldType): boolean;
begin
  Result := Value in [ftDate, ftTime, ftDateTime];
end;

function isFieldTypeCounting(Value: TFieldType): boolean;
begin
  Result := Value in [ftString, ftBoolean, ftGraphic];
end;


procedure TfmEditGetStatist.FormCreate(Sender: TObject);
var
  I: integer;
begin
  inherited;
  for I := 0 to StringGrid.ColCount - 1 do
    CO[I] := I;
end;

procedure TfmEditGetStatist.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  inherited;
end;



function TfmEditGetStatist.GetColumnOrder(Index: TColumnRange): integer;
begin
  Result := FColumnOrder[Index];
end;

procedure TfmEditGetStatist.SetColumnOrder(Index: TColumnRange; const Value: integer);
begin
  FColumnOrder[Index] := Value;
end;

procedure TfmEditGetStatist.SetSelfPointer(const Value: PPointer);
begin
  SelfPointer^ := Self;
  FSelfPointer := Value;
end;

procedure TfmEditGetStatist.SetTableName(const Value: TFileName);
begin
  if Value <> FTableName then
  begin
    FTableName := Value;
    Caption    := Value;
    Change;
  end;
end;

procedure TfmEditGetStatist.ButtonOKClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmEditGetStatist.ButtonSaveAsClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    SaveDialog.InitialDir  := ExpandPath(DirReport);
    SaveDialog.DefaultExt  := TableExt;
    SaveDialog.Filter      := Format('*.%s |*.%0:s', [SaveDialog.DefaultExt]);
    SaveDialog.FilterIndex := 1;
    SaveDialog.FileName    := ExtractFileName(TableName);
    SaveDialog.HelpContext := HelpContext;
    if SaveDialog.Execute then
    begin
      SaveStatistTable;
    end;
  end;
end;

procedure TfmEditGetStatist.Change;
var
  I, K:  integer;
  Table: TTable;
  FN:    string; //FieldName
  Separator: char;
  OldCursor: TCursor;
  Value: double;

begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Table := TTable.Create(Self);
    try
      Table.TableName := TableName;
      Table.Open;
      Query.Sql.Clear;
      Query.Sql.Add('SELECT');
      Separator := ',';
      for I := 0 to Table.FieldCount - 1 do
      begin
        if I = Table.FieldCount - 1 then
          Separator := ' ';
        FN := Table.Fields[I].FieldName;
        if isFieldTypeNumber(Table.Fields[I].DataType) then
          Query.Sql.Add(
            'MIN(T."' + FN + '") C1_' + IntToStr(I + 1) + ',MAX(T."' +
            FN + '") C2_' + FloatToStr(I + 1) + ',AVG(T."' + FN +
            '") C3_' + FloatToStr(I + 1) + ',COUNT(T."' + FN + '") C4_' +
            IntToStr(I + 1) + ',SUM(T."' + FN + '") C5_' + IntToStr(I + 1) + Separator)
        else if isFieldTypeDateTime(Table.Fields[I].DataType) then
        begin
          Query.Sql.Add(
            'MIN(T."' + FN + '") C1_' + IntToStr(I + 1) + //Col:Row
            ',MAX(T."' + FN + '") C2_' + IntToStr(I + 1) + ',COUNT(T."' +
            FN + '") C4_' + IntToStr(I + 1) + Separator);
        end
        else if isFieldTypeCounting(Table.Fields[I].DataType) then
          Query.Sql.Add(
            'COUNT(T."' + FN + '") C1_' + FloatToStr(I + 1) + Separator)
        else
          Query.Sql.Add(' 8 C1_' + FloatToStr(I + 1) + Separator);
      end;
      Query.Sql.Add('FROM "' + Table.TableName + '" T');
      Query.Open;
      K := 0;
      StringGrid.RowCount := Table.FieldCount;
      with StringGrid do
        for I := 0 to Table.FieldCount - 1 do
        begin
          FN := Table.Fields[I].FieldName;
          Cells[0, I] := FN;
          if isFieldTypeNumber(Table.Fields[I].DataType) then
          begin
            Value := RoundTo(Query.Fields[K + 0].AsFloat, Precision);
            Cells[CO[1], I] := FloatToStr(Value);
            Value := RoundTo(Query.Fields[K + 1].AsFloat, Precision);
            Cells[CO[2], I] := FloatToStr(Value);
            Value := RoundTo(Query.Fields[K + 2].AsFloat, Precision);
            Cells[CO[3], I] := FloatToStr(Value);
            Cells[CO[4], I] := Query.Fields[K + 3].AsString;    //Integer
            Value := RoundTo(Query.Fields[K + 4].AsFloat, Precision);
            Cells[CO[5], I] := FloatToStr(Value);
            Inc(K, 4);
          end
          else if isFieldTypeDateTime(Table.Fields[I].DataType) then
          begin
            Cells[CO[1], I] := Query.Fields[K + 0].AsString;
            Cells[CO[2], I] := Query.Fields[K + 1].AsString;
            Cells[CO[3], I] := '';
            Cells[CO[4], I] := Query.Fields[K + 2].AsString;
            Cells[CO[5], I] := '';
            Inc(K, 2);
          end
          else if Table.Fields[I].DataType <> ftBytes then
          begin
            Cells[CO[1], I] := '';
            Cells[CO[2], I] := '';
            Cells[CO[3], I] := '';
            Cells[CO[4], I] := Query.Fields[K].AsString;
            Cells[CO[5], I] := '';
          end
          else
            Dec(K);
          Cells[CO[6], I] := FieldTypeToStr(Table.Fields[I].DataType);
          Inc(K);
        end;
    finally
      Table.Free;
    end;
    if Assigned(onChange) then
      onChange(Self);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfmEditGetStatist.SaveStatistTable;
var
  TableStatist: TTable;
begin
  TableStatist := TTable.Create(Self);
  with TableStatist do
    try
      TableName := dmDialogs.SaveDialog.FileName;
      FieldDefs.Clear;
      FieldDefs.Add(HeaderControl.Sections[0].Text, ftString, 20, False);
      FieldDefs.Add(HeaderControl.Sections[1].Text, ftFloat, 0, False);
      FieldDefs.Add(HeaderControl.Sections[2].Text, ftFloat, 0, False);
      FieldDefs.Add(HeaderControl.Sections[3].Text, ftFloat, 0, False);
      FieldDefs.Add(HeaderControl.Sections[4].Text, ftFloat, 0, False);
      FieldDefs.Add(HeaderControl.Sections[5].Text, ftFloat, 0, False);
      FieldDefs.Add(HeaderControl.Sections[6].Text, ftFloat, 0, False);
      //FieldDefs.Add(HeaderControl.Sections[7].Text {VARIANCE},ftFloat,0,False);
      CreateTable;
    finally
      Free;
    end;
end;

destructor TfmEditGetStatist.Destroy;
begin
  if SelfPointer <> nil then
    SelfPointer^ := nil;
  inherited;
end;

procedure TfmEditGetStatist.StringGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);

var
  Col, Row: longint;

begin
  StringGrid.MouseToCell(X, Y, Col, Row);
  try
    Hint := StringGrid.Cells[Col, Row];
  except
  end;
  StatusBar.SimpleText := Hint;
end;

procedure TfmEditGetStatist.HeaderControlSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
const
  D: array[boolean] of byte = (1, 3);
begin
  StringGrid.ColWidths[Section.Index] := Section.Width - D[Section.Index = 0];
end;

function TfmEditGetStatist.GetTableName: TFileName;
begin
  Result := FTableName;
end;

procedure TfmEditGetStatist.ButtonCancelClick(Sender: TObject);
begin
  inherited;
  Close;
end;

end.
