//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------

unit uCommon;

interface

uses
  Winapi.Windows,
  System.Win.Registry,
  System.Math,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,

  //BDE
  Data.DB, 
  Bde.DBTables,

  // Localization
  GnuGetText,

  cInterpol;   // for data types

// Initialization of general registry}
procedure InitGeneralRegistry;
procedure InitCursors;
// Initialization of interface languages}
procedure InitLanguage;
function IndexOf(Str: string; Items: TStrings): integer;

procedure WriteFramework(FileName: TFileName; X0, Y0, Z0: Double;
  DX, DY, DZ: Double; NX, NY, NZ: Integer);
function ReadFramework(ATableName: TFileName; var X0, Y0, Z0: Double;
  var DX, DY, DZ: Double; var NX, NY, NZ: Integer): Boolean;
function ReadParFile(ATableName: TFileName; var XO, YO, ZO: Double;
  var DX, DY, DZ: Double; var NX, NY, NZ: Integer): Boolean;
procedure WriteParFile(ATableName: TFileName; XO, YO, ZO: Double;
  DX, DY, DZ: Double; NX, NY, NZ: Integer);

function CheckFieldName(const s: string): boolean;
function IsNumericAttribute(Field: TField): boolean;
function IsRealAttribute(Field: TField): boolean;
function IsScalarAttribute(Field: TField; Mode3D: boolean = True): boolean;
function IsIntegerAttribute(Field: TField): boolean;
function IsAttributeField(Field: TField): boolean;

procedure AddTableField(TableName: TFileName; FieldName: string;
  FieldType: TFieldType; Size: integer = 0);

function FieldExists(Table: TTable; FieldName: string): boolean;

procedure AddAllTableFields(SourceTableName, DestTableName: TFileName; Mode3d: boolean);

procedure GetFieldValues(const TableName: TFileName; const FieldName: string;
  Proc: TGetStrProc; Sort: boolean = False);

function IsEqualStructure(TableName1, TableName2: TFileName): boolean;
function FieldTypeToStr(Value: TFieldType): string;
function FieldTypeToSQLTypeStr(FieldType: TFieldType; Size: integer = 0): string;
function FieldTypeParadoxToMIF(Field: TField): string;
function CopyScalarFieldsStructure(const Source: TTable; var Dest: TTable): boolean;

function TransposeNumericTable(OldTableName: TFileName;
  NewTableName: TFileName = ''): boolean;
function SetExtention(FileName: TFileName; NewExtention: string): TFileName;

procedure CheckItemIndex(ListBox: TListBox);
function GetCurrentItem(ListBox: TListBox): string; overload;

function MinMaxXYZ(ATableName: TFileName; var Xmin, Ymin, Zmin, Xmax, Ymax, Zmax: Double;
  ProgressBar: TProgressBar = nil): boolean;
procedure GetMinMaxXYZ(TableName: TFileName; var Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: double); overload;
procedure GetMinMaxXYZ(Points: TCoordinateArray; var Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: double); overload;

function GetRowCount(FileName: TFileName): integer;
function GetEmptyBitmap(AHeight, AWidth: integer): TBitmap;

procedure DrawPolygon(Dest: TCanvas; Poly: array of TPoint;
  BrushColor, BkGround: TColor; BrushStyle: TBrushStyle; Bitmap: TBitMap; cPen: TPen);

function ReadPolygon(TablePolyFace, TablePolyVert: TTable;
  out Xp, Yp, Zp: array of single; var VertexCount: integer): boolean;

function AverageColor(Colors: array of TColor): TColor;

//==========================================================================\\
implementation
//==========================================================================\\

uses
  cGlobals,
  cProfuns,
  cResStrings;

procedure InitGeneralRegistry;
var
  RegIni:      TRegistryIniFile;
  FileVersion: cardinal;

begin
  GeneralSection := RegGeoblock + 'General';
  FileVersion := GetFileVersion(ParamStr(0));
  ExePath := ExtractFilePath(ParamStr(0));
  AppPath := ExePath;
  System.Delete(AppPath, Pos('BIN', Uppercase(AppPath)), 4); //Delete 'Bin\' Subdir
  DataPath := AppPath + DirData;
  DataAssetsPath := AppPath + DirDataAssets;
  DataBasePath := AppPath + DirDataBase; //Default DataBase directory
  RegIni := TRegistryIniFile.Create(GeneralSection);
  try
    with RegIni do
    begin
      if not RegIni.SectionExists(GeneralSection) then
      begin
        WriteString(GeneralSection, 'AppPath', AppPath);  //Don't translate the strings
        WriteString(GeneralSection, 'DataBasePath', DataBasePath);
        WriteInteger(GeneralSection, 'FileVersion', FileVersion);
        WriteString(GeneralSection, 'Licensee', 'Getos Ltd.');
        WriteInteger(GeneralSection, 'LangID', LANG_ENGLISH);
      end
      else
      begin
        AppPath  := ReadString(GeneralSection, 'AppPath', AppPath);
        DataBasePath := ReadString(GeneralSection, 'DataBasePath', DataBasePath);
        LangID := ReadInteger(GeneralSection, 'LangID', LANG_RUSSIAN);
      end;
    end;
  finally
    RegIni.Free;
  end;
end; //InitGeneralRegistry

procedure InitLanguage;
begin
  if LangID <> LANG_ENGLISH then
  begin
    Textdomain('geoblock');
    BindTextDomain ('geoblock', AppPath + 'Locale'+ PathDelim);
    AddDomainForResourceString('delphi');
    BindTextDomain ('delphi', AppPath + 'Locale'+ PathDelim);
    AddDomainForResourceString('language');
    BindTextDomain ('language', AppPath + 'Locale'+ PathDelim);
    TP_GlobalIgnoreClass(TTable);
    TP_GlobalIgnoreClass(TFields);
    TP_GlobalIgnoreClass(TFont);
    //TP_GlobalIgnoreClass(TStaticText);
    //TP_GlobalIgnoreClass(TGLLibMaterial);
    //TP_GlobalIgnoreClass(TGLMaterialLibrary);
    //TP_GlobalIgnoreClass(TListBox);
    TP_GlobalIgnoreClassProperty(TAction, 'Category');
    // Removing the upper line will cause long loading but Action.Category translation
  end;
  case LangID of
    LANG_ENGLISH:
    begin
      UseLanguage('en');
      Application.HelpFile := UpperCase(AppPath +'Help'+PathDelim+'en'+ PathDelim+'Geoblock.chm');
    end;
    LANG_SPANISH:
    begin
      UseLanguage('es');
      Application.HelpFile := UpperCase(AppPath + 'Help'+PathDelim+'es'+ PathDelim + 'Geoblock.chm');
    end;
    LANG_RUSSIAN:
    begin
      UseLanguage('ru');
      Application.HelpFile := UpperCase(AppPath  +'Help'+PathDelim+'ru'+ PathDelim+'Geoblock.chm');
    end
    else   //DEFAULT LANGUAGE
    begin
      UseLanguage('en');
      Application.HelpFile := UpperCase(AppPath + 'Help' + PathDelim +'en' + PathDelim + 'Geoblock.chm');
    end;
  end;
  //LoadNewResourceModule(Language);//when using ITE, ENU for English USA
end;

procedure InitCursors;
begin
{.$R Gb.res} // include to rc as .cur files
  Screen.Cursors[crPenCursor]    := LoadCursor(HInstance, 'PEN');
  Screen.Cursors[crPanCursor]    := LoadCursor(HInstance, 'PAN');
  Screen.Cursors[crScrollCursor] := LoadCursor(HInstance, 'SCROLL');
  Screen.Cursors[crZoomCursor]   := LoadCursor(HInstance, 'ZOOM');
  Screen.Cursors[crCrossCursor]  := LoadCursor(HInstance, 'CROSS');
end;

function SetExtention(FileName: TFileName; NewExtention: string): TFileName;
var
  Pos: integer;
begin
  Pos := LastDelimiter('.', FileName);
  if Pos <> 0 then
    SetLength(FileName, Pos - 1);
  if NewExtention[1] <> '.' then
    NewExtention := '.' + NewExtention;
  Result := FileName + NewExtention;
end;

 //========================================================================\\
 {Field Routines}

procedure GetFieldValues(const TableName: TFileName; const FieldName: string;
  Proc: TGetStrProc; Sort: boolean = False);
var
  Query: TQuery;
  I:     integer;
begin
  Query := TQuery.Create(Application);
  try
    Query.SQL.Add('SELECT DISTINCT D."' + FieldName + '"');
    Query.SQL.Add('FROM "' + TableName + '" D');
    if Sort then
      Query.SQL.Add('ORDER BY D."' + FieldName + '"');
    Query.Open;
    for I := 1 to Query.RecordCount do
    begin
      Proc(Query.Fields[0].AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TransposeNumericTable
  (OldTableName: TFileName; NewTableName: TFileName = ''): boolean;
var
  Table: TTable;
  I, J:  integer;
  MaxLen, Len: integer;
begin
  Result := False;
  if NewTableName = '' then
    NewTableName := OldTableName;
  with TQuery.Create(nil) do
    try
      Sql.Add('SELECT * FROM "' + OldTableName + '"');
      Open;
      Table := TTable.Create(nil);
      try
        Table.TableName := NewTableName;
        Table.FieldDefs.Clear;
        MaxLen := Length(Fields[0].FieldName);
        for I := 1 to FieldCount - 1 do
        begin
          Len := Length(Fields[I].FieldName);
          if Len > MaxLen then
            MaxLen := Len;
        end;
        Table.FieldDefs.Add(Fields[0].FieldName, ftString, MaxLen + 1, False);
        for I := 1 to RecordCount do
        begin
          RecNo := I;
          Table.FieldDefs.Add(Fields[0].AsString, ftFloat, 0, False);
        end;
        Table.CreateTable;
        Table.Open;
        for I := 1 to FieldCount - 1 do
        begin
          Table.Append;
          Table.Fields[0].AsString := Fields[I].FieldName;
          Table.Post;
        end;
        for I := 1 to FieldCount - 1 do
        begin
          Table.RecNo := I;
          Table.Edit;
          for J := 1 to RecordCount do
          begin
            RecNo := J;
            Table.Fields[J].AsString := Fields[I].AsString;
          end;
          Table.Post;
        end;
      finally
        Table.Free;
      end;
    finally
      Free;
    end;
  if not Result then
    Result := True;
end;

procedure AddTableField(TableName: TFileName; FieldName: string;
  FieldType: TFieldType; Size: integer = 0);
var
  Table: TTable;
  Query: TQuery;
begin
  Table := TTable.Create(Application);
  try
    Query := TQuery.Create(Application);
    try
      Table.TableName := TableName;
      Table.Open;
      if Table.FindField(FieldName) <> nil then
      begin
        Table.Close;
        Query.SQL.Add('ALTER TABLE "' + TableName + '" DROP "' +
          TableName + '"."' + FieldName + '"');
        Query.ExecSQL;
      end;
      Table.Close;
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('ALTER TABLE "' + TableName + '" ADD "' +
        TableName + '"."' + FieldName + '"' + FieldTypeToSQLTypeStr(FieldType, Size));
      Query.ExecSQL;
    finally
      Query.Free;
    end;
  finally
    Table.Free;
  end;
end;

function IsAttributeField(Field: TField): boolean;
var
  fn: string;
begin
  fn     := Field.FieldName;
  Result := (UpperCase(fn) <> UpperCase(fldID)) and
    (UpperCase(fn) <> UpperCase(fldX)) and (UpperCase(fn) <> UpperCase(fldY)) and
    (UpperCase(LeftStr(fn, 3)) <> 'ID_');
end;

function FieldExists(Table: TTable; FieldName: string): boolean;
var
  I: integer;
begin
  i := 0;
  FieldName := UpperCase(FieldName);
  while (i < Table.FieldCount) and (UpperCase(Table.Fields[i].FieldName) <> FieldName) do
    Inc(i);
  Result := i < Table.FieldCount;
end;

procedure AddAllTableFields(SourceTableName, DestTableName: TFileName; Mode3d: boolean);
var
  dest, src: TTable;
  query: TQuery;
  i, n: integer;
  fld: TField;
  field_included: boolean;
  buf: array of string;
begin
  SourceTableName := SourceTableName + TableExt;
  DestTableName := DestTableName + TableExt;
  dest := TTable.Create(Application);
  src  := TTable.Create(Application);
  src.TableName := SourceTableName;
  src.Open;
  dest.TableName := DestTableName;
  dest.Open;
  SetLength(buf, 100);
  n := 0;

  // construct array of query strings to add new fields in destination table,
  // added fields shouldn't be coordinates or identifiers
  for i := 0 to src.FieldCount - 1 do
  begin
    fld := src.Fields[i];
    field_included := IsAttributeField(fld) and
      (not Mode3d or (UpperCase(fld.FieldName) <> UpperCase(fldZ)));
    if field_included and not FieldExists(dest, fld.FieldName) then
    begin
      buf[n] := '"' + fld.FieldName + '" ' +
        FieldTypeToSQLTypeStr(fld.DataType, fld.Size);
      Inc(n);
    end;
  end;
  src.Close;
  dest.Close;
  dest.Free;
  src.Free;

  // execute query incrementally
  query := TQuery.Create(Application);
  for i := 0 to n - 1 do
    with query do
    begin
      SQL.Clear;
      SQL.Add('ALTER TABLE "' + DestTableName + '"');
      SQL.Add('ADD "' + DestTableName + '".' + buf[i]);
      ExecSQL;
    end;
  query.Free;
  SetLength(buf, 0);
end;

 //-----------------------------------------------------\\
 // Compares structures of two tables                   \\
 //-----------------------------------------------------\\
function isEqualStructure(TableName1, TableName2: TFileName): boolean;
var
  Table1, Table2: TTable;
  I: integer;
begin
  Result := False;
  Table1 := TTable.Create(Application);
  try
    Table2 := TTable.Create(Application);
    try
      Table1.TableName := TableName1;
      Table1.Open;
      Table2.TableName := TableName2;
      Table2.Open;
      if Table1.FieldCount = Table2.FieldCount then
      begin
        Result := True;
        I      := 0;
        while (I < Table1.FieldCount) and Result do
        begin
          Result := (Table1.Fields[I].FieldName =
            Table2.Fields[I].FieldName) and
            (Table1.Fields[I].DataType = Table2.Fields[I].DataType) and
            (Table1.Fields[I].DataSize = Table2.Fields[I].DataSize);
          Inc(I);
        end;
      end;
    finally
      Table2.Free;
    end;
  finally
    Table1.Free;
  end;
end;

function FieldTypeToStr(Value: TFieldType): string;
begin
  case Value of
    ftUnknown: Result  := LoadResString(@rsUnknown);
    ftString: Result   := LoadResString(@rsString);
    ftSmallint: Result := LoadResString(@rsSmallint);
    ftInteger: Result  := LoadResString(@rsInteger);
    ftWord: Result     := LoadResString(@rsWord);
    ftBoolean: Result  := LoadResString(@rsBoolean);
    ftFloat: Result    := LoadResString(@rsFloat);
    ftCurrency: Result := LoadResString(@rsCurrency);
    ftBCD: Result      := LoadResString(@rsBinaryCodedDecimal);
    ftDate: Result     := LoadResString(@rsDate);
    ftTime: Result     := LoadResString(@rsTime);
    ftDateTime: Result := LoadResString(@rsDateAndTime);
    ftBytes: Result    := LoadResString(@rsFixedBytes);
    ftVarBytes: Result := LoadResString(@rsVariableBytes);
    ftAutoInc: Result  := LoadResString(@rsAutoincrement);
    ftBlob: Result     := LoadResString(@rsBinaryLargeObject);
    ftMemo: Result     := LoadResString(@rsMemo);
    ftGraphic: Result  := LoadResString(@rsGraphic);
    ftFmtMemo: Result  := LoadResString(@rsFormattedMemo);
    ftParadoxOle: Result := LoadResString(@rsParadoxOLE);
    ftDBaseOle: Result := LoadResString(@rsdBASEOLE);
    ftTypedBinary: Result := LoadResString(@rsTypedBinary);
    ftCursor: Result   := LoadResString(@rsCursorFromOracleStoredProcedure);
    ftFixedChar: Result := LoadResString(@rsFixedCharacter);
    ftWideString: Result := LoadResString(@rsWideString);
    ftLargeInt: Result := LoadResString(@rsLargeInteger);
    ftADT: Result      := LoadResString(@rsAbstractDataType);
    ftArray: Result    := LoadResString(@rsArray);
    ftReference: Result := LoadResString(@rsREF);
    ftDataSet: Result  := LoadResString(@rsDataSet);
  end;
end;

function FieldTypeToSQLTypeStr(FieldType: TFieldType; Size: integer = 0): string;
var
  FieldSize: string;
begin
  FieldSize := IntToStr(Size);
  case FieldType of
    ftUnknown: Result  := ' UNKNOWN';     //Unknown
    ftString: Result   := ' VARCHAR(' + FieldSize + ')'; //Character or string
    ftSmallint: Result := ' SMALLINT';    //16-bit integer
    ftInteger: Result  := ' INTEGER';     //32-bit integer
    ftWord: Result     := ' BAD';         //16-bit unsigned integer
    ftBoolean: Result  := ' BOOLEAN';     //Boolean
    ftFloat: Result    := ' FLOAT(32,2)'; //Floating-point numeric
    ftCurrency: Result := ' MONEY';       //Money
    ftBCD: Result      := ' DECIMAL(' + FieldSize + ',2)'; //Binary-coded Decimal
    ftDate: Result     := ' DATE';        //Date field
    ftTime: Result     := ' TIME';        //Time field
    ftDateTime: Result := ' TIMESTAMP';   //Date and time field
    ftBytes: Result    := ' BYTES(' + FieldSize + ')';
    //Fixed number of bytes (binary storage)
    ftVarBytes: Result := ' BYTES(' + FieldSize + ')'; //Variable bytes
    ftAutoInc: Result  := ' AUTOINC';
    //Auto-incrementing 32-bit integer counter
    ftBlob: Result     := ' BLOB(' + FieldSize + ',1)'; //Binary Large OBject
    ftMemo: Result     := ' BLOB(' + FieldSize + ',1)'; //Text memo
    ftGraphic: Result  := ' BLOB(' + FieldSize + ',5)'; //Bitmap
    ftFmtMemo: Result  := ' BLOB(' + FieldSize + ',3)'; //Formatted memo
    ftParadoxOle: Result := ' BLOB(' + FieldSize + ',4)'; //Paradox OLE
    ftDBaseOle: Result := ' BLOB(' + FieldSize + ',4)'; //dBASE OLE
    ftTypedBinary: Result := ' BLOB(' + FieldSize + ',2)'; //Typed binary
    ftCursor: Result   := ' BAD'; //Does not apply to field components.
    else
      Result := ' UNKNOWN';
  end;
end;

function CopyScalarFieldsStructure(const Source: TTable; var Dest: TTable): boolean;
var
  I:     integer;
  Separator: string;
  Query: TQuery;
begin
  Result := True;

  //Delete scalar Fields in Dest that exist in Source
  Separator := ' ';

  Source.Open;
  Dest.Open;

  Query := TQuery.Create(nil);
  try
    Query.SQL.Add('Alter table "' + Dest.TableName + '"');
    for I := 0 to Dest.FieldCount - 1 do
    begin
      if IsScalarAttribute(Dest.Fields[I]) and
        (Source.FindField(Dest.Fields[I].FieldName) <> nil) then
      begin
        Query.SQL.Add(Separator + 'DROP DBT."' + Dest.Fields[I].FieldName +
          '"');
        Separator := ',';
      end;
    end;
    if Separator = ',' then
      try
        Dest.Close;
        Query.ExecSQL;
      except
        Result := False;
      end;

    //Add scalar Fields from Source to Dest
    if Result then
    begin
      Query.SQL.Clear;
      Separator := ' ';
      Query.SQL.Add('Alter table "' + Dest.TableName + '"');
      for I := 0 to Source.FieldCount - 1 do
      begin
        if IsScalarAttribute(Source.Fields[I]) then
        begin
          if Source.Fields[I].DataType = ftCurrency then
            Query.SQL.Add(Separator + 'Add DBT."' +
              Source.Fields[I].FieldName + '" Money')
          else
            Query.SQL.Add(Separator + 'Add DBT."' +
              Source.Fields[I].FieldName + '" Float');
          Separator := ',';
        end;
      end;
      if Separator = ',' then
        try
          Dest.Close;
          Query.ExecSQL;
        except
          Result := False;
        end;
    end;
  finally
    FreeAndNil(Query);
  end;
end;


function FieldTypeParadoxToMIF(Field: TField): string;
begin
  if Field is TBooleanField then
    Result := '  %s Logical'
  else if Field is TIntegerField then
    Result := '  %s Integer'
  else if Field is TFloatField then
    Result := '  %s Float'
  else if Field is TStringField then
    Result := '  %s Char(%d)'
  else
    Result := '  %s Char(30)';
end;


function IndexOf(Str: string; Items: TStrings): integer;
begin
  Result := -1;
  if Items = nil then
    Exit;
  Result := Items.Count;
  while (Result > 0) and (CompareText(Str, Items[Result]) <> 0) do
    Dec(Result);
end;

function CheckFieldName(const S: string): boolean;
var
  I: integer;
begin
  Result := Length(S) > 0;
  I      := 1;
  Result := Result and CharInSet(S[1], ['A'..'Z', 'a'..'z', '_']);
  while Result and (I <= length(s)) do
  begin
    Result := CharInSet(S[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']);
    Inc(I);
  end;
end;

function IsIntegerAttribute(Field: TField): boolean;
begin
  Result := False;
  if Field.DataType in [ftSmallint, ftInteger, ftLargeInt, ftWord, ftAutoInc] then
  begin
    if (Pos(fldID + '_', UpperCase(Field.FieldName)) > 0) or
      (Pos(fldNO + '_', UpperCase(Field.FieldName)) > 0) or
      (Pos(fldVARIANT, UpperCase(Field.FieldName)) > 0) or
      (UpperCase(Field.FieldName) = fldID) or
      (UpperCase(Field.FieldName) = fldVERTEX) or
      (UpperCase(Field.FieldName) = fldV1) or
      (UpperCase(Field.FieldName) = fldV2) or
      (UpperCase(Field.FieldName) = fldV3) then
      Exit;
    Result := True;
  end;
end;

function IsNumericAttribute(Field: TField): boolean;
begin
  Result := False;
  if (Field.DataType in [ftInteger, ftLargeInt, ftWord, ftFloat, ftCurrency]) then
  begin
    if Pos(fldID, UpperCase(Field.FieldName)) = 1 then
      Exit;
    if (UpperCase(Field.FieldName) = fldX) or (UpperCase(Field.FieldName) = fldY) or
      (UpperCase(Field.FieldName) = fldZ) or
      (UpperCase(Field.FieldName) = fldFROM) or
      (UpperCase(Field.FieldName) = fldTO) or
      (UpperCase(Field.FieldName) = fldDEPTH) or
      (UpperCase(Field.FieldName) = fldLENGTH) or
      (UpperCase(Field.FieldName) = fldVARIANT) or
      (UpperCase(Field.FieldName) = fldCORE) or
      (UpperCase(Field.FieldName) = fldDENSITY) or
      (UpperCase(Field.FieldName) = fldMOISTURE) or
      (UpperCase(Field.FieldName) = fldTHICKNESS) or
      (UpperCase(Field.FieldName) = fldAREA) or
      (UpperCase(Field.FieldName) = fldVOLUME) or
      (Pos(fldVARIANT, UpperCase(Field.FieldName)) > 0) or
      (UpperCase(Field.FieldName) = fldVERTEX) then
      Exit;
    Result := True;
  end;
end;

function IsRealAttribute(Field: TField): boolean;
begin
  Result := False;
  if Field.DataType in [ftFloat, ftCurrency] then
  begin
    if Pos(fldID, UpperCase(Field.FieldName)) = 1 then
      Exit;
    if (UpperCase(Field.FieldName) = fldX) or (UpperCase(Field.FieldName) = fldY) or
      (UpperCase(Field.FieldName) = fldZ) or
      (UpperCase(Field.FieldName) = fldFROM) or
      (UpperCase(Field.FieldName) = fldTO) or
      (UpperCase(Field.FieldName) = fldDEPTH) or
      (UpperCase(Field.FieldName) = fldLENGTH) or
      (UpperCase(Field.FieldName) = fldVARIANT) or
      (UpperCase(Field.FieldName) = fldCORE) or
      (UpperCase(Field.FieldName) = fldDENSITY) or
      (UpperCase(Field.FieldName) = fldMOISTURE) or
      (UpperCase(Field.FieldName) = fldTHICKNESS) or
      (Pos(fldVARIANT, UpperCase(Field.FieldName)) > 0) or
      (UpperCase(Field.FieldName) = fldVERTEX) then
      Exit;
    Result := True;
  end;
end;

function IsScalarAttribute(Field: TField; Mode3D: boolean = True): boolean;
begin
  Result := IsRealAttribute(Field) or ((not Mode3D) and
    (UpperCase(Field.FieldName) = fldZ));
end;


//---------------------------------------

procedure CheckItemIndex(ListBox: TListBox);
begin
  ListBox.ItemIndex := Min(Max(0, ListBox.ItemIndex), ListBox.Items.Count - 1);
  if ListBox.ItemIndex < 0 then
    Abort;
end;

//---------------------------------------

function GetCurrentItem(ListBox: TListBox): string; overload;
begin
  Result := ListBox.Items[ListBox.ItemIndex];
end;

 //==========================================================\\
 // Reads initial limits of deposit model from the Framework \\
 //==========================================================\\
function ReadFramework(ATableName: TFileName; var X0, Y0, Z0: Double;
  var DX, DY, DZ: Double; var NX, NY, NZ: Integer): boolean;
var
  FrameworkTable: TTable;
begin
  FrameworkTable := TTable.Create(nil);
  try
    FrameworkTable.TableName := ExpandFileName(ATableName);
    FrameworkTable.Open;
    FrameworkTable.First;
    X0 := FrameworkTable.Fields[1].AsFloat;
    FrameworkTable.Next;
    Y0 := FrameworkTable.Fields[1].AsFloat;
    FrameworkTable.Next;
    Z0 := FrameworkTable.Fields[1].AsFloat;
    FrameworkTable.Next;

    DX := FrameworkTable.Fields[1].AsInteger;
    FrameworkTable.Next;
    DY := FrameworkTable.Fields[1].AsInteger;
    FrameworkTable.Next;
    DZ := FrameworkTable.Fields[1].AsInteger;
    FrameworkTable.Next;

    NX := Round(FrameworkTable.Fields[1].AsFloat);
    FrameworkTable.Next;
    NY := Round(FrameworkTable.Fields[1].AsFloat);
    FrameworkTable.Next;
    NZ := Round(FrameworkTable.Fields[1].AsFloat);
    FrameworkTable.Close;
    Result := True;
  finally
    FrameworkTable.Free;
  end;
end;

 //=========================================================\\
 // Writes project limits of deposit model to the Framework \\
 //=========================================================\\
procedure WriteFramework(FileName: TFileName; X0, Y0, Z0: Double;
  DX, DY, DZ: Double; NX, NY, NZ: Integer);
var
  FrameworkTable: TTable;

begin
  FrameworkTable := TTable.Create(nil);
  try
    FrameworkTable.TableName := ExpandFileName(FileName);
    FrameworkTable.FieldDefs.Add(fldPARAMETER, ftString, 2);
    FrameworkTable.FieldDefs.Add(fldG, ftFloat);
    FrameworkTable.CreateTable;
    FrameworkTable.Open;
    FrameworkTable.AppendRecord(['X0', X0]);
    FrameworkTable.AppendRecord(['Y0', Y0]);
    FrameworkTable.AppendRecord(['Z0', Z0]);
    FrameworkTable.AppendRecord(['DX', DX]);
    FrameworkTable.AppendRecord(['DY', DY]);
    FrameworkTable.AppendRecord(['DZ', DZ]);
    FrameworkTable.AppendRecord(['NX', NX]);
    FrameworkTable.AppendRecord(['NY', NY]);
    FrameworkTable.AppendRecord(['NZ', NZ]);
    FrameworkTable.Close;
  finally
    FrameworkTable.Free;
  end;
end;

function ReadParFile(ATableName: TFileName; var XO, YO, ZO: Double;
  var DX, DY, DZ: Double; var NX, NY, NZ: integer): boolean;
var
  Code:  integer;
  ParFile, S: string;
  F:     Text;
  Value: Double;
  N:     integer;

begin
  Result     := False;
  ATableName := ExpandFileName(ATableName);
  ParFile    := ExtractFilePath(ATableName) + NameOnly(ATableName) + ParExt;
  if FileExists(ParFile) then
  begin
    DX := 15;   DY := 15;   DZ := 15; //???
    AssignFile(F, ParFile);
{$I-}
    Reset(F);
    Readln(F, S);
    Val(Copy(S, 6, 10), Value, Code);
    if Code = 0 then
      XO := Value;
    Val(Copy(S, 16, 10), Value, Code);
    if Code = 0 then
      YO := Value;
    Val(Copy(S, 26, 10), Value, Code);
    if Code = 0 then
      ZO := Value;
    Readln(F, S);
    Val(Copy(S, 6, 10), Value, Code);
    if Code = 0 then
      DX := Value;
    Val(Copy(S, 16, 10), Value, Code);
    if Code = 0 then
      DY := Value;
    Val(Copy(S, 26, 10), Value, Code);
    if Code = 0 then
      DZ := Value;
    Readln(F, S);
    Val(Copy(S, 6, 10), N, Code);
    if Code = 0 then
      NX := N;
    Val(Copy(S, 16, 10), N, Code);
    if Code = 0 then
      NY := N;
    Val(Copy(S, 26, 10), N, Code);
    if Code = 0 then
      NZ := N;
{$I+}
    Close(F);
    Result := True;
  end;
end;

procedure WriteParFile(ATableName: TFileName; XO, YO, ZO: Double;
  DX, DY, DZ: Double; NX, NY, NZ: Integer);

var
  ParFile: string;
  F: Text;

begin
  ATableName := ExpandFileName(ATableName);
  ParFile    := ExtractFilePath(ATableName) + NameOnly(ATableName) + ParExt;
  AssignFile(F, ParFile);
{$I-}
  Rewrite(F);
  Writeln(F, '  1  ', XO: 10, YO: 10, ZO: 10);
  Writeln(F, '  2  ', DX: 10, DY: 10, DZ: 10);
  Writeln(F, '  3  ', NX: 10, NY: 10, NZ: 10);
  Writeln(F, '  4  ', 1: 10, 2: 5, 3: 5, 2000: 10);
  Writeln(F, '  5  ', -2000: 10);
  Writeln(F, '  6  ', 1: 10);
  Writeln(F, '  7  ', 1: 10, NX: 10, 1: 10, NY: 10, 1: 10, NZ: 10);
  Writeln(F, '  8  ', 3: 10, 7: 10);
  Writeln(F, '  9  ', 80: 10, 45: 10);
  Writeln(F, ' 10  ', 180: 10, 50: 10);
  Writeln(F, ' 11  ', 280: 10, 45: 10);
  NZ := Min(NZ, 999 * 3 div 2);
  Writeln(F, ' 12  ', 3: 3, 1 + 2 * (NZ div 3): 3, 1 + (NZ div 3): 3, 1: 3);
{$I+}
  Close(F);
end;

//---------------------------------------\\
function MinMaxXYZ(ATableName: TFileName;
  var Xmin, Ymin, Zmin, Xmax, Ymax, Zmax: Double;
  ProgressBar: TProgressBar = nil): boolean;
var
  I:     integer;
  X, Y, Z: Double;
  Table: TTable;
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Table := TTable.Create(Application);
    try
      Table.TableName := ExpandFileName(ATableName);
      Table.Open;
      Table.First;

      Xmin := Table.FieldByName(fldX).AsFloat;
      Xmax := Xmin;
      Ymin := Table.FieldByName(fldY).AsFloat;
      Ymax := Ymin;
      Zmin := Table.FieldByName(fldZ).AsFloat;
      Zmax := Zmin;
      if ProgressBar <> nil then
        with ProgressBar do
        begin
          Min      := 0;
          Max      := Table.RecordCount;
          Min      := System.Math.Min(1, Table.RecordCount);
          Position := Min;
        end;
      for I := 2 to Table.RecordCount do
      begin
        if I mod 500 = 0 then
          if ProgressBar <> nil then
            ProgressBar.Position := I;
        Table.Next;
        X := Table.FieldByName(fldX).AsFloat;
        Y := Table.FieldByName(fldY).AsFloat;
        Z := Table.FieldByName(fldZ).AsFloat;
        if Xmin > X then
          Xmin := X;
        if Xmax < X then
          Xmax := X;
        if Ymin > Y then
          Ymin := Y;
        if Ymax < Y then
          Ymax := Y;
        if Zmin > Z then
          Zmin := Z;
        if Zmax < Z then
          Zmax := Z;
      end;
      Table.Close;
    finally
      Table.Free;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
  Result := True;
  if ProgressBar <> nil then
    ProgressBar.Position := ProgressBar.Min;
end;

//-----------------------------------------------------------------------------

procedure GetMinMaxXYZ(TableName: TFileName;
  var Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: Double); overload;
begin
  with TQuery.Create(nil) do
  begin
    SQL.Clear;
    SQL.Add('SELECT MIN(x) as MINX, MAX(x) as MAXX, MIN(y) as MINY, MAX(y) as MAXY, MIN(z) as MINZ, MAX(z) as MAXZ');
    SQL.Add('FROM "' + TableName + '"');
    Open;
    Xmin := FieldByName('MINX').Value;
    Xmax := FieldByName('MAXX').Value;
    Ymin := FieldByName('MINY').Value;
    Ymax := FieldByName('MAXY').Value;
    Zmin := FieldByName('MINZ').Value;
    Zmax := FieldByName('MAXZ').Value;
    Close;
    Free;
  end;
end;

//-----------------------------------------------------------------------------

procedure GetMinMaxXYZ(Points: TCoordinateArray;
          var Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: double); overload;
var
  I: integer;
begin
  Xmin := Points[0].x;
  Xmax := xmin;
  Ymin := Points[0].y;
  Ymax := ymin;
  Zmin := Points[0].z;
  Zmax := Zmin;
  for I := 1 to High(Points) do
  begin
    if Xmax < Points[i].X then
      Xmax := Points[i].X;
    if Xmin > Points[i].X then
      Xmin := Points[i].X;
    if ymax < Points[i].Y then
      ymax := Points[i].Y;
    if ymin > Points[i].Y then
      ymin := Points[i].Y;
    if zmax < Points[i].Z then
      zmax := Points[i].Z;
    if zmin > Points[i].Z then
      zmin := Points[i].Z;
  end;
end;

//---------------------------------------\\
function GetEmptyBitmap(AHeight, AWidth: integer): TBitmap;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;
  Result := Bitmap;
end;

//---------------------------------------\\
function GetRowCount(FileName: TFileName): integer;
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromFile(FileName);
    Result := Strings.Count;
  except
    Result := 0;
  end;
  Strings.Free;
end;

procedure DrawPolygon(Dest: TCanvas; Poly: array of TPoint;
  BrushColor, BkGround: TColor; BrushStyle: TBrushStyle; Bitmap: TBitMap; cPen: TPen);

var
  BM1, BM2:    TBitMap;
  OldCopyMode: TCopyMode;
begin
  BM1 := TBitmap.Create;
  BM1.Height := Dest.ClipRect.Bottom - Dest.ClipRect.Top;
  BM1.Width := Dest.ClipRect.Right - Dest.ClipRect.Left;
  OldCopyMode := Dest.CopyMode;
  //bkGround
  if BkGround <> clNone then
  begin
    Dest.Pen.Style   := psClear;
    Dest.Brush.Color := BkGround;
    Dest.Brush.Style := bsSolid;
    Dest.Polygon(poly);
  end;

  //Brush mask
  with BM1.Canvas do
  begin
    CopyMode := cmWhiteness;
    CopyRect(ClipRect, BM1.Canvas, ClipRect);

    //Pen:=cPen;Pen.Color:=clBlack;
    Pen.Style := psClear;
    if Assigned(Bitmap) then
      Brush.Bitmap := Bitmap
    else
    begin
      Brush.Color := clBlack;
      Brush.Style := BrushStyle;
    end;
    Polygon(Poly);
  end;
  Dest.CopyMode := cmSrcAnd;
  Dest.CopyRect(Dest.ClipRect, BM1.Canvas, Dest.ClipRect);

  {Brush }
  BM2 := TBitmap.Create;
  BM2.Height := Dest.ClipRect.Bottom - Dest.ClipRect.Top;
  BM2.Width := Dest.ClipRect.Right - Dest.ClipRect.Left;
  BM2.Canvas.Brush.Color := BrushColor;
  BM2.Canvas.Brush.Style := bsSolid;
  BM2.Canvas.FillRect(BM2.Canvas.ClipRect);

  BM1.Canvas.CopyMode := cmSrcErase;
  BM1.Canvas.CopyRect(BM1.Canvas.ClipRect, BM2.Canvas, BM1.Canvas.ClipRect);
  Dest.CopyMode := cmSrcPaint;
  Dest.CopyRect(Dest.ClipRect, BM1.Canvas, Dest.ClipRect);
  BM2.Free;
  BM1.Free;
  Dest.CopyMode := OldCopyMode;

  //Border
  Dest.Brush.Bitmap := nil;
  Dest.Brush.Style := bsClear;
  Dest.Pen := cPen;
  Dest.Polygon(Poly);
end;

function ReadPolygon(TablePolyFace, TablePolyVert: TTable;
  out xp, yp, zp: array of single; var VertexCount: integer): boolean;

begin
  TablePolyVert.Filter := Format(fldID_POLY + '=%d',
    [TablePolyFace.FieldByName(fldID).AsInteger]);
  Result := TablePolyVert.FindFirst;
  if not Result then
    Exit;
  VertexCount := 0;
  if High(xp) = High(yp) then
    repeat
      xp[VertexCount] := TablePolyVert.FieldByName(fldX).AsFloat;
      yp[VertexCount] := TablePolyVert.FieldByName(fldY).AsFloat;
      zp[VertexCount] := TablePolyVert.FieldByName(fldZ).AsFloat;
      Inc(VertexCount);
    until not TablePolyVert.FindNext;
end;

//---------------------------------------------------------------------------

function AverageColor(Colors: array of TColor): TColor;
var
  I: integer;
  R, G, B: double;
begin
  R := 0;
  G := 0;
  B := 0;
  for I := Low(Colors) to High(Colors) do
  begin
    R := R + (Colors[I] shr 0 and $FF);
    G := G + (Colors[I] shr 8 and $FF);
    B := B + (Colors[I] shr 16 and $FF);
  end;
  Result := TColor($20000000 or Round(R / Length(Colors)) or
    Round(G / Length(Colors)) shl 8 or Round(B / Length(Colors)) shl 16);
end;

initialization

end.
