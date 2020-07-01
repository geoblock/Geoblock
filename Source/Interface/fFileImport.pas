// -----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
// -----------------------------------------------------------------------------
{ ! The dialog for file import routines }

unit fFileImport;

interface

uses
  System.SysUtils, 
  System.StrUtils, 
  System.Math, 
  System.UITypes,
  System.IniFiles, 
  System.Variants, 
  System.Classes,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.StdCtrls,
  Vcl.ExtCtrls, 
  Vcl.ComCtrls, 
  Vcl.Buttons, 
  Vcl.ToolWin,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  fInitialForm,
  fInitialDialog,
  dBase;

type
  TfmFileImport = class(TfmInitialDialog)
    GroupBoxInput: TGroupBox;
    ProgressBar: TProgressBar;
    GroupBoxOutput: TGroupBox;
    ToolBarShowAs: TToolBar;
    ToolButton3: TToolButton;
    ToolButtonMap: TToolButton;
    ToolButtonTable: TToolButton;
    ToolButtonGraph: TToolButton;
    ToolButton4: TToolButton;
    PanelOutputPath: TPanel;
    EditOutputName: TEdit;
    SpeedButtonOutputBrowse: TSpeedButton;
    PanelInFile: TPanel;
    procedure ButtonOKClick(Sender: TObject);
    procedure EditOutputNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolButtonShowAsClick(Sender: TObject);
  private
    ImportIndex: Integer;
    FInFile: TFileName;
    FOutputPath: TFileName;
    FOutModelName: TFileName;
    FOutModelType: Integer;

    procedure CreateMineSequenceTable;

    procedure KDR_To_Poly(FileNameKDR, TableNamePoly: TFileName);
    procedure DXF_To_Poly(FileNameDXF, FileNamePoly: TFileName);
    procedure MIF_To_Poly(FileNameMIF, FileNamePoly: TFileName);
    procedure DAT_To_Points2D; // dat file of Surfer
    procedure GRD_To_Grid2D; // grd file of Surfer
    procedure ECO_Or_RES_To_Grid3D; // Whittle ECO or RES
    procedure MOD_To_Grid3D; // Whittle MOD with parcels
    procedure MSQ_To_Grid3D; // Whittle MSQ to Grid3D
    procedure EAS_To_Points2D; // GeoEAS to Points2D

    function FeMapINP_Mesh3D(InFile, OutModelName: TFileName): Integer;

    function GetOutputPath: TFileName;
    procedure SetOutModelType(const Value: Integer);
    procedure SetOutModelName(const Value: TFileName);
    procedure SetOutputPath(const Value: TFileName);
    procedure SetInFile(const Value: TFileName);
     
  public
     
    property InFile: TFileName Read FInFile Write SetInFile;
    property OutputPath: TFileName Read GetOutputPath Write SetOutputPath;
    property OutModelName: TFileName Read FOutModelName Write SetOutModelName;
    property OutModelType: Integer Read FOutModelType Write SetOutModelType;

    function ChooseModelType: Boolean;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

  TTextStream = class(TFileStream)
  private
    FDelimiters: string;
    FCRCount: Integer;
    FonCRCountChange: TNotifyEvent;
    procedure CallCRCountChange;
    procedure SetCRCount(const Value: Integer);
  public
    constructor Create(const FileName: TFileName; Mode: Word);
  public
    procedure InitProgress;
    procedure DefaultChangeCRCount(Sender: TObject);
    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;
    procedure SkipDelimiters;
    property Delimiters: string Read FDelimiters Write FDelimiters;
    property CRCount: Integer Read FCRCount Write SetCRCount;
    function IsDelimeter(Ch: Char): Boolean;
    function ReadItem: string;
    function ReadLine: string;
    function LineIndex: Longint;
    function ReadChar: Char;
    function EOF: Boolean;
    function BOF: Boolean;
  published
    property OnCRCountChange: TNotifyEvent Read FonCRCountChange
      Write FonCRCountChange;
  end;

var
  FmFileImport: TfmFileImport;
  ProgressBar: TProgressBar;

function SetProgressMin(Value: Integer): Boolean;
function SetProgressMax(Value: Integer): Boolean;
function SetProgressPosition(Value: Integer): Boolean;
function SetProgressPercent(Percent: Real): Boolean; overload;
function EmptyProgressFunc(Percent: Real): Boolean; overload;
function InitProgress(AMin: Integer = 0; APosition: Integer = 0;
  AMax: Integer = 100): Boolean;

//==========================================================================
implementation
//==========================================================================

uses
  uGlobals,
  uCommon,
  uProfuns,
  uFileCreator,
  uWhittle,
  dDialogs;

{$R *.dfm}

const
  IeOk = 0;
  IeUserAbort = 1;
  IeError = 2;

  ForbiddenWords: array [1 .. 18] of string = ('ORDER', 'GROUP', 'SELECT', 'IN',
    'BY', 'FROM', 'WHERE', 'HAVING', 'EXISTS', 'ASC', 'DESC', 'AS', 'COUNT',
    'MAX', 'MIN', 'AVG', 'NULL', 'NOT');

  ForbiddenChars: set of Char = [' ', '.', ',', '/', '\', '=', ':', ';', '*',
    '-', '+'];
  // -----------------------------------------------------------------------------

function CutItem(var Value: string): string;
begin
  Result := Copy(Value, 1, Pos(',', Value) - 1);
  System.Delete(Value, 1, Length(Result) + 1);
  Result := Trim(Result);
end;

// -----------------------------------------------------------------------------

procedure StrToXYZ(Value: string; var X, Y, Z: Double);
begin
  X := StrToFloat(CutItem(Value));
  Y := StrToFloat(CutItem(Value));
  Z := StrToFloat(CutItem(Value));
end;

// -----------------------------------------------------------------------------

function InitProgress(AMin: Integer = 0; APosition: Integer = 0;
  AMax: Integer = 100): Boolean;
begin
  Result := True;
  try
    if ProgressBar <> nil then
      with ProgressBar do
      begin
        Min := AMin;
        Position := APosition;
        Max := AMax;
      end
    else
      Result := False;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

function EmptyProgressFunc(Percent: Real): Boolean; overload;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function SetProgressPercent(Percent: Real): Boolean; overload;
begin
  Result := True;
  try
    if ProgressBar <> nil then
      ProgressBar.Position := Max(
        ProgressBar.Max,
        Round((ProgressBar.Max - ProgressBar.Min) * Percent + ProgressBar.Min))
    else
      Result := False;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

function SetProgressMin(Value: Integer): Boolean;
begin
  Result := True;
  try
    if ProgressBar <> nil then
      ProgressBar.Min := Value
    else
      Result := False;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

function SetProgressMax(Value: Integer): Boolean;
begin
  Result := True;
  try
    if ProgressBar <> nil then
      ProgressBar.Max := Value
    else
      Result := False;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------

function SetProgressPosition(Value: Integer): Boolean;
begin
  Result := True;
  try
    if ProgressBar <> nil then
      ProgressBar.Position := Value
    else
      Result := False;
  except
    Result := False;
  end;
end;

// -----------------------------------------------------------------------------
{ TfmFileImport }

procedure TfmFileImport.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  fFileImport.ProgressBar := ProgressBar;
end;

// -----------------------------------------------------------------------------

function TfmFileImport.GetOutputPath: TFileName;
begin
  Result := ExtractFilePath(OutModelName);
end;

// -----------------------------------------------------------------------------
function TfmFileImport.ChooseModelType: Boolean;
var
  FileExt: string;

begin
  with dmDialogs do
  begin
    OpenDialogImport.InitialDir := ExpandPath(DirFiles);
    OpenDialogImport.DefaultExt := 'krd';
    OpenDialogImport.FilterIndex := 13; // *.kdr
    if OpenDialogImport.Execute then
    begin
      InFile := OpenDialogImport.FileName;
      FileExt := LowerCase(ExtractFileExt(InFile));
      // Polygons.POL to Poly.DB
      if (FileExt = '.kdr') then
      begin
        ImportIndex := 1;
        OutModelType := mtPolygons;
      end;
      // Surfer.DAT) to Point2D.DB
      if (FileExt = '.dat') or (FileExt = '.csv') or (FileExt = '.csv') then
      begin
        ImportIndex := 2;
        OutModelType := mtPoints2D;
      end;
      // Surfer.GRD to Grid2D.DB
      if (FileExt = '.grd') then
      begin
        ImportIndex := 3;
        OutModelType := mtGrids2D;
      end;
      // Whittle ThreeD.ECO to Grid3D.DB
      if (FileExt = '.eco') then
      begin
        ImportIndex := 4;
        OutModelType := mtGrids3D;
      end;
      // Whittle ThreeD.RES to Grid3D.DB
      if (FileExt = '.res') then
      begin
        ImportIndex := 5;
        OutModelType := mtGrids3D;
      end;
      // Whittle FourD.MOD to Grid3D.DB
      if (FileExt = '.mod') then
      begin
        ImportIndex := 6;
        OutModelType := mtGrids3D;
      end;
      // Whittle FourD.MSQ to Grid3D.DB
      if (FileExt = '.msq') then
      begin
        ImportIndex := 7;
        OutModelType := mtGrids3D;
      end;
      // AutoCAD.DXF to Poly.DB
      if (FileExt = '.dxf') then
      begin
        ImportIndex := 8;
        OutModelType := mtPolygons;
      end;
      // MapInfo.MIF to Poly.DB
      if (FileExt = '.mif') or (FileExt = '.mid') then
      begin
        ImportIndex := 9;
        OutModelType := mtPolygons;
        InFile := ExtractFilePath(InFile) + NameOnly(InFile) + '.mif';
      end;
      // FeMap.INP to Poly.DB
      if (FileExt = '.inp') then
      begin
        ImportIndex := 10;
        OutModelType := mtMeshes3D;
      end;
      // Geo-EAS to Points
      if FileExt = '.eas' then
      begin
        ImportIndex := 11;
        OutModelType := mtPoints2D;
      end;
      // Shape files of ArcView
      if FileExt = '.shp' then
      begin
        ImportIndex := 12;
        OutModelType := mtAll;
      end;
      Result := True;
    end
    else
      Result := False;
  end;
  PanelInFile.Caption := InFile;
  PanelInFile.Hint := PanelInFile.Caption;
  PanelOutputPath.Caption := FOutputPath;
  PanelOutputPath.Hint := PanelOutputPath.Caption;
  EditOutputName.Text := NameOnly(InFile);
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.SetOutModelType(const Value: Integer);
begin
  FOutModelType := Value;
  case FOutModelType of
    mtDholes:   OutputPath := ExpandPath(DirDholes);
    mtPoints2D: OutputPath := ExpandPath(DirPoints2D);
    mtPoints3D: OutputPath := ExpandPath(DirPoints2D);
    mtPolygons: OutputPath := ExpandPath(DirPolygonPoly);
    mtTins:     OutputPath := ExpandPath(DirTinFaces);
    mtSolids:   OutputPath := ExpandPath(DirSolids);
    mtGrids2D:  OutputPath := ExpandPath(DirGrid2D);
    mtGrids3D:  OutputPath := ExpandPath(DirGrid3D);
    mtMeshes2D: OutputPath := ExpandPath(DirMesh2D);
    mtMeshes3D: OutputPath := ExpandPath(DirMesh3D);
  end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.SetOutModelName(const Value: TFileName);
begin
  FOutModelName := Value;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.SetOutputPath(const Value: TFileName);
begin
  if CompareText(FOutputPath, Value) <> 0 then
    FOutputPath := Value;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.SetInFile(const Value: TFileName);
begin
  if CompareText(FInFile, Value) <> 0 then
    FInFile := Value;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.CreateMineSequenceTable;
begin
  with dmBase do
  begin
    TableImport := TTable.Create(Self);
    try
      TableImport.TableName := OutModelName;
      TableImport.FieldDefs.Clear;
      TableImport.FieldDefs.Add(FldID, FtAutoInc, 0, False);
      TableImport.FieldDefs.Add(FldX, FtFloat, 0, False);
      TableImport.FieldDefs.Add(FldY, FtFloat, 0, False);
      TableImport.FieldDefs.Add(FldZ, FtFloat, 0, False);
      TableImport.FieldDefs.Add(FldG, FtCurrency, 0, False);
      TableImport.FieldDefs.Add(FldSEQUENCE, FtInteger, 0, False);
      TableImport.CreateTable;
    finally
      TableImport.Close;
    end;
  end;
end;

// -----------------------------------------------------------------------------

{ **************************************************** }
{ Import of AutoCAD DXF file into  Table of POlygons }
{ **************************************************** }
procedure TfmFileImport.DXF_To_Poly(FileNameDXF, FileNamePoly: TFileName);
const
  ObjectArr: string = 'LINE      ' + 'SOLID     ' + 'VERTEX    ' + '3DLINE    '
    + 'TEXT      ' + 'SEQEND    ' + 'POINT     ' + 'SHAPE     ' + '3DFACE    ' +
    'CIRCLE    ' + 'INSERT    ' + 'DIMENSION ' + 'ARC       ' + 'ATTRIB    ' +
    'TRACE     ' + 'POLYLINE  ';
  DxfoLINE = 001;
  DxfoSOLID = 011;
  DxfoVERTEX = 021;
  Dxfo3DLINE = 031;
  DxfoTEXT = 041;
  DxfoSEQEND = 051;
  DxfoPOINT = 061;
  DxfoSHAPE = 071;
  Dxfo3DFACE = 081;
  DxfoCIRCLE = 091;
  DxfoINSERT = 101;
  DxfoDIMENSION = 111;
  DxfoARC = 121;
  DxfoATTRIB = 131;
  DxfoTRACE = 141;
  DxfoPOLYLINE = 151;

var
  TablePolyFace, TablePolyVertex: TTable;
  I: Integer;

  X, Y, Z: Single;
  VertexID: Integer;
  VertexNo: Integer;

  DXFFile: TStrings;
  Pos: Integer;

  PolyX, PolyY, PolyZ: Single;
  PolyID: Integer;
  PolyName: string;
  PolyType: TPolygonType;

  Code: Integer;
  Object_: string;

  function EOF: Boolean;
  begin
    Result := Pos >= DXFFile.Count;
  end;

{ sub }
  procedure Next;
  begin
    if Pos < DXFFile.Count then
      Pos := Pos + 1
    else
      Pos := DXFFile.Count;
  end;

{ sub }
  procedure Prev;
  begin
    if Pos > 0 then
      Pos := Pos - 1
    else
      Pos := 0;
  end;

{ sub }
  function ReadString: string;
  begin
    if EOF then
      Result := ''
    else
    begin
      Result := DXFFile[Pos];
      Inc(Pos);
    end;
  end;

{ sub }
  function ReadCode: Integer;
  begin
    if EOF then
      Result := -1
    else
    begin
      Result := StrToInt(DXFFile[Pos]);
      Inc(Pos);
    end;
  end;

{ sub }
  procedure WritePolyInfo;
  begin
    try
      TablePolyFace.Append;
      TablePolyFace.FieldByName(FldID).AsInteger := PolyID;
      TablePolyFace.FieldByName(FldID_TYPE).AsInteger := Ord(PolyType);
      TablePolyFace.FieldByName(FldMATERIAL).AsInteger := 0;
      TablePolyFace.FieldByName(FldNAME).AsString := PolyName;
      TablePolyFace.FieldByName(FldX).AsFloat :=
        RoundTo(PolyX / Max(1, VertexNo - 1), -2);
      TablePolyFace.FieldByName(FldY).AsFloat :=
        RoundTo(PolyY / Max(1, VertexNo - 1), -2);
      TablePolyFace.FieldByName(FldZ).AsFloat :=
        RoundTo(PolyZ / Max(1, VertexNo - 1), -2);
      TablePolyFace.Post;
    except
    end;
    VertexNo := 1;
    PolyX := 0;
    PolyY := 0;
    PolyZ := 0;
    Inc(PolyID);
  end;

{ sub }
  procedure WriteVertexInfo;
  begin
    try
      PolyX := PolyX + X;
      PolyY := PolyY + Y;
      PolyZ := PolyZ + Z;
      TablePolyVertex.Append;
      TablePolyVertex.FieldByName(FldID).AsInteger := VertexID;
      TablePolyVertex.FieldByName(FldID_POLY).AsInteger := PolyID;
      TablePolyVertex.FieldByName(FldID_NO).AsInteger := VertexNo;
      TablePolyVertex.FieldByName(FldX).AsFloat := RoundTo(X, -2);
      TablePolyVertex.FieldByName(FldY).AsFloat := RoundTo(Y, -2);
      TablePolyVertex.FieldByName(FldZ).AsFloat := RoundTo(Z, -2);
      TablePolyVertex.Post;
      X := 0;
      Y := 0;
      Z := 0;
      Inc(VertexNo);
      Inc(VertexID);
    except
    end;
  end;

{ sub }
  procedure ReadVertex;
  begin
    X := 0;
    Y := 0;
    Z := 0;
    Code := ReadCode;
    while (not EOF) and (Code <> 0) do
    begin
      Object_ := Trim(ReadString);
      case Code of
        10 .. 18:
          begin
            if Code <> 10 then
              WriteVertexInfo;
            X := StrToFloat(Object_);
          end;
        20 .. 28:
          Y := StrToFloat(Object_);
        30 .. 38:
          Z := StrToFloat(Object_);
      end;
      Code := ReadCode;
    end;
    WriteVertexInfo;
    if not EOF then
      Prev;
  end;

{ sub }
  procedure Read3DFace;
  begin
    PolyType := PtPolygon;
    PolyName := '3DFACE';
    repeat
      ReadVertex;
    until (EOF) or (Code = 0);
    WritePolyInfo;
  end;

{ sub }
  procedure ReadPolyLine;
  begin
    PolyType := PtPolyline;
    PolyName := 'POLYLINE';
    repeat
      Code := ReadCode;
      Object_ := ReadString;
      case Code of
        0:
          if Object_ = 'VERTEX' then
            ReadVertex;
        70:
          if (StrToInt(Trim(Object_)) and 1) = 1 then
            PolyType := PtPolygon;
      end;
    until EOF or (Object_ = 'SEQEND');
    WritePolyInfo;
  end;

{ sub }
  procedure ReadEntities;
  begin
    VertexID := 1;
    VertexNo := 1;
    PolyID := 1;
    repeat
      while (not EOF) and (ReadCode <> 0) do
      begin
        Inc(I);
        ProgressBar.Position := I;
        ReadString;
      end;
      Object_ := ReadString;
      if Object_ <> '' then
        case System.Pos(Object_, ObjectArr) of
          DxfoPOLYLINE:
            ReadPolyLine;
          Dxfo3DFACE:
            Read3DFace;
        end;
    until EOF or (Object_ = 'ENDSEC');
  end;

begin
  ProgressBar.Min := 0;
  ProgressBar.Position := 0;
  PolyX := 0;
  PolyY := 0;
  PolyZ := 0;

  TablePolyFace := TTable.Create(Application);
  try
    TablePolyFace.TableName := FileNamePoly;
    CreatePolygonTables(FileNamePoly);
    TablePolyFace.Open;
    TablePolyVertex := TTable.Create(Application);
    try
      TablePolyVertex.TableName := ChangeModelTable(DirPolygonPoly,
        DirPolygonVertex, FileNamePoly);
      TablePolyVertex.Open;
      DXFFile := TStringList.Create;
      try
        DXFFile.LoadFromFile(FileNameDXF);
        ProgressBar.Max := DXFFile.Count - 1;
        Pos := 0;
        ReadCode;
        while (not EOF) and (ReadString <> 'ENTITIES') do
        begin
          Inc(I);
          ProgressBar.Position := I;
          ReadCode;
        end;
        if not EOF then
        begin
          Inc(I);
          ProgressBar.Position := I;
          ReadEntities;
        end;
      finally
        DXFFile.Free;
      end;
      TablePolyVertex.Close;
    finally
      TablePolyVertex.Free;
    end;
    TablePolyFace.Close;
  finally
    TablePolyFace.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TfmFileImport.FeMapINP_Mesh3D(InFile, OutModelName: TFileName)
  : Integer;

const
  VertexNo: array [1 .. 8] of Byte = (5, 4, 0, 1, 7, 6, 2, 3);
var
  SrcFile: Textfile;
  Str: string;
  TableElement, TableNode, TableLink: TTable;

  DelimeterPos, ID_Cell, I: Integer;
  Query: TQuery;
  BatMove: TBatchMove;

begin
  Result := IeError;
  AssignFile(SrcFile, InFile);
  Reset(SrcFile);
  try
    TableElement := TTable.Create(nil);
    try
      TableLink := TTable.Create(nil);
      try
        TableNode := TTable.Create(nil);
        try
          TableLink.TableName := OutModelName;
          TableNode.TableName := ChangeModelTable(DirMesh3D, DirMesh3DNode,
            OutModelName);
          TableElement.TableName := ChangeModelTable(DirMesh3D,
            DirMesh3DElement, OutModelName);
          Str := '';
          while (Pos('*NODE', Str) = 0) and (not EOF(SrcFile)) do
          begin
            ReadLn(SrcFile, Str);
          end;

          TableNode.FieldDefs.Clear;
          TableNode.FieldDefs.Add(FldID_NODE, FtInteger, 0, False);
          TableNode.FieldDefs.Add(FldX, FtFloat, 0, False);
          TableNode.FieldDefs.Add(FldY, FtFloat, 0, False);
          TableNode.FieldDefs.Add(FldZ, FtFloat, 0, False);
          TableNode.CreateTable;
          TableNode.Open;
          TableNode.FieldDefs[1].Precision := Precision; // X
          TableNode.FieldDefs[2].Precision := Precision; // Y
          TableNode.FieldDefs[3].Precision := Precision; // Z
          ReadLn(SrcFile, Str);
          while (Pos('*ELEMENT', Str) = 0) and (not EOF(SrcFile)) do
          begin
            TableNode.Append;
            DelimeterPos := Pos(',', Str);
            TableNode.Fields[0].AsString := Copy(Str, 1, DelimeterPos - 1);
            Str := Copy(Str, DelimeterPos + 1, Length(Str));
            DelimeterPos := Pos(',', Str);
            TableNode.Fields[1].AsString := Copy(Str, 1, DelimeterPos - 1);
            Str := Copy(Str, DelimeterPos + 1, Length(Str));
            DelimeterPos := Pos(',', Str);
            TableNode.Fields[2].AsString := Copy(Str, 1, DelimeterPos - 1);
            Str := Copy(Str, DelimeterPos + 1, Length(Str));
            DelimeterPos := Length(Str) + 1;
            TableNode.Fields[3].AsString := Copy(Str, 1, DelimeterPos - 1);
            TableNode.Post;
            ReadLn(SrcFile, Str);
          end;
          TableNode.Close;
        finally
          TableNode.Free;
        end;
        TableElement.FieldDefs.Clear;
        TableElement.FieldDefs.Add(FldID_ELEMENT, FtInteger, 0, False);
        TableElement.CreateTable;
        TableElement.Open;

        TableLink.FieldDefs.Clear;
        TableLink.FieldDefs.Add(FldID_MATRIX, FtInteger, 0, False);
        TableLink.FieldDefs.Add(FldID_ELEMENT, FtInteger, 0, False);
        TableLink.FieldDefs.Add(FldID_NODE, FtInteger, 0, False);
        TableLink.FieldDefs.Add(FldID_NO, FtWord, 0, False);
        TableLink.CreateTable;
        TableLink.Open;
        ReadLn(SrcFile, Str);
        while (Pos('*', Str) = 0) and (not EOF(SrcFile)) do
        begin
          TableElement.Append;
          DelimeterPos := Pos(',', Str);
          ID_Cell := StrToInt(Copy(Str, 1, DelimeterPos - 1));
          TableElement.Fields[0].AsInteger := ID_Cell;
          TableElement.Post;
          for I := 1 to 8 do
          begin
            TableLink.Append;
            TableLink.Fields[0].AsInteger := 1; // MatrixCount
            TableLink.Fields[1].AsInteger := ID_Cell;

            Str := Copy(Str, DelimeterPos + 1, Length(Str));
            DelimeterPos := Pos(',', Str);
            if DelimeterPos = 0 then
              DelimeterPos := Length(Str) + 1;

            TableLink.Fields[2].AsString := Copy(Str, 1, DelimeterPos - 1);
            TableLink.Fields[3].AsInteger := VertexNo[I];
            TableLink.Post;
          end;
          ReadLn(SrcFile, Str);
        end;
        TableElement.Close;
        TableLink.Close;
      finally
        TableLink.Free;
      end;
      Query := TQuery.Create(nil);
      try
        Query.SQL.Add('SELECT E.' + FldID_ELEMENT + ' ' + FldID_ELEMENT +
          ', AVG(N.X) X, AVG(N.Y) Y, AVG(N.Z) Z');
        Query.SQL.Add(Format('FROM "%s" E, "%s" L, "%s" N',
          [ChangeModelTable(DirMesh3D, DirMesh3DElement, OutModelName),
          OutModelName, ChangeModelTable(DirMesh3D, DirMesh3DNode,
          OutModelName)]));
        Query.SQL.Add('WHERE E.' + FldID_ELEMENT + '=L.' + FldID_ELEMENT +
          ' AND L.' + FldID_NODE + '=N.' + FldID_NODE);
        Query.SQL.Add('GROUP BY E.' + FldID_ELEMENT);
        try
          Query.ExecSQL;
        except
          on E: Exception do
          begin
            ShowMessage('Error "' + E.Message + '" with SQL:'#13 + Query.Text);
            raise;
          end;
        end;
        BatMove := TBatchMove.Create(nil);
        try
          BatMove.Source := Query;
          BatMove.Destination := TableElement;
          BatMove.Mode := BatCopy;
          BatMove.Execute;
        finally
          BatMove.Free;
        end;
      finally
        Query.Free;
      end;
    finally
      TableElement.Free;
    end;
  finally
    CloseFile(SrcFile);
  end;
  if Result <> IeOk then
    Result := IeOk;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.GRD_To_Grid2D;
var
  F: System.Text;
  K: Longint;
  NR: Integer;
  Code: Integer;
  S: string[32];
  LineString: string;
  X, Y: Single;
  XO, YO, ZO, XE, YE, ZE, DX, DY, DZ: Single;
  NX, NY, NZ: Integer;
  Value: Single;
  NODATA_value: Single;

begin
  with dmBase do
  begin
    NR := 0;
    System.Assign(F, InFile);
    Reset(F);
    while True do
    begin
      ReadLn(F);
      if EOF(F) then
        Break;
      Inc(NR);
    end;
    ProgressBar.Min := 0;
    ProgressBar.StepIt;
    ProgressBar.Max := NR;
    ProgressBar.Position := 0;
    if NR = 0 then
      Exit;
    CreateGridTables(OutModelName); // with default ID, X, Y, Z fields
    TableImport.TableName := FOutModelName;
    TableImport.Open;
    TableImport.First;

    Reset(F);
    ReadLn(F, LineString);
    if LineString = 'DSAA' then // Surfer grid
    begin
      // 50 39        - NX and NY
      // 0 9          - XO and XE
      // 0 7          - YO and YE
      // 25 104.262   - ZO and ZE
      ReadLn(F, LineString);
      S := Copy(LineString, 1, Pos(' ', LineString) - 1);
      Trim(S);
      Val(S, NX, Code);
      Delete(LineString, 1, Length(S) + 1);
      S := LineString;
      Trim(S);
      Val(S, NY, Code);

      ReadLn(F, LineString);
      S := Copy(LineString, 1, Pos(' ', LineString) - 1);
      Trim(S);
      System.Val(S, Value, Code);
      if Code = 0 then
        XO := Value;
      Delete(LineString, 1, Length(S) + 1);
      S := LineString;
      Trim(S);
      Val(S, Value, Code);
      if Code = 0 then
        XE := Value;

      ReadLn(F, LineString);
      S := Copy(LineString, 1, Pos(' ', LineString) - 1);
      Trim(S);
      Val(S, Value, Code);
      if Code = 0 then
        YO := Value;
      Delete(LineString, 1, Length(S) + 1);
      S := LineString;
      Trim(S);
      Val(S, Value, Code);
      if Code = 0 then
        YE := Value;

      ReadLn(F, LineString);
      S := Copy(LineString, 1, Pos(' ', LineString) - 1);
      Trim(S);
      Val(S, Value, Code);
      if Code = 0 then
        ZO := Value;
      Delete(LineString, 1, Length(S) + 1);
      S := LineString;
      Trim(S);
      Val(S, Value, Code);
      if Code = 0 then
        ZE := Value;

      DX := (XE - XO) / NX;
      DY := (YE - YO) / NY;
    end
    else // ArcInfo Grid
    begin
      { e.g.
        // ncols         200
        // nrows         225
        // xllcorner     2667000
        // yllcorner     6033000
        // cellsize      40
        // NODATA_value  -9999
      }
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, NX, Code);

      ReadLn(F, LineString);
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, NY, Code);

      ReadLn(F, LineString);
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, XO, Code);

      ReadLn(F, LineString);
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, YO, Code);

      ReadLn(F, LineString);
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, DX, Code);
      DY := DX;

      ReadLn(F, LineString);
      Delete(LineString, 1, 14);
      S := LineString;
      Trim(S);
      Val(S, NODATA_value, Code);
    end;
    ZO := 0;
    NZ := 1;
    DZ := 1;

    WriteParFile(OutModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);

    // Grid Z values
    K := 0;
    while not EOF(F) do
    begin
      TableImport.Edit;
      ProgressBar.Position := K;
      X := XO;
      Y := YO;
      ReadLn(F, LineString);
      while (LineString <> '') do
      begin
        X := XO + (K mod NX) * DX;
        Y := YO + DY * (K div NX);
        S := Copy(LineString, 1, Pos(' ', LineString) - 1);
        Trim(S);
        Val(S, Value, Code);
        if (Value <> NODATA_value) then
        begin
          if (Value > -1E10) and (Value < 1E10) then
          begin
            // Value := ZO;
            TableImport.Append;
            TableImport.FieldByName(FldID).AsInteger := K + 1;
            TableImport.FieldByName(FldX).AsFloat := RoundTo(X, -2);
            TableImport.FieldByName(FldY).AsFloat := RoundTo(Y, -2);
            TableImport.FieldByName(FldZ).AsFloat := RoundTo(Value, -2);
            TableImport.Post;
          end;
        end;
        Delete(LineString, 1, Length(S) + 1);
        Inc(K);
      end;
    end; // while Eof
    System.Close(F);
    TableImport.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.KDR_To_Poly(FileNameKDR, TableNamePoly: TFileName);
const
  WaitName = 1;
  WaitXYZ = 2;
var
  KDRFile: TStringList;
  X, Y, Z: Double;
  I: Integer;

  TablePolyFace, TablePolyVertex: TTable;
  PolyID: Integer;
  PolyName: string;
  PolyX, PolyY, PolyZ: Double;

  VertexID: Integer;
  VertexNo: Integer;
  S: Integer;

begin
  ProgressBar.Min := 0;
  ProgressBar.Position := 0;
  CreatePolygonTables(TableNamePoly);
  TablePolyFace := TTable.Create(Application);
  TablePolyFace.TableName := TableNamePoly;
  TablePolyVertex := TTable.Create(Application);
  TablePolyVertex.TableName := ChangeModelTable(DirPolygonPoly,
    DirPolygonVertex, TableNamePoly);
  try
    TablePolyFace.Open;
    TablePolyVertex.Open;

    KDRFile := TStringList.Create;
    try
      KDRFile.LoadFromFile(FileNameKDR);
      PolyName := '';
      PolyID := 0;
      PolyX := 0;
      PolyY := 0;
      PolyZ := 0;
      VertexID := 1;
      VertexNo := 1;
      ProgressBar.Max := KDRFile.Count - 1;
      S := WaitName;
      for I := 0 to KDRFile.Count - 1 do
      begin
        ProgressBar.Position := I;
        if (KDRFile[I][1] = '*') then
          S := WaitName
        else
          case S of
            WaitName:
              begin
                S := WaitXYZ;
                PolyName := UpperCase(KDRFile[I]);
                Dec(VertexNo);
                if PolyID > 0 then
                  try
                    TablePolyFace.Edit;
                    TablePolyFace.FieldByName(FldX).AsFloat :=
                      RoundTo(PolyX / Max(1, VertexNo), -2);
                    TablePolyFace.FieldByName(FldY).AsFloat :=
                      RoundTo(PolyY / Max(1, VertexNo), -2);
                    TablePolyFace.FieldByName(FldZ).AsFloat :=
                      RoundTo(PolyZ / Max(1, VertexNo), -2);
                    TablePolyFace.Post;
                  except
                  end;
                VertexNo := 1;
                Inc(PolyID);
                TablePolyFace.Append;
                TablePolyFace.FieldByName(FldID).AsInteger := PolyID;
                TablePolyFace.FieldByName(FldID_TYPE).AsInteger :=
                  Integer(PtPolyline);
                TablePolyFace.FieldByName(FldMATERIAL).AsInteger := 0;
                TablePolyFace.FieldByName(FldCODE).AsString := PolyName;
                TablePolyFace.Post;
                PolyX := 0;
                PolyY := 0;
                PolyZ := 0;
              end;
            WaitXYZ:
              try
                StrToXYZ(Trim(KDRFile[I]), X, Y, Z);
                PolyX := PolyX + X;
                PolyY := PolyY + Y;
                PolyZ := PolyZ + Z;
                TablePolyVertex.Append;
                TablePolyVertex.FieldByName(FldID).AsInteger := VertexID;
                TablePolyVertex.FieldByName(FldID_POLY).AsInteger := PolyID;
                TablePolyVertex.FieldByName(FldID_NO).AsInteger := VertexNo;
                TablePolyVertex.FieldByName(FldX).AsFloat := X;
                TablePolyVertex.FieldByName(FldY).AsFloat := Y;
                TablePolyVertex.FieldByName(FldZ).AsFloat := Z;
                TablePolyVertex.Post;
                Inc(VertexNo);
                Inc(VertexID);
              except
              end;
          end;
      end;
      Dec(VertexNo);
      if PolyID > 0 then
        try
          TablePolyFace.Edit;
          TablePolyFace.FieldByName(FldX).AsFloat :=
            RoundTo(PolyX / Max(1, VertexNo), -2);
          TablePolyFace.FieldByName(FldY).AsFloat :=
            RoundTo(PolyY / Max(1, VertexNo), -2);
          TablePolyFace.FieldByName(FldZ).AsFloat :=
            RoundTo(PolyZ / Max(1, VertexNo), -2);
          TablePolyFace.Post;
        except
        end;
    finally
      KDRFile.Free;
    end;
    TablePolyVertex.Close;
    TablePolyFace.Close;
  finally
    TablePolyVertex.Free;
    TablePolyFace.Free;
  end;
end;

// -----------------------------------------------------------------------------

{ ****************************************** }
{ Imports data from a MIF file to DB table }
{ All MapInfo datatypes will be saved in }
{ a table of GB's POLYGONS dataset }
{ ****************************************** }
procedure TfmFileImport.MIF_To_Poly(FileNameMIF, FileNamePoly: TFileName);
type
  TImportMode = (ImXY, ImXZ, ImYZ);
var
  TablePolyFace, TablePolyVertex: TTable;
  PolyID: Integer;
  PolyName: string;
  PolyType: TPolygonType;
  PolyText: string;
  Poly_X, Poly_Y, Poly_Z: Float;

  VertexID: Integer;
  VertexNo: Integer;

  MIFFile: TTextStream;
  MIFNameExt: TFileName;

  FileNameMID: TFileName;
  FileMID: Textfile;

  ColumnsNames: TStrings;
  ColumnsValues: TStrings;
  ColumnsCount: Integer;
  ImportMode: TImportMode;
  WarningMessage: string;

  { sub }
  procedure WriteValues;
  var
    I: Integer;
  begin
    for I := 0 to ColumnsCount - 2 do
    begin
      try
        TablePolyFace.FieldByName(ColumnsNames[I]).AsString := ColumnsValues[I]
      except
      end;
    end;
  end;

{ sub }
  function GetPolyName: string;
  begin
    Result := Format('%s(%d):%s', [MIFNameExt, MIFFile.CRCount, PolyName]);
  end;

{ sub }
  procedure ReadStyleText(Tag: string = '');
  var
    S: string;
  begin
    S := Trim(MIFFile.ReadLine);
    if Tag <> '' then
      PolyText := PolyText + Format('<%s>%s<\%0:s>', [Tag, S])
    else
      PolyText := PolyText + S;
  end;

{ sub }
  function FieldByNameAsFloat(const FieldName: string): Double;
  var
    I: Integer;
    S: string;
  begin
    S := '';
    for I := 0 to ColumnsCount - 1 do
    begin
      if CompareText(ColumnsNames[I], FieldName) = 0 then
      begin
        S := Trim(ColumnsValues[I]);
      end;
    end;
    Result := 0;
    try
      if S <> '' then
        Result := StrToFloat(S)
    except
    end;
  end;

{ sub }
  procedure BeginWritePoly;
  begin
    Inc(PolyID);
    VertexNo := 0;
    Poly_X := 0;
    Poly_Y := 0;
    Poly_Z := 0;
    PolyText := '';
    TablePolyFace.Append;
    WriteValues;
    TablePolyFace.FieldByName(FldID).AsInteger := PolyID;
    TablePolyFace.FieldByName(FldID_TYPE).AsInteger := Integer(PolyType);
    TablePolyFace.FieldByName(FldMATERIAL).AsInteger := 0;
    TablePolyFace.FieldByName(FldNAME).AsString := GetPolyName;
    // TablePolyFace[StartNo+0..I].AsString:=TablePolyAttr[0..I].AsString
    TablePolyFace.Post;
  end;

{ sub }
  procedure EndWritePoly;
  begin
    TablePolyFace.Edit;
    TablePolyFace.FieldByName(FldX).AsFloat :=
      RoundTo(Poly_X / Max(1, VertexNo), -2);
    TablePolyFace.FieldByName(FldY).AsFloat :=
      RoundTo(Poly_Y / Max(1, VertexNo), -2);
    TablePolyFace.FieldByName(FldZ).AsFloat :=
      RoundTo(Poly_Z / Max(1, VertexNo), -2);
    TablePolyFace.FieldByName('MIF_TEXT').AsString := PolyText;
    TablePolyFace.Post;
  end;

{ sub }
  procedure ReadVertex;
  var
    X, Y, Z: Double;
  begin
    X := 0;
    Y := 0;
    Z := 0;
    case ImportMode of
      ImXY:
        begin
          X := StrToVal(MIFFile.ReadItem);
          Y := StrToVal(MIFFile.ReadItem);
          Z := FieldByNameAsFloat(FldZ);
        end;
      ImXZ:
        begin
          X := StrToVal(MIFFile.ReadItem);
          Y := FieldByNameAsFloat(FldY);
          Z := StrToVal(MIFFile.ReadItem);
        end;
      ImYZ:
        begin
          X := FieldByNameAsFloat(FldX);
          Y := StrToVal(MIFFile.ReadItem);
          Z := StrToVal(MIFFile.ReadItem);
        end;
    end;
    Poly_X := Poly_X + X;
    Poly_Y := Poly_Y + Y;
    Poly_Z := Poly_Z + Z;
    Inc(VertexNo);
    Inc(VertexID);

    TablePolyVertex.Append;
    TablePolyVertex.FieldByName(FldID).AsInteger := VertexID;
    TablePolyVertex.FieldByName(FldID_NO).AsInteger := VertexNo;
    TablePolyVertex.FieldByName(FldID_POLY).AsInteger := PolyID;
    TablePolyVertex.FieldByName(FldX).AsFloat := X;
    TablePolyVertex.FieldByName(FldY).AsFloat := Y;
    TablePolyVertex.FieldByName(FldZ).AsFloat := Z;
    TablePolyVertex.Post;
  end;

{ sub }
  procedure ReadNone;
  begin
    PolyType := PtNone;
    BeginWritePoly;
    EndWritePoly;
  end;

{ sub }
  procedure ReadPoint;
  begin
    PolyType := PtSymbol;
    BeginWritePoly;
    ReadVertex;
    ReadStyleText('SYMBOL');
    EndWritePoly;
  end;

{ sub }
  procedure ReadLine;
  begin
    PolyType := PtPolyline;
    BeginWritePoly;
    ReadVertex;
    ReadVertex;
    ReadStyleText('PEN');
    EndWritePoly;
  end;

{ sub }
  procedure ReadPLine;
  var
    VertNumber: Integer;
  begin
    PolyType := PtPolyline;
    BeginWritePoly;
    VertNumber := StrToInt(MIFFile.ReadLine);
    for VertNumber := VertNumber downto 1 do
      ReadVertex;
    ReadStyleText('PEN');
    EndWritePoly;
  end;

{ sub }
  procedure ReadArc;
  begin
    PolyType := PtArc;
    BeginWritePoly;
    ReadVertex;
    ReadVertex;
    ReadStyleText('ANGLES');
    ReadStyleText('PEN');
    EndWritePoly;
  end;

{ sub }
  procedure ReadEllipse;
  begin
    PolyType := PtEllipse;
    BeginWritePoly;
    ReadVertex;
    ReadVertex;
    ReadStyleText('PEN');
    ReadStyleText('BRUSH');
    EndWritePoly;
  end;

{ sub }
  procedure ReadRoundRect;
  begin
    PolyType := PtRoundRect;
    BeginWritePoly;
    ReadVertex;
    ReadVertex;
    ReadStyleText('RADIUS');
    ReadStyleText('PEN');
    ReadStyleText('BRUSH');
    EndWritePoly;
  end;

{ sub }
  procedure ReadRect;
  begin
    PolyType := PtRect;
    BeginWritePoly;
    ReadVertex;
    ReadVertex;
    ReadStyleText('PEN');
    ReadStyleText('BRUSH');
    EndWritePoly;
  end;

{ sub }
  procedure ReadMIFPolygon; // Simple Polygon don`t Region
  var
    VertNumber: Integer;
  begin
    PolyType := PtPolygon;
    BeginWritePoly;
    VertNumber := StrToInt(MIFFile.ReadLine);
    for VertNumber := VertNumber downto 1 do
      ReadVertex;
    EndWritePoly;
  end;

{ sub }
  procedure WritePolyText(APolyText: string);
  begin
    TablePolyFace.Edit;
    TablePolyFace.FieldByName('MIF_TEXT').AsString := APolyText;
    TablePolyFace.Post;
  end;

{ sub }
  procedure ReadRegion;
  var
    I, PolygonNumber: Integer;
    CommonPolyName: string;
  begin
    PolyType := PtPolygon;
    PolygonNumber := StrToInt(MIFFile.ReadLine);
    if PolygonNumber > 1 then
      WarningMessage := WarningMessage +
        Format('Неразрезан полигон, строка %d'#13#10, [MIFFile.LineIndex]);
    CommonPolyName := PolyName;
    for I := 1 to PolygonNumber do
    begin
      PolyName := CommonPolyName + IntToStr(PolygonNumber) + '.' + IntToStr(I);
      ReadMIFPolygon;
    end;
    ReadStyleText('PEN');
    ReadStyleText('BRUSH');
    ReadStyleText('CENTER');
    TablePolyFace.RecNo := TablePolyFace.RecNo - (PolygonNumber - 1);
    for I := 1 to PolygonNumber do
    begin
      WritePolyText(PolyText);
      TablePolyFace.Next;
    end;
  end;

{ sub }
  procedure ReadText;
  var
    PPolyText: PChar;
  begin
    PolyType := PtText;
    BeginWritePoly;
    PolyText := Trim(MIFFile.ReadLine);
    PPolyText := PChar(PolyText);
    if PolyText[1] = '"' then
      PolyText := AnsiExtractQuotedStr(PPolyText, '"');
    PolyText := StringReplace(PolyText, '\n', #13, [RfReplaceAll]);
    ReadVertex;
    ReadVertex;
    ReadStyleText('FONT');
    ReadStyleText('ANGLE');
    EndWritePoly;
  end;

{ sub }
  procedure ReadValues;
  var
    S: string;
    I: Integer;
  begin
    ReadLn(FileMID, S);
    S := StringReplace(S, ',', #13, [RfReplaceAll]);
    ColumnsValues.Text := S;
    for I := 0 to ColumnsValues.Count - 1 do
      ColumnsValues[I] := ExtractCharQuotedStr(ColumnsValues[I], '"');
  end;

{ sub }
  procedure ReadData;
  const
    PolyTypeStrings = 'NONE      ' + 'POINT     ' + 'LINE      ' + 'PLINE     '
      + 'ARC       ' + 'ELLIPSE   ' + 'ROUNDRECT ' + 'RECT      ' + 'REGION    '
      + 'TEXT      ';
  begin
    PolyID := 0;
    PolyName := '';
    VertexID := 1;

    while not MIFFile.EOF do
      try
        PolyName := MIFFile.ReadItem;
        if Pos(UpperCase(PolyName), PolyTypeStrings) > 0 then
          ReadValues;
        case Pos(UpperCase(PolyName), PolyTypeStrings) of
          1:
            ReadNone;
          11:
            ReadPoint;
          21:
            ReadLine;
          31:
            ReadPLine;
          41:
            ReadArc;
          51:
            ReadEllipse;
          61:
            ReadRoundRect;
          71:
            ReadRect;
          81:
            ReadRegion;
          91:
            ReadText;
        end;
      except
      end;
  end;

{ sub }
  procedure ReadColumns;
  const
    FieldTypeStr: array [1 .. 7] of string = ('CHAR', 'INTEGER', 'SMALLINT',
      'FLOAT', 'DECIMAL', 'DATE', 'LOGICAL');
    FieldTypes: array [1 .. 7] of TFieldType = (FtString, FtInteger, FtSmallint,
      FtFloat, FtFloat, FtDateTime, FtBoolean);
  var
    I: Integer;
    J: Integer;
    Name, S: string;
    FieldType: TFieldType;
    FieldSize: Integer;
    ErrorCode: Integer;
    CheckMode: Boolean;
  begin
    ImportMode := ImXY;
    S := MIFFile.ReadItem; // columns
    S := MIFFile.ReadItem; // count
    ColumnsCount := StrToInt(S);
    CheckMode := True;
    for I := 1 to ColumnsCount do
    begin
      FieldSize := 0;
      Name := Trim(MIFFile.ReadItem);
      ColumnsNames.Add(Name);
      S := UpperCase(Trim(MIFFile.ReadLine));
      if CheckMode and (CompareText(Name, FldX) = 0) then
      begin
        ImportMode := ImYZ;
        CheckMode := False;
      end
      else if CheckMode and (CompareText(Name, FldY) = 0) then
      begin
        ImportMode := ImXZ;
        CheckMode := False;
      end
      else if CheckMode and (CompareText(Name, FldZ) = 0) then
      begin
        ImportMode := ImXY;
        CheckMode := False;
      end
      else
      begin
        FieldType := FieldTypes[1];
        for J := 1 to 7 do
        begin
          if Pos(FieldTypeStr[J], S) > 0 then
          begin
            Delete(S, 1, 5);
            Val(S, FieldSize, ErrorCode);
            FieldType := FieldTypes[J];
          end;
        end;
        if FieldType = FieldTypes[1] then
          FieldSize := Max(1, FieldSize);
        AddTableField(TablePolyFace.TableName, Name, FieldType, FieldSize);
      end;
    end;
  end;

begin // body of MifToPoly
  { I- }
  MIFNameExt := NameOnly(FileNameMIF);
  FileNameMID := ChangeFileExt(FileNameMIF, '.mid');
  AssignFile(FileMID, FileNameMID);
  FileMode := 0; // Set file access to read only
  System.Reset(FileMID);

  try
    TablePolyFace := TTable.Create(Application);
    try
      TablePolyFace.TableName := FileNamePoly;
      CreatePolygonTables(FileNamePoly);
      AddTableField(FileNamePoly, 'MIF_TEXT', FtString, 120);

      TablePolyVertex := TTable.Create(Application);
      try
        TablePolyVertex.TableName := ChangeModelTable(DirPolygonPoly,
          DirPolygonVertex, FileNamePoly);
        TablePolyVertex.Open;

        MIFFile := TTextStream.Create(FileNameMIF, FmOpenRead or
          FmShareDenyWrite);
        try
          ColumnsNames := TStringList.Create;
          try
            ColumnsValues := TStringList.Create;
            try
              MIFFile.Delimiters := #9#10#13' ';
              MIFFile.InitProgress;
              MIFFile.OnCRCountChange := MIFFile.DefaultChangeCRCount;
              if not MIFFile.EOF then
                MIFFile.ReadLine; // Version 300
              if not MIFFile.EOF then
                MIFFile.ReadLine; // Charset "WindowsCyrillic"
              if not MIFFile.EOF then
                MIFFile.ReadLine; // Delimiter ","
              if not MIFFile.EOF then
                MIFFile.ReadLine;
              // CoordSys NonEarth Units "m" Bounds (120000, -500) (125000, 500)
              if not MIFFile.EOF then
                ReadColumns; // Columns 7

              TablePolyFace.Open;

              if not MIFFile.EOF then
                MIFFile.ReadLine; // Data
              if not MIFFile.EOF then
                ReadData;
            finally
              ColumnsValues.Free;
            end;
          finally
            ColumnsNames.Free;
          end;
        finally
          MIFFile.Free;
        end;
        TablePolyVertex.Close;
      finally
        TablePolyVertex.Free;
      end;
      TablePolyFace.Close;
    finally
      TablePolyFace.Free;
    end;
  finally
    CloseFile(FileMID);
  end;
  { I+ }
  WriteParFile(OutModelName, 0, 0, 0, 15, 15, 15, 100, 100, 20);
  if WarningMessage <> '' then
    ShowMessage(WarningMessage);
end;

{
  type
  TFileOfChar = File of Char;

  procedure ReadStrLn(var F : TFileOfChar; Str : String);
  var
  Ch : Char;
  begin
  Str:='';
  if EOF(F) then Exit else Read(F,Ch);
  while (Ch<>#13) do
  begin
  if Ch<>#10 then Str:=Str+Ch;
  if EOF(F) then Exit else Read(F,Ch);
  end;
  if EOF(F) then Exit else Read(F,Ch);
  if Ch<>#10 then Seek(F,FilePos(F)-1);
  end;
}

// -----------------------------------------------------------------------------
// Import from DAT files of Surfer to Points2D dataset
// -----------------------------------------------------------------------------
procedure TfmFileImport.DAT_To_Points2D;
var
  F: Textfile;
  Fch: Textfile;
  I, K: Longint;
  J: Word;
  FieldNames, FieldTypes: TNamesArray;
  NNames, NFields: Word;
  NR: Integer;
  Code: Integer;
  Doub: Double;
  S: string;
  Sl: string;
  AFieldTypes: TFieldType;

begin
  with dmBase do
  begin
    NR := 0;
    AssignFile(F, InFile);
    Reset(F);
    while True do
    begin
      ReadLn(F);
      if EOF(F) then
        Break;
      Inc(NR);
    end;
    ProgressBar.Min := 0;
    ProgressBar.StepIt;
    ProgressBar.Max := NR;
    ProgressBar.Position := 0;
    System.Close(F);
    if NR = 0 then
      Exit;
    System.Assign(Fch, InFile);
    GetFieldsInfo(Fch, NNames, FieldNames, NFields, FieldTypes);
    CreatePoint2DTables(OutModelName); // with default ID, X, Y, Z fields
    if NFields > 3 then
    begin
      if NNames = 0 then // Without fieldnames in the first line
      begin
        for I := 4 to NFields do
          AddTableField(OutModelName, 'G' + IntToStr(I), FtFloat, 0);
      end
      else
      begin
        for I := 3 to NFields - 1 do
        begin
          AddTableField(OutModelName, ExtractCharQuotedStr(FieldNames[I], '"'),
            FtFloat, 0);
        end;
      end;
    end;
    if NFields = 0 then
    begin
      CloseFile(Fch);
      Exit;
    end;
    TableImport.TableName := OutModelName;
    TableImport.Open;
    TableImport.First;
    Reset(Fch);
    if NNames <> 0 then
      ReadLn(Fch, Sl);
    K := 0;
    while not EOF(Fch) do
    begin
      Inc(K);
      TableImport.Edit;
      ProgressBar.Position := K;
      ReadLn(Fch, Sl);

      TableImport.Append;
      TableImport.FieldByName(FldID).AsInteger := K;
      J := 1;
      for I := 1 to NFields do
      begin
        S := GetFieldTxt(Sl, J);
        if S <> '' then
        begin
          Val(S, Doub, Code);
          if Code = 0 then
            TableImport.Fields[I].AsFloat := RoundTo(Doub, -2);
        end;
      end;
      TableImport.Post;
    end; // while Eof
    System.Close(Fch);
    TableImport.Close;
  end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.MOD_To_Grid3D;
type
  TString2 = string[2];
var
  ParFileName: TFileName;
  ParametersFile: Twt4XParametersFile;
  F: System.Text;
  S: string;
  I, J, K, RecNo: Integer;
  NParcel: Integer;
  _MiningCAF, _ProcessingCAF: Double;
  _TotalTonnage: Double;
  // _RockTypeCode : String[4];
  // _ParscelTonnes: Double;
  ElementUnits: array of Double;

  // Compare to sort elements by order in input model file
  { sub } function Compare(Item1, Item2: Pointer): Integer;
  begin
    Result := Sign(TElement(Item1).PositionInModelFile - TElement(Item2)
      .PositionInModelFile);
  end;

begin
  ParametersFile := Twt4XParametersFile.Create(Self);
  with dmBase, dmDialogs do
    try
      ParFileName := SetExtention(OpenDialogImport.FileName, ParExt);
      ParametersFile.LoadFromFile(ParFileName);
      ParametersFile.Elements.Sort(@Compare);
      with ParametersFile do
      begin
        SetLength(ElementUnits, Elements.Count);
        try
          TableImport := TTable.Create(Self);
          try
            TableImport.TableName := OutModelName;
            WriteParFile(FOutModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);

            TableImport.FieldDefs.Clear;
            TableImport.FieldDefs.Add(FldID, FtFloat);
            TableImport.FieldDefs.Add(FldX, FtFloat);
            TableImport.FieldDefs.Add(FldY, FtFloat);
            TableImport.FieldDefs.Add(FldZ, FtFloat);
            TableImport.FieldDefs.Add('MININGCAF', FtFloat);
            TableImport.FieldDefs.Add('PROCESSINGCAF', FtFloat);
            TableImport.FieldDefs.Add('TOTALTONNAGE', FtFloat);
            for I := 0 to Elements.Count - 1 do
              TableImport.FieldDefs.Add(Elements[I].ElementTypeCode, FtFloat);
            TableImport.FieldDefs.Add(FldROCKTYPE, FtInteger);
            TableImport.CreateTable;
            TableImport.Open;
            AssignFile(F, InFile);
            Reset(F);
            ProgressBar.Min := 0;
            ProgressBar.Max := (FileSize(F) - 1) div 1024;
            RecNo := 0;
            while not EOF(F) do
            begin
              try
                ReadLn(F, S);
                ProgressBar.Position := FilePos(F) div 1024;
                if (Length(S) > 47) and (TString2(S) <> '-1') then
                begin
                  I := StrToInt(Copy(S, 2, -(2 - 5)));
                  J := StrToInt(Copy(S, 5, -(5 - 8)));
                  K := StrToInt(Copy(S, 8, -(8 - 11)));
                  NParcel := StrToInt(Copy(S, 12, -(12 - 14)));
                  _MiningCAF := StrToFloat(Copy(S, 17, -(17 - 27)));
                  _ProcessingCAF := StrToFloat(Copy(S, 28, -(28 - 38)));
                  _TotalTonnage := StrToFloat(Copy(S, 39, -(39 - 49)));
                  TableImport.Append;
                  try
                    Inc(RecNo);
                    TableImport[FldID] := RecNo;
                    TableImport[FldX] := XO + I * DX;
                    TableImport[FldY] := YO + J * DY;
                    TableImport[FldZ] := ZO + K * DZ;
                    TableImport['MININGCAF'] := _MiningCAF;
                    TableImport['PROCESSINGCAF'] := _ProcessingCAF;
                    TableImport['TOTALTONNAGE'] := _TotalTonnage;
                    for I := 0 to Elements.Count - 1 do
                      ElementUnits[I] := 0;
                    for I := 1 to NParcel do
                    begin
                      S := '';
                      if not EOF(F) then
                        ReadLn(F, S);
                      ProgressBar.Position := FilePos(F) div 1024;
                      for J := 0 to Elements.Count - 1 do
                        if Length(S) > (27 + J * 11) then
                          ElementUnits[J] := ElementUnits[J] +
                            StrToFloat(Copy(S, 28 + J * 11, 10));
                    end;
                    if NParcel > 0 then
                    begin
                      for I := 0 to Elements.Count - 1 do
                      begin
                        try
                          TableImport[Elements[I].ElementTypeCode] :=
                            ElementUnits[I];
                        except
                        end;
                      end;
                      if Length(S) > 14 then
                        TableImport[FldROCKTYPE] :=
                          RockList.IndexByName(Copy(S, 12, -(12 - 16)));
                    end;
                  finally
                    TableImport.Post;
                  end;
                end;
              except
                on EConvertError do;
              end;
            end;
          finally
            TableImport.Free;
          end;
        finally
          ElementUnits := nil;
        end;
      end;
    finally
      ParametersFile.Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.MSQ_To_Grid3D;
type
  TString2 = string[2];
var
  ParFileName: TFileName;
  ParametersFile: Twt4XParametersFile;
  TableImport: TTable;
  F: System.Text;
  S: string;
  I, J, K, RecNo: Integer;
  NParcel: Integer;
  _MiningCAF, _ProcessingCAF: Double;
  _TotalTonnage: Double;
  // _RockTypeCode : String[4];
  // _ParscelTonnes: Double;
  _SEQUENCE: Integer;
  ElementUnits: array of Double;

  { sub }
  function Compare(Item1, Item2: Pointer): Integer;
  // Sort elements by order in input model file }
  begin
    Result := Sign(TElement(Item1).PositionInModelFile - TElement(Item2)
      .PositionInModelFile);
  end;

begin
  ParametersFile := Twt4XParametersFile.Create(Self);
  with dmBase, dmDialogs do
    try
      ParFileName := SetExtention(OpenDialogImport.FileName, ParExt);
      ParametersFile.LoadFromFile(ParFileName);
      ParametersFile.Elements.Sort(@Compare);
      with ParametersFile do
      begin
        SetLength(ElementUnits, Elements.Count);
        try
          TableImport := TTable.Create(Self);
          try
            TableImport.TableName := OutModelName;
            WriteParFile(TableImport.TableName, XO, YO, ZO, DX, DY, DZ,
              NX, NY, NZ);
            TableImport.FieldDefs.Clear;
            TableImport.FieldDefs.Add(FldID, FtFloat, 0, False);
            TableImport.FieldDefs.Add(FldX, FtFloat, 0, False);
            TableImport.FieldDefs.Add(FldY, FtFloat, 0, False);
            TableImport.FieldDefs.Add(FldZ, FtFloat, 0, False);
            TableImport.FieldDefs.Add('MININGCAF', FtFloat, 0, False);
            TableImport.FieldDefs.Add('PROCESSINGCAF', FtFloat, 0, False);
            TableImport.FieldDefs.Add('TOTALTONNAGE', FtFloat, 0, False);
            for I := 0 to Elements.Count - 1 do
              TableImport.FieldDefs.Add(Elements[I].ElementTypeCode, FtFloat,
                0, False);
            TableImport.FieldDefs.Add(FldROCKTYPE, FtInteger, 0, False);
            TableImport.FieldDefs.Add(FldSEQUENCE, FtInteger, 0, False);
            TableImport.CreateTable;
            TableImport.Open;
            AssignFile(F, InFile);
            Reset(F);
            ProgressBar.Min := 0;
            ProgressBar.Max := (FileSize(F) - 1) div 1024;
            RecNo := 0;
            while not EOF(F) do
            begin
              try
                ReadLn(F, S);
                ProgressBar.Position := FilePos(F) div 1024;
                if (Length(S) > 47) and (TString2(S) <> '-1') then
                begin
                  I := StrToInt(Copy(S, 2, -(2 - 5)));
                  J := StrToInt(Copy(S, 5, -(5 - 8)));
                  K := StrToInt(Copy(S, 8, -(8 - 11)));
                  NParcel := StrToInt(Copy(S, 12, -(12 - 14)));
                  _MiningCAF := StrToFloat(Copy(S, 17, -(17 - 27)));
                  _ProcessingCAF := StrToFloat(Copy(S, 28, -(28 - 38)));
                  _TotalTonnage := StrToFloat(Copy(S, 39, -(39 - 49)));
                  if (Length(S) > 51) then
                    _SEQUENCE := StrToInt(Copy(S, 50, -(50 - 53)))
                  else
                    _SEQUENCE := -1;

                  TableImport.Append;
                  try
                    Inc(RecNo);
                    TableImport[FldID] := RecNo;
                    TableImport[FldX] := XO + I * DX;
                    TableImport[FldY] := YO + J * DY;
                    TableImport[FldZ] := ZO + K * DZ;
                    TableImport['MININGCAF'] := _MiningCAF;
                    TableImport['PROCESSINGCAF'] := _ProcessingCAF;
                    TableImport['TOTALTONNAGE'] := _TotalTonnage;
                    TableImport[FldSEQUENCE] := _SEQUENCE;
                    for I := 0 to Elements.Count - 1 do
                      ElementUnits[I] := 0;
                    for I := 1 to NParcel do
                    begin
                      S := '';
                      if not EOF(F) then
                        ReadLn(F, S);
                      ProgressBar.Position := FilePos(F) div 1024;
                      for J := 0 to Elements.Count - 1 do
                        if Length(S) > (27 + J * 11) then
                          ElementUnits[J] := ElementUnits[J] +
                            StrToFloat(Copy(S, 28 + J * 11, 10));
                    end;
                    if NParcel > 0 then
                    begin
                      for I := 0 to Elements.Count - 1 do
                      begin
                        try
                          TableImport[Elements[I].ElementTypeCode] :=
                            ElementUnits[I];
                        except
                        end;
                      end;
                      if Length(S) > 11 then
                        TableImport[FldROCKTYPE] :=
                          RockList.IndexByName(Copy(S, 12, -(12 - 16)));
                    end;
                  finally
                    TableImport.Post;
                  end;
                end;
              except
                on EConvertError do;
              end;
            end;
          finally
            TableImport.Free;
          end;
        finally
          ElementUnits := nil;
        end;
      end;
    finally
      ParametersFile.Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.ECO_Or_RES_To_Grid3D;
var
  ParFileName: TFileName;
  F1, F2: System.Text;
  K: Longint;
  Code: Integer;
  S: string;
  ID: Longint;
  X, Y, Z, Value, Doub: Float;
  XO, YO, ZO, DX, DY, DZ: Float;
  NX, NY, NZ, SEQUENCE: Integer;
  NXNY, IZM1, IXIY, IYM1, IX, IY, IZ: Longint;
begin
  with dmBase do
  begin
    XO := 0;
    YO := 0;
    ZO := 0;
    DX := 10;
    DY := 10;
    DZ := 10;
    NX := 10;
    NY := 10;
    NZ := 10;

    ProgressBar.Min := 0;
    ProgressBar.StepIt;
    ProgressBar.Max := 1; // TableImport.RecordCount;
    ProgressBar.Position := 0;
    CreateMineSequenceTable;
    ParFileName := ExtractFilePath(InFile) + NameOnly(ExtractFileName(InFile)
      ) + '.MPA';
    if FileExists(ParFileName) then
    begin
      System.Assign(F2, ParFileName); // Read MPA file
      System.Reset(F2);
      while not EOF(F2) do
      begin
        ReadLn(F2, S);
        case S[3] of
          '1':
            begin // Linetype 1
              Val(Copy(S, 6, 10), Doub, Code);
              if Code = 0 then
                DX := Doub;
              Val(Copy(S, 16, 10), Doub, Code);
              if Code = 0 then
                DY := Doub;
              Val(Copy(S, 26, 10), Doub, Code);
              if Code = 0 then
                DZ := Doub;

              Val(Copy(S, 36, 10), Doub, Code);
              if Code = 0 then
                XO := Doub;
              Val(Copy(S, 46, 10), Doub, Code);
              if Code = 0 then
                YO := Doub;
              Val(Copy(S, 56, 10), Doub, Code);
              if Code = 0 then
                ZO := Doub;
            end;
          '2':
            begin // Linetype 2
              Val(Copy(S, 6, 10), Doub, Code);
              if Code = 0 then
                NX := Round(Doub);
              Val(Copy(S, 16, 10), Doub, Code);
              if Code = 0 then
                NY := Round(Doub);
              Val(Copy(S, 26, 10), Doub, Code);
              if Code = 0 then
                NZ := Round(Doub);
            end;
        end; { case }
      end;
      System.Close(F2);
    end
    else
    begin
      MessageDlg(ExtractFileName(ParFileName) + ' ???', MtError, MbOKCancel, 0);
      Exit;
    end;
    System.Assign(F1, InFile);
{$I-}
    Reset(F1);
    TableImport.Open;
    TableImport.First;
    K := 0;
    S := '';
    NXNY := NX * NY;
    while not EOF(F1) do
    begin
      Inc(K);
      TableImport.Edit;
      ReadLn(F1, S);
      Trim(S);
      ProgressBar.Position := K;
      Val(Copy(S, 11, 20), Value, Code);
      if Value = 0 then
        Continue;
      if Value < 0 then
        Value := 0;
      if Code = 0 then
        TableImport.FieldByName(FldG).AsCurrency := Value;
      Val(Copy(S, 31, 10), SEQUENCE, Code);
      if Code = 0 then
        TableImport.FieldByName(FldSEQUENCE).AsInteger := SEQUENCE;

      Val(Copy(S, 1, 10), ID, Code);
      if Code <> 0 then
        Continue;
      IZM1 := Trunc((ID - 1) / NXNY);
      IXIY := (Trunc(ID - 1) - IZM1 * NXNY);
      IYM1 := Trunc(IXIY / NX);
      IX := (IXIY - IYM1 * NX);
      IY := (IYM1);
      IZ := (IZM1);
      X := XO + IX * DX + DX / 2;
      TableImport.FieldByName(FldX).AsFloat := RoundTo(X, -2);
      Y := YO + IY * DY + DY / 2;
      TableImport.FieldByName(FldY).AsFloat := RoundTo(Y, -2);
      Z := ZO + IZ * DZ + DZ / 2;
      TableImport.FieldByName(FldZ).AsFloat := RoundTo(Z, -2);
      // ID:=IX+(NY-(IY-1))*NX+(IZ-1)*NX*NY;
      // TableImport.FieldByName(fldID).asFloat:=K;
      TableImport.Append;
    end; { while Eof }
    WriteParFile(OutModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
    System.Close(F1);
    TableImport.Close;
  end;
end;

// -----------------------------------------------------------------------------
procedure TfmFileImport.EAS_To_Points2D;
var
  F: Textfile;
  S, St: string;
  I, J, N, Ecode, P, A: Integer;
  FieldNames: array [1 .. 20] of string;
  NoZAttr: Boolean;
  Table: TTable;
  R: Double;
begin
  AssignFile(F, InFile);
  Reset(F);
  ReadLn(F, S);
  while not EOF(F) do
  begin
    Val(S, N, Ecode);
    if Ecode = 0 then
      Break;
    ReadLn(F, S);
  end;
  if not EOF(F) then
  begin
    I := 1;
    while I <= N do
    begin
      ReadLn(F, S);
      S := Trim(S);
      // Change field name to correspond Paradox requirements
      for J := 1 to Length(S) do
        if S[J] in ForbiddenChars then
          S[J] := '_';
      for J := Low(ForbiddenWords) to High(ForbiddenWords) do
        if UpperCase(S) = ForbiddenWords[J] then
        begin
          S := S + '_';
          Break;
        end;
      FieldNames[I] := S;
      Inc(I);
    end;

    CreatePoint2DTables(OutModelName); // with default ID, X, Y, Z fields
    FieldNames[1] := FldX;
    FieldNames[2] := FldY;
    NoZAttr := not(UpCase(FieldNames[3][1]) = 'Z');
    if NoZAttr then
      A := 3
    else
    begin
      A := 4;
      FieldNames[3] := FldZ;
    end;
    for I := A to N do
      AddTableField(OutModelName, FieldNames[I], FtFloat);
    Table := TTable.Create(Self);
    Table.TableName := OutModelName;
    Table.Open;
    I := 1;
    while not EOF(F) do
    begin
      Table.Append;
      Table.FieldByName(FldID).Value := I;
      J := 1;
      while J <= N do
      begin
        Read(F, R);
        Table.FieldByName(FieldNames[J]).Value := R;
        Inc(J);
      end;
      ReadLn(F);
      if NoZAttr then
        Table.FieldByName(FldZ).Value := 0;
      Table.Post;
      Inc(I);
    end;
    Table.Close;
  end;
  CloseFile(F);
end;

// -----------------------------------------------------------------------------
procedure TfmFileImport.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      // dmDialogs.OpenDialogImport.FilterIndex := ReadInteger(Name, 'FilterIndex', 1);
      ToolButtonMap.Down := ReadBool(Name, ToolButtonMap.Caption, True);
      if ToolButtonMap.Down then
        ToolButtonMap.Click;
      ToolButtonTable.Down := ReadBool(Name, ToolButtonTable.Caption, False);
      if ToolButtonTable.Down then
        ToolButtonTable.Click;
      ToolButtonGraph.Down := ReadBool(Name, ToolButtonGraph.Caption, False);
      if ToolButtonGraph.Down then
        ToolButtonGraph.Click;
    finally
      IniFile.Free;
    end;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      // WriteInteger(Name, 'FilterIndex', dmDialogs.OpenDialogImport.FilterIndex);
      WriteBool(Name, ToolButtonMap.Caption, ToolButtonMap.Down);
      WriteBool(Name, ToolButtonTable.Caption, ToolButtonTable.Down);
      WriteBool(Name, ToolButtonGraph.Caption, ToolButtonGraph.Down);
    finally
      IniFile.Free;
    end;
end;

// -----------------------------------------------------------------------------
{ TTextStream }
function TTextStream.BOF: Boolean;
begin
  Result := Position = 0;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.CallCRCountChange;
begin
  if Assigned(OnCRCountChange) then
    OnCRCountChange(Self);
end;

// -----------------------------------------------------------------------------

constructor TTextStream.Create(const FileName: TFileName; Mode: Word);
begin
  Delimiters := #9#10#13' ';
end;

// -----------------------------------------------------------------------------

procedure TTextStream.DefaultChangeCRCount(Sender: TObject);
begin
  if (Sender is TTextStream) then
    SetProgressPosition((Sender as TTextStream).Position)
  else
    SetProgressPosition(Self.Position);
end;

// -----------------------------------------------------------------------------

function TTextStream.EOF: Boolean;
begin
  Result := Position = Size;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.First;
begin
  Seek(0, SoFromBeginning);
end;

// -----------------------------------------------------------------------------
procedure TTextStream.InitProgress;
begin
  fFileImport.InitProgress(0, 0, Size);
end;

// -----------------------------------------------------------------------------

function TTextStream.IsDelimeter(Ch: Char): Boolean;
begin
  Result := Pos(Ch, Delimiters) > 0;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.Last;
begin
  Seek(0, SoFromEnd);
end;

// -----------------------------------------------------------------------------

function TTextStream.LineIndex: Longint;
var
  OldPosition: Longint;
begin
  OldPosition := Position;
  try
    First;
    CRCount := 0;
    while Position < OldPosition do
      ReadChar;
    Result := CRCount;
  finally
    Position := OldPosition;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.Next;
begin
  if not EOF then
    Seek(1, SoFromCurrent);
end;

// -----------------------------------------------------------------------------

procedure TTextStream.Prior;
begin
  if not BOF then
    Seek(-1, SoFromCurrent);
end;

// -----------------------------------------------------------------------------

function TTextStream.ReadChar: Char;
begin
  if Read(Result, 1) = 0 then
    Result := #0;
  if Result = #13 then
    CRCount := CRCount + 1;
end;

// -----------------------------------------------------------------------------

function TTextStream.ReadItem: string;
var
  Ch: Char;
begin
  SkipDelimiters;
  Result := '';
  if EOF then
    Exit;
  Ch := ReadChar;
  repeat
    Result := Result + Ch;
    Ch := ReadChar;
  until EOF or IsDelimeter(Ch);
  if not IsDelimeter(Ch) then
    Result := Result + Ch;
end;

// -----------------------------------------------------------------------------

function TTextStream.ReadLine: string;
var
  OldDelimiters: string;
begin
  OldDelimiters := Delimiters;
  try
    Delimiters := #10#13;
    Result := ReadItem;
  finally
    Delimiters := OldDelimiters;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.SetCRCount(const Value: Integer);
begin
  if FCRCount <> Value then
  begin
    FCRCount := Value;
    CallCRCountChange;
  end;
end;

// -----------------------------------------------------------------------------

procedure TTextStream.SkipDelimiters;
var
  Ch: Char;
begin
  if EOF then
    Exit;
  repeat
    Ch := ReadChar;
  until EOF or (Pos(Ch, Delimiters) <= 0);
  if Pos(Ch, Delimiters) <= 0 then
    Prior;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.EditOutputNameChange(Sender: TObject);
begin
  FOutModelName := FOutputPath + EditOutputName.Text;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.ToolButtonShowAsClick(Sender: TObject);
begin
  ToolBarShowAs.Tag := (Sender as TToolButton).Tag;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.FormDestroy(Sender: TObject);
begin
  fFileImport.ProgressBar := nil;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TfmFileImport.ButtonOKClick(Sender: TObject);

var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := CrHourGlass;
  case ImportIndex of
    1:  KDR_To_Poly(InFile, OutModelName);
    2:  DAT_To_Points2D;
    3:  GRD_To_Grid2D;
    4, 5:
      ECO_Or_RES_To_Grid3D;
    6:  MOD_To_Grid3D;
    7:  MSQ_To_Grid3D;
    8:  DXF_To_Poly(InFile, OutModelName);
    9:  MIF_To_Poly(InFile, OutModelName);
    10: FeMapINP_Mesh3D(InFile, OutModelName);
    11: EAS_To_Points2D;
    12:
      ; // ArcInfo shape files
  end;
  Screen.Cursor := OldCursor;
end;

end.
