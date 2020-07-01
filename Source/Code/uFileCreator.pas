 //------------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------
{! The unit includes routines for common usage }

unit uFileCreator;

interface

uses
  Winapi.Windows, 
  Winapi.Messages,
  System.SysUtils,  
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.FileCtrl,
  //DB
  Data.DB, 
  Bde.DBTables,
  
  uGlobals,
  uCommon,
  uResStrings,
  uProfuns;

const
  dbAssays    = 0;
  dbCollars   = 1;
  dbInclins   = 2;
  dbLithology = 3;

procedure CreateNewTable(const Table: TTable);

//Database tables
function CreateAssaysTable(FileName: TFileName): boolean;
function CreateCollarsTable(FileName: TFileName): boolean;
function CreateInclinsTable(FileName: TFileName): boolean;
function CreateLithologyTable(FileName: TFileName): boolean;

//Model tables
procedure CreateHoleTables(FileName: TFileName);
procedure CreatePoint2DTables(FileName: TFileName);
procedure CreatePoint3DTables(FileName: TFileName);
procedure CreatePolygonTables(FileName: TFileName);
procedure CreateTinTables(FileName: TFileName);
procedure CreateSolidTables(FileName: TFileName);
procedure CreateGridTables(FileName: TFileName);
procedure CreateMesh2DTables(FileName: TFileName);
procedure CreateMesh3DTables(FileName: TFileName);

procedure CreateDrawingTables(FileName: TFileName);

//Variogram tables
procedure CreateExpVariogramTables(FileName: TFileName);
procedure CreateModelVariogramTables(FileName: TFileName);

//Legend, Pattern and Material tables
function CreateLegendTable(Table: TTable): boolean;
function DefaultLegendTable(Table: TTable; Min, Max: double): boolean;
function CreateMaterialTable(Table: TTable): boolean;
function CreatePatternTable(Table: TTable): boolean;
procedure CreatePolyBounds(FileName: TFileName; X0, Y0, Z0: double;
  DX, DY, DZ: double; NX, NY, NZ: integer);

//Copy, Rename and Delete files, associated with a table
procedure CopyFiles(const AOldName, ANewName: TFileName; ADatatype: integer;
  bFailIfExists: boolean);
procedure RenameFiles(const AOldName, ANewName: TFileName);
{! Deletes all files associated with the table }
procedure DeleteFiles(const AFileName: TFileName; AModeltype: integer);



//======================================================================
implementation
//======================================================================

var
  FileNameNew: TFileName;

//--------------------------------------------------------

procedure CreateNewTable(const Table: TTable);
begin
  ForceDirectories(ExtractFilePath(Table.TableName));
  DeleteFile(Table.TableName);
  Table.CreateTable;
end;

//=======================================================================\\
// DATABASE
//=======================================================================\\
//Assays

function CreateAssaysTable(FileName: TFileName): boolean;
var
  TableNew: TTable;
begin
  Result := True;
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + NameOnly(Filename) +
      '?', mtInformation, [mbYes, mbNo], 0) = mrNo then
    begin
      Result := False;
      Exit;
    end;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldDHOLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldSAMPLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldFROM, ftFloat);
    TableNew.FieldDefs.Add(fldTO, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    TableNew.FieldDefs.Add(fldCOMPONENT, ftFloat);
    TableNew.FieldDefs.Add(fldDENSITY, ftFloat);
    TableNew.FieldDefs.Add(fldORETYPE, ftInteger);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
end;

//Collars =============================================

function CreateCollarsTable(FileName: TFileName): boolean;
var
  TableNew: TTable;
begin
  Result := True;
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation, [mbYes, mbNo], 0) =
      mrNo then
    begin
      Result := False;
      Exit;
    end;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldDHOLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldPROFILE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldDEPTH, ftFloat);
    TableNew.FieldDefs.Add(fldSTART, ftDate);
    TableNew.FieldDefs.Add(fldFINISH, ftDate);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
end;

//Inclins =============================================

function CreateInclinsTable(FileName: TFileName): boolean;
var
  TableNew: TTable;
begin
  Result := True;
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
    begin
      Result := False;
      Exit;
    end;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldDHOLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldAZIMUTH, ftFloat);
    TableNew.FieldDefs.Add(fldINCLINATION, ftFloat);
    TableNew.FieldDefs.Add(fldDEPTH, ftFloat);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
end;

function CreateLithologyTable(FileName: TFileName): boolean;
var
  TableNew: TTable;
begin
  Result := True;
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + NameOnly(Filename) +
      '?', mtInformation, [mbYes, mbNo], 0) = mrNo then
    begin
      Result := False;
      Exit;
    end;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldDHOLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldSAMPLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldFROM, ftFloat);
    TableNew.FieldDefs.Add(fldTO, ftFloat);
    TableNew.FieldDefs.Add(fldROCKTYPE, ftInteger);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
end;


//=======================================================================\\
// MODELS
//=======================================================================\\

//Drill Holes ------------------------------------------------------------
procedure CreateHoleTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    TableNew.FieldDefs.Add(fldPROFILE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldDHOLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldSAMPLE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldORETYPE, ftInteger);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  WriteParFile(FileName, 0, 0, 0, 50, 50, 15, 10, 10, 10);
end;

//Points 2D ==================================================

procedure CreatePoint2DTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  WriteParFile(FileName, 0, 0, 0, 50, 50, 15, 10, 10, 10);
end;

//Points 3D ==================================================

procedure CreatePoint3DTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    TableNew.FieldDefs.Add(fldC, ftWord);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  WriteParFile(FileName, 0, 0, 0, 50, 50, 15, 10, 10, 10);
end;

//Polygons ===========================================

procedure CreatePolygonPolyTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldID_TYPE, ftInteger);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger); //???
    TableNew.FieldDefs.Add(fldCODE, ftString, 32, False);
    TableNew.FieldDefs.Add(fldNAME, ftString, 64, False);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreatePolygonVertexTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldID_POLY, ftInteger);
    TableNew.FieldDefs.Add(fldID_NO, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreatePolygonTables(FileName: TFileName);
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  CreatePolygonPolyTable(FileName);
  CreatePolygonVertexTable(ChangeModelTable(DirPolygonPoly, DirPolygonVertex,
    FileName));
  FileNameNew := FileName;
  //WritePolygonParFile(FileName,50,50,15,0,0,0,10,10,10);
end;

//Tins ==================================================

procedure CreateTinTriangleTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldV1, ftInteger);
    TableNew.FieldDefs.Add(fldV2, ftInteger);
    TableNew.FieldDefs.Add(fldV3, ftInteger);
    TableNew.FieldDefs.Add(fldN1, ftInteger);
    TableNew.FieldDefs.Add(fldN2, ftInteger);
    TableNew.FieldDefs.Add(fldN3, ftInteger);
    TableNew.FieldDefs.Add(fldAREA, ftFloat);
    TableNew.FieldDefs.Add(fldSLOPE, ftFloat);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateTinVertexTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldID_TRIANGLE, ftInteger);
    TableNew.FieldDefs.Add(fldID_NO, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateTinTables(FileName: TFileName);
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  CreateTinTriangleTable(FileName);
  CreateTinVertexTable(ChangeModelTable(DirTinFaces, DirTinVertices, FileName));
  FileNameNew := FileName;
  //  WriteParFile(FileName,0,0,0,50,50,15,10,10,10);
end;

//Solids
// ================================================================
// The structure to store ore bodies or multiphase rock 3D textures
// ================================================================
// The Solid Body table lists polyhedrons
procedure CreateSolidBodyTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldNAME, ftString, 32, False);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

// The Solid Facet table lists faces or polygons
procedure CreateSolidFacetTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldAREA, ftFloat);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

// The Solid Vertex table lists all vertices
procedure CreateSolidVertexTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateSolidLinkTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldID_CELL, ftInteger);
    TableNew.FieldDefs.Add(fldID_FACE, ftInteger);
    TableNew.FieldDefs.Add(fldID_VERTEX, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateSolidTables(FileName: TFileName);
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  CreateSolidBodyTable(FileName);
  CreateSolidFacetTable(ChangeModelTable(DirSolidCells, DirSolidFaces, FileName));
  CreateSolidVertexTable(ChangeModelTable(DirSolidCells, DirSolidVertices, FileName));
  CreateSolidLinkTable(ChangeModelTable(DirSolidCells, DirSolids, FileName));
  FileNameNew := FileName;
  //  WriteParFile(FileName,0,0,0,50,50,15,10,10,10);
end;

//Grids ==================================================

procedure CreateGridTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  WriteParFile(FileName, 0, 0, 0, 50, 50, 15, 10, 10, 10);
end;

//Grids 3D ==================================================
procedure CreateGrid3DTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  WriteParFile(FileName, 0, 0, 0, 50, 50, 15, 10, 10, 10);
end;

 // ================================================== \\
 { Creates Mesh2D data tables }
 // ================================================== \\
procedure CreateMesh2DLinkTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID_MATRIX, ftInteger);
    TableNew.FieldDefs.Add(fldID_ELEMENT, ftInteger);
    TableNew.FieldDefs.Add(fldID_NODE, ftInteger);

    ForceDirectories(ExtractFilePath(TableNew.TableName));
    DeleteFile(TableNew.TableName);
    TableNew.CreateTable;

  finally
    TableNew.Free;
  end;
end;


procedure CreateMesh2DMatrixTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID_MATRIX, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldNAME, ftString, 32, False);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    TableNew.FieldDefs.Add(fldAREA, ftFloat);

    ForceDirectories(ExtractFilePath(TableNew.TableName));
    DeleteFile(TableNew.TableName);
    TableNew.CreateTable;
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh2DElementTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID_ELEMENT, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldNAME, ftString, 8, False);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    TableNew.FieldDefs.Add(fldAREA, ftFloat);
    TableNew.FieldDefs.Add(fldSLOPE, ftFloat);
    ForceDirectories(ExtractFilePath(TableNew.TableName));
    DeleteFile(TableNew.TableName);
    TableNew.CreateTable;
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh2DNodeTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID_NODE, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);

    ForceDirectories(ExtractFilePath(TableNew.TableName));
    DeleteFile(TableNew.TableName);
    TableNew.CreateTable;
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh2DTables(FileName: TFileName);
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  CreateMesh2DLinkTable(FileName);
  CreateMesh2DMatrixTable(ChangeModelTable(DirMesh2D, DirMesh2DFaces, FileName));
  CreateMesh2DNodeTable(ChangeModelTable(DirMesh2D, DirMesh2DVertices, FileName));
  FileNameNew := FileName;
  //WriteMesh2DParFile(FileName,0,0,0,50,50,15,10,10,10);
end;

//Meshes 3D
//==================================================
procedure CreateMesh3DElementTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    TableNew.FieldDefs.Add(fldMATERIAL, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh3DVertexTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh3DLinkTable(FileName: TFileName);
var
  TableNew: TTable;
begin
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID_ELEMENT, ftInteger);
    TableNew.FieldDefs.Add(fldID_NODE, ftInteger);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
end;

procedure CreateMesh3DTables(FileName: TFileName);
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  CreateMesh3DElementTable(FileName);
  CreateMesh3DVertexTable(ChangeModelTable(DirMesh3DElement, DirMesh3DNode,
    FileName));
  CreateMesh3DLinkTable(ChangeModelTable(DirMesh3DElement, DirMesh3D, FileName));
  FileNameNew := FileName;
  // WriteMesh3DParFile(FileName,0,0,0,50,50,15,10,10,10);
end;

procedure CreateDrawingTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(nil);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldX, ftFloat);
    TableNew.FieldDefs.Add(fldY, ftFloat);
    TableNew.FieldDefs.Add(fldZ, ftFloat);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
  FileNameNew := FileName;
end;

//====================================================================\\
// Variograms
//====================================================================\\

procedure CreateExpVariogramTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldVarType, ftInteger);
    TableNew.FieldDefs.Add(fldDim3D, ftBoolean);
    TableNew.FieldDefs.Add(fldATTRIBUTE, ftString);
    TableNew.FieldDefs.Items[TableNew.FieldDefs.Count - 1].Size := 20;
    TableNew.FieldDefs.Add(fldAzimuthAngle, ftFloat);
    TableNew.FieldDefs.Add(fldAzimuthTolerance, ftFloat);
    TableNew.FieldDefs.Add(fldAzimuthBandwidth, ftFloat);
    TableNew.FieldDefs.Add(fldDipAngle, ftFloat);
    TableNew.FieldDefs.Add(fldDipTolerance, ftFloat);
    TableNew.FieldDefs.Add(fldDipBandwidth, ftFloat);
    TableNew.FieldDefs.Add(fldLagsN, ftInteger);
    TableNew.FieldDefs.Add(fldLagDistance, ftFloat);
    TableNew.FieldDefs.Add(fldLagTolerance, ftFloat);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
  FileNameNew := FileName;
  FileName    := FileName + '_DATA';
  TableNew    := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldVarID, ftInteger);
    TableNew.FieldDefs.Add(fldAvgseparationDistance, ftFloat);
    TableNew.FieldDefs.Add(fldG, ftFloat);
    TableNew.FieldDefs.Add(fldLagPairsNum, ftFloat);
    TableNew.FieldDefs.Add(fldMeanTail, ftFloat);
    TableNew.FieldDefs.Add(fldMeanHead, ftFloat);
    TableNew.FieldDefs.Add(fldVarianceTail, ftFloat);
    TableNew.FieldDefs.Add(fldVarianceHead, ftFloat);
    CreateNewTable(TableNew);
    FileNameNew := FileName;
  finally
    TableNew.Free;
  end;
end;

//==================================================================
procedure CreateModelVariogramTables(FileName: TFileName);
var
  TableNew: TTable;
begin
  if FileExists(FileName) then
    if MessageDlg(LoadResString(@rsRewrite) + '?', mtInformation,
      [mbYes, mbNo], 0) = mrNo then
      Exit;
  TableNew := TTable.Create(Application);
  try
    TableNew.TableName := FileName;
    TableNew.FieldDefs.Clear;
    TableNew.FieldDefs.Add(fldID, ftInteger);
    TableNew.FieldDefs.Add(fldVarID, ftInteger);
    TableNew.FieldDefs.Add(fldDim3D, ftBoolean);
    TableNew.FieldDefs.Add(fldATTRIBUTE, ftString);
    TableNew.FieldDefs.Items[TableNew.FieldDefs.Count - 1].Size := 20;
    TableNew.FieldDefs.Add(fldModelFunctionID, ftInteger);
    TableNew.FieldDefs.Add(fldNugget, ftFloat);
    TableNew.FieldDefs.Add(fldContribution, ftFloat);
    TableNew.FieldDefs.Add(fldRange, ftFloat);
    TableNew.FieldDefs.Add(fldAnis1, ftFloat);
    TableNew.FieldDefs.Add(fldAnis2, ftFloat);
    TableNew.FieldDefs.Add(fldAzimuthAngle, ftFloat);
    TableNew.FieldDefs.Add(fldDipAngle, ftFloat);
    TableNew.FieldDefs.Add(fldPlungeAngle, ftFloat);
    CreateNewTable(TableNew);
  finally
    TableNew.Free;
  end;
  FileNameNew := FileName;
end;


//==================================================================
function CreateLegendTable(Table: TTable): boolean;
begin
  ForceDirectories(ExtractFilePath(Table.TableName));
  with Table do
    try
      FieldDefs.Clear;
      FieldDefs.Add(fldVISIBLE, ftBoolean);
      FieldDefs.Add('FILLFIXED', ftBoolean);
      FieldDefs.Add('FILLCOLOR', ftInteger);
      FieldDefs.Add('BKCOLOR', ftInteger);
      FieldDefs.Add('BMPNUM', ftInteger);
      FieldDefs.Add('LINEFIXED', ftBoolean);
      FieldDefs.Add('LINECOLOR', ftInteger);
      FieldDefs.Add('LINETYPE', ftInteger);
      FieldDefs.Add('LINEWIDTH', ftInteger);
      FieldDefs.Add('VALUE', ftFloat);
      CreateTable;
      Result := True;
    except
      Result := False;
    end;
end;

function DefaultLegendTable(Table: TTable; Min, Max: double): boolean;
const
  N = 14;
  Colors: array[0..N - 1] of TColor =
    (clWhite, clGray, clMaroon, clRed, clYellow, clLime,
    clGreen, clOlive, clAqua, clBlue, clNavy, clFuchsia,
    clPurple, clGray);
var
  H: double;
  I: integer;
begin
  try
    CreateLegendTable(Table);
    Table.Open;
    H := (Max - Min) / (N - 4);
    Table.Edit;
    Table.AppendRecord([1, 1, Colors[0], 0, 0, 1, Colors[0], 0, 0, 0]);
    Table.AppendRecord([1, 1, Colors[1], 0, 0, 1, Colors[1], 0, 0, 0]);
    for I := 1 to (N - 4) do
    begin
      Table.AppendRecord([1, 0, Colors[1 + I], 0, 0, 0, Colors[1 + I],
        0, 0, Max - I * H]);
    end;
    Table.AppendRecord([1, 1, Colors[N - 1], 0, 0, 1, Colors[N - 1], 0, 0, Min - H]);
    Table.Close;
    Result := True;
  except
    Table.Close;
    Result := False;
  end;
end;

//--------------------------------------------------------
function CreateMaterialTable(Table: TTable): boolean;
begin
  with Table do
    try
      FieldDefs.Clear;
      { TODO : Create a default table for materials }
      FieldDefs.Add(fldVISIBLE, ftInteger);
    {
    FieldDefs.Add('FILLFIXED', ftInteger);
    FieldDefs.Add('FILLCOLOR', ftInteger);
    FieldDefs.Add('BKCOLOR', ftInteger);
    FieldDefs.Add('BMPNUM', ftInteger);

    FieldDefs.Add('LINEFIXED', ftInteger);
    FieldDefs.Add('LINECOLOR', ftInteger);
    FieldDefs.Add('LINETYPE', ftInteger);
    FieldDefs.Add('LINEWIDTH', ftInteger);

    FieldDefs.Add('VALUE', ftFloat);
    }
      CreateTable;
      Result := True;
    except
      Result := False;
    end;
end;

//==================================================================
function CreatePatternTable(Table: TTable): boolean;
begin
  Table.FieldDefs.Add(fldID, ftInteger, 0, False);
  Table.FieldDefs.Add(fldIMAGE, ftGraphic, 0, False);
  Table.FieldDefs.Add(fldNAME, ftString, 64, False);
  Table.CreateTable;
  Result := True;
end;

//==================================================================
procedure CreatePolyBounds(FileName: TFileName; X0, Y0, Z0: double;
  DX, DY, DZ: double; NX, NY, NZ: integer);
var
  TablePolygon, TableVertex: TTable;
  FileNamePoly: TFileName;
  PolyID, VertID, VertNo: integer;
  LX, LY, LZ:   double;

begin
  FileNamePoly := DataBasePath + DirPolygonPoly + NameOnly(FileName);
  CreatePolygonTables(FileNamePoly);
  TablePolygon := TTable.Create(Application);
  try
    TablePolygon.TableName := FileNamePoly;
    TableVertex := TTable.Create(Application);
    try
      TableVertex.TableName :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, FileNamePoly);
      TablePolygon.Open;
      TableVertex.Open;

      LX := DX * NX;
      LY := DY * NY;
      LZ := DZ * NZ;

      PolyID := 1;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldX).AsFloat      := X0;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0 + LY / 2;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0 + LZ / 2;
      TablePolygon.FieldByName(fldNAME).AsString  := 'LEFT';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      VertID := 1;
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0, PolyID, VertNO]);

      PolyID := 2;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldNAME).AsString  := 'RIGHT';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldX).AsFloat      := X0 + LX;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0 + LY / 2;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0 + LZ / 2;
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      Inc(VertID);
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0, PolyID, VertNO]);

      PolyID := 3;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldNAME).AsString  := 'TOP';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldX).AsFloat      := X0 + LX / 2;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0 + LY / 2;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0 + LZ;
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      Inc(VertID);
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0 + LZ,
        PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0 + LZ, PolyID, VertNO]);

      PolyID := 4;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldNAME).AsString  := 'BOTTOM';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldX).AsFloat      := X0 + LX / 2;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0 + LY / 2;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0;
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      Inc(VertID);
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0, PolyID, VertNO]);

      PolyID := 5;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldNAME).AsString  := 'FRONT';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldX).AsFloat      := X0 + LX / 2;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0 + LZ / 2;
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      Inc(VertID);
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0, Z0, PolyID, VertNO]);

      PolyID := 6;
      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger   := PolyID;
      TablePolygon.FieldByName(fldNAME).AsString  := 'BACK';
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolyline);
      TablePolygon.FieldByName(fldX).AsFloat      := X0 + LX / 2;
      TablePolygon.FieldByName(fldY).AsFloat      := Y0 + LY;
      TablePolygon.FieldByName(fldZ).AsFloat      := Z0 + LZ / 2;
      TablePolygon.FieldByName(fldMATERIAL).AsInteger := 0;
      TablePolygon.FieldByName(fldCODE).AsInteger := 0;
      TablePolygon.Post;
      Inc(VertID);
      VertNo := 1;
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0 + LZ, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0 + LZ,
        PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0 + LX, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0, PolyID, VertNO]);
      Inc(VertID);
      Inc(VertNo);
      TableVertex.AppendRecord([VertID, X0, Y0 + LY, Z0 + LZ, PolyID, VertNO]);

    finally
      TableVertex.Free;
    end;
  finally
    TablePolygon.Free;
  end;
end;


 //===========================================================\\
 // Creates a copy of table with associated tables and files  \\
 // Note: If bFailIfExists = True then the old file remains   \\
 // untouched otherwise it is changed with new one            \\
 //===========================================================\\
procedure CopyFiles(const AOldName, ANewName: TFileName; ADatatype: integer;
  bFailIfExists: boolean);
var
  OldName, NewName, Ext: TFileName;
  FileRec: TSearchRec;
  FileNameOld, FileNameNew: TFileName;

begin
  OldName := ChangeFileExt(AOldName, '');
  NewName := ChangeFileExt(ANewName, '');

  if FindFirst(AOldName, faAnyFile - faDirectory, FileRec) = 0 then
    repeat
      Ext := ExtractFileExt(FileRec.Name);
      try
        CopyFile(PChar(OldName + Ext), PChar(NewName + Ext), bFailIfExists);
        CopyFile(PChar(OldName + '.*'), PChar(NewName + '.*'), bFailIfExists);
      except
      end;
    until FindNext(FileRec) <> 0;
  FindClose(FileRec);
  //and additional tables from other directories
  case ADatatype of
    mtPolygons:
    begin
      FileNameOld := ChangeModelTable(DirPolygonPoly, DirPolygonVertex, AOldName);
      FileNameNew := ChangeModelTable(DirPolygonPoly, DirPolygonVertex, ANewName);
      CopyFiles(PChar(FileNameOld), PChar(FileNameNew), mtUnknown, bFailIfExists);
    end;
    mtTins:
    begin
      FileNameOld := ChangeModelTable(DirTinFaces, DirTinVertices, AOldName);
      FileNameNew := ChangeModelTable(DirTinFaces, DirTinVertices, ANewName);
      CopyFiles(PChar(FileNameOld), PChar(FileNameNew), mtUnknown, bFailIfExists);
    end;
    mtSolids:
    begin
      FileNameOld := ChangeModelTable(DirSolids, DirSolidVertices, AOldName);
      FileNameNew := ChangeModelTable(DirSolids, DirSolidVertices, ANewName);
      CopyFiles(PChar(FileNameOld), PChar(FileNameNew), mtUnknown, bFailIfExists);
      FileNameOld := ChangeModelTable(DirSolids, DirSolidFaces, AOldName);
      FileNameNew := ChangeModelTable(DirSolids, DirSolidFaces, ANewName);
      CopyFiles(PChar(FileNameOld), PChar(FileNameNew), mtUnknown, bFailIfExists);
      FileNameOld := ChangeModelTable(DirSolids, DirSolidCells, AOldName);
      FileNameNew := ChangeModelTable(DirSolids, DirSolidCells, ANewName);
      CopyFiles(PChar(FileNameOld), PChar(FileNameNew), mtUnknown, bFailIfExists);
    end;
    mtMeshes2D:
    begin
      FileNameOld := ChangeModelTable(DirMesh2D, DirMesh2DVertices, AOldName);
      FileNameNew := ChangeModelTable(DirMesh2D, DirMesh2DVertices, ANewName);
      CopyFile(PChar(FileNameOld), PChar(FileNameNew), bFailIfExists);
      FileNameOld := ChangeModelTable(DirMesh2D, DirMesh2DFaces, AOldName);
      FileNameNew := ChangeModelTable(DirMesh2D, DirMesh2DFaces, ANewName);
      CopyFile(PChar(FileNameOld), PChar(FileNameNew), bFailIfExists);
    end;
    mtMeshes3D:
    begin
      FileNameOld := ChangeModelTable(DirMesh3D, DirMesh3DNode, AOldName);
      FileNameNew := ChangeModelTable(DirMesh3D, DirMesh3DNode, ANewName);
      CopyFile(PChar(FileNameOld), PChar(FileNameNew), bFailIfExists);
      FileNameOld := ChangeModelTable(DirMesh3D, DirMesh3DElement, AOldName);
      FileNameNew := ChangeModelTable(DirMesh3D, DirMesh3DElement, ANewName);
      CopyFile(PChar(FileNameOld), PChar(FileNameNew), bFailIfExists);
      FileNameOld := ChangeModelTable(DirMesh3D, DirMesh3DMatrix, AOldName);
      FileNameNew := ChangeModelTable(DirMesh3D, DirMesh3DMatrix, ANewName);
      CopyFile(PChar(FileNameOld), PChar(FileNameNew), bFailIfExists);
    end;
  end;
end;

 //==========================================================================\\
 { Renames all files with a name to new directories with new name if needed  }
 //==========================================================================\\
procedure RenameFiles(const AOldName, ANewName: TFileName);
var
  NewName, Ext, OldDir: TFileName;
  FileRec: TSearchRec;
begin
  NewName := ExtractFilePath(ANewName) + NameOnly(ANewName);
  OldDir  := ExtractFilePath(AOldName);
  if FindFirst(AOldName, faAnyFile - faDirectory, FileRec) = 0 then
    repeat
      Ext := ExtractFileExt(FileRec.Name);
      try
        RenameFile(SlashSep(OldDir, FileRec.Name), NewName + Ext);
      except
      end;
    until FindNext(FileRec) <> 0;
  FindClose(FileRec);
end;

 //----------------------------------------------------------------\\
procedure DeleteFiles(const AFileName: TFileName; AModeltype: integer);
var
  FileRec:  TSearchRec;
  FileName: TFileName;
  FilePath: TFileName;
begin
  FilePath := ExtractFilePath(AFileName);
  FileName := FilePath + NameOnly(AFileName) + '.*';
  if FindFirst(FileName, faAnyFile - faDirectory, FileRec) = 0 then
    repeat
      try
        DeleteFile(FilePath + FileRec.Name);
      except
      end;
    until FindNext(FileRec) <> 0;
  FindClose(FileRec);
  //Additional tables
  case AModeltype of
    mtPolygons:
    begin
      FileName := ChangeModelTable(DirPolygonPoly, DirPolygonVertex, AFileName);
      DeleteFile(FileName + TableExt);
    end;
    mtTins:
    begin
      FileName := ChangeModelTable(DirTinFaces, DirTinVertices, AFileName);
      DeleteFile(FileName + TableExt);
    end;
    mtSolids:
    begin
      FileName := ChangeModelTable(DirSolids, DirSolidVertices, AFileName);
      DeleteFile(FileName + TableExt);
      FileName := ChangeModelTable(DirSolids, DirSolidFaces, AFileName);
      DeleteFile(FileName + TableExt);
      FileName := ChangeModelTable(DirSolids, DirSolidCells, AFileName);
      DeleteFile(FileName + TableExt);
    end;
    mtMeshes2D:
    begin
      FileName := ChangeModelTable(DirMesh2D, DirMesh2DVertices, AFileName);
      DeleteFile(FileName + TableExt);
      FileName := ChangeModelTable(DirMesh2D, DirMesh2DFaces, AFileName);
      DeleteFile(FileName + TableExt);
    end;
    mtMeshes3D:
    begin
      FileName := ChangeModelTable(DirMesh3D, DirMesh3DNode, AFileName);
      DeleteFile(FileName + TableExt);
      FileName := ChangeModelTable(DirMesh3D, DirMesh3DElement, AFileName);
      DeleteFile(FileName + TableExt);
      FileName := ChangeModelTable(DirMesh3D, DirMesh3DMatrix, AFileName);
      DeleteFile(FileName + TableExt);
    end;
  end;
end;



end.
