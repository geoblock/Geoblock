//-------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-------------------------------------------------------------------------
(* Global types, variables and constants with utils *)

unit cGlobals;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IniFiles;

const
  // Cursors
  crSelectCursor = 0; //crDefault
  crPenCursor    = 1;
  crPanCursor    = 2;
  crScrollCursor = 3;
  crZoomCursor   = 4;
  crCrossCursor  = 5;

type
  TShowType    = (stMap, stTable, stGraph, stReport);
  TViewType    = (vtTop, vtBottom, vtLeft, vtRight, vtFront, vtBack, vtPerspective);
  TPolygonType = (ptNone, ptPoint, ptPolyline, ptPolygon, ptRect, ptRoundRect,
    ptEllipse, ptArc, ptSymbol, ptText);

const
  // ModelTypes - numbers associated with FileModels.PageIndex
  mtUnknown  = -1;
  mtFirst    = 0;
  mtDholes   = 0;      // Drillholes
  mtPoints2D = 1;      // 2D points  - XYZG
  mtPoints3D = 2;      // 3D points  - XYZG
  mtPolygons = 3;      // TPolygonTypes
  mtTins     = 4;      // 2D Triangulated Irregular Network
  mtSolids   = 5;      // Solids or shells of orebodies
  mtGrids2D  = 6;      // 2D Grid
  mtGrids3D  = 7;      // 3D Grid
  mtMeshes2D = 8;      // 2D Finite Difference Elements
  mtMeshes3D = 9;      // 3D Finite Difference Elements
  mtLast     = 10;

  { Image Datatypes BMP, PCX, JPEG, GIF }
  mtImage = 12;
  mtAll   = 13;

const
  CR   = #$0D;
  LF   = #$0A;
  CRLF = CR + LF;
  LFCR = LF + CR;
  Epsilon:   double = 0.00001;
  Seed:      integer = 1000;

const
  RegGeoblock = '\SOFTWARE\Geoblock\';
  RegGexoblock = '\SOFTWARE\Gexoblock\';

var
  GeneralSection: string = '\SOFTWARE\Geoblock\General';
  GeneralSextion: string = '\SOFTWARE\Gexoblock\General';
  // This vars  must not be declared as const
  TableExt:  string = '.db';
  TableInd:  string = '.px';
  TableMask: string = '*.db';
  PrjExt:    string = '.prj';
  ParExt:    string = '.par';
  TextExt:   string = '.txt';
  PlugExt:   string = '*.gpl';


var
  // Paths and Dirs
  AppPath:  TFileName = '';  //..\geoblock\
  ExePath:  TFileName = '';  //..\geoblock\bin\ for geoblock.exe
  DataPath : TFileName = '';
  DataAssetsPath: TFileName = '';
  DataBasePath: TFileName = '';

  //------------------------------ Dirs for    --------------------------------
  DirData: TFileName = 'Data'+ PathDelim;

  DirModels: TFileName = 'Models' + PathDelim;
  DirDholes: TFileName = 'Models' + PathDelim + 'Dholes' + PathDelim;
  DirPoints2D: TFileName = 'Models' + PathDelim + 'Points2D' + PathDelim;
  DirPoints3D: TFileName = 'Models' + PathDelim + 'Points3D' + PathDelim;
  DirPolygons: TFileName = 'Models' + PathDelim + 'Polygons' + PathDelim;
  DirPolygonVertex: TFileName = 'Models' + PathDelim + 'Polygons' + PathDelim + 'Vertex' + PathDelim;
  DirPolygonPoly:   TFileName = 'Models' + PathDelim + 'Polygons' + PathDelim + 'Poly' + PathDelim;
  DirTins:      TFileName = 'Models' + PathDelim + 'Tins' + PathDelim;
  DirTinVertices: TFileName = 'Models'+ PathDelim + 'Tins' + PathDelim + 'Vertices' + PathDelim;
  DirTinFaces: TFileName = 'Models'+ PathDelim + 'Tins' + PathDelim + 'Faces' + PathDelim;
  DirSolids:      TFileName = 'Models' + PathDelim + 'Solids' + PathDelim;
  DirSolidVertices: TFileName = 'Models' + PathDelim + 'Solids' + PathDelim + 'Vertices' + PathDelim;
  DirSolidFaces:  TFileName = 'Models' + PathDelim + 'Solids' + PathDelim + 'Faces' + PathDelim;
  DirSolidCells:   TFileName = 'Models' + PathDelim + 'Solids' + PathDelim + 'Cells' + PathDelim;
  DirGrid2D: TFileName = 'Models' + PathDelim + 'Grids2D' + PathDelim;
  DirGrid3D: TFileName = 'Models' + PathDelim + 'Grids3D' + PathDelim;
  DirMesh2D:     TFileName = 'Models' + PathDelim + 'Meshes2D' + PathDelim;
  DirMesh2DVertices: TFileName = 'Models' + PathDelim + 'Meshes2D' + PathDelim + 'Vertices' + PathDelim;
  DirMesh2DFaces: TFileName = 'Models' + PathDelim + 'Meshes2D' + PathDelim + 'Faces' + PathDelim;
  DirMesh3D:     TFileName = 'Models' + PathDelim + 'Meshes3D' + PathDelim;
  DirMesh3DNode: TFileName = 'Models' + PathDelim + 'Meshes3D' + PathDelim + 'Node' + PathDelim;
  DirMesh3DElement: TFileName = 'Models' + PathDelim + 'Meshes3D' + PathDelim + 'Element' + PathDelim;
  DirMesh3DMatrix: TFileName = 'Models' + PathDelim + 'Meshes3D' + PathDelim + 'Matrix' + PathDelim;

  //-------------------------------------------------------------------------------------
  DirDataBase: TFileName = 'Data' + PathDelim + 'Base' + PathDelim;

  DirDataReference: TFileName = 'Data'+ PathDelim + 'Base' + PathDelim + 'Reference' + PathDelim;
  DirDataAssets: TFileName = 'Data'+ PathDelim + 'Assets' + PathDelim;
  DirDataSQLs: TFileName = 'Data'+ PathDelim + 'SQLs' + PathDelim;

  DirExploring: TFileName = 'Data'+ PathDelim + 'Base' + 'Exploring' + PathDelim;
  DirReports:  TFileName = 'Data'+ PathDelim + 'Base' + 'Reports' + PathDelim;


  DirFiles: TFileName = 'Data'+ PathDelim + 'Files' + PathDelim;
  DirLegends: TFileName = 'Data' + PathDelim + 'Legends' + PathDelim;
  DirProjects: TFileName = 'Data' + PathDelim + 'Projects' + PathDelim;

  //GeoStatistical Directory for variography
  DirExpVar: TFileName = 'GeoStat' + PathDelim + 'ExpVar' + PathDelim;
  DirFitVar: TFileName = 'GeoStat' + PathDelim + 'FitVar' + PathDelim;
  DirHisto:  TFileName = 'GeoStat' + PathDelim + 'Histo' + PathDelim;

  //Assets Directory
  DirObjects:  TFileName = 'Data'+ PathDelim + 'Assets' + PathDelim + 'Objects' + PathDelim;
  DirSkins:    TFileName = 'Data'+ PathDelim + 'Assets' + PathDelim + 'Skins' + PathDelim;
  DirTextures: TFileName = 'Data'+ PathDelim + 'Assets' + PathDelim + 'Textures' + PathDelim;
  DirSounds:   TFileName = 'Data'+ PathDelim + 'Assets' + PathDelim + 'Sounds' + PathDelim;

  //Plugins and execs
  DirPlugins: TFileName = 'Plugins' + PathDelim;

// Vars for tables
var
  //Tables
  tblAssays:      string = 'Assays';
  tblCollars:     string = 'Collars';
  tblInclins:     string = 'Inclins';
  tblDictionary:  string = 'Dictionary';
  tblSettings:    string = 'Settings';
  tblFramework:   string = 'Framework';
  tblLevels:      string = 'Default';
  tblMaterial:    string = 'Default';
  tblHoleMat:     string = 'Default';
  tblPolyMat:     string = 'Polytype';
  tblPoints2DMat: string = 'Pointtype';
  tblPoint3DMat:  string = 'Point3DType';

// FieldType Mappings
  fldAREA:   string = 'AREA';           //ftFloat
  fldAREADIF: string = 'AREADIF';       //ftFloat
  fldATTRIBUTE: string = 'ATTRIBUTE';   //ftString
  fldAZIMUTH: string = 'AZIMUTH';       //ftFloat
  fldC:   string = 'C';                 //ftWord
  fldCATEGORY: string = 'CATEGORY';     //ftInteger
  fldCODE:   string = 'CODE';           //ftInteger
  fldCOMMENT: string = 'COMMENT';       //ftString
  fldCOMPONENT: string = 'COMPONENT';   //ftFloat
  fldCORE:   string = 'CORE';           //ftFloat
  fldCORE_PERCENT: string = 'CORE_PERCENT'; //ftFloat
  fldDENSITY: string = 'DENSITY';       //ftFloat
  fldDEPTH:  string = 'DEPTH';          //ftFloat
  fldDIP:    string = 'DIP';            //ftFloat
  fldDISTANCE: string = 'DISTANCE';     //ftFloat
  fldEASTING: string = 'EASTING';       //ftFloat
  fldELEMENT: string = 'ELEMENT';       //ftFloat
  fldELEVATION: string = 'ELEVATION';   //ftFloat
  fldENGLISH: string = 'ENGLISH';       //ftString
  fldFACIT:  string = 'FACET';          //ftInteger
  fldFACTOR: string = 'FACTOR';         //ftFloat
  fldFINISH: string = 'FINISH';         //ftDate
  fldFORMULA: string = 'FORMULA';       //ftString
  fldFROM:   string = 'FROM';           //ftFloat
  fldG:      string = 'G';              //ftFloat
  fldGRADE:  string = 'GRADE';          //ftFloat
  fldGRAINSIZE: string = 'GRAINSIZE';   //ftFloat
  fldDHOLE:  string = 'DHOLE';          //ftString
  fldHORIZON: string = 'HORIZON';       //ftInteger
  fldID:     string = 'ID';             //ftAutoincrement
  fldID_CELL: string = 'ID_CELL';       //ftInteger
  fldID_ELEMENT: string = 'ID_ELEMENT'; //ftInteger
  fldID_FACE: string = 'ID_FACE';     //ftInteger
  fldID_MATRIX: string = 'ID_MATRIX';   //ftInteger
  fldID_NO:  string = 'ID_NO';          //ftInteger
  fldID_NODE: string = 'ID_NODE';       //ftInteger
  fldID_POLY: string = 'ID_POLY';       //ftInteger
  fldID_TRIANGLE: string = 'ID_TRIANGLE'; //ftInteger
  fldID_TYPE: string = 'ID_TYPE';       //ftInteger
  fldID_VERTEX: string = 'ID_VERTEX';   //ftInteger
  fldIMAGE:  string = 'IMAGE';
  fldINCLINATION: string = 'INCLINATION'; //ftFloat
  fldINTERVAL: string = 'INTERVAL'; //ftFloat
  fldITEM:   string = 'ITEM';
  fldLABEL:  string = 'LABEL';     //ftString
  fldLENGTH: string = 'LENGTH';    //ftFloat
  fldMATERIAL: string = 'MATERIAL';
  fldMOISTURE: string = 'MOISTURE'; //ftFloat
  fldN1:     string = 'N1';         //ftInteger
  fldN2:     string = 'N2';         //ftInteger
  fldN3:     string = 'N3';         //ftInteger
  fldNAME:   string = 'NAME';       //ftString
  fldNO:     string = 'NO';             //ftInteger
  fldNORTHING: string = 'NORTHING'; //ftFloat
  fldORESORT: string = 'ORESORT';   //ftInteger
  fldOREKIND: string = 'OREKIND';   //ftInteger
  fldORETYPE: string = 'ORETYPE';   //ftInteger
  fldOREBODY: string = 'OREBODY';
  fldOREBLOCK: string = 'OREBLOCK'; //ftInteger
  fldPARAMETER: string = 'PARAMETER';
  fldPATTERN: string = 'PATTERN';
  fldPROFILE: string = 'PROFILE';   //ftString
  fldRECOVERY: string = 'RECOVERY'; //ftFloat
  fldRESERVE: string = 'RESERVE';   //ftFloat
  fldRHYTHM: string = 'RHYTHM';
  fldROCKTYPE: string = 'ROCKTYPE'; //ftInteger
  fldRUSSIAN: string = 'RUSSIAN';   //ftString
  fldSAMPLE: string = 'SAMPLE';     //ftString
  fldSEQUENCE: string = 'SEQUENCE'; //ftInteger
  fldSHORT:  string = 'SHORT';      //ftString
  fldSLOPE:  string = 'SLOPE';      //ftFloat
  fldSTART:  string = 'START';      //ftDate
  fldSUBBLOCK: string = 'SUBBLOCK'; //ftFloat
  fldSYMBOL: string = 'SYMBOL';
  fldT: string = 'T';              // ftWord or ftTime ?
  fldTEXTURE: string = 'TEXTURE';
  fldTHICKNESS: string = 'THICKNESS'; //ftFloat
  fldTO:     string = 'TO';           //ftFloat
  fldVALUE:  string = 'VALUE';        //ftFloat
  fldVARIANT: string = 'VARIANT';     //ftInteger
  fldVERTEX: string = 'VERTEX';       //ftInteger
  fldVISIBLE: string = 'VISIBLE';
  fldVOLUME: string = 'VOLUME'; //ftFloat
  fldV1:     string = 'V1'; //ftInteger
  fldV2:     string = 'V2'; //ftInteger
  fldV3:     string = 'V3'; //ftInteger
  fldX:      string = 'X'; //ftFloat
  fldY:      string = 'Y'; //ftFloat
  fldYIELD:  string = 'YIELD'; //ftFloat
  fldZ:      string = 'Z'; //ftFloat
  // Fields for Variograms
  fldVarID:  string = 'VAR_ID';  // ftIntege
  fldVarType: string = 'VAR_TYPE';  // ftInteger
  fldDim3D:  string = 'DIM_3D';  // ftBoolean
  fldAzimuthAngle: string = 'AZIMUTH_ANGLE';  // ftFloat
  fldAzimuthTolerance: string = 'AZIMUTH_TOLERANCE';  // ftFloat
  fldAzimuthBandwidth: string = 'AZIMUTH_BANDWIDTH';  // ftFloat
  fldDipAngle: string = 'DIP_ANGLE';  // ftFloat
  fldDipTolerance: string = 'DIP_TOLERANCE';  // ftFloat
  fldDipBandwidth: string = 'DIP_BANDWIDTH';  // ftFloat
  fldLagsN:  string = 'LAGS';  // ftInteger
  fldLagDistance: string = 'LAG_DISTANCE';  // ftFloat
  fldLagTolerance: string = 'LAG_TOLERANCE';  // ftFloat
  fldAvgSeparationDistance: string = 'SEPARATION_DISTANCE';  // ftFloat
  fldLagPairsNum: string = 'PAIRS_IN_LAG';  // ftFloat
  fldMeanTail: string = 'TAIL_MEAN';  // ftFloat
  fldMeanHead: string = 'HEAD_MEAN';  // ftFloat
  fldVarianceTail: string = 'TAIL_VARIANCE';  // ftFloat
  fldVarianceHead: string = 'HEAD_VARIANCE';  // ftFloat
  fldModelFunctionID: string = 'MODEL_FUNCTION_ID';  // ftInteger
  fldNugget: string = 'NUGGET';  // ftFloat
  fldContribution: string = 'CONTRIBUTION';  // ftFloat
  fldRange:  string = 'RANGE';  // ftFloat
  fldAnis1:  string = 'ANIS1';  // ftFloat
  fldAnis2:  string = 'ANIS2';  // ftFloat
  fldPlungeAngle: string = 'PLUNGE_ANGLE';  // ftFloat

var
  IniFile:  TIniFile;
  LangID: integer;
  CurLang:   string = 'ru';  //Current default is 'en', localized is 'ru' etc.

  Precision: integer = -2;

function SlashSep(const Path, S: string): string;
function ExpandPath(Path: TFileName): TFileName;
function GetDataPath: TFileName;


//=========================================================================
implementation
//=========================================================================

function GetDataPath(): TFileName;
var
  path: TFileName;
  p: Integer;
begin
// SetCurrentDir(ExtractFilePath(Application.ExeName));  // for win32/win64
// SetCurrentDir(ExtractFilePath(ParamStr(0)));          // for crossplatform

  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  path := LowerCase(ExtractFilePath(ParamStr(0)));
  p := Pos('geoblock', path);
  Delete(path, p + 8, Length(path));
  path := IncludeTrailingPathDelimiter(path) + 'data';
  Result := path;
end;

//---------------------------------------------------------------------------

function SlashSep(const Path, S: string): string;
begin
  if (Path <> '') and (S <> '') then
    Result := Path + S
  else
    Result := Path + PathDelim + S;
end;

//================================================================\\

function ExpandPath(Path: TFileName): TFileName;
begin
  if AppPath = '' then
    Exit;
  Result := Path;
  if Pos('Geoblock', Result) <> 0 then
    Result := AppPath
//  if Pos('Gexoblock', Result) <> 0 then
//    Result := AppPath
  else if Pos(DirPlugins, Result) <> 0 then
    Result := SlashSep(AppPath, DirPlugins)
  else if Pos(DirDataSQLs, Result) <> 0 then
    Result := SlashSep(AppPath, DirDataSQLs)
  else if Pos(DirProjects, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirProjects)
  else if Pos(DirExploring, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirExploring)
  else if Pos(DirFiles, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirFiles)
  else if Pos(DirLegends, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirLegends);

  try
    if not DirectoryExists(Result) then
      ForceDirectories(Result);
  finally
  end;
end;

(*
function ExpandPath(Path: TFileName): TFileName;
begin
  if AppPath = '' then
    Exit;
  Result := Path;
  if Pos('Geoblock', Result) <> 0 then
    Result := AppPath
  else if Pos(DirPlugins, Result) <> 0 then
    Result := SlashSep(AppPath, DirPlugins)
  else if Pos(DirDataSQLs, Result) <> 0 then
    Result := SlashSep(AppPath, DirDataSQLs)

  else if Pos(DirProjects, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirProjects)
  else if Pos(DirExploring, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirExploring)
  else if Pos(DirFiles, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirFiles)
  else if Pos(DirLegends, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirLegends)
  else if Pos(DirDholes, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirDholes)
  else if Pos(DirPoints2D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirPoints2D)
  else if Pos(DirPoints3D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirPoints3D)
  else if Pos(DirPolygonPoly, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirPolygonPoly)
  else if Pos(DirPolygonVertex, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirPolygonVertex)
  else if Pos(DirPolygons, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirPolygons)
  else if Pos(DirTinFaces, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirTinFaces)
  else if Pos(DirTinVertices, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirTinVertices)
  else if Pos(DirGrid2D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirGrid2D)
  else if Pos(DirGrid3D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirGrid3D)
  else if Pos(DirSolidCells, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirSolidCells)
  else if Pos(DirSolidFaces, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirSolidFaces)
  else if Pos(DirSolidVertices, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirSolidVertices)
  else if Pos(DirSolids, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirSolids)
  else if Pos(DirMesh2DFaces, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh2DFaces)
  else if Pos(DirMesh2DVertices, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh2DVertices)
  else if Pos(DirMesh2D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh2D)
  else if Pos(DirMesh2D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh2D)
  else if Pos(DirMesh3DMatrix, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh3DMatrix)
  else if Pos(DirMesh3DElement, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh3DElement)
  else if Pos(DirMesh3DNode, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh3DNode)
  else if Pos(DirMesh3D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh3D)
  else if Pos(DirMesh3D, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirMesh3D)
  else if Pos(DirDataAssets, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirDataAssets)
  else if Pos(DirModels, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirModels)
  else if Pos(DataBasePath, Result) <> 0 then
    Result := DataBasePath
  else if Pos(DirReports, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirReports)
  else if Pos(DirExpVar, Result) <> 0 then
  //Geostat Directories
    Result := SlashSep(DataBasePath, DirExpVar)
  else if Pos(DirFitVar, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirFitVar)
  else if Pos(DirHisto, Result) <> 0 then
    Result := SlashSep(DataBasePath, DirHisto)
  //Reference Directory
  else if Pos(DirDataReference, Result) <> 0 then
    Result := DataReferencePath;

  try
    if not DirectoryExists(Result) then
      ForceDirectories(Result);
  finally
  end;
end;
*)

//===========================================
initialization
//===========================================

  FormatSettings.DecimalSeparator := '.';
  FormatSettings.TwoDigitYearCenturyWindow := 60;
  //Converts a date from two digits to interval (CurrentYear-60 .. CurrentYear+40)
end.
