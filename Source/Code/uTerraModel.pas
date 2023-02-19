//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*
  The unit to view the virtual reality with TarraObjects
*)

unit uTerraModel;

interface

uses
  Winapi.Windows, 
  System.SysUtils,
  Vcl.Graphics, 
  Vcl.ComCtrls,
  Bde.DBTables, 
  DBCtrls, 
  Data.DB,

  
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,

  //GB
  uGlobals,
  uCommon;

type
  TGeoScene3DPoint = record //3D point
    x, y, z: extended;
    No:      integer;
  end;

type
  TInterval = record
    max, min: double;
  end;

  TGeoSceneSimpleTriangle = record //Triangle
    A, B, C: TGeoScene3DPoint;
    No:      integer;
  end;

  TGeoSceneTriangle = record //Vertices numbers
    v1, v2, v3: integer;
    MaterialNo, PrevMaterialNo: integer;
    No: integer;
    Selected: boolean;
  end;

  TTriMaterial = record
    Image: TBitmap;
    No:    integer;
  end;

  TTriMaterials = class
    MaterialQuery: TQuery;
    DBImage:    TDBImage;
    DataSource: TDataSource;
    Count:      integer;
    Materials:  array of TTriMaterial;
    constructor Create;
    destructor Destroy;
    function Add(MaterialNumber: integer): boolean;
  end;

  TGeoSceneModel = class
  private

    //Checking the table existence
    function ThereIsTableInDB(TableForCheck: string): boolean;

    //Allocation memory for vertices array
    procedure SetVerticesArraySize(size: integer);

    //Allocation memory for triangles array
    procedure SetTrianglesArraySize(size: integer);

  public
    Vertex_Query: TQuery;        //Query for vertices database
    Triangles_Query: TQuery;     //Query for triangles database
    Materials: TTriMaterials;
    // SetOfTriangles: array of TGeoSceneTriangleProperties;
    //Vertices array
    VerticesArray: array of TGeoScene3DPoint;
    TrianglesArray: array of TGeoSceneTriangle;
    CurTri:   TGeoSceneSimpleTriangle;
    Max:      TGeoScene3DPoint;
    Min:      TGeoScene3DPoint;
    TriCount: integer;                //(Triangles count)
    VertCount: integer;
    TheTableName: TFileName;    //Name of the current table
    procedure InitDataBase;  //Databases initialization
    procedure FreeDataBase;  //Databases releasing
    function LoadTerrain(CurTbl: string): boolean;
    procedure GetTriangleByNum(num: integer); //Get a triangle by number
    function ReturnXInterval(y: double): TInterval;
    function GetZCoord(DotToCheck: TGeoScene3DPoint): double;
    procedure IndexTrianglePoints();
    procedure Vertices_Centering;
    procedure TranslateCoordinates;   //transformation of coordinates
    destructor Destroy;
  end;

//==========================================================================
implementation
//==========================================================================

destructor TGeoSceneModel.Destroy; //Freeing memory
begin
  SetLength(VerticesArray, 0);
  SetLength(TrianglesArray, 0);
  FreeMemory(Materials);
end;

procedure TGeoSceneModel.Vertices_Centering;
var
  i: integer;
  tempMax: extended;
begin
  for i := 0 to VertCount - 1 do
  begin
    VerticesArray[i].X :=
      VerticesArray[i].X - ((Self.Max.x + Self.Min.x) / 2);
    VerticesArray[i].Y :=
      VerticesArray[i].Y - ((Self.Max.y + Self.Min.y) / 2);
    VerticesArray[i].Z := VerticesArray[i].Z - Self.Min.z;
  end;

  tempMax    := self.Max.x;
  self.Max.x := self.Max.x - ((Self.Max.x + Self.Min.x) / 2);
  self.Min.x := self.Min.x - ((tempMax + Self.Min.x) / 2);

  tempMax    := self.Max.y;
  self.Max.y := self.Max.y - ((Self.Max.y + Self.Min.y) / 2);
  self.Min.y := self.Min.y - ((tempMax + Self.Min.y) / 2);

  self.Max.z := self.Max.z - self.Min.z;
  self.Min.z := 0;
end;


//Get a triangle by its number
procedure TGeoSceneModel.GetTriangleByNum(num: integer);
begin
  CurTri.No   := num;
  CurTri.A.No := TrianglesArray[num].V1;
  CurTri.B.No := TrianglesArray[num].V2;
  CurTri.C.No := TrianglesArray[num].V3;

  CurTri.A.X := VerticesArray[CurTri.A.No - 1].X;
  CurTri.A.Y := VerticesArray[CurTri.A.No - 1].Y;
  CurTri.A.Z := VerticesArray[CurTri.A.No - 1].Z;

  CurTri.B.X := VerticesArray[CurTri.B.No - 1].X;
  CurTri.B.Y := VerticesArray[CurTri.B.No - 1].Y;
  CurTri.B.Z := VerticesArray[CurTri.B.No - 1].Z;

  CurTri.C.X := VerticesArray[CurTri.C.No - 1].X;
  CurTri.C.Y := VerticesArray[CurTri.C.No - 1].Y;
  CurTri.C.Z := VerticesArray[CurTri.C.No - 1].Z;

  IndexTrianglePoints();
end;

procedure TGeoSceneModel.TranslateCoordinates();
begin
  CurTri.A.X := CurTri.A.X - ((Self.Max.x + Self.Min.x) / 2);
  CurTri.B.X := CurTri.B.X - ((Self.Max.x + Self.Min.x) / 2);
  CurTri.C.X := CurTri.C.X - ((Self.Max.x + Self.Min.x) / 2);

  CurTri.A.Y := CurTri.A.Y - ((Self.Max.y + Self.Min.y) / 2);
  CurTri.B.Y := CurTri.B.Y - ((Self.Max.y + Self.Min.y) / 2);
  CurTri.C.Y := CurTri.C.Y - ((Self.Max.y + Self.Min.y) / 2);

  CurTri.A.Z := CurTri.A.Z - Self.Min.z;
  CurTri.B.Z := CurTri.B.Z - Self.Min.z;
  CurTri.C.Z := CurTri.C.Z - Self.Min.z;
end;

//Setting a vertices array size
procedure TGeoSceneModel.SetVerticesArraySize(size: integer);
begin
  SetLength(VerticesArray, size);
end;

//Setting a triangles array size
procedure TGeoSceneModel.SetTrianglesArraySize(size: integer);
begin
  SetLength(TrianglesArray, size);
end;

function TGeoSceneModel.ThereIsTableInDB(TableForCheck: string): boolean;
begin
  if FileExists(DataBasePath + DirTinFaces + TableForCheck + TableExt) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

procedure TGeoSceneModel.FreeDataBase;
begin
  Vertex_Query.Close;
  Triangles_Query.Close;
  Vertex_Query.Free;
  Triangles_Query.Free;
end;

procedure TGeoSceneModel.InitDataBase;
begin
  Vertex_Query    := TQuery.Create(nil);
  Triangles_Query := TQuery.Create(nil);

  Vertex_Query.Active    := False;
  Triangles_Query.Active := False;

  Vertex_Query.DatabaseName    := DataBasePath + DirTinVertices;
  Triangles_Query.DatabaseName := DataBasePath + DirTinFaces;

  Vertex_Query.SQL.Text    := 'SELECT X, Y, Z FROM ' + TheTableName;
  Triangles_Query.SQL.Text :=
    'SELECT V1, V2, V3, Material FROM ' + TheTableName;

  Vertex_Query.Active    := True;
  Triangles_Query.Active := True;

  TriCount  := Triangles_Query.RecordCount;
  VertCount := Vertex_Query.RecordCount;
end;

function TGeoSceneModel.LoadTerrain(CurTbl: string): boolean;
var
  i, xx, yy, color: integer;
  // X_min, Y_min, Z_min, X_max, Y_max, Z_max: integer;
  TempDot:  TGeoScene3DPoint;
  min, max: TAffineVector;
begin
  Result := False;
  TheTableName := CurTbl;

  Materials := TTriMaterials.Create;

  i := Length(TheTableName);

  while i <> 1 do
  begin
    if TheTableName[i] = PathDelim then
    begin
      break;
    end;
    Dec(i);
  end;

  TheTableName := Copy(TheTableName, i + 1, Length(TheTableName) - i);

  //-------------------------------------------------------------------------//
  if ThereIsTableInDB(TheTableName) then
    //If there's table in database then...
  begin
    InitDataBase;
    //Allocating memory for vertices dynamic array
    SetVerticesArraySize(Vertex_Query.RecordCount);
    //Allocating memory for triangles dynamic array
    SetTrianglesArraySize(Triangles_Query.RecordCount);

{      Triangles_Query.Active := False;
      Triangles_Query.SQL.Text :=
         'SELECT V1, V2, V3, Material FROM ' + TheTableName;
      Triangles_Query.Active := True;                       }

    Triangles_Query.First;


    for i := 0 to TriCount - 1 do
    begin  //Saving V1,V2,V3 fields in TrianglesArray
      TrianglesArray[i].V1 :=
        Triangles_Query.FieldByName('V1').AsInteger;
      TrianglesArray[i].V2 :=
        Triangles_Query.FieldByName('V2').AsInteger;
      TrianglesArray[i].V3 :=
        Triangles_Query.FieldByName('V3').AsInteger;
      Triangles_Query.Next;
      TrianglesArray[i].MaterialNo     :=
        Triangles_Query.FieldByName('Material').AsInteger;
      TrianglesArray[i].PrevMaterialNo := TrianglesArray[i].MaterialNo;
      Materials.Add(TrianglesArray[i].MaterialNo);
    end;

      {Vertex_Query.Active := False;
      Vertex_Query.SQL.Text := 'SELECT X, Y, Z FROM ' + TheTableName;
      Vertex_Query.Active := True;}

    Vertex_Query.First;

    for i := 0 to VertCount - 1 do
    begin //Saving X,Y,Z fields in VerticesArray
      VerticesArray[i].X := Vertex_Query.FieldByName('X').AsFloat;
      VerticesArray[i].Y := Vertex_Query.FieldByName('Y').AsFloat;
      VerticesArray[i].Z := Vertex_Query.FieldByName('Z').AsFloat;
      Vertex_Query.Next;
    end;

    Vertex_Query.Active   := False;
    Vertex_Query.SQL.Text :=
      'SELECT MIN(X),MAX(X),MIN(Y),MAX(Y),MIN(Z),MAX(Z) FROM ' + TheTableName;
    Vertex_Query.Active   := True;

    Self.Min.x := Vertex_Query.Fields.Fields[0].AsInteger;
    Self.Max.x := Vertex_Query.Fields.Fields[1].AsInteger;
    Self.Min.y := Vertex_Query.Fields.Fields[2].AsInteger;
    Self.Max.y := Vertex_Query.Fields.Fields[3].AsInteger;
    Self.Min.z := Vertex_Query.Fields.Fields[4].AsInteger;
    Self.Max.z := Vertex_Query.Fields.Fields[5].AsInteger;

    //Vertices_Centering; //Vertices array values centring
    Result := True;
  end;
  //////////////////////////////---------------------////////////////////////////
end;

function TGeoSceneModel.ReturnXInterval(y: double): TInterval;
var
  x:    TInterval;
  dot1, dot2, dot3: TGeoScene3DPoint;
  temp: double;
begin
  dot1 := CurTri.A;
  dot2 := CurTri.B;
  dot3 := CurTri.C;
  if (y < Dot3.y) then
  begin
    y := Dot3.y;
  end;
  if (y > Dot1.y) then
  begin
    y := Dot1.y;
  end;
  if (y > dot2.y) then
  begin
    x.min := ((y - dot1.y) * (dot2.x - dot1.x)) / (dot2.y - dot1.y) + dot1.x;
    x.max := ((y - dot1.y) * (dot3.x - dot1.x)) / (dot3.y - dot1.y) + dot1.x;
  end
  else// if ((y<>dot3.y)AND(y<>dot2.y)) then
  begin
    x.min := ((y - dot1.y) * (dot3.x - dot1.x)) / (dot3.y - dot1.y) + dot1.x;
    x.max := ((y - dot2.y) * (dot3.x - dot2.x)) / (dot3.y - dot2.y) + dot2.x;
    if (dot2.y = dot3.y) then
    begin
      x.min := dot2.x;
      x.max := dot3.x;
    end;
  end;
  if (x.min > x.max) then
  begin
    temp  := x.min;
    x.min := x.max;
    x.max := temp;
  end;
  Result := x;
end;

function TGeoSceneModel.GetZCoord(DotToCheck: TGeoScene3DPoint): double;
begin
  Result := CurTri.A.Z - ((DotToCheck.X - CurTri.A.X) *
    ((CurTri.B.Y - CurTri.A.Y) * (CurTri.C.Z - CurTri.A.Z) -
    (CurTri.B.Z - CurTri.A.Z) * (CurTri.C.Y - CurTri.A.Y)) -
    (DotToCheck.Y - CurTri.A.Y) * ((CurTri.B.X - CurTri.A.X) *
    (CurTri.C.Z - CurTri.A.Z) - (CurTri.B.Z - CurTri.A.Z) *
    (CurTri.C.X - CurTri.A.X))) / ((CurTri.B.X - CurTri.A.X) *
    (CurTri.C.Y - CurTri.A.Y) - (CurTri.B.Y - CurTri.A.Y) *
    (CurTri.C.X - CurTri.A.X));
end;

procedure TGeoSceneModel.IndexTrianglePoints();
var
  PointsArray: array[1..3] of TGeoScene3DPoint;
  temp: TGeoScene3dPOint;
  i, j: integer;
begin

  PointsArray[1] := CurTri.A;
  PointsArray[2] := CurTri.B;
  PointsArray[3] := CurTri.C;
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      if PointsArray[i].Y > PointsArray[j].Y then
      begin
        temp := PointsArray[j];
        PointsArray[j] := PointsArray[i];
        PointsArray[i] := temp;
      end;
    end;
  end;

  CurTri.A := PointsArray[1];
  CurTri.B := PointsArray[2];
  CurTri.C := PointsArray[3];

end;

constructor TTriMaterials.Create;
begin
  Count      := 0;
  DBImage    := TDBImage.Create(nil);
  MaterialQuery := TQuery.Create(nil);
  MaterialQuery.DatabaseName := ExpandPath(DirDataReference);
  DataSource := TDataSource.Create(nil);
  DataSource.DataSet := MaterialQuery;
  DBImage.DataSource := DataSource;
end;

destructor TTriMaterials.Destroy;
begin
  MaterialQuery.Close;
  FreeAndNil(DBImage);
  FreeAndNil(DataSource);
  FreeAndNil(MaterialQuery);
  SetLength(Materials, 0);
end;

function TTriMaterials.Add(MaterialNumber: integer): boolean;
var
  tempArray: array of TTriMaterial;
  i: integer;
begin

  for i := 0 to Count - 1 do
  begin
    if Materials[i].No = MaterialNumber then
    begin
      Result := False;
      exit;
    end;
  end;

  setlength(tempArray, Count);
  for i := 0 to Count - 1 do
  begin
    tempArray[i].No    := Materials[i].No;
    tempArray[i].Image := TBitmap.Create;
    tempArray[i].Image.Assign(Materials[i].Image);
  end;

  setlength(Materials, Count + 1);

  for i := 0 to Count - 1 do
  begin
    Materials[i].No    := tempArray[i].No;
    Materials[i].Image := TBitmap.Create;
    Materials[i].Image.Assign(tempArray[i].Image);
  end;

  setlength(tempArray, 0);
  Inc(Count);

  MaterialQuery.Active   := False;
  MaterialQuery.SQL.Text :=
    'SELECT ID, Image FROM Textures WHERE ID=' + IntToStr(MaterialNumber);
  MaterialQuery.Active   := True;
  DBImage.DataField      := 'Image';

  //Materials[Count - 1].No := MaterialQuery.FieldByName('ID').AsInteger;
  Materials[Count - 1].No    := MaterialNumber;
  Materials[Count - 1].Image := TBitMap.Create;
  Materials[Count - 1].Image.Assign(DBImage.Picture.Bitmap);
  Result := True;
end;

end.
