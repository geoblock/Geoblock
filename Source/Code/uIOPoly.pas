 //------------------------------------------------------------------------------
 // The modeling system Geoblock http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------

{ 
  Description: the procedures to read or write to/from an ASCII file the
  points or the topology of the mesh.
}


unit uIOPoly;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Contnrs,

  uTetraMesh,
  uObjects3D; //  fMapScrenery;

function SaveVoronoiToFile(FileName: TFileName): boolean;
 //  procedure StrToPoint(StrPoi: String; out tpo: Tpoint);
 //  procedure StrTointarray(Strint: String; out apo: array of integer);
function LoadVoronoiFromFile(FileName: TFileName): boolean;
function LoadDelaunayFromFile(FileName: TFileName): boolean;
procedure SavePoints(FileName: TFileName);
procedure ReadPoints(FileName: TFileName);
procedure SaveNodes(FileName: TFileName);
function ReadNodes(FileName: TFileName): boolean;
procedure SaveDelaunay(FileName: TFileName);
procedure SaveMeshTopology(FileName: TFileName);

var
  NR:     double;
  //////////Remove it to main pas
  GlobalNodes: array of TGBPoint3D;
  GlobalTetras: array of array[0..4] of TGBPoint3D;

//========================================================================
implementation
//========================================================================

var
  no:     integer = 0;
  DBFileName: TFileName;
  Vcount: integer;
  VcellPointcount: integer;
  VFaceCount: integer;


procedure SaveNodes(FileName: TFileName);
var
  i:      integer;
  pt:     TGBPoint3D;
  lstPts: TList;
  f:      TextFile;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  lstPts := m_mesh.GetListPts;
  //Writting  the first line of node
  // <points count> <dimension 3>   <# of attributes> <# of boundary markers (0 or 1)>
  Write(f, IntToStr(lstPts.Count - 4) + #9);
  Write(f, IntToStr(3) + #9);
  Write(f, IntToStr(1) + #9);
  WriteLn(f, IntToStr(0));
  // Writing nodes
  //<point #> <x> <y> <z> [attributes] [boundary marker]
  for i := 4 to (lstPts.Count - 1) do
  begin
    pt := lstPts[i];
    Write(f, IntToStr(i - 3) + #9);
    Write(f, FloatToStrF(pt.x, fffixed, 15, 15) + #9);
    Write(f, FloatToStrF(pt.y, fffixed, 15, 15) + #9);
    Write(f, FloatToStrF(pt.z, fffixed, 15, 15) + #9);
    WriteLn(f, FloatToStrF(pt.Data, fffixed, 5, 5));
  end;
  CloseFile(f);
end;

function ReadNodes(FileName: TFileName): boolean;
var
  ts:    string;
  att:   integer;
  nb:    integer;
  i, j, t, dim, ti: integer;
  coord: array[0..3] of double;
  temp2, ttt: double;
  times: array of double;
  startTime, endTime: double;
  f:     TextFile;
begin
  Result := False;
  try
    Assign(f, FileName);
    Reset(f);
    Read(f, nb);
    ti := nb;
    Read(f, dim);
    ti := dim;
    Read(f, att);
    ti := att;
    Read(f, t);
    SetLength(GlobalNodes, nb);

    if (att = 0) then //-- file without attributes
      for i := 0 to nb - 1 do
      begin
        Read(f, t);
        for j := 0 to 2 do
        begin
          Read(f, temp2);
          coord[j] := temp2;
        end;
        GlobalNodes[i] := TGBPoint3D.Create(coord[0], coord[1], coord[2]);
      end
    else  //-- file with attributes
      for i := 0 to nb - 1 do
      begin
        Read(f, t);
        for j := 0 to 3 do
        begin
          Read(f, temp2);
          coord[j] := temp2;
        end;
        GlobalNodes[i] := TGBPoint3D.Create(coord[0], coord[1], coord[2], coord[3]);
      end;
  finally
    CloseFile(f);
    Result := True;
  end;
end;


function LoadDelaunayFromFile(FileName: TFileName): boolean;
var
  DBFile: TextFile;
  i, j, k, t, cp, nb: integer;
  xs, ys, zs, ds: double;
  st:     string;
begin
  //  MyPol.Create;
  Result := False;
  try
    Assign(DBFile, FileName);
    Reset(DBFile);
    //Reading first record in 1st line in the file (Count of Tetras)
    Read(DBFile, nb);
    //Reading second record in 1st line in the file (Count of cell Points in tetra)
    Read(DBFile, cp);
    //Reading fird record in 1st line in the file (Count of args)
    Read(DBFile, t);

    //Setting length for dinamic arrays

    SetLength(GlobalTetras, nb);
    //Reading Vertics!!!
    for I := 0 to nb - 1 do
    begin
      Read(DBFile, k);
      for j := 0 to 3 do
      begin
        Read(DBFile, t);
        GlobalTetras[i][j] := GlobalNodes[t - 1];
      end;
    end;
  finally
    CloseFile(DBFile);
    Result := True;
  end;
end;

//  m_mesh.InsertPoint(0.2443, 0.6435, 0.34565, 5.0);
procedure SaveDelaunay(FileName: TFileName);
var
  i, j: integer;
  pt:   TGBTetrahedron;
  lstPts, templist: TList;
  f:    TextFile;
  ptt:  TGBPoint3D;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  templist := m_mesh.GetListTetra;
  lstPts   := TList.Create;
  //Writting  the first line of tetra
  // <# of tetrahedra> <nodes per tetrahedron> <# of attributes>
  for i := 1 to (templist.Count - 1) do
  begin
    if m_mesh.IsOutsideCH(templist[i]) = False then
      lstPts.Add(templist[i]);
  end;

  Write(f, IntToStr(lstPts.Count) + #9);
  Write(f, IntToStr(4) + #9);
  WriteLn(f, IntToStr(0));
  // Writing tetrahedrons
  //<tetrahedron #> <node> <node> <node> <node> ... [attributes]
  for i := 0 to (lstPts.Count - 1) do
  begin
    Write(f, IntToStr(i + 1) + #9);
    pt := lstPts[i];
    for j := 1 to 4 do
    begin
      ptt := pt.GetP(j);
      Write(f, IntToStr(ptt.m_no - 3) + #9);
    end;
    WriteLn(f, #13);
  end;
  CloseFile(f);
  lstPts.Destroy;
end;

function SaveVoronoiToFile(FileName: TFileName): boolean;
var
  DBFile:  TextFile;
  i, j, k: integer;
  xs, ys, zs, ds: double;
  st:      string;
  pt:      TGBPoint3D;
begin
  AssignFile(DBFile, FileName);
  Rewrite(DBFile);

  Vcount     := MyPol.Polyheders.CurrVert;
  VcellPointcount := MyPol.Polyheders.CellPoinCount;
  VFaceCount := MyPol.Polyheders.FacesCount;
  //Writing Voronoi polyhedres to the File
  Writeln(DBFile, Vcount);
  Writeln(DBFile, VcellPointcount);
  Writeln(DBFile, VFaceCount);
  Writeln(DBFile, '//////////Vert//////////');
  for i := 0 to Vcount - 1 do
  begin
    pt := MyPol.Polyheders.Vert[i];
    Write(DBFile, IntToStr(i + 1) + #9);
    Write(DBFile, FloatToStrF(pt.x, fffixed, 15, 15) + #9);
    Write(DBFile, FloatToStrF(pt.y, fffixed, 15, 15) + #9);
    Write(DBFile, FloatToStrF(pt.z, fffixed, 15, 15) + #9);
    WriteLn(DBFile, FloatToStrF(pt.Data, fffixed, 5, 5));

  end;
  Writeln(DBFile, '//////////Cell//////////');
  for i := 0 to VcellPointcount - 1 do
  begin
    pt := MyPol.Polyheders.CellPoint[i];
    Write(DBFile, IntToStr(i + 1) + #9);
    Write(DBFile, FloatToStrF(pt.x, fffixed, 15, 15) + #9);
    Write(DBFile, FloatToStrF(pt.y, fffixed, 15, 15) + #9);
    Writeln(DBFile, FloatToStrF(pt.z, fffixed, 15, 15));
  end;
  Writeln(DBFile, '//////////Faces//////////');

  for i := 0 to VFaceCount - 1 do
  begin
    k := Length(MyPol.Polyheders.Faces[i]);
    Write(DBFile, IntToStr(k) + #9);
    for j := 0 to k - 2 do
      Write(DBFile, IntToStr(MyPol.Polyheders.Faces[i][j]) + #9);
    Writeln(DBFile, IntToStr(MyPol.Polyheders.Faces[i][k - 1]));
  end;
  Writeln(DBFile, '//////////Voronoi//////////');

  for i := 0 to VCount - 1 do
  begin
    k := Length(MyPol.Polyheders.VoronoiPoly[i]);
    Write(DBFile, IntToStr(k) + #9);
    for j := 0 to k - 2 do
      Write(DBFile, IntToStr(MyPol.Polyheders.VoronoiPoly[i][j]) + #9);
    Writeln(DBFile, IntToStr(MyPol.Polyheders.VoronoiPoly[i][k - 1]));
  end;
  Writeln(DBFile, '//////////End of File//////////');


  CloseFile(DBFile);
end;


function LoadVoronoiFromFile(FileName: TFileName): boolean;
var
  DBFile: TextFile;
  i, j, k, t: integer;
  xs, ys, zs, ds: double;
  st:     string;
  apoint: array of TPoint;
  Cellpoint: array of TPoint;
  Facearray: array of array of integer;
  voronoiarray: array of array of integer;
  coord:  array[0..3] of double;
  temp:   double;
begin
  //  MyPol.Create;

  Assign(DBFile, FileName);
  Reset(DBFile);
  // Rewrite(DBFile);
  //Reading first line in the file (Count of Vertics)
  Readln(DBFile, st);
  Vcount := StrToInt(st);
  //Reading second line in the file (Count of cell Points)
  Readln(DBFile, st);
  VcellPointcount := StrToInt(st);
  //Reading fird line in the file (Count of Faces)
  Readln(DBFile, st);
  VFaceCount := StrToInt(st);
  //Read comment line
  Readln(DBFile, st);
  //Setting length for dinamic arrays
  MyPol.Polyheders.CurrVert      := Vcount;
  MyPol.Polyheders.CellPoinCount := VcellPointcount;
  MyPol.Polyheders.FacesCount    := VFaceCount;
  SetLength(MyPol.Polyheders.Vert, Vcount);
  SetLength(MyPol.Polyheders.CellPoint, VcellPointcount);
  SetLength(MyPol.Polyheders.Faces, VFaceCount);
  SetLength(MyPol.Polyheders.VoronoiPoly, Vcount);

  //Reading Vertics!!!
  for i := 0 to Vcount - 1 do
  begin
    Read(DBFile, k);
    for j := 0 to 3 do
    begin
      Read(DBFile, temp);
      coord[j] := temp;
    end;
    MyPol.Polyheders.Vert[i] := TGBPoint3D.Create;
    MyPol.Polyheders.Vert[i].SetCoord(coord[0], coord[1], coord[2]);
    MyPol.Polyheders.Vert[i].Data := coord[3];
  end;
  //Read comment line
  Readln(DBFile, st);
  Readln(DBFile, st);

  //Truncate(DBFile);

  //Reading Cell Points
  for i := 0 to VcellPointcount - 1 do
  begin
    Read(DBFile, k);
    for j := 0 to 2 do
    begin
      Read(DBFile, temp);
      coord[j] := temp;
    end;
    MyPol.Polyheders.CellPoint[i] := TGBPoint3D.Create;
    MyPol.Polyheders.CellPoint[i].SetCoord(coord[0], coord[1], coord[2]);
  end;
  //Read comment line
  Readln(DBFile, st);
  Readln(DBFile, st);
 {   Read(DBFile, t);
    Read(DBFile, t);
    Read(DBFile, t);
    testLine(inttostr(t));
    }
  //Reading Faces
  //           testLine(st);
  for i := 0 to VFaceCount - 1 do
  begin
    Read(DBFile, k);
    SetLength(MyPol.Polyheders.Faces[i], k);
    for j := 0 to k - 1 do
    begin
      Read(DBFile, t);
      MyPol.Polyheders.Faces[i][j] := t;
    end;
  end;

  //Read comment line
  Readln(DBFile, st);
  Readln(DBFile, st);
  // Read(DBFile, t);
  // Read(DBFile, t);
  //  testLine(inttostr(t));

 { testLine(st);
  }
  //Reading Voronoi polihedres
  for i := 0 to Vcount - 1 do
  begin
    Read(DBFile, k);
    SetLength(MyPol.Polyheders.VoronoiPoly[i], k);
    for j := 0 to k - 1 do
    begin
      Read(DBFile, t);
      MyPol.Polyheders.VoronoiPoly[i][j] := t;
    end;
  end;

  // Form1.Caption:= IntToStr(MyPol.Polyheders.Faces[1][1])+';'+IntToStr(MyPol.Polyheders.Faces[6][1])+';'+IntToStr(MyPol.Polyheders.VoronoiPoly[1][1]);
  CloseFile(DBFile);
end;

 //******************************************************************************
 // Description: Read the points stored in a text file (*.pts)

// Input:       (the TextFile, by reference)

// Output:      the points are read and added to the current mesh.

 // Format of the *.pts file:
 //    first line: 0 (no attributes) OR 1 (attributes)
 //    second line: number of points in the file
 //    other lines: each line is a point (with or w/o attributes)
 //******************************************************************************
procedure ReadPoints(FileName: TFileName);

var
  att:   integer;
  nb:    integer;
  i, j:  integer;
  coord: array[0..3] of double;
  temp:  double;
  times: array of double;
  startTime, endTime: double;
  f:     TextFile;
begin
  Assign(f, FileName);
  Reset(f);
  Read(f, att);
  Read(f, nb);
  if (att = 0) then //-- file without attributes
  begin
    for i := 1 to nb do
    begin
      for j := 0 to 2 do
      begin
        Read(f, temp);
        coord[j] := temp;
      end;
      m_mesh.InsertPoint(coord[0], coord[1], coord[2]);
    end;
  end
  else  //-- file with attributes
  begin
    //ShowMessage(inttostr(nb));
    for i := 1 to nb do
    begin
      for j := 0 to 3 do
      begin
        Read(f, temp);
        coord[j] := temp;
      end;
      m_mesh.InsertPoint(coord[0], coord[1], coord[2], coord[3]);
    end;

  end;
  CloseFile(f);
  //     m_mesh.InsertPoint(0.2443, 0.6435, 0.34565, 5.0);
end;


//******************************************************************************
// Description: Save the point in a text file
// Input:       (the TextFile, by reference)
// Output:      the TextFile is 'updated' with the points and their attributes
//              one line per point: x y z att
// Remarks:     The 4 points of the BigTetra are not saved.
//******************************************************************************
procedure SavePoints(FileName: TFileName);
var
  f:      TextFile;
  i:      integer;
  pt:     TGBPoint3D;
  lstPts: TList;
begin
  Assign(f, FileName);
  Rewrite(f);
  lstPts := m_mesh.GetListPts;
  WriteLn(f, '1');
  WriteLn(f, IntToStr(lstPts.Count - 4));
  for i := 4 to (lstPts.Count - 1) do
  begin
    pt := lstPts[i];
    Write(f, FloatToStrF(pt.x, fffixed, 15, 15) + #9);
    Write(f, FloatToStrF(pt.y, fffixed, 15, 15) + #9);
    Write(f, FloatToStrF(pt.z, fffixed, 15, 15) + #9);
    WriteLn(f, FloatToStrF(pt.Data, fffixed, 5, 5));
  end;
  CloseFile(f);
end;

procedure SaveMeshTopology(FileName: TFileName);
var
  f:      TextFile;
  i: integer;
  j: integer;
  t: TTetraMesh; //TTetra;
  adj: TTetraMesh;
  pt: TGBPoint3D;
  lstTetra: TList;
begin
  Assign(f, FileName);
  Rewrite(f);
  lstTetra := m_mesh.GetListTetra;
  for i := 0 to (lstTetra.Count - 1) do
  begin
    t := lstTetra[i];
   /// Write(f, IntToStr(t.m_no) + '   *   ');
    // -- TP
    for j := 1 to 4 do
    begin
///      adj := t.GetT(j);
      if (adj = nil) then
      begin
        Write(f, '-');
      end
      else
      begin
///        Write(f, IntToStr(adj.m_no));
      end;
      Write(f, #9);
    end;
    Write(f, '*' + #9);
    // -- TT
    for j := 1 to 4 do
    begin
///      pt := t.GetP(j);
      Write(f, IntToStr(pt.m_no) + #9);
    end;
    WriteLn(f, #13);
  end;
  CloseFile(f);
end;

end.
