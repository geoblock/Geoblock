//-------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-------------------------------------------------------------------------
{! Natural Neighbors Interpolation routines }
(**********************************************************
 Some parts of the algorithm adopted from the paper:

 Sung W. Park, Lars Linsen, Oliver Kreylos, John D. Owens, and Bernd Hamann Hamann,
 "Discrete Sibson interpolation" (2006). IEEE Transactions on Visualization and Computer Vcl.Vcl.CheckLst,. 12 (2),
 pp. 243-253. Postprint available free at: http://repositories.cdlib.org/postprints/1462
***********************************************************)

unit uNaturalNeighbors;

interface

uses
  System.SysUtils,
  System.Math,
  Vcl.ComCtrls,

  GLS.VectorTypes,
  GLS.VectorGeometry,

  GBGeometry,
  cInterpol;

{!  Natural Neighbors 2D Interpolation }
procedure NaturalNeighborsInterpolation2D(var Points: TCoordinateArray;
  var Nodes: TCoordinateArray; ExtraValue: double; ProgressBar: TProgressBar);

//======================================================================
implementation
//======================================================================

uses
  uCommon,
  uDelaunay2D,
  cSuperblock;


type
  TDiscreteVoronoiCell = record
    X, Y, Z:   double;
    PolygonID: integer;
    Distance:  double;
    Value:     double;
  end;

  TDiscreteVoronoiDiagram2D = array of array of TDiscreteVoronoiCell;

// var tt, tt2: integer;    // for debug

{! Estimates Voronoi grid size
 Note: the Voronoi grid cell size for each dimension 10 times smaller
 than the size of Nodes grid cell }
procedure GetVoronoiGridSize(var Nodes: TCoordinateArray; var Nx, Ny, Nz: integer;
  var dx, dy, dz: double);
var
  Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: double;  // min and max values on the Nodes set
  Xmin2, Ymin2, Zmin2: double; // the next above min values
  I: integer;

begin
  GetMinMaxXYZ(Nodes, double(Xmin), double(Xmax), double(Ymin), double(Ymax),
               double(Zmin), double(Zmax));
  Xmin2 := xmax;
  Ymin2 := ymax;
  Zmin2 := zmax;
  for i := 0 to High(Nodes) do
  begin
    if (Xmin2 > Nodes[i].x) and (Nodes[i].X > Xmin) then
      Xmin2 := Nodes[i].x;
    if (Ymin2 > Nodes[i].y) and (Nodes[i].Y > Ymin) then
      Ymin2 := Nodes[i].y;
    if (Zmin2 > Nodes[i].z) and (Nodes[i].Z > Zmin) then
      Zmin2 := Nodes[i].z;
  end;
  Nx := Trunc((Xmax - xmin) / (Xmin2 - xmin)) * 10;
  Ny := Trunc((Ymax - ymin) / (Ymin2 - ymin)) * 10;
  Nz := Trunc((Zmax - zmin) / (Zmin2 - zmin)) * 10;
  Dx := (xmax - xmin) / Nx;
  Dy := (ymax - ymin) / Ny;
  if Nz <> 0 then
    Dz := (Zmax - Zmin) / Nz
  else
    Dz := 0;
end;

{! Determines the position of point P relative to the line along vector (P1,P2):
   Area > 0 : at the left;
   Area < 0 : at the right;
   Area = 0 : on the line. }
{function Area (P1, P2, P: TVector3D): Double;
begin
  Result := (P2[0] - P1[0])*(P[1] - P1[1]) - (P2[1] - P1[1])*(P[0] - P1[0]);
end;
}

/// Determines whether point (cx, cy) belongs to Voronoi polygon V
{function PointInVoronoiPolygon (var V: array of TVector3D; cx, cy: Double): Boolean;
var
  i, a, b: Integer; p: TVector3d; cont: Boolean;
begin
  if High(V) < 2 then
  begin
    Result := False;
    Exit;
  end;
  i := 0; p[0] := cx; p[1] := cy; cont := True;
  a := Sign(Area(V[High(V)], V[0], p));
  while cont and (i < High(V)) do begin
    b := Sign(Area(V[i], V[i+1], p));
    cont := (a = b) or (b = 0);
    if b <> 0 then a := b;
    Inc(i);
  end;
  Result := cont;
  inc(tt2);
end;
}

function PointInVoronoiPolygon(var V: array of TVector3D; cx, cy: double): boolean;
var
  xp, yp: array of single;
  i, n:   integer;
begin
  if High(V) < 2 then
  begin
    Result := False;
    Exit;
  end;
  n := High(V) + 1;
  SetLength(xp, n);
  SetLength(yp, n);
  for i := 0 to n - 1 do
  begin
    xp[i] := V[i].V[0];
    yp[i] := V[i].V[1];
  end;
  Result := PointInPolygon(xp, yp, cx, cy);
  //  inc(tt2);
end;


// 2D Natural Neighbors Interpolation
procedure NaturalNeighborsInterpolation2D(var Points: TCoordinateArray;
  var Nodes: TCoordinateArray; ExtraValue: double; ProgressBar: TProgressBar);
var
  Tri, TriV: TTriangulator2D;
  i, j, t, tr, v: Longint;
  cx, cy: double;
  opt: integer;

  // Discrete Voronoi diagram structures
  _x, _y, _xmin, _xmax, _ymin, _ymax, _zmin, _zmax, dx, dy, dz: double;
  nx, ny, nz: integer;
  DVD: TDiscreteVoronoiDiagram2D;

  // Superblock structures
  Super_Block: TCoordListArray;
  Pattern: TGridPatternArray;
  Pattern_len, found: integer;
  Dist: double;
  Dist_Match: TCoordMatchArray;

  // Discrete Sibson interpolation strictures
  m, k: integer;
  Radius: double;
  Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: double;
  c: array of double;
  n: array of integer;

begin
  // Initializing grid for discrete Voronoi diagram
  GetMinMaxXYZ(Nodes, double(_xmin), double(_xmax), double(_ymin),
    double(_ymax), double(_zmin), double(_zmax));
  GetVoronoiGridSize(Nodes, nx, ny, nz, dx, dy, dz);
  SetLength(DVD, nx);
  for i := 0 to nx - 1 do
    SetLength(DVD[i], ny);
  _x := _xmin;
  for i := 0 to nx - 1 do
  begin
    _y := _ymin;
    for j := 0 to ny - 1 do
    begin
      with DVD[i, j] do
      begin
        x := _x;
        y := _y;
        z := 0;
      end;
      _y := _y + dy;
    end;
    _x := _x + dx;
  end;

  with ProgressBar do
  begin
    Min      := Low(Nodes);
    Max      := 2 * nx + 1;
    Position := Min;
    Step     := 1;
  end;

  // Tri is used for Delaulay triangulation, TriV - for Voronoi diagram
  Tri  := TTriangulator2D.Create;
  TriV := TTriangulator2D.Create;
  GetMinMaxXYZ(Points, Double(Xmin), Double(Xmax), Double(Ymin),
    double(Ymax), Double(Zmin), Double(Zmax));
  // Generating the discrete Voronoi diagram (DVD)
  Tri.Npts  := High(Points) + 1;
  TriV.Npts := High(Points) + 1;
  for i := 0 to High(Points) do
  begin
    Tri.Px^[i]  := Points[i].x;
    Tri.Py^[i]  := Points[i].y;
    TriV.Px^[i] := Points[i].x;
    TriV.Py^[i] := Points[i].y;
  end;

  Tri.Delaunay2D;     // Creating Delaunay triangulation to improve the search
  TriV.Delaunay2D;    // Building "continuous" Voronoi diagram
  TriV.Voronoi2D;

  //    tt := 0; tt2 := 0;          // debug

  for i := 0 to High(DVD) do
  begin
    for j := 0 to High(DVD[i]) do
    begin
      cx := DVD[i, j].x;
      cy := DVD[i, j].y;
      tr := 0;

      if (cX >= Xmin) and (cX <= Xmax) and (cY >= Ymin) and (cY <= Ymax) then
        t := tri.TriFind(cx, cy, tr, opt)
      else
        t := -1;

      if t >= 0 then
      begin
        // triangle found
        v := tri.Vt1^[t];
        if PointInVoronoiPolygon(triv.VorDiagram[v], cx, cy) then
          t := v
        else
        begin
          v := tri.Vt2^[t];
          if PointInVoronoiPolygon(triv.VorDiagram[v], cx, cy) then
            t := v
          else
          begin
            v := tri.Vt3^[t];
            if PointInVoronoiPolygon(triv.VorDiagram[v], cx, cy) then
              t := v
            else
            begin
              // Voronoi polygon not found
              t := -1;
              //                      inc(tt);  //debug
            end;
          end;
        end;
        if t <> -1 then
        begin
          DVD[i, j].PolygonID := t;
          DVD[i, j].Distance  := Sqrt(Sqr(tri.px^[t] - cx) + Sqr(tri.py^[t] - cy));
          DVD[i, j].Value     := Points[t].Value;
        end
        else
          DVD[i, j].PolygonID := -1;
      end // of it t > 0 (triangle found)
      else
        DVD[i, j].PolygonID := -1;
    end;  // of for j
    ProgressBar.StepIt;
  end; // of for i
  tri.Free;
  triv.Free;

  // Initializing superblock structure
  for i := 0 to High(Nodes) - 1 do
    Nodes[i].id := i;

  Initialize_super_block(super_block, pattern, pattern_len);
  load_super_block(super_block, Nodes, High(Nodes) + 1, _xmin, _xmax,
    _ymin, _ymax, _zmin, _zmax, dist);

  // Interpolation using DVD
  SetLength(c, High(Nodes) + 1);
  SetLength(n, High(Nodes) + 1);
  for i := 0 to High(Nodes) do
  begin
    c[i] := 0;
    n[i] := 0;
  end;
  for i := 0 to nx - 1 do
  begin
    for j := 0 to ny - 1 do
      if DVD[i, j].PolygonID <> -1 then
      begin
        radius     := DVD[i, j].Distance;
        dist_match := get_sb_nearest_neighbors(DVD[i, j].x, DVD[i, j].y,
          DVD[i, j].z, pattern, pattern_len, super_block, radius,
          found, dist, _xmin, _ymin, _zmin);
        for m := 0 to found - 1 do
        begin
          k    := dist_match[m].id;
          c[k] := c[k] + DVD[i, j].Value;
          Inc(n[k]);
        end;
        SetLength(dist_match, 0);
      end;
    ProgressBar.StepIt;
  end;

  for i := 0 to High(Nodes) do
    if n[i] <> 0 then
      Nodes[i].Value := c[i] / n[i]
    else
      Nodes[i].Value := ExtraValue;

  // Releasing dynamic structures
  SetLength(c, 0);
  SetLength(n, 0);
  for i := 0 to nx - 1 do
    SetLength(DVD[i], 0);
  SetLength(DVD, 0);
end;

end.
