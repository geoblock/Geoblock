//----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{! Linear interpolation from TIN points to GRID 2D nodes }

unit uLinearByTin;

interface

uses
  System.SysUtils,
  System.Variants,
  Vcl.ComCtrls,

  cInterpol;

function Linear2DInterpolation(var Points, Nodes: TCoordinateArray; ExtraValue: double;
  ProgressBar: TProgressBar): boolean; overload;

function Linear2DInterpolation(var Points, Nodes: TCoordinateArray; var Triangles: variant;
  ExtraValue: double; ProgressBar: TProgressBar): boolean; overload;

//=============================================================================
implementation
//=============================================================================

uses
  cGlobals,
  uCommon,
  cProfuns,
  uDelaunay2D;

type
  TTriCoords = array[0..2] of double;

var
  aPx, aPy, aPg: TTriCoords;

 {================================================}
 {  DMAT2 - Computes determinants of a 2*2 matrix }
 {================================================}
 {sub}
function dmat2(pv1, pv2: array of double; p1, p2: longint): double;
begin
  dmat2 := pv1[p1] * pv2[p2] - pv1[p2] * pv2[p1];
end;

 {==============================================}
 { DMAT3 - Calc determinant of matrix 3*3       }
 {==============================================}
 {sub}
function dmat3(pv1, pv2, pv3: array of double): double;
begin
  dmat3 := pv3[2] * dmat2(pv1, pv2, 0, 1) - pv3[1] * dmat2(pv1, pv2, 0, 2) +
    pv3[0] * dmat2(pv1, pv2, 1, 2);
end;

 (*==============================================*)
 (* DMAT3R - Computes determinant of a 3*3 matrx *)
 (*==============================================*)
 {sub}
function dmat3r(pv1, pv2: array of double): double;
begin
  dmat3r := dmat2(pv1, pv2, 0, 1) - dmat2(pv1, pv2, 0, 2) + dmat2(pv1, pv2, 1, 2);
end;

 {===============================================}
 {  INTERP - Calc interpolating plane parameters }
 {===============================================}
 {sub}
procedure interp(px, py, pg: TTriCoords; var a1, b1, c1: double);
var
  det: double;
begin
  det := dmat3r(px, py);
  if det = 0 then
  begin
    a1 := 0;
    b1 := 0;
    c1 := VAL_BLANK;
  end
  else
  begin
    a1 := dmat3r(pg, py) / det;
    b1 := dmat3r(px, pg) / det;
    c1 := dmat3(px, py, pg) / det;
  end;
end;

function Linear2DInterpolation(var Points, Nodes: TCoordinateArray; ExtraValue: double;
  ProgressBar: TProgressBar): boolean; overload;
var
  Triangulator2D: TTriangulator2D;
  i: integer;
  Triangles: variant;
begin
  Triangulator2D := TTriangulator2D.Create;

  with Triangulator2D do
  begin
    Npts := High(Points) + 1;
    for i := 0 to Npts - 1 do
    begin
      Px^[i] := Points[i].x;
      Py^[i] := Points[i].y;
    end;

    Delaunay2D;     //Create Delaunay triangulation

    Triangles := VarArrayCreate([1, Ntri + 1, 1, 6], varVariant);

    for i := 1 to Ntri + 1 do
    begin
      Triangles[i, 1] := vt1^[i - 1];
      Triangles[i, 2] := vt2^[i - 1];
      Triangles[i, 3] := vt3^[i - 1];
      Triangles[i, 4] := nt1^[i - 1];
      Triangles[i, 5] := nt2^[i - 1];
      Triangles[i, 6] := nt3^[i - 1];
    end;

  end;
  Triangulator2D.Free;
  Result := Linear2DInterpolation(Points, Nodes, Triangles, ExtraValue, ProgressBar);
end;

function Linear2DInterpolation(var Points, Nodes: TCoordinateArray; var Triangles: variant;
  ExtraValue: double; ProgressBar: TProgressBar): boolean; overload;
var
  i, Tr, itt, v: longint;
  Triangulator2D: TTriangulator2D;
  cX, cY, par1, par2, par3: double;
  Option: integer;
begin
  Triangulator2D := TTriangulator2D.Create;
  with Triangulator2D do
  begin
    Npts := High(Points) + 1;
    for i := 0 to Npts - 1 do
    begin
      Px^[i] := Points[i].x;
      Py^[i] := Points[i].y;
    end;

    (* Reads triangles vertices and neighbour triangles     *)
    //Fill work arrays for triangles
    Ntri := VarArrayHighBound(Triangles, 1) - 1;
    for i := 0 to Ntri do
    begin
      Vt1^[i] := Triangles[i + 1, 1];
      Vt2^[i] := Triangles[i + 1, 2];
      Vt3^[i] := Triangles[i + 1, 3];
      Nt1^[i] := Triangles[i + 1, 4];
      Nt2^[i] := Triangles[i + 1, 5];
      Nt3^[i] := Triangles[i + 1, 6];
    end;

    GetMinMaxXYZ(Points, double(Xmin), double(Xmax), double(Ymin),
      double(Ymax), double(Zmin), double(Zmax));

    Tr := 0;
    for i := 0 to High(Nodes) do
    begin
      cX := Nodes[i].x;
      cY := Nodes[i].y;
      ProgressBar.StepIt;

      // Search a triangle with the current grid node in TIN limits
      if (cX >= Xmin) and (cX <= Xmax) and (cY >= Ymin) and (cY <= Ymax) then
        Itt := TriFind(cX, cY, Tr, Option)
      else
        Itt := -1;
      if (Itt <> -1) then
      begin
        v      := Vt1^[Itt];
        apx[0] := Points[v].x;
        apy[0] := Points[v].y;
        apg[0] := Points[v].Value;

        v      := Vt2^[Itt];
        apx[1] := Points[v].x;
        apy[1] := Points[v].y;
        apg[1] := Points[v].Value;

        v      := Vt3^[Itt];
        apx[2] := Points[v].x;
        apy[2] := Points[v].y;
        apg[2] := Points[v].Value;

        Interp(apx, apy, apg, par1, par2, par3);
        Nodes[i].Value := cX * par1 + cY * par2 + par3;

      end //IF Itt<>-1
      else
      begin //all blank if current point out of TIN
        Nodes[i].Value := ExtraValue;
      end;
    end;
  end;
  Triangulator2D.Free;
  Result := True;
end;

end.
