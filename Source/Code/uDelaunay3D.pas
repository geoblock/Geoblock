// -------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
// -------------------------------------------------------------------------
// ! The unit for 3D triangulations and voronoi diagrams in 3D
// ---------------------------------------------------------------------------
// This Delaunay triangulation method is a modification of code written by
// Dave Watson.  It uses an algorithm described in

// Watson, D.F. (1981) Computing the n-dimensional Delaunay
// tessellation with application to Voronoi polytopes:
// The Computer J., 24(2), p. 167-172.
// ---------------------------------------------------------------------------

unit uDelaunay3D;

interface

type
  TTetrahedron = array [0 .. 3] of Integer;

function Tess3D(N: Integer; X, Y, Z: array of Double;
  var NumTetrahedra: Integer; var ATetrahedron: array of TTetrahedron): Integer;

//==========================================================================
implementation
//==========================================================================

function Tess3D(N: Integer; X, Y, Z: array of Double;
  var NumTetrahedra: Integer; var ATetrahedron: array of TTetrahedron): Integer;
type
  TPoint = array [0 .. 2] of Double;
var
  I: Integer;
  Point: array of TPoint;
  Tetrahedron: array of TTetrahedron;

const
  EPSILON = 0.00001;
const
  TSIZE = 75;
const
  RANGE = 10.0;
const
  FLT_MAX: Double = 1.7E308;

var
  Wrk: array [0 .. 2, 0 .. 3] of Double;
  // = ((8 * RANGE, -RANGE, -RANGE, -RANGE),
  // (-RANGE, 8 * RANGE, -RANGE, -RANGE),
  // (-RANGE, -RANGE, 8 * RANGE, -RANGE));
var
  Xmin: Double;
  Ymin: Double;
  Zmin: Double;
  Xmax: Double;
  Ymax: Double;
  Zmax: Double;
  Value: Double;
  Xrange, Yrange, Zrange, Maxrange, Maxrange3: Double;
  Bgs: Double;

var
  Nts, I0, I1, I2, I3, I4, I5, I6, I7, I8, I9, I11: Integer;
  Ii: array [0 .. 2] of Integer;
  Xx: Double;

const
  Tsz: Integer = 6 * TSIZE;

var
  Id: array of Integer;
  Tmp: array of array [0 .. 2] of Integer;
  A3s: array of array [0 .. 3] of Integer;
  Ccr: array of array [0 .. 3] of Double;
var
  _z: Double;

label
  Corner1, Corner2, Corner3,
  ExitDelaunay;

var
  Delta: Integer;

begin
  SetLength(Point, N + 4); // double** point = new double*[N+4];

  for I := 0 to N - 1 do
  begin
    Point[I][0] := X[I];
    Point[I][1] := Y[I];
    Point[I][2] := Z[I];
  end;

  Xmin := Point[0][0];
  Xmax := Xmin;
  Ymin := Point[0][1];
  Ymax := Ymin;
  Zmin := Point[0][2];
  Zmax := Zmin;

  for I := 0 to N - 1 do
  begin
    Value := Point[I][0];
    if (Xmax < Value) then
      Xmax := Value;
    if (Xmin > Value) then
      Xmin := Value;

    Value := Point[I][1];
    if (Ymax < Value) then
      Ymax := Value;
    if (Ymin > Value) then
      Ymin := Value;

    Value := Point[I][2];
    if (Zmax < Value) then
      Zmax := Value;
    if (Zmin > Value) then
      Zmin := Value;
  end;

  Xrange := Xmax - Xmin;
  Yrange := Ymax - Ymin;
  Zrange := Zmax - Zmin;
  Maxrange := Xrange;
  if (Maxrange < Yrange) then
    Maxrange := Yrange;
  if (Maxrange < Zrange) then
    Maxrange := Zrange;

  // need to scale the data later to do a correct tetrahedron count
  Maxrange3 := Maxrange * Maxrange * Maxrange;

  // tweak the points by very small random numbers
  Bgs := EPSILON * Maxrange;
  Randomize;
  for I := 0 to N - 1 do
  begin
    Point[I][0] := Point[I][0] + Bgs * (0.5 - Random);
    Point[I][1] := Point[I][1] + Bgs * (0.5 - Random);
    Point[I][2] := Point[I][2] + Bgs * (0.5 - Random);
  end;

  for I := 0 to 3 do
  begin
    Point[N + I][0] := Xmin + Xrange * Wrk[0][I];
    Point[N + I][1] := Ymin + Yrange * Wrk[1][I];
    Point[N + I][2] := Zmin + Zrange * Wrk[2][I];
  end;

  // int** tmp = new int*[tsz+1];
  // tmp[0] = new int[3*(tsz+1)];
  // for i0 := 1 to tsz do tmp[i0] := tmp[0] + 3*i0;

  SetLength(Tmp, Tsz + 1);

  // Estimate of how many tetrahedrons there can be.  Since theoretically
  // the number is O(N^2), this could be quite large.  You may need to
  // increase i1 if a call to this function fails...
  I1 := 6 * (N + 6);
  // i1 = 16*N;  // Had to increase i1 in the case of N=3000 randomly generated
  // points in [0,10]^3

  SetLength(Id, I1);
  for I0 := 0 to I1 - 1 do
    Id[I0] := I0;

  // int** a3s = new int*[i1];
  // a3s[0] = new int[4*i1];
  // for i0 = 1 to i1-1 do  a3s[i0] := a3s[0] + 4*i0;
  A3s[0][0] := N;
  A3s[0][1] := N + 1;
  A3s[0][2] := N + 2;
  A3s[0][3] := N + 3;

  // double** ccr = new double*[i1];  // circumscribed centers and radii
  // ccr[0] = new double[4*i1];
  // for i0 := 1 to i1-1 do ccr[i0] := ccr[0] + 4*i0;
  Ccr[0][0] := 0.0;
  Ccr[0][1] := 0.0;
  Ccr[0][2] := 0.0;
  Ccr[0][3] := FLT_MAX;

  Nts := 1; // number of tetrahedra
  I4 := 1;

  // compute tetrahedralization
  for I0 := 0 to N - 1 do
  begin
    I1 := -1;
    I7 := -1;
    I9 := 0;
    for I11 := 0 to Nts - 1 do
    begin
      Inc(I1);
      while (A3s[I1][0] < 0) do
        Inc(I1);
      Xx := Ccr[I1][3];
      for I2 := 0 to 2 do
      begin
        _z := Point[I0][I2] - Ccr[I1][I2];
        Xx := Xx - _z * _z;
        if (Xx < 0) then
          goto Corner3;
      end;
      Dec(I9);
      Dec(I4);
      Id[I4] := I1;
      for I2 := 0 to 3 do
      begin
        Ii[0] := 0;
        if (Ii[0] = I2) then
          Inc(Ii[0]);
        for I3 := 1 to 2 do
        begin
          Ii[I3] := Ii[I3 - 1] + 1;
          if (Ii[I3] = I2) then
            Inc(Ii[I3]);
        end;
        if (I7 > 2) then
        begin
          I8 := I7;
          for I3 := 0 to I8 do
          begin
            for I5 := 0 to 2 do
              if (A3s[I1][Ii[I5]] <> Tmp[I3][I5]) then
                goto Corner1;
            for I6 := 0 to 2 do
              Tmp[I3][I6] := Tmp[I8][I6];
            Dec(I7);
            goto Corner2;
          Corner1:;
          end;
        end;
        Inc(I7);
        if (I7 > Tsz) then
        begin
          // temporary storage exceeded, increase TSIZE
          Result := 0;
          goto ExitDelaunay;
        end;
        for I3 := 0 to 2 do
          Tmp[I7][I3] := A3s[I1][Ii[I3]];
      Corner2:;
      end;
      A3s[I1][0] := -1;
    Corner3:;
    end;

    for I1 := 0 to I7 do
    begin
      for I2 := 0 to 2 do
        Wrk[I2][3] := 0;
      for I3 := 0 to 2 do
      begin
        Wrk[I2][I3] := Point[Tmp[I1][I2]][I3] - Point[I0][I3];
        Wrk[I2][3] := Wrk[I2][3] + 0.5 * Wrk[I2][I3] *
          (Point[Tmp[I1][I2]][I3] + Point[I0][I3]);
      end;

      Xx := (Wrk[0][0] * (Wrk[1][1] * Wrk[2][2] - Wrk[2][1] * Wrk[1][2])) -
        (Wrk[0][1] * (Wrk[1][0] * Wrk[2][2] - Wrk[2][0] * Wrk[1][2])) +
        (Wrk[0][2] * (Wrk[1][0] * Wrk[2][1] - Wrk[2][0] * Wrk[1][1]));
      Ccr[Id[I4]][0] :=
        ((Wrk[0][3] * (Wrk[1][1] * Wrk[2][2] - Wrk[2][1] * Wrk[1][2])) -
        (Wrk[0][1] * (Wrk[1][3] * Wrk[2][2] - Wrk[2][3] * Wrk[1][2])) +
        (Wrk[0][2] * (Wrk[1][3] * Wrk[2][1] - Wrk[2][3] * Wrk[1][1]))) / Xx;
      Ccr[Id[I4]][1] :=
        ((Wrk[0][0] * (Wrk[1][3] * Wrk[2][2] - Wrk[2][3] * Wrk[1][2])) -
        (Wrk[0][3] * (Wrk[1][0] * Wrk[2][2] - Wrk[2][0] * Wrk[1][2])) +
        (Wrk[0][2] * (Wrk[1][0] * Wrk[2][3] - Wrk[2][0] * Wrk[1][3]))) / Xx;
      Ccr[Id[I4]][2] :=
        ((Wrk[0][0] * (Wrk[1][1] * Wrk[2][3] - Wrk[2][1] * Wrk[1][3])) -
        (Wrk[0][1] * (Wrk[1][0] * Wrk[2][3] - Wrk[2][0] * Wrk[1][3])) +
        (Wrk[0][3] * (Wrk[1][0] * Wrk[2][1] - Wrk[2][0] * Wrk[1][1]))) / Xx;
      Ccr[Id[I4]][3] := 0;
      for I2 := 0 to 2 do
      begin
        _z := Point[I0][I2] - Ccr[Id[I4]][I2];
        Ccr[Id[I4]][3] := Ccr[Id[I4]][3] + _z * _z;
        A3s[Id[I4]][I2] := Tmp[I1][I2];
      end;

      A3s[Id[I4]][3] := I0;
      Inc(I4);
      Inc(I9);
    end;
    Nts := Nts + I9;
  end;

  // count the number of tetrahedra
  NumTetrahedra := 0;
  I0 := -1;
  for I11 := 0 to Nts - 1 do
  begin
    Inc(I0);
    while (A3s[I0][0] < 0) do
      Inc(I0);
    if (A3s[I0][0] < N) then
    begin
      for I1 := 0 to 2 do
        for I2 := 0 to 2 do
          Wrk[I1][I2] := Point[A3s[I0][I1]][I2] - Point[A3s[I0][3]][I2];

      Xx := ((Wrk[0][0] * (Wrk[1][1] * Wrk[2][2] - Wrk[2][1] * Wrk[1][2])) -
        (Wrk[0][1] * (Wrk[1][0] * Wrk[2][2] - Wrk[2][0] * Wrk[1][2])) +
        (Wrk[0][2] * (Wrk[1][0] * Wrk[2][1] - Wrk[2][0] * Wrk[1][1])));

      if (Abs(Xx) > EPSILON * Maxrange3) then
        Inc(NumTetrahedra);
    end;
  end;

  // create the tetrahedra
  SetLength(Tetrahedron, NumTetrahedra);

  NumTetrahedra := 0;
  I0 := -1;
  for I11 := 0 to Nts - 1 do
  begin
    Inc(I0);
    while (A3s[I0][0] < 0) do
      Inc(I0);
    if (A3s[I0][0] < N) then
    begin
      for I1 := 0 to 2 do
        for I2 := 0 to 2 do
          Wrk[I1][I2] := Point[A3s[I0][I1]][I2] - Point[A3s[I0][3]][I2];
      Xx := ((Wrk[0][0] * (Wrk[1][1] * Wrk[2][2] - Wrk[2][1] * Wrk[1][2])) -
        (Wrk[0][1] * (Wrk[1][0] * Wrk[2][2] - Wrk[2][0] * Wrk[1][2])) +
        (Wrk[0][2] * (Wrk[1][0] * Wrk[2][1] - Wrk[2][0] * Wrk[1][1])));

      if (Abs(Xx) > EPSILON * Maxrange3) then
      begin
        if (Xx < 0) then
          Delta := 1
        else
          Delta := 0; // int delta = xx < 0 ? 1 : 0;
        Tetrahedron[NumTetrahedra][0] := A3s[I0][0];
        Tetrahedron[NumTetrahedra][1] := A3s[I0][1];
        Tetrahedron[NumTetrahedra][2] := A3s[I0][2 + Delta];
        Tetrahedron[NumTetrahedra][3] := A3s[I0][3 - Delta];
        Inc(NumTetrahedra);
      end;
    end;
  end;

  Result := 1;

ExitDelaunay:
  Tmp := nil;
  Id := nil;
  A3s := nil;
  Ccr := nil;
  Point := nil;
end;

end.
