// -------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
// -------------------------------------------------------------------------
(*  The unit to define a Superblock based on:

  ******************************************************************************
  * UNCERT
  * - A Geostatistical  Uncertainty Analysis Package Applied to Groundwater
  *   Flow and Contaminant Transport Modeling.
  *
  * Copyright (C) 1994 Colorado School of Mines, Department of Geology and
  * Geological Engineering.  All rights reserved.
  *
  * The programs in the UNCERT package are distributed in the hope that they
  * be useful, but WITHOUT ANY WARRANTY.  No author or distributor accepts
  * responsibility to anyone for the consequences of using them or for
  * whether they serve any paticular purpose or work at all, unless stated
  * so in writing.  No author or distributor accepts responsibility for the
  * quality of data generated, nor the damage to existing data.  Everyone is
  * granted permission to copy, modify, and redistribute the UNCERT program,
  * but only under the condition that this notice and the above copyright
  * notice remain intact.
  *
  * Developed By: William L. Wingle
  *     Sean A. McKenna
  *     Eileen P. Poeter
  *
  * January 20, 1994
  *
  ******************************************************************************
*)

unit uSuperblock;

interface

uses
  uInterpol;

(* Initialize_super_block(super_block, &search_pattern, &pattern_len);
   This subroutine is called once to initialize the super block arrays *)
procedure Initialize_Super_Block(var Super_Block: TCoordListArray;
  var Search_Pattern: TGridPatternArray; var Pattern_Len: integer);
function Get_Super_Block_Residents(List: PCoordList; var Len: integer): TCoordinateArray;
function Merge_Coordinate_Lists(La: TCoordinateArray; Len_a: integer;
  Lb: TCoordinateArray; Len_b: integer): TCoordinateArray;
(*
  dist_match = get_sb_nearest_neighbors(x, y, z, search_pattern, pattern_len,
       super_block, 10, &found, dist, xmin, ymin, zmin);
  This subroutine returns the requested number of nearest points to x,y,z
  and there realitive distance in a coord_match_array structure.
*)
function Get_Sb_Nearest_Neighbors(X, Y, Z: double;
  var Pattern: TGridPatternArray; Pattern_Len: integer;
  var Super_block: TCoordListArray; Count: integer; var Found: integer;
  Dist: double; Xmin, Ymin, Zmin: double): TCoordMatchArray; overload;
{ Returns array of points laying in the circle/sphere with given radius
  with the center at (x, y, z) }
function Get_Sb_Nearest_Neighbors(X, Y, Z: double;
  var Pattern: TGridPatternArray; Pattern_Len: integer;
  var Super_block: TCoordListArray; Radius: double; var Found: integer;
  Dist: double; Xmin, Ymin, Zmin: double): TCoordMatchArray; overload;
function Define_Search_Pattern(var Size: integer): TGridPatternArray;
procedure Insert_Super_Block(List: PCoordList; Sample: PCoordinate);
(* Load_super_block (super_block, sample, samples,
   xmin, &xmax, ymin, &ymax, zmin, &zmax, &dist);
   This subroutine is called every time a new dataset is loaded. *)
procedure Load_Super_Block(var Super_block: TCoordListArray;
  Sample: TCoordinateArray; Samples: integer; Xmin: double; var Xmax: double;
  Ymin: double; var Ymax: double; Zmin: double; var Zmax: double; var Dist: double);
procedure Cm_Sort_Points(var List: TCoordMatchArray; Size: integer);
procedure Release_Super_Block(Super_block: TCoordListArray;
  var Pattern: TGridPatternArray; Pattern_Len: integer);

// ==================================================================
implementation

// ==================================================================

function MAX(i, j: integer): integer; overload;
begin
  if i > j then
    Result := i
  else
    Result := j;
end;

// ---------------------------------------------------------------------------

function MIN(i, j: integer): integer;
begin
  if i < j then
    Result := i
  else
    Result := j;
end;

// ---------------------------------------------------------------------------

function MAX(i, j: double): double; overload;
begin
  if i > j then
    Result := i
  else
    Result := j;
end;

// ---------------------------------------------------------------------------

function Define_Search_Pattern(var Size: integer): TGridPatternArray;
var
  Dist: array [0 .. SB_NUM_BLOCK - 1] of array [0 .. SB_NUM_BLOCK - 1] of array
    [0 .. SB_NUM_BLOCK - 1] of double;
  Pattern: TGridPatternArray;
  i, ii, j, k, pos, Value, step: integer;
begin
  step := 0;
  for i := 0 to SB_NUM_BLOCK - 1 do
    for j := 0 to SB_NUM_BLOCK - 1 do
      for k := 0 to SB_NUM_BLOCK - 1 do
        Dist[i, j, k] := sqrt(((i * i) + (j * j) + (k * k)));

  Size := Trunc(Dist[SB_NUM_BLOCK - 1][SB_NUM_BLOCK - 1][SB_NUM_BLOCK - 1] + 1);
  SetLength(Pattern, Size);

  Pattern[0].Size := 1;
  SetLength(Pattern[0].Pattern, 1);
  Pattern[0].Pattern[0].X := 0;
  Pattern[0].Pattern[0].Y := 0;
  Pattern[0].Pattern[0].Z := 0;

  step := 1;
  ii := 1;
  for i := 1 to Size - 1 do
  begin
    j := i * 2 + 1;
    k := j * j * j;
    step := k - ii;
    ii := k;
    Pattern[i].Size := 0;
    SetLength(Pattern[i].Pattern, step);
  end;

  for k := 0 to 19 do
    for i := 0 to 19 do
      for j := 0 to 19 do
      begin
        if ((i <> 0) and (j <> 0)) then
        begin
          Value := MAX(i, MAX(j, k));
          if (k = 0) then
          begin
            if (Value > Dist[i - 1][j - 1][0]) then
              pos := Value
            else
              pos := Trunc(Dist[i - 1][j - 1][0]) + 1;
          end
          else
          begin
            if (Value > Dist[i - 1][j - 1][k - 1]) then
              pos := Value
            else
              pos := Trunc(Dist[i - 1][j - 1][k - 1]) + 1;
          end;
        end
        else if (k <> 0) then
        begin
          Value := MAX(i, MAX(j, k));
          if (j <> 0) then
          begin
            if (Value > Dist[0][j - 1][k - 1]) then
              pos := Value
            else
              pos := Trunc(Dist[0][j - 1][k - 1]) + 1;
          end
          else if (i <> 0) then
          begin
            if (Value > Dist[i - 1][0][k - 1]) then
              pos := Value
            else
              pos := Trunc(Dist[i - 1][0][k - 1]) + 1;
          end
          else
            pos := Trunc(Dist[0][0][k]);
        end
        else
          pos := Trunc(Dist[i][j][k]);

        ii := Pattern[pos].Size;
        if (i <> 0) and (j <> 0) then
        begin
          Pattern[pos].Pattern[ii].X := i;
          Pattern[pos].Pattern[ii].Y := j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := -i;
          Pattern[pos].Pattern[ii].Y := -j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := i;
          Pattern[pos].Pattern[ii].Y := -j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := -i;
          Pattern[pos].Pattern[ii].Y := j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);

          if (k <> 0) then
          begin
            Pattern[pos].Pattern[ii].X := i;
            Pattern[pos].Pattern[ii].Y := j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
            Pattern[pos].Pattern[ii].X := -i;
            Pattern[pos].Pattern[ii].Y := -j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
            Pattern[pos].Pattern[ii].X := i;
            Pattern[pos].Pattern[ii].Y := -j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
            Pattern[pos].Pattern[ii].X := -i;
            Pattern[pos].Pattern[ii].Y := j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
          end;
        end
        else if (j <> 0) then
        begin
          Pattern[pos].Pattern[ii].X := 0;
          Pattern[pos].Pattern[ii].Y := j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := 0;
          Pattern[pos].Pattern[ii].Y := -j;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);

          if (k <> 0) then
          begin
            Pattern[pos].Pattern[ii].X := 0;
            Pattern[pos].Pattern[ii].Y := j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
            Pattern[pos].Pattern[ii].X := 0;
            Pattern[pos].Pattern[ii].Y := -j;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
          end;
        end
        else if (i <> 0) then
        begin
          Pattern[pos].Pattern[ii].X := i;
          Pattern[pos].Pattern[ii].Y := 0;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := -i;
          Pattern[pos].Pattern[ii].Y := 0;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);

          if (k <> 0) then
          begin
            Pattern[pos].Pattern[ii].X := i;
            Pattern[pos].Pattern[ii].Y := 0;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
            Pattern[pos].Pattern[ii].X := -i;
            Pattern[pos].Pattern[ii].Y := 0;
            Pattern[pos].Pattern[ii].Z := -k;
            Inc(ii);
          end;
        end
        else if ((k <> 0) and (i = 0) and (j = 0)) then
        begin
          Pattern[pos].Pattern[ii].X := 0;
          Pattern[pos].Pattern[ii].Y := 0;
          Pattern[pos].Pattern[ii].Z := k;
          Inc(ii);
          Pattern[pos].Pattern[ii].X := 0;
          Pattern[pos].Pattern[ii].Y := 0;
          Pattern[pos].Pattern[ii].Z := -k;
          Inc(ii);
        end;

        Pattern[pos].Size := ii;
      end;

  Result := Pattern;
end;

// ---------------------------------------------------------------------------

function Get_Sb_Nearest_Neighbors(X, Y, Z: double;
  var Pattern: TGridPatternArray; Pattern_Len: integer;
  var Super_block: TCoordListArray; Count: integer; var Found: integer;
  Dist: double; Xmin, Ymin, Zmin: double): TCoordMatchArray; overload;

var
  Cont: boolean;
  Neighbors, cell_list: TCoordinateArray;
  Dist_match: TCoordMatchArray;
  dx, dy, dz: double;
  i, ii, j, Len, px, py, pz, sx, sy, sz, opx, opy, opz, ofound: integer;
begin
  Neighbors := nil;
  Dist_match := nil;
  opx := -1;
  opy := -1;
  opz := -1;
  ofound := -1;
  px := Trunc((X - Xmin) / Dist);
  py := Trunc((Y - Ymin) / Dist);
  pz := Trunc((Z - Zmin) / Dist);

  if ((px <> opx) or (py <> opy) or (pz <> opz)) then
  begin
    Cont := True;
    Found := 0;
    Neighbors := nil;
    i := 0;
    repeat
      if (Found > Count) then
        Cont := False;

      for j := 0 to Pattern[i].Size - 1 do
      begin
        sx := px + Pattern[i].Pattern[j].X;
        sy := py + Pattern[i].Pattern[j].Y;
        sz := pz + Pattern[i].Pattern[j].Z;
        if ((sx >= 0) and (sx < SB_NUM_BLOCK) and (sy >= 0) and
          (sy < SB_NUM_BLOCK) and (sz >= 0) and (sz < SB_NUM_BLOCK)) then
        begin
          cell_list := Get_Super_Block_Residents(@Super_block[sz][sy][sx], Len);
          if (Len > 0) then
          begin
            if (Neighbors = nil) then
              Neighbors := cell_list
            else
              Neighbors := Merge_Coordinate_Lists(Neighbors, Found,
                cell_list, Len);
            Found := Found + Len;
          end;
        end;
      end;
      Inc(i);
    until not(Cont and (i < Pattern_Len));
  end
  else
    Found := ofound;

  SetLength(Dist_match, Found);
  for ii := 0 to Found - 1 do
  begin
    dx := Neighbors[ii].X - X;
    dy := Neighbors[ii].Y - Y;
    dz := Neighbors[ii].Z - Z;

    Dist_match[ii].id := Neighbors[ii].id;
    Dist_match[ii].Value := Neighbors[ii].Value;
    Dist_match[ii].distance := (dx * dx) + (dy * dy) + (dz * dz);
  end;

  Cm_Sort_Points(Dist_match, Found);

  for ii := 0 to MIN(Found, Count) - 1 do
    Dist_match[ii].distance := sqrt(Dist_match[ii].distance);

  opx := px;
  opy := py;
  opz := pz;
  ofound := Found;

  Result := Dist_match;
end;

// ---------------------------------------------------------------------------

function Get_Sb_Nearest_Neighbors(X, Y, Z: double;
  var Pattern: TGridPatternArray; Pattern_Len: integer;
  var Super_block: TCoordListArray; Radius: double; var Found: integer;
  Dist: double; Xmin, Ymin, Zmin: double): TCoordMatchArray; overload;
var
  Cont: boolean;
  Neighbors, cell_list: TCoordinateArray;
  Dist_match: TCoordMatchArray;
  r, dx, dy, dz: double;
  i, ii, j, Len, px, py, pz, sx, sy, sz, opx, opy, opz, ofound: integer;
begin
  Neighbors := nil;
  Dist_match := nil;
  opx := -1;
  opy := -1;
  opz := -1;
  ofound := -1;
  px := Trunc((X - Xmin) / Dist);
  py := Trunc((Y - Ymin) / Dist);
  pz := Trunc((Z - Zmin) / Dist);

  if ((px <> opx) or (py <> opy) or (pz <> opz)) then
  begin
    Cont := True;
    Found := 0;
    Neighbors := nil;
    i := 0;
    repeat
      if (r > Radius) then
        Cont := False;

      for j := 0 to Pattern[i].Size - 1 do
      begin
        sx := px + Pattern[i].Pattern[j].X;
        sy := py + Pattern[i].Pattern[j].Y;
        sz := pz + Pattern[i].Pattern[j].Z;
        if ((sx >= 0) and (sx < SB_NUM_BLOCK) and (sy >= 0) and
          (sy < SB_NUM_BLOCK) and (sz >= 0) and (sz < SB_NUM_BLOCK)) then
        begin
          cell_list := Get_Super_Block_Residents(@Super_block[sz][sy][sx], Len);
          if (Len > 0) then
          begin
            if (Neighbors = nil) then
              Neighbors := cell_list
            else
              Neighbors := Merge_Coordinate_Lists(Neighbors, Found,
                cell_list, Len);

            Found := Found + Len;
          end;
        end;
      end;

      for ii := 0 to Found - 1 do
      begin
        dx := Neighbors[ii].X - X;
        dy := Neighbors[ii].Y - Y;
        dz := Neighbors[ii].Z - Z;
        r := (dx * dx) + (dy * dy) + (dz * dz);
        if r > Radius then
          break;
      end;

      Inc(i);
    until not(Cont and (i < Pattern_Len));
  end
  else
    Found := ofound;

  if Found <> ofound then
  begin
    SetLength(Dist_match, Found);
    for ii := 0 to Found - 1 do
    begin
      dx := Neighbors[ii].X - X;
      dy := Neighbors[ii].Y - Y;
      dz := Neighbors[ii].Z - Z;

      Dist_match[ii].id := Neighbors[ii].id;
      Dist_match[ii].Value := Neighbors[ii].Value;
      Dist_match[ii].distance := sqrt((dx * dx) + (dy * dy) + (dz * dz));
    end;

    Cm_Sort_Points(Dist_match, Found);

    ii := Found - 1;
    while (ii >= 0) and (Dist_match[ii].distance > Radius) do
      Dec(ii);

    SetLength(Dist_match, ii + 1);
    Found := ii + 1;
  end;

  Result := Dist_match;
end;

// ---------------------------------------------------------------------------

function Get_Super_Block_Residents(List: PCoordList; var Len: integer)
  : TCoordinateArray;
var
  temp: array [0 .. 10000 - 1] of TCoordinate;
  residents: TCoordinateArray;
  prt: PCoordList;
  i: integer;
begin
  i := 0;
  prt := List;
  while (prt^.Next <> nil) do
  begin
    prt := prt^.Next;
    temp[i].id := (prt^.item)^.id;
    temp[i].X := (prt^.item)^.X;
    temp[i].Y := (prt^.item)^.Y;
    temp[i].Z := (prt^.item)^.Z;
    temp[i].Value := (prt^.item)^.Value;
    Inc(i);
  end;
  Len := i;

  SetLength(residents, Len);
  for i := 0 to Len - 1 do
  begin
    residents[i].id := temp[i].id;
    residents[i].X := temp[i].X;
    residents[i].Y := temp[i].Y;
    residents[i].Z := temp[i].Z;
    residents[i].Value := temp[i].Value;
  end;

  Result := residents;
end;

// ---------------------------------------------------------------------------

procedure Initialize_Super_Block(var Super_block: TCoordListArray;
  var Search_Pattern: TGridPatternArray; var Pattern_Len: integer);
var
  i, j, k: integer;
begin
  Search_Pattern := Define_Search_Pattern(Pattern_Len);

  for k := 0 to SB_NUM_BLOCK - 1 do
    for j := 0 to SB_NUM_BLOCK - 1 do
      for i := 0 to SB_NUM_BLOCK - 1 do
        Super_block[k][j][i].Next := nil;
end;

// ---------------------------------------------------------------------------

procedure Insert_Super_Block(List: PCoordList; Sample: PCoordinate);
var
  t, prt: PCoordList;
begin
  prt := List;
  while (prt^.Next <> nil) do
    prt := prt^.Next;

  New(t);
  prt^.Next := t;
  prt := prt^.Next;
  prt^.item := Sample;
  prt^.Next := nil;
end;

// ---------------------------------------------------------------------------

procedure Load_Super_Block(var Super_block: TCoordListArray;
  Sample: TCoordinateArray; Samples: integer; Xmin: double; var Xmax: double;
  Ymin: double; var Ymax: double; Zmin: double; var Zmax: double;
  var Dist: double);
var
  dsx, dsy, dsz: double;
  i: integer;
  px, py, pz: integer;
begin
  Xmax := Xmax + Xmax / 1000.0;
  Ymax := Ymax + Ymax / 1000.0;
  Zmax := Zmax + Zmax / 1000.0;

  dsx := (Xmax - Xmin) / SB_NUM_BLOCK;
  dsy := (Ymax - Ymin) / SB_NUM_BLOCK;
  dsz := (Zmax - Zmin) / SB_NUM_BLOCK;
  Dist := MAX(dsx, MAX(dsy, dsz));

  for i := 0 to Samples - 1 do
  begin
    px := Trunc((Sample[i].X - Xmin) / Dist);
    py := Trunc((Sample[i].Y - Ymin) / Dist);
    pz := Trunc((Sample[i].Z - Zmin) / Dist);

    Insert_Super_Block(@Super_block[pz][py][px], @Sample[i]);
  end;

end;

// ---------------------------------------------------------------------------

function Merge_Coordinate_Lists(La: TCoordinateArray; Len_a: integer;
  Lb: TCoordinateArray; Len_b: integer): TCoordinateArray;
var
  List: TCoordinateArray;
  i, n: integer;
begin
  SetLength(List, Len_a + Len_b);
  n := 0;
  for i := 0 to Len_a - 1 do
  begin
    List[n].id := La[i].id;
    List[n].X := La[i].X;
    List[n].Y := La[i].Y;
    List[n].Z := La[i].Z;
    List[n].Value := La[i].Value;

    Inc(n);
  end;
  for i := 0 to Len_b - 1 do
  begin
    List[n].id := Lb[i].id;
    List[n].X := Lb[i].X;
    List[n].Y := Lb[i].Y;
    List[n].Z := Lb[i].Z;
    List[n].Value := Lb[i].Value;

    Inc(n);
  end;

  SetLength(La, 0);
  SetLength(Lb, 0);
  Result := List;
end;

// ---------------------------------------------------------------------------

procedure Release_Super_Block(Super_block: TCoordListArray;
  var Pattern: TGridPatternArray; Pattern_Len: integer);
var
  i, j, k: integer;
  prt, t: PCoordList;
begin
  // releasing lists of points for superblock structure
  for k := 0 to SB_NUM_BLOCK - 1 do
    for j := 0 to SB_NUM_BLOCK - 1 do
      for i := 0 to SB_NUM_BLOCK - 1 do
      begin
        prt := @Super_block[k][j][i];
        prt := prt^.Next;
        while prt <> nil do
        begin
          t := prt^.Next;
          Dispose(prt);
          prt := t;
        end;
      end;
  // releasing pattern structure
  for i := 0 to Pattern_Len - 1 do
    SetLength(Pattern[i].Pattern, 0);
  SetLength(Pattern, 0);
end;

// ---------------------------------------------------------------------------

procedure Cm_Sort_Points(var List: TCoordMatchArray; Size: integer);
var
  temp: TCoordMatch;
  i, j: integer;
begin
  if (Size > 0) then
  begin
    for j := Size - 2 downto 0 do
    begin
      i := j + 1;
      temp.id := List[j].id;
      temp.Value := List[j].Value;
      temp.distance := List[j].distance;

      while ((i < Size) and (temp.distance > List[i].distance)) do
      begin
        List[i - 1].id := List[i].id;
        List[i - 1].Value := List[i].Value;
        List[i - 1].distance := List[i].distance;
        Inc(i);
      end;

      List[i - 1].id := temp.id;
      List[i - 1].Value := temp.Value;
      List[i - 1].distance := temp.distance;
    end;
  end;
end;

end.
