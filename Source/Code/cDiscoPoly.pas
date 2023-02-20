//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
(*-------------------------------------------------------------------
  The functions that pertain polygons.
---------------------------------------------------------------------*)

unit cDiscoPoly;

interface

uses
  System.Math,
  cDiscoCore;


//-------------------------------------------------------------------
// AngleDistance computes a pseudo angle and distance that react the
//  same as an exact angle and distance.
// ------------------------------------------------------------------
procedure AngleDistance(p1, p2: TPoint2D; a, d: double);
//-------------------------------------------------------------------
//  ExtremePnts returns the points of a quadrilateral that fits inside
//  the point set.
// -------------------------------------------------------------------
procedure ExtremePnts(pointset: xypolygon; nrpnts, bigx, bigy, smallx, smally: integer;
  poly: xypolygon);
(*----------------------------------------------------------------------
 Puts a point on the heap
----------------------------------------------------------------------*)
procedure PutPointOnHeapin(rootin: polypntr; pointset: xypolygon; idx: integer);

procedure PutPointOnHeapOut(rootout: polypntr; pointset: xypolygon; idx: integer);

(*----------------------------------------------------------------------
  ConvexHull returns the points within a random set of points that make
   up the convex hull of the set of points.
----------------------------------------------------------------------*)
procedure ConvexHull(pointset: xypolygon; nrpnts, nrhpnts: integer);
(*------------------------------------------------------------------------
  Inside determines if a point (Pnt) is inside or outside of a polygon
   (poly).  If the point is inside the polygon the function returns a
   boolean true else a false.
--------------------------------------------------------------------------*)
function Inside(pnt: TPoint2D; poly: xy4polygon; var nrpnts, xpntr: integer): boolean;

function NoPtsInTriangle(poly: polyarray; v: IA; h, i, j, mnrv: integer): boolean;

function PtsInSquare(poly: polyarray; v: IA; h, i, j, mnrv: integer): boolean;

function CounterClockWise(poly: polyarray; v: IA; h, j, i: integer): boolean;

procedure DecomposePoly(poly: polyarray; nrpnts: integer; MaxDiag: double;
  var pntr: PpolyRec);
function InPolygon(pnt: TPoint2D; poly: xypolygon; var nrpnts, xpntr: integer): boolean;

procedure RearrangePnts(poly: xypolygon; nrpnts: integer; root: polypntr);

function OrderPolygon(gmode: integer; var poly: xypolygon; nrpnts: integer): integer;

(*---------------------------------------------------------------------------
 PolygonPnts returns the length of each side and XY coordinates of each
 vertex of a regular polygon circumscribed by a circle.
----------------------------------------------------------------------------*)
procedure PolygonPnts(cp: TPoint2D; rad: double; polysides: integer;
  regpoly: xypolygon; sidelen: double);

(*-------------------------------------------------------------------------
 PolygonType determines if the polygon in variable poly is convex, concave or collinear.
--------------------------------------------------------------------------*)
procedure PolygonType(poly: xypolygon; nrpnts: integer; var polytype: integer);

(*------------------------------------------------------------------------
 PolyRectangle returns the coordinates of a rectangle (pr) that encloses a polygon (Poly)
-------------------------------------------------------------------------*)
procedure PolyRectangle(poly: xypolygon; nrpnts: integer; var pr: LineSeg2D);

function Hull_Poly(pp: ppolyarray; pnpnts: integer; ch: ppolyarray;
  var cnpnts: integer): integer;
function DiamPolygon(npnts: integer; pl: xypolygon; ndiags: integer;
  diag_lst: xypolygon; diam: array of TPoint2D; diam_lenghth: double): double;
function IntPolygones(intrs, pnpnts, qnpnts: integer; pp, pq: xypolygon): integer;

function PoinToPoly(v0, y: TPoint2D; w1, w2, pnpnts: integer;
  dd: double; p: xypolygon): double;
function PltoGol(pnpnts, qnpnts: integer; pp, pq: xypolygon;
  sp, ep: TPoint2D; hdist: double): double;
function PolToPol(pnpnts, qnpnts: integer; pp, pq: xypolygon;
  sp, ep: TPoint2D; hdist: double): double;


implementation

(*-------------------------------------------------------------------
  BuildPolygon creates a double linked list of points on a polygon in
  dynamic memory.
 --------------------------------------------------------------------*)
procedure BuildPolygon(PolyRoot, PolyRec: PolyPntr);
var
  CurrPntr: polypntr;
  LastPntr: polypntr;
begin
  if PolyRoot = nil then
  begin  //Create new linked list
    New(PolyRoot);
    PolyRec^.Next := nil;
    PolyRec^.prev := nil;
    PolyRoot      := PolyRec;
  end
  else
  begin  // List Exists, add record to the end of list
    CurrPntr := PolyRoot;
    repeat
      LastPntr := CurrPntr;
      CurrPntr := CurrPntr^.Next;
    until ((CurrPntr <> nil) or (CurrPntr^.seqnr < PolyRec^.seqnr));
    if (PolyRoot^.seqnr > PolyRec^.seqnr) then
    begin  (*Insert new point in front of list*)
      New(PolyRoot);
      PolyRec^.prev := nil;
      PolyRec^.Next := LastPntr;
      LastPntr^.prev := PolyRoot;
      PolyRoot := PolyRec;
    end
    else
    begin  (*Add point to middle or end*)
      New(LastPntr^.Next);
      PolyRec^.Next  := CurrPntr;
      PolyRec^.prev  := LastPntr;
      LastPntr^.Next := PolyRec;
    end; //else
  end; //else
end; //BuildPolygon

procedure AngleDistance(p1, p2: TPoint2D; a, d: double);
var
  dx, dy: double;    //Change in X and Y respectively
begin
  dx := p2.x - p1.x;
  dy := p2.y - p1.y;
  if (dx = 0) and (dy = 0) then
    a := 0.0 (*Same Point*)
  else
    a := dy / (abs(dx) + abs(dy));
  if (dx < 0) then
    a := 2 - a
  else if (dy < 0) then
    a := 4 + a;
  a := a * 90.0;
  d := SumSqrdParms(dx, dy);
end; (*AngleDistance*)

procedure ExtremePnts(PointSet: XYPolygon; NrPnts, bigx, bigy, smallx, smally: integer;
  poly: XYPolygon);
var
  i: integer;
  xmax, ymax, Xmin, Ymin: double; (*Temp Hold Areas*)
begin
  Xmin := MAXREAL;
  Ymin := MAXREAL;
  xmax := MINREAL;
  ymax := MINREAL;
  for i := 0 to NrPnts do
  begin
    if (PointSet[i].x > xmax) then
    begin
      bigx := i;
      xmax := PointSet[i].x;
    end;
    if ((PointSet[i].x = xmax) and (PointSet[i].y > PointSet[bigx].y)) then
      bigx := i;
    if (PointSet[i].y < Ymin) then
    begin
      smally := i;
      Ymin   := PointSet[i].y;
    end;
    if ((PointSet[i].y = Ymin) and (PointSet[i].x > PointSet[smally].x)) then
      smally := i;
    if (PointSet[i].x < Xmin) then
    begin
      smallx := i;
      Xmin   := PointSet[i].x;
    end;
    if ((PointSet[i].x = Xmin) and (PointSet[i].y > PointSet[smallx].y)) then
      smallx := i;
    if (PointSet[i].y > ymax) then
    begin
      bigy := i;
      ymax := PointSet[i].y;
    end;
    if ((PointSet[i].y = ymax) and (PointSet[i].x > PointSet[bigy].x)) then
      bigy := i;
  end; (* for i *)
       //   setgeostatus(44);
  if ((bigy <> bigx) and (bigy <> smallx)) then
    if ((bigx <> smally) and (smally <> smallx)) then
    begin
      poly[0] := PointSet[smally];
      poly[1] := PointSet[bigx];
      poly[2] := PointSet[bigy];
      poly[3] := PointSet[smallx];
      Exit;
    end;
end; (*ExtremePnts*)

procedure PutPointOnHeapin(rootin: polypntr; pointset: XYPolygon; idx: integer);
var
  PolyRec: polypntr; (*Heap and Temp Polygon Record*)
begin
  PolyRec^.seqnr := idx;
  PolyRec^.pnt   := PointSet[idx];
  BuildPolygon(rootin, PolyRec);
end; (*PutPointOnHeapin*)


procedure PutPointOnHeapOut(rootout: polypntr; pointset: XYPolygon; idx: integer);
var
  PolyRec: polypntr; (*Heap and Temp Polygon Record*)

  (*Puts a point on the heap*)
begin
  PolyRec^.seqnr := idx;
  PolyRec^.pnt   := PointSet[idx];
  BuildPolygon(rootout, PolyRec);
end; (*PutPointOnHeapOut*)


procedure ConvexHull(Pointset: XYPolygon; NrPnts, NrhPnts: integer);
var
  xmax, ymax, Xmin, Ymin: double;      (*Temp Hold Areas*)
  bigx, bigy, smallx, smally: integer; (*Pointers to XY coordinates*)
  poly:  XYPolygon;                    (*Four points of quadrilateral*)
  spnt:  integer;                      (*Start Point*)
  rootin, rootout, CurrPntr: polypntr; (*Root Pntr to points in and out Quad*)
  i:     integer;
  tnp, tNrPnts: integer;
  seqnr: integer;           (*Counter*)
  PolyRec: polypntr;        (*Heap and Temp Polygon Record*)
  curangle, minangle,       (*Curangle and Minimum angles between points*)
  a, mindist,               (*Minimum Distance*)
  d:     double;
  t:     TPoint2D;            (*Temp Storage*)
begin
  tNrPnts := NrPnts - 1;
  if (tNrPnts < 3) then
    // setgeostatus(42);  (*Point set must be > 2*)
  begin
    ExtremePnts(PointSet, tNrPnts, bigx, bigy, smallx, smally, poly);
    spnt    := 0; (*Eliminate Points in quadrilateral.*)
    seqnr   := -4;
    rootin  := nil;
    rootout := nil;
    PolyRec^.seqnr := 0;
    for i := 0 to 3 do
    begin
      PolyRec^.seqnr := seqnr + 1;
      PolyRec^.pnt   := poly[i];
      BuildPolygon(rootout, PolyRec);
    end;
    tnp := 4;
    for i := 0 to tNrPnts do
      (*Gather all points not in Quadrilateral and place on Heap*)
      if ((((((i <> bigx) and (i <> bigy)) and (i <> smallx)) and
        (i <> smally)))) then
        if (InPolygon(PointSet[i], poly, tnp, spnt)) then
          PutPointOnHeapOut(rootout, PointSet, i)
        else
          PutPointOnHeapin(rootin, PointSet, i);
  end (*If GeoStatus Ext*)
  else
  begin (*No Quadrilateral so use all points*)
    PolyRec^.seqnr := 0;
    PolyRec^.pnt   := PointSet[smally];
    BuildPolygon(rootout, PolyRec);
    for i := 0 to tNrPnts do
      (*Gather all points and place on Heap*)
      if (i <> smally) then
        PutPointOnHeapOut(rootout, PointSet, i);
  end;
  tnp      := 0;
  CurrPntr := rootout;
  repeat
    PointSet[tnp] := CurrPntr^.pnt;
    CurrPntr      := CurrPntr^.Next;
    Inc(tnp);
  until (CurrPntr <> nil);
  //     DisposePolygon(rootout);
  nrhpnts  := 0;
  spnt     := 0;
  minangle := 0.0;
  mindist  := MAXREAL;
  repeat
    t    := PointSet[nrhpnts];
    PointSet[nrhpnts] := PointSet[spnt];
    PointSet[spnt] := t;
    spnt := tnp + 1;
    curangle := minangle;
    minangle := 360.0;
    for i := (nrhpnts + 1) to tnp do
    begin
      AngleDistance(PointSet[nrhpnts], PointSet[i], a, d);
      if (a >= curangle) then
        if (a <= minangle) then
          if (a = minangle) then
          begin
            if (d < mindist) then
            begin
              spnt     := i;
              minangle := a;
              mindist  := d;
            end;
          end
          else
          begin
            spnt     := i;
            minangle := a;
            mindist  := d;
          end;
    end; (*for i*)
    Inc(nrhpnts);
  until (spnt <> tnp + 1);
  if (rootin <> nil) then
  begin  (*Reload polygon with points in quadrilateral*)
    tnp      := nrhpnts;
    CurrPntr := rootin;
    repeat
      Inc(tnp);
      PointSet[tnp] := CurrPntr^.pnt;
      CurrPntr      := CurrPntr^.Next;
    until (CurrPntr^.Next <> nil);
    //       disposepolygon(rootin);
  end;
end; (*ConvexHull*)

function Inside(pnt: TPoint2D; poly: xy4polygon; var NrPnts, xpntr: integer): boolean;
var
  Count:     integer;        (*Number of Intersections*)
  i, j:      integer;        (*Counter*)
  lastpnt:   integer;        (*Pointer to last point not on the scan line.*)
  pnt1, pnt2: TPoint2D;      (*Misc holding area.*)
  l1, l2:    LineSeg2D;      (*Line segments between points.*)
  xmax:      double;         (*Maximum X coordiante + 1000.*)
  intp:      TPoint2D;
  IntExists: boolean;
  ile, ile1: implineeq;
begin
  if (NrPnts < 3) then
  begin
    (*Polygon points must be > 2*)
    Inside := False;
    exit;
  end;
  if (xpntr = 0) then
  begin  (*Find Point with largest X Value*)
    xmax := MINREAL;
    for i := 0 to NrPnts - 1 do
      if (poly[i].x > xmax) then
      begin
        xpntr := i;
        xmax  := poly[i].x;
      end;
  end;
  xmax    := poly[xpntr].x + 1000.0;
  Count   := 0;
  lastpnt := 0;
  pnt1    := poly[NrPnts - 1];
  pnt2    := poly[0];
  l1.p1   := pnt;
  l1.p2   := pnt;
  l1.p2.x := xmax;
  (*build scan line*)
  LineEquation(0, l1.p1, l1.p2, ile, pleglb); (*Build Scan Line Infinite*)
  for i := 1 to NrPnts do
  begin
    j     := i - 1;
    l2.p1 := poly[j];
    l2.p2 := poly[j];
    if not (Intersectf(l2, l1)) then
    begin  (*Does not Intersect with a vertex*)
      if lastpnt = 0 then
        l2.p2 := pnt1
      else
        l2.p2 := poly[lastpnt - 1];

      if ((i - lastpnt) > 1) then
      begin  (*Last Intersection was thru a vertex*)
        LineEquation(0, l2.p1, l2.p2, ile1, pleglb);
        Intersect(0, ile, ile1, pleglb1, pleglb2, l1, l2, intp, IntExists);
        if IntExists then
          Inc(Count);
      end
      else if Intersectf(l2, l1) then
        Inc(Count);
      lastpnt := i;
    end;
  end; (*for i*)
  inside := Count div 2 = 1;
end; (*inside*)

(*-----------------------------------------------------------------*)
function NoPtsInTriangle(poly: polyarray; v: IA; h, i, j, mnrv: integer): boolean;
var
  a: xy4polygon;
  lp, ii, bp: integer;
  Fretval: boolean;
begin
  a[0]    := poly[h];
  a[1]    := poly[i];
  a[2]    := poly[j];
  a[3]    := poly[h];
  ii      := 0;
  lp      := 0;
  bp      := 3;
  Fretval := True;
  while (ii <= mnrv) do
  begin
    if (ii <> h) and (ii <> i) and (ii <> j) then
    begin
      if inside(poly[v[ii]], a, bp, lp) then
      begin
        Fretval := False;
        ii      := mnrv + 1;
      end;
    end;
    Inc(ii);
  end;
  NoPtsInTriangle := Fretval;
end; (*NoPtsInTriangle*)

(*-----------------------------------------------------------------*)
function PtsInSquare(poly: polyarray; v: IA; h, i, j, mnrv: integer): boolean;
var
  ii:      integer;
  p1, p2:  TPoint2D;
  rect:    LineSeg2D;
  Fretval: boolean;
begin (*PtsInSquare*)
  rect.p2.x := max(poly[v[h]].x, poly[v[i]].x);
  rect.p2.x := max(poly[v[j]].x, rect.p2.x);
  rect.p2.y := max(poly[v[h]].y, poly[v[i]].y);
  rect.p2.y := max(poly[v[j]].y, rect.p2.y);
  rect.p1.x := min(poly[v[h]].x, poly[v[i]].x);
  rect.p1.x := min(poly[v[j]].x, rect.p1.x);
  rect.p1.y := min(poly[v[h]].y, poly[v[i]].y);
  rect.p1.y := min(poly[v[j]].y, rect.p1.y);
  ii      := 0;
  Fretval := False;
  while (ii <= mnrv) do
  begin
    if (ii <> h) and (ii <> i) and (ii <> j) then
    begin
      if inrectangle(rect, poly[v[ii]]) then
      begin
        p1 := poly[v[ii]];
        p2 := poly[v[h]];
        if not EqualPoints2D(p1, p2) then
        begin
          p2 := poly[v[i]];
          if not EqualPoints2D(p1, p2) then
          begin
            p2 := poly[v[j]];
            if not EqualPoints2D(p1, p2) then
            begin
              Fretval := True;
              ii      := mnrv + 1;
            end;
          end;
        end;
      end;
    end;
    Inc(ii);
  end;
  PtsInSquare := Fretval;
end; (*PtsInSquare*)

(*-----------------------------------------------------------------*)

function CounterClockWise(poly: polyarray; v: IA; h, j, i: integer): boolean;
var
  Value: double;
begin (*CounterClockWise*)
  Value := poly[v[h]].x * poly[v[i]].y + poly[v[h]].y * poly[v[j]].x +
    poly[v[j]].y * poly[v[i]].x - poly[v[h]].y * poly[v[i]].x -
    poly[v[i]].y * poly[v[j]].x - poly[v[h]].x * poly[v[j]].y;
  CounterClockWise := Value > 0;
end; (*CounterClockWise*)

(*---------------------------------------------------------------------------
 DecomposePoly decomposes a polygon into a series of convex polygons
----------------------------------------------------------------------------*)
procedure DecomposePoly(poly: polyarray; NrPnts: integer; MaxDiag: double;
  var pntr: PPolyRec);
var
  h, i, j, k, l, nrv, mnrv: integer;      (*Number of vertices in polygon*)
  dd, MinDiag: double;
  Imin: integer;
  tr:   TPolyRec;
  tpntr, CurrPntr: PPolyRec;
  v:    IA;
begin (*DecomposePoly*)
  for i := 0 to NrPnts do
    v[i] :={NrPnts-}i;
  pntr := nil;
  CurrPntr := nil;
  nrv      := NrPnts;
  mnrv     := NrPnts - 1;
  while (nrv >= 3) do
  begin
    MinDiag := MaxDiag;
    for i := 0 to mnrv do
    begin
      if i = mnrv then
        j := 0
      else
        j := i + 1;
      if i = 0 then
        h := nrv - 1
      else
        h := i - 1;

      dd := Distance2D(poly[v[h]], poly[v[j]]);
      if not (CounterClockWise(poly, v, h, i, j)) and (dd < MinDiag) then
      begin
        if (PtsInSquare(Poly, v, h, i, j, mnrv)) then
        begin
          if (NoPtsInTriangle(Poly, v, h, i, j, mnrv)) then
          begin
            MinDiag := dd;
            Imin    := i;
          end;
        end
        else
        begin
          MinDiag := dd;
          Imin    := i;
        end;
      end;
    end;
    i := Imin;
    if i = 0 then
      h := mnrv
    else
      h := i - 1;
    if i = mnrv then
      j := 0
    else
      j := i + 1;
    if (MinDiag = MaxDiag) then
    //setgeostatus(66)
    else
    begin
      tr.v1  := Poly[v[h]];
      tr.v2  := Poly[v[i]];
      tr.v3  := Poly[v[j]];
      tr.nxt := nil;
      if (pntr = nil) then
      begin
        New(pntr);
        TPolyRec(pntr^) := tr;
        CurrPntr := pntr;
      end
      else
      begin
        New(CurrPntr^.nxt);
        TPolyRec(CurrPntr^.nxt^) := tr;
        CurrPntr := CurrPntr^.nxt;
        tpntr    := pntr^.nxt;
      end;
      Dec(nrv);
      mnrv := nrv - 1;
      for l := i to NrPnts - 1 do
        v[l] := v[l + 1];
    end; (* else *)
  end; (*while nrv*)
end; (*DecomposePoly*)


(*----------------------------------------------------------------
  InPolygon determines if a point (pnt) is inside or
  outside of a polygon (poly)
  ----------------------------------------------------------------*)
function InPolygon(pnt: TPoint2D; poly: XYPolygon; var NrPnts, xpntr: integer): boolean;
var
  Count:     integer;          (*Number of Intersections*)
  i, j:      integer;          (*Counter*)
  lastpnt:   integer;          (*Pointer to last point not on the scan line.*)
  pnt1, pnt2: TPoint2D;        (*Misc holding area.*)
  l1, l2:    LineSeg2D;        (*Line segments between points.*)
  xmax:      double;           (*Maximum X coordiante + 1000.*)
  intp:      TPoint2D;
  IntExists: boolean;
  ile, ile1: implineeq;
begin
  if (NrPnts < 3) then
  begin
    InPolygon := False;  (*Polygon points must be > 2*)
    exit;
  end;
  if (xpntr = 0) then
  begin  (*Find Point with largest X Value*)
    xmax := MINREAL;
    for i := 0 to NrPnts - 1 do
      if (poly[i].x > xmax) then
      begin
        xpntr := i;
        xmax  := poly[i].x;
      end;
  end;
  xmax    := poly[xpntr].x + 1000.0;
  Count   := 0;
  lastpnt := 0;
  pnt1    := poly[NrPnts - 1];
  pnt2    := poly[0];
  l1.p1   := pnt;
  l1.p2   := pnt;
  l1.p2.x := xmax;
  (*build scan line*)
  LineEquation(0, l1.p1, l1.p2, ile, pleglb);
  (*Build Scan Line Infinite*)
  for i := 1 to NrPnts do
  begin
    j     := i - 1;
    l2.p1 := poly[j];
    l2.p2 := poly[j];
    if (Intersectf(l2, l1)) then
    begin
      (*Does not Intersect with a vertex*)
      if lastpnt = 0 then
        l2.p2 := pnt1
      else
        l2.p2 := poly[lastpnt - 1];
      if (Intersectf(l2, l1)) then
        Inc(Count);
      lastpnt := i;
    end;
  end; (*for*)
       {return(((count % 2) = 1));}
  if Count div 2 = 1 then
    InPolygon := True
  else
    InPolygon := False;
end; (*InPolygon*)


 //-----------------------------------------------------------------------//
 // This routine suffles the array points in the opposite direction       //
 //-----------------------------------------------------------------------//
procedure RearrangePnts(poly: XYPolygon; NrPnts: integer; root: polypntr);
var
  i: integer;
  CurrPntr, LastPntr: polypntr;
begin
  CurrPntr := root;
  repeat
    LastPntr := CurrPntr;
    CurrPntr := CurrPntr^.Next;
  until (CurrPntr <> nil);
  CurrPntr := LastPntr;
  for i := 1 to NrPnts - 1 do
  begin
    poly[i]  := CurrPntr^.pnt;
    CurrPntr := CurrPntr^.prev;
  end;
end; (*RearrangePnts*)

(*----------------------------------------------------------------------------
 OrderPolygon orders the points in a polygon so that the first point is
   the point with the smallest X coordinate amoung all the points with the
   smallest Y coordinate and the points are arranged in the direction
   indicated by the value of gmode (Clockwise, CounterClockWise, No Change)
------------------------------------------------------------------------------*)
function OrderPolygon(gmode: integer; var poly: XYPolygon; NrPnts: integer): integer;
var
  i, j, firstpnt: integer;      (*Pointer to first point in polygon*)
  Xmin, Ymin: double;     (*X and Y Minimum coordinate.*)
  PolyRec:  polypntr;
  seqnum:   integer;        (*Sequence number.*)
  root,                     (*Pointer to first element in Heap*)
  LastPntr, CurrPntr: polypntr;     (*Pointer to current element in Heap*)
  polylineseg, scanline: LineSeg2D;
  mp, intp: TPoint2D;      (*MidPoint, Intersect Pnt*)
  intloc:   integer;
  leftx, rightx: double;
  leftpntr, rightpntr: integer;
  cw, ccw:  boolean;          (*Clockwise/CounterClockWise flags*)
begin (*OrderPolygon*)
  if ((gmode > 2) or (gmode < 0)) then
  //  setgeostatus(1)
  else
  begin
    root     := nil;
    Xmin     := MAXREAL;
    Ymin     := MAXREAL;
    seqnum   := 0;
    firstpnt := 0;
    for i := 0 to NrPnts - 1 do   (*Find start point*)
      if (poly[i].x <= Xmin) then
        if (poly[i].x = Xmin) then
        begin
          if (poly[i].y <= Ymin) then
          begin (*NewFirstPoint*)
            firstpnt := i;
            Ymin     := poly[i].y;
            Xmin     := poly[i].x;
          end;
        end
        else
        begin  (*NewFirstPoint*)
          firstpnt := i;
          Ymin     := poly[i].y;
          Xmin     := poly[i].x;
        end;
    if (firstpnt > 0) then
    begin  (*Arrange polygon to start with new first point*)
      i := firstpnt;
      repeat
        PolyRec^.seqnr := seqnum + 1;
        PolyRec^.pnt   := poly[i];
        BuildPolygon(root, PolyRec);
        if i = (NrPnts - 1) then
          i := 0
        else
          i := i + 1;
      until (i <> firstpnt);
      CurrPntr := root;
      for i := 0 to NrPnts - 1 do
      begin
        poly[i]  := CurrPntr^.pnt;
        CurrPntr := CurrPntr^.Next;
      end;
    end;
    if ((gmode = 1) or (gmode = 2)) then
    begin  (*Find clock direction of polygon*)
      PolyRectangle(poly, NrPnts, scanline);
      midpoint(scanline.p1, scanline.p2, mp);
      (*Create a horizontal scan line through polygon*)
      leftx  := MAXREAL;
      rightx := MINREAL;
      for i := 0 to NrPnts - 1 do
      begin
        (*Get closest polygon point to extreme Intersections of polygon and scan line*)
        if i = NrPnts - 1 then
          j := 0
        else
          j := i + 1;
        polylineseg.p1.x := poly[i].x;
        polylineseg.p1.y := poly[i].y;
        polylineseg.p2.x := poly[j].x;
        polylineseg.p2.y := poly[j].y;
        Intersectls(scanline, polylineseg, intp, intloc);
        if (intloc = 0) then
        begin
          if (intp.x < leftx) then
          begin
            leftx    := intp.x;
            leftpntr := i;
          end;
          if (intp.x > rightx) then
          begin
            rightpntr := i;
            rightx    := intp.x;
          end;
        end;
      end; (*for i*)
      ccw := False;
      cw  := False;
      i   := 0;
      while ((ccw) and (cw)) do
      begin
        if (i = leftpntr) then
          cw := True;
        if (i = rightpntr) then
          ccw := True;
        Inc(i);
      end;
    end; (*if gmode := 1 or 2*)
    if (gmode > 0) then
    begin
      if (root = nil) then
      begin  (*PutPolygonOnHeap*)
        i := firstpnt;
        repeat
          PolyRec^.seqnr := seqnum + 1;
          PolyRec^.pnt   := poly[i];
          BuildPolygon((root), PolyRec);
          if i = NrPnts - 1 then
            i := 0
          else
            i := i + 1;
        until (i <> firstpnt);
      end; (*if root*)
      CurrPntr := root;
      case (gmode) of
        1:
          if ccw then
            RearrangePnts(poly, NrPnts, CurrPntr);
        (*Put polygon in clockwise order*)
        2:
          if cw then
            RearrangePnts(poly, NrPnts, CurrPntr);
        (*Put polygon in conterclockwise order*)
      end;
    end;(* if gmode > 0*)
    if (root <> nil) then;
    //       disposepolygon( (root));
  end; (*if geostatus*)
  OrderPolygon := geostatus;
end; (*OrderPolygon*)

procedure PolygonPnts(cp: TPoint2D; rad: double; PolySides: integer;
  RegPoly: XYPolygon; sidelen: double);
var
  CosA, SinA, RadAngle, tRadAngle: double;
  i: integer;
begin //PolygonPnts
  if (PolySides < 3) then
  // setgeostatus(32)
  else if (rad = 0) then
  // setgeostatus(33)
  else
  begin
    RadAngle := DegToRad(360.0 / PolySides);
    for i := 0 to PolySides - 1 do
    begin
      tRadAngle := RadAngle * i;
      CosA      := Cos(tRadAngle);
      SinA      := Sin(tRadAngle);
      RegPoly[i].x := cp.x + (rad * CosA);
      RegPoly[i].y := cp.y + (rad * SinA);
      if (RegPoly[i].x = 0) then
        RegPoly[i].x := 0.0;
      if (RegPoly[i].y = 0) then
        RegPoly[i].y := 0.0;
    end;
    SideLen := Distance2D(RegPoly[0], RegPoly[1]);
  end; (*else*)
end; (*PolygonPnts*)

procedure PolygonType(Poly: XYPolygon; NrPnts: integer; var PolyType: integer);
var
  nrm1,                     (*Number of vertices less one.*)
  h, i, j: integer;         (*Pointers*)
  v1, v2, v3: TVector;      (*Vectors*)
  cp: double;
  PlusCount, MinusCount, zerocount: integer;
begin (*PolygonType*)
  if (NrPnts < 3) then
  begin
    PolyType := -1;
    exit;
  end;

  PlusCount := 0;
  MinusCount := 0;
  zerocount := 0;
  v1.x := 1.0;
  v1.y := 1.0;
  v1.z := 1.0;
  v2.x := 1.0;
  v2.y := 1.0;
  v2.z := 1.0;
  v3.x := 1.0;
  v3.y := 1.0;
  v3.z := 1.0;
  nrm1 := NrPnts - 1;
  for i := 0 to NrPnts - 1 do
  begin
    j := i + 1;
    h := i - 1;
    if (i = 0) then
    begin
      h := NrPnts - 1;
      j := 1;
    end;
    if (i = (nrm1 - 1)) then
    begin
      h := i - 1;
      j := NrPnts - 1;
    end;
    if (i = (NrPnts - 1)) then
    begin
      h := i - 1;
      j := 0;
    end;
    v1.x := poly[i].x - poly[h].x;
    v1.y := poly[i].y - poly[h].y;
    v1.z := 0.0;
    v2.x := poly[j].x - poly[i].x;
    v2.y := poly[j].y - poly[i].y;
    v2.z := 0.0;
    v3.x := v1.y * v2.z - v1.z * v2.y;
    v3.y := v1.z * v2.x - v1.x * v2.z;
    v3.z := v1.x * v2.y - v1.y * v2.x;
    cp   := v3.x + v3.y + v3.z;
    if (cp = 0) then
      Inc(zerocount);
    if (cp > 0) then
      Inc(PlusCount);
    if (cp < 0) then
      Inc(MinusCount);
  end; (*for i*)
  if ((PlusCount > 0) and (MinusCount > 0)) then
    PolyType := 1;
  if ((PlusCount > 0) and (MinusCount = 0)) then
    PolyType := 2;
  if ((PlusCount = 0) and (MinusCount > 0)) then
    PolyType := 2;
  if ((PlusCount = 0) and (MinusCount = 0)) then
    PolyType := 0;
end; (*PolygonType*)

procedure PolyRectangle(poly: XYPolygon; NrPnts: integer; var pr: LineSeg2D);
var
  i: integer;
begin
  pr.p1.x := MAXREAL;
  pr.p1.y := MAXREAL;
  pr.p2.x := MINREAL;
  pr.p2.y := MINREAL;
  i := 0;
  while (i < NrPnts) do
  begin
    pr.p1.x := min(poly[i].x, pr.p1.x);
    pr.p1.y := min(poly[i].y, pr.p1.y);
    pr.p2.x := max(poly[i].x, pr.p2.x);
    pr.p2.y := max(poly[i].y, pr.p2.y);
    Inc(i);
  end;
end; (*PolyRectangle*)


function hull_poly(pp: ppolyarray; PnPnts: integer; ch: ppolyarray;
  var cnpnts: integer): integer;

  {sub}
  function o_tar(v1, v2, v3: TPoint2D): double;
  var
    I: longint;
  begin
    o_tar := ((v1.x) * ((v3.y) - (v2.y)) + (v2.x) * ((v1.y) - (v3.y)) +
      (v3.x) * ((v2.y) - (v1.y)));
  end;

  {sub}
  function Next(vc, maxv: integer): integer;
  begin
    if vc = maxv - 1 then
      Next := 0
    else
      Next := vc + 1;
  end;

  {sub}
  function prev(vc, maxv: integer): integer;
  begin
    if vc = 0 then
      prev := (maxv - 1)
    else
      prev := vc - 1;
  end;

  {sub}
  function ad(x, y: integer): integer;
  begin
    if x < y - 1 then
      ad := x + 1
    else
      ad := 0;
  end;

  {sub}
  function sb(x, m_v: integer): integer;
  begin
    if x > 0 then
      sb := x - 1
    else
      sb := m_v - 1;
  end;

var
  MinXp, MaxXp: double;           (*  Minimal   maximal abscissa of of the Q  *)
  minpi: integer;                 (*  Index of the vertex with minimal        *)
  (*  abscissa   of the P                     *)
  maxpi: integer;                 (*  Index of the vertex with minimal        *)
  (*  abscissa   of the Q                     *)
  pma:   longint;
  chup:  polyarray;                (*  Stack for the previous verteses of the  *)
  (*  upper chain of the CH                   *)
  usp,                           (*  Stack pinter for te upper stack         *)
  i, part, pind: longint;
  arr:   double;
begin {hull_poly}
  MinXp := MAXREAL;
  MaxXp := 0;
  (*---------------- Find   maxx   minx for  polygon -------------------*)
  for i := 0 to PnPnts - 1 do
  begin
    if (MaxXp < pp^[i].x) then
    begin
      MaxXp := pp^[i].x;
      maxpi := i;
    end;
    if (MinXp > pp^[i].x) then
    begin
      MinXp := pp^[i].x;
      minpi := i;
    end;
  end;

  (*--------------- Build the upper chain of the CH -------------------*)
  pind     := minpi;
  usp      := 1;
  ch^[0].x := pp^[minpi].x;
  ch^[0].y := pp^[minpi].y + (pp^[minpi].y + 1) / 10000;
  ch^[1].x := pp^[minpi].x;
  ch^[1].y := pp^[minpi].y;
  chup[1].x := pp^[prev(minpi, PnPnts)].x;
  chup[1].y := pp^[prev(minpi, PnPnts)].y;
  pma      := pind;


  (*----- Building the polygon   convex hull by two loops: ---------*)
  (*----- first loop is building the upper part of the CH  ---------*)
  (*----- and the second loop is building it's lower part  ---------*)

  for part := 0 to 1 do
  begin
    (*-- Loop while don't reach the maximum X point of the polygon -*)
    while ((pp^[pma].x <> MaxXp)) do
    begin
      pma  := Next(pind, PnPnts);
      pind := ad(pind, PnPnts);

      (*------ If the next point of the polygon is to right ---------*)
      (*----------- from line segment: Q(i-1)Q(i) ---------------------*)
      arr := o_tar(ch^[usp - 1], ch^[usp], pp^[pma]);
      if (arr < 0) then
      begin
        (*------ If the next point of the polygon is to right ---------*)
        (*--- from line segment: previous_point_of_polygon  (Q(i))Q(i) --*)
        arr := o_tar(chup[usp], ch^[usp], pp^[pma]);
        if (arr <= 0) then
        begin
          (*------ If the next point of the polygon is to right ---------*)
          (*- from line segment: Q(i)point_of_the_polygon_with_maximum_X *)
          {arr=o_tar(( pp[maxpi]),( ch[usp]),pma);}
          arr := o_tar((pp^[maxpi]), ch^[usp], pp^[pma]);
          if (arr <= 0) then
          begin
            (*------ Update this point to the convex hull -------------------*)
            usp := ad(usp, PnPnts);
            ch^[usp].x := pp^[pma].x;
            ch^[usp].y := pp^[pma].y;
            chup[usp] := pp^[prev(pind, PnPnts)];
          end

          (*--- Else while next point is to the left, from line segment: ---*)
          (*-----  Q(i)point_of_the_polygon_with_maximum_X, remove it. ---*)
          else
          begin
            while o_tar(pp^[maxpi], ch^[usp], pp^[pma]) > 0 do
            begin
              pma  := Next(pind, PnPnts);
              pind := ad(pind, PnPnts);
            end;
          end;
        end

        (*--- Else while next point is to the left, from line segment: ---*)
        (*---      previous_point_of_polygon(Q(i))Q(i) remove it.    ---*)

        else
        begin
          while o_tar(chup[usp], ch^[usp], pp^[pma]) > 0 do
          begin
            pma  := Next(pind, PnPnts);
            pind := ad(pind, PnPnts);
          end;
        end;
      end

      (*--- Else while current point of the convex hull is to the left -*)
      (*---                          Q(i-1)Q(i) remove it.             -*)
      else
      begin
        while o_tar(ch^[usp - 1], ch^[usp], pp^[pma]) >= 0 do
        begin
          usp := sb(usp, PnPnts);
          if usp = 0 then
            break;
        end;
        usp := ad(usp, PnPnts);
        ch^[usp].x := pp^[pma].x;
        ch^[usp].y := pp^[pma].y;
        chup[usp] := pp^[prev(pind, PnPnts)];
      end;
    end;
    usp   := ad(usp, PnPnts);
    ch^[usp].x := pp^[pma].x;
    ch^[usp].y := pp^[pma].y;
    chup[usp] := pp^[prev(pind, PnPnts)];
    MaxXp := MinXp;
  end;
  if usp = 1 then
    cnpnts := PnPnts - 1
  else
    cnpnts := usp - 1;
end;


(* -----------------------------------------------------------------
                               DIAM
--------------------------------------------------------------------*)
function DiamPolygon(npnts: integer;
  (* Number of the vertices in polygon          *)
  pl: XYPolygon;         (* Polygon                                    *)
  ndiags: integer;       (* Nomber of the antipode vertices in polygon *)
  diag_lst: XYPolygon;   (* List of the antipode vertices              *)
  diam: array of TPoint2D;   (* Pointer to diameter in 'diag_lst'          *)
  diam_lenghth: double   (* Squere lenghth of the diameter             *)
  ): double;

{
#define o_tar(v1,v2,v3) ( (v1->x)*((v3->y)-(v2->y))+\
                          (v2->x)*((v1->y)-(v3->y))+\
                          (v3->x)*((v2->y)-(v1->y)))
#define next(v,vc,maxv) (((vc)>=(maxv)-1)?(v)-((maxv)-1):(v)+1)
#define ad(x,y) (x)=((x)<(y)-1)?(x)+1:0
}

  {sub}
  function o_tar(v1, v2, v3: TPoint2D): double;
  var
    i: longint;
  begin
    o_tar := ((v1.x) * ((v3.y) - (v2.y)) + (v2.x) * ((v1.y) - (v3.y)) +
      (v3.x) * ((v2.y) - (v1.y)));
  end;

  {sub}
  function Next(v, vc, maxv: double): double;
  begin
    if vc >= maxv - 1 then
      Next := v - (maxv - 1)
    else
      Next := v + 1;
  end;

  {sub}
  function ad(x, y: double): double;
  begin
    if x < y - 1 then
      x := x + 1
    else
      x := 0;
  end;

var
  p, p0, px, q, q0, qx: TPoint2D;
  i, np, pc, qc: integer;
  d, dl: double;
  type_: integer;
begin
  qc := 0;
  (*--------------- Checking if the polygon is convex --------------------*)
  PolygonType(pl, npnts, type_);
  //setgeostatus(80); (* Polygon is not convex *)
  if (type_ <> 2) then
  begin
    DiamPolygon := -1;
    exit;
  end;
  (*---------- Ordering the polygon in a counter-clockwise ---------------*)
  OrderPolygon(1, pl, npnts);
  (*------------------ Searching for all antipodal vertices ---------------*)
  np     := npnts;
  pc     := np - 1;
    {p:=pl+pc;
    q:=pl;}
  p.x    := pl[np].x + pc;
  q.x    := pl[np].x;
  ndiags := 0;
  px.x   := Next(p.x, pc, np);
  qx.x   := Next(q.x, qc, np);
  while (o_tar(p, px, qx) > o_tar(p, px, q)) do
  begin
    q.x := Next(q.x, qc, np);
    ad(qc, np);
    qx.x := Next(q.x, qc, np);
  end;
  q0 := q;
  p0 := p;
  {dl=diag_lst; C }
  dl := diag_lst[0].x;
  while (q0.x <> p.x) do
  begin
    p.x := Next(p.x, pc, np);
    ad(pc, np);
    Inc(ndiags);
       {
       *(dl++):=p->x;
       *(dl++):=p->y;
       *(dl++):=q->x;
       *(dl++):=q->y;
       }

    px.x := Next(p.x, pc, np);
    qx.x := Next(q.x, qc, np);
    while (o_tar(p, px, qx) > o_tar(p, px, q)) do
    begin
      q.x := Next(q.x, qc, np);
      ad(qc, np);
      px.x := Next(p.x, pc, np);
      qx.x := Next(q.x, qc, np);
      if ((p.x <> p0.x) and (q.x <> q0.x)) then
      begin
        Inc(ndiags);
             {
             *(dl++):=p^.x;
             *(dl++):=p^.y;
             *(dl++):=q^.x;
             *(dl++):=q^.y;
             }
      end;
    end;
    if (o_tar(p, px, q) = o_tar(p, px, qx)) then
    begin
      if ((p.x <> q0.x) and (p.y <> q0.y) and (q.x <> p0.x + np - 1)) then
      begin
        Inc(ndiags);
              {
              *(dl++):=p^.x;
              *(dl++):=p^.y;
              *(dl++):=q^.x;
              *(dl++):=q^.y;
              }
      end;
    end;
  end;

  (*--------------------- Choosing the maximum lenghth ------------------*)
  (*-------------------- antipodal vertex i.e. diameter -----------------*)

  dl := diag_lst[0].x;
  diam_lenghth := 0;
  for np := 0 to ndiags - 1 do
  begin
    d := ((dl - (dl + 2)) * (dl - (dl + 2)) + ((dl + 1) - (dl + 3)) *
      ((dl + 1) - (dl + 3)));
    if (diam_lenghth < d) then
      {if (diam_lenghth < (d:=(((dl)-(dl+2))*((dl)-(dl+2))+((dl+1)-(dl+3))*((dl+1)-(dl+3))))) then}
    begin
      diam_lenghth := d;
      diam[0].x := dl;
      diam[1].x := dl + 2;
      dl := dl + 4;
    end;
  end;
  diampolygon := diam_lenghth;
end; (* diampolygon *)



  (*-------------------------------------------------------------------*)
function IntPolygones(intrs: integer; (* Return value: 1 - if polygones Intersect; *)
  (*               0 - if not.                 *)
  PnPnts: integer;(* Number of verteces in polygon  P          *)
  QnPnts: integer;(* Number of verteces in polygon  Q          *)
  pp,            (* Polygon  PP                               *)
  pq: XYPolygon   (* Polygon  PQ                               *)
  ): integer;
var
  MinXp: double;
  MaxXp: double;   (*  Minimal   maximal abscissa of the P  *)
  MinXq: double;
  maxxq: double;   (*  Minimal   maximal abscissa of the Q  *)

  minpi: integer;     (*  Index of the vertex with minimal abscissa of the P *)
  minqi: integer;     (*  Index of the vertex with minimal abscissa of the Q *)
  maxpi: integer;     (*  Index of the vertex with maximal abscissa of the P *)
  maxqi: integer;     (*  Index of the vertex with maximal abscissa of the Q *)
  gmax, gmin: double; (*  Global minimum maximum  *)
  pnt, pma, qma, pu, pl, qu, ql: TPoint2D;
  pint: integer;
  ile:  implineeq;
  ple:  paralineeq;
  ls, ls1, ls2: LineSeg2D;
  IntExists: boolean;
type
  seael = record
    vrtx: TPoint2D;      // Current vertex
    i,                   // Previous vertex in the upper part of P
    j,                   // Previous vertex in the lower part of P
    k,                   // Previous vertex in the upper part of Q
    l:    integer;       // Previous vertex in the lower part of Q
  end;
var
  stp:     array [1..12 * MAXPOLYPNTS + 1] of seael;
  seartab: array [1..12 * MAXPOLYPNTS + 1] of seael;
  sidep, sideq, i, j, k, l, m, maxv, inci: integer;

  //new
  ii: longint;

  //sub
  function Next(v, vc, maxv: double): double;
  begin
    if vc >= maxv - 1 then
      Next := v - (maxv - 1)
    else
      Next := v + 1;
  end;

  //sub
  function prev(v, vc, maxv: double): double;
  begin
    if vc <= 0 then
      prev := v + (maxv - 1)
    else
      prev := v - 1;
  end;

  //sub
  function ad(x, y: integer): integer;
  begin
    if x < y - 1 then
      x := x + 1
    else
      x := 0;
  end;

  //sub
  function sb(x, m_v: integer): integer;
  begin
    if x > 0 then
      x := x - 1
    else
      x := m_v - 1;
  end;

  //sub
  function evalt(e: longint; x: double; a, b, c, d: longint): double;
  begin
    seartab[e].vrtx.x := x;
    seartab[e].i      := a;
    seartab[e].j      := b;
    seartab[e].k      := c;
    seartab[e].l      := d;
  end;

begin
  MinXp := MAXREAL;
  MaxXp := 0; // Minimal maximal abscissa of the P
  MinXq := MAXREAL;
  maxxq := 0; // Minimal maximal abscissa of the Q

  (*--------------- Checking if polygones are convex  ------------------*)
  PolygonType(pp, PnPnts, m);
  //setgeostatus(80);
  if (m <> 2) then
  begin
    IntPolygones := -1;
    exit;
  end;

  PolygonType(pq, QnPnts, m);
  //setgeostatus(80);
  if (m <> 2) then
  begin
    IntPolygones := -1;
    exit;
  end;

  (*---------------- Find  maxx   minx for : ------------------------*)
  (*---------------- Polygon P --------------------------------------*)
  pma := pp[0];
  for i := 0 to PnPnts - 1 do
  begin
    if (MaxXp < pma.x) then
    begin
      MaxXp := pma.x;
      maxpi := i;
    end;
    if (MinXp > pma.x) then
    begin
      MinXp := pma.x;
      minpi := i;
    end;
    pma := pp[i];
  end;
  qma := pq[0];
  (*---------------- Polygon Q --------------------------------------*)
  for i := 0 to QnPnts - 1 do
  begin
    if (maxxq < qma.x) then
    begin
      maxxq := qma.x;
      maxqi := i;
    end;
    if (MinXq > qma.x) then
    begin
      MinXq := qma.x;
      minqi := i;
    end;
    {qma++;}
    //qma := pq[i];
  end;
  if (MinXp > maxxq) or (MinXq > MaxXp) then
  begin
    intrs := 0;
    IntPolygones := 0;
  end;

  (*---------------- Global minimum   maximum -------------------------*)
  if MaxXp < maxxq then
    gmax := MaxXp
  else
    gmax := maxxq;
  if MinXp > MinXq then
    gmin := MinXp
  else
    gmin := MinXq;

  (*---------------- Order vertices of two polygones -------------------*)
  m  := 0;
  i  := minpi;
  j  := minpi;
  k  := minqi;
  l  := minqi;
  pu := pp[i];
  pl := pp[j];
  qu := pq[k];
  ql := pq[l];
  for ii := 0 to PnPnts + QnPnts do
    stp[ii] := seartab[ii];
  repeat
    //---- Compare vertices on the upper  lower side of the polygon P -----\\
    if (pu.x <= pl.x) then
    begin
      pma   := pu;
      sidep := 1;
    end
    else
    begin
      pma   := pl;
      sidep := 0;
    end;
    (*---- Compare verteces on the upper   lower side of the polygon Q -----*)
    if (qu.x <= ql.x) then
    begin
      qma   := qu;
      sideq := 1;
    end
    else
    begin
      qma   := ql;
      sideq := 0;
    end;
    (*---- Compare vertices of the polygones --------------------------------*)
    if (pma.x < qma.x) then
    begin
      if (sidep = 1) then
      begin
        evalt(m, pu.x, i, j, k, l);
        {evalt(m,pu,i,j,k,l); C}
        if (pu.x = pl.x) then
          pl.x := Next(pl.x, j, PnPnts);
        pu.x := prev(pu.x, i, PnPnts);
        sb(i, PnPnts);
      end
      else
      begin
        evalt(m, pma.x, i, j, k, l);
        pl.x := Next(pl.x, j, PnPnts);
        ad(j, PnPnts);
      end;
      if (pma.x = gmax) then
        break;
    end
    else
    begin
      if (sideq = 1) then
      begin
        evalt(m, qu.x, i, j, k, l);
        if (qu.x = ql.x) then
          ql.x := Next(ql.x, k, PnPnts);
        qu.x := prev(qu.x, k, PnPnts);
        sb(k, PnPnts);
      end
      else
      begin
        evalt(m, ql.x, i, j, k, l);
        ql.x := Next(ql.x, l, PnPnts);
        ad(l, PnPnts);
      end;
      if (qma.x = gmax) then
        break;
    end;
    Inc(m);
  until (m > PnPnts + QnPnts);
  //----------- Last value of the 'seartabl' must be MAXREAL ---------\\
  //-----------             to stop search                   ---------\\
  seartab[m + 1].vrtx.x := MAXREAL;

  //--------------- Searching for Intersection -----------------------\\

  //---------------------- Searching for minx in table ---------------\\

  m := 0;
  while (seartab[m].vrtx.x < gmin) do
    Inc(m);
  for ii := 0 to m do
    stp[ii] := seartab[ii];
  (*--------------------- Checking Intersection ----------------------*)
  ii := 0;
  repeat
    pnt.x := stp[ii].vrtx.x;
    if (stp[ii].vrtx.y = 0.0) then
      pnt.y := 1.0
    else
      pnt.y := 0.0;
    LineEquation(0, pnt, stp[ii].vrtx, ile, ple);
    (*------ Intesection of the current vertical line with ------------*)
    (*------     current upper side of the polygon P       ------------*)
    pint    := stp[ii].i;
    ls.p1.x := pp[pint].x;
    ls.p1.y := pp[pint].y;
    pint    := sb(pint, PnPnts);
    ls.p2.x := pp[pint].x;
    ls.p2.y := pp[pint].y;
    Intersect(4, ile, ile, ple, ple, ls, ls, ls1.p1, IntExists);
    if IntExists then
    begin
      ls1.p1.x := ls.p1.x;
      ls1.p1.y := ls.p1.y;
    end;
    (*------ Intesection of the current vertical line with ------------*)
    (*------     current lower side of the polygon P       ------------*)
    pint    := stp[ii].j;
    ls.p1.x := pp[pint].x;
    ls.p1.y := pp[pint].y;
    pint    := ad(pint, PnPnts);
    ls.p2.x := pp[pint].x;
    ls.p2.y := pp[pint].y;
    Intersect(4, ile, ile, ple, ple, ls, ls, ls1.p2, IntExists);
    if IntExists then
    begin
      ls1.p2.x := ls.p1.x;
      ls1.p2.y := ls.p1.y;
    end;

    (*------ Intesection of the current vertical line with ------------*)
    (*------     current upper side of the polygon Q      ------------*)
    pint    := stp[ii].k;
    ls.p1.x := pq[pint].x;
    ls.p1.y := pq[pint].y;
    pint    := sb(pint, PnPnts);
    ls.p2.x := pq[pint].x;
    ls.p2.y := pq[pint].y;
    Intersect(4, ile, ile, ple, ple, ls, ls, ls2.p1, IntExists);
    if IntExists then
    begin
      ls2.p1.x := ls.p1.x;
      ls2.p1.y := ls.p1.y;
    end;

    (*------ Intesection of the current vertical line with ------------*)
    (*------     current lower side of the polygon Q       ------------*)
    pint    := stp[ii].l;
    ls.p1.x := pq[pint].x;
    ls.p1.y := pq[pint].y;
    pint    := ad(pint, PnPnts);
    ls.p2.x := pq[pint].x;
    ls.p2.y := pq[pint].y;
    Intersect(4, ile, ile, ple, ple, ls, ls, ls2.p2, IntExists);
    if IntExists then
    begin
      ls2.p2.x := ls.p1.x;
      ls2.p2.y := ls.p1.y;
    end;

    (*----- Checking the Intersection of the vertical segments --------*)
    if (ls2.p1.y <= ls.p2.y) then
    begin
      if ((ls1.p1.y <= ls2.p2.y) and (ls1.p1.y >= ls2.p1.y)) or
        ((ls1.p2.y <= ls2.p2.y) and (ls2.p1.y >= ls2.p1.y)) then
      begin
        intrs := 1;
        IntPolygones := 1;
        exit;
      end;
    end
    else if ((ls1.p1.y <= ls2.p1.y) and (ls1.p1.y >= ls2.p2.y)) or
      ((ls1.p2.y <= ls2.p1.y) and (ls2.p1.y >= ls2.p2.y)) then
    begin
      intrs := 1;
      IntPolygones := 1;
      exit;
    end;
    {stp++;}
    Inc(ii);
  until (stp[ii].vrtx.x > gmax);
  intrs := 0;
  IntPolygones := 0;
end; (* IntPolygones *)

(*------------------------------------------------------------------------*)
function PointoPoly(v0: TPoint2D;  (*   Point   V                            *)
  y: TPoint2D;   (*   Intersection point                   *)
  w1,           (*   Verteces, betwen which point Y is    *)
  w2,           (*   situated.                            *)
  PnPnts: integer;   (*   Nomber of verteces of the polygon    *)
  dd: double; p: XYPolygon   (*   Polygon P                            *)
  ): double;
var
  dist, d: double;
  i, mini, vc: integer;
  intp: TPoint2D;
  IntExists: boolean;
  v, v1: TPoint2D;
  l1, l2: implineeq;
  pl:  paralineeq;
  ls1: LineSeg2D;


  //sub
  function Next(v, vc, maxv: double): double;
  begin
    if vc >= maxv - 1 then
      Next := v - (maxv - 1)
    else
      Next := v + 1;
  end;

  //sub
  function Prev(v, vc, maxv: double): double;
  begin
    if vc <= 0 then
      prev := v + (maxv - 1)
    else
      prev := v - 1;
  end;

  //sub
  function Ad(x, y: integer): integer;
  begin
    if x < y - 1 then
      x := x + 1
    else
      x := 0;
  end;

  //sub
  function sb(x, m_v: integer): integer;
  begin
    if x > 0 then
      x := x - 1
    else
      x := m_v - 1;
  end;

begin
  mini := 0;
  vc   := 0;
  (*--------------- Checking if polygone is convex  -------------------*)
  PolygonType(p, PnPnts, i);
  if (i <> 2) then
  begin
    pointopoly := 0.0;
    exit;
  end;

  (*-------------- Searching for minimal distance vertex --------------*)
  i    := 0;
  v    := p[0];
  dist := distance2D(v, v0);
  for i := 0 to PnPnts - 1 do
  begin
    v := p[i];
    d := distance2D(v0, v);
    if (dist > d) then
      {if (dist >(d:= distance(v0,*v++))) then}
    begin
      mini := i;
      dist := d;
    end;
  end;
  //------------- Computing the distance to polygon ------------------//
  v  := p[mini];
  w1 := mini;
  w2 := mini;
  ad(w2, PnPnts);
  v1.x     := Next(v.x, mini, PnPnts);
  ls1.p1.x := v.x;
  ls1.p1.y := v.y;
  ls1.p2.x := v1.x;
  ls1.p2.y := v1.y;
  LineEquation(0, v, v1, l1, pl);
  LinePntPerPend(l1, v0, l2);
  Intersect(4, l2, l2, pl, pl, ls1, ls1, intp, IntExists);
  if (IntExists) then
    ad(w2, PnPnts)
  else
  begin
    sb(w2, PnPnts);
    v1.x     := prev(v.x, mini, PnPnts);
    ls1.p2.x := v1.x;
    ls1.p2.y := v1.y;
    LineEquation(0, v, v1, l1, pl);
    LinePntPerPend(l1, v0, l2);
    Intersect(4, l2, l2, pl, pl, ls1, ls1, intp, IntExists);
    if (IntExists) then
    begin
      intp.x := v.x;
      intp.y := v.y;
    end;
  end;
  dd  := distance2D(v0, intp);
  y.x := intp.x;
  y.y := intp.y;
  pointopoly := dd;
end; (* pointopoly *)


//-------------------------------------------------------------//
function PltoGol(PnPnts,  (*   Number of vertices of the polygon  P *)
  QnPnts: integer; (*   Number of vertices of the polygon  Q *)
  pp,             (*   Polygon P                            *)
  pq: XYPolygon;   (*   Polygon Q                            *)
  sp,             (*   Start point of the max(D(P,Q),D(Q,P) *)
  ep: TPoint2D;    (*   end; point of the max(D(P,Q),D(Q,P)   *)
  hdist: double    (*   Hausdorf distance from P to Q        *)
  ): double;


  {sub}
  function Next(v, vc, maxv: double): double;
  begin
    if vc >= maxv - 1 then
      Next := v - (maxv - 1)
    else
      Next := v + 1;
  end;

  {sub}
  function prev(v, vc, maxv: double): double;
  begin
    if vc <= 0 then
      prev := v + (maxv - 1)
    else
      prev := v - 1;
  end;

  {sub}
  function ad(x, y: integer): integer;
  begin
    if x < y - 1 then
      x := x + 1
    else
      x := 0;
  end;

  {sub}
  function sb(x, m_v: integer): integer;
  begin
    if x > 0 then
      x := x - 1
    else
      x := m_v - 1;
  end;

var
  maxd, d: double;   (*  Current distanse *)
  pos: double;
  search: boolean;
  k, pvert: integer;
  y, v, v1, intp: TPoint2D;
  w1, w2: integer;  (* Nombers of the vertecies of the Q, between which *)
  (*  the y situtes                                   *)
  l1, l2: implineeq;
  pl:  paralineeq;
  ls1: LineSeg2D;
  IntExists, oldabs: boolean;

  {new}
  nextxy, prevxy: TPoint2D;
begin
  (*--------------- Setting some global constants   ---------------*)
  oldabs     := absdistglb;
  absdistglb := False;

  (*--------------- Computing distance from p0 to Q ---------------*)
  maxd := pointopoly(pp[0], y, w1, w2, QnPnts, d, pq);
  if (geostatus = 80) then
  begin
    PltoGol := 0;
    exit;
  end;
  pos := distpntline(2, pp[1], pp[0], y, l1, pl);
  if (pos < 0) then
  begin
    v  := pq[w1];
    v1 := pq[w2];
  end
  else
  begin
    v1 := pq[w1];
    v  := pq[w2];
  end;
  (*--------------- Computing distance from P to Q ----------------*)
  for k := 1 to PnPnts - 1 do
  begin
    (*--------------- Search for the next y, accordingly ------------*)
    (*---------------            to the pos              ------------*)
    repeat
      search := False;
      (*-------------- If  p(k+1) is on the segment (p(k),y) ----------*)
      if (pos = 0) then
      begin
        d      := maxd;
        search := True;
        continue;
      end;
      if (pos > 0) then
      begin
        v1  := v;
        v.x := prev(v.x, w1, QnPnts);
        sb(w1, QnPnts);
      end
      else
      begin
        v1  := v;
        v.x := Next(v.x, w1, QnPnts);
        ad(w1, QnPnts);
      end;
      (*-------------- Computing perpendicular from y ----------------*)
      (*------------------------ to q(i) q(i+1) ----------------------*)
      ls1.p1.x := v.x;
      ls1.p1.y := v.y;
      ls1.p2.x := v1.x;
      ls1.p2.y := v1.y;
      LineEquation(0, v, v1, l1, pl);
      LinePntPerPend(l1, pp[k], l2);
      Intersect(4, l2, l2, pl, pl, ls1, ls1, intp, IntExists);
      if (IntExists) then
      begin
        y.x    := intp.x;
        y.y    := intp.y;
        pvert  := k;
        search := True;
      end
      else
      begin
        (*-------------- If perpendicular from q(i) to q(i) q(i+1) -----*)
        (*--------------          supporting line to  Q           ------*)
        LineEquation(0, v, pp[k], l1, pl);
        LinePntPerPend(l1, v, l2);
        nextxy.x := Next(v.x, w1, QnPnts);
        prevxy.x := prev(v.x, w1, QnPnts);
        if distpntline(0, nextxy, v, v, l2, pl) *
          distpntline(0, prevxy, v, v, l2, pl) > 0 then
        begin
          y.x    := v.x;
          y.y    := v.y;
          search := True;
        end;
      end;
    until not search;
    if (pos <> 0) then
    begin
      d := distance2D(y, pp[k]);
      if maxd > d then
        maxd := maxd
      else
        maxd := d;
    end;
    (*--------------- Compute relative position of the --------------*)
    (*----------- next vertex of the P   segment (p(k),y)------------*)

    nextxy.x := Next(pp[k].x, k, PnPnts);
    nextxy.y := Next(pp[k].y, k, PnPnts);
    pos      := distpntline(2, nextxy, pp[k], y, l1, pl);
  end;
  hdist   := maxd;
  sp.x    := pp[pvert].x;
  sp.y    := pp[pvert].y;
  ep.x    := y.x;
  ep.y    := y.y;
  absdistglb := oldabs;
  PltoGol := hdist;
end; (* pltoqol *)


(*-------------------------------------------------------------------------*)
function PolToPol(PnPnts,        (*   Number of vertices of the polygon  P *)
  QnPnts: integer;(*   Number of vertices of the polygon  Q *)
  pp,            (*   Polygon P                            *)
  pq: XYPolygon;  (*   Polygon Q                            *)
  sp,            (*   Start point of the max(D(P,Q),D(Q,P) *)
  ep: TPoint2D;   (*   point of the max(D(P,Q),D(Q,P)       *)
  hdist: double   (*   Hausdorf distance from P to Q        *)
  ): double;
var
  dist1:    double;
  sp1, ep1: TPoint2D;
begin
  PltoGol(PnPnts, QnPnts, pp, pq, sp1, ep1, dist1);
  PltoGol(QnPnts, PnPnts, pq, pp, sp, ep, hdist);
  if (hdist < dist1) then
  begin
    hdist := dist1;
    sp.x  := sp1.x;
    sp.y  := sp1.y;
    ep.x  := ep1.x;
    ep.y  := ep1.y;
  end;
  PolToPol := hdist;
end; (* PolToPol *)


end.
