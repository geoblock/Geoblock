//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
{! Kernel unit implements base types and routines for descrete geometry library }

unit uDiscoCore;

interface

uses
  System.Math;


(********************************************************************
 *                  D I S C O   C O N S T A N T S                   *
 ********************************************************************)
const
  MINREAL     = 1.7E-308; //Minimum Real (Float) Number
  MAXREAL     = 1.7E+308; //Maximum Real (Float) Number
  ARADIAN     = 57.2957795131;
  MAXPOLYPNTS = 64000;
  MAXSPLINEPNTS = 200;
  MAXFACEPNTS = 200;  //Maximum face points on the object
  MAXVERTICES = 200;  //Maximum vertices that define the object
  MAXDWGPNTS  = 200;  //Maximum drawing points

  IDMATRIX2D: array[1..3, 1..3] of double = ((1, 0, 0), (0, 1, 0), (0, 0, 1));
  IDMATRIX3D: array[1..4, 1..4] of double = ((1, 0, 0, 0), (0, 1, 0, 0),
    (0, 0, 1, 0), (0, 0, 0, 1));

  VERT = 65535.0; // Point of intersection of "vertical"lines with Oy axis
  LMAX = 50;      // Permissible maximal number of initial lines

 //******************************************************************
 //*                  D I S C O  T Y P E S                          *
 //******************************************************************
type
  TPoint2D = record
    X, Y: double;
  end;

type
  TProbe2D = record
    X, Y, C: double;
  end;

type
  TPoint3D = record
    X, Y, Z: double;
  end;

type
  TProbe3D = record
    X, Y, Z, C: double;
  end;

type
  TLine2D = record
    case byte of
      0: (StartPoint, EndPoint: TPoint2D);
      1: (XStart, YStart, XEnd, YEnd: double);
  end;

  TLine3D = record
    case byte of
      0: (StartPoint, EndPoint: TPoint3D);
      1: (XStart, YStart, ZStart, XEnd, YEnd, ZEnd: double);
  end;

  TPolyline2D = array of TLine2D;
  TPolyline3D = array of TLine3D;

type
  PolyPntr = ^PolyXYCoord;

  PolyXYCoord = record
    seqnr: integer;
    pnt:   TPoint2D;
    Prev:  PolyPntr;
    Next:  PolyPntr;
  end;

type
  TVector = record
    X: double;
    Y: double;
    Z: double;
    D: double;
    M: double;
  end;

type
  TCircleEq = record
    Cp:  TPoint2D;
    Rad: double;
    D:   double;
    E:   double;
    F:   double;
  end;

type
  LineSeg2D = record
    p1: TPoint2D;
    p2: TPoint2D;
  end;

type
  LineSeg3D = record
    p1: TPoint3D;
    p2: TPoint3D;
  end;

type
  ImpLineEq = record
    a: double;
    b: double;
    c: double;
  end; //Implict Line Equation

type
  ParalineEq = record
    xo: double;
    yo: double;
    dx: double;
    dy: double;
  end;

type
  ParalnEqSpace = record
    xo: double;
    yo: double;
    zo: double;
    dx: double;
    dy: double;
    dz: double;
  end;

type
  SphericalEq = record
    Rho:   double;
    Theta: double;
    Phi:   double;
  end;

type
  TriangleEq = record
    v1: TPoint2D;
    v2: TPoint2D;
    v3: TPoint2D;
  end;

type
  ArcType = record
    cp:      TPoint2D;
    rad:     double;
    angmeas: integer;
    sa:      double;
    ea:      double;
  end;

type
  NormalPlaneEq = record
    a: double;
    b: double;
    c: double;
    d: double;
  end;

type
  GeomsgRec = record
    msgnr:  integer;
    msgtxt: string[72];
  end;

type
  ClipSegRecord = record
    pnt:    TPoint2D;
    segidx: integer;
  end;

type
  SplinePntr = ^splinerecord;

  SplineRecord = record
    idx:    integer;
    dx:     double;
    dy:     double;
    dist:   double;
    distx:  double;
    disty:  double;
    adist:  double;
    cosx:   double;
    siny:   double;
    xcoeff: double;
    ycoeff: double;
    nxt:    splinepntr;
  end;

type
  DrawingRecord = record
    x:   double;
    y:   double;
    pen: integer;
  end;

type
  SolidObjVertex = record
    pntnr: integer;
    pnt:   TPoint3D;
  end;

type
  SolidObjPnt = record
    pntnr:  integer;
    faceid: integer;
  end;

type
  PPolyRec = ^TPolyRec;

  TPolyRec = record
    v1, v2, v3: TPoint2D;
    nxt: PPolyRec;
  end;

type
  Inter = record
    x1, x2: double;
  end;


type
  PInt = ^TInt;
  TInt = record
    k: Inter;
    n: PInt;
  end;

{
type
  PRec = ^Rec;
  Rec = record
    k: TRect;
    n: PRec;
  end;
}
type
  XY4polygon = array[0..3] of TPoint2D;

type
  IA = array[0..MAXPOLYPNTS] of integer;

type
  PSource = ^TSource;

  TSource = record
    x, y: double;
    n:    PSource;
    {struct Source huge *n;}
  end;

  PObl = ^TObl;

  TObl = record
    x, y: double;
    n, p: PObl;
    //struct Obl huge *n,huge *p;
  end;

type
  ClipSeg   = array[1..30] of clipsegrecord;
  SolidObjFaces = array[1..500] of solidobjpnt;
  SolidObjVertices = array[1..200] of solidobjvertex;
  Drawing   = array[1..500] of DrawingRecord;
  SplineIdx = array[1..400] of Splinepntr;

type
  PolyArray  = array[0..200] of TPoint2D;
  PPolyArray = ^polyArray;

type
  xyPolygon  = array[0..MAXPOLYPNTS] of TPoint2D;
  xyzPolygon = array[0..MAXPOLYPNTS] of TPoint3D;
  gwrkstring = array[0..81] of char;
  ViewMatrix = array[1..4, 1..4] of double;
  Matrix33   = array[1..3, 1..3] of double;
  Matrix44   = array[1..4, 1..4] of double;

type
  AngleLineEq = record
    k:  double; // Line equation in angle coefficient form
    b:  double; // k and b are used for y=kx+b form, and
    ro: double; // ro and fi - for x*cos(fi)+y*sin(fi)=ro
    fi: double;
  end;

type
  AngleMeasType = record
    aRadians:  double;
    aDegrees:  double;
    aParmtanh: double;
  end;

type
  PRSDS = ^RSDS;

  RSDS = record  // RSDS - double connection arc list
    VB:  TPoint2D; // Arc beginning
    VE:  TPoint2D; // Arc end
    FL:  integer; // Left  area number
    FR:  integer; // Right area number
    P1:  integer; // Pointer of arc beginning
    P2:  integer; // POinter of arc end
    rsr: array[1..12] of integer; // Reserved                 *)
    // Usually: rsr[0] border index (if -1)
    //          rsr[1] beginning node number
    //          rsr[2] end node number
    //          rsr[3] delete mark (if 1)
    //          rsr[4] beg node power
    //          rsr[5] end node power
  end;

type
  PointSet = ^pst;

  Pst = record      // Type - set of the points
    p:    TPoint2D; // Point coordinates
    prev: pointset; // Pointer to the previous point
    Next: pointset; // Pointer to the next point
  end;

type
  TREE = ^spt;

  spt = record      // Type - tree
    sv:   TPoint2D; // Start vertex coordinates of the edge
    ev:   TPoint2D; // End vertex coordinates of the edge
    Next: TREE;     // Pointer to the next edge
  end;

type
  OutList = record   // List of arcs rounding out_surface of D_T
    outarc: integer; // Number of arc
    nbarc:  integer; // Case of its neighbourhood
    prev:   integer; // Number of the previous arc in the list
    Next:   integer; // Number of the next arc in the list
    rad:    double;  // Radius of the circle rounding this arc and the next one
  end;

type
  PArea = ^TArea;

  TArea = record
    X, Y: double;
    P:    PArea;
  end;

  (*********************************************************************
   *               G L O B A L   V A R I A B L E S                     *
   *********************************************************************)
var
  AngleGlb:  double;
  L1glb, L2glb, L3glb, L4glb: implineeq;
  P1glb, P2glb, P3glb, P4glb: TPoint2D;
  AbsDistGlb: boolean;
  Cglb:      TCircleEq;
  GeoProcGlb: gwrkstring;
  GeoRoundOff, GeoRoundOff2: double;
  GeoStatus: integer;
  DistGlb:   double;
  DirectGlb: integer;
  NormalGlb: boolean;
  XspacingGlb, YspacingGlb: double;
  Gmode:     integer;
  Gsulx:     integer;
  Gsuly:     integer;
  Gslrx:     integer;
  Gslry:     integer;
  PleGlb, pleGlb1, pleGlb2: ParalineEq;
  Ileglb, ileGlb1, ileGlb2: ImplineEq;
  GmsgRec:   geomsgrec;
  LsGlb, lsglb1, lsglb2: LineSeg2D;
  LssGlb, lssGlb1, lssglb2: LineSeg3D;
  PlesGlb, plesGlb1: paralneqspace;
  M2dGlb:    matrix33;
  M3dGlb:    matrix44;

 //=========================== KERNEL ================================\\
//! Returns the arc tangent in degrees
function ArcTang(x: double; y: double): double;
procedure Add_To_List(List: integer; var Nelem: integer; var New: integer);
procedure Exchange(var a: double; var b: double);
procedure IntExchange(var a: integer; var b: integer);
// Inverses the parameter x
function Inverse(x: double): double;
procedure SetNormalOn;
procedure SetNormalOff;
function SumSqrdParms(dx: double; dy: double): double;
function Point_In_Interval(x: double; e: double): boolean;

 { =============== 2D ==================}
 {! This routine calculates the distance between two points on a line}
function Distance2D(p1: TPoint2D; p2: TPoint2D): double;
function EqualPoints2D(p1: TPoint2D; p2: TPoint2D): boolean;
procedure NormalizePle;
procedure NormalizeLine(gmode: integer; Ile: Implineeq; ple: paralineeq);

procedure Normalize_Plane(npe: normalplaneeq);
function Point_On_Border(pnt: TPoint2D; x1, y1, x2, y2: integer): boolean;


function InBetween(Ls: LineSeg2D; P1: TPoint2D): boolean;
function InCircle(c1: TCircleEq; p1: TPoint2D): boolean;
procedure IntCircles(gmode: integer; c1, c2: TCircleEq; intp1, intp2: TPoint2D;
  nrint: integer);
procedure LineEquation(gmode: integer; p1, p2: TPoint2D; var ile: implineeq;
  var ple: paralineeq);
procedure IntCircleLine(gmode: integer; c1: TCircleEq; l1: LineSeg2D;
  ple: paralineeq; intp1, intp2: TPoint2D; nrint: integer);

procedure IntArcLine(gmode: integer; arc1: arctype; ls: LineSeg2D;
  ile: implineeq; ple: paralineeq; intp1, intp2: TPoint2D; nrint: integer);
function Abscissa(gmode: integer; ile: implineeq; ple: paralineeq;
  ytvalue: double): double;
function AngleParmTan(gmode: integer; tha: double): double;
function AngleTwoLines(gmode: integer; ile1: implineeq; ile2: implineeq): double;

function ArcAngle(gmode: integer; cp, ep: TPoint2D): double;
procedure ArcPnts(arc1: arctype; sp, ep: TPoint2D);
procedure ArcRectangle(arc1: arctype; sp, ep: TPoint2D; arcrect: LineSeg2D);

procedure CircleCoeff(c1: TCircleEq);
procedure CircleEquation(gmode: integer; p1, p2, p3: TPoint2D; c1: TCircleEq);

procedure ConvLineEq(gmode: integer; ile: implineeq; ple: paralineeq);
procedure Cpradcircle(c1: TCircleEq);
function DistPntArc(arc1: arctype; p1: TPoint2D): double;
function DistPntLine(gmode: integer; p1, p2, p3: TPoint2D; var ile: implineeq;
  var ple: paralineeq): double;
function InRectangle(rect: LineSeg2D; p1: TPoint2D): boolean;
procedure Intbisecttriangle(p1, p2, p3, cp: TPoint2D);
procedure Linebisector(p1, p2: TPoint2D; l1: implineeq);
function LineLoc(l1, l2: LineSeg2D): integer;
procedure LinePntParallel(l1: implineeq; p1: TPoint2D; l2: implineeq);
procedure LinePntPerpend(l1: implineeq; p1: TPoint2D; var l2: implineeq);

procedure LineRectangle(l1: LineSeg2D; var linerect: LineSeg2D);
procedure LineTwoCircles(gmode: integer; c1, c2: TCircleEq; ple: paralineeq;
  ile: implineeq);
procedure MidPoint(p1, p2: TPoint2D; var mp: TPoint2D);
procedure NormalVector2d(v, nv: TVector);
function Octant(gmode: integer; angle: double; p1: TPoint2D): integer;
function Ordinate(gmode: integer; ile: implineeq; ple: paralineeq;
  xtvalue: double): double;

procedure ParallelLines(l3: implineeq; dist: double; var l1, l2: implineeq);

function ParmTanhangle(gmode: integer; angle: double): double;
function Perpendicular(l1, l2: LineSeg2D): boolean;
procedure PntAngleDist(gmode: integer; ao: TPoint2D; dist, angle: double; p1: TPoint2D);

function PointOnLine(ls: LineSeg2D; Point: TPoint2D): boolean;
function Quadrant(Gmode: integer; Angle: double; Point: TPoint2D): integer;

procedure Slope(L1: LineSeg2D; s: double);
function Point_Segm_Side(pnt, p1, p2: TPoint2D): integer;
function Point_In_Area(A, B: double; Num: longint; Obl: PAREA): integer;


{=============== 3D ==================}
function AngleLinePlane(gmode: integer; npe: normalplaneeq; ples: paralneqspace): double;

function AngleLinesSpace(gmode: integer; ples1, ples2: paralneqspace): double;

function AngleTwoPlanes(gmode: integer; npe1, npe2: normalplaneeq): double;

function Between(a, b, c: double): boolean;
procedure ConvSpherical(gmode: integer; scoord: sphericaleq; p1: TPoint3D);

procedure CrossProductV1V2(v1, v2, cp: Tvector);
function Distance3D(pz1, pz2: TPoint3D): double;
function DistLineSpace(ples1, ples2: paralneqspace): double;
function DistPntLineSpace(gmode: integer; pnt: TPoint3D; lss: LineSeg3D;
  ples: paralneqspace): double;
function DistPntPlane(npe: normalplaneeq; pz1: TPoint3D): double;
function EqualPoints3D(p1, p2: TPoint3D): boolean;
function DotProductV1V2(v1, v2: TVector): double;
procedure IntLinePlane(gmode: integer; lss: LineSeg3D; ples: paralneqspace;
  npe: normalplaneeq; intp: TPoint3D);
procedure IntLineSpace(gmode: integer; ples1, ples2: paralneqspace;
  lss1, lss2: LineSeg3D; var intp: TPoint3D; var intexists: boolean);
procedure IntThreePlanes(npe1, npe2, npe3: normalplaneeq; intp: TPoint3D);

procedure IntTwoPlanes(npe1, npe2: normalplaneeq; ples: paralneqspace);
procedure LineEqSpace(p1, p2: TPoint3D; ples: paralneqspace);
procedure MPLineSpace(lss: LineSeg3D; var mp: TPoint3D);
procedure NormalLineEqSpace(ples: paralneqspace);
function ParallelPlanes(npe1, npe2: normalplaneeq): boolean;
procedure PlaneCoeff(pnt: TPoint3D; dx1, dy1, dz1, dx2, dy2, dz2: double;
  npe: NormalPlaneEq; norm, invnorm: double);
procedure CheckPointDiff(dx, dy, dz: double);
procedure PlaneEquation(gmode: integer; lss: LineSeg3D; ples: paralneqspace;
  p1, p2, p3: TPoint3D; var npe: normalplaneeq);
procedure ProjectedNormal(v1, v2: TVector; var nv: TVector);
procedure ProjectVector(v1, v2, pv: TVector);
procedure UnitVector(v, uv: TVector);
procedure VectorAdd(v1, v2, v3: TVector);
procedure VectorDirection(v: TVector);
procedure VectorLength(v: TVector);
procedure TriGravity(p1, p2, p3: TPoint3D; var TriG: TPoint3D);

{! Intersection of two arcs}
procedure IntArcs(arc1, arc2: arctype; intp1, intp2: TPoint2D; nrint: integer);

procedure Intersect(gmode: integer; ile1, ile2: implineeq; ple1, ple2: paralineeq;
  ls1, ls2: LineSeg2D; var intp: TPoint2D; var intexists: boolean);
function IntersectF(l1, l2: LineSeg2D): boolean;
procedure IntersectLs(ls1, ls2: LineSeg2D; var intp: TPoint2D; var intloc: integer);


//====================================================================
implementation
//====================================================================

{! This procedure sets all computational results for equations that are
   normalized.  This is recommended since it avoids potential numerical
   problems.
}
procedure SetNormalOn;
begin
  NormalGlb := True;
end;

{! This procedure sets the flag NormalGlb to false which indicates that
   all computations are not to be normalized}
procedure SetNormalOff;
begin
  normalglb := False;
end;

{! This function returns the sum of the squared parameters}
function SumSqrdParms(dx: double; dy: double): double;
begin
  Result := ((dx * dx) + (dy * dy));
end;

function ArcTang(x: double; y: double): double;
var
  angle: double;
begin
  if (x = 0) then
    if (y = 0) then
      Result := 0
    else if (y > 0) then
      Result := 90.0
    else
      Result := 270.0
  else if y = 0 then
    if x > 0 then
      Result := 0.0
    else
      Result := 180.0
  else
  begin
    angle := arctan(abs(y / x)) * ARADIAN;
    if x > 0 then
      if y > 0 then
        Result := angle
      else
        Result := 360.0 - angle
    else if y > 0 then
      Result := 180.0 - angle
    else
      Result := 180.0 + angle;
  end;
end;

function Inverse(x: double): double;
begin
  if x = 0 then
    Result := 0
  else
    Result := 1 / x;
end;

{! This procedure Exchanges one variable with the other.  After the
 function is run A = B and B = A}
procedure Exchange(var a: double; var b: double);
var
  t: double;
begin
  t := a;
  a := b;
  b := t;
end;

{! The function determines if point x belongs to interval [0,e]}
function Point_In_Interval(x: double; e: double): boolean;
begin
  if ((x >= 0) and (x <= e)) then
    Result := True
  else
    Result := False;
end;


{! This function Exchanges the values of two integer variables a and b }
procedure IntExchange(var a: integer; var b: integer);
var
  t: integer;
begin
  t := a;
  a := b;
  b := t;
end;

{! This function adds new integer value to the list of integer values
 if it was not present there yet. Number of elements in the list
 will be increased by one during the work of this function}
procedure Add_To_List(List: integer; var Nelem: integer; var New: integer);
var
  i: integer;
  present: boolean;
begin
  present := False;
  for i := 0 to nelem - 1 do
  begin
    if ((list + i) = new) then
    begin
      present := True;
      break;
    end;
  end;
  if present then
  begin
    New := (List + Nelem);
    Inc(Nelem);
  end;
end;


// ________________2D routines______________________\\
function Distance2D(p1: TPoint2D; p2: TPoint2D): double;
var
  dx, dy: double;
begin
  dx     := p1.x - p2.x;
  dy     := p1.y - p2.y;
  Result := sqrt((dx * dx) + (dy * dy));
end;

 //---------- Equal points ------------------\\
 {! This function determines if points P1 and P2 are the same point}
function EqualPoints2D(p1: TPoint2D; p2: TPoint2D): boolean;
begin
  if ((p1.x = p2.x) and (p1.y = p2.y)) then
    Result := True
  else
    Result := False;
end;

{! NormalizeLine normalizes the coefficients of a line equation.
 The line equation form can be either parametric(ple) or
 implicit(ile).  The form of the line equation to be normalized
 is determined by the variable gmode}
procedure NormalizeLine(gmode: integer; ile: ImPlineEq; ple: ParalineEq);

var
  NormParm: double;
begin
  case gmode of
    0: // Implicit Line Equation
    begin
      NormParm := SumSqrdParms(ile.a, ile.b);
      if (NormParm = 0) then
      else
      begin
        NormParm := 1 / sqrt(NormParm);
        if (ile.c > 0) then
          NormParm := -NormParm;
        if (ile.c = 0) then
          if (ile.b < 0) then
            NormParm := -NormParm;
        ile.a := ile.a * NormParm;
        ile.b := ile.b * NormParm;
        ile.c := ile.c * NormParm;
      end;
    end;
    1:  // Parametric Line Equation
    begin
      NormParm := SumSqrdParms(ple.dx, ple.dy);
      if (NormParm = 0) then
      else
      begin
        NormParm := 1 / sqrt(NormParm);
        ple.dx   := ple.dx * NormParm;
        ple.dy   := ple.dy * NormParm;
      end;
    end;
  end;
end; // NormalizeLine

{!This procedure normalizes the parametric line equation coefficients if
  necessary.  Normalizing the line equation makes the code more efficient}
procedure NormalizePle;
var
  tgmode: integer;
  sdxdy:  double;
begin
  sdxdy := SumSqrdParms(pleglb.dx, pleglb.dy);
  if (sdxdy <> 1.0) then
  begin
    //Parametric line needs to be normalized
    tgmode := 1;
    NormalizeLine(tgmode, ileglb, pleglb);
  end;
end;

{! This function returns the normalized coefficients of plane equation}
procedure Normalize_Plane(npe: NormalPlaneEq);
var
  norm:    double;    // Normalizing Factor
  invnorm: double;    // Inverse of Normalizing Factor
begin
  norm := (npe.a * npe.a) + (npe.b * npe.b) + (npe.c * npe.c);
  if (norm = 0) then
    gmode := 25
  else if (norm <> 1.0) then
  begin // Normalize coefficients
    invnorm := inverse(sqrt(norm));
    npe.a   := npe.a * invnorm;
    npe.b   := npe.b * invnorm;
    npe.c   := npe.c * invnorm;
    npe.d   := npe.d * invnorm;
  end;
end;

{! This function verifies if given point lies on the border of the
 rectangle which is described by two diagonal points (x1,y1) and
 (x2,y2). Returns TRUE or FALSE}
function Point_On_Border(pnt: TPoint2D; x1, y1, x2, y2: integer): boolean;
begin
  if (x1 = pnt.x) or (x2 = pnt.x) or (y1 = pnt.y) or (y2 = pnt.y) then
    Result := True
  else
    Result := False;
end;


{!  This function determines if a point is between the end points of
  line segment}
function InBetween(ls: LineSeg2D; p1: TPoint2D): boolean;
begin
  Result := False;
  if (p1.x <= ls.p1.x) and (p1.y <= ls.p1.y) and (p1.x >= ls.p2.x) and
    (p1.y >= ls.p2.y) then
    Result := True;
  if (p1.x >= ls.p2.x) and (p1.y <= ls.p2.y) and (p1.x <= ls.p1.x) and
    (p1.y >= ls.p1.y) then
    Result := True;
  if (p1.x >= ls.p1.x) and (p1.y >= ls.p1.y) and (p1.x <= ls.p2.x) and
    (p1.y <= ls.p2.y) then
    Result := True;
  if (p1.x <= ls.p2.x) and (p1.y >= ls.p2.y) and (p1.x >= ls.p1.x) and
    (p1.y <= ls.p1.y) then
    Result := True;
end;


{! InCircle determines if a point (P1) is inside a circle (C1)}
function InCircle(c1: TCircleEq; p1: TPoint2D): boolean;
var
  dx, dy, dist: double;
begin
  if (c1.rad = 0) then
    Result := False
  else
  begin
    dx := abs(c1.cp.x - p1.x);
    dy := abs(c1.cp.y - p1.y);
    if ((dx = 0) and (dy = 0)) then
      Result := True
    else
    begin
      dist := sqrt((dx * dx) + (dy * dy));
      if (dist <= c1.rad) then
        Result := True
      else
        Result := False;
    end;
  end;
end;


{!  Abscissa returns the value of X coordinate given the Y coordinate
  (YTValue) and the coefficients of an implicit line equation or given
  the value of T (YTValue) and the coefficients of a parameteric equation}
function Abscissa(gmode: integer; ile: ImpLineEq; ple: ParaLineEq;
  ytvalue: double): double;
begin
  case gmode of
    0: //Coefficients of an implicit line equation
    begin
      Result := -((ile.b * ytvalue) + ile.c) / ile.a;
    end;
    1: //Coefficients of a parametric line equation
    begin
      Result := ple.xo + (ple.dx * ytvalue);
    end
    else
      Result := 0;
  end;
end;

{!  AngleParmTan returns the angle in degrees or radians based on the
  parameterized tangent of the half angle (THA)}
function AngleParmTan(gmode: integer; tha: double): double;
var
  a,    // Angle
  tana, //Tangent of the angle
  intportion: double;
begin
  if tha <= 0 then
    if tha = 0 then
      Result := 0
    else
      gmode := 20
  else
  begin
    //tana := atan(modf(tha,&intportion)) * 2;}
    tana := arctan(tha) * 2; //Double the angle
    case gmode of
      0: //Return angle in degrees
      begin
        a := RadToDeg(tana);
        if tha < 1.0 then
          Result := a
        else if (tha >= 1.0) and (tha < 2.0) then
          Result := 90.0 + a
        else if (tha >= 2.0) and (tha < 3.0) then
          Result := 180 + a
        else if (tha >= 3.0) and (tha < 4.0) then
          Result := 270 + a
        else
          gmode := 19
      end;
      1: //Return angle in radians
      begin
        if tha < 1.0 then
          Result := tana
        else if (tha >= 1.0) and (tha < 2.0) then
          Result := 1.5707961 + tana
        else if (tha >= 2.0) and (tha < 3.0) then
          Result := 3.1415922 + tana
        else if (tha >= 3.0) and (tha < 4.0) then
          Result := 4.7123883 + tana
        else
          gmode := 19
      end;
      else
        gmode := 1
    end; //switch gmode of
  end;
end;

{!  This function returns the acute angle between two lines}
function AngleTwoLines(gmode: integer; ile1: ImPlineEq; ile2: ImPlineEq): double;

var
  tile1, tile2: ImPlineEq; (*Temporary Storage*)
  angle: double;
begin
  tile1 := ile1;
  tile2 := ile2;
  NormalizeLine(0, tile1, pleglb);
  NormalizeLine(0, tile2, pleglb);
  if (geostatus = 0) then
  begin
    angle := tile1.a * tile2.a + tile1.b * tile2.b;
    if angle <= 0 then
      angle := -angle;
    case gmode of
      0: //Angle in Degrees
      begin
        if angle = 0 then
          Result := 90.0
        else
          Result := arccos(angle);
      end;
      1: //Angle in Radians
      begin
        if angle = 0 then
          Result := PI / 2
        else
          Result := DegToRad(arccos(angle));
      end;
      2: //Cosine of angle
      begin
        if angle = 0 then
          Result := 0.0
        else
          Result := angle;
      end;
      else
        gmode := 1
    end; //switch gmode
  end; //if
end;

(*------------------------------------------------------------
 This function returns the tangent of the half angle
 -------------------------------------------------------------*)
function TanhalfAngle_Intcircles(tanparm: double): double;
begin
  (*The following equation is derived from the Secant of the angles*)
  if tanparm = 0 then
    Result := 0
  else
    Result := (sqrt(1.0 + tanparm * tanparm) - 1.0) / tanparm;
end;

(* ----------------------------------------------------------------------
  ArcAngle returns the angle formed by a horizontal line passing through
   the center-point (CP) and a line passing through CP and the end-point
   of the arc (EP).
------------------------------------------------------------------------*)
function ArcAngle(gmode: integer; cp, ep: TPoint2D): double;
var
  aa,                    // ArcAngle
  xdiff, ydiff, tanparm: double;        (* Tangent of the angle *)
  (* TanHalfAngle *)
  q: integer;             (* Quadrant *)
begin
  xdiff := ep.x - cp.x;
  ydiff := ep.y - cp.y;
  if ((xdiff = 0) and (ydiff = 0)) then
  begin
    aa := 0;
  end
  else
  begin
    q := Quadrant(0, angleglb, ep);
    case q of
      1: //First Quadrant
      begin
        if (xdiff > 0) then
        begin
          TanParm := Ydiff / Xdiff;
          aa      := tanhalfangle_intcircles(TanParm);
        end
        else
          aa := 1.0; (*90 degrees*)
      end;
      2: //Second Quadrant
      begin
        if (ydiff >= 0) then
        begin
          tanparm := -xdiff / ydiff;
          aa      := 1.0 + tanhalfangle_intcircles(tanparm);
        end
        else
          aa := 2.0;
      end;
      3: //Third Quadrant
      begin
        if xdiff < 0 then
        begin
          tanparm := ydiff / xdiff;
          aa      := 2.0 + tanhalfangle_intcircles(tanparm);
        end
        else
          aa := 3.0;
      end;
      4: //Fourth Quadrant
      begin
        if (ydiff < 0) then
        begin
          tanparm := -xdiff / ydiff;
          aa      := 3.0 + tanhalfangle_intcircles(tanparm);
        end
        else
          aa := 0;
      end;
    end;  // switch Q

    case gmode of
      0: //return parametized tangent of the half angle
        Result := aa;
      1: //Return angle in degrees
        Result := AngleParmTan(0, aa);
      2: //Return angle in radians
        Result := AngleParmTan(1, aa);
      else
        gmode := 1
    end; //switch gmode
  end; //else
end; //ArcAngle

(*--------------------------------------------------------------------------
  ArcPnts returns the XY coordinates of the start and end point of the arc
  defined by the center-point (CP), Radius (Rad) and start and end angles.
 ---------------------------------------------------------------------------*)
procedure ArcPnts(arc1: arctype; sp, ep: TPoint2D);
var
  tgmode: integer;
begin
   {
   case arc1.angmeas of
      aradians: tgmode := 1;
      adegrees: tgmode := 0;
      aparmtanh:tgmode := 2;
   end;
   }
  tgmode := 1; { aradians }
  PntAngleDist(tgmode, arc1.cp, arc1.rad, arc1.sa, sp);
end;

(*-------------------------------------------------------------------------
 If points in the same Quadrant and angle between is lt 90 degrees use
 this routine
 -------------------------------------------------------------------------*)
procedure Anglelt90(ArcRect: LineSeg2D; sp, ep: TPoint2D);
begin
  ArcRect.p1.x := min(sp.x, ep.x);
  ArcRect.p1.y := min(sp.y, ep.y);
  ArcRect.p2.x := max(sp.x, ep.x);
  ArcRect.p2.y := max(sp.y, ep.y);
end;


(*-------------------------------------------------------------------------
 This function determines the corners of the rectangles if the start and
 end points are in the same Quadrant
 -------------------------------------------------------------------------*)
procedure SameQuadrant(ArcRect: LineSeg2D; sp, ep: TPoint2D; rsq: integer;
  arc1: arctype);
begin
  if rsq < 3 then
    if (sp.x <= ep.x) then
    begin  (*almost a circle*)
      ArcRect.p1.x := arc1.cp.x - arc1.rad;
      ArcRect.p1.y := arc1.cp.y - arc1.rad;
      ArcRect.p2.x := arc1.cp.x + arc1.rad;
      ArcRect.p2.y := arc1.cp.y + arc1.rad;
    end
    else
      anglelt90(ArcRect, sp, ep)
  else if (ep.x <= sp.x) then
  begin  (*almost a circle*)
    ArcRect.p1.x := arc1.cp.x - arc1.rad;
    ArcRect.p1.y := arc1.cp.y - arc1.rad;
    ArcRect.p2.x := arc1.cp.x + arc1.rad;
    ArcRect.p2.y := arc1.cp.y + arc1.rad;
  end
  else
    anglelt90(ArcRect, sp, ep);
end;

(* ----------------------------------------------------------------------
  ArcRectangle returns the coordinates of a rectangle (ArcRect) that
  surround an arc (arc1).
 ------------------------------------------------------------------------*)
procedure ArcRectangle(arc1: arctype; sp, ep: TPoint2D; ArcRect: LineSeg2D);
var
  rep: TPoint2D;     (*Relative End Point to origin*)
  req: integer;      (*Relative End Quadrant*)
  rsp: TPoint2D;     (*Relative Start Point to origin*)
  rsq: integer;      (*Relative Start Quadrant*)
begin
  rsp.x := sp.x - arc1.cp.x;
  rsp.y := sp.y - arc1.cp.y;
  rep.x := ep.x - arc1.cp.x;
  rep.y := ep.y - arc1.cp.y;
  rsq   := Quadrant(0, angleglb, rsp);
  req   := Quadrant(0, angleglb, rep);
  case rsq of
    1: case req of
        1: SameQuadrant(ArcRect, sp, ep, rsq, arc1);
        2: (*EP in Quadrant 2*)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := min(sp.y, ep.y);
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := arc1.cp.y + arc1.rad;
        end;
        3: (*EP in Quadrant 3*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := min(sp.y, ep.y);
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := arc1.cp.y + arc1.rad;
        end;
        4: (*EP in Quadrant 4*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := arc1.cp.y + arc1.rad;
        end;
      end;
    2: case req of
        1: (*EP in Quadrant 1*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
        2: SameQuadrant(ArcRect, sp, ep, rsq, arc1);
        3: (*EP in Quadrant 3*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := min(sp.y, ep.y);
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
        4: (*EP in Quadrant 4*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
      end;
    3: case req of
        1: (*EP in Quadrant 1*)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
        2: (*EP in Quadrant 2*)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := arc1.cp.y + arc1.rad;
        end;
        3: SameQuadrant(ArcRect, sp, ep, rsq, arc1);
        4: (*EP in Quadrant 4*)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := max(sp.x, ep.x);
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
      end;
    4: case req of
        1: (*EP in Quadrant 1*)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := min(sp.y, ep.y);
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
        2: (* EP in Quadrant 2 *)
        begin
          ArcRect.p1.x := min(sp.x, ep.x);
          ArcRect.p1.y := min(sp.y, ep.y);
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := arc1.cp.y + arc1.rad;
        end;
        3: (*EP in Quadrant 3*)
        begin
          ArcRect.p1.x := arc1.cp.x - arc1.rad;
          ArcRect.p1.y := arc1.cp.y - arc1.rad;
          ArcRect.p2.x := arc1.cp.x + arc1.rad;
          ArcRect.p2.y := max(sp.y, ep.y);
        end;
        4: (*EP in Quadrant 4*)
          SameQuadrant(ArcRect, sp, ep, rsq, arc1);
      end;
  end; //Case RSQ of
end; //ArcRectangle

(*---------------------------------------------------------------------
 Definition: This procedure determine the coefficients of the circle
 equation given the radius and the center point
 --------------------------------------------------------------------*)
procedure CircleCoeff(c1: TCircleEq);
begin
  c1.d := (-2) * c1.cp.x;
  c1.e := (-2) * c1.cp.y;
  c1.f := (c1.rad * c1.rad) + (-1 * (c1.cp.x * c1.cp.x) + (-1 * (c1.cp.y * c1.cp.y)));
  c1.f := -1 * c1.f;
end;

(*------------------------------------------------------------------------
  This procedure determines the point of intersection of the bisectors
  of the interior angles of a triangle defined by its vertices.
-------------------------------------------------------------------------*)
procedure IntBisectTriangle(p1, p2, p3, cp: TPoint2D);
var
  l23, (*Distance between points P2 and P3*)
  l31, (*Distance between points P3 and P1*)
  l12, (*Distance between points P1 and P2*)
  pt: double;  (*Perimeter of the triangle*)
begin
  l23 := distance2D(p2, p3);
  l31 := distance2D(p3, p1);
  l12 := distance2D(p1, p2);
  pt  := l12 + l31 + l23;
  if ((l23 <> 0) and (l31 <> 0) and (l12 <> 0)) then
  begin
    pt   := inverse(pt);
    cp.x := ((l23 * p1.x) + (l31 * p2.x) + (l12 * p3.x)) * pt;
    cp.y := ((l23 * p1.y) + (l31 * p2.y) + (l12 * p3.y)) * pt;
  end
  else
    gmode := 9
end;

(*----------------------------------------------------------------------
  This procedure determines the equation of a circle (C1)
  given various parameters.  The input parmeters used to compute the
  equation of the circle are based on the value of the variable GMode.
------------------------------------------------------------------------*)
procedure CircleEquation(gmode: integer; p1, p2, p3: TPoint2D; c1: TCircleEq);
var
  Xdiff, Ydiff,     // X and Y Difference
  Xdiff12, Ydiff12,  // difference in x and y between pnts P1 and P2
  Xdiff13, Ydiff13,  // difference in x and y between pnts P1 and P3
  sdist12, sdist13, (*Distance squared between pnts P1 and P2/P3*)
  det:    double;        (*Determinate*)
  tgmode: integer;    (*Temporary GMode*)
  ss:     boolean;        (*Save Absolute Distance Status*)
begin //CircleEquation
  ss := absdistglb;
  case gmode of
    0: //Compute using line segment P1 to P2 as diameter
    begin
      xdiff := p1.x - p2.x;
      ydiff := p1.y - p2.y;
      if ((xdiff = 0) and (ydiff = 0)) then
        gmode := 0
      else
      begin
        c1.rad := sqrt((xdiff * xdiff) + (ydiff * ydiff)) * 0.5;
        MidPoint(p1, p2, c1.cp);
        circlecoeff(c1);
      end;
    end;
    1: //Compute using three points P1,P2,P3
    begin
      xdiff12 := p2.x - p1.x;
      ydiff12 := p2.y - p1.y;
      xdiff13 := p3.x - p1.x;
      ydiff13 := p3.y - p1.y;
      det     := ydiff13 * xdiff12 - xdiff13 * ydiff12;
      if (det = 0) then
        gmode := 5
      else
      begin
        det     := inverse(2 * det);
        sdist12 := SumSqrdParms(xdiff12, ydiff12);
        sdist13 := SumSqrdParms(xdiff13, ydiff13);
        c1.cp.x := det * (sdist12 * ydiff13 - sdist13 * ydiff12);
        c1.cp.y := det * (sdist13 * xdiff12 - sdist12 * xdiff13);
        c1.rad  := (c1.cp.x * c1.cp.x) + (c1.cp.y * c1.cp.y);
        c1.rad  := sqrt(c1.rad);
        c1.cp.x := c1.cp.x + p1.x;
        c1.cp.y := c1.cp.y + p1.y;
        circlecoeff(c1);
      end;
    end;
    2: //Compute using triangle vertices P1,P2,P3
    begin
      //Get intersection for bisectors of the interior angles of the triangle
      intbisecttriangle(p1, p2, p3, c1.cp);
      if (geostatus = 0) then
      begin
        (*Determine the radius of the circle by computing
          the perpendicular distance from the above
          intersection(CP) to the triangle side P1 to P2.*)
        setnormalon;
        tgmode := 2;
        c1.rad := distpntline(tgmode, c1.cp, p1, p2, ileglb, pleglb);
        circlecoeff(c1);
      end;
    end;
    3: //Compute using box corners P1,P2
    begin
      //First determine if coordinates are a box
      Xdiff := (p1.x - p2.x);
      Ydiff := (p1.y - p2.y);
      if ((Xdiff = 0) and (ydiff = 0)) then
        gmode := 0 //Two points are the same
      else if ((Xdiff - Ydiff) <> 0) then
        gmode := 5 //Two points not a square box
      else
      begin
        c1.cp.x := p1.x + abs(xdiff * 0.5);
        c1.cp.y := p1.y + abs(ydiff * 0.5);
        c1.rad  := abs(Xdiff * 0.5);
        CircleCoeff(c1);
      end;
    end;
    else
      gmode := 1
  end; //case
  absdistglb := ss;
end; //CircleEquation

(*-------------------------------------------------------------------------
 This procedure converts a parametric line equation to an
 implicit line equation and visa versa.
--------------------------------------------------------------------------*)
procedure ConvLineEq(gmode: integer; ile: ImPlineEq; ple: ParalineEq);
var
  InvsDist,        //Inverse of Squared Dist
  Tf: double;      //Temporary Factor
begin
  case gmode of
    0: //Convert Implicit to Parametric
    begin
      if (ile.a = 0) and (ile.b = 0) then
        gmode := 3
      else
      begin
        if (1 - ile.a + ile.b) = 0 then
          //Coeff already Normalized
          invsdist := 1
        else
          invsdist := 1 / SumSqrdParms(ile.a, ile.b);
        //Coeff not normalized
        tf := -ile.c * invsdist;
        ple.xo   := ile.a * tf;
        ple.yo   := ile.b * tf;
        invsdist := sqrt(invsdist);
        ple.dx   := ile.b * invsdist;
        ple.dy   := -ile.a * invsdist;
      end;
    end;
    1: //Convert Parametric to Implicit
    begin
      ile.a := -ple.dy;
      ile.b := ple.dx;
      ile.c := (ple.xo * ple.dy) - (ple.yo * ple.dx);
    end;
    else
    //setgeostatus(1);
  end;
end;

{!  This procedure calculates the Center Point and the Radius (Rad) of a
    circle given the coefficients of the equation of a circle (C1)}
procedure CpRadCircle(c1: TCircleEq);
var
  root: double;
begin
  c1.cp.x := -c1.d / 2;
  c1.cp.y := -c1.e / 2;
  root    := (c1.d * c1.d) + (c1.e * c1.e) - (4 * c1.f);
  if (root = 0) then
  begin
    //setgeostatus(13);
    c1.rad := 0.0;
    gmode  := 13;
  end
  else if (root < 0) then
    //setgeostatus(14)
    gmode  := 14
  else
    c1.rad := sqrt(root) / 2;
end;

(*--------------------------------------------------------------------------
 This function returns the distance from a point to the surface of the arc.
----------------------------------------------------------------------------*)
function DistToArc_IntCircles(arc1: arctype; p1: TPoint2D): double;
begin
  Result := Distance2D(arc1.cp, p1) - arc1.rad;
end;

(* ---------------------------------------------------------------------
 This function returns the distance form the closes end point of the
 arc to the point.
-----------------------------------------------------------------------*)
function DistToEndPnt(arc1: arctype; p1: TPoint2D): double;
var
  d1:     double;   (*Distance to Start Point*)
  d2:     double;   (*Distance to End Point*)
  sp, ep: TPoint2D; (*Coordinates of the end points of an arc*)
begin
  Result := 0;
  ArcPnts(arc1, sp, ep);
  begin
    d1 := distance2D(p1, sp);
    d2 := distance2D(p1, ep);
    if (d1 < d2) then
      Result := d1
    else
      Result := d2;
  end;
end;

(* -----------------------------------------------------------------
  DistPntArc returns the distance from a point to an arc.
 --------------------------------------------------------------------*)
function DistPntArc(arc1: arctype; p1: TPoint2D): double;
var
  tgmode: integer;
  aa:     double; (*ArcAngle*)
begin
   {
   case arc1.angmeas of
      aradians: tgmode := 2;
      adegrees: tgmode := 1;
      aparmtanh:tgmode := 0;
      else
        ShowMessage('gmode = 21')  //setgeostatus(21);
   end;
   }
  tgmode := 2; //aradians
  begin
    aa := ArcAngle(tgmode, arc1.cp, p1); (*Get the angle form CP to P1*)
    if (geostatus = 0) then
      if (arc1.ea < arc1.sa) then
        if ((aa >= arc1.ea) and (aa <= arc1.sa)) then
          Result := abs(disttoarc_intcircles(arc1, p1))
        else
          Result := abs(DistToEndPnt(arc1, p1))
      else if ((aa >= arc1.sa) and (aa <= arc1.ea)) then
        Result := abs(disttoarc_intcircles(arc1, p1))
      else
        Result := abs(DistToEndPnt(arc1, p1));
  end;
end;

{!  This function returns the perpendicular distance from a line to a point}
function DistPntLine(gmode: integer; p1, p2, p3: TPoint2D; var ile: ImPlineEq;
  var ple: ParalineEq): double;
var
  sdx, sdy, sdxy, sdistxy, pdx, pdy, tdist, xdiff, ydiff: double;
  tmode: integer;
  tile:  ImPlineEq;
  tple:  ParalineEq;
begin
  tdist := 1;
  case gmode of
    0: //Implicit line equation used
    begin
      tdist := (ile.a * p1.x) + (ile.b * p1.y) + ile.c;
    end;
    1: //Parametric line equation used
    begin
      sdx     := ple.dx * ple.dx;
      sdy     := ple.dy * ple.dy;
      sdistxy := sdx + sdy;
      if (sdistxy = 0) then
      //setgeostatus(4)
      else
      begin
        sdistxy := 1 / sdistxy;
        pdx     := p1.x - ple.xo;
        pdy     := p1.y - ple.yo;
        sdxy    := ple.dx * ple.dy;
        xdiff   := (sdy * pdx) - (sdxy * pdy);
        ydiff   := (sdx * pdy) - (sdxy * pdx);
        tdist   := SumSqrdParms(xdiff, ydiff) * (sdistxy * sdistxy);
        tdist   := sqrt(tdist);
      end;
    end;
    2: //Pnts P2 and P3 define line
    begin
      xdiff := p3.x - p2.x;
      ydiff := p3.y - p2.y;
      if (xdiff = 0) and (ydiff = 0) then
      //setgeostatus(2)
      else
      begin
        tmode := 0;
        LineEquation(tmode, p2, p3, tile, tple);
        if (geostatus = 0) then
          tdist := (tile.a * p1.x) + (tile.b * p1.y) + tile.c;
      end;
    end;
    else
    //setgeostatus(1);
  end;
  if (absdistglb) then
    Result := abs(tdist)
  else
    Result := tdist;
end;

{!  InRectangle determines if a point is inside or outside of a rectangle}
function InRectangle(rect: LineSeg2D; p1: TPoint2D): boolean;
var
  trect: LineSeg2D;  //Temp Rectangle
begin
  if EqualPoints2D(rect.p1, rect.p2) then
    // setgeostatus(22);
    LineRectangle(rect, trect);
  if ((p1.x <= trect.p2.x) and (p1.y <= trect.p2.y) and (p1.x >= trect.p1.x) and
    (p1.y >= trect.p1.y)) then
    Result := True
  else
    Result := False;
end;

{!  This linebisector determines the coefficients of an implicit
  line equation (L1) that is the perpendicular bisector of the
  line segment defined by the end points P1 and P2}
procedure LineBisector(p1, p2: TPoint2D; l1: ImPlineEq);
var
  dx, dy: double;
  mp:     TPoint2D;  // MidPoint
  le:     ImPlineEq; //Line Equation of line segment defined by P1 and P2
begin
  dx := p1.x - p2.x;
  dy := p1.y - p2.y;
  if ((dx = 0) and (dy = 0)) then
    Exit// setgeostatus(2)
  else
  begin
    l1.a := dx;
    l1.b := dy;
    l1.c := -0.5 * (SumSqrdParms(p1.x, p1.y) - SumSqrdParms(p2.x, p2.y));
    if (normalglb) then
      NormalizeLine(0, l1, pleglb);
  end;
end;

{!  This procedure returns the coefficients of an a line equation in implicit
  or parametric form that pass through two given points p1 and p2}
procedure LineEquation(gmode: integer; p1, p2: TPoint2D; var ile: ImPlineEq;
  var ple: ParalineEq);
var
  dx, dy,          //Change in X and Y
  dist,            //Distance between P1 and P2
  invdist: double; //Inverse of Dist
begin
  invdist := 1;
  dx      := p1.x - p2.x;
  dy      := p1.y - p2.y;
  if ((dx = 0) and (dy = 0)) then
    Exit //P1 and P2 are same point
  else
  begin
    if (normalglb) then
    begin  (*Get Normalize constant*)
      dist    := (dx * dx) + (dy * dy);
      invdist := 1.0 / sqrt(dist);
    end;
    case gmode of
      0: //Implicit Line Equation
      begin
        ile.a := -dy * invdist;
        ile.b := dx * invdist;
        ile.c := (p2.x * p1.y - p1.x * p2.y) * invdist;
      end;
      1: //Parametric Line Equation
      begin
        ple.xo := p1.x;
        ple.yo := p1.y;
        ple.dx := dx * invdist;
        ple.dy := dy * invdist;
      end;
      else
      // setgeostatus(1);
    end;
  end;
end;

(*-----------------------------------------------------------------
 LineLoc returns an integer value that indicates the location
 of end points of line segment L1 relative to line segment L2.
 ------------------------------------------------------------------*)
function LineLoc(l1, l2: LineSeg2D): integer;
var
  l2dx, l2dy, p1dx, p1dy, p2dx, p2dy, t: double;              (*Temp result*)
  parm1, parm2: boolean;  (*results of InBetween function*)
begin
  l2dx := l2.p2.x - l2.p1.x;
  l2dy := l2.p2.y - l2.p1.y;
  p1dx := l1.p1.x - l2.p1.x;
  p1dy := l1.p1.y - l2.p1.y;
  p2dx := l1.p2.x - l2.p2.x;
  p2dy := l1.p2.y - l2.p2.y;
  t    := (l2dx * p1dy - l2dy * p1dx) * (l2dx * p2dy - l2dy * p2dx);
  if (t > 0) then (*Same Side*)
    Result := 2
  else if (t < 0) then (*Opposite Sides*)
    Result := 1
  else
  begin
    parm1 := InBetween(l2, l1.p1);
    parm2 := InBetween(l2, l1.p2);
    if ((parm1 and parm2)) then (*L1 is on L2*)
      Result := 3
    else if (parm1) then (*L1.P1 touches L2*)
      Result := 4
    else if (parm2) then (*L1.P2 touches L2*)
      Result := 5
    else if ((InBetween(l1, l2.p1) and InBetween(l1, l2.p2))) then
      (*L2 is in L1*)
      Result := 6
    else if (perpendicular(l1, l2)) then (*L1 and L2 are perpendicular*)
      Result := 7
    else
      Result := 0; (*L1 and L2 Collinear*)
  end;
end; (*LineLoc*)

(*-------------------------------------------------------------------------
  This procedure determines the implicit line coefficients of a line (L2)
  the passes through a point (P1) and is parallel to another line (L1).
 --------------------------------------------------------------------------*)
procedure LinePntParallel(l1: ImPlineEq; p1: TPoint2D; l2: ImPlineEq);
begin
  l1.a := l2.a;
  l1.b := l2.b;
  l1.c := ((l2.a * p1.x) + (l2.b * p1.y)) * (-1);
end; (*LinePntParallel*)

(*-----------------------------------------------------------------------
  This procedure determines the coefficients (L2) of an implicit line
  equation that passes through a given point (P1) and is perpendicular
  to a given line (L1)
 ------------------------------------------------------------------------*)
procedure LinePntPerpend(l1: ImPlineEq; p1: TPoint2D; var l2: ImPlineEq);
begin
  l2.a := l1.b;
  l2.b := l1.a * (-1);
  l2.c := (l1.a * p1.y) - (l1.b * p1.x);
end; (*LinePntPerpend*)


(*------------------------------------------------------------------------
  LineRectangle returns the coordinates of a rectangle (LineRect)
  that surround a line (L1)
 --------------------------------------------------------------------------*)
procedure LineRectangle(l1: LineSeg2D; var linerect: LineSeg2D);
begin
  linerect.p1.x := min(l1.p1.x, l1.p2.x);
  linerect.p1.y := min(l1.p1.y, l1.p2.y);
  linerect.p2.x := max(l1.p1.x, l1.p2.x);
  linerect.p2.y := max(l1.p1.y, l1.p2.y);
  if EqualPoints2D(l1.p1, l1.p2) then
    gmode := 10 //It is not rectangle
end;

(*----------------------------------------------------------------------------
  LineTwoCircles returns the coefficients of the line equation that
  passes through the intersection point(s) of two circles.
 -----------------------------------------------------------------------------*)
procedure LineTwoCircles(gmode: integer; c1, c2: TCircleEq; ple: ParalineEq;
  ile: ImPlineEq);
var
  tgmode, (*Temporary GMode*)
  nrint: integer;
  ile1:  ImPlineEq;
  intp1, intp2: TPoint2D;
begin
  case gmode of
    0: ;
    3: intcircles(0, c1, c2, intp1, intp2, nrint);
    1: ;
    4: intcircles(1, c1, c2, intp1, intp2, nrint);
    2: ;
    5: intcircles(2, c1, c2, intp1, intp2, nrint);
    else
    // setgeostatus(1); (*Bad Mode*)
  end;
  if (geostatus = 0) then
  begin
    if (gmode < 3) then
      tgmode := 1
    else
      tgmode := 0;
    case nrint of
      1: (*One intersection found Circles Tangent*)
      begin
        LineEquation(0, c1.cp, c2.cp, ileglb, pleglb);
        LinePntPerpend(ileglb, intp1, ile1);
        ileglb := ile1;
        case gmode of
          0: ;
          1: ;
          2:
          begin
            NormalizeLine(0, ileglb, pleglb);
            ConvLineEq(0, ileglb, ple);
          end;
          3: ;
          4: ;
          5: ile := ileglb;
        end; (*switch gmode*)
      end;
      2: LineEquation(tgmode, intp1, intp2, ile, ple);
    end; (*switch nrint*)
  end; (*if geostatus*)
end;

(*--------------------------------------------------------------------
  MidPoint returns the MidPoint (MP) of the line segment defined by
  the two points (P1,P2).
 ---------------------------------------------------------------------*)
procedure MidPoint(p1, p2: TPoint2D; var mp: TPoint2D);
begin
  mp.X := (p1.X + p2.X) / 2;
  mp.Y := (p1.Y + p2.Y) / 2;
end; (*MidPoint*)

(*--------------------------------------------------------------------
  NormalVector returns the components of a vector (NV) that
  is perpendicular to the given vector (V) in 2D space.
 --------------------------------------------------------------------*)
procedure NormalVector2D(v, nv: TVector);
var
  p1: TPoint2D; (*Point*)
begin
  if (v.x = 0) and (v.y = 0) then
  //  setgeostatus(43)
  else if (v.x = 0) then
    if (v.y > 0) then
    begin  (*90*)
      nv.x := -1.0;
      nv.y := 0.0;
    end
    else
    begin  (*270*)
      nv.x := 1.0;
      nv.y := 0.0;
    end
  else if (v.y = 0) then
    if (v.x > 0) then
    begin  (*0*)
      nv.x := 0.0;
      nv.y := 1.0;
    end
    else
    begin  (*180*)
      nv.x := 0.0;
      nv.y := -1.0;
    end
  else
  begin
    p1.x := v.x;
    p1.y := v.y;
    case (Quadrant(0, angleglb, p1)) of
      (*get Quadrant of vector*)
      1:
      begin (*Quadrant 1 (-+)*)
        nv.x := -abs(v.y / v.x);
        nv.y := 1.0;
      end;

      2:
      begin  (*Quadrant 2 (--)*)
        nv.x := -abs(v.y / v.x);
        nv.y := -1.0;
      end;

      3:
      begin  (*Quadrant 3 (+-)*)
        nv.x := abs(v.y / v.x);
        nv.y := -1.0;
      end;

      4:
      begin  (*Quadrant 3 (++)*)
        nv.x := abs(v.y / v.x);
        nv.y := 1.0;
      end;
    end; (*switch*)
  end; (* else *)
end; (*NormalVector2d*)

{!  Octant returns the Octant that an angle or point is within}
function Octant(gmode: integer; angle: double; p1: TPoint2D): integer;
var
  cp:     TPoint2D;   (*Center Point*)
  aa,     (*Arc Angle*)
  deg,    (*Angle in degrees*)
  rad,    (*Angle in Radians*)
  rad45,  (*Radian at 45 degrees*)
  rad90,  (*Radian at 90 degrees*)
  rad135, (*Radian at 135 degrees*)
  rad180, (*Radian at 180 degrees*)
  rad225, (*Radian at 225 degrees*)
  rad270, (*Radian at 270 degrees*)
  rad315, (*Radian at 315 degrees*)
  rad360: double; (*Radian at 360 degrees*)
  q         (*Quadrant*): integer;
begin
  Result := 0;
  case gmode of
    0:
    begin
      //Use XY coordinates of a Point
      cp.x := 0;
      cp.y := 0;
      //Set center point to origin
      aa   := ArcAngle(0, cp, p1);
      if ((p1.x >= 0) and (p1.y >= 0)) then
        q := 1;
      if ((p1.x < 0) and (p1.y >= 0)) then
        q := 2;
      if ((p1.x < 0) and (p1.y < 0)) then
        q := 3;
      if ((p1.x >= 0) and (p1.y < 0)) then
        q := 4;
      case q of
        1:
          if (aa <= 0.414213562) then
            Result := 1
          else
            Result := 2;
        2:
          if (aa <= 1.414213562) then
            Result := 3
          else
            Result := 4;
        3:
          if (aa <= 2.414213562) then
            Result := 5
          else
            Result := 6;

        4:
          if (aa <= 3.414213562) then
            Result := 7;
        else
          Result := 8;
      end; (*switch Q of*)
    end;
    (*gmode := 0*)
    1:
    begin
      //Use Parameterized Tangent of Half Angle
      if (angle < 0) then
      begin
        Result := 0;
        //setgeostatus(19);
      end
      else
      begin
        if (angle <= 0.414213562) then
          Result := 1;
        if ((angle > 0.414213562) and (angle <= 1.0)) then
          Result := 2;
        if ((angle > 1.0) and (angle <= 1.414213562)) then
          Result := 3;
        if ((angle > 1.414213562) and (angle <= 2.0)) then
          Result := 4;
        if ((angle > 2.0) and (angle <= 2.414213562)) then
          Result := 5;
        if ((angle > 2.414213562) and (angle <= 3.0)) then
          Result := 6;
        if ((angle > 3.0) and (angle <= 3.414213562)) then
          Result := 7;
        if ((angle > 3.414213562) and (angle < 4.0)) then
          Result := 8;
        if (angle >= 4.0) then
        begin
          Result := 0;
          //setgeostatus(19);
        end;
      end;
    end;
    (*gmode := 1*)
    2:
    begin
      (*Use Angle in degrees*)
      if (angle < 0) then
      //setgeostatus(20)
      else
      begin
        deg := angle;
        while (deg >= 360.0) do
          deg := deg - 360;
        if ((deg >= 0) and (deg <= 45.0)) then
          Result := 1;
        if ((deg > 45.0) and (deg <= 90.0)) then
          Result := 2;
        if ((deg > 90.0) and (deg <= 135.0)) then
          Result := 3;
        if ((deg > 135.0) and (deg <= 180.0)) then
          Result := 4;
        if ((deg > 180.0) and (deg <= 225.0)) then
          Result := 5;
        if ((deg > 225.0) and (deg <= 270.0)) then
          Result := 6;
        if ((deg > 270.0) and (deg <= 315.0)) then
          Result := 7;
        if (deg > 315.0) then
          Result := 8;
      end; //else
    end;
       (*gmode := 2*)
    3: (*Use Angle in radians*)
    begin
      if (angle < 0) then
      // setgeostatus(20)
      else
      begin
        rad45  := DegToRad(45.0);
        rad90  := DegToRad(90.0);
        rad135 := DegToRad(135.0);
        rad180 := DegToRad(180.0);
        rad225 := DegToRad(225.0);
        rad270 := DegToRad(270.0);
        rad315 := DegToRad(315.0);
        rad360 := DegToRad(360.0);
        rad    := angle;
        while (rad >= rad360) do
          rad := rad - rad360;
        if ((rad >= 0) and (rad <= rad45)) then
          Result := 1;
        if ((rad > rad45) and (rad <= rad90)) then
          Result := 2;
        if ((rad > rad90) and (rad <= rad135)) then
          Result := 3;
        if ((rad > rad135) and (rad <= rad180)) then
          Result := 4;
        if ((rad > rad180) and (rad <= rad225)) then
          Result := 5;
        if ((rad > rad225) and (rad <= rad270)) then
          Result := 6;
        if ((rad > rad270) and (rad <= rad315)) then
          Result := 7;
        if (rad > rad315) then
          Result := 8;
      end;
    end;
    else
      gmode := 1
  end;
end;

(*--------------------------------------------------------------------------
  Ordinate returns the value of Y coordinate given the X coordinate
  (XTValue) and the coefficients of an implicit line equation or given
  the value of T (YTValue) and the coefficients of a parameteric equation
 ---------------------------------------------------------------------------*)
function Ordinate(gmode: integer; ile: ImPlineEq; ple: ParalineEq;
  xtvalue: double): double;

begin
  case gmode of
    0:  (*Coefficients of an implicit line equation*)
    begin
      Result := -((ile.a * xtvalue) + ile.c) / ile.b;
    end;

    1:  (*Coefficients of a parametric line equation*)
    begin
      Result := ple.yo + (ple.dy * xtvalue);
    end;
    else
    //setgeostatus(1);
  end;
end; (*Ordinate*)

(*--------------------------------------------------------------------------
  Parallellines returns the line equations of lines that are parallel to
  a given line at a given distance
 ---------------------------------------------------------------------------*)
procedure ParallelLines(l3: ImPlineEq; dist: double; var l1, l2: ImPlineEq);
var
  abdist: double;  (*Dist A to B*)
  adist:  double;  (*Absolute Distance*)
begin
  abdist := sqrt((l3.a * l3.a) + (l3.b * l3.b));
  adist  := abs(dist) * abdist;
  l1     := l3;
  l1.c   := l1.c + adist;
  l2     := l3;
  l2.c   := l2.c - adist;
end; (*parallellines*)

(*--------------------------------------------------------------------------
  This function returns a parameterized tangent of the half angle
 ---------------------------------------------------------------------------*)
function ParmTanhAngle(gmode: integer; angle: double): double;
var
  a:   double;      //Angle in Radians
  tha: double;      //Tangent of the half angle
  q:   integer;     //Quadrant
begin
  Result := 0.0;
  a := angle;
  if (angle = 0) then
    Result := 0
  else
  begin
    case gmode of
      0: (*angle in degrees*)
      begin
        while (a >= 360.0) do
          a := a - 360.0;     //reduce angle to < 360
        a := DegToRad(angle); //Convert Degrees to radians
      end;

      1: //Angle in radians, reduce to < 2 Pi radians
      begin
        while (a >= 6.283185307) do
          a := a - 6.283185307;
      end;
      else
      // setgeostatus(1);
    end;
    if (geostatus = 0) then
    begin
      q := Quadrant(3, a, p1glb);
      if (geostatus = 0) then
      begin
        case (q) of
          2: a := a - 1.570796327;   (*Second Quadrant*)
          3: a := a - 3.141592654;   (*Third Quadrant*)
          4: a := a - 4.712388981;   (*Fourth Quadrant*)
        end; (*switch Q of*)
        tha := (1 - cos(a)) / sin(a);
        case (q) of
          1: Result := tha;            (*First Quadrant*)
          2: Result := 1.0 + tha;      (*Second Quadrant*)
          3: Result := 2.0 + tha;      (*Third Quadrant*)
          4: Result := 3.0 + tha;      (*Fourth Quadrant*)
        end;
      end;
    end;
  end;
end;

(*--------------------------------------------------------------------
 Perpendicular returns true if the line segments L1 and L2 are
 perpendicular else false
 --------------------------------------------------------------------*)
function Perpendicular(l1, l2: LineSeg2D): boolean;
var
  l1verticalline: boolean; (*Flag indicating Line L1 is vertival*)
  s1, s2: double;  (*Slope of L1 and L2 respectively*)
begin
  l1verticalline := False;
  Slope(l1, s1);
  if (geostatus <> 0) then
    if (geostatus = 8) then
      l1verticalline := True
    else
      Result := False;
  if l1verticalline then
  begin
    Result := False;
    (*Assume false at this point*)
    Slope(l2, s2);
    case (geostatus) of
      0:
      begin
        case (l1verticalline) of
          True:
            if (s2 = 0) then (*Chk if L2 is horizontal*)
              Result := True;
          False:
          begin
            s1 := s1 * s2; (*get product of Slopes*)
            if (s1 = -1.0) then
              Result := True;
          end;
        end; (*switch L1VerticalLine*)
      end;

      else (*Check to see if lines mirror axis*)
      begin
        if (geostatus = 8) then (*L2 is a Vertical Line*)
          if (l1verticalline) then
            if (s1 = 0) then (*L1 is Horizontal Line*)
              Result := True;
      end; (*<>0*)
    end;
  end;
end;

{!  PntAngleDist returns the XY coordinates of a point (P1) at a given
  distance (Dist) from another point (AO) at a specified angle (ANGLE)}
procedure PntAngleDist(gmode: integer; ao: TPoint2D; dist, angle: double;
  p1: TPoint2D);
var
  a,    // Angle
  dx,   // Change in X
  dy,   // Change in Y
  ptha, (*Parmeterized tangent of the half angle*)
  ratio, tha, itha, (*Tangent of the half angle*)
  thas: double; (*Square Tangent of the half angle*)

begin
  a := angle;
  case gmode of
    0:  // Angle in degrees has to be reduced to an angle within 360
    begin
      while (a >= 360.0) do
        a := a - 360.0;
      ptha := parmtanhangle(0, a);
    end;
    1: // Angle in Radians has to be reduced to less than 2 PI radians
    begin
      while (a >= 6.283185307) do
        a := a - 6.283185307;
      ptha := parmtanhangle(1, a);
    end;

    2: //Angle already parameterized
    begin
      if (a >= 4.0) then
      // setgeostatus(19)
      else
        ptha := a;
    end;
    else
    // setgeostatus(1);
  end;
  begin
    {tha := modf(ptha,&itha);}(*get the tangent of half angle*)
    tha   := round(ptha) mod round(itha); (*get the tangent of half angle*)
    thas  := tha * tha;
    ratio := abs(dist) / (1.0 + thas);
    dx    := (1.0 - thas) * ratio;
    dy    := (tha + tha) * ratio;
    if (ptha < 1.0) then (*Angle 0 to 90*)
    begin
      p1.x := ao.x + dx;
      p1.y := ao.y + dy;
    end
    else if (ptha < 2.0) then (*Angle 90+ to 180*)
    begin
      p1.x := ao.x - dy;
      p1.y := ao.y + dx;
    end
    else if (ptha < 3.0) then (*Angle 180+ to 270*)
    begin
      p1.x := ao.x - dx;
      p1.y := ao.y - dy;
    end
    else (*Angle 270+ to 360*)
    begin
      p1.x := ao.x + dy;
      p1.y := ao.y - dx;
    end;
  end;
  (*if geostatus*)
  begin
    p1.x := 0;
    p1.y := 0;
  end;
end;

(*----------------------------------------------------------------
 This function determines if a point (Point) is on a line(LS)
 -----------------------------------------------------------------*)
function PointOnLine(ls: LineSeg2D; Point: TPoint2D): boolean;
begin
  Result := False;
  if (((Point.x <= ls.p1.x) and (Point.y <= ls.p1.y)) and
    ((Point.x >= ls.p2.x) and (Point.y >= ls.p2.y))) then
    Result := True;
  if (((Point.x >= ls.p2.x) and (Point.y <= ls.p2.y)) and
    ((Point.x <= ls.p1.x) and (Point.y >= ls.p1.y))) then
    Result := True;
  if (((Point.x >= ls.p1.x) and (Point.y >= ls.p1.y)) and
    ((Point.x <= ls.p2.x) and (Point.y <= ls.p2.y))) then
    Result := True;
  if (((Point.x <= ls.p2.x) and (Point.y >= ls.p2.y)) and
    ((Point.x >= ls.p1.x) and (Point.y <= ls.p1.y))) then
    Result := True;
  PointOnLine := Result;
end; //PointOnLine

(*-------------------------------------------------------------------------
 Quandrant returns the Quadrant that an angle or point is within
 --------------------------------------------------------------------------*)
function Quadrant(gmode: integer; angle: double; Point: TPoint2D): integer;
var
  deg,    //Angle in degrees
  rad,    //Angle in Radians
  rad90,  //Radians 90 degrees
  rad180, //Radians 180 degrees
  rad270, //Radians 270 degrees
  rad360, //Radians 360 degrees
  fp, rq: double;  //Radians
begin
  Result := 0;
  case gmode of
    0:  //Use XY coordinates of a Point
    begin
      if ((Point.x >= 0) and (Point.y >= 0)) then
        Result := 1
      else if ((Point.x < 0) and (Point.y >= 0)) then
        Result := 2
      else if ((Point.x < 0) and (Point.y <= 0)) then
        Result := 3
      else
        Result := 4;
    end; (*gmode := 0*)
    1:   //Use Parameterized Tangent of Half Angle
    begin
      {fp := modf(angle,&rq);}
      fp := round(angle) mod round(rq);
      if (rq = 0) then
        Result := 1
      else if (rq = 1.0) then
        Result := 2
      else if (rq = 2.0) then
        Result := 3
      else if (rq = 3.0) then
        Result := 4
      else;
          //  setgeostatus(19);
    end;  (*gmode := 1*)
    2:    //Use Angle in degrees
    begin
      if (angle < 0) then
      //setgeostatus(20)
      else
      begin
        deg := angle;
        while (deg > 360.0) do
          deg := deg - 360;
        if ((deg >= 0) and (deg < 90.0)) then
          Result := 1
        else if ((deg > 90.0) and (deg < 180.0)) then
          Result := 2
        else if ((deg > 180.0) and (deg < 270.0)) then
          Result := 3
        else
          Result := 4;
      end; (*else*)
    end;
       (* gmode := 2*)
    3: //Use Angle in radians
    begin
      if (angle < 0) then
      //setgeostatus(20)
      else
      begin
        rad90  := PI / 2;
        rad180 := PI;
        rad270 := rad90 + rad180;
        rad360 := 2 * PI;
        rad    := angle;
        while (rad >= rad360) do
          rad := rad - rad360;
        if ((rad >= 0) and (rad <= rad90)) then
          Result := 1
        else if ((rad > rad90) and (rad <= rad180)) then
          Result := 2
        else if ((rad > rad180) and (rad <= rad270)) then
          Result := 3
        else
          Result := 4;
      end;
    end;
      (*gmode := 3*)
    else
    // setgeostatus(1);
  end;//case
end; //Quadrant

{!  Slope calculates the Slope (S) of a line (L1)}
procedure Slope(l1: LineSeg2D; s: double);
var
  dx, dy: double; (*Change in X and Y*)
begin
  dx := l1.p2.x - l1.p1.x;
  dy := l1.p2.y - l1.p1.y;
  if (dx = 0) then
  begin
    s := MAXREAL;
    if (dy = 0) then
    // setgeostatus(7) (*P1 and P2 are the same point*)
    else;
    // setgeostatus(8); (*Line is vertical*)
  end
  else if (dy = 0) then  (*Horizontal Line*)
    s := 0.0
  else
    s := dy / dx;
end; (*Slope*)

 (* ------------------------------------------------------------------- *)
 (* This function determines the layout of point pnt and line segment   *)
 (* defined by its end points p1 and p2. Returning values are:          *)
 (*   0 - some error was detected, see GEOSTATUS value;                 *)
 (*   1 - point is to the left from line segment;                       *)
 (*   2 - point is to the right from line segment;                      *)
 (*   3 - point is on the line segment.                                 *)
 (* Warning! Screen coordinate system is used!                          *)
 (* ------------------------------------------------------------------- *)
function Point_Segm_Side(pnt, p1, p2: TPoint2D): integer;
var
  S: double;
begin
  if EqualPoints2D(p1, p2) then
  begin
    //setgeostatus(2);
    Result := 0;
    exit;
  end;

  S := (p1.x) * (p2.y - pnt.y) + (p2.x) * (pnt.y - p1.y) + (pnt.x) * (p1.y - p2.y);

  if S < 0 then
  begin
    Result := 1;
    exit; (* point is to the left  from segment *)
  end;
  if (S > 0) then
  begin
    Result := 2;
    exit;
  end;  (* point is to the right from segment *)
  Result := 3;                  // point is on the segment
end; (* Point_Segm_Side *)


{!  Points in area}
function Point_In_Area(A, B: double; Num: longint; Obl: PAREA): integer;
var
  AX, AY, BX, BY: double;
  i, COL, VH, VIH, H, IH: longint;
  Z, ZY: double;
  Ob:    PAREA;
begin
  Num := Num + 1;
  Ob  := Obl;
  COL := 0;
  VH  := 0;
  VIH := 0;
  H   := 0;
  IH  := 0;
  for i := 1 to Num - 1 do
  begin
    AX := Ob^.X;
    AY := Ob^.Y;
    Ob := Ob^.p;
    BX := Ob^.X;
    BY := Ob^.Y;

    if ((BX < A) and (AX < A)) or ((BX > A) and (AX > A)) then
      continue;
    if ((AX = A) and (AY = B)) or ((BX = A) and (BY = B)) then
    begin
      Result := 3;
      exit;
    end; (* Summit *)
    if ((AX = A) and (BX = A)) then
    begin
      if ((AY < B) and (BY > B)) or ((AY > B) and (BY < B)) then
      begin
        Result := 1;
        exit;
      end
      else
        continue;
    end;(* Border *)
    if ((AY = B) and (BY = B)) then
    begin
      Result := 1;
      exit;
    end;

    Z  := (BY - AY) / (BX - AX);
    ZY := A * Z + AY - AX * Z;

    if (ZY = B) then
    begin
      Result := 1;
      exit;
    end; (* border *)
    if (B > ZY) then
      continue;

    if ((AX < A) and (BX > A)) or ((AX > A) and (BX < A)) then
    begin
      Inc(COL);
      continue;
    end;

    if (AX < A) and (BX = A) then
    begin
      Inc(VH);
      continue;
    end;
    if (AX = A) and (BX > A) then
    begin
      Inc(VIH);
      continue;
    end;
    if (AX > A) and (BX = A) then
    begin
      Inc(H);
      continue;
    end;
    if (AX = A) and (BX < A) then
    begin
      Inc(IH);
      continue;
    end;
  end;
    {
    if (VH<VIH) then COL+:=VH else COL+:=VIH;
    if (H<IH)  then COL+:=H else COL+:=IH;
    }
  if (VH < VIH) then
    COL := COL + VH
  else
    COL := COL + VIH;
  if (H < IH) then
    COL := COL + H
  else
    COL := COL + IH;

  writeln(VH, VIH, H, IH, COL);
  if (COL mod 2 = 0) then
    Result := 1
  else
    Result := 0;
end;

 //------------------------------------------\\
 // ___________________3D____________________\\


(*--------------------------------------------------------------------
  This routine calculates the distance between two points in space
 ---------------------------------------------------------------------*)
function Distance3D(pz1, pz2: TPoint3D): double;
var
  dx, dy, dz: double;
begin
  dx := pz1.x - pz2.x;
  dy := pz1.y - pz2.y;
  dz := pz1.z - pz2.z;
  if ((dx = 0) and (dy = 0) and (dz = 0)) then
    Result := 0.0
  else
    Result := sqrt((dx * dx) + (dy * dy) + (dz * dz));
end;

(*---------------------------------------------------------------------------
  This function returns the perpendicular distance between two lines in space
  ----------------------------------------------------------------------------*)
function DistLineSpace(ples1, ples2: paralneqspace): double;
var
  dxo, dyo, dzo, dxy, dyz, dxz: double; // Misc
  pnt:   TPoint3D;  // Used when lines are parallel to define a pnt.
  denom: double;
begin
  dxo   := ples2.xo - ples1.xo;
  dyo   := ples2.yo - ples2.yo;
  dzo   := ples2.zo - ples2.zo;
  dxy   := ples1.dx * ples2.dy - ples2.dx * ples1.dy;
  dyz   := ples1.dy * ples2.dz - ples2.dy * ples1.dz;
  dxz   := ples1.dx * ples2.dz - ples2.dx * ples1.dz;
  denom := (dxy * dxy) + (dyz * dyz) + (dxz * dxz);
  if denom = 0 then
  begin // Lines are parallel
    pnt.x := ples1.xo;
    pnt.y := ples1.yo;
    pnt.z := ples1.zo;
    // Compute distance point to line
    Result := DistPntLineSpace(1, pnt, lssglb, ples2);
  end
  else
    Result := abs(dxo * dyz + dyo * dxz + dzo * dxy) / sqrt(denom);
end;

(*-------------------------------------------------------------------------
  This function returns the perpendicular distance from a point (Pnt) to
  a line (LSS,PLES) in space
  -------------------------------------------------------------------------*)
function DistPntLineSpace(gmode: integer; pnt: TPoint3D; lss: LineSeg3D;
  ples: paralneqspace): double;
var
  dx1, dy1, dz1, dx2, dy2, dz2: double;
  s:     boolean;
  tples: paralneqspace;
begin
  case gmode of
    0:
    begin // Convert line segment to Parametric equation
      s := normalglb;
      setnormalon;
      LineEqSpace(lss.p1, lss.p2, tples);
      normalglb := s;
    end;
    1:
    begin (*Check Coefficients of line eq*)
      tples := ples;
      NormalLineEqSpace(tples);
    end;
    else
    // setgeostatus(1);
  end; (*switch*)
  if (geostatus = 0) then
  begin
    dx1 := pnt.x - tples.xo;
    dy1 := pnt.y - tples.yo;
    dz1 := pnt.z - tples.zo;
    dx2 := tples.dx * dy1 - tples.dy * dx1;
    dy2 := tples.dx * dz1 - tples.dz * dx1;
    dz2 := tples.dy * dz1 - tples.dz * dy1;
    dx1 := tples.dy * dx2 + tples.dz * dy2;
    dy1 := tples.dz * dz2 - tples.dx * dx2;
    dz1 := -tples.dx * dy2 - tples.dy * dz2;
    Result := sqrt((dx1 * dx1) + (dy1 * dy1) + (dz1 * dz1));
  end  (*if geostatus*)
  else
    Result := 0.0;
end; (*DistPntLineSpace*)

(*-----------------------------------------------------------------------
  This function returns the perpendicular distance from a point (PZ1) to
   a plane (NPE)
  -----------------------------------------------------------------------*)
function DistPntPlane(npe: normalplaneeq; pz1: TPoint3D): double;
var
  tdist, denom: double;
begin
  denom := (npe.a * npe.a) + (npe.b * npe.b) + (npe.c * npe.c);
  if denom = 0 then
  // setgeostatus(25)
  else
  begin
    denom := inverse(sqrt(denom));
    tdist := (npe.a * pz1.x) + (npe.b * pz1.y) + (npe.c * pz1.z) + npe.d;
    if (npe.d >= 0) then
      denom := -denom;
    tdist := tdist * denom;
    Result := tdist;
  end;
end; (*DistPntPlane*)


function EqualPoints3D(p1, p2: TPoint3D): boolean;
begin
  if (p1.x = p2.x) and (p1.y = p2.y) and (p1.z = p2.z) then
    Result := True
  else
    Result := False;
end;

 // ------------------------------------------------------------------
 //  This function returns the acute angle between a line and a plane
 //-------------------------------------------------------------------
function AngleLinePlane(gmode: integer; npe: normalplaneeq;
  ples: paralneqspace): double;
var
  tnpe:  NormalPlaneEq;  (*Temporary Storage*)
  tples: ParalnEqSpace;
  angle: double;
begin
  tnpe  := npe;
  tples := ples;
  Normalize_Plane(tnpe);
  NormalLineEqSpace(tples);
  begin
    angle := (tnpe.a * tples.dx) + (tnpe.b * tples.dy) + (tnpe.c * tples.dz);
    case gmode of
      0:
      begin (*Angle in Degrees*)
        if (angle = 0) then
          Result := 90.0
        else
          Result := 90.0 - arccos(angle);
      end;
      1:
      begin (*Angle in Radians*)
        if (angle = 0) then
          Result := PI / 2
        else
          Result := 1.5707962 - DegToRad(arccos(angle));
      end;
      2:
      begin (*Cosine of angle*)
        if (angle = 0) then
          Result := 0.0
        else
          Result := angle;
      end;

      else
      //   setgeostatus(1);
    end;
  end;
end;

(*-----------------------------------------------------------------
  This function returns the angle between two lines in space
 ------------------------------------------------------------------*)
function AngleLinesSpace(gmode: integer; ples1, ples2: paralneqspace): double;
var
  numer: double;  (*Numerator - Sum of the direction numbers of the lines*)
  denom, denom1, denom2: double; (*Denominator - Normalizer*)
  angle: double;
begin
  Result := 0.0;
  denom1 := (ples1.dx * ples1.dx) + (ples1.dy * ples1.dy) + (ples1.dz * ples1.dz);
  denom2 := (ples2.dx * ples2.dx) + (ples2.dy * ples2.dy) + (ples2.dz * ples2.dz);
  denom  := denom1 * denom2;
  if denom = 0 then
  //  setgeostatus(4)
  else
  begin
    numer := ples1.dx * ples2.dx + ples1.dy * ples2.dy + ples1.dz * ples2.dz;
    angle := numer / sqrt(denom);
    case gmode of
      0:
      begin //Angle in Degrees
        if (angle = 0) then
          Result := 90.0
        else
          Result := arccos(angle);
      end;
      1:
      begin //Angle in Radians
        if (angle = 0) then
          Result := PI / 2
        else
          Result := DegToRad(arccos(angle));
      end;
      2:
      begin //Cosine of angle
        if (angle = 0) then
          Result := 0.0
        else
          Result := angle;
      end;

      else
       gmode := 1 //  setgeostatus(1);
    end;
  end;
end;

(*----------------------------------------------------------------
  This function returns the acute angle between two planes
  ----------------------------------------------------------------*)
function AngleTwoPlanes(gmode: integer; npe1, npe2: NormalPlaneEq): double;
var
  Tnpe1, Tnpe2: NormalPlaneEq;  (*Temporary Storage*)
  Angle: double;
begin
  tnpe1 := npe1;
  tnpe2 := npe2;
  Normalize_Plane(Tnpe1);
  begin
    Normalize_Plane(Tnpe2);
    if (geostatus = 0) then
      if (ParallelPlanes(tnpe1, tnpe2)) then
        if (gmode = 2) then
          Result := 1.0
        else
          Result := 0.0
      else
      begin
        Angle := abs((tnpe1.a * tnpe2.a) + (tnpe1.b * tnpe2.b) + (tnpe1.c * tnpe2.c));
        case gmode of
          0:
          begin //Angle in Degrees
            if (angle = 0) then
              Result := 90.0
            else
              Result := arccos(angle);
          end;
          1:
          begin //Angle in Radians
            if (angle = 0) then
              Result := PI / 2
            else
              Result := DegToRad(arccos(angle));
          end;
          2:
          begin //Cosine of angle
            if (angle = 0) then
              Result := 0.0
            else
              Result := angle;
          end;
          else
           geostatus := 1;
        end; //switch gmode
      end;
  end;
end;

{! ConvSpherical converts spherical coordinates to rectangular coordinates
  and visa versa}
procedure ConvSpherical(gmode: integer; scoord: sphericaleq; p1: TPoint3D);
var
  sinphi: double;    //Sin of Phi
  p0:     TPoint3D;  //Origin
begin
  case gmode of
    0:
    begin //Convert from spherical to rectangular
      if (scoord.rho = 0) then
      begin
        p1.x := 0.0;
        p1.y := 0.0;
        p1.z := 0.0;
      end
      else
      begin
        if ((scoord.phi < 0) or (scoord.theta < 0)) then
           geostatus := 58;  // setgeostatus(58);
          if (scoord.theta > 2 * PI) then
             geostatus := 59; // setgeostatus(59);
            if (scoord.phi > PI) then
               geostatus := 60;  // setgeostatus(60);
            if (geostatus = 0) then
            begin
              sinphi := sin(scoord.phi);
              p1.x   := scoord.rho * sinphi * cos(scoord.theta);
              p1.y   := scoord.rho * sinphi * sin(scoord.theta);
              p1.z   := scoord.rho * cos(scoord.phi);
            end;
      end; //else
    end; //case 0
    1:
    begin //Convert from rectangular to spherical
      p0.x := 0.0;
      p0.y := 0.0;
      p0.z := 0.0;
      if ((p0.x = p1.x) and (p0.y = p1.y) and (p0.z = p1.z)) then
      begin
        scoord.rho   := 0.0;
        scoord.phi   := 0.0;
        scoord.theta := 0.0;
      end
      else
      begin
        scoord.rho   := Distance3D(p0, p1);
        scoord.theta := DegToRad(arctang(p1.x, p1.y));
        if (scoord.rho <> 0) then
          scoord.phi := DegToRad(arccos(p1.z / scoord.rho))
        else
          scoord.phi := 0.0;
      end; (*else*)
    end;
      (*Case 1*)
    else
    // setgeostatus(1);
  end; (*switch*)
end; (*ConvSpherical*)

(*---------------------------------------------------------------------------
  CrossProduct returns the cross product (CP) of two vectors (V1,V2)
  ---------------------------------------------------------------------------*)
procedure CrossProductV1V2(v1, v2, cp: TVector);
begin
  cp.x := v1.y * v2.z - v1.z * v2.y;
  cp.y := v1.z * v2.x - v1.x * v2.z;
  cp.z := v1.x * v2.y - v1.y * v2.x;
end;


(*------------------------------------------------------------------------
  DotProduct returns the product of two vectors (V1,V2) in 2 or 3D space
  ------------------------------------------------------------------------*)
function DotProductV1V2(V1, V2: TVector): double;
begin
  Result := V1.X * V2.X + V1.Y * V2.Y + V1.Z * V2.Z;
end;

(*----------------------------------------------------------------
 This function determines if the value in A is between B and C.
 (B >= A <= C or C >= A <= B)
 -----------------------------------------------------------------*)
function Between(a, b, c: double): boolean;
begin
  if (b > c) then (*(C >= A <= B)*)
    if (a >= c) and (a <= b) then
      Result := True
    else
      Between := False
  else (*(B >:= A <:= C)*)
  if (a >= b) and (a <= c) then
    Result := True
  else
    Result := False;
end; (*Between*)

{! This procedure determines the XYZ coordinates (IntP) of the
  intersect of a line (PLES or LSS) and a plane (NPE)}
procedure IntLinePlane(gmode: integer; lss: LineSeg3D; ples: paralneqspace;
  npe: normalplaneeq; intp: TPoint3D);
var
  tples: paralneqspace; // Temporary Storage
  ratio, denom: double;
begin
  case gmode of
    0:
    begin // Line Segment
      LineEqSpace(lss.p1, lss.p2, tples); // get parametric equation
    end;
    1: tples := ples;         // parametric line equation
    else
    //setgeostatus(1);
  end; (*switch gmode *)
  if (geostatus = 0) then
  begin
    denom := (tples.dx * npe.a) + (tples.dy * npe.b) + (tples.dz * npe.c);
    if (denom = 0) then (*Check if Parallel*)
    //setgeostatus(29)
    else
    begin
      ratio  := -(tples.xo * npe.a + tples.yo * npe.b + tples.zo *
        npe.c + npe.d) / denom;
       (*Ratio is now the value of t at the intersection of the
         line and the plane. Now substitute back into the parametric
         equation of the line to get the XYZ coordinates.*)
      intp.x := tples.xo + tples.dx * ratio;
      intp.y := tples.yo + tples.dy * ratio;
      intp.z := tples.zo + tples.dz * ratio;
      if (gmode = 0) then (*Check on Line Segment*)
        if (((Between(intp.x, lss.p1.x, lss.p2.x) and Between(intp.y, lss.p1.y, lss.p2.y)) and Between(intp.y, lss.p1.y, lss.p2.y))) then;
      // setgeostatus(17);
    end; (* else *)
  end; (*if geostatus*)
end;

(*------------------------------------------------------------------------
  This procedure returns the XYZ coordinates of the intersection point
  of two lines in space
  ------------------------------------------------------------------------*)
procedure IntLineSpace(gmode: integer; ples1, ples2: paralneqspace;
  lss1, lss2: LineSeg3D; var intp: TPoint3D; var IntExists: boolean);
var
  denom1, denom2, denom3, s, t: double;
  //Parameters of parametric line equation
  snormal:    boolean; //Save Area for NormalGlb
  //Temporary Storage of intersection points
  //Direction Numbers of lines
  x1, y1, z1, x2, y2, z2, adx, ady, adz, udx, udy, udz, audx, audy, audz: double;
  tls1, tls2: LineSeg3D;
begin
  snormal := normalglb;
  setnormalon;
  IntExists := False;
  case gmode of
    0:
    begin // Coefficients two line equations
      tls1.p1.x := ples1.xo;
      tls1.p1.y := ples1.yo;
      tls1.p1.z := ples1.zo;
      tls1.p2.x := ples1.xo + ples1.dx;
      tls1.p2.y := ples1.yo + ples1.dy;
      tls1.p2.z := ples1.zo + ples1.dz;
      tls2.p1.x := ples2.xo;
      tls2.p1.y := ples2.yo;
      tls2.p1.z := ples2.zo;
      tls2.p2.x := ples2.xo + ples2.dx;
      tls2.p2.y := ples2.yo + ples2.dy;
      tls2.p2.z := ples2.zo + ples2.dz;
    end;
    1:
    begin (*Two Line segments*)
      tls1 := lss1;
      tls2 := lss2;
    end;
    2:
    begin (*Coefficients and line segment*)
      tls1.p1.x := ples1.xo;
      tls1.p1.y := ples1.yo;
      tls1.p1.z := ples1.zo;
      tls1.p2.x := ples1.xo + ples1.dx;
      tls1.p2.y := ples1.yo + ples1.dy;
      tls1.p2.z := ples1.zo + ples1.dz;
      tls2      := lss1;
    end;
    else
    //setgeostatus(1);
  end; (*switch*)
  if (geostatus = 0) then
  begin  (*GetIntersection*)
    adx    := tls1.p2.x - tls1.p1.x;
    ady    := tls1.p2.y - tls1.p1.y;
    adz    := tls1.p2.z - tls1.p1.z;
    udx    := tls2.p2.x - tls2.p1.x;
    udy    := tls2.p2.y - tls2.p1.y;
    udz    := tls2.p2.z - tls2.p1.z;
    denom1 := adx * udy - ady * udx;
    denom2 := adx * udz - adz * udx;
    denom3 := ady * udz - adz * udy;
    if (denom1 = 0) and (denom2 = 0) and (denom2 = 0) then
    // setgeostatus(31)
    else
    begin
      audy := tls1.p1.y - tls2.p1.y;
      audx := tls1.p1.x - tls2.p1.x;
      audz := tls1.p1.z - tls2.p1.z;
      if denom1 <> 0 then
      begin
        denom1 := inverse(denom1);
        t      := (audy * adx - audx * ady) * denom1;
        s      := (audy * udx - audx * udy) * denom1;
      end
      else
      begin
        if denom2 <> 0 then
        begin
          denom2 := inverse(denom2);
          t      := (audz * adx - audx * adz) * denom2;
          s      := (audz * udx - audx * udz) * denom2;
        end
        else
        begin
          denom3 := inverse(denom3);
          t      := (audz * ady - audy * adz) * denom3;
          s      := (audz * udy - audy * udz) * denom3;
        end;
      end;
      x1 := tls1.p1.x + adx * s;
      y1 := tls1.p1.y + ady * s;
      z1 := tls1.p1.z + (tls1.p2.z - tls1.p1.z) * s;
      x2 := tls2.p1.x + udx * t;
      y2 := tls2.p1.y + udy * t;
      z2 := tls2.p1.z + (tls2.p2.z - tls2.p1.z) * t;
      if (x1 = x2) and (y1 = y2) and (z1 = z2) then
        case gmode of
          0:
          begin
            intp.x    := x1;
            intp.y    := y1;
            intp.z    := z1;
            IntExists := True;
          end;
          1:
            if (t >= 0.0) and (t <= 1.0) and (s >= 0.0) and (s <= 1.0)
            (*Intersection point is within both line segment*) then
            begin
              intp.x    := x1;
              intp.y    := y1;
              intp.z    := z1;
              IntExists := True;
            end;
          2:
            if ((((Between(x2, tls2.p1.x, tls2.p2.x) and
              Between(y2, tls2.p1.y, tls2.p2.y)) and
              Between(z2, tls2.p1.z, tls2.p2.z)))) then
            begin
              intp.x    := x1;
              intp.y    := y1;
              intp.z    := z1;
              IntExists := True;
            end;
        end; (*switch*)
    end; (*else*)
  end; (* if *)
  normalglb := snormal;
end;

(*-------------------------------------------------------------
 This function returns the intersection point of three planes
 --------------------------------------------------------------*)
procedure IntThreePlanes(npe1, npe2, npe3: normalplaneeq; intp: TPoint3D);
var
  det: double; (*Minor Determinates*)
  ma, mb, mc, mda, mdb, mdc: double;
begin
  ma  := npe2.b * npe3.c - npe2.c * npe3.b;
  mb  := npe2.a * npe3.c - npe2.c * npe3.a;
  mc  := npe2.a * npe3.b - npe2.b * npe3.a;
  det := npe1.a * ma - npe1.b * mb + npe1.c * mc;
  if (det = 0) then
  //  setgeostatus(30) (*Planes are parallel*)
  else
  begin
    mdc    := npe2.d * npe3.c - npe3.d * npe2.c;
    mdb    := npe2.d * npe3.b - npe3.d * npe2.b;
    mda    := npe3.d * npe2.a - npe2.d * npe3.a;
    det    := inverse(det);
    intp.x := (npe1.b * mdc - npe1.d * ma - npe1.c * mdb) * det;
    intp.y := (npe1.d * mb - npe1.a * mdc - npe1.c * mda) * det;
    intp.z := (npe1.b * mda + npe1.a * mdb - npe1.d * mc) * det;
  end; (*else*)
end; (*IntThreePlanes*)

(*-----------------------------------------------------------
  This function returns the intersection line of two planes
  -----------------------------------------------------------*)
procedure IntTwoPlanes(npe1, npe2: NormalPlaneEq; ples: ParalnEqSpace);
var
  det, // direction numbers of intersection line
  dx, dy, dz, dda, ddb, ddc: double;  // Elimination factors
begin
  // Compute the direction numbers of the line of intersection
  dx  := (npe1.b * npe2.c) - (npe1.c * npe2.b);
  dy  := (npe1.c * npe2.a) - (npe1.a * npe2.c);
  dz  := (npe1.a * npe2.b) - (npe1.b * npe2.a);
  det := (dx * dx) + (dy * dy) + (dz * dz);
  if (det = 0) then
  //  Planes are parallel
  else
  begin
    ddb     := (npe1.d * npe2.b) - (npe2.d * npe1.b);
    ddc     := (npe1.d * npe2.c) - (npe2.d * npe1.c);
    ples.yo := -(ddc / dx);
    ples.zo := (ddb / dx);
    ples.xo := (ples.yo * npe1.b + ples.zo * npe1.c + npe1.d) / npe1.a;
    ples.dx := dx;
    ples.dy := dy;
    ples.dz := dz;
    if (normalglb) then
    begin
      det     := inverse(sqrt(det));
      ples.dx := ples.dx * det;
      ples.dy := ples.dy * det;
      ples.dz := ples.dz * det;
    end; (*if*)
  end;(*else*)
end; (*IntTwoPlanes*)

(*----------------------------------------------------------------------
  This function returns the coefficients a parametric line equation
  defined by the points P1 and P2 in three dimensional space
  ----------------------------------------------------------------------*)
procedure LineEqSpace(p1, p2: TPoint3D; ples: paralneqspace);
var
  dx,   (*Change in X*)
  dy,   (*Change in Y*)
  dz,   (*Change in Z*)
  norm: double; (*Normalizing factor*)
begin
  norm := 1.0;
  dx   := p2.x - p1.x;
  dy   := p2.y - p1.y;
  dz   := p2.z - p1.z;
  if ((dx = 0) and (dy = 0) and (dz = 0)) then
  //  setgeostatus(2) (*p1 and p2 are same point*)
  else
  begin
    if (normalglb) then
    begin
      (*Get Normalize constant*)
      norm := (dx * dx) + (dy * dy) + (dz * dz);
      if (norm = 0) then
      // setgeostatus(2)
      else
        norm := 1.0 / sqrt(norm);
    end;(*if*)
    ples.xo := p1.x;
    ples.yo := p1.y;
    ples.zo := p1.z;
    ples.dx := dx * norm;
    ples.dy := dy * norm;
    ples.dz := dz * norm;
  end;
end;

(*----------------------------------------------------------------------
  This function returns the midpoint (MP) of the line segment (lss) in
 three dimension space
 -----------------------------------------------------------------------*)
procedure MPLineSpace(lss: LineSeg3D; var mp: TPoint3D);
begin
  mp.x := (lss.p2.x + lss.p1.x) / 2;
  mp.y := (lss.p2.y + lss.p1.y) / 2;
  mp.z := (lss.p2.z + lss.p1.z) / 2;
end;


(*------------------------------------------------------------------------
  This function returns the normalized coefficients a parametric line
   equation in three dimensional space
  -----------------------------------------------------------------------*)
procedure NormalLineEqSpace(ples: paralneqspace);
var
  norm: double; (*Normalizing factor*)
begin
  norm := (ples.dx * ples.dx) + (ples.dy * ples.dy) + (ples.dz * ples.dz);
  if (norm = 0) then
  //  setgeostatus(4)
  else if (norm <> 1.0) then
  begin
    norm    := 1.0 / sqrt(norm);
    ples.dx := ples.dx * norm;
    ples.dy := ples.dy * norm;
    ples.dz := ples.dz * norm;
  end;
end; (*NormalLineEqSpace*)

(*-----------------------------------------------------------------------
  This function returns true if the planes NPE1 and NPE2 are parallel
  else false
  ----------------------------------------------------------------------*)
function ParallelPlanes(npe1, npe2: normalplaneeq): boolean;
var
  ra, rb, rc: double;  (*Ratio of coefficients A,B,C*)
  a1, a2, b1, b2, c1, c2: boolean;    (*True if Coeff := zero*)
begin
  if (npe1.a = 0) then
    a1 := True
  else
    a1 := False;
  if (npe1.b = 0) then
    b1 := True
  else
    b1 := False;
  if (npe1.c = 0) then
    c1 := True
  else
    c1 := False;
  if (npe2.a = 0) then
    a2 := True
  else
    a2 := False;
  if (npe2.b = 0) then
    b2 := True
  else
    b2 := False;
  if (npe2.c = 0) then
    c2 := True
  else
    c2 := False;
  if ((a2 and b2) and c2) then
  begin
    ra := npe1.a / npe2.a;
    rb := npe1.b / npe2.b;
    rc := npe1.c / npe2.c;
    if (ra = rb) and (ra = rc) then
      Result := True
    else
      Result := False;
  end
  else
  begin
     (*Check special planes perpendicular to XY,XZ and YZ planes
       or Perpendicular to the X,Y and Z axis.*)
    if (a1 and a2) then (*Test Perpendicular Z Axis*)
      if (b1 and b2) then
        if (c1 and c2) then
          (*Planes perpendicular Z Axis*)
          Result := True
        else
        begin
          //setgeostatus(25);
          Result := False;
        end;
    if (a1 and a2) then (*Test Perpendicular Y Axis*)
      if (c1 and c2) then
        if (b1 and b2) then
          (*Planes perpendicular Y Axis*)
          Result := True
        else
        begin
          //setgeostatus(25);
          Result := False;
        end;
    if (b1 and b2) then (*Test Perpendicular X Axis*)
      if (c1 and c2) then
        if (a1 and a2) then
          (*Planes perpendicular X Axis*)
          Result := True
        else
        begin
          //setgeostatus(25);
          Result := False;
        end;
    if (a1 and a2) then (*Test Perpendicular YZ Plane*)
      if (b1 and b2) then
        if (c1 and c2) then
        begin
          rb := npe1.b / npe2.b;
          rc := npe1.c / npe2.c;
          if (rb = rc) then (*Planes Perpendicular YZ plane*)
            Result := True
          else
            Result := False;
        end
        else
          Result := False;
    if (b1 and b2) then (*Test Perpendicular XZ Plane*)
      if (a1 and a2) then
        if (c1 and c2) then
        begin
          ra := npe1.a / npe2.a;
          rc := npe1.c / npe2.c;
          if (ra = rc) then (*Planes Perpendicular XZ plane*)
            Result := True
          else
            Result := False;
        end
        else
          Result := False;
    if (c1 and c2) then (*Test Perpendicular XY Plane*)
      if (b1 and b2) then
        if (a1 and a2) then
        begin
          rb := npe1.b / npe2.b;
          ra := npe1.a / npe2.a;
          if (rb = ra) then (*Planes Perpendicular XY plane*)
            Result := True
          else
            Result := False;
        end
        else
          Result := False;
  end; (*else*)
end; (*ParallelPlanes*)

procedure PlaneCoeff(pnt: TPoint3D; dx1, dy1, dz1, dx2, dy2, dz2: double;
  npe: NormalPlaneEq; Norm, InvNorm: double);
begin
  npe.a := (dy1 * dz2) - (dz1 * dy2);
  npe.b := (dz1 * dx2) - (dx1 * dz2);
  npe.c := (dx1 * dy2) - (dy1 * dx2);
  npe.d := -((pnt.x * npe.a) + (pnt.y * npe.b) + (pnt.z * npe.c));
  if ((npe.a = 0) and (npe.b = 0) and (npe.c = 0)) then
  //  setgeostatus(25)
  else if (normalglb) then
  begin (*Get Normalize constant*)
    norm := (npe.a * npe.a) + (npe.b * npe.b) + (npe.c * npe.c);
    if (norm <> 0) then
    begin
      invnorm := inverse(sqrt(norm));
      npe.a   := npe.a * invnorm;
      npe.b   := npe.b * invnorm;
      npe.c   := npe.c * invnorm;
      npe.d   := npe.d * invnorm;
    end
    else;
    // setgeostatus(25);
  end; (*If NormalGlb*)
end; (*PlaneCoeff*)

(* --------------------------------------------------------------- *)
procedure CheckPointDiff(dx, dy, dz: double);
begin
  if ((dx = 0) and (dy = 0) and (dz = 0)) then;
  // setgeostatus(24);
end; (*CheckPointDiff*)

(* ----------------------------------------------------------------
  This procedure returns the coefficients a normal plane equation
  -----------------------------------------------------------------*)
procedure PlaneEquation(gmode: integer; lss: LineSeg3D; ples: ParalnEqSpace;
  p1, p2, p3: TPoint3D; var npe: normalplaneeq);
var
  dx1, dy1, dz1, dx2, dy2, dz2, dx3, dy3, dz3: double; (*Change in X,Y,Z*)
  tples:   paralneqspace; (*Temporary Storage*)
  norm,  (*Normalizing Factor*)
  invnorm: double; (*Inverse of Normalizing Factor*)
begin
  case gmode of
    0:
    begin (*Three points define plane*)
      dx1 := p2.x - p1.x;
      dy1 := p2.y - p1.y;
      dz1 := p2.z - p1.z;
      dx2 := p3.x - p1.x;
      dy2 := p3.y - p1.y;
      dz2 := p3.z - p1.z;
      dx3 := p3.x - p2.x;
      dy3 := p3.y - p2.y;
      dz3 := p3.z - p2.z;
      CheckPointDiff(dx1, dy1, dz1);
      CheckPointDiff(dx2, dy2, dz2);
      CheckPointDiff(dx3, dy3, dz3);
      if (geostatus = 0) then
        PlaneCoeff(p2, dx1, dy1, dz1, dx2, dy2, dz2, npe, norm, invnorm);
    end;
    1: ;
    3:
    begin (*Two Points and line parallel*)
      case gmode of
        1:
        begin (*Coefficients of a line*)
          dx1 := ples.dx;
          dy1 := ples.dy;
          dz1 := ples.dz;
        end;
        3:
        begin (*Line Segment*)
          dx1 := lss.p2.x - lss.p1.x;
          dy1 := lss.p2.y - lss.p1.y;
          dz1 := lss.p2.z - lss.p1.z;
        end;
      end; (*switch*)
      dx2 := p2.x - p1.x;
      dy2 := p2.y - p1.y;
      dz2 := p2.z - p1.z;
      CheckPointDiff(dx1, dy1, dz1);
      CheckPointDiff(dx2, dy2, dz2);
      if (geostatus = 0) then
        PlaneCoeff(p2, dx1, dy1, dz1, dx2, dy2, dz2, npe, norm, invnorm);
    end;  (*gmode := 1,3*)

    2: ;
    4:
    begin (*Point on plane and a line normal to plane*)
      case gmode of
        2:
        begin (*Coefficients of a line*)
          tples := ples;
          if (normalglb) then
            NormalLineEqSpace(tples);
          npe.a := tples.dx;
          npe.b := tples.dy;
          npe.c := tples.dz;
          npe.d := -((p1.x * npe.a) + (p1.y * npe.b) + (p1.z * npe.c));
        end;
        4:
        begin (*Line Segment*)
          npe.a := lss.p2.x - lss.p1.x;
          npe.b := lss.p2.y - lss.p1.y;
          npe.c := lss.p2.z - lss.p1.z;
          npe.d := -((p1.x * npe.a) + (p1.y * npe.b) + (p1.z * npe.c));
          if ((npe.a = 0) and (npe.b = 0) and (npe.c = 0)) then
          //setgeostatus(25)
          else if (normalglb) then
          begin (*Get Normalize constant*)
            norm := (npe.a * npe.a) + (npe.b * npe.b) + (npe.c * npe.c);
            if (norm <> 0) then
            begin
              invnorm := inverse(sqrt(norm));
              npe.a   := npe.a * invnorm;
              npe.b   := npe.b * invnorm;
              npe.c   := npe.c * invnorm;
              npe.d   := npe.d * invnorm;
            end
            else;
            //setgeostatus(25);
          end; (*if normalglb*)
        end;
        (*case := 4*)
      end;(*case*)
    end; (*gmode := 2,4*)
    else
    // setgeostatus(1);
  end; (*switch gmode*)
end; (*PlaneEquation*)

(*-------------------------------------------------------------------------
  ProjectedNormal returns the components of a vector (nv) that is
  perpendicular to the vector (v2) in the direction of the vector v1
  -------------------------------------------------------------------------*)
procedure ProjectedNormal(v1, v2: TVector; var nv: TVector);
begin
  if ((v2.x = 0) and (v2.y = 0) and (v2.z = 0)) then
  begin
    nv.x := 0;
    nv.y := 0;
    nv.z := 0;
  end
  else
  begin
    ProjectVector(v1, v2, nv);
    nv.x := v1.x - nv.x;
    nv.y := v1.y - nv.y;
    nv.z := v1.z - nv.z;
  end;
end; (*ProjectedNormal*)

(*--------------------------------------------------------------------------
  ProjectVector returns the components of a vector (pv) that
   is result of projecting the vector (v1) onto vector (v2)
  -------------------------------------------------------------------------*)
procedure ProjectVector(v1, v2, pv: TVector);
var
  p1:     TPoint2D;
  factor: double;
begin
  if ((v2.x = 0) and (v2.y = 0) and (v2.z = 0)) then
  begin
    pv.x := 0;
    pv.y := 0;
    pv.z := 0;
  end
  else
  begin
    factor := DotProductV1V2(v1, v2) / ((v2.x * v2.x) + (v2.y * v2.y) + (v2.z * v2.z));
    pv.x   := factor * v2.x;
    pv.y   := factor * v2.y;
    pv.z   := factor * v2.z;
  end;
end; (*ProjectVector*)

(*--------------------------------------------------------------
  UnitVector returns the unit vector (UV) of the vector (V)
  --------------------------------------------------------------*)
procedure UnitVector(v, uv: TVector);
var
  norm: double; (*normal factor (Vector Magnitude)*)
begin
  norm := (v.x * v.x) + (v.y * v.y) + (v.z * v.z);
  if (norm = 0) then
  // setgeostatus(43)
  else
  begin
    norm := inverse(sqrt(norm));
    uv.x := v.x * norm;
    uv.y := v.y * norm;
    uv.z := v.z * norm;
  end;
end; (*UnitVector*)

(*-------------------------------------------------------------------------
  VectorAdd returns the resultant vector (v3) of the two component
  vectors (v1,v2) added together
  -------------------------------------------------------------------------*)
procedure VectorAdd(v1, v2, v3: TVector);
begin
  v3.x := v1.x + v2.x;
  v3.y := v1.y + v2.y;
  v3.z := v1.z + v2.z;
end; (*VectorAdd*)

(*------------------------------------------------------------------*)
(* VectorDirection returns the direction of the vector in radians in
   2D space.*)
procedure VectorDirection(v: TVector);
begin (*VectorDirection*)
  if (v.x = 0) and (v.y = 0) then
  //  setgeostatus(43)
  else
    v.d := DegToRad(arctang(v.x, v.y));
end; (*VectorDirection*)

(*--------------------------------------------------------------------
  This function returns the length or magnitude of a vector
  --------------------------------------------------------------------*)
procedure VectorLength(V: TVector);
begin
  V.M := sqrt((V.X * V.X) + (V.Y * V.Y) + (V.Z * V.Z));
end;

(*--------------------------------------------------------------------
  Calculates the center of triangle in space for vertices with
  different weights
 --------------------------------------------------------------------*)
procedure TriGravity(p1, p2, p3: TPoint3D; var TriG: TPoint3D);
var
  lss1, lss2: LineSeg3D;
  mp: TPoint3D;
  IntExists: boolean;
  ples1, ples2: paralneqspace;

  {sub}
  procedure LineSegment(p1, p2: TPoint3D; var lss: LineSeg3D);
  begin
    if (p1.x < p2.x) then
    begin
      lss.p1 := p1;
      lss.p2 := p2;
    end
    else
    begin
      lss.p1 := p2;
      lss.p2 := p1;
    end;
  end;

begin
  LineSegment(p1, p2, lss1);
  MPLineSpace(lss1, mp);
  LineSegment(p3, mp, lss1);

  LineSegment(p1, p3, lss2);
  MPLineSpace(lss2, mp);
  LineSegment(p2, mp, lss2);

  IntLineSpace(1, ples1, ples2, lss1, lss2, TriG, IntExists);
end;

(*----------------------------------------------------------
  Computes the intersection point(s) of two arcs
  ----------------------------------------------------------*)
procedure IntArcs(arc1, arc2: arctype; intp1, intp2: TPoint2D; nrint: integer);
var
  circ1, circ2: TCircleEq;
  tnrint: integer;
  p1, p2, sp1, sp2, ep1, ep2: TPoint2D;
  ArcRect1, ArcRect2: LineSeg2D;
begin
  nrint  := 0;
  tnrint := 0;
  arcpnts(arc1, sp1, ep1);
  if (geostatus = 0) then
    arcpnts(arc2, sp2, ep2);
  if (geostatus = 0) then
  begin
    ArcRectangle(arc1, sp1, ep1, ArcRect1);
    ArcRectangle(arc2, sp2, ep2, ArcRect2);
    circ1.cp  := arc1.cp;
    circ1.rad := arc1.rad;
    circ2.cp  := arc2.cp;
    circ2.rad := arc2.rad;
    if (geostatus = 0) then
    begin
      if (((((InRectangle(ArcRect1, ArcRect2.p1) or
        InRectangle(ArcRect1, ArcRect2.p2)) or
        InRectangle(ArcRect2, ArcRect1.p1)) or
        InRectangle(ArcRect2, ArcRect1.p2)))) then
        IntCircles(0, circ1, circ2, p1, p2, tnrint)
      else if (lineloc(ArcRect1, ArcRect2) = 1) then
        (*Two diagonals possibly intersect*)
        IntCircles(0, circ1, circ2, p1, p2, tnrint);
      if (geostatus = 0) then
        case tnrint of
          1:
            if (InRectangle(ArcRect1, p1)) then
            begin
              nrint := 1;
              intp1 := p1;
            end;
          2:
            if (InRectangle(ArcRect1, p1)) then
            begin  (*Get first intersection*)
              nrint := 1;
              intp1 := p1;
              if (InRectangle(ArcRect1, p2)) then
              begin  (*Get second intersection*)
                nrint := 2;
                intp2 := p2;
              end;
            end;
          else
            if (InRectangle(ArcRect1, p2)) then
            begin  (*Move second intersection to first*)
              nrint := 1;
              intp1 := p2;
            end;
        end; (*switch tnrint*)
    end; (*if geostatus*)
  end; (*if geostatus*)
end; (*intarcs*)

(*-------------------------------------------------------------
  IntArcLine computes the intersection of a line and an arc
  -------------------------------------------------------------*)
procedure IntArcLine(gmode: integer; arc1: arctype; ls: LineSeg2D;
  ile: ImPlineEq; ple: ParalineEq; intp1, intp2: TPoint2D; nrint: integer);
var
  aa:      double;  // ArcAngle
  ArcRect: LineSeg2D;
  p1, p2, sp, ep: TPoint2D;
  tgmode:  integer;
  circ:    TCircleEq;
  tple:    ParalineEq;
  tile:    ImPlineEq;
begin
  nrint := 0;
  arcpnts(arc1, sp, ep);
  if (geostatus = 0) then
  begin
    ArcRectangle(arc1, sp, ep, ArcRect);
    circ.cp  := arc1.cp;
    circ.rad := arc1.rad;
    if (geostatus = 0) then
    begin
      case gmode of
        0:
        begin //Intesect arc and line segment
          if (((((InRectangle(ArcRect, ls.p1) or
            InRectangle(ArcRect, ls.p2)) or InRectangle(ls, ArcRect.p1)) or
            InRectangle(ls, ArcRect.p2)))) then
            intcircleline(0, circ, ls, ple, p1, p2, nrint)
          else if (lineloc(ArcRect, ls) = 1) then
            // line and diagonal possibly intersect
            intcircleline(0, circ, ls, ple, p1, p2, nrint);
        end;
        1:
        begin  (*intersect PLE and Arc*)
          intcircleline(2, circ, ls, ple, p1, p2, nrint);
        end;
        2:
        begin //Intersection arc and implcit line equation
          tile := ile;
          convlineeq(0, tile, tple);
          if (geostatus = 0) then
            intcircleline(2, circ, ls, tple, p1, p2, nrint);
        end;
        else
        //setgeostatus(1);
      end; (*switch gmode*)
      if (geostatus = 0) then
        case (nrint) of
          1: if (InRectangle(ArcRect, p1)) then
            begin
              nrint := 1;
              intp1 := p1;
            end;
          2: if (InRectangle(ArcRect, p1)) then
            begin  (*Get first intersection*)
              nrint := 1;
              intp1 := p1;
              if (InRectangle(ArcRect, p2)) then
              begin  (*Get second intersection*)
                nrint := 2;
                intp2 := p2;
              end;
            end;
          else
            if (InRectangle(ArcRect, p2)) then
            begin  (*Move second intersection to first*)
              nrint := 1;
              intp1 := p2;
            end;
        end; (*switch nrint*)
    end; (*if geostatus*)
  end; (*if geostatus*)
end;


 {! Calculates the interesection point(s) of two circles}
procedure IntCircles(gmode: integer; c1, c2: TCircleEq; intp1, intp2: TPoint2D;
  nrint: integer);
var
  sraddiff, sradsum, srad1, srad2, parm1, parm2, parm3, x1, y1, x2,
  y2, xdiff, ydiff: double;
begin
  nrint := 0;
  case gmode of
    0: ;  //Both Circles in Rad and CP form
    1:    //Need cp and rad of c1 and c2
    begin
      CpRadCircle(c1);
      CpRadCircle(c2);
    end;
    2:
      CpRadCircle(c2); (*CP and Radius of C2 Needed*)
    else
      Exit; (*Bad Mode*)
  end; (*case*)
  begin
    srad1 := c1.rad * c1.rad;
    srad2 := c2.rad * c2.rad;
    xdiff := c1.cp.x - c2.cp.x;
    ydiff := c1.cp.y - c2.cp.y;
    parm1 := SumSqrdParms(xdiff, ydiff); // Sqrd cp Distance
    if (parm1 <= 0) then
    begin  //Circles Same Center
           (*nrint) := 0;     (*Circles do not intersect*)
    end
    else
    begin
      sraddiff := srad2 - srad1;
      sradsum  := srad1 + srad2;
      parm3    := 2.0 * sradsum * parm1 - (parm1 * parm1) - (sraddiff * sraddiff);
      if (parm3 >= 0) then
      begin
        parm1 := 0.5 / parm1;
        parm2 := 0.5 - sraddiff * parm1;
        if (c1.cp.x <= c2.cp.x) then
          x1 := c1.cp.x + abs(xdiff) * parm2
        else
          x1 := c1.cp.x - abs(xdiff) * parm2;
        if (c1.cp.y <= c2.cp.y) then
          y1 := c1.cp.y + abs(ydiff) * parm2
        else
          y1 := c1.cp.y - abs(ydiff) * parm2;
        if (parm3 = 0) then
        begin
          nrint   := 1;
          intp1.x := x1;
          intp1.y := y1;
        end
        else
        begin
          nrint   := 2;
          parm3   := parm1 * sqrt(parm3);
          x2      := xdiff * parm3;
          y2      := ydiff * parm3;
          intp1.x := x1 - y2;
          intp1.y := y1 + x2;
          intp2.x := x1 + y2;
          intp2.y := y1 - x2;
        end;
      end;
    end;
  end; (*if geoStatus*)
end;

{!  Determines the interesection point(s) of a line and a circle}
procedure IntCircleLine(gmode: integer; c1: TCircleEq; l1: LineSeg2D;
  ple: ParalineEq; intp1, intp2: TPoint2D; nrint: integer);
var
  sdxdy, parm1, parm2, t1, t2,  //Value of T at intersection
  xdiff, ydiff: double;
  tgmode: integer;  //Temporary GMode
  sn:     boolean;  //Save NormalGlb Status
begin  //intcircleline
  nrint  := 0;
  pleglb := ple;
  cglb   := c1;
  case gmode of
    0:
    begin  //Need to convert line segment to parametric Eq
      tgmode := 1;
      sn     := normalglb;
      SetNormalOn;
      LineEquation(tgmode, l1.p1, l1.p2, ileglb, pleglb);
      NormalGlb := sn;
    end;
    1:
    begin  //Need to get CP and Radius and convert line
      tgmode := 1;
      sn     := normalglb;
      setnormalon;
      lineequation(tgmode, l1.p1, l1.p2, ileglb, pleglb);
      normalglb := sn;
      CpRadCircle(cglb);
    end;
    2: NormalizePle; // Need to check if Parametric Line is Normalized
    3:
    begin
      CpRadCircle(cglb);
      NormalizePle;
    end;
    else
      gmode := 0 //Bad gmode
  end; // switch
  if (geostatus = 0) then
  begin
    (*Get the difference between XO and CP.X to determine
     if difference is less than Radius. If not no intersection*)
    xdiff := cglb.cp.x - pleglb.xo;
    ydiff := cglb.cp.y - pleglb.yo;
    parm1 := pleglb.dx * ydiff - pleglb.dy * xdiff;
    parm2 := (cglb.rad * cglb.rad) - (parm1 * parm1);
    if (parm2 < 0) then
      nrint := 0    //Line does not intersect circle
    else
    begin
      parm1 := pleglb.dx * xdiff + pleglb.dy * ydiff;
      if (parm2 = 0) then
      begin  //One Intersection - Line is tangent
        intp1.x := pleglb.xo + pleglb.dx * parm1;
        intp1.y := pleglb.yo + pleglb.dy * parm1;
        if (((gmode = 0) or (gmode = 1))) then
        begin
          if (InBetween(l1, intp1)) then
            nrint := 1;
        end
        else
          nrint := 1;
      end
      else
      begin  //Two Intersections
        parm2   := sqrt(parm2);
        t1      := parm1 - parm2;
        t2      := parm1 + parm2;
        intp1.x := pleglb.xo + pleglb.dx * t1;
        intp1.y := pleglb.yo + pleglb.dy * t1;
        intp2.x := pleglb.xo + pleglb.dx * t2;
        intp2.y := pleglb.yo + pleglb.dy * t2;
        if (((gmode = 0) or (gmode = 1))) then
        begin
          if (InBetween(l1, intp1)) then
            nrint := nrint + 1;
          if (InBetween(l1, intp2)) then
          begin
            if (nrint = 0) then
              intp1 := intp2;
            nrint := nrint + 1;
          end;
        end
        else
          nrint := 2;
      end;
    end;
  end;
end;

{! Calculates the intersection of two given lines}
procedure Intersect(gmode: integer; ile1, ile2: ImPlineEq; ple1, ple2: ParalineEq;
  ls1, ls2: LineSeg2D; var intp: TPoint2D; var IntExists: boolean);
var
  s, t, parm1, parm2, parm3, // Temp Storage
  d: double;                 // Distance
  tmode, intloc: integer;    // intercept location - See IntersectLs
begin
  intloc    := 0;
  IntExists := False;
  case gmode of
    0:
    begin  //Two lines defined by implicit line equations
      parm1 := abs(ile1.a - ile2.a);
      parm2 := abs(ile1.b - ile2.b);
      parm3 := abs(ile1.c - ile2.c);
      if (parm1 = 0) and (parm2 = 0) and (parm3 = 0) then
         gmode := 5 // setgeostatus - the same Line
      else
      begin
        d := ile1.a * ile2.b - ile2.a * ile1.b;
        if (d = 0) then
          gmode := 5 //Lines are parallel
        else
        begin
          d      := inverse(d);
          intp.x := (ile1.b * ile2.c - ile2.b * ile1.c) * d;
          intp.y := (ile2.a * ile1.c - ile1.a * ile2.c) * d;
          IntExists := True;
        end;
      end;
    end;
    1:
    begin  //Two lines defined by parametric line equations
      parm1 := ple1.dx * ple2.dy;
      parm2 := ple2.dx * ple1.dy;
      d     := parm2 - parm1;
      if (d = 0) then
        gmode := 5 //Line are parallel
      else
      begin
        parm1  := ple2.xo - ple1.xo;
        parm2  := ple2.yo - ple1.yo;
        d      := inverse(d);
        parm3  := ((ple2.dx * parm2) - (ple2.dy * parm1)) * d;
        intp.x := ple1.xo + ple1.dx * parm3;
        intp.y := ple1.yo + ple1.dy * parm3;
        if (intp.x = 0) then
          intp.x := 0.0;
        if (intp.y = 0) then
          intp.x := 0.0;
        IntExists := True;
      end;
    end;
    2:
    begin  (*One Line Implicit the other Parametric*)
      d := (ile1.a * ple1.dx) + (ile1.b * ple1.dy);
      if (d = 0) then
        gmode := 5 //Line are parallel
      else
      begin
        d      := inverse(d);
        parm1  := (ple1.xo * ple1.dy) - (ple1.yo * ple1.dx);
        intp.x := (ile1.b * parm1 - ile1.c * ple1.dx) * d;
        intp.y := -(ile1.a * parm1 + ile1.c * ple2.dy) * d;
        IntExists := True;
      end; (*else*)
    end;
    3:
    begin (*Intersection two line segments*)
      IntersectLs(ls1, ls2, intp, intloc);
      if (intloc = 0) then
        IntExists := True;
    end;
    4:
    begin (*Implicit line equation and line segment*)
      tmode := 0;
      LineEquation(tmode, ls1.p1, ls1.p2, ileglb, pleglb);
      if (geostatus = 0) then
      begin
        Intersect(tmode, ile1, ileglb, ple1, ple2, ls1, ls2, intp, IntExists);
        if (IntExists) then
          if not (InBetween(ls1, intp)) then
            IntExists := False;
      end; (*if geostatus*)
    end;
    5:
    begin  (*Parametric line equation and line segment.*)
      tmode := 1;
      LineEquation(tmode, ls1.p1, ls1.p2, ileglb, pleglb);
      if (geostatus = 0) then
      begin
        Intersect(tmode, ile1, ile2, ple1, pleglb, ls1, ls2, intp, IntExists);
        if (IntExists) then
          if not (InBetween(ls1, intp)) then
            IntExists := False;
      end;
    end;
      (*gmode 5*)
    else
    // setgeostatus(1); (*Bad Mode*)
  end; (*switch gmode*)
end;

function Location(l1, l2: LineSeg2D): double;
var
  dx, dy, dx1, dy1, dx2, dy2: double;
begin
  dx     := l1.p2.x - l1.p1.x;
  dy     := l1.p2.y - l1.p1.y;
  dx1    := l2.p1.x - l1.p1.x;
  dy1    := l2.p1.y - l1.p1.y;
  dx2    := l2.p2.x - l1.p2.x;
  dy2    := l2.p2.y - l1.p2.y;
  Result := (dx * dy1 - dy * dx1) * (dx * dy2 - dy * dx2);
end;

(*-----------------------------------------------------------------
  This function determines if two lines (L1,L2) intersect
  -----------------------------------------------------------------*)
function IntersectF(l1, l2: LineSeg2D): boolean;
begin
  if (location(l1, l2) <= 0) and (location(l2, l1) <= 0) then
    Result := True
  else
    Result := False;
end;

{! It Calculates the intersection point of two line segments if an
 intersection is found}
procedure IntersectLs(ls1, ls2: LineSeg2D; var intp: TPoint2D;
  var intloc: integer);
var
  dl2x,  //Change in X in line segment 2
  dl2y,  (*Change in Y in line segment 2*)
  dl1x,  (*Change in X in line segment 1*)
  dl1y,  (*Change in X in line segment 1*)
  dl12x, (*Change in X between line segments 1 and 2*)
  dl12y, (*Change in Y between line segments 1 and 2*)
  denom, (*Denominator*)
  parm1, parm2: double; (*Misc*)
begin    (*IntersectLs*)
  dl2x  := ls2.p2.x - ls2.p1.x;
  dl2y  := ls2.p2.y - ls2.p1.y;
  dl1x  := ls1.p2.x - ls1.p1.x;
  dl1y  := ls1.p2.y - ls1.p1.y;
  dl12x := ls2.p1.x - ls1.p1.x;
  dl12y := ls2.p1.y - ls1.p1.y;
  denom := dl2x * dl1y - dl2y * dl1x;
  if (denom = 0) then
    intloc := 1
  else
  begin
    denom := inverse(denom);
    parm1 := ((dl2x * dl12y) - (dl2y * dl12x)) * denom;
    parm2 := ((dl1x * dl12y) - (dl1y * dl12x)) * denom;
    if ((parm1 < 0) or (parm1 > 1.0) or (parm2 < 0) or (parm2 > 1.0)) then
      intloc := 2
    else
    begin //Intersection found
      intloc := 0;
      intp.x := ls1.p1.x + dl1x * parm1;
      intp.y := ls1.p1.y + dl1y * parm1;
      if (intp.x = 0) then
        intp.x := 0.0;
      if (intp.y = 0) then
        intp.y := 0.0;
    end;
  end;
end;

end.
