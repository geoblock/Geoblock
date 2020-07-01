//-------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-------------------------------------------------------------------------
// The unit for constructing triangulations and voronoi diagrams in 2D

(*! It creates 2D Delaunay triangulation and Voronoi tesselation of a set
   of points. Coincident points are rejected.
   Translated from C code published in paper
   'An algorithm for the Triangulation of Arbitrary Distributed Points:
   Applications to Volume Estimate and Terrain Fitting'. G.Macedonio and
   M.T.Pareschi. Computers & Geosciences. Vol.17, No.7, pp.859-874.
   Main parts of the algorithm are taken from
       Preparata, F.P. and Shamos, M.I., 1985, Computational Gedometry:
       Springer-Verlag, New York, 391 p.

    MAXOPT = Maximum number of recursive calls of the routine that
             performs delaunay  optimization (Optim).
             This number is always less than MAXTRI/3.
             you can set  any value.  Greater numbers assure that
             delaunay triangulation is well performed but the program
             may require too much RAM memory. Usually the number of
             recursive calls is less than ten.
 *)

unit uDelaunay2D;

interface

uses
  System.SysUtils,

  uCommon,
  GBGeometry,
  uDiscoCore;

const
  MAXPTS = 1000024; // MAX. NUM. OF POINTS
  MAXTRI = 2000048; // MAX. NUM. OF TRIANGLES = 2*MAXPTS
  MAXOPT = 1512;    // MAXIMUM OPTIMIZ. LEVEL < MAXTRI/3
  DBERR  = 1.0E-15;  // DOUBLE  PRECISION  ACCURACY
  Jnd: array[0..2] of integer = (1, 2, 0); // Auxiliary  vector
  Jrd: array[0..2] of integer = (2, 0, 1); // Auxiliary  vector

type
  PNumbers = ^TNumbers;
  TNumbers = array[0..MAXPTS - 1] of integer;

type
  PCoords = ^TCoords;
  TCoords = array[0..MAXPTS - 1] of real;

type
  Pvt = ^Tvt;
  Tvt = array[0..MAXTRI - 1] of integer;

type
  TBreakSegment = TSegment3D;

type
  PTriangulator2D = ^TTriangulator2D;

  TTriangulator2D = class(TObject)
  public
    px, py, pz: PCoords;  // Coordinates of vertices
    vt1, vt2, vt3: Pvt;   // Numbers of vertices
    nt1, nt2, nt3: Pvt;   // Numbers of nearest triangles
    Dcd:  PNumbers;
    Npts: integer;        // Number of vertices
    Ntri: integer;        // Number of triangles  -1
    Nvor: integer;        // Number of Voronoi polygons
    Xmin, Xmax, Ymin, Ymax, Zmin, Zmax: real; // Min and max coordinates
    VorDiagram: array of TPolygonArr3d;
    TriVertices: array of integer; //three vertices of a triangle
    ConvexHull: array of integer;  //Indices of selected points for the hull
    constructor Create;
    destructor Destroy; override;
    function CkTurn(var pv0: Pvt; i2, i3: integer): integer;
    function TriFind(X, Y: real; Tr: integer; var option: integer): integer;
    function GetNumFirstTriCW(Nv, Nt: integer): integer;
    function GetNumNextTriCW(Nv, Nt: integer): integer;
    function GetNumPrevTriCW(Nv, Nt: integer): integer;
    function CkLop(i1, i2, i3, i4: integer): integer;
    procedure SwapTr(t1, t2, k1, k2: integer);
    procedure Optim(it, k1: integer);
    procedure SortNtCW;
   (* GetVertNum returns Number of Vertices whith Coords (X;Y),
      if they are exist and -1 in opposite case *)
    function GetVertNum(X, Y: real): integer;
    procedure Delaunay2D;
    procedure Voronoi2D;
   (* AddBreakSegment finds a first quadrilateral through which goes
      BreakSegment = Value and then swaps diagonals in this quadrilateral
      if needed. This is not perfect but might work in 90% cases *)
    procedure AddBreakSegment(Value: TBreakSegment);
    function GetIndexNearTriangle(Triangle, NearTriangle: integer): shortint;
    function GetNumTriangleOppositeVertex(TriangleNum, VertNum: integer): integer;
    (* GetEdgeOppositeVertex returns Segment2D opposite Vertix
       with VertNum number in array of vertices for the triangle
       with TriangleNum number *)
    function GetEdgeOppositeVertex(TriangleNum, VertNum: integer): TSegment2D;
    property BreakSegment: TBreakSegment Write AddBreakSegment;
  private
    function SwapTriangle(Trian1, Trian2: integer): boolean;
    procedure ChangeNearTriangle(Triangle, OldNearTriangle,
      NewNearTriangle: integer);
    function GetEdge(TriangleNum: integer; Index1_3: shortint): TSegment2D;
    function ConvertVertexToSegment2D(VertNum1, VertNum2: integer): TSegment2D;
    (* isIntersectTwoSegment2D return True on Intersection of Segment1 and Segment2,
       when else return False
       WARNING: if adjacent Segment1 and Segment2 (<) in a one point
       isIntersectTwoSegment2D return Fasle *)
    function isIntersectTwoSegment2D(Segment1, Segment2: TSegment2D): boolean;
  end;

//==========================================================================
implementation
//==========================================================================

var
  vtm, ntm, ntr: array[0..2] of integer; // Auxiliary pointers
  Loptim: integer; // Optimization level

constructor TTriangulator2D.Create;
begin
  inherited;
  New(Px);
  New(Py);
  New(Pz);
  New(dcd);
  New(vt1);
  Fillchar(vt1^, Sizeof(vt1^), 0);
  New(vt2);
  Fillchar(vt2^, Sizeof(vt2^), 0);
  New(vt3);
  Fillchar(vt3^, Sizeof(vt3^), 0);
  New(nt1);
  New(nt2);
  New(nt3);
  Fillchar(nt1^, Sizeof(nt1^), 0);
  Fillchar(nt2^, Sizeof(nt2^), 0);
  Fillchar(nt3^, Sizeof(nt3^), 0);
end;

destructor TTriangulator2D.Destroy;
begin
  try
    Dispose(Px);
  finally
    try
      Dispose(Py);
    finally
      try
        Dispose(Pz);
      finally
        try
          Dispose(vt1);
        finally
          try
            Dispose(nt1);
          finally
            try
              Dispose(vt2);
            finally
              try
                Dispose(nt2);
              finally
                try
                  Dispose(vt3);
                finally
                  try
                    Dispose(nt3);
                  finally
                    try
                      Dispose(dcd);
                    finally
                      try
                        VorDiagram := nil;
                      finally
                        try
                          TriVertices := nil;
                        finally
                          inherited;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//===============================================================
(*  CKTURN - Checks if three vertices are :
                 alligned                (0)
                 counterclockwise tern   (-1)
                 clockwise tern          (1)
                 coincident points       (2)                    *)
//===============================================================

function TTriangulator2D.CkTurn(var pv0: Pvt; i2, i3: integer): integer;

var
  aux: double;
  i1:  integer;

begin
  i1 := i2 - 1;
  if i1 = -1 then
    aux := 0
  else
    aux := (py^[pv0^[i2]] - py^[pv0^[i1]]) * (px^[pv0^[i3]] - px^[pv0^[i1]]) -
      (py^[pv0^[i3]] - py^[pv0^[i1]]) * (px^[pv0^[i2]] - px^[pv0^[i1]]);
  if (aux > DBERR) then
  begin
    ckturn := 1;
    exit;
  end;
  if (aux < -DBERR) then
  begin
    ckturn := -1;
    exit;
  end;
  if i1 = -1 then
    aux := 0
  else
    aux := (px^[pv0^[i3]] - px^[pv0^[i2]]) * (px^[pv0^[i2]] - px^[pv0^[i1]]) +
      (py^[pv0^[i3]] - py^[pv0^[i2]]) * (py^[pv0^[i2]] - py^[pv0^[i1]]);

  if (aux <= -DBERR) then
  begin
    ckturn := 1;
    exit;
  end;
  if (aux >= DBERR) then
  begin
    ckturn := 0;
    exit;
  end;
  ckturn := 2;
end;

//========================================================================
(*  TRIFIND - find triangle with point ind
             Tr := initial triangle
   Return triangle number or:
           -1 if triangle not found
           -2 if the vertix coincide with exist vertex (!!!SIDE!!!)       *)
//=========================================================================

function TTriangulator2D.TriFind(X, Y: real; Tr: integer; var Option: integer): integer;
var
  Find:  boolean;
  Newtr: integer;
  area, x21, x31, y21, y31: real;
  ind1, ind2: integer;
label
  the_end;

begin
  option := 0;
  Newtr  := tr;
  repeat
    Find := True;
    //       i1:=jnd[0];
    ind1 := vt1^[Newtr];
    ind2 := vt2^[Newtr];
    x31  := x - px^[ind1];
    y31  := y - py^[ind1];
    x21  := px^[ind2] - px^[ind1];
    y21  := py^[ind2] - py^[ind1];
    area := y31 * x21 - y21 * x31;
    if (area < 0) then
    begin
      Newtr := nt2^[Newtr];
      if (Newtr = -1) then
      begin
        TriFind := Newtr;
        Exit;
      end;
      Find := False;
      goto the_end;
    end;
    if (area = 0) then
      if ((x31 = 0) and (y31 = 0)) then
      begin
        Option  := -2;
        TriFind := NewTr;
        Exit;
      end;
    {---------------------------}
    //       i1:=jnd[1];
    ind1 := vt2^[Newtr];
    ind2 := vt3^[Newtr];
    x31  := x - px^[ind1];
    y31  := y - py^[ind1];
    x21  := px^[ind2] - px^[ind1];
    y21  := py^[ind2] - py^[ind1];
    area := y31 * x21 - y21 * x31;
    if (area < 0) then
    begin
      Newtr := nt3^[Newtr];
      if (Newtr = -1) then
      begin
        TriFind := Newtr;
        Exit;
      end;
      Find := False;
      goto the_end;
    end;
    if (area = 0) then
      if ((x31 = 0) and (y31 = 0)) then
      begin
        Option  := -2;
        TriFind := NewTr;
        Exit;
      end;
    {--------------------------}
    //       i1:=jnd[2];
    ind1 := vt3^[Newtr];
    ind2 := vt1^[Newtr];
    x31  := x - px^[ind1];
    y31  := y - py^[ind1];
    x21  := px^[ind2] - px^[ind1];
    y21  := py^[ind2] - py^[ind1];
    area := y31 * x21 - y21 * x31;
    if (area < 0) then
    begin
      Newtr := nt1^[Newtr];
      if (Newtr = -1) then
      begin
        TriFind := Newtr;
        Exit;
      end;
      Find := False;
      goto the_end;
    end;
    if (area = 0) then
      if ((x31 = 0) and (y31 = 0)) then
      begin
        Option  := -2;
        TriFind := NewTr;
        Exit;
      end;
    the_end: ;
  until Find;
  TriFind := Newtr;
end;

 (*========================================================================
  CKLOP - Check if four points satisfy to local optimization,
          comparing diagonal sizes of quadrilaterals - current and

          CkLop:=1 if satisfied        ( not swap diagonal  )
          CkLop:=0 if not satisftied      ( swap diagonal  )
 ========================================================================*)

function TTriangulator2D.CkLop(i1, i2, i3, i4: integer): integer;
var
  x2sq, a, b, c, d, e, f, xc, yc, rsq: double;
begin
  a     := 2 * (px^[i2] - px^[i1]);
  b     := 2 * (py^[i2] - py^[i1]);
  c     := sqr(py^[i2]) - sqr(py^[i1]) + sqr(px^[i2]) - sqr(px^[i1]);
  d     := 2 * (px^[i3] - px^[i2]);
  e     := 2 * (py^[i3] - py^[i2]);
  f     := sqr(py^[i3]) - sqr(py^[i2]) + sqr(px^[i3]) - sqr(px^[i2]);
  x2sq  := d * b - e * a;
  rsq   := sqr(x2sq);
  CkLop := 0;
  if (rsq > 0) then
  begin
    xc  := (f * b - c * e) / x2sq;
    yc  := (d * c - f * a) / x2sq;
    rsq := (sqr(xc - px^[i1]) + sqr(yc - py^[i1]));
    if ((sqr(py^[i4] - yc) + sqr(px^[i4] - xc)) > rsq) then
    begin
      CkLop := 1;
    end;
  end;
end;

//=========================================================
//  SwapTr - Swaps the diagonals  of a quadrilater
//=========================================================

procedure TTriangulator2D.SwapTr(t1, t2, k1, k2: integer);
var
  ind1: integer;
  ind2: integer;
  save: integer;
begin
  if jrd[k1] = 0 then
  begin
    if jnd[k2] = 0 then
      vt1^[t1] := vt1^[t2];
    if jnd[k2] = 1 then
      vt1^[t1] := vt2^[t2];
    if jnd[k2] = 2 then
      vt1^[t1] := vt3^[t2];
  end;
  if jrd[k1] = 1 then
  begin
    if jnd[k2] = 0 then
      vt2^[t1] := vt1^[t2];
    if jnd[k2] = 1 then
      vt2^[t1] := vt2^[t2];
    if jnd[k2] = 2 then
      vt2^[t1] := vt3^[t2];
  end;
  if jrd[k1] = 2 then
  begin
    if jnd[k2] = 0 then
      vt3^[t1] := vt1^[t2];
    if jnd[k2] = 1 then
      vt3^[t1] := vt2^[t2];
    if jnd[k2] = 2 then
      vt3^[t1] := vt3^[t2];
  end;
  {vt^[jrd[k1]][t1]:=vt^[jnd[k2]][t2];}
  if k2 = 0 then
  begin
    if k1 = 0 then
      vt1^[t2] := vt1^[t1];
    if k1 = 1 then
      vt1^[t2] := vt2^[t1];
    if k1 = 2 then
      vt1^[t2] := vt3^[t1];
  end;
  if k2 = 1 then
  begin
    if k1 = 0 then
      vt2^[t2] := vt1^[t1];
    if k1 = 1 then
      vt2^[t2] := vt2^[t1];
    if k1 = 2 then
      vt2^[t2] := vt3^[t1];
  end;
  if k2 = 2 then
  begin
    if k1 = 0 then
      vt3^[t2] := vt1^[t1];
    if k1 = 1 then
      vt3^[t2] := vt2^[t1];
    if k1 = 2 then
      vt3^[t2] := vt3^[t1];
  end;
  {vt^[k2][t2]:=vt^[k1][t1];}
  if k1 = 0 then
  begin
    save     := nt1^[t1];
    nt1^[t1] := t2;
  end;
  if k1 = 1 then
  begin
    save     := nt2^[t1];
    nt2^[t1] := t2;
  end;
  if k1 = 2 then
  begin
    save     := nt3^[t1];
    nt3^[t1] := t2;
  end;
 (* save:=nt^[k1][t1]; nt^[k1][t1]:=t2;*)
  if jrd[k1] = 0 then
  begin
    if jnd[k2] = 0 then
      nt1^[t1] := nt1^[t2];
    if jnd[k2] = 1 then
      nt1^[t1] := nt2^[t2];
    if jnd[k2] = 2 then
      nt1^[t1] := nt3^[t2];
  end;
  if jrd[k1] = 1 then
  begin
    if jnd[k2] = 0 then
      nt2^[t1] := nt1^[t2];
    if jnd[k2] = 1 then
      nt2^[t1] := nt2^[t2];
    if jnd[k2] = 2 then
      nt2^[t1] := nt3^[t2];
  end;
  if jrd[k1] = 2 then
  begin
    if jnd[k2] = 0 then
      nt3^[t1] := nt1^[t2];
    if jnd[k2] = 1 then
      nt3^[t1] := nt2^[t2];
    if jnd[k2] = 2 then
      nt3^[t1] := nt3^[t2];
  end;
  (* nt^[jrd[k1]][t1]:=nt^[jnd[k2]][t2]; *)
  if jnd[k2] = 0 then
    nt1^[t2] := t1;
  if jnd[k2] = 1 then
    nt2^[t2] := t1;
  if jnd[k2] = 2 then
    nt3^[t2] := t1;
  {nt^[jnd[k2]][t2]:=t1;}
  if k2 = 0 then
    nt1^[t2] := save;
  if k2 = 1 then
    nt2^[t2] := save;
  if k2 = 2 then
    nt3^[t2] := save;
  (* nt^[k2][t2]:=save; *)
  if jrd[k1] = 0 then
    ind1 := nt1^[t1];
  if jrd[k1] = 1 then
    ind1 := nt2^[t1];
  if jrd[k1] = 2 then
    ind1 := nt3^[t1];
  (* ind1:=nt^[jrd[k1]][t1]; *)
  if k2 = 0 then
    ind2 := nt1^[t2];
  if k2 = 1 then
    ind2 := nt2^[t2];
  if k2 = 2 then
    ind2 := nt3^[t2];
  (* ind2:=nt^[k2][t2]; *)

  if ((ind1 <> -1) and (nt1^[ind1] = t2)) then
    nt1^[ind1] := t1;
  if ((ind2 <> -1) and (nt1^[ind2] = t1)) then
    nt1^[ind2] := t2;
  if ((ind1 <> -1) and (nt2^[ind1] = t2)) then
    nt2^[ind1] := t1;
  if ((ind2 <> -1) and (nt2^[ind2] = t1)) then
    nt2^[ind2] := t2;
  if ((ind1 <> -1) and (nt3^[ind1] = t2)) then
    nt3^[ind1] := t1;
  if ((ind2 <> -1) and (nt3^[ind2] = t1)) then
    nt3^[ind2] := t2;

end;

(* ==============================================
 OPTIM - Local optimization of two triangles
        it -pointer to triangle
        k1 = 2 on a random tesselation
           = 0 on triangulation
 ==============================================*)

procedure TTriangulator2D.Optim(it, k1: integer);
var
  i, i1, k2, k3: integer;
  a, b, c, d: integer;
  near_: integer;
label
  further;
begin
  near_ := -1;
  a     := 0;
  b     := 0;
  c     := 0;
  d     := 0;
  if (loptim >= MAXOPT) then
    exit;
  k2 := jnd[k1];
  k3 := jrd[k1];
  if k3 = 0 then
    near_ := nt1^[it];
  if k3 = 1 then
    near_ := nt2^[it];
  if k3 = 2 then
    near_ := nt3^[it];
  if (near_ = -1) then
    exit;
  Inc(loptim);
  if nt1^[near_] = it then
  begin
    i := 0;
    goto further;
  end;
  if nt2^[near_] = it then
  begin
    i := 1;
    goto further;
  end;
  if nt3^[near_] = it then
  begin
    i := 2;
    goto further;
  end;
  exit; // sometimes there is a bug in optim !

  further:
    i1 := jnd[i];
  if k3 = 0 then
    a := vt1^[it];
  if k3 = 1 then
    a := vt2^[it];
  if k3 = 2 then
    a := vt3^[it];
  if k1 = 0 then
    b := vt1^[it];
  if k1 = 1 then
    b := vt2^[it];
  if k1 = 2 then
    b := vt3^[it];
  if k2 = 0 then
    c := vt1^[it];
  if k2 = 1 then
    c := vt2^[it];
  if k2 = 2 then
    c := vt3^[it];
  if i1 = 0 then
    d := vt1^[near_];
  if i1 = 1 then
    d := vt2^[near_];
  if i1 = 2 then
    d := vt3^[near_];

  if (CkLop(a, b, c, d) = 0) then
  begin
    SwapTr(it, near_, k1, i);
    Optim(it, k1);
    Optim(near_, i);
  end;
  Dec(loptim);
  Exit;
end;

{=======================================================================}

procedure TTriangulator2D.Delaunay2D;

var
  nptf, nptfs, nptfi: integer; // Hull vertices (tot, up, low)
  i, k, l, n1, j: integer; // Auxiliary variables
  it:    integer; // Pointer to triangle
  nrej1: integer; // Number of rejected points
  nrej2: integer;

  strip:    real; // External frame fraction
  DxStrip, DyStrip: real; // Dimensions of external frame
  px1, px2, py1, py2, aux: real; // Auxiliary variables
  m, q, x0: real; // Params straight line

  nx, ny, k1, k2: integer;
  it1, ntrk, knd, krd: integer;
  ind: integer;

  pt, option: integer;
  s: string[20];

begin
  { //test array for debug
    px^[0]:=110;  px^[1]:=105;  px^[2]:=100;  px^[3]:=055;
    px^[4]:=205;  px^[5]:=165;  px^[6]:=140;  px^[7]:=065;
    px^[8]:=200;  px^[9]:=115;  px^[10]:=0;   px^[11]:=0;
    px^[12]:=0;   px^[13]:=0;   px^[14]:=0;   px^[15]:=0;
    px^[16]:=0;   px^[17]:=0;   px^[18]:=0;   px^[19]:=0;

    py^[0]:=170;  py^[1]:=130;  py^[2]:=095;  py^[3]:=090;
    py^[4]:=085;  py^[5]:=110;  py^[6]:=090;  py^[7]:=135;
    py^[8]:=130;  py^[9]:=050;  py^[10]:=0;   py^[11]:=0;
    py^[12]:=0;   py^[13]:=0;   py^[14]:=0;   py^[15]:=0;
    py^[16]:=0;   py^[17]:=0;   py^[18]:=0;   py^[19]:=0;

    npts:=10;(*{

    k:=5;
    l:=3;
    npts:=(k)*(l);

    for i:=0 to 2*k*l do
      begin
        Px^[i]:=0;
        Py^[i]:=0;
      end;
    for j:=0 to k-1 do
      for i:=0 to l-1 do
      begin
        Px^[j*l+i]:=35 - (J mod 2)*0*25 + I * 50;
        Py^[j*l+i]:=30 + J * 40;
      end;
  {}(** )

    Px^[5]:=Px^[2];
    npts:=30;{}
    k:=3;
    J:=100;
    for i:=0 to npts-1 do
    begin
      if ((i div (k)) mod 2) =1 then Px^[i]:=J+25 + (i mod (k))* 50+random(round(10))
                                else Px^[i]:=J    + (i mod (k))* 50+random(round(10));
      Py^[i]:=10 + (i div (k)) * 40+random(round(10));
    end;
    {}(**)
  Xmin := px^[0];
  Xmax := px^[0];
  Ymin := py^[0];
  Ymax := py^[0];
  Zmin := pz^[0];
  Zmax := pz^[0];
  for I := 2 to npts do
  begin
    if Xmin > px^[i - 1] then
      Xmin := px^[i - 1];
    if Xmax < px^[i - 1] then
      Xmax := px^[i - 1];
    if Ymin > py^[i - 1] then
      Ymin := py^[i - 1];
    if Ymax < py^[i - 1] then
      Ymax := py^[i - 1];
    if Zmin > pz^[i - 1] then
      Zmin := pz^[i - 1];
    if Zmax < pz^[i - 1] then
      Zmax := pz^[i - 1];
  end;

  // ====== Computes external frame ======
  strip   := 0.05;
  DxStrip := strip * (Xmax - Xmin);
  DyStrip := strip * (Ymax - Ymin);
  px1     := Xmin - DxStrip;
  py1     := Ymin - DyStrip;
  n1      := 1 + round(Sqrt(sqrt(npts))); //john
  px2     := (Xmax - Xmin + 2 * DxStrip) / n1;
  py2     := (Ymax - Ymin + 2 * DyStrip) / n1;
  for i := 0 to npts - 1 do
  begin
    nx := 1 + round((px^[i] - px1) / px2);
    ny := 1 + round((py^[i] - py1) / py2);
    k1 := (ny mod 2);
    if k1 = 0 then
      k1 := 1
    else
      k1 := -1;
    k2 := (1 - k1) div 2;
    vt1^[i] := n1 * ny - k1 * nx - k2 * (n1 + 1) + 1;
    vt2^[i] := i;
    vt3^[i] := i;
  end;
  // Sort by bin number and increasing abscissa \\
  for i := 0 to npts - 1 do
  begin
    for j := i + 1 to npts - 1 do
    begin
      if (px^[vt3^[j]] < px^[vt3^[i]]) then
        //**** Swaps two memory locations by abscissa ***\\
      begin
        pt      := vt3^[i];
        vt3^[i] := vt3^[j];
        vt3^[j] := pt;
      end;
      if (vt1^[vt2^[j]] < vt1^[vt2^[i]]) then
      begin
        //***  Swaps two memory locations by bin ***\\
        pt      := vt2^[i];
        vt2^[i] := vt2^[j];
        vt2^[j] := pt;
      end;
    end;
  end;

  //====== Creates the convex hull =====\\
  x0    := px^[vt3^[0]];
  q     := py^[vt3^[0]];
  m     := (py^[vt3^[npts - 1]] - q) / (px^[vt3^[npts - 1]] - x0);
  nptfi := 1;
  nptfs := npts;
  nt1^[0] := vt3^[0];
  for i := 1 to npts - 1 do
  begin
    ind := vt3^[i];
    if ((py^[ind] - q) >= m * (px^[ind] - x0)) then
    begin
      Dec(nptfs);
      nt1^[nptfs] := ind;
    end
    else
    begin
      nt1^[nptfi] := ind;
      Inc(nptfi);
    end;
  end;
  nptf := 1;
  for i := 2 to npts - 1 do
  begin
    while ((ckturn(nt1, nptf, i) > 0) and (nptf >= 1)) do
      Dec(nptf);
    Inc(nptf);
    nt1^[nptf] := nt1^[i];
  end;

  while ((ckturn(nt1, nptf, 0) > 0) and (nptf >= 1)) do
    Dec(nptf);

  //* Builds the ordered list of vertices *\\
  SetLength(ConvexHull, nptf + 2);
  for i := 0 to nptf do
  begin
    ind     := nt1^[i];
    ConvexHull[i] := ind; //Create Convex Hull indices
    dcd^[i] := ind;
    vt1^[ind] := 0;
  end;
  ConvexHull[High(ConvexHull)] := ConvexHull[0]; //Close the hull

  k := nptf;
  for i := 0 to npts - 1 do
  begin
    ind := vt2^[i];
    if (vt1^[ind] <> 0) then
    begin
      Inc(k);
      dcd^[k] := ind;
    end;
  end;

  (* Builds first triangles *)
  Ntri := nptf - 2;
  for i := 0 to Ntri do
  begin
    vt1^[i] := dcd^[0];
    vt2^[i] := dcd^[i + 1];
    vt3^[i] := dcd^[i + 2];
    nt1^[i] := i + 1;
    nt2^[i] := i - 1;
    nt3^[i] := -1;
    optim(i, 2);
  end;
  //  DrawTriangles;ReadKey;
  // Beginning of triangulation \\
  nt1^[Ntri] := -1;
  it    := 0;
  nrej1 := 0;
  nrej2 := 0;
  for i := nptf + 1 to npts - 1 do
  begin
    n1  := dcd^[i];
    it1 := TriFind(px^[n1], py^[n1], it, option);
    if (it1 >= 0) and (option = 0) then
    begin
      it     := it1;
      {New triangles}
      ntr[0] := it;
      ntr[1] := Ntri + 1;
      ntr[2] := Ntri + 2;
      vtm[0] := vt1^[it]; { Vertices }
      ntm[0] := nt1^[it]; { Near triangles }
      vtm[1] := vt2^[it];
      ntm[1] := nt2^[it];
      vtm[2] := vt3^[it];
      ntm[2] := nt3^[it];
      for k := 0 to 2 do
      begin
        ntrk := ntr[k];
        knd  := jnd[k];
        krd  := jrd[k];
        vt1^[ntrk] := n1;
        vt2^[ntrk] := vtm[k];
        vt3^[ntrk] := vtm[knd];
        nt1^[ntrk] := ntr[knd];
        nt2^[ntrk] := ntr[krd];
        nt3^[ntrk] := ntm[knd];
      end;
      for k := 0 to 2 do
      begin
        ind := nt3^[ntr[k]];
        if (ind >= 0) then
        begin
          if (nt1^[Ind] = ntr[0]) then
            nt1^[Ind] := ntr[K];
          if (nt2^[Ind] = ntr[0]) then
            nt2^[Ind] := ntr[K];
          if (nt3^[Ind] = ntr[0]) then
            nt3^[Ind] := ntr[K];
        end;
      end;
      Inc(Ntri, 2);
      //      DrawTriangles;ReadKey; //To debug
      Optim(ntr[0], 0);
      //      DrawTriangles;ReadKey;
      Optim(ntr[1], 0);
      //      DrawTriangles;ReadKey;
      Optim(ntr[2], 0);
      //      DrawTriangles;ReadKey;
    end
    else
    begin
      Inc(nrej1);
      if (it1 = -2) then
        Inc(nrej2);
    end;
  end;
  (*
  Str(Nrej2,S);
  OutTextXY(0,0,'Совпавших точек: ' + s);
  Str(Nrej1,S);
  OutTextXY(0,20,'Отброшеннных точек: ' + s);
  Str(Ntri,s);
  OutTextXY(0,40,'ТNumber of triangles: ' + S);
  *)

end;

function TTriangulator2D.GetNumNextTriCW(Nv, Nt: integer): integer;
begin
  if Nv = vt1^[Nt] then
    Result := nt3^[Nt]
  else if Nv = vt2^[Nt] then
    Result := nt1^[Nt]
  else if Nv = vt3^[Nt] then
    Result := nt2^[Nt]
  else
  begin
    Result := -1;
    // Writeln('Error GetNumPrevTriCW');
  end;
  GetNumNextTriCW := Result;
end;

function TTriangulator2D.GetNumPrevTriCW(Nv, Nt: integer): integer;
begin
  if Nv = vt1^[Nt] then
    Result := nt1^[Nt]
  else if Nv = vt2^[Nt] then
    Result := nt2^[Nt]
  else if Nv = vt3^[Nt] then
    Result := nt3^[Nt]
  else
  begin
    Result := -1;
    //      Writeln('Error GetNumPrevTriCW');
  end;
  GetNumPrevTriCW := Result;
end;

function TTriangulator2D.GetNumFirstTriCW(Nv, Nt: integer): integer;
var
  FromNt: integer;
  PrevNt: integer;
begin
  FromNt := Nt;
  Result := Nt;
  repeat
    PrevNt := Result;
    Result := Nt;
    Nt     := GetNumPrevTriCW(Nv, Nt);
    if Nt = PrevNt then
    begin
      Result := FromNt; //ERROR: endless loop on two triangles
      Nt     := -1;
    end;
  until ((Nt < 0) or (Nt = FromNt)); //Found first or made turn
  GetNumFirstTriCW := Result;
end;

procedure TTriangulator2D.SortNtCW;

(*sub*) procedure SwapLL(var A, B: integer);
  var
    T: integer;
  begin //SwapLL
    T := A;
    A := B;
    B := T;
  end; //SwapLL

var
  I, nt, vt1I, vt2I, vt3I: integer;

  //  snt: array [0..2,0..200] of Integer;{~~~for trace~~~}

begin //SortNtCW
  (*
    for i:=0 to Ntri do               {---for trace---}
    begin                             {---for trace~~~}
      snt[0][I]:=nt1^[I]+1;           {---for trace~~~}
      snt[1][I]:=nt2^[I]+1;           {---for trace~~~}
      snt[2][I]:=nt3^[I]+1;           {---for trace~~~}
    end;                              {~~~for trace~~~}(**)
  for I := 0 to Ntri do
  begin
    vt1I := vt1^[I];
    vt2I := vt2^[I];
    vt3I := vt3^[I];

    nt := 4 + 2 + 1; //111b //Finds number of Nt1

    if (nt1^[I] <> -1) then
      if (vt3I <> vt1^[nt1^[I]]) and (vt3I <> vt2^[nt1^[I]]) and
        (vt3I <> vt3^[nt1^[I]]) then
        nt := 1 //nt:=001b
      else
        nt := nt xor 1; //nt:=110b
    if (nt <> 1) then
    begin
      if (nt2^[I] <> -1) then
        if (vt3I <> vt1^[nt2^[I]]) and (vt3I <> vt2^[nt2^[I]]) and
          (vt3I <> vt3^[nt2^[I]]) then
          nt := 2 //nt:=010b
        else
          nt := nt xor 2; //nt:=10?b
      if (nt <> 2) then
        if (nt3^[I] <> -1) then
          if (vt3I <> vt1^[nt3^[I]]) and (vt3I <> vt2^[nt3^[I]]) and
            (vt3I <> vt3^[nt3^[I]]) then
            nt := 4 //nt:=100b
          else
            nt := nt xor 4; //nt:=0??b
      if ((nt and 1) <> 1) then
        if nt = 0 then
        begin
          //!!! ERROR All near triangles have vt3^[J] !!!
        end
        else if nt = 4 (*100b*) then
          SwapLL(nt1^[I], nt3^[I])
        else
          SwapLL(nt1^[I], nt2^[I]);
    end;
    nt := 2;
    if (nt2^[I] <> -1) and //is nt2 real nt3?
      (vt2I <> vt1^[nt2^[I]]) and (vt2I <> vt2^[nt2^[I]]) and
      (vt2I <> vt3^[nt2^[I]]) then
      nt := 3;
    if (nt3^[I] <> -1) and // is nt3 real nt2?
      (vt1I <> vt1^[nt3^[I]]) and (vt1I <> vt2^[nt3^[I]]) and
      (vt1I <> vt3^[nt3^[I]]) then
      nt := 3;
    //if nt3 is real nt2 then swap(nt2,nt3)
    if (nt = 3) then
      SwapLL(nt2^[I], nt3^[I]);
  end;

end; // SortNtCW

function ConvertSegment3Dto2D(Value: TSegment3D): TSegment2D;
begin
  Result.XStart := Value.XStart;
  Result.YStart := Value.YStart;
  Result.XEnd   := Value.XEnd;
  Result.YEnd   := Value.YEnd;
end;

function TTriangulator2D.isIntersectTwoSegment2D(Segment1, Segment2: TSegment2D): boolean;
var
  dl2x,  // Change in X in line segment 2
  dl2y,  // Change in Y in line segment 2
  dl1x,  // Change in X in line segment 1
  dl1y,  // Change in X in line segment 1
  dl12x, // Change in X between line segments 1 and 2
  dl12y, // Change in Y between line segments 1 and 2
  denom, // Denominator
  parm1, parm2: double; // Misc

begin // IntersectLs
  Result := False;
  dl2x   := Segment2.EndPoint[0] - Segment2.StartPoint[0];
  dl2y   := Segment2.EndPoint[1] - Segment2.StartPoint[1];
  dl1x   := Segment1.EndPoint[0] - Segment1.StartPoint[0];
  dl1y   := Segment1.EndPoint[1] - Segment1.StartPoint[1];
  dl12x  := Segment2.StartPoint[0] - Segment1.StartPoint[0];
  dl12y  := Segment2.StartPoint[1] - Segment1.StartPoint[1];
  denom  := dl2y * dl1x - dl2x * dl1y;
  if (denom <> 0) then
  begin
    parm1  := ((dl2y * dl12x) - (dl2x * dl12y)) / denom;
    parm2  := ((dl1y * dl12x) - (dl1x * dl12y)) / denom;
    Result := (0 < parm1) and (Parm1 < 1.0) and
      //line1 and line2 intersect inner Segment1 and
      (0 < parm2) and (Parm2 < 1.0);
    //line1 and line2 intersect inner Segment2
  end;
end;

function TTriangulator2D.GetEdgeOppositeVertex
  (TriangleNum, VertNum: integer): TSegment2D;
begin
  if VertNum = vt1^[TriangleNum] then
  begin
    Result.XStart := Px^[vt2^[TriangleNum]];
    Result.YStart := Py^[vt2^[TriangleNum]];
    Result.XEnd   := Px^[vt3^[TriangleNum]];
    Result.YEnd   := Py^[vt3^[TriangleNum]];
  end
  else if VertNum = vt2^[TriangleNum] then
  begin
    Result.XStart := Px^[vt3^[TriangleNum]];
    Result.YStart := Py^[vt3^[TriangleNum]];
    Result.XEnd   := Px^[vt1^[TriangleNum]];
    Result.YEnd   := Py^[vt1^[TriangleNum]];
  end
  else
  begin
    Result.XStart := Px^[vt1^[TriangleNum]];
    Result.YStart := Py^[vt1^[TriangleNum]];
    Result.XEnd   := Px^[vt2^[TriangleNum]];
    Result.YEnd   := Py^[vt2^[TriangleNum]];
  end;
end;

function TTriangulator2D.GetNumTriangleOppositeVertex
  (TriangleNum, VertNum: integer): integer;
begin
  if VertNum = vt1^[TriangleNum] then
    Result := nt2^[TriangleNum]
  else if VertNum = vt2^[TriangleNum] then
    Result := nt3^[TriangleNum]
  else
    Result := nt1^[TriangleNum];
end;

function TTriangulator2D.GetIndexNearTriangle
  (Triangle, NearTriangle: integer): shortint;
begin
  Result := -1;
  if nt1^[Triangle] = NearTriangle then
    Result := 0
  else if nt2^[Triangle] = NearTriangle then
    Result := 1
  else if nt3^[Triangle] = NearTriangle then
    Result := 2;
end;

function TTriangulator2D.ConvertVertexToSegment2D(VertNum1, VertNum2:
  integer): TSegment2D;
begin
  Result.XStart := Px^[VertNum1];
  Result.YStart := Py^[VertNum1];
  Result.XEnd   := Px^[VertNum2];
  Result.YEnd   := Py^[VertNum2];
end;

function TTriangulator2D.SwapTriangle(Trian1, Trian2: integer): boolean;
type
  TTriangle = record
    Vert, NearTri: array[1..3] of integer;
  end;
var
  TempTri1, TempTri2: TTriangle;

begin
  case GetIndexNearTriangle(Trian1, Trian2) of
    0:
    begin
      TempTri1.Vert[1]    := vt3^[Trian1];
      TempTri1.Vert[2]    := vt1^[Trian1];
      TempTri1.Vert[3]    := vt2^[Trian1];
      TempTri1.NearTri[1] := nt3^[Trian1];
      TempTri1.NearTri[2] := nt1^[Trian1];
      TempTri1.NearTri[3] := nt2^[Trian1];
    end;
    1:
    begin
      TempTri1.Vert[1]    := vt1^[Trian1];
      TempTri1.Vert[2]    := vt2^[Trian1];
      TempTri1.Vert[3]    := vt3^[Trian1];
      TempTri1.NearTri[1] := nt1^[Trian1];
      TempTri1.NearTri[2] := nt2^[Trian1];
      TempTri1.NearTri[3] := nt3^[Trian1];
    end;
    2:
    begin
      TempTri1.Vert[1]    := vt2^[Trian1];
      TempTri1.Vert[2]    := vt3^[Trian1];
      TempTri1.Vert[3]    := vt1^[Trian1];
      TempTri1.NearTri[1] := nt2^[Trian1];
      TempTri1.NearTri[2] := nt3^[Trian1];
      TempTri1.NearTri[3] := nt1^[Trian1];
    end;
  end;
  case GetIndexNearTriangle(Trian2, Trian1) of
    0:
    begin
      TempTri2.Vert[1]    := vt3^[Trian2];
      TempTri2.Vert[2]    := vt1^[Trian2];
      TempTri2.Vert[3]    := vt2^[Trian2];
      TempTri2.NearTri[1] := nt3^[Trian2];
      TempTri2.NearTri[2] := nt1^[Trian2];
      TempTri2.NearTri[3] := nt2^[Trian2];
    end;
    1:
    begin
      TempTri2.Vert[1]    := vt1^[Trian2];
      TempTri2.Vert[2]    := vt2^[Trian2];
      TempTri2.Vert[3]    := vt3^[Trian2];
      TempTri2.NearTri[1] := nt1^[Trian2];
      TempTri2.NearTri[2] := nt2^[Trian2];
      TempTri2.NearTri[3] := nt3^[Trian2];
    end;
    2:
    begin
      TempTri2.Vert[1]    := vt2^[Trian2];
      TempTri2.Vert[2]    := vt3^[Trian2];
      TempTri2.Vert[3]    := vt1^[Trian2];
      TempTri2.NearTri[1] := nt2^[Trian2];
      TempTri2.NearTri[2] := nt3^[Trian2];
      TempTri2.NearTri[3] := nt1^[Trian2];
    end;
  end;
  if isIntersectTwoSegment2D(ConvertVertexToSegment2D(TempTri1.Vert[1],
    TempTri2.Vert[1]), ConvertVertexToSegment2D(TempTri1.Vert[2],
    TempTri2.Vert[2])) then
  begin
    // TempTri[1|2] vert[1] opposite swap diagonal
    vt1^[Trian1] := TempTri1.Vert[1];
    vt2^[Trian1] := TempTri1.Vert[2];
    vt3^[Trian1] := TempTri2.Vert[1];
    nt1^[Trian1] := TempTri1.NearTri[1];
    nt2^[Trian1] := TempTri2.NearTri[3];
    nt3^[Trian1] := TempTri1.NearTri[2];

    vt1^[Trian2] := TempTri2.Vert[1];
    vt2^[Trian2] := TempTri2.Vert[2];
    vt3^[Trian2] := TempTri1.Vert[1];
    nt1^[Trian2] := TempTri2.NearTri[1];
    nt2^[Trian2] := TempTri1.NearTri[3];
    nt3^[Trian2] := TempTri2.NearTri[2];

    ChangeNearTriangle(TempTri1.NearTri[3], Trian1, Trian2);
    ChangeNearTriangle(TempTri2.NearTri[3], Trian2, Trian1);
    Result := True;
  end
  else
    Result := False;
end;

procedure TTriangulator2D.ChangeNearTriangle(Triangle, OldNearTriangle,
  NewNearTriangle: integer);
begin
  if Triangle <> -1 then
  begin
    if nt1^[Triangle] = OldNearTriangle then
      nt1^[Triangle] := NewNearTriangle
    else if nt2^[Triangle] = OldNearTriangle then
      nt2^[Triangle] := NewNearTriangle
    else if nt3^[Triangle] = OldNearTriangle then
      nt3^[Triangle] := NewNearTriangle;
  end;
end;

(* GetEdge return a Edge type of TSegment2D with
  Index=Index1_3 for the Triangle with number=TriangleNum
  case index of
  1 : Edge=(vt1,vt2);
  2 : Edge=(vt2,vt3);
  3 : Edge=(vt3,vt1);
  end;
*)

function TTriangulator2D.GetEdge(TriangleNum: integer; Index1_3: shortint): TSegment2D;
begin
  case Index1_3 of
    1:
    begin
      Result.XStart := Px^[Vt1^[TriangleNum]];
      Result.YStart := Py^[Vt1^[TriangleNum]];
      Result.XEnd   := Px^[Vt2^[TriangleNum]];
      Result.YEnd   := Py^[Vt2^[TriangleNum]];
    end;
    2:
    begin
      Result.XStart := Px^[Vt2^[TriangleNum]];
      Result.YStart := Py^[Vt2^[TriangleNum]];
      Result.XEnd   := Px^[Vt3^[TriangleNum]];
      Result.YEnd   := Py^[Vt3^[TriangleNum]];
    end;
    3:
    begin
      Result.XStart := Px^[Vt3^[TriangleNum]];
      Result.YStart := Py^[Vt3^[TriangleNum]];
      Result.XEnd   := Px^[Vt1^[TriangleNum]];
      Result.YEnd   := Py^[Vt1^[TriangleNum]];
    end;
  end;
end;

procedure TTriangulator2D.AddBreakSegment(Value: TBreakSegment);

  (*sub*)function GetTriangleForPoint(X, Y: real): integer;
  var
    I: integer;
    PointNum: integer;
  begin
    I      := 0;
    PointNum := -1;
    Result := -1;
    while (I <= Npts) and (PointNum < 0) do
    begin
      if (Px^[I] = X) and (Py^[I] = Y) then
      begin
        PointNum := I;
      end;
      Inc(I);
    end;
    if PointNum > 0 then
    begin
      I := 0;
      while (I <= Ntri) and (Result < 0) do
      begin
        if (Vt1^[I] = PointNum) or (Vt2^[I] = PointNum) or
          (Vt3^[I] = PointNum) then
          Result := I;
        Inc(I);
      end;
    end;
  end;

var
  TriangleNum, NearTriangle: integer;
  VertNum: integer;          //Vertex number in the set of vertices
  StartTriangle: integer;
  TriangleIsFinded: boolean; //True, if finds a triangle that has an edge
  // coincidented with a BreakSegment
  Level:   integer;
begin
  Level := 0;
  repeat
    Inc(Level);
    TriangleIsFinded := False;
    TriangleNum      := GetTriangleForPoint(Value.XStart, Value.YStart);
    if TriangleNum > -1 then
    begin
      // Segment starts at any vertex of triangle with index = TriangleNum 
      VertNum := Vt3^[TriangleNum];

      if (Px^[Vt1^[TriangleNum]] = Value.XStart) and
        (Py^[Vt1^[TriangleNum]] = Value.YStart) then
      begin
        VertNum := Vt1^[TriangleNum];
      end;

      if (Px^[Vt2^[TriangleNum]] = Value.XStart) and
        (Py^[Vt2^[TriangleNum]] = Value.YStart) then
      begin
        VertNum := Vt2^[TriangleNum];
      end;
      // Finds a triangle which is intersected by the BreakSegment 
      TriangleIsFinded := False;
      TriangleNum      := GetNumFirstTriCW(VertNum, TriangleNum);
      StartTriangle    := TriangleNum;
      repeat
        if isIntersectTwoSegment2D(ConvertSegment3Dto2D(Value),
          GetEdgeOppositeVertex(TriangleNum, VertNum)) then
        begin
          TriangleIsFinded := True;
        end
        else
          TriangleNum := GetNumNextTriCW(VertNum, TriangleNum);
      until (TriangleNum = StartTriangle) or (TriangleNum = -1) or
        (TriangleIsFinded);
      if TriangleIsFinded then
      begin
        // Swaps diagonals of a quadrilater 
        NearTriangle := GetNumTriangleOppositeVertex(TriangleNum, VertNum);
        if NearTriangle <> -1 then
        begin
          TriangleIsFinded := False;
          repeat
            if SwapTriangle(TriangleNum, NearTriangle) then
            begin // If convex quadrilater
              TriangleIsFinded := True;
            end
            else //Finds next quadrilater
            begin
              case GetIndexNearTriangle(NearTriangle, TriangleNum) of
                0:
                begin
                  TriangleNum := NearTriangle;
                  if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 2)) then
                  begin
                    NearTriangle := nt2^[NearTriangle];
                  end
                  else if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 3)) then
                  begin
                    NearTriangle := nt3^[NearTriangle];
                  end
                  else
                    NearTriangle := -1;
                end;
                1:
                begin
                  TriangleNum := NearTriangle;
                  if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 1)) then
                  begin
                    NearTriangle := nt1^[NearTriangle];
                  end
                  else if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 3)) then
                  begin
                    NearTriangle := nt3^[NearTriangle];
                  end
                  else
                    NearTriangle := -1;
                end;
                2:
                begin
                  TriangleNum := NearTriangle;
                  if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 1)) then
                  begin
                    NearTriangle := nt1^[NearTriangle];
                  end
                  else if isIntersectTwoSegment2D(
                    ConvertSegment3Dto2D(Value),
                    GetEdge(NearTriangle, 2)) then
                  begin
                    NearTriangle := nt2^[NearTriangle];
                  end
                  else
                    NearTriangle := -1;
                end;
              end;
            end;
          until (TriangleIsFinded) or (NearTriangle < 0);
        end
        else
          TriangleIsFinded := False;
      end;
    end;
  until (not TriangleIsFinded) or (Level > 100);
end;

function TTriangulator2D.GetVertNum(X, Y: real): integer;
begin
  Result := 0;
  while (Result <= Npts) and ((Px^[Result] <> X) and (Py^[Result] <> Y)) do
    Inc(Result);
  if Result > Npts then
    Result := -1;
end;

//------------------------------------------------------------
procedure TTriangulator2D.Voronoi2D;

var
  I, J, K: longint;
  vtNum:  byte;
  KOut, PrevX, PrevY: real;
  Center: TPoint3d;
  NPoly, Nt, Option: longint;
  p1, p2, p3, mp1, mp2, Intp, intp1, pc: TPoint2D;
  l1, l2, ile1, ile2: ImpLineEq;
  ple, ple1, ple2: ParaLineEq;  // not use
  ls1, ls2: LineSeg2D;          // not use
  IntExists: boolean;
  VorFile: file;
  S:      string;
  OutOff: boolean;

const
  ViewMinY = 0;
  ViewMaxY = 400;
  ViewMinX = 0;
  ViewMaxX = 400;

  (*sub*) function FindTri(x, y: real; tr: longint; var option: longint): longint;
  var
    //  ii:byte;
    aux, Newtr:     longint;
    area, x21, x31, y21, y31: real;
    i1, ind1, ind2: longint;

  label
    the_end;

  begin
    Option := 0;
    Newtr  := tr;
    repeat
      aux  := 0;
      i1   := jnd[0];
      ind1 := vt1^[Newtr];
      ind2 := vt2^[Newtr];
      x31  := x - px^[ind1];
      y31  := y - py^[ind1];
      x21  := px^[ind2] - px^[ind1];
      y21  := py^[ind2] - py^[ind1];
      Area := y31 * x21 - y21 * x31;
      if (Area < 0) then
      begin
        Newtr := nt1^[Newtr];
        if (Newtr = -1) then
        begin
          FindTri := Newtr;
          Exit;
        end;
        aux := 1;
        goto the_end;
      end;
      if (area = 0) then
        if ((x31 = 0) and (y31 = 0)) then
        begin
          Option  := -2;
          FindTri := NewTr;
          Exit;
        end;
      i1   := jnd[1];
      ind1 := vt2^[Newtr];
      ind2 := vt3^[Newtr];
      x31  := x - px^[ind1];
      y31  := y - py^[ind1];
      x21  := px^[ind2] - px^[ind1];
      y21  := py^[ind2] - py^[ind1];
      area := y31 * x21 - y21 * x31;
      if (area < 0) then
      begin
        Newtr := nt2^[Newtr];
        if (Newtr = -1) then
        begin
          FindTri := Newtr;
          Exit;
        end;
        aux := 1;
        goto the_end;
      end;
      if (area = 0) then
        if ((x31 = 0) and (y31 = 0)) then
        begin
          Option  := -2;
          FindTri := NewTr;
          Exit;
        end;
      i1   := jnd[2];
      ind1 := vt3^[Newtr];
      ind2 := vt1^[Newtr];
      x31  := x - px^[ind1];
      y31  := y - py^[ind1];
      x21  := px^[ind2] - px^[ind1];
      y21  := py^[ind2] - py^[ind1];
      area := y31 * x21 - y21 * x31;
      if (area < 0) then
      begin
        Newtr := nt3^[Newtr];
        if (Newtr = -1) then
        begin
          FindTri := Newtr;
          Exit;
        end;
        aux := 1;
        goto the_end;
      end;
      if (area = 0) then
        if ((x31 = 0) and (y31 = 0)) then
        begin
          Option  := -2;
          FindTri := NewTr;
          Exit;
        end;
      the_end: ;
    until aux <> 1;
    FindTri := Newtr;
  end;

begin
  SortNtCW;
  //  Nt:=1; PrevX:=-1000; PrevY:=-1000;
  SetLength(VorDiagram, Npts);
  for I := 0 to Npts - 1 do
  begin
    SetLength(TriVertices, 1);
    K := 0;
    for J := 0 to Ntri do // Finds all triangles with I point
    begin
      if (vt1^[J] = I) or (vt2^[J] = I) or (vt3^[J] = I) then
      begin
        Inc(K);
        TriVertices[0] := J;
      end;
    end;

    SetLength(VorDiagram[I], K + 1);
    VorDiagram[I][0].V[0] := Px^[I];
    VorDiagram[I][0].V[1] := Py^[I];
    VorDiagram[I][0].V[2] := Pz^[I];
    //Writeln(VorFile,'P',I+1,',',round(Px^[I]),',',round(Py^[I]),',',round(Pz^[I]));

    nt := GetNumFirstTriCW(I, TriVertices[0]);
    J  := 1;
    while J <= K do
    begin
      if nt < 0 then
        Break;
      p1.x := Px^[vt1^[nt]];
      p1.y := Py^[vt1^[nt]];
      p2.x := Px^[vt2^[nt]];
      p2.y := Py^[vt2^[nt]];
      p3.x := Px^[vt3^[nt]];
      p3.y := Py^[vt3^[nt]];
      pc.x := ((p1.x + p2.x + p3.x) / 3);
      pc.y := ((p1.y + p2.y + p3.y) / 3);

      vtNum := 0;
      if (vt1^[nt] = I) then
        vtNum := 1
      else if (vt2^[nt] = I) then
        vtNum := 2
      else if (vt3^[nt] = I) then
        vtNum := 3;
      case vtNum of
        0:
        begin
          //  Writeln ('Error in voronoi I=',I,',Nt=',nt,' Vertix I not found in nt');
        end;
        1:
        begin
          MidPoint(p1, p2, mp1);
          LineEquation(0, p1, p2, l1, ple);
          MidPoint(p1, p3, mp2);
          LineEquation(0, p1, p3, l2, ple);
        end;
        2:
        begin
          MidPoint(p2, p1, mp1);
          LineEquation(0, p2, p1, l1, ple);
          MidPoint(p2, p3, mp2);
          LineEquation(0, p2, p3, l2, ple);
        end;
        3:
        begin
          MidPoint(p3, p1, mp1);
          LineEquation(0, p3, p1, l1, ple);
          MidPoint(p3, p2, mp2);
          LineEquation(0, p3, p2, l2, ple);
        end;
      end;
      LinePntPerpend(l1, mp1, ile1);
      LinePntPerpend(l2, mp2, ile2);
      Intersect(0, ile1, ile2, ple1, ple2, ls1, ls2, intp, IntExists);
      if IntExists then
      begin
        //  if Abs(Intp.x)>10000 then Intp.x:=Sign(Intp.x)*10000;   //Sign
        //  if Abs(Intp.y)>10000 then Intp.y:=Sign(Intp.y)*10000;   //Sign
        if (FindTri(Intp.x, Intp.y, nt, Option) = -1) or (Option < -2) then
          KOut := -1000
        else
          KOut := 1000;

        if GetNumPrevTriCW(I, Nt) < 0 then {First triangle}
        begin
          Inc(K, 2);
          SetLength(VorDiagram[I], K + 1);
          case VtNum of
            1:
              MidPoint(p1, p2, mp1);
            2:
              MidPoint(p2, p3, mp1);
            3:
              MidPoint(p3, p1, mp1);
          end;
          try
            KOut := KOut / Sqrt((mp1.x - intp.x) * (mp1.x - intp.x) +
              (mp1.y - intp.y) * (mp1.y - intp.y));
          except
          end;

          mp2.x := intp.x + KOut * (mp1.x - intp.x);
          mp2.y := intp.y + KOut * (mp1.y - intp.y);

          VorDiagram[I][J].V[0] := mp2.X;
          VorDiagram[I][J].V[1] := mp2.Y;
          VorDiagram[I][J].V[2] := VorDiagram[I][0].V[2];
          //May be it should be Z of the triangle center
          Inc(J);
          //Writeln(VorFile, mp2.X:1:0,',',mp2.Y:1:0);{}
        end; {First triangle}

        (*      VorDiagram[I,J].X:=pc.X;
                VorDiagram[I,J].Y:=pc.Y;     *)
        VorDiagram[I][J].V[0] := Intp.X;
        VorDiagram[I][J].V[1] := Intp.Y;
        VorDiagram[I][J].V[2] := VorDiagram[I][0].V[2];
        //May be it should be Z of the triangle center
        Inc(J);
        //Writeln(VorFile, Intp.X:1:0,',',Intp.Y:1:0);
        //PrevX:=Intp.X; PrevY:=Intp.Y;

        if GetNumNextTriCW(I, Nt) < 0 then //Last triangle
        begin
          case VtNum of
            1: MidPoint(p3, p1, mp1);
            2: MidPoint(p1, p2, mp1);
            3: MidPoint(p2, p3, mp1);
          end;
          try
            KOut := KOut / Sqrt((mp1.x - intp.x) * (mp1.x - intp.x) +
              (mp1.y - intp.y) * (mp1.y - intp.y));
          except
          end;
          mp2.x := intp.x + KOut * (mp1.x - intp.x);
          mp2.y := intp.y + KOut * (mp1.y - intp.y);

          VorDiagram[I][J].V[0] := mp2.X;
          VorDiagram[I][J].V[1] := mp2.Y;
          VorDiagram[I][J].V[2] := VorDiagram[I][0].V[2];
          //May be it should be Z of the triangle center
          Inc(J);
          //Writeln(VorFile, mp2.X:1:0,',',mp2.Y:1:0);{}
          //PrevX:=Intp.X; PrevY:=Intp.Y;
        end; // Last triangle
      end
      else
      begin
        (*
        Writeln(VorFile,'{'#9'Bad triangle}');
        Writeln(VorFile,'{'#9'I=',I,',nt=',nt,'}');
        Writeln(VorFile,'{',#13,                     #13,

                        '            (',vt2^[nt]:3,') (',Px^[vt2^[nt]]:5:1,',',Py^[vt2^[nt]]:5:1,')',#13,
                        '             / \                ',#13,
                        '            /   \               ',#13,
                        '        ',nt1^[nt]:3,'/     \',nt2^[nt]:3,#13,
                        '          / <',nt:3,'> \             ',#13,
                        '         /         \            ',#13,
                        '      (',vt1^[nt]:3,')------(',vt3^[nt]:3,') (',Px^[vt3^[nt]]:5:1,',',Py^[vt3^[nt]]:5:1,') ',#13,
                        '             ',nt3^[nt]:3,#13,
                        '      (',Px^[vt1^[nt]]:5:1,',',Py^[vt1^[nt]]:5:1,')                   ',#13,

                        #13,
                        '}');
        *)
        Inc(J);
      end;
      Nt := GetNumNextTriCW(I, Nt);
    end; { for J }
         //Writeln(VorFile,'*');
  end;
end;

end.
