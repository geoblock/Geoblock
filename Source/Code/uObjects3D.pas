// -------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
// -------------------------------------------------------------------------
{
  The unit for basic classes to create 3D Voronoi diagrams
  Description: Represents basic classes in 3d; it manages its own memory.
  TGBTriangle class is only used to draw isosurfaces in the application.
  Note: TGBTriangle is creating new TGBPoint3D because the point are not
  permanent (they are not part of the mesh, only intermediate
  points in a tetra, e.g. for faces of an isosurface).
}

unit uObjects3D;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Contnrs,
  System.Math;

type
  TGBPoint3D = class(TObject)
  private
    m_x: Single;
    m_y: Single;
    m_z: Single;
    m_data: Single;
    m_flag: Boolean; // -- to draw some points in different colours
    procedure SetX(x: Single);
    procedure SetY(y: Single);
    procedure SetZ(z: Single);
    procedure SetData(Data: Single);
    procedure SetFlag(b: Boolean);
  public
    m_no: integer;
    constructor Create(x: Single = 0.0; y: Single = 0.0; z: Single = 0.0;
      Data: Single = 0.0); overload;
    constructor Create(flag: Boolean; x: Single = 0.0; y: Single = 0.0;
      z: Single = 0.0; Data: Single = 0.0); overload;
    procedure SetCoord(x, y, z: single);
    function GetCoord: TGBPoint3D;
    procedure CopyValue(tempt: TGBPoint3D);
    (*
     Description: Determine if a point if too close (according to a certain
     tolerance) from the current point (self)
     Input:       pt : the point to test
     tolerance : the distance in 3D
     Output:      TRUE  -> distance self-pt is shorter or equal to the tolerance
     FALSE -> distance is longer than the tolerance
    *)
    function Hit(const pt: TGBPoint3D; tolerance: single): boolean;
  published
    property X: Single read m_x write SetX;
    property Y: Single read m_y write SetY;
    property Z: Single read m_z write SetZ;
    property Data: Single read m_data write SetData;
    property Flag: Boolean read m_flag write SetFlag;
  end;

type
  TGBVector = class(TObject)
  private
    m_coord: TGBPoint3D;
    procedure SetCoord(const Coord: TGBPoint3D);
  public
    constructor Create; overload;
    constructor Create(dest: TGBPoint3D); overload;
    constructor Create(destX, destY, destZ: double); overload;
  (*
   Input:       orig and dest of the vector
   Remarks:     only the length and the direction is preserved, so the orig of
   every edge stored is (0, 0, 0)
  *)
    constructor Create(orig, dest: TGBPoint3D); overload;
    destructor Destroy; override;
    procedure Add(const a: TGBVector);
    procedure Subtract(const a: TGBVector);
    procedure Multiply(a: double);
    function GetLength: double;
    (*
     Output:      - (the vector is normalized)
     Remarks:     only the length and the direction is preserved, so the orig of
     every edge stored is (0, 0, 0)
    *)
    procedure Normalize;
    class procedure CrossProduct(const a, b: TGBVector; out r: TGBVector);
    class function DotProduct(const a, b: TGBVector): double;
  published
    property Coord: TGBPoint3D read m_Coord write SetCoord;
  end;

type
  TGBTriangle = class(TObject)
  private
    m_points: array [1 .. 3] of TGBPoint3D;
    m_normale: TGBVector;
  public
    constructor Create(ax, ay, az, bx, by, bz, cx, cy, cz, nx, ny,
      nz: double); overload;
    constructor Create(a, b, c: TGBPoint3D; n: TGBVector); overload;
    destructor Destroy; override;
    procedure SetPoint(i: integer; pt: TGBPoint3D);
    function GetPoint(i: integer): TGBPoint3D;
    procedure SetNormale(x, y, z: double);
    function GetNormale: TGBVector;
  end;

type
  TGBTetrahedron = class(TObject)
  private
    m_points: array [1 .. 4] of TGBPoint3D;
    m_tetra: array [1 .. 4] of TGBTetrahedron;
    m_centre: TGBPoint3D;
    m_cur: integer; // -- current vertex/face in the tetra
    (*
     Description: to "rotate" in a tetrahedron (from one vertex to another; or
     from one edge to another)
     Input:       i : the number of rotations (0 (same place), 1, 2 or 3)
     j : the starting position (an integer from 1 to 4)
     Output:      the new position (an integer from 1 to 4)
    *)
    function Rot(i, j: integer): integer;
    procedure TestInvariants;
  public
    m_no: integer;
    (*
     Description: Constructor of a tetra. The vertices are needed, and the
     neighbours are set to NIL.
     Input:  the 4 vertices, correctly oriented according to Orient3d()
    *)
    constructor Create(v1, v2, v3, v4: TGBPoint3D);
    destructor Destroy; override;
    (*
     Description: Get one vertex in the tetrahedron.
     Input:       the index of the vertex in the array of the tetra [1..4]
     Output:      the vertex (a TPoint3D)
    *)
    function GetP(i: integer): TGBPoint3D;
    (*
     Description: Get one vertex in the tetra.
     Input:       r: the number of rotation (0, 1, 2 or 3) starting from m_cur
     Output:      the vertex (a TPoint3D)
    *)
    function GetProt(r: integer): TGBPoint3D;
    (*
     Description: Set one vertex of the tetra to a new value.
     Input:       i : the index of the vertex in the array of the tetra [1..4]
     pt: the point
    *)
    procedure SetP(i: integer; pt: TGBPoint3D);
    (*
     Description: Set one vertex of the tetrahedron to a new value.
     Input:       r: the number of rotation (0, 1, 2 or 3) starting from m_cur
     pt: the point
    *)
    procedure SetProt(r: integer; pt: TGBPoint3D);
    function GetT(i: integer): TGBTetrahedron;
    function GetTrot(r: integer): TGBTetrahedron;
    procedure SetT(i: integer; te: TGBTetrahedron);
    procedure SetTrot(r: integer; te: TGBTetrahedron);
    (*
     Description: Set the current vertex-neighbour to a certain position in the
     the array m_points[]
     Input:       i : integer between [1..4]
     Output:      the tetrahedron with an updated m_cur
    *)
    function SetIndex(i: integer): TGBTetrahedron; overload;
    (*
     Description: Set the current vertex-neighbour to a certain TPoint3D
     Input:       pt : the point
     Output:      the tetrahedron with an updated m_cur
     if pt was one of the vertex of the tetrahedron;
     if not the old m_cur is kept
    *)
    function SetIndex(const pt: TGBPoint3D): TGBTetrahedron; overload;
    (*
     Description: Set the current vertex-neighbour to the opposite of a TTetra
     Input:       te : the tetrahedron to find in the neighbouring list
     Output:      the tetrahedron with an updated m_cur if te is a neighbours of self
    *)
    function SetIndex(const te: TGBTetrahedron): TGBTetrahedron; overload;
    (*
     Description: Rotate the m_cur of the tetra by rotating by [0..3] rotations.
     Input:       r: number of rotations
     Output:      the tetra with a m_cur updated.
    *)
    function RotIndex(r: integer): TGBTetrahedron;
    (*
     Description: Get the current index of the tetra
     Input:       -
     Output:      the current index (the value of m_cur which is [1..4]
    *)
    function GetIndex: integer; overload;
    (*
     Description: Get the index of a point in the tetrahedron
     Input:       pt: the point whose index is asked
     Output:      the index [1..4] if pt is a vertex of the tetra
     0 if pt is NOT a vertex
    *)
    function GetIndex(const pt: TGBPoint3D): integer; overload;
    (*
     Description: Get the index of a tetra (neighbour) in the tetrahedron
     Input:       te: the tetrahedron whose index is asked
     Output:      the index [1..4] if te is a neighbour of the tetra
     0 if te is NOT a neighbour
    *)
    function GetIndex(const te: TGBTetrahedron): integer; overload;
    procedure ResetCentre;
    function HasPoint(const pt: TGBPoint3D): boolean;
    (*
     Description: Determine if a tetra is one of the neighbours of a tetra.
     Input:       te: the tetra to check
     Output:      TRUE  -> te is a neighbour
     FALSE -> te is NOT a neighbour
    *)
    function HasNeighbour(const te: TGBTetrahedron): boolean;
    (*
     Description: Determine is an edge is present in the tetra
     Input:       the origin and destination of the edge
     Output:      TRUE  -> edge is present
     FALSE -> edge is NOT there
     Remarks:     the directtion of the edge is not important. This is just
     testing whether the 2 points are in the tetrahedron.
    *)
    function HasEdge(const org, dest: TGBPoint3D): boolean;
    function HasFace(const pt1, pt2, pt3: TGBPoint3D): boolean;
    function IsOrient3D: boolean;
    (*
     Description: Determine if a tetra is flat - if the 4 vertices are coplanar
     Output:      TRUE  -> te is flat
     FALSE -> te is NOT flat
    *)
    function IsFlat: boolean;
    procedure GetCircumCentre(out centre: TGBPoint3D);
    function GetVolume: double;
  end;

type
  TGBPoly = record
    Vert: array of TGBPoint3D;
    VertCount: integer;
    CurrVert: integer;
    CellPoint: array of TGBPoint3D;
    CellPoinCount: integer;
    Faces: array of array of integer;
    FacesCount: integer;
    VoronoiPoly: array of array of integer;
  end;

type
  TGBPoint3DArr = array of TGBPoint3D;

  TGBPolyhedron = class(TObject)
  private
    // PP1 :TPoly;
    // function    Rot(i, j: integer) : integer;
    // procedure   TestInvariants;
  public
    Polyheders: TGBPoly;
    constructor Create;
    destructor Destroy; override;
    function FindVert(Point1: TGBPoint3D): integer;
    function InsertVert(Point1: TGBPoint3D): integer;
    function FindCellPoint(Point1: TGBPoint3D): integer;
    procedure AddCellPoint(Point1: TGBPoint3D);
    function InsertCellPoint(Point1: TGBPoint3D): integer;
    function FindFace(F: array of integer): integer;
    function FindFaceA(PoiArr: TGBPoint3DArr): integer;
    function FindFaceFromList(FList: TList): integer;
    procedure AddFace(FList: TList);
    procedure AddFaceA(PoiArr: TGBPoint3DArr);
    function InsertFace(FList: TList): integer;
    function InsertFaceA(PoiArr: TGBPoint3DArr): integer;
    // procedure AddPolyhedron(vpoint: TPoint3D; FList:array of TList);
    procedure AddPolyhedron(Vpoint: TGBPoint3D; Farr: array of TGBPoint3DArr;
      Cn: integer);
    function FindVorPoint(P: TGBPoint3D): integer;
    // function    AddVorPoint(p: TGBPoint3D): integer;
    // function    FindCellPoint(p : TGBPoint3D) : integer ;
    // function    AddCellPoint(p: TGBPoint3D): integer;
    // function  FindFace(lst: TList): integer;
    // function    AddFace(lst: TList): integer;
    // destructor  Destroy; override;
  end;

type
  TGBEdge = class(TObject)
  private
    m_org: TGBPoint3D;
    m_dest: TGBPoint3D;
    m_adjTetra: TGBTetrahedron;
    // -- pointer to a tetra that has the edge - only one is ok
    procedure TestInvariants;
  public
    constructor Create; overload;
    constructor Create(org, dest: TGBPoint3D; te: TGBTetrahedron); overload;
    function GetOrg: TGBPoint3D;
    function GetDest: TGBPoint3D;
    function GetAdjTetra: TGBTetrahedron;
    procedure SetTetra(te: TGBTetrahedron);
    procedure Reset(const org, dest: TGBPoint3D; const te: TGBTetrahedron);
    (*
       Description: Determine if 2 edges are the same by comparing their origin and
       their destination.  The orientation is NOT a factor with this
       function (edge a-b == edge b-a); and the tetra connected is not
       a factor either.
       Input: the edge
       Output: TRUE  -> edge e is the same as self
       FALSE -> edge e is different
    *)
    function Compare(const e: TGBEdge): boolean;
    function HasPoint(const pt: TGBPoint3D): boolean;
  end;


type
  (*
   Class: TGLEar
   Description: TEar represents an ear of a polyhedron. Actually, only 2-ears
   are stored in the program; 3-ear are detected 'on-the-fly'.
   An ear is defined by the 2 tetra underlying, and 4 neighbours.
  *)
  TGBEar = class(TObject)
  private
    m_te: array [1 .. 2] of TGBTetrahedron;
    m_adj: array [1 .. 4] of TGBEar;
    m_v: TGBPoint3D; // = nil -- the vertex to be deleted in DeleteVertex()
    procedure TestInvariants;
  public
    m_no: integer;
    m_flag: boolean;
    constructor Create(te1, te2: TGBTetrahedron; v: TGBPoint3D);
    function GetAdj(i: integer): TGBEar;
    procedure SetAdj(i: integer; ear: TGBEar);
    function GetTetra(i: integer): TGBTetrahedron;
    procedure SetTetra(i: integer; const te: TGBTetrahedron);
    function GetVertex: TGBPoint3D;
    (*
     Description: Compares the ear with another 2-ear formed by 2 tetra. 2 ears
     'the same' if they are formed by the 2 same tetra, no matter
     what the order is; the 4 pointers to adjacent ears doesn't
     matter either.
     Input:       te1, te2 : the 2 TTetra forming the ear to compare against
     Output:      TRUE  -> ear & te1-te2 are the same
     FALSE -> NOT the same
    *)
    function Compare(const te1, te2: TGBTetrahedron): boolean;
    function ContainTetra(const te: TGBTetrahedron): boolean;
    function Is3ear(out adjEar1, adjEar2: TGBEar): boolean;
    (*
     Description: Determines if the ear is CONVEX. An ear is convex with respect
     to the deleted point in the middle of H.
     Output:  1  : convex (w/r to deleted point)
     0  : flat ear
     -1 : concave (w/r to deleted point)
    *)
    function IsConvex: integer;
    (*
     Description: Determines if the ear can be flipped; it actually returns the
     result of the intersection test of the ear with the common facet.
     Output:      -1 : no intersection
     0 : 4 points are coplanar
     1 : yes intersection
    *)
    function IsFlippable: integer;
    (*
     Description: Determines if the ear is DELAUNAY with respect to the other
     vertices forming H. When deleting a vertex from a mesh, it
     suffices to test the Delaunayhood of the ear with simply these vertices.
     Input:       lstPts : the list of TPoint3D forming the polyhedron H.
     Output:      TRUE  -> the ear is Delaunay
     FALSE -> the ear is NOT Delaunay
     Note:        Only DeleteVertex_insphere needs that function.
     DeleteVertex_power uses the power to test if an ear is Delaunay.
    *)
    function IsDelaunay(const lstPts: TList): boolean;
  end;

  (*
   Earp is a TGBEar with the attribute power attached to it; this is used for
   DeleteVertex_power. The power of an ear is actually the power of the
   deleted vertex w/r to the circumsphere of the ear (an imaginary tetra)
  *)
  TGBEarp = class(TGBEar)
  private
    m_power: double;
    (*
     Description: The power of an ear is the power of the deleted vertex w/r to
     the circumsphere of the ear (a tetrahedron).
     pow(m_v, sphere(ear))
     If the ear is not convex, then power is set to -9999 (smallest
     possible number), since we're looking for highest power.
     Input:  (the TGBEarp)
     Output:  m_power for the TGBEarp is updated
    *)
    procedure ComputePower;
  public
    constructor Create(te1, te2: TGBTetrahedron; v: TGBPoint3D);
    function GetPower: double;
    procedure UpdatePower;
    procedure SetPower(pow: double);
  end;

type
  (*
   Description: TGeomTools contains static methods for geometric computation in
   3d. Orient3d() and InSphere() use robust arithmetic by default,
   if normal floating-point arithmetic (with a tolerance) is wanted
   then 'FLOAT' should be a conditional define for the compilation.
   Note: Shewchuk's predicates are used for robust arithmetic. They are
   compiled in the 'predicates.dll' and 'predicates.lib', both
   are required to compile the program.
  *)
  TGBGeomTools = class(TObject)
  public
    (*
     Description: Compute the determinant of 3 points (x-y-z)
     Input:       coord (double) of the 3 points (anti-clockwise order)
     Output:      the determinant of the 3 points
     Note:        the value of the determinant is twice the area of the triangle
     defined by the 3 points in 3d space. Or 6 times the volume of
     the tetra formed by the triangular face and the origin (0, 0, 0)
    *)
    class function Det3x3(ax, ay, az, bx, by, bz, cx, cy, cz: double): double; overload;
    (*
     Description: Compute the determinant of 3 point
     Input:       coord (TPoint3D) of the 3 points (anti-clockwise order)
     Output:      the determinant of the 3 points
     Note:        the value of the determinant is twice the area of the triangle
     defined by the 3 points in 3d space. Or 6 times the volume of
     the tetra formed by the triangular face and the origin (0, 0, 0)
    *)
    class function Det3x3(const a, b, c: TGBPoint3D): double; overload;
    (*
     Description: Compute the determinant of 4 points.
     Input:       coord (TPoint3D) of the 4 points (left-hand rule order)
     Output:      the value (a double)
    *)
    class function Det4x4(A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3, C4, D1,
      D2, D3, D4: double): double;
    (*
     Description: Compute the determinant of 4 points with homogenous coord.
     Translation of -d to reduce to Det3x3.
     Input:       coord (TPoint3D) of the 4 points (left-hand rule order)
     Output:      the determinant of the 4 points (6 times the volumes of tetra)
    *)
    class function Det4x4t(const a, b, c, D: TGBPoint3D): double;
    (*
     Description: Compute the determinant of a matrix 5x5. This is used to
     calculate the InSphere_pert(). The InSphere() can usually be
     reduced to a Det4x4 by a translation but the symbolic perturbation
     makes that impossible and Det5x5 must be calculated.
     Input:       a,b,c,d : 4 points defining the sphere
     p: the point to test
     a2..p2: the value (perturbed or not) of the points lifted to a
     paraboloid in (d+1) dimensions.
     Output:      value of the determinant (a double)
     Note:        i cannot use robust arithmetic for that since i cannot modify
     Shewchuk's code. Maybe i could but way too much complicated!
     And if arithmetic causes problems here just increase the pertub.
    *)
    class function Det5x5_perturb(const a, b, c, D, P: TGBPoint3D; A2, B2, C2, D2, P2: double): double;
    (*
     Description: Determine if a point pt is above or below the plane defined by
     a-b-c (anti-clockwise order)
     Input:    a,b,c : the points in anti-clockwise order
     pt : the point to test
     Output:      1  -> pt is BELOW of the plane (OK for left-hand rule)
     0  -> 4 points are coplanar
     -1  -> pt is ABOVE of the plane (NOT OK for left-hand rule)
     Note: "above and below" means when looking from above; or when using the left-hand rule
    *)
    class function Orient3D(const a, b, c, pt: TGBPoint3D): integer;
    (*
     Description: Determine the value of the Orient3d predicate (the value is
     equal to the 4x4 det of the 4 points )
     Input:       a,b,c : the points in anti-clockwise order
     pt : the point to test
     Output:      1  -> pt is BELOW of the plane (OK for left-hand rule)
     0  -> 4 points are coplanar
     -1  -> pt is ABOVE of the plane (NOT OK for left-hand rule)
     Note:        "above and below" means when looking from above;
     or when using the left-hand rule
    *)
    class function Orient3D_value(const a, b, c, pt: TGBPoint3D): double;
    (*
     Description: Determine if a point pt is inside the circumSphere around the
     tetrahedron formed by a-b-c-d
     Input:       a,b,c,d : the 4 vertices of the tetrahedron
     pt : the point to test
     Output:      TRUE  -> pt is INSIDE the sphere
     FALSE -> pt is OUTSIDE or ON the sphere
     Note:        That's an Orient test made in 4D space with the points lifted
     onto the 4D paraboloid [x4 = x1^2 + x2^2 + x3^2]. So simply
     a Det5x5 of the points with homogenous coords.
     *** IMPORTANT ***
     This assumes that the 4 points are ordered correctly according
     to the left-hand rule, i.e. Orient3d(a,b,c,d) must return TRUE.
    *)
    class function InSphere(const a, b, c, D, pt: TGBPoint3D): boolean; overload;
    (*
     Description: Determine if a point pt is inside the circumSphere around the
     tetrahedron formed by a-b-c-d
     Input:       te : the tetrahedron, which contains 4 vertices
     pt : the point to test
     Output:      TRUE  -> pt is INSIDE the sphere
     FALSE -> pt is OUTSIDE or ON the sphere
     Note:        That's an Orient test made in 4D space with the points lifted
     onto the 4D paraboloid [x4 = x1^2 + x2^2 + x3^2]. So simply
     a Det5x5 of the points with homogenous coords.
     *** IMPORTANT ***
     This assumes that the 4 points are ordered correctly according
     to the left-hand rule, i.e. Orient3d(a,b,c,d) must return TRUE.
    *)
    class function InSphere(const te: TGBTetrahedron; const pt: TGBPoint3D): boolean; overload;
    (*
     Description: Determine if a point pt is inside the circumSphere around a
     tetrahedron. USE OF SYMBOLIC PERTURBATION. The perturbation
     scheme is as follows. The point with the highest index is
     perturbed the most in 4D. An arbitrary value is added to its lifted value.
     Input:       a, b, c, d : the 4 points forming the tetrahedron
     pt : the point to test
     Output:      TRUE  -> pt is INSIDE the sphere
     FALSE -> pt is OUTSIDE or ON the sphere
     Note: That's an Orient test made in 4D space with the points lifted
     onto the 4D paraboloid [x4 = x1^2 + x2^2 + x3^2]. So simply
     a Det5x5 of the points with homogenous coords.
     *** IMPORTANT ***
     This assumes that the 4 points are ordered correctly according
     to the left-hand rule, i.e. Orient3d(a,b,c,d) must return TRUE.
    *)
    class function InSphere_perturb(const a, b, c, D, P: TGBPoint3D): boolean; overload;
    class function InSphere_perturb(const te: TGBTetrahedron; const pt: TGBPoint3D): boolean; overload;
    (*
     Description: Calculate the value of the InSphere predicate (the value is the
     result of a 5x5 matrix with the (d+1)-lifted points to the
     revolution paraboloid). A translation of -pt to all the points
     is applied to reduce the computation to a Det4x4.
     Input:       a,b,c,d : 4 points defining the sphere
     pt: the point to test
     Output:      value of the 5x5 determinant (a double)
     positive value -> pt is INSIDE the sphere
     0 -> pt directly on the sphere
     negative value -> pt is OUTSIDE the sphere
    *)
    class function InSphere_value(const a, b, c, D, pt: TGBPoint3D): double;
    (*
     Description: Calculate the centre of the sphere formed by the 4 vertices of
     a tetrahedron
     Input:       a,b,c,d : the 4 vertices of the tetrahedron
     centre : the result
     Output:  the centre (a TPoint3D)
     Note:  source: [FAQ - comp.Vcl.Vcl.CheckLst,.algorithms (5.21)]
    *)
    class procedure CircumSphere(const a, b, c, D: TGBPoint3D; out centre: TGBPoint3D); overload;
    (*
     Description: Calculate the centre of the sphere formed by the 4 vertices of
     a tetrahedron
     Input:  te: the tetrahedron (TTetra), which contains 4 vertices
     centre : the result
     Output: the centre (a TPoint3D)
     Note: source: [FAQ - comp.Vcl.Vcl.CheckLst,.algorithms (5.21)]
    *)
    class procedure CircumSphere(const te: TGBTetrahedron; out centre: TGBPoint3D); overload;
    (*
     Description: determine if an edge intersects a triangle in 3D
     Input:       a, b: the coord of the edge (TPoint3D)
     t1, t2, t3: the coord of the triangle (TPoint3D)
     Output:      1  -> (a-b) intersects the triangle
     0  -> (a-b) is coplanar with 2 points of the triangle
     -1  -> the edge doesn't intersect the triangle
     2  -> a is directly on one edge of t1-t2-t3 (flat tetra)
     3  -> b is directly on one edge of t1-t2-t3 (flat tetra)
    *)
    class function IntersectionEdgeTriangle(const a, b, T1, T2, T3: TGBPoint3D): integer;
    class function IntersectionLineSphere(const P1, P2: TGBPoint3D;
      const te: TGBTetrahedron; out T1, T2: double): boolean; overload;
    (*
     Description: Compute the intersection between a line (defined by a starting
     and an end point) and a sphere.
     Input:       p1, p2: start/end points of the line segment
     te: the 4 points of the tetra defining the sphere
     Output:      value of 't' (a double) in the parametric equation
     [p = p1 + t(p2-p1)]
     Note:        Details of the method (solving of a quadratic equation) is
     available at http://astronomy.swin.edu.au/~pbourke/geometry/sphereline/
    *)
    class function IntersectionLineSphere(const P1, P2, pt1, pt2, pt3,
      Pt4: TGBPoint3D; out T1, T2: double): boolean; overload;
    (*
     Description: Check if the intersection between a line segment and a sphere
     exists, i.e. by projecting the centre of the sphere onto theline we can determine
     if this projection is before or after the start point of the segment.
     Input:       p1, p2: starting-end points of the line
     te: the 4 points of the tetra define the sphere
     Output:      TRUE  -> projection is after the start point of segment
     FALSE -> projection is before the start point of segment
     Note:        Details of the method (solving of a quadratic equation) is
     available at http://astronomy.swin.edu.au/~pbourke/geometry/sphereline/
    *)
    class function IntersectionLineSphere_projCentre(const P1, P2: TGBPoint3D;
      const te: TGBTetrahedron): boolean;
    (*
     Description: compute the euclidian distance between 2 points in 3D
     Input:       a, b: the coord of the edge (TPoint3D)
     Output:      the distance
    *)
    class function Distance3d(const a, b: TGBPoint3D): double;
    (*
     Description: compute the normal vector of a triangle, according to a viewpoint.
     The normal will point in the OPPOSITE direction of this pt
     Input:       a, b, c: the 3 vertices of the triangle
     pt : the view point
     Output:      n: the vector pointing in the opposite direction of pt
    *)
    class procedure NormalOppositePt(const a, b, c, pt: TGBPoint3D; out n: TGBVector);
    (*
      Description: compute the normalized normal vector of a triangle, according to
      a viewpoint.  The normal will point in the DIRECTION of this pt
      Input: a, b, c: the 3 vertices of the triangle
      pt : the view point
      Output:  n: the vector pointing in the direction of pt
    *)
    class procedure NormalDirectionPt(const a, b, c, pt: TGBPoint3D; out n: TGBVector);
    (*
     Description: Compute the power of a tetrahedron with respect to a point
     Input:  A, B, C, D : the 4 vertices defining the tetrahedron
     v : the point
     Output: value of the pow(v, s) -- s being the circumsphere of tetra
     negative  means v inside s.
     positive  means v outside s.
     zero      means v lying on s.
     Remarks: Power function in 3d is (-InSphere / Orient3d)
     is negative to InSphere because it returns positive if v is
     inside the sphere.
    *)
    class function PowerTet(const a, b, c, D, v: TGBPoint3D): Single;
    class procedure SetCountCallToZero;
    class function GetCountCallOrient: integer;
    class function GetCountCallInSphere: integer;
  end;

// =======================================================================
implementation
// =======================================================================

// -- static variable (to debug)
var
  no: integer = 0;
  Numcall_insphere: integer = 0;
  Numcall_orient: integer = 0;
  // -- tolerance because of floating-point precision
const
  tolerance: double = 1E-12;

// -- definition of the functions of the DLL ------------------------------------
function Orient3dR(ax, ay, az, bx, by, bz, cx, cy, cz, Dx, Dy, Dz: double)
  : double; stdcall; external 'PREDICATES.DLL' Name 'Orient3dR';
// -- definition of the functions of the DLL ------------------------------------
function InSphereR(ax, ay, az, bx, by, bz, cx, cy, cz, Dx, Dy, Dz, Ex, Ey,
  Ez: double): double; stdcall; external 'PREDICATES.DLL' Name 'InSphereR';

//-----------------------------------------------------------------------
//----------- TGBPoint3D
//-----------------------------------------------------------------------

constructor TGBPoint3D.Create(x, y, z, Data: Single);
begin
  SetCoord(x, y, z);
  m_data := Data;
  m_no := 0;
  m_flag := False;
end;

procedure TGBPoint3D.CopyValue(tempt: TGBPoint3D);
begin
  m_x := tempt.x;
  m_y := tempt.y;
  m_z := tempt.z;
  m_data := tempt.Data;
end;

constructor TGBPoint3D.Create(flag: boolean; x, y, z, Data: Single);
begin
  SetCoord(x, y, z);
  m_data := Data;
  m_no := no;
  Inc(no);
end;

function TGBPoint3D.GetCoord: TGBPoint3D;
begin
  Result := Self;
end;

function TGBPoint3D.Hit(const pt: TGBPoint3D; tolerance: Single): boolean;
begin
  if (TGBGeomTools.Distance3d(Self, pt) <= tolerance) then
    Result := True
  else
    Result := False;
end;

procedure TGBPoint3D.SetCoord(x, y, z: Single);
begin
  m_x := x;
  m_y := y;
  m_z := z;
end;

procedure TGBPoint3D.SetData(Data: Single);
begin
  m_data := Data;
end;

procedure TGBPoint3D.SetX(x: Single);
begin
  m_x := x;
end;

procedure TGBPoint3D.SetY(y: Single);
begin
  m_y := y;
end;

procedure TGBPoint3D.SetZ(z: Single);
begin
  m_z := z;
end;

procedure TGBPoint3D.SetFlag(b: boolean);
begin
  m_flag := b;
end;

//----------------------------------------------
//----------- TGBVector
//----------------------------------------------

constructor TGBVector.Create;
begin
  m_coord := TGBPoint3D.Create(0, 0, 0);
end;

constructor TGBVector.Create(dest: TGBPoint3D);
begin
  m_coord := TGBPoint3D.Create(dest.x, dest.y, dest.z);
end;

constructor TGBVector.Create(destX, destY, destZ: double);
begin
  m_coord := TGBPoint3D.Create(destX, destY, destZ);
end;

constructor TGBVector.Create(orig, dest: TGBPoint3D);
begin
  m_coord := TGBPoint3D.Create(dest.x - orig.x, dest.y - orig.y,
    dest.z - orig.z);
end;

destructor TGBVector.Destroy;
begin
  m_coord.Free;
  inherited Destroy;
end;

procedure TGBVector.Add(const a: TGBVector);
begin
  m_coord.x := m_coord.x + a.m_coord.x;
  m_coord.y := m_coord.y + a.m_coord.y;
  m_coord.z := m_coord.z + a.m_coord.z;
end;

class procedure TGBVector.CrossProduct(const a, b: TGBVector; out r: TGBVector);
begin
  r.m_coord.x := (a.m_coord.y * b.m_coord.z) - (a.m_coord.z * b.m_coord.y);
  r.m_coord.y := -((a.m_coord.x * b.m_coord.z) - (a.m_coord.z * b.m_coord.x));
  r.m_coord.z := (a.m_coord.x * b.m_coord.y) - (a.m_coord.y * b.m_coord.x);
end;

procedure TGBVector.Multiply(a: double);
begin
  m_coord.x := m_coord.x * a;
  m_coord.y := m_coord.y * a;
  m_coord.z := m_coord.z * a;
end;

procedure TGBVector.Subtract(const a: TGBVector);
begin
  m_coord.x := m_coord.x - a.m_coord.x;
  m_coord.y := m_coord.y - a.m_coord.y;
  m_coord.z := m_coord.z - a.m_coord.z;
end;

function TGBVector.GetLength: double;
begin
  Result := Sqrt((m_coord.x * m_coord.x) + (m_coord.y * m_coord.y) +
    (m_coord.z * m_coord.z));
end;

procedure TGBVector.SetCoord(const Coord: TGBPoint3D);
begin
  m_coord.x := coord.x;
  m_coord.y := coord.y;
  m_coord.z := coord.z;
end;

procedure TGBVector.Normalize;
const
  tolerance: double = 1E-12;
var
  Length: double;
begin
  Length := Self.GetLength;
  // -- raise an EZeroDivide if the lenght of the vector is 0 (or near 0)
  if (abs(Length) < tolerance) then
  begin
    raise EZeroDivide.Create('Can not normalize vector');
  end;
  m_coord.x := m_coord.x / Length;
  m_coord.y := m_coord.y / Length;
  m_coord.z := m_coord.z / Length;
end;

class function TGBVector.DotProduct(const a, b: TGBVector): double;
begin
  Result := ((a.m_coord.x * b.m_coord.x) + (a.m_coord.y * b.m_coord.y) +
    (a.m_coord.z * b.m_coord.z));
end;

//-----------------------------------------------------------
//---------- TGBTriangle
//-----------------------------------------------------------
constructor TGBTriangle.Create(ax, ay, az, bx, by, bz, cx, cy, cz, nx, ny,
  nz: double);
begin
  m_points[1] := TGBPoint3D.Create(ax, ay, az);
  m_points[2] := TGBPoint3D.Create(bx, by, bz);
  m_points[3] := TGBPoint3D.Create(cx, cy, cz);
  m_normale := TGBVector.Create(nx, ny, nz);
end;

constructor TGBTriangle.Create(a, b, c: TGBPoint3D; n: TGBVector);
begin
  m_points[1] := TGBPoint3D.Create(a.x, a.y, a.z);
  m_points[2] := TGBPoint3D.Create(b.x, b.y, b.z);
  m_points[3] := TGBPoint3D.Create(c.x, c.y, c.z);
  m_normale := TGBVector.Create(n.coord);
end;

destructor TGBTriangle.Destroy;
begin
  m_points[1].Free;
  m_points[2].Free;
  m_points[3].Free;
  m_normale.Free;
  inherited Destroy;
end;

function TGBTriangle.GetNormale: TGBVector;
begin
  Result := m_normale;
end;

procedure TGBTriangle.SetNormale(x, y, z: double);
begin
  m_normale.coord.x := x;
  m_normale.coord.y := y;
  m_normale.coord.z := z;
end;

function TGBTriangle.GetPoint(i: integer): TGBPoint3D;
begin
  Result := m_points[i];
end;

procedure TGBTriangle.SetPoint(i: integer; pt: TGBPoint3D);
begin
  m_points[i] := pt;
end;

//
// =========== TGBTetrahedron ===============
//
constructor TGBTetrahedron.Create(v1, v2, v3, v4: TGBPoint3D);
var
  i: integer;
begin
  m_points[1] := v1;
  m_points[2] := v2;
  m_points[3] := v3;
  m_points[4] := v4;
  for i := 1 to 4 do
  begin
    m_tetra[i] := nil;
  end;
  m_no := no;
  Inc(no);
  m_cur := 1;
  m_centre := TGBPoint3D.Create(-999, -999, -999);
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBTetrahedron.GetP(i: integer): TGBPoint3D;
begin
  Assert((i >= 1) and (i <= 4));
  Result := m_points[i];
end;

function TGBTetrahedron.GetProt(r: integer): TGBPoint3D;
begin
  Assert((r >= 0) and (r <= 3));
  Result := m_points[Rot(r, m_cur)];
end;

procedure TGBTetrahedron.SetP(i: integer; pt: TGBPoint3D);
begin
  Assert((i >= 1) and (i <= 4));
  m_points[i] := pt;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

procedure TGBTetrahedron.SetProt(r: integer; pt: TGBPoint3D);
begin
  Assert((r >= 0) and (r <= 3));
  m_points[Rot(r, m_cur)] := pt;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBTetrahedron.GetT(i: integer): TGBTetrahedron;
begin
  Assert((i >= 1) and (i <= 4));
  Result := m_tetra[i];
end;

function TGBTetrahedron.GetTrot(r: integer): TGBTetrahedron;
begin
  Assert((r >= 0) and (r <= 3));
  Result := m_tetra[Rot(r, m_cur)];
end;

procedure TGBTetrahedron.SetT(i: integer; te: TGBTetrahedron);
begin
  Assert((i >= 1) and (i <= 4));
  m_tetra[i] := te;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

procedure TGBTetrahedron.SetTrot(r: integer; te: TGBTetrahedron);
begin
  Assert((r >= 0) and (r <= 3));
  m_tetra[Rot(r, m_cur)] := te;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBTetrahedron.SetIndex(i: integer): TGBTetrahedron;
begin
  Assert((i >= 1) and (i <= 4));
  m_cur := i;
  Result := Self;
end;

function TGBTetrahedron.SetIndex(const pt: TGBPoint3D): TGBTetrahedron;
var
  index: integer;
begin
  index := Self.GetIndex(pt);
  Assert(index <> 0);
  m_cur := index;
  Result := Self;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBTetrahedron.SetIndex(const te: TGBTetrahedron): TGBTetrahedron;
var
  index: integer;
begin
  index := GetIndex(te);
  Assert(index <> 0);
  if (index <> 0) then
  begin
    m_cur := index;
  end;
  Result := Self;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBTetrahedron.HasEdge(const org, dest: TGBPoint3D): boolean;
begin
  if ((Self.HasPoint(org) = True) and (Self.HasPoint(dest) = True)) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// ******************************************************************************
// Description: Determine if the 4 vertices of a tetra are "correctly ordered"
// (according to the Orient3D test in the GeomTools unit)
// Input:       -
// Output:      TRUE  -> tetra is correctly ordered or the 4 points are coplanar
// FALSE -> tetra is not correctly ordered
// ******************************************************************************
function TGBTetrahedron.IsOrient3D: boolean;
var
  orient: integer;
begin
  orient := TGBGeomTools.Orient3D(m_points[1], m_points[2], m_points[3],
    m_points[4]);
  if (orient = -1) then
    Result := False
  else
    Result := True;
end;

procedure TGBTetrahedron.GetCircumCentre(out centre: TGBPoint3D);
begin
  // if (m_centre.x = -999) then
  // begin
  TGBGeomTools.CircumSphere(m_points[1], m_points[2], m_points[3],
    m_points[4], centre);
  { m_centre.CopyValue(centre);
    end
    else
    begin
    centre.x := m_centre.x;
    centre.y := m_centre.y;
    centre.z := m_centre.z;
    end;
  } end;

// ******************************************************************************
// Description: Determine if a point is one of the vertices of a tetra.
// Input:       pt: the point to check
// Output:      TRUE  -> point is there
// FALSE -> point is NOT there
// ******************************************************************************
function TGBTetrahedron.HasPoint(const pt: TGBPoint3D): boolean;
begin
  if (GetIndex(pt) <> 0) then
    Result := True
  else
    Result := False;
end;

function TGBTetrahedron.HasNeighbour(const te: TGBTetrahedron): boolean;
var
  i: integer;
  found: boolean;
begin
  found := False;
  for i := 1 to 4 do
  begin
    if (m_tetra[i] = te) then
    begin
      found := True;
      break;
    end;
  end;
  Result := found;
end;

function TGBTetrahedron.Rot(i, j: integer): integer;
const
  a: array [0 .. 3, 1 .. 4] of integer = ((1, 2, 3, 4), (2, 3, 4, 1),
    (3, 4, 1, 2), (4, 1, 2, 3));
begin
  Assert((i >= 0) and (i <= 3) and (j >= 1) and (j <= 4));
  Result := a[i, j];
end;

function TGBTetrahedron.GetIndex(const pt: TGBPoint3D): integer;
var
  i: integer;
  r: integer;
begin
  r := 0;
  for i := 1 to 4 do
  begin
    if (m_points[i] = pt) then
    begin
      r := i;
      break;
    end;
  end;
  Result := r;
end;

function TGBTetrahedron.GetIndex(const te: TGBTetrahedron): integer;
var
  i: integer;
  r: integer;
begin
  r := 0;
  for i := 1 to 4 do
  begin
    if (m_tetra[i] = te) then
    begin
      r := i;
      break;
    end;
  end;
  Result := r;
end;

function TGBTetrahedron.GetIndex: integer;
begin
  Result := m_cur;
end;

function TGBTetrahedron.IsFlat: boolean;
begin
  if (TGBGeomTools.Orient3D(m_points[1], m_points[2], m_points[3], m_points[4]
    ) = 0) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TGBTetrahedron.RotIndex(r: integer): TGBTetrahedron;
begin
  Assert((r > 0) and (r < 3));
  m_cur := Rot(r, m_cur);
  Result := Self;
end;

procedure TGBTetrahedron.TestInvariants;
var
  i: integer;
begin
  Assert(Self.IsOrient3D = True);
  for i := 1 to 4 do
  begin
    Assert(m_points[i] <> nil);
  end;
  Assert((m_cur >= 1) and (m_cur <= 4));
end;

function TGBTetrahedron.GetVolume: double;
var
  re: double;
begin
  re := TGBGeomTools.Det4x4t(m_points[1], m_points[2], m_points[3], m_points[4]);
  Result := re / 6;
end;

destructor TGBTetrahedron.Destroy;
begin
  m_centre.Free;
  inherited Destroy;
end;

procedure TGBTetrahedron.ResetCentre;
begin
  m_centre.SetCoord(-999, -999, -999);
end;

function TGBTetrahedron.HasFace(const pt1, pt2, pt3: TGBPoint3D): boolean;
begin
  if ((Self.HasPoint(pt1) = True) and (Self.HasPoint(pt2) = True) and
    (Self.HasPoint(pt3) = True)) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function EqualIntArrays(a: array of integer; b: array of integer): boolean;
var
  An, Bn, i: integer;
  r: boolean;
begin
  An := Length(a);
  Bn := Length(b);
  if (An = Bn) then
  begin
    r := True;
    i := 0;
    while (r = True) and (i < An) do
    begin
      r := (a[i] = b[i]);
      Inc(i);
    end;
  end
  else
    r := False;
  Result := r;
end;

function EqualPoints3D(P1, P2: TGBPoint3D): boolean;
begin
  if (P1.x = P2.x) and (P1.y = P2.y) and (P1.z = P2.z) then
    Result := True
  else
    Result := False;
end;

function AssertArray(b: array of integer; out a: array of integer): boolean;
var
  An, Bn, i: integer;
  r: boolean;
begin
  An := Length(a);
  Bn := Length(b);
  if An = Bn then
  begin
    r := True;
    for i := 0 to An - 1 do
      a[i] := b[i];

  end
  else
    r := False;
  Result := r;
end;

//-------------------------------------------------------------------------
// ====================== TGBPolyhedron ===================================
//-------------------------------------------------------------------------
constructor TGBPolyhedron.Create;
var
  i: integer;
begin
  Polyheders.VertCount := 0;
  // SetLength(Polyheders.Vert, count);
  // SetLength(Polyheders.VoronoiPoly, count);
  Polyheders.CellPoinCount := 0;
  Polyheders.FacesCount := 0;
  Polyheders.CurrVert := 0;
end;

destructor TGBPolyhedron.Destroy;
begin
  inherited Destroy; // m_centre.Free;
end;

function TGBPolyhedron.InsertVert(Point1: TGBPoint3D): integer;
var
  i: integer;
begin
  i := Polyheders.CurrVert;
  Polyheders.Vert[i] := TGBPoint3D.Create;
  Polyheders.Vert[i] := Point1;
  Polyheders.CurrVert := i + 1;
  Result := Polyheders.CurrVert;
end;

function TGBPolyhedron.FindVert(Point1: TGBPoint3D): integer;
var
  i, r: integer;
begin
  r := -1;
  for i := 0 to Polyheders.CurrVert - 1 do
  begin
    if EqualPoints3D(Polyheders.Vert[i], Point1) then
    begin
      r := i;
      break;
    end;
  end;
  Result := r;
end;

function TGBPolyhedron.FindCellPoint(Point1: TGBPoint3D): integer;
var
  i, r: integer;
begin
  r := -1;
  for i := 0 to Polyheders.CellPoinCount - 1 do
  begin
    if EqualPoints3D(Polyheders.CellPoint[i], Point1) then
    begin
      r := i;
      break;
    end;
  end;
  Result := r;
end;

procedure TGBPolyhedron.AddCellPoint(Point1: TGBPoint3D);
var
  i, r: integer;
begin
  i := Polyheders.CellPoinCount;
  SetLength(Polyheders.CellPoint, (i + 1));
  Polyheders.CellPoint[i] := TGBPoint3D.Create;
  Polyheders.CellPoint[i] := Point1;
  Polyheders.CellPoinCount := i + 1;
end;

function TGBPolyhedron.InsertCellPoint(Point1: TGBPoint3D): integer;
var
  i, r: integer;
begin
  r := FindCellPoint(Point1);
  if r < 0 then
  begin
    AddCellPoint(Point1);
    r := Polyheders.CellPoinCount - 1;
  end;
  Result := r;
end;

function TGBPolyhedron.FindFace(F: array of integer): integer;
var
  i, Count, r: integer;
begin
  r := -1;
  Count := Polyheders.FacesCount;
  for i := 0 to Count - 1 do
  begin
    if EqualIntArrays(Polyheders.Faces[i], F) then
      r := i;
  end;
  Result := r;
end;

function TGBPolyhedron.FindFaceA(PoiArr: TGBPoint3DArr): integer;
var
  i, Count, r: integer;
  Farr: array of integer;

begin
  Count := Length(PoiArr);
  SetLength(Farr, Count);
  r := -1;
  for i := 0 to Count - 1 do
  begin
    r := FindCellPoint(PoiArr[i]);
    if r < 0 then
    begin
      Result := r;
      Exit;
    end
    else
      Farr[i] := r;
  end;
  r := FindFace(Farr);
  Result := r;
end;

function TGBPolyhedron.FindFaceFromList(FList: TList): integer;
var
  i, Count, r: integer;
  Farr: array of integer;
begin
  Count := FList.Count;
  SetLength(Farr, Count);
  r := -1;
  for i := 0 to Count - 1 do
  begin
    r := FindCellPoint(TGBPoint3D(FList[i]));
    if (r < 0) then
    begin
      Result := r;
      Exit;
    end
    else
      Farr[i] := r;
  end;
  r := FindFace(Farr);
  Result := r;
end;

procedure TGBPolyhedron.AddFace(FList: TList);
var
  i, Count: integer;
  Farr: array of integer;
begin
  Count := FList.Count;
  SetLength(Farr, Count);
  for i := 0 to Count - 1 do
    Farr[i] := InsertCellPoint(TGBPoint3D(FList[i]));

  i := Polyheders.FacesCount;
  SetLength(Polyheders.Faces, i + 1);
  SetLength(Polyheders.Faces[i], Count);
  AssertArray(Farr, Polyheders.Faces[i]);
  Polyheders.FacesCount := i + 1;
end;

procedure TGBPolyhedron.AddFaceA(PoiArr: TGBPoint3DArr);
var
  i, Count: integer;
  Farr: array of integer;
begin
  Count := Length(PoiArr);
  SetLength(Farr, Count);
  for i := 0 to Count - 1 do
    Farr[i] := InsertCellPoint(PoiArr[i]);

  i := Polyheders.FacesCount;
  SetLength(Polyheders.Faces, i + 1);
  SetLength(Polyheders.Faces[i], Count);
  AssertArray(Farr, Polyheders.Faces[i]);
  Polyheders.FacesCount := i + 1;
end;

function TGBPolyhedron.InsertFace(FList: TList): integer;
var
  r: integer;
begin
  r := FindFaceFromList(FList);
  if r < 0 then
  begin
    AddFace(FList);
    r := Polyheders.FacesCount - 1;
  end;
  Result := r;
end;

function TGBPolyhedron.InsertFaceA(PoiArr: TGBPoint3DArr): integer;
var
  r: integer;
begin
  r := FindFaceA(PoiArr);
  if r < 0 then
  begin
    AddFaceA(PoiArr);
    r := Polyheders.FacesCount - 1;
  end;
  Result := r;
end;

procedure TGBPolyhedron.AddPolyhedron(Vpoint: TGBPoint3D;
  Farr: array of TGBPoint3DArr; Cn: integer);
var
  Nfaces, n, i, j, Test, Tt: integer;
  // tempList: TList;
begin
  if FindVert(Vpoint) < 0 then
  begin
    n := Polyheders.CurrVert;
    Polyheders.CurrVert := n + 1;
    SetLength(Polyheders.Vert, n + 1);
    SetLength(Polyheders.VoronoiPoly, n + 1);
    // tempList:=TList.Create;
    Polyheders.Vert[n] := Vpoint;
    Nfaces := Cn;
    SetLength(Polyheders.VoronoiPoly[n], Nfaces);
    for i := 0 to Nfaces - 1 do
    begin
      Test := Length(Farr[i]);
      // for j := 0 to test - 1 do tempList.Add(Farr[i][j]);
      // tempList:=Flist[i];
      Polyheders.VoronoiPoly[n][i] := InsertFaceA(Farr[i]);
      // tempList.Clear;
    end;
    Tt := 1;
  end;
  // tempList.Destroy;
end;

{
  procedure TPolyhedr.AddPolyhedron(vpoint: TPoint3D;
     Farr :array of TarrPoint; cn: integer);
  var
    nfaces, n, i, j, test, tt: integer;
    tempList: TList;
  begin
    n := Polyheders.CurrVert;
    Polyheders.CurrVert := n+1;
    tempList := TList.Create;
    Polyheders.Vert[n] := vpoint;
    nfaces:=cn;
    SetLength(Polyheders.VoronoiPoly[n],nfaces);
    for i:= 0 to nfaces - 1 do
    begin
      test:=Length(Farr[i]);
      for j := 0 to test - 1 do tempList.Add(Farr[i][j]);
      //tempList:=Flist[i];
      Polyheders.VoronoiPoly[n][i]:= InsertFace(tempList);
      tempList.Clear;
    end;
    tt:=1;
    // tempList.Destroy;
  end;

  {
  procedure TPolyhedr.AddPolyhedron(vpoint: TPoint3D; FList:array of TList);
  var
    nfaces, n, i, test: integer;
    tempList: TList;
  begin
    n:=Polyheders.CurrVert;
    Polyheders.CurrVert:=n+1;
    tempList:=TList.Create;
    Polyheders.Vert[n]:=vpoint;
    nfaces:=Length(FList);
    SetLength(Polyheders.VoronoiPoly[n],nfaces);
    for i:= 0 to nfaces - 1 do
    begin
    test:=FList[i].Count;
    tempList:=Flist[i];
    InsertFace(tempList);
    tempList.Clear;
  end;
  // tempList.Destroy;
end;

}
function TGBPolyhedron.FindVorPoint(P: TGBPoint3D): integer;
var
  i, r: integer;
begin
  r := -1;
  {
    for I := 0 to p_centres.Count - 1 do
    begin
      if p_centres[i]=p then r:=I;
    end;
  }
  Result := r;
end;

{
  function    TPolyhedr.AddVorPoint(p : TPoint3D) : integer ;
  var
    r: integer;
  begin
    r:=FindVorPoint(p);
    if r<0 then
    begin
      r:=p_centres.Count;
      p_centres.Add(p);
    end;
    result:=r;
  end;

  function    TPolyhedr.FindCellPoint(p : TPoint3D) : integer ;
  var
    i, r: integer;
  begin
    r:=-1;
    for I := 0 to p_points.Count - 1 do
    begin
      if p_points[i] = p then
      begin
        r:=I;
        Break;
    end;
  end;

  result:=r;
  end;

  function    TPolyhedr.AddCellPoint(p : TPoint3D) : integer ;
  var
    r: integer;
  begin
    r := FindCellPoint(p);
    if r<0 then
    begin
      r := p_points.Count;
      p_points.Add(p);
    end;
    result := r;
  end;

  function  TPolyhedr.FindFace(lst: TList): integer;
  var
    i, r: integer;
    templist: Tlist;
    point1: TPoint3D;
  begin
    templist := TList.Create;
    r := -1;
    result := -1;
    for i := 0 to lst.Count - 1 do
    begin
      point1 := TPoint3D(lst[i]);
      r := FindCellPoint(point1);
      if r >= 0 then r:=templist.Add(@r) else
      begin
        result := r;
        exit;
    end;
  end;

  for i := 0 to p_faces.Count - 1 do
  begin
    if TList(p_faces[i]) = templist then
    begin
      result := i;
      break;
    end;
  end;
  end;

  function    TPolyhedr.AddFace(lst: TList): integer;
  var
    i, r: integer;
    templist: Tlist;
    point1: TPoint3D;
  begin
    templist := TList.Create;

    for i := 0 to lst.Count - 1 do
    begin
      point1 := TPoint3D(lst[i]);
      r := AddCellPoint(point1);
      templist.Add(@r);
    end;
    p_faces.Add(templist);

    result := p_faces.Count-1;
  end;
}

//----------------------------------------------------------------
// =================== TGBEdge ===================================
//----------------------------------------------------------------

constructor TGBEdge.Create;
begin
  m_org := nil;
  m_dest := nil;
  m_adjTetra := nil;
end;

constructor TGBEdge.Create(org, dest: TGBPoint3D; te: TGBTetrahedron);
begin
  // Assert( (org <> nil) and (dest <> nil) and (te <> nil) );
  Assert((org <> nil) and (dest <> nil));
  m_org := org;
  m_dest := dest;
  m_adjTetra := te;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;


function TGBEdge.Compare(const e: TGBEdge): boolean;
begin
  if (((Self.m_org = e.m_org) or (Self.m_org = e.m_dest)) and
    ((Self.m_dest = e.m_dest) or (Self.m_dest = e.m_org))) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TGBEdge.GetAdjTetra: TGBTetrahedron;
begin
  Result := m_adjTetra;
end;

function TGBEdge.GetDest: TGBPoint3D;
begin
  Result := m_dest;
end;

function TGBEdge.GetOrg: TGBPoint3D;
begin
  Result := m_org;
end;

procedure TGBEdge.Reset(const org, dest: TGBPoint3D; const te: TGBTetrahedron);
begin
  // Assert( (org <> nil) and (dest <> nil) and (te <> nil) );
  Assert((org <> nil) and (dest <> nil));
  m_org := org;
  m_dest := dest;
  m_adjTetra := te;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

procedure TGBEdge.TestInvariants;
begin
  // Assert(m_adjTetra <> nil);
  Assert(m_dest <> nil);
  Assert(m_org <> nil);
end;

procedure TGBEdge.SetTetra(te: TGBTetrahedron);
begin
  m_adjTetra := te;
end;

function TGBEdge.HasPoint(const pt: TGBPoint3D): boolean;
begin
  if ((m_org = pt) or (m_dest = pt)) then
    Result := True
  else
    Result := False;
end;

class procedure TGBGeomTools.SetCountCallToZero;
begin
  Numcall_insphere := 0;
  Numcall_orient := 0;
end;

class function TGBGeomTools.GetCountCallOrient: integer;
begin
  Result := Numcall_orient;
end;

class function TGBGeomTools.GetCountCallInSphere: integer;
begin
  Result := Numcall_insphere;
end;


class function TGBGeomTools.Det3x3(const a, b, c: TGBPoint3D): double;
begin
  Result := Det3x3(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z);
end;

class function TGBGeomTools.Det3x3(ax, ay, az, bx, by, bz, cx, cy,
  cz: double): double;
var
  Temp1: double;
  Temp2: double;
  Temp3: double;
begin
  Temp1 := ax * (by * cz - bz * cy);
  Temp2 := ay * (bx * cz - bz * cx);
  Temp3 := az * (bx * cy - by * cx);
  Result := Temp1 - Temp2 + Temp3;
end;

class function TGBGeomTools.Det4x4t(const a, b, c, D: TGBPoint3D): double;
begin
  Result := Det3x3(a.x - D.x, a.y - D.y, a.z - D.z, b.x - D.x, b.y - D.y,
    b.z - D.z, c.x - D.x, c.y - D.y, c.z - D.z);
end;

class function TGBGeomTools.Det4x4(A1, A2, A3, A4, B1, B2, B3, B4, C1, C2, C3,
  C4, D1, D2, D3, D4: double): double;
begin
  A1 := A1 * Det3x3(B2, B3, B4, C2, C3, C4, D2, D3, D4);
  A2 := A2 * Det3x3(B1, B3, B4, C1, C3, C4, D1, D3, D4);
  A3 := A3 * Det3x3(B1, B2, B4, C1, C2, C4, D1, D2, D4);
  A4 := A4 * Det3x3(B1, B2, B3, C1, C2, C3, D1, D2, D3);

  Result := A1 - A2 + A3 - A4;
end;

class procedure TGBGeomTools.CircumSphere(const te: TGBTetrahedron;
  out centre: TGBPoint3D);
begin
  CircumSphere(te.GetP(1), te.GetP(2), te.GetP(3), te.GetP(4), centre);
end;

class procedure TGBGeomTools.CircumSphere(const a, b, c, D: TGBPoint3D;
  out centre: TGBPoint3D);
var
  Temp1: TGBVector;
  Temp2: TGBVector;
  Temp3: TGBVector;
  Part1: TGBVector;
  Part2: TGBVector;
  Part3: TGBVector;
  Norm: double;
  pt1: TGBPoint3D;
  pt2: TGBPoint3D;
  pt3: TGBPoint3D;
begin
  Temp1 := TGBVector.Create(a, b);
  Temp2 := TGBVector.Create(a, c);
  Temp3 := TGBVector.Create(a);
  Part1 := TGBVector.Create;
  TGBVector.CrossProduct(Temp1, Temp2, Part1);
  Temp1.coord := D;
  Temp1.Subtract(Temp3);
  Norm := Sqr(Temp1.GetLength);
  Part1.Multiply(Norm);

  Temp1.coord := D;
  Temp2.coord := b;
  Temp1.Subtract(Temp3);
  Temp2.Subtract(Temp3);
  Part2 := TGBVector.Create;
  TGBVector.CrossProduct(Temp1, Temp2, Part2);
  Temp1.coord := c;
  Temp1.Subtract(Temp3);
  Norm := Sqr(Temp1.GetLength);
  Part2.Multiply(Norm);

  Temp1.coord := c;
  Temp2.coord := D;
  Temp1.Subtract(Temp3);
  Temp2.Subtract(Temp3);
  Part3 := TGBVector.Create;
  TGBVector.CrossProduct(Temp1, Temp2, Part3);
  Temp1.coord := b;
  Temp1.Subtract(Temp3);
  Norm := Sqr(Temp1.GetLength);
  Part3.Multiply(Norm);

  pt1 := TGBPoint3D.Create(b.x - a.x, b.y - a.y, b.z - a.z);
  pt2 := TGBPoint3D.Create(c.x - a.x, c.y - a.y, c.z - a.z);
  pt3 := TGBPoint3D.Create(D.x - a.x, D.y - a.y, D.z - a.z);

  Part1.Add(Part2);
  Part1.Add(Part3);

  Part1.Multiply(1 / (2 * Det3x3(pt1, pt2, pt3)));
  Temp1.coord := a;
  Part1.Add(Temp1);

  centre.x := Part1.coord.x;
  centre.y := Part1.coord.y;
  centre.z := Part1.coord.z;

  Temp1.Free;
  Temp2.Free;
  Temp3.Free;
  Part1.Free;
  Part2.Free;
  Part3.Free;
  pt1.Free;
  pt2.Free;
  pt3.Free;
end;

class function TGBGeomTools.InSphere(const te: TGBTetrahedron;
  const pt: TGBPoint3D): boolean;
begin
  Result := InSphere(te.GetP(1), te.GetP(2), te.GetP(3), te.GetP(4), pt);
end;

class function TGBGeomTools.InSphere(const a, b, c, D, pt: TGBPoint3D): boolean;
var
  re: double;
begin
  Inc(Numcall_insphere);
  // -- if a-b-c-d are coplanar, then pt is automatically inside the sphere
  if (Orient3D(a, b, c, D) = 0) then
  begin
    Result := True;
  end
  else
  begin
{$IFDEF FLOAT}
    re := InSphere_value(a, b, c, D, pt);
    if (abs(re) < tolerance) then
      Result := False
    else if (re > 0) then
      Result := True
    else
      Result := False;

{$ELSEIF True}
    re := InSphereR(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, D.x, D.y, D.z,
      pt.x, pt.y, pt.z);
    if (re > 0) then
      Result := True
    else
      Result := False; // -- if directly on the sphere, then return false

{$IFEND}
  end;
end;

class function TGBGeomTools.InSphere_perturb(const a, b, c, D,
  P: TGBPoint3D): boolean;
// ---------------------------------
  function Compareindex(Item1, Item2: Pointer): integer;
  var
    a, b: TGBPoint3D;
  begin
    a := TGBPoint3D(Item1);
    b := TGBPoint3D(Item2);
    if (a.m_no < b.m_no) then
      Result := -1
    else if (a.m_no > b.m_no) then
      Result := 1
    else
      Result := 0;
  end;
// ---------------------------------
const
  Pert: Single = 0.1;
var
  re: Single;
  Final: boolean;
  LstIndex: TList;
  A2, B2, C2, D2, P2: Single;
  HighPt: TGBPoint3D;
  Continue: boolean;
  Pass: integer;
begin
  final := False;
  // -- if a-b-c-d are coplanar, then pt is automatically inside the sphere
  if (Orient3D(a, b, c, D) = 0) then
  begin
    final := True;
  end
  else
  begin
    re := InSphereR(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, D.x, D.y, D.z,
      P.x, P.y, P.z);
    if (re > 0) then
      final := True
    else if (re < 0) then
      final := False
    else // -- (re = 0) if result is 0 then perturb the points in (d+1) dimension
    begin
      // -- the point with highest index is perturbed the most upwards, if the
      // -- result is 0 again then the second highest is perturbed, and so on...
      A2 := Sqr(a.x) + Sqr(a.y) + Sqr(a.z);
      B2 := Sqr(b.x) + Sqr(b.y) + Sqr(b.z);
      C2 := Sqr(c.x) + Sqr(c.y) + Sqr(c.z);
      D2 := Sqr(D.x) + Sqr(D.y) + Sqr(D.z);
      P2 := Sqr(P.x) + Sqr(P.y) + Sqr(P.z);

      LstIndex := TList.Create;
      LstIndex.Add(a);
      LstIndex.Add(b);
      LstIndex.Add(c);
      LstIndex.Add(D);
      LstIndex.Add(P);
      LstIndex.Sort(@Compareindex);

      Continue := True;
      Pass := 1;
      while (Continue = True) do
      begin
        if (Pass = 5) then
        begin
          final := False;
          break;
        end;
        HighPt := LstIndex[5 - Pass];
        if (HighPt = a) then
          A2 := A2 + Power(Pert, Pass)
        else if (HighPt = b) then
          B2 := B2 + Power(Pert, Pass)
        else if (HighPt = c) then
          C2 := C2 + Power(Pert, Pass)
        else if (HighPt = D) then
          D2 := D2 + Power(Pert, Pass)
        else
          P2 := P2 + Power(Pert, Pass);

        // -- compute Det5x5 with one point perturbed in 4 dimensions
        re := Det5x5_perturb(a, b, c, D, P, A2, B2, C2, D2, P2);
        if (abs(re) < 1E-12) then
          // -- floating-point arithmetic, do it again if
          Inc(Pass) // -- not sure about result.
        else if (re > 0) then
        begin
          Continue := False;
          final := True;
        end
        else if (re < 0) then
        begin
          final := False;
          Continue := False;
        end;
      end;
    end;
  end;
  Result := final;
end;

class function TGBGeomTools.InSphere_perturb(const te: TGBTetrahedron;
  const pt: TGBPoint3D): boolean;
begin
  Result := InSphere_perturb(te.GetP(1), te.GetP(2), te.GetP(3),
    te.GetP(4), pt);
end;

class function TGBGeomTools.Orient3D(const a, b, c, pt: TGBPoint3D): integer;
var
  re: double;
begin
  Inc(Numcall_orient);

{$IFDEF FLOAT}
  // -- floating point arithmetic
  re := Det4x4t(a, b, c, pt);
  if (abs(re) < tolerance) then
    Result := 0
  else if (re > 0) then
    Result := 1
  else // -- (determinant < 0)
    Result := -1;

{$ELSEIF True}
  // -- robust arithmetic
  re := Orient3dR(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, pt.x,
    pt.y, pt.z);
  if (re > 0) then
    Result := 1
  else if (re = 0) then
    Result := 0
  else
    Result := -1;
{$IFEND}
end;

class function TGBGeomTools.Orient3D_value(const a, b, c,
  pt: TGBPoint3D): double;
begin
  Result := Det4x4t(a, b, c, pt);
end;

class function TGBGeomTools.IntersectionEdgeTriangle(const a, b, T1, T2,
  T3: TGBPoint3D): integer;
var
  OrderA: integer;
  OrderB: integer;
  Test1: integer;
  Test2: integer;
  Test3: integer;
begin
  OrderA := Orient3D(T1, T2, T3, a);
  OrderB := Orient3D(T1, T2, T3, b);

  // -- a & b are on the same side of the triangle; or they are both coplanar
  // -- with the triangle
  if (OrderA = OrderB) then
  begin
    if (OrderA = 0) then // -- means that the 5 pts are coplanar
    begin
      raise Exception.Create('5 pts are coplanar');
      { TODO : 5 pts are coplanar }
      Result := 1;
    end
    else
    begin
      Result := -1; // -- this case is impossible in my app, but for a general
    end; // -- case it's possible that both a and b are on the same
  end // -- side of the triangle

  // -- a & b are on each side of the triangle; or one (only one) can be on the
  // -- plane formed by t1-t2-t3
  else if ((OrderA <> 0) and (OrderB <> 0)) then
  begin
    Test1 := Orient3D(b, T2, T3, a);
    Test2 := Orient3D(T1, b, T3, a);
    Test3 := Orient3D(T1, T2, b, a);
    if ((Test1 = OrderB) or (Test2 = OrderB) or (Test3 = OrderB)) then
    begin
      Result := -1;
    end
    else if ((Test1 = 0) or (Test2 = 0) or (Test3 = 0)) then
    begin
      Result := 0;
    end
    else
    begin
      Result := 1;
    end;
  end

  // -- a is coplanar with t1-t2-t3 and b is on one side of the triangle
  else if (OrderA = 0) then
  begin
    Test1 := Orient3D(b, T2, T3, a);
    Test2 := Orient3D(T1, b, T3, a);
    Test3 := Orient3D(T1, T2, b, a);
    if ((Test1 = OrderB) or (Test2 = OrderB) or (Test3 = OrderB)) then
    begin
      Result := -1;
    end
    else if ((Test1 = 0) or (Test2 = 0) or (Test3 = 0)) then
    begin // -- a is directly on one edge of t1-t2-t3
      Result := 2;
    end
    else
    begin
      Result := 1;
    end;
  end
  { TODO : is this case possible? }
  // -- b is coplanar with t1-t2-t3 and a is on one side of the triangle
  else // -- if (orderB = 0) then
  begin
    // raise Exception.Create('Pt b is on the edge!!! -> IntersectionEdgeTriangle');
    Test1 := Orient3D(a, T2, T3, b);
    Test2 := Orient3D(T1, a, T3, b);
    Test3 := Orient3D(T1, T2, a, b);
    if ((Test1 = OrderA) or (Test2 = OrderA) or (Test3 = OrderA)) then
    begin
      Result := -1;
    end
    else if ((Test1 = 0) or (Test2 = 0) or (Test3 = 0)) then
    begin
      Result := 3;
    end
    else
    begin
      Result := 1;
    end;
  end;
end;

class function TGBGeomTools.Distance3d(const a, b: TGBPoint3D): double;
begin
  Result := Sqrt(Sqr(a.x - b.x) + Sqr(a.y - b.y) + Sqr(a.z - b.z));
end;

class procedure TGBGeomTools.NormalOppositePt(const a, b, c, pt: TGBPoint3D;
  out n: TGBVector);
var
  v1, v2: TGBVector;
  Temp: TGBVector;
begin
  // -- creation of 2 vectors on the plane formed by a-b-c
  v1 := TGBVector.Create(a, c);
  v2 := TGBVector.Create(a, b);

  // -- we want the normale to point in the direction opposite to pt
  if (Orient3D(a, b, c, pt) <> -1) then
  begin
    Temp := v1;
    v1 := v2;
    v2 := Temp;
  end;

  // -- calculation of the cross-product
  TGBVector.CrossProduct(v1, v2, n);
  n.Normalize;
  v1.Free;
  v2.Free;
end;

class procedure TGBGeomTools.NormalDirectionPt(const a, b, c, pt: TGBPoint3D;
  out n: TGBVector);
var
  v1, v2: TGBVector;
  Temp: TGBVector;
begin
  // -- creation of 2 vectors on the plane formed by a-b-c
  v1 := TGBVector.Create(a, c);
  v2 := TGBVector.Create(a, b);

  // -- we want the normale to point in the direction opposite to pt
  if (Orient3D(a, b, c, pt) = -1) then
  begin
    Temp := v1;
    v1 := v2;
    v2 := Temp;
  end;

  // -- calculation of the cross-product
  TGBVector.CrossProduct(v1, v2, n);
  n.Normalize;
  v1.Free;
  v2.Free;
end;

class function TGBGeomTools.PowerTet(const a, b, c, D, v: TGBPoint3D): Single;
begin
  Inc(Numcall_orient);
  Inc(Numcall_insphere);
{$IFDEF FLOAT}
  Result := -InSphere_value(a, b, c, D, v) / Orient3D_value(a, b, c, D);
{$ELSEIF True}
  Result := -InSphereR(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z, D.x, D.y,
    D.z, v.x, v.y, v.z) / Orient3dR(a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z,
    D.x, D.y, D.z);
{$IFEND}
end;

class function TGBGeomTools.InSphere_value(const a, b, c, D,
  pt: TGBPoint3D): double;
var
  ax, ay, az, A2: double;
  bx, by, bz, B2: double;
  cx, cy, cz, C2: double;
  Dx, Dy, Dz, D2: double;
begin
  ax := a.x - pt.x;
  ay := a.y - pt.y;
  az := a.z - pt.z;
  A2 := Sqr(ax) + Sqr(ay) + Sqr(az);
  bx := b.x - pt.x;
  by := b.y - pt.y;
  bz := b.z - pt.z;
  B2 := Sqr(bx) + Sqr(by) + Sqr(bz);
  cx := c.x - pt.x;
  cy := c.y - pt.y;
  cz := c.z - pt.z;
  C2 := Sqr(cx) + Sqr(cy) + Sqr(cz);
  Dx := D.x - pt.x;
  Dy := D.y - pt.y;
  Dz := D.z - pt.z;
  D2 := Sqr(Dx) + Sqr(Dy) + Sqr(Dz);
  ax := ax * Det3x3(by, bz, B2, cy, cz, C2, Dy, Dz, D2);
  ay := ay * Det3x3(bx, bz, B2, cx, cz, C2, Dx, Dz, D2);
  az := az * Det3x3(bx, by, B2, cx, cy, C2, Dx, Dy, D2);
  A2 := A2 * Det3x3(bx, by, bz, cx, cy, cz, Dx, Dy, Dz);
  Result := (ax - ay + az - A2);
end;

class function TGBGeomTools.Det5x5_perturb(const a, b, c, D, P: TGBPoint3D;
  A2, B2, C2, D2, P2: double): double;
var
  ax, ay, az, One: double;
begin
  ax := a.x * Det4x4(b.y, b.z, B2, 1, c.y, c.z, C2, 1, D.y, D.z, D2, 1, P.y,
    P.z, P2, 1);
  ay := a.y * Det4x4(b.x, b.z, B2, 1, c.x, c.z, C2, 1, D.x, D.z, D2, 1, P.x,
    P.z, P2, 1);
  az := a.z * Det4x4(b.x, b.y, B2, 1, c.x, c.y, C2, 1, D.x, D.y, D2, 1, P.x,
    P.y, P2, 1);
  A2 := A2 * Det4x4(b.x, b.y, b.z, 1, c.x, c.y, c.z, 1, D.x, D.y, D.z, 1, P.x,
    P.y, P.z, 1);
  One := Det4x4(b.x, b.y, b.z, B2, c.x, c.y, c.z, C2, D.x, D.y, D.z, D2, P.x,
    P.y, P.z, P2);
  Result := ax - ay + az - A2 + One;
end;

class function TGBGeomTools.IntersectionLineSphere(const P1, P2, pt1, pt2, pt3,
  Pt4: TGBPoint3D; out T1, T2: double): boolean;
var
  a, b, c: double;
  P3: TGBPoint3D; // -- centre of the sphere
  R2: double; // -- radius of the sphere
  Delta: double;
  re: boolean;
begin
  P3 := TGBPoint3D.Create;
  Self.CircumSphere(pt1, pt2, pt3, Pt4, P3);
  R2 := Sqr(P3.x - pt1.x) + Sqr(P3.y - pt1.y) + Sqr(P3.z - pt1.z);

  a := Sqr(P2.x - P1.x) + Sqr(P2.y - P1.y) + Sqr(P2.z - P1.z);
  b := 2 * ((P2.x - P1.x) * (P1.x - P3.x) + (P2.y - P1.y) * (P1.y - P3.y) +
    (P2.z - P1.z) * (P1.z - P3.z));
  c := Sqr(P3.x) + Sqr(P3.y) + Sqr(P3.z) + Sqr(P1.x) + Sqr(P1.y) + Sqr(P1.z) - 2
    * ((P3.x * P1.x) + (P3.y * P1.y) + (P3.z * P1.z)) - R2;

  re := False;
  Delta := Sqr(b) - (4 * a * c);
  if (abs(Delta) < tolerance) then // -- equals to 0
    re := False //ShowMessage('Trajectory tangent to a sphere. Do nothing')
  else if (Delta > 0) then // -- 2 solution -> intersections
  begin
    re := True;
    T1 := (-b - Sqrt(Delta)) / (2 * a);
    T2 := (-b + Sqrt(Delta)) / (2 * a);
  end;
  P3.Free;
  Result := re;
end;

class function TGBGeomTools.IntersectionLineSphere(const P1, P2: TGBPoint3D;
  const te: TGBTetrahedron; out T1, T2: double): boolean;
var
  re: boolean;
begin
  re := IntersectionLineSphere(P1, P2, te.GetP(1), te.GetP(2), te.GetP(3),
    te.GetP(4), T1, T2);
  Result := re;
end;

class function TGBGeomTools.IntersectionLineSphere_projCentre(const P1,
  P2: TGBPoint3D; const te: TGBTetrahedron): boolean;
var
  P3: TGBPoint3D;
  T: double;
begin
  P3 := TGBPoint3D.Create;
  te.GetCircumCentre(P3);
  T := ((P3.x - P1.x) * (P2.x - P1.x) + (P3.y - P1.y) * (P2.y - P1.y) +
    (P3.z - P1.z) * (P2.z - P1.z)) /
    ((P2.x - P1.x) * (P2.x - P1.x) + (P2.y - P1.y) * (P2.y - P1.y) +
    (P2.z - P1.z) * (P2.z - P1.z));
  P3.Free;
  if (T > 0.0) then
    Result := True
  else
    Result := False;
end;

//-------------------------------------------------------------------------
// =========================== TGBEar =====================================
//-------------------------------------------------------------------------

constructor TGBEar.Create(te1, te2: TGBTetrahedron; v: TGBPoint3D);
begin
  m_v := v;
  Assert((te1 <> nil) and (te2 <> nil));
  Assert(te1 <> te2);
  m_te[1] := te1;
  m_te[2] := te2;
  m_adj[1] := nil;
  m_adj[2] := nil;
  m_adj[3] := nil;
  m_adj[4] := nil;
  m_no := no;
  m_flag := False;
  Inc(no);
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBEar.GetAdj(i: integer): TGBEar;
begin
  Assert((i >= 1) and (i <= 4));
  Result := m_adj[i];
end;

procedure TGBEar.SetAdj(i: integer; ear: TGBEar);
begin
  Assert((i >= 1) and (i <= 4));
  Assert(ear <> nil);
  m_adj[i] := ear;
end;

function TGBEar.GetTetra(i: integer): TGBTetrahedron;
begin
  Assert((i >= 1) and (i <= 2));
  Result := m_te[i];
end;

procedure TGBEar.SetTetra(i: integer; const te: TGBTetrahedron);
begin
  Assert((i >= 1) and (i <= 2));
  Assert(te <> nil);
  m_te[i] := te;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBEar.IsConvex: integer;
var
  i: integer;
  j: integer;
begin
  m_te[1].SetIndex(m_v);
  m_te[2].SetIndex(m_te[1]);
  i := TGBGeomTools.Orient3D(m_te[1].GetProt(1), m_te[1].GetProt(2),
    m_te[1].GetProt(3), m_v);
  j := TGBGeomTools.Orient3D(m_te[1].GetProt(1), m_te[1].GetProt(2),
    m_te[1].GetProt(3), m_te[2].GetProt(0));

  // -- check if the first ear is formed by a flat tetra, if it's the case then
  // -- switch the ear & the problem is solved; unless the 2 ears are formed by
  // -- flat tetra.
  if ((i = 0) or (j = 0)) then
  begin
    m_te[2].SetIndex(m_v);
    m_te[1].SetIndex(m_te[2]);
    i := TGBGeomTools.Orient3D(m_te[2].GetProt(1), m_te[2].GetProt(2),
      m_te[2].GetProt(3), m_v);
    j := TGBGeomTools.Orient3D(m_te[2].GetProt(1), m_te[2].GetProt(2),
      m_te[2].GetProt(3), m_te[1].GetProt(0));
  end;

  if ((i = 0) or (j = 0)) then
  begin
    Result := 0;
  end
  else
  begin
    if (i = j) then
      Result := 1
    else
      Result := -1;
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;


function TGBEar.IsDelaunay(const lstPts: TList): boolean;
var
  i: integer;
  r: boolean;
  tempt: TGBPoint3D;
  tempTe: TGBTetrahedron;
  circum: TGBPoint3D;
  radius, distance: double;
begin
  r := True;
  m_te[1].SetIndex(m_v);
  m_te[2].SetIndex(m_te[1]);
  if (TGBGeomTools.Orient3D(m_te[1].GetProt(1), m_te[1].GetProt(2),
    m_te[1].GetProt(3), m_te[2].GetProt(0)) = 1) then
  begin
    tempTe := TGBTetrahedron.Create(m_te[1].GetProt(1), m_te[1].GetProt(2),
      m_te[1].GetProt(3), m_te[2].GetProt(0));
  end
  else
  begin
    tempTe := TGBTetrahedron.Create(m_te[1].GetProt(2), m_te[1].GetProt(1),
      m_te[1].GetProt(3), m_te[2].GetProt(0));
  end;

  for i := 0 to (lstPts.Count - 1) do
  begin
    tempt := lstPts[i];
    if (tempTe.HasPoint(tempt) = False) then
    begin
{$IFDEF PERTURB}
      if (TGeomTools.InSphere_perturb(tempTe, tempt) = True) then
{$ELSEIF True}
      if (TGBGeomTools.InSphere(tempTe, tempt) = True) then
{$IFEND}
      begin
        { Circum := TGBPoint3D.Create;
          TGeomTools.CircumSphere(tempTe, circum);
          Radius := TGeomTools.Distance3d(circum, tempTe.GetP(1));
          Distance := TGeomTools.Distance3d(circum, tempt);
          ShowMessage(FloatToStr(radius - distance));
        }
        r := False;
        break;
      end;
    end;
  end;
  tempTe.Free;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  Result := r;
end;


function TGBEar.IsFlippable: integer;
var
  P: TGBPoint3D;
  q: TGBPoint3D;
begin
  P := m_te[1].SetIndex(m_te[2]).GetProt(0);
  q := m_te[2].SetIndex(m_te[1]).GetProt(0);
  Result := TGBGeomTools.IntersectionEdgeTriangle(P, q, m_te[1].GetProt(1),
    m_te[1].GetProt(2), m_te[1].GetProt(3));
end;


function TGBEar.Compare(const te1, te2: TGBTetrahedron): boolean;
begin
  if (((te1 = m_te[1]) and (te2 = m_te[2])) or
    ((te1 = m_te[2]) and (te2 = m_te[1]))) then
    Result := True
  else
    Result := False;
end;

function TGBEar.GetVertex: TGBPoint3D;
begin
  Result := m_v;
end;

function TGBEar.Is3ear(out adjEar1, adjEar2: TGBEar): boolean;
var
  te, adj: TGBTetrahedron;
  commonPt: TGBPoint3D;
  i: integer;
  Temp: TGBTetrahedron;
  re: boolean;
  tempear: TGBEar;
begin
  te := m_te[1];
  adj := m_te[2];
  commonPt := adj.SetIndex(te).GetProt(0);
  te.SetIndex(adj);
  re := False;
  Temp := nil;
  for i := 1 to 3 do
  begin
    if (te.GetProt(i) <> m_v) then
    begin
      Temp := te.GetTrot(i);
      if (Temp <> nil) then
      begin
        if (Temp.HasPoint(commonPt) = True) then
        begin
          re := True;
          break;
        end;
      end;
    end;
  end;
  if (re = True) then
  begin
    adjEar1 := nil;
    adjEar2 := nil;
    for i := 1 to 4 do
    begin
      tempear := m_adj[i];
      if (tempear.ContainTetra(Temp) = True) then
      begin
        if (adjEar1 = nil) then
          adjEar1 := tempear
        else
          adjEar2 := tempear;
      end;
    end;
    Assert(adjEar1 <> nil);
    Assert(adjEar2 <> nil);
  end;
  Result := re;
end;

function TGBEar.ContainTetra(const te: TGBTetrahedron): boolean;
begin
  if ((te = m_te[1]) or (te = m_te[2])) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

// =============================================================================
// TGBEarp
// =============================================================================

constructor TGBEarp.Create(te1, te2: TGBTetrahedron; v: TGBPoint3D);
begin
  inherited;
  Self.ComputePower;
end;

procedure TGBEarp.ComputePower;
var
  i1, i2: integer;
begin
  if (Self.IsConvex = 1) then
  begin
    i1 := m_te[1].GetIndex;
    i2 := m_te[2].GetIndex;
    m_te[1].SetIndex(m_v);
    m_te[2].SetIndex(m_te[1]);

    m_power := TGBGeomTools.PowerTet(m_te[1].GetProt(1), m_te[1].GetProt(2),
      m_te[1].GetProt(3), m_te[2].GetProt(0), m_v);
    m_te[1].SetIndex(i1);
    m_te[2].SetIndex(i2);
  end
  else
  begin
    m_power := -9999;
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

function TGBEarp.GetPower: double;
begin
  Result := m_power;
end;

procedure TGBEarp.UpdatePower;
begin
  Self.ComputePower;
end;

procedure TGBEar.TestInvariants;
begin
  Assert(m_te[1] <> nil);
  Assert(m_te[2] <> nil);
end;

procedure TGBEarp.SetPower(pow: double);
begin
  m_power := pow;
end;

end.
