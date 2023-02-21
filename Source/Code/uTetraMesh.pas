// -------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
// -------------------------------------------------------------------------

unit uTetraMesh;

interface

uses
  Winapi.OpenGL,
  System.Math,
  System.SysUtils,
  System.Classes,
  System.Contnrs,
  Vcl.Dialogs,

  uObjects3D;

type
  TListz = class(TList)
  public
    procedure Zap;
  end;

type
(*
  Description: TTetraMesh contains the data points and the 'topology table' for
  the Delaunay tetrahedralization. The methods to draw Delaunay,
  Voronoi, highlighted tetrahedrons, etc. are also present. The
  incremental algorithm is used to build a Delaunay tetrahedralization.
  Deleting a single vertex is also possible.
*)
  TTetraMesh = class(TObject)
  private
    m_lstTetra: TList;
    // -- list of all tetra in the net, obtained with traversal
    m_curTe: TGBTetrahedron; // -- the current tetrahedron to navigate in mesh
    m_boundingBox: array [1 .. 2] of TGBPoint3D;
    m_tempPt: TGBPoint3D;
    m_hlVertex: TGBPoint3D;
    m_maxAtt: double;
    m_minAtt: double;
    m_viewpoint: TGBPoint3D; // -- traversal viewpoint
    m_normalIN: TGBVector; // -- normal that defines a IN face for traversal
    m_dlDV: Cardinal; // -- OpenGL display lists for Delaunay/Voronoi/Points
    m_dlExtraPt: Cardinal; // -- OpenGL display list for the extra point
    m_dlHLTetra: Cardinal; // -- OpenGL display list for highlighted tetra
    m_dlHLVertex: Cardinal; // -- OpenGL display list for highlighted vertex
    m_dlHLCell: Cardinal; // -- OpenGL display list for highlighted Voronoi cell
    m_dlIsosurface: Cardinal; // -- OpenGL display list for isosurface
    m_dlPartition: Cardinal; // -- OpenGL display list for separation surface
    m_dlCrust: Cardinal; // -- OpenGL display list for crust
    m_dlDeletion: Cardinal;
    // -- OpenGL display list for 'envelope' around a point
    m_dlInterpol: Cardinal;
    // -- OpenGL display list for 'envelope' around a point
    m_dlSlicePts: Cardinal; // -- OpenGL display list for cells sliced by plane
    m_dlMovingPt: Cardinal;
    // -- OpenGL display list for stuff related to moving point
    // procedure       DrawHLTetra(te: TTetra; drawAdj: boolean);
    // procedure       DrawHLVertex(v: TGBPoint3D; bEdges, bLink: boolean);
    // procedure       DrawDelaunay;
    // procedure       DrawVoronoi;
    // procedure       DrawDataPoints;
    // procedure       DrawIsosurface(lstTr: TList);
    // procedure       DrawPartitionSurface(lstEdges: TList);
    // procedure       DrawCrust(lstTr: TList);
    procedure Optim(te: TGBTetrahedron; lastPt: TGBPoint3D); overload;
    procedure Optim(te: TGBTetrahedron; lastPt: TGBPoint3D; lstFlips, lstNN: TList); overload;
    function Flip(te, adj: TGBTetrahedron; stack: TStack; lstDeleted: TListz): boolean;
    function Flip_remember(te, adj: TGBTetrahedron; stack: TStack;
      lstDeleted: TListz; lstFlips, lstNN: TList): boolean;
    procedure Flip_power(te, adj: TGBTetrahedron);
    procedure Flip__2(te, adj: TGBTetrahedron; stack: TStack; lstDeleted: TListz);
    function Flip23(te0, te1: TGBTetrahedron): TGBTetrahedron;
    procedure Flip32(te0, te1, te3: TGBTetrahedron);
    function Flip44(te, adj, te1, adj1: TGBTetrahedron;
      out flat: TGBTetrahedron): TGBTetrahedron;
    procedure Flip41(pt: TGBPoint3D; te: TGBTetrahedron);
    procedure Flip14(pt: TGBPoint3D; split: TGBTetrahedron);
    function Walk(pt: TGBPoint3D): TGBTetrahedron;
    function WalkAroundVertex(org, target: TGBPoint3D; te: TGBTetrahedron): TGBTetrahedron;
    function WalkAroundEdge(e: TGBEdge; pt: TGBPoint3D): TGBTetrahedron;
    function GetNN_edges_linkofV(gen: TGBPoint3D; v: TGBPoint3D): TList;
    function GetNN_vertices(pt: TGBPoint3D): TList;
    function GetStar_edges(gen: TGBPoint3D): TList;
    procedure GetStar_v_tetra_ear(pt: TGBPoint3D; out lstEar: TListz; out lstPts, lstTe: TList);
    procedure GetStar_v_tetra(pt: TGBPoint3D; out lstTetra, lstPts: TList);
    procedure GetStar_v_ear(pt: TGBPoint3D; out lstEar: TListz; out lstPts: TList; pow: boolean);
    function VolumeVoronoiCell_partial(umbrella: TList): double;
    function VolumeVoronoiCell_modifiedFacesOnly(gen, v: TGBPoint3D; step: integer; lstNN: TList): double;
    procedure ComputeVoronoiFaceAroundEdge(e: TGBEdge; lstPts: TList);
    procedure ComputeVoronoiFaceAroundEdge2(e: TGBEdge; v: TGBPoint3D; lstPts: TList);
    function Find3rdTetra(te0, te1: TGBTetrahedron): TGBTetrahedron;
    procedure FindCommonEdge(te0, te1, te2: TGBTetrahedron; out common1, common2: TGBPoint3D);
    function GetNeighbouringTetraHavingPt(te: TGBTetrahedron; p: TGBPoint3D): TGBTetrahedron;
    function AreTetraConfig44(te, adj: TGBTetrahedron; out te1, adj1: TGBTetrahedron): integer;
    function CheckCollision(te: TGBTetrahedron; pt: TGBPoint3D): boolean;
    (*
     Description: Determine if a point is inside the bounding box
     Input:       pt: the coord of the point
     it uses the global variable m_boundingBox[]
     Output:      TRUE  -> inside
     FALSE -> outside
     *)
    function IsInsideBoundingBox(pt: TGBPoint3D): boolean;
    function DeleteVertex_insphere_perturb(pt: TGBPoint3D; draw: boolean = False): integer;
    (*
     Description: Delete a vertex from the mesh -- with InSphere() method --
     WITHOUT using symbolic perturbation.
     Input:       pt: the vertex to delete
     draw: drawing or not the envelope (a boolean)
     Output:      (the mesh is updated)
     the # of flips needed to delete the vertex
     Remarks:     using the method where a list of TEar is kept up-to-date
     *)
    function DeleteVertex_insphere_no_perturb(pt: TGBPoint3D; draw: boolean = False): integer;
    function IsEarConvex(te, adj: TGBTetrahedron; pt: TGBPoint3D): boolean;
    function IsEarDelaunay(te, adj: TGBTetrahedron; pt: TGBPoint3D; lstPts: TList): boolean;
    function ProcessEar_t(te, adj: TGBTetrahedron; pt: TGBPoint3D; lstPts, lstTetra: TList): boolean;
    function ProcessEar_e(ear: TGBEar; lstPts, lstEar: TList; flatflag: boolean): integer;
    function ProcessEar_e_pert(ear: TGBEar; lstPts, lstEar: TList; flatflag: boolean): integer;
    function ProcessEar_e__2(ear: TGBEar; lstPts, lstEar: TList): integer;
    function ProcessEar_p(ear: TGBEar; lstEar: TListz; lstPts: TList; flag44: boolean): integer;
    function ProcessEar_p__2(ear: TGBEar; lstEar: TListz; lstPts: TList): boolean;
    function ProcessEar_p__clean(ear: TGBEar; lstEar: TListz; lstPts: TList): boolean;
    // -- TEMP ------------------------------------------------------------------
    function ProcessEar3(te, adj: TGBTetrahedron; pt: TGBPoint3D;
      lstPts, lstTetra: TList): boolean;
    function ProcessEar2(te, adj: TGBTetrahedron; pt: TGBPoint3D;
      lstPts, lstTetra: TList): boolean;
    function NbDelaunayEars(lstEar: TListz; lstPts: TList): integer;
    // -- TEMP ------------------------------------------------------------------
    procedure ProcessEar_flip23(ear: TGBEar);
    (*
     Description: Flip32 and update of the ears when a vertex is deleted. It is
     used for a 3-ear (3 facets sharing a common vertex form a 3-ear)
     Input:       ear    : the TEar to flip
     lstEar : the list of all the TEar forming H
     lstPts : all the vertices of H (testing if ear is Delaunay)
     Output:      TRUE  -> flip32 was possible (lstEar is updated)
     FALSE -> flip32 was impossible and no flip was performed.
     Note:        If a flip32 is possible, then 3 2-ears are deleted from the list
     of ears, and the 3 neighbouring ears are updated.
    *)
    function ProcessEar_flip32(ear: TGBEar; lstEar, lstPts: TList): boolean;
    function ProcessEar_flip32__2(ear: TGBEar; lstEar, lstPts: TList;
      flag: boolean): boolean;
    function ProcessEar_flip32_p(ear: TGBEar; lstEar: TListz; lstPts: TList): boolean;
    function ProcessEar_flip44(ear: TGBEar; lstEar, lstPts: TList): boolean;
    function ProcessEar_degenerate_e(ear: TGBEar; lstEar, lstPts: TList; flag44: boolean): integer;
    function ProcessEar_degenerate_e__2(ear: TGBEar; lstEar, lstPts: TList): integer;
    function ProcessEar_degenerate_p(ear: TGBEar; lstEar: TListz; lstPts: TList): boolean;
    function ProcessEar_degenerate_p__2(ear: TGBEar; lstEar: TListz; lstPts: TList; flag44: boolean): integer;
    function ProcessEar_degenerate_p__3(ear: TGBEar; lstEar: TListz; lstPts: TList): boolean;
    function Delete_unflip(lstEar: TList; lstPts: TList; lastProcessed: TGBEar): TGBEar;
    procedure Move_BuildListReal(out lstReal: TList; lstTe, lstNN: TList; pt: TGBPoint3D);
    function Move_GetClosest_t(lstReal: TList; lstEar: TListz;
      pt, pt2: TGBPoint3D; out nearte: TGBTetrahedron; out nearear: TGBEar;
      out tmin: double): boolean;
    function Move_Flip32_ear(ear: TGBEar; lstEar: TList): TGBTetrahedron;
    function Move_IsBehindTetraIncidentLink(behind: TGBTetrahedron;
      pt: TGBPoint3D): boolean;
    function Move_FlippabilityTest(pt, pt2: TGBPoint3D; adj: TGBTetrahedron;
      t: double): boolean;
    function GetMinList(lst: TList; pt: TGBPoint3D): TGBTetrahedron;
    function GetMaxList(lst: TList): TGBEar;
    function NextInList(i, Count: integer): integer;
    procedure TestAllTetraOrient3D;
    procedure TestAllTetraInSphere;
    procedure TestAllTetraFlat;
    procedure TestTopology;
    procedure TestEars(lstEar: TList);
  public
    m_lstVertex: TList; // -- list of all vertices in the mesh
    bDrawDelaunay: boolean;
    bDrawVoronoi: boolean;
    bDrawPoints: boolean;
    bDrawIso: boolean;
    bDrawPartition: boolean;
    bDrawCrust: boolean;
    bDrawDeletion: boolean;
    bDrawCHOnly: boolean;
    bDrawCHFaces: boolean;
    bDrawSlicePts: boolean;
    bDrawPtsOnCH: boolean;
    bDrawInterpol: boolean;
    bDrawExtraPt: boolean;
    bDrawHLCell: boolean;
    bHLTetra: boolean;
    bHLVertex: boolean;
    bDrawMovingPt: boolean;
    bOptim: boolean;
    constructor Create(v1, v2, v3, v4, bb1, bb2: TGBPoint3D);
    destructor Destroy; override;
   (*
   Description: Insert a point in the mesh (and optimize the mesh to have a
   Delaunay mesh in 3D)
   Input:       x-y-z : the coord of the point to insert
   Output:      -
   Remarks:     This program uses the incremental algorithm to build the
   Delaunay tetra.  This function simply finds the good tetra to
   insert the point, split this tetra in 4 new tetra and after
   calls the function Optim to transform the mesh in a Delaunay mesh
   *)
    procedure InsertPoint(x, y, z: double; Data: double = 0.0);
    procedure InsertPoint_power(x, y, z: double; Data: double = 0.0);
    procedure InsertDelete(x, y, z: double);
    /// ///////////////////////////////////////////
    // procedure       TestDrawNode(pt: TGBPoint3D);
    /// ///////////////////////////////////////////
    // procedure       DrawHLCell(pt: TGBPoint3D);
    /// /////////////////////////////////////////////////
    // procedure       MovePointTo(pt: TGBPoint3D; x, y, z: double);
    /// ///////////////////
    function DellOutside(lst: TList; plane: char; mp: double;
      less: boolean): TList;
    function DellOutsideBox(lst: TList): TList;
    function IsAllInsideBoundingBox(lpt: TList): boolean;
    function IsAllOutsideBoundingBox(lpt: TList): boolean;
    function CreateVorCell(pt: TGBPoint3D): integer;
    function CreateALLVorCell: integer;
    function DeleteVertex_insphere(pt: TGBPoint3D; draw: boolean = False): integer;
    function DeleteVertex_tetra(pt: TGBPoint3D): integer;
    procedure DeleteVertex_optimal(pt: TGBPoint3D; dlimit: integer);
    function DeleteVertex_ear__2(pt: TGBPoint3D; draw: boolean = False): integer;
    function DeleteVertex_power(pt: TGBPoint3D; draw: boolean = False): integer;
    function DeleteVertex_power__2(pt: TGBPoint3D; draw: boolean = False): integer;
    function DeleteVertex_power__clean(pt: TGBPoint3D; draw: boolean = False): double;
    function GetNearestNeighbour(x, y, z: double): TGBPoint3D;
    function Interpolate_nearest(x, y, z: double): double;
    function Interpolate_nn(x, y, z: double): double;
    function Interpolate_linear(x, y, z: double): double;
    function Interpolate_idw(x, y, z, radius: double): double;
    procedure CrossValidation(method: integer; out real, estim: array of double);
    procedure SliceInterpolation(method: integer; gridSize: integer;
      zPos: double; out estimates: array of double; radius: double = 0.0);
    // procedure       Draw; override;
    // procedure       BuildDisplayList;
    // procedure       SetHLTetra(x, y, z: double; neighbor: integer; drawAdj: boolean); overload;
    // procedure       SetHLTetra(te: TTetra; drawAdj: boolean); overload;
    // procedure       SetHLVertex(v: TGBPoint3D; bEdges, bLink: boolean);
    // procedure       SetHLCell(pt: TGBPoint3D);
    // procedure       SetExtraPt(x, y, z: double);
    // procedure       SetEnvelope(pt: TGBPoint3D; te, adj: TTetra; lstTetra, lstPts: TList); overload;
    // procedure       SetEnvelope(v: TGBPoint3D; ear: TEar; lstEar, lstPts: TList); overload;
    // procedure       SetSlicePts(bDrawCells: boolean);
    // procedure       SetMovingPt(pt1, pt2: TGBPoint3D);
    // procedure       SetPartitionSurface(lstEdges: TList);
    // procedure       DrawSliceInterpolation(out estimates: array of double; gridSize: integer; zPos: double);
    // procedure       DrawSliceInterpolationStartEnd(start: boolean);
    function GetHLPt: TGBPoint3D;
    procedure SetCrust(lstTr: TList);
    procedure ResetFlagAllPts;
    procedure SetIsosurface(lstTr: TList);
    function GetVolumeVoronoiCell(gen: TGBPoint3D): double;
    function IsOutsideCH(te: TGBTetrahedron): boolean;
    function IsBoundaryCH(pt: TGBPoint3D): boolean;
    function IsBigTetra(pt: TGBPoint3D): boolean;
    procedure Traverse;
    function GetListPts: TList;
    function GetListTetra: TList;
    function GetTempPt: TGBPoint3D;
    function GetAvgDegree: double;
    function GetNbPts: integer;
    function GetNbPtsOnCH: integer;
    function GetMinValueAttribute: double;
    function GetMaxValueAttribute: double;
    function GetDegree(pt: TGBPoint3D): integer;
    function GetNbEdgeInStar(pt: TGBPoint3D): integer;
    function GetNbTetraInStar(pt: TGBPoint3D): integer;
    (*
     Description: Test the "invariants" of the class TTetraMesh.  It means this function
     tests if the topology is correct, if every tetra are correctly
     oriented and if the circum-sphere of every tetra is empty.  This
     is to be sure that the mesh is a Delaunay mesh.
     Input:       -
     Output:      - (if something is wrong, an exception is thrown)
    *)
    procedure TestInvariants;
  end;

type
  TTetraMesh_IO = class
  private
    m_mesh: TTetraMesh;
    procedure SaveCurve(times: array of double; nb: integer);
  public
    constructor Create(mesh: TTetraMesh);
    (*
     Description: Save the point in a text file
     Input:       (the TextFile, by reference)
     Output:      the TextFile is 'updated' with the points and their attributes
     one line per point: x y z att
     Remarks:     The 4 points of the BigTetra are not saved.
    *)
    procedure SavePoints(var f: TextFile);
    procedure SaveTopology(var f: TextFile);
    procedure ReadPoints(var f: TextFile);
  end;

type
  (*
   Class:  TGBCrust for Isosurface calculation
   Description: TGBCrust is supposed to automatically extract the exterior surface
   of a cloud of points in 3d using Delaunay. Here the extension of
   the 2d crust extraction is used; but it doesn't really work for
   the moment...
   *)
  TGBCrust = class
  private
    m_mesh: TTetraMesh;
    procedure ComputeCrust(lstTr: TList);
  public
    constructor Create(mesh: TTetraMesh);
    procedure ExtractCrust;
  end;

var
  MyPol: TGBPolyhedron;
  m_mesh: TTetraMesh;

// ===========================================================================
implementation
// ===========================================================================

function Initialize: double; stdcall;
  external 'PREDICATES.DLL' Name 'Initialize';

procedure TListz.Zap;
var
  temp: TObject;
begin
  while Count > 0 do
  begin
    temp := Items[0];
    temp.Free;
    Delete(0);
  end;
end;

//------------------------------------------
//---------- TGBTetraMesh ------------------
//------------------------------------------
constructor TTetraMesh.Create(v1, v2, v3, v4, bb1, bb2: TGBPoint3D);
var
  te: TGBTetrahedron;
begin
  // -- creation of the big triangle with neighbors set to NIL
  m_lstVertex := TList.Create;
  m_lstVertex.Add(TGBPoint3D.Create(True, v1.x, v1.y, v1.z, 0));
  m_lstVertex.Add(TGBPoint3D.Create(True, v2.x, v2.y, v2.z, 0));
  m_lstVertex.Add(TGBPoint3D.Create(True, v3.x, v3.y, v3.z, 0));
  m_lstVertex.Add(TGBPoint3D.Create(True, v4.x, v4.y, v4.z, 0));

  te := TGBTetrahedron.Create(m_lstVertex[0], m_lstVertex[1], m_lstVertex[2],
    m_lstVertex[3]);
  m_lstTetra := TList.Create;
  m_curTe := te;

  // -- stuff for scan3D
  m_viewpoint := TGBPoint3D.Create(0.5, 0.5, -150);
  m_normalIN := TGBVector.Create(0, 0, 1);

  // -- stock the coord of the bounding box to test if a new pt is inside it
  m_boundingBox[1] := TGBPoint3D.Create(bb1.x, bb1.y, bb1.z);
  m_boundingBox[2] := TGBPoint3D.Create(bb2.x, bb2.y, bb2.z);

  m_maxAtt := -999;
  m_minAtt := 999;
  m_tempPt := TGBPoint3D.Create;

  bOptim := True;
  {
    //-- display lists

    bDrawDelaunay := false;
    bDrawCHOnly := true;
    bDrawCHFaces := false;
    bDrawPtsOnCH := false;
    bDrawVoronoi := false;
    bDrawPoints := false;
    bDrawIso := false;
    bDrawPartition := false;
    bDrawCrust := false;
    bDrawDeletion := false;
    bDrawInterpol := false;
    bDrawHLCell := false;
    bHLTetra := false;
    bHLVertex := false;
    bDrawExtraPt := false;
    bDrawSlicePts := false;
    bDrawMovingPt := false;
  }
  // -- initialize the variables in the predicates.dll for robust arithmetic
  Initialize;
end;

destructor TTetraMesh.Destroy;
var
  i: integer;
begin
  self.Traverse;
  // -- free every triangle in the lstTr
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    TGBTetrahedron(m_lstTetra[i]).Free;
  end;
  m_lstTetra.Free;

  // -- free every points in the m_lstVertex
  for i := 0 to (m_lstVertex.Count - 1) do
  begin
    TGBPoint3D(m_lstVertex[i]).Free;
  end;
  m_lstVertex.Free;

  // -- free the bounding box
  m_boundingBox[1].Free;
  m_boundingBox[2].Free;

  // -- free the m_tempPt
  m_tempPt.Free;

  m_normalIN.Free;
  m_viewpoint.Free;

  { //-- free the display lists
    glDeleteLists(m_dlDV, 3);
    glDeleteLists(m_dlHLTetra, 1);
    glDeleteLists(m_dlHLVertex, 1);
    glDeleteLists(m_dlHLCell, 1);
    glDeleteLists(m_dlIsosurface, 1);
    glDeleteLists(m_dlPartition, 1);
    glDeleteLists(m_dlCrust, 1);
    glDeleteLists(m_dlDeletion, 1);
    glDeleteLists(m_dlInterpol, 1);
    glDeleteLists(m_dlSlicePts, 1);
  }
  inherited Destroy;
end;


// ******************************************************************************
// Description: Draw the frame of every tetrahedron in the mesh.
// Input:       -
// Output:      -
// Remarks:     Each edge is drawn many times: 3 times for each tetra and
// other times depending on the degree of an edge
// ******************************************************************************

{
  procedure TTetraMesh.DrawDelaunay;
  var
  i, j: integer;
  total: integer;
  tempte: TTetra;
  tempt: TGBPoint3D;
  nb: integer;
  begin
  total := m_lstTetra.Count;
  if (bDrawCHOnly = true) then
  begin
  for i := 0 to (total - 1) do
  begin
  tempte := m_lstTetra[i];
  if (self.IsOutsideCH(tempte) = false) then
  begin
  TDrawTools.DrawTetraFrame(tempte, 0, 0, 0.7, 2);
  end
  else
  begin
  if (bDrawCHFaces = true) then
  begin
  nb := 0;
  tempt := nil;
  for j := 1 to 4 do
  begin
  if (self.IsBigTetra(tempte.GetP(j)) = true) then
  begin
  tempt := tempte.GetP(j);
  inc(nb);
  end;
  end;
  Assert(nb <> 0);
  if (nb = 1) then
  begin
  tempte.SetIndex(tempt);
  TDrawTools.DrawTriangleFace_facingPT(tempte.GetProt(1),
  tempte.GetProt(2),
  tempte.GetProt(3),
  tempt, 0, 0.4, 0.5, 1);
  end;
  end;
  end;
  end;
  end
  else //-- (bDrawCH = false) -> draw everything with 'bounding box'
  begin
  for i := 0 to (total - 1) do
  TDrawTools.DrawTetraFrame(m_lstTetra[i], 0, 0, 0.7, 2);
  end;
  end;
}

// ******************************************************************************
// Description: Draw each segment of the Voronoi diagram
// Input:       -
// Output:      -
// Remarks:     It is impossible with this method to draw only one voronoi cell.
// They are all drawn here... The mesh is scanned and the segment
// between the circumcenter of adjacant tetra is drawn (so each
// segment is actually drawn twice).
// ******************************************************************************
{
  procedure TTetraMesh.DrawVoronoi;
  var
  i: integer;
  j: integer;
  te: TTetra;
  adj: TTetra;
  centreTe: TGBPoint3D;
  centreAdj: TGBPoint3D;
  begin
  centreTe := TGBPoint3D.Create;
  centreAdj := TGBPoint3D.Create;
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
  te := m_lstTetra[i];
  TGeomTools.CircumSphere(te, centreTe);
  for j := 1 to 4 do
  begin
  adj := te.GetT(j);
  if (adj <> nil) then
  begin
  TGeomTools.CircumSphere(adj, centreAdj);
  TDrawTools.DrawLine(centreTe, centreAdj, 1, 0, 0, 2);
  end;
  end;
  end;
  centreTe.Free;
  centreAdj.Free;
  end;
}

{
  procedure TTetraMesh.DrawSliceInterpolation(out estimates: array of double; gridSize: integer; zPos: double);
  var
  spacing: double;
  i, j: integer;
  value: double;
  x, y: double;
  p1, p2, p3, p4: TGBPoint3D;
  deltaAtt: double;
  scaling: double;
  begin
  //  glDeleteLists(m_dlInterpol, 1);
  //  m_dlInterpol := glGenLists(1);
  //  glNewList(m_dlInterpol, GL_COMPILE);
  spacing := 1 / gridSize;
  //-- draw the grid surface
  p1 := TGBPoint3D.Create(0, 0, zPos);
  p2 := TGBPoint3D.Create(0, 0, zPos);
  p3 := TGBPoint3D.Create(0, 0, zPos);
  p4 := TGBPoint3D.Create(0, 0, zPos);
  deltaAtt := m_maxAtt - m_minAtt;
  for i := 0 to (gridSize - 1) do
  begin
  for j := 0 to (gridSize - 1) do
  begin
  p1.x := i * spacing;
  p1.y := j * spacing;
  p2.x := (i * spacing) + spacing;
  p2.y := j * spacing;
  p3.x := (i * spacing) + spacing;
  p3.y := (j * spacing) + spacing;
  p4.x := i * spacing;
  p4.y := (j * spacing) + spacing;
  value := estimates[i * gridSize + j];
  if (value = -1.0) then //-- outside the
  begin
  TDrawTools.DrawQuad(p1, p2, p3, p4, 0, 0, 0, 0.0);
  end
  else
  begin
  scaling := (value - m_minAtt) / deltaAtt;
  TDrawTools.DrawQuad(p1, p2, p3, p4, scaling, 1 - scaling, 1 - scaling, 0.9);
  end;
  end;
  end;
  //  glEndList;
  end;
}

procedure TTetraMesh.InsertPoint(x, y, z, Data: double);
var
  splitTe: TGBTetrahedron;
  pt: TGBPoint3D;
begin
  m_tempPt.SetCoord(x, y, z);
  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create('Point is outside the bounding box');
  end;
  splitTe := Walk(m_tempPt);
  // -- check collision with an existing data point
  if (CheckCollision(splitTe, m_tempPt) = True) then
  begin
    raise Exception.Create('Insertion impossible : this point already exists');
  end;
  // -- insertion of the point with a flip14
  pt := TGBPoint3D.Create(True, x, y, z, Data);
  m_lstVertex.Add(pt);
  if (Data > m_maxAtt) then
    m_maxAtt := Data;
  if (Data < m_minAtt) then
    m_minAtt := Data;
  Flip14(pt, splitTe);

  // -- check if the new tetra (and their neighbors) are Delaunay tetra
  if (bOptim = True) then
  begin
    Optim(splitTe, pt);
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;


function TTetraMesh.IsInsideBoundingBox(pt: TGBPoint3D): boolean;
begin
  if ((pt.x >= m_boundingBox[1].x) and (pt.x <= m_boundingBox[2].x) and
    (pt.y >= m_boundingBox[1].y) and (pt.y <= m_boundingBox[2].y) and
    (pt.z >= m_boundingBox[1].z) and (pt.z <= m_boundingBox[2].z)) then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

function TTetraMesh.IsAllInsideBoundingBox(lpt: TList): boolean;
var
  re: boolean;
  i: integer;
begin
  re := True;
  for i := 0 to lpt.Count - 1 do
  begin
    re := (re) and (IsInsideBoundingBox(lpt[i]));
  end;
  Result := re;
end;

function TTetraMesh.IsAllOutsideBoundingBox(lpt: TList): boolean;
var
  re: boolean;
  i: integer;
begin
  re := True;
  for i := 0 to lpt.Count - 1 do
  begin
    re := (re) and (not(IsInsideBoundingBox(lpt[i])));
  end;

  Result := re;

end;

// ******************************************************************************
// Description: to check if every tetrahedra in the mesh are Delaunay tetra
// (if the sphere formed by the tetra is empty)

// Input:       te : the tetra that has been split by the new point
// lastPt : the last inserted in the mesh
// lstFlips: list for order of flips
// lstNN: list of nn of lastPt
// Output:      - (the mesh is a Delaunay mesh)

// Remarks:     here's the algorithm:
// pop the 4 new tetra and check with the opposite tetra if it's
// Delaunay, if not flip and put the modified tetra on the stack.
// Continue until the stack is empty.
// There's 2 kind of flip: 1- flip23 (it creates a new tetra)
// 2- flip23 (it deletes a tetra).  Finally, in some cases a flip
// is impossible because another tetra "blocks" the flip; in this
// case do nothing cause the problem will be solved by another flip
// ******************************************************************************
procedure TTetraMesh.Optim(te: TGBTetrahedron; lastPt: TGBPoint3D;
  lstFlips, lstNN: TList);
var
  stack: TStack;
  current: TGBTetrahedron;
  adj: TGBTetrahedron;
  lstDeleted: TListz;
  nbFlips: integer;
begin
  nbFlips := 0;
  // -- push the 4 new triangles on the stack
  stack := TStack.Create;
  stack.Push(te);
  stack.Push(te.GetT(2));
  stack.Push(te.GetT(3));
  stack.Push(te.GetT(4));

  // -- to keep track of the deleted tetra (with Flip32)
  lstDeleted := TListz.Create;
  while (stack.Count > 0) do
  begin
    current := stack.Pop;
    // -- check if the tetra has been previously deleted by another flip
    if (lstDeleted.IndexOf(current) = -1) then
    begin
      // -- find the triangle to do the test (the opposite of the lastPt)
      // -- if the triangle doesn't contain lastPt, don't process it
      if (current.HasPoint(lastPt) = True) then
      begin
        adj := current.SetIndex(lastPt).GetTrot(0);
        if (adj <> nil) then
        begin
          adj.SetIndex(current);
          if (TGBGeomTools.InSphere(current, adj.GetProt(0)) = True) then
          begin
            if (lstFlips = nil) then
            begin
              if (Flip(current, adj, stack, lstDeleted) = True) then
                Inc(nbFlips);
              // Flip__2(current, adj, stack, lstDeleted);
            end
            else
              Flip_remember(current, adj, stack, lstDeleted, lstFlips, lstNN);
          end;
        end;
      end;
    end;
  end;
  FreeAndNil(stack);
  // Form1.Edit3.Text := IntToStr(nbFlips);
  lstDeleted.Zap; // -- delete the "deleted tetra" from lstTetra
  lstDeleted.Free;
end;


// ******************************************************************************
// Description: to check if every tetrahedra in the mesh are Delaunay tetra
// (if the sphere formed by the tetra is empty)

// Input:       te : the tetra that has been split by the new point
// lastPt : the last inserted in the mesh
// Output:      - (the mesh is a Delaunay mesh)

// Remarks:     here's the algorithms:
// pop the 4 new tetra and check with the opposite tetra if it's
// Delaunay, if not flip and put the modified tetra on the stack.
// Continue until the stack is empty.
// There's 2 kind of flip: 1- flip23 (it creates a new tetra)
// 2- flip23 (it deletes a tetra).  Finally, in some cases a flip
// is impossible because another tetra "blocks" the flip; in this
// case do nothing cause the problem will be solved by another flip
// ******************************************************************************
procedure TTetraMesh.Optim(te: TGBTetrahedron; lastPt: TGBPoint3D);
var
  stack: TStack;
  current: TGBTetrahedron;
  adj: TGBTetrahedron;
  lstDeleted: TListz;
  nbFlips: integer;
begin
  nbFlips := 0;
  // -- push the 4 new triangles on the stack
  stack := TStack.Create;
  stack.Push(te);
  stack.Push(te.GetT(2));
  stack.Push(te.GetT(3));
  stack.Push(te.GetT(4));

  // -- to keep track of the deleted tetra (with Flip32)
  lstDeleted := TListz.Create;
  while (stack.Count > 0) do
  begin
    current := stack.Pop;
    // -- check if the tetra has been previously deleted by another flip
    if (lstDeleted.IndexOf(current) = -1) then
    begin
      // -- find the triangle to do the test (the opposite of the lastPt)
      // -- if the triangle doesn't contain lastPt, don't process it
      if (current.HasPoint(lastPt) = True) then
      begin
        adj := current.SetIndex(lastPt).GetTrot(0);
        if (adj <> nil) then
        begin
          adj.SetIndex(current);
          if (TGBGeomTools.InSphere(current, adj.GetProt(0)) = True) then
          begin
            if (Flip(current, adj, stack, lstDeleted) = True) then
              Inc(nbFlips);
            // Flip__2(current, adj, stack, lstDeleted);
          end;
        end;
      end;
    end;
  end;
  FreeAndNil(stack);
  // Form1.Edit3.Text := IntToStr(nbFlips);
  lstDeleted.Zap; // -- delete the "deleted tetra" from lstTetra
  lstDeleted.Free;
end;


// ******************************************************************************
// Description: to "flip" 2 non-Delaunay tetra. It creates a new tetrahedron

// Input:       te0, te1: the 2 tetrahedra
// Output:      the new tetra created
// ******************************************************************************
function TTetraMesh.Flip23(te0, te1: TGBTetrahedron): TGBTetrahedron;
var
  i: integer;
  pos: integer;
  te0next1: TGBTetrahedron;
  te0next2: TGBTetrahedron;
  te1next1: TGBTetrahedron;
  te1next2: TGBTetrahedron;
  new1: TGBTetrahedron;
  tempPt: TGBPoint3D;
  tempPos: integer;
begin
  // -- store the 2 opposite vertices as 'starting vertex' of each tetra
  te0.SetIndex(te1);
  te1.SetIndex(te0);
  // -- store the 2 adjacent tetra whose neighbours will be modified. there are
  // -- only 2 (the other one won't be modified)
  te0next1 := te0.GetTrot(2);
  te0next2 := te0.GetTrot(3);

  te1next1 := te1.GetT(te1.GetIndex(te0.GetProt(1)));
  te1next2 := te1.GetT(te1.GetIndex(te0.GetProt(2)));
  // -- pos of the point to change in te1
  pos := te1.GetIndex(te0.GetProt(3));

  // -- creation of the new tetra
  new1 := TGBTetrahedron.Create(te0.GetP(1), te0.GetP(2), te0.GetP(3),
    te0.GetP(4));
  new1.SetIndex(te0.GetProt(0)).SetProt(2, te1.GetProt(0));

  // -- update the adj of the adjacent tetrahedra
  if (te0next1 <> nil) then
    te0next1.SetIndex(te0).SetTrot(0, new1);
  if (te0next2 <> nil) then
    te0next2.SetIndex(te0).SetTrot(0, te1);
  if (te1next1 <> nil) then
    te1next1.SetIndex(te1).SetTrot(0, te0);
  if (te1next2 <> nil) then
    te1next2.SetIndex(te1).SetTrot(0, new1);

  // -- update neigbours of te0
  te0.SetTrot(2, new1);
  te0.SetTrot(3, te1);
  te0.SetTrot(0, te1next1);
  // -- update the neighbours of new1
  new1.SetTrot(0, te1next2);
  new1.SetTrot(1, te0);
  new1.SetTrot(2, te0next1);
  new1.SetTrot(3, te1);

  // -- update the adj for te1
  tempPos := te1.GetIndex;
  te1.SetIndex(pos);
  for i := 1 to 3 do
  begin
    tempPt := te1.GetProt(i);
    if (tempPt = te0.GetProt(1)) then
    begin
      te1.SetTrot(i, te0);
    end
    else if (tempPt = te0.GetProt(2)) then
    begin
      te1.SetTrot(i, new1);
    end
    else
    begin
      te1.SetTrot(i, te0next2);
    end;
  end;

  // -- update of points of te0 & te1
  te1.SetIndex(tempPos);
  te0.SetProt(1, te1.GetProt(0));
  te1.SetP(pos, te0.GetProt(0));

  te0.ResetCentre;
  te1.ResetCentre;
  m_curTe := te0;
  Result := new1;
end;

// ******************************************************************************
// Description: Test all the tetra in the mesh to be sure they are correctly
// oriented. Points 1-2-3-4 must have an 'Orient3d()' positive,
// i.e. they must be oriented according to the left-handed rule.
// Input:       -
// Output:      -
// Note:        An exception is raised as soon as one tetra is NOT Orient3d()
// ******************************************************************************
procedure TTetraMesh.TestAllTetraOrient3D;
var
  i: integer;
  t: TGBTetrahedron;
begin
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    t := m_lstTetra[i];
    if (t.IsOrient3D = False) then
    begin
      raise Exception.Create('[Orient3D] Tetra #' + IntToStr(i));
    end;
  end;
end;


// ******************************************************************************
// Description: Locate which tetra in the mesh contains a newly inserted point
// Point Location function.
// Input:       pt : a point (x-y-z)
// Output:      the tetra containing the point
// Note:        m_curTe is also set to this tetra.
// *** Stochastic Walk ***
// ******************************************************************************

function TTetraMesh.Walk(pt: TGBPoint3D): TGBTetrahedron;
var
  found: boolean;
  orient: integer;
  i: integer;
  prev: TGBTetrahedron;
  start: integer;
begin
  // -- if by accident the m_curTe was set to nil when deleting tetra in the
  // -- previous insertion
  if (m_curTe = nil) then
  begin
    m_curTe := m_lstTetra[0]; { TODO : m_curTe in Walk() }
  end;

  // Randomize; //-- fucks with the randomize of insertion of point.
  prev := nil;
  found := False;
  while (found = False) do
  begin
    // -- stochastic walk: the edges are selected in random order
    // start := Ceil(Random * 4);
    // m_curTe.SetIndex(start);
    m_curTe.SetIndex(1);
    for i := 1 to 4 do
    begin
      // -- memory walk: do not check again the same face w/r to pt
      if (m_curTe.GetTrot(0) <> prev) then
      begin
        if ((m_curTe.GetIndex mod 2) = 0) then
        begin
          orient := TGBGeomTools.Orient3D(m_curTe.GetProt(1),
            m_curTe.GetProt(2), m_curTe.GetProt(3), pt);
        end
        else
        begin
          orient := TGBGeomTools.Orient3D(m_curTe.GetProt(1),
            m_curTe.GetProt(3), m_curTe.GetProt(2), pt);
        end;
        if (orient = -1) then
        begin
          prev := m_curTe;
          m_curTe := m_curTe.GetTrot(0);
          found := False;
          break;
        end;
      end;
      if (i = 4) then
        found := True
      else
        m_curTe.RotIndex(1); // -- next vertex in the tetra
    end;
  end;
  Result := m_curTe;
end;

// ******************************************************************************
// Description: Walk around a given vertex in the mesh to find a tetra having
// a particular vertex. It is actually like locating one tetrahedron
// in the mesh incident to an edge.
// Input:       org: the vertex around which we are walking
// dest: the vertex that the tetra must have (besides org)
// te: a tetra incident to e.org, anyway one.
// Output:      one tetra spanned by org and target
// Note:        prerequisite: there must be one such tetra in the mesh
// ******************************************************************************
function TTetraMesh.WalkAroundVertex(org, target: TGBPoint3D;
  te: TGBTetrahedron): TGBTetrahedron;
var
  i, j: integer;
  prev: TGBTetrahedron;
  orient: integer;
  start: integer;
begin
  prev := nil;
  while (True) do
  begin
    Assert(te.HasPoint(org) = True);
    if (te.HasPoint(target) = True) then
    begin
      break;
    end
    else
    begin
      te.SetIndex(org);
      te.RotIndex(1);
      for i := 1 to 3 do
      begin
        // -- memory walk: do not check again the same face w/r to pt
        if (te.GetTrot(0) <> prev) then
        begin
          if ((te.GetIndex mod 2) = 0) then
          begin
            orient := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2),
              te.GetProt(3), target);
          end
          else
          begin
            orient := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(3),
              te.GetProt(2), target);
          end;
          if (orient = -1) then
          begin
            prev := te;
            te := te.GetTrot(0);
            Assert(te.HasPoint(org));
            break;
          end;
        end;
        if (i = 3) then
        begin
          // -- means that no test returned -1 and we already know  that te is not
          // -- what we are looking for... DEGENERATE CASE caused by coplanar points
          // -- solution is to traverse to neighbouring tetra when orient = 0
          te.SetIndex(org);
          te.RotIndex(1);
          for j := 1 to 3 do
          begin
            if (te.GetTrot(0) <> prev) then
            begin
              if ((te.GetIndex mod 2) = 0) then
              begin
                orient := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2),
                  te.GetProt(3), target);
              end
              else
              begin
                orient := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(3),
                  te.GetProt(2), target);
              end;
              if (orient = 0) then
              begin
                prev := te;
                te := te.GetTrot(0);
                Assert(te.HasPoint(org));
                break;
              end;
            end;
            te.RotIndex(1);
          end;
        end
        else
        begin
          te.RotIndex(1);
        end;
      end;
    end;
  end;
  Assert(te.HasPoint(org) = True);
  Assert(te.HasPoint(target) = True);
  Result := te;
end;

// ******************************************************************************
// Description: Given an edge (which has a pointer to an incident tetra), this
// function finds the tetra that has the edge and another vertex.
// Input:       e: the edge
// pt: the vertex that the tetra must have
// Output:      the tetrahedron
// Note:        the tetra MUST exist. this function is used for nn interpolation
// ******************************************************************************
function TTetraMesh.WalkAroundEdge(e: TGBEdge; pt: TGBPoint3D): TGBTetrahedron;
var
  te: TGBTetrahedron;
  a: TGBPoint3D;
  b: TGBPoint3D;
  i: integer;
  org, dest: TGBPoint3D;
  tempt: TGBPoint3D;
begin
  te := e.GetAdjTetra;
  Assert(te <> nil);
  if (te.HasPoint(pt) = False) then
  begin
    org := e.GetOrg;
    dest := e.GetDest;
    Assert(te.HasPoint(org) = True);
    Assert(te.HasPoint(dest) = True);
    te.SetIndex(org);
    if (te.GetProt(1) = dest) then
    begin
      a := te.GetProt(2);
      b := te.GetProt(3);
    end
    else if (te.GetProt(2) = dest) then
    begin
      a := te.GetProt(1);
      b := te.GetProt(3);
    end
    else
    begin
      a := te.GetProt(1);
      b := te.GetProt(2);
    end;

    while (True) do
    begin
      te.SetIndex(a);
      if (te.GetTrot(0) <> nil) then
        te := te.GetTrot(0)
      else
      begin // -- if next tetra is nil, change the sense of rotation
        tempt := a;
        a := b;
        b := tempt;
        te := te.SetIndex(a).GetTrot(0);
      end;
      if (te.HasPoint(pt) = True) then
        break
      else
      begin
        a := b;
        for i := 1 to 4 do
        begin
          if ((te.GetP(i) <> org) and (te.GetP(i) <> dest) and (te.GetP(i) <> a))
          then
          begin
            b := te.GetP(i);
            break;
          end;
        end;
      end;
    end;
  end;
  Assert(te.HasPoint(pt));
  Result := te;
end;

// ******************************************************************************
// Description: Test all the tetra in the mesh to be sure they respect the
// Delaunay criterion
// Input:       -
// Output:      -
// Note:        An exception is raised if the circumsphere of one tetra is not
// empty
// ******************************************************************************
procedure TTetraMesh.TestAllTetraInSphere;
var
  i: integer;
  j: integer;
  te: TGBTetrahedron;
  radius: double;
  distance: double;
  // diff: double;
  circum: TGBPoint3D;
  tempt: TGBPoint3D;
begin
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    te := m_lstTetra[i];
    for j := 0 to (m_lstVertex.Count - 1) do
    begin
      tempt := m_lstVertex[j];
      if (tempt <> te.GetP(1)) and (tempt <> te.GetP(2)) and
        (tempt <> te.GetP(3)) and (tempt <> te.GetP(4)) then
      begin
        if (TGBGeomTools.InSphere(te, tempt) = True) then
        begin
          if (TGBGeomTools.Orient3D(te.GetP(1), te.GetP(2), te.GetP(3),
            te.GetP(4)) = 0) then
          begin
            ShowMessage('mesh contains a (stupid) flat tetra');
          end;
          circum := TGBPoint3D.Create;
          TGBGeomTools.CircumSphere(te, circum);
          radius := TGBGeomTools.Distance3d(circum, te.GetP(1));
          distance := TGBGeomTools.Distance3d(circum, tempt);
          raise Exception.Create('[InSphere] ' + FloatToStr(radius - distance));
        end;
      end;
    end;
  end;
end;


// ******************************************************************************
// Description: flip 3 tetra that are in a certain configuration.  This flip
// deletes a tetra.  It's exactly the opposite of the Flip23

// Input:       the 3 tetra to flip
// Output:      - (te3 is "removed" from the mesh & te0 and te1 have changed)
// ******************************************************************************
procedure TTetraMesh.Flip32(te0, te1, te3: TGBTetrahedron);
var
  common0, common1: TGBPoint3D;
  adj: TGBTetrahedron;
  pos00: integer;
  pos01: integer;
  pos10: integer;
  pos11: integer;
begin
  // -- find the common edge between the 3 tetrahedra
  FindCommonEdge(te0, te1, te3, common0, common1);

  pos00 := te0.GetIndex(common0);
  pos01 := te0.GetIndex(common1);
  pos10 := te1.GetIndex(common0);
  pos11 := te1.GetIndex(common1);

  // -- update the neighbors of the deleted triangle
  adj := te3.SetIndex(common0).GetTrot(0);
  if (adj <> nil) then
  begin
    adj.SetIndex(te3).SetTrot(0, te0);
  end;
  te0.SetIndex(te3).SetTrot(0, adj);
  adj := te3.SetIndex(common1).GetTrot(0);
  if (adj <> nil) then
  begin
    adj.SetIndex(te3).SetTrot(0, te1);
  end;
  te1.SetIndex(te3).SetTrot(0, adj);
  // -- update neighbours of te0 and te1
  te0.SetIndex(te1);
  te1.SetIndex(te0);
  adj := te0.GetT(pos01);
  te1.SetTrot(0, adj);
  if (adj <> nil) then
  begin
    adj.SetIndex(te0).SetTrot(0, te1);
  end;
  te0.SetT(pos01, te1);

  adj := te1.GetT(pos10);
  te0.SetTrot(0, adj);
  if (adj <> nil) then
  begin
    adj.SetIndex(te1).SetTrot(0, te0);
  end;
  te1.SetT(pos10, te0);

  // -- update the vertices of te0 & te1
  te0.SetP(pos00, te1.GetProt(0));
  te1.SetP(pos11, te0.GetProt(0));

  m_curTe := te0;
  te0.ResetCentre;
  te1.ResetCentre;
  te3.ResetCentre;
end;


procedure TTetraMesh.TestInvariants;
begin
  self.Traverse;
  TestTopology;
  TestAllTetraFlat;
  TestAllTetraOrient3D;
  if (bOptim = True) then
  begin
    TestAllTetraInSphere;
  end;
end;

// ******************************************************************************
// Description: Determine if 2 tetra are in a certain configuration in the mesh
// (if they both have another tetra as a neighbor).  If the answer
// is yes, a Flip32 can be performed.

// Input:       2 tetrahedrons that are adjacent one to each other.
// Output:      - the 3rd tetrahedron if it exits - otherwise: NIL
// ******************************************************************************
function TTetraMesh.Find3rdTetra(te0, te1: TGBTetrahedron): TGBTetrahedron;
var
  i: integer;
  temp: TGBTetrahedron;
  third: TGBTetrahedron;
begin
  third := nil;
  te0.SetIndex(te1);
  te1.SetIndex(te0);
  for i := 1 to 3 do
  begin
    temp := te0.GetTrot(i);
    if (temp <> nil) then
    begin
      if (temp.HasPoint(te1.GetProt(0)) = True) then
      begin
        third := temp;
        break;
      end;
    end;
  end;
  Result := third;
end;


// ******************************************************************************
// Description: Find the common edge between 3 tetra that are in the right
// configuration to perform a Flip32

// Input:       the 3 tetra
// Output:      common1 and common2 : the coord of the edge

// Remarks:     the 3 tetrahedrons must be in the configuration
// ******************************************************************************
procedure TTetraMesh.FindCommonEdge(te0, te1, te2: TGBTetrahedron;
  out common1, common2: TGBPoint3D);
begin
  te0.SetIndex(te1);
  if (te2.HasPoint(te0.GetProt(1)) = True) and
    (te2.HasPoint(te0.GetProt(2)) = True) then
  begin
    common1 := te0.GetProt(1);
    common2 := te0.GetProt(2);
  end
  else if (te2.HasPoint(te0.GetProt(2)) = True) and
    (te2.HasPoint(te0.GetProt(3)) = True) then
  begin
    common1 := te0.GetProt(2);
    common2 := te0.GetProt(3);
  end
  else if (te2.HasPoint(te0.GetProt(1)) = True) and
    (te2.HasPoint(te0.GetProt(3)) = True) then
  begin
    common1 := te0.GetProt(1);
    common2 := te0.GetProt(3);
  end;
end;


// ******************************************************************************
// Description: Test the 'topology' of the mesh, i.e. make sure all the
// neighbours of each tetra are coherent.

// Input:       -
// Output:      -

// Note:        An exception is raised as soon as a 'topologic' error is found
// ******************************************************************************
procedure TTetraMesh.TestTopology;
var
  i: integer;
  j: integer;
  te: TGBTetrahedron;
  adj: TGBTetrahedron;
begin
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    te := m_lstTetra[i];
    for j := 1 to 4 do
    begin
      adj := te.GetT(j);
      if (adj <> nil) then
      begin
        adj.SetIndex(te);
        if ((adj.HasNeighbour(te) = False) or
          (te.HasPoint(adj.GetProt(0)) = True) or
          (te.HasPoint(adj.GetProt(1)) = False) or
          (te.HasPoint(adj.GetProt(2)) = False) or
          (te.HasPoint(adj.GetProt(3)) = False)) then
        begin
          raise Exception.Create('Topology problem. #te: ' + IntToStr(te.m_no) +
            '  #adj: ' + IntToStr(adj.m_no));
        end;
      end;
    end;
  end;
end;



// ******************************************************************************
// Description: flip 2 tetra that are not Delaunay.  This function in fact
// determine which flip (Flip23, Flip32 or Flip44) to perform.

// Input:       te and adj : the 2 tetra
// stack and lstDeleted : to keep track of the changes
// Output:      TRUE  : a flip was perfomed
// FALSE : NO flip was performed

// Remarks:     4 cases are possible for the flip.  Let 'p' be the viewpoint,
// a-b-c the coord of the common face of 2 adjacent tetra and 'd'
// the opposite vertex:
{
  case #1:     b
  *
  / \
  /   \          The edge p-d passes through the triangle abc.
  /  *d \         Note that p-abc can be coplanar (this case can
  /       \        happen after a Flip23 where 4 points are
  *---------*       coplanar; see case #3)
  a         b


  case #2:     b
  *
  / \           The edge p-d doesn't pass through the triangle abc.
  /   \ *d       2 cases are then possible : Flip32 if no tetra are
  /     \         "between" the edge p-d.  If there is one tetra
  /       \        between, then no flip is possible
  *---------*
  a         b


  case #3:     b
  *
  / \           4 points are coplanar : p, d and 2 points of the
  /   \          common triangle.  Here again, 2 cases are possible:
  /     \         1- those 2 tetra are in the "configuration44" and
  /  d    \        a Flip44 is possible (Flip44 = Flip23 + Flip32).
  *---*-----*       If the tetra are not in the configuration44, no
  a         b       flip is possible.


  case #4:                  This the same case as #3, but the point p is
  directly on one edge of the triangle a-b-c. It
  basically means that a new vertex was inserted
  directly into an existing edge. The edge must be
  split and that affects all the tetra incident to
  that edge: they must all be split into 2 tetra. If
  the 2 tetra are in Configuration44, a Flip44 must
  be performed; if not a Flip23 must be performed to
  delete the flat tetra a-b-c-p. }
// ******************************************************************************
function TTetraMesh.Flip(te, adj: TGBTetrahedron; stack: TStack;
  lstDeleted: TListz): boolean;
var
  p: TGBPoint3D;
  d: TGBPoint3D;
  intersect: integer;
  newTetra: TGBTetrahedron;
  te3: TGBTetrahedron;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
  flat: TGBTetrahedron;
  done: boolean;
begin
  done := False;
  // -- does the edge pass through the triangle?
  p := te.SetIndex(adj).GetProt(0);
  d := adj.SetIndex(te).GetProt(0);

  // -- do the right flip according to the position of the tetrahedra
  intersect := TGBGeomTools.IntersectionEdgeTriangle(p, d, te.GetProt(1),
    te.GetProt(2), te.GetProt(3));
  // -- case #1 : the edge passes through the triangle
  if (intersect = 1) then
  begin
    newTetra := Flip23(te, adj);
    done := True;
    stack.Push(te);
    stack.Push(adj);
    stack.Push(newTetra);
  end
  // -- case #2 : the edge doesn't pass through the triangle
  else if (intersect = -1) then
  begin
    te3 := Find3rdTetra(te, adj);
    if (te3 <> nil) then
    begin
      // -- case #2 and no other tetra are "between" p and d : Flip32.
      // -- if there is one (or more) tetra between : do NOTHING, the problem
      // -- will be solved somewhere else by flipping another adjacent tetra
      Flip32(te, adj, te3);
      done := True;
      stack.Push(te);
      stack.Push(adj);
      // -- put the deleted tetra in a temporary list
      lstDeleted.Add(te3);
    end;
  end
  // -- case #3 : p, d and 2 points in the triangle are coplanar
  else // -- (intersect = 0) or (intersect = 2)
  begin
    if (AreTetraConfig44(te, adj, te1, adj1) <> 0) then
    begin
      newTetra := Flip44(te, adj, te1, adj1, flat);
      if (te = flat) then
      begin
        stack.Push(adj);
        stack.Push(newTetra);
      end
      else if (adj = flat) then
      begin
        stack.Push(te);
        stack.Push(newTetra);
      end
      else
      begin
        stack.Push(te);
        stack.Push(adj);
      end;
      lstDeleted.Add(flat);
      stack.Push(te1);
      stack.Push(adj1);
    end
    // -- case #4
    else if (intersect = 2) then
    begin
      newTetra := Flip23(te, adj);
      stack.Push(te);
      stack.Push(adj);
      stack.Push(newTetra);
    end;
  end;
  Result := done;
end;


// ******************************************************************************
// Description: Determine which flip (Flip23, Flip32 or Flip44) to perform,
// based on the configuration of 2 tetra. This does the same thing
// as Flip but the sequence of flips is kept in a list, along with
// with a list of the natural neighbours of the last inserted point.

// Input:       te and adj : the 2 tetra
// stack and lstDeleted : to keep track of the changes
// lstFlips: list of edges (to keep track of flips perfomred)
// lstNN: list of nn of the last point inserted in the mesh

// Output:      TRUE  : a flip was perfomed
// FALSE : NO flip was performed
// ******************************************************************************
function TTetraMesh.Flip_remember(te, adj: TGBTetrahedron; stack: TStack;
  lstDeleted: TListz; lstFlips, lstNN: TList): boolean;
var
  p, d: TGBPoint3D;
  intersect: integer;
  newTetra: TGBTetrahedron;
  te3: TGBTetrahedron;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
  flat: TGBTetrahedron;
  done: boolean;
  a, b: TGBPoint3D;
  e: TGBEdge;
  i: integer;
begin
  done := False;
  // -- does the edge pass through the triangle?
  p := te.SetIndex(adj).GetProt(0);
  d := adj.SetIndex(te).GetProt(0);

  // -- do the right flip according to the position of the tetrahedra
  intersect := TGBGeomTools.IntersectionEdgeTriangle(p, d, te.GetProt(1),
    te.GetProt(2), te.GetProt(3));

  // -- case #1 : the edge passes through the triangle
  if (intersect = 1) then
  begin
    newTetra := Flip23(te, adj);
    done := True;
    stack.Push(te);
    stack.Push(adj);
    stack.Push(newTetra);
    // -- create an edge v-pi
    FindCommonEdge(te, adj, newTetra, a, b);
    if (a = p) then
    begin
      e := TGBEdge.Create(p, b, nil);
      lstNN.Add(b);
    end
    else
    begin
      e := TGBEdge.Create(p, a, nil);
      lstNN.Add(a);
    end;
    lstFlips.Add(e);
  end
  // -- case #2 : the edge doesn't pass through the triangle
  else if (intersect = -1) then
  begin
    te3 := Find3rdTetra(te, adj);
    if (te3 <> nil) then
    begin
      Flip32(te, adj, te3);
      done := True;
      stack.Push(te);
      stack.Push(adj);
      // -- put the deleted tetra in a temporary list
      lstDeleted.Add(te3);

      // -- create an edge a-b (the 'exterior' edge)
      a := nil;
      b := nil;
      te.SetIndex(adj);
      for i := 1 to 3 do
      begin
        if (te.GetProt(i) <> p) then
        begin
          if (a = nil) then
            a := te.GetProt(i)
          else
            b := te.GetProt(i);
        end;
      end;
      e := TGBEdge.Create(a, b, nil);
      lstFlips.Add(e);
    end;
  end
  // -- case #3 : p, d and 2 points in the triangle are coplanar
  else if (intersect = 0) then
  begin
    if (AreTetraConfig44(te, adj, te1, adj1) <> 0) then
    begin
      newTetra := Flip23(te, adj);
      stack.Push(te);
      stack.Push(adj);
      stack.Push(newTetra);
      // -- create an edge v-pi
      FindCommonEdge(te, adj, newTetra, a, b);
      if (a = p) then
      begin
        e := TGBEdge.Create(p, b, nil);
        lstNN.Add(b);
      end
      else
      begin
        e := TGBEdge.Create(p, a, nil);
        lstNN.Add(a);
      end;
      lstFlips.Add(e);
    end;
  end
  else // -- (intersect = 2) [intersection directly into an edge]
  begin
    te3 := Find3rdTetra(te, adj);
    if (te3 <> nil) then
    begin
      Flip32(te, adj, te3);
      stack.Push(te);
      stack.Push(adj);
      // -- put the deleted tetra in a temporary list
      lstDeleted.Add(te3);
      // -- create an edge a-b (the 'exterior' edge)
      a := nil;
      b := nil;
      te.SetIndex(adj);
      for i := 1 to 3 do
      begin
        if (te.GetProt(i) <> p) then
        begin
          if (a = nil) then
            a := te.GetProt(i)
          else
            b := te.GetProt(i);
        end;
      end;
      e := TGBEdge.Create(a, b, nil);
      lstFlips.Add(e);
    end
    else
    begin
      newTetra := Flip23(te, adj);
      stack.Push(te);
      stack.Push(adj);
      stack.Push(newTetra);
      // -- create an edge v-pi
      FindCommonEdge(te, adj, newTetra, a, b);
      if (a = p) then
      begin
        e := TGBEdge.Create(p, b, nil);
        lstNN.Add(b);
      end
      else
      begin
        e := TGBEdge.Create(p, a, nil);
        lstNN.Add(a);
      end;
      lstFlips.Add(e);
    end;
  end;
  Result := done;
end;


// ******************************************************************************
// Description: Check if 2 tetra have 2 neighbors so that those 4 tetra are in
// the configuration 44.  If they are, a Flip44 can be performed.

// Input:       - 2 adjacent tetra (where the 2 opposite vertices and 2 points
// forming the shared face are coplanar)
// - (out) 2 pointers to the 2 other tetra in case they effectively
// in Configuration 44

// Output:      0 : NOT in config44
// 1 : YES config44, coplanar with 1 edge of the common facet
// 2 : YES config44, coplanar with 2 edge of the common facet

// Remark:      In the case where 4 pts are coplanar with 2 edges
// ******************************************************************************

function TTetraMesh.AreTetraConfig44(te, adj: TGBTetrahedron;
  out te1, adj1: TGBTetrahedron): integer;
var
  p: TGBPoint3D;
  d: TGBPoint3D;
  r: boolean;
  Count: integer;
  oppPt: TGBPoint3D;
  te3: TGBTetrahedron;
  te4: TGBTetrahedron;
  test1: integer;
  test2: integer;
  test3: integer;
begin
  r := False;
  p := te.SetIndex(adj).GetProt(0);
  d := adj.SetIndex(te).GetProt(0);

  // -- test the 2 points with each pair of vertices of the common face to find
  // -- the 4 coplanar points
  test1 := TGBGeomTools.Orient3D(p, d, te.GetProt(1), te.GetProt(2));
  test2 := TGBGeomTools.Orient3D(p, d, te.GetProt(2), te.GetProt(3));
  test3 := TGBGeomTools.Orient3D(p, d, te.GetProt(1), te.GetProt(3));
  Count := 0;
  if (test1 = 0) then
    Inc(Count);
  if (test2 = 0) then
    Inc(Count);
  if (test3 = 0) then
    Inc(Count);
  Assert(Count < 3);

  // -- with points 1 & 2
  if (test1 = 0) then
  begin
    oppPt := te.GetProt(3);
    te3 := te.SetIndex(oppPt).GetTrot(0);
    te4 := adj.SetIndex(oppPt).GetTrot(0);
    if ((te3.HasNeighbour(te4) = True) and (te4.HasNeighbour(te3) = True)) then
    begin
      te1 := te3;
      adj1 := te4;
      r := True;
    end;
  end;
  // -- with points 2 & 3
  te.SetIndex(adj);
  if ((r = False) and (test2 = 0)) then
  begin
    oppPt := te.GetProt(1);
    te3 := te.SetIndex(oppPt).GetTrot(0);
    te4 := adj.SetIndex(oppPt).GetTrot(0);
    if ((te3.HasNeighbour(te4) = True) and (te4.HasNeighbour(te3) = True)) then
    begin
      te1 := te3;
      adj1 := te4;
      r := True;
    end;
  end;
  // -- with points 3 & 1
  te.SetIndex(adj);
  if ((r = False) and (test3 = 0)) then
  begin
    oppPt := te.GetProt(2);
    te3 := te.SetIndex(oppPt).GetTrot(0);
    te4 := adj.SetIndex(oppPt).GetTrot(0);
    if ((te3.HasNeighbour(te4) = True) and (te4.HasNeighbour(te3) = True)) then
    begin
      te1 := te3;
      adj1 := te4;
      r := True;
    end;
  end;
  if (r = True) then
  begin
    Result := Count;
  end
  else
  begin
    Result := 0;
  end;
end;



// ******************************************************************************
// Description: Determine if a newly inserted point hits an old data point in
// the mesh.  A certain tolerance is used

// Input:       te : the tetra found by the Walk function
// pt : the coord of the new point

// Output:      TRUE  -> the new point hits one of the data points already there
// FALSE -> the new point doesn't hit
// ******************************************************************************
function TTetraMesh.CheckCollision(te: TGBTetrahedron; pt: TGBPoint3D)
  : boolean;
const
  tolerance: double = 1E-12;
var
  i: integer;
begin
  Result := False;
  for i := 1 to 4 do
  begin
    if (pt.Hit(te.GetP(i), tolerance) = True) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TTetraMesh.GetListPts: TList;
begin
  Result := m_lstVertex;
end;

function TTetraMesh.GetListTetra: TList;
begin
  Result := m_lstTetra;
end;

{
  procedure TTetraMesh.Draw;
  begin
  inherited;
  //-- Delaunay/Voronoi/Data Points
  if (bHLVertex = true) then
  begin
  glCallList(m_dlHLVertex);
  end;

  if (bDrawDelaunay = true) then
  begin
  glCallList(m_dlDV);
  end;
  if (bDrawVoronoi = true) then
  begin
  glCallList(m_dlDV + 1);
  end;
  if (bDrawPoints = true) then
  begin
  glCallList(m_dlDV + 2);
  end;
  if (bDrawExtraPt = true) then
  begin
  glCallList(m_dlExtraPt);
  end;

  //-- highlighted features
  if (bHLTetra = true) then
  begin
  glCallList(m_dlHLTetra);
  end;
  if (bDrawHLCell = true) then
  begin
  glCallList(m_dlHLCell);
  end;
  if (bDrawSlicePts = true) then
  begin
  glCallList(m_dlSlicePts);
  end;

  //-- isosurface
  if (bDrawIso = true) then
  begin
  glCallList(m_dlIsosurface);
  end;

  //-- split surface
  if (bDrawPartition = true) then
  begin
  glCallList(m_dlPartition);
  end;

  //-- crust
  if (bDrawCrust = true) then
  begin
  glCallList(m_dlCrust);
  end;

  //-- envelope
  if (bDrawDeletion = true) then
  begin
  glCallList(m_dlDeletion);
  end;

  //-- interpolation grid surface
  if (bDrawInterpol = true) then
  begin
  glCallList(m_dlInterpol);
  end;

  //-- moving point stuff
  if (bDrawMovingPt = true) then
  begin
  glCallList(m_dlMovingPt);
  end;
  end;
}

// ******************************************************************************
// Description: Build the OpenGL display lists for Delaunay & Voronoi

// Input:       -

// Output:      -
// ******************************************************************************
{
  procedure TTetraMesh.BuildDisplayList;
  begin
  self.Traverse;
  //-- display list for the delaunay triangulation
  //-- displayList     = Delaunay tetra
  //-- displayList + 1 = Voronoi cells
  //-- displayList + 2 = Data Points
  glDeleteLists(m_dlDV, 3);
  m_dlDV := glGenLists(3);

  glNewList(m_dlDV, GL_COMPILE);
  DrawDelaunay;
  glEndList;

  glNewList(m_dlDV + 1, GL_COMPILE);
  DrawVoronoi;
  glEndList;

  glNewList(m_dlDV + 2, GL_COMPILE);
  DrawDataPoints;
  glEndList;
  end;
}


// ******************************************************************************
// Description: Set a tetra to be highlighted, this tetra is the one that
// contains some coord

// Input:       the coord (x-y-z)
// neighbor (0 -> 4) : 0 means the tetra; 1-4 means one of its
// neighbors

// Output:      -
// ******************************************************************************
{
  procedure TTetraMesh.SetHLTetra(x, y, z: double; neighbor: integer; drawAdj: boolean);
  var
  te: TTetra;
  begin
  //-- search the tetra
  m_tempPt.SetCoord(x, y, z);
  te := Walk(m_tempPt);
  if (neighbor > 0) then
  begin
  te := te.GetT(neighbor);
  end;

  glDeleteLists(m_dlHLTetra, 1);
  m_dlHLTetra := glGenLists(1);
  glNewList(m_dlHLTetra, GL_COMPILE);
  DrawHLTetra(te, drawAdj);
  glEndList;
  end;
}

// ******************************************************************************
// Description: Draw a tetra with transparent faces and HL edges
// Input:       the tetrahedron
// Output:      -
// ******************************************************************************
{
  procedure TTetraMesh.DrawHLTetra(te: TTetra; drawAdj: boolean);
  var
  i: integer;
  adj: TTetra;
  begin
  if (te <> nil) then
  begin
  //-- draw the HL tetra edges in another color (green)
  TDrawTools.DrawTetraFrame(te, 0, 1, 0, 3);
  //-- draw the "HL point"
  //    TDrawTools.DrawNode(m_tempPt, 1, 0, 0, 1);

  //-- draw the nodes for the HL tetra
  for i := 1 to 4 do
  begin
  TDrawTools.DrawNode(te.GetP(i), 0, 1, 0, 1);
  end;

  //-- neighbors of the HL tetra
  if (drawAdj = true) then
  begin
  for i := 1 to 4 do
  begin
  adj := te.GetT(i);
  if (adj <> nil) then
  begin
  TDrawTools.DrawTetraFrame(adj, 0.7, 0.3, 0.0, 3);
  TDrawTools.DrawNode(adj.SetIndex(te).GetProt(0), 0.7, 0.3, 0.0, 1);
  end;
  end;
  end;
  //-- draw the faces of the HL tetra
  TDrawTools.DrawTetraFaces(te, 0.0, 0.4, 0.8, 0.7);
  //-- draw the faces of the neighbors
  if (drawAdj = true) then
  begin
  for i := 1 to 4 do
  begin
  adj := te.GetT(i);
  if (adj <> nil) then
  TDrawTools.DrawTetraFaces(adj, 0.0, 0.0, 0.3, 0.3);
  end;
  end;
  end;
  end;
}

// ******************************************************************************
// Description: Get m_tempPt from m_mesh. Only used to translate the centre of
// rotation for display.

// Input:       -
// Output:      m_tempPt
// ******************************************************************************
function TTetraMesh.GetTempPt: TGBPoint3D;
begin
  Result := m_tempPt;
end;

/// ////////////////////////////
// *****************************
function TTetraMesh.DellOutside(lst: TList; plane: char; mp: double;
  less: boolean): TList;
var
  // lstEdges: TList;
  // i: integer;
  // j: integer;
  k: integer;
  // lstPts,
  tmplst, lpoi: TList;
  // edge: TEdge;
  poif, pois, poi1, poi2: TGBPoint3D;
  p1c, p2c: integer;
  isl: boolean;
  px, py, pz: double;

begin
  tmplst := TList.Create;
  lpoi := TList.Create;
  // lpoix:=TList.Create;
  // lpoiy:=TList.Create;
  poif := TGBPoint3D.Create(0, 0, 0, 0);
  pois := TGBPoint3D.Create(0, 0, 0, 0);
  poi1 := TGBPoint3D.Create(0, 0, 0, 0);
  poi2 := TGBPoint3D.Create(1, 0, 0, 0);

  p1c := -1;
  p2c := -1;
  /// ////////////////////////////
  isl := False;

  for k := 0 to (lst.Count - 1) do
    tmplst.Add(lst[k]);

  tmplst.Add(lst[0]);
  ///
  if plane = 'y' then
  begin
    for k := 0 to (tmplst.Count - 2) do
    begin
      poi1 := tmplst[k];
      poi2 := tmplst[k + 1];

      if (poi1.y < mp) and (poi2.y >= mp) or (poi1.y >= mp) and (poi2.y < mp)
      then
      begin
        isl := True;
        py := mp;
        px := (py - poi1.y) * (poi2.x - poi1.x) / (poi2.y - poi1.y) + poi1.x;
        pz := (py - poi1.y) * (poi2.z - poi1.z) / (poi2.y - poi1.y) + poi1.z;
        if p1c < 0 then
        begin
          p1c := k;
          poif.SetCoord(px, py, pz);
        end
        else
        begin
          p2c := k;
          pois.SetCoord(px, py, pz);
        end;

        // TDrawTools.DrawNode(poi0, 0, 0, 1, 1);
        // TGBPoint3D(lstPts[k]).SetCoord(poi0.x,poi0.y,poi0.z);
        // poi1:=lstPts[k];
      end;

    end;

    if (isl) then
    begin
      if less = True then
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).y <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).y <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).y <= mp then
            lpoi.Add(tmplst[k]);
        end;

      end
      else
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).y >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).y >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).y >= mp then
            lpoi.Add(tmplst[k]);
        end;
      end;

    end
    else
    begin
      for k := 0 to (tmplst.Count - 2) do
      begin
        lpoi.Add(tmplst[k]);
      end;
    end;
  end;
  /// z

  if plane = 'z' then
  begin
    for k := 0 to (tmplst.Count - 2) do
    begin
      poi1 := tmplst[k];
      poi2 := tmplst[k + 1];

      if (poi1.z < mp) and (poi2.z >= mp) or (poi1.z >= mp) and (poi2.z < mp)
      then
      begin
        isl := True;
        pz := mp;
        px := (pz - poi1.z) * (poi2.x - poi1.x) / (poi2.z - poi1.z) + poi1.x;
        py := (pz - poi1.z) * (poi2.y - poi1.y) / (poi2.z - poi1.z) + poi1.y;
        if p1c < 0 then
        begin
          p1c := k;
          poif.SetCoord(px, py, pz);
        end
        else
        begin
          p2c := k;
          pois.SetCoord(px, py, pz);
        end;
      end;
    end;

    if (isl) then
    begin
      if less = True then
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).z <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).z <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).z <= mp then
            lpoi.Add(tmplst[k]);
        end;

      end
      else
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).z >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).z >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).z >= mp then
            lpoi.Add(tmplst[k]);
        end;
      end;
    end
    else
    begin
      for k := 0 to (tmplst.Count - 2) do
      begin
        lpoi.Add(tmplst[k]);
      end;
    end;

  end;

  /// xxxxx
  if plane = 'x' then
  begin
    for k := 0 to (tmplst.Count - 2) do
    begin
      poi1 := tmplst[k];
      poi2 := tmplst[k + 1];

      if (poi1.x < mp) and (poi2.x >= mp) or (poi1.x >= mp) and (poi2.x < mp)
      then
      begin
        isl := True;
        px := mp;
        pz := (px - poi1.x) * (poi2.z - poi1.z) / (poi2.x - poi1.x) + poi1.z;
        py := (px - poi1.x) * (poi2.y - poi1.y) / (poi2.x - poi1.x) + poi1.y;
        if p1c < 0 then
        begin
          p1c := k;
          poif.SetCoord(px, py, pz);
        end
        else
        begin
          p2c := k;
          pois.SetCoord(px, py, pz);
        end;
      end;
    end;

    if (isl) then
    begin
      if less = True then
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).x <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).x <= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).x <= mp then
            lpoi.Add(tmplst[k]);
        end;
      end
      else
      begin
        for k := 0 to p1c do
        begin
          if TGBPoint3D(tmplst[k]).x >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(poif);
        for k := (p1c + 1) to p2c do
        begin
          if TGBPoint3D(tmplst[k]).x >= mp then
            lpoi.Add(tmplst[k]);
        end;
        lpoi.Add(pois);
        for k := (p2c + 1) to (tmplst.Count - 2) do
        begin
          if TGBPoint3D(tmplst[k]).x >= mp then
            lpoi.Add(tmplst[k]);
        end;
      end;

    end
    else
    begin
      for k := 0 to (tmplst.Count - 2) do
      begin
        lpoi.Add(tmplst[k]);
      end;
    end;
  end;
  /// ////////////////////////////////////////
  tmplst.Clear;
  Result := lpoi;
end;

// *******************************
function TTetraMesh.DellOutsideBox(lst: TList): TList;
var
  // lstEdges: TList;
  // i: integer;
  // j: integer;
  k: integer;
  // lstPts,
  tmplst, lpoi: TList;
  // edge: TEdge;
  poif, pois, poi1, poi2: TGBPoint3D;
  p1c, p2c: integer;
  isl: boolean;
  px, py, pz: double;

begin
  tmplst := TList.Create;
  lpoi := TList.Create;
  tmplst := DellOutside(lst, 'x', m_boundingBox[1].x, False);
  lpoi := DellOutside(tmplst, 'y', m_boundingBox[1].y, False);
  tmplst.Clear;
  tmplst := DellOutside(lpoi, 'z', m_boundingBox[1].z, False);
  lpoi.Clear;
  lpoi := DellOutside(tmplst, 'x', m_boundingBox[2].x, True);
  tmplst.Clear;
  tmplst := DellOutside(lpoi, 'y', m_boundingBox[2].y, True);
  lpoi.Clear;
  lpoi := DellOutside(tmplst, 'z', m_boundingBox[2].z, True);
  Result := lpoi;
end;



// ******************************************************************************
// Description: Draw the voronoi cell around one data point
// Input:       the point (around which the cell will be drawn)
// Output:      -

// Remarks:     1 - build a list of every edge connected to this point
// 2 - "rotate" around each edge and compute the circumsphere of
// every tetra
// 3 - connect this center to form one face of the cell
// 4 - do this for every edge in the list
// ******************************************************************************

function TTetraMesh.CreateVorCell(pt: TGBPoint3D): integer;
var
  lstEdges: TList;
  i, ia, ir, currVor: integer;
  j: integer;
  k: integer;
  lstPts, tmplst, lpoi, lpoix, lpoiy: TList;
  edge: TGBEdge;
  poif, pois, poi1, poi2: TGBPoint3D;
  p1c, p2c: integer;
  isl, allout, allin: boolean;
  px, py, pz: double;
  /// //////////
  ///
  /// /////////
  arrList: array of TList;
  // MyPol: TPolyhedr;
  ArrPOL: array of TGBPoint3DArr;
  /// /////////
  ///
  apf: boolean;
  apx, apy, apz, apd: double;
  rr: integer;
begin
  rr := MyPol.FindVert(pt);
  if rr < 0 then
  begin
    // -- build a list of every TEdge connected to the data point "pt"
    lstEdges := GetStar_edges(pt);

    i := pt.m_no;
    // -- draw each face of the voronoi cell
    // -- to do this, rotate around each edge connected to a the data point and
    // -- compute this circumsphere of each tetra
    lstPts := TList.Create;
    lpoi := TList.Create;

    SetLength(arrList, lstEdges.Count);
    SetLength(ArrPOL, lstEdges.Count);

    ir := 0;
    for j := 0 to (lstEdges.Count - 1) do
    begin
      edge := lstEdges[j];
      ComputeVoronoiFaceAroundEdge(edge, lstPts);

      allout := IsAllOutsideBoundingBox(lstPts);
      allin := IsAllInsideBoundingBox(lstPts);
      if allout = False then
      begin
        if allin then
          lpoi := lstPts
        else
        begin
          lpoi := DellOutsideBox(lstPts);
        end;

        SetLength(ArrPOL[ir], lpoi.Count);
        for ia := 0 to lpoi.Count - 1 do
        begin
          apf := TGBPoint3D(lpoi[ia]).flag;
          apx := TGBPoint3D(lpoi[ia]).x;
          apy := TGBPoint3D(lpoi[ia]).y;
          apz := TGBPoint3D(lpoi[ia]).z;
          apd := TGBPoint3D(lpoi[ia]).Data;
          TGBPoint3D(ArrPOL[ir][ia]) := TGBPoint3D.Create(apf, apx, apy,
            apz, apd);
        end;
        Inc(ir);
      end;
      // -- free the list of points
      for k := 0 to (lstPts.Count - 1) do
      begin
        TGBPoint3D(lstPts[k]).Free;
      end;
      lstPts.Clear;
      lpoi.Clear;
    end;
    SetLength(ArrPOL, ir);
    rr := MyPol.Polyheders.CurrVert;
    MyPol.AddPolyhedron(pt, ArrPOL, ir);

    // -- free the list of edges
    for k := 0 to (lstEdges.Count - 1) do
    begin
      TGBEdge(lstEdges[k]).Free;
    end;
    lstEdges.Free;
    lstPts.Free;
  end;
  Result := rr;
end;

/// //////////////////

function TTetraMesh.CreateALLVorCell: integer;
var
  i, r: integer;
begin
  for i := 4 to m_mesh.m_lstVertex.Count - 1 do
  begin
    r := CreateVorCell(TGBPoint3D(m_lstVertex[i]));
  end;
  Result := r;
end;

{
  procedure TTetraMesh.DrawHLCell(pt: TGBPoint3D);
  var
  lstEdges: TList;
  i, ia, ir, currVor: integer;
  j: integer;
  k: integer;
  lstPts, tmplst, lpoi, lpoix, lpoiy: TList;
  edge: TEdge;
  poif, pois,  poi1, poi2 :TGBPoint3D;
  p1c, p2c : integer;
  isl, allout, allin: boolean;
  px, py,pz: Double;
  /////////////
  ///
  ////////////
  arrList: array of TList;

  ArrPOL: array of TarrPoint;
  ////////////
  ///
  apf: Boolean;
  apx, apy, apz, apd: Double;

  begin
  lpoi:=TList.Create;

  currVor:=MyPol.FindVert(pt);  //CreateVorCell(pt);

  ir:=length(MyPol.Polyheders.VoronoiPoly[currVor]);
  // currVor:=MyPol.Polyheders.CurrVert;
  for k := 0 to ir-1 do
  begin
  lpoi.Clear;
  ia:=MyPol.Polyheders.VoronoiPoly[currVor][k];

  for I := 0 to Length(MyPol.Polyheders.Faces[ia])-1 do
  begin
  p1C:=MyPol.Polyheders.Faces[ia][i];
  lpoi.Add(Mypol.Polyheders.CellPoint[p1c]);
  end;
  TDrawTools.DrawPolygonFrame(lpoi, 0.0, 0.0, 0.9, 2);
  TDrawTools.DrawPolygonFace(lpoi, pt, pt.data, 0.0, 0.0, 0.4);

  end;


  end;

}


// ******************************************************************************
// Description: Set the highlighted vertex/star in the mesh

// Input:       v: the vertex
// bEdge: drawing of edges in star(v) or not
// bLink: drawing of link(v)

// Output:      -
// ******************************************************************************
{ procedure TTetraMesh.SetHLVertex(v: TGBPoint3D; bEdges, bLink: boolean);
  begin
  m_hlVertex := v;
  glDeleteLists(m_dlHLVertex, 1);
  m_dlHLVertex := glGenLists(1);
  glNewList(m_dlHLVertex, GL_COMPILE);
  DrawHLVertex(v, bEdges, bLink);
  glEndList;
  end;
}

  function TTetraMesh.GetHLPt: TGBPoint3D;
  begin
    Result := m_hlVertex;
  end;


// ******************************************************************************
// Description: Draw the data points on the screen - except the 4 forming the
// big tetra

// Input:       -
// Output:      -

// ******************************************************************************
{ procedure TTetraMesh.DrawDataPoints;
  var
  i: integer;
  tempt: TGBPoint3D;
  begin
  for i := 4 to (m_lstVertex.Count - 1) do
  begin
  if (bDrawPtsOnCH = false) then
  TDrawTools.DrawNode(m_lstVertex[i], 0.7, 0.3, 0)
  else
  begin
  tempt := m_lstVertex[i];
  if (IsBoundaryCH(tempt) = true) then
  TDrawTools.DrawNode(m_lstVertex[i], 0.0, 0.7, 0)
  else
  TDrawTools.DrawNode(m_lstVertex[i], 0.7, 0.3, 0);
  end;
  end;
  end;
}

// ******************************************************************************
// Description: Create the OpenGL display list for the isosuface
// Input:       list of triangles (TTriangle)
// Output:      -
// ******************************************************************************
procedure TTetraMesh.SetIsosurface(lstTr: TList);
begin
  glDeleteLists(m_dlIsosurface, 1);
  m_dlIsosurface := glGenLists(1);
  glNewList(m_dlIsosurface, GL_COMPILE);
  // DrawIsosurface(lstTr);
  glEndList;
end;


// ******************************************************************************
// Description: Draw the isosurface
// Input:       list of triangles (TTriangle)
// Output:      -
// ******************************************************************************
{ procedure TTetraMesh.DrawIsosurface(lstTr: TList);
  var
  i: integer;
  begin
  for i := 0 to (lstTr.Count - 1) do
  begin
  //    TDrawTools.DrawTriangleFace(TTriangle(lstTr[i]), 0.0, 0.5, 0.0, 0.5);
  TDrawTools.DrawTriangleFace(TTriangle(lstTr[i]), 0.4, 0.4, 0.4, 0.9);
  TDrawTools.DrawTriangleFrame(TTriangle(lstTr[i]), 0.0, 0.0, 0.5, 1);
  end;
  end;
}

// ******************************************************************************
// Description: flip 4 tetra that are in a certain configuration.

// Input:       - te and adj : the 2 tetra on the 'top'
// - te1 and adj1 : respectively the 2 tetra under te and adj
// - (out) flat: the flat tetra created by flip23 and then deleted by
// flip32

// Output:      newTetra: the tetra created by flip23. This tetra can be the flat
// one, as well as te or adj.

// Remarks:     The Flip44 is in fact a combination of (Flip23 + Flip32).  The
// tetra created by Flip23 is deleted by Flip32.

// ******************************************************************************

function TTetraMesh.Flip44(te, adj, te1, adj1: TGBTetrahedron;
  out flat: TGBTetrahedron): TGBTetrahedron;
var
  newTetra: TGBTetrahedron;
  p: TGBPoint3D;
begin
  // -- find the point 'p' (the view point in my sketches)
  p := te.SetIndex(adj).GetProt(0);
  adj.SetIndex(te);

  // -- Flip23 and creation of a new tetra
  // -- one of these tetra is flat.
  newTetra := Flip23(te, adj);

  // -- identify the flat tetra among the 3
  te.SetIndex(p);
  adj.SetIndex(p);
  newTetra.SetIndex(p);
  if (te.GetTrot(0) = adj1) then
    flat := te
  else if (adj.GetTrot(0) = adj1) then
    flat := adj
  else
    flat := newTetra;

  // -- perform the Flip32 that will delete the flat tetra
  Flip32(te1, adj1, flat);

  Result := newTetra;
end;

{
  procedure TTetraMesh.SetHLTetra(te: TTetra; drawAdj: boolean);
  begin
  glDeleteLists(m_dlHLTetra, 1);
  m_dlHLTetra := glGenLists(1);
  glNewList(m_dlHLTetra, GL_COMPILE);
  DrawHLTetra(te, drawAdj);
  glEndList;
  end;
}

procedure TTetraMesh.ComputeVoronoiFaceAroundEdge(e: TGBEdge; lstPts: TList);
var
  centre: TGBPoint3D;
  te: TGBTetrahedron;
  a: TGBPoint3D;
  b: TGBPoint3D;
  tempt: TGBPoint3D;
  i: integer;
begin
  // -- define the 2 opposite vertices (of the edge) in a tetra to rotate
  // -- around the edge
  centre := TGBPoint3D.Create;
  te := e.GetAdjTetra;
  te.SetIndex(e.GetOrg);
  if (te.GetProt(1) = e.GetDest) then
  begin
    a := te.GetProt(2);
    b := te.GetProt(3);
  end
  else if (te.GetProt(2) = e.GetDest) then
  begin
    a := te.GetProt(1);
    b := te.GetProt(3);
  end
  else
  begin
    a := te.GetProt(1);
    b := te.GetProt(2);
  end;

  // -- order the vertices CW when viewed from e.org (so CCW from outside)
  if (TGBGeomTools.Orient3D(e.GetOrg, a, b, e.GetDest) <> -1) then
  begin
    tempt := a;
    a := b;
    b := tempt;
  end;

  repeat
    te.GetCircumCentre(centre);
    lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
    te := te.SetIndex(a).GetTrot(0);
    a := b;
    for i := 1 to 4 do
    begin
      if ((te.GetP(i) <> e.GetOrg) and (te.GetP(i) <> e.GetDest) and
        (te.GetP(i) <> a)) then
      begin
        b := te.GetP(i);
        break;
      end;
    end;
  until (te = e.GetAdjTetra);
  centre.Free;
end;


// ******************************************************************************
// Description: Compute the voronoi vertices of ONE voronoi face around an edge
// and store them in a TList
// Input:       e: the edge
// v: the dummy point for nn interpolation
// lstPts: list of star(v)
// Output:      the TList contains the vertices (TGBPoint3D)
// Note:        new TGBPoint3D are created, they must be destroyed somewhere else.
// ******************************************************************************
procedure TTetraMesh.ComputeVoronoiFaceAroundEdge2(e: TGBEdge; v: TGBPoint3D;
  lstPts: TList);
var
  centre: TGBPoint3D;
  te: TGBTetrahedron;
  a: TGBPoint3D;
  b: TGBPoint3D;
  tempt: TGBPoint3D;
  i, j: integer;
  chedge: boolean; // -- edge on the CH of the dataset
begin
  // -- define the 2 opposite vertices (of the edge) in a tetra to rotate
  // -- around the edge
  centre := TGBPoint3D.Create;
  te := e.GetAdjTetra;
  te.SetIndex(e.GetOrg);
  if (te.GetProt(1) = e.GetDest) then
  begin
    a := te.GetProt(2);
    b := te.GetProt(3);
  end
  else if (te.GetProt(2) = e.GetDest) then
  begin
    a := te.GetProt(1);
    b := te.GetProt(3);
  end
  else
  begin
    a := te.GetProt(1);
    b := te.GetProt(2);
  end;

  // -- order the vertices CW when viewed from e.org (so CCW from outside)
  if (TGBGeomTools.Orient3D(e.GetOrg, a, b, e.GetDest) <> -1) then
  begin
    tempt := a;
    a := b;
    b := tempt;
  end;

  chedge := False;
  repeat
    TGBGeomTools.CircumSphere(te, centre);
    lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
    te.SetIndex(a);
    // if (te.GetTrot(0) <> nil) then
    if (self.IsOutsideCH(te.GetTrot(0)) = False) then
    begin
      te := te.GetTrot(0);
      a := b;
      for i := 1 to 4 do
      begin
        if ((te.GetP(i) <> e.GetOrg) and (te.GetP(i) <> e.GetDest) and
          (te.GetP(i) <> a)) then
        begin
          b := te.GetP(i);
          break;
        end;
      end;
    end
    // -- next tetra is nil -> the edge is on the boundary of CH
    else
    begin
      chedge := True;
      tempt := a;
      a := b;
      b := tempt;
      // -- free the list of points
      for j := 0 to (lstPts.Count - 1) do
      begin
        TGBPoint3D(lstPts[j]).Free;
      end;
      lstPts.Clear;
      break;
    end;
  until (te = e.GetAdjTetra);

  // -- if the edge is on the boundary of the CH then it's (much) more tricky...
  if (chedge = True) then
  begin
    // -- go back to the nil on the 'other side'
    while (True) do
    begin
      te.SetIndex(a);
      // if (te.GetTrot(0) <> nil) then
      if (self.IsOutsideCH(te.GetTrot(0)) = False) then
      begin
        te := te.GetTrot(0);
        a := b;
        for i := 1 to 4 do
        begin
          if ((te.GetP(i) <> e.GetOrg) and (te.GetP(i) <> e.GetDest) and
            (te.GetP(i) <> a)) then
          begin
            b := te.GetP(i);
            break;
          end;
        end;
      end
      else
      begin
        tempt := a;
        a := b;
        b := tempt;
        break;
      end;
    end;

    if (v = nil) then // -- step = 1
    begin
      TGBGeomTools.CircumSphere(te, centre);
      lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
    end
    else // -- step = 2
    begin
      if (TGBGeomTools.InSphere(te, v) = True) then // -- store cc(v + face)
      begin
        TGBGeomTools.CircumSphere(a, e.GetOrg, e.GetDest, v, centre);
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
        TGBGeomTools.CircumSphere(te, centre);
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      end
      else
      begin // -- store cc(te)
        TGBGeomTools.CircumSphere(te, centre);
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      end;
    end;

    while (True) do
    begin
      te.SetIndex(a);
      // if (te.GetTrot(0) <> nil) then
      if (self.IsOutsideCH(te.GetTrot(0)) = False) then
      begin
        te := te.GetTrot(0);
        a := b;
        for i := 1 to 4 do
        begin
          if ((te.GetP(i) <> e.GetOrg) and (te.GetP(i) <> e.GetDest) and
            (te.GetP(i) <> a)) then
          begin
            b := te.GetP(i);
            break;
          end;
        end;
        TGBGeomTools.CircumSphere(te, centre);
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      end
      else
      begin
        break;
      end;
    end;

    if (v = nil) then // -- step = 1 -> store again the cc(te)
    begin
      lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
    end
    else // -- step = 2
    begin
      if (TGBGeomTools.InSphere(te, v) = True) then // -- store cc(v + face)
      begin
        TGBGeomTools.CircumSphere(b, e.GetOrg, e.GetDest, v, centre);
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      end
      else
      begin // -- store cc(te)
        lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
      end;
    end;
  end;
  centre.Free;
end;

{ var
  centre: TGBPoint3D;
  te: TTetra;
  a: TGBPoint3D;
  b: TGBPoint3D;
  tempt:   TGBPoint3D;
  i: integer;
  niltetra: boolean;
  begin
  //-- define the 2 opposite vertices (of the edge) in a tetra to rotate
  //-- around the edge
  centre := TGBPoint3D.Create;
  te := e.GetAdjTetra;
  te.SetIndex(e.GetOrg);
  if (te.GetProt(1) = e.GetDest) then
  begin
  a := te.GetProt(2);
  b := te.GetProt(3);
  end
  else if (te.GetProt(2) = e.GetDest) then
  begin
  a := te.GetProt(1);
  b := te.GetProt(3);
  end
  else
  begin
  a := te.GetProt(1);
  b := te.GetProt(2);
  end;

  //-- order the vertices CW when viewed from e.org (so CCW from outside)
  if (TGeomTools.Orient3D(e.GetOrg, a, b, e.GetDest) <> -1) then
  begin
  tempt := a;
  a := b;
  b := tempt;
  end;

  repeat
  TGeomTools.CircumSphere(te, centre);
  lstPts.Add(TGBPoint3D.Create(centre.x, centre.y, centre.z));
  te := te.SetIndex(a).GetTrot(0);
  a := b;
  for i := 1 to 4 do
  begin
  if ( (te.GetP(i) <> e.GetOrg) and (te.GetP(i) <> e.GetDest) and
  (te.GetP(i) <> a) ) then
  begin
  b := te.GetP(i);
  break;
  end;
  end;
  until (te = e.GetAdjTetra);
  centre.Free;
  end;
}

{
  procedure TTetraMesh.SetPartitionSurface(lstEdges: TList);
  begin
  glDeleteLists(m_dlPartition, 1);
  m_dlPartition := glGenLists(1);
  glNewList(m_dlPartition, GL_COMPILE);
  DrawPartitionSurface(lstEdges);
  glEndList;
  end;
}

{
  procedure TTetraMesh.DrawPartitionSurface(lstEdges: TList);
  var
  lstPts: TList;
  i: integer;
  j: integer;
  e: TEdge;
  begin
  lstPts := TList.Create;
  for i := 0 to (lstEdges.Count - 1) do
  begin
  e := lstEdges[i];
  ComputeVoronoiFaceAroundEdge(e, lstPts);
  //-- draw the frame and the face
  TDrawTools.DrawPolygonFrame(lstPts, 0.0, 0.5, 0.0, 3);
  if (e.GetOrg.data > e.GetDest.data) then
  begin
  TDrawTools.DrawPolygonFace(lstPts, e.GetOrg, 0.0, 0.4, 0.8, 0.7);
  end
  else
  begin
  TDrawTools.DrawPolygonFace(lstPts, e.GetDest, 0.0, 0.4, 0.8, 0.7);
  end;
  //-- free the list of points
  for j := 0 to (lstPts.Count - 1) do
  begin
  TGBPoint3D(lstPts[j]).Free;
  end;
  lstPts.Clear;
  end;
  end;
}
{
  procedure TTetraMesh.DrawCrust(lstTr: TList);
  var
  i: integer;
  begin
  for i := 0 to (lstTr.Count - 1) do
  begin
  TDrawTools.DrawTriangleFace(TTriangle(lstTr[i]), 0.4, 0.4, 0.4, 0.7);
  TDrawTools.DrawTriangleFrame(TTriangle(lstTr[i]), 0.0, 0.0, 0.5, 1);
  end;
  end;
}

procedure TTetraMesh.SetCrust(lstTr: TList);
begin
  glDeleteLists(m_dlCrust, 1);
  m_dlCrust := glGenLists(1);
  glNewList(m_dlCrust, GL_COMPILE);
  // self.DrawCrust(lstTr);
  glEndList;
end;


// ******************************************************************************
// Description: Draw the 'envelope' around a point in the mesh. The envelope
// represents all the exterior faces of the tetra that have a point
// pt as one of their vertices. Mostly used for visualization for
// deletion. This version of SetEnvelope highlights one ear

// Input:       pt    : the point
// te-adj: the ear to highlight
// lstEar: the list of ears forming the polyhedron H
// lstPts: list of points forming the polyhedron H

// Output:      - (the display list for the envelope is built)
// ******************************************************************************
{
  procedure TTetraMesh.SetEnvelope(pt: TGBPoint3D; te, adj: TTetra; lstTetra, lstPts: TList);
  var
  i: integer;
  tempte: TTetra;
  tempt: TGBPoint3D;
  begin
  glDeleteLists(m_dlDeletion, 1);
  m_dlDeletion := glGenLists(1);
  glNewList(m_dlDeletion, GL_COMPILE);
  //-- draw the node in the middle
  TDrawTools.DrawNode(pt, 1.0, 0, 0, 1);

  if (te <> nil) then
  begin
  //-- draw HL ears
  TDrawTools.DrawTetraFrame(te, 1, 0, 0, 2);
  TDrawTools.DrawTetraFrame(adj, 1, 0, 0, 2);
  te.SetIndex(pt);
  adj.SetIndex(pt);
  TDrawTools.DrawTriangleFace(te.GetProt(1), te.GetProt(2), te.GetProt(3),
  te.GetProt(0), 0.5, 0, 0, 0.5);
  TDrawTools.DrawTriangleFace(adj.GetProt(1), adj.GetProt(2), adj.GetProt(3),
  adj.GetProt(0), 0, 0.5, 0, 0.5);
  end;

  for i := 0 to (lstPts.Count - 1) do
  begin
  tempt := lstPts[i];
  if (te = nil) then
  begin
  TDrawTools.DrawLine(pt, tempt, 0.0, 0.8, 0.0, 2);
  end
  else if ( (te.HasPoint(tempt) = false) and (adj.HasPoint(tempt) = false) ) then
  begin
  TDrawTools.DrawLine(pt, tempt, 0.0, 0.8, 0.0, 2);
  end;
  end;

  for i := 0 to (lstTetra.Count - 1) do
  begin
  tempte := lstTetra[i];
  tempte.SetIndex(pt);
  //-- draw the exterior face
  TDrawTools.DrawTriangleFace(tempte.GetProt(1), tempte.GetProt(2),
  tempte.GetProt(3), pt, 0.4, 0.4, 0.4, 0.7);
  TDrawTools.DrawTriangleFrame(tempte.GetProt(1), tempte.GetProt(2),
  tempte.GetProt(3), 0.0, 0.0, 0.5, 1);
  end;
  glEndList;
  end;
}

// ******************************************************************************
// Description: Compute the average degree of the points in the mesh

// Input:       - (all the point in the mesh are scanned)

// Output:      the average degree
// ******************************************************************************
function TTetraMesh.GetAvgDegree: double;
var
  total: integer;
  i: integer;
  no: integer;
begin
  total := 0;
  no := 0;
  for i := 4 to (m_lstVertex.Count - 1) do // -- do not compute for big tetra
  begin
    total := total + GetDegree(m_lstVertex[i]);
    no := no + 1;
  end;
  Result := (total / no);
end;


// ******************************************************************************
// Description: Compute the degree of a point in the mesh (this is actually the
// number of Voronoi neighbours; or the number of edges connected
// to this point)

// Input:       pt : the point

// Output:      the degree of the point
// ******************************************************************************
function TTetraMesh.GetDegree(pt: TGBPoint3D): integer;
var
  lst: TList;
begin
  lst := self.GetNN_vertices(pt);
  Result := lst.Count;
  lst.Free;
end;


// ******************************************************************************
// Description: Delete a vertex from the mesh -- with a simple list of TTetra

// Input:       pt: the vertex to delete

// Output:      - (the mesh is updated)

// Remarks:     To delete the vertex, a simple list of all the tetra incident to
// pt is kept and the ears are built 'on-the-fly'; so the same ear
// can be tested twice.
// ******************************************************************************
function TTetraMesh.DeleteVertex_tetra(pt: TGBPoint3D): integer;
var
  lstTetra: TList;
  lstPts: TList;
  te: TGBTetrahedron;
  adj: TGBTetrahedron;
  i: integer;
  j: integer;
  k: integer;
  l: integer;
begin
  // -- list of all the tetra that have pt as one of their vertices, not ordered
  lstTetra := TList.Create;
  // -- list of the vertices forming the polyhedron H
  lstPts := TList.Create;

  // -- creation of those 2 lists
  self.GetStar_v_tetra(pt, lstTetra, lstPts);

  k := 0; // -- # of ears processed, successfully or not.
  l := 0; // -- # of flip performed to delete the point
  i := 0;
  te := lstTetra[i];

  while (lstTetra.Count > 4) do
  begin
    for j := 1 to 3 do
    begin
      te.SetIndex(pt);
      adj := te.GetTrot(j);
      if (lstTetra.IndexOf(adj) <> -1) then
      begin
        Inc(k);
        // self.SetEnvelope(pt, te, adj, lstTetra, lstPts);
        // Form1.Refresh;
        if (self.ProcessEar_t(te, adj, pt, lstPts, lstTetra) = True) then
        begin
          Inc(l);
          // self.SetEnvelope(pt, nil, nil, lstTetra, lstPts);
          // Form1.Refresh;
          break;
        end;
      end;
{$IFDEF DEBUG}
      self.Traverse;
      self.TestAllTetraOrient3D;
      self.TestTopology;
{$ENDIF}
    end;
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
{$ENDIF}
    // -- next tetra in the list
    i := self.NextInList(i, lstTetra.Count);
    te := lstTetra[i];
    te.SetIndex(pt);
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, lstTetra[0]);
  m_curTe := lstTetra[0];

  // ShowMessage('# of processed ears: ' + IntToStr(k) +
  // '    # of flips: ' + IntToStr(l));

  lstTetra.Clear;
  lstTetra.Free;
  lstPts.Clear;
  lstPts.Free;

{$IFDEF DEBUG}
  TestAllTetraFlat;
  TestInvariants;
{$ENDIF}
  Result := l;
end;


function TTetraMesh.DeleteVertex_insphere_no_perturb(pt: TGBPoint3D;
  draw: boolean): integer;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  i: integer;
  k: integer;
  l: integer;
  nbD: integer;
  updated: integer;
  endless: integer;
  flag44: boolean;
  superflag: boolean;
  markedEar: TGBEar;
  lastProcessed: TGBEar;
begin
  { //-- drawing or not the envelope
    if (self.bDrawDeletion = true) then
    draw := true;
    if (draw = true) then
    bDrawDeletion := true;
  }
  lstEar := TListz.Create;
  lstPts := TList.Create;
  // -- creation of those 2 lists
  self.GetStar_v_ear(pt, lstEar, lstPts, False);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  k := 0; // -- # of ears processed, successfully or not.
  l := 0; // -- # of flip performed to delete the point
  i := 0;

  ear := lstEar[i];
  flag44 := False;
  superflag := False;
  markedEar := nil;
  lastProcessed := nil;
  endless := 0;
  // nbD := NbDelaunayEars(lstEar, lstPts);
  // ShowMessage('Delaunay  :' + IntToStr(nbD));
  while (lstEar.Count > 6) do
  begin
    if ((endless > lstEar.Count) and (flag44 = False)) then
    begin
      lastProcessed := Delete_unflip(lstEar, lstPts, lastProcessed);
      endless := 0;
    end;
    Inc(k);
    { if (draw = true) then
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //    Form1.Refresh;
      end;
    }
    if ((flag44 = True) and (ear = markedEar)) then
      superflag := True;
    if (flag44 = False) then
      superflag := False;

    updated := self.ProcessEar_e(ear, lstPts, lstEar, superflag);

    if (updated = 1) then
    begin
      endless := 0;
      { if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //        Form1.Refresh;
        end;
      }
      Inc(l);
      flag44 := False;
    end
    else if (updated = 2) then
    begin
      if (flag44 = False) then
      begin
        flag44 := True;
        markedEar := ear;
      end;
    end
    else if (updated = 0) then
      Inc(endless);

    i := self.NextInList(i, lstEar.Count);
    ear := lstEar[i];
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  // m_curTe := ear.GetTetra(1);

  lstEar.Zap;
  lstEar.Free;
  lstPts.Clear;
  lstPts.Free;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  // bDrawDeletion := false;
  Result := l;
end;


// ******************************************************************************
// Description: Delete a vertex from the mesh -- with InSphere() method --
// WITH using symbolic perturbation.

// Input:       pt: the vertex to delete
// draw: drawing or not the envelope (a boolean)

// Output:      (the mesh is updated)
// the # of flips needed to delete the vertex

// Remarks:     using the method where a list of TEar is kept up-to-date
// ******************************************************************************
function TTetraMesh.DeleteVertex_insphere_perturb(pt: TGBPoint3D;
  draw: boolean): integer;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  i: integer;
  k: integer;
  l: integer;
  updated: integer;
  flag44: boolean;
  superflag: boolean;
  markedEar: TGBEar;
begin
  // self.bDrawDeletion := draw;

  lstEar := TListz.Create;
  lstPts := TList.Create;
  // -- creation of those 2 lists
  self.GetStar_v_ear(pt, lstEar, lstPts, False);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  k := 0; // -- # of ears processed, successfully or not.
  l := 0; // -- # of flip performed to delete the point
  i := 0;

  ear := lstEar[i];
  flag44 := False;
  superflag := False;
  markedEar := nil;
  while (lstEar.Count > 6) do
  begin
    Inc(k);
    { if (draw = true) then
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //     Form1.Refresh;
      end;
    }
    if ((flag44 = True) and (ear = markedEar)) then
      superflag := True;
    if (flag44 = False) then
      superflag := False;

    updated := self.ProcessEar_e(ear, lstPts, lstEar, superflag);

    if (updated = 1) then
    begin
      { if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //       Form1.Refresh;
        Sleep(300);
        end;
      }
      Inc(l);
      flag44 := False;
    end
    else if (updated = 2) then
    begin
      if (flag44 = False) then
      begin
        flag44 := True;
        markedEar := ear;
      end;
    end;

    i := self.NextInList(i, lstEar.Count);
    ear := lstEar[i];
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  m_curTe := ear.GetTetra(1);

  lstEar.Zap;
  lstEar.Free;
  lstPts.Clear;
  lstPts.Free;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  // bDrawDeletion := false;
  Result := l;
end;


// ******************************************************************************
// Description: Delete a vertex from the mesh -- POWER method.

// Input:       pt: the vertex to delete

// Output:      # of flips needed to delete the vertex

// Remarks:     using the method where a list of TEar is kept up-to-date
// and a priority queue
// ******************************************************************************
function TTetraMesh.DeleteVertex_power(pt: TGBPoint3D; draw: boolean): integer;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  noFlips: integer;
  noStucks: integer;
  updated: integer;
  stuckCtr: integer;
  flag44: boolean;
  superflag: boolean;
  power: double;
  i: integer;
begin
  // -- drawing or not the envelope
  // if (self.bDrawDeletion = true) then
  draw := True;
  if (draw = True) then
    // bDrawDeletion := true;

    lstEar := TListz.Create;
  lstPts := TList.Create;
  self.GetStar_v_ear(pt, lstEar, lstPts, True);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  noFlips := 0; // -- # of flip performed to delete the point
  noStucks := 0; // -- # of ears that cannot be flipped

  ear := GetMaxList(lstEar);
  flag44 := False;
  superflag := False;
  stuckCtr := -1;
  while (lstEar.Count > 6) do
  begin
    power := TGBEarp(ear).GetPower;
    // -- if stuck, then do something
    if (power = -9999) then
    begin
      if (flag44 = True) then
      begin
        superflag := True;
        stuckCtr := 0;
      end;
      if ((superflag = True) and (stuckCtr = lstEar.Count)) then
      begin
        Delete_unflip(lstEar, lstPts, nil);
      end;
    end;
    {
      if (draw = true) then //-- draw the selected ear
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //     Form1.Refresh;
      end;
    }
    updated := ProcessEar_p(ear, lstEar, lstPts, superflag);

    if (updated = 1) then
    begin
      { if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //     Form1.Refresh;
        end;
      }
      Inc(noFlips);
      flag44 := False;
      stuckCtr := -1;
    end
    else if (updated = 2) then
    begin
      flag44 := True;
    end
    else if (updated = 0) then
    begin
      Inc(noStucks);
      if (stuckCtr <> -1) then
        Inc(stuckCtr);
    end;

    if ((power = -9999) and (updated = 0)) then
    begin
      i := lstEar.IndexOf(ear);
      i := self.NextInList(i, lstEar.Count);
      ear := lstEar[i];
    end
    else
      ear := GetMaxList(lstEar);

{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  m_curTe := ear.GetTetra(1);
  // ShowMessage('# of flips: ' + IntToStr(noFlips));
  // if (noStucks > 0) then
  // ShowMessage('# of stuck ears: ' + IntToStr(noStucks));
  lstEar.Zap;
  lstPts.Clear;
  lstPts.Free;
  // Form1.Refresh;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  // bDrawDeletion := false;
  Result := noFlips;
end;


// ******************************************************************************
// Description: To build a list of all the ears formed by the incident tetra of
// a certain vertex in the mesh; and also of the natural neighbours
// of the vertex (the points)

// Input:       pt            : the vertex
// (out) lstPts  : the empty list for the points
// (out) lstEar  : the empty list for the ears
// pow: boolean (if the power of the ear must be calculated)

// Output:      (lstPts) & (lstEar)
// ******************************************************************************
procedure TTetraMesh.GetStar_v_ear(pt: TGBPoint3D; out lstEar: TListz;
  out lstPts: TList; pow: boolean);
var
  cur: TGBTetrahedron;
  tempte: TGBTetrahedron;
  queue: TQueue;
  i: integer;
  j: integer;
  lstTetra: TList;
  ear: array [1 .. 3] of TGBEar;
  exist: array [1 .. 3] of boolean;
  tempear: TGBEar;
begin
  cur := Walk(pt);
  queue := TQueue.Create;
  queue.Push(cur);
  lstTetra := TList.Create;

  while (queue.Count > 0) do
  begin
    cur := queue.Pop;
    cur.SetIndex(pt);
    if (lstTetra.IndexOf(cur) = -1) then
    begin
      lstTetra.Add(cur);
      // -- add the exterior points if they are not already in the list
      for i := 1 to 3 do
      begin
        if (lstPts.IndexOf(cur.GetProt(i)) = -1) then
        begin
          lstPts.Add(cur.GetProt(i));
        end;
      end;
      // -- add the neighbours if they are valid
      for i := 1 to 3 do
      begin
        tempte := cur.SetIndex(pt).GetTrot(i);
        Assert(tempte <> nil);
        Assert(tempte.HasPoint(pt));
        if (lstTetra.IndexOf(tempte) = -1) then
        begin
          queue.Push(tempte);
          exist[i] := False;
          if (pow = True) then
            ear[i] := TGBEarp.Create(cur, tempte, pt)
          else
            ear[i] := TGBEar.Create(cur, tempte, pt);
          lstEar.Add(ear[i]);
        end
        else
        // -- look in the list of ears to find the one that contains the same
        // -- 2 tetra
        begin
          exist[i] := True;
          for j := 0 to (lstEar.Count - 1) do
          begin
            tempear := lstEar[j];
            if (tempear.Compare(tempte, cur) = True) then
            begin
              ear[i] := tempear;
            end;
          end;
        end;
      end;
      // -- update adjacent fields for the ears
      if (exist[1] = False) then
      begin
        ear[1].SetAdj(1, ear[2]);
        ear[1].SetAdj(2, ear[3]);
      end
      else
      begin
        ear[1].SetAdj(3, ear[2]);
        ear[1].SetAdj(4, ear[3]);
      end;
      if (exist[2] = False) then
      begin
        ear[2].SetAdj(1, ear[1]);
        ear[2].SetAdj(2, ear[3]);
      end
      else
      begin
        ear[2].SetAdj(3, ear[1]);
        ear[2].SetAdj(4, ear[3]);
      end;
      if (exist[3] = False) then
      begin
        ear[3].SetAdj(1, ear[1]);
        ear[3].SetAdj(2, ear[2]);
      end
      else
      begin
        ear[3].SetAdj(3, ear[1]);
        ear[3].SetAdj(4, ear[2]);
      end;
    end;
  end;
  FreeAndNil(queue);
  FreeAndNil(lstTetra);
end;




// ******************************************************************************
// Description: To build a list of all the tetra incident to a certain vertex,
// and also of the natural neighbours of the vertex

// Input:       pt    : the vertex
// (out) lstPts : the empty list for the points
// (out) lstTetra : the empty list for the tetra

// Output:      (lstPts) & (lstTetra)
// ******************************************************************************
procedure TTetraMesh.GetStar_v_tetra(pt: TGBPoint3D;
  out lstTetra, lstPts: TList);
var
  cur: TGBTetrahedron;
  tempte: TGBTetrahedron;
  queue: TQueue;
  i: integer;
begin
  cur := Walk(pt);
  queue := TQueue.Create;
  queue.Push(cur);
  while (queue.Count > 0) do
  begin
    cur := queue.Pop;
    cur.SetIndex(pt);
    if (lstTetra.IndexOf(cur) = -1) then
    begin
      lstTetra.Add(cur);
      // -- add the exterior points if they are not already in the list
      for i := 1 to 3 do
      begin
        if (lstPts.IndexOf(cur.GetProt(i)) = -1) then
        begin
          lstPts.Add(cur.GetProt(i));
        end;
      end;
    end;
    // -- add the neighbours if they are valid
    for i := 1 to 3 do
    begin
      tempte := cur.GetTrot(i);
      if ((tempte <> nil) and (lstTetra.IndexOf(tempte) = -1) and
        (tempte.HasPoint(pt))) then
      begin
        queue.Push(tempte);
      end;
    end;
  end;
  FreeAndNil(queue);
end;


// ******************************************************************************
// Description: To build a list of all the natural neighbours of a vertex in
// the mesh (the other vertices that share an edge with the point)

// Input:       pt    : the vertex for which we want to find the neighbours

// Output:      lstPts: the list containing the nn (TGBPoint3D)
// ******************************************************************************
function TTetraMesh.GetNN_vertices(pt: TGBPoint3D): TList;
var
  cur: TGBTetrahedron;
  tempte: TGBTetrahedron;
  queue: TQueue;
  i: integer;
  lstTetra: TList;
  lstPts: TList;
begin
  cur := Walk(pt);
  queue := TQueue.Create;
  queue.Push(cur);
  lstTetra := TList.Create;
  lstPts := TList.Create;

  while (queue.Count > 0) do
  begin
    cur := queue.Pop;
    cur.SetIndex(pt);
    if (lstTetra.IndexOf(cur) = -1) then
    begin
      lstTetra.Add(cur);
      // -- add the exterior points if they are not already in the list
      for i := 1 to 3 do
      begin
        if (lstPts.IndexOf(cur.GetProt(i)) = -1) then
        begin
          lstPts.Add(cur.GetProt(i));
        end;
      end;
    end;
    // -- add the neighbours if they are valid
    for i := 1 to 3 do
    begin
      tempte := cur.GetTrot(i);
      if ((tempte <> nil) and (lstTetra.IndexOf(tempte) = -1) and
        (tempte.HasPoint(pt))) then
      begin
        queue.Push(tempte);
      end;
    end;
  end;
  FreeAndNil(queue);
  FreeAndNil(lstTetra);
  Result := lstPts;
end;


// ******************************************************************************
// Description: To navigate in a TList continually; i.e. after the last element
// comes the first one.

// Input:       i    : the current element in the TList
// count: total number of elements in the mesh

// Output:      the next element in the TList

// Remarks:     -
// ******************************************************************************
function TTetraMesh.NextInList(i, Count: integer): integer;
begin
  if (i >= (Count - 1)) then
  begin
    Result := 0;
  end
  else
  begin
    Result := (i + 1);
  end;
end;

function TTetraMesh.IsEarConvex(te, adj: TGBTetrahedron;
  pt: TGBPoint3D): boolean;
var
  i: integer;
  j: integer;
begin
  te.SetIndex(pt);
  adj.SetIndex(te);
  i := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2), te.GetProt(3), pt);
  j := TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2), te.GetProt(3),
    adj.GetProt(0));
  // -- check if the first ear is formed by a flat tetra, if it's the case then
  // -- switch the ear & the problem is solved; unless the 2 ears are formed by
  // -- flat tetra.
  if ((i = 0) or (j = 0)) then
  begin
    adj.SetIndex(pt);
    te.SetIndex(adj);
    i := TGBGeomTools.Orient3D(adj.GetProt(1), adj.GetProt(2),
      adj.GetProt(3), pt);
    j := TGBGeomTools.Orient3D(adj.GetProt(1), adj.GetProt(2), adj.GetProt(3),
      te.GetProt(0));
  end;

  if ((i = 0) or (j = 0)) then
  begin
    Result := False;
  end
  else
  begin
    if (i = j) then
      Result := True
    else
      Result := False;
  end;
end;

function TTetraMesh.IsEarDelaunay(te, adj: TGBTetrahedron; pt: TGBPoint3D;
  lstPts: TList): boolean;
var
  i: integer;
  r: boolean;
  tempt: TGBPoint3D;
  tempte: TGBTetrahedron;
begin
  r := True;
  te.SetIndex(pt);
  adj.SetIndex(te);
  if (TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2), te.GetProt(3),
    adj.GetProt(0)) = 1) then
  begin
    tempte := TGBTetrahedron.Create(te.GetProt(1), te.GetProt(2), te.GetProt(3),
      adj.GetProt(0));
  end
  else
  begin
    tempte := TGBTetrahedron.Create(te.GetProt(2), te.GetProt(1), te.GetProt(3),
      adj.GetProt(0));
  end;

  for i := 0 to (lstPts.Count - 1) do
  begin
    tempt := lstPts[i];
    if (tempte.HasPoint(tempt) = False) then
    begin
      if (TGBGeomTools.InSphere(tempte, tempt) = True) then
      begin
        r := False;
        break;
      end;
    end;
  end;
  FreeAndNil(tempte);
  Result := r;
end;


// ******************************************************************************
// Description: *** TEST VERSION - ProcessEar is supposed for every case ***
// To determine what action to do with an ear of the polyhedron. If
// the ear is valid (i.e. convex, Delaunay, flippable) the
// appropriate flip is performed.

// Input:       te, adj : the 2 tetra forming the ear
// pt      : the vertex to delete
// lstPts  : list of vertices forming H
// lstTetra: list of tetra incident to pt

// Output:      TRUE  -> a flip was performed; lstPts & lstEar were updated.
// FALSE -> no flip was performed
// ******************************************************************************
function TTetraMesh.ProcessEar2(te, adj: TGBTetrahedron; pt: TGBPoint3D;
  lstPts, lstTetra: TList): boolean;
var
  p: TGBPoint3D;
  q: TGBPoint3D;
  intersect: integer;
  intersect2: integer;
  te3: TGBTetrahedron;
  updated: boolean;
  newTetra: TGBTetrahedron;
  i: integer;
  planePt: TGBPoint3D;
  removePt: TGBPoint3D;
  tempt: TGBPoint3D;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
begin
  updated := False;

  // -- 1st, the ear must be CONVEX
  if (self.IsEarConvex(te, adj, pt) = True) then
  begin
    // -- 2nd, the ear must be FLIPPABLE
    // -- does the edge pass through the triangle?
    p := te.SetIndex(adj).GetProt(0);
    q := adj.SetIndex(te).GetProt(0);

    // -- do the right flip according to the position of the tetrahedra
    intersect := TGBGeomTools.IntersectionEdgeTriangle(p, q, te.GetProt(1),
      te.GetProt(2), te.GetProt(3));
    Assert(intersect <> 2);
    Assert(intersect <> 3);

    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      // -- 3rd, test if the ear is DELAUNAY
      if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
      begin
        updated := True;
        newTetra := Flip23(te, adj);
        // -- if newTetra is the tetra that is deleted from H, then no need to
        // -- to add it or delete it from the list lstTetra
        if (te.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(te);
          lstTetra.Add(newTetra);
        end
        else if (adj.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(adj);
          lstTetra.Add(newTetra);
        end;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      te3 := Find3rdTetra(te, adj);
      // -- this 3rd tetra must exist
      if ((te3 <> nil) and (lstTetra.IndexOf(te3) <> -1)) then
      begin
        removePt := nil;
        planePt := nil;
        // -- find the point that will be removed from H at the end; and also find
        // -- the point which will be part of the future facet after the flip.
        for i := 1 to 3 do
        begin
          tempt := te.GetProt(i);
          if (tempt <> pt) then
          begin
            if (te3.HasPoint(tempt) = True) then
              removePt := tempt
            else
              planePt := tempt;
          end;
        end;

        // -- the 2 points of the edge of degree 3 must be of each side of the
        // -- middle plane
        intersect2 := TGBGeomTools.IntersectionEdgeTriangle(pt, removePt, p,
          q, planePt);
        if (intersect2 = 2) then
          ShowMessage('intersect = 2');
        if ((intersect2 = 1) or (intersect2 = 2) or (intersect2 = 0)) then
        begin
          // -- 3rd, test if the ear is DELAUNAY
          if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
          begin
            updated := True;
            Flip32(te, adj, te3);
            lstTetra.Remove(te3);
            te3.Free;
            lstPts.Remove(removePt);
            if (te.HasPoint(pt) = False) then
              lstTetra.Remove(te)
            else
              lstTetra.Remove(adj);
          end;
        end;
      end;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      te3 := Find3rdTetra(te, adj);
      if ((te3 <> nil) and (lstTetra.IndexOf(te3) <> -1)) then
      begin
        removePt := nil;
        planePt := nil;
        // -- find the point that will be removed from H at the end; and also find
        // -- the point which will be part of the future facet after the flip.
        for i := 1 to 3 do
        begin
          tempt := te.GetProt(i);
          if (tempt <> pt) then
          begin
            if (te3.HasPoint(tempt) = True) then
              removePt := tempt
            else
              planePt := tempt;
          end;
        end;

        // -- the 2 points of the edge of degree 3 must be of each side of the
        // -- middle plane
        intersect2 := TGBGeomTools.IntersectionEdgeTriangle(pt, removePt, p,
          q, planePt);
        if ((intersect2 = 1) or (intersect2 = 2) or (intersect2 = 0)) then
        begin
          // -- 3rd, test if the ear is DELAUNAY
          if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
          begin
            updated := True;
            // -- INTERSECT = 0
            Flip32(te, adj, te3);
            lstTetra.Remove(te3);
            te3.Free;
            lstPts.Remove(removePt);
            if (te.HasPoint(pt) = False) then
              lstTetra.Remove(te)
            else
              lstTetra.Remove(adj);
          end;
        end;
      end
      else if (AreTetraConfig44(te, adj, te1, adj1) <> 0) then
      begin
        if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
        begin
          updated := True;
          // -- INTERSECT = 0
          newTetra := Flip23(te, adj);
          // -- if newTetra is the tetra that is deleted from H, then no need to
          // -- to add it or delete it from the list lstTetra
          if (te.HasPoint(pt) = False) then
          begin
            lstTetra.Remove(te);
            lstTetra.Add(newTetra);
          end
          else if (adj.HasPoint(pt) = False) then
          begin
            lstTetra.Remove(adj);
            lstTetra.Add(newTetra);
          end;
        end;
      end;
    end;
  end;
  Result := updated;
end;



// ******************************************************************************
// Description: Flip41 is for deleting a point lying inside a tetra, and
// deleting the 4 tetra tetra incident to the point, leaving only
// one tetra. Last operation needed for deleting a vertex from a
// tetrahedralization.

// Input:       pt : the TGBPoint3D to delete from the mesh
// te : one of the 4 tetra incident to 'pt'

// Output:      -

// Remarks:     -
// ******************************************************************************
procedure TTetraMesh.Flip41(pt: TGBPoint3D; te: TGBTetrahedron);
var
  i: integer;
  adj: TGBTetrahedron;
  adjout: TGBTetrahedron;
begin
  te.SetIndex(pt);
  te.SetProt(0, te.GetTrot(1).SetIndex(te).GetProt(0));
  for i := 1 to 3 do
  begin
    adj := te.GetTrot(i);
    adjout := adj.SetIndex(pt).GetTrot(0);
    if (adjout <> nil) then
    begin
      adjout.SetIndex(adj).SetTrot(0, te);
      te.SetTrot(i, adjout);
    end
    else
    begin
      te.SetTrot(i, nil);
    end;
    adj.Free;
  end;
  m_lstVertex.Remove(pt);
  te.ResetCentre;
  m_curTe := te;
end;


// ******************************************************************************
// Description: Test all the tetra in the mesh to be sure they are not flat
// (tetra is flat if the 4 vertices are coplanar)

// Input:       -

// Output:      -

// Note:        An exception is raised if a tetra is flat.
// ******************************************************************************
procedure TTetraMesh.TestAllTetraFlat;
var
  i: integer;
  te: TGBTetrahedron;
begin
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    te := m_lstTetra[i];
    if (te.IsFlat = True) then
      raise Exception.Create('[Flat Tetra] Tetra #' + IntToStr(te.m_no));
  end;
end;


// ******************************************************************************
// Description: To determine what action to do with an ear of the polyhedron. If
// the ear is valid (i.e. convex, Delaunay, flippable) the
// appropriate flip is performed.

// Input:       te, adj : the 2 tetra forming the ear
// pt      : the vertex to delete
// lstPts  : list of vertices forming H
// lstTetra: list of tetra incident to pt

// Output:      TRUE  -> a flip was performed; lstPts & lstEar were updated.
// FALSE -> no flip was performed
// ******************************************************************************
function TTetraMesh.ProcessEar_t(te, adj: TGBTetrahedron; pt: TGBPoint3D;
  lstPts, lstTetra: TList): boolean;
var
  p: TGBPoint3D;
  q: TGBPoint3D;
  intersect: integer;
  intersect2: integer;
  te3: TGBTetrahedron;
  updated: boolean;
  newTetra: TGBTetrahedron;
  i: integer;
  planePt: TGBPoint3D;
  removePt: TGBPoint3D;
  tempt: TGBPoint3D;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
  flat: TGBTetrahedron;
begin
  updated := False;

  // -- 1st, the ear must be CONVEX
  if (self.IsEarConvex(te, adj, pt) = True) then
  begin
    // -- 2nd, the ear must be FLIPPABLE
    // -- does the edge pass through the triangle?
    p := te.SetIndex(adj).GetProt(0);
    q := adj.SetIndex(te).GetProt(0);

    // -- do the right flip according to the position of the tetrahedra
    intersect := TGBGeomTools.IntersectionEdgeTriangle(p, q, te.GetProt(1),
      te.GetProt(2), te.GetProt(3));
    Assert(intersect <> 2);
    Assert(intersect <> 3);

    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      // -- 3rd, test if the ear is DELAUNAY
      if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
      begin
        updated := True;
        newTetra := Flip23(te, adj);
        // -- if newTetra is the tetra that is deleted from H, then no need to
        // -- to add it or delete it from the list lstTetra
        if (te.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(te);
          lstTetra.Add(newTetra);
        end
        else if (adj.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(adj);
          lstTetra.Add(newTetra);
        end;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      te3 := Find3rdTetra(te, adj);
      // -- this 3rd tetra must exist & be inside H
      if ((te3 <> nil) and (lstTetra.IndexOf(te3) <> -1)) then
      begin
        removePt := nil;
        planePt := nil;
        // -- find the point that will be removed from H at the end; and also find
        // -- the point which will be part of the future facet after the flip.
        for i := 1 to 3 do
        begin
          tempt := te.GetProt(i);
          if (tempt <> pt) then
          begin
            if (te3.HasPoint(tempt) = True) then
              removePt := tempt
            else
              planePt := tempt;
          end;
        end;

        // -- the 2 points of the edge of degree 3 must be of each side of the
        // -- middle plane
        intersect2 := TGBGeomTools.IntersectionEdgeTriangle(pt, removePt, p,
          q, planePt);
        if (intersect2 <> -1) then
        begin
          // -- 3rd, test if the ear is DELAUNAY
          if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
          begin
            updated := True;
            Flip32(te, adj, te3);
            lstTetra.Remove(te3);
            te3.Free;
            lstPts.Remove(removePt);
            if (te.HasPoint(pt) = False) then
              lstTetra.Remove(te)
            else
              lstTetra.Remove(adj);
          end;
        end;
      end;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      te3 := Find3rdTetra(te, adj);
      if ((te3 <> nil) and (lstTetra.IndexOf(te3) <> -1)) then
      begin
        removePt := nil;
        planePt := nil;
        // -- find the point that will be removed from H at the end; and also find
        // -- the point which will be part of the future facet after the swap.
        for i := 1 to 3 do
        begin
          tempt := te.GetProt(i);
          if (tempt <> pt) then
          begin
            if (te3.HasPoint(tempt) = True) then
              removePt := tempt
            else
              planePt := tempt;
          end;
        end;

        // -- the 2 points of the edge of degree 3 must be of each side of the
        // -- middle plane
        intersect2 := TGBGeomTools.IntersectionEdgeTriangle(pt, removePt, p,
          q, planePt);
        Assert(intersect2 <> -1);
        if ((intersect2 = 1) or (intersect2 = 2) or (intersect2 = 0)) then
        begin
          // -- 3rd, test if the ear is DELAUNAY
          if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
          begin
            updated := True;
            // -- INTERSECT = 0
            Flip32(te, adj, te3);
            lstTetra.Remove(te3);
            te3.Free;
            lstPts.Remove(removePt);
            if (te.HasPoint(pt) = False) then
              lstTetra.Remove(te)
            else
              lstTetra.Remove(adj);
          end;
        end;
      end
      else if (AreTetraConfig44(te, adj, te1, adj1) <> 0) then
      begin
        if ((self.IsEarDelaunay(te, adj, pt, lstPts) = True) and
          (self.IsEarDelaunay(te1, adj1, pt, lstPts) = True)) then
        begin
          // -- check if the 2 other tetra are inside H
          Assert(lstTetra.IndexOf(te1) <> -1);
          Assert(lstTetra.IndexOf(adj1) <> -1);

          updated := True;
          newTetra := Flip44(te, adj, te1, adj1, flat);

          // -- update the upper 2 tetra.
          if (te = flat) then
          begin
            lstTetra.Remove(te);
            te.Free;
            if (newTetra.HasPoint(pt) = True) then
            begin
              lstTetra.Add(newTetra);
              lstTetra.Remove(adj);
            end;
          end
          else if (adj = flat) then
          begin
            lstTetra.Remove(adj);
            adj.Free;
            if (newTetra.HasPoint(pt) = True) then
            begin
              lstTetra.Add(newTetra);
              lstTetra.Remove(te);
            end;
          end
          else // -- newTetra is flat
          begin
            newTetra.Free;
            if (te.HasPoint(pt) = True) then
              lstTetra.Remove(adj)
            else
              lstTetra.Remove(te);
          end;

          // -- update the 2 tetra 'under'
          if (te1.HasPoint(pt) = True) then
          begin
            lstTetra.Remove(adj1);
            removePt := adj1.SetIndex(te1).GetProt(0);
          end
          else
          begin
            lstTetra.Remove(te1);
            removePt := te1.SetIndex(adj1).GetProt(0);
          end;
          lstPts.Remove(removePt);
        end;
      end;
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: To determine what action to do with an ear of the polyhedron. If
// the ear is valid (i.e. convex, Delaunay, flippable) the
// appropriate flip is performed.

// Input:       ear     : the ear to flip
// lstPts  : list of vertices forming H
// lstTetra: list of tetra incident to pt

// Output:      0 -> no flip was performed
// 1 -> a flip23 or flip32 was performed
// 2 -> (config44 = 2), no flip was performed.
// ******************************************************************************
function TTetraMesh.ProcessEar_e(ear: TGBEar; lstPts, lstEar: TList;
  flatflag: boolean): integer;
var
  updated: integer;
  intersect: integer;
begin
  updated := 0;
  if (ear.IsConvex = 1) then
  begin
    intersect := ear.IsFlippable;
    Assert(intersect <> 2);
    Assert(intersect <> 3);
    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      if (ear.IsDelaunay(lstPts) = True) then
      begin
        ProcessEar_flip23(ear);
        updated := 1;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      if (ProcessEar_flip32(ear, lstEar, lstPts) = True) then
        updated := 1;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      updated := ProcessEar_degenerate_e(ear, lstEar, lstPts, flatflag);
      // updated := ProcessEar_degenerate_e(ear, lstEar, lstPts);
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: To determine what action to do with an ear of the polyhedron. If
// the ear is valid (i.e. convex, Delaunay, flippable) the
// appropriate flip is performed.

// Input:       ear     : the ear to flip
// lstPts  : list of vertices forming H
// lstTetra: list of tetra incident to pt

// Output:      0 -> no flip was performed
// 1 -> a flip23 or flip32 was performed
// 2 -> (config44 = 2), no flip was performed.
// ******************************************************************************
function TTetraMesh.ProcessEar_e_pert(ear: TGBEar; lstPts, lstEar: TList;
  flatflag: boolean): integer;
var
  updated: integer;
  intersect: integer;
begin
  updated := 0;
  if (ear.IsConvex = 1) then
  begin
    intersect := ear.IsFlippable;
    Assert(intersect <> 2);
    Assert(intersect <> 3);
    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      if (ear.IsDelaunay(lstPts) = True) then
      begin
        ProcessEar_flip23(ear);
        updated := 1;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      if (ProcessEar_flip32(ear, lstEar, lstPts) = True) then
        updated := 1;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      updated := ProcessEar_degenerate_e(ear, lstEar, lstPts, flatflag);
      // updated := ProcessEar_degenerate_e(ear, lstEar, lstPts);
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: *** TEST VERSION - ProcessEar is supposed for every case ***
// To determine what action to do with an ear of the polyhedron. If
// the ear is valid (i.e. convex, Delaunay, flippable) the
// appropriate flip is performed.

// Input:       te, adj : the 2 tetra forming the ear
// pt      : the vertex to delete
// lstPts  : list of vertices forming H
// lstTetra: list of tetra incident to pt

// Output:      TRUE  -> a flip was performed; lstPts & lstEar were updated.
// FALSE -> no flip was performed
// ******************************************************************************
function TTetraMesh.ProcessEar3(te, adj: TGBTetrahedron; pt: TGBPoint3D;
  lstPts, lstTetra: TList): boolean;
var
  p: TGBPoint3D;
  q: TGBPoint3D;
  intersect: integer;
  intersect2: integer;
  te3: TGBTetrahedron;
  updated: boolean;
  newTetra: TGBTetrahedron;
  i: integer;
  planePt: TGBPoint3D;
  removePt: TGBPoint3D;
  tempt: TGBPoint3D;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
  flat: TGBTetrahedron;
begin
  updated := False;

  // -- 1st, the ear must be CONVEX
  if (self.IsEarConvex(te, adj, pt) = True) then
  begin
    // -- 2nd, the ear must be FLIPPABLE
    // -- does the edge pass through the triangle?
    p := te.SetIndex(adj).GetProt(0);
    q := adj.SetIndex(te).GetProt(0);

    // -- do the right flip according to the position of the tetrahedra
    intersect := TGBGeomTools.IntersectionEdgeTriangle(p, q, te.GetProt(1),
      te.GetProt(2), te.GetProt(3));
    Assert(intersect <> 2);
    Assert(intersect <> 3);

    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      // -- 3rd, test if the ear is DELAUNAY
      if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
      begin
        updated := True;
        newTetra := Flip23(te, adj);
        // -- if newTetra is the tetra that is deleted from H, then no need to
        // -- to add it or delete it from the list lstTetra
        if (te.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(te);
          lstTetra.Add(newTetra);
        end
        else if (adj.HasPoint(pt) = False) then
        begin
          lstTetra.Remove(adj);
          lstTetra.Add(newTetra);
        end;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      te3 := Find3rdTetra(te, adj);
      // -- this 3rd tetra must exist
      if ((te3 <> nil) and (lstTetra.IndexOf(te3) <> -1)) then
      begin
        removePt := nil;
        planePt := nil;
        // -- find the point that will be removed from H at the end; and also find
        // -- the point which will be part of the future facet after the flip.
        for i := 1 to 3 do
        begin
          tempt := te.GetProt(i);
          if (tempt <> pt) then
          begin
            if (te3.HasPoint(tempt) = True) then
              removePt := tempt
            else
              planePt := tempt;
          end;
        end;

        // -- the 2 points of the edge of degree 3 must be of each side of the
        // -- middle plane
        intersect2 := TGBGeomTools.IntersectionEdgeTriangle(pt, removePt, p,
          q, planePt);
        if ((intersect2 = 1) or (intersect2 = 2) or (intersect2 = 0)) then
        begin
          // -- 3rd, test if the ear is DELAUNAY
          if (self.IsEarDelaunay(te, adj, pt, lstPts) = True) then
          begin
            updated := True;
            Flip32(te, adj, te3);
            lstTetra.Remove(te3);
            te3.Free;
            lstPts.Remove(removePt);
            if (te.HasPoint(pt) = False) then
              lstTetra.Remove(te)
            else
              lstTetra.Remove(adj);
          end;
        end;
      end;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      if ((lstTetra.Count > 5) and (AreTetraConfig44(te, adj, te1, adj1) <> 0))
      then
      begin
        if ((self.IsEarDelaunay(te, adj, pt, lstPts) = True) and
          (self.IsEarDelaunay(te1, adj1, pt, lstPts) = True)) then
        begin
          // -- check if the 2 other tetra are inside H
          Assert(lstTetra.IndexOf(te1) <> -1);
          Assert(lstTetra.IndexOf(adj1) <> -1);

          updated := True;
          newTetra := Flip44(te, adj, te1, adj1, flat);

          // -- update the upper 2 tetra.
          if (te = flat) then
          begin
            lstTetra.Remove(te);
            te.Free;
            if (newTetra.HasPoint(pt) = True) then
            begin
              lstTetra.Add(newTetra);
              lstTetra.Remove(adj);
            end;
          end
          else if (adj = flat) then
          begin
            lstTetra.Remove(adj);
            adj.Free;
            if (newTetra.HasPoint(pt) = True) then
            begin
              lstTetra.Add(newTetra);
              lstTetra.Remove(te);
            end;
          end
          else // -- newTetra is flat
          begin
            newTetra.Free;
            if (te.HasPoint(pt) = True) then
              lstTetra.Remove(adj)
            else
              lstTetra.Remove(te);
          end;

          // -- update the 2 tetra 'under'
          if (te1.HasPoint(pt) = True) then
          begin
            lstTetra.Remove(adj1);
            removePt := adj1.SetIndex(te1).GetProt(0);
          end
          else
          begin
            lstTetra.Remove(te1);
            removePt := te1.SetIndex(adj1).GetProt(0);
          end;
          lstPts.Remove(removePt);
        end;
      end;
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: *** MODIFICATION OF FLIP METHOD. PERFORMS THE SAME THING ***
// flip 2 tetra that are not Delaunay.  This function in fact
// determine which flip (Flip23, Flip32 or Flip44) to perform.

// Input:       te and adj : the 2 tetra
// stack and lstDeleted : to keep track of the changes

// Output:      -

// Remarks:     see Remarks of method Flip
// ******************************************************************************
procedure TTetraMesh.Flip__2(te, adj: TGBTetrahedron; stack: TStack;
  lstDeleted: TListz);
var
  p: TGBPoint3D;
  d: TGBPoint3D;
  intersect: integer;
  newTetra: TGBTetrahedron;
  te3: TGBTetrahedron;
  te1: TGBTetrahedron;
  adj1: TGBTetrahedron;
begin
  // -- does the edge pass through the triangle?
  p := te.SetIndex(adj).GetProt(0);
  d := adj.SetIndex(te).GetProt(0);

  // -- do the right flip according to the position of the tetrahedra
  intersect := TGBGeomTools.IntersectionEdgeTriangle(p, d, te.GetProt(1),
    te.GetProt(2), te.GetProt(3));
  // -- case #1 : the edge passes through the triangle
  if (intersect = 1) then
  begin
    newTetra := Flip23(te, adj);
    stack.Push(te);
    stack.Push(adj);
    stack.Push(newTetra);
  end
  // -- case #2 : the edge doesn't pass through the triangle
  else if (intersect = -1) then
  begin
    te3 := Find3rdTetra(te, adj);
    if (te3 <> nil) then
    begin
      // -- case #2 and no other tetra are "between" p and d : Flip32.
      // -- if there is one (or more) tetra between : do NOTHING, the problem
      // -- will be solved somewhere else by flipping another adjacent tetra
      Flip32(te, adj, te3);
      stack.Push(te);
      stack.Push(adj);
      // -- put the deleted tetra in a temporary list
      lstDeleted.Add(te3);
    end;
  end
  // -- case #3 : p, d and 2 points in the triangle are coplanar
  else if (intersect = 0) then
  begin
    if (AreTetraConfig44(te, adj, te1, adj1) <> 0) then
    begin
      newTetra := Flip23(te, adj);
      stack.Push(te);
      stack.Push(adj);
      stack.Push(newTetra);
    end;
  end
  else // -- (intersect = 2) [4 pts coplanar according to 2 planes]
  begin
    te3 := Find3rdTetra(te, adj);
    if (te3 <> nil) then
    begin
      // -- case #2 and no other tetra are "between" p and d : Flip32.
      // -- if there is one (or more) tetra between : do NOTHING, the problem
      // -- will be solved somewhere else by flipping another adjacent tetra
      Flip32(te, adj, te3);
      stack.Push(te);
      stack.Push(adj);
      // -- put the deleted tetra in a temporary list
      lstDeleted.Add(te3);
    end
    else
    begin
      newTetra := Flip23(te, adj);
      stack.Push(te);
      stack.Push(adj);
      stack.Push(newTetra);
    end;
  end;
end;


// ******************************************************************************
// Description: Insertion of a vertex in a tetra; the operation creates the new
// point, split the tetra into 4 tetra and updates the global lists

// Input:       pt    : the TGBPoint3D to insert in the mesh
// split : the tetra that contains the point

// Output:      -

// Remarks:     -
// ******************************************************************************
procedure TTetraMesh.Flip14(pt: TGBPoint3D; split: TGBTetrahedron);
var
  new1: TGBTetrahedron;
  new2: TGBTetrahedron;
  new3: TGBTetrahedron;
  adj: TGBTetrahedron;
begin
  // -- create and update the points for the 4 tetrahedra
  new1 := TGBTetrahedron.Create(pt, split.GetP(3), split.GetP(1),
    split.GetP(4));
  new2 := TGBTetrahedron.Create(pt, split.GetP(4), split.GetP(1),
    split.GetP(2));
  new3 := TGBTetrahedron.Create(pt, split.GetP(1), split.GetP(3),
    split.GetP(2));
  split.SetP(1, pt);

  // -- update Neighbours of the adjacent tetra
  adj := split.GetT(2);
  if (adj <> nil) then
    adj.SetIndex(split).SetTrot(0, new1);
  adj := split.GetT(3);
  if (adj <> nil) then
    adj.SetIndex(split).SetTrot(0, new2);
  adj := split.GetT(4);
  if (adj <> nil) then
    adj.SetIndex(split).SetTrot(0, new3);

  // -- update neighbours of the 4 new tetra
  new1.SetIndex(1).SetTrot(0, split.GetT(2));
  new1.SetTrot(1, new2);
  new1.SetTrot(2, split);
  new1.SetTrot(3, new3);

  new2.SetIndex(1).SetTrot(0, split.GetT(3));
  new2.SetTrot(1, new3);
  new2.SetTrot(2, split);
  new2.SetTrot(3, new1);

  new3.SetIndex(1).SetTrot(0, split.GetT(4));
  new3.SetTrot(1, split);
  new3.SetTrot(2, new2);
  new3.SetTrot(3, new1);

  split.SetT(2, new1);
  split.SetT(3, new2);
  split.SetT(4, new3);

  split.ResetCentre;
  new1.ResetCentre;
  new2.ResetCentre;
  new3.ResetCentre;
  m_curTe := new1;
end;


// ******************************************************************************
// Description: Draw the 'envelope' around a point in the mesh. The envelope
// represents all the exterior faces of the tetra that have a point
// pt as one of their vertices. Mostly used for visualization for
// deletion. This version of SetEnvelope highlights one ear

// Input:       v     : the point
// ear   : the ear to highlight
// lstEar: the list of ears forming the polyhedron H
// lstPts: list of points forming the polyhedron H

// Output:      - (the display list for the envelope is built)
// ******************************************************************************
{ procedure TTetraMesh.SetEnvelope(v: TGBPoint3D; ear: TEar; lstEar, lstPts: TList);
  var
  i: integer;
  te1: TTetra;
  tempt: TGBPoint3D;
  tempear: TEar;
  te: TTetra;
  adj: TTetra;
  begin
  glDeleteLists(m_dlDeletion, 1);
  m_dlDeletion := glGenLists(1);
  glNewList(m_dlDeletion, GL_COMPILE);
  //-- draw the node in the middle
  TDrawTools.DrawNode(v, 1.0, 0, 0, 1);
  if (ear <> nil) then
  begin
  te := ear.GetTetra(1);
  adj := ear.GetTetra(2);
  //-- draw HL ears
  TDrawTools.DrawTetraFrame(te, 1, 0, 0, 2);
  TDrawTools.DrawTetraFrame(adj, 1, 0, 0, 2);
  te.SetIndex(v);
  adj.SetIndex(v);
  TDrawTools.DrawTriangleFace(te.GetProt(1), te.GetProt(2), te.GetProt(3),
  te.GetProt(0), 0.5, 0, 0, 0.5);
  TDrawTools.DrawTriangleFace(adj.GetProt(1), adj.GetProt(2), adj.GetProt(3),
  adj.GetProt(0), 0, 0.5, 0, 0.5);
  end;

  //-- draw edges connecting pt and the points of the envelope
  for i := 0 to (lstPts.Count - 1) do
  begin
  tempt := lstPts[i];
  TDrawTools.DrawLine(v, tempt, 0.0, 0.8, 0.0, 2);
  end;

  for i := 0 to (lstEar.Count - 1) do
  begin
  tempear := lstEar[i];
  te1 := tempear.GetTetra(1);
  te1.SetIndex(v);
  //-- draw the exterior face
  TDrawTools.DrawTriangleFace(te1.GetProt(1), te1.GetProt(2), te1.GetProt(3),
  v, 0.4, 0.4, 0.4, 0.7);
  TDrawTools.DrawTriangleFrame(te1.GetProt(1), te1.GetProt(2), te1.GetProt(3),
  0.0, 0.0, 0.5, 1);
  te1 := tempear.GetTetra(2);
  te1.SetIndex(v);
  //-- draw the exterior face
  TDrawTools.DrawTriangleFace(te1.GetProt(1), te1.GetProt(2), te1.GetProt(3),
  v, 0.4, 0.4, 0.4, 0.7);
  TDrawTools.DrawTriangleFrame(te1.GetProt(1), te1.GetProt(2), te1.GetProt(3),
  0.0, 0.0, 0.5, 1);
  end;
  glEndList;
  end;
}

// ******************************************************************************
// Description: Test the ears of a polyhedron to see if they are 'coherent'.

// Input:       lstEar : the list of ears

// Output:      -

// Note:        An exception is raised if an ear is not coherent
// ******************************************************************************
procedure TTetraMesh.TestEars(lstEar: TList);
var
  i: integer;
  j: integer;
  k: integer;
  ear: TGBEar;
  tempear: TGBEar;
  tempear2: TGBEar;
  te: TGBTetrahedron;
  adj: TGBTetrahedron;
  v: TGBPoint3D;
begin
  ear := lstEar[0];
  v := ear.GetVertex;
  for i := 0 to (lstEar.Count - 1) do
  begin
    ear := lstEar[i];
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    if ((te.HasNeighbour(adj) = False) or (adj.HasNeighbour(te) = False) or
      (te.HasPoint(v) = False) or (adj.HasPoint(v) = False)) then
    begin
      raise Exception.Create('[Ear not normal]');
    end;

    for j := 1 to 2 do
    begin
      tempear := ear.GetAdj(j);
      if (tempear.ContainTetra(te) = False) then
      begin
        raise Exception.Create('[Ear not normal]');
      end;
      if not((te.HasNeighbour(tempear.GetTetra(1)) = True)
        xor (te.HasNeighbour(tempear.GetTetra(2)) = True)) then
      begin
        raise Exception.Create('[Ear not normal]');
      end;
    end;
    for j := 3 to 4 do
    begin
      tempear := ear.GetAdj(j);
      if (tempear.ContainTetra(adj) = False) then
      begin
        raise Exception.Create('[Ear not normal]');
      end;
      if not((adj.HasNeighbour(tempear.GetTetra(1)) = True)
        xor (adj.HasNeighbour(tempear.GetTetra(2)) = True)) then
      begin
        raise Exception.Create('[Ear not normal]');
      end;
    end;

    for j := 1 to 4 do
    begin
      tempear := ear.GetAdj(j);
      for k := (j + 1) to 4 do
      begin
        tempear2 := ear.GetAdj(k);
        if (tempear.m_no = tempear2.m_no) then
        begin
          raise Exception.Create('[Ear not normal]');
        end;
      end;
    end;
  end;
end;


// ******************************************************************************
// Description: Flip23 and update of the ears when a vertex is deleted. It is
// used for a 2-ear (ear formed by 2 facets sharing an edge)

// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H

// Output:      - (lstEar is updated)

// Note:        No ears are deleted from the list of ears. The ear that is
// flipped is replaced by another one, so we update the 4
// neighbouring ears and modify the value of the current ear.
// ******************************************************************************
procedure TTetraMesh.ProcessEar_flip23(ear: TGBEar);
var
  te: TGBTetrahedron;
  adj: TGBTetrahedron;
  newTetra: TGBTetrahedron;
  lstAdjEar: TList;
  v: TGBPoint3D;
  stay1: TGBTetrahedron;
  stay2: TGBTetrahedron;
  i, j, k, n: integer;
  tempear, tempear2: TGBEar;
  tempte, tempte2: TGBTetrahedron;
begin
  v := ear.GetVertex;
  te := ear.GetTetra(1);
  adj := ear.GetTetra(2);
  newTetra := Flip23(te, adj);

  // -- construct a TList of ear + the 4 adj ears
  lstAdjEar := TList.Create;
  lstAdjEar.Add(ear);
  for i := 1 to 4 do
  begin
    lstAdjEar.Add(ear.GetAdj(i));
  end;

  // -- identify the 2 tetra that stay inside H after the flip
  if (te.HasPoint(v) = False) then
  begin
    stay1 := newTetra;
    stay2 := adj;
  end
  else if (adj.HasPoint(v) = False) then
  begin
    stay1 := te;
    stay2 := newTetra;
  end
  else // -- if (newTetra.HasPoint(v) = false)
  begin
    stay1 := te;
    stay2 := adj;
  end;

  // -- update all the fields of 'ear'
  ear.SetTetra(1, stay1);
  ear.SetTetra(2, stay2);
  stay1.SetIndex(stay2);
  n := 1;
  for i := 1 to 3 do
  begin
    if (stay1.GetProt(i) <> v) then
    begin
      tempte := stay1.GetTrot(i);
      tempear := nil;
      // -- get ear in lstAdjEar that contains tempte
      for j := 1 to (lstAdjEar.Count - 1) do
      begin
        tempear := lstAdjEar[j];
        if (tempear.ContainTetra(tempte) = True) then
        begin
          break;
        end;
      end;
      ear.SetAdj(n, tempear);
      Inc(n);
      // -- update tetra field of tempear
      if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj)) then
        tempear.SetTetra(1, stay1)
      else
        tempear.SetTetra(2, stay1);
    end;
  end;
  stay2.SetIndex(stay1);
  n := 3;
  for i := 1 to 3 do
  begin
    if (stay2.GetProt(i) <> v) then
    begin
      tempte := stay2.GetTrot(i);
      tempear := nil;
      // -- get ear in lstAdjEar that contains tempte
      for j := 1 to (lstAdjEar.Count - 1) do
      begin
        tempear := lstAdjEar[j];
        if (tempear.ContainTetra(tempte) = True) then
        begin
          break;
        end;
      end;
      ear.SetAdj(n, tempear);
      Inc(n);
      // -- update tetra field of tempear
      if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj)) then
        tempear.SetTetra(1, stay2)
      else
        tempear.SetTetra(2, stay2);
    end;
  end;
  // -- update of the adj fields of the 4 adjacent ears
  for i := 1 to 4 do
  begin
    tempear := ear.GetAdj(i);
    // -- only 2 fields (linked to the same tetra) need to be changed.
    // -- the ones where either stay1 or stay2 are.
    tempte := tempear.GetTetra(1);
    if ((tempte = stay1) or (tempte = stay2)) then
    begin
      n := 1;
      tempte.SetIndex(tempear.GetTetra(2));
    end
    else
    begin
      n := 3;
      tempte := tempear.GetTetra(2);
      tempte.SetIndex(tempear.GetTetra(1));
    end;
    for j := 1 to 3 do
    begin
      if (tempte.GetProt(j) <> v) then
      begin
        tempte2 := tempte.GetTrot(j);
        // -- get ear in lstAdjEar that contains tempte
        tempear2 := nil;
        for k := 0 to (lstAdjEar.Count - 1) do
        begin
          tempear2 := lstAdjEar[k];
          if ((tempear2.ContainTetra(tempte2) = True) and (tempear2 <> tempear))
          then
          begin
            break;
          end;
        end;
        Assert(tempear2 <> nil);
        tempear.SetAdj(n, tempear2);
        Inc(n);
      end;
    end;
  end;
  // -- update the power of the 5 modified ears
  if (ear is TGBEarp) then
  begin
    TGBEarp(ear).UpdatePower;
    for i := 1 to 4 do
    begin
      TGBEarp(ear.GetAdj(i)).UpdatePower;
    end;
  end;
  lstAdjEar.Free;
end;


function TTetraMesh.ProcessEar_flip32(ear: TGBEar;
  lstEar, lstPts: TList): boolean;
var
  te, adj, te3: TGBTetrahedron;
  v: TGBPoint3D;
  removePt, planePt: TGBPoint3D;
  tempt: TGBPoint3D;
  i, j, n: integer;
  intersect2: integer;
  ear3: array [1 .. 2] of TGBEar;
  ext: array [1 .. 3] of TGBEar;
  updated: boolean;
  tempear: TGBEar;
  stay1: TGBTetrahedron;
  tempte: TGBTetrahedron;
begin
  updated := False;
  v := ear.GetVertex;
  te := ear.GetTetra(1); // -- is the ear a 3-ear
  adj := ear.GetTetra(2);
  te3 := Find3rdTetra(te, adj);
  // -- this 3rd tetra must exist & be inside H
  if (te3 <> nil) then
  begin
    removePt := nil;
    planePt := nil;
    te.SetIndex(adj);
    adj.SetIndex(te);
    // -- find the point that will be removed from H at the end; and also find
    // -- the point which will be part of the future facet after the flip.
    for i := 1 to 3 do
    begin
      tempt := te.GetProt(i);
      if (tempt <> v) then
      begin
        if (te3.HasPoint(tempt) = True) then
          removePt := tempt
        else
          planePt := tempt;
      end;
    end;

    // -- the 2 points of the edge of degree 3 must be of each side of the
    // -- middle plane
    intersect2 := TGBGeomTools.IntersectionEdgeTriangle(v, removePt,
      te.GetProt(0), adj.GetProt(0), planePt);
    if (intersect2 <> -1) then
    begin
      // Assert(ear.IsDelaunay(lstPts) = true);  { TODO : 3-ear always Delaunay? }
      // -- 3rd, test if the ear is DELAUNAY
      if (ear.IsDelaunay(lstPts) = True) then
      begin
        // -- get pointers to 3 ears forming the 3ear & to 3 exterior ears
        j := 1;
        for i := 1 to 4 do
        begin
          tempear := ear.GetAdj(i);
          if (tempear.ContainTetra(te3) = True) then
          begin
            ear3[j] := tempear;
            Inc(j);
          end
          else
          begin
            if (tempear.ContainTetra(te) = True) then
              ext[1] := tempear
            else
              ext[2] := tempear;
          end;
        end;
        for i := 1 to 4 do
        begin
          tempear := ear3[1].GetAdj(i);
          if ((tempear.ContainTetra(te3) = True) and
            (tempear.ContainTetra(te) = False) and
            (tempear.ContainTetra(adj) = False)) then
          begin
            ext[3] := tempear;
            break;
          end;
        end;
        // -- perform the flip
        updated := True;
        Flip32(te, adj, te3);
        lstPts.Remove(removePt); // -- remove 'exterior' point from TList
        if (te.HasPoint(v) = True) then
        begin
          stay1 := te;
        end
        else
        begin
          stay1 := adj;
        end;

        // -- update the 3 ears (the ext ears) adjacent to the 3 deleted ears
        for i := 1 to 3 do
        begin
          tempear := ext[i];
          if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj) or
            (tempear.GetTetra(1) = te3)) then
          begin
            tempear.SetTetra(1, stay1);
          end
          else
          begin
            tempear.SetTetra(2, stay1);
          end;
        end;

        // -- update of the adj fields of the 3 ext ears
        for i := 1 to 3 do
        begin
          tempear := ext[i];
          tempte := tempear.GetTetra(1);
          if (tempte = stay1) then
          begin
            n := 1;
            tempte.SetIndex(tempear.GetTetra(2));
          end
          else
          begin
            n := 3;
            tempte := tempear.GetTetra(2);
            tempte.SetIndex(tempear.GetTetra(1));
          end;
          if (i = 1) then
          begin
            tempear.SetAdj(n, ext[2]);
            tempear.SetAdj(n + 1, ext[3]);
          end
          else if (i = 2) then
          begin
            tempear.SetAdj(n, ext[1]);
            tempear.SetAdj(n + 1, ext[3]);
          end
          else // if (i = 3) then
          begin
            tempear.SetAdj(n, ext[1]);
            tempear.SetAdj(n + 1, ext[2]);
          end;
        end;
        // -- destroy 3 ears forming the 3ear
        lstEar.Remove(ear);
        lstEar.Remove(ear3[1]);
        lstEar.Remove(ear3[2]);
        ear.Free;
        ear3[1].Free;
        ear3[2].Free;
        te3.Free;
      end;
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: Flip44 and update of the ears when a vertex is deleted. It is
// used for a 4-ear; when intersection returns 0 and the ear is in
// config44.

// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)

// Output:      TRUE  -> flip44 was possible (lstEar is updated)
// FALSE -> flip44 was impossible and no flip was performed.
// ******************************************************************************
function TTetraMesh.ProcessEar_flip44(ear: TGBEar;
  lstEar, lstPts: TList): boolean;
var
  v: TGBPoint3D;
  te, adj, te1, adj1: TGBTetrahedron;
  ear2: TGBEar;
  lstInt, lstExt: TList;
  i, j, k: integer;
  n: integer;
  tempear, tempear2: TGBEar;
  tempte, tempte2: TGBTetrahedron;
  newTetra, flat: TGBTetrahedron;
  removePt: TGBPoint3D;
  updated: boolean;
  stay1, stay2: TGBTetrahedron;
  te3: TGBTetrahedron;
  continue: boolean;
  config44: integer;
begin
  updated := False;
  continue := True;
  te := ear.GetTetra(1);
  adj := ear.GetTetra(2);
  v := ear.GetVertex;
  config44 := AreTetraConfig44(te, adj, te1, adj1);
  if (config44 = 2) then
  begin
    // -- check if 3rd tetra exists 'under' one the 4 tetra
    te3 := Find3rdTetra(te, te1);
    if ((te3 <> nil) and (te3.HasPoint(v) = True)) then
      continue := False;
    if (continue = True) then
    begin
      te3 := Find3rdTetra(adj, adj1);
      if ((te3 <> nil) and (te3.HasPoint(v) = True)) then
        continue := False;
    end;
    if (continue = True) then
    begin
      te3 := Find3rdTetra(te1, adj1);
      if ((te3 <> nil) and (te3.HasPoint(v) = True)) then
        continue := False;
    end;
  end;

  if ((config44 <> 0) and (continue = True)) then
  begin
    // -- build 2 lists: ears 'inside' and 'outside'
    lstInt := TList.Create;
    lstExt := TList.Create;
    lstInt.Add(ear);
    for i := 1 to 4 do
    begin
      tempear := ear.GetAdj(i);
      if ((tempear.ContainTetra(te1) = True) or
        (tempear.ContainTetra(adj1) = True)) then
        lstInt.Add(tempear)
      else
        lstExt.Add(tempear);
    end;
    tempear := lstInt[1]; // -- tempear = te-te1
    for i := 1 to 4 do
    begin
      tempear2 := tempear.GetAdj(i);
      if (tempear2.ContainTetra(te1) = True) then
      begin
        if (tempear2.ContainTetra(adj1) = True) then
          lstInt.Add(tempear2)
        else
          lstExt.Add(tempear2);
      end;
    end;
    tempear := lstInt[3];
    for i := 1 to 4 do
    begin
      tempear2 := tempear.GetAdj(i);
      if ((tempear2.ContainTetra(adj1) = True) and
        (tempear2.ContainTetra(adj) = False)) then
      begin
        lstExt.Add(tempear2);
        break;
      end;
    end;
    ear2 := lstInt[3]; // -- ear2 is te1-adj1 before the flip

    // -- check if the 2 new ears are Delaunay
    if ((ear.IsDelaunay(lstPts) = True) and (ear2.IsDelaunay(lstPts) = True))
    then
    begin
      // -- check if the 2 other tetra are inside H
      Assert(te1.HasPoint(v) = True);
      Assert(adj1.HasPoint(v) = True);
      updated := True;
      newTetra := Flip44(te, adj, te1, adj1, flat);

      // -- update the 2 'upper' tetra
      if (te = flat) then
      begin
        if (adj.HasPoint(v) = True) then
          stay1 := adj
        else
          stay1 := newTetra;
      end
      else if (adj = flat) then
      begin
        if (te.HasPoint(v) = True) then
          stay1 := te
        else
          stay1 := newTetra;
      end
      else // -- if (newTetra = flat)
      begin
        if (te.HasPoint(v) = True) then
          stay1 := te
        else
          stay1 := adj;
      end;

      // -- update the 2 tetra 'under' & delete the exterior vertex
      if (te1.HasPoint(v) = True) then
      begin
        stay2 := te1;
        removePt := adj1.SetIndex(te1).GetProt(0);
      end
      else
      begin
        stay2 := adj1;
        removePt := te1.SetIndex(adj1).GetProt(0);
      end;
      lstPts.Remove(removePt);

      // -- update all the fields of 'ear'
      ear.SetTetra(1, stay1);
      ear.SetTetra(2, stay2);
      stay1.SetIndex(stay2);
      n := 1;
      for i := 1 to 3 do
      begin
        if (stay1.GetProt(i) <> v) then
        begin
          tempte := stay1.GetTrot(i);
          tempear := nil;
          // -- get ear in lstExt that contains tempte
          for j := 0 to (lstExt.Count - 1) do
          begin
            tempear := lstExt[j];
            if (tempear.ContainTetra(tempte) = True) then
            begin
              break;
            end;
          end;
          Assert(tempear <> nil);
          ear.SetAdj(n, tempear);
          Inc(n);
          // -- update tetra field of tempear
          if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj) or
            (tempear.GetTetra(1) = te1) or (tempear.GetTetra(1) = adj1)) then
            tempear.SetTetra(1, stay1)
          else
            tempear.SetTetra(2, stay1);
        end;
      end;
      stay2.SetIndex(stay1);
      n := 3;
      for i := 1 to 3 do
      begin
        if (stay2.GetProt(i) <> v) then
        begin
          tempte := stay2.GetTrot(i);
          // -- get ear in lstExt that contains tempte
          for j := 0 to (lstExt.Count - 1) do
          begin
            tempear := lstExt[j];
            if (tempear.ContainTetra(tempte) = True) then
            begin
              tempear2 := tempear;
            end;
          end;
          Assert(tempear2 <> nil);
          ear.SetAdj(n, tempear2);
          Inc(n);
          // -- update tetra field of tempear
          if ((tempear2.GetTetra(1) = te) or (tempear2.GetTetra(1) = adj) or
            (tempear2.GetTetra(1) = te1) or (tempear2.GetTetra(1) = adj1)) then
            tempear2.SetTetra(1, stay2)
          else
            tempear2.SetTetra(2, stay2);
        end;
      end;

      // -- update of the adj fields of the 4 adjacent ears
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        // -- only 2 fields (linked to the same tetra) need to be changed.
        // -- the ones where either stay1 or stay2 are.
        tempte := tempear.GetTetra(1);
        if ((tempte = stay1) or (tempte = stay2)) then
        begin
          n := 1;
          tempte.SetIndex(tempear.GetTetra(2));
        end
        else
        begin
          n := 3;
          tempte := tempear.GetTetra(2);
          tempte.SetIndex(tempear.GetTetra(1));
        end;
        for j := 1 to 3 do
        begin
          if (tempte.GetProt(j) <> v) then
          begin
            tempte2 := tempte.GetTrot(j);
            if ((tempte2 = stay1) or (tempte2 = stay2)) then
            begin
              tempear.SetAdj(n, ear);
            end
            else
            begin
              // -- get ear in lstExt that contains tempte
              tempear2 := nil;
              for k := 0 to (lstExt.Count - 1) do
              begin
                tempear2 := lstExt[k];
                if ((tempear2.ContainTetra(tempte) = True) and
                  (tempear2.ContainTetra(tempte2) = True) and (tempear2 <> ear))
                then
                begin
                  break;
                end;
              end;
              tempear.SetAdj(n, tempear2);
            end;
            Inc(n);
          end;
        end;
      end;
      flat.Free;
      lstEar.Remove(lstInt[1]);
      lstEar.Remove(lstInt[2]);
      lstEar.Remove(lstInt[3]);
    end;
    lstInt.Clear;
    lstInt.Free;
    lstExt.Clear;
    lstExt.Free;
  end;
  Result := updated;
end;

function TTetraMesh.GetMaxList(lst: TList): TGBEar;
var
  i: integer;
  power: double;
  ear, tempear: TGBEar;
  temp: double;
begin
  ear := lst[0];
  power := TGBEarp(ear).GetPower;
  for i := 1 to (lst.Count - 1) do
  begin
    tempear := lst[i];
    temp := TGBEarp(tempear).GetPower;
    if (temp > power) then
    begin
      power := temp;
      ear := tempear;
    end;
  end;
  Result := ear;
end;


// ******************************************************************************
// Description: Process a single ear when deleting a vertex. The ear processed
// is the ear with the biggest power.

// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)

// Output:      0 -> no flip was performed
// 1 -> a flip23 or flip32 was performed
// 2 -> (config44 = 2), no flip was performed.
// ******************************************************************************
function TTetraMesh.ProcessEar_p(ear: TGBEar; lstEar: TListz; lstPts: TList;
  flag44: boolean): integer;
var
  intersect: integer;
  updated: integer;
begin
  updated := 0;
  Assert(ear.IsConvex = 1);
  // Assert(ear.IsDelaunay(lstPts) = true);
  // if (ear.IsDelaunay(lstPts) = false) then //**************************
  // ShowMessage('not delaunay');           //**************************

  intersect := ear.IsFlippable;
  Assert(intersect <> 2);
  Assert(intersect <> 3);

  // -------------- Flip23 -------------------------------
  if (intersect = 1) then
  begin
    ProcessEar_flip23(ear);
    updated := 1;
  end
  // -------------- Flip32 -------------------------------
  else if (intersect = -1) then
  begin
    if (ProcessEar_flip32_p(ear, lstEar, lstPts) = True) then
      updated := 1;
  end
  // -------------- Degenerate cases -------------------------------
  else if (intersect = 0) then
  begin
    updated := ProcessEar_degenerate_p__2(ear, lstEar, lstPts, flag44);
  end;
  Result := updated;
end;

// -- flip32 associated with Deviller's deletion method
function TTetraMesh.ProcessEar_flip32_p(ear: TGBEar; lstEar: TListz;
  lstPts: TList): boolean;
var
  v: TGBPoint3D;
  te, adj, te3: TGBTetrahedron;
  i, j, n: integer;
  tempt: TGBPoint3D;
  tempear: TGBEar;
  tempte: TGBTetrahedron;
  ear3: array [1 .. 2] of TGBEar;
  ext: array [1 .. 3] of TGBEar;
  stay1: TGBTetrahedron;
  removePt: TGBPoint3D;
  planePt: TGBPoint3D;
  updated: boolean;
  intersect2: integer;
begin
  updated := False;
  v := ear.GetVertex;
  te := ear.GetTetra(1);
  adj := ear.GetTetra(2);
  te3 := Find3rdTetra(te, adj);

  if (te3 <> nil) then
  begin
    te.SetIndex(adj);
    adj.SetIndex(te);
    removePt := nil;
    planePt := nil;
    // -- find the point that will be removed from H at the end
    for i := 1 to 3 do
    begin
      tempt := te.GetProt(i);
      if (tempt <> v) then
      begin
        if (te3.HasPoint(tempt) = True) then
          removePt := tempt
        else
          planePt := tempt;
      end;
    end;

    // -- the 2 points of the edge of degree 3 must be of each side of the
    // -- middle plane
    intersect2 := TGBGeomTools.IntersectionEdgeTriangle(v, removePt,
      te.GetProt(0), adj.GetProt(0), planePt);
    if (intersect2 <> -1) then
    begin
      // -- get pointers to 3 ears forming the 3ear & to 3 exterior ears
      j := 1;
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te3) = True) then
        begin
          ear3[j] := tempear;
          Inc(j);
        end
        else
        begin
          if (tempear.ContainTetra(te) = True) then
            ext[1] := tempear
          else
            ext[2] := tempear;
        end;
      end;
      for i := 1 to 4 do
      begin
        tempear := ear3[1].GetAdj(i);
        if ((tempear.ContainTetra(te3) = True) and
          (tempear.ContainTetra(te) = False) and
          (tempear.ContainTetra(adj) = False)) then
        begin
          ext[3] := tempear;
          break;
        end;
      end;
      Flip32(te, adj, te3);
      updated := True;
      // lstPts.Remove(removePt); //-- remove 'exterior' point from TList     { TODO : remove lstPts in DeletePower }
      if (te.HasPoint(v) = True) then
      begin
        stay1 := te;
      end
      else
      begin
        stay1 := adj;
      end;

      // -- update the 3 ears (the ext ears) adjacent to the 3 deleted ears
      for i := 1 to 3 do
      begin
        tempear := ext[i];
        if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj) or
          (tempear.GetTetra(1) = te3)) then
        begin
          tempear.SetTetra(1, stay1);
        end
        else
        begin
          tempear.SetTetra(2, stay1);
        end;
      end;

      // -- update of the adj fields of the 3 ext ears
      for i := 1 to 3 do
      begin
        tempear := ext[i];
        tempte := tempear.GetTetra(1);
        if (tempte = stay1) then
        begin
          n := 1;
          tempte.SetIndex(tempear.GetTetra(2));
        end
        else
        begin
          n := 3;
          tempte := tempear.GetTetra(2);
          tempte.SetIndex(tempear.GetTetra(1));
        end;
        if (i = 1) then
        begin
          tempear.SetAdj(n, ext[2]);
          tempear.SetAdj(n + 1, ext[3]);
        end
        else if (i = 2) then
        begin
          tempear.SetAdj(n, ext[1]);
          tempear.SetAdj(n + 1, ext[3]);
        end
        else // if (i = 3) then
        begin
          tempear.SetAdj(n, ext[1]);
          tempear.SetAdj(n + 1, ext[2]);
        end;
        // -- update power of 3 ears that stay
        TGBEarp(tempear).UpdatePower;
      end;
      // -- destroy 3 ears forming the 3ear
      lstEar.Remove(ear);
      lstEar.Remove(ear3[1]);
      lstEar.Remove(ear3[2]);
      ear.Free;
      ear3[1].Free;
      ear3[2].Free;
      te3.Free;
    end;
  end;
  if (updated = False) then
  begin
    TGBEarp(ear).SetPower(-9999);
  end;
  Result := updated;
end;

function TTetraMesh.NbDelaunayEars(lstEar: TListz; lstPts: TList): integer;
var
  i: integer;
  ear: TGBEar;
  nb: integer;
begin
  nb := 0;
  for i := 0 to (lstEar.Count - 1) do
  begin
    ear := lstEar[i];
    if (ear.IsDelaunay(lstPts) = True) then
      Inc(nb);
  end;
  Result := nb;
end;


// ******************************************************************************
// Description: Estimate the attribute value at a given location with linear
// interpolation. This is done with the 3d barycentric coordinates.

// Input:       x, y, z: location where we want to estimate value of attribute.

// Output:      estimation of the attribute
// ******************************************************************************
function TTetraMesh.Interpolate_linear(x, y, z: double): double;
const
  tolerance: double = 1E-12;
var
  splite: TGBTetrahedron;
  pt: TGBPoint3D;
  collision: boolean;
  estimate: double;
  i: integer;
  extrapol: boolean;
begin
  m_tempPt.SetCoord(x, y, z);

  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create
      ('Location outside the bounding box. Extrapolation impossible.');
  end;

  extrapol := False;
  estimate := 0;
  splite := Walk(m_tempPt);
  // -- check if 'collision' with a data point, if yes then return its value
  collision := False;
  for i := 1 to 4 do
  begin
    if (m_tempPt.Hit(splite.GetP(i), tolerance) = True) then
    begin
      estimate := splite.GetP(i).Data;
      collision := True;
      break;
    end;
  end;

  if (self.IsOutsideCH(splite) = True) then
  begin
    extrapol := True;
    estimate := -1;
  end;

  if ((collision = False) and (extrapol = False)) then
  begin
    for i := 1 to 4 do
    begin
      splite.SetIndex(i);
      estimate := estimate +
        (splite.GetP(i).Data * abs(TGBGeomTools.Det4x4t(m_tempPt,
        splite.GetProt(1), splite.GetProt(2), splite.GetProt(3))));
    end;
    estimate := estimate / (TGBGeomTools.Det4x4t(splite.GetP(1), splite.GetP(2),
      splite.GetP(3), splite.GetP(4)));
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  Result := estimate;
end;


// ******************************************************************************
// Description: Determine if one (or more) of the vertices of a tetra is a vertex
// of the big tetra.

// Input:       te: the tetra to test

// Output:      TRUE -> yes one of the vertices of te is part of big tetra
// NO   -> no te does not contain any points from big tetra
// ******************************************************************************
function TTetraMesh.IsOutsideCH(te: TGBTetrahedron): boolean;
begin
  if ((te.HasPoint(m_lstVertex[0]) = False) and
    (te.HasPoint(m_lstVertex[1]) = False) and
    (te.HasPoint(m_lstVertex[2]) = False) and
    (te.HasPoint(m_lstVertex[3]) = False)) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
  end;
end;


// ******************************************************************************
// Description: Insert a new point in the mesh. The order of the flips is
// determined by the power of the new point w/r to the circumsphere
// of each tetra in the mesh.

// Input:       x, y, z: coord of the new point to add

// Output:      -

// Note:        This method is NOT optimized & is rather slow. First, all the
// tetra in the mesh are scanned and those whose circumsphere contain
// the new point are put in a list. Then this list is ordered from
// the smallest to biggest power. This guarantees that the tetra will
// be flippable all the time -- only for case where points are in
// general position.
// ******************************************************************************
procedure TTetraMesh.InsertPoint_power(x, y, z, Data: double);
var
  splitTe: TGBTetrahedron;
  pt: TGBPoint3D;
  i, j: integer;
  lstPower: TList;
  te: TGBTetrahedron;
  tempte: TGBTetrahedron;
begin
  m_tempPt.SetCoord(x, y, z);
  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create('Point is outside the bounding box');
  end;

  splitTe := Walk(m_tempPt);

  // -- check collision with an existing data point
  if (CheckCollision(splitTe, m_tempPt) = True) then
  begin
    raise Exception.Create('Insertion impossible : this point already exists');
  end;

  pt := TGBPoint3D.Create(True, x, y, z, Data);
  m_lstVertex.Add(pt);

  lstPower := TList.Create;
  for i := 0 to (m_lstTetra.Count - 1) do
  begin
    te := m_lstTetra[i];
    if (TGBGeomTools.InSphere(te, pt) = True) then
    begin
      lstPower.Add(te);
    end;
  end;

  te := GetMinList(lstPower, pt);
  lstPower.Remove(te);
  Flip14(pt, te);

  while (lstPower.Count > 0) do
  begin
    te := GetMinList(lstPower, pt);
    lstPower.Remove(te);
    for j := 1 to 4 do
    begin
      tempte := te.GetT(j);
      if (tempte.HasPoint(pt) = True) then
      begin
        break;
      end;
    end;
    Assert(tempte.HasPoint(pt) = True);
    Flip_power(tempte, te);
  end;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;


// ******************************************************************************
// Description: Flip 2 (or 3) tetra that are not Delaunay after the insertion of a
// new point in the mesh. This is guaranteed that the flip can be
// performed as the power determines the order.

// Input:       te, adj: the 2 tetra to flip (there can be a 3rd one)

// Output:      - (the flip is performed)

// Note:        Only works for points in general position. The flip44 hasn't been
// implemented yet here.
// ******************************************************************************
procedure TTetraMesh.Flip_power(te, adj: TGBTetrahedron);
var
  p: TGBPoint3D;
  d: TGBPoint3D;
  intersect: integer;
  newTetra: TGBTetrahedron;
  te3: TGBTetrahedron;
  flat: TGBTetrahedron;
  done: boolean;
begin
  // -- does the edge pass through the triangle?
  p := te.SetIndex(adj).GetProt(0);
  d := adj.SetIndex(te).GetProt(0);

  // -- do the right flip according to the position of the tetrahedra
  intersect := TGBGeomTools.IntersectionEdgeTriangle(p, d, te.GetProt(1),
    te.GetProt(2), te.GetProt(3));
  if (intersect = 1) then // -- case #1 : the edge passes through the triangle
  begin
    newTetra := Flip23(te, adj);
  end
  else if (intersect = -1) then // -- case #2 : the edge doesn't pass through
  begin // -- the triangle
    te3 := Find3rdTetra(te, adj);
    Assert(te3 <> nil);
    Flip32(te, adj, te3);
  end;
end;

function TTetraMesh.GetMinList(lst: TList; pt: TGBPoint3D): TGBTetrahedron;
var
  i: integer;
  power, temp: double;
  te, r: TGBTetrahedron;
begin
  te := lst[0];
  power := TGBGeomTools.PowerTet(te.GetP(1), te.GetP(2), te.GetP(3),
    te.GetP(4), pt);
  r := te;
  for i := 1 to (lst.Count - 1) do
  begin
    te := lst[i];
    temp := TGBGeomTools.PowerTet(te.GetP(1), te.GetP(2), te.GetP(3),
      te.GetP(4), pt);
    if (temp < power) then
    begin
      power := temp;
      r := te;
    end;
  end;
  Result := r;
end;



// ******************************************************************************
// Description: Process an ear that is in a degenerate configuration, which
// means that 4 vertices of the ear are coplanar.

// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)

// Output:      0: cannot flip
// 1: flip was done (can mean a flip44)
// 2: config44 = 2 (did not flip)
// ******************************************************************************
function TTetraMesh.ProcessEar_degenerate_e(ear: TGBEar; lstEar, lstPts: TList;
  flag44: boolean): integer;
var
  updated: integer;
  continue: boolean;
  te, adj: TGBTetrahedron;
  v: TGBPoint3D;
  te1, adj1: TGBTetrahedron;
  config44: integer;
  te3: TGBTetrahedron;
  i: integer;
  tempear: TGBEar;
  ear2: TGBEar;
begin
  updated := 0;
  // -- if the ear is a 3-ear, then apply a flip32 to kill the flat tetra
  if (ProcessEar_flip32(ear, lstEar, lstPts) = True) then
  begin
    updated := 1;
  end
  else // --this is a 2-ear
  begin
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    v := ear.GetVertex;
    continue := True;
    config44 := AreTetraConfig44(te, adj, te1, adj1);

    if (config44 = 2) then
    begin
      updated := 2;
      if (flag44 = False) then
        continue := False;
    end;

    if ((config44 <> 0) and (continue = True)) then
    begin
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te1) = True) then
          break;
      end;
      for i := 1 to 4 do
      begin
        ear2 := tempear.GetAdj(i);
        if (ear2.ContainTetra(adj1) = True) then
          break;
      end;
      Assert(ear2.ContainTetra(te1));
      Assert(ear2.ContainTetra(adj1));

      if ((ear.IsDelaunay(lstPts) = True) and (ear2.IsDelaunay(lstPts) = True))
      then
      begin
        // -- check if the 2 other tetra are inside H
        Assert(te1.HasPoint(v) = True);
        Assert(adj1.HasPoint(v) = True);
        updated := 1;
        // -- flip44
        ProcessEar_flip23(ear);
        ProcessEar_flip32(ear2, lstEar, lstPts);
      end;
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: Process an ear that is in a degenerate configuration, which
// means that 4 vertices of the ear are coplanar.

// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)

// Output:      TRUE  -> the ear was flipped & the lists were updated
// FALSE -> the ear wasn't flipped -- no action was performed

// Note:        The algo, in brief:
// if (e is a 3-ear) then
// flip32 if possible
// elseif (config44) & (no flat tetra under)
// flip23 OR flip44 (flip23 + flip32)
// endif.
// ******************************************************************************
function TTetraMesh.ProcessEar_degenerate_p(ear: TGBEar; lstEar: TListz;
  lstPts: TList): boolean;
var
  te3: TGBTetrahedron;
  te, adj: TGBTetrahedron;
  te1, adj1: TGBTetrahedron;
  v: TGBPoint3D;
  r: boolean;
  updated, continue: boolean;
  config44: integer;
  i: integer;
  tempear, ear2: TGBEar;
begin
  // -- if 3rd tetra exists -- which means that it is flat -- then flip32 to
  // -- delete it
  updated := ProcessEar_flip32_p(ear, lstEar, lstPts);

  if (updated = False) then
  begin
    continue := True;
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    v := ear.GetVertex;
    config44 := AreTetraConfig44(te, adj, te1, adj1);
    if (config44 = 2) then
    begin
      // -- check if 3rd tetra exists 'under' one the 4 tetra
      te3 := Find3rdTetra(te, te1);
      if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
      then
        continue := False;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(adj, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(te1, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
    end;

    if ((config44 <> 0) and (continue = True)) then
    begin
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te1) = True) then
          break;
      end;
      for i := 1 to 4 do
      begin
        ear2 := tempear.GetAdj(i);
        if (ear2.ContainTetra(adj1) = True) then
          break;
      end;
      Assert(ear2.ContainTetra(te1));
      Assert(ear2.ContainTetra(adj1));
      // -- check if the 2 other tetra are inside H
      Assert(te1.HasPoint(v) = True);
      Assert(adj1.HasPoint(v) = True);

      updated := True;
      ProcessEar_flip23(ear);
      // ProcessEar_flip32_p(ear2, lstEar, lstPts);
    end;
  end;
  if (updated = False) then
  begin
    TGBEarp(ear).SetPower(-9999);
  end;
  Result := updated;
end;

// **********************
// -- TEMP FUNCTION
// **********************
function TTetraMesh.ProcessEar_degenerate_p__2(ear: TGBEar; lstEar: TListz;
  lstPts: TList; flag44: boolean): integer;
var
  te3: TGBTetrahedron;
  te, adj: TGBTetrahedron;
  te1, adj1: TGBTetrahedron;
  v: TGBPoint3D;
  r: boolean;
  updated: integer;
  config44: integer;
  i: integer;
  tempear, ear2: TGBEar;
  continue: boolean;
begin
  updated := 0;
  // -- if the ear is a 3-ear, then apply a flip32 to kill the flat tetra
  if (ProcessEar_flip32_p(ear, lstEar, lstPts) = True) then
  begin
    updated := 1;
  end
  else // --this is a 2-ear
  begin
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    v := ear.GetVertex;
    continue := True;
    config44 := AreTetraConfig44(te, adj, te1, adj1);

    if (config44 = 2) then
    begin
      updated := 2;
      if (flag44 = False) then
        continue := False;
    end;

    if ((config44 <> 0) and (continue = True)) then
    begin
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te1) = True) then
          break;
      end;
      for i := 1 to 4 do
      begin
        ear2 := tempear.GetAdj(i);
        if (ear2.ContainTetra(adj1) = True) then
          break;
      end;
      Assert(ear2.ContainTetra(te1));
      Assert(ear2.ContainTetra(adj1));
      // -- check if the 2 other tetra are inside H
      Assert(te1.HasPoint(v) = True);
      Assert(adj1.HasPoint(v) = True);
      Assert(ear.IsDelaunay(lstPts) = True);
      Assert(ear2.IsDelaunay(lstPts) = True);

      updated := 1;
      // -- flip44
      ProcessEar_flip23(ear);
      ProcessEar_flip32_p(ear2, lstEar, lstPts);
    end;
  end;
  if (updated <> 1) then
  begin
    TGBEarp(ear).SetPower(-9999);
  end;
  Result := updated;
end;


// -----------------------------------------------------------------------------
// Description: Compute the volume of a Voronoi cell
// Input:       gen: the generator of the voronoi cell
// Output:      volume of the cell (a double)
// Note:        Description of the algo:
// first all the edges incident to the generator are stored in a
// list. Then the voronoi face (a convex polygon) is computed around
// each edge. Then we triangulate the face (all triangle oriented
// in the same direction) and compute the volume by signed volume
// w/r to the origin (0, 0, 0). The generator could also be used
// more computations as with origin we have already a 3x3 matrix.
// [det(a, b, c, d) gives 6 times the volume of a tetra]
// -----------------------------------------------------------------------------
function TTetraMesh.GetVolumeVoronoiCell(gen: TGBPoint3D): double;
var
  lstEdges: TList;
  lstPts: TList;
  i, j: integer;
  vol: double;
  e: TGBEdge;
begin
  vol := 0;
  // -- build a list of every TEdge connected to the data point "pt"
  lstEdges := GetStar_edges(gen);
  lstPts := TList.Create;
  for i := 0 to (lstEdges.Count - 1) do
  begin
    // -- store in a list CCW from outside all the vertices of the convex face
    // -- around the edge
    e := lstEdges[i];
    ComputeVoronoiFaceAroundEdge(e, lstPts);
    // -- triangulate the face CCW and compute volume of each tetra with ORIGIN
    j := 2;
    while j <= (lstPts.Count - 1) do
    begin
      vol := vol + TGBGeomTools.Det3x3(TGBPoint3D(lstPts[0]),
        TGBPoint3D(lstPts[j - 1]), TGBPoint3D(lstPts[j]));
      Inc(j);
    end;
    // -- free the list of points
    for j := 0 to (lstPts.Count - 1) do
    begin
      TGBPoint3D(lstPts[j]).Free;
    end;
    lstPts.Clear;
  end;
  // -- free the list of edges
  for i := 0 to (lstEdges.Count - 1) do
  begin
    TGBEdge(lstEdges[i]).Free;
  end;
  lstEdges.Clear;
  Result := (vol / 6);
end;

// -----------------------------------------------------------------------------
// Description: Compute the volume of a Voronoi cell partly. It is used when the
// volume of a cell is needed before and after (like for natural
// neighbour interpolation) of a new point. Only the faces of the
// Voronoi cell that are modified by the insertion are computed --
// because anyway we are interested only in the difference it is
// stupid to compute twice the same stuff...
// Input:       gen:  the generator of the voronoi cell
// v:    the dummy point
// step: 1 -> v is in the mesh; 2 -> v is removed from the mesh
// lstNN: list of nn of the dummy point
// Output:      part of the volume of the cell (a double)
// Note:        Description of the algo:
// first all the edges incident to the generator are stored in a
// list. Then the voronoi face (a convex polygon) is computed around
// each edge. Then we triangulate the face (all triangle oriented
// in the same direction) and compute the volume by signed volume
// w/r to the origin (0, 0, 0). The generator could also be used but
// more computations as with origin we have already a 3x3 matrix.
// [det(a, b, c, d) gives 6 times the volume of a tetra]
// ----------------------------------------------------------------------------
function TTetraMesh.VolumeVoronoiCell_modifiedFacesOnly(gen, v: TGBPoint3D;
  step: integer; lstNN: TList): double;
var
  lstEdges: TList;
  lstPts: TList;
  i, j: integer;
  vol: double;
  e: TGBEdge;
  tempt: TGBPoint3D;
begin
  vol := 0;
  // -- build a list of every TEdge connected to the generator
  lstEdges := GetStar_edges(gen);
  lstPts := TList.Create;
  for i := 0 to (lstEdges.Count - 1) do
  begin
    e := lstEdges[i];
    tempt := e.GetDest;
    if ((lstNN.IndexOf(tempt) <> -1) or (tempt = v)) then
    begin
      ComputeVoronoiFaceAroundEdge(e, lstPts);
      { if (step = 1) then
          ComputeVoronoiFaceAroundEdge2(e, nil, lstPts)
        else  // (step = 2)
          ComputeVoronoiFaceAroundEdge2(e, v, lstPts);
      }
      // -- triangulate the face CCW and compute volume of each tetra with origin
      j := 2;
      while j <= (lstPts.Count - 1) do
      begin
        vol := vol + TGBGeomTools.Det3x3(TGBPoint3D(lstPts[0]),
          TGBPoint3D(lstPts[j - 1]), TGBPoint3D(lstPts[j]));
        Inc(j);
      end;
      // -- free the list of voronoi points
      for j := 0 to (lstPts.Count - 1) do
      begin
        TGBPoint3D(lstPts[j]).Free;
      end;
      lstPts.Clear;
    end;
  end;
  // -- free the list of points
  for i := 0 to (lstEdges.Count - 1) do
  begin
    TGBEdge(lstEdges[i]).Free;
  end;
  lstEdges.Clear;
  Result := vol;
end;

// ******************************************************************************
// Description: Estimate the attribute value at a given location with the
// natural neighbour interpolation method
// Input:       x, y, z: location where we want to estimate value of attribute.
// Output:      estimation of the attribute (a double)
// ******************************************************************************
function TTetraMesh.Interpolate_nn(x, y, z: double): double;
const
  tolerance: double = 1E-12;
var
  splite: TGBTetrahedron;
  v: TGBPoint3D;
  tempt: TGBPoint3D;
  collision: boolean;
  estimate: double;
  volDummy: double;
  i, j: integer;
  lstFlips: TList; // -- contains one edge identifying uniquely each flip
  lstNN: TList;
  lstVol: array of double;
  e: TGBEdge;
  te0, te1, te2: TGBTetrahedron;
  extrapol: boolean;
  tempe: TGBEdge;
  incte: TGBTetrahedron; // -- the tetra always incident to the dummy vertex
  tempte: TGBTetrahedron;
  lstUmbrellas: TList;
  lst: TList;
  p, q: TGBPoint3D;
begin
  m_tempPt.SetCoord(x, y, z);
  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create
      ('Location outside the bounding box. Extrapolation impossible.');
  end;

  estimate := 0;
  // -- check if 'collision' with a data point, if yes then return its value
  splite := Walk(m_tempPt);
  collision := False;
  for i := 1 to 4 do
  begin
    if (m_tempPt.Hit(splite.GetP(i), tolerance) = True) then
    begin
      estimate := splite.GetP(i).Data;
      collision := True;
      break;
    end;
  end;

  if (collision = False) then
  begin
    // -- insert point in the mesh while storing order of the flips performed
    extrapol := False;
    v := TGBPoint3D.Create(True, x, y, z);
    m_lstVertex.Add(v);
    lstNN := TList.Create; // -- store first four nn in lstNN, before flip14
    lstNN.Add(splite.GetP(1));
    lstNN.Add(splite.GetP(2));
    lstNN.Add(splite.GetP(3));
    lstNN.Add(splite.GetP(4));
    Flip14(v, splite);
    lstFlips := TList.Create;
    Optim(splite, v, lstFlips, lstNN);
{$IFDEF DEBUG}
    TestInvariants;
{$ENDIF}
    // -- build the lists of the 'umbrella' for each nn
    lstUmbrellas := TList.Create;
    lstUmbrellas.Capacity := lstNN.Count;
    for i := 0 to (lstNN.Count - 1) do
    begin
      lstUmbrellas.Add(GetNN_edges_linkofV(TGBPoint3D(lstNN[i]), v));
    end;

    // -- compute volume of every nn
    SetLength(lstVol, (lstNN.Count));
    for i := 0 to (lstNN.Count - 1) do
    begin
      tempt := lstNN[i];
      if (m_lstVertex.IndexOf(tempt) <= 3) then
      begin
        extrapol := True;
        estimate := -1; // -- value is -1 when extrapolation...
        break;
      end
      else
        lstVol[i] := VolumeVoronoiCell_partial(lstUmbrellas[i]);
    end;

    // -- compute cell volume of interpolation point
    if (extrapol = False) then
      volDummy := (GetVolumeVoronoiCell(v) * 6)
    else
      volDummy := 1;

    // -- delete edge v-nn of every umbrella
    for i := 0 to (lstNN.Count - 1) do
    begin
      lst := lstUmbrellas[i];
      lst.Delete(lst.Count - 1);
    end;

    // -- delete the vertex by applying inverse flips in reverse order
    incte := Walk(v);
    Assert(incte.HasPoint(v) = True);
    for i := (lstFlips.Count - 1) downto 0 do
    begin
      e := lstFlips[i];
      if (e.HasPoint(v) = True) then
      begin
        // -- apply a flip32
        te0 := WalkAroundVertex(v, e.GetDest, incte);
        te0.SetIndex(v);
        te1 := nil;
        te2 := nil;
        for j := 1 to 3 do
        begin
          if (te0.GetProt(j) <> e.GetDest) then
          begin
            if (te1 = nil) then
              te1 := te0.GetTrot(j)
            else
              te2 := te0.GetTrot(j);
          end;
        end;
        Assert(te1 <> nil);
        Assert(te2 <> nil);
        Assert(Find3rdTetra(te0, te1) <> nil);
        Flip32(te0, te1, te2);
        if (te0.HasPoint(v) = True) then
          incte := te0
        else
          incte := te1;
      end
      else // -- then apply a flip23
      begin
        e.SetTetra(WalkAroundVertex(v, e.GetOrg, incte));
        tempe := TGBEdge.Create(v, e.GetOrg, e.GetAdjTetra);
        te0 := WalkAroundEdge(tempe, e.GetDest);
        tempe.Free;
        Assert(te0.HasPoint(e.GetOrg) = True);
        Assert(te0.HasPoint(e.GetDest) = True);
        te0.SetIndex(v);
        te1 := nil;

        for j := 1 to 3 do
        begin
          if ((te0.GetProt(j) <> e.GetOrg) and (te0.GetProt(j) <> e.GetDest))
          then
          begin
            te1 := te0.GetTrot(j);
            break;
          end;
        end;
        Assert(te0.HasNeighbour(te1) = True);
        Assert(te0.HasPoint(v) = True);
        Assert(te1.HasNeighbour(te0) = True);
        Assert(te1.HasPoint(v) = True);
        p := te0.SetIndex(te1).GetProt(0);
        q := te1.SetIndex(te0).GetProt(0);
        te2 := Flip23(te0, te1);
        if (te0.HasPoint(v) = True) then
          incte := te0
        else if (te1.HasPoint(v) = True) then
          incte := te1
        else
          incte := te2;
        // --
        if ((te0.HasPoint(e.GetOrg)) and (te0.HasPoint(e.GetDest))) then
          tempte := te0
        else if ((te1.HasPoint(e.GetOrg)) and (te1.HasPoint(e.GetDest))) then
          tempte := te1
        else
          tempte := te2;

        lst := lstUmbrellas[lstNN.IndexOf(p)];
        lst.Add(TGBEdge.Create(p, q, tempte));
        lst := lstUmbrellas[lstNN.IndexOf(q)];
        lst.Add(TGBEdge.Create(q, p, tempte));
      end;
    end;
    Flip41(v, incte);

    // -- compute (again) cell volume of every nn (w/o the dummy coord in the mesh)
    // -- and compute volume difference to get nn-coord of each nn.
    if (extrapol = False) then
    begin
      for i := 0 to (lstNN.Count - 1) do
      begin
        tempt := lstNN[i];
        estimate := estimate +
          ((-lstVol[i] + VolumeVoronoiCell_partial(lstUmbrellas[i])) *
          tempt.Data);
      end;
      estimate := estimate / volDummy;
    end;

    // -- free the edges in lstFlips
    for i := 0 to (lstFlips.Count - 1) do
    begin
      e := lstFlips[i];
      e.Free;
    end;
    lstFlips.Free;
    lstNN.Free;

    // -- free lists of umbrellas
    for i := 0 to (lstUmbrellas.Count - 1) do
    begin
      lst := lstUmbrellas[i];
      for j := 0 to (lst.Count - 1) do
      begin
        e := lst[j];
        e.Free;
      end;
      lst.Free;
    end;
    lstUmbrellas.Free;

  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  Result := estimate;
end;

// ******************************************************************************
// Description: Estimate the attribute value at a given location with inverse
// distance to a power of 2 (IDW) method.

// Input:       x, y, z: location where we want to estimate value of attribute.
// radius:  radius of the counting sphere

// Output:      estimation of the attribute (a double)
// ******************************************************************************
function TTetraMesh.Interpolate_idw(x, y, z, radius: double): double;
const
  tolerance: double = 1E-12;
var
  lstNeigh: TList;
  tempDist: double;
  splite: TGBTetrahedron;
  tempt: TGBPoint3D;
  collision: boolean;
  estimate: double;
  totalweight: double;
  i: integer;
begin
  m_tempPt.SetCoord(x, y, z);
  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create
      ('Location outside the bounding box. Extrapolation impossible.');
  end;
  splite := Walk(m_tempPt);
  // -- check if 'collision' with a data point, if yes then return its value
  estimate := 0;
  collision := False;
  for i := 1 to 4 do
  begin
    if (m_tempPt.Hit(splite.GetP(i), tolerance) = True) then
    begin
      estimate := splite.GetP(i).Data;
      collision := True;
      break;
    end;
  end;
  if (collision = False) then
  begin
    estimate := 0;
    totalweight := 0;
    for i := 0 to (m_lstVertex.Count - 1) do
    begin
      tempt := m_lstVertex[i];
      tempDist := TGBGeomTools.Distance3d(tempt, m_tempPt);
      if (tempDist <= radius) then
      begin
        estimate := estimate + (power(1 / tempDist, 2) * tempt.Data);
        totalweight := totalweight + power(1 / tempDist, 2);
      end;
    end;
    // -- if no points were inside the sphere
    if (totalweight = 0) then
      estimate := -1
    else
      estimate := estimate / totalweight;
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  Result := estimate;
end;

// ******************************************************************************
// Description: Cross-validation of an interpolation method. Each point is
// removed one at a time, and the value of the attribute is
// estimated at this location -- this permits to verify if one
// method yields good results for a given dataset.

// Input:       the interpolation method:
// 1- linear interpolation inside a Delaunay tetra
// 2- natural neighbours
// 3- IDW
// Output:      a TList containing values : real-estimated-real-estimated etc...
// ******************************************************************************
procedure TTetraMesh.CrossValidation(method: integer;
  out real, estim: array of double);
var
  i: integer;
  tempt: TGBPoint3D;
  x, y, z, Data: double;
  estimate: double;
begin
  for i := 4 to (m_lstVertex.Count - 1) do
  begin
    tempt := m_lstVertex[4];
    x := tempt.x;
    y := tempt.y;
    z := tempt.z;
    Data := tempt.Data;
    self.DeleteVertex_insphere(tempt);
    real[i - 4] := Data;
    if (method = 1) then
      estimate := self.Interpolate_linear(x, y, z)
    else if (method = 2) then
      estimate := self.Interpolate_nn(x, y, z)
    else
      estimate := self.Interpolate_idw(x, y, z, 1);

    estim[i - 4] := estimate;
    self.InsertPoint(x, y, z, Data);
    // ShowMessage(inttostr(i));
    // Form1.ProgressBar1.StepIt;
  end;
end;

function TTetraMesh.Delete_unflip(lstEar: TList; lstPts: TList;
  lastProcessed: TGBEar): TGBEar;
var
  ear: TGBEar;
  i, j, k, n: integer;
  te, adj, te1, adj1: TGBTetrahedron;
  v: TGBPoint3D;
  lstAdjEar: TList;
  newTetra, flatTe: TGBTetrahedron;
  stay1, stay2: TGBTetrahedron;
  middle: TGBTetrahedron;
  tempte, tempte2: TGBTetrahedron;
  tempear, tempear2: TGBEar;
  tempt: TGBPoint3D;
  a, b, t1, t2, t3: TGBPoint3D;
  found: boolean;
  start: integer;
  nb: integer;
  earProcessed: TGBEar;
begin
  ear := nil;
  te := nil;
  adj := nil;
  te1 := nil;
  adj1 := nil;
  v := nil;

  // -- get one flat ear
  found := False;
  nb := lstEar.Count;
  for i := 0 to (nb - 1) do
  // for i := (nb - 1) downto 0 do
  begin
    ear := lstEar[i];
    if ((ear.IsConvex = 0) and (ear <> lastProcessed)) then
    begin
      v := ear.GetVertex;
      te := ear.GetTetra(1);
      te.SetIndex(v);
      te1 := te.GetTrot(0);
      adj := ear.GetTetra(2);
      adj.SetIndex(v);
      adj1 := adj.GetTrot(0);
      // self.SetEnvelope(v, ear, lstEar, lstPts);
      // Form1.Refresh;
      { if ((self.IsOutsideCH(te) = false) and (self.IsOutsideCH(adj) = false) and
        (te1.HasNeighbour(adj1)) and (adj1.HasNeighbour(te1)) and
        (te.IsFlat = false) and (adj.IsFlat = false) ) then
      } if ((te1.HasNeighbour(adj1)) and (adj1.HasNeighbour(te1)) and
        (te.IsFlat = False) and (adj.IsFlat = False)) then
      begin
        // -- check if the new ones are Delaunay w/r to lstPts
        te1.SetIndex(adj1);
        a := te1.GetProt(0);
        adj1.SetIndex(te1);
        b := adj1.GetProt(0);
        t1 := te1.GetProt(1);
        t2 := te1.GetProt(2);
        t3 := te1.GetProt(3);
        if (TGBGeomTools.Orient3D(a, b, t1, t2) = 0) then
        begin
          tempt := t2;
          t2 := t3;
          t3 := tempt;
        end;
        if (TGBGeomTools.Orient3D(a, t1, t2, b) = 1) then
        begin
          if (TGBGeomTools.InSphere(a, t1, t2, b, t3) = False) then
          begin
            found := True;
            earProcessed := ear;
            break;
          end;
        end
        else
        begin
          if (TGBGeomTools.InSphere(a, t2, t1, b, t3) = False) then
          begin
            found := True;
            earProcessed := ear;
            break;
          end;
        end;
      end;
    end;
  end;

  if (found = False) then
  begin
    middle := nil;
    for i := 0 to (lstEar.Count - 1) do
    begin
      ear := lstEar[i];
      if (ear.IsConvex = 0) then
      begin
        v := ear.GetVertex;
        te := ear.GetTetra(1);
        te.SetIndex(v);
        te1 := te.GetTrot(0);
        adj := ear.GetTetra(2);
        adj.SetIndex(v);
        adj1 := adj.GetTrot(0);

        // -- check what tetra has te1 and adj1 as a neighbour
        te1.SetIndex(te);
        for j := 1 to 3 do
        begin
          tempte := te1.GetTrot(j);
          if (adj1.HasNeighbour(tempte) = True) then
          begin
            te1.SetIndex(tempte);
            tempte.SetIndex(te1);
            if (TGBGeomTools.IntersectionEdgeTriangle(te1.GetProt(0),
              tempte.GetProt(0), te1.GetProt(1), te1.GetProt(2), te1.GetProt(3)
              ) = 1) then
            begin
              middle := tempte;
              found := True;
              break;
            end;
          end;
        end;
        if (found = True) then
        begin
          newTetra := self.Flip23(te1, middle);
          Assert(te1.IsFlat = False);
          Assert(middle.IsFlat = False);
          Assert(newTetra.IsFlat = False);
          te1 := te.SetIndex(v).GetTrot(0);
          earProcessed := ear;
          break;
        end;
      end;
    end;
  end;

  // -- be sure we've got a config44 here
  Assert(te1.HasNeighbour(adj1));
  Assert(adj1.HasNeighbour(te1));
  // -- store the 4 neighbouring ears & the modified ear
  // -- construct a TList of ear + the 4 adj ears
  lstAdjEar := TList.Create;
  lstAdjEar.Add(ear);
  for i := 1 to 4 do
  begin
    lstAdjEar.Add(ear.GetAdj(i));
  end;

  // --  --> flip44 <--
  newTetra := self.Flip44(te, adj, te1, adj1, flatTe);

  // -- make sure the new tetra are Delaunay  { TODO : remove this Delaunay test }
  for i := 0 to (lstPts.Count - 1) do
  begin
    tempt := lstPts[i];
    if ((te1.HasPoint(tempt) = False) and (TGBGeomTools.InSphere(te1, tempt)
      = True)) then
      raise Exception.Create('Houston, we have a problem...');
    if ((adj1.HasPoint(tempt) = False) and (TGBGeomTools.InSphere(adj1, tempt)
      = True)) then
      raise Exception.Create('Houston, we have a problem...');
  end;

  // -- update the remaining ears
  // -- identify the 2 tetra that stay inside H after the flip
  if (te.HasPoint(v) = False) then
  begin
    stay1 := newTetra;
    stay2 := adj;
  end
  else if (adj.HasPoint(v) = False) then
  begin
    stay1 := te;
    stay2 := newTetra;
  end
  else // -- if (newTetra.HasPoint(v) = false)
  begin
    stay1 := te;
    stay2 := adj;
  end;

  // -- update all the fields of 'ear'
  ear.SetTetra(1, stay1);
  ear.SetTetra(2, stay2);
  stay1.SetIndex(stay2);
  n := 1;
  for i := 1 to 3 do
  begin
    if (stay1.GetProt(i) <> v) then
    begin
      tempte := stay1.GetTrot(i);
      tempear := nil;
      // -- get ear in lstAdjEar that contains tempte
      for j := 1 to (lstAdjEar.Count - 1) do
      begin
        tempear := lstAdjEar[j];
        if (tempear.ContainTetra(tempte) = True) then
        begin
          break;
        end;
      end;
      ear.SetAdj(n, tempear);
      Inc(n);
      // -- update tetra field of tempear
      if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj)) then
        tempear.SetTetra(1, stay1)
      else
        tempear.SetTetra(2, stay1);
    end;
  end;
  stay2.SetIndex(stay1);
  n := 3;
  for i := 1 to 3 do
  begin
    if (stay2.GetProt(i) <> v) then
    begin
      tempte := stay2.GetTrot(i);
      tempear := nil;
      // -- get ear in lstAdjEar that contains tempte
      for j := 1 to (lstAdjEar.Count - 1) do
      begin
        tempear := lstAdjEar[j];
        if (tempear.ContainTetra(tempte) = True) then
        begin
          break;
        end;
      end;
      ear.SetAdj(n, tempear);
      Inc(n);
      // -- update tetra field of tempear
      if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj)) then
        tempear.SetTetra(1, stay2)
      else
        tempear.SetTetra(2, stay2);
    end;
  end;
  // -- update of the adj fields of the 4 adjacent ears
  for i := 1 to 4 do
  begin
    tempear := ear.GetAdj(i);
    // -- only 2 fields (linked to the same tetra) need to be changed.
    // -- the ones where either stay1 or stay2 are.
    tempte := tempear.GetTetra(1);
    if ((tempte = stay1) or (tempte = stay2)) then
    begin
      n := 1;
      tempte.SetIndex(tempear.GetTetra(2));
    end
    else
    begin
      n := 3;
      tempte := tempear.GetTetra(2);
      tempte.SetIndex(tempear.GetTetra(1));
    end;
    for j := 1 to 3 do
    begin
      if (tempte.GetProt(j) <> v) then
      begin
        tempte2 := tempte.GetTrot(j);
        // -- get ear in lstAdjEar that contains tempte
        tempear2 := nil;
        for k := 0 to (lstAdjEar.Count - 1) do
        begin
          tempear2 := lstAdjEar[k];
          if ((tempear2.ContainTetra(tempte2) = True) and (tempear2 <> tempear))
          then
          begin
            break;
          end;
        end;
        Assert(tempear2 <> nil);
        tempear.SetAdj(n, tempear2);
        Inc(n);
      end;
    end;
  end;
  // -- update the power of the 5 modified ears
  if (ear is TGBEarp) then
  begin
    TGBEarp(ear).UpdatePower;
    for i := 1 to 4 do
    begin
      TGBEarp(ear.GetAdj(i)).UpdatePower;
    end;
  end;
  lstAdjEar.Free;
  Result := earProcessed;
end;

// ******************************************************************************
// Description: Delete a vertex from the mesh -- POWER method.
// Input:       pt: the vertex to delete
// Output:      # of flips needed to delete the vertex
// Remarks:     using the method where a list of TEar is kept up-to-date
// and a priority queue
// ******************************************************************************
function TTetraMesh.DeleteVertex_power__2(pt: TGBPoint3D;
  draw: boolean): integer;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  noFlips: integer;
  noStucks: integer;
  r: boolean;
  te, adj, te3: TGBTetrahedron;
  power: double;
begin
  // -- drawing or not the envelope
  // if (self.bDrawDeletion = true) then
  // draw := true;
  // if (draw = true) then
  // bDrawDeletion := true;
  lstEar := TListz.Create;
  lstPts := TList.Create;
  self.GetStar_v_ear(pt, lstEar, lstPts, True);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  noFlips := 0; // -- # of flip performed to delete the point
  noStucks := 0; // -- # of ears that cannot be flipped

  ear := GetMaxList(lstEar);
  while (lstEar.Count > 6) do
  begin
    power := TGBEarp(ear).GetPower;
    if (power = -9999) then
    begin
      Delete_unflip(lstEar, lstPts, nil);
      ear := GetMaxList(lstEar);
    end;
    {
      if (draw = true) then
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //     Form1.Refresh;
      end;
    }
    r := ProcessEar_p__2(ear, lstEar, lstPts);
    if (r = True) then
    begin
      {
        if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //        Form1.Refresh;
        end;
      }
      Inc(noFlips);
    end
    else
      Inc(noStucks);
    ear := GetMaxList(lstEar);
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  m_curTe := ear.GetTetra(1);
  te := m_curTe;
  if (te.IsFlat = True) then
  begin
    adj := te.GetT(1);
    te3 := Find3rdTetra(te, adj);
    self.Flip32(te, adj, te3);
  end;

  // ShowMessage('# of flips: ' + IntToStr(noFlips));
  // if (noStucks > 0) then
  // ShowMessage('# of stuck ears: ' + IntToStr(noStucks));
  lstEar.Zap;
  lstPts.Clear;
  lstPts.Free;
  // Form1.Refresh;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  // bDrawDeletion := false;
  Result := noFlips;
end;


// ******************************************************************************
// Description: Delete a vertex from the mesh -- POWER method.
// Input:       pt: the vertex to delete
// Output:      # of flips needed to delete the vertex
// Remarks:     using the method where a list of TEar is kept up-to-date
// and a priority queue
// *** ONLY FOR TESTING. NO DEGENERATE CASES HANDLED. ***
// ******************************************************************************
function TTetraMesh.DeleteVertex_power__clean(pt: TGBPoint3D;
  draw: boolean): double;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  noFlips: integer;
  noStucks: integer;
  r: boolean;
  te, adj, te3: TGBTetrahedron;
  power: double;
  startTime, endTime, totalTime: double;
begin
  // -- drawing or not the envelope
  { if (self.bDrawDeletion = true) then
    draw := true;
    if (draw = true) then
    bDrawDeletion := true;
  }
  lstEar := TListz.Create;
  lstPts := TList.Create;
  self.GetStar_v_ear(pt, lstEar, lstPts, True);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  noFlips := 0; // -- # of flip performed to delete the point
  noStucks := 0; // -- # of ears that cannot be flipped

  // startTime := Form1.StampTime;
  ear := GetMaxList(lstEar);
  // endTime := Form1.StampTime;
  // totalTime := (endTime - startTime);

  while (lstEar.Count > 6) do
  begin
    power := TGBEarp(ear).GetPower;
    { if (draw = true) then
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //      Form1.Refresh;
      end;
    }
    r := ProcessEar_p__clean(ear, lstEar, lstPts);
    if (r = True) then
    begin
      { if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //       Form1.Refresh;
        end;
      }
      Inc(noFlips);
    end
    else
      Inc(noStucks);

    // startTime := Form1.StampTime;
    ear := GetMaxList(lstEar);
    // endTime := Form1.StampTime;
    totalTime := totalTime + (endTime - startTime);
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  m_curTe := ear.GetTetra(1);
  te := m_curTe;

  // ShowMessage('# of flips: ' + IntToStr(noFlips));
  // if (noStucks > 0) then
  // ShowMessage('# of stuck ears: ' + IntToStr(noStucks));
  lstEar.Zap;
  lstPts.Clear;
  lstPts.Free;
  // Form1.Refresh;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  // bDrawDeletion := false;
  Result := totalTime;
end;


// ******************************************************************************
// Description: Process a single ear when deleting a vertex. The ear processed
// is the ear with the biggest power.
// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)
// Output:      TRUE  -> the ear was flipped (should be the case when points are
// in general position.
// FALSE -> the ear cannot be flipped
// ******************************************************************************
function TTetraMesh.ProcessEar_p__2(ear: TGBEar; lstEar: TListz;
  lstPts: TList): boolean;
var
  intersect: integer;
  r: boolean;
  bDelaunay: boolean;
begin
  r := True;
  Assert(ear.IsConvex = 1);
  // Assert(ear.IsDelaunay(lstPts) = true);
  if (ear.IsDelaunay(lstPts) = False) then
  begin
    ShowMessage('non-delaunay ear.');
  end;
  bDelaunay := ear.IsDelaunay(lstPts);
  intersect := ear.IsFlippable;
  Assert(intersect <> 2);
  Assert(intersect <> 3);

  // -------------- Flip23 -------------------------------
  if (intersect = 1) then
  begin
    ProcessEar_flip23(ear);
  end
  // -------------- Flip32 -------------------------------
  else if (intersect = -1) then
  begin
    r := ProcessEar_flip32_p(ear, lstEar, lstPts);
    // if (r = false) then
    // ShowMessage('flip impossible.');
  end
  // -------------- Degenerate cases -------------------------------
  else if (intersect = 0) then
  begin
    r := ProcessEar_degenerate_p__3(ear, lstEar, lstPts);
  end;
  if ((bDelaunay = False) and (r = True)) then
    ShowMessage('Flip with non-Delaunay ear.');
  Result := r;
end;

// ******************************************************************************
// Description: Process a single ear when deleting a vertex. The ear processed
// is the ear with the biggest power.
// Input:       ear    : the TEar to flip
// lstEar : the list of all the TEar forming H
// lstPts : all the vertices of H (testing if ear is Delaunay)
// Output:      TRUE  -> the ear was flipped (should be the case when points are
// in general position.
// FALSE -> the ear cannot be flipped
// ******************************************************************************
function TTetraMesh.ProcessEar_p__clean(ear: TGBEar; lstEar: TListz;
  lstPts: TList): boolean;
var
  intersect: integer;
  r: boolean;
  bDelaunay: boolean;
begin
  r := True;
  Assert(ear.IsConvex = 1);

  intersect := ear.IsFlippable;
  Assert(intersect <> 2);
  Assert(intersect <> 3);

  // -------------- Flip23 -------------------------------
  if (intersect = 1) then
  begin
    ProcessEar_flip23(ear);
  end
  // -------------- Flip32 -------------------------------
  else if (intersect = -1) then
  begin
    r := ProcessEar_flip32_p(ear, lstEar, lstPts);
    // if (r = false) then
    // ShowMessage('flip impossible.');
  end
  // -------------- Degenerate cases -------------------------------
  else if (intersect = 0) then
  begin
    r := ProcessEar_degenerate_p__3(ear, lstEar, lstPts);
  end;
  Result := r;
end;

// **********************
// -- TEMP FUNCTION
// **********************
function TTetraMesh.ProcessEar_degenerate_p__3(ear: TGBEar; lstEar: TListz;
  lstPts: TList): boolean;
var
  te3: TGBTetrahedron;
  te, adj: TGBTetrahedron;
  te1, adj1: TGBTetrahedron;
  v: TGBPoint3D;
  r: boolean;
  updated, continue: boolean;
  config44: integer;
  i: integer;
  tempear, ear2: TGBEar;
begin
  // -- if 3rd tetra exists -- which means that it is flat -- then flip32 to
  // -- delete it
  updated := ProcessEar_flip32_p(ear, lstEar, lstPts);

  if (updated = False) then
  begin
    continue := True;
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    v := ear.GetVertex;
    config44 := AreTetraConfig44(te, adj, te1, adj1);
    if (config44 = 2) then
    begin
      // -- check if 3rd tetra exists 'under' one the 4 tetra
      te3 := Find3rdTetra(te, te1);
      if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
      then
        continue := False;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(adj, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(te1, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
    end;

    if ((config44 <> 0) and (continue = True)) then
    begin
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te1) = True) then
          break;
      end;
      for i := 1 to 4 do
      begin
        ear2 := tempear.GetAdj(i);
        if (ear2.ContainTetra(adj1) = True) then
          break;
      end;
      Assert(ear2.ContainTetra(te1));
      Assert(ear2.ContainTetra(adj1));
      // -- check if the 2 other tetra are inside H
      Assert(te1.HasPoint(v) = True);
      Assert(adj1.HasPoint(v) = True);

      updated := True;
      ProcessEar_flip23(ear);
      ProcessEar_flip32_p(ear2, lstEar, lstPts);
    end;
  end;
  if (updated = False) then
  begin
    TGBEarp(ear).SetPower(-9999);
  end;
  Result := updated;
end;

function TTetraMesh.ProcessEar_degenerate_e__2(ear: TGBEar;
  lstEar, lstPts: TList): integer;
var
  updated: integer;
  continue: boolean;
  te, adj: TGBTetrahedron;
  v: TGBPoint3D;
  te1, adj1: TGBTetrahedron;
  config44: integer;
  te3: TGBTetrahedron;
  i: integer;
  tempear: TGBEar;
  ear2: TGBEar;
begin
  updated := 0;
  // -- if the ear is a 3-ear, then apply a flip32 to kill the flat tetra
  if (ProcessEar_flip32(ear, lstEar, lstPts) = True) then
  begin
    updated := 1;
  end
  else // --this is a 2-ear
  begin
    te := ear.GetTetra(1);
    adj := ear.GetTetra(2);
    v := ear.GetVertex;
    continue := True;
    config44 := AreTetraConfig44(te, adj, te1, adj1);
    if (config44 = 2) then // -- means that config44 w/r to 2 planes
    begin
      // -- check if flat exists 'under' one the 4 tetra
      te3 := Find3rdTetra(te, te1);
      if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
      then
        continue := False;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(adj, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
      if (continue = True) then
      begin
        te3 := Find3rdTetra(te1, adj1);
        if ((te3 <> nil) and (te3.IsFlat = True) and (te3.HasPoint(v) = True))
        then
          continue := False;
      end;
    end;

    if ((config44 <> 0) and (continue = True)) then
    begin
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te1) = True) then
          break;
      end;
      for i := 1 to 4 do
      begin
        ear2 := tempear.GetAdj(i);
        if (ear2.ContainTetra(adj1) = True) then
          break;
      end;
      Assert(ear2.ContainTetra(te1));
      Assert(ear2.ContainTetra(adj1));

      if ((ear.IsDelaunay(lstPts) = True) and (ear2.IsDelaunay(lstPts) = True))
      then
      begin
        // -- check if the 2 other tetra are inside H
        Assert(te1.HasPoint(v) = True);
        Assert(adj1.HasPoint(v) = True);
        updated := 1;
        // -- flip44
        ProcessEar_flip23(ear);
        ProcessEar_flip32(ear2, lstEar, lstPts);
      end;
    end;
  end;
  Result := updated;
end;

function TTetraMesh.DeleteVertex_ear__2(pt: TGBPoint3D; draw: boolean)
  : integer;
var
  lstEar: TListz;
  lstPts: TList;
  ear: TGBEar;
  i: integer;
  k: integer;
  l: integer;
  nbD: integer;
  updated: integer;
  endless: integer;
  te, adj, te3: TGBTetrahedron;
begin
  { if (self.bDrawDeletion = true) then
    draw := true;
    if (draw = true) then
    bDrawDeletion := true;
  }
  lstEar := TListz.Create;
  lstPts := TList.Create;
  // -- creation of those 2 lists
  self.GetStar_v_ear(pt, lstEar, lstPts, False);
{$IFDEF DEBUG}
  self.TestEars(lstEar);
{$ENDIF}
  k := 0; // -- # of ears processed, successfully or not.
  l := 0; // -- # of flip performed to delete the point
  i := 0;

  ear := lstEar[i];
  endless := 0;
  while (lstEar.Count > 6) do
  begin
    if (endless > lstEar.Count) then
    begin
      Delete_unflip(lstEar, lstPts, nil);
      endless := 0;
    end;
    { if (draw = true) then
      begin
      self.SetEnvelope(ear.GetVertex, ear, lstEar, lstPts);
      //     Form1.Refresh;
      end;
    }
    updated := self.ProcessEar_e__2(ear, lstPts, lstEar);

    if (updated <> 0) then
    begin
      endless := 0;
      { if (draw = true) then
        begin
        self.SetEnvelope(ear.GetVertex, nil, lstEar, lstPts);
        //      Form1.Refresh;

        end;

      }
      Inc(l);
    end
    else
    begin
      Inc(endless);
    end;
    i := self.NextInList(i, lstEar.Count);
    ear := lstEar[i];
{$IFDEF DEBUG}
    self.Traverse;
    self.TestAllTetraOrient3D;
    self.TestTopology;
    self.TestEars(lstEar);
{$ENDIF}
  end;

  // -- remove the last vertex and update the adjacent tetra
  self.Flip41(pt, ear.GetTetra(1));
  m_curTe := ear.GetTetra(1);
  te := m_curTe;
  if (te.IsFlat = True) then
  begin
    for i := 1 to 4 do
    begin
      adj := te.GetT(i);
      te3 := Find3rdTetra(te, adj);
      if (te3 <> nil) then
      begin
        self.Flip32(te, adj, te3);
        break;
      end;
    end;
  end;

  // ShowMessage('# of processed ears: ' + IntToStr(k) +
  // '    # of flips: ' + IntToStr(l));
  lstEar.Zap;
  lstEar.Free;
  lstPts.Clear;
  lstPts.Free;

{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
  { if (draw = true) then
    begin
    bDrawDeletion := false;
    end; }
  Result := l;
end;

function TTetraMesh.ProcessEar_e__2(ear: TGBEar;
  lstPts, lstEar: TList): integer;
var
  updated: integer;
  intersect: integer;
begin
  updated := 0;
  if (ear.IsConvex = 1) then
  begin
    intersect := ear.IsFlippable;
    Assert(intersect <> 2);
    Assert(intersect <> 3);
    // -------------- Flip23 -------------------------------
    if (intersect = 1) then
    begin
      if (ear.IsDelaunay(lstPts) = True) then
      begin
        ProcessEar_flip23(ear);
        updated := 1;
      end;
    end
    // -------------- Flip32 -------------------------------
    else if (intersect = -1) then
    begin
      if (ProcessEar_flip32(ear, lstEar, lstPts) = True) then
        updated := 1;
    end
    // -------------- Flip44 -------------------------------
    else if (intersect = 0) then
    begin
      updated := ProcessEar_degenerate_e__2(ear, lstEar, lstPts);
    end;
  end;
  Result := updated;
end;

function TTetraMesh.ProcessEar_flip32__2(ear: TGBEar; lstEar, lstPts: TList;
  flag: boolean): boolean;
var
  te, adj, te3: TGBTetrahedron;
  v: TGBPoint3D;
  removePt, planePt: TGBPoint3D;
  tempt: TGBPoint3D;
  i, j, n: integer;
  intersect2: integer;
  ear3: array [1 .. 2] of TGBEar;
  ext: array [1 .. 3] of TGBEar;
  updated: boolean;
  tempear: TGBEar;
  stay1: TGBTetrahedron;
  tempte: TGBTetrahedron;
begin
  updated := False;
  v := ear.GetVertex;
  te := ear.GetTetra(1);
  adj := ear.GetTetra(2);
  te3 := Find3rdTetra(te, adj);
  // -- this 3rd tetra must exist & be inside H
  if (te3 <> nil) then
  begin
    removePt := nil;
    planePt := nil;
    te.SetIndex(adj);
    adj.SetIndex(te);
    // -- find the point that will be removed from H at the end; and also find
    // -- the point which will be part of the future facet after the flip.
    for i := 1 to 3 do
    begin
      tempt := te.GetProt(i);
      if (tempt <> v) then
      begin
        if (te3.HasPoint(tempt) = True) then
          removePt := tempt
        else
          planePt := tempt;
      end;
    end;

    // if (flag = true) then
    // -- the 2 points of the edge of degree 3 must be of each side of the
    // -- middle plane
    intersect2 := TGBGeomTools.IntersectionEdgeTriangle(v, removePt,
      te.GetProt(0), adj.GetProt(0), planePt);
    if (intersect2 <> -1) then
    begin
      // Assert(ear.IsDelaunay(lstPts) = true);  { TODO : 3-ear always Delaunay? }
      // -- 3rd, test if the ear is DELAUNAY
      if (ear.IsDelaunay(lstPts) = True) then
      begin
        // -- get pointers to 3 ears forming the 3ear & to 3 exterior ears
        j := 1;
        for i := 1 to 4 do
        begin
          tempear := ear.GetAdj(i);
          if (tempear.ContainTetra(te3) = True) then
          begin
            ear3[j] := tempear;
            Inc(j);
          end
          else
          begin
            if (tempear.ContainTetra(te) = True) then
              ext[1] := tempear
            else
              ext[2] := tempear;
          end;
        end;
        for i := 1 to 4 do
        begin
          tempear := ear3[1].GetAdj(i);
          if ((tempear.ContainTetra(te3) = True) and
            (tempear.ContainTetra(te) = False) and
            (tempear.ContainTetra(adj) = False)) then
          begin
            ext[3] := tempear;
            break;
          end;
        end;
        // -- perform the flip
        updated := True;
        Flip32(te, adj, te3);
        lstPts.Remove(removePt); // -- remove 'exterior' point from TList
        if (te.HasPoint(v) = True) then
        begin
          stay1 := te;
        end
        else
        begin
          stay1 := adj;
        end;

        // -- update the 3 ears (the ext ears) adjacent to the 3 deleted ears
        for i := 1 to 3 do
        begin
          tempear := ext[i];
          if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj) or
            (tempear.GetTetra(1) = te3)) then
          begin
            tempear.SetTetra(1, stay1);
          end
          else
          begin
            tempear.SetTetra(2, stay1);
          end;
        end;

        // -- update of the adj fields of the 3 ext ears
        for i := 1 to 3 do
        begin
          tempear := ext[i];
          tempte := tempear.GetTetra(1);
          if (tempte = stay1) then
          begin
            n := 1;
            tempte.SetIndex(tempear.GetTetra(2));
          end
          else
          begin
            n := 3;
            tempte := tempear.GetTetra(2);
            tempte.SetIndex(tempear.GetTetra(1));
          end;
          if (i = 1) then
          begin
            tempear.SetAdj(n, ext[2]);
            tempear.SetAdj(n + 1, ext[3]);
          end
          else if (i = 2) then
          begin
            tempear.SetAdj(n, ext[1]);
            tempear.SetAdj(n + 1, ext[3]);
          end
          else // if (i = 3) then
          begin
            tempear.SetAdj(n, ext[1]);
            tempear.SetAdj(n + 1, ext[2]);
          end;
        end;
        // -- destroy 3 ears forming the 3ear
        lstEar.Remove(ear);
        lstEar.Remove(ear3[1]);
        lstEar.Remove(ear3[2]);
        ear.Free;
        ear3[1].Free;
        ear3[2].Free;
        te3.Free;
      end;
    end;
  end;
  Result := updated;
end;


// ******************************************************************************
// Description: Delete a vertex from the mesh -- with InSphere() method.
// Input:       pt: the vertex to delete
// draw: drawing or not the envelope (a boolean)
// Output:      (the mesh is updated)
// the # of flips needed to delete the vertex
// Remarks:     using the method where a list of TEar is kept up-to-date
// ******************************************************************************
function TTetraMesh.DeleteVertex_insphere(pt: TGBPoint3D;
  draw: boolean): integer;
var
  re: integer;
begin
{$IFDEF PERTURB}
  re := self.DeleteVertex_insphere_perturb(pt, draw);
{$ELSEIF True}
  re := self.DeleteVertex_insphere_no_perturb(pt, draw);
{$IFEND}
  Result := re;
end;

procedure TTetraMesh.DeleteVertex_optimal(pt: TGBPoint3D; dlimit: integer);
begin
  if (GetDegree(pt) <= dlimit) then
    self.DeleteVertex_insphere_no_perturb(pt)
  else
    self.DeleteVertex_power__clean(pt);
end;

function TTetraMesh.GetNbPts: integer;
begin
  Result := m_lstVertex.Count;
end;

function TTetraMesh.GetNbPtsOnCH: integer;
var
  i: integer;
  nb: integer;
begin
  nb := 0;
  for i := 4 to (m_lstVertex.Count - 1) do // -- do not compute for big tetra
  begin
    if (IsBoundaryCH(m_lstVertex[i]) = True) then
    begin
      Inc(nb);
    end;
  end;
  Result := nb;
end;

function TTetraMesh.IsBoundaryCH(pt: TGBPoint3D): boolean;
var
  lstNN: TList;
  i: integer;
  tempt: TGBPoint3D;
  re: boolean;
begin
  re := False;
  if (self.IsBigTetra(pt) = False) then
  begin
    lstNN := self.GetNN_vertices(pt);
    for i := 0 to (lstNN.Count - 1) do
    begin
      tempt := lstNN[i];
      if (m_lstVertex.IndexOf(tempt) <= 3) then
      begin
        re := True;
        break;
      end;
    end;
    lstNN.Free;
  end;
  Result := re;
end;

function TTetraMesh.IsBigTetra(pt: TGBPoint3D): boolean;
begin
  if (m_lstVertex.IndexOf(pt) <= 3) then
    Result := True
  else
    Result := False;
end;

procedure TTetraMesh.InsertDelete(x, y, z: double);
const
  tolerance: double = 1E-12;
var
  splite: TGBTetrahedron;
  v: TGBPoint3D;
  tempt: TGBPoint3D;
  collision: boolean;
  i, j: integer;
  lstFlips: TList;
  lstNN: TList;
  e: TGBEdge;
  te0, te1, te2: TGBTetrahedron;
  extrapol: boolean;
  tempe: TGBEdge;
  incte: TGBTetrahedron; // -- the tetra always incident to the dummy vertex
begin
  m_tempPt.SetCoord(x, y, z);

  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create
      ('Location outside the bounding box. Extrapolation impossible.');
  end;

  // -- check if 'collision' with a data point, if yes then return its value
  splite := Walk(m_tempPt);
  collision := False;
  for i := 1 to 4 do
  begin
    if (m_tempPt.Hit(splite.GetP(i), tolerance) = True) then
    begin
      collision := True;
      break;
    end;
  end;

  if (collision = False) then
  begin
    // -- insert point in the mesh while storing order of the flips performed
    extrapol := False;
    v := TGBPoint3D.Create(True, x, y, z);
    m_lstVertex.Add(v);
    lstNN := TList.Create; // -- store first four nn in lstNN, before flip14
    lstNN.Add(splite.GetP(1));
    lstNN.Add(splite.GetP(2));
    lstNN.Add(splite.GetP(3));
    lstNN.Add(splite.GetP(4));
    Flip14(v, splite);
    lstFlips := TList.Create;
    Optim(splite, v, lstFlips, lstNN);
{$IFDEF DEBUG}
    TestInvariants;
{$ENDIF}
    // -- delete the vertex by applying inverse flips in reverse order
    incte := Walk(v);
    Assert(incte.HasPoint(v) = True);
    for i := (lstFlips.Count - 1) downto 0 do
    begin
      e := lstFlips[i];
      if (e.HasPoint(v) = True) then
      begin
        // -- apply a flip32
        te0 := WalkAroundVertex(v, e.GetDest, incte);
        te0.SetIndex(v);
        te1 := nil;
        te2 := nil;
        for j := 1 to 3 do
        begin
          if (te0.GetProt(j) <> e.GetDest) then
          begin
            if (te1 = nil) then
              te1 := te0.GetTrot(j)
            else
              te2 := te0.GetTrot(j);
          end;
        end;
        Assert(te1 <> nil);
        Assert(te2 <> nil);
        Assert(Find3rdTetra(te0, te1) <> nil);
        Flip32(te0, te1, te2);
        if (te0.HasPoint(v) = True) then
          incte := te0
        else
          incte := te1;
      end
      else // -- then apply a flip23
      begin
        e.SetTetra(WalkAroundVertex(v, e.GetOrg, incte));
        tempe := TGBEdge.Create(v, e.GetOrg, e.GetAdjTetra);
        te0 := WalkAroundEdge(tempe, e.GetDest);
        tempe.Free;
        Assert(te0.HasPoint(e.GetOrg) = True);
        Assert(te0.HasPoint(e.GetDest) = True);
        te0.SetIndex(v);
        te1 := nil;
        for j := 1 to 3 do
        begin
          if ((te0.GetProt(j) <> e.GetOrg) and (te0.GetProt(j) <> e.GetDest))
          then
          begin
            te1 := te0.GetTrot(j);
            break;
          end;
        end;
        Assert(te0.HasNeighbour(te1) = True);
        Assert(te0.HasPoint(v) = True);
        Assert(te1.HasNeighbour(te0) = True);
        Assert(te1.HasPoint(v) = True);
        te2 := Flip23(te0, te1);
        if (te0.HasPoint(v) = True) then
          incte := te0
        else if (te1.HasPoint(v) = True) then
          incte := te1
        else
          incte := te2;
      end;
    end;
    Flip41(v, incte);
    // -- free the edges in lstFlips
    j := (lstFlips.Count) - 1;
    for i := j downto 0 do
    begin
      e := lstFlips[i];
      e.Free;
    end;
    lstFlips.Free;
    lstNN.Free;
  end;
{$IFDEF DEBUG}
  TestInvariants;
{$ENDIF}
end;

// ******************************************************************************
// Description: Traverse (or scan) the whole mesh to extract every tetra. This
// is done instead of keeping a permanent list. It is also possible
// to extract each face and each vertex.
// Input:       - (mesh will be traversed)
// Output:      (m_lstTetra is updated)
// ******************************************************************************
procedure TTetraMesh.Traverse;
var
  curTe: TGBTetrahedron;
  child: TGBTetrahedron;
  tempEdge: TGBEdge;
  i, j: integer;
  i_child: integer;
  stack: TStack;
  o, o2: integer;
  processChild: boolean;
  normFace: TGBVector;
begin
  m_lstTetra.Clear;
  // -- get the 'down tetra', the only one seen from the viewpoint
  curTe := self.Walk(m_lstVertex[0]);
  curTe := self.WalkAroundVertex(m_lstVertex[0], m_lstVertex[1], curTe);
  tempEdge := TGBEdge.Create(TGBPoint3D(m_lstVertex[0]),
    TGBPoint3D(m_lstVertex[1]), curTe);
  curTe := self.WalkAroundEdge(tempEdge, m_lstVertex[2]);
  tempEdge.Free;
  Assert(curTe.HasFace(m_lstVertex[0], m_lstVertex[1], m_lstVertex[2]));
  for i := 1 to 4 do
  begin
    if ((curTe.GetP(i) <> m_lstVertex[0]) and (curTe.GetP(i) <> m_lstVertex[1])
      and (curTe.GetP(i) <> m_lstVertex[2])) then
    begin
      curTe.SetIndex(i);
      break;
    end;
  end;

  normFace := TGBVector.Create;
  stack := TStack.Create;
  stack.Push(curTe);
  while (stack.Count > 0) do
  begin
    curTe := stack.Pop;
    m_lstTetra.Add(curTe);
    curTe.RotIndex(1);
    child := nil;
    for i := 0 to 2 do
    begin
      if ((curTe.GetIndex mod 2) = 0) then
        o := TGBGeomTools.Orient3D(curTe.GetProt(1), curTe.GetProt(2),
          curTe.GetProt(3), m_viewpoint)
      else
        o := TGBGeomTools.Orient3D(curTe.GetProt(1), curTe.GetProt(3),
          curTe.GetProt(2), m_viewpoint);

      processChild := False;
      if (o > 0) then // out face
      begin
        child := curTe.GetTrot(0);
        if (child = nil) then
          processChild := False
        else
          processChild := True;
      end
      else if (o = 0) then // -- must make sure it's a out face first
      begin
        TGBGeomTools.NormalDirectionPt(curTe.GetProt(1), curTe.GetProt(2),
          curTe.GetProt(3), curTe.GetProt(0), normFace);
        if (TGBVector.DotProduct(m_normalIN, normFace) < 0) then
        begin
          child := curTe.GetTrot(0);
          if (child = nil) then
            processChild := False
          else
            processChild := True;
        end
        else
          processChild := False;
      end;

      if (processChild = True) then
      begin
        child.SetIndex(curTe);
        i_child := child.GetIndex;
        child.RotIndex(1);
        for j := 0 to 2 do // -- test each face of the child
        begin
          if ((child.GetIndex mod 2) = 0) then
            o2 := TGBGeomTools.Orient3D(child.GetProt(1), child.GetProt(2),
              child.GetProt(3), m_viewpoint)
          else
            o2 := TGBGeomTools.Orient3D(child.GetProt(1), child.GetProt(3),
              child.GetProt(2), m_viewpoint);

          if (o2 < 0) then // -- if this is a IN face also
          begin
            if (child.GetIndex < i_child) then
            begin
              // -- do NOT process this child, not entering from the valid_in
              processChild := False;
              break;
            end;
          end
          else if (o2 = 0) then
          begin
            // -- decide is the face is a IN face also first
            TGBGeomTools.NormalDirectionPt(child.GetProt(1), child.GetProt(2),
              child.GetProt(3), child.GetProt(0), normFace);
            if (TGBVector.DotProduct(m_normalIN, normFace) > 0) then
            begin
              if (child.GetIndex < i_child) then
              begin
                // -- do NOT process this child, not entering from the valid_in
                processChild := False;
                break;
              end;
            end;
          end;
          child.RotIndex(1);
        end;
        if (processChild = True) then
        begin
          child.SetIndex(i_child);
          stack.Push(child);
        end;
      end;
      curTe.RotIndex(1);
    end;
  end;
  normFace.Free;
end;

// ******************************************************************************
// Description: Used to compute the partial volume of a nn when using natural
// neighbour interpolation.
// Input:       umbrella: TList of all the Delaunay edges we want to compute the
// volumes of.
// Output:      the volume of the cell (partial value)
// ******************************************************************************
function TTetraMesh.VolumeVoronoiCell_partial(umbrella: TList): double;
var
  e: TGBEdge;
  lstPts: TList;
  i, j: integer;
  vol: double;
  tempte: TGBTetrahedron;
begin
  vol := 0;
  lstPts := TList.Create;
  for i := 0 to (umbrella.Count - 1) do
  begin
    e := umbrella[i];
    ComputeVoronoiFaceAroundEdge(e, lstPts);
    // -- triangulate the face CCW and compute volume of each tetra with origin
    j := 2;
    while j <= (lstPts.Count - 1) do
    begin
      vol := vol + TGBGeomTools.Det3x3(TGBPoint3D(lstPts[0]),
        TGBPoint3D(lstPts[j - 1]), TGBPoint3D(lstPts[j]));
      Inc(j);
    end;
    // -- free the list of voronoi points
    for j := 0 to (lstPts.Count - 1) do
    begin
      TGBPoint3D(lstPts[j]).Free;
    end;
    lstPts.Clear;
  end;
  Result := vol;
end;

// ******************************************************************************
// Description: Build a list containing all the edges (TEdge) that are incident
// to a given vertex in a mesh, but only the ones that are on the
// link [link(v)] of the vertex v. This is used not to compute too
// much stuff for natural neighbour interpolation.
// Input:       gen:  the TGBPoint3D the edges are sought
// v:    the TGBPoint3D for link(v)
// Output:      a TList containing the TEdge
// ******************************************************************************
function TTetraMesh.GetNN_edges_linkofV(gen: TGBPoint3D; v: TGBPoint3D): TList;
var
  a: TGBPoint3D;
  b: TGBPoint3D;
  i: integer;
  lstEdges: TList;
  cur: TGBTetrahedron;
  te: TGBTetrahedron;
begin
  cur := Walk(gen);
  te := WalkAroundVertex(gen, v, cur);
  cur := te;
  Assert(cur.HasPoint(v));
  Assert(cur.HasPoint(gen));

  lstEdges := TList.Create;

  // -- define the 2 opposite vertices (of the edge) in a tetra to rotate
  // -- around the edge
  cur.SetIndex(v);
  if (cur.GetProt(1) = gen) then
  begin
    a := cur.GetProt(2);
    b := cur.GetProt(3);
  end
  else if (cur.GetProt(2) = gen) then
  begin
    a := cur.GetProt(1);
    b := cur.GetProt(3);
  end
  else
  begin
    a := cur.GetProt(1);
    b := cur.GetProt(2);
  end;

  repeat
    Assert(cur.HasEdge(gen, b));
    lstEdges.Add(TGBEdge.Create(gen, b, cur.SetIndex(v).GetTrot(0)));
    cur := cur.SetIndex(a).GetTrot(0);
    Assert(cur.HasPoint(b));
    Assert(cur.HasPoint(gen));
    Assert(cur.HasPoint(v));
    a := b;
    for i := 1 to 4 do
    begin
      if ((cur.GetP(i) <> v) and (cur.GetP(i) <> gen) and (cur.GetP(i) <> a))
      then
      begin
        b := cur.GetP(i);
        break;
      end;
    end;
  until (cur = te);
  // -- store edge gen-v
  lstEdges.Add(TGBEdge.Create(gen, v, cur));

  Result := lstEdges;
end;

// ******************************************************************************
// Description: Build a list containing all the edges (TEdge) that are incident
// to a given vertex in a mesh.
// Input:       gen: the TGBPoint3D
// Output:      a TList containing all the TEdge
// Note:        WATCH OUT: new TEdge are created and this method does NOT manage
// the memory. The TList of TEdge output must be destroyed
// somewhere else.
// ******************************************************************************
function TTetraMesh.GetStar_edges(gen: TGBPoint3D): TList;
var
  cur: TGBTetrahedron;
  tempte: TGBTetrahedron;
  queue: TQueue;
  i: integer;
  lstTetra: TList;
  lstEdges: TList;
  lstPts: TList;
begin
  cur := Walk(gen);
  queue := TQueue.Create;
  queue.Push(cur);
  lstTetra := TList.Create;
  lstEdges := TList.Create;
  lstPts := TList.Create;

  while (queue.Count > 0) do
  begin
    cur := queue.Pop;
    cur.SetIndex(gen);
    if (lstTetra.IndexOf(cur) = -1) then
    begin
      lstTetra.Add(cur);
      // -- add the exterior points if they are not already in the list
      for i := 1 to 3 do
      begin
        if (lstPts.IndexOf(cur.GetProt(i)) = -1) then
        begin
          lstPts.Add(cur.GetProt(i));
          lstEdges.Add(TGBEdge.Create(gen, cur.GetProt(i), cur));
        end;
      end;
    end;
    // -- add the neighbours if they are valid
    for i := 1 to 3 do
    begin
      tempte := cur.GetTrot(i);
      if ((tempte <> nil) and (lstTetra.IndexOf(tempte) = -1) and
        (tempte.HasPoint(gen))) then
      begin
        queue.Push(tempte);
      end;
    end;
  end;
  FreeAndNil(queue);
  FreeAndNil(lstTetra);
  FreeAndNil(lstPts);
  Result := lstEdges;
end;


// ******************************************************************************
// Description: Moving a point to another location.
// Input:       pt:     the point to move
// x,y,z:  coords of the destination
// Output:      - (TTetraMesh is updated and hopefully not corrupted)
// ******************************************************************************
{
  !!!!!!!!!!!!!!!DELETED!!!!!!!!!!!!!!!!
}

procedure TTetraMesh.GetStar_v_tetra_ear(pt: TGBPoint3D; out lstEar: TListz;
  out lstPts, lstTe: TList);
var
  cur: TGBTetrahedron;
  tempte: TGBTetrahedron;
  queue: TQueue;
  i: integer;
  j: integer;
  ear: array [1 .. 3] of TGBEar;
  exist: array [1 .. 3] of boolean;
  tempear: TGBEar;
begin
  cur := Walk(pt);
  queue := TQueue.Create;
  queue.Push(cur);
  while (queue.Count > 0) do
  begin
    cur := queue.Pop;
    cur.SetIndex(pt);
    if (lstTe.IndexOf(cur) = -1) then
    begin
      lstTe.Add(cur);
      // -- add the exterior points if they are not already in the list
      for i := 1 to 3 do
      begin
        if (lstPts.IndexOf(cur.GetProt(i)) = -1) then
        begin
          lstPts.Add(cur.GetProt(i));
        end;
      end;
      // -- add the neighbours if they are valid
      for i := 1 to 3 do
      begin
        tempte := cur.SetIndex(pt).GetTrot(i);
        Assert(tempte <> nil);
        Assert(tempte.HasPoint(pt));
        if (lstTe.IndexOf(tempte) = -1) then
        begin
          queue.Push(tempte);
          exist[i] := False;
          ear[i] := TGBEar.Create(cur, tempte, pt);
          lstEar.Add(ear[i]);
        end
        else
        // -- look in the list of ears to find the one that contains the same
        // -- 2 tetra
        begin
          exist[i] := True;
          for j := 0 to (lstEar.Count - 1) do
          begin
            tempear := lstEar[j];
            if (tempear.Compare(tempte, cur) = True) then
            begin
              ear[i] := tempear;
            end;
          end;
        end;
      end;
      // -- update adjacent fields for the ears
      if (exist[1] = False) then
      begin
        ear[1].SetAdj(1, ear[2]);
        ear[1].SetAdj(2, ear[3]);
      end
      else
      begin
        ear[1].SetAdj(3, ear[2]);
        ear[1].SetAdj(4, ear[3]);
      end;
      if (exist[2] = False) then
      begin
        ear[2].SetAdj(1, ear[1]);
        ear[2].SetAdj(2, ear[3]);
      end
      else
      begin
        ear[2].SetAdj(3, ear[1]);
        ear[2].SetAdj(4, ear[3]);
      end;
      if (exist[3] = False) then
      begin
        ear[3].SetAdj(1, ear[1]);
        ear[3].SetAdj(2, ear[2]);
      end
      else
      begin
        ear[3].SetAdj(3, ear[1]);
        ear[3].SetAdj(4, ear[2]);
      end;
    end;
  end;
  FreeAndNil(queue);
end;

function TTetraMesh.GetNearestNeighbour(x, y, z: double): TGBPoint3D;
const
  tolerance: double = 1E-12;
var
  tempte: TGBTetrahedron;
  tempt, closest: TGBPoint3D;
  i: integer;
  dist, minDist: double;
  found: boolean;
  bCloser: boolean;
  lstNN: TList;
  extrapol: boolean;
begin
  m_tempPt.SetCoord(x, y, z);
  // -- check if the new point is inside the big tetrahedron
  if (IsInsideBoundingBox(m_tempPt) = False) then
  begin
    raise Exception.Create('Location outside the bounding box.');
  end;

  tempte := Walk(m_tempPt);
  closest := nil;
  minDist := 9999;
  for i := 1 to 4 do
  begin
    dist := TGBGeomTools.Distance3d(m_tempPt, tempte.GetP(i));
    if (dist < minDist) then
    begin
      closest := tempte.GetP(i);
      minDist := dist;
    end;
  end;

  Assert(closest <> nil);

  while True do
  begin
    lstNN := GetNN_vertices(closest);
    bCloser := False;
    for i := 0 to (lstNN.Count - 1) do
    begin
      dist := TGBGeomTools.Distance3d(m_tempPt, lstNN[i]);
      if (dist < minDist) then
      begin
        bCloser := True;
        closest := lstNN[i];
        minDist := dist;
      end;
    end;
    if (bCloser = False) then
      break;
  end;
  Result := closest;
end;

{
  procedure TTetraMesh.SetExtraPt(x, y, z: double);
  var
  pt: TGBPoint3D;
  begin
  pt := TGBPoint3D.Create(x, y, z);
  glDeleteLists(m_dlExtraPt, 1);
  m_dlExtraPt := glGenLists(1);
  glNewList(m_dlExtraPt, GL_COMPILE);
  TDrawTools.DrawNode(pt, 1, 1, 1);
  glEndList;
  pt.Free;
  end;
}

// ******************************************************************************
// Description: Estimate the attribute value at a given location with the
// nearest neighbour interpolation method. The value of the
// attribute of the closest neighbour is assigned to the location.
// Input:       x, y, z: location where we want to estimate value of attribute.
// Output:      estimation of the attribute
// ******************************************************************************
function TTetraMesh.Interpolate_nearest(x, y, z: double): double;
var
  nearPt: TGBPoint3D;
begin
  nearPt := self.GetNearestNeighbour(x, y, z);
  // if (bDrawSlicePts = true) then
  nearPt.flag := True;
  Result := nearPt.Data;
end;

procedure TTetraMesh.SliceInterpolation(method, gridSize: integer;
  zPos: double; out estimates: array of double; radius: double);
var
  spacing: double;
  i, j: integer;
  Value: double;
  x, y: double;
  p1, p2, p3, p4: TGBPoint3D;
  deltaAtt: double;
  scaling: double;
begin
  spacing := 1 / gridSize;
  { Form1.ProgressBar1.Min := 0;
    Form1.ProgressBar1.Max := gridSize * gridSize;
    Form1.ProgressBar1.Step := 1;
    Form1.ProgressBar1.Position := 0;
  }
  // -- compute values of estimates
  for i := 0 to (gridSize - 1) do
  begin
    for j := 0 to (gridSize - 1) do
    begin
      x := (i * spacing) + (spacing / 2);
      y := (j * spacing) + (spacing / 2);
      if (method = 1) then
        estimates[i * gridSize + j] := self.Interpolate_linear(x, y, zPos)
      else if (method = 2) then
        estimates[i * gridSize + j] := self.Interpolate_nn(x, y, zPos)
      else if (method = 3) then
        estimates[i * gridSize + j] := self.Interpolate_idw(x, y, zPos, radius)
      else if (method = 4) then
        estimates[i * gridSize + j] := self.Interpolate_nearest(x, y, zPos);
      // Form1.ProgressBar1.StepIt;
    end;
  end;
  // Form1.ProgressBar1.Position := 0;
end;

function TTetraMesh.GetMaxValueAttribute: double;
begin
  Result := m_maxAtt;
end;

function TTetraMesh.GetMinValueAttribute: double;
begin
  Result := m_minAtt;
end;

procedure TTetraMesh.ResetFlagAllPts;
var
  i: integer;
begin
  for i := 4 to (m_lstVertex.Count - 1) do
  begin
    TGBPoint3D(m_lstVertex[i]).flag := False;
  end;
end;

{
  procedure TTetraMesh.SetSlicePts(bDrawCells: boolean);
  var
  tempt: TGBPoint3D;
  i: integer;
  begin
  glDeleteLists(m_dlSlicePts, 1);
  m_dlSlicePts := glGenLists(1);
  glNewList(m_dlSlicePts, GL_COMPILE);
  for i := 4 to (m_lstVertex.Count - 1) do
  begin
  tempt := m_lstVertex[i];
  if ( (tempt.flag = true) and (self.IsBoundaryCH(tempt) = false) ) then
  begin
  if (bDrawCells = true) then
  self.DrawHLCell(tempt)
  else
  TDrawTools.DrawNode(tempt, 0, 1, 0, 1);
  end;
  end;
  glEndList;
  end;
}

{
  procedure TTetraMesh.DrawSliceInterpolationStartEnd(start: boolean);
  begin
  if (start = true) then
  begin
  glDeleteLists(m_dlInterpol, 1);
  m_dlInterpol := glGenLists(1);
  glNewList(m_dlInterpol, GL_COMPILE);
  end
  else
  begin
  glEndList;
  end;
  end;
}

procedure TTetraMesh.Move_BuildListReal(out lstReal: TList;
  lstTe, lstNN: TList; pt: TGBPoint3D);
var
  i, j: integer;
  te, adj: TGBTetrahedron;
  tempt: TGBPoint3D;
  tempte: TGBTetrahedron;
begin
  for i := 0 to (lstTe.Count - 1) do
  begin
    te := lstTe[i];
    adj := te.SetIndex(pt).GetTrot(0);
    if (adj <> nil) then
    begin
      tempt := adj.SetIndex(te).GetProt(0);
      if (lstNN.IndexOf(tempt) = -1) then
      begin
        lstReal.Add(adj);
      end
      else
      begin
        for j := 1 to 3 do
        begin
          tempte := adj.GetTrot(j);
          if ((tempte <> nil) and (tempte.HasPoint(pt) = True)) then
            break;
        end;
        if (lstTe.IndexOf(te) < lstTe.IndexOf(tempte)) then
        begin
          lstReal.Add(adj);
        end;
      end;
    end;
  end;
end;

function TTetraMesh.Move_GetClosest_t(lstReal: TList; lstEar: TListz;
  pt, pt2: TGBPoint3D; out nearte: TGBTetrahedron; out nearear: TGBEar;
  out tmin: double): boolean;
var
  t1, t2: double;
  i: integer;
  te, adj: TGBTetrahedron;
  inout: boolean;
  adjEar1, adjEar2: TGBEar;
  ear: TGBEar;
begin
  // -- compute intersections trajectory-sphere for all REAL tetra
  tmin := 999;
  nearte := nil;
  for i := 0 to (lstReal.Count - 1) do
  begin
    te := lstReal[i];
    if (TGBGeomTools.IntersectionLineSphere(pt, pt2, te, t1, t2) = True) then
    begin
      Assert(t1 < t2);
      if (t1 < tmin) then
      begin
        if ((t1 < 0) or (t2 < 0)) then
        begin
          if (self.Move_FlippabilityTest(pt, pt2, te, t1) = True) then
          begin // -- otherwise don't do anything, will create shit later on...
            nearte := te;
            tmin := t1;
          end;
        end
        else
        begin
          nearte := te;
          tmin := t1;
        end;
      end;
    end;
  end;
  inout := True; // -- set the move type to IN

  {
    //-- compute intersections trajectory-sphere for all REAL tetra
    tmin := 999;
    nearte := nil;
    for i := 0 to (lstReal.Count - 1) do
    begin
    te := lstReal[i];
    if (TGeomTools.IntersectionLineSphere(pt, pt2, te, t1, t2) = true) then
    begin
    Assert(t1 < t2);
    if (t1 < tmin) then
    //      if ( (t1 < tmin) and (t2 > 0) ) then
    begin
    nearte := te;
    tmin := t1;
    end;
    end;
    end;
    inout := true; //-- set the move type to IN
  }

  // -- compute intersections trajectory-sphere for all IMAGINARY tetra
  nearear := nil;
  adjEar1 := nil;
  adjEar2 := nil;
  for i := 0 to (lstEar.Count - 1) do
  begin
    ear := lstEar[i];
    if (ear.IsConvex = 1) then // -- only valid ears are IMA tetra
    begin
      if (ear.Is3ear(adjEar1, adjEar2) = False) then
      begin
        te := ear.GetTetra(1);
        adj := ear.GetTetra(2);
        te.SetIndex(ear.GetVertex);
        adj.SetIndex(te);

        if (TGBGeomTools.IntersectionLineSphere(pt, pt2, te.GetProt(1),
          te.GetProt(2), te.GetProt(3), adj.GetProt(0), t1, t2) = True) then
        begin
          Assert(t1 < t2);
          if (t2 < tmin) then
          begin
            inout := False;
            nearear := ear;
            tmin := t2;
          end;
        end;
      end
      else
      begin // -- the ear is a 3-ear
        // -- only process the ear if it is the first one in the lstEar...
        if ((i < lstEar.IndexOf(adjEar1)) and (i < lstEar.IndexOf(adjEar2)))
        then
        begin
          te := ear.GetTetra(1);
          adj := ear.GetTetra(2);
          te.SetIndex(ear.GetVertex);
          adj.SetIndex(te);

          if (TGBGeomTools.IntersectionLineSphere(pt, pt2, te.GetProt(1),
            te.GetProt(2), te.GetProt(3), adj.GetProt(0), t1, t2) = True) then
          begin
            Assert(t1 < t2);
            if (t2 < tmin) then
            begin
              inout := False;
              nearear := ear;
              tmin := t2;
            end;
          end;
        end;
      end;
    end;
  end;
  Result := inout;
end;

function TTetraMesh.Move_Flip32_ear(ear: TGBEar; lstEar: TList)
  : TGBTetrahedron;
var
  te, adj, te3: TGBTetrahedron;
  v: TGBPoint3D;
  removePt, planePt: TGBPoint3D;
  tempt: TGBPoint3D;
  i, j, n: integer;
  intersect2: integer;
  ear3: array [1 .. 2] of TGBEar;
  ext: array [1 .. 3] of TGBEar;
  updated: boolean;
  tempear: TGBEar;
  stay1: TGBTetrahedron;
  tempte: TGBTetrahedron;
begin
  updated := False;
  v := ear.GetVertex;
  te := ear.GetTetra(1); // -- is the ear a 3-ear
  adj := ear.GetTetra(2);
  te3 := Find3rdTetra(te, adj);
  stay1 := nil;
  // -- this 3rd tetra must exist & be inside H
  if (te3 <> nil) then
  begin
    removePt := nil;
    planePt := nil;
    te.SetIndex(adj);
    adj.SetIndex(te);
    // -- find the point that will be removed from H at the end; and also find
    // -- the point which will be part of the future facet after the flip.
    for i := 1 to 3 do
    begin
      tempt := te.GetProt(i);
      if (tempt <> v) then
      begin
        if (te3.HasPoint(tempt) = True) then
          removePt := tempt
        else
          planePt := tempt;
      end;
    end;

    // -- the 2 points of the edge of degree 3 must be of each side of the
    // -- middle plane
    intersect2 := TGBGeomTools.IntersectionEdgeTriangle(v, removePt,
      te.GetProt(0), adj.GetProt(0), planePt);
    if (intersect2 <> -1) then
    begin
      // -- get pointers to 3 ears forming the 3ear & to 3 exterior ears
      j := 1;
      for i := 1 to 4 do
      begin
        tempear := ear.GetAdj(i);
        if (tempear.ContainTetra(te3) = True) then
        begin
          ear3[j] := tempear;
          Inc(j);
        end
        else
        begin
          if (tempear.ContainTetra(te) = True) then
            ext[1] := tempear
          else
            ext[2] := tempear;
        end;
      end;
      for i := 1 to 4 do
      begin
        tempear := ear3[1].GetAdj(i);
        if ((tempear.ContainTetra(te3) = True) and
          (tempear.ContainTetra(te) = False) and
          (tempear.ContainTetra(adj) = False)) then
        begin
          ext[3] := tempear;
          break;
        end;
      end;
      // -- perform the flip
      updated := True;
      Flip32(te, adj, te3);
      // lstPts.Remove(removePt); //-- remove 'exterior' point from TList
      if (te.HasPoint(v) = True) then
      begin
        stay1 := te;
      end
      else
      begin
        stay1 := adj;
      end;

      // -- update the 3 ears (the ext ears) adjacent to the 3 deleted ears
      for i := 1 to 3 do
      begin
        tempear := ext[i];
        if ((tempear.GetTetra(1) = te) or (tempear.GetTetra(1) = adj) or
          (tempear.GetTetra(1) = te3)) then
        begin
          tempear.SetTetra(1, stay1);
        end
        else
        begin
          tempear.SetTetra(2, stay1);
        end;
      end;

      // -- update of the adj fields of the 3 ext ears
      for i := 1 to 3 do
      begin
        tempear := ext[i];
        tempte := tempear.GetTetra(1);
        if (tempte = stay1) then
        begin
          n := 1;
          tempte.SetIndex(tempear.GetTetra(2));
        end
        else
        begin
          n := 3;
          tempte := tempear.GetTetra(2);
          tempte.SetIndex(tempear.GetTetra(1));
        end;
        if (i = 1) then
        begin
          tempear.SetAdj(n, ext[2]);
          tempear.SetAdj(n + 1, ext[3]);
        end
        else if (i = 2) then
        begin
          tempear.SetAdj(n, ext[1]);
          tempear.SetAdj(n + 1, ext[3]);
        end
        else // if (i = 3) then
        begin
          tempear.SetAdj(n, ext[1]);
          tempear.SetAdj(n + 1, ext[2]);
        end;
      end;
      // -- destroy 3 ears forming the 3ear
      lstEar.Remove(ear);
      lstEar.Remove(ear3[1]);
      lstEar.Remove(ear3[2]);
      ear.Free;
      ear3[1].Free;
      ear3[2].Free;
      te3.Free;
    end;
  end;
  Result := stay1;
end;

{
  procedure TTetraMesh.SetMovingPt(pt1, pt2: TGBPoint3D);
  begin
  glDeleteLists(m_dlMovingPt, 1);
  m_dlMovingPt := glGenLists(1);
  glNewList(m_dlMovingPt, GL_COMPILE);
  //    TDrawTools.DrawNode(pt1, 0, 1, 0, 1);
  //    TDrawTools.DrawNode(pt2, 1, 0, 0);
  TDrawTools.DrawLine(pt1, pt2, 0, 0.4, 0.4, 3);
  glEndList;
  end;
}

function TTetraMesh.Move_IsBehindTetraIncidentLink(behind: TGBTetrahedron;
  pt: TGBPoint3D): boolean;
var
  re: boolean;
  tempte: TGBTetrahedron;
  i: integer;
begin
  re := False;
  for i := 1 to 3 do
  begin
    tempte := behind.GetTrot(i);
    if (tempte.HasPoint(pt) = True) then
    begin
      re := True;
      break;
    end;
  end;
  Result := re;
end;

// ******************************************************************************
// Description: Get the neighbouring tetra, of a given tetra, that contains a
// certain vertex.
// Input:       te: the tetra whose neighbour we're looking for
// p:  the point
// Output:      the neighbouring tetra containing p
// nil if if doesn't exist
// ******************************************************************************
function TTetraMesh.GetNeighbouringTetraHavingPt(te: TGBTetrahedron;
  p: TGBPoint3D): TGBTetrahedron;
var
  i: integer;
  re: TGBTetrahedron;
  adj: TGBTetrahedron;
begin
  re := nil;
  for i := 1 to 4 do
  begin
    adj := te.GetT(i);
    if ((adj <> nil) and (adj.HasPoint(p) = True)) then
    begin
      re := adj;
      break;
    end;
  end;
  Result := re;
end;

function TTetraMesh.Move_FlippabilityTest(pt, pt2: TGBPoint3D;
  adj: TGBTetrahedron; t: double): boolean;
var
  te: TGBTetrahedron;
  newPos: TGBPoint3D;
begin
  te := GetNeighbouringTetraHavingPt(adj, pt);
  te.SetIndex(adj);
  newPos := TGBPoint3D.Create(pt.x + t * (pt2.x - pt.x),
    pt.y + t * (pt2.y - pt.y), pt.z + t * (pt2.z - pt.z));
  if (TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2), te.GetProt(3), pt)
    = TGBGeomTools.Orient3D(te.GetProt(1), te.GetProt(2), te.GetProt(3), newPos))
  then
    Result := True
  else
    Result := False;
  newPos.Free;
end;

function TTetraMesh.GetNbEdgeInStar(pt: TGBPoint3D): integer;
begin
  Result := TList(GetNN_vertices(pt)).Count;
end;

function TTetraMesh.GetNbTetraInStar(pt: TGBPoint3D): integer;
var
  lstPts, lstTetra: TList;
begin
  lstPts := TList.Create;
  lstTetra := TList.Create;
  GetStar_v_tetra(pt, lstTetra, lstPts);
  Result := lstTetra.Count;
end;

{
procedure TTetraMesh.SetHLCell(pt: TGBPoint3D);
begin
  glDeleteLists(m_dlHLCell, 1);
  m_dlHLCell := glGenLists(1);
  glNewList(m_dlHLCell, GL_COMPILE);
  DrawHLCell(pt);
  glEndList;
end;
}

{
  procedure TTetraMesh.DrawHLVertex(v: TGBPoint3D; bEdges, bLink: boolean);
  var
    lstPts, testList: TList;
    lstTetra: TList;
    i: integer;
    tempt, poi1: TGBPoint3D;
    te: TTetra;
    px, py, pz: Double;
    tp1, tp2,tp3,tp4, tp0: TGBPoint3D;
    begin
    //-- draw the HL vertex in red and a bit bigger
    TDrawTools.DrawNode(v, 1.0, 0, 0, 1);
    { tp1:= TGBPoint3D.Create(0,0,0, 0);
    tp2:= TGBPoint3D.Create(1,0,0, 0);
    tp3:= TGBPoint3D.Create(1,1,0, 0);
    tp4:= TGBPoint3D.Create(0,1,0, 0);
    tp0:= TGBPoint3D.Create(0.5,0.5,0, 0);
    testList := TList.Create;
    testList.Add(tp1);
    testList.Add(tp2);
    testList.Add(tp3);
    testList.Add(tp4);
    // TDrawTools.DrawNode(tp1, 1.0, 0, 0, 1);
    TDrawTools.DrawPolygonFace( testList ,tp0,1.0,0,0,1);
    tp0.Free;
    tp1.Free;
    tp2.Free;
    tp3.Free;
    tp4.Free;
    testList.Free;
    tp1:= TGBPoint3D.Create(0,0,0, 0);
    tp2:= TGBPoint3D.Create(0,0,1, 0);
    tp3:= TGBPoint3D.Create(0,1,1, 0);
    tp4:= TGBPoint3D.Create(0,1,0, 0);
    tp0:= TGBPoint3D.Create(0,0.5,0.5, 0);
    testList := TList.Create;
    testList.Add(tp1);
    testList.Add(tp2);
    testList.Add(tp3);
    testList.Add(tp4);
    // TDrawTools.DrawNode(tp1, 1.0, 0, 0, 1);
    TDrawTools.DrawPolygonFace( testList ,tp0,0,1.0,0,1);
    tp0.Free;
    tp1.Free;
    tp2.Free;
    tp3.Free;
    tp4.Free;
    testList.Free;
}
{ if ( (bEdges = true) or (bLink = true) ) then
  begin
  lstTetra := TList.Create;
  lstPts := TList.Create;
  GetStar_v_tetra(v, lstTetra, lstPts);

  //-- draw edges connecting pt and the points of the envelope
  if (bEdges = true) then
  begin
  for i := 0 to (lstPts.Count - 1) do
  begin
  tempt := lstPts[i];
  ///////////////////   0
  if (tempt = m_lstVertex[0])  then
  begin
  pz:=0;
  px:= (pz-v.z)*(tempt.x-v.x)/(tempt.z-v.z)+v.x;
  py:= (pz-v.z)*(tempt.y-v.y)/(tempt.z-v.z)+v.y;
  poi1:= TGBPoint3D.Create(px,py,pz, 0);
  if (IsInsideBoundingBox(poi1) = true) then
  begin
  TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end else begin
  py:=0;
  px:=(py-v.y)*(tempt.x-v.x)/(tempt.y-v.y)+v.x;
  pz:=(py-v.y)*(tempt.z-v.z)/(tempt.y-v.y)+v.z;
  poi1.SetCoord(px,py,pz);
  if (IsInsideBoundingBox(poi1) = true) then
  begin
  TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end else begin
  px:=0;
  py:=(px-v.x)*(tempt.y-v.y)/(tempt.x-v.x)+v.y;
  pz:=(px-v.x)*(tempt.z-v.z)/(tempt.x-v.x)+v.z;
  poi1.SetCoord(px,py,pz);
  TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end;


  end;
  TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end;
  //////////////////    1
  if (tempt = m_lstVertex[1])  then
  begin
    pz:=0;
    px:= (pz-v.z)*(tempt.x-v.x)/(tempt.z-v.z)+v.x;
    py:= (pz-v.z)*(tempt.y-v.y)/(tempt.z-v.z)+v.y;
    poi1:= TGBPoint3D.Create(px,py,pz, 0);
    if (IsInsideBoundingBox(poi1) = true) then
    begin
      TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
    end else
    begin
      py:=1;
      px:=(py-v.y)*(tempt.x-v.x)/(tempt.y-v.y)+v.x;
      pz:=(py-v.y)*(tempt.z-v.z)/(tempt.y-v.y)+v.z;
      poi1.SetCoord(px,py,pz);
      if (IsInsideBoundingBox(poi1) = true) then
      begin
        TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
        end else begin
        px:=0;
        py:=(px-v.x)*(tempt.y-v.y)/(tempt.x-v.x)+v.y;
        pz:=(px-v.x)*(tempt.z-v.z)/(tempt.x-v.x)+v.z;
        poi1.SetCoord(px,py,pz);
        TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
      end;
    end;
    TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end;
  //////////////////////////////  2
  if (tempt = m_lstVertex[2])  then
  begin
    pz:=0;
    px:= (pz-v.z)*(tempt.x-v.x)/(tempt.z-v.z)+v.x;
    py:= (pz-v.z)*(tempt.y-v.y)/(tempt.z-v.z)+v.y;
    poi1:= TGBPoint3D.Create(px,py,pz, 0);
    if (IsInsideBoundingBox(poi1) = true) then
    begin
      TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
    end
    else
    begin
        py:=0;
        px:=(py-v.y)*(tempt.x-v.x)/(tempt.y-v.y)+v.x;
        pz:=(py-v.y)*(tempt.z-v.z)/(tempt.y-v.y)+v.z;
        poi1.SetCoord(px,py,pz);
        if (IsInsideBoundingBox(poi1) = true) then
      begin
        TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
      end
      else
      begin
        px:=1;
        py:=(px-v.x)*(tempt.y-v.y)/(tempt.x-v.x)+v.y;
        pz:=(px-v.x)*(tempt.z-v.z)/(tempt.x-v.x)+v.z;
        poi1.SetCoord(px,py,pz);
        TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
      end;
    end;
    TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end;
  ////////////////////////////////////////////////////    3

  if (tempt = m_lstVertex[3])  then
  begin
    pz:=1;
    px:= (pz-v.z)*(tempt.x-v.x)/(tempt.z-v.z)+v.x;
    py:= (pz-v.z)*(tempt.y-v.y)/(tempt.z-v.z)+v.y;
    poi1:= TGBPoint3D.Create(px,py,pz, 0);
    if (IsInsideBoundingBox(poi1) = true) then
    begin
      TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
      end else
      begin
      py:=0;
      px:=(py-v.y)*(tempt.x-v.x)/(tempt.y-v.y)+v.x;
      pz:=(py-v.y)*(tempt.z-v.z)/(tempt.y-v.y)+v.z;
      poi1.SetCoord(px,py,pz);
      if (IsInsideBoundingBox(poi1) = true) then
      begin
      TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
      end else begin
      px:=0;
      py:=(px-v.x)*(tempt.y-v.y)/(tempt.x-v.x)+v.y;
      pz:=(px-v.x)*(tempt.z-v.z)/(tempt.x-v.x)+v.z;
      poi1.SetCoord(px,py,pz);
      TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
    end;
  end;
  TDrawTools.DrawNode(poi1, 0, 0, 1.0, 1);
  end;

  ////////////////////////////
  TDrawTools.DrawLine(v, tempt, 0.7, 0.3, 0, 4);
  end;
  end;

  if (bLink = true) then
  begin
    for i := 0 to (lstTetra.Count - 1) do
    begin
      te := lstTetra[i];
      te.SetIndex(v);
      //-- draw the exterior face
      TDrawTools.DrawTriangleFace(te.GetProt(1), te.GetProt(2), te.GetProt(3),
      v, 0.4, 0.4, 0.4, 0.7);
      TDrawTools.DrawTriangleFrame(te.GetProt(1), te.GetProt(2), te.GetProt(3),
      0.0, 0.0, 0.5, 1);
      end;
      end;
    end;
  end;
}

{ TTetraMesh_IO }

constructor TTetraMesh_IO.Create(mesh: TTetraMesh);
begin
  m_mesh := mesh;
end;

// ******************************************************************************
// Description: Read the points stored in a text file (*.pts)
// Input:       (the TextFile, by reference)
// Output:      the points are read and added to the current mesh.
// Format of the *.pts file:
// first line: 0 (no attributes) OR 1 (attributes)
// second line: number of points in the file
// other lines: each line is a point (with or w/o attributes)
// ******************************************************************************
procedure TTetraMesh_IO.ReadPoints(var f: TextFile);
var
  att: integer;
  nb: integer;
  i, j: integer;
  coord: array [0 .. 3] of double;
  temp: double;
  times: array of double;
  startTime, endTime: double;
begin
  Read(f, att);
  Read(f, nb);
  { Form1.ProgressBar1.Min := 0;
    Form1.ProgressBar1.Max := nb;
    Form1.ProgressBar1.Step := 1;
    Form1.ProgressBar1.Position := 0;
  }
  // SetLength(times, Floor(nb / 500));
  // startTime := Form1.StampTime;
  if (att = 0) then // -- file without attributes
  begin
    for i := 1 to nb do
    begin
      for j := 0 to 2 do
      begin
        Read(f, temp);
        coord[j] := temp;
      end;
      m_mesh.InsertPoint(coord[0], coord[1], coord[2]);
      // Form1.ProgressBar1.StepIt;
    end;
  end
  else // -- file with attributes
  begin
    for i := 1 to nb do
    begin
      for j := 0 to 3 do
      begin
        Read(f, temp);
        coord[j] := temp;
      end;
      m_mesh.InsertPoint(coord[0], coord[1], coord[2], coord[3]);
      // Form1.ProgressBar1.StepIt;
      if (i mod 500 = 0) then
      begin
        // endTime := Form1.StampTime;
        // times[Floor(i/500) - 1] := (endTime - startTime);
      end;
    end;
  end;
  self.SaveCurve(times, nb);
  // Form1.ProgressBar1.Position := 0;
end;

procedure TTetraMesh_IO.SaveCurve(times: array of double; nb: integer);
var
  f1: TextFile;
  i: integer;
  FileDir: TFileName;

begin
  FileDir := ExtractFilePath(ParamStr(0));
  AssignFile(f1, FileDir + 'curve.txt');
  Rewrite(f1);
  try
    Write(f1, FloatToStrF(0, fffixed, 5, 0) + #9);
    WriteLn(f1, FloatToStrF(0, fffixed, 10, 3));
    for i := 0 to (Floor(nb / 500) - 1) do
    begin
      Write(f1, FloatToStrF((i * 500) + 500, fffixed, 5, 0) + #9);
      WriteLn(f1, FloatToStrF(times[i], fffixed, 10, 3));
    end;
  finally
    CloseFile(f1);
  end;
end;

procedure TTetraMesh_IO.SavePoints(var f: TextFile);
var
  i: integer;
  pt: TGBPoint3D;
  lstPts: TList;
begin
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
end;

procedure TTetraMesh_IO.SaveTopology(var f: TextFile);
var
  i: integer;
  j: integer;
  t: TGBTetrahedron;
  adj: TGBTetrahedron;
  pt: TGBPoint3D;
  lstTetra: TList;
begin
  lstTetra := m_mesh.GetListTetra;
  for i := 0 to (lstTetra.Count - 1) do
  begin
    t := lstTetra[i];
    Write(f, IntToStr(t.m_no) + '   *   ');
    // -- TP
    for j := 1 to 4 do
    begin
      adj := t.GetT(j);
      if (adj = nil) then
      begin
        Write(f, '-');
      end
      else
      begin
        Write(f, IntToStr(adj.m_no));
      end;
      Write(f, #9);
    end;
    Write(f, '*' + #9);
    // -- TT
    for j := 1 to 4 do
    begin
      pt := t.GetP(j);
      Write(f, IntToStr(pt.m_no) + #9);
    end;
    WriteLn(f, #13);
  end;
end;

// ====================== TGBCrust =================================
{ TIsosurface }
constructor TGBCrust.Create(mesh: TTetraMesh);
begin
  m_mesh := mesh;
end;

procedure TGBCrust.ExtractCrust;
var
  lstTr: TList;
  i: integer;
begin
  lstTr := TList.Create;
  self.ComputeCrust(lstTr);
  m_mesh.bDrawCrust := True;
  m_mesh.SetCrust(lstTr);

  // -- free the list of triangles
  for i := 0 to (lstTr.Count - 1) do
  begin
    TGBTriangle(lstTr[i]).Free;
  end;
  lstTr.Free;
end;

procedure TGBCrust.ComputeCrust(lstTr: TList);
var
  i: integer;
  j: integer;
  te: TGBTetrahedron;
  adj: TGBTetrahedron;
  lstPts: TList;
  lstTe: TList;
  centre: TGBPoint3D;
  centreAdj: TGBPoint3D;
  n: TGBVector;
begin
  centre := TGBPoint3D.Create;
  centreAdj := TGBPoint3D.Create;
  n := TGBVector.Create;
  lstPts := m_mesh.GetListPts;
  lstTe := m_mesh.GetListTetra;
  for i := 0 to (lstTe.Count - 1) do // -- for each tetrahedron in the mesh
  begin
    te := lstTe[i];
    // -- if the tetra has one point from the big tetra, ignore it
    if not((te.GetP(1) = lstPts[0]) or (te.GetP(1) = lstPts[1]) or
      (te.GetP(1) = lstPts[2]) or (te.GetP(1) = lstPts[3]) or
      (te.GetP(2) = lstPts[0]) or (te.GetP(2) = lstPts[1]) or
      (te.GetP(2) = lstPts[2]) or (te.GetP(2) = lstPts[3]) or
      (te.GetP(3) = lstPts[0]) or (te.GetP(3) = lstPts[1]) or
      (te.GetP(3) = lstPts[2]) or (te.GetP(3) = lstPts[3]) or
      (te.GetP(4) = lstPts[0]) or (te.GetP(4) = lstPts[1]) or
      (te.GetP(4) = lstPts[2]) or (te.GetP(4) = lstPts[3])) then
    begin
      te.GetCircumCentre(centre);
      // -- test the 4 faces of the triangle
      for j := 1 to 4 do
      begin
        adj := te.SetIndex(j).GetTrot(0);
        adj.GetCircumCentre(centreAdj);
        // -- if the voronoi vertex of the adjacent tetra is not inside the
        // -- sphere formed by the face and the centre of the tetra, then the
        // -- face is part of the crust.
        if not(TGBGeomTools.InSphere(te.GetProt(1), te.GetProt(2),
          te.GetProt(3), centre, centreAdj)) then
        begin
          TGBGeomTools.NormalOppositePt(te.GetProt(1), te.GetProt(2),
            te.GetProt(3), lstPts[0], n);
          lstTr.Add(TGBTriangle.Create(te.GetProt(1), te.GetProt(2),
            te.GetProt(3), n));
        end;
      end;
    end;
  end;
  centre.Free;
  centreAdj.Free;
  n.Free;
end;

end.
