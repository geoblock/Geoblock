//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! DrawVor unit for drawing 3D tetrahedraliation and voronoi diagram }


unit uDrawVor;

interface

uses
  System.SysUtils,
  Vcl.Dialogs,

  GLVectorTypes,
  GLObjects,
  GLGeomObjects,
  GLCoordinates,
  uObjects3D,
  uTetraMesh,
  uIOPoly,
  fMapScenery,
  gnuGettext;

procedure InitDraw;
procedure DestructDraw;

procedure LoadDrawVorFromFile(FsName: TFileName);
procedure LoadDrawDelaunayFromFileAndVor(Fnn: TFileName);
procedure LoadGlobNodesFromVor;
procedure CreateDrawVorNode;
procedure CreateDrawVorFaces;
function CreateMaterialMap: boolean;
{
  Function for Creating Delaunay draw
  May be used only after Loading Nodes array and Tetras array
  result: isDelaunayCreated = true
  and DelaunayLines array created
}
procedure CreatedrawDelaunay;
procedure TestLine(st: string);
procedure DrawVorAsLine;

procedure CalcDelaunayVorFromDB(VorFile: TFileName; NX, NY, NZ, NV: array of single);
procedure CreateBigTetra;

procedure ActionVisibleNode;
procedure ActionVisibleVoronoi;
procedure ActionVisibleMaterial;
procedure ActionVisibleDelaunay;
procedure ActionVisibleVorAsLine;

procedure ActionLoadDrawVorFromFile;
procedure ActionLoadDrawDelaunayFromFile;
procedure ActionLoadDrawDelaunayFromFileNodes;

procedure ActionMapSceneryShowCalculated;


// GLScene's instances
var
  NodeCube: TGLDummyCube;
  VorCube:  array of TGLDummyCube;
  VorFaces: array of TGLPolygon;
  VorLines: array of TGLLines;
  VorCn:  integer;
  dNodes:   array of TGLSphere;
  DelaunayLines: array of TGLLines;

  MaterialMap: array of integer;
  CurrMatN: integer;  // the current material number
  Nrad:   double;

  isPolCreated: boolean = False;
  isDelaunayCreated: boolean = False;
  isCalculated: boolean = False;
  isGlobalNodesCreated: boolean = False;
  isGlobalTetrasCreated: boolean = False;
  toDrawNodes: boolean;
  toDrawVoronoi: boolean;
  toDrawVorasLine: boolean;
  toDrawDelaunay: boolean;

//=====================================================================
implementation
//=====================================================================

procedure ActionMapSceneryShowCalculated;
begin
  if fmMapScenery = nil then
  begin
    fmMapScenery := TfmMapScenery.Create(nil);
    initDraw;
    fmMapScenery.VorPanel.Visible := True;
    CreateDrawVorNode;
    if CreateMaterialMap then
      CreateDrawVorFaces
    else
      ShowMessage(_('Not enough textures!'));
    CreateDrawDelaunay;
  end;
  fmMapScenery.Show;

end;

procedure ActionVisibleNode;
begin
  NodeCube.Visible := not toDrawNodes;
  toDrawNodes      := NodeCube.Visible;
end;

procedure ActionVisibleDelaunay;
begin
  fmMapScenery.DCDelaunay.Visible := not toDrawDelaunay;
  toDrawDelaunay := fmMapScenery.DCDelaunay.Visible;
end;

procedure ActionVisibleVoronoi;
begin
  fmMapScenery.DCVoronoi.Visible := not toDrawVoronoi;
  toDrawVoronoi := fmMapScenery.DCVoronoi.Visible;
end;

procedure ActionVisibleVorAsLine;
begin
  toDrawVorasLine := not toDrawVorasLine;
  DrawVorAsLine;
end;

procedure ActionVisibleMaterial;
var
  i, si, mi, tm: integer;
begin
  si := fmMapScenery.CheckListBox2.ItemIndex;
  mi := si + 1;
  for i := 0 to VorCn - 1 do
  begin
    tm := MaterialMap[i];
    if tm = mi then
    begin
      VorCube[i].Visible := fmMapScenery.CheckListBox2.Checked[si];
      dNodes[i].Visible  := fmMapScenery.CheckListBox2.Checked[si];
      fmMapScenery.CheckListBox1.Checked[i] := fmMapScenery.CheckListBox2.Checked[si];
    end;
  end;
end;

/////////////////ACTIONS/////////////////////
procedure ActionLoadDrawVorFromFile;
var
  fn: string;
begin
  fmMapScenery.OpenDialog.Execute;
  fn := fmMapScenery.OpenDialog.FileName;
  if fn <> '' then
    LoadDrawVorFromFile(FN);
end;

procedure ActionLoadDrawDelaunayFromFileNodes;
var
  fnn, fnd: string;
begin
  fmMapScenery.OpenDialog.Execute;
  fnn := fmMapScenery.OpenDialog.FileName;
  if fnn <> '' then
  begin
    if ReadNodes(fnn) then
    begin
      isGlobalNodesCreated := True;
      fnd := ChangeFileExt(fnn, '.ele');
      isGlobalTetrasCreated := LoadDelaunayFromFile(fnd);
      CreatedrawDelaunay;
      isDelaunayCreated := True;
    end;
  end;
end;

procedure ActionLoadDrawDelaunayFromFile;
var
  fnn: string;
begin
  fmMapScenery.OpenDialog.Execute;
  fnn := fmMapScenery.OpenDialog.FileName;
  if fnn <> '' then
  begin
    LoadGlobNodesFromVor;
    isGlobalTetrasCreated := LoadDelaunayFromFile(fnn);
    CreatedrawDelaunay;
    isDelaunayCreated := True;
  end;
end;
//////////////END ACTIONS////////////////////

procedure LoadDrawDelaunayFromFileAndVor(Fnn: TFileName);
begin
  LoadGlobNodesFromVor;
  isGlobalTetrasCreated := LoadDelaunayFromFile(fnn);
  CreatedrawDelaunay;
  isDelaunayCreated := True;
end;


procedure testLine(st: string);
begin
  fmMapScenery.Caption := st;
end;



procedure InitDraw;
begin
  // isPolCreated:=false;
  // isDelaunayCreated:=false;
  // isGlobalNodesCreated:=false;
  // isGlobalTetrasCreated:=false;
  toDrawNodes := True;
  toDrawVoronoi := True;
  toDrawDelaunay := False;
  toDrawVorAsLine := False;
  nrad := 0.01;
end;

procedure DestructDraw;
begin
  if isPolCreated then
  begin
    MyPol.Destroy;
    SetLength(dNodes, 0);
    SetLength(MaterialMap, 0);
    SetLength(VorCube, 0);
    SetLength(VorLines, 0);
    isPolCreated := False;
  end;
  if isDelaunayCreated then
  begin
    SetLength(DelaunayLines, 0);
    SetLength(GlobalNodes, 0);
    SetLength(GlobalTetras, 0);
    isDelaunayCreated := False;
  end;
  if isCalculated then
  begin
    m_mesh.Destroy;
    isCalculated := False;
  end;
end;

 ///////////////////////////////
 // Function for Loading NodeArray from MyPol Vertises
 // may be used only if MyPol is created
 // it used in Delaunay triangulation
 // Nodes array must loaded before drawing Delaunay triangulation
 ///////////////////////////////
procedure LoadGlobNodesFromVor;
var
  i, ci: integer;
begin
  ci := MyPol.Polyheders.CurrVert;
  SetLength(GlobalNodes, ci);
  for i := 0 to ci - 1 do
    globalnodes[i] := MyPol.Polyheders.Vert[i];
  isGlobalNodesCreated := True;
end;

procedure DrawVorAsLine;
var
  i, ci: integer;
begin
  ci := Length(VorFaces);
  if toDrawVorAsLine then
    for I := 0 to ci - 1 do
    begin
      VorFaces[i].Material.PolygonMode  :=
        fmMapScenery.GLPolygon1.Material.PolygonMode;
    end
  else
    for I := 0 to ci - 1 do
    begin
      VorFaces[i].Material.PolygonMode  :=
        fmMapScenery.GLPolygon1.Material.PolygonMode;
    end;
end;

procedure CreateDrawDelaunay;
var
  i, j, k, tcount: integer;
  temp, tmpx, tmpy, tmpz: double;
  //EgePt: array[0..3]
begin
  if isGlobalNodesCreated and isGlobalTetrasCreated then
  begin
    tcount := Length(GlobalTetras);
    SetLength(DelaunayLines, tcount);
    fmMapScenery.DCDelaunay.Visible := toDrawDelaunay;
    for I := 0 to tcount - 1 do
    begin
      DelaunayLines[i] := TGLLines.CreateAsChild(fmMapScenery.DCDelaunay);
      DelaunayLines[i].NodesAspect := lnaInvisible;
      DelaunayLines[i].LineWidth := 2;
      tmpx := GlobalTetras[i][0].x;
      tmpy := GlobalTetras[i][0].y;
      tmpz := GlobalTetras[i][0].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][1].x;
      tmpy := GlobalTetras[i][1].y;
      tmpz := GlobalTetras[i][1].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][2].x;
      tmpy := GlobalTetras[i][2].y;
      tmpz := GlobalTetras[i][2].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][0].x;
      tmpy := GlobalTetras[i][0].y;
      tmpz := GlobalTetras[i][0].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][3].x;
      tmpy := GlobalTetras[i][3].y;
      tmpz := GlobalTetras[i][3].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][1].x;
      tmpy := GlobalTetras[i][1].y;
      tmpz := GlobalTetras[i][1].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][2].x;
      tmpy := GlobalTetras[i][2].y;
      tmpz := GlobalTetras[i][2].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      tmpx := GlobalTetras[i][3].x;
      tmpy := GlobalTetras[i][3].y;
      tmpz := GlobalTetras[i][3].z;
      DelaunayLines[i].AddNode(tmpx, tmpy, tmpz);
      DelaunayLines[i].Visible := True;
    end;
    isDelaunayCreated := True;
  end
  else
    isDelaunayCreated := False;
end;

procedure LoadDrawVorFromFile(Fsname: TFileName);
var
  DelaunayFile: TFileName;
begin
  if isPolCreated then
  begin
    if isCalculated then
    begin
      ///////////////////
    end;
  end
  else
  begin
    MyPol := TGBPolyhedron.Create;
    LoadVoronoiFromFile(fsname);
    VorCn := MyPol.Polyheders.CurrVert;
    isPolCreated := True;
    CreateDrawVorNode;
    DelaunayFile := ChangeFileExt(fsname, '.ele');
    if FileExists(DelaunayFile) then
      LoadDrawDelaunayFromFileAndVor(DelaunayFile);
    //Set material
    if CreateMaterialMap then
      CreateDrawVorFaces
    else
      ShowMessage(_('Not enough textures!'));
  end;
end;

procedure CreateDrawVorNodeFNodes;
var
  i: integer;
begin
  SetLength(dNodes, VorCn);
  NodeCube := TGLDummyCube.CreateAsChild(fmMapScenery.DCPoints);
  NodeCube.Position.X := 0;
  NodeCube.Position.Y := 0;
  NodeCube.Position.Z := 0;
  //NodeCube.Name:='NodeCube1';
  NodeCube.Visible := toDrawNodes;
  for i := 0 to VorCn - 1 do
  begin
    dNodes[i] := TGLSphere.CreateAsChild(NodeCube);
    dNodes[i].Position.X := Globalnodes[i].X;
    dNodes[i].Position.Y := Globalnodes[i].Y;
    dNodes[i].Position.Z := Globalnodes[i].Z;
    dNodes[i].Radius := Nrad;
    dNodes[i].Material := fmMapScenery.GLMaterialLibrary.Materials.Items[0].Material;
    dNodes[i].Visible := True;
    fmMapScenery.CheckListBox1.Items.Add(IntToStr(i + 1));
  end;

end;


//=================================

procedure CalcDelaunayVorFromDB(VorFile: TFileName; NX, NY, NZ, NV: array of single);
var
  I, nb: integer;
begin
  CreateBigTetra;
  nb := Length(NX);

  for I := 0 to nb - 1 do
    M_mesh.InserTPoint(Nx[i], Ny[i], Nz[i], Nv[i]);
  M_mesh.CreateALLVorCell;
  VorCn := MyPol.Polyheders.CurrVert;
  isCalculated := True;
  LoadGlobNodesFromVor;
  SaveDelaunay(VorFile + '.ele');
  SaveVoronoiToFile(VorFile + '.vor');
  isGlobalTetrasCreated := LoadDelaunayFromFile(VorFile + '.ele');
  ActionMapSceneryShowCalculated;
  //isPolCreated:=true;
end;

procedure CreateBigTetra;
var
  pt0: TGBPoint3D;
  pt1: TGBPoint3D;
  pt2: TGBPoint3D;
  pt3: TGBPoint3D;
  bb0: TGBPoint3D;
  bb1: TGBPoint3D;
begin
  //-- points for the big tetra
  pt0 := TGBPoint3D.Create(-101, -102, -101);
  pt1 := TGBPoint3D.Create(-99, 1004, -106);
  pt2 := TGBPoint3D.Create(1002, -98, -103);
  pt3 := TGBPoint3D.Create(-104, -97, 1003);

  //-- points for the bounding box (for the insertion)
  bb0    := TGBPoint3D.Create(0, 0, 0);
  bb1    := TGBPoint3D.Create(1, 1, 1);
  m_mesh := TTetraMesh.Create(pt0, pt1, pt2, pt3, bb0, bb1);
  /////////////////////
  MyPol  := TGBPolyhedron.Create;
  isPolCreated := True;
  ///  //////////////////
  pt0.Free;
  pt1.Free;
  pt2.Free;
  pt3.Free;
  bb0.Free;
  bb1.Free;
end;


procedure CreateDrawVorNode;
var
  i: integer;
begin
  SetLength(dNodes, VorCn);
  NodeCube := TGLDummyCube.CreateAsChild(fmMapScenery.DCPoints);
  NodeCube.Position.X := 0;
  NodeCube.Position.y := 0;
  NodeCube.Position.z := 0;
  //NodeCube.Name:='NodeCube1';
  NodeCube.Visible := toDrawNodes;
  for I := 0 to VorCn - 1 do
  begin
    dNodes[i] := TGLSphere.CreateAsChild(NodeCube);
    dNodes[i].Position.X := MyPol.Polyheders.Vert[i].X;
    dNodes[i].Position.Y := MyPol.Polyheders.Vert[i].Y;
    dNodes[i].Position.Z := MyPol.Polyheders.Vert[i].Z;
    dNodes[i].Radius := Nrad;
    dNodes[i].Material :=
      fmMapScenery.GLMaterialLibrary.Materials.Items[0].Material;
    dNodes[i].Visible := True;
    fmMapScenery.CheckListBox1.Items.Add(IntToStr(i + 1));
  end;
end;

procedure CreateDrawVorFaces;
var
  i, j, k, FC, PC, tn, np, currn: integer;
  px, py, pz: double;
  mi: integer;
begin
  mi    := 0;
  currn := 0;
  //FC:=MyPol.Polyheders.FacesCount;
  SetLength(VorCube, VorCn);
  for i := 0 to VorCn - 1 do
  begin
    fmMapScenery.CheckListBox1.Checked[i] := True;
    //Create polyhedr cube
    VorCube[i] := TGLDummyCube.CreateAsChild(fmMapScenery.DCVoronoi);
    VorCube[i].Visible := toDrawVoronoi;
    FC := length(MyPol.Polyheders.VoronoiPoly[i]);
    //0 to count of faces in i-ui polyhedr
    //if (mi=0)or(mi=1) then inc(mi) else mi:=0;
    for j := 0 to FC - 1 do
    begin
      SetLength(VorFaces, currn + 1);
      ///////// Lines
      SetLength(VorLines, currn + 1);
      VorFaces[currn] := TGLPolygon.CreateAsChild(VorCube[i]);
      ///////// Lines
      VorLines[currn] := TGLLines.CreateAsChild(VorCube[i]);
      //Set material
      mi := MaterialMap[i];
      VorFaces[currn].Material :=
        fmMapScenery.GLMaterialLibrary.Materials.Items[mi].Material;
      VorLines[currn].NodesAspect := lnaInvisible;
      VorLines[currn].LineWidth := 2;
      VorLines[currn].Visible := True;
      VorFaces[currn].Visible := True;
      //Getting num of face
      tn := MyPol.Polyheders.VoronoiPoly[i][j];
      for k := 0 to length(MyPol.Polyheders.Faces[tn]) - 1 do
      begin
        np := MyPol.Polyheders.Faces[tn][k];
        px := MyPol.Polyheders.CellPoint[np].x;
        py := MyPol.Polyheders.CellPoint[np].y;
        pz := MyPol.Polyheders.CellPoint[np].z;
        VorFaces[currn].AddNode(px, py, pz);
        VorLines[currn].AddNode(px, py, pz);
      end;
      np := MyPol.Polyheders.Faces[tn][0];
      px := MyPol.Polyheders.CellPoint[np].x;
      py := MyPol.Polyheders.CellPoint[np].y;
      pz := MyPol.Polyheders.CellPoint[np].z;
      VorLines[currn].AddNode(px, py, pz);
      Inc(currn);
    end;
  end;
end;

function FindMaterial(zn: integer): integer;
var
  i:     integer;
  Mdata: double;
begin
  Mdata := MyPol.Polyheders.Vert[zn].Data;
  for i := 0 to zn - 1 do
  begin
    if MyPol.Polyheders.Vert[i].Data = Mdata then
    begin
      Result := MaterialMap[i];
      Exit;
    end;
  end;
  Inc(currmatn);
  Result := CurrMatn;
end;


function CreateMaterialMap: boolean;
var
  i, j, mv: integer;
begin
  currmatn := 1;
  SetLength(MaterialMap, VorCn);
  MaterialMap[0] := currmatn;
  for I := 1 to Vorcn - 1 do
    MaterialMap[i] := findmaterial(i);
  // mv:=maxintvalue(MaterialMap);
  fmMapScenery.CheckListBox2.Clear;
  if currmatn > fmMapScenery.GLMaterialLibrary.Materials.Count then
    Result := False
  else
  begin
    for I := 1 to currmatn do
      fmMapScenery.CheckListBox2.items.Add(IntToStr(i));
    for I := 0 to currmatn - 1 do
      fmMapScenery.CheckListBox2.Checked[i] := True;
    Result := True;
  end;
end;


end.
