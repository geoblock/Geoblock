//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
(*  The Terrascene unit to view geolobjects in VR *)

unit fTerraScene;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.ImageList,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Imaging.JPeg,
  Vcl.Dialogs, 
  Vcl.StdCtrls, 
  Vcl.ComCtrls, 
  Vcl.ToolWin, 
  Vcl.ExtDlgs,
  Vcl.ImgList, 
  Vcl.ExtCtrls, 
  Vcl.Menus,
  //DB
  Bde.DBTables,
  
  GLS.Texture,
  GLS.Canvas,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Cadencer,
  GLS.Objects,
  GLS.Scene,
  GLS.Graph,
  GLS.Skydome,
  GLS.SceneViewer,
  GLS.FileMD3,
  GLS.FileQ3MD3,
  GLS.Mesh,
  GLS.BitmapFont,
  GLS.FileObj,
  Ode.Import,
  GLS.WindowsFont,
  Physics.ODEManager, {old GLOXOde,}
  GLS.SoundManager,
  Sounds.BASS,
  GLS.TexLensFlare,
  GLS.LensFlare,
  GLS.GeomObjects, {oxOdeGl,}
  GLS.MeshUtils,
  GLS.VectorTypes,
  GLS.RenderContextInfo,
  GLS.Material,
  GLS.Keyboard,
  GLS.PersistentClasses,
  GLS.VectorFileObjects,
  GLS.AVIRecorder,
  GLS.GameMenu,
  GLS.Coordinates,
  GLS.BaseClasses,

  //GB
  fInitialForm,
  fDrawFillStyle,
  uTerraModel,
  uTerraLayers,
  uTerraBalloon;


type
  TGeoSceneTextureDialog = class(TfmDrawFillStyle)
    aLabel: TLabel;
    MaterialLibrary: TGlMaterialLibrary;
    constructor CreateDialog(AOwner: TComponent; MatLib: TGlMaterialLibrary);
    procedure OnSelectNewTexture(Sender: TObject);
  end;

  TfmGeoScene = class(TfmInitialForm)
    StatusBar1: TStatusBar;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    SkyBox:  TGLSkyBox;
    Light:   TGLLightSource;
    //OdeEngine1: TGLOXOdeEngine;
    SurfaceCube: TGLDummyCube;
    // Old:    StaMesh: TGLOXStaMesh;
    GLDirectOpenGL1: TGLDirectOpenGL;
    LensFlare: TGLLensFlare;
    Cam:     TGLCamera;
    GLCadencer: TGLCadencer;
    MainMenu1: TMainMenu;
    File1:   TMenuItem;
    OpenMeshModel: TMenuItem;
    N1:      TMenuItem;
    CloseGeoScene: TMenuItem;
    View1:   TMenuItem;
    N3:      TMenuItem;
    ShowMapBut: TMenuItem;
    FreeCameraButton: TMenuItem;
    N4:      TMenuItem;
    N6:      TMenuItem;
    N2:      TMenuItem;
    Car1:    TMenuItem;
    btnHighCam: TMenuItem;
    btnMediumCam: TMenuItem;
    btnLowCam: TMenuItem;
    btnFirstPersonCam: TMenuItem;
    WinBmpFont: TGLWindowsBitmapFont;
    GLSoundLibrary: TGLSoundLibrary;
    GLSMBASS: TGLSMBASS;
    MLSkyBox: TGLMaterialLibrary;
    Timer1:  TTimer;
    TreeMatLib: TGLMaterialLibrary;
    MeshMatLib: TGLMaterialLibrary;
    CarMatLib: TGLMaterialLibrary;
    MapMatLib: TGLMaterialLibrary;
    MainMenu: TGLGameMenu;
    GSInterface: TGLDummyCube;
    MenuFont: TGLWindowsBitmapFont;
    Style1:  TMenuItem;
    Setecttexture1: TMenuItem;
    Edittexturesmode1: TMenuItem;
    ApplyTexturesForMesh1: TMenuItem;
    Layers1: TMenuItem;
    Save1:   TMenuItem;
    Load1:   TMenuItem;
    GeoSceneTreeView: TTreeView;
    ObjectsViewer: TMenuItem;
    TreeViewImageList: TImageList;
    OpenDialog: TOpenDialog;
    Logo:    TImage;
    New:     TMenuItem;
    CompLayer: TMenuItem;
    Reset:   TMenuItem;
    procedure GeoSceneTreeViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GeoSceneTreeViewDragOver(Sender, Source: TObject;
      X, Y: integer; State: TDragState; var Accept: boolean);
    procedure GeoSceneTreeViewDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure ResetClick(Sender: TObject);
    procedure GeoSceneTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure CompLayerClick(Sender: TObject);
    procedure ObjectsViewerClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplyTexturesForMesh1Click(Sender: TObject);
    procedure Edittexturesmode1Click(Sender: TObject);
    procedure Setecttexture1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure btnLowCamClick(Sender: TObject);
    procedure FreeCameraButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure OdeEngine1MultiStepRender(delta: single);
    procedure ShowMapButClick(Sender: TObject);
    procedure GLSceneViewerPostRender(Sender: TObject);
    procedure OdeEngine1StepRender(delta: single);
    procedure N6Click(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure CloseGeoSceneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure HandleKeys(const deltaTime: double);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure LogoClick(Sender: TObject);
    procedure NewClick(Sender: TObject);
    procedure GeoSceneTreeViewClick(Sender: TObject);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    mx, my: integer;
    procedure InitWorld;
    procedure GeoSceneMainMenu;
    procedure ApplyNewTexturesForMesh;
    procedure ShowObjectsInTree;
    procedure FindAndSetTriangles(X, Y, Mode: integer);
    //if Mode 1 - selection, 2 - unselection, 0 - both
    procedure CloseObjectBrowserButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
  end;

procedure FreeMemory(theObject: TObject);

var
  fmGeoScene: TfmGeoScene;
  TriMesh_Height, Mx, My, Mx2, My2, model_w, model_h: integer;
  bmpScale:   TBitmap;
  CurrentModelName: TFileName;
  x_prew, y_prew, x, y: single;
  BeginTime:  TTime;
  MainMenuIsActive: boolean = True;

  SelectedTrianglesCount: integer = 0;
  EditTexturesMode: boolean = False;

  CurrentTexture: integer = 1;
  cX, cY: integer;

  LayersList:    TGeoSceneLayerList;
  MouseDownMode: boolean;

  // OBJECT TREE \\
  MainNode, LayersNode, TexturesNode, CarsNode, TreesNode: TTreeNode;
  LastHintedNode: TTreeNode;
  CloseObjectBrowserButton: TButton;

//======================================================================
implementation
//======================================================================

uses
  fGeoblock,
  fTerraSplash,
  uCommon,
  gnuGettext,
  cGlobals,
  cResStrings,
  uTerraSound,
  uTerraLoader,
  uTerraObjects;

{$R *.dfm}

const
  cTurnSpeed = 300;
  cMoveSpeed = 9;

procedure TfmGeoScene.FormCreate(Sender: TObject);
var
  OldCursor: TCursor;
  bmp, bmp2: TBitmap;
  i, j:      integer;
  maxSize:   integer;
begin
  inherited;

  //Logo.Picture.LoadFromFile('C:\Geoblock\Data\Base\GeoScene\Logo\GLScene.bmp');
  Logo.Left := (ClientWidth - Logo.Width) div 2;
  Logo.Top  := (ClientHeight - Logo.Height) div 2;

  MouseDownMode := False;

  OldCursor := Screen.Cursor;

  Screen.Cursor := crHourGlass;

  bmpScale := TBitmap.Create;
  bmpScale.Width := MAP_SIZE;
  bmpScale.Height := MAP_SIZE;

  GlSceneViewer.Align := AlClient;
  InitWorld;

  MapMatLib.AddTextureMaterial('map', bmpScale);

  GeoSceneMainMenu;
  LayersList   := TGeoSceneLayerList.Create; // List to store layers
(*
  mainNode     := GeoSceneTreeView.Items.Add(nil, Tri.TheTableName);
*)
  // Nodes for object tree
  layersNode   := GeoSceneTreeView.Items.AddChild(mainNode, _('Layers'));
  texturesNode := GeoSceneTreeView.Items.AddChild(mainNode, _('Textures'));
  carsNode     := GeoSceneTreeView.Items.AddChild(mainNode, _('Cars'));
  treesNode    := GeoSceneTreeView.Items.AddChild(mainNode, _('Trees'));

  LastHintedNode := mainNode;

  CreateToolTips(Handle); // Support of hints

  CloseObjectBrowserButton := TButton.Create(nil); // Button to close object browser
  with CloseObjectBrowserButton do
  begin
    Name    := 'CloseObjectBrowserButton';
    Caption := 'X';
    Parent  := GeoSceneTreeView;
    Visible := True;
    Width   := 23;
    Height  := 23;
    Top     := 0;
    Left    := GeoSceneTreeView.Width - CloseObjectBrowserButton.Width div 2;
    OnClick := ObjectsViewerClick;
    OnMouseMove := CloseObjectBrowserButtonMouseMove;
  end;

  if Assigned(Car) then
  begin
    x := Car.Position.X; // ox -Frame
    y := Car.Position.Z;
    //Position of camera with auto
    Cam.Position.Y  := Car.Position.Y + 10;
    Cam.Position.X  := Car.Position.Y + 20;
  end
  else
  begin
    x := 0;
    y := 0;
    Cam.Position.Y  := 0;
    Cam.Position.X  := 0;
  end;

  //depth of view
  Cam.DepthOfView := 2000;

  GLCadencer.Enabled := True;
  BeginTime      := Time;
  Timer1.Enabled := True;
  Screen.Cursor  := OldCursor;

(*
  for i := 0 to Tri.TriCount - 1 do
    Tri.TrianglesArray[i].Selected := True;
  LayersList.Add(Tri, _('Default'));
  for i := 0 to Tri.TriCount - 1 do
    Tri.TrianglesArray[i].Selected := False;
*)
end;


procedure TfmGeoScene.ApplyNewTexturesForMesh;
var
  i: integer;
  s: string;
begin
  if (SelectedTrianglesCount <> 0) then
  begin
    i := 0;
    while (i < MeshTris.Count) do
    begin
      if Tri.TrianglesArray[Round(i / 3)].Selected then
      begin
///        StaMesh.MeshObjects[0].FaceGroups[round(i / 3)].Prepare;
///        StaMesh.MeshObjects[0].FaceGroups[round(i / 3)].MaterialName :=
///               'm' + IntToStr(CurrentTexture);
      end;
    end;
    i := i + 3;
  end;
  /// ox - StaMesh.StructureChanged;

  for i := 0 to Tri.TriCount - 1 do
  begin
    Tri.TrianglesArray[round(i / 3)].Selected := False;
  end;
end;

procedure TfmGeoScene.GeoSceneMainMenu;
begin
  MainMenuIsActive := True;
  with mainMenu do
  begin
    Selected := 0;
    Visible  := True;
  end;
end;

procedure TfmGeoScene.GeoSceneTreeViewClick(Sender: TObject);
var
  LNo: integer;
  layerONindex, layerOFFindex: integer;
  SelItemNo: integer;
begin
  inherited;
  layerONindex  := 18;
  layerOFFindex := 17;

  if Assigned(GeoSceneTreeView.Selected.Parent) then
  begin
    if (GeoSceneTreeView.Selected.Parent.ItemId = layersNode.ItemId) then
    begin
      SelItemNo := GeoSceneTreeView.Selected.Index;
      LNo := layersList.GetLayerNoByName(GeoSceneTreeView.Selected.Text);
      if (GeoSceneTreeView.Selected.ImageIndex = layerONindex) then
      begin
        layersList.Layers[LNo].Enabled := False;
        GeoSceneTreeView.Selected.ImageIndex := layerOFFindex;

                {GeoSceneTreeView.Items.BeginUpdate;
                GeoSceneTreeView.Items[SelItemNo].Selected := false;
                GeoSceneTreeView.Items[SelItemNo].Selected := true;
                GeoSceneTreeView.Items.EndUpdate;   }
      end
      else if (GeoSceneTreeView.Selected.ImageIndex = layerOFFindex) then
      begin
        layersList.Layers[LNo].Enabled := True;
        GeoSceneTreeView.Selected.ImageIndex := layerONindex;

                {GeoSceneTreeView.Items.BeginUpdate;
                GeoSceneTreeView.Items[SelItemNo].Selected := false;
                GeoSceneTreeView.Items[SelItemNo].Selected := true;
                GeoSceneTreeView.Items.EndUpdate; }
      end;
      // ox -layersList.ApplyAllLayers(StaMesh, MeshMatLib, Tri.Materials);
    end;
  end;
end;

procedure TfmGeoScene.GeoSceneTreeViewDragDrop(Sender, Source: TObject; X, Y: integer);
var
  DestNode, SourceNode: TTreeNode;
begin
  inherited;
  if (Assigned(GeoSceneTreeView.GetNodeAt(X, Y)) and
    (Assigned(GeoSceneTreeView.Selected))) then
  begin
    DestNode   := GeoSceneTreeView.GetNodeAt(x, y);
    SourceNode := GeoSceneTreeView.Selected;

    if ((SourceNode.Parent.ItemId = layersNode.ItemId) and
      (DestNode.Parent.ItemId = layersNode.ItemId)) then
    begin
      SourceNode.MoveTo(DestNode, naAddFirst);
    end;
    layersList.ReplaceLayers(SourceNode.Text, DestNode.Text);
    // ox -layersList.ApplyAllLayers(Stamesh, MeshMatLib, Tri.Materials);
  end;
end;

procedure TfmGeoScene.GeoSceneTreeViewDragOver(Sender, Source: TObject;
  X, Y: integer; State: TDragState; var Accept: boolean);
var
  DestNode, SourceNode: TTreeNode;
begin
  inherited;
  if (Assigned(GeoSceneTreeView.GetNodeAt(X, Y)) and
    (Assigned(GeoSceneTreeView.Selected))) then
  begin
    DestNode   := GeoSceneTreeView.GetNodeAt(x, y);
    SourceNode := GeoSceneTreeView.Selected;

    if (Assigned(DestNode.Parent) and Assigned(SourceNode.Parent)) then

    begin
      Accept := (Sender = Source) and (DestNode.Parent.ItemId =
        layersNode.ItemId) and (SourceNode.Parent.ItemId = layersNode.ItemId);
    end
    else
    begin
      Accept := False;
    end;
  end;
end;

procedure TfmGeoScene.GeoSceneTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  inherited;
  layersList.ChangeName(Node.Text, S);
end;

procedure TfmGeoScene.GeoSceneTreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  if Assigned(GeoSceneTreeView.GetNodeAt(X, Y)) then
  begin
    if (lastHintedNode.ItemId <> GeoSceneTreeView.GetNodeAt(X, Y).ItemId) then
    begin
      DeleteToolTip;
    end;

    if (GeoSceneTreeView.GetNodeAt(X, Y).ItemId = layersNode.ItemId) then
    begin
      AddToolTip(layersNode.Handle, @ti, 1,
        'In this node presented' + #13 + 'the layers list', 'Layers');
    end
    else if (GeoSceneTreeView.GetNodeAt(X, Y).ItemId = texturesNode.ItemId) then
    begin
      AddToolTip(texturesNode.Handle, @ti, 1,
        'In this node presented textures' + #13 + 'which are used in program',
        'Textures');
    end
    else if (GeoSceneTreeView.GetNodeAt(X, Y).ItemId = carsNode.ItemId) then
    begin
      AddToolTip(carsNode.Handle, @ti, 1,
        'In this node presented' + #13 + 'a list of the cars', 'Cars');
    end
    else if (GeoSceneTreeView.GetNodeAt(X, Y).ItemId = treesNode.ItemId) then
    begin
      AddToolTip(treesNode.Handle, @ti, 1,
        'In this node presented' + #13 + 'a list of the trees', 'Trees');
    end
    else if (GeoSceneTreeView.GetNodeAt(X, Y).ItemId = mainNode.ItemId) then
    begin
      AddToolTip(mainNode.Handle, @ti, 1,
        'It is the model name', 'Model');
    end;
    lastHintedNode := GeoSceneTreeView.GetNodeAt(X, Y);
  end
  else
  begin
    DeleteToolTip;
  end;
end;

procedure TfmGeoScene.CloseObjectBrowserButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  DeleteToolTip;
  AddToolTip(CloseObjectBrowserButton.Handle, @ti, 1,
    'Close object browser', 'Close');
end;

procedure TfmGeoScene.FindAndSetTriangles(X, Y, Mode: integer);

  procedure Select(index: integer);
  begin
    Tri.TrianglesArray[round(index)].Selected   := True;
    Tri.TrianglesArray[round(index)].PrevMaterialNo :=
      Tri.TrianglesArray[round(index)].MaterialNo;
    Tri.TrianglesArray[round(index)].MaterialNo := CurrentTexture;

      { ox
      StaMesh.MeshObjects[0].FaceGroups[round(index)].Prepare;
      StaMesh.MeshObjects[0].FaceGroups[round(index)].MaterialName :=
        'm' + IntToStr(Tri.TrianglesArray[round(index)].MaterialNo);
       }
    Inc(SelectedTrianglesCount);
  end;

  procedure UnSelect(index: integer);
  begin
    Tri.TrianglesArray[round(index)].Selected := False;

    Tri.TrianglesArray[round(index)].MaterialNo :=
      Tri.TrianglesArray[round(index)].PrevMaterialNo;

       { ox
      StaMesh.MeshObjects[0].FaceGroups[round(index)].Prepare;
      StaMesh.MeshObjects[0].FaceGroups[round(index)].MaterialName :=
        'm' + IntToStr(Tri.TrianglesArray[round(index)].MaterialNo);
       }
    Dec(SelectedTrianglesCount);
  end;

var
  rayStart, rayVector, iPoint, iNormal: TGLVector;
  i: integer;
  v: TVector4f;
begin

  SetVector(rayStart, Cam.AbsolutePosition);
  SetVector(rayVector, GLSceneViewer.Buffer.ScreenToVector(
    AffineVectorMake(X, GLSceneViewer.Height - Y, 0)));
  NormalizeVector(rayVector);

  i := 0;
  while (i < MeshTris.Count) do
  begin
    if RayCastTriangleIntersect(rayStart, rayVector, MeshTris.List[i],
      MeshTris.List[i + 1], MeshTris.List[i + 2], @iPoint, @iNormal) then
    begin
      if not Tri.TrianglesArray[round(i / 3)].Selected then
      begin
        if (Mode = 1) then
          Select(round(i / 3));
        if (Mode = 0) then
          Select(round(i / 3));
      end
      else
      begin
        if (Mode = 2) then
          UnSelect(round(i / 3));
        if (Mode = 0) then
          UnSelect(round(i / 3));
      end;
      break;
    end;
    i := i + 3;
  end;

end;

procedure TfmGeoScene.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  try
    GLCadencer.Enabled := False;
    Timer1.Enabled     := False;

    FreeMemory(bmpscale);
    FreeMemory(GSTree);
    FreeMemory(Car);
    FreeMemory(Map);

    Tri.FreeDataBase;
    FreeMemory(Tri);
    layersList.Free;

  except
    on E: Exception do
    begin
      MessageDlg(LoadResString(@rsError) + '1', mtError, [mbOK], 0);
    end;
  end;
end;


procedure TfmGeoScene.FormDestroy(Sender: TObject);
begin
  inherited;
end;

//SWITCHING THE CAR CAMERA VIEW (DISTANCE)
procedure TfmGeoScene.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  inherited;
  if MainMenuIsActive then
  begin
    if (Key = VK_DOWN) then
    begin
      MainMenu.SelectNext;
    end;
    if (Key = VK_UP) then
    begin
      MainMenu.SelectPrev;
    end;
    if (Key = VK_RETURN) then
    begin
      if MainMenu.Index = 0 then
      begin
        MainMenuIsActive := False;
        MainMenu.Visible := False;
      end;
    end;
  end;
end;

procedure TfmGeoScene.FormKeyPress(Sender: TObject; var Key: char);
begin
  inherited;
  if Assigned(Car) then
  begin
    if ((Key = 'v') or (Key = 'V')) then
    begin
      case Car.CameraDistance of
        3: Car.CameraDistance := 10;
       10: Car.CameraDistance := 20;
       20: Car.CameraDistance := 3;
      end;
    end;
  end;
  if ((Key = 'x') or (Key = 'X')) then
  begin
    ApplyNewTexturesForMesh;
  end;
end;

procedure TfmGeoScene.InitWorld; //World initialization
begin
  try
    //Initialization of the physics engine
        { ox -
        OdeEngine1.LogEnable := True;
        OdeEngine1.InitODE;
        }

    //loading terrain into the mesh
    // ox - LoadTerrainToMesh(StaMesh);

    //Initialization of the car
    { ox - InitCar(GLScene, Cam, OdeEngine1, StaMesh, CarMatLib);}

    //initialization of the map
    InitMap(MapMatLib, WinBmpFont);

    //Setting up the sound
    InitSound(Car, GLSoundLibrary);

    //Initialization of the sky
    InitSky(MLSkyBox);

    //Initialization of the Sun
    InitSun(LensFlare);

    //Light initialization
    InitLight(Light);

    //Initialization of the trees
    // ox - InitTrees(GLScene, TreeMatLib, StaMesh, SurfaceCube);

    //Current model name
    Caption := CurrentModelName;

  except
    on E: Exception do
    begin
      MessageDlg(LoadResString(@rsError) + '2', mtError, [mbOK], 0);
      Close;
    end;
  end;
end;

procedure TfmGeoScene.Load1Click(Sender: TObject);
begin
  inherited;
  OpenDialog.InitialDir := LAYERS_PATH + Tri.TheTableName;
  if (OpenDialog.Execute) then
  begin
    layersList.AddLayerFromFile(OpenDialog.FileName);
    // ox - layersList.ApplyAllLayers(Stamesh, MeshMatLib, Tri.Materials);
    if GeoSceneTreeView.Visible then
    begin
      ShowObjectsInTree;
      GeoSceneTreeView.Items.BeginUpdate;
      layersNode.Expand(False);
      GeoSceneTreeView.Items.EndUpdate;
    end;
  end;
end;

procedure TfmGeoScene.LogoClick(Sender: TObject);
begin
  inherited;

end;

//__________HANDLING BY DISTANCE OF THE CAR CAMERA____________
procedure TfmGeoScene.ApplyTexturesForMesh1Click(Sender: TObject);
begin
  inherited;
  ApplyNewTexturesForMesh;
end;

procedure TfmGeoScene.btnLowCamClick(Sender: TObject);
begin
  inherited;

  btnHighCam.Checked   := False;
  btnMediumCam.Checked := False;
  btnLowCam.Checked    := False;
  btnFirstPersonCam.Checked := False;

  case (Sender as TMenuItem).Tag of
    0: Car.CameraDistance := 1;
    1: Car.CameraDistance := 3;
    2: Car.CameraDistance := 10;
    3: Car.CameraDistance := 20;
  end;

  (Sender as TMenuItem).Checked := True;
end;

//SHOW THE MAP (OR NO)
procedure TfmGeoScene.Save1Click(Sender: TObject);
var
  LayerNo:  integer;
  FileName: string;
begin
  inherited;
  LAYERS_PATH := DataAssetsPath + 'Layer' + PathDelim;
  FileName    := LAYERS_PATH + Tri.TheTableName + PathDelim +
    InputBox('Ahtung!!!', 'Input layer name:', GeoSceneTreeView.Selected.Text) +
    FILE_EXT;

  if not DirectoryExists(LAYERS_PATH + Tri.TheTableName) then
  begin
    ForceDirectories(LAYERS_PATH + Tri.TheTableName);
  end;

  LayerNo := layersList.GetLayerNoByName(GeoSceneTreeView.Selected.Text);
  layersList.Layers[LayerNo].SaveToFile(FileName);

end;

procedure TfmGeoScene.Setecttexture1Click(Sender: TObject);
begin
  inherited;
  with TGeoSceneTextureDialog.CreateDialog(Self, MeshMatLib) do
  begin
    try
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfmGeoScene.ShowMapButClick(Sender: TObject);
begin
  inherited;
  ShowMapBut.Checked := not ShowMapBut.Checked;
end;


procedure TfmGeoScene.Timer1Timer(Sender: TObject);
begin
  inherited;
  //Time from start of the scene
  StatusBar1.Panels[0].Text :=
    LoadResString(@rsTime) + ': ' + TimeToStr(Time - BeginTime);

  //FPS count
  StatusBar1.Panels[1].Text :=
    Format('FPS: %0.0f', [GLSceneViewer.FramesPerSecond]);

  GLSceneViewer.ResetPerformanceMonitor;
end;


procedure TfmGeoScene.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  //Adjust Camera distance with mousewheel
  GLSceneViewer.Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TfmGeoScene.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: double);
begin
  if Assigned(Car) then
  begin
    //If the car is falling then restore it
    if Car.Position.Y < -10 then
    begin
      FreeAndNil(Map);
      InitMap(MapMatLib, WinBmpFont);
      Car.Behaviours.Clear;
      // ox Cam.TargetObject := StaMesh;
      FreeMemory(Car);
      // ox InitCar(GLScene, Cam, OdeEngine1, StaMesh, CarMatLib);
      InitSound(Car, GLSoundLibrary);
    end;
    PlaySound;
    StatusBar1.Panels[2].Text :=
      Format(LoadResString(@rsSpeed) + ': %0.0f ' + LoadResString(@rsKmh),
      [Car.Animations]);  //[Car.Speed]
  end;

  //Catching the keys pressing
  HandleKeys(deltaTime);

  //GLViewer invalidation
  GLSceneViewer.Invalidate;

  if MainMenuIsActive then
  begin
    Cam.MoveAroundTarget(deltaTime, 0.1);
  end;
end;

procedure TfmGeoScene.GLDirectOpenGL1Render(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  rayStart, rayVector, iPoint, iNormal: TGLVector;
  i, j: integer;
  v:    TVector4f;
begin
  inherited;
  glDisable(GL_BLEND);
  if ShowMapBut.Checked then //Showing the map
  begin
    if (Assigned(Map) and (not MainMenuIsActive)) then
    begin
      Map.rci := rci;
      Map.DrawImage('map', round(MAP_SIZE / 2),
        round(MAP_OFFSET + MAP_SIZE / 2),
        round(768 - MAP_OFFSET - MAP_SIZE / 2));
    end;
  end;

    {if EditTexturesMode then
    begin
        glLineWidth(2);
        glEnable(GL_LINE_SMOOTH);
        v[3] := 1;
        SetVector(v, ConvertWinColor(RGB(0, 255, 0), v[3]));
        glColor4fv(@v);

        i := 0;
        while (i < MeshTris.Count) do
        begin
            if Tri.TrianglesArray[round(i / 3)].Selected then
            begin
                glBegin(GL_LINE_LOOP);
                glVertex3f(MeshTris.List[i][0],
                    MeshTris.List[i][1],
                    MeshTris.List[i][2]);
                glVertex3f(MeshTris.List[i + 1][0],
                    MeshTris.List[i + 1][1],
                    MeshTris.List[i + 1][2]);
                glVertex3f(MeshTris.List[i + 2][0],
                    MeshTris.List[i + 2][1],
                    MeshTris.List[i + 2][2]);
                glEnd;
            end;
            i := i + 3;
        end;
    end;
    glLineWidth(1);
    glDisable(GL_LINE_SMOOTH);}

  // ox StaMesh.StructureChanged;

end;

procedure TfmGeoScene.HandleKeys(const deltaTime: double);
begin
  //move the camera around the target if mouse was dragged
  if ((mx <> mx2) or (my <> my2)) then
  begin
    GLSceneViewer.Camera.MoveAroundTarget(my - my2, mx - mx2);
    mx := mx2;
    my := my2;
  end;
end;

procedure TfmGeoScene.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  if (not MainMenuIsActive) and (not (ssShift in Shift)) and
    (not (ssCtrl in Shift)) then
  begin
    if ssLeft in Shift then
    begin
      mx2 := X;
      my2 := Y;
    end;
    if ssRight in Shift then
    begin
      GLSceneViewer.Camera.Move(cMoveSpeed * 0.2);
    end;
    cX := X;
    cY := Y;
  end;

  if (ssShift in Shift) and (EditTexturesMode) and (not MainMenuIsActive) and
    (MouseDownMode) then
  begin
    FindAndSetTriangles(X, Y, 1);
  end;

  if (ssCtrl in Shift) and (EditTexturesMode) and (not MainMenuIsActive) and
    (MouseDownMode) then
  begin
    FindAndSetTriangles(X, Y, 2);
  end;

end;

procedure TfmGeoScene.GLSceneViewerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited;
  MouseDownMode := False;
end;

procedure TfmGeoScene.GLSceneViewerPostRender(Sender: TObject);
begin
  inherited;
  if ShowMapBut.Checked then
  begin
    if (Assigned(Car) and Assigned(Map) and (not MainMenuIsActive)) then
    begin
      if ((abs(x - Car.Position.X) > 0.005) and
        (abs(y - Car.Position.Z) > 0.005)) then
      begin
        x_prew := x;
        y_prew := y;
        x      := Car.Position.X;
        y      := Car.Position.Z;
        Map.Show(x_prew, y_prew, x, y);
      end
      else
      begin
        Map.Show(x_prew, y_prew, x, y);
      end;
    end;
  end;
end;

procedure TfmGeoScene.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  //Camera correction
  // ox - Cam.Up := StaMesh.Up;
  MouseDownMode := True;

  if (not MainMenuIsactive) then
  begin
    mx  := X;
    my  := Y;
    mx2 := X;
    my2 := Y;

    cX := X;
    cY := Y;
  end;

  if (EditTexturesMode) and (Button = TMouseButton(mbLeft)) then
    FindAndSetTriangles(X, Y, 0);

end;

procedure TfmGeoScene.CloseGeoSceneClick(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TfmGeoScene.CompLayerClick(Sender: TObject);
var
  i: integer;
begin
  inherited;
  CompLayer.Enabled := False;
  Reset.Enabled     := False;
  layersList.Add(Tri, 'Layer' + IntToStr(layersList.Count));
  for i := 0 to Tri.TriCount - 1 do
  begin
    Tri.TrianglesArray[i].Selected := False;
  end;
  Edittexturesmode := False;
  if GeosceneTreeView.Visible then
  begin
    ShowObjectsInTree;
    GeoSceneTreeView.Items.BeginUpdate;
    layersNode.Expand(False);
    GeoSceneTreeView.Items.EndUpdate;
  end;
  // ox layersList.ApplyAllLayers(StaMesh, MeshMatLib, Tri.Materials);
end;

procedure TfmGeoScene.Edittexturesmode1Click(Sender: TObject);
begin
  inherited;
  Edittexturesmode1.Checked := not Edittexturesmode1.Checked;
  EditTexturesMode := Edittexturesmode1.Checked;
end;

//TO SHOW THE MESH COVERAGE (OR NO)
procedure TfmGeoScene.N3Click(Sender: TObject);
begin
  inherited;
  if (N3.Checked) then
    // ox StaMesh.Material.FrontProperties.PolygonMode := pmLines;
    N3.Checked := False
  else
    // ox StaMesh.Material.FrontProperties.PolygonMode := pmFill;
    N3.Checked := True;
end;

//THE FREE VIEW OF THE CAR CAMERA
procedure TfmGeoScene.FreeCameraButtonClick(Sender: TObject);
begin
  inherited;
  FreeCameraButton.Checked := not FreeCameraButton.Checked;
end;

procedure TfmGeoScene.N6Click(Sender: TObject);
begin
  inherited;
  if (Assigned(Car) and (not MainMenuIsActive)) then
    Car.ResetCar;
end;

procedure TfmGeoScene.NewClick(Sender: TObject);
begin
  inherited;
  CompLayer.Enabled := True;
  Edittexturesmode  := True;
  Reset.Enabled     := True;
end;

//Call the object tree of GeoScene
procedure TfmGeoScene.ObjectsViewerClick(Sender: TObject);
begin
  inherited;
  ObjectsViewer.Checked := not ObjectsViewer.Checked;
  if ObjectsViewer.Checked then
  begin
    //Показать дерево
    GlSceneViewer.Align    := alNone;
    GlSceneViewer.Left     := 1;
    GeoSceneTreeView.Left  := 0;
    GlSceneViewer.Align    := alNone;
    GeoSceneTreeView.Width := round(self.Width / 5);
    GeoSceneTreeView.Align := alLeft;
    GlSceneViewer.Align    := alClient;
    GeoSceneTreeView.Visible := True;
    ShowObjectsInTree;
  end
  else
  begin
    //Hide tree
    GeoSceneTreeView.Visible := False;
    GlSceneViewer.Align      := alClient;
  end;
end;

procedure TfmGeoScene.ShowObjectsInTree;
const
  CAR_IMAGE     = 12;
  TREE_IMAGE    = 14;
  TEXTURE_IMAGE = 2;
  LAYER_IMAGE   = 16;
  ROOT_IMAGE    = 7;

  ON_IMAGE  = 18;
  OFF_IMAGE = 17;
var
  i:     integer;
  carsCount, layersCount, TreesCount, texturesCount: integer;
  textureBitmap: TBitmap;
  layer: TGeoSceneLayer;
begin

  GeoSceneTreeView.Items.Clear;
  mainNode   := GeoSceneTreeView.Items.Add(nil, Tri.TheTableName);
  layersNode := GeoSceneTreeView.Items.AddChild(mainNode, 'Layers');

  texturesNode := GeoSceneTreeView.Items.AddChild(mainNode, 'Textures');
  carsNode     := GeoSceneTreeView.Items.AddChild(mainNode, 'Cars');
  treesNode    := GeoSceneTreeView.Items.AddChild(mainNode, 'Trees');

  mainNode.ImageIndex     := ROOT_IMAGE;
  layersNode.ImageIndex   := LAYER_IMAGE;
  texturesNode.ImageIndex := TEXTURE_IMAGE;
  carsNode.ImageIndex     := CAR_IMAGE;
  treesNode.ImageIndex    := TREE_IMAGE;

  mainNode.SelectedIndex     := ROOT_IMAGE;
  layersNode.SelectedIndex   := LAYER_IMAGE;
  texturesNode.SelectedIndex := TEXTURE_IMAGE;
  carsNode.SelectedIndex     := CAR_IMAGE;
  treesNode.SelectedIndex    := TREE_IMAGE;

  carsCount     := 0;
  layersCount   := 0;
  TreesCount    := 0;
  texturesCount := 0;

  textureBitmap := TBitmap.Create;
  textureBitmap.Width := 32;
  textureBitmap.Height := 32;

  for i := 0 to layersList.Count - 1 do
  begin
    Inc(layersCount);
    with GeoSceneTreeView.Items.AddChild(layersNode,
        layersList.Layers[i].layerName) do
    begin
      ImageIndex    := ON_IMAGE;
      SelectedIndex := ON_IMAGE;
    end;
  end;

    { ox
    for i := 0 to StaMesh.MaterialLibrary.Materials.Count - 1 do
    begin
        with GeoSceneTreeView.Items.AddChild(texturesNode,
                StaMesh.MaterialLibrary.Materials[i].Name) do
        begin
            textureBitmap.Canvas.StretchDraw(rect(0, 0, 32, 31),
                StaMesh.MaterialLibrary.Materials[
                i].Material.Texture.Image.GetBitmap32(0).Create32BitsBitmap);
            TreeViewImageList.Add(textureBitmap, nil);
            ImageIndex := TreeViewImageList.Count - 1;
            SelectedIndex := TreeViewImageList.Count - 1;
        end;
    end;
    }

  textureBitmap.Free;

  for i := 0 to GLScene.Objects.Count - 1 do
  begin
    if (GLScene.Objects[i].ClassName = 'TGeoSceneCar') then
    begin
      Inc(carsCount);
      with GeoSceneTreeView.Items.AddChild(carsNode, 'Car ' +
          IntToStr(carsCount)) do
      begin
        ImageIndex    := ON_IMAGE;
        SelectedIndex := ON_IMAGE;
      end;
    end;
  end;

  for i := 0 to SurfaceCube.Count - 1 do
  begin
    if (SurfaceCube.Children[i].ClassName = 'TGeoSceneTree') then
    begin
      Inc(treesCount);
      with GeoSceneTreeView.Items.AddChild(treesNode, 'Tree ' +
          IntToStr(treesCount)) do
      begin
        ImageIndex    := ON_IMAGE;
        SelectedIndex := ON_IMAGE;
      end;
    end;
  end;

  GeoSceneTreeView.Items[0].Expand(False);
  GeoSceneTreeView.Items[0].Selected := True;
  GeoSceneTreeView.SetFocus;
end;


procedure TfmGeoScene.OdeEngine1MultiStepRender(delta: single);
begin
  inherited;
  if (Assigned(Car) and (not MainMenuIsActive)) then
  begin
    Car.SteeringTheCar;
  end;
end;

procedure TfmGeoScene.OdeEngine1StepRender(delta: single);
begin
  inherited;
  if not (FreeCameraButton.Checked) then
  begin
    if Assigned(Car) then
    begin
      Car.CarCameraCorrection;
    end;
  end;
end;

procedure TfmGeoScene.ResetClick(Sender: TObject);
var
  i: integer;
begin
  inherited;
  for i := 0 to Tri.TriCount - 1 do
  begin
    Tri.TrianglesArray[i].Selected := False;
  end;
  // ox layersList.ApplyAllLayers(StaMesh, MeshMatLib, Tri.Materials);
  Edittexturesmode  := False;
  Reset.Enabled     := False;
  CompLayer.Enabled := False;
end;

procedure FreeMemory(theObject: TObject);
begin
  if Assigned(theObject) then
  begin
    FreeAndNil(theObject);
  end;
end;

{ TGeoSceneTextureDialog }

constructor TGeoSceneTextureDialog.CreateDialog(AOwner: TComponent;
  MatLib: TGlMaterialLibrary);
begin
  inherited Create(AOwner);

  MaterialLibrary := MatLib;

  Self.Height := Self.Height + 200;
  Self.Width  := Self.Width + 50;
  aLabel      := TLabel.Create(Self.PanelBottom);
  aLabel.Parent := Self.PanelBottom;
  aLabel.Top  := Self.PanelBottom.Height - aLabel.Height - 20;
  aLabel.Left := 10;
  aLabel.Caption := _('Current texture is:') + ' #' + IntToStr(CurrentTexture);

  Self.ListBoxTexture.OnClick := OnSelectNewTexture;
end;

procedure TGeoSceneTextureDialog.OnSelectNewTexture;
var
  i: integer;
begin
  CurrentTexture := ListBoxTexture.ItemIndex + 1;
  aLabel.Caption := _('Current texture is:') + ' #' + IntToStr(CurrentTexture);

  if Assigned(Tri.Materials) then
  begin
    Tri.Materials.Add(CurrentTexture);
    MaterialLibrary.AddTextureMaterial('m' +
      IntToStr(Tri.Materials.Materials[Tri.Materials.Count - 1].No),
      Tri.Materials.Materials[Tri.Materials.Count - 1].Image);
  end;
end;


end.
