//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! MapScenery is based on Eric Grange GLSViewer for GLScene: www.glscene.org}


unit fMapScenery;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  WinApi.Messages,
  System.SysUtils,
  System.Classes,
  System.Actions,
  System.IniFiles,
  System.Math,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ActnList,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ToolWin,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.CheckLst,
  Vcl.ExtDlgs,


  GLScene,
  GLVectorTypes,
  GLVectorFileObjects,
  GLObjects,
  GLVectorGeometry,
  GLTexture,
  GLContext,
  GLVectorLists,
  GLCadencer,
  GLMesh,
  GLGeomObjects,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,

  
  fInitialForm,
  Gnugettext, GLSceneViewer;

type
  TfmMapScenery = class(TfmInitialForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ImageListScene: TImageList;
    ToolBar: TToolBar;
    MIFile:  TMenuItem;
    ACOpen:  TAction;
    ACExit:  TAction;
    Open1:   TMenuItem;
    N1:      TMenuItem;
    ToolButtonFileOpen: TToolButton;
    StatusBar: TStatusBar;
    GLSceneViewer: TGLSceneViewer;
    GLScene1: TGLScene;
    MIOptions: TMenuItem;
    MIAntiAlias: TMenuItem;
    MIAADefault: TMenuItem;
    MIAA2x:  TMenuItem;
    MIAA4X:  TMenuItem;
    ACSaveAs: TAction;
    ACZoomIn: TAction;
    ACZoomOut: TAction;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    MIView:  TMenuItem;
    ZoomIn1: TMenuItem;
    ZoomOut1: TMenuItem;
    FreeForm: TGLFreeForm;
    OpenDialog: TOpenDialog;
    GLLightSource: TGLLightSource;
    GLMaterialLibrary0: TGLMaterialLibrary;
    CubeExtents: TGLCube;
    ACResetView: TAction;
    Resetview1: TMenuItem;
    ToolButton5: TToolButton;
    ACShadeSmooth: TAction;
    ACFlatShading: TAction;
    ACWireframe: TAction;
    ACHiddenLines: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    N2:      TMenuItem;
    Smoothshading1: TMenuItem;
    Flatshading1: TMenuItem;
    Hiddenlines1: TMenuItem;
    Wireframe1: TMenuItem;
    ToolButton10: TToolButton;
    ACCullFace: TAction;
    Faceculling1: TMenuItem;
    N3:      TMenuItem;
    MIBgColor: TMenuItem;
    MITexturing: TMenuItem;
    ACTexturing: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    OpenPictureDialog: TOpenPictureDialog;
    MIPickTexture: TMenuItem;
    DCTarget: TGLDummyCube;
    GLCamera: TGLCamera;
    DCAxis:  TGLDummyCube;
    ACFlatLined: TAction;
    ToolButton13: TToolButton;
    FlatShadingwithlines1: TMenuItem;
    ACInvertNormals: TAction;
    MIActions: TMenuItem;
    InvertNormals1: TMenuItem;
    N4:      TMenuItem;
    Saveas1: TMenuItem;
    SaveDialog: TSaveDialog;
    ACReverseRenderingOrder: TAction;
    ReverseRenderingOrder1: TMenuItem;
    ACConvertToIndexedTriangles: TAction;
    ConverttoIndexedTriangles1: TMenuItem;
    ACFPS:   TAction;
    FramesPerSecond1: TMenuItem;
    GLCadencer: TGLCadencer;
    Timer:   TTimer;
    GLLightmapLibrary: TGLMaterialLibrary;
    ACSaveTextures: TAction;
    SDTextures: TSaveDialog;
    Savetextures1: TMenuItem;
    MIOpenTexLib: TMenuItem;
    ODTextures: TOpenDialog;
    Optimize1: TMenuItem;
    N5:      TMenuItem;
    ACOptimize: TAction;
    Stripify1: TMenuItem;
    ACStripify: TAction;
    N6:      TMenuItem;
    ACLighting: TAction;
    Lighting1: TMenuItem;
    TBLighting: TToolButton;
    PopupMenuMap: TPopupMenu;
    ActionBackground: TAction;
    GLMesh1: TGLMesh;
    Splitter1: TSplitter;
    VorPanel: TPanel;
    GLMaterialLibrary: TGLMaterialLibrary;
    CheckBoxSeeds: TCheckBox;
    CheckBoxTetras: TCheckBox;
    CheckBoxVors: TCheckBox;
    CheckBoxWireframe: TCheckBox;
    GroupBox1: TGroupBox;
    CheckListBox1: TCheckListBox;
    GroupBox2: TGroupBox;
    CheckListBox2: TCheckListBox;
    GLScene: TGLScene;
    GLDummyCube1: TGLDummyCube;
    DCVoronoi: TGLDummyCube;
    DCPoints: TGLDummyCube;
    DCDelaunay: TGLDummyCube;
    GLPoints1: TGLPoints;
    AxesCube: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    GLLines1: TGLLines;
    GLPolygon1: TGLPolygon;
    GLCamera1: TGLCamera;
    OpenVoronoi1: TMenuItem;
    OpenDialog1: TOpenDialog;
    ACOpenVor: TAction;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    procedure ActionBackgroundExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ACOpenExecute(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure GLSceneViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ACZoomInExecute(Sender: TObject);
    procedure ACZoomOutExecute(Sender: TObject);
    procedure ACExitExecute(Sender: TObject);
    procedure ACShadeSmoothExecute(Sender: TObject);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
    procedure MIAADefaultClick(Sender: TObject);
    procedure GLSceneViewerAfterRender(Sender: TObject);
    procedure ACResetViewExecute(Sender: TObject);
    procedure ACCullFaceExecute(Sender: TObject);
    procedure GLMaterialLibrary0TextureNeeded(Sender: TObject;
      var textureFileName: string);
    procedure ACTexturingExecute(Sender: TObject);
    procedure MIPickTextureClick(Sender: TObject);
    procedure MIFileClick(Sender: TObject);
    procedure ACInvertNormalsExecute(Sender: TObject);
    procedure ACSaveAsExecute(Sender: TObject);
    procedure ACSaveAsUpdate(Sender: TObject);
    procedure ACReverseRenderingOrderExecute(Sender: TObject);
    procedure ACConvertToIndexedTrianglesExecute(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject; const deltaTime, newTime: double);
    procedure ACFPSExecute(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ACSaveTexturesExecute(Sender: TObject);
    procedure MIOpenTexLibClick(Sender: TObject);
    procedure ACOptimizeExecute(Sender: TObject);
    procedure ACStripifyExecute(Sender: TObject);
    procedure ACLightingExecute(Sender: TObject);
    procedure ACOpenVorExecute(Sender: TObject);
    procedure CheckListBox2Click(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckBoxVorsClick(Sender: TObject);
    procedure CheckBoxSeedsClick(Sender: TObject);
    procedure CheckBoxTetrasClick(Sender: TObject);
    procedure CheckBoxWireframeClick(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
  private
    procedure DoResetCamera;
    procedure SetupFreeFormShading;
    procedure ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
    procedure ApplyShadeMode;
    procedure ApplyFSAA;
    procedure ApplyFaceCull;
    procedure ApplyTexturing;
    procedure ApplyFPS;
    procedure DoOpen(const FileName: TFileName);

  public
    md, nthShow: boolean;
    mx, my:      integer;
    hlShader:    TGLShader;
    LastFileName: TFileName;
    LastLoadWithTextures: boolean;
    BkColor:     TColor;
    procedure ApplyBgColor;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMapScenery: TfmMapScenery;

implementation

{$R *.dfm}

uses
  System.Win.Registry,

  GLPersistentClasses,
  GLMeshUtils,
  GLColor,
  GLKeyboard,
  GLFileOBJ,
  GLFileSTL,
  GLFileLWO,
  GLFileQ3BSP,
  GLFileOCT,
  GLFileMS3D,
  GLFileNMF,
  GLFileMD3,
  GLFile3DS,
  GLFileMD2,
  GLFileSMD,
  GLRenderContextInfo,
  GLFilePLY,
  GLFileGTS,
  GLFileMD5,
  GLS.FileTIN,
  GLMeshOptimizer,
  GLState,

  uGlobals,
  uCommon,
  dDialogs,
  fGeoblock,
  uDrawVor;

type
  // Hidden line shader (specific implem for the viewer, *not* generic)
  THiddenLineShader = class(TGLShader)
  private
    LinesColor: TColorVector;
    BackgroundColor: TColorVector;
    PassCount: integer;
  public
    procedure DoApply(var rci: TGLRenderContextInfo; Sender: TObject); override;
    function DoUnApply(var rci: TGLRenderContextInfo): boolean; override;
  end;

procedure TfmMapScenery.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  // instantiate our specific hidden-lines shader
  hlShader := THiddenLineShader.Create(Self);
  FreeForm.IgnoreMissingTextures := True;
  fmGeoblock.MapBackColor.Enabled := True;
end;

procedure THiddenLineShader.DoApply(var rci: TGLRenderContextInfo; Sender: TObject);
begin
  PassCount := 1;
  glPushAttrib(GL_ENABLE_BIT);
  glPushAttrib(GL_CURRENT_BIT + GL_ENABLE_BIT);
  glColor3fv(@BackgroundColor);
  glDisable(GL_TEXTURE_2D);
  glEnable(GL_POLYGON_OFFSET_FILL);
  glPolygonOffset(1, 2);
end;

function THiddenLineShader.DoUnApply(var rci: TGLRenderContextInfo): boolean;
begin
  case PassCount of
    1:
    begin
      PassCount := 2;
      glPopAttrib;
      glColor3fv(@LinesColor);
      glDisable(GL_LIGHTING);
      Result := True;
    end;
    2:
    begin
      glPopAttrib;
      Result := False;
    end;
    else
      // doesn't hurt to be cautious
      Assert(False);
      Result := False;
  end;
end;

procedure TfmMapScenery.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //////
  DestructDraw;
  //////
  WriteIniFile;
  inherited;
  Free;
  fmMapScenery := nil;
  fmGeoblock.MapBackColor.Enabled := False;
end;

procedure TfmMapScenery.FormShow(Sender: TObject);
var
  I: integer;
begin
  if not nthShow then
  begin
    OpenDialog.Filter := VectorFileFormatsFilter;
    SaveDialog.Filter := VectorFileFormatsSaveFilter;
    with ActionList do
      for I := 0 to ActionCount - 1 do
        if Actions[I] is TCustomAction then
          with TCustomAction(Actions[I]) do
            Hint := Caption;
    ApplyFSAA;
    ApplyFaceCull;
    ApplyBgColor;
    ApplyFPS;
    if ParamCount > 0 then
      DoOpen(ParamStr(1));
    nthShow := True;
  end;
end;

procedure TfmMapScenery.GLSceneViewerBeforeRender(Sender: TObject);
begin
  THiddenLineShader(hlShader).LinesColor      :=
    VectorMake(107 / 256, 123 / 256, 173 / 256, 1);
  THiddenLineShader(hlShader).BackgroundColor :=
    ConvertWinColor(GLSceneViewer.Buffer.BackgroundColor);
  (*
  if not GL_ARB_multisample then
  begin
    MIAADefault.Checked := True;
    MIAA2x.Enabled      := False;
    MIAA4X.Enabled      := False;
  end;
  *)
end;

procedure TfmMapScenery.GLSceneViewerAfterRender(Sender: TObject);
begin
  ApplyFSAA;
  Screen.Cursor := crDefault;
end;

procedure TfmMapScenery.DoResetCamera;
var
  objSize: single;
begin
  DCTarget.Position.AsVector := NullHmgPoint;
  GLCamera.Position.SetPoint(7, 3, 5);
  FreeForm.Position.AsVector := NullHmgPoint;
  FreeForm.Up.Assign(DCAxis.Up);
  FreeForm.Direction.Assign(DCAxis.Direction);

  objSize := FreeForm.BoundingSphereRadius;
  if objSize > 0 then
  begin
    if objSize < 1 then
    begin
      GLCamera.SceneScale := 1 / objSize;
      objSize := 1;
    end
    else
      GLCamera.SceneScale := 1;
    GLCamera.AdjustDistanceToTarget(objSize * 0.27);
    GLCamera.DepthOfView := 1.5 * GLCamera.DistanceToTarget + 2 * objSize;
  end;
end;

procedure TfmMapScenery.ApplyShadeModeToMaterial(aMaterial: TGLMaterial);
begin
  with aMaterial do
  begin
    if ACShadeSmooth.Checked then
    begin
      GLSceneViewer.Buffer.Lighting   := True;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmFill;
    end
    else if ACFlatShading.Checked then
    begin
      GLSceneViewer.Buffer.Lighting   := True;
      GLSceneViewer.Buffer.ShadeModel := smFlat;
      aMaterial.PolygonMode := pmFill;
    end
    else if ACFlatLined.Checked then
    begin
      GLSceneViewer.Buffer.Lighting   := True;
      GLSceneViewer.Buffer.ShadeModel := smFlat;
      aMaterial.PolygonMode := pmLines;
    end
    else if ACHiddenLines.Checked then
    begin
      GLSceneViewer.Buffer.Lighting   := False;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmLines;
    end
    else if ACWireframe.Checked then
    begin
      GLSceneViewer.Buffer.Lighting   := False;
      GLSceneViewer.Buffer.ShadeModel := smSmooth;
      aMaterial.PolygonMode := pmLines;
    end;
  end;
end;

procedure TfmMapScenery.ApplyShadeMode;
var
  i: integer;
begin
  with GLMaterialLibrary.Materials do
    for i := 0 to Count - 1 do
    begin
      ApplyShadeModeToMaterial(Items[i].Material);
      if (ACHiddenLines.Checked) or (ACFlatLined.Checked) then
        Items[i].Shader := hlShader
      else
        Items[i].Shader := nil;
    end;
  GLSceneViewer.Buffer.Lighting := ACLighting.Checked;
  FreeForm.StructureChanged;
end;

procedure TfmMapScenery.ApplyFSAA;
begin
  with GLSceneViewer.Buffer do
  begin
    if MIAADefault.Checked then
      AntiAliasing := aaDefault
    else if MIAA2X.Checked then
      AntiAliasing := aa2x
    else if MIAA4X.Checked then
      AntiAliasing := aa4x;
  end;
end;

procedure TfmMapScenery.ApplyFaceCull;
begin
  with GLSceneViewer.Buffer do
  begin
    if ACCullFace.Checked then
    begin
      FaceCulling    := True;
      ContextOptions := ContextOptions - [roTwoSideLighting];
    end
    else
    begin
      FaceCulling    := False;
      ContextOptions := ContextOptions + [roTwoSideLighting];
    end;
  end;
end;

procedure TfmMapScenery.ACOpenVorExecute(Sender: TObject);
begin
  inherited;
  OpenDialog1.InitialDir := ExpandPath(DirTinFaces);
  if OpenDialog1.Execute then
    DoOpen(OpenDialog1.FileName);
end;

procedure TfmMapScenery.ActionBackgroundExecute(Sender: TObject);
begin
  with dmDialogs do
  begin
    ColorDialog.Color := GLSceneViewer.Buffer.BackgroundColor;
    ColorDialog.HelpContext := 409; //IDH_MapBackground;
    if ColorDialog.Execute then
      GLSceneViewer.Buffer.BackgroundColor := ColorToRGB(ColorDialog.Color);
  end;
end;


procedure TfmMapScenery.ApplyBgColor;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := 16;
    Bitmap.Height := 16;
    BkColor := ColorToRGB(dmDialogs.ColorDialog.Color);
    GLSceneViewer.Buffer.BackgroundColor := BkColor;
    with Bitmap.Canvas do
    begin
      Pen.Color   := BkColor xor $FFFFFF;
      Brush.Color := BkColor;
      Rectangle(0, 0, 16, 16);
    end;
    MIBgColor.Bitmap := Bitmap;
  finally
    Bitmap.Free;
  end;
end;

procedure TfmMapScenery.ApplyTexturing;
var
  i: integer;
begin
  with GLMaterialLibrary.Materials do
    for i := 0 to Count - 1 do
    begin
      with Items[i].Material.Texture do
      begin
        if Enabled then
          Items[i].Tag := integer(True);
        Enabled := boolean(Items[i].Tag) and ACTexturing.Checked;
      end;
    end;
  FreeForm.StructureChanged;
end;

procedure TfmMapScenery.CheckBoxTetrasClick(Sender: TObject);
begin
  ActionVisibleDelaunay;
end;

procedure TfmMapScenery.CheckBoxVorsClick(Sender: TObject);
begin
  ActionVisibleVoronoi;
end;

procedure TfmMapScenery.CheckBoxSeedsClick(Sender: TObject);
begin
  ActionVisibleNode;
end;

procedure TfmMapScenery.CheckBoxWireframeClick(Sender: TObject);
begin
  ActionVisibleVorAsLine;
end;

procedure TfmMapScenery.CheckListBox1Click(Sender: TObject);
var
  si: integer;
begin
  si := CheckListBox1.ItemIndex; //CheckListBox1.SelCount;
  VorCube[si].Visible := CheckListBox1.Checked[si];
  dNodes[si].Visible := CheckListBox1.Checked[si];
end;

procedure TfmMapScenery.CheckListBox2Click(Sender: TObject);
begin
  ActionVisibleMaterial;
end;

procedure TfmMapScenery.ApplyFPS;
begin
  if ACFPS.Checked then
  begin
    Timer.Enabled      := True;
    GLCadencer.Enabled := True;
  end
  else
  begin
    Timer.Enabled      := False;
    GLCadencer.Enabled := False;
    StatusBar.Panels[1].Text := '--- FPS';
  end;
end;

procedure TfmMapScenery.SetupFreeFormShading;
var
  i:      Integer;
  libMat: TGLLibMaterial;
begin
  with GLMaterialLibrary do
  begin
    if Materials.Count = 0 then
    begin
      FreeForm.Material.MaterialLibrary := GLMaterialLibrary;
      libMat := Materials.Add;
      FreeForm.Material.LibMaterialName := libMat.Name;
      libMat.Material.FrontProperties.Diffuse.Red := 0;
    end;
    for i := 0 to Materials.Count - 1 do
      with Materials[i].Material do
        BackProperties.Assign(FrontProperties);
  end;
  ApplyShadeMode;
  ApplyTexturing;
  ApplyFPS;
end;

procedure TfmMapScenery.DoOpen(const FileName: TFileName);
var
  min, max: TAffineVector;
begin
  if not FileExists(FileName) then
    Exit;
  if ExtractFileExt(FileName) = '.vor' then
  begin
    GLSceneViewer.Camera := GLCamera1;
    InitDraw;
    LoadDrawVorFromFile(FileName);
    LoadDrawDelaunayFromFileAndVor(FileName);
    VorPanel.Visible := True;
  end
  else
  begin
    GLSceneViewer.Visible := False;
    GLSceneViewer.Camera  := GLCamera;
    GLSceneViewer.Visible := True;
    GLSceneViewer.Update;
    Screen.Cursor := crHourGlass;

    Caption := _('Scenery') + ' - ' + ExtractFileName(FileName);

    FreeForm.MeshObjects.Clear;
    GLMaterialLibrary.Materials.Clear;

    FreeForm.LoadFromFile(FileName);

    SetupFreeFormShading;

    StatusBar.Panels[0].Text := IntToStr(FreeForm.MeshObjects.TriangleCount) + ' NT';
    StatusBar.Panels[2].Text := fileName;
    ACSaveTextures.Enabled := (GLMaterialLibrary.Materials.Count > 0);
    MIOpenTexLib.Enabled := (GLMaterialLibrary.Materials.Count > 0);
    lastFileName := fileName;
    lastLoadWithTextures := ACTexturing.Enabled;

    FreeForm.GetExtents(min, max);
    with CubeExtents do
    begin
      CubeWidth  := max.X - min.X;
      CubeHeight := max.Y - min.Y;
      CubeDepth  := max.Z - min.Z;
      Position.AsAffineVector := VectorLerp(min, max, 0.5);
    end;
    DoResetCamera;
  end;
end;

procedure TfmMapScenery.ACOpenExecute(Sender: TObject);
begin
  OpenDialog.InitialDir := DataAssetsPath+'Objects';
  if OpenDialog.Execute then
    DoOpen(OpenDialog.FileName);
end;

procedure TfmMapScenery.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mx := X;
  my := Y;
  md := True;
end;

procedure TfmMapScenery.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  d: single;
begin
  if md and (Shift <> []) then
  begin
    if ssLeft in Shift then
      if ssShift in Shift then

        GLSceneViewer.Camera.MoveAroundTarget((my - y) * 0.1, (mx - x) * 0.1)
      else
        GLSceneViewer.Camera.MoveAroundTarget(my - y, mx - x)
    else if ssRight in Shift then
    begin
      d := GLSceneViewer.Camera.DistanceToTarget * 0.01 * (x - mx + y - my);
      if IsKeyDown('X') then
        FreeForm.Translate(d, 0, 0)
      else if IsKeyDown('y') then
        FreeForm.Translate(0, d, 0)
      else if IsKeyDown('z') then
        FreeForm.Translate(0, 0, d)
      else
      begin
        if ssShift in Shift then
          GLSceneViewer.Camera.RotateObject(GLSceneViewer.Camera.TargetObject,
            (my - y) * 0.1, (mx - x) * 0.1)
        else
          GLSceneViewer.Camera.RotateObject(GLSceneViewer.Camera.TargetObject,
            my - y, mx - x);
      end;
    end;
    mx := x;
    my := y;
  end;
end;

procedure TfmMapScenery.GLSceneViewerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  md := False;
end;

procedure TfmMapScenery.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  //if FreeForm.MeshObjects.Count > 0 then
  // begin
  GLSceneViewer.Camera.AdjustDistanceToTarget(Power(1.05, WheelDelta / 120));
  GLSceneViewer.Camera.DepthOfView := 2 * GLSceneViewer.Camera.DistanceToTarget + 2;
  //* FreeForm.BoundingSphereRadius;
  // end;
  Handled := True;
end;

procedure TfmMapScenery.ACZoomInExecute(Sender: TObject);
var
  h: boolean;
begin
  FormMouseWheel(Self, [], -120 * 4, Point(0, 0), h);
end;

procedure TfmMapScenery.ACZoomOutExecute(Sender: TObject);
var
  h: boolean;
begin
  FormMouseWheel(Self, [], 120 * 4, Point(0, 0), h);
end;

procedure TfmMapScenery.ACExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMapScenery.ACShadeSmoothExecute(Sender: TObject);
begin
  ApplyShadeMode;
end;

procedure TfmMapScenery.MIAADefaultClick(Sender: TObject);
begin
  (Sender as TMenuItem).Checked := True;
  ApplyFSAA;
end;

procedure TfmMapScenery.ACResetViewExecute(Sender: TObject);
begin
  DoResetCamera;
end;

procedure TfmMapScenery.ACCullFaceExecute(Sender: TObject);
begin
  ACCullFace.Checked := not ACCullFace.Checked;
  ApplyFaceCull;
end;

procedure TfmMapScenery.GLMaterialLibrary0TextureNeeded(Sender: TObject;
  var textureFileName: string);
begin
  if not ACTexturing.Enabled then
    textureFileName := '';
end;

procedure TfmMapScenery.ACTexturingExecute(Sender: TObject);
begin
  ACTexturing.Checked := not ACTexturing.Checked;
  if ACTexturing.Checked then
    if lastLoadWithTextures then
      ApplyTexturing
    else
    begin
      DoOpen(lastFileName);
    end
  else
    ApplyTexturing;
end;

procedure TfmMapScenery.MIFileClick(Sender: TObject);
begin
  MIPickTexture.Enabled := (GLMaterialLibrary.Materials.Count > 0);
end;

procedure TfmMapScenery.MIPickTextureClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    with GLMaterialLibrary.Materials do
    begin
      with Items[Count - 1] do
      begin
        Tag := 1;
        Material.Texture.Image.LoadFromFile(OpenPictureDialog.FileName);
        Material.Texture.Enabled := True;
      end;
    end;
  ApplyTexturing;
end;


procedure TfmMapScenery.MIOpenTexLibClick(Sender: TObject);
var
  i: integer;
begin
  if ODTextures.Execute then
    with GLMaterialLibrary do
    begin
      LoadFromFile(ODTextures.FileName);
      for i := 0 to Materials.Count - 1 do
        with Materials[i].Material do
          BackProperties.Assign(FrontProperties);
      ApplyShadeMode;
      ApplyTexturing;
    end;
end;

procedure TfmMapScenery.ACInvertNormalsExecute(Sender: TObject);
var
  i: integer;
begin
  with FreeForm.MeshObjects do
    for i := 0 to Count - 1 do
      Items[i].Normals.Scale(-1);
  FreeForm.StructureChanged;
end;

procedure TfmMapScenery.ACReverseRenderingOrderExecute(Sender: TObject);
var
  i, j, n: integer;
  fg:      TGLFaceGroup;
begin
  with FreeForm.MeshObjects do
  begin
    // invert meshobjects order
    for i := 0 to (Count div 2) do
      Exchange(i, Count - 1 - i);
    // for each mesh object
    for i := 0 to Count - 1 do
      with Items[i] do
      begin
        // invert facegroups order
        n := FaceGroups.Count;
        for j := 0 to (n div 2) do
          Exchange(j, n - 1 - j);
        // for each facegroup
        for j := 0 to n - 1 do
        begin
          fg := FaceGroups[j];
          fg.Reverse;
        end;
      end;
  end;
  FreeForm.StructureChanged;
end;

procedure TfmMapScenery.ACSaveAsExecute(Sender: TObject);
var
  ext: string;
begin
  if SaveDialog.Execute then
  begin
    ext := ExtractFileExt(SaveDialog.FileName);
    if ext = '' then
      SaveDialog.FileName :=
        ChangeFileExt(SaveDialog.FileName, '.' +
        GetVectorFileFormats.FindExtByIndex(SaveDialog.FilterIndex, False, True));
    if GetVectorFileFormats.FindFromFileName(SaveDialog.FileName) = nil then
      ShowMessage(_('Unsupported file extension'))
    else
      FreeForm.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TfmMapScenery.ACSaveAsUpdate(Sender: TObject);
begin
  ACSaveAs.Enabled := (FreeForm.MeshObjects.Count > 0);
end;

procedure TfmMapScenery.ACConvertToIndexedTrianglesExecute(Sender: TObject);
var
  v:  TAffineVectorList;
  IntegerList:  TIntegerList;
  MeshObject: TMeshObject;
  fg: TFGVertexIndexList;
begin
  v := FreeForm.MeshObjects.ExtractTriangles;
  try
    IntegerList := BuildVectorCountOptimizedIndices(v);
    try
      RemapAndCleanupReferences(v, IntegerList);
      IncreaseCoherency(IntegerList, 12);
      IntegerList.Capacity := IntegerList.Count;
      FreeForm.MeshObjects.Clean;
      MeshObject := TMeshObject.CreateOwned(FreeForm.MeshObjects);
      MeshObject.Vertices := v;
      MeshObject.BuildNormals(IntegerList, momTriangles);
      MeshObject.Mode := momFaceGroups;
      fg      := TFGVertexIndexList.CreateOwned(MeshObject.FaceGroups);
      fg.VertexIndices := IntegerList;
      fg.Mode := fgmmTriangles;
      FreeForm.StructureChanged;
    finally
      IntegerList.Free;
    end;
  finally
    v.Free;
  end;
  GLMaterialLibrary.Materials.Clear;
  SetupFreeFormShading;
end;

procedure TfmMapScenery.ACStripifyExecute(Sender: TObject);
var
  i:      integer;
  mo:     TMeshObject;
  fg:     TFGVertexIndexList;
  strips: TPersistentObjectList;
begin
  ACConvertToIndexedTriangles.Execute;
  mo     := FreeForm.MeshObjects[0];
  fg     := (mo.FaceGroups[0] as TFGVertexIndexList);
  strips := StripifyMesh(fg.VertexIndices, mo.Vertices.Count, True);
  try
    fg.Free;
    for i := 0 to strips.Count - 1 do
    begin
      fg := TFGVertexIndexList.CreateOwned(mo.FaceGroups);
      fg.VertexIndices := (strips[i] as TIntegerList);
      if i = 0 then
        fg.Mode := fgmmTriangles
      else
        fg.Mode := fgmmTriangleStrip;
    end;
  finally
    strips.Free;
  end;
end;

procedure TfmMapScenery.ACOptimizeExecute(Sender: TObject);
begin
  OptimizeMesh(FreeForm.MeshObjects, [mooVertexCache, mooSortByMaterials]);
  FreeForm.StructureChanged;
  SetupFreeFormShading;
end;

procedure TfmMapScenery.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: double);
begin
  if Self.Focused then
    fmMapScenery.Invalidate;
end;

procedure TfmMapScenery.ACFPSExecute(Sender: TObject);
begin
  ACFPS.Checked := not ACFPS.Checked;
  ApplyFPS;
end;

procedure TfmMapScenery.ACLightingExecute(Sender: TObject);
begin
  ACLighting.Checked := not ACLighting.Checked;
  //   TBLighting
  ApplyShadeMode;
end;

procedure TfmMapScenery.TimerTimer(Sender: TObject);
begin
  StatusBar.Panels[1].Text := Format('%.1f FPS', [GLSceneViewer.FramesPerSecond]);
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TfmMapScenery.ToolButton16Click(Sender: TObject);
begin
  inherited;
  GLSceneViewer.Visible := False;
end;

procedure TfmMapScenery.ACSaveTexturesExecute(Sender: TObject);
begin
  if SDTextures.Execute then
    GLMaterialLibrary.SaveToFile(SDTextures.FileName);
end;

procedure TfmMapScenery.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top    := ReadInteger(Name, 'Top', Top);
      Left   := ReadInteger(Name, 'Left', Left);
      Height := ReadInteger(Name, 'Height', Height);
      Width  := ReadInteger(Name, 'Width', Width);
      GLSceneViewer.Buffer.BackgroundColor := ReadInteger(Name, 'Color', clSilver);
    finally
      IniFile.Free;
    end;
end;


procedure TfmMapScenery.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
      WriteInteger(Name, 'Height', Height);
      WriteInteger(Name, 'Width', Width);
      WriteInteger(Name, 'Color', GLSceneViewer.Buffer.BackgroundColor);
    finally
      IniFile.Free;
    end;
end;


end.
