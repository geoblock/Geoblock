//-----------------------------------------------------------------------------

// The modeling system Geoblock http://sourceforge.net/projects/geoblock

//----------------------------------------------------------------------------
{
  The Terra Objects
}

unit uTerraObjects;

interface

uses
  //Common
  Winapi.Windows,
  Winapi.OpenGL,
  System.Classes, 
  System.SysUtils, 
  System.Types, 
  System.Math,
  Vcl.Graphics, 
  Vcl.Imaging.JPeg, 
  Vcl.Dialogs, 
  Vcl.ComCtrls,

  
  GLS.Scene,
  GLS.Coordinates,
  GLS.Canvas,
  GLS.Keyboard,
  GLS.Tree,
  GLS.Texture,
  GLS.ShadowVolume,
  GLS.WindowsFont,
  GLS.LensFlare,
  GLS.VectorLists,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.FileMD3,
  GLS.FileQ3MD3,
  GLS.Material,
  GLS.RenderContextInfo,
  GLS.Color,
  GLS.SoundManager,
  GLS.FileWAV,

  //Physics
  Physics.ODEManager,
  GLS.Mesh, // Old - oxOdeImport, GLOxOde,

  uTerraModel;

const
  //CAR CONSTANTS
  MAX_SPEED = 140;
  MIN_SPEED = 0;

  //MAP CONSTANTS
  MAP_SIZE      = 200;
  MAP_OFFSET    = 15;
  POINT_SIZE    = 5;
  MAP_COLOR     = clBlue;
  POINT_COLOR   = clRed;
  ARROW_COLOR   = clLime;
  ARROW_LENGTH  = 40;
  ARROW_POINTER = 5;
  ARROW_ANGLE   = 7;
  CIRCLE_COLOR  = clYellow;
  MAP_TRANSPARENCY_VALUE = 0.5;

  //TREE CONSTANTS
  TREES_COUNT = 0;

type
  //++++++++++ TREE ++++++++++++
  TGeoSceneTree = class(TGLTree)
  private
  public
    constructor Create(aOwner: TGLBaseSceneObject);
    procedure ApplyTreeMaterials;
  end;

  //+++++++++++++ CAR +++++++++++++++
  TGeoSceneCar = class(TGLActor) // Old - (TGLOxDynCar)
  private
    IsGoingBack:     boolean;
    FCameraDistance: integer;
    procedure AddWeapon;
  public
    Weapon:    TGLMesh; // Old: Weapon: TGLOxStaMesh;
    CarCamera: TGLCamera;
    MatLib:    TGLMaterialLibrary;
    MotorRunning : Boolean;
    property CameraDistance: integer read FCameraDistance write FCameraDistance;
    constructor Create(aOwner: TGLBaseSceneObject);
    procedure SteeringTheCar;
    procedure ResetCar;
    procedure LoadModelCar;
    procedure CarCameraCorrection;
    procedure AdjustCamera;
  end;

  //+++++++++++++ MAP +++++++++++++++++
  TGeoSceneMap = class
  private
    MapCanvas: TGLCanvas;
  public
    WinBmpFont: TGLWindowsBitmapFont;
    rci:    TGLRenderContextInfo;
    MatLib: TGLMaterialLibrary;
    procedure Show(x_prew, y_prew, x, y: single);
    procedure LoadMapToBmp(bmpTemp: TBitmap; TerrainObject: TGeoSceneModel);
    procedure DrawImage(ImgName: string; size, x0, y0: integer);
  end;

procedure InitCar(Scene: TGLScene; Camera: TGLCamera; Man: TGLODEManager;
  SMesh: TGLMesh; {Man: TGLOxOdeEngine; SMesh: TGLOxStaMesh;} aMatLib:
  TGLMaterialLibrary);
procedure InitSky(libMat: TGLMaterialLibrary);
procedure InitLight(light: TGLLightsource);
procedure InitSun(Sun: TGLLensFlare);
procedure InitTrees(Scene: TGLScene; matLib: TGLMaterialLibrary;
  SMesh: TGLMesh; aBox: TGLDummyCube);
procedure InitMap(MatLibrary: TGLMaterialLibrary; WinFont: TGLWindowsBitmapFont);


var
  GSTree: TGeoSceneTree;
  Car:    TGeoSceneCar;
  Map:    TGeoSceneMap;

//=========================================================================
implementation
//=========================================================================

uses
  uGlobals,
  fTerraScene,
  uResStrings,
  uTerraLoader;
 //                             ____________________                          //
 // ++++++++++++++++++++++++++ |                    | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |  *-*-* TREE *-*-*  | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |____________________| ++++++++++++++++++++++++++

constructor TGeoSceneTree.Create(aOwner: TGLBaseSceneObject);
begin
  Self := TGeoSceneTree(aOwner.AddNewChild(TGeoSceneTree));

  with Self do
  begin
    Direction.X := 0;
    Direction.Y := 0;
    Direction.Z := 1;

    LeafMaterialName     := 'LeafFront';
    LeafBackMaterialName := 'LeafBack';
    BranchMaterialName   := 'Branch';

    Depth    := 8;
    LeafSize := 0.2;
    BranchRadius := 0.08;
    BranchNoise := 0.5;
    //BranchSize := 0.1;

    Seed := Round((2 * Random - 1) * (MaxInt - 1));
  end;
end;

procedure TGeoSceneTree.ApplyTreeMaterials;
var
  ImgPath: TFileName;
begin
  Randomize;
  ImgPath := DirDataAssets + 'Textures' + PathDelim;

  // Set up default textures
  try
    with MaterialLibrary.AddTextureMaterial('LeafFront', ImgPath +
        'maple_multi.jpg') do
    begin
      Material.BlendingMode := bmOpaque;
      Material.Texture.TextureMode := tmModulate;
      Material.Texture.TextureFormat := tfRGBA;
    end;
    with MaterialLibrary.AddTextureMaterial('LeafBack', imgPath +
        'maple_multi.jpg') do
    begin
      Material.BlendingMode := bmOpaque;
      Material.Texture.TextureMode := tmModulate;
      Material.Texture.TextureFormat := tfRGBA;
    end;
    with MaterialLibrary.AddTextureMaterial('Branch', imgPath +
        'zbark_016.jpg') do
    begin
      Material.Texture.TextureMode := tmModulate;
    end;

  except
    on E: Exception do
    begin
      MessageDlg(LoadResString(@rsError) + '4', mtError, [mbOK], 0);
    end;
  end;

end;

 //                             ___________________                          //
 // ++++++++++++++++++++++++++ |                   | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |  *-*-* CAR *-*-*  | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |___________________| ++++++++++++++++++++++++++

constructor TGeoSceneCar.Create(aOwner: TGLBaseSceneObject);
begin
  Self := TGeoSceneCar(aOwner.AddNewChild(TGeoSceneCar));

  Self.Direction.X := 0;
  Self.Direction.Y := 1;
  Self.Direction.Z := 0;

  Self.Up.X := 0;
  Self.Up.Y := 0;
  Self.Up.Z := -1;

  Self.Position.X := 0;
  Self.Position.Y := 70;
  Self.Position.Z := 0;

  {
    Self.ACCEL := 0.15;
    Self.CMASS := 20;
    Self.WMASS := 0.3;
    Self.WHEEL_OFFSET := 0.425;
    Self.ZOOM := 1;
    Self.ZOOM_CUBED := 1;
    Self.RADIUS := 0.25;
    Self.TURN_SPEED := 0.05;

    Self.FrictionForce := 10;
    Self.FrontBackWheelAltForce := 1;
    Self.FrontBackWheelRunForce := 12;

    Self.WIDTH := 1.3;
    Self.HEIGHT := 0.7;
    Self.LENGTH := 2.8;

    Self.FRONTSUSPENSION_ERP := 0.8;
    Self.FRONTSUSPENSION_CFM := 0.01;
    Self.BACKSUSPENSION_ERP := 0.8;
    Self.BACKSUSPENSION_CFM := 0.01;

    Self.ContactNum := 24;
    Self.Approx0 := false;
    Self.Approx1 := false;
    Self.Approx1_1 := false;
    Self.Approx1_2 := false;
    Self.Density := 1;
    Self.DisableSteps := 15;
    Self.DisableThreshold := 0.001;
    Self.EdgeColor.AsWinColor := RGB(1, 1, 1);


    Self.FrontWheelBackUp := 0.2;
    Self.FrontWheelLimit := 1;
    Self.FrontWheelTurnDegre := 3;
    Self.IgnoreSelfModel := false;
    Self.Modes := Self.Modes + [mdSoftERP];
    Self.ModeTraction := mdBoth;
    Self.Mu := 45;
    Self.ObjectsSurfaceMode := mdoNoneSurface;
    Self.RollFriction := false;
    Self.Soft_erp := 0.5;

    Self.WheelPosA.X := -0.375;
    Self.WheelPosA.Y := 0;
    Self.WheelPosA.Z := 0;

    Self.WheelPosB.X := -0.375;
    Self.WheelPosB.Y := 0;
    Self.WheelPosB.Z := 0;

    Self.WheelPosC.X := 0.55;
    Self.WheelPosC.Y := 0;
    Self.WheelPosC.Z := 0;

    Self.WheelPosD.X := 0.55;
    Self.WheelPosD.Y := 0;
    Self.WheelPosD.Z := 0;

    Self.WheelsFrontLoHiStop := 1;
    Self.WheelsParamFMax := 100;

    Self.Speed := MIN_SPEED;
    Self.MotorRunning := true;

    Self.IsGoingBack := false;
    Self.CameraDistance := 10;
 }
  Self.Visible := True;
end;

//ADD THE WEAPON FOR CAR
procedure TGeoSceneCar.AddWeapon;
var
  ImagePath:   string;
  ObjectsPath: string;
begin
  try
    ImagePath   := DataAssetsPath + 'Textures' + PathDelim;
    ObjectsPath := DataAssetsPath + 'Objects' + PathDelim + '001' + PathDelim;

    Weapon := TGLMesh(Self.AddNewChild(TGLMesh));
    //Weapon.LoadFromFile(ObjectsPath + 'weapon.obj');

    Weapon.Material.Texture.Image.LoadFromFile(ImagePath + 'Weapon001.tga');
    Weapon.Material.Texture.Disabled := False;
  except
    on E: Exception do
      MessageDlg(LoadResString(@rsError) + '5', mtError, [mbOK], 0);
  end;

  Weapon.Scale.Scale(0.0015);
  Weapon.Direction.X := 0;
  Weapon.Direction.Y := 1;
  Weapon.Direction.Z := 0;

  Weapon.Position.X := 0;
  Weapon.Position.Y := 0;
  Weapon.Position.Z := 0.7;

  Weapon.Up.X := 0;
  Weapon.Up.Y := 0;
  Weapon.Up.Z := 1;

  Weapon.Turn(-90);

  //Weapon.Manager := Self.Manager;
  //Weapon.InitODE;
end;

//LOADING A MODEL OF THE CAR
procedure TGeoSceneCar.LoadModelCar;
const
  BODY_SKIN  = '1';
  WHEEL_SKIN = 'mag5';
var
  i:      integer;
  CarAct: TGLActor;
  roueact: TGLActor;

  ImagePath:   string;
  ObjectsPath: string;
begin
  try
    ImagePath   := DataAssetsPath + 'Textures' + PathDelim;
    ObjectsPath := DataAssetsPath + 'Objects' + PathDelim;

    CarAct := TGLActor(Self.AddNewChild(TGLActor));
    CarAct.LoadFromFile(ObjectsPath + 'body.md3');
    CarAct.MaterialLibrary := Self.MatLib;
    CarAct.LightmapLibrary := Self.MatLib;
    CarAct.Material.FrontProperties.Shininess := 64;
    CarAct.Material.FrontProperties.Specular.AsWinColor := clwhite;

    SetCurrentDir(ImagePath);
    LoadQ3Skin(ImagePath + BODY_SKIN + '.skin', CarAct);
  except
    on E: Exception do
    begin
      MessageDlg(LoadResString(@rsError) + '6', mtError, [mbOK], 0);
    end;
  end;

  with Self.MatLib.Materials[0] do
  begin
    Material.FrontProperties.Shininess := 64;
    Material.FrontProperties.Specular.AsWinColor := clwhite;
  end;
  with Self.MatLib.Materials[1] do
  begin
    Material.FrontProperties.Shininess := 64;
    Material.FrontProperties.Specular.AsWinColor := clwhite;
  end;

  with CarAct.Scale do
  begin
    X := 0.03;
    Y := 0.03;
    Z := 0.03;
  end;
  with CarAct.Direction do
  begin
    X := 0;
    Y := 0;
    Z := 1;
  end;
  for i := 1 to 4 do
  begin
    if (i = 1) then
      roueact := TGLActor(Self.AddNewChild(TGLActor));
    if (i = 2) then
      // old - roueact := TGLActor(Self.WheelLB.AddNewChild(TGLActor))
      roueact := TGLActor(Self.AddNewChild(TGLActor));
    if (i = 3) then
      roueact := TGLActor(Self.AddNewChild(TGLActor));
    if (i = 4) then
      roueact := TGLActor(Self.AddNewChild(TGLActor));
    try
      roueact.LoadFromFile(ObjectsPath + 'wheel.md3');

      with roueact.Scale do
      begin
        X := 0.03;
        Y := 0.03;
        Z := 0.03;
      end;
      if (i = 1) or (i = 2) then
        roueact.RollAngle := 90;
      if (i = 3) or (i = 4) then
        roueact.RollAngle := -90;

      roueact.MaterialLibrary := Self.MatLib;
      roueact.LightmapLibrary := Self.MatLib;
      roueact.Material.FrontProperties.Specular.AsWinColor := clWhite;
      roueact.Material.BlendingMode := bmModulate;

      SetCurrentDir(ImagePath);
      LoadQ3Skin(ImagePath + WHEEL_SKIN + '.skin', roueact);
    except
      on E: Exception do
      begin
        MessageDlg(LoadResString(@rsError) + '7', mtError, [mbOK], 0);
      end;
    end;
  end;
end;


//THE CAR STEERING
procedure TGeoSceneCar.SteeringTheCar;
begin
(*
    if Self.Steer > 1 then
        Self.Steer := 1
    if Self.Steer < -1 then
        Self.Steer := -1;
    if Self.Speed > MAX_SPEED then
        Self.Speed := MAX_SPEED
    if Self.Speed < MIN_SPEED then
    begin
        if not Self.IsGoingBack then
        begin
            Self.Speed := MIN_SPEED
        end
        else if Self.Speed < -MAX_SPEED then
        begin
            Self.Speed := -MAX_SPEED
        end;

    end;
    if (not IsKeyDown(VK_UP) and not IsKeyDown(VK_DOWN)) then
    begin
        if Self.Speed > MIN_SPEED then
        begin
            Self.Speed := Self.Speed - 0.8;
        end;
    end;
    if IsKeyDown(VK_RIGHT) then
    begin
        Self.Steer := Self.Steer + Self.TURN_SPEED
    end
    else if IsKeyDown(VK_LEFT) then
    begin
        Self.Steer := Self.Steer - Self.TURN_SPEED
    end
    else
    begin
        Self.Steer := Self.Steer * 0.1
    end;
    if not IsKeyDown(' ') then
    begin
        if Self.MotorRunning then
        begin
            if IsKeyDown(VK_UP) then
            begin
                if (Self.Speed >= 0) then
                begin
                    Self.Speed := Self.Speed + Self.ACCEL
                end
                else
                begin
                    Self.Speed := MAX_SPEED;
                end;
            end
            else if IsKeyDown(VK_DOWN) then
            begin
                Self.IsGoingBack := true;
                if (Self.Speed <= 0) then
                begin
                    Self.Speed := Self.Speed - Self.ACCEL
                end
                else
                begin
                    Self.Speed := -MAX_SPEED;
                end;
            end
            else
            begin
                Self.Speed := Self.Speed;
                Self.IsGoingBack := false;
            end
        end
    end;

    if IsKeyDown('r') then
    begin
        dBodyAddForce(Self.body[1], 0, 150, 0);
        dBodyAddForce(Self.body[2], 0, 150, 0);
    end;

    if IsKeyDown('a') then
    begin
        Self.Weapon.Turn(-1);
    end;

    if IsKeyDown('d') then
    begin
        Self.Weapon.Turn(1);
    end;
*)
end;

//REPAIR THE CAR
procedure TGeoSceneCar.ResetCar;
begin
(*
    if Self.Geom[0] <> nil then
    begin
        dBodyAddForce(Self.body[1], 0, 4000, 0);
        dBodyAddForce(Self.body[2], 0, 4000, 0);
    end;
*)
end;

//CAR CAMERA HANDLING
procedure TGeoSceneCar.CarCameraCorrection;
const
  CamVector: THomogeneousVector = (X:0; Y:1; Z:0; W:0);
var
  i: integer;
begin
(*
    for i := 0 to 100 do
    begin
        if (Self.CarCamera.Position.Y - Self.Frame.Position.Y) <
            Self.CameraDistance / 2 then
        begin
            Self.CarCamera.Position.Y :=
                Self.Frame.Position.Y + Self.CameraDistance / 2
        end;
        Self.CarCamera.PointTo((Self.Frame), CamVector);
        if Self.CarCamera.DistanceTo(Self.Frame) > Self.CameraDistance then
        begin
            Self.CarCamera.Move(0.01);
        end;
    end;
*)
end;

//CORRECTING THE CAMERA VIEW
procedure TGeoSceneCar.AdjustCamera;
var
  objSize: single;
begin
  objSize := Self.CarCamera.TargetObject.BoundingSphereRadius;
  if objSize > 0 then
  begin
    if objSize < 1 then
    begin
      Self.CarCamera.SceneScale := 1 / objSize;
      objSize := 1;
    end
    else
    begin
      Self.CarCamera.SceneScale := 1;
    end;
    Self.CarCamera.AdjustDistanceToTarget(objSize / 50);
    Self.CarCamera.DepthOfView :=
      Self.CarCamera.DistanceToTarget + 2 * objSize;
  end;
end;

 //                             ___________________                          //
 // ++++++++++++++++++++++++++ |                   | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |  *-*-* MAP *-*-*  | ++++++++++++++++++++++++++
 // ++++++++++++++++++++++++++ |___________________| ++++++++++++++++++++++++++


procedure TGeoSceneMap.Show(x_prew, y_prew, x, y: single);

var
  window_size_w, window_size_h, mapcenterX, mapcenterY: integer;
  min_w, min_h:     integer;
  x_pos, y_pos:     integer;
  y_scale, x_scale: integer;
  Wheel1, Wheel2:   TVector4f;
  DirectionVectorLength: single;
  DirectionVector:  TGeoScene3DPoint;
  Alpha, Betta:     single;
  i, j, h_mult, w_mult, step: integer;

begin

  min_w := -round(model_w / 2);            //minimal x in the mesh model
  min_h := -round(model_h / 2);            //minimal y in the mesh model
  window_size_w := round(1024);
  window_size_h := round(768);

  x_scale := round(model_w / MAP_SIZE);    //scale of the x coordinate
  y_scale := round(model_h / MAP_SIZE);    //scale of the y coordinate

  mapcenterX := round((MAP_SIZE + MAP_OFFSET) / 2);
  mapcenterY := round(((window_size_h - MAP_OFFSET) +
    (window_size_h - MAP_SIZE - MAP_OFFSET)) / 2);

  MapCanvas := TGLCanvas.Create(window_size_w, window_size_h);
  //canvas creating
  //MapCanvas.
  glEnable(GL_LINE_SMOOTH);
  MapCanvas.PenWidth := 2;

  x_pos := round((x - min_w) / x_scale);
  y_pos := round((y - min_h) / y_scale);

  x_pos := x_pos + MAP_OFFSET;
  y_pos := y_pos + window_size_h - MAP_SIZE - MAP_OFFSET;

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   {
   if (model_w > model_h) then
   begin
      x_pos := x_pos + round(model_h / model_w);
      y_pos := y_pos + round(model_w / model_h)
   end
   else
   begin
      x_pos := x_pos + round(model_w / model_h);
      y_pos := y_pos + round(model_h / model_w)
   end;
   }
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  MapCanvas.PenColor := POINT_COLOR;  //point color
  MapCanvas.PenAlpha := 1;    //transparency

  //if coordinates of the point are in the map region

  if ((x_pos <= (MAP_SIZE + MAP_OFFSET)) and
    (y_pos <= (window_size_h - MAP_OFFSET)) and (x_pos >= MAP_OFFSET) and
    (y_pos >= (window_size_h - MAP_SIZE - MAP_OFFSET))) then

  begin
    MapCanvas.FillRect(x_pos, y_pos, x_pos + POINT_SIZE,
      y_pos + POINT_SIZE);
  end;  //set point

  MapCanvas.PenColor := ARROW_COLOR;  //point color
  MapCanvas.PenAlpha := 1;    //transparency

  MapCanvas.FillEllipse(mapcenterX,
    mapcenterY,
    3, 3);
  MapCanvas.PenColor := CIRCLE_COLOR;  //point color
  MapCanvas.PenAlpha := 1;    //transparency

  MapCanvas.Ellipse(mapcenterX - ARROW_LENGTH,
    mapcenterY - ARROW_LENGTH,
    mapcenterX + ARROW_LENGTH,
    mapcenterY + ARROW_LENGTH);
  MapCanvas.MoveTo(mapcenterX, mapcenterY);


  //Begin of arrow constructin
  MapCanvas.PenColor    := ARROW_COLOR;
  //Arrow color
  MapCanvas.PenAlpha    := 1;
  //transparency
  DirectionVector.X     := x - x_prew;
  //Calculating the vector of
  DirectionVector.Y     := y - y_prew;
  //arrow direction (x,y)
  DirectionVectorLength := sqrt(power(DirectionVector.X, 2)
    + power(DirectionVector.Y, 2));
  //Calculating the length of the vector for arrow direction

  DirectionVector.X := DirectionVector.X / DirectionVectorLength;
  //Normalizing the vector
  DirectionVector.Y := DirectionVector.Y / DirectionVectorLength;
  MapCanvas.LineTo(DirectionVector.X * ARROW_LENGTH + mapcenterX,
    //Arrow main line
    DirectionVector.Y * ARROW_LENGTH + mapcenterY);
  if (DirectionVector.Y > 0) then
  begin
    Alpha := arccos(DirectionVector.X);
  end             //Calculating the angle
  else
  begin
    Alpha := -arccos(DirectionVector.X);
  end;
  Alpha := Alpha + DegToRad(ARROW_ANGLE);
  //Upper part of arrow pointer
  DirectionVector.x := cos(Alpha);
  DirectionVector.y := sin(Alpha);
  MapCanvas.MoveTo(DirectionVector.X * (ARROW_LENGTH - ARROW_POINTER) + mapcenterX,
    DirectionVector.Y * (ARROW_LENGTH - ARROW_POINTER) + mapcenterY);
  Alpha := Alpha - DegToRad(ARROW_ANGLE);
  DirectionVector.x := cos(Alpha);
  DirectionVector.y := sin(Alpha);
  MapCanvas.LineTo(DirectionVector.X * ARROW_LENGTH + mapcenterX,
    DirectionVector.Y * ARROW_LENGTH + mapcenterY);
  Alpha := Alpha - DegToRad(ARROW_ANGLE);
  //Downer part of arrow pointer
  DirectionVector.x := cos(Alpha);
  DirectionVector.y := sin(Alpha);
  MapCanvas.MoveTo(DirectionVector.X * (ARROW_LENGTH - ARROW_POINTER) + mapcenterX,
    DirectionVector.Y * (ARROW_LENGTH - ARROW_POINTER) + mapcenterY);
  Alpha := Alpha + DegToRad(ARROW_ANGLE);
  DirectionVector.x := cos(Alpha);
  DirectionVector.y := sin(Alpha);
  MapCanvas.LineTo(DirectionVector.X * ARROW_LENGTH + mapcenterX,
    DirectionVector.Y * ARROW_LENGTH + mapcenterY);
  //End of pointer constructing

  MapCanvas.PenColor := clWhite;
  MapCanvas.PenWidth := 1;
  MapCanvas.MoveTo(MAP_OFFSET, 768 - MAP_OFFSET - MAP_SIZE);
  MapCanvas.LineTo(MAP_OFFSET, 768 - MAP_OFFSET);
  MapCanvas.LineTo(MAP_OFFSET + MAP_SIZE, 768 - MAP_OFFSET);
  MapCanvas.LineTo(MAP_OFFSET + MAP_SIZE, 768 - MAP_OFFSET - MAP_SIZE);
  MapCanvas.LineTo(MAP_OFFSET, 768 - MAP_OFFSET - MAP_SIZE);

  MapCanvas.Free; //free memory

end;

procedure TGeoSceneMap.LoadMapToBmp(bmpTemp: TBitmap; TerrainObject: TGeoSceneModel);
const
  ISOLINE = True;       //Isoline mode
  ISOLINES_NUMBER = 50; //Number of isolines
var
  x_interval: TInterval;
  i, k, j, xx, yy, color: integer;
  TempDot:    TGeoScene3DPoint;
  IsolineLevels: array of integer; //The massive of isolines colors
  IsolineColors: array of integer;
  IsolineLevel, h_mult, w_mult: integer;
  ColorLevel: integer;
  ZCoord:     double;
  window_size_w, window_size_h: integer;
  bmp, bmpInverted: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.Width := round(TerrainObject.Max.X - TerrainObject.Min.X);
  Bmp.Height := round(TerrainObject.Max.Y - TerrainObject.Min.Y);

  //MaxProgress := TerrainObject.TriCount;

  if (ISOLINE = True) then
  begin
    SetLength(IsolineLevels, ISOLINES_NUMBER);
    SetLength(IsolineColors, ISOLINES_NUMBER);
    IsolineLevel := round(TerrainObject.Max.Z / ISOLINES_NUMBER);
    ColorLevel   := round(16755555 / ISOLINES_NUMBER);
    for i := 1 to ISOLINES_NUMBER do
    begin
      IsolineLevels[i - 1] := i * IsolineLevel;
      IsolineColors[i - 1] := i * ColorLevel;
    end;
  end;
  //-----------------------------------------------------------------
  for i := 0 to TerrainObject.TriCount - 1 do
  begin

    TerrainObject.GetTriangleByNum(i);
    for yy := round(TerrainObject.CurTri.C.y)
      to round(TerrainObject.CurTri.A.y) do
    begin
      x_interval := TerrainObject.ReturnXInterval(yy);
      for xx := Round(x_interval.min) to Round(x_interval.max) do
      begin
        TempDot.x := xx;
        TempDot.y := yy;
        color     := clWhite;
        ZCoord    := TerrainObject.GetZCoord(TempDot);
        if not (ISOLINE = True) then
        begin
          color :=
            round(255 / (abs(TerrainObject.Min.z) +
            abs(TerrainObject.Max.z)) * (ZCoord - TerrainObject.Min.z));
        end;
        Bmp.Canvas.Pixels[xx + round(abs(TerrainObject.Min.x)),
          yy + round(abs(TerrainObject.Min.y))] :=
          RGB(color, color, color);
        if (ISOLINE = True) then
        begin
          color := $5E5E5E;
          for k := 1 to ISOLINES_NUMBER do
          begin

            if ((round(ZCoord) >= IsolineLevels[k - 1]) and
              (round(ZCoord) < IsolineLevels[k])) then
            begin
              color := IsolineColors[k - 1];
              break;
            end;
          end;
          Bmp.Canvas.Pixels[xx + round(abs(TerrainObject.Min.x)),
            yy + round(abs(TerrainObject.Min.y))] := color;
        end;
      end;
    end;
  end;
  if (ISOLINE = True) then
  begin
    SetLength(IsolineLevels, 0);
    SetLength(IsolineColors, 0);
  end;

  bmpTemp.Canvas.StretchDraw(rect(0, 0, MAP_SIZE, MAP_SIZE), bmp);

  bmpInverted := TBitmap.Create;
  bmpInverted.Width := MAP_SIZE;
  bmpInverted.Height := MAP_SIZE;
  bmpInverted.Canvas.Draw(0, 0, bmpTemp);

  for i := 0 to MAP_SIZE - 1 do
  begin
    for j := 0 to MAP_SIZE - 1 do
    begin
      bmpTemp.Canvas.Pixels[MAP_SIZE - i, j] :=
        bmpInverted.Canvas.Pixels[i, j];
    end;
  end;

  bmp.Free;
  bmpInverted.Free;

end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TGeoSceneMap.DrawImage(ImgName: string; size, x0, y0: integer);
var
  window_size_w, window_size_h: integer;
  v: TVector4f;
begin
  glClear(GL_COLOR_BUFFER_BIT and GL_DEPTH_BUFFER_BIT);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  gluOrtho2D(0, 1024, 768, 0);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@IdentityHmgMatrix);

  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_BLEND);

  v.V[3] := 1;
  SetVector(v, ConvertWinColor(RGB(80, 80, 90), v.V[3]));
  glColor4fv(@v);

  Self.MatLib.ApplyMaterial(ImgName, Self.rci);
  //glBitmap(size * 2, size * 2, 0, 0, 0, 0, @bmpscale);
  glBegin(GL_QUADS);
  glTexCoord2f(0, 0);
  glVertex2f(x0 - size, y0 + size);
  glTexCoord2f(1, 0);
  glVertex2f(x0 + size, y0 + size);
  glTexCoord2f(1, 1);
  glVertex2f(x0 + size, y0 - size);
  glTexCoord2f(0, 1);
  glVertex2f(x0 - size, y0 - size);
  glEnd;

  //glTexImage2D(GL_QUADS, 0, GL_RGB, size * 2 + 2, size * 2 + 2, 1,
  //             GL_RGB, GL_BITMAP, @bmpscale);
  Self.MatLib.UnApplyMaterial(Self.rci);

   {WinBmpFont.TextOut(Self.rci, 50, 45, 'Советы:', clYellow);
   WinBmpFont.TextOut(Self.rci, 50, 60,
      'Для восстановления авто нажмите R', clWhite);
   WinBmpFont.TextOut(Self.rci, 50, 75,
      'Для смены расстояния камеры нажмите V', clWhite);}

  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glPopMatrix;

  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
end;

 /////////////////////////////////////////////////////////////////////////////////
 ///////////////////// GEOSCENE OBJECTS INITIALIZTION ////////////////////////////
 /////////////////////////////////////////////////////////////////////////////////

procedure InitMap(MatLibrary: TGLMaterialLibrary; WinFont: TGLWindowsBitmapFont);
begin
  Map := TGeoSceneMap.Create;
  Map.MatLib := MatLibrary;
  Map.WinBmpFont := WinFont;
end;

procedure InitCar(Scene: TGLScene; Camera: TGLCamera; Man: TGLOdeManager;
  SMesh: TGLMesh; aMatLib: TGLMaterialLibrary);
var
  i: integer;
  maxH, triNum, aTri: integer;
begin
  Car := TGeoSceneCar.Create(Scene.Objects);
  Car.CarCamera := Camera;
  //Old - Car.Manager := Man;
  Car.MatLib := aMatLib;

{   triNum := round(MeshTris.Count / 2);
   aTri := 0;

   maxH := round(MeshTris.List[triNum][1]);
   if maxH > round(MeshTris.List[triNum + 1][1]) then
   begin
      aTri := 1;
      maxH := round(MeshTris.List[triNum + 1][1])
   end;
   if maxH > round(MeshTris.List[triNum + 2][1]) then
   begin
      aTri := 2;
      maxH := round(MeshTris.List[triNum + 2][1])
   end;

   Car.Position.X := round(MeshTris.List[triNum + aTri ][0]);
   Car.Position.Z := maxH + 100;
   Car.Position.Y := round(MeshTris.List[triNum + aTri ][2]);  }

  // OX -  Car.InitODE;

  //Setting mass of the back wheels = 1 (more than front wheels (0.3))

{
    Car.Body[1].mass.mass := 5;  //right back wheel
    Car.Body[3].mass.mass := 5;  //left back wheel

    Car.Body[2].mass.mass := 4;  //right back wheel
    Car.Body[4].mass.mass := 4;  //left back wheel

    Car.LoadModelCar;
    //Car.AddWeapon;

    //Car.AdjustCamera;
    Car.CarCamera.TargetObject := Car.Frame;
    Car.CarCamera.DepthOfView := 2000;
}
end;

procedure InitTrees(Scene: TGLScene; matLib: TGLMaterialLibrary;
  SMesh: TGLMesh; aBox: TGLDummyCube);
var
  curTriNum: integer;
  MeshTriangles: TGLAffineVectorList;
  i:     integer;
  proxy: TGLProxyObject;
  s:     TGLVector;
  f:     single;
begin
  Randomize;
  GSTree := TGeoSceneTree.Create(aBox);
  GSTree.MaterialLibrary := matLib;
  GSTree.ApplyTreeMaterials;
  MeshTriangles := TGLAffineVectorList.Create;
  // Ox - MeshTriangles := SMesh.MeshObjects[0].ExtractTriangles;
  curTriNum     := Random(MeshTriangles.Count - 1);
  GSTree.Position.X := MeshTriangles.items[curTriNum].V[0];
  GSTree.Position.Y := MeshTriangles.items[curTriNum].V[1];
  GSTree.Position.Z := MeshTriangles.items[curTriNum].V[2];
  //GSTree.Scale.Scale(10);

  // spawn some more mushrooms using proxy objects
  for i := 0 to TREES_COUNT - 1 do
  begin
    // create a new proxy and set its MasterObject property
    proxy := TGLProxyObject(aBox.AddNewChild(TGLProxyObject));
    with proxy do
    begin
      MasterObject := GSTree;
      ProxyOptions := [pooObjects];
      // randomize position
      Randomize;
      curTriNum := Random(MeshTriangles.Count - 1);
      Scale.Scale(Random(5));
      Position.SetPoint(MeshTriangles.items[curTriNum].V[0],
        MeshTriangles.items[curTriNum].V[1],
        MeshTriangles.items[curTriNum].V[2]);
      // randomize orientation
      RollAngle := Random(360);
    end;
  end;
  MeshTriangles.Free;
end;

procedure InitSky(libMat: TGLMaterialLibrary);

{sub}procedure SetTexImageName(ml: TGLMaterialLibrary; const matName, fileName: string);
  var
    libMat: TGLLibMaterial;
    img:    TGLPicFileImage;
  begin
    try
      libMat := ml.LibMaterialByName(matName);
      libMat.Material.Texture.ImageClassName :=
        TGLPicFileImage.ClassName;
      img    := TGLPicFileImage(libMat.Material.Texture.Image);
      img.PictureFileName := fileName;
    except
      on E: Exception do
      begin
        MessageDlg(LoadResString(@rsError), mtError, [mbOK], 0);
      end;
    end;
  end;

begin
  SetTexImageName(libMat, 'NorthSky', DataAssetsPath + 'Skymap\North.jpg');
  SetTexImageName(libMat, 'EastSky', DataAssetsPath  + 'Skymap\East.jpg');
  SetTexImageName(libMat, 'SouthSky', DataAssetsPath + 'Skymap\South.jpg');
  SetTexImageName(libMat, 'WestSky', DataAssetsPath  + 'Skymap\West.jpg');
  SetTexImageName(libMat, 'TopSky', DataAssetsPath  + 'Skymap\Top.jpg');
end;


procedure InitLight(light: TGLLightsource);
begin
  Light.Position.Y := TriMesh_Height * 2;
end;

procedure InitSun(Sun: TGLLensFlare);
var
  sun_space: integer;
begin
  Sun.Position.Y := TriMesh_Height * 1.5;
  sun_space      := model_w;
  if sun_space < model_h then
    sun_space := model_h;
  Sun.Position.X := sun_space + 100;
  Sun.Position.Z := sun_space + 100;
end;

end.
