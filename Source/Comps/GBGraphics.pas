//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
unit GBGraphics;

(* Graphics methods and TGBCanvas class *)

interface

uses
  Windows, // not Winapi!
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.OpenGLTokens,
  GLS.OpenGLAdapter;

type
  TGLMat = array [0 .. 3] of TGLFloat;

  TRotationSequence = (RPY, RYP, PYR, PRY, YRP, YPR);
  TMaterialType = (mtChrome, mtEmerald, mtCyanPlastic, mtBrass, mtBronze,
    mtCopper, mtCustom);

type
  TVertex = TAffineDblVector; // don`t change. Double sensitive.

type
  PEnum0_3 = ^TEnum0_3;
  TEnum0_3 = 0 .. 3;
  PArrayTEnum0_3OfGLFloat = ^TArrayTEnum0_3OfGLFloat;
  TArrayTEnum0_3OfGLFloat = array [TEnum0_3] of TGLFloat;
  PGLRGBAfv = ^TGLRGBAfv;
  TGLRGBAfv = TArrayTEnum0_3OfGLFloat;

  PGLRGBAfr = ^TGLRGBAfr;

  TGLRGBAfr = packed record
    case Byte of
      0:
        (RGBAfv: TGLRGBAfv);
      1:
        (R: TGLFloat;
          G: TGLFloat;
          B: TGLFloat;
          A: TGLFloat;);
  end;

  PEnum0_2 = ^TEnum0_2;
  TEnum0_2 = 0 .. 2;
  PArrayTEnum0_2OfGLFloat = ^TArrayTEnum0_2OfGLFloat;
  TArrayTEnum0_2OfGLFloat = array [TEnum0_2] of TGLFloat;
  PGLRGBfv = ^TGLRGBfv;
  TGLRGBfv = TArrayTEnum0_2OfGLFloat;
  PGLRGBfr = ^TGLRGBfr;

  TGLRGBfr = record
    case Byte of
      0:
        (RGBfv: TGLRGBfv);
      1:
        (R: TGLFloat;
          G: TGLFloat;
          B: TGLFloat;);
  end;

  // ---------------------TGBColor---------------------//
type
  TGBColor = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FItems: TGLRGBAfv;
    procedure SetColor(Value: TColor);
    function GetColor: TColor;
    procedure SetItems(Index: Integer; Value: TGLFloat); virtual;
    function GetItems(Index: Integer): TGLFloat; virtual;
    procedure Change;
  protected
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    property Items[Index: Integer]: TGLFloat read GetItems
      write SetItems; default;
    procedure InitRGBAfv(Value: TGLRGBAfv);
    procedure InitRGBAf(R, G, B, A: TGLFloat);
    procedure SetRGBAfv(Value: TGLRGBAfv);
    function GetRGBAfv: TGLRGBAfv;
    procedure SetRGBAf(R, G, B, A: TGLFloat);
    procedure SetRGBfv(Value: TGLRGBfv);
    procedure SetRGBf(R, G, B: TGLFloat);
    procedure UpdateOpenGL;
  published
    property Color: TColor read GetColor write SetColor default clBlack;
    property Red: TGLFloat index 0 read GetItems write SetItems stored False;
    property Green: TGLFloat index 1 read GetItems write SetItems stored False;
    property Blue: TGLFloat index 2 read GetItems write SetItems stored False;
    property Alpha: TGLFloat index 3 read GetItems write SetItems;
  end;

  // --------------------------TPointStyle----------------------//
  TPointStyle = class(TPersistent)
  private
    FSize: TGLFloat;
    FColor: TGBColor;
    FSmooth: TGLEnum;
  public
    procedure Use;
  published
    property Size: TGLFloat read FSize write FSize;
    property Color: TGBColor read FColor write FColor;
    property Smooth: TGLEnum read FSmooth write FSmooth;
  end;

  // ----------------Initial Rotation---------------//
  TInitialRotation = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FInitialPitch: Double;
    FInitialRoll: Double;
    FInitialYaw: Double;
    procedure SetInitialPitch(Value: Double);
    procedure SetInitialRoll(Value: Double);
    procedure SetInitialYaw(Value: Double);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
  published
    property Pitch: Double read FInitialPitch write SetInitialPitch;
    property Roll: Double read FInitialRoll write SetInitialRoll;
    property Yaw: Double read FInitialYaw write SetInitialYaw;
  end;

  // ---------------------TAxes---------------------//
  TAxes = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPitchColor: TColor;
    FYawColor: TColor;
    FAxisRadius: Single;
    FAxisLength: Single;
    FVisible: Boolean;
    procedure SetPitchColor(Value: TColor);
    procedure SetYawColor(Value: TColor);
    procedure SetAxisRadius(Value: Single);
    procedure SetAxisLength(Value: Single);
    procedure SetVisible(Value: Boolean);
  protected
    PitchAxisEmission: array [0 .. 3] of TGLFloat;
    YawAxisEmission: array [0 .. 3] of TGLFloat;
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
  published
    property PitchAxisColor: TColor read FPitchColor write SetPitchColor;
    property YawAxisColor: TColor read FYawColor write SetYawColor;
    property AxisRadius: Single read FAxisRadius write SetAxisRadius;
    property AxisLength: Single read FAxisLength write SetAxisLength;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  // ---------------------TGBMaterial---------------------\\
type
  TGBMaterial = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FAmbientColor: TGBColor;
    FDiffuseColor: TGBColor;
    FSpecularColor: TGBColor;
    FEmissionColor: TGBColor;
    FShininess: TGLFloat;
    FMaterial: TMaterialType;
    procedure SetMaterialType(Value: TMaterialType);
    procedure SetMaterialShininess(Value: TGLFloat);
    procedure ColorChanged(Sender: TObject);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CallGLMaterial;
  published
    property MaterialType: TMaterialType read FMaterial write SetMaterialType;
    property AmbientColor: TGBColor read FAmbientColor write FAmbientColor;
    property DiffuseColor: TGBColor read FDiffuseColor write FDiffuseColor;
    property SpecularColor: TGBColor read FSpecularColor write FSpecularColor;
    property EmissionColor: TGBColor read FEmissionColor write FEmissionColor;
    property Shininess: TGLFloat read FShininess write SetMaterialShininess;
  end;

  // ---------------------TGLVector3Df---------------------//
type
  TGLVector3Df = class(TPersistent)
  private
    FCoordinates: array [0 .. 2] of TGLFloat;
    FOnChange: TNotifyEvent;
    procedure Change;
    procedure SetCoordinates(Index: Integer; Value: TGLFloat);
    function GetCoordinates(Index: Integer): TGLFloat;
  public
    property Coordinates[_Index: Integer]: TGLFloat read GetCoordinates
      write SetCoordinates; default;
    // procedure getVector3dfv : Tvector3dfv
  published
    property X: TGLFloat index 0 read GetCoordinates write SetCoordinates;
    property Y: TGLFloat index 1 read GetCoordinates write SetCoordinates;
    property Z: TGLFloat index 2 read GetCoordinates write SetCoordinates;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // ---------------------TGLVector4Df---------------------//
type
  TGLVector4Df = class(TPersistent)
  private
    FCoordinates: array [0 .. 3] of TGLFloat;
    FOnChange: TNotifyEvent;
    procedure Change;
    procedure SetCoordinates(Index: Integer; Value: TGLFloat);
    function GetCoordinates(Index: Integer): TGLFloat;
  public
    property Coordinates[_Index: Integer]: TGLFloat read GetCoordinates
      write SetCoordinates; default;
  published
    property X: TGLFloat index 0 read GetCoordinates write SetCoordinates;
    property Y: TGLFloat index 1 read GetCoordinates write SetCoordinates;
    property Z: TGLFloat index 2 read GetCoordinates write SetCoordinates;
    property W: TGLFloat index 3 read GetCoordinates write SetCoordinates;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  // ---------------------TGBLightModel---------------------//
type
  TGBLightModel = class(TPersistent)
  private
    FLocalViewer: Boolean;
    FTwoSide: Boolean;
    FOnChange: TNotifyEvent;
    FAmbient: TGBColor;
    FEnable: Boolean;
    procedure SetEnable(Value: Boolean); virtual;
    function GetEnable: Boolean; virtual;
    procedure SetLocalViewer(Value: Boolean); virtual;
    function GetLocalViewer: Boolean; virtual;
    procedure SetTwoSide(Value: Boolean); virtual;
    function GetTwoSide: Boolean; virtual;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure CallGLLightModel;
  published
    property Enable: Boolean read GetEnable write SetEnable default False;
    property LocalViewer: Boolean read GetLocalViewer write SetLocalViewer;
    property TwoSide: Boolean read GetTwoSide write SetTwoSide;
    procedure Change(Sender: TObject);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Ambient: TGBColor read FAmbient write FAmbient;
  end;

  // ---------------------TLight---------------------//
type
  TLight = class(TPersistent)
  private
    FNumber: TGLEnum;
    FEnable: Boolean;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;

    FAmbientColor: TGBColor;
    FDiffuseColor: TGBColor;
    FSpecularColor: TGBColor;

    // Light Arrays
    FPosition: TGLVector4Df;
    FSpotDirection: TGLVector3Df;
    FLightModel: TGBLightModel;

    FSpotExponent: TGLFloat;
    FSpotCutoff: TGLFloat;

    FAttenuations: TArrayTEnum0_2OfGLFloat;

    procedure SetSpotExponent(Value: TGLFloat); virtual;
    function GetSpotExponent: TGLFloat; virtual;
    procedure SetSpotCutoff(Value: TGLFloat); virtual;
    function GetSpotCutoff: TGLFloat; virtual;
    procedure SetAttenuations(Index: Integer; Value: TGLFloat); virtual;
    function GetAttenuations(Index: Integer): TGLFloat; virtual;
    procedure Change(Sender: TObject);
    procedure SetEnable(Value: Boolean); virtual;
    function GetEnable: Boolean; virtual;
    procedure SetVisible(Value: Boolean); virtual;
    function GetVisible: Boolean; virtual;
    procedure SetNumber(Value: TGLEnum); virtual;
    function GetNumber: TGLEnum; virtual;
  protected
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Attenuations[Index: Integer]: TGLFloat read GetAttenuations
      write SetAttenuations;
  public
    destructor Destroy; override;
    procedure CallGLLight; virtual;
  published
    property SpotExponent: TGLFloat read GetSpotExponent write SetSpotExponent;
    property SpotCutoff: TGLFloat read GetSpotCutoff write SetSpotCutoff;
    property ConstantAttenuation: TGLFloat index 0 read GetAttenuations
      write SetAttenuations;
    property LinearAttenuation: TGLFloat index 1 read GetAttenuations
      write SetAttenuations;
    property QuadraticAttenuation: TGLFloat index 2 read GetAttenuations
      write SetAttenuations;
    property Enable: Boolean read GetEnable write SetEnable default True;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Number: TGLEnum read GetNumber write SetNumber default 1;
    property AmbientColor: TGBColor read FAmbientColor write FAmbientColor;
    property DiffuseColor: TGBColor read FDiffuseColor write FDiffuseColor;
    property SpecularColor: TGBColor read FSpecularColor write FSpecularColor;
    property Position: TGLVector4Df read FPosition write FPosition;
    property SpotDirection: TGLVector3Df read FSpotDirection
      write FSpotDirection;
    property LightModel: TGBLightModel read FLightModel write FLightModel;
  end;

type
  TArrayOfGLuint = array of TGLuint;

  TDrawPrimitive = record
    Z: Double;
    IDs: TArrayOfGLuint;
  end;

  TArrayOfDrawPrimitive = array of TDrawPrimitive;

  TSelectPrimitiveEvent = procedure(Sender: TObject; X, Y: Integer;
    var Primitive: TDrawPrimitive) of object;
  TSelectPrimitivesEvent = procedure(Sender: TObject; X, Y: Integer;
    var Primitive: TArrayOfDrawPrimitive) of object;
  TRejectPrimitiveEvent = procedure(Sender: TObject; X, Y: Integer;
    Primitive: TDrawPrimitive; var Reject: Boolean) of object;

type
  TProjectionMode = (pmOrtho, pmPerspective);
  TRenderMode = (rmRENDER, rmSELECT, rmFEEDBACK);
  TViewMode = (vmXY, vmXZ, vmYZ, vmXYZ);

  // ---------------------TGBCanvas---------------------//
type
  TGBCanvas = class(TPanel)
  private
    Designing: Boolean;
    FInitialRotation: TInitialRotation;
    FMinX: Double;
    FMaxX: Double;
    FMinY: Double;
    FMaxY: Double;
    FMinZ: Double;
    FMaxZ: Double;
    FAxes: TAxes;
    FColor: TColor;
    FRollAngle: Double;
    FPitchAngle: Double;
    FYawAngle: Double;
    FViewAngle: TGLFloat;
    FirstTime: Boolean;
    FLight: TLight;
    FMaterial: TGBMaterial;
    FRotationSequence: TRotationSequence;
    ConstructorFlag: Boolean;
    FLX, FLY, FLZ: TGLFloat;
    FProjectionMode: TProjectionMode;
    // FCullFace: TGLEnum;
    // Position
    FwAngleY: TGLFloat;
    FwAngleX: TGLFloat;
    FwAngleZ: TGLFloat;
    FwAngleL: TGLFloat;
    FwVectorX: TGLFloat;
    FwVectorY: TGLFloat;
    FwVectorZ: TGLFloat;
    FCenterX, FCenterY, FCenterZ: TGLFloat;
    FCursor: TVector4d;
    FWorldCursor: TVector4d;

    hrc: HGLRC;

    FNearZ: TGLFloat;
    FFarZ: TGLFloat;
    FOnChange: TNotifyEvent;
    FOnCallLists: TNotifyEvent;
    // Material Arrays
    MaterialAmbient: array [0 .. 3] of TGLFloat;
    MaterialDiffuse: array [0 .. 3] of TGLFloat;
    MaterialSpecular: array [0 .. 3] of TGLFloat;
    MaterialEmission: array [0 .. 3] of TGLFloat;
    MaterialShininess: TGLFloat;
    { DimMaterialAmbient: Array[0..3] of TGLFloat;
      DimMaterialDiffuse: Array[0..3] of TGLFloat;
      DimMaterialSpecular: Array[0..3] of TGLFloat;
      DimMaterialEmission: Array[0..3] of TGLFloat;
      DimMaterialShininess: TGLFloat; { }
    // The Axes are Emmisive Only
    PitchAxisEmission: array [0 .. 3] of TGLFloat;
    YawAxisEmission: array [0 .. 3] of TGLFloat;
    FTriangulator: PGLUtriangulatorObj;

    FStartX: Integer;
    FStartY: Integer;
    FRenderMode: TRenderMode;
    FViewMode: TViewMode;
    FPickHeight: TGLDouble;
    FPickWidth: TGLDouble;
    FOnSelectPrimitive: TSelectPrimitiveEvent;
    FOnSelectPrimitives: TSelectPrimitivesEvent;

    FDrawCursor: TCursor;
    FMouseWheelStep: Double;
    FkY: Double;
    FkX: Double;
    FkZ: Double;
    FSelectPrimitivesArray: TArrayOfDrawPrimitive;
    FAppendPrimitives: Boolean;
    FOnRejectPrimitive: TRejectPrimitiveEvent;
    FFPS: Double;
    property StartX: Integer read FStartX write FStartX;
    property StartY: Integer read FStartY write FStartY;
    procedure Change(Sender: TObject);
    procedure LightChange(Sender: TObject);
    procedure SetAxes(Sender: TObject);
    procedure SetMaterial(Sender: TObject);
    procedure SetTextMaterial(AColor: TColor);
    procedure FontChange(Sender: TObject);
    procedure CallGLClearColor;
    procedure CreateAxisList;
    function GetCursor(const Index: Integer): TGLDouble;
    procedure SetCursor(const Index: Integer; const Value: TGLDouble);
    function GetWorldCursor(const Index: Integer): TGLDouble;
    procedure SetWorldCursor(const Index: Integer; const Value: TGLDouble);
    procedure SetViewMode(const Value: TViewMode);
    function GetFarZ: TGLFloat; virtual;
    function GetNearZ: TGLFloat; virtual;
    procedure SetFarZ(Value: TGLFloat); virtual;
    procedure SetNearZ(Value: TGLFloat); virtual;
    function GetTriangulator: PGLUtriangulatorObj;
    procedure SetTriangulator(const Value: PGLUtriangulatorObj);
    procedure SetViewAngle(Value: TGLFloat); virtual;
    procedure SetRollAngle(Value: Double);
    procedure SetPitchAngle(Value: Double);
    procedure SetYawAngle(Value: Double);
    procedure SetGLClearColor(Value: TColor);
    procedure SetMinX(Value: Double); virtual;
    function GetMinX: Double; virtual;
    procedure SetMaxX(Value: Double); virtual;
    function GetMaxX: Double; virtual;
    procedure SetMinY(Value: Double); virtual;
    function GetMinY: Double; virtual;
    procedure SetMaxY(Value: Double); virtual;
    function GetMaxY: Double; virtual;
    procedure SetMinZ(Value: Double); virtual;
    function GetMinZ: Double; virtual;
    procedure SetMaxZ(Value: Double); virtual;
    function GetMaxZ: Double; virtual;
    procedure SetDCPixelFormat;
    procedure InitOpenGl;
    procedure SetMouseWheelStep(const Value: Double);
    procedure SetkX(const Value: Double);
    procedure SetkY(const Value: Double);
    procedure SetkZ(const Value: Double);
    procedure SetAppendPrimitives(const Value: Boolean);
    function RejectPrimitive(X, Y: Integer; Primitive: TDrawPrimitive): Boolean;
    procedure SetFPS(const Value: Double);
  protected
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMClick(var Message: TWMMouse); message WM_MBUTTONUP;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    DC: HDC;
    Ratio: TGLFloat;
    ViewPort: TVector4i;
    ViewMatrix, ProjectMatrix: TMatrix4d;
    FontListBase: TGLEnum;
  public
    procedure SetPenPos(X, Y: Double);
    procedure SetPenColor(Color: TColor); overload;
    procedure SetPenColor(R, G, B: TGLDouble); overload;
    procedure SetPenWidth(W: TGLDouble);
    procedure SetPenStyle(AFactor: Integer; Pattern: word); overload;
    procedure SetPenStyle(AStyle: TPenStyle); overload;
    procedure MoveTo(X, Y, Z: TGLDouble);
    procedure Point(X, Y, Z: TGLDouble);
    procedure LineTo(X, Y, Z: TGLDouble);
    procedure Line(P1, P2: TVertex);
    procedure PolyLine(const Points: array of TVertex);
    procedure Polygon(const Points: array of TVertex);
    procedure DrawCube(CentX, CentY, CentZ, SizeX, SizeY, SizeZ: TGLFloat);
    // procedure parallelepiped;
    procedure SolidCylinder(X1, Y1, Z1, X2, Y2, Z2, radius: TGLDouble);
    procedure SolidSphere(X, Y, Z, radius: TGLDouble; N1: Integer = 5;
      N2: Integer = 5);
    procedure TextOut(Text: AnsiString; X1, Y1, Z1, Ax, Ay, Az, ScaleX, ScaleY,
      ScaleZ: TGLDouble; AColor: TColor);
    procedure TextOut3D(const Text: string; X, Y, Z: Single; Color: TColor);
  public
    procedure DrawCursorRect;
    procedure MakeCurrent;
    procedure SetProjection;
    procedure Redraw;
    procedure Paint; override;
    procedure SwapBuffers;
    procedure DeleteHGLRC;
    procedure WndProc(var Message: TMessage); override;
    procedure DefaultMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DefaultMouseDblClick(Sender: TObject);
    procedure DefaultKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    function SelectPrimitive(X, Y: Integer): TDrawPrimitive;
    function SelectPrimitives(X, Y: TGLint): TArrayOfDrawPrimitive;
    procedure SetPrimitiveID(ID: LongWord); overload;
    procedure SetPrimitiveID(ID: Pointer); overload;
    procedure DisablePrimitiveID;
    procedure EnablePrimitiveID;
    property SelectPrimitivesArray: TArrayOfDrawPrimitive
      read FSelectPrimitivesArray;
    property AppendPrimitives: Boolean read FAppendPrimitives
      write SetAppendPrimitives;
    procedure CanvasToWorld(cX, cY: Integer; var wX, wY, wZ: TGLDouble);
    procedure CursorToWorldCusror;
    procedure WorldCursorToCusror;
    procedure SetBounds3D(AMinX, AMaxX, AMinY, AMaxY, AMinZ, AMaxZ: TGLDouble);
  public
    property Triangulator: PGLUtriangulatorObj read GetTriangulator
      write SetTriangulator;
    property RenderMode: TRenderMode read FRenderMode write FRenderMode
      default rmRENDER;
    property CursorX: TGLDouble index 0 read GetCursor write SetCursor;
    property CursorY: TGLDouble index 1 read GetCursor write SetCursor;
    property CursorZ: TGLDouble index 2 read GetCursor write SetCursor;
    property CursorW: TGLDouble index 3 read GetCursor write SetCursor;
    property WorldCursorX: TGLDouble index 0 read GetWorldCursor
      write SetWorldCursor;
    property WorldCursorY: TGLDouble index 1 read GetWorldCursor
      write SetWorldCursor;
    property WorldCursorZ: TGLDouble index 2 read GetWorldCursor
      write SetWorldCursor;
    property WorldCursorW: TGLDouble index 3 read GetWorldCursor
      write SetWorldCursor;
    property FPS: Double read FFPS write SetFPS;
  published
    property Cursor; // Standart property
    property Align;
    property Enabled;
    property Visible;
    property OnKeyDown; // Standart events
    property OnClick;
    property OnDblClick;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
    // Custom
    property ProjectionMode: TProjectionMode read FProjectionMode
      write FProjectionMode default pmPerspective;
    property ViewMode: TViewMode read FViewMode write SetViewMode;
    property ViewAngle: TGLFloat read FViewAngle write SetViewAngle;
    property NearZ: TGLFloat read GetNearZ write SetNearZ;
    property FarZ: TGLFloat read GetFarZ write SetFarZ;
    property PickWidth: TGLDouble read FPickWidth write FPickWidth;
    property PickHeight: TGLDouble read FPickHeight write FPickHeight;
    property DrawCursor: TCursor read FDrawCursor write FDrawCursor;
    property Color read FColor write SetGLClearColor default clBlack;
    property Material: TGBMaterial read FMaterial write FMaterial;
    property Lighting: TLight read FLight write FLight;
    property Axes: TAxes read FAxes write FAxes;
    // Position
    property CenterX: TGLFloat read FCenterX write FCenterX;
    property CenterY: TGLFloat read FCenterY write FCenterY;
    property CenterZ: TGLFloat read FCenterZ write FCenterZ;
    property wAngleY: TGLFloat read FwAngleY write FwAngleY;
    property wAngleX: TGLFloat read FwAngleX write FwAngleX;
    property wAngleZ: TGLFloat read FwAngleZ write FwAngleZ;
    property wAngleL: TGLFloat read FwAngleL write FwAngleL;
    property wVectorX: TGLFloat read FwVectorX write FwVectorX;
    property wVectorY: TGLFloat read FwVectorY write FwVectorY;
    property wVectorZ: TGLFloat read FwVectorZ write FwVectorZ;
    property kX: Double read FkX write SetkX;
    property kY: Double read FkY write SetkY;
    property kZ: Double read FkZ write SetkZ;
    property MouseWheelStep: Double read FMouseWheelStep
      write SetMouseWheelStep;
    property MinX: Double read GetMinX write SetMinX stored True;
    property MaxX: Double read GetMaxX write SetMaxX stored True;
    property MinY: Double read GetMinY write SetMinY stored True;
    property MaxY: Double read GetMaxY write SetMaxY stored True;
    property MinZ: Double read GetMinZ write SetMinZ stored True;
    property MaxZ: Double read GetMaxZ write SetMaxZ stored True;
    property Lx: TGLFloat read FLX write FLX;
    property Ly: TGLFloat read FLY write FLY;
    property Lz: TGLFloat read FLZ write FLZ;
    property InitialRotation: TInitialRotation read FInitialRotation
      write FInitialRotation;
    property RotationSequence: TRotationSequence read FRotationSequence
      write FRotationSequence;
    property RollAngle: Double read FRollAngle write SetRollAngle;
    property PitchAngle: Double read FPitchAngle write SetPitchAngle;
    property YawAngle: Double read FYawAngle write SetYawAngle;
    // Events
    property OnCallLists: TNotifyEvent read FOnCallLists write FOnCallLists;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectPrimitive: TSelectPrimitiveEvent read FOnSelectPrimitive
      write FOnSelectPrimitive;
    property OnSelectPrimitives: TSelectPrimitivesEvent read FOnSelectPrimitives
      write FOnSelectPrimitives;
    property OnRejectPrimitive: TRejectPrimitiveEvent read FOnRejectPrimitive
      write FOnRejectPrimitive;
  public
    procedure ViewScale;
    procedure ViewAxis;
    procedure ViewTop;
    procedure ViewBottom;
    procedure ViewFront;
    procedure ViewBack;
    procedure ViewRight;
    procedure ViewLeft;
    procedure ViewRotate;
    procedure View3D;
    procedure ViewDefault;
    procedure ZoomIn;
    procedure ZoomOut;
  end;

  TFontLists = array [Char] of TGLuint;
  TGLLists = array of TGLuint;

  TFont3D = class(TFont)
  private
    FFontLists: TFontLists;
    FRaster: Boolean;
    procedure AddChar(C: Char);
  public
    function TranslateText(S: string): TGLLists;
    property Raster: Boolean read FRaster;
  end;

const
  PitchAxisList = 43;
  YawAxisList = 44;

procedure Cross(Ax, Ay, Az, bx, by, bz, cX, cY, cz: Double;
  var nx, ny, nz: Double);
procedure DrawTile(ARect: TRect; Canvas: TCanvas; Bitmap: TBitmap);
procedure ColorToGL(AColor: TColor; var C: TGLMat);

// ========================================================
implementation

// ========================================================

var
  LastCanvas: TGBCanvas;

procedure ColorToGL(AColor: TColor; var C: TGLMat);
begin
  AColor := ColorToRGB(AColor);
  C[0] := (1 + GetRValue(AColor)) / 128.0 - 1.0;
  C[1] := (1 + GetGValue(AColor)) / 128.0 - 1.0;
  C[2] := (1 + GetBValue(AColor)) / 128.0 - 1.0;
  C[3] := 1;
end;

procedure DrawTile(ARect: TRect; Canvas: TCanvas; Bitmap: TBitmap);

{ sub } function Width(Rect: TRect): Integer;
  begin
    Result := Rect.Right - Rect.Left;
  end;

{ sub } function Height(Rect: TRect): Integer;
  begin
    Result := Rect.Bottom - Rect.Top;
  end;

var
  I, J: Integer;
  DestR, SourceR, BitmapR: TRect;
begin
  if (Canvas = nil) or (Bitmap = nil) then
  begin
    Exit
  end;
  BitmapR := Rect(0, 0, Bitmap.Width, Bitmap.Height);
  for I := 0 to (Width(ARect) div (Bitmap.Width + 1)) do
  begin
    for J := 0 to (Height(ARect) div (Bitmap.Height + 1)) do
    begin
      DestR := BitmapR;
      OffsetRect(DestR, ARect.Left, ARect.Top);
      OffsetRect(DestR, Bitmap.Width * I, Bitmap.Height * J);
      IntersectRect(DestR, DestR, ARect);
      SourceR := DestR;
      OffsetRect(SourceR, -SourceR.Left, -SourceR.Top);
      Canvas.CopyRect(DestR, Bitmap.Canvas, SourceR);
    end
  end;
end;

{ TGBCanvas }

procedure TGBCanvas.DrawCube(CentX, CentY, CentZ, SizeX, SizeY,
  SizeZ: TGLFloat);
var
  I: Integer;
  nx, ny, nz: Double;
  GL_Mode: Cardinal;
const
  SignK: array [0 .. 1] of Double = (-0.5, 0.5);
type // (3)-----(7)      (0,1,1)-(1,1,1)
  TVertex3D = record // z/|      /|      z / |     / |
    X: TGLFloat; // (1)-----(5)|   (0,0,1)-(1,0,1)|
    Y: TGLFloat; // | |y    | |      |   |y  |   |
    Z: TGLFloat; // |(2)----|(6)     |(0,1,0)|(1,1,0)
  end; // |/      |/       | /     | /
var // (0)-----(4)x   (0,0,0)-(1,0,0)x
  Vert: array [0 .. 7] of TVertex3D;

  procedure DrawSide(V1, V2, V3, V4: TVertex3D);
  begin
    glBegin(GL_Mode);
    Cross(V1.X, V1.Y, V1.Z, V2.X, V2.Y, V2.Z, V3.X, V3.Y, V3.Z, nx, ny, nz);
    glNormal3f(nx, ny, nz);
    glVertex3f(V1.X, V1.Y, V1.Z);
    glVertex3f(V2.X, V2.Y, V2.Z);
    glVertex3f(V3.X, V3.Y, V3.Z);
    glVertex3f(V4.X, V4.Y, V4.Z);
    glEnd;
  end;

begin
  for I := 0 to 7 do
  begin
    Vert[I].X := CentX + SignK[(I and 4) shr 2] * SizeX;
    Vert[I].Y := CentY + SignK[(I and 2) shr 1] * SizeY;
    Vert[I].Z := CentZ + SignK[(I and 1) shr 0] * SizeZ;
  end;

  GL_Mode := { GL_QUADS;//{ } GL_POLYGON;
  DrawSide(Vert[0], Vert[2], Vert[3], Vert[1]); // left
  DrawSide(Vert[7], Vert[6], Vert[4], Vert[5]); // right
  DrawSide(Vert[0], Vert[4], Vert[6], Vert[2]); // bottom
  DrawSide(Vert[7], Vert[5], Vert[1], Vert[3]); // top
  DrawSide(Vert[0], Vert[1], Vert[5], Vert[4]); // near
  DrawSide(Vert[7], Vert[3], Vert[2], Vert[6]); // far
end;

// ----------------TInitialRotation Procedures---------------//

procedure TInitialRotation.SetInitialPitch(Value: Double);
begin
  if FInitialPitch <> Value then
  begin
    FInitialPitch := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end
  end;
end;

procedure TInitialRotation.SetInitialRoll(Value: Double);
begin
  if FInitialRoll <> Value then
  begin
    FInitialRoll := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end
  end;
end;

procedure TInitialRotation.SetInitialYaw(Value: Double);
begin
  if FInitialYaw <> Value then
  begin
    FInitialYaw := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end
  end;
end;

// ---------------------TAxes Procedures---------------------//

constructor TAxes.Create;
begin
  SetPitchColor(clWhite);
  SetYawColor(clYellow);
  FAxisLength := 30;
  FAxisRadius := 0.25;
  Visible := False;
end;

procedure TAxes.SetPitchColor(Value: TColor);
var
  RedLevel, GreenLevel, BlueLevel: Integer;
begin
  if Value <> FPitchColor then
  begin
    FPitchColor := Value;
    RedLevel := Value and $000000FF;
    GreenLevel := (Value and $0000FF00) shr 8;
    BlueLevel := (Value and $00FF0000) shr 16;
    PitchAxisEmission[0] := RedLevel / 255;
    PitchAxisEmission[1] := GreenLevel / 255;
    PitchAxisEmission[2] := BlueLevel / 255;
    PitchAxisEmission[3] := 0;
    if Assigned(FOnChange) then
      FOnChange(Self)
  end;
end;

procedure TAxes.SetYawColor(Value: TColor);
var
  RedLevel, GreenLevel, BlueLevel: Integer;
begin
  if Value <> FYawColor then
  begin
    FYawColor := Value;
    RedLevel := Value and $000000FF;
    GreenLevel := (Value and $0000FF00) shr 8;
    BlueLevel := (Value and $00FF0000) shr 16;
    YawAxisEmission[0] := RedLevel / 255;
    YawAxisEmission[1] := GreenLevel / 255;
    YawAxisEmission[2] := BlueLevel / 255;
    YawAxisEmission[3] := 0;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end;
  end;
end;

procedure TAxes.SetAxisRadius(Value: Single);
begin
  if Value <> FAxisRadius then
  begin
    FAxisRadius := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end;
  end;
end;

procedure TAxes.SetAxisLength(Value: Single);
begin
  if Value <> FAxisLength then
  begin
    FAxisLength := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end;
  end;
end;

procedure TAxes.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self)
    end;
  end;
end;

// ---------------------TGBColor Procedures---------------------//

constructor TGBColor.Create;
begin
  inherited Create;
  InitRGBAf(1, 1, 1, 1);
end;

procedure TGBColor.InitRGBAf(R, G, B, A: TGLFloat);
begin
  FItems[0] := R;
  FItems[1] := G;
  FItems[2] := B;
  FItems[3] := A;
end;

procedure TGBColor.InitRGBAfv(Value: TGLRGBAfv);
begin
  FItems := Value;
end;

procedure TGBColor.SetRGBAf(R, G, B, A: TGLFloat);
begin
  if (FItems[0] <> R) or (FItems[1] <> G) or (FItems[2] <> B) or (FItems[3] <> A)
  then
  begin
    InitRGBAf(R, G, B, A);
    Change;
  end;
end;

procedure TGBColor.SetRGBAfv(Value: TGLRGBAfv);
begin
  with TGLRGBAfr(Value) do
  begin
    SetRGBAf(R, G, B, A)
  end;
end;

function TGBColor.GetRGBAfv: TGLRGBAfv;
begin
  Result := FItems;
end;

procedure TGBColor.SetRGBf(R, G, B: TGLFloat);
begin
  SetRGBAf(R, G, B, Alpha);
end;

procedure TGBColor.SetRGBfv(Value: TGLRGBfv);
begin
  with TGLRGBfr(Value) do
  begin
    SetRGBf(R, G, B)
  end;
end;

function TGBColor.GetColor: TColor; { BGR }
begin
  Result := ((Round(FItems[2] * 255) and 255) shl 16) or
    ((Round(FItems[1] * 255) and 255) shl 8) or (Round(FItems[0] * 255));
end;

procedure TGBColor.SetColor(Value: TColor);
begin
  if Color <> Value then
  begin
    FItems[0] := (Value and $0000FF) / 255;
    FItems[1] := ((Value and $00FF00) shr 8) / 255;
    FItems[2] := ((Value and $FF0000) shr 16) / 255;
    Change;
  end;
end;

procedure TGBColor.SetItems(Index: Integer; Value: TGLFloat);
begin
  if FItems[Index] <> Value then
  begin
    FItems[Index] := Value;
    Change;
  end;
end;

function TGBColor.GetItems(Index: Integer): TGLFloat;
begin
  Result := FItems[Index];
end;

procedure TGBColor.Change;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

procedure TGBColor.UpdateOpenGL;
begin
  glColor4bv(@FItems);
end;

{ TLight }

// ---------------------TLight Procedures---------------------//

constructor TLight.Create;
begin
  AmbientColor := TGBColor.Create;
  DiffuseColor := TGBColor.Create;
  SpecularColor := TGBColor.Create;
  Position := TGLVector4Df.Create;
  SpotDirection := TGLVector3Df.Create;
  LightModel := TGBLightModel.Create;

  AmbientColor.OnChange := Change;
  DiffuseColor.OnChange := Change;
  SpecularColor.OnChange := Change;
  Position.OnChange := Change;
  SpotDirection.OnChange := Change;
  LightModel.OnChange := Change;

  AmbientColor.InitRGBAf(0.0, 0.0, 0.0, 1.0);
  DiffuseColor.InitRGBAf(1.0, 1.0, 1.0, 1.0);
  SpecularColor.InitRGBAf(1.0, 1.0, 1.0, 1.0);

  Position.X := 0;
  Position.Y := 0;
  Position.Z := 1;
  Position.W := 1;

  SpotDirection.X := 0;
  SpotDirection.Y := 0;
  SpotDirection.Z := -1;

  SpotExponent := 0;
  SpotCutoff := 180;

  ConstantAttenuation := 1;
  LinearAttenuation := 0;
  QuadraticAttenuation := 0;

  Number := 1;
  Visible := True;
  Enable := True;
end;

destructor TLight.Destroy;
begin
  AmbientColor.Free;
  DiffuseColor.Free;
  SpecularColor.Free;
  Position.Free;
  SpotDirection.Free;
  LightModel.Free;
  inherited Destroy;
end;

procedure TLight.SetEnable(Value: Boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
    Change(Self);
  end;
end;

function TLight.GetEnable: Boolean;
begin
  Result := FEnable;
end;

procedure TLight.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change(Self);
  end;
end;

function TLight.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TLight.SetNumber(Value: TGLEnum);
begin
  if FNumber <> Value then
  begin
    FNumber := Value;
    Change(Self);
  end;
end;

function TLight.GetNumber: TGLEnum;
begin
  Result := FNumber;
end;

procedure TLight.CallGLLight;
var
  OldPointSize: TGLFloat;
begin
  LightModel.CallGLLightModel;

  if Visible then
  begin
    glGetFloatv(GL_POINT_SIZE, @OldPointSize);
    try
      glPointSize(5);
      glBegin(GL_POINTS);
      glColor4fv(@(FSpecularColor.FItems));
      glVertex3fv(@(Position.FCoordinates));
      glEnd;
    finally
      glPointSize(OldPointSize);
    end;
  end;

  if Enable then
  begin
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0 + Number);
    glLightf(GL_LIGHT0 + Number, GL_SPOT_EXPONENT, SpotExponent);
    glLightf(GL_LIGHT0 + Number, GL_SPOT_CUTOFF, SpotCutoff);
    glLightf(GL_LIGHT0 + Number, GL_CONSTANT_ATTENUATION, ConstantAttenuation);
    glLightf(GL_LIGHT0 + Number, GL_LINEAR_ATTENUATION, LinearAttenuation);
    glLightf(GL_LIGHT0 + Number, GL_QUADRATIC_ATTENUATION,
      QuadraticAttenuation);
    glLightfv(GL_LIGHT0 + Number, GL_AMBIENT, @(AmbientColor.FItems));
    glLightfv(GL_LIGHT0 + Number, GL_DIFFUSE, @(DiffuseColor.FItems));
    glLightfv(GL_LIGHT0 + Number, GL_SPECULAR, @(SpecularColor.FItems));
    glLightfv(GL_LIGHT0 + Number, GL_POSITION, @(Position.FCoordinates));
    glLightfv(GL_LIGHT0 + Number, GL_SPOT_DIRECTION,
      @(SpotDirection.FCoordinates));
  end
  else
  begin
    glDisable(GL_LIGHT0 + Number)
  end;
end;

procedure TLight.SetSpotExponent(Value: TGLFloat);
begin
  if Value < 0 then
  begin
    Value := Abs(Value)
  end;
  if Value > 128 then
  begin
    Value := 128
  end;
  if FSpotExponent <> Value then
  begin
    FSpotExponent := Value;
    Change(Self);
  end;
end;

function TLight.GetSpotExponent: TGLFloat;
begin
  Result := FSpotExponent;
end;

procedure TLight.SetSpotCutoff(Value: TGLFloat);
begin
  if Value < 0 then
  begin
    Value := Abs(Value)
  end;
  if ((Value > 90) and (Value <> 180.0)) then
  begin
    Value := 90
  end;
  if FSpotCutoff <> Value then
  begin
    FSpotCutoff := Value;
    Change(Self);
  end;
end;

function TLight.GetSpotCutoff: TGLFloat;
begin
  Result := FSpotCutoff;
end;

procedure TLight.SetAttenuations(Index: Integer; Value: TGLFloat);
begin
  if Value < 0 then
  begin
    Value := 0
  end;
  if FAttenuations[Index] <> Value then
  begin
    FAttenuations[Index] := Value;
    Change(Self);
  end;
end;

function TLight.GetAttenuations(Index: Integer): TGLFloat;
begin
  Result := FAttenuations[Index];
end;

procedure TLight.Change(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

{ implementation TGLVector3Df }

procedure TGLVector3Df.SetCoordinates(Index: Integer; Value: TGLFloat);
begin
  if FCoordinates[Index] <> Value then
  begin
    FCoordinates[Index] := Value;
    Change;
  end;
end;

function TGLVector3Df.GetCoordinates(Index: Integer): TGLFloat;
begin
  Result := FCoordinates[Index];
end;

procedure TGLVector3Df.Change;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

{ end of implementation TGLVector3Df }

{ implementation TGLVector4Df }

procedure TGLVector4Df.SetCoordinates(Index: Integer; Value: TGLFloat);
begin
  if FCoordinates[Index] <> Value then
  begin
    FCoordinates[Index] := Value;
    Change;
  end;
end;

function TGLVector4Df.GetCoordinates(Index: Integer): TGLFloat;
begin
  Result := FCoordinates[Index];
end;

procedure TGLVector4Df.Change;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

{ end of implementation TGLVector4Df }

// ---------------------TGBMaterial Procedures---------------------//

constructor TGBMaterial.Create;
begin
  inherited Create;
  FAmbientColor := TGBColor.Create;
  FDiffuseColor := TGBColor.Create;
  FSpecularColor := TGBColor.Create;
  FEmissionColor := TGBColor.Create;
  FAmbientColor.OnChange := ColorChanged;
  FDiffuseColor.OnChange := ColorChanged;
  FSpecularColor.OnChange := ColorChanged;
  FEmissionColor.OnChange := ColorChanged;

  SetMaterialType(mtCyanPlastic);
end;

destructor TGBMaterial.Destroy;
begin
  FAmbientColor.Free;
  FDiffuseColor.Free;
  FSpecularColor.Free;
  FEmissionColor.Free;
  inherited Destroy;
end;

procedure TGBMaterial.ColorChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

procedure TGBMaterial.CallGLMaterial;
begin
  // glColor4f(0,0,0,1);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @(AmbientColor.FItems));
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @(DiffuseColor.FItems));
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @(SpecularColor.FItems));
  glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @(EmissionColor.FItems));
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, @(FShininess));
end;

procedure TGBMaterial.SetMaterialType(Value: TMaterialType);
const
  // ------Material List------//
  // Chrome
  ChromeAmbient: TGLRGBAfv = (0.25, 0.25, 0.25, 0);
  ChromeDiffuse: TGLRGBAfv = (0.4, 0.4, 0.4, 0);
  ChromeSpecular: TGLRGBAfv = (0.774597, 0.774597, 0.774597, 0);
  ChromeShininess: TGLFloat = 0.6;
  // Emerald
  EmeraldAmbient: TGLRGBAfv = (0.0215, 0.1745, 0.0215, 0);
  EmeraldDiffuse: TGLRGBAfv = (0.07568, 0.61424, 0.07568, 0);
  EmeraldSpecular: TGLRGBAfv = (0.633, 0.727811, 0.633, 0);
  EmeraldShininess: TGLFloat = 0.6;
  // Cyan Plastic
  CyanPlasticAmbient: TGLRGBAfv = (0, 0.1, 0.6, 0);
  CyanPlasticDiffuse: TGLRGBAfv = (0, 0.50980392, 0.50980392, 0);
  CyanPlasticSpecular: TGLRGBAfv = (0.50196078, 0.50196078, 0.50196078, 0);
  CyanPlasticShininess: TGLFloat = 0.25;
  // Brass
  BrassAmbient: TGLRGBAfv = (0.329412, 0.223529, 0.027451, 0);
  BrassDiffuse: TGLRGBAfv = (0.780392, 0.56827, 0.113725, 0);
  BrassSpecular: TGLRGBAfv = (0.992157, 0.941176, 0.807843, 0);
  BrassShininess: TGLFloat = 0.21794872;
  // Bronze
  BronzeAmbient: TGLRGBAfv = (0.2125, 0.1275, 0.054, 0);
  BronzeDiffuse: TGLRGBAfv = (0.714, 0.4284, 0.18144, 0);
  BronzeSpecular: TGLRGBAfv = (0.393548, 0.271906, 0.166721, 0);
  BronzeShininess: TGLFloat = 0.2;
  // Copper
  CopperAmbient: TGLRGBAfv = (0.19125, 0.0735, 0.0225, 0);
  CopperDiffuse: TGLRGBAfv = (0.7038, 0.27048, 0.0828, 0);
  CopperSpecular: TGLRGBAfv = (0.256777, 0.137622, 0.086104, 0);
  CopperShininess: TGLFloat = 0.1;
begin
  // If a redraw is done for each property update, setting a new material will
  // really be slow.
  // If the Material selected isn't mtCustom, set the properties
  case Value of
    mtChrome:
      begin
        AmbientColor.InitRGBAfv(ChromeAmbient);
        DiffuseColor.InitRGBAfv(ChromeDiffuse);
        SpecularColor.InitRGBAfv(ChromeSpecular);
        FShininess := ChromeShininess;
      end;

    mtEmerald:
      begin
        with AmbientColor do
        begin
          Red := EmeraldAmbient[0];
          Green := EmeraldAmbient[1];
          Blue := EmeraldAmbient[2];
          Alpha := EmeraldAmbient[3]
        end;

        with DiffuseColor do
        begin
          Red := EmeraldDiffuse[0];
          Green := EmeraldDiffuse[1];
          Blue := EmeraldDiffuse[2];
          Alpha := EmeraldDiffuse[3]
        end;

        with SpecularColor do
        begin
          Red := EmeraldSpecular[0];
          Green := EmeraldSpecular[1];
          Blue := EmeraldSpecular[2];
          Alpha := EmeraldSpecular[3]
        end;
        FShininess := EmeraldShininess;
      end;

    mtCyanPlastic:
      begin
        with AmbientColor do
        begin
          Red := CyanPlasticAmbient[0];
          Green := CyanPlasticAmbient[1];
          Blue := CyanPlasticAmbient[2];
          Alpha := CyanPlasticAmbient[3];
        end;
        with DiffuseColor do
        begin
          Red := CyanPlasticDiffuse[0];
          Green := CyanPlasticDiffuse[1];
          Blue := CyanPlasticDiffuse[2];
          Alpha := CyanPlasticDiffuse[3];
        end;
        with SpecularColor do
        begin
          Red := CyanPlasticSpecular[0];
          Green := CyanPlasticSpecular[1];
          Blue := CyanPlasticSpecular[2];
          Alpha := CyanPlasticSpecular[3];
        end;
        FShininess := CyanPlasticShininess;
      end;

    mtBrass:
      begin
        with AmbientColor do
        begin
          Red := BrassAmbient[0];
          Green := BrassAmbient[1];
          Blue := BrassAmbient[2];
          Alpha := BrassAmbient[3];
        end;
        with DiffuseColor do
        begin
          Red := BrassDiffuse[0];
          Green := BrassDiffuse[1];
          Blue := BrassDiffuse[2];
          Alpha := BrassDiffuse[3];
        end;
        with SpecularColor do
        begin
          Red := BrassSpecular[0];
          Green := BrassSpecular[1];
          Blue := BrassSpecular[2];
          Alpha := BrassSpecular[3];
        end;
        FShininess := BrassShininess;
      end;
    mtBronze:
      begin
        with AmbientColor do
        begin
          Red := BronzeAmbient[0];
          Green := BronzeAmbient[1];
          Blue := BronzeAmbient[2];
          Alpha := BronzeAmbient[3];
        end;
        with DiffuseColor do
        begin
          Red := BronzeDiffuse[0];
          Green := BronzeDiffuse[1];
          Blue := BronzeDiffuse[2];
          Alpha := BronzeDiffuse[3];
        end;
        with SpecularColor do
        begin
          Red := BronzeSpecular[0];
          Green := BronzeSpecular[1];
          Blue := BronzeSpecular[2];
          Alpha := BronzeSpecular[3];
        end;
        FShininess := BrassShininess;
      end;

    mtCopper:
      begin
        with AmbientColor do
        begin
          Red := CopperAmbient[0];
          Green := CopperAmbient[1];
          Blue := CopperAmbient[2];
          Alpha := CopperAmbient[3];
        end;
        with DiffuseColor do
        begin
          Red := CopperDiffuse[0];
          Green := CopperDiffuse[1];
          Blue := CopperDiffuse[2];
          Alpha := CopperDiffuse[3];
        end;
        with SpecularColor do
        begin
          Red := CopperSpecular[0];
          Green := CopperSpecular[1];
          Blue := CopperSpecular[2];
          Alpha := CopperSpecular[3];
        end;
        FShininess := CopperShininess;
      end;
  end;

  EmissionColor.Red := 0;
  EmissionColor.Green := 0;
  EmissionColor.Blue := 0;
  EmissionColor.Alpha := 0;

  FMaterial := Value;

  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

procedure TGBMaterial.SetMaterialShininess(Value: TGLFloat);
begin
  if Value <> FShininess then
  begin
    FShininess := Value;
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
      FMaterial := mtCustom;
    end;
  end;
end;

// ---------------------TGBCanvas Procedures---------------------//

procedure TGBCanvas.SetDCPixelFormat;
var
  nPixelFormat: Integer;
var
  pfd: TPixelFormatDescriptor;
begin
  with pfd do
  begin
    nSize := SizeOf(pfd); // Size of this structure
    nVersion := 1; // Version number
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL
    { or
      PFD_SWAP_COPY{ } or PFD_DOUBLEBUFFER; // Flags
    iPixelType := PFD_TYPE_RGBA; // RGBA pixel values
    cColorBits := 32; // 32-bit Color
    cDepthBits := 32; // 32-bit Depth Z buffer
    iLayerType := PFD_MAIN_PLANE; // Layer type
  end;

  DC := GetDC(Handle);
  nPixelFormat := ChoosePixelFormat(DC, @pfd); // Delay is here!
  // nPixelFormat := 11;
  SetPixelFormat(DC, nPixelFormat, @pfd);
  DescribePixelFormat(DC, nPixelFormat, SizeOf(TPixelFormatDescriptor), pfd);
end;

constructor TGBCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FirstTime := True;
  // Width := 300;  Height := 300;
  FInitialRotation := TInitialRotation.Create;
  FInitialRotation.Roll := 0;
  FInitialRotation.Pitch := 90;
  FInitialRotation.Yaw := 0;
  FInitialRotation.OnChange := nil;
  FRollAngle := 0;
  FPitchAngle := 0;
  FYawAngle := 0;
  Axes := TAxes.Create;
  Axes.OnChange := SetAxes;
  FLight := TLight.Create;
  FLight.OnChange := LightChange;
  FMaterial := TGBMaterial.Create;
  FMaterial.OnChange := SetMaterial;
  FRotationSequence := RPY;
  FProjectionMode := pmPerspective;
  FViewAngle := 30;
  FViewMode := vmXYZ;
  FColor := clBlack;
  Font.OnChange := FontChange;
  FDrawCursor := crDefault;
  FMouseWheelStep := 1;
  ConstructorFlag := True;
  SetMaterial(Self);

  FPickWidth := 1;
  FPickHeight := 1;

  kX := 4;
  kY := 2;
  kZ := 1;

  OnMouseMove := DefaultMouseMove;
  OnDblClick := DefaultMouseDblClick;
  OnKeyDown := DefaultKeyDown;

  SetBounds3D(-100, 100, -100, 100, -100, 100);

  ConstructorFlag := False;
  if csDesigning in ComponentState then
  begin
    Designing := True
  end
  else
  begin
    Designing := False
  end;
end;

destructor TGBCanvas.Destroy;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(hrc);
  InitialRotation.Free;
  Axes.Free;
  FLight.Free;
  FMaterial.Free;
  FSelectPrimitivesArray := nil;
  if LastCanvas = Self then
  begin
    LastCanvas := nil
  end;
  inherited;
end;

procedure TGBCanvas.Change(Sender: TObject);
begin
  if Assigned(OnChange) then
  begin
    OnChange(Self)
  end;
end;

procedure TGBCanvas.SetNearZ(Value: TGLFloat);
begin
  if FNearZ <> Value then
  begin
    FNearZ := Value;
    Redraw;
    Change(Self);
  end;
end;

procedure TGBCanvas.SetViewAngle(Value: TGLFloat);
begin
  if ViewAngle <> Value then
  begin
    FViewAngle := Value;
    Redraw;
    Change(Self);
  end;
end;

function TGBCanvas.GetNearZ: TGLFloat;
begin
  Result := FNearZ;
end;

procedure TGBCanvas.SetFarZ(Value: TGLFloat);
begin
  if FFarZ <> Value then
  begin
    FFarZ := Value;
    Redraw;
    Change(Self);
  end;
end;

function TGBCanvas.GetFarZ: TGLFloat;
begin
  Result := FFarZ;
end;

procedure TGBCanvas.SetAxes(Sender: TObject);
begin
  // Redo the Axis List and Redraw
  CreateAxisList;
  if not ConstructorFlag then
  begin
    if Designing then
    begin
      Paint
    end
  end;
end;

procedure TGBCanvas.SetMaterial;
begin
  // Copy the material properties into the arrays, recreate the display list, and redraw
  // glColor4f(0,0,0,1);
  with FMaterial do
  begin
    with AmbientColor do
    begin
      MaterialAmbient[0] := Red;
      MaterialAmbient[1] := Green;
      MaterialAmbient[2] := Blue;
      MaterialAmbient[3] := Alpha;
    end;

    with DiffuseColor do
    begin
      MaterialDiffuse[0] := DiffuseColor.Red;
      MaterialDiffuse[1] := DiffuseColor.Green;
      MaterialDiffuse[2] := DiffuseColor.Blue;
      MaterialDiffuse[3] := DiffuseColor.Alpha;
    end;

    with SpecularColor do
    begin
      MaterialSpecular[0] := Red;
      MaterialSpecular[1] := Green;
      MaterialSpecular[2] := Blue;
      MaterialSpecular[3] := Alpha;
    end;

    with EmissionColor do
    begin
      MaterialEmission[0] := Red;
      MaterialEmission[1] := Green;
      MaterialEmission[2] := Blue;
      MaterialEmission[3] := Alpha;
    end;

    MaterialShininess := Shininess;
  end;

  if not ConstructorFlag then
  begin
    if Designing then
    begin
      Paint
    end
  end;
end;

procedure TGBCanvas.LightChange;
begin
  // Copy the material properties into the arrays, recreate the display list, and redraw
  if not ConstructorFlag then
  begin
    Redraw
  end;
end;

procedure TGBCanvas.SetGLClearColor(Value: TColor);
var
  RedLevel, GreenLevel, BlueLevel: Integer;
  Redfloat, GreenFloat, BlueFloat: Single;
begin
  if FColor <> Value then
  begin
    FColor := Value;
    // Pick out the components
    RedLevel := Value and $000000FF;
    GreenLevel := (Value and $0000FF00) shr 8;
    BlueLevel := (Value and $00FF0000) shr 16;
    // Convert to Floats
    Redfloat := RedLevel;
    GreenFloat := GreenLevel;
    BlueFloat := BlueLevel;
    // Set the Color
    glClearColor(Redfloat / 255, GreenFloat / 255, BlueFloat / 255, 1);
    Redraw;
  end;
end;

procedure TGBCanvas.CallGLClearColor;
var
  RedLevel, GreenLevel, BlueLevel: Integer;
  Redfloat, GreenFloat, BlueFloat: Single;
begin
  // Pick out the components
  RedLevel := Color and $000000FF;
  GreenLevel := (Color and $0000FF00) shr 8;
  BlueLevel := (Color and $00FF0000) shr 16;
  // Convert to Floats
  Redfloat := RedLevel;
  GreenFloat := GreenLevel;
  BlueFloat := BlueLevel;
  // Set the Color
  glClearColor(Redfloat / 255, GreenFloat / 255, BlueFloat / 255, 1);
end;

procedure TGBCanvas.SetRollAngle(Value: Double);
begin
  if FRollAngle <> Value then
  begin
    FRollAngle := Value;
    if not ConstructorFlag then
    begin
      if Designing then
      begin
        Paint
      end
    end;
  end;
end;

procedure TGBCanvas.SetPitchAngle(Value: Double);
begin
  if FPitchAngle <> Value then
  begin
    FPitchAngle := Value;
    if not ConstructorFlag then
    begin
      if Designing then
      begin
        Paint
      end
    end;
  end;
end;

procedure TGBCanvas.SetYawAngle(Value: Double);
begin
  if FYawAngle <> Value then
  begin
    FYawAngle := Value;
    if not ConstructorFlag then
    begin
      if Designing then
      begin
        Paint
      end
    end;
  end;
end;

procedure TGBCanvas.Redraw;
begin
  // Perform(WM_Paint, 0, 0);
  Paint;
end;

var
  FontStartGLList: TGLEnum = 1000;

procedure TGBCanvas.InitOpenGl;
begin
  FirstTime := False;
  // Create a Rendering context.
  wglMakeCurrent(0, 0);
  wglDeleteContext(hrc);

  SetDCPixelFormat; // All delay is here!
  hrc := wglCreateContext(DC);
  LastCanvas := Self;
  wglMakeCurrent(DC, hrc);
  SetGLClearColor(FColor);
  // Enable depth testing and backface culling.
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL); // GL_GREATER //GL_LEQUAL//GL_LESS
  // glEnable(GL_CULL_FACE);
  glDisable(GL_CULL_FACE);
  glEnable(GL_COLOR_MATERIAL);
  // glCullFace(GL_BACK);
  // glFrontFace(GL_CW);
  // glCullFace(GL_FRONT);
  // Enable the Lighting
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  // glLightModelf(GL_LIGHT_MODEL_TWO_SIDE,1);
  glShadeModel(GL_SMOOTH);
  //
  glDisable(GL_ALPHA_TEST); {
    glEnable(GL_ALPHA_TEST);{ }
  // Create the Display List
end;

procedure TGBCanvas.Paint;
var
  OldCursor: TCursor;
  I: Integer;
  StartTime: Integer;
begin
  if (csLoading in ComponentState) then
  begin
    Exit
  end;
  if (csDestroying in ComponentState) then
  begin
    Exit
  end;

  StartTime := GetTickCount;
  OldCursor := Screen.Cursor;
  try
    if DrawCursor <> crDefault then
    begin
      Screen.Cursor := DrawCursor
    end;

    if FirstTime then
    begin
      InitOpenGl
    end;
    MakeCurrent;

    CallGLClearColor;
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    SetProjection;
    // Material properties

    Material.CallGLMaterial;
    glShadeModel(GL_SMOOTH);

    Lighting.CallGLLight;

    if Assigned(OnCallLists) then
    begin
      OnCallLists(Self)
    end;

    // Show the Axes?
    if Axes.FVisible then
    begin
      // All the material properties are zero at this point
      // Exit plane being black and the axes are emissive only
      for I := 0 to 3 do
      begin
        PitchAxisEmission[I] := Axes.PitchAxisEmission[I];
        YawAxisEmission[I] := Axes.YawAxisEmission[I]
      end;
      glMaterialfv(GL_FRONT, GL_EMISSION, @PitchAxisEmission);
      glCallList(PitchAxisList);
      glMaterialfv(GL_FRONT, GL_EMISSION, @YawAxisEmission);
      glCallList(YawAxisList);
    end;

    Windows.SwapBuffers(DC);
  finally
    Screen.Cursor := OldCursor;
  end;
  StartTime := GetTickCount - StartTime;
  if StartTime = 0 then
  begin
    StartTime := 1
  end;
  FPS := Round(1000 / StartTime);
end;

const
  BUFSIZE = 1024000;

function TGBCanvas.SelectPrimitives(X, Y: TGLint): TArrayOfDrawPrimitive;

{ sub } function Inc1(var N: Integer): Integer;
  begin
    Inc(N);
    Result := N;
  end;

var
  Hits: TGLint;
  SelectBuf: TArrayOfGLuint;

  procedure processHits;

    function IndexOf(Arr: TArrayOfDrawPrimitive; Value: TDrawPrimitive)
      : Integer;
    var
      I, J: Integer;
      NameCount: Integer;
    begin
      Result := -1;
      NameCount := Length(Value.IDs);
      for I := High(Arr) downto Low(Arr) do
      begin
        if NameCount = Length(Arr[I].IDs) then
        begin
          for J := Low(Value.IDs) to High(Value.IDs) do
          begin
            if Value.IDs[J] <> Arr[I].IDs[J] then
            begin
              Break
            end;
            if J = High(Value.IDs) then
            begin
              Result := I
            end;
          end;
          if Result <> -1 then
          begin
            Exit
          end; // We found an element with the same list of IDs
        end;
      end;
    end;

    procedure AddItem(var Arr: TArrayOfDrawPrimitive; Value: TDrawPrimitive);
    var
      I: Integer;
    begin
      SetLength(Arr, Length(Arr) + 1);
      with Arr[High(Arr)] do
      begin
        Z := Value.Z;
        SetLength(IDs, Length(Value.IDs));
        for I := Low(Value.IDs) to High(Value.IDs) do
        begin
          IDs[I] := Value.IDs[I]
        end;
      end;
    end;

  var
    I, J, k, NameCount: Integer;
    Ptr: TArrayOfGLuint;
    Primitive: TDrawPrimitive;
    PrimitiveCount: Integer;
  begin
    Ptr := SelectBuf;
    k := 0;
    SetLength(Result, Hits);
    PrimitiveCount := 0;
    for I := 0 to Hits - 1 do
    begin (* for each hit *)
      NameCount := Ptr[k];
      Primitive.Z := (Ptr[Inc1(k)] / High(TGLuint) + Ptr[Inc1(k)] /
        High(TGLuint)) / 2;
      SetLength(Primitive.IDs, NameCount);
      for J := 0 to NameCount - 1 do
      begin
        Primitive.IDs[J] := Ptr[Inc1(k)];
      end;
      try
        if not RejectPrimitive(X, Y, Primitive) then
        begin
          Result[PrimitiveCount] := Primitive;
          Inc(PrimitiveCount);
        end;
      except
        Application.HandleException(Self);
      end;
      Inc1(k); // go to next promitive if it exists
    end;
    SetLength(Result, PrimitiveCount);

    if AppendPrimitives then // write
    begin
      for I := 0 to High(FSelectPrimitivesArray) do
      begin
        if Length(FSelectPrimitivesArray[I].IDs) > 0 then
        begin
          if (not RejectPrimitive(X, Y, Primitive)) and
            (IndexOf(Result, FSelectPrimitivesArray[I]) < 0) then
          begin
            AddItem(Result, FSelectPrimitivesArray[I])
          end
          else
          begin
            SetLength(Result[I].IDs, 0)
          end;
        end;
      end
    end;
    FSelectPrimitivesArray := nil;
    FSelectPrimitivesArray := Result;
  end;

begin
  SetLength(SelectBuf, BUFSIZE);
  try
    glSelectBuffer(BUFSIZE, @SelectBuf[0]);
    RenderMode := rmSELECT;
    glRenderMode(GL_SELECT);
    SetProjection;

    glInitNames;
    // glPushName(TGLUint(-1+01));

    if Assigned(OnCallLists) then
    begin
      OnCallLists(Self)
    end;
    // glPopName;
    glFinish;

    Hits := glRenderMode(GL_RENDER);
    RenderMode := rmRENDER;
    // if Hits*6<High(Result)+1 then SetLength(Result, Hits*6);
    processHits;
    try
      if Assigned(OnSelectPrimitives) then
      begin
        FOnSelectPrimitives(Self, X, Y, Result)
      end;
    except
    end;
  except
    Result := nil;
  end;
end;

procedure TGBCanvas.CreateAxisList;
// This procedure sets up the axis list
var
  I: Integer;
  Theta, Delta: Double;
begin
  with Axes do
  begin
    // Pitch
    Delta := 120 * pi / 180;
    for I := 0 to 2 do
    begin
      Theta := I * 120 * pi / 180;
      glBegin(GL_POLYGON);
      glNormal3f(0, -Sin(Theta + pi / 3), Cos(Theta + pi / 3));
      glVertex3f(AxisLength, AxisRadius * Sin(Theta), AxisRadius * Cos(Theta));
      glVertex3f(0, AxisRadius * Sin(Theta), AxisRadius * Cos(Theta));
      glVertex3f(0, AxisRadius * Sin(Theta + Delta),
        AxisRadius * Cos(Theta + Delta));
      glVertex3f(AxisLength, AxisRadius * Sin(Theta + Delta),
        AxisRadius * Cos(Theta + Delta));
      glEnd;
    end;

    glBegin(GL_POLYGON);
    glNormal3f(1, 0, 0);
    glVertex3f(AxisLength, 0, AxisRadius);
    glVertex3f(AxisLength, 0.866 * AxisRadius, -0.5 * AxisRadius);
    glVertex3f(AxisLength, -0.866 * AxisRadius, -0.5 * AxisRadius);
    glEnd;

    // Yaw
    Delta := 120 * pi / 180;
    for I := 0 to 2 do
    begin
      Theta := I * 120 * pi / 180;
      glBegin(GL_POLYGON);
      glNormal3f(Sin(Theta + pi / 3), 0, Cos(Theta + pi / 3));
      glVertex3f(AxisRadius * Sin(Theta), -AxisLength, AxisRadius * Cos(Theta));
      glVertex3f(AxisRadius * Sin(Theta), 0, AxisRadius * Cos(Theta));
      glVertex3f(AxisRadius * Sin(Theta + Delta), 0,
        AxisRadius * Cos(Theta + Delta));
      glVertex3f(AxisRadius * Sin(Theta + Delta), -AxisLength,
        AxisRadius * Cos(Theta + Delta));
      glEnd;
    end;

    glBegin(GL_POLYGON);
    glNormal3f(0, 1, 0);
    glVertex3f(0, -AxisLength, AxisRadius);
    glVertex3f(0.866 * AxisRadius, -AxisLength, -0.5 * AxisRadius);
    glVertex3f(-0.866 * AxisRadius, -AxisLength, -0.5 * AxisRadius);
    glEnd;
  end;
end;

// -------------------------------------------------------------------\\
// Result: Vector Cross Product n = a x b                            \\
// -------------------------------------------------------------------\\
procedure Cross(Ax, Ay, Az, bx, by, bz, cX, cY, cz: Double;
  var nx, ny, nz: Double);
var
  vx, vy, vz, wX, wY, wZ, Z: Double;
begin
  vx := Ax - bx;
  vy := Ay - by;
  vz := Az - bz;

  wX := bx - cX; // |i  j  k |
  wY := by - cY;
  // |vx vy vz| = (vy*wz-vz*wy)*i - (vx*wz-vz*wx)*j + (vx*wy-vy*wx)*k
  wZ := bz - cz; // |wx wy wz|
  //
  nx := vy * wZ - vz * wY;
  ny := vz * wX - vx * wZ;
  nz := vx * wY - vy * wX;

  Z := sqrt(nx * nx + ny * ny + nz * nz);
  try
    nx := nx / Z;
    ny := ny / Z;
    nz := nz / Z;
  except
    nx := 0;
    ny := 0;
    nz := 1;
  end;
end;

procedure TGBCanvas.MakeCurrent;
begin
  LastCanvas := Self;
  if FirstTime then
  begin
    InitOpenGl
  end;
  // DC:=GetDC(Handle);
  wglMakeCurrent(DC, hrc);
end;

procedure TGBCanvas.SwapBuffers;
begin
  if FirstTime then
  begin
    InitOpenGl
  end;
  Windows.SwapBuffers(DC);
end;

procedure TGBCanvas.DefaultKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
const
  DAX = 1;
  DAY = 1;
  DAZ = 1;
var
  k: Double;
  DV: Double;
  V: TVector3f;
begin
  V.X := Lx;
  V.Y := Ly;
  V.Z := Lz;
  DV := VectorNorm(V) / 100;

  if ssShift in Shift then
  begin
    k := 10
  end
  else
  begin
    k := 1
  end;
  if ssCtrl in Shift then
  begin
    k := 0.1
  end;
  case Key of
    VK_F1:
      begin
        Application.HelpContext(HelpContext)
      end;
    VK_HOME:
      begin
        ViewDefault;
      end;
    VK_ADD:
      begin
        wVectorZ := wVectorZ - k * Abs(wVectorZ) / 100 - DV
      end;
    VK_SUBTRACT:
      begin
        wVectorZ := wVectorZ + k * Abs(wVectorZ) / 100 + DV
      end;
    { VK_ADD     : begin
      wVectorX:=CenterX-(CenterX - wVectorX) / (k*0.09+1);
      wVectorY:=CenterY-(CenterY - wVectorY) / (k*0.09+1);
      wVectorZ:=CenterZ-(CenterZ - wVectorZ) / (k*0.09+1);
      end;
      VK_SUBTRACT: begin
      wVectorX:=CenterX-(CenterX - wVectorX) * (k*0.09+1);
      wVectorY:=CenterY-(CenterY - wVectorY) * (k*0.09+1);
      wVectorZ:=CenterZ-(CenterZ - wVectorZ) * (k*0.09+1);
      end; }
  else
    begin
      if ssAlt in Shift then
      begin
        case Key of
          VK_LEFT:
            begin
              wVectorX := wVectorX - k * DV
            end; // strife
          VK_RIGHT:
            begin
              wVectorX := wVectorX + k * DV
            end;
          VK_UP:
            begin
              wVectorY := wVectorY + k * DV
            end;
          VK_DOWN:
            begin
              wVectorY := wVectorY - k * DV
            end;
        else
          begin
            Exit
          end;
        end
      end
      else if ProjectionMode = pmPerspective then // rotate
      //
      {
        case Key of
        VK_LEFT: begin
        wAngleZ:= wAngleZ - K*DAZ;
        if wAngleZ<-360 then wAngleZ:=wAngleZ+360;
        end;
        VK_RIGHT: begin
        wAngleZ:= wAngleZ + K*DAZ;
        if wAngleZ>360 then wAngleZ:=wAngleZ-360;
        end;
        VK_UP: begin
        wAngleX:= wAngleX - K*DAX;
        if wAngleX<-360 then wAngleX:=wAngleX+360;
        end;
        VK_DOWN: begin
        wAngleX:= wAngleX + K*DAX;
        if wAngleX>360 then wAngleX:=wAngleX-360;
        end;
        else Exit;
        end(*{ }
      begin
        case Key of
          Ord('A'), Ord('a'):
            begin
              wAngleZ := wAngleZ - k * DAZ;
              if wAngleZ < -360 then
              begin
                wAngleZ := wAngleZ + 360
              end;
            end;
          Ord('D'), Ord('d'):
            begin
              wAngleZ := wAngleZ + k * DAZ;
              if wAngleZ > 360 then
              begin
                wAngleZ := wAngleZ - 360
              end;
            end;
          Ord('W'), Ord('w'):
            begin
              wAngleX := wAngleX - k * DAX;
              if wAngleX < -360 then
              begin
                wAngleX := wAngleX + 360
              end;
            end;
          Ord('S'), Ord('s'):
            begin
              wAngleX := wAngleX + k * DAX;
              if wAngleX > 360 then
              begin
                wAngleX := wAngleX - 360
              end;
            end;
        else
          begin
            Exit
          end;
        end
      end (* *)
      else
      begin
        Exit
      end
    end;
  end;
  Redraw;
end;

procedure TGBCanvas.ViewScale;
begin
  //
end;

procedure TGBCanvas.ViewAxis;
begin
  //
end;

procedure TGBCanvas.ViewTop;
begin
  wAngleX := 360 - 90;
  wAngleY := 0;
  wAngleZ := 0;
  ViewMode := vmXY;
  Redraw;
end;

procedure TGBCanvas.ViewBottom;
begin
  wAngleX := 90;
  wAngleY := 0;
  wAngleZ := 0;
  ViewMode := vmXYZ;
  Redraw;
end;

procedure TGBCanvas.ViewFront;
begin
  wAngleX := 0;
  wAngleY := 0;
  wAngleZ := 0;
  ViewMode := vmXZ;
  Redraw;
end;

procedure TGBCanvas.ViewBack;
begin
  wAngleX := 0;
  wAngleY := 0;
  wAngleZ := 180;
  ViewMode := vmXYZ;
  Redraw;
end;

procedure TGBCanvas.ViewRight;
begin
  wAngleX := 0;
  wAngleY := 0;
  wAngleZ := 90;
  ViewMode := vmYZ;
  Redraw;
end;

procedure TGBCanvas.ViewLeft;
begin
  wAngleX := 0;
  wAngleY := 0;
  wAngleZ := -90;
  ViewMode := vmXYZ;
  Redraw;
end;

procedure TGBCanvas.ViewRotate;
begin
  ViewMode := vmXYZ;
end;

procedure TGBCanvas.View3D;
begin
  wAngleX := -20;
  wAngleY := 0;
  wAngleZ := -20;
  ViewMode := vmXYZ;
  Redraw;
end;

procedure TGBCanvas.ViewDefault;
var
  RatioF: TGLFloat;
begin
  ViewMode := vmXYZ;

  wVectorX := 0;
  wVectorY := 0;
  // DefwVectorZ:=-CenterZ;
  if (ClientHeight = 0) { or (ClientWidth>ClientHeight){ } then
  begin
    RatioF := 1
  end
  else
  begin
    RatioF := Max(ClientHeight, ClientWidth) / Min(ClientHeight, ClientWidth)
  end;

  wVectorZ := RatioF * Norm([Lx, Ly, Lz]) / (2 * Tan(DegToRad(ViewAngle / 2)));

end;

procedure TGBCanvas.ZoomIn;
begin
  wVectorZ := wVectorZ + 100;
end;

procedure TGBCanvas.ZoomOut;
begin
  wVectorZ := wVectorZ - 100;
end;

procedure TGBCanvas.SetMinX(Value: Double);
begin
  if FMinX <> Value then
  begin
    FMinX := Value;
    CenterX := (MinX + MaxX) / 2;
    Lx := MaxX - MinX;
  end;
end;

function TGBCanvas.GetMinX: Double;
begin
  Result := FMinX;
end;

procedure TGBCanvas.SetMaxX(Value: Double);
begin
  if FMaxX <> Value then
  begin
    FMaxX := Value;
    CenterX := (MinX + MaxX) / 2;
    Lx := MaxX - MinX;
  end;
end;

function TGBCanvas.GetMaxX: Double;
begin
  Result := FMaxX;
end;

procedure TGBCanvas.SetMinY(Value: Double);
begin
  if FMinY <> Value then
  begin
    FMinY := Value;
    CenterY := (MinY + MaxY) / 2;
    Ly := MaxY - MinY;
  end;
end;

function TGBCanvas.GetMinY: Double;
begin
  Result := FMinY;
end;

procedure TGBCanvas.SetMaxY(Value: Double);
begin
  if FMaxY <> Value then
  begin
    FMaxY := Value;
    CenterY := (MinY + MaxY) / 2;
    Ly := MaxY - MinY;
  end;
end;

function TGBCanvas.GetMaxY: Double;
begin
  Result := FMaxY;
end;

procedure TGBCanvas.SetMinZ(Value: Double);
begin
  if FMinZ <> Value then
  begin
    FMinZ := Value;
    CenterZ := (MinZ + MaxZ) / 2;
    Lz := MaxZ - MinZ;
  end;
end;

function TGBCanvas.GetMinZ: Double;
begin
  Result := FMinZ;
end;

procedure TGBCanvas.SetMaxZ(Value: Double);
begin
  if FMaxZ <> Value then
  begin
    FMaxZ := Value;
    CenterZ := (MinZ + MaxZ) / 2;
    Lz := MaxZ - MinZ;
  end;
end;

function TGBCanvas.GetMaxZ: Double;
begin
  Result := FMaxZ;
end;

{ implementation of TGBLightModel }

constructor TGBLightModel.Create;
begin
  inherited;
  FLocalViewer := Boolean(GL_FALSE);
  FTwoSide := Boolean(GL_FALSE);
  FOnChange := nil;
  FAmbient := TGBColor.Create;
  FAmbient.InitRGBAf(0.2, 0.2, 0.2, 1.0);
  FAmbient.OnChange := Change;
end;

destructor TGBLightModel.Destroy;
begin
  FAmbient.Free;
  inherited;
end;

procedure TGBLightModel.SetEnable(Value: Boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
    Change(Self);
  end;
end;

function TGBLightModel.GetEnable: Boolean;
begin
  Result := FEnable;
end;

procedure TGBLightModel.SetLocalViewer(Value: Boolean);
begin
  if FLocalViewer <> Value then
  begin
    FLocalViewer := Value;
    Change(Self);
  end;
end;

function TGBLightModel.GetLocalViewer: Boolean;
begin
  Result := FLocalViewer;
end;

procedure TGBLightModel.SetTwoSide(Value: Boolean);
begin
  if FTwoSide <> Value then
  begin
    FTwoSide := Value;
    Change(Self);
  end;
end;

function TGBLightModel.GetTwoSide: Boolean;
begin
  Result := FTwoSide;
end;

procedure TGBLightModel.CallGLLightModel;
var
  I, MaxLights: TGLint;
  AmbientColor: TGLRGBAfv;
begin
  if Enable then
  begin
    glEnable(GL_LIGHTING)
  end
  else
  begin
    glGetIntegerv(GL_MAX_LIGHTS, @MaxLights);
    for I := 0 to MaxLights - 1 do
    begin
      glDisable(GL_LIGHT0 + I)
    end;
    glDisable(GL_LIGHTING);
  end;
  AmbientColor := Ambient.GetRGBAfv;
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, TGLint(LocalViewer));
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, TGLint(TwoSide));
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @AmbientColor);
end;

procedure TGBLightModel.Change(Sender: TObject);
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self)
  end;
end;

{ end implementation of TGBLightModel }

procedure TGBCanvas.CanvasToWorld(cX, cY: Integer; var wX, wY, wZ: TGLDouble);
begin
  {
    TGLint viewport[4];
    TGLDouble mvmatrix[16], projmatrix[16];
    TGLint realy;  /*  OpenGL y coordinate position  */
    TGLDouble wx, wy, wz;  /*  returned world x, y, z coords  */

    switch (button) {
    case GLUT_LEFT_BUTTON:
    if (state == GLUT_DOWN) Begin
    glGetIntegerv (GL_VIEWPORT, viewport);
    glGetDoublev (GL_MODELVIEW_MATRIX, mvmatrix);
    glGetDoublev (GL_PROJECTION_MATRIX, projmatrix);
    /*  note viewport[3] is height of window in pixels  */
    realy = viewport[3] - (TGLint) y - 1;
    printf ("Coordinates at cursor are (%4d, %4d)\n", x, realy);
    gluUnProject ((TGLDouble) x, (TGLDouble) realy, 0.0,
    mvmatrix, projmatrix, viewport, &wx, &wy, &wz);
    printf ("World coords at z=0.0 are (%f, %f, %f)\n",
    wx, wy, wz);
    gluUnProject ((TGLDouble) x, (TGLDouble) realy, 1.0,
    mvmatrix, projmatrix, viewport, &wx, &wy, &wz);
    printf ("World coords at z=1.0 are (%f, %f, %f)\n",
    wx, wy, wz);
    end;
    break;
    case GLUT_RIGHT_BUTTON:
    if (state == GLUT_DOWN)
    exit(0);
    break;
    default:
    break;
    end;
  }
end;

procedure TGBCanvas.DefaultMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const
  Sign: array [Boolean] of Double = (1, -1);
var
  k: Double;
  DV: Double;
  NeedPaint: Boolean;
begin
  NeedPaint := False;
  k := 1;
  DV := Norm([MaxX - MinX, MaxY - MinY, MaxZ - MinZ]) / 200;
  if ssShift in Shift then
  begin
    k := 10
  end
  else if ssCtrl in Shift then
  begin
    k := 0.1
  end;

  if (ssAlt in Shift) and (ssLeft in Shift) then
  begin
    wVectorX := wVectorX + DV * k * (X - StartX); // strife
    wVectorY := wVectorY - DV * k * (Y - StartY);
    NeedPaint := True;
  end
  else if ssLeft in Shift then
  begin
    wAngleX := wAngleX - k * (Y - StartY);
    wAngleZ := wAngleZ + k * (X - StartX);
    NeedPaint := True;
  end;

  if ssRight in Shift then
  begin
    wVectorZ := wVectorZ - Sign[Y - StartY < 0] *
      Max(k, k * Abs(wVectorZ / 100 * (Y - StartY)));
    NeedPaint := True;
  end;

  CursorX := X;
  CursorY := Y;
  StartX := X;
  StartY := Y;
  if NeedPaint then
  begin
    Redraw
  end;
end;

procedure TGBCanvas.DeleteHGLRC;
begin
  if LastCanvas = Self then
  begin
    LastCanvas := nil
  end;
  wglMakeCurrent(0, 0);
  wglDeleteContext(hrc);
  FirstTime := True;
end;

procedure TGBCanvas.Polygon(const Points: array of TVertex);
var
  I: Integer;
  Vertex: TVertex;
  nx, ny, nz: Double;
begin
  if High(Points) > 1 then
  begin
    try
      Cross(Points[2].X, Points[2].Y, Points[2].Z, Points[1].X, Points[1].Y,
        Points[1].Z, Points[0].X, Points[0].Y, Points[0].Z, nx, ny, nz);
      glNormal3d(nx, ny, nz);
    except
    end
  end;
  gluBeginPolygon(Triangulator);
  for I := 0 to High(Points) do
  begin
    Vertex := Points[I];
    gluTessVertex(Triangulator, Vertex, @Points[I]);
  end;
  gluEndPolygon(Triangulator);
end;

function TGBCanvas.GetTriangulator: PGLUtriangulatorObj;
begin
  if FTriangulator = nil then
  begin
    FTriangulator := gluNewTess;
    gluTessCallback(FTriangulator, GLU_BEGIN, @glBegin);
    gluTessCallback(FTriangulator, GLU_VERTEX, @glVertex3dv);
    gluTessCallback(FTriangulator, GLU_END, @glEnd);
  end;
  Result := FTriangulator;
end;

procedure TGBCanvas.SetTriangulator(const Value: PGLUtriangulatorObj);
begin
  if FTriangulator <> nil then
  begin
    gluDeleteTess(FTriangulator)
  end;
  FTriangulator := Value;
end;

procedure TGBCanvas.SetProjection;
var
  Ratio, Diameter, radius, cX, cY, cz: Real;
  // Scale : Double;
  PixelPerMeter: Double;
begin
  ViewPort.X := 0;
  ViewPort.Y := 0;
  ViewPort.Z := Width;
  ViewPort.W := Height;
  glViewport(ViewPort.X, ViewPort.Y, ViewPort.Z, ViewPort.W);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  case RenderMode of
    rmSELECT:
      begin
        gluPickMatrix(CursorX, Height - CursorY, PickWidth,
          PickHeight, ViewPort)
      end;
  end;

  Diameter := Min(5 * Norm([Lx, Ly, Lz]), 1000);
  if ClientRect.Bottom = 0 then
  begin
    Ratio := 1
  end
  else
  begin
    Ratio := ClientRect.Right / ClientRect.Bottom
  end;

  case ProjectionMode of
    pmOrtho:
      begin
        PixelPerMeter := { Screen.PixelsPerInch } 80 * 40.816326530;
        radius := wVectorZ * (Width / PixelPerMeter) / 2;
        // glOrtho(MinX,MaxX,MinY,MaxY,MinZ,MaxZ);
        glOrtho(CenterX - radius, CenterX + radius, CenterY - radius,
          CenterY + radius, 0, Diameter);
        { CenterZ-1000,CenterZ+1000);{ }
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;

        // Add the Lighting
        // Lighting.CallGLLight;

        // glScaled(wVectorZ,wVectorZ,wVectorZ);
        glTranslateF(wVectorX, wVectorY, -Diameter / 2);

        // Rotation around a point with CenterX,CenterY,CenterZ)
        // {
        cX := CenterX; // rotate Object around (CenterX,CenterY,CenterZ)
        cY := CenterY;
        cz := CenterZ; (* }

          CX:=-wVectorX; //rotate Object around Me
          CY:=-wVectorY;
          CZ:=-wVectorZ;(* *)

        glTranslateF(cX, cY, cz);
        glScaled(1, Ratio, 1);

        glRotateF(wAngleX + 90, -1, 0, 0);
        glRotateF(wAngleY, 0, 1, 0);
        glRotateF(wAngleZ, 0, 0, 1); // no rotate

        glTranslateF(-cX, -cY, -cz); // ratate Object (CenterX,CenterY,CenterZ){
      end;
  else
    begin
      if ClientRect.Bottom = 0 then
      begin
        Ratio := 1
      end
      else
      begin
        Ratio := ClientRect.Right / ClientRect.Bottom
      end;

      { NearZ:=Max(1, wVectorZ-Diameter);
        FarZ :=Max(1, wVectorZ+Diameter);
        NearZ:=Max(1,(Min(Min(CenterX,CenterY),-CenterZ)+wVectorZ-Diameter));
        FarZ :=Max(10,(Max(Max(CenterX,CenterY),-CenterZ)+wVectorZ+Diameter));
        NearZ:=1;
        FarZ:=10000000;    { }
      gluPerspective(ViewAngle, Ratio, NearZ, FarZ);
      // gluPerspective(ViewAngle,Ratio,100,100000);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;

      glDisable(GL_LIGHTING);
      // Add the Lighting
      // Lighting.CallGLLight;

      // Centering on X, Y and pan
      // glTranslateF(-CenterX+wVectorX,-CenterY+wVectorY,-wVectorZ);
      // Rotation around a point with CenterX,CenterY,CenterZ)

      glTranslateF(wVectorX, wVectorY, -wVectorZ);
      cX := CenterX; // rotate Object around (CenterX,CenterY,CenterZ)
      cY := CenterY;
      cz := CenterZ;
      glScaled(2, 2, 1);

      (* }

        CX:=-wVectorX; //rotate Object around Me
        CY:=-wVectorY;
        CZ:=wVectorZ;(* *)

      // glTranslateF(CX,CY,CZ);

      glRotateF(wAngleX + 90, -1, 0, 0);
      glRotateF(wAngleY, 0, 1, 0);
      glRotateF(wAngleZ, 0, 0, 1); // no rotate

      // glScaled(kX,kY,kZ);
      glTranslateF(-cX, -cY, -cz); // ratate Object (CenterX,CenterY,CenterZ)
    end;
  end;
  glGetDoublev(GL_MODELVIEW_MATRIX, @ViewMatrix);
  glMatrixMode(GL_PROJECTION);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjectMatrix);
  glMatrixMode(GL_MODELVIEW);
end;

function TGBCanvas.GetCursor(const Index: Integer): TGLDouble;
begin
  Result := FCursor.V[Index];
end;

procedure TGBCanvas.SetCursor(const Index: Integer; const Value: TGLDouble);
begin
  if FCursor.V[Index] <> Value then
  begin
    FCursor.V[Index] := Value
  end;
end;

procedure TGBCanvas.CursorToWorldCusror;
begin
  SetProjection;
  gluUnProject(CursorX, ViewPort.W - CursorY - 1, CursorZ, ViewMatrix,
    ProjectMatrix, ViewPort, @FWorldCursor.X, @FWorldCursor.Y, @FWorldCursor.Z);
end;

function TGBCanvas.GetWorldCursor(const Index: Integer): TGLDouble;
begin
  Result := FWorldCursor.V[Index];
end;

procedure TGBCanvas.SetWorldCursor(const Index: Integer;
  const Value: TGLDouble);
begin
  if FWorldCursor.V[Index] <> Value then
  begin
    FWorldCursor.V[Index] := Value;
  end;
end;

procedure TGBCanvas.WorldCursorToCusror;
begin
  SetProjection;
  gluProject(WorldCursorX, WorldCursorY, WorldCursorZ, ViewMatrix,
    ProjectMatrix, ViewPort, @FCursor.X, @FCursor.Y, @FCursor.Z);
  CursorY := ViewPort.W - CursorY - 1;
end;

procedure TGBCanvas.SetViewMode(const Value: TViewMode);
begin
  if FViewMode <> Value then
  begin
    FViewMode := Value;
  end;
end;

procedure TGBCanvas.SetPenColor(Color: TColor);
begin
  glColor3ub(Color, Color, Color);
  {
    glColor3ub(Chr(Color and $FF),
    Chr((Color shr  8) and $FF),
    Chr((Color shr 16) and $FF));
  }
end;

procedure TGBCanvas.SetPenColor(R, G, B: TGLDouble);
begin
  glColor3d(R, G, B);
end;

procedure TGBCanvas.SetPenWidth(W: TGLDouble);
begin
  glPointSize(W);
  glLineWidth(W);
end;

procedure TGBCanvas.SetPenStyle(AStyle: TPenStyle);
begin
  case AStyle of
    psSolid:
      begin
        SetPenStyle(1, $FFFF)
      end; // ################
    psDash:
      begin
        SetPenStyle(1, $F0F0)
      end; // ####    ####
    psDot:
      begin
        SetPenStyle(1, $8888)
      end; // #   #   #   #
    psDashDot:
      begin
        SetPenStyle(1, $E4E4)
      end; // ###  #  ###  #
    psDashDotDot:
      begin
        SetPenStyle(1, $7F)
      end; // ### # # ### # #
    psClear:
      begin
        SetPenStyle(1, $0000)
      end; //
    psInsideFrame:
      begin
        SetPenStyle(1, $FFFF)
      end; // ################
  end;
end;

procedure TGBCanvas.SetPenStyle(AFactor: Integer; Pattern: word);
begin
  glLineStipple(AFactor, Pattern);
end;

procedure TGBCanvas.MoveTo(X, Y, Z: TGLDouble);
var
  V: array of Double;
begin
  V[0] := X;
  V[1] := Y;
  V[2] := Z;
  V[3] := 1;
  FWorldCursor := MakeDblVector(V);
  glRasterPos3d(X, Y, Z);
end;

procedure TGBCanvas.Point(X, Y, Z: TGLDouble);
begin
  glBegin(GL_POINTS);
  glVertex3d(X, Y, Z);
  glEnd;
end;

procedure TGBCanvas.Line(P1, P2: TVertex);
begin
  glBegin(GL_LINES);
  glVertex3dv(@P1.X);
  glVertex3dv(@P2.X);
  glEnd;
end;

procedure TGBCanvas.PolyLine(const Points: array of TVertex);
var
  N: TAffineDblVector;
  I: Integer;
begin
  if High(Points) > 1 then
  begin
    try
      Cross(Points[2].X, Points[2].Y, Points[2].Z, Points[1].X, Points[1].Y,
        Points[1].Z, Points[0].X, Points[0].Y, Points[0].Z, N.X, N.Y, N.Z);
      glNormal3dv(@N.X);
    except
    end
  end;
  glBegin(GL_LINE_STRIP);
  for I := 0 to High(Points) do
  begin
    glVertex3d(Points[I].X, Points[I].Y, Points[I].Z)
  end;
  glEnd;
end;

procedure TGBCanvas.LineTo(X, Y, Z: TGLDouble);
begin
  glBegin(GL_LINES);
  glVertex3d(WorldCursorX, WorldCursorY, WorldCursorZ);
  glVertex3d(X, Y, Z);
  glEnd;
end;

procedure TGBCanvas.SetBounds3D(AMinX, AMaxX, AMinY, AMaxY, AMinZ,
  AMaxZ: TGLDouble);
begin
  MinX := AMinX;
  MaxX := AMaxX;
  MinY := AMinY;
  MaxY := AMaxY;
  MinZ := AMinZ;
  MaxZ := AMaxZ;
end;

procedure TGBCanvas.SetPrimitiveID(ID: LongWord);
begin
  glLoadName(ID);
end;

procedure TGBCanvas.SetPrimitiveID(ID: Pointer);
begin
  glLoadName(TGLEnum(ID));
end;

procedure TGBCanvas.DisablePrimitiveID;
begin
  glPopName;
end;

procedure TGBCanvas.EnablePrimitiveID;
begin
  glPushName(TGLEnum(-1));
end;

procedure TGBCanvas.SolidSphere(X, Y, Z, radius: TGLDouble; N1: Integer = 5;
  // lines of longitude
  N2: Integer = 5); // lines of latitude
var
  QuadObj: PGLUquadric;
begin
  glPushMatrix;
  QuadObj := gluNewQuadric;
  gluQuadricDrawStyle(QuadObj, GLU_FILL); // GLU_POINT, GLU_LINE, GLU_FILL
  gluQuadricNormals(QuadObj, GLU_NONE);
  glTranslateF(X, Y, Z);
  gluSphere(QuadObj, radius, N1, N2);
  glPopMatrix;
end;

function gbArctan2(X, Y: Double): Double;
begin
  Result := 0;
  if (Y = 0) and (X = 0) then
  begin
  end
  else
  begin
    Result := arctan(X / Y) / pi * 180;
    if Y < 0 then
    begin
      Result := Result + 180
    end;
  end;
end;

procedure TGBCanvas.SolidCylinder(X1, Y1, Z1, X2, Y2, Z2, radius: TGLDouble);
var
  H, dX, dY, dZ: TGLDouble;
  Ax, Az: TGLDouble;
  QuadObj: PGLUquadric;
begin
  dX := X2 - X1;
  dY := Y2 - Y1;
  dZ := Z2 - Z1;
  H := sqrt(sqr(dX) + sqr(dY) + sqr(dZ));
  dX := dX / H;
  dY := dY / H;
  dZ := dZ / H;

  Ax := 90 - gbArctan2(dZ, sqrt(1 - sqr(dZ)));
  Az := gbArctan2(dX, dY);

  glPushMatrix;
  QuadObj := gluNewQuadric;
  gluQuadricDrawStyle(QuadObj, GLU_FILL);
  // gluQuadricNormals(quadObj,GLU_NONE);
  glTranslateF(X1, Y1, Z1);
  glRotateF(-Az, 0, 0, 1);
  glRotateF(-Ax, 1, 0, 0);
  // glColor3f(1,random,random);
  gluCylinder(QuadObj, radius, radius, H, 8, 1);
  gluDeleteQuadric(QuadObj);
  QuadObj := gluNewQuadric;
  gluDisk(QuadObj, 0, radius, 8, 1);
  gluDeleteQuadric(QuadObj);
  glPopMatrix;
  // auxCreateSolidSphere(X1,Y1,Z1, Radius);
  // auxCreateSolidSphere(X2,Y2,Z2, Radius);
end;

procedure TGBCanvas.TextOut(Text: AnsiString; X1, Y1, Z1, Ax, Ay, Az, ScaleX,
  ScaleY, ScaleZ: TGLDouble; AColor: TColor);
begin
  SetPenColor(AColor);
  SetTextMaterial(AColor);
  glPushMatrix;
  glRasterPos3f(X1, Y1, Z1);
  // glNormal3d(0,0,1);
  glTranslateF(X1, Y1 - 0.05, Z1);
  glRotateF(Az, 0, 0, 1);
  glRotateF(Ay, 0, 1, 0);
  glRotateF(Ax, 1, 0, 0);
  glScalef(ScaleX, ScaleY, ScaleZ);
  glListBase(FontStartGLList);
  glCallLists(System.Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
  glPopMatrix;
end;

procedure TGBCanvas.TextOut3D(const Text: string; X, Y, Z: Single;
  Color: TColor);

var
  Tmp: TSize;

const
  GLF_START_LIST = 1000;

begin
  glPushMatrix;

  glListBase(GLF_START_LIST);
  GetTextExtentPoint(DC, PChar(Text), Length(Text), Tmp);
  { if (FTextAlign and TA_CENTER)=TA_CENTER then
    x:=x-(tmp.Cx div 2)
    else }
  // if (FTextAlign and TA_RIGHT)=TA_RIGHT then
  X := X - 2 * (Tmp.cX div 3);

  // if (FTextAlign and TA_BOTTOM)<>TA_BOTTOM then
  Y := Y + 2 * (Tmp.cY div 3);

  glTranslateF(X, -Y, -Z + 1);
  // glScalef(Font.Size * 1.5, Font.Size * 1.5, 1);
  SetTextMaterial(Color);
  glCallLists(1 + Length(Text), GL_UNSIGNED_BYTE, PChar(Text));
  glPopMatrix;
end;

procedure TGBCanvas.FontChange(Sender: TObject);
var
  agmf: array [0 .. 255] of GLYPHMETRICSFLOAT; // Delphi 4
  hNewFont, hOldFont: HFONT;
begin
  MakeCurrent;
  // hNewFont := Font.Handle;
  // hOldFont := SelectObject(DC, hNewFont);
  try
    if glIsList(FontStartGLList) = 0 then  //False
    begin
      FontStartGLList := glGenLists(256)
    end;
    // else    glDeleteLists(FontStartGLList,256); ?
    if not wglUseFontBitmaps(DC, 0, 255, FontStartGLList) then
    begin
      // ShowMessage('Raster font: "'+Font.Name+'", Error: '+IntToStr(GetLastError));
      wglUseFontOutlines(DC, 0, 255, FontStartGLList, 0, 0.1,
        WGL_FONT_POLYGONS, @agmf);
    end;
  finally
    // DeleteObject(SelectObject(DC, hNewFont));
    // SelectObject(DC, hOldFont);
  end;

end;

function TGBCanvas.RejectPrimitive(X, Y: Integer;
  Primitive: TDrawPrimitive): Boolean;
begin
  Result := False;
  if Assigned(FOnRejectPrimitive) then
  begin
    FOnRejectPrimitive(Self, X, Y, Primitive, Result)
  end;
end;

function TGBCanvas.SelectPrimitive(X, Y: Integer): TDrawPrimitive;
var
  Arr: TArrayOfDrawPrimitive;
  I, J: Integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  Arr := SelectPrimitives(X, Y);
  if Arr <> nil then
  begin
    J := High(Arr) + 1;
    // Find first not rejected primitive
    for I := 0 to High(Arr) do
    begin
      if not RejectPrimitive(X, Y, Arr[I]) then
      begin
        Result := Arr[I];
        J := I;
        Break;
      end
    end;
    // Find nearest not rejected primitive
    for I := J to High(Arr) do
    begin
      if (not RejectPrimitive(X, Y, Arr[I])) and (Result.Z > Arr[I].Z) then
      begin
        Result := Arr[I]
      end
    end;
    // call event if exist
    if (Result.IDs <> nil) and Assigned(FOnSelectPrimitive) then
    begin
      FOnSelectPrimitive(Self, X, Y, Result)
    end;
  end;
end;

procedure TGBCanvas.WMMouseMove(var Message: TWMMouse);
begin
  inherited;
  SetPenPos(Message.XPos, Message.YPos);
end;

procedure TGBCanvas.DefaultMouseDblClick(Sender: TObject);
begin
  SelectPrimitive(Round(CursorX), Round(CursorY));
end;

procedure TGBCanvas.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

procedure TGBCanvas.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  wVectorZ := wVectorZ + Message.WheelDelta * MouseWheelStep;
  Redraw;
end;

procedure TGBCanvas.SetMouseWheelStep(const Value: Double);
begin
  FMouseWheelStep := Value;
end;

procedure TGBCanvas.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if not(csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_MOUSEWHEEL:
        begin
          WMMouseWheel(TWMMouseWheel(Message))
        end;
    end; { case }
  end; { if (not (csDesigning in ComponentState) }
end;

procedure TGBCanvas.SetkX(const Value: Double);
begin
  FkX := Value;
end;

procedure TGBCanvas.SetkY(const Value: Double);
begin
  FkY := Value;
end;

procedure TGBCanvas.SetkZ(const Value: Double);
begin
  FkZ := Value;
end;

procedure TGBCanvas.WMClick(var Message: TWMMouse);
begin
  SetFocus;
end;

procedure TGBCanvas.SetAppendPrimitives(const Value: Boolean);
begin
  FAppendPrimitives := Value;
end;

procedure TGBCanvas.DrawCursorRect;
begin

end;

procedure TGBCanvas.SetPenPos(X, Y: Double);
var
  ViewPort: TVector4i;
  mvMatrix, ProjMatrix: TMatrix4d;
  RealY: TGLint;
  Zval: TGLFloat;
begin
  glGetIntegerv(GL_VIEWPORT, @ViewPort);
  glGetDoublev(GL_MODELVIEW_MATRIX, @mvMatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix);

  // viewport[3] - The height of the window in pixels
  RealY := ViewPort.W - Round(Y) - 1;

  glReadPixels(Round(X), RealY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @Zval);

  CursorX := X;
  CursorY := Y;
  CursorZ := Zval;

  gluUnProject(Round(X), RealY, Zval, mvMatrix, ProjMatrix, ViewPort,
    @FWorldCursor.X, @FWorldCursor.Y, @FWorldCursor.Z);
end;

procedure TGBCanvas.SetTextMaterial(AColor: TColor);
var
  Tmp: TGLMat;
begin
  ColorToGL(AColor, Tmp);
  (*
    if FUseMaterial then
    glMaterialfv(GL_FRONT{_AND_BACK}, GL_AMBIENT{_AND_DIFFUSE}, @tmp)
    else
  *)
  glColor3f(Tmp[0], Tmp[1], Tmp[2]);
end;

procedure TGBCanvas.SetFPS(const Value: Double);
begin
  FFPS := Value;
end;

{ TPointStyle }

procedure TPointStyle.Use;
begin
  // glColor4fv(Color.As4fv);
  glPointSize(Size);
end;

{ TFont3D }

procedure TFont3D.AddChar(C: Char);
var
  DC: HDC;
  agmf: array [0 .. 255] of GLYPHMETRICSFLOAT; // Delphi 4
  hFontNew, hOldFont: HFONT;
begin
  if FFontLists[C] = 0 then
  begin
    // DC:=GetDC(0);
    DC := LastCanvas.DC;
    try
      FRaster := True;
      hFontNew := Handle;
      hOldFont := SelectObject(DC, hFontNew);
      try
        FFontLists[C] := glGenLists(1);
        if not wglUseFontOutlines(DC, Ord(C), 1, FFontLists[C], 0, 0.1,
          WGL_FONT_POLYGONS, @agmf) then
        begin
          // ShowMessage('Raster Font "'+Font.Name+'", due to error: '+IntToStr(GetLastError));
          wglUseFontBitmaps(DC, Ord(C), 1, FFontLists[C]);
        end
        else
        begin
          FRaster := False
        end; // on success with vector font installation
      finally
        // DeleteObject (SelectObject(DC, hFontNew));
        // DeleteObject (SelectObject(DC, hOldFont));
        SelectObject(DC, hOldFont);
        DeleteObject(hFontNew);
      end;
    finally
      ReleaseDC(0, DC);
    end;
  end;
end;

function TFont3D.TranslateText(S: string): TGLLists;
var
  I: Integer;
begin
  if LastCanvas = nil then
  begin
    Exit
  end;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
  begin
    AddChar(S[I]);
    Result[I - 1] := FFontLists[S[I]];
  end;
end;

initialization

InitOpenGLFromLibrary('OpenGL32.dll', 'Glu32.dll');

end.
