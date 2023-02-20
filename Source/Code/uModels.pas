//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
 {! Classes for Dataset Models }

unit uModels;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.Contnrs,
  System.Math,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  Data.DB,
  Bde.DBTables,

  GLS.VectorGeometry,
  GLS.Objects,
  GLS.SpaceText,
  GLS.VectorTypes,
  GLS.Isolines,

  GBGeometry,
  GBGraphics,
  fMapLegend,

  cGlobals,
  uCommon,
  cResStrings,
  cInterpol;

type
  TGBModel = class;

  TGBAttributeType  =
    (atReal, atInteger, atText, atVector, atDateTime, atPattern);
  TGBAttributeTypes = set of TGBAttributeType;

  TDrawMode = (dmPoint, dmLine, dmFill);

  TGBAttribute = class(TCollectionItem)
  private
    FLevels:   TLegend;
    FMaxValue: Single;
    FMinValue: Single;
    FChecked:  Boolean;
    procedure SetLevels(const Value: TLegend);
    function GetLevels: TLegend;
    procedure SetMaxValue(const Value: Single);
    procedure SetMinValue(const Value: Single);
    function GetLevel: TLevel;
    procedure SetChecked(const Value: boolean);
  public
    AttributeName: string;
    AttributeType: TGBAttributeType;
    Value: variant;

    AsText:     string;
    AsInteger:  integer;
    AsReal:     Single;
    AsVector:   TVector3D;
    AsDataTime: TDateTime;

    property Levels: TLegend read GetLevels write SetLevels;
    property Level: TLevel Read GetLevel;
    property MinValue: Single read FMinValue write SetMinValue;
    property MaxValue: Single read FMaxValue write SetMaxValue;
    property Checked: boolean read FChecked write SetChecked;

    destructor Destroy; override;
    procedure LegendDialog; virtual;
  end;

  TAttributeClass = class of TGBAttribute;

  TAttributesCollection = class(TOwnedCollection)
  public
    function GetOwner: TPersistent; override;
  end;

  TBaseComponent = class(TComponent)
  private
    FUpdateCount: Integer;
    FonChange:    TNotifyEvent;
  protected
    procedure Change; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Update; virtual;
    property onChange: TNotifyEvent read FonChange write FonChange;
  end;

  TGBAttributes = class(TBaseComponent)
  private
    FModel: TGBModel;
    FAttributeList: TOwnedCollection;
    procedure SetModel(const Value: TGBModel);
    function GetAttributeCount: integer;
    function GetAttributes(Index: integer): TGBAttribute;
    procedure SetAttributes(Index: integer; const Value: TGBAttribute);
  public
    constructor Create(AModel: TGBModel); reintroduce;
    destructor Destroy; override;
    procedure Clear;

    function AttributeByName(AName: string): TGBAttribute; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function AddAttribute(AName: string; AType: TGBAttributeType;
      AMinValue, AMaxValue: double): TGBAttribute;

    property Model: TGBModel read FModel write SetModel;
    property AttributeCount: integer read GetAttributeCount;
    property Attributes[Index: integer]: TGBAttribute
      Read GetAttributes write SetAttributes; default;
  end;

  TModelState  = (msHandleValid, msOpenGLListValid, msMinMaxValueValid,
    msDataValid);
  TModelStates = set of TModelState;

  (* Origin Model with options, properties and methods *)
  TGBModel = class(TBaseComponent)
  private
    FCanvas: TComponent;
    FOGLListNo: Integer;
    FActive: Boolean;
    FModelName: string;
    FVisible: Boolean;
    FAttributes: TGBAttributes;
    FActiveAttributeNo: integer;
    FQuery: TQuery;
    FTable: TTable;
    FModelID: Integer;
    FDrawMode: TDrawMode;
    FAttribTextNo: integer;
    FFont: TFont;
    function GetAttributeName: string;
    function GetActiveAttribute: TGBAttribute;
    procedure SetOGLListNo(const Value: integer);
    procedure SetActive(const Value: boolean);
    procedure SetModelName(const Value: string);
    procedure SetVisible(const Value: boolean);
    function GetOGLListNo: integer;
    procedure SetAttributes(const Value: TGBAttributes);
    procedure SetActiveAttributeNo(const Value: integer);
    function GetQuery: TQuery;
    function GetTableModel: TTable;
    function GetCanvas3D: TGBCanvas;
    procedure SetCanvas3D(const Value: TGBCanvas);
    procedure SetDrawMode(const Value: TDrawmode);
    procedure SetFont(const Value: TFont);
  protected
    property Query: TQuery read GetQuery;
    property TableModel: TTable read GetTableModel;

    procedure InitAttributes; virtual;
    procedure InternalOpen; virtual;
    procedure InternalClose; virtual;
    procedure CreateOpenGLList; virtual;
    procedure BuildPointList; virtual;
    procedure BuildLineList; virtual;
    procedure BuildPolygonList; virtual;
    procedure QueryData; virtual;
    procedure QueryMinMax; virtual;
    procedure RequiredState(ReqState: TModelStates); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AFileName: TFileName);
      reintroduce; overload; virtual;
    destructor Destroy; override;
  public
    State:      TModelStates;
    SpaceText:  TGLSpaceText;
    //Options for map bounds
    MinX, MaxX: Single;
    MinY, MaxY: Single;
    MinZ, MaxZ: Single;
    XO, YO, ZO: double;

    TextAttributesChecked: boolean;
    AsSphere:   boolean; //Shows points as spheres
    AsCylinder: boolean; //Shows lines as cylinders

    SizeMode:   integer;
    SizeFactor: integer;

    IsTransparent: boolean;

    property ModelID: integer read FModelID;
    property ModelName: string read FModelName write SetModelName;
    class function ModelType: integer; virtual;
    property OGLListNo: integer read GetOGLListNo write SetOGLListNo;

    property AttribTextNo: integer read FAttribTextNo write FAttribTextNo;
    property Attributes: TGBAttributes read FAttributes write SetAttributes;

    property ActiveAttribute: TGBAttribute read GetActiveAttribute;
    property ActiveAttributeNo: integer read FActiveAttributeNo write SetActiveAttributeNo;
    property AttributeName: string read GetAttributeName;

    property DrawMode: TDrawmode read FDrawMode write SetDrawMode;
    property Visible: boolean read FVisible write SetVisible;

    property Active: boolean read FActive write SetActive;
    property Font: TFont read FFont write SetFont;
    property Canvas3D: TGBCanvas read GetCanvas3D write SetCanvas3D;

    procedure Assign(Source: TPersistent); override;
    function SelectOptions: boolean; virtual;
    procedure ReadParFile; virtual;
    procedure Open;
    procedure Close;

    procedure GetLevelStyle(const Value: variant; out R, G, B: double;
      out IsVisible: boolean);
  end;

type
  TLine = class
  end;

  TSample = class //TLine
    Name:   string;
    Length: double;
    StartPoint: TVector3D;
    FinishPoint: TVector3D;
    Attributes: TGBAttributes;
  end;

  TSampleList = class(TObjectList)
  private
    function GetItems(Index: integer): TSample;
    procedure SetItems(Index: integer; const Value: TSample);
  public
    property Items[Index: integer]: TSample Read GetItems Write SetItems; default;
  end;

  TGBHole = class
  private
    FSamples: TSampleList;
    procedure SetSamples(const Value: TSampleList);
    function GetSamples: TSampleList;
  public
    destructor Destroy; override;
    property Samples: TSampleList Read GetSamples Write SetSamples;
  end;

  TGBHoleList = class(TObjectList)
  private
    function GetItems(Index: integer): TGBHole;
    procedure SetItems(Index: integer; const Value: TGBHole);
  public
    property Items[Index: integer]: TGBHole Read GetItems Write SetItems; default;
  end;

  { ============================= Dholes ================================== }
  TGBHoles = class(TGBModel)
  private
    FDholes: TGBHoleList;
    procedure SetDholes(const Value: TGBHoleList);
    function GetDholes: TGBHoleList;
  protected
    procedure CreateOpenGLList; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Dholes: TGBHoleList Read GetDholes Write SetDholes;
  public
    DrawTextOnChange: boolean;
    Scale: integer; // The scale of map

    procedure Assign(Source: TPersistent); override;
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  { ============================= Points ============================== }
  TGBPoints = class(TGBModel)
  protected
    procedure CreateOpenGLList; override;
    constructor Create(AOwner: TComponent); override;
  end;

  TGBPoints2D = class(TGBPoints)
  protected
    procedure CreateOpenGLList; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  TGBPoints3D = class(TGBPoints)
  protected
    procedure CreateOpenGLList; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  //==================== Polygons ==========================\\
  TGBPolygons = class(TGBModel)
  protected
    procedure CreateOpenGLList; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function ModelType: integer; override;
    procedure Assign(Source: TPersistent); override;
    function SelectOptions: boolean; override;
  end;

  { -------------------- Tin ---------------------- }
  TGBTin = class(TGBModel)
  protected
    procedure CreateOpenGLList; override;
  public
    ShowByVertices: boolean;
    ShowContours:   boolean;
    ShowVectors:    boolean;
    Isolines:   TGLIsolines;
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  //====================== Solids ============================\\
  TGBSolids = class(TGBModel)
  protected
    procedure CreateOpenGLList; override;
    procedure QueryData; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  //====================== Grids ============================\\
  TGBGrid = class(TGBModel)
  public
    DX, DY, DZ:     double;
    NX, NY, NZ:     longint;
    ShowByCenters: boolean;
    ShowAsHeight:   boolean;
    ShowIsolines:   boolean;
    ShowVectors:    boolean;
    Isolines:   TGLIsolines;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TGBGrid2D = class(TGBGrid)
  protected
    procedure CreateOpenGLList; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  TGBGrid3D = class(TGBGrid)
  protected
    procedure CreateOpenGLList; override;
  public
    ShowIsosurface: boolean;
    ShowFilm: boolean;
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  //====================== Meshes ============================\\
  TGBMesh = class(TGBModel)
  public
    ShowContours: boolean;
    ShowVectors:  boolean;
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
  end;

  TGBMesh2D = class(TGBMesh)
  protected
    procedure CreateOpenGLList; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

  TGBMesh3D = class(TGBMesh)
  protected
    procedure CreateOpenGLList; override;
    procedure QueryData; override;
  public
    class function ModelType: integer; override;
    function SelectOptions: boolean; override;
  end;

type
  //========== TModelList =============\\
  TModelList = class(TList)
  private
    function Get(Index: integer): TGBModel;
    procedure Put(Index: integer; Item: TGBModel);
  public
    function IndexOfID(aID: integer): integer;
    procedure Clear; override;
    function Add(Item: TGBModel): integer;
    property Items[Index: integer]: TGBModel read Get write Put; default;
  end;

function GetModelID: integer;

//============================================================================
implementation
//============================================================================

uses
  cProfuns,
  fDisplayHolesOptions,
  fDisplayPoints2DOptions,
  fDisplayPoints3DOptions,
  fDisplayPolygonsOptions,
  fDisplayTinOptions,
  fDisplaySolidOptions,
  fDisplayGrid2DOptions,
  fDisplayGrid3DOptions,
  fDisplayMesh2DOptions,
  fDisplayMesh3DOptions;


{ TBaseComponent }

procedure TBaseComponent.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBaseComponent.Change;
begin
  if FUpdateCount <> 0 then
    Exit;
  if Assigned(FonChange) then
    FOnChange(Self);
end;

procedure TBaseComponent.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Change;
end;

procedure TBaseComponent.Update;
begin

end;

function GetModelID: integer;
var
  ModelID: integer;
  //  ModelID :=1;
begin
  Result := ModelID;
  Inc(ModelID);
end;

{_____________ TGBModel______________ }

constructor TGBModel.Create(AOwner: TComponent);
begin
  inherited;
  FModelID   := GetModelID;
  FAttributes := TGBAttributes.Create(Self);
  State      := [];
  SizeFactor := 2;
end;

procedure TGBModel.Assign(Source: TPersistent);
var
  SourceModel: TGBModel;
begin
  if Source is TGBModel then
  begin
    BeginUpdate;
    try
      SourceModel := TGBModel(Source);
      OGLListNo := SourceModel.OGLListNo;
      ModelName := SourceModel.ModelName;
      MinX    := SourceModel.MinX;
      MaxX    := SourceModel.MaxX;
      MinY    := SourceModel.MinY;
      MaxY    := SourceModel.MaxY;
      MinZ    := SourceModel.MinZ;
      MaxZ    := SourceModel.MaxZ;
      XO      := SourceModel.XO;
      YO      := SourceModel.YO;
      ZO      := SourceModel.ZO;
      Visible := SourceModel.Visible;

      TextAttributesChecked := SourceModel.TextAttributesChecked;

      AsSphere   := SourceModel.AsSphere;
      AsCylinder := SourceModel.AsCylinder;
      SizeMode   := SourceModel.SizeMode;
      SizeFactor := SourceModel.SizeFactor;
      DrawMode   := SourceModel.DrawMode;

      ActiveAttributeNo := SourceModel.ActiveAttributeNo;
      AttribTextNo := SourceModel.AttribTextNo;
      Font := SourceModel.Font;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

function TGBModel.GetAttributeName: string;
begin
  Result := '';
  try
    Result := Attributes[ActiveAttributeNo].AttributeName;
  except
  end;
end;

function TGBModel.SelectOptions: boolean;
begin
  Result := True;
end;

procedure TGBModel.CreateOpenGLList;
begin
  //Implemented in descendants
end;

procedure TGBModel.SetOGLListNo(const Value: integer);
begin
  FOGLListNo := Value;
end;

procedure TGBModel.ReadParFile;
begin
  //Implements in descendants
end;

procedure TGBModel.Close;
begin
  Active := False;
end;

procedure TGBModel.Open;
begin
  Active := True;
end;

procedure TGBModel.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      InternalOpen
    else
      InternalClose;
  end;
end;

procedure TGBModel.InternalClose;
begin

end;

procedure TGBModel.InternalOpen;
begin
  State := [];
  InitAttributes;
end;

procedure TGBModel.SetModelName(const Value: string);
var
  OldActive:    boolean;
  OldModelName: string;
begin
  if CompareText(FModelName, Value) <> 0 then
  begin
    OldActive    := Active;
    OldModelName := ModelName;
    try
      Close;
      FModelName := Value;
      if OldActive then
        Open;
    except
      FModelName := OldModelName;
      Active     := OldActive;
    end;
  end;
end;

class function TGBModel.ModelType: integer;
begin
  Result := mtUnknown;
end;

destructor TGBModel.Destroy;
begin
  Query.Free;
  TableModel.Free;
  FAttributes.Free;
  inherited;
end;

procedure TGBModel.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change;
  end;
end;

constructor TGBModel.Create(AOwner: TComponent; AFileName: TFileName);
begin
  Create(AOwner);
  ModelName := AFileName;
end;

function TGBModel.GetOGLListNo: integer;
begin
  RequiredState([msOpenGLListValid, msMinMaxValueValid, msDataValid]);
  Result := FOGLListNo;
end;

procedure TGBModel.SetAttributes(const Value: TGBAttributes);
begin
  FAttributes := Value;
end;

procedure TGBModel.InitAttributes;
var
  I: integer;
  UpCaseName: string;
begin
  TableModel.Close;
  TableModel.TableName := ModelName;
  TableModel.Open;
  Attributes.BeginUpdate;
  try
    Attributes.Clear;
    for I := 0 to TableModel.FieldCount - 1 do
      with TableModel, Fields[I] do
      begin
        UpCaseName := UpperCase(FieldName);
        if Fields[I] is TNumericField then
        begin
          if (Pos(fldID + '_', UpCaseName) < 1) and (UpCaseName <> fldID) and
            (UpCaseName <> fldV1) and (UpCaseName <> fldV2) and
            (UpCaseName <> fldV3) and (UpCaseName <> fldN1) and
            (UpCaseName <> fldN2) and (UpCaseName <> fldN3) then
            Attributes.AddAttribute(TableModel.Fields[I].FieldName, atReal, 0, 100);
        end
        else
        if Fields[I] is TStringField then //add also to use as labels
        begin
          Attributes.AddAttribute(TableModel.Fields[I].FieldName, atText, 0, 1);
        end;
      end;
    ActiveAttributeNo := 0;
  finally
    Attributes.EndUpdate;
  end;
  TableModel.Close;
end;

procedure TGBModel.QueryData;
begin

end;

procedure TGBModel.QueryMinMax;
var
  OldCursor: TCursor;
begin
  if (ActiveAttribute = nil) or (ActiveAttribute.AttributeType <> atReal) and
    (ActiveAttribute.AttributeType <> atInteger) then
    Exit;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    //Min Values for 4 attributes
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT Min(' + fldX + '),' + ' Min(' + fldY +
      '),' + ' Min(' + fldZ + '),' + ' Min(' + ActiveAttribute.AttributeName +
      ')' + ' FROM "' + ModelName + '"');
    try
      Query.Open;
      MinX := Query.Fields[0].AsFloat;
      MinY := Query.Fields[1].AsFloat;
      MinZ := Query.Fields[2].AsFloat;
      ActiveAttribute.MinValue := Query.Fields[3].AsFloat;
    except
      MessageDlg(LoadResString(@rsErrorWithTable), mtError, [mbOK], 0);
      Abort;
    end;
    //Max Values for 4 attributes
    Query.Close;
    Query.SQL.Clear;
    Query.SQL.Add('SELECT Max(' + fldX + '),' + ' Max(' + fldY +
      '),' + ' Max(' + fldZ + '),' + ' Max(' + ActiveAttribute.AttributeName +
      ')' + ' FROM "' + ModelName + '"');
    try
      Query.Open;
      MaxX := Query.Fields[0].AsFloat;
      MaxY := Query.Fields[1].AsFloat;
      MaxZ := Query.Fields[2].AsFloat;
      ActiveAttribute.MaxValue := Query.Fields[3].AsFloat;
    except
      MessageDlg(LoadResString(@rsErrorWithTable), mtError, [mbOK], 0);
      Abort;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TGBModel.RequiredState(ReqState: TModelStates);
var
  NeededState: TModelStates;
begin
  NeededState := ReqState - State;
  if NeededState <> [] then
  begin
    if msHandleValid in NeededState then
    begin
      {
       CreateHandle;
        if FHandle = 0 then
         raise EInvalidOperation.CreateRes(@rsNoCanvasHandle);
      {}
    end;
    if msMinMaxValueValid in NeededState then
      QueryMinMax;

    if msDataValid in NeededState then
      QueryData;
    if msOpenGLListValid in NeededState then
      CreateOpenGLList;
    State := State + NeededState;
  end;
end;

procedure TGBModel.SetActiveAttributeNo(const Value: integer);
begin
  if FActiveAttributeNo <> Value then
  begin
    FActiveAttributeNo := Value;
    State := State - [msOpenGLListValid, msMinMaxValueValid, msDataValid];
    Change;
  end;
end;

procedure TGBModel.GetLevelStyle(const Value: variant; out R, G, B: double;
  out IsVisible: boolean);
begin
  Attributes[ActiveAttributeNo].Levels.GetLevelStyle(Value, R, G, B, IsVisible);
end;

function TGBModel.GetActiveAttribute: TGBAttribute;
begin
  try
    Result := Attributes[ActiveAttributeNo];
  except
    Result := nil;
  end;
end;

function TGBModel.GetQuery: TQuery;
begin
  if FQuery = nil then
    FQuery := TQuery.Create(Self);
  Result := FQuery;
end;

function TGBModel.GetTableModel: TTable;
begin
  if FTable = nil then
    FTable := TTable.Create(Self);
  Result := FTable;
end;

function TGBModel.GetCanvas3D: TGBCanvas;
begin
  if FCanvas is TGBCanvas then
    Result := TGBCanvas(FCanvas)
  else
    Result := nil;
end;

procedure TGBModel.SetCanvas3D(const Value: TGBCanvas);
begin
  if FCanvas <> nil then
    RemoveFreeNotification(FCanvas);
  FCanvas := Value;
  if FCanvas <> nil then
    FreeNotification(FCanvas);
end;

procedure TGBModel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCanvas) then
    FCanvas := nil;
end;

procedure TGBModel.BuildPointList;

type
  HGLRC = THandle;

var
  Size: Integer;
  X, Y, Z: Single;
  ValueReal: Single;
  ValueInteger: integer;
  R, G, B: double;
  OldCursor: TCursor;
  IsVisible: boolean;
  hrc: HGLRC;

const
  GLF_START_LIST = 1000;

begin
  TableModel.Open;
  TableModel.First;
  OldCursor := Screen.Cursor;
  if FOGLListNO = 0 then
    FOGLListNo := glGenLists(1);
  try
    Screen.Cursor := crHourGlass;
    {
    SpaceText := TSpaceText.Create(TDummyCube);
    with SpaceText do
    begin
      Material.MaterialOptions := [];
      Extrusion := 1;
      Font.Assign(Font);
      Text := 'Screen Saver';
      AllowedDeviation := 1;
      CharacterRange := stcrAlphaNum;
    end;
    {}
    glNewList(FOGLListNO, GL_COMPILE);
    if AsSphere then
    begin
      {   glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,PGLfloat(@spec));
          glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,PGLfloat(@amb));
          glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,PGLfloat(@diff));
          glMaterialf (GL_FRONT_AND_BACK,GL_SHININESS,128);
          glTexture
          {}
    end;
    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;
    while not TableModel.EOF do
    begin
      X := TableModel.FieldByName(fldX).AsFloat;
      Y := TableModel.FieldByName(fldY).AsFloat;
      Z := TableModel.FieldByName(fldZ).AsFloat;

      ValueReal := TableModel.FieldByName(ActiveAttribute.AttributeName).AsFloat;
      GetLevelStyle(ValueReal, R, G, B, IsVisible);
      glColor3f(R, G, B);
      if IsVisible then
      begin
        Canvas3D.SetPrimitiveID(integer(TableModel.Fields[0]));
        if SizeMode = 0 then
          Size := SizeFactor
        else
        if (ActiveAttribute.AttributeType = atReal) then
          Size := Round(SizeFactor * (ValueReal - ActiveAttribute.MinValue) /
            (ActiveAttribute.MaxValue - ActiveAttribute.MinValue))
        else
          Size := Round(SizeFactor * ValueReal);
        if AsSphere then
          Canvas3D.SolidSphere(X, Y, Z, Size)
        else
        begin
          { TODO -oVas -cMap : GL_POINT_SMOOTH not working }
          glPointSize(Size);
          glEnable(GL_POINT_SMOOTH);
          glBegin(GL_POINTS);
          glVertex3f(X, Y, Z);
          glEnd;
        end;
      end;
      if TextAttributesChecked then
      begin
        //  SpaceText.Text := TableModel.TableModel.Fields[AttribTextNo+1].AsString;
        //  Canvas3D.TextOut3D(TableModel.Fields[AttribTextNo+1].AsString, X, Y, Z, Font.Color);
        Canvas3D.TextOut(TableModel.Fields[AttribTextNo + 1].AsString,
          X + Size + 0.5, Y, Z, 0, 0, 0, 1, 1, 1, Canvas3D.Font.Color);
      end;
      TableModel.Next;
    end;
  finally
    Canvas3D.DisablePrimitiveID;
    Canvas3D.DisablePrimitiveID;
    glEndList;
    //delete text lists
    glDeleteLists(GLF_START_LIST, 256);

    Screen.Cursor := OldCursor;
  end;
  TableModel.Close;
  //SpaceText.Free;
end;

procedure TGBModel.BuildLineList;
begin

end;

procedure TGBModel.BuildPolygonList;
begin

end;

procedure TGBModel.SetDrawMode(const Value: TDrawmode);
begin
  FDrawMode := Value;
end;

procedure TGBModel.SetFont(const Value: TFont);
begin
  FFont := Value;
end;

{ TModelList }

function TModelList.Add(Item: TGBModel): integer;
begin
  Result := inherited Add(Item);
end;

procedure TModelList.Clear;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].Free;
    inherited Items[I] := nil;
  end;
  inherited Clear;
end;

function TModelList.Get(Index: integer): TGBModel;
begin
  Result := inherited Get(Index);
end;

function TModelList.IndexOfID(aID: integer): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Items[I].ModelID = aID then
      Result := I;
end;

procedure TModelList.Put(Index: integer; Item: TGBModel);
begin
  inherited Put(Index, Item);
end;

{ TSampleList }

function TSampleList.GetItems(Index: integer): TSample;
begin
  Result := TSample(inherited Items[Index]);
end;

procedure TSampleList.SetItems(Index: integer; const Value: TSample);
begin
  inherited Items[Index] := Value;
end;

{_______________________ TGBHole________________________ }

function TGBHole.GetSamples: TSampleList;
begin
  if FSamples = nil then
    FSamples := TSampleList.Create;
  Result := FSamples;
end;

procedure TGBHole.SetSamples(const Value: TSampleList);
begin
  FSamples := Value;
end;

destructor TGBHole.Destroy;
begin
  FSamples.Free;
  FSamples := nil;
  inherited;
end;

{ TDholeList }

function TGBHoleList.GetItems(Index: integer): TGBHole;
begin
  Result := TGBHole(inherited Items[Index]);
end;

procedure TGBHoleList.SetItems(Index: integer; const Value: TGBHole);
begin
  inherited Items[Index] := Value;
end;

{______________________ TGBHoles________________________ }

constructor TGBHoles.Create(AOwner: TComponent);
begin
  inherited;
  FDholes  := TGBHoleList.Create;
  DrawMode := dmLine;
end;

function TGBHoles.GetDholes: TGBHoleList;
begin
  if FDholes = nil then
    FDholes := TGBHoleList.Create;
  Result := FDholes;
end;

procedure TGBHoles.Assign(Source: TPersistent);
begin
  if Source is TGBHoles then
  begin
    BeginUpdate;
    try
      inherited;
      DrawTextOnChange := TGBHoles(Source).DrawTextOnChange;
      Scale := TGBHoles(Source).Scale;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

class function TGBHoles.ModelType: integer;
begin
  Result := mtDholes;
end;

procedure TGBHoles.SetDholes(const Value: TGBHoleList);
begin
  FDholes := Value;
end;

procedure TGBHoles.CreateOpenGLList;
var
  Size:      Integer;
  ValueReal: real;
  ValueInt:  integer;

  X1, X2, Y1, Y2, Z1, Z2: Single;
  R, G, B:   double;
  IsVisible: boolean;
  HoleOld, HoleNew: string;

  OldCursor: TCursor;
  TextLabel: string;

  {sub} procedure GLOutTextXYZ(const Value: string; X, Y, Z: Single;
    Height: integer; Color: TColor);
  var
    LightEnable: Byte;
    s: integer;
  begin
    if Trim(Value) = '' then
      Exit;
    LightEnable := glIsEnabled(GL_LIGHTING);
    try
      glDisable(GL_LIGHTING);
      S := Abs(Height);
      try
        Canvas3D.TextOut(Value, X, Y, Z, 90, 0, 0, S, S, S, Color);
      except
      end;
    finally
      if LightEnable > 0 then
        glEnable(GL_LIGHTING);
    end;
  end; //GLOutTextXYZ

begin
  case DrawMode of
    dmPoint:
    begin
      BuildPointList;
      Exit;
    end;
    dmLine: ;
  end;
  TableModel.Open;
  RequiredState([msMinMaxValueValid, msDataValid]);
  if FOGLListNo = 0 then
    FOGLListNo := glGenLists(1);
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    glNewList(FOGLListNo, GL_COMPILE);

    glDisable(GL_NORMALIZE);
    glLineStipple(1, $FFFF);

    glEnable(GL_LINE_SMOOTH);
    X1 := (TableModel.FieldByName(fldX).AsFloat);
    Y1 := (TableModel.FieldByName(fldY).AsFloat);
    Z1 := (TableModel.FieldByName(fldZ).AsFloat);

    HoleOld := TableModel.FieldByName(fldDHOLE).AsString;
    Canvas3D.EnablePrimitiveID; //ModelID after Picking
    Canvas3D.SetPrimitiveID(ModelID); //Sets ModelID
    Canvas3D.EnablePrimitiveID; //ID of a sample after picking with mouse
    while not TableModel.EOF do
    begin
      X2 := TableModel.FieldByName(fldX).AsFloat;
      Y2 := TableModel.FieldByName(fldY).AsFloat;
      Z2 := TableModel.FieldByName(fldZ).AsFloat;

      ValueReal := TableModel.FieldByName(ActiveAttribute.AttributeName).AsFloat;
      GetLevelStyle(ValueReal, R, G, B, IsVisible);
      if TextAttributesChecked then
        TextLabel := TableModel.Fields[AttribTextNo + 1].AsString;

      HoleNew := TableModel.FieldByName(fldDHOLE).AsString;
      if HoleOld = HoleNew then
      begin
        if IsVisible then
        begin
          Canvas3D.SetPrimitiveID(integer(TableModel[fldID]));
          if SizeMode = 0 then
            Size := SizeFactor
          else
          begin
            if (ActiveAttribute.AttributeType = atReal) then
              Size := Round(SizeFactor * (ValueReal -
                ActiveAttribute.MinValue) / (ActiveAttribute.MaxValue -
                ActiveAttribute.MinValue))
            else
              Size := Round(SizeFactor * ValueReal);
          end;
          if AsCylinder then
            Canvas3D.SolidCylinder(X1, Y1, Z1, X2, Y2, Z2, Size)
          else
          begin
            glPointSize(Size);
            glLineWidth(Size);
            glBegin(GL_LINES);
            glVertex3f(X1, Y1, Z1);
            glVertex3f(X2, Y2, Z2);
            glEnd;
          end;
        end;
      end
      else
      begin
        HoleOld := HoleNew;
        if DrawTextOnChange then
          if isVisible and TextAttributesChecked then
            Canvas3D.TextOut(TextLabel, X2 + 0.5, Y2 + 0.5, Z2, 0, 0, 0, 1, 1, 1,
              Canvas3D.Font.Color);
      end;
      if not DrawTextOnChange then
        if isVisible and TextAttributesChecked then
          Canvas3D.TextOut(TextLabel, X1 + Size + 0.5, Y1 + 0.5, Z1, 0, 0, 0, 1, 1, 1,
            Canvas3D.Font.Color);
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;

      TableModel.Next;
      glColor3f(R, G, B);
    end;
    if not DrawTextOnChange then
      if isVisible and TextAttributesChecked then
        Canvas3D.TextOut(TextLabel, X1 + Size + 0.5, Y1 + 0.5, Z1, 0, 0, 0, 1, 1, 1,
          Canvas3D.Font.Color);

    Canvas3D.DisablePrimitiveID; //SampleID
    Canvas3D.DisablePrimitiveID; //ModelID
    glEnable(GL_NORMALIZE);
    glDisable(GL_LINE_SMOOTH);
  finally
    Screen.Cursor := OldCursor;
    glEndList;
  end;
  TableModel.Close;
end;

function TGBHoles.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayHolesOptions;
begin
  OptionDlg := TfmDisplayHolesOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

destructor TGBHoles.Destroy;
begin
  FDholes.Free;
  FDholes := nil;
  inherited;
end;

{________________________ TGBPoints________________________ }

constructor TGBPoints.Create(AOwner: TComponent);
begin
  inherited;
  Drawmode := dmPoint;
end;

procedure TGBPoints.CreateOpenGLList;
begin
  inherited;
end;

{ TPoints2D }

class function TGBPoints2D.ModelType: integer;
begin
  Result := mtPoints2D;
end;

procedure TGBPoints2D.CreateOpenGLList;
begin
  BuildPointList;
end;

function TGBPoints2D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayPoints2DOptions;
begin
  OptionDlg := TfmDisplayPoints2DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

{_________________________ TGBPoints3D___________________________ }

class function TGBPoints3D.ModelType: integer;
begin
  Result := mtPoints3D;
end;

procedure TGBPoints3D.CreateOpenGLList;
begin
  BuildPointList;
end;

function TGBPoints3D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayPoints3DOptions;
begin
  OptionDlg := TfmDisplayPoints3DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

{_______________________ TGBPolygons__________________________ }

procedure TGBPolygons.Assign(Source: TPersistent);
begin
  if Source is TGBPolygons then
  begin
    BeginUpdate;
    try
      inherited;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

constructor TGBPolygons.Create(AOwner: TComponent);
begin
  inherited;
  DrawMode := dmLine;
end;

procedure TGBPolygons.CreateOpenGLList;
type
  PVertexArr = ^TVertexArr;
  TVertexArr = array of TVertex;

  {sub} procedure AddVertex(X, Y, Z: Double; V: PVertexArr; var Count: integer);
  begin
    if High(V^) < Count then
      SetLength(V^, Count + 20);
    V^[Count].V[0] := X;
    V^[Count].V[1] := Y;
    V^[Count].V[2] := Z;
    Count := Count + 1;
  end;

var
  TableVertex:  TTable;
  Vertices:     array of TVertex;
  VertexCount:  integer;
  X1, X2, Y1, Y2, Z1, Z2: Single;
  Mode, Size:   Integer;
  ValueReal:    Single;
  ValueInteger: integer;
  R, G, B:      double;
  PolyOld, PolyNew: integer;
  OldCursor:    TCursor;
  IsVisible:    boolean;
  PolygonType:  TPolygonType;
  TextLabel:    string;

begin
  if FOGLListNo = 0 then
    FOGLListNo := glGenLists(1);

  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  glNewList(FOGLListNo, GL_COMPILE);

  TableModel.Open;
  TableModel.First;
  TableVertex := TTable.Create(nil);
  TableVertex.TableName :=
    ChangeModelTable(DirPolygonPoly, DirPolygonVertex, ModelName);
  case DrawMode of
    dmPoint: glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
    dmLine:     //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      Mode := GL_LINE_LOOP;
    dmFill:
    begin
      Mode := GL_POLYGON;
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    end;
  end;

  try
    glLineStipple(1, $FFFF);
    case ActiveAttribute.AttributeType of
      atReal:
      begin
        if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueReal := 0
        else
          ValueReal :=
            TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant;
        GetLevelStyle(ValueReal, R, G, B, IsVisible);
      end;
      atInteger:
      begin
        if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueInteger := 0
        else
          ValueInteger :=
            TableModel.FieldByName(ActiveAttribute.AttributeName).AsInteger;
        //GetAttribMaterial(ValueInteger,False,R,G,B,IsVisible);
      end;
    end;

    glColor4f(R, G, B, 0.5);
    SetLength(Vertices, 5);

    glPushName(ModelID);
    glPushName(0);

    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;

    TableVertex.AddIndex('', fldID, [ixPrimary], ''); //Adds primary index
    TableVertex.Open;
    TableVertex.First;
    X1 := TableVertex.FieldByName(fldX).AsFloat;
    Y1 := TableVertex.FieldByName(fldY).AsFloat;
    Z1 := TableVertex.FieldByName(fldZ).AsFloat;
    TableVertex.Next;

    PolyOld     := TableVertex.FieldByName(fldID_POLY).AsInteger;
    VertexCount := 0;
    while not TableVertex.EOF do
    begin
      X2      := TableVertex.FieldByName(fldX).AsFloat;
      Y2      := TableVertex.FieldByName(fldY).AsFloat;
      Z2      := TableVertex.FieldByName(fldZ).AsFloat;
      PolyNew := TableVertex.FieldByName(fldID_POLY).AsInteger;
      if (PolyOld = PolyNew) then //the old polygon
      begin
        if True or IsVisible then
        begin
          if (DrawMode = dmFill) then
          begin
            AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
          end
          else
          begin
            glPointSize(SizeFactor);
            glLineWidth(SizeFactor);
            glBegin(Mode);
            glVertex3f(X1, Y1, Z1);
            glVertex3f(X2, Y2, Z2);
            glEnd;
          end;
        end;
      end
      else // the new polygon
      begin
        Canvas3D.SetPrimitiveID(PolyOld);
        if DrawMode = dmFill then
        begin
          AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
          if VertexCount > 2 then
          begin
            SetLength(Vertices, VertexCount);
            try
              Canvas3D.Polygon(Vertices);
            except
            end;
          end;
        end;
        if TableModel.Locate(fldID, PolyOld, [loPartialKey]) then
        begin
          if TableModel.FieldByName(ActiveAttribute.AttributeName).IsNull then
            ValueReal := 0
          else
            ValueReal :=
              TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant;
        end;
        GetLevelStyle(ValueReal, R, G, B, IsVisible);
        glColor3f(R, G, B);

        VertexCount := 0;
        Vertices    := nil;
        SetLength(Vertices, 5);
        PolyOld := PolyNew;
      end;
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;
      if TextAttributesChecked then
        Canvas3D.TextOut(TextLabel, X1, Y1, Z1, 0, 0, 0, 0, 0, 0, Canvas3D.Font.Color);
      glColor3f(R, G, B);
      TableVertex.Next;
    end;
    // Add Vertices in dmFill
    if DrawMode = dmFill then
    begin
      AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
      if VertexCount > 2 then
      begin
        SetLength(Vertices, VertexCount);
        try
          Canvas3D.Polygon(Vertices);
        except
        end;
      end;
    end;
  finally
    glPopName;
    glPopName;
    glEndList;
    //    glFinish;
    Canvas3D.DisablePrimitiveID;
    Canvas3D.DisablePrimitiveID;
    Vertices := nil;
  end;
  TableVertex.Close;
  DeleteFile(TableVertex.TableName + TableInd);   //Deletes primary index
  TableVertex.Free;             //Don't change the position with deletefile
  TableModel.Close;
  Screen.Cursor := OldCursor;
end;

function TGBPolygons.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayPolygonsOptions;
begin
  OptionDlg := TfmDisplayPolygonsOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBPolygons.ModelType: integer;
begin
  Result := mtPolygons;
end;

{_________________________ TGBTin__________________________ }

constructor TGBTin.Create(AOwner: TComponent);
begin
  inherited;
  DrawMode := dmLine;
end;

procedure TGBTin.CreateOpenGLList;
var
  Red, Green, Blue: double;
  X, Y, Z, G, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, G1, G2, G3: double;
  ValueInteger: Integer;
  OldCursor:  TCursor;
  IsVisible:  boolean;
  Nx, Ny, Nz: double;
  Mode, Size: Integer;

  V1, V2, V3:  integer;
  TableVertex: TTable;

begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  TableModel.Open;
  TableModel.First;
  TableVertex := TTable.Create(Self);
  TableVertex.TableName := ChangeModelTable(DirTinFaces, DirTinVertices, ModelName);
  case DrawMode of
    dmPoint: Mode := GL_POINTS;
    dmLine: Mode  := GL_LINE_LOOP;
    dmFill: Mode  := GL_TRIANGLES; //GL_TRIANGLE_STRIP;
  end;
  if FOGLListNo = 0 then
    FOGLListNo := glGenLists(1);
  try
    TableVertex.AddIndex('', fldID, [ixPrimary], ''); //Adds primary index
    TableVertex.Open;
    TableVertex.First;

    glNewList(FOGLListNo, GL_COMPILE);
    glLineStipple(1, $FFFF);
    glShadeModel(GL_SMOOTH);

    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;
    while not TableModel.EOF do
    begin
      X  := TableVertex.FieldByName(fldX).AsFloat;
      Y  := TableVertex.FieldByName(fldY).AsFloat;
      Z  := TableVertex.FieldByName(fldZ).AsFloat;
      V1 := TableModel.FieldByName(fldV1).AsInteger;
      if TableVertex.FindKey([V1]) then
      begin
        X1 := TableVertex.FieldByName(fldX).AsFloat;
        Y1 := TableVertex.FieldByName(fldY).AsFloat;
        Z1 := TableVertex.FieldByName(fldZ).AsFloat;
      end;
      V2 := TableModel.FieldByName(fldV2).AsInteger;
      if TableVertex.FindKey([V2]) then
      begin
        X2 := TableVertex.FieldByName(fldX).AsFloat;
        Y2 := TableVertex.FieldByName(fldY).AsFloat;
        Z2 := TableVertex.FieldByName(fldZ).AsFloat;
      end;
      V3 := TableModel.FieldByName(fldV3).AsInteger;
      if TableVertex.FindKey([V3]) then
      begin
        X3 := TableVertex.FieldByName(fldX).AsFloat;
        Y3 := TableVertex.FieldByName(fldY).AsFloat;
        Z3 := TableVertex.FieldByName(fldZ).AsFloat;
      end;
      G := TableModel.FieldByName(ActiveAttribute.AttributeName).AsFloat;
      GetLevelStyle(G, Red, Green, Blue, IsVisible); //in triangle
      glColor3f(Red, Green, Blue);
      if IsVisible then
      begin
        Canvas3D.SetPrimitiveID(TableModel.FieldByName(fldID).AsInteger);
        if SizeMode = 0 then
          Size := SizeFactor
        else
        begin
          if (ActiveAttribute.AttributeType = atReal) then
            Size := Round(SizeFactor * (G - ActiveAttribute.MinValue) /
              (ActiveAttribute.MaxValue - ActiveAttribute.MinValue))
          else  //for integer types
            Size := Round(SizeFactor * G);
        end;
        Cross(X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, NX, NY, NZ);
        glPointSize(Size);
        glLineWidth(Size);
        glBegin(Mode);
        glNormal3f(NX, NY, NZ);
        glVertex3f(X1, Y1, Z1);
        glVertex3f(X2, Y2, Z2);
        glVertex3f(X3, Y3, Z3);
        glEnd;
      end;
      TableModel.Next;
    end;
    Canvas3D.DisablePrimitiveID;
    Canvas3D.DisablePrimitiveID;
    glEndList;
  finally
    TableVertex.Close;
    DeleteFile(TableVertex.TableName + TableInd);   //Delete primary index
    TableVertex.Free;
    Screen.Cursor := OldCursor;
  end;
  TableModel.Close;
end;

function TGBTin.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayTinOptions;
begin
  OptionDlg := TfmDisplayTinOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBTin.ModelType: integer;
begin
  Result := mtTins;
end;


procedure TGBTin.Assign(Source: TPersistent);
begin
  if Source is TGBTin then
  begin
    BeginUpdate;
    try
      inherited;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;


{_________________________ TGBGrids_________________________ }

procedure TGBGrid.Assign(Source: TPersistent);
begin
  if Source is TGBGrid then
  begin
    BeginUpdate;
    try
      inherited;
      DX := TGBGrid(Source).DX;
      DY := TGBGrid(Source).DY;
      DZ := TGBGrid(Source).DZ;
      NX := TGBGrid(Source).NX;
      NY := TGBGrid(Source).NY;
      NZ := TGBGrid(Source).NZ;
      ShowAsHeight := TGBGrid(Source).ShowAsHeight;
      ShowByCenters := TGBGrid(Source).ShowByCenters;
      ShowIsolines := TGBGrid(Source).ShowIsolines;
      ShowVectors := TGBGrid(Source).ShowVectors;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

constructor TGBGrid.Create(AOwner: TComponent);
begin
  inherited;
  DrawMode := dmLine;
end;

{________________ TGrid2D________________ }

class function TGBGrid2D.ModelType: integer;
begin
  Result := mtGrids2D;
end;

//Old
(*
procedure TGrid2D.CreateOpenGLList;

var
  I, J, K, ID: Integer;
  R, G, B, R1, R2, R3, R4,
  G1, G2, G3, G4,
  B1, B2, B3, B4: Double;
  X, Y, Z, X1, X2, X3, X4,
  Y1, Y2, Y3, Y4,
  Z1, Z2, Z3, Z4: TGLfloat;
  DXdiv2, DYdiv2: TGLfloat;
  Mode, Size: TGLint;
  ValueReal: TGLfloat;
  ValueInteger: Integer;


begin

      if IsVisible then
      begin
        Canvas3D.SetPrimitiveID(TableModel.FieldByName(fldID).AsInteger);
        if SizeMode = 0 then
          Size := SizeFactor
        else
        begin
          if (ActiveAttribute.AttributeType = atReal)  then
            Size := Round(SizeFactor *
                    (ValueReal - ActiveAttribute.MinValue)/
                    (ActiveAttribute.MaxValue - ActiveAttribute.MinValue))
          else
            Size := Round(SizeFactor * ValueReal);
        end;
        if AsCylinder then
          Canvas3D.SolidCylinder(X, Y, Z, X, Y, Z + Size, Radius)
        else
        begin
          glPointSize(Size);
          glLineWidth(Size);
          glBegin(Mode);
            glVertex3f(X-DXDiv2, Y-DYDiv2, Z);
            glVertex3f(X+DXDiv2, Y-DYDiv2, Z);
            glVertex3f(X+DXDiv2, Y+DYDiv2, Z);
            glVertex3f(X-DXDiv2, Y+DYDiv2, Z);
          glEnd;
        end;
      end;

      TableModel.Next;
    end;

end;
*)

{ -----------------------------------------------------------------
  If there are missing nodes in Grid table after editing
  then we need to create a full grid array [1..NX, 1..NY] of nodes
  and take quadruples of nodes to visualize the grid
 ----------------------------------------------------------------- }

procedure TGBGrid2D.CreateOpenGLList;

var
  I, J, K, ID, IDz: integer;
  Xarr, Yarr, Zarr, Garr: TMatrix2D;
  Xa, Ya, Za:     TVectorD; // the array of z levels
  R, G, B, R1, R2, R3, R4, G1, G2, G3, G4, B1, B2, B3, B4: double;
  X, Y, Z, X1, X2, X3, X4, Y1, Y2, Y3, Y4, Z1, Z2, Z3, Z4: Single;
  DXdiv2, DYdiv2: Single;
  Mode, Size:     Integer;
  ValueReal:      Single;
  ValueInteger:   integer;

  OldCursor: TCursor;
  IsVisible: boolean;
  TextLabel: string;
  N, SF:     integer;
  Contour:   Single;
  Radius:    double;

begin
  OldCursor := Screen.Cursor;
  TableModel.AddIndex('', fldID, [ixPrimary], ''); //Adds primary index
  TableModel.Open;
  uCommon.ReadParFile(ModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  case DrawMode of
    dmPoint: Mode := GL_POINTS;
    dmLine: Mode  := GL_LINE_LOOP;
    dmFill: Mode  := GL_POLYGON;
  end;
  if FOGLListNo = 0 then
    FOGLListNo := glGenLists(1);
  try
    Screen.Cursor := crHourGlass;
    //    Canvas3D.Font.Assign(Font);
    glNewList(FOGLListNo, GL_COMPILE);

    DXdiv2 := DX / 2;
    DYdiv2 := DY / 2;

    //DZdiv2:=DZ/2;
    Radius := Min(DXdiv2, DYdiv2);

    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;

    SetLength(Xarr, NX, NY);
    SetLength(Yarr, NX, NY);
    SetLength(Zarr, NX, NY);
    SetLength(Garr, NX, NY);
    SetLength(Xa, NX);
    SetLength(Ya, NY);
    SetLength(Za, ActiveAttribute.Levels.LevelCount);

    //Fill grid2d array with values
    TableModel.First;
    K := 0;
    for J := 0 to NY - 1 do
      for I := 0 to NX - 1 do
      begin
        Inc(K);
        ID := K;
        Xarr[I, J] := XO + I * DX + DX / 2;
        Yarr[I, J] := YO + J * DY + DY / 2;
        // Search the table to find a record with currect ID
        if TableModel.FindKey([ID]) then
        begin
          Zarr[I, J] := TableModel.FieldByName(fldZ).AsFloat;
          if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
            Garr[I, J] := 0
          else
            Garr[I, J] :=
              TableModel.FieldByName(ActiveAttribute.AttributeName).Value;

          {
          if LabelTextChecked then
             Garr[I,J] := TableModel.FieldByName(LabelTextField).AsString;
          }
        end
        else
          Garr[I, J] := ZO;

        //Display attribute as height
        if ShowAsHeight then
          Zarr[i, j] := Garr[i, j];

      end;
    K := 0;
    if ShowByCenters then
      //Display grid arrays
      for J := 0 to NY - 2 do
        for I := 0 to NX - 2 do
        begin
          Inc(K);
          X1 := Xarr[I, J];
          Y1 := Yarr[I, J];
          Z1 := Zarr[I, J];
          GetLevelStyle(Garr[I, J], R1, G1, B1, IsVisible);
          X2 := Xarr[I + 1, J];
          Y2 := Yarr[I + 1, J];
          Z2 := Zarr[I + 1, J];
          GetLevelStyle(Garr[I + 1, J], R2, G2, B2, IsVisible);
          X3 := Xarr[I + 1, J + 1];
          Y3 := Yarr[I + 1, J + 1];
          Z3 := Zarr[I + 1, J + 1];
          GetLevelStyle(Garr[I + 1, J + 1], R3, G3, B3, IsVisible);
          X4 := Xarr[I, J + 1];
          Y4 := Yarr[I, J + 1];
          Z4 := Zarr[I, J + 1];
          GetLevelStyle(Garr[I, J + 1], R4, G4, B4, IsVisible);

          if IsVisible then
          begin
            Canvas3D.SetPrimitiveID(K);
            glBegin(Mode);
            glColor3f(R1, G1, B1);
            glVertex3f(X1, Y1, Z1);
            glColor3f(R2, G2, B2);
            glVertex3f(X2, Y2, Z2);
            glColor3f(R3, G3, B3);
            glVertex3f(X3, Y3, Z3);
            glColor3f(R4, G4, B4);
            glVertex3f(X4, Y4, Z4);
            glEnd;
          end;

          if TextAttributesChecked then //Only for first vertex!
          begin
            //   Canvas3D.TextOut(Tarr[I,J], X1, Y1, Z1, 0, 0, 0, 1, 1, 1, Font.Color);
          end;
        end
    else   //Show by vertices
      for J := 0 to NY - 1 do
        for I := 0 to NX - 1 do
        begin
          Inc(K);
          X := Xarr[I, J];
          Y := Yarr[I, J];
          Z := Zarr[I, J];
          GetLevelStyle(Garr[I, J], R, G, B, IsVisible);
          glColor3f(R, G, B);
          if IsVisible then
          begin
            // Canvas3D.SetPrimitiveID(TableModel.FieldByName(fldID).AsInteger);
            if SizeMode = 0 then
              Size := SizeFactor
            else
              Size := Round(SizeFactor * ValueReal);
            glPointSize(Size);
            glLineWidth(Size);
            glBegin(Mode);
            glVertex3f(X - DXDiv2, Y - DYDiv2, Z);
            glVertex3f(X + DXDiv2, Y - DYDiv2, Z);
            glVertex3f(X + DXDiv2, Y + DYDiv2, Z);
            glVertex3f(X - DXDiv2, Y + DYDiv2, Z);
            glEnd;
          end;

          if TextAttributesChecked then  //Only X, Y, Z or G
          begin
            Canvas3D.TextOut(TableModel.Fields[AttribTextNo + 1].AsString,
              X + Size + 0.5, Y, Z, 0, 0, 0, 1, 1, 1, Canvas3D.Font.Color);
            //Canvas3D.TextOut(Tarr[I,J], X1, Y1, Z1, 0, 0, 0, 1, 1, 1, Canvas3D.Font.Color);
            //TextLabel := TableModel.FieldByName(LabelTextField).AsString;
            //Canvas3D.TextOut(TextLabel, X, Y, Z, 0, 0, 0, 1, 1, 1, Font.Color);
          end;
        end;
    if ShowIsolines then
    begin
      Isolines := TGLIsolines.Create(nil);
      K := 0; // getting isoline levels in increasing order
      for I := ActiveAttribute.Levels.LevelCount - 1 downto 0 do
      begin
        Za[K] := ActiveAttribute.Levels[I].Value;
        //        Isolines.LineList.Items[K] := ActiveAttribute.Levels[I].Value;
        Inc(K);
      end;
      //    ConRec(Garr, 1, NX, 1, NY, Xa, Ya, ActiveAttribute.Levels.LevelCount, Za);
      {
      Isolines.CoordRange      := 100; //?
      Isolines.IsolineState := ilsReady;

      if Assigned(Isolines) and (Isolines.IsolineState = ilsReady) then
      begin
        SF := Round(Canvas3D.Width / Isolines.CoordRange);
        //      with Canvas3D do
        begin
          //        pen.color := clblack;
          //        pen.width := 1;
          //        pen.mode := pmcopy;
          Isolines.LineList.Count := ActiveAttribute.Levels.LevelCount;
          for N := 0 to Isolines.LineList.Count - 1 do
          begin
///            Initialize_Isolining(Garr, NX, NY, ActiveAttribute.Levels[N].Value);
            with TGLIsoline(Isolines.LineList.Items[N]) do
              if NP > 2 then
              begin
                Canvas3D.MoveTo(Trunc(Line^[0].X * SF),
                  Trunc((Isolines.CoordRange - Line^[0].Y) * SF), 0);
                for I := 1 to NP - 1 do
                  Canvas3D.LineTo(Trunc(Line^[I].X * SF),
                    Trunc((Isolines.CoordRange - Line^[I].Y) * SF), 0);
              end;
          end;
          Isolines.Free;
        end;
      end;
      }
    end;
  finally
    Canvas3D.DisablePrimitiveID;
    Canvas3D.DisablePrimitiveID;
    DeleteFile(TableModel.TableName + TableInd);   //Delete primary index
    glEndList;
  end;
  Screen.Cursor := OldCursor;
  TableModel.Close;
end;


function TGBGrid2D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayGrid2DOptions;
begin
  OptionDlg := TfmDisplayGrid2DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

{_____________________ TGrid3D_____________________ }
procedure TGBGrid3D.CreateOpenGLList;

var
  I, EdgeColor: longint;
  R, G, B:      double;
  //  X, Y, Z: Double;

  Mode: Integer;
  Size: Single;

  ValueReal:    Single;
  ValueInteger: integer;
  OldCursor:    TCursor;
  IsVisible:    boolean;
  StippleTable: TTable;
  StippleImage: TImage;
  //DBImage : TDBImage;

  //From GLScene.TCube v.8 - not used now!
  {
  procedure BuildCube(XCenter, YCenter, ZCenter,
                      CubeWidth, CubeHeight, CubeDepth: TGLfloat;
                      Mode: TGLEnum);   //Mode := GL_QUADS
  }

  {sub} procedure DrawCube(CentX, CentY, CentZ, SizeX, SizeY, SizeZ: Single;
    Mode: Cardinal);
  var
    I: integer;
    Nx, Ny, Nz: double;

  const
    SignK: array[0..1] of double = (-1, 1);

  type                 //   (3)-----(7)      (0,1,1)-(1,1,1)
    TVertex3D = record //  z/|      /|      z / |     / |
      X: Single;       // (1)-----(5)|   (0,0,1)-(1,0,1)|
      Y: Single;       //  | |y    | |      |   |y  |   |
      Z: Single;       //  |(2)----|(6)     |(0,1,0)|(1,1,0)
    end;               //  |/      |/       | /     | /

  var                  // (0)-----(4)x   (0,0,0)-(1,0,0)x
    Vert: array[0..7] of TVertex3D;

    {sub} procedure DrawSide(V1, V2, V3, V4: TVertex3D);
    begin
      glBegin(Mode);
      Cross(V1.X, V1.Y, V1.Z, V2.X, V2.Y, V2.Z, V3.X, V3.Y, V3.Z, Nx, Ny, Nz);
      glNormal3f(Nx, Ny, Nz);
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
    DrawSide(Vert[0], Vert[2], Vert[3], Vert[1]); //left
    DrawSide(Vert[7], Vert[6], Vert[4], Vert[5]); //right
    DrawSide(Vert[0], Vert[4], Vert[6], Vert[2]); //bottom
    DrawSide(Vert[7], Vert[5], Vert[1], Vert[3]); //top
    DrawSide(Vert[0], Vert[1], Vert[5], Vert[4]); //near
    DrawSide(Vert[7], Vert[3], Vert[2], Vert[6]); //far
  end;

  {sub} procedure DrawStippledCube(CentX, CentY, CentZ, SizeX, SizeY, SizeZ: Single;
    Mode: Cardinal; Mask: TImage);
  var
    I: integer;
    Nx, Ny, Nz: double;
  const
    SignK: array[0..1] of double = (-1, 1);
      //   (3)-----(7)      (0,1,1)-(1,1,1)
      //  z/|      /|      z / |     / |
      // (1)-----(5)|   (0,0,1)-(1,0,1)|
      //  | |y    | |      |   |y  |   |
      //  |(2)----|(6)     |(0,1,0)|(1,1,0)
      //  |/      |/       | /     | /
  var // (0)-----(4)x   (0,0,0)-(1,0,0)x
    Vert: array[0..7] of TVector3D;

    {sub} procedure DrawSide(V1, V2, V3, V4: TVector3D);
    begin
      glBegin(Mode);
      Cross(V1.V[0], V1.V[1], V1.V[2], V2.V[0], V2.V[1], V2.V[2], V3.V[0], V3.V[1],
            V3.V[2], Nx, Ny, Nz);
      glNormal3d(Nx, Ny, Nz);
      //   StippledPolygonI([V1,V2,V3,V4],Mask);
      glEnd;
    end;

  begin
    for I := 0 to 7 do
    begin
      Vert[I].V[0] := CentX + SignK[(I and 4) shr 2] * SizeX;
      Vert[I].V[1] := CentY + SignK[(I and 2) shr 1] * SizeY;
      Vert[I].V[2] := CentZ + SignK[(I and 1) shr 0] * SizeZ;
    end;
    DrawSide(Vert[0], Vert[2], Vert[3], Vert[1]); //left
    DrawSide(Vert[7], Vert[6], Vert[4], Vert[5]); //right
    DrawSide(Vert[0], Vert[4], Vert[6], Vert[2]); //bottom
    DrawSide(Vert[7], Vert[5], Vert[1], Vert[3]); //top
    DrawSide(Vert[0], Vert[1], Vert[5], Vert[4]); //near
    DrawSide(Vert[7], Vert[3], Vert[2], Vert[6]); //far
  end;

begin
  case DrawMode of
    dmPoint:
    begin
      Mode := GL_POINTS;
      glPolygonMode(GL_FRONT_AND_BACK, GL_POINTS);
    end;
    dmLine:
    begin
      Mode := GL_QUADS;
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    end;
    dmFill:
    begin
      Mode := GL_POLYGON;
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    end;
  end;

  TableModel.Open;
  TableModel.First;
  uCommon.ReadParFile(ModelName, XO, YO, ZO, DX, DY, DZ, NX, NY, NZ);
  OldCursor := Screen.Cursor;
  if FOGLListNo = 0 then
    FOGLListNo := glGenLists(1);
  try
    Screen.Cursor := crHourGlass;
    // glEnable(GL_CULL_FACE);
    glNewList(FOGLListNo, GL_COMPILE);
    glPointSize(2);


    Size := 0.95; //1.00 - without chinks
    begin
      DX := Size * DX / 2;
      DY := Size * DY / 2;
      DZ := Size * DZ / 2;
    end;
    { glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,PGLfloat(@spec));
      glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,PGLfloat(@amb));
      glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,PGLfloat(@diff));
      glMaterialf (GL_FRONT_AND_BACK,GL_SHININESS,128);{}
    glLineWidth(3);
    glLineStipple(1, $FFFF);
    StippleTable := nil;
    //  StippleImage:=nil;

    case ActiveAttribute.AttributeType of
      atReal:
      begin
        if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueReal := 0
        else
          ValueReal :=
            TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant;
        GetLevelStyle(ValueReal, R, G, B, IsVisible);
      end;
      atInteger:
      begin
        if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueInteger := 0
        else
          ValueInteger :=
            TableModel.FieldByName(ActiveAttribute.AttributeName).AsInteger;
        //GetAttribMaterial(ValueInteger,False,R,G,B,IsVisible);
        StippleTable := TTable.Create(Self);
        StippleTable.TableName := ExpandPath(DirDataReference) + 'DEFAULT';
        StippleTable.Open;
        StippleImage := TImage.Create(Self);
        StippleImage.Width := 32;
        StippleImage.Height := 32;

        StippleTable.Locate(fldID,
          TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant, []);
        //    StippleImage.Canvas.StretchDraw(Rect(0,0,33,33),StippleTableModel.FieldByName('PATTERN'));
        StippleImage.Picture.Assign(StippleTable.FieldByName('PATTERN'));
      end;
    end;

    while not TableModel.EOF do
    begin
      {      X := TableModel.FieldByName(fldX).AsFloat;
            Y := TableModel.FieldByName(fldY).AsFloat;
            Z := TableModel.FieldByName(fldZ).AsFloat;
           }
      (*    if IsIntegerField(AttribType) then
            begin
              StippleTable.Locate(fldID,TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant,[]);
      //      StippleImage.Canvas.StretchDraw(Rect(0,0,33,33),StippleTableModel.FieldByName('PATTERN'));
              StippleImage.Picture.Assign(StippleTableModel.FieldByName('PATTERN'));
              DrawStippledCube (TableModel.FieldByName(fldX).AsFloat,
                                TableModel.FieldByName(fldY).AsFloat,
                                TableModel.FieldByName(fldZ).AsFloat,
      //                 DX,DZ,DY);{DX,DY,DZ,StippleImage);{}
            end
            else *)
      begin
        if TableModel.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueReal := 0
        else
          ValueReal :=
            TableModel.FieldByName(ActiveAttribute.AttributeName).AsVariant;

        // ValueReal := TableModel.FieldByName(ActiveAttribute.AttributeName).Value;
        GetLevelStyle(ValueReal, R, G, B, IsVisible);
        glColor3f(R, G, B);
        if IsVisible then
        begin
          DrawCube(TableModel.FieldByName(fldX).AsFloat,
            TableModel.FieldByName(fldY).AsFloat,
            TableModel.FieldByName(fldZ).AsFloat,
            DX, DY, DZ, Mode);

        end;
      end;
      TableModel.Next;
    end;
    if ActiveAttribute.AttributeType = atInteger then
    begin
      StippleTable.Free;
      //      StippleImage.Free;
    end;
  finally
    glEndList;
    Screen.Cursor := OldCursor;
  end;
  TableModel.Close;
end;

function TGBGrid3D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayGrid3DOptions;
begin
  OptionDlg := TfmDisplayGrid3DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBGrid3D.ModelType: integer;
begin
  Result := mtGrids3D;
end;
//--------------------------------------------------\\
{____________________ TMesh________________________ }

procedure TGBMesh.Assign(Source: TPersistent);
begin
  if Source is TGBMesh then
  begin
    BeginUpdate;
    try
      inherited;

      ShowContours := TGBMesh(Source).ShowContours;
      ShowVectors  := TGBMesh(Source).ShowVectors;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

constructor TGBMesh.Create(AOwner: TComponent);
begin
  inherited;
  DrawMode   := dmLine;
  SizeFactor := 3;
end;


 {______________________ TGBMesh2D _____________________}

procedure TGBMesh2D.CreateOpenGLList;

var
  I, K:    Integer;
  Nn:      TAffineDblVector;
  NE:      Integer;   //Element;
  R, G, B: double;

  Xp, Yp, Zp: array[0..2] of double;
  N: array [0..2] of integer;

  Mode, Size:   Integer;
  ValueReal:    Single;
  ValueInteger: Integer;
  IsVisible:    Boolean;
  OldCursor:    TCursor;

  LinkTable, NodeTable, ElementTable: TFileName;
  QueryLink, QueryMatrix, QueryElement, QueryNode: TQuery;
  TableVertex: TTable;

begin
  ElementTable := ModelName;
  NodeTable    := ChangeModelTable(DirMesh2DFaces, DirMesh2DVertices, ElementTable);
  LinkTable    := ExpandPath(DirMesh2D) + NameOnly(ModelName);

  TableVertex := TTable.Create(Self);
  TableVertex.TableName := NodeTable;

  try
    OldCursor     := Screen.Cursor;
    Screen.Cursor := crHourGlass;

    case DrawMode of
      dmPoint:
      begin
        TableModel.Close;
        TableModel.TableName := TableVertex.TableName;
        BuildPointList; //Points of Mesh2D Centers
        Exit;
      end;
      dmLine:
        Mode := GL_LINE_LOOP;
      dmFill:
        Mode := GL_TRIANGLE_STRIP; //GL_TRIANGLES;
    end;

    QueryLink    := TQuery.Create(Self);
    QueryMatrix  := TQuery.Create(Self);
    QueryElement := TQuery.Create(Self);
    QueryNode    := TQuery.Create(Self);

    if FOGLListNo = 0 then
      FOGLListNo := glGenLists(1);

    glNewList(FOGLListNo, GL_COMPILE);
    glLineStipple(1, $AAAA);
    glShadeModel(GL_SMOOTH);

    glPushName(ModelID);
    glPushName(0);

    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;

    //First selects all records from the table of elements
    QueryElement.SQL.Add('SELECT * ' + ' FROM "' + ElementTable + '"');
    QueryElement.Open;

    case ActiveAttribute.AttributeType of
      atReal:
      begin
        if QueryElement.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueReal := 0
        else
          ValueReal := QueryElement.FieldByName(ActiveAttribute.AttributeName).AsVariant;
        GetLevelStyle(ValueReal, R, G, B, IsVisible);
      end;
      atInteger:
      begin
        if QueryElement.FieldByName(ActiveAttribute.AttributeName).isNull then
          ValueInteger := 0
        else
          ValueInteger :=
            QueryElement.FieldByName(ActiveAttribute.AttributeName).AsInteger;
      end;
    end;

    TableVertex.Open;
    QueryElement.First;
    NE := 0;
    while not QueryElement.EOF do
    begin
      Inc(NE);
      NE := QueryElement[fldID_ELEMENT];
      QueryLink.SQL.Clear;
      QueryLink.SQL.Add('SELECT LINK.ID_NODE ' + 'FROM "' + LinkTable +
        '" LINK ' + 'WHERE LINK.ID_ELEMENT=' + IntToStr(NE));
      QueryLink.Open;

      QueryLink.First;
      K := 0;
      while not QueryLink.EOF do
      begin
        N[K] := QueryLink[fldID_NODE];
        if TableVertex.Locate(fldID_NODE, N[K], []) then
        begin
          Xp[K] := TableVertex.FieldByName(fldX).AsFloat;
          Yp[K] := TableVertex.FieldByName(fldY).AsFloat;
          Zp[K] := TableVertex.FieldByName(fldZ).AsFloat;
        end;
        Inc(K);
        QueryLink.Next;
      end;
      ValueReal := QueryElement.FieldByName(ActiveAttribute.AttributeName).AsFloat;
      GetLevelStyle(ValueReal, R, G, B, IsVisible); //For triangles
      glColor3f(R, G, B);

      if IsVisible then
      begin
        Canvas3D.SetPrimitiveID(QueryElement[fldID_ELEMENT]);
        Cross(Xp[0], Yp[0], Zp[0], Xp[1], Yp[1], Zp[1], Xp[2], Yp[2], Zp[2],
          Nn.V[0], Nn.V[1], Nn.V[2]);
        glBegin(Mode);
        glNormal3f(Nn.V[0], Nn.V[1], Nn.V[2]);
        glVertex3f(Xp[0], Yp[0], Zp[0]);
        glVertex3f(Xp[1], Yp[1], Zp[1]);
        glVertex3f(Xp[2], Yp[2], Zp[2]);
        glEnd;
      end;
      QueryElement.Next;
    end;
    glEnd;
    glEndList;
  finally
    Screen.Cursor := OldCursor;
    QueryMatrix.Free;
    QueryElement.Free;
    QueryNode.Free;
    QueryLink.Free;
    TableVertex.Close;
    TableVertex.Free;
    Canvas3D.DisablePrimitiveID;
    Canvas3D.DisablePrimitiveID;
  end;
end;

function TGBMesh2D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayMesh2DOptions;
begin
  OptionDlg := TfmDisplayMesh2DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBMesh2D.ModelType: integer;
begin
  Result := mtMeshes2D;
end;


//_____________________ TMesh3D ___________________\\
procedure TGBMesh3D.QueryData;
var
  OldCursor: TCursor;
  NodeTable, ElementTable, LinkTable: TFileName;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Query.Close;
    Query.SQL.Clear;

    ElementTable := ModelName;
    NodeTable    := ChangeModelTable(DirMesh3DElement, DirMesh3DNode, ElementTable);
    LinkTable    := ChangeModelTable(DirMesh3DElement, DirMesh3D, ElementTable);

    Query.SQL.Add('SELECT DISTINCT E.' + fldID_ELEMENT + ', L.' +
      fldID_NO + ', N.X X, N.Y Y, N.Z Z,' + CRLF + ' E."' +
      ActiveAttribute.AttributeName + '" G_Attribute' + CRLF + 'FROM "' +
      ElementTable + '" E,"' + LinkTable + '" L,"' + NodeTable +
      '" N' + CRLF + 'WHERE E.ID_ELEMENT=L.' + fldID_ELEMENT +
      ' AND L.' + fldID_NODE + ' = N.ID_NODE' + CRLF +
      'ORDER BY E.ID_ELEMENT, L.' + fldID_NO);

    Query.Open;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure DrawMesh8f(VV: array of TVector3D);
var
  GL_Mode: cardinal;
  //   (3)-----(7)      (0,1,1)-(1,1,1)
  //  z/|      /|      z / |     / |
  // (1)-----(5)|   (0,0,1)-(1,0,1)|
  //  | |y    | |      |   |y  |   |
  //  |(2)----|(6)     |(0,1,0)|(1,1,0)
  //  |/      |/       | /     | /
  // (0)-----(4)x   (0,0,0)-(1,0,0)x
  {sub} procedure DrawSide(V1, V2, V3, V4: TVector3D);
  var
    Nx, Ny, Nz: double;
  begin
    glBegin(GL_Mode);
    Cross(V1.V[0], V1.V[1], V1.V[2],
          V2.V[0], V2.V[1], V2.V[2],
          V3.V[0], V3.V[1], V3.V[2],
          Nx, Ny, Nz);
    glNormal3d(Nx, Ny, Nz);
    glVertex3dv(@V1);
    glVertex3dv(@V2);
    glVertex3dv(@V3);
    glVertex3dv(@V4);
    glEnd;
  end;

begin
  if (Low(VV) = 0) and (High(VV) = 7) then
  begin
    GL_Mode := {GL_QUADS;//{} GL_POLYGON;
    DrawSide(VV[0], VV[2], VV[3], VV[1]); //left
    DrawSide(VV[7], VV[6], VV[4], VV[5]); //right
    DrawSide(VV[0], VV[4], VV[6], VV[2]); //bottom
    DrawSide(VV[7], VV[5], VV[1], VV[3]); //top
    DrawSide(VV[0], VV[1], VV[5], VV[4]); //near
    DrawSide(VV[7], VV[3], VV[2], VV[6]); //far
  end;
end;

procedure TGBMesh3D.CreateOpenGLList;
 //   (3)-----(7)      (0,1,1)-(1,1,1)
 //  z/|      /|      z / |     / |
 // (1)-----(5)|   (0,0,1)-(1,0,1)|
 //  | |y    | |      |   |y  |   |
 //  |(2)----|(6)     |(0,1,0)|(1,1,0)
 //  |/      |/       | /     | /
 // (0)-----(4)x   (0,0,0)-(1,0,0)x
var
  I: longint;
  X1, Y1, Z1: Single;
  R, G, B: double;
  OldCursor: TCursor;
  Isvisible: boolean;

  IDCell1, IDCell2: integer;
  V:   TVector3D;
  Vertexs3f: array of TVector3D;
  VNo: byte;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if FOGLListNo = 0 then
      FOGLListNo := glGenLists(1);
    glNewList(FOGLListNo, GL_COMPILE);
    Query.First;
    glLineWidth(2);
    glLineStipple(1, $FFFF);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

    Canvas3D.EnablePrimitiveID;
    Canvas3D.SetPrimitiveID(ModelID);
    Canvas3D.EnablePrimitiveID;
    try
      IDCell1 := Query[fldID_ELEMENT];
      VNo     := 0;
      SetLength(Vertexs3f, 8);
      try
        for I := 0 to Query.RecordCount - 1 do
        begin
          GetLevelStyle(Query.FieldByName('G_Attribute').Value, R, G, B,
            IsVisible);
          glColor3f(R, G, B);

          X1 := Query.FieldByName(fldX).AsFloat;
          Y1 := Query.FieldByName(fldY).AsFloat;
          Z1 := Query.FieldByName(fldZ).AsFloat;
          V  := MakeVector3d([X1, Y1, Z1]);

          Vertexs3f[VNo] := V;
          VNo := Min(VNo + 1, 7);

          Query.Next;
          IDCell2 := Query[fldID_ELEMENT];

          if (IDCell1 <> IDCell2) or Query.EOF then
          begin
            VNo     := 0;
            IDCEll1 := IDCell2;
            if IsVisible then
            begin
              Canvas3D.SetPrimitiveID(Query.FieldByName(fldID_ELEMENT).AsInteger);
              DrawMesh8f(Vertexs3f);
            end;
          end;
        end;
      finally
        Vertexs3f := nil;
      end;
    finally
      Canvas3D.DisablePrimitiveID;
      Canvas3D.DisablePrimitiveID;
    end;

    glEndList;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function TGBMesh3D.SelectOptions: boolean;
var
  OptionDlg: TfmDisplayMesh3DOptions;
begin
  OptionDlg := TfmDisplayMesh3DOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBMesh3D.ModelType: integer;
begin
  Result := mtMeshes3D;
end;


//================================ TSolids =================================\\
procedure TGBSolids.CreateOpenGLList;
var
  I: longint;
  X1, Y1, Z1: Single;
  R, G, B: double;
  Mode, Size: Integer;

  OldCursor: TCursor;
  Isvisible: boolean;
  IDCell1, IDCell2: integer;

begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if FOGLListNo = 0 then
      FOGLListNo := glGenLists(1);
    glNewList(FOGLListNo, GL_COMPILE);
    Query.First;
    //glLineStipple(1,$AAAA);
    glLineWidth(2);
    IDCell1 := Query[fldID_TRIANGLE];
    glBegin(GL_TRIANGLE_STRIP);
    for I := 0 to Query.RecordCount - 1 do
    begin
      GetLevelStyle(Query.FieldByName('G_Attribute').Value, R, G, B, IsVisible);
      glColor3f(R, G, B);

      X1 := Query.FieldByName(fldX).AsFloat;
      Y1 := Query.FieldByName(fldY).AsFloat;
      Z1 := Query.FieldByName(fldZ).AsFloat;

      glVertex3d(X1, Y1, Z1);
      if not Query.EOF then
      begin
        Query.Next;
        IDCell2 := Query[fldID_TRIANGLE];
        if IDCell1 <> IDCell2 then
        begin
          glEnd;
          glBegin(GL_TRIANGLE_STRIP);
          IDCell1 := IDCell2;
        end;
      end;
    end;
    glEnd;
    glEndList;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function TGBSolids.SelectOptions: boolean;

var
  OptionDlg: TfmDisplaySolidsOptions;

begin
  OptionDlg := TfmDisplaySolidsOptions.Create(Self);
  try
    OptionDlg.Assign(Self);
    Result := OptionDlg.Execute;
    if Result then
      Assign(OptionDlg);
  finally
    OptionDlg.Free;
  end;
end;

class function TGBSolids.ModelType: integer;
begin
  Result := mtSolids;
end;

procedure TGBSolids.QueryData;
var
  OldCursor: TCursor;
  VerticesName, FacesName, CellsName, LinkName: TFileName;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    Query.Close;
    Query.SQL.Clear;

    CellsName   := ModelName;
    FacesName  := ChangeModelTable(DirSolidCells, DirSolidFaces, CellsName);
    VerticesName := ChangeModelTable(DirSolidCells, DirSolidVertices, CellsName);
    LinkName   := ChangeModelTable(DirSolidCells, DirSolids, CellsName);

    Query.SQL.Add('SELECT DISTINCT L.' + fldID_FACE + ', L.' +
      fldID_NO + ', N.X X, N.Y Y, N.Z Z,' + CRLF + '   N."' +
      ActiveAttribute.AttributeName + '" N."G_Attribute"' + CRLF +
      'FROM "' + LinkName + '" L,"' + VerticesName + '" N' + CRLF +
      'WHERE L.' + fldID_VERTEX + ' = N.ID' + CRLF + 'ORDER BY L.' +
      fldID_FACE + ', L.' + fldID_NO);

    Query.Open;
  finally
    Screen.Cursor := OldCursor;
  end;
end;


//==================== TAttribute ==============================\\

destructor TGBAttribute.Destroy;
begin
  if FLevels <> nil then
    FLevels.Free;
  inherited;
end;

procedure TGBAttribute.LegendDialog;
begin
  fmMapLegend := TfmMapLegend.Create(nil);
  try
    fmMapLegend.Caption := fmMapLegend.Caption + ' ' + {LoadResStringW(@SgbFor) +
      ' ' + } AttributeName;
    fmMapLegend.Levels  := Levels;
    if fmMapLegend.Execute then
    begin
      Levels := fmMapLegend.Levels;
      if (Levels.Mode = lmFromFile) and (Levels.FileName <> '') then
        Levels.SaveToFile(Levels.FileName);    //Forced save
    end;
  finally
    fmMapLegend.Free;
  end;
end;

function TGBAttribute.GetLevel: TLevel;
begin
  Result := Levels.GetLevelByValue(AsReal);
end;

function TGBAttribute.GetLevels: TLegend;
begin
  if FLevels = nil then
  begin
    FLevels := TLegend.Create(nil);
    FLevels.MinValue := MinValue;
    FLevels.MaxValue := MaxValue;
    FLevels.BuildDefault;
  end;
  Result := FLevels;
end;

procedure TGBAttribute.SetChecked(const Value: boolean);
begin
  FChecked := Value;
end;

procedure TGBAttribute.SetLevels(const Value: TLegend);
begin
  Levels.Assign(Value);
  if (Collection is TAttributesCollection) and
    (TAttributesCollection(Collection).GetOwner is TGBAttributes) and
    (TGBAttributes(TAttributesCollection(Collection).GetOwner).Model <> nil) then
    with (TGBAttributes(TAttributesCollection(Collection).GetOwner).Model) do
    begin
      State := State - [msOpenGLListValid];
    end;
end;

procedure TGBAttribute.SetMaxValue(const Value: Single);
begin
  FMaxValue := Value;
end;

procedure TGBAttribute.SetMinValue(const Value: Single);
begin
  FMinValue := Value;
end;

//=========================== TAttributes ==========================\\

function TGBAttributes.AddAttribute(AName: string; AType: TGBAttributeType;
  AMinValue, AMaxValue: double): TGBAttribute;
begin
  Result := TGBAttribute(FAttributeList.Add);
  Result.AttributeName := AName;
  Result.AttributeType := AType;
  Result.MinValue := AMinValue;
  Result.MaxValue := AMaxValue;
end;

function TGBAttributes.AttributeByName(AName: string): TGBAttribute;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to AttributeCount - 1 do
  begin
    if CompareText(AName, Attributes[I].AttributeName) = 0 then
    begin
      Result := Attributes[I];
      Break;
    end;
  end;
end;

procedure TGBAttributes.Clear;
begin
  if FAttributeList <> nil then
    FAttributeList.Clear;
end;

constructor TGBAttributes.Create(AModel: TGBModel);
begin
  inherited Create(AModel);
  FAttributeList := TAttributesCollection.Create(Self, TGBAttribute);
  FModel := AModel;
  //  if FModel<>nil then FModel.Notification(Self, opInsert);
end;

destructor TGBAttributes.Destroy;
begin
  FAttributeList.Free;
  inherited;
end;

function TGBAttributes.GetAttributeCount: integer;
begin
  Result := FAttributeList.Count;
end;

function TGBAttributes.GetAttributes(Index: integer): TGBAttribute;
begin
  Result := TGBAttribute(FAttributeList.Items[Index]);
end;

procedure TGBAttributes.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    if (AComponent = FModel) then
      FModel := nil
    else
      //if (AComponent=FAttributeList) then FAttributeList:=nil
  ;
end;

procedure TGBAttributes.SetAttributes(Index: integer; const Value: TGBAttribute);
begin
  FAttributeList.Items[Index].Assign(Value);
end;

procedure TGBAttributes.SetModel(const Value: TGBModel);
begin
  if FModel <> Value then
  begin
    if FModel <> nil then
      RemoveFreeNotification(FModel);
    FModel := Value;
    if FModel <> nil then
      FreeNotification(FModel);
  end;
end;

{ TAttributesCollection }

function TAttributesCollection.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

end.
