//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------

unit fMapWindow;

(* The MapWindow with an OpenGL viewer *)

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.IniFiles,
  System.Math,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs, 
  Vcl.ExtCtrls, 
  Vcl.Menus, 
  Vcl.Grids, 
  Vcl.DBGrids,
  Vcl.StdCtrls, 
  Vcl.Printers,
  Vcl.Clipbrd,
  Vcl.Imaging.Jpeg,
  Vcl.ToolWin,
  Vcl.ComCtrls,

  Data.DB,
  Bde.DBTables,

  fInitialForm,
  uGlobals, 
  uModels,
  fDisplayHolesOptions,
  fDisplayPoints2DOptions,
  fDisplayPoints3DOptions,
  fDisplayPolygonsOptions,
  fDisplayTinOptions,
  fDisplayGrid2DOptions,
  fDisplayGrid3DOptions,
  fDisplayMesh2DOptions,
  fDisplayMesh3dOptions,

  GBGraphics,
  GLS.OpenGlx ;

type
  TfmMapWindow = class(TfmInitialForm)
    TableMap:     TTable;
    PopupMenuMap: TPopupMenu;
    MenuItemMapSavePolygon: TMenuItem;
    MenuItemMapDeletePolygon: TMenuItem;
    GBCanvas:     TGBCanvas;
    StatusBar: TStatusBar;
    procedure GBCanvasCallLists(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GBCanvasDblClick(Sender: TObject);
    procedure GBCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure GBCanvasMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MenuItemMapSavePolygonClick(Sender: TObject);
    procedure GBCanvasSelectPrimitive(Sender: TObject; X, Y: integer;
      var Primitive: TDrawPrimitive);
  private
    FModelList:  TModelList;
    FModelIndex: integer;
    procedure DefineCoords;
    procedure ReloadModel;
    procedure SetModel(const Value: TGBModel);
    function GetModel: TGBModel;
    function GetModelCount: integer;
    procedure SetModelList(const Value: TModelList);
    function GetModelList: TModelList;
    procedure SetModelIndex(const Value: integer);
    function GetModelIndex: integer;
  public
    OnCreation:  boolean;
    //Current Model frame
    FrameMinX, FrameMaxX: TGLfloat;
    FrameMinY, FrameMaxY: TGLfloat;
    FrameMinZ, FrameMaxZ: TGLfloat;
    OGLListAxis: Cardinal;

    //Indicates the Active Model in List
    property ModelIndex: integer Read GetModelIndex Write SetModelIndex;
    //Active Model list within Models
    property ModelList: TModelList Read GetModelList Write SetModelList;
    //Counts Models in Collector
    property ModelCount: Integer Read GetModelCount;
    //Active model
    property Model: TGBModel Read GetModel Write SetModel;

    procedure InitMapWin(AFileName: string; AModelType: integer);
    procedure OpenNewModel(AFileName: string; AModelType: integer);
    procedure LightDlg;
    procedure BackColorDlg;

    procedure PerformCrossSection;

    procedure ViewScale;
    procedure ViewTop;
    procedure ViewBottom;
    procedure ViewFront;
    procedure ViewBack;
    procedure ViewRight;
    procedure ViewLeft;
    procedure ViewRotate;
    procedure View3D;
    procedure ViewDefault;
    procedure ViewZoomToAll;
    procedure ViewZoomToModel;
    procedure ViewXYZ;
    procedure ViewXY;
    procedure ViewXZ;
    procedure ViewYZ;
    procedure Zoom;

    procedure ChangeModel(AModelID: integer);
    procedure RemoveModel(AModelID: integer);
    procedure SaveAs(FileName: TFileName);

    procedure Grid2DSelectCell;
    procedure Grid2DSelectRow;
    procedure Grid2DSelectCol;

    procedure ExportToMIF(AFileName: TFileName);
    procedure DoExportToMIF;
    procedure Print; overload;
  private
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmMapWindow: TfmMapWindow;

//======================================================================
implementation
//======================================================================

uses
  dDialogs,
  dBase,
  fGeoblock,
  uCommon,
  uResStrings,
  uProfuns,
  fFileExport,
  fMapLight,
  fViewProjectManager,
  fViewScale,
  fViewRotate,
  fDrawFillStyle,
  fDrawLineStyle,
  fRecordEditor,
  fTableWindow;


{$R *.dfm}

var
  DAX: TGLfloat;  //=1
  DAY: TGLfloat;
  DAZ: TGLfloat;

  DVX: TGLfloat;
  DVY: TGLfloat;
  DVZ: TGLfloat;  //=1

  DefwVectorX: TGLfloat;
  DefwVectorY: TGLfloat;
  DefwVectorZ: TGLfloat;

  LastDir: string;

const
  DefwAngleX = -20;
  DefwAngleY = 0;
  DefwAngleZ = -20;
  DefwAngleL = 0;
  //Axis
  ShowXYBottom: boolean = True;
  ShowXZBack: boolean = True;
  ShowYZLeft: boolean = True;


//====================================================================

procedure TfmMapWindow.OpenNewModel(AFileName: string; AModelType: integer);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    try
      if ModelIndex >= 0 then
        fmGeoblock.EnableMapItems(Model.ModelType, False);   //Enables menu commands
    except
    end;
    case AModelType of
      mtDholes: ModelIndex   := ModelList.Add(TGBHoles.Create(Self, AFileName));
      mtPoints2D: ModelIndex := ModelList.Add(TGBPoints2D.Create(Self, AFileName));
      mtPoints3D: ModelIndex := ModelList.Add(TGBPoints3D.Create(Self, AFileName));
      mtPolygons: ModelIndex := ModelList.Add(TGBPolygons.Create(Self, AFileName));
      mtTins: ModelIndex      := ModelList.Add(TGBTin.Create(Self, AFileName));
      mtSolids: ModelIndex   := ModelList.Add(TGBSolids.Create(Self, AFileName));
      mtGrids2D: ModelIndex   := ModelList.Add(TGBGrid2D.Create(Self, AFileName));
      mtGrids3D: ModelIndex   := ModelList.Add(TGBGrid3D.Create(Self, AFileName));
      mtMeshes2D:
      begin
        AFileName  := ChangeModelTable(DirMesh2D, DirMesh2DFaces, AFileName);
        ModelIndex := ModelList.Add(TGBMesh2D.Create(Self, AFileName));
      end;
      mtMeshes3D:
      begin
        AFileName  := ChangeModelTable(DirMesh3D, DirMesh3DElement, AFileName);
        ModelIndex := ModelList.Add(TGBMesh3D.Create(Self, AFileName));
      end;
      else
        ModelIndex := ModelList.Add(TGBModel.Create(Self, AFileName));
    end;
    try
      Model.Canvas3D := GBCanvas;
      Model.Visible  := True;
      Model.Open;
      Model.ActiveAttributeNo := 2; // Default value Z for all spatial datasets 
      try
        Caption := Model.ModelName;
        fmViewProjectManager.AddToManager;
      except
        on E: Exception do
        begin
          StatusBar.Panels[0].Text := E.Message;
          raise EAbort.Create(E.Message);
        end;
      end;
    except
      on E: Exception do
      begin
        StatusBar.Panels[0].Text := E.Message;
        raise EAbort.Create(E.Message);
      end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;

  try
    fmGeoblock.EnableMapItems(AModelType, True);
    fmGeoblock.EnableFileItems(True);
  except
  end;
  ViewZoomToAll;
end;

procedure TfmMapWindow.InitMapWin(AFileName: string; AModelType: integer);
begin
  try
    OpenNewModel(AFileName, AModelType);
    with Model do
    begin
      GBCanvas.MinX := MinX;
      GBCanvas.MaxX := MaxX;
      GBCanvas.MinY := MinY;
      GBCanvas.MaxY := MaxY;
      GBCanvas.MinZ := MinZ;
      GBCanvas.MaxZ := MaxZ;
      FrameMinX     := MinX;
      FrameMaxX     := MaxX;
      FrameMinY     := MinY;
      FrameMaxY     := MaxY;
      FrameMinZ     := MinZ;
      FrameMaxZ     := MaxZ;
    end;
    DefineCoords;
    with GBCanvas do
    begin
      wAngleX  := DefwAngleX;
      wAngleY  := DefwAngleY;
      wAngleZ  := DefwAngleZ;
      wAngleL  := DefwAngleL;
      wVectorX := DefwVectorX;
      wVectorY := DefwVectorY;
      wVectorZ := DefwVectorZ;
    end;
    //      fmMain.ToolButtonView3D.Click;
  except
    raise EAbort.Create('');
  end;
  //  fmMapWindow.WindowState := wsMaximized;
  ReadIniFile;
  GBCanvas.Visible := True;
end;


//===================== private =========================
procedure TfmMapWindow.DefineCoords;
var
  RatioF: TGLfloat;
begin
  with GBCanvas do
  begin
    //Size of Map
    LX := abs(FrameMaxX - FrameMinX);
    LY := abs(FrameMaxY - FrameMinY); // if LY=0 then Exit;
    LZ := abs(FrameMaxZ - FrameMinZ);

    CenterX := (FrameMaxX + FrameMinX) / 2;
    CenterY := (FrameMaxY + FrameMinY) / 2;
    CenterZ := (FrameMaxZ + FrameMinZ) / 2;

    DVX := abs(FrameMaxX - FrameMinX) / 100;
    DVY := abs(FrameMaxY - FrameMinY) / 100;
    DVZ := abs(FrameMaxZ - FrameMinZ) / 100;

    DefwVectorX := 0;
    DefwVectorY := 0;
    DefwVectorZ := 2000;
    //DefwVectorZ:=-CenterZ;

    if (ClientHeight = 0) { or (ClientWidth>ClientHeight){} then
      RatioF := 1
    else
      RatioF := Max(ClientHeight, ClientWidth) / Min(ClientHeight, ClientWidth);
    //    RatioF:=RatioF/RatioF;
    //    DefwVectorZ:=Max(Max(LX,LY),LZ)*3*RatioF;//Best Fit
    //    DefwVectorZ:=Max(Max(LX,LY),LZ)*RatioF/ArcTan(DegreeToRadian(ViewAngle/2));//Best Fit
    //    DefwVectorZ:=LY/(Sin(5*pi/180)/Cos(5*pi/180));
    DefwVectorZ := RatioF * Norm([LX, LY, LZ]) / (2 * Tan(DegToRad(ViewAngle / 2)));
  end;
end;

//====================== public ======================
procedure TfmMapWindow.BackColorDlg;
begin
  with dmDialogs do
  begin
    ColorDialog.Color := GBCanvas.Color;
    ColorDialog.HelpContext := 409; //IDH_MapBackground;
    if ColorDialog.Execute then
      GBCanvas.Color := ColorDialog.Color;
  end;
end;

//======================== Load Datasets =======================
procedure TfmMapWindow.RemoveModel(AModelID: integer);
var
  AttributeNo: integer;
  Temp: TGBModel;
begin
  Temp := ModelList.Items[AModelID];
  ModelList.Delete(AModelID);
  Temp.Free;
  if ModelCount > 0 then
  begin
    if AModelID <= (ModelCount - 1) then
      ModelIndex := AModelID
    else
      ModelIndex := ModelCount - 1;
    AttributeNo := Model.ActiveAttributeNo;
  end;
end;

procedure TfmMapWindow.ChangeModel(AModelID: integer);
var
  AttribNo: integer;
begin
  if (Model.ModelName = ModelList[AModelID].ModelName) and
    (ModelIndex = AModelID) then
    Exit;
  try
    //Save previous parameters
    ModelList[ModelIndex].Assign(Model);
  except
  end;
  fmGeoblock.EnableMapItems(ModelList[ModelIndex].ModelType, False);
  ModelIndex := AModelID;
  fmGeoblock.EnableMapItems(ModelList[AModelID].ModelType, True);
  Model.Assign(ModelList[ModelIndex]);
  {  try   //if visible for current level of data
      GBCanvas.MinX:=Model.MinX;
      GBCanvas.MaxX:=Model.MaxX;
      GBCanvas.MinY:=Model.MinY;
      GBCanvas.MaxY:=Model.MaxY;
      GBCanvas.MinZ:=Model.MinZ;
      GBCanvas.MaxZ:=Model.MaxZ;

      GBCanvas.wVectorX:=-GBCanvas.CenterX;
      GBCanvas.wVectorY:=-GBCanvas.CenterY;
    except
    end;{}
  AttribNo := Model.ActiveAttributeNo;
  fmMapWindow.FormPaint(Self);
  //fmMain.SetToolBars(Model.ModelType,True);
end;

procedure TfmMapWindow.FormResize(Sender: TObject);
begin
  DefineCoords;
  fmMapWindow.FormPaint(Self);
  StatusBar.Panels[1].Text := IntToStr(ClientWidth);
  StatusBar.Panels[2].Text := IntToStr(ClientHeight);
end;

procedure TfmMapWindow.FormPaint(Sender: TObject);
begin
  try
    GBCanvas.Redraw;
  except
    raise
  end;
end;

 //Display Options
 //*****************************************

procedure TfmMapWindow.PerformCrossSection;
begin
  {...}
end;

procedure TfmMapWindow.LightDlg;
begin
  with TfmMapLighting.Create(Self) do
  begin
    SpinEditX.Value     := round(GBCanvas.Lighting.Position.X);
    SpinEditY.Value     := round(GBCanvas.Lighting.Position.Y);
    SpinEditZ.Value     := round(GBCanvas.Lighting.Position.Z);
    PanelAmbient.Color  := GBCanvas.Lighting.AmbientColor.Color;
    PanelDiffuse.Color  := GBCanvas.Lighting.DiffuseColor.Color;
    PanelSpecular.Color := GBCanvas.Lighting.SpecularColor.Color;
    if ShowModal = mrOk then
    begin
      GBCanvas.Lighting.AmbientColor.Color := PanelAmbient.Color;
      GBCanvas.Lighting.DiffuseColor.Color := PanelDiffuse.Color;
      GBCanvas.Lighting.SpecularColor.Color := PanelSpecular.Color;
      GBCanvas.Lighting.Position.X := SpinEditX.Value;
      GBCanvas.Lighting.Position.Y := SpinEditY.Value;
      GBCanvas.Lighting.Position.Z := SpinEditZ.Value;
      GBCanvas.Lighting.Enable     := CheckBoxLightColor.Checked;
      FormPaint(Self);
    end;
    Free;
  end;
end;

procedure TfmMapWindow.ViewScale;
begin
  with TfmViewScale.Create(Self) do
  begin
    with GBCanvas do
    begin
      ScaleRatio := wVectorZ;
      if ShowModal = mrOk then
      begin
        wVectorZ := GetScaleRatio;
        FormPaint(Self);
      end;
    end;
    Free;
  end;
end;

procedure TfmMapWindow.ViewTop;
begin
  GBCanvas.ViewTop;
end;

procedure TfmMapWindow.ViewBottom;
begin
  GBCanvas.ViewBottom;
end;

procedure TfmMapWindow.ViewFront;
begin
  GBCanvas.ViewFront;
end;

procedure TfmMapWindow.ViewBack;
begin
  GBCanvas.ViewBack;
end;

procedure TfmMapWindow.ViewRight;
begin
  GBCanvas.ViewRight;
end;

procedure TfmMapWindow.ViewLeft;
begin
  GBCanvas.ViewLeft;
end;

procedure TfmMapWindow.View3D;
begin
  GBCanvas.View3D;
end;

procedure TfmMapWindow.ViewRotate;
begin
  with TfmViewRotate.Create(Self) do
  begin
    with GBCanvas do
    begin
      ScrollBarSlope.Position := (round(wAngleX) + 360) mod 360;
      ScrollBarAzimuth.Position := (round(wAngleZ) + 360) mod 360;
      EditDistance.Text := FloatToStr(wVectorZ);
      if ShowModal = mrOk then
        with GBCanvas do
        begin
          wAngleZ  := ScrollBarAzimuth.Position;
          wAngleX  := ScrollBarSlope.Position;
          wVectorZ := StrToFloat(EditDistance.Text);
          FormPaint(Self);
        end;
    end;
    Free;
  end;
end;

procedure TfmMapWindow.ViewDefault;
begin
  with GBCanvas do
  begin
    if fmGeoblock.Observe3D.Checked then
    begin
      wAngleY := DefwAngleY;
      wAngleX := DefwAngleX;
      wAngleZ := DefwAngleZ;
      wAngleL := DefwAngleL;
    end;
    FrameMinX := MinX;
    FrameMaxX := MaxX;
    FrameMinY := MinY;
    FrameMaxY := MaxY;
    FrameMinZ := MinZ;
    FrameMaxZ := MaxZ;
    DefineCoords;
    wVectorX := DefwVectorX;
    wVectorY := DefwVectorY;
    wVectorZ := DefwVectorZ;
  end;
  FormPaint(Self);
end;

procedure TfmMapWindow.Zoom;
begin
  GBCanvas.ZoomIn;
end;

procedure TfmMapWindow.ViewXYZ;
begin
  with GBCanvas do
  begin
    View3D;
    ProjectionMode := pmPerspective;
  end;
end;

procedure TfmMapWindow.ViewXY;
begin
  with GBCanvas do
  begin
    ViewTop;
    ProjectionMode := pmOrtho;
  end;
end;

procedure TfmMapWindow.ViewXZ;
begin
  with GBCanvas do
  begin
    GBCanvas.ViewFront;
    ProjectionMode := pmOrtho;
  end;
end;

procedure TfmMapWindow.ViewYZ;
begin
  with GBCanvas do
  begin
    GBCanvas.ViewLeft;
    ProjectionMode := pmOrtho;
  end;
end;

procedure TfmMapWindow.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  K: Float;
begin
  if ssShift in Shift then
    K := 10
  else
    K := 1;
  with GBCanvas do
  begin
    case Key of
      VK_HOME:
      begin
        if fmGeoblock.Observe3D.Checked then
        begin
          wAngleY := DefwAngleY;
          wAngleX := DefwAngleX;
          wAngleZ := DefwAngleZ;
          wAngleL := DefwAngleL;
        end;
        wVectorX := DefwVectorX;
        wVectorY := DefwVectorY;
        wVectorZ := DefwVectorZ;
      end;
      VK_ADD:
        wVectorZ := wVectorZ - k * abs(wVectorZ) / 100 - DVZ;
      VK_SUBTRACT:
        wVectorZ := wVectorZ + k * abs(wVectorZ) / 100 + DVZ;

      else
        if ssAlt in Shift then // Strife
          case Key of
            VK_LEFT:
              wVectorX := wVectorX - K * DVX;
            VK_RIGHT:
              wVectorX := wVectorX + K * DVX;
            VK_UP:
              wVectorY := wVectorY + K * DVY;
            VK_DOWN:
              wVectorY := wVectorY - K * DVY;
            else
              Exit;
          end
        else if fmGeoblock.ViewVolumeXYZ.Checked then
        begin // rotate
          case Key of
            VK_LEFT:
            begin
              wAngleZ := wAngleZ - K * DAZ;
              if wAngleZ < -360 then
                wAngleZ := wAngleZ + 360;
            end;
            VK_RIGHT:
            begin
              wAngleZ := wAngleZ + K * DAZ;
              if wAngleZ > 360 then
                wAngleZ := wAngleZ - 360;
            end;
            VK_UP:
            begin
              wAngleX := wAngleX - K * DAX;
              if wAngleX < -360 then
                wAngleX := wAngleX + 360;
            end;
            VK_DOWN:
            begin
              wAngleX := wAngleX + K * DAX;
              if wAngleX > 360 then
                wAngleX := wAngleX - 360;
            end;
            else
              Exit;
          end;
        end
        else
          Exit;
    end;
  end;
  FormPaint(Self);
end;

procedure TfmMapWindow.SaveAs(FileName: TFileName);
var
  Image:     TBitmap;
  JpegImage: TJPEGImage;
  FileExt:   TFileName;
  MetaFile:  TMetaFile;
begin
  GBCanvas.Redraw;
  if FileName <> '' then
  begin
    Image := TBitmap.Create;
    try
      Image.Width  := ClientRect.Right - ClientRect.Left;
      Image.Height := ClientRect.Bottom - ClientRect.Top;
      Image.Canvas.CopyRect(ClientRect, Canvas, ClientRect);

      FileExt := ExtractFileExt(FileName);
      if (StrUpper(PChar(FileExt)) = '.JPG') or
        (StrUpper(PChar(FileExt)) = '.JPEG') then
      begin
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.Assign(Image);
          JPEGImage.SaveToFile(FileName);
        finally
          JPEGImage.Free;
        end;
      end
      else if (StrUpper(PChar(FileExt)) = '.WMF') then
      begin
        Metafile := TMetafile.Create;
        try
          Metafile.Assign(Image);
          Metafile.SaveToFile(FileName);
        finally
          Metafile.Free;
        end;
      end
      else
        Image.SaveToFile(FileName);
    finally
      Image.Free;
    end;
  end;
end;

procedure TfmMapWindow.ViewZoomToAll;
var
  I: integer;
  AllMinX, AllMaxX, AllMinY, AllMaxY, AllMinZ, AllMaxZ: double;
begin
  with GBCanvas do
  begin
    AllMinX := MinX;
    AllMaxX := MaxX;
    AllMinY := MinY;
    AllMaxY := MaxY;
    AllMinZ := MinZ;
    AllMaxZ := MaxZ;
    for I := 0 to ModelCount - 1 do
      if ModelList[I].Visible then
        with TGBModel(ModelList[I]) do
        begin
          AllMinX := MinX;
          AllMaxX := MaxX;
          AllMinY := MinY;
          AllMaxY := MaxY;
          AllMinZ := MinZ;
          AllMaxZ := MaxZ;
        end;
    for I := 0 to ModelCount - 1 do
      if ModelList[I].Visible then
        with ModelList[I] do
        begin
          if AllMinX > MinX then
            AllMinX := MinX;
          if AllMaxX < MaxX then
            AllMaxX := MaxX;
          if AllMinY > MinY then
            AllMinY := MinY;
          if AllMaxY < MaxY then
            AllMaxY := MaxY;
          if AllMinZ > MinZ then
            AllMinZ := MinZ;
          if AllMaxZ < MaxZ then
            AllMaxZ := MaxZ;
        end;
    MinX      := AllMinX;
    MaxX      := AllMaxX;
    MinY      := AllMinY;
    MaxY      := AllMaxY;
    MinZ      := AllMinZ;
    MaxZ      := AllMaxZ;
    FrameMinX := AllMinX;
    FrameMaxX := AllMaxX;
    FrameMinY := AllMinY;
    FrameMaxY := AllMaxY;
    FrameMinZ := AllMinZ;
    FrameMaxZ := AllMaxZ;
    DefineCoords;
    wVectorX := DefwVectorX;
    wVectorY := DefwVectorY;
    wVectorZ := DefwVectorZ;
  end;
  FormPaint(Self);
end;

procedure TfmMapWindow.ViewZoomToModel;
begin
  try {if active for current data level}
    GBCanvas.MinX := Model.MinX;
    GBCanvas.MaxX := Model.MaxX;
    GBCanvas.MinY := Model.MinZ;
    GBCanvas.MaxY := Model.MaxZ;
    GBCanvas.MinZ := Model.MinY;
    GBCanvas.MaxZ := Model.MaxY;

    GBCanvas.wVectorX := -GBCanvas.CenterX;
    GBCanvas.wVectorY := -GBCanvas.CenterY;
  except
  end;
  FormPaint(Self);
end;

procedure TfmMapWindow.GBCanvasMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  NewShift: TShiftState;
begin
  //  if not (ssLeft in Shift) or ToolButton1.Down then
  with StatusBar, GBCanvas do
  begin
    if ssLeft in Shift then
      NewShift := [ssLeft]
    else
      NewShift := [];
    if ssCtrl in Shift then
      Include(NewShift, ssCtrl)
    else
    if ssShift in Shift then
      Include(NewShift, ssShift);
    if ssLeft in Shift then
      if fmGeoblock.ViewSelect.Checked then
        Exclude(NewShift, ssLeft)
      else
      if fmGeoblock.ViewScroll.Checked then
        Include(NewShift, ssAlt)
      else
      if fmGeoblock.ViewPan.Checked then
        Include(NewShift, ssLeft)
      else
      if fmGeoblock.aZoomInOut.Checked then
      begin
        Include(NewShift, ssRight);
        Exclude(NewShift, ssLeft);
      end;
    DefaultMouseMove(Sender, NewShift, X, Y);

    //Write to panel sections
    Panels[0].Text := 'FPS:' + FloatToStr(FPS);
    try
      Panels[1].Text := fldX + ':' + FloatToStr(RoundTo(WorldCursorX, -3));
    except
    end;
    try
      Panels[2].Text := fldY + ':' + FloatToStr(RoundTo(WorldCursorY, -3));
    except
    end;
    try
      Panels[3].Text := fldZ + ':' + FloatToStr(RoundTo(WorldCursorZ, -3));
    except
    end;
  end;
end;

procedure TfmMapWindow.GBCanvasMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  MouseDown: TPoint;
begin
  GBCanvas.Redraw;
  // GBCanvas.SelectObjects(X,Y);
  GBCanvasMouseMove(Sender, [], X, Y);
  GBCanvas.Redraw;
end;

procedure TfmMapWindow.GBCanvasDblClick(Sender: TObject);
begin
  GBCanvas.DefaultMouseDblClick(Sender);
end;

procedure TfmMapWindow.MenuItemMapSavePolygonClick(Sender: TObject);
var
  TablePolygon: TTable;
  TableVertex: TTable;
  PolyID: integer;
  VertID: integer;
  I:      integer;
  PolyX, PolyY, PolyZ: double;
  OldCursor: TCursor;
begin
  OldCursor    := Screen.Cursor;
  TablePolygon := TTable.Create(Application);
  try
    Screen.Cursor := crHourGlass;
    TableVertex   := TTable.Create(Application);
    try
      TablePolygon.TableName := Model.ModelName;
      TableVertex.TableName  :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, Model.ModelName);
      TablePolygon.Open;

      PolyID := TablePolygon.FieldByName(fldID).AsInteger;
      while not TablePolygon.EOF do
      begin
        TablePolygon.Next;
        if PolyID < TablePolygon.FieldByName(fldID).AsInteger then
          PolyID := TablePolygon.FieldByName(fldID).AsInteger;
      end;
      PolyID := PolyID + 1;

      TableVertex.Open;
      VertID := TableVertex.FieldByName(fldID).AsInteger;
      while not TableVertex.EOF do
      begin
        TableVertex.Next;
        if VertID < TableVertex.FieldByName(fldID).AsInteger then
          VertID := TableVertex.FieldByName(fldID).AsInteger;
      end;
      VertID := VertID + 1;

      PolyX := 0;
      PolyY := 0;
      PolyZ := 0;

      TablePolygon.Append;
      TablePolygon.FieldByName(fldID).AsInteger := PolyID;
      TablePolygon.FieldByName(fldID_TYPE).AsInteger := integer(ptPolygon);
      // TablePolygon.FieldByName(fldName).AsString := fmMain.ComboBoxPolygonType.Text;
      TablePolygon.Post;

      TablePolygon.Close;
      TableVertex.Close;
    finally
      TableVertex.Free;
    end;
  finally
    TablePolygon.Free;
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfmMapWindow.Print;
begin
  Clipboard.Assign(GetFormImage);
  { Printer.BeginDoc;
    with GBCanvas do Printer.Canvas.CopyRect(Rect(0,0,Width,Height),GBCanvas.,Rect(0,0,Width,Height))
    Printer.EndDoc;{}
end;

//---------------------------------------------------------------
// Export GB data models to MIF/MIF of MapInfo files
//----------------------------------------------------------------
procedure TfmMapWindow.ExportToMIF(AFileName: TFileName);
var
  Value, ValuePrev, R, G, B, R2, G2, B2: double;
  X1, X2, Y1, Y2, Z1, Z2: double;
  IsVisible1, IsVisible2: boolean;
  AttribName:     string;
  XShift, YShift: double;
  AttribType:     TFieldType;
  Drillhole1, Drillhole2, SampleNo, SampleNo2, ProfileNo, SNo: string;
  //  ValueInt:Integer;
  OldCursor:      TCursor;

  FileNameMIF, FileNameMID: TFileName;
  FileMIF, FileMID: TextFile;
  Mode: TViewMode;

  {sub}
  function RGBToColor(R, G, B: single): TColor;
  var
    Rb, Bb, Gb: byte;
  begin
    Rb     := trunc(R * 255);
    Gb     := trunc(G * 255);
    Bb     := trunc(B * 255);
    Result := Rb * $10000 + Gb * $100 + Bb;
  end;

  {sub}
  procedure ConvertCoord(var X, Y, Z: double);
  var
    Temp: double;
  begin
    case Mode of
      vmXY: ;
      vmXZ: begin
              Temp := Y;
              Y    := Z;
              Z    := Temp;
            end;
      vmYZ:
            begin
              Temp := X;
              X    := Y;
              Y    := Z;
              Z    := Temp;
            end;
    end;
    X := X + XShift;
    Y := Y + YShift;
  end;

  {sub}
  procedure MIFPointXYZ(X, Y, Z: double; const Value: string;
  const
    DrillHole, Sample: string; Color: TColor);
  begin
    ConvertCoord(X, Y, Z);
    Writeln(FileMIF, Format('Point %f %f', [X, Y]));
    Writeln(FileMIF, Format('    Symbol (40,%d,6)', [Color]));
    Writeln(FileMID, Format('%f,%s,%s,%s', [Z, Value, (DrillHole), (Sample)]));
  end;

  {sub}
  procedure MIFTextXYZ(const Value: string; X, Y, Z: double; Color: TColor);
  begin
    if Value = '' then Exit;
    ConvertCoord(X, Y, Z);
    Writeln(FileMIF, 'Text');

    Writeln(FileMIF, Format('    %s', [CharQuotedStr(Value, '"')]));
    Writeln(FileMIF, Format('%f %f %f %f', [X, Y, X + 30, Y + 2]));
    Writeln(FileMIF, '    Font ("Arial Cyr",0,0,0)');
    Writeln(FileMIF, '    Justify Left');

    Writeln(FileMID, Format('%f,,,', [Z]));
  end;

  {sub}
  procedure MIFLine(X1, Y1, Z1, X2, Y2, Z2: double; const Value: string;
  const
    DrillHole, Sample: string; Color: TColor);
  begin
    ConvertCoord(X1, Y1, Z1);
    ConvertCoord(X2, Y2, Z2);
    Writeln(FileMIF, Format('Line %f %f %f %f', [X1, Y1, X2, Y2]));
    Writeln(FileMIF, Format('    Pen (4,2,%d)', [Color]));
    Writeln(FileMID, Format('%f,%s,%s,%s', [Z1, Value, (DrillHole), (Sample)]));
  end;

const
  N = 14;
  Colors: array[1..14] of TColor =
    (clWhite, clGray, clMaroon, clRed, clYellow, clLime, clGreen,
    clOlive, clAqua, clBlue, clNavy, clFuchsia, clPurple, clGray);
  MinG = 0;
  MaxG = 100;
var
  H: double;

  {sub}
  procedure ExportDholeToMIF;
  var
    I: longint;
    PrevVisible: boolean;

    {sub}
    procedure MIFLine(X1, Y1, Z1, X2, Y2, Z2: double; const Value: string;
    const DrillHole, Sample: string; Color: TColor);
    var
      I: integer;
    begin
      ConvertCoord(X1, Y1, Z1);
      ConvertCoord(X2, Y2, Z2);
      Writeln(FileMIF, Format('Line %f %f %f %f', [X1, Y1, X2, Y2]));
      Writeln(FileMIF, Format('    Pen (4,2,%d)', [Color]));
      Write(FileMID, Format('%f', [Z1]));
      TableMap.Prior;
      for I := 0 to TableMap.FieldCount - 1 do
        Write(FileMid, ',' + TableMap.Fields[I].AsString);
      Writeln(FileMid, '0');
      TableMap.Next;
    end;

    {sub}
    procedure MIFPointXYZ(X, Y, Z: double; const Value: string;
    const DrillHole, Sample: string; Color: TColor);
    var
      I: integer;
    begin
      ConvertCoord(X, Y, Z);
      Writeln(FileMIF, Format('Point %f %f', [X, Y]));
      Writeln(FileMIF, Format('    Symbol (40,%d,6)', [Color]));
      Write(FileMID, Format('%f', [Z1]));
      for I := 0 to TableMap.FieldCount - 1 do
        Write(FileMid, ',' + TableMap.Fields[I].AsString);
      Writeln(FileMid, ',1');
    end;

  begin
    OldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      TableMap.TableName := Model.ModelName;
      AttribName := Model.AttributeName;
      Mode := GBCanvas.ViewMode;
      X1   := GBCanvas.MinX;
      Y1   := GBCanvas.MinY;
      Z1   := GBCanvas.MinZ;
      X2   := GBCanvas.MaxX;
      Y2   := GBCanvas.MaxY;
      Z2   := GBCanvas.MaxZ;
      ConvertCoord(X1, Y1, Z1);
      ConvertCoord(X2, Y2, Z2);
      try
        TableMap.Open;

        Writeln(FileMIF, 'Version 300');
        Writeln(FileMIF, 'Charset "WindowsCyrillic"');
        Writeln(FileMIF, 'Delimiter ","');
        Writeln(FileMIF,
          Format('CoordSys NonEarth Units "m" Bounds (%f, %f) (%f, %f)',
          [X1 - 1000, Y1 - 1000, X2 + 1000, Y2 + 1000]));
        Writeln(FileMIF, Format('Columns %d', [TableMap.Fields.Count + 2]));
        case Mode of
          vmXY:
            Writeln(FileMIF, '  Z Float');
          vmXZ:
            Writeln(FileMIF, '  Y Float');
            //        vmYZ: Writeln(FileMIF,'  X Float');
          else
            Writeln(FileMIF, '  X Float');
        end;
        for I := 0 to TableMap.FieldCount - 1 do
          with TableMap.Fields[I] do
            Writeln(FileMIF, Format(FieldTypeParadoxToMIF(TableMap.Fields[I]),
              [FieldName, Size]));

        Writeln(FileMIF, '  ID_Start Integer');
        Writeln(FileMIF, 'Data');
        Writeln(FileMIF, ' ');
        TableMap.First;
        X1    := (TableMap.FieldByName(fldX).AsFloat);
        Y1    := (TableMap.FieldByName(fldY).AsFloat);
        Z1    := (TableMap.FieldByName(fldZ).AsFloat);
        Value := TableMap.FieldByName(AttribName).AsFloat;
        Model.GetLevelStyle(Value, R, G, B, IsVisible1);

        Drillhole1 := TableMap.FieldByName(fldDHOLE).AsString;
        ProfileNo  := '';
        SampleNo   := '';
        if TableMap.FindField(fldPROFILE) <> nil then
          ProfileNo := TableMap.FieldByName(fldPROFILE).AsString;
        MIFPointXYZ(X1, Y1, Z1, FloatToStr(Value), Drillhole1, SNo,
          Model.Canvas3D.Font.Color);
        PrevVisible := True;
        for I := 0 to TableMap.RecordCount - 1 do
        begin
          X2    := TableMap.FieldByName(fldX).AsFloat;
          Y2    := TableMap.FieldByName(fldY).AsFloat;
          Z2    := TableMap.FieldByName(fldZ).AsFloat;
          ValuePrev := Value;
          Value := TableMap.FieldByName(AttribName).AsFloat;
          Model.GetLevelStyle(Value, R, G, B, IsVisible2);
          if TableMap.FindField(fldLABEL) <> nil then
            SampleNo2 := TableMap.FieldByName(fldLABEL).AsString;
          Drillhole2 := TableMap.FieldByName(fldDHOLE).AsString;
          if Drillhole1 = Drillhole2 then
          begin
            if IsVisible1 then
            begin
              MIFLine(X1, Y1, Z1, X2, Y2, Z2, FloatToStr(ValuePrev),
                Drillhole1, '', RGBToColor(R, G, B));

              if not PrevVisible then
              begin
                TableMap.Prior;
                MIFPointXYZ(X1, Y1, Z1, FloatToStr(ValuePrev), Drillhole1,
                  SampleNo, Model.Canvas3D.Font.Color);
                TableMap.Next;
              end;
              if (not TGBHoles(Model).DrawTextOnChange or
                (Value <> ValuePrev)) then
                MIFPointXYZ(X2, Y2, Z2, FloatToStr(Value),
                  Drillhole1, SampleNo2, Model.Canvas3D.Font.Color);
            end;
          end
          else
          begin
            if TableMap.FindField(fldPROFILE) <> nil then
              ProfileNo := TableMap.FieldByName(fldPROFILE).AsString
            else
              ProfileNo := '';
            {           if IsVisible1 then
                        begin
                          TableMap.Prior;
                          MIFPointXYZ(X1,Y1,Z1, FloatToStr(ValuePrev),
                                      Drillhole1, SampleNo, DholeOpts.Font.Color);
                          TableMap.Next;
                        end;{}
            SampleNo2 := '';
            SNo := '';
            {            if DholeOpts.ProfileBox then SNo:=SNo+ProfileNo+' ';
                        if DholeOpts.DrillholeBox then SNo:=SNo+Drillhole2+' ';
                        if (DholeOpts.DrillholeBox)or
                           (DholeOpts.ProfileBox)or
                           (DholeOpts.SampleBox)then
                          MIFPointXYZ(X2, Y2, Z2, FloatToStr(Value), Drillhole2, SNo, DholeOpts.Font.Color);
            {} Drillhole1 := Drillhole2;
            IsVisible1 := True;
          end;

          X1 := X2;  Y1 := Y2;   Z1 := Z2;
          R  := R2;  G  := G2;   B  := B2;
          PrevVisible := IsVisible1;
          IsVisible1 := IsVisible2;
          SampleNo := SampleNo2;

          TableMap.Next;
        end;
        if IsVisible1 then
        begin
          TableMap.Prior;
          MIFPointXYZ(X1, Y1, Z1, FloatToStr(ValuePrev), Drillhole1,
            SampleNo, Model.Canvas3D.Font.Color);
          TableMap.Next;
        end;
      finally
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  end;

const
  X = 0;
  Y = 1;
  Z = 2;

  {sub}
  procedure MIFPolygon(Vertices: array of TVertex; Attributes: string);
  var
    I: integer;
  begin
    Writeln(FileMIF, 'Region 1');
    Writeln(FileMIF, Format('  %d', [High(Vertices)]));
    for I := 0 to High(Vertices) - 1 do
      Writeln(FileMIF, Format('%f %f', [Vertices[I].V[X], Vertices[I].V[Y]]));
    Writeln(FileMID, Format('%f,%s', [Vertices[0].V[Z], Attributes]));
  end;

  {sub}
  procedure MIFPolyLine(Vertices: array of TVertex; Attribute: double);
  var
    I: integer;
  begin
    Writeln(FileMIF, Format('PLine %d', [High(Vertices)]));
    for I := 0 to High(Vertices) - 1 do
      Writeln(FileMIF, Format('%f %f', [Vertices[I].V[X], Vertices[I].V[Y]]));
    Writeln(FileMID, Format('%f,%g', [Vertices[0].V[Z], Attribute]));
  end;

  {sub}
  procedure ExportPolyToMIF;
  type
    TVertexArr = array of TVertex;
    PVertexArr = ^TVertexArr;

    procedure AddVertex(X, Y, Z: TGLDouble; V: PVertexArr; var Count: integer);
    begin
      if High(V^) < Count then
        SetLength(V^, Count + 20);
      V^[Count].V[0] := X;
      V^[Count].V[1] := Y;
      V^[Count].V[2] := Z;
      Count := Count + 1;
    end;

  var
    I, J:      longint;
    X1, X2, Y1, Y2, Z1, Z2: double;
    R, G, B:   double;
    Attributes: string;
    AttribName: string;
    Poly1, Poly2: integer;
    ValueFloat: TGLfloat;
    //  ValueInteger:Integer;
    OldCursor: TCursor;
    IsVisible: boolean;

    TableVertex: TTable;

    Vertices:    array of TVertex;
    VertexCount: integer;
    PolygonType: TPolygonType;

  begin
    OldCursor   := Screen.Cursor;
    TableVertex := TTable.Create(nil);
    try
      Screen.Cursor := crHourGlass;

      TableMap.TableName    := Model.ModelName;
      TableVertex.TableName :=
        ChangeModelTable(DirPolygonPoly, DirPolygonVertex, TableMap.TableName);
      TableMap.Open;
      TableVertex.Open;

      AttribName := Model.AttributeName;
      Mode := GBCanvas.ViewMode;

      X1 := GBCanvas.MinX;
      Y1 := GBCanvas.MinY;
      Z1 := GBCanvas.MinZ;
      X2 := GBCanvas.MaxX;
      Y2 := GBCanvas.MaxY;
      Z2 := GBCanvas.MaxZ;

      ConvertCoord(X1, Y1, Z1);
      ConvertCoord(X2, Y2, Z2);

      Writeln(FileMIF, 'Version 300');
      Writeln(FileMIF, 'Charset "WindowsCyrillic"');
      Writeln(FileMIF, 'Delimiter ","');
      Writeln(FileMIF,
        Format('CoordSys NonEarth Units "m" Bounds (%f, %f) (%f, %f)',
        [X1 - 1000, Y1 - 1000, X2 + 1000, Y2 + 1000]));
      Writeln(FileMIF, Format('Columns %d', [TableMap.FieldCount + 1]));
      case Mode of
        vmXY: Writeln(FileMIF, '  Z Float');
        vmXZ: Writeln(FileMIF, '  Y Float');
        vmYZ: Writeln(FileMIF, '  X Float');
      end;
      for I := 0 to TableMap.FieldCount - 1 do
        with TableMap, Fields[I] do
          Writeln(FileMIF, Format(FieldTypeParadoxToMIF(Fields[I]),
            [FieldName, Size]));
      {        if TableMap.FieldByName(AttribName) is TIntegerField then
            Writeln(FileMIF,Format('  %s Integer',[AttribName]))
            else Writeln(FileMIF,Format('  %s Float',[AttribName]));{}
      Writeln(FileMIF, 'Data');
      Writeln(FileMIF, ' ');

      TableMap.First;
      TableVertex.First;

      X1 := (TableVertex.FieldByName(fldX).AsFloat);
      Y1 := (TableVertex.FieldByName(fldY).AsFloat);
      Z1 := (TableVertex.FieldByName(fldZ).AsFloat);

      ConvertCoord(X1, Y1, Z1);

      Poly1 := TableVertex.FieldByName(fldID_POLY).AsInteger;

      ValueFloat := TableVertex.FieldByName(AttribName).AsFloat;
      Model.GetLevelStyle(ValueFloat, R, G, B, IsVisible);

      glColor3f(R, G, B);

      try
        PolygonType := TPolygonType(TableMap.FieldByName(fldID_TYPE).AsInteger);
      except
        PolygonType := ptPolygon;
      end;

      SetLength(Vertices, 5);
      VertexCount := 0;

      for I := 1 to TableVertex.RecordCount - 1 do
      begin
        X2 := TableVertex.FieldByName(fldX).AsFloat;
        Y2 := TableVertex.FieldByName(fldY).AsFloat;
        Z2 := TableVertex.FieldByName(fldZ).AsFloat;
        ConvertCoord(X2, Y2, Z2);
        Poly2 := TableVertex.FieldByName(fldID_POLY).AsInteger;
        if (Poly1 = Poly2) then
        begin
          if IsVisible then
            AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
        end
        else
        // if ModelList[ModelIndex].Fill then
        begin
          AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
          if VertexCount > 2 then
          begin
            SetLength(Vertices, VertexCount);
            TableMap.Locate(fldID, Poly1, []);
            Attributes := TableMap.Fields[0].AsString;
            for J := 1 to TableMap.FieldCount - 1 do
              Attributes := Attributes + ',' + TableMap.Fields[J].AsString;
            case PolygonType of
              ptPolygon:
                MIFPolygon(Vertices, Attributes {FloatToStr(ValueFloat){});
              ptPolyline:
                MIFPolyLine(Vertices, ValueFloat);
            else
                MIFPolyLine(Vertices, ValueFloat);
          end;
        end;

        VertexCount := 0;
        Vertices    := nil;
        SetLength(Vertices, 5);

        Poly1 := Poly2;

        try
          PolygonType :=
            TPolygonType(TableMap.FieldByName(fldID_TYPE).AsInteger);
        except
          PolygonType := ptPolygon;
        end;

        ValueFloat := TableVertex.FieldByName(AttribName).AsFloat;
        Model.GetLevelStyle(ValueFloat, R, G, B, IsVisible);
      end;
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;
      TableVertex.Next;
    end;
      //        if ModelList[ModelIndex].Fill then
      begin
        AddVertex(X1, Y1, Z1, @Vertices, VertexCount);
        if VertexCount > 2 then
        begin
          SetLength(Vertices, VertexCount);
          TableMap.Locate(fldID, Poly1, []);
          Attributes := TableMap.Fields[0].AsString;
          for J := 1 to TableMap.FieldCount - 1 do
            Attributes := Attributes + ',' + TableMap.Fields[J].AsString;
          case PolygonType of
            ptPolygon:
              MIFPolygon(Vertices, Attributes {FloatToStr(ValueFloat){});
            ptPolyline:
              MIFPolyLine(Vertices, ValueFloat);
            else
              MIFPolyLine(Vertices, ValueFloat);
          end;
        end;
      end;
    finally
      Vertices      := nil;
      Screen.Cursor := OldCursor;
      TableVertex.Free;
    end; (**)
  end;

  {sub}
  procedure ExportPointsToMIF;
  var
    I: longint;
    X1, X2, Y1, Y2, Z1, Z2: double;
    AttribName: string;
    OldCursor: TCursor;
    IsVisible: boolean;
    FieldCount: integer;

    procedure MIFPointXYZ(X, Y, Z: double; Color: TColor);
    var
      I: integer;
    begin
      ConvertCoord(X, Y, Z);
      Writeln(FileMIF, Format('Point %f %f', [X, Y]));
      Writeln(FileMIF, Format('    Symbol (40,%d,6)', [Color]));
      Write(FileMID, Z);
      for I := 0 to TableMap.FieldCount - 1 do
      begin
        with TableMap.Fields[I] do
          if (FieldName <> fldX) and (FieldName <> fldY) and
            (FieldName <> fldZ) then
            if (TableMap.Fields[I] is TNumericField) or
              (TableMap.Fields[I] is TStringField) or
              (TableMap.Fields[I] is TBooleanField) then
              Write(FileMID, ',' + AsString);
      end;
      Writeln(FileMID);
    end;

  begin
    OldCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourGlass;
      try
        TableMap.TableName := Model.ModelName;
        TableMap.Open;

        AttribName := Model.AttributeName;
        Mode := GBCanvas.ViewMode;

        Value := TableMap.FieldByName(AttribName).AsFloat;
        Model.GetLevelStyle(Value, R, G, B, IsVisible);

        X1 := GBCanvas.MinX;
        Y1 := GBCanvas.MinY;
        Z1 := GBCanvas.MinZ;
        X2 := GBCanvas.MaxX;
        Y2 := GBCanvas.MaxY;
        Z2 := GBCanvas.MaxZ;

        ConvertCoord(X1, Y1, Z1);
        ConvertCoord(X2, Y2, Z2);

        Writeln(FileMIF, 'Version 300');
        Writeln(FileMIF, 'Charset "WindowsCyrillic"');
        Writeln(FileMIF, 'Delimiter ","');
        Writeln(FileMIF,
          Format('CoordSys NonEarth Units "m" Bounds (%f, %f) (%f, %f)',
          [X1 - 1000, Y1 - 1000, X2 + 1000, Y2 + 1000]));

        FieldCount := 0;
        for I := 0 to TableMap.FieldCount - 1 do
        begin
          with TableMap.Fields[I] do
            if (FieldName <> fldX) and (FieldName <> fldY) and
              (FieldName <> fldZ) then
              if (TableMap.Fields[I] is TNumericField) then
                if (TableMap.Fields[I] is TIntegerField) then
                begin
                  //            Writeln(FileMIF,Format('  %s Decimal(11, 0)',[FieldName]));
                  Inc(FieldCount);
                end
                else
                begin
                  //            Writeln(FileMIF,Format('  %s Float',[FieldName]));
                  Inc(FieldCount);
                end
              else if (TableMap.Fields[I] is TStringField) then
              begin
                //            Writeln(FileMIF,Format('  %s Char(%d)',[FieldName, Size]));
                Inc(FieldCount);
              end
              else if (TableMap.Fields[I] is TBooleanField) then
              begin
                //            Writeln(FileMIF,Format('  %s Logical',[FieldName]));
                Inc(FieldCount);
              end;
        end;

        Writeln(FileMIF, 'Columns ' + IntToStr(FieldCount + 1));
        case Mode of
          vmXY:
            Writeln(FileMIF, '  Z Float');
          vmXZ:
            Writeln(FileMIF, '  Y Float');
            //        vmYZ: Writeln(FileMIF,'  X Float');
          else
            Writeln(FileMIF, '  X Float');
        end;

        for I := 0 to TableMap.FieldCount - 1 do
        begin
          with TableMap.Fields[I] do
            if (FieldName <> fldX) and (FieldName <> fldY) and
              (FieldName <> fldZ) then
              if (TableMap.Fields[I] is TNumericField) then
                if (TableMap.Fields[I] is TIntegerField) then
                begin
                  Writeln(FileMIF, Format('  %s Decimal(11, 0)', [FieldName]));
                  Inc(FieldCount);
                end
                else
                begin
                  Writeln(FileMIF, Format('  %s Float', [FieldName]));
                  Inc(FieldCount);
                end
              else if (TableMap.Fields[I] is TStringField) then
              begin
                Writeln(FileMIF, Format('  %s Char(%d)', [FieldName, Size]));
                Inc(FieldCount);
              end
              else if (TableMap.Fields[I] is TBooleanField) then
              begin
                Writeln(FileMIF, Format('  %s Logical', [FieldName]));
                Inc(FieldCount);
              end;
        end;

        Writeln(FileMIF, 'Data');
        Writeln(FileMIF, ' ');

        TableMap.First;
        for I := 1 to TableMap.RecordCount do
        begin
          X1 := TableMap.FieldByName(fldX).AsFloat;
          Y1 := TableMap.FieldByName(fldY).AsFloat;
          Z1 := TableMap.FieldByName(fldZ).AsFloat;

          Value := TableMap.FieldByName(AttribName).AsFloat;
          Model.GetLevelStyle(Value, R2, G2, B2, IsVisible2);

          if IsVisible then
          begin
            MIFPointXYZ(X1, Y1, Z1, RGBToColor(R, G, B));
          end;
          TableMap.Next;
        end;
      finally
      end;
    finally
      Screen.Cursor := OldCursor;
    end;
  end; //PointsToMIF

begin
  YShift      := 0; // Y values could be shifted
  XShift      := 0; // X values could be shifted
  FileNameMIF := ChangeFileExt(AFileName, '.MIF');
  FileNameMID := ChangeFileExt(AFileName, '.MID');
  AssignFile(FileMIF, FileNameMIF);
  AssignFile(FileMID, FileNameMID);
  Rewrite(FileMIF);
  Rewrite(FileMID);
  try
    if (ModelCount > 0) then
      case ModelList[ModelIndex].ModelType of
        mtDholes: ExportDholeToMIF;
        mtPolygons: ExportPolyToMIF;
        mtPoints2D, mtPoints3D: ExportPointsToMIF;
      end;
  finally
    CloseFile(FileMIF);
    CloseFile(FileMID);
  end;
end;

procedure TfmMapWindow.DoExportToMIF;
begin
  with TfmFileExport.Create(Self) do
  begin
    try
      SourceFile := Model.ModelName;
      if LastDir = '' then
        OutputName := ChangeFileExt(ExtractFileName(Model.ModelName), '.mif')
      else
        OutputFile := SlashSep(LastDir, ChangeFileExt(NameOnly(Model.ModelName),
          '.mif'));
      ComboBoxType.Items.Text := 'MapInfo (mif/mid)';
      ComboBoxType.ItemIndex := 0;
      dmDialogs.SaveDialogExport.DefaultExt := 'mif';
      if Showmodal = mrOk then
      begin
        ExportToMIF(OutputFile);
        LastDir := ExtractFilePath(OutputFile);
      end;
    finally
      Free;
    end;
  end;
end;


procedure TfmMapWindow.SetModel(const Value: TGBModel);
begin
  Model.Assign(Value);
end;

function TfmMapWindow.GetModel: TGBModel;
begin
  Result := ModelList[ModelIndex];
end;

function TfmMapWindow.GetModelCount: integer;
begin
  Result := ModelList.Count;
end;

procedure TfmMapWindow.SetModelList(const Value: TModelList);
begin
  FModelList := Value;
end;

function TfmMapWindow.GetModelList: TModelList;
begin
  if FModelList = nil then
    FModelList := TModelList.Create;
  Result := FModelList;
end;

procedure TfmMapWindow.SetModelIndex(const Value: integer);
begin
  if FModelIndex <> Min(ModelCount - 1, Max(0, Value)) then
  begin
    FModelIndex := Value;
  end;
end;

function TfmMapWindow.GetModelIndex: integer;
begin
  Result := Min(ModelCount - 1, Max(0, FModelIndex));
end;

procedure TfmMapWindow.ReloadModel;
begin
  if ModelCount > 0 then
  begin
    Model.Close;
    Model.Open;
  end;
end;

procedure TfmMapWindow.GBCanvasCallLists(Sender: TObject);
var
  I: integer;
  V: TVertex;
  OldPointSize: TGLint;
begin
  for I := 0 to ModelCount - 1 do
  begin
    if ModelList[I].Visible and (glIsList(ModelList[I].OGLListNo) = True) then
      glCallList(ModelList[I].OGLListNo);
  end;
  //  StatusBar.Panels[0].Text:=FloatToStr(GBCanvas.wVectorZ);
  glColor3d(1, 1, 1);
end;

procedure TfmMapWindow.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      Top    := ReadInteger(Name, 'Top', Top);
      Left   := ReadInteger(Name, 'Left', Left);
      Height := ReadInteger(Name, 'Height', Height);
      Width  := ReadInteger(Name, 'Width', Width);
      GBCanvas.Color := ReadInteger(GBCanvas.Caption, 'Color', clSilver);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMapWindow.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, 'Top', Top);
      WriteInteger(Name, 'Left', Left);
      WriteInteger(Name, 'Height', Height);
      WriteInteger(Name, 'Width', Width);
      WriteInteger(GBCanvas.Caption, 'Color', GBCanvas.Color);
    finally
      IniFile.Free;
    end;
end;

procedure TfmMapWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile; //before fmMapWindow := nil!
  try
    TableMap.Close;
    if fmGeoblock.MDIChildCount = 1 then
    begin
      fmGeoblock.EnableFileItems(False);
      fmGeoblock.EnableMapItems(Model.ModelType, False);
    end;
    fmViewProjectManager.Clear;
    fmMapWindow := nil;
    Action      := caFree;
    FModelList.Free;
    FModelList := nil;
    StatusBar.SimplePanel := True;
  except
  end;
  inherited;
end;


procedure TfmMapWindow.Grid2DSelectCell;
begin
  GBCanvas.Cursor := crSelectCursor;
end;

procedure TfmMapWindow.Grid2DSelectCol;
begin
  GBCanvas.Cursor := crSelectCursor;
end;

procedure TfmMapWindow.Grid2DSelectRow;
begin
  GBCanvas.Cursor := crSelectCursor;
end;

procedure TfmMapWindow.GBCanvasSelectPrimitive(Sender: TObject;
  X, Y: integer; var Primitive: TDrawPrimitive);
var
  ModelIndex, ID: integer;
  S: string;
begin
  ModelIndex := ModelList.IndexOfID(Primitive.IDs[0]);
  ID := Primitive.IDs[1];
  {fmMain.ShowTable(Model.ModelName, Model.ModelType);}

  { TODO -ovassiliev -c1 : The procedure does not work with polygons }
  with TfmRecordEditor.Create(Self) do
    try
      Str(ID, S);
      Caption := Caption + ' ' + S;
      Str(ModelIndex + 1, S);
      StaticTextModelIndex.Caption := S;
      StaticTextModel.Caption := ModelList[ModelIndex].ModelName;
      StaticTextModel.Hint := StaticTextModel.Caption;
      TableEdit.TableName  := ModelList[ModelIndex].ModelName;
      TableEdit.Open;

      TableEdit.Locate(TableEdit.Fields[0].FieldName, ID, [loCaseInsensitive]);
      UpdateStringGrid;
      ShowModal;
    finally
      TableEdit.Close;
      Free;
    end;

end;

end.

