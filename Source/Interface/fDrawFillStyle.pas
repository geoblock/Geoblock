//------------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//------------------------------------------------------------------------------
(* The pattern and texture editor *)

unit fDrawFillStyle;

interface

uses
  Winapi.Windows,
  System.SysUtils, 
  System.Classes, 
  System.IniFiles,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.ExtCtrls, 
  Vcl.ImgList, 
  Vcl.ComCtrls,
  Vcl.ToolWin, 
  Vcl.Buttons, 
  Vcl.DBCtrls, 
  Vcl.Mask, 
  //DB
  Data.DB,
  Bde.DBTables,

  
  fInitialDialog,
  dBase,
  dDialogs,
  fMapLegend, System.ImageList;

type
  PFillStyle = ^TFillStyle;

  TFillStyle = record
    FillColor, BkColor: TColor;
    BrushStyle: TBrushStyle;
    BMapNum:  integer;
    BMap:     TBitMap;
    PenWidth: integer;
    PenColor: TColor;
    PenStyle: TPenStyle;
  end;

// Store pattern data
type
  TBitMapPars = class
    Num:  integer;
    BMap: TBitmap;
    destructor Destroy; override;
  end;

type
  TfmDrawFillStyle = class(TfmInitialDialog)
    ToolBar:      TToolBar;
    ToolButton4:  TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonDelete: TToolButton;
    ToolButtonEdit: TToolButton;
    ToolButton1:  TToolButton;
    DataSourcePattern: TDataSource;
    DataSourceLine: TDataSource;
    TableLine:    TTable;
    TablePattern: TTable;
    TableTexture: TTable;
    PageControl:  TPageControl;
    TabSheetPattern: TTabSheet;
    ListBoxPattern: TListBox;
    TabSheetTexture: TTabSheet;
    ListBoxTexture: TListBox;
    TabSheetLine: TTabSheet;
    ListBoxLines: TListBox;
    StaticTextName: TStaticText;
    DataSourceTexture: TDataSource;
    GroupBoxOptions: TGroupBox;
    LabelColor:   TLabel;
    DBImage:      TDBImage;
    CheckBoxBackground: TCheckBox;
    PanelFillColor: TPanel;
    sbPolygonFillShowColor: TSpeedButton;
    ImageFillColor: TImage;
    PanelBkColor: TPanel;
    SpeedButtonBkColor: TSpeedButton;
    ImageBkColor: TImage;
    GroupBoxExample: TGroupBox;
    ImageExample: TImage;
    ButtonLineStyle: TButton;
    ImageList: TImageList;
    procedure ButtonLineStyleClick(Sender: TObject);
    procedure ImageFillColorClick(Sender: TObject);
    procedure ToolButtonNewClick(Sender: TObject);
    procedure ToolButtonDeleteClick(Sender: TObject);
    procedure ToolButtonEditClick(Sender: TObject);
    procedure ListBoxDrawItems(Control: TWinControl; Index: integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure CheckBoxBackgroundClick(Sender: TObject);
    procedure ImageBkColorClick(Sender: TObject);
    procedure PanelBkColorEnter(Sender: TObject);
    procedure PanelFillColorEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FLevel: TLevel;
    procedure DrawExample;
    procedure UpdateListPatterns;
    procedure UpdateListTextures;
    procedure SetLevel(const Value: TLevel);
    function GetLevel: TLevel;
    procedure ReadIniFile;
    procedure WriteIniFile;
  public
    FillStyle: TFillStyle;
    property Level: TLevel Read GetLevel Write SetLevel;
    constructor CreateServicePolygons(AOwner: TComponent;
      TheFillStyle: TFillStyle; IsLine: boolean);
  end;

var
  fmDrawFillStyle: TfmDrawFillStyle;

//========================================================================
implementation
//========================================================================

uses
  cGlobals,
  uCommon,
  cResStrings,
  fDrawLineStyle,
  fDrawImageEditor,
  GBGraphics,
  uFileCreator;

{$R *.DFM}

destructor TBitMapPars.Destroy;
begin
  Bmap.Free;
  inherited Destroy;
end;

{ TfmDrawFillStyles }

procedure TfmDrawFillStyle.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  TableTexture.DatabaseName := ExpandPath(DirDataReference);
  TablePattern.DatabaseName := ExpandPath(DirDataReference);
  TableLine.DatabaseName    := ExpandPath(DirDataReference);
  TableTexture.Open;
  TablePattern.Open;
  TableLine.Open;
  with ImageFillColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageFillColor.Width, ImageFillColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := FillStyle.FillColor;
    FillRect(Rect(1, 1, ImageFillColor.Width - 1, ImageFillColor.Height - 1));
  end;
  {with Bk color}
  FillStyle.BkColor := clWhite;
  with ImageBkColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageBkColor.Width, ImageBkColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := FillStyle.BkColor;
    FillRect(Rect(1, 1, ImageBkColor.Width - 1, ImageBkColor.Height - 1));
  end;
  UpdateListPatterns;
  UpdateListTextures;
  //  ListBoxPattern.ItemIndex := 0;
  //  ListBoxTexture.ItemIndex := 0;
end;


constructor TfmDrawFillStyle.CreateServicePolygons(AOwner: TComponent;
  TheFillStyle: TFillStyle; IsLine: boolean);
begin
  inherited Create(AOwner);
  FillStyle := TheFillStyle;
  with ImageFillColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageFillColor.Width, ImageFillColor.Height));
    Brush.Style := bsSolid;
    Brush.Color := FillStyle.FillColor;
    FillRect(Rect(1, 1, ImageFillColor.Width - 1, ImageFillColor.Height - 1));
  end;
  {with Bk color}
  with ImageBkColor.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    FillRect(Rect(0, 0, ImageBkColor.Width, ImageBkColor.Height));
    if FillStyle.BkColor = clNone then
    begin
      Brush.Color := clWhite;
    end
    else
    begin
      Brush.Color := FillStyle.BkColor;
    end;
    Brush.Style := bsSolid;
    FillRect(Rect(1, 1, ImageBkColor.Width - 1, ImageBkColor.Height - 1));
  end;
  if FillStyle.BMapNum <> 0 then
  begin
    try
      TablePattern.Open;
      if TablePattern.Locate(fldID, FillStyle.BMapNum, []) then
      begin
        ListBoxPattern.ItemIndex := TablePattern.RecNo + 1;
      end
      else
      begin
        ListBoxPattern.ItemIndex := 0;
      end;
      TablePattern.Close;
    except
      ListBoxPattern.ItemIndex := 0;
    end;
  end
  else
  begin
    if FillStyle.BrushStyle = bsClear then
    begin
      ListBoxPattern.ItemIndex := 1;
    end
    else
    begin
      ListBoxPattern.ItemIndex := 0;
    end;
  end;
  //  if not IsLine then ButtonLineStyle.Visible:=False;
  try
    CheckBoxBackGround.Checked := FillStyle.BkColor <> clNone;
  except
  end;
end;

procedure TfmDrawFillStyle.DrawExample;
var
  vPen: TPen;
  BMap: TBitmap;
  BrushStyle: TBrushStyle;
begin
  vPen := TPen.Create;
  try
    vPen.Color := FillStyle.PenColor;
    vPen.Width := FillStyle.PenWidth;
    vPen.Style := FillStyle.PenStyle;
    ImageExample.Canvas.Brush.Color := clBtnFace;
    ImageExample.Canvas.FillRect(Rect(0, 0, Width, Height));

    BrushStyle := bsClear;
    {   Bmap:=nil;
        if ListBoxPattern.ItemIndex=0 then
          BrushStyle:=bsSolid
        else{}
    case PageControl.ActivePageIndex of
      0:
      begin
        Bmap :=
          TBitMapPars(ListBoxPattern.Items.Objects
          [ListBoxPattern.ItemIndex]).Bmap;
        FillStyle.BMapNum :=
          TBitMapPars(ListBoxPattern.Items.Objects
          [ListBoxPattern.ItemIndex]).Num;
        FillStyle.BMap :=
          (TBitMapPars(ListBoxPattern.Items.Objects
          [ListBoxPattern.ItemIndex]).BMap);
        if CheckBoxBackGround.Checked then
        begin
          FillStyle.BkColor := ImageBkColor.Canvas.Brush.Color;
        end
        else
        begin
          FillStyle.BkColor := clNone;
        end;
        FillStyle.FillColor  := ImageFillColor.Canvas.Brush.Color;
        FillStyle.BrushStyle := BrushStyle;
        DrawPolygon(ImageExample.Canvas,
          [Point(0, 0), Point(Width, 0), Point(Width, Height),
          Point(0, Height), Point(0, 0)],
          FillStyle.FillColor, FillStyle.BkColor, FillStyle.BrushStyle,
          BMap, vPen);
      end;
      1:
      begin
        try
          Bmap :=
            TBitMapPars(ListBoxTexture.Items.Objects[
            ListBoxTexture.ItemIndex]).Bmap;
          //  DrawTile(Rect(0,0,Width,Height),ImageExample.Canvas,DBImage.Picture.Bitmap);
        except
        end;
      end;
    end;
  finally
    vPen.Free;
  end;
end;

procedure TfmDrawFillStyle.UpdateListPatterns;
var
  I:    longint;
  BMapPars: TBitMapPars;
  Bmap: TBitmap;
begin
  if ListBoxPattern.Items.Count <> 0 then
  begin
    for I := 0 to ListBoxPattern.Items.Count - 1 do
    begin
      try
        ListBoxPattern.Items.Objects[I].Free;
      except
      end;
    end;
    ListBoxPattern.Items.Clear;
  end;
  //Here must be solid and transparent patterns
{  BMappars:=TBitMapPars.Create;
  BMap := EmptyPattern;
  with BMap.Canvas do
  begin
    Brush.Color := clBlack;
    RectAngle(0,0,Width,Height);
  end;
  BMapPars.BMap:=BMap;
  BMapPars.Num:=0;
  ListBoxPattern.Items.AddObject('',BMapPars);
  BMappars:=TBitMapPars.Create;
  BMapPars.BMap:=nil;
  BMapPars.Num:=0;
  ListBoxPattern.Items.AddObject('',BMapPars);
{}
  try
    TablePattern.Open;
    TablePattern.First;
    DBImage.DataSource := DataSourcePattern;
    for I := 0 to TablePattern.RecordCount - 1 do
    begin
      try
        BMapPars := TBitMapPars.Create;
        if DBImage.Picture.Bitmap.Empty then
        begin
          Bmap := nil;
        end
        else
        begin
          BMap := TBitmap.Create;
          Bmap.Assign(DBImage.Picture);
        end;
        BMapPars.Bmap := Bmap;
        BMapPars.Num  := TablePattern.FieldByName(fldID).AsInteger;
        ListBoxPattern.Items.AddObject(
          TablePattern.FieldByName(fldNAME).AsString,
          BMapPars);
        TablePattern.Next;
      except
      end;
    end;
  except
    CreatePatternTable(TablePattern);     //Not here!
  end;
  ListBoxPattern.ItemIndex := ListBoxPattern.Items.Count - 1;
end;

procedure TfmDrawFillStyle.UpdateListTextures;
var
  I:    longint;
  BMapPars: TBitMapPars;
  Bmap: TBitmap;
begin
  if ListBoxTexture.Items.Count <> 0 then
  begin
    for I := 0 to ListBoxTexture.Items.Count - 1 do
    begin
      try
        ListBoxTexture.Items.Objects[I].Free;
      except
      end;
    end;
    ListBoxTexture.Items.Clear;
  end;
  //Read Textures from table
  try
    TableTexture.Open;
    TableTexture.First;
    DBImage.DataSource := DataSourceTexture;
    for I := 0 to TableTexture.RecordCount - 1 do
    begin
      try
        BMapPars := TBitMapPars.Create;
        if DBImage.Picture.Bitmap.Empty then
        begin
          Bmap := nil;
        end
        else
        begin
          BMap := TBitmap.Create;
          Bmap.Assign(DBImage.Picture);
        end;
        BMapPars.Bmap := Bmap;
        BMapPars.Num  := TableTexture.FieldByName(fldID).AsInteger;
        ListBoxTexture.Items.AddObject(
          TableTexture.FieldByName(fldNAME).AsString + ' (' +
          TableTexture.FieldByName('ID').AsString + ')',
          BMapPars);
        TableTexture.Next;
      except
      end;
    end;
  except
    TableTexture.FieldDefs.Clear;
    TableTexture.FieldDefs.Add(fldID, ftAutoInc, 0, False);
    TableTexture.FieldDefs.Add(fldIMAGE, ftGraphic, 0, False);
    TableTexture.FieldDefs.Add(fldNAME, ftString, 64, False);
    try
      TableTexture.CreateTable;
    except
    end;
  end;
  ListBoxTexture.ItemIndex := ListBoxTexture.Items.Count - 1;
end;

procedure TfmDrawFillStyle.ButtonLineStyleClick(Sender: TObject);
begin
  fmDrawLineStyle := TfmDrawLineStyle.CreateServiceLines(
    FillStyle.PenWidth, FillStyle.PenColor, FillStyle.PenStyle);
  if fmDrawLineStyle.ShowModal = mrOk then
  begin
    FillStyle.PenWidth := fmDrawLineStyle.LineOptions.Width;
    FillStyle.PenColor := fmDrawLineStyle.LineOptions.Color;
    FillStyle.PenStyle := fmDrawLineStyle.LineOptions.Style;
  end;
  fmDrawLineStyle.Free;
  DrawExample;
end;

procedure TfmDrawFillStyle.ToolButtonNewClick(Sender: TObject);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  DBImage.DataSource := DataSourcePattern;
  DBImage.DataField := fldIMAGE;
  case PageControl.ActivePageIndex of
    0:
    begin
      TablePattern.Open;
      try
        Picture.Bitmap.Width  := 8;
        Picture.Bitmap.Height := 8;
        TablePattern.Append;
        DBImage.Picture.Assign(Picture);
        TablePattern.Post;
      finally
        Picture.Free;
        UpdateListPatterns;
      end;
    end;
    1:
    begin
      TableTexture.Open;
      try
        Picture.Bitmap.Width  := ImageSize;
        Picture.Bitmap.Height := ImageSize;
        TableTexture.Append;
        DBImage.Picture.Assign(Picture);
        TableTexture.Post;
      finally
        Picture.Free;
        UpdateListTextures;
      end;
    end;
  end;
  DrawExample;
end;

procedure TfmDrawFillStyle.ToolButtonDeleteClick(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0:
    begin
      if ListBoxPattern.ItemIndex < $10 then
      begin
        Exit;
      end;
      TablePattern.Open;
      if MessageDlg(LoadResString(@rsDelete) + '?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then
      begin
        raise EAbort.Create('');
      end;
      TablePattern.Delete; //Deletes the active record
      TablePattern.Locate(fldID,
        TBitMapPars(ListBoxPattern.Items.Objects[ListBoxPattern.ItemIndex]).Num, []);
      UpdateListPatterns;
    end;
    1:
    begin
      if ListBoxTexture.ItemIndex < $10 then
      begin
        Exit;
      end;
      TableTexture.Open;
      if MessageDlg(LoadResString(@rsDelete) + '?', mtConfirmation,
        [mbYes, mbNo], 0) = mrNo then
      begin
        raise EAbort.Create('');
      end;
      TableTexture.Delete; //Deletes the active record
      TableTexture.Locate(fldID,
        TBitMapPars(ListBoxTexture.Items.Objects[ListBoxTexture.ItemIndex]).Num, []);
      UpdateListTextures;
    end;
  end;
  DrawExample;
end;

procedure TfmDrawFillStyle.ToolButtonEditClick(Sender: TObject);
var
  TempDataSource: TDataSource;
  TempTable:      TTable;
  TempListBox:    TListBox;
begin
  case PageControl.ActivePageIndex of
    0:
    begin
      if ListBoxPattern.ItemIndex < $10 then
      begin
        Exit;
      end;
      TempTable      := TablePattern;
      TempListBox    := ListBoxPattern;
      TempDataSource := DataSourcePattern;
    end;
    1:
    begin
      if ListBoxTexture.ItemIndex < $10 then
      begin
        Exit;
      end;
      TempTable      := TableTexture;
      TempListBox    := ListBoxTexture;
      TempDataSource := DataSourceTexture;
    end;
  end;
  fmDrawImageEditor := TfmDrawImageEditor.Create(Self);
  try
    fmDrawImageEditor.DBImage.DataSource := TempDataSource;
    fmDrawImageEditor.DBEditName.DataSource := TempDataSource;
    fmDrawImageEditor.DBEditName.DataField := fldNAME;
    TempTable.RecNo := TempListBox.ItemIndex + 1;
    TempTable.Edit;
    fmDrawImageEditor.DBImage.DataField := fldIMAGE;
    if fmDrawImageEditor.ShowModal = mrOk then
    begin
      TempTable.Post;
      UpdateListPatterns;
      UpdateListTextures;
      //      TempListBox.ItemIndex:=Table.RecNo;
      DrawExample;
    end
    else
    begin
      TempTable.Cancel;
    end;
  finally
    FreeAndNil(fmDrawImageEditor);
  end;
end;

procedure TfmDrawFillStyle.ListBoxDrawItems(Control: TWinControl;
  Index: integer; Rect: TRect; State: TOwnerDrawState);
var
  R: TRect;
begin
  R := Rect;
  with TListBox(Control) do
  begin
    Canvas.FillRect(Rect);
    InflateRect(R, -2, -2);
    Canvas.Pen.Color := clBlack;
    //    Canvas.Brush.Bitmap:=TBitMapPars(Items.Objects[Index]).BMap;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, -1, -1);
    DrawTile(R, Canvas, TBitMapPars(Items.Objects[Index]).BMap);
    if odSelected in State then
    begin
      DrawExample;
      StaticTextName.Caption   := Items.Strings[Index];
      ToolButtonDelete.Enabled := not (Index < $10);
      ToolButtonEdit.Enabled   := not (Index < $10);
    end;
  end;
end;

procedure TfmDrawFillStyle.ImageFillColorClick(Sender: TObject);
begin
  PanelFillColor.SetFocus;
  dmDialogs.ColorDialog.Color := ImageFillColor.Canvas.Brush.Color;
  if dmDialogs.ColorDialog.Execute then
  begin
    with ImageFillColor.Canvas do
    begin
      PanelFillColorEnter(Sender);
      Brush.Color := dmDialogs.ColorDialog.Color;
      FillRect(Rect(1, 1, ImageFillColor.Width - 1, ImageFillColor.Height - 1));
      PanelFillColorEnter(Sender);
    end;
    FillStyle.FillColor := ImageFillColor.Canvas.Brush.Color;
    DrawExample;
  end;
end;

procedure TfmDrawFillStyle.ImageBkColorClick(Sender: TObject);
begin
  PanelBkColor.SetFocus;
  dmDialogs.ColorDialog.Color := ImageBkColor.Canvas.Brush.Color;
  if dmDialogs.ColorDialog.Execute then
  begin
    with ImageBkColor.Canvas do
    begin
      PanelBkColorEnter(Sender);
      Brush.Color := dmDialogs.ColorDialog.Color;
      FillRect(Rect(1, 1, ImageBkColor.Width - 1, ImageBkColor.Height - 1));
      PanelBkColorEnter(Sender);
    end;
    DrawExample;
  end;
end;

procedure TfmDrawFillStyle.PanelBkColorEnter(Sender: TObject);
begin
  with ImageBkColor.Canvas do
  begin
    DrawFocusRect(Rect(0, 0, ImageBkColor.Width, ImageBkColor.Height));
  end;
end;

procedure TfmDrawFillStyle.PanelFillColorEnter(Sender: TObject);
begin
  with ImageFillColor.Canvas do
  begin
    DrawFocusRect(Rect(0, 0, ImageFillColor.Width, ImageFillColor.Height));
  end;
end;

procedure TfmDrawFillStyle.CheckBoxBackgroundClick(Sender: TObject);
begin
  ImageBkColor.Enabled := CheckBoxBackground.Checked;
  SpeedButtonBkColor.Enabled := CheckBoxBackground.Checked;
  DrawExample;
end;

procedure TfmDrawFillStyle.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I: integer;
begin
  if ListBoxTexture.Items.Count <> 0 then
  begin
    for I := 0 to ListBoxTexture.Items.Count - 1 do
    begin
      try
        ListBoxTexture.Items.Objects[I].Free;
      except
      end;
    end;
    ListBoxTexture.Items.Clear;
  end;

  if ListBoxPattern.Items.Count <> 0 then
  begin
    for I := 0 to ListBoxPattern.Items.Count - 1 do
    begin
      try
        ListBoxPattern.Items.Objects[I].Free;
      except
      end;
    end;
    ListBoxPattern.Items.Clear;
  end;
  WriteIniFile;
  inherited;
end;

procedure TfmDrawFillStyle.FormDestroy(Sender: TObject);
begin
  FLevel.Free;
  TableTexture.Close;
  TablePattern.Close;
  TableLine.Close;
  inherited;
end;

procedure TfmDrawFillStyle.SetLevel(const Value: TLevel);
begin
  Level.Assign(Value);
end;

function TfmDrawFillStyle.GetLevel: TLevel;
begin
  if FLevel = nil then
  begin
    FLevel := TLevel.Create(nil);
  end;
  Result := FLevel;
end;

procedure TfmDrawFillStyle.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
  begin
    try
      CheckBoxBackGround.Checked :=
        ReadBool(Name, CheckBoxBackGround.Caption, False);
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TfmDrawFillStyle.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
  begin
    try
      WriteBool(Name, CheckBoxBackGround.Caption,
        CheckBoxBackGround.Checked);
    finally
      IniFile.Free;
    end;
  end;
end;

end.
