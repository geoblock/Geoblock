//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{!  The dialog to select legends for colorfill operations }

unit fMapLegend;

interface

uses
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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.Samples.Spin,
  Vcl.Clipbrd,

  Data.DB,
  Bde.DBTables,
  
  fInitialDialog;

type
  TLegendMode = (lmSpectrum, lmFromFile);

type
  TLegend = class;

  TfmMapLegend = class(TfmInitialDialog)
    RadioGroupRamp: TRadioGroup;
    HeaderControl: THeaderControl;
    ImageList: TImageList;
    PopupMenu: TPopupMenu;
    MenuItemAdd: TMenuItem;
    MenuitemDelete: TMenuItem;
    N6:     TMenuItem;
    MenuItemSetColor: TMenuItem;
    MenuItemSetLine: TMenuItem;
    N8:     TMenuItem;
    MenuItemHide: TMenuItem;
    MenuItemShow: TMenuItem;
    MenuItemCheckAll: TMenuItem;
    StringGrid: TStringGrid;
    TableLevels: TTable;
    Panel1: TPanel;
    LabelNumberOfLevels: TLabel;
    EditNClasses: TEdit;
    SpinButton: TSpinButton;
    SpeedButtonSaveAs: TSpeedButton;
    Panel2: TPanel;
    ListBoxLegends: TListBox;
    ButtonReset: TButton;
    SpeedButtonSave: TSpeedButton;
    procedure SpinButtonDownClick(Sender: TObject);
    procedure SpinButtonUpClick(Sender: TObject);
    procedure RadioGroupRampClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButtonSaveAsClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
    procedure StringGridDrawCell(Sender: TObject; ACol, ARow: integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGridGetEditText(Sender: TObject; ACol, ARow: integer;
      var Value: string);
    procedure StringGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure StringGridKeyPress(Sender: TObject; var Key: char);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure StringGridSelectCell(Sender: TObject; ACol, ARow: integer;
      var CanSelect: boolean);
    procedure StringGridSetEditText(Sender: TObject; ACol, ARow: integer;
      ValueAsText: string);
    procedure MenuItemSetColorClick(Sender: TObject);
    procedure MenuItemSetLineClick(Sender: TObject);
    procedure MenuItemHideClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemCheckAllClick(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure HeaderControlSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure ListBoxLegendsClick(Sender: TObject);
    procedure SpeedButtonSaveClick(Sender: TObject);
  private
    FUpdateCount: integer;
    FOldValue:    double;
    FLevels:      TLegend;
    procedure RampColor;
    procedure SpectrumColor;
    procedure SetLevels(const Value: TLegend);
    function GetLevels: TLegend;
    function GetCurrentLevel: integer;
    procedure SetCurrentLevel(const Value: integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Change;
  public
    MinVal, MaxVal: double;
    PathLevel, FileName: TFileName;
    Regim:    TLegendMode;
    NClasses: integer;
    BrightFillColor, BrightLineColor: TColor;
    property Levels: TLegend Read GetLevels Write SetLevels;
    property CurrentLevel: integer Read GetCurrentLevel Write SetCurrentLevel;
    constructor Create(AOwner: TComponent); override;
    procedure ChangeChecked(Column: integer);
    procedure FillColorClasses;
    procedure ReadFileList;
    function Execute: boolean;
    procedure LevelsChange(Sender: TObject);
  end;

  TPointStyle = class(TPersistent)
  private
    FSize:     double;
    FColor:    TColor;
    FonChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: double);
  public
    property Color: TColor read FColor write SetColor;
    property Size: double read FSize write SetSize;
    property onChange: TNotifyEvent read FonChange write FonChange;

    procedure Assign(Source: TPersistent); override;
    procedure Change;
  end;

  TLineStyle = class(TPersistent)
  private
    FStippleFactor: byte;
    FSize:     integer;
    FColor:    TColor;
    FonChange: TNotifyEvent;
    FStipplePattern: word;
    procedure SetColor(const Value: TColor);
    procedure SetSize(const Value: integer);
    procedure SetStippleFactor(const Value: byte);
    procedure SetStipplePattern(const Value: word);
  public
    property Color: TColor read FColor write SetColor;
    property Size: integer read FSize write SetSize;
    property StippleFactor: byte read FStippleFactor write SetStippleFactor;
    property StipplePattern: word read FStipplePattern write SetStipplePattern;
    property onChange: TNotifyEvent read FonChange write FonChange;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
  end;

  TFillStyle = class(TPersistent)
  private
    FUpdateCounter: integer;
    FTransparent: boolean;
    FPattern:  TBitmap;
    FTexture:  TBitmap;
    FPatternColor: TColor;
    FColor:    TColor;
    FonChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetPattern(const Value: TBitmap);
    procedure SetPatternColor(const Value: TColor);
    procedure SetTexture(const Value: TBitmap);
    procedure SetTransparent(const Value: boolean);
    procedure ChangeFill(Sender: TObject);
  public
    property Color: TColor read FColor write SetColor;
    property PatternColor: TColor read FPatternColor write SetPatternColor;
    property Transparent: boolean read FTransparent write SetTransparent;
    property Pattern: TBitmap read FPattern write SetPattern;
    property Texture: TBitmap read FTexture write SetTexture;
    property onChange: TNotifyEvent read FonChange write FonChange;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
    destructor Destroy; override;
  end;

  TLevel = class(TCollectionItem)
  private
    FUpdateCounter: integer;
    FVisible:  boolean;
    FAutoCalc: boolean;
    FValue:    double;
    FLine:     TLineStyle;
    FonChange: TNotifyEvent;
    FPoint:    TPointStyle;
    FFill:     TFillStyle;
    procedure SetAutoCalc(const Value: boolean);
    procedure SetFill(const Value: TFillStyle);
    procedure SetLine(const Value: TLineStyle);
    procedure SetPoint(const Value: TPointStyle);
    procedure SetValue(const Value: double);
    procedure SetVisible(const Value: boolean);
  public
    property Visible: boolean read FVisible write SetVisible;
    property AutoCalc: boolean read FAutoCalc write SetAutoCalc;
    property Value: double read FValue write SetValue;
    property Point: TPointStyle read FPoint write SetPoint;
    property Line: TLineStyle read FLine write SetLine;
    property Fill: TFillStyle read FFill write SetFill;
    property onChange: TNotifyEvent read FonChange write FonChange;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Edit;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Change;
    procedure ChangeStyle(Sender: TObject);
  end;

  TLevelClass = class of TLevel;

  TLegend = class(TComponent)
  private
    FLevels:   TCollection;
    FUpdateCounter: integer;
    FMode:     TLegendMode;
    FonChange: TNotifyEvent;
    FMaxValue: double;
    FMinValue: double;
    FFileName: TFileName;
    FMaxCount: integer;
    FMinCount: integer;
    function GetItems(Index: integer): TLevel;
    procedure SetItems(Index: integer; const Value: TLevel);
    procedure SetMode(const Value: TLegendMode);
    procedure SetAsString(const Value: string);
    function GetAsString: string;
    function GetLevels(Index: integer): TLevel;
    procedure SetLevels(Index: integer; const Value: TLevel);
    function GetLevelCount: integer;
    procedure SetLevelCount(Value: integer);
    procedure SetMaxValue(const Value: double);
    procedure SetMinValue(const Value: double);
    procedure ChangeLevel(Sender: TObject);
    procedure SetFileName(const Value: TFileName);
  public
    property Items[Index: integer]: TLevel Read GetItems Write SetItems;
    property onChange: TNotifyEvent Read FonChange Write FonChange;
    property Mode: TLegendMode Read FMode Write SetMode;
    property AsString: string Read GetAsString Write SetAsString;
    property MinValue: double Read FMinValue Write SetMinValue;
    property MaxValue: double Read FMaxValue Write SetMaxValue;
    property FileName: TFileName Read FFileName Write SetFileName;
    property LevelCount: integer Read GetLevelCount Write SetLevelCount;
    property Levels[Index: integer]: TLevel Read GetLevels Write SetLevels; default;
    constructor Create(AOwner: TComponent); override;
    procedure GetLevelStyle(const Value: variant; out R, G, B: double;
      out IsVisible: boolean);

    procedure Assign(Source: TPersistent); override;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure LoadFromTable(Table: TTable);
    procedure SaveToTable(Table: TTable);
    procedure LoadFromFile(AFileName: TFileName);
    procedure SaveToFile(AFileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    function GetLevelByValue(AValue: double): TLevel;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetAllVisible;
    procedure Build(AMinValue, AMaxValue: double; ACount: integer;
      AMode: TLegendMode);
    procedure BuildDefault;
    procedure DeleteLevel(Index: integer);
    function InsertLevel(Index: integer): TLevel;
    procedure Change;
  end;


var
  fmMapLegend: TfmMapLegend;

implementation

uses
  uGlobals,
  uResStrings,
  uCommon,
  fDrawLineStyle,
  fDrawFillStyle,
  dDialogs,
  uFileCreator,
  uProfuns;

const
  CheckedColumn: integer = 0;
  NoColumn: integer      = 1;
  CheckedFillColumn: integer = 2;
  FillColumn: integer    = 3;
  CheckedLineColumn: integer = 4;
  LineColumn: integer    = 5;
  ValueColumn: integer   = 6;

{$R *.dfm}

{ TfmMapLegend }

constructor TfmMapLegend.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BrightFillColor := clRed;
  BrightLineColor := clGreen;

  PathLevel := ExpandPath(DirLegends);
  ReadFileList;
  NClasses := 14;    //The default number of classes
  StringGrid.RowCount := NClasses + 4;
  EditNClasses.Text := IntToStr(14 + 3);

  if Regim = lmFromFile then
  begin
    ListBoxLegends.ItemIndex := ListBoxLegends.Items.IndexOf(tblLevels);
    if ListBoxLegends.ItemIndex < 0 then
      ListBoxLegends.ItemIndex := 0;
    if ListBoxLegends.Items.Count = 0 then
      Regim := lmSpectrum;
  end;

  if Regim <> lmFromFile then
  begin
    //Selection
    TableLevels.TableName := PathLevel + tblLevels;
    Levels.SetAllVisible;
    ListBoxLegends.ItemIndex := ListBoxLegends.Items.IndexOf(tblLevels);
    if ListBoxLegends.ItemIndex < 0 then
    begin
      if CreateLegendTable(TableLevels) then
        ListBoxLegends.Items.Add(tblLevels);
      ListBoxLegends.ItemIndex := ListBoxLegends.Items.IndexOf(tblLevels);
    end;
  end;
  //Set width of columns, fill with numbers, colors and values
  FillColorClasses;
  if (Regim <> lmFromFile) and (ListBoxLegends.ItemIndex >= 0) then
    Levels.SaveToTable(TableLevels);
  RadioGroupRamp.ItemIndex := integer(Regim);
end;

procedure TfmMapLegend.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfmMapLegend.Change;
begin
  if FUpdateCount > 0 then
    Exit;

  EditNClasses.Text   := IntToStr(Levels.LevelCount);
  RadioGroupRamp.ItemIndex := integer(Levels.Mode);
  StringGrid.RowCount := Levels.LevelCount;

  ListBoxLegends.ItemIndex :=
    ListBoxLegends.Items.IndexOf(NameOnly(Levels.FileName));
  if ListBoxLegends.ItemIndex < 0 then
    ListBoxLegends.ItemIndex := ListBoxLegends.Items.IndexOf(tblLevels);
  if ListBoxLegends.ItemIndex < 0 then
    ListBoxLegends.ItemIndex := 0;
  StringGrid.Invalidate;
  StringGrid.Repaint;
end;

procedure TfmMapLegend.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Change;
end;

function TfmMapLegend.GetCurrentLevel: integer;
begin
  Result := StringGrid.Row;
end;

function TfmMapLegend.GetLevels: TLegend;
begin
  if FLevels = nil then
  begin
    FLevels := TLegend.Create(Self);
    FLevels.onChange := LevelsChange;
    FLevels.FMinValue := 0;
    FLevels.FMaxValue := 100;
    FLevels.BuildDefault;
  end;
  Result := FLevels;
end;

procedure TfmMapLegend.RampColor;
var
  I: integer;

  {sub}function GetClassColor(AClass: integer; BrightColor: TColor): TColor;
  var
    RMax, R, GMax, G, BMax, B: integer;
  const
    ShiftC = 120 / 255;
  begin
    RMax   := (BrightColor shr 0) and 255;
    GMax   := (BrightColor shr 8) and 255;
    BMax   := (BrightColor shr 16) and 255;
    R      := Round(RMax * (((AClass) / (NClasses) * (1 - ShiftC)) + ShiftC));
    G      := Round(GMax * (((AClass) / (NClasses) * (1 - ShiftC)) + ShiftC));
    B      := Round(BMax * (((AClass) / (NClasses) * (1 - ShiftC)) + ShiftC));
    Result := $02000000 + (B shl 16) + (G shl 8) + R;
  end;

begin
  Levels.BeginUpdate;
  try
    for I := 0 to Levels.LevelCount - 1 do
    begin
      Levels[I].Fill.Color := GetClassColor(I, BrightFillColor);
      Levels[I].Line.Color := GetClassColor(I, BrightLineColor);
    end;
  finally
    Levels.EndUpdate;
  end;
end;

procedure TfmMapLegend.SpectrumColor;
var
  I, J, FromI, ToI, Status: integer;

  {sub}
  function GetClassColor(AClass: integer; FirstColor, LastColor: TColor;
    NClasses: integer): TColor;
  var
    RMax, RMin, R, GMax, GMin, G, BMax, BMin, B: integer;
  begin
    RMax := (FirstColor shr 0) and 255;
    GMax := (FirstColor shr 8) and 255;
    BMax := (FirstColor shr 16) and 255;

    RMin := (LastColor shr 0) and 255;
    GMin := (LastColor shr 8) and 255;
    BMin := (LastColor shr 16) and 255;

    if NClasses = 0 then
      NClasses := 1;
    R := Round(RMax - (((RMax - RMin) / NClasses) * AClass));
    G := Round(GMax - (((GMax - GMin) / NClasses) * AClass));
    B := Round(BMax - (((BMax - BMin) / NClasses) * AClass));

    Result := $02000000 + (B shl 16) + (G shl 8) + R;
  end;

begin
  FromI := 0;
  ToI   := 0;
  with StringGrid do
  begin
    for I := 0 to NClasses - 1 do
    begin
      status := StrToInt(Cells[CheckedFillColumn, I + 1]);
      if (status = 0) then
        Cells[FillColumn, I + 1] :=
          IntToStr(GetClassColor((I - Fromi), StrToInt(
          Cells[FillColumn, FromI + 1]), StrToInt(Cells[FillColumn, ToI + 1]),
          (Toi - Fromi)))
      else
      begin
        FromI := I;
        toI   := I;
        for J := I + 1 to NClasses do
        begin
          status := StrToInt(Cells[CheckedFillColumn, J + 1]);
          if (status = 1) then
          begin
            toi := j;
            break;
          end;
        end;
      end;
    end;
    for I := 0 to NClasses - 1 do
    begin
      status := StrToInt(Cells[CheckedLineColumn, I + 2]);
      if (status = 0) then
        Cells[LineColumn, I + 1] :=
          IntToStr(GetClassColor((I - FromI), StrToInt(
          Cells[LineColumn, FromI + 1]), StrToInt(Cells[LineColumn, ToI + 1]),
          (ToI - FromI)))
      else
      begin
        fromi := i;
        toi   := i;
        for j := i to NClasses do
        begin
          status := StrToInt(Cells[CheckedLineColumn, J + 1]);
          if (status = 1) then
          begin
            toi := j;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfmMapLegend.SetCurrentLevel(const Value: integer);
begin
  StringGrid.Row := Value;
end;

procedure TfmMapLegend.SetLevels(const Value: TLegend);
begin
  BeginUpdate;
  try
    Levels.Assign(Value);
  finally
    EndUpdate;
  end;
end;

procedure TfmMapLegend.ChangeChecked(Column: integer);
var
  Checked: boolean;
begin
  if (Regim = lmSpectrum) and (Column <> CheckedColumn) and
    (StringGrid.Row > 0) and (StringGrid.Row < StringGrid.RowCount - 1) then
    Exit;
  Checked := True;
  case Column of
    0: Checked := not Levels[CurrentLevel].Visible;
    2,
    4: Checked := not Levels[CurrentLevel].AutoCalc;
  end;
  if (Column = CheckedFillColumn) or (Column = CheckedLineColumn) then
  begin
    if Regim <> lmFromFile then
    begin
      if (StringGrid.Row <= 1) or (StringGrid.Row = StringGrid.RowCount - 1) then
        Checked := False;
      if ((StringGrid.Row = 2) or (StringGrid.Row = StringGrid.RowCount - 1)) then
        Checked := False;
      Levels[CurrentLevel].AutoCalc := Checked;
      FillColorClasses;
    end;
  end;
  case Column of
    0: Levels[CurrentLevel].Visible  := Checked;
    2,
    4: Levels[CurrentLevel].AutoCalc := Checked;
  end;
  StringGrid.Invalidate;
end;

function TfmMapLegend.Execute: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfmMapLegend.FillColorClasses;
begin
  case Regim of
    lmSpectrum: RampColor;
    lmFromFile:  { Levels.LoadFromFile(ChangeFileExt(ExpandPath(DirLevel)+
      ComboBoxLegends.Text, TableExt))}    //???
  end;
end;

procedure TfmMapLegend.LevelsChange(Sender: TObject);
begin
  Change;
end;

procedure TfmMapLegend.ReadFileList;
var
  SearchRec: TSearchRec;
  Res: integer;
begin
  ListBoxLegends.Items.BeginUpdate;
  ListBoxLegends.Items.Clear;

  Res := FindFirst(PathLevel + TableMask, not faAnyFile, SearchRec);
  while Res = 0 do
  begin
    ListBoxLegends.Items.Add(NameOnly(SearchRec.Name));
    Res := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  if ListBoxLegends.Items.Count = 0 then
    ListBoxLegends.Items.Add(tblLevels);
  ListBoxLegends.Items.EndUpDate;
end;

{ TLegend }

procedure TLegend.Assign(Source: TPersistent);
begin
  if Source is TLegend then
  begin
    BeginUpdate;
    try
      FMode     := TLegend(Source).Mode;
      FFileName := TLegend(Source).FileName;
      FMinValue := TLegend(Source).MinValue;
      FMaxValue := TLegend(Source).MaxValue;
      FLevels.Assign(TLegend(Source).FLevels);
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TLegend.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TLegend.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    Change;
end;

procedure TLegend.Build(AMinValue, AMaxValue: double; ACount: integer;
  AMode: TLegendMode);
begin

end;

procedure TLegend.BuildDefault;
const
  LCount = 14;
  Colors: array[1..LCount] of TColor =
    (clWhite, clGray, clMaroon, clRed, clYellow, clLime,
    clGreen, clOlive, clAqua, clBlue, clNavy, clFuchsia,
    clPurple, clGray);
var
  H:     double;
  I:     integer;
  Level: TLevel;
begin
  BeginUpdate;
  try
    FLevels.Clear;
    if MaxValue = MinValue then
    begin
      MaxValue := 1;
      MinValue := 0;
    end
    else if MaxValue < MinValue then
    begin
      H := MinValue;
      MinValue := MaxValue;
      MaxValue := H;
    end;

    H := (MaxValue - MinValue) / (LCount - 3); //Empty, Above, Below
    for I := 1 to LCount do
    begin
      Level := TLevel(FLevels.Add);
      Level.onChange := ChangeLevel;
      Level.Visible := True;
      Level.AutoCalc := (I > 2) and (I < LCount);
      Level.Line.Color := Colors[I];
      Level.Fill.Color := Colors[I];
      Level.Value := MaxValue - (I - 2) * H;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TLegend.Change;
begin
  if FUpdateCounter <> 0 then
    Exit;
  if Assigned(FonChange) then
    FonChange(Self);
end;

procedure TLegend.ChangeLevel(Sender: TObject);
begin
  if not (Sender is TLevel) then
    Exit;
  Change;
end;

procedure TLegend.CopyToClipboard;
begin

end;

constructor TLegend.Create(AOwner: TComponent);
begin
  inherited;
  FLevels   := TCollection.Create(TLevel);
  FMode     := lmFromFile;
  FMaxCount := 255 + 4;   //Needs a control to define the value
  FMinCount := 4;
end;

function TLegend.InsertLevel(Index: integer): TLevel;
begin
  if LevelCount < FMaxCount then
  begin
    if Index < 1 then
      Index := 1;
    Result := TLevel(FLevels.Insert(Index));
    Result.BeginUpdate;
    try
      Result.onChange := ChangeLevel;
      Result.Visible := True;
      Index := Result.Index;
      Result.AutoCalc := Index > 1;
      if (Index > 0) and (Index < LevelCount - 1) then
      begin
        Result.Fill.Color := AverageColor([Levels[Index - 1].Fill.Color,
          Levels[Index + 1].Fill.Color]);
        Result.Line.Color := AverageColor([Levels[Index - 1].Line.Color,
          Levels[Index + 1].Line.Color]);
        Result.Value      := (Levels[Index - 1].Value + Levels[Index + 1].Value) / 2;
      end;
    finally
      Result.EndUpdate;
    end;
  end;
end;

procedure TLegend.DeleteLevel(Index: integer);
begin
  if LevelCount > FMinCount then
  begin
    if Index < 1 then
      Index := 1;
    if Index > LevelCount - 2 then
      Index := LevelCount - 2;
    FLevels.Delete(Index);
    Change;
  end;
end;


function TLegend.GetAsString: string;
begin

end;

function TLegend.GetItems(Index: integer): TLevel;
begin
  Result := TLevel(FLevels.Items[Index]);
end;

function TLegend.GetLevelByValue(AValue: double): TLevel;
var
  I: integer;
begin
  Result := nil;
  if LevelCount > 0 then
    Result := Levels[0];
  if LevelCount > 1 then
    Result := Levels[1];
  for I := LevelCount - 2 downto 2 do
    if Levels[I].Value > AValue then
    begin
      Result := Levels[I + 1];
      Break;
    end;
end;

function TLegend.GetLevelCount: integer;
begin
  if FLevels = nil then
    Result := 0
  else
    Result := FLevels.Count;
end;

function TLegend.GetLevels(Index: integer): TLevel;
begin
  Result := TLevel(FLevels.Items[Index]);
end;

procedure TLegend.GetLevelStyle(const Value: variant; out R, G, B: double;
  out IsVisible: boolean);
var
  Color: TColor;
  I:     integer;
begin
  if LevelCount = 0 then
  begin
    IsVisible := True;
    Color     := clWhite;
  end
  else if VarIsNull(Value) then
  begin
    Color     := Levels[1].Fill.Color;
    IsVisible := Levels[1].Visible;
  end
  else
  begin
    Color     := Levels[2].Fill.Color;
    IsVisible := Levels[2].Visible;
    for I := LevelCount - 2 downto 3 do
    begin
      if Value < Levels[I - 2].Value then
      begin
        Color     := Levels[I].Fill.Color;
        IsVisible := Levels[I].Visible;
        Break;
      end;
    end;
  end;
  R := (Color and 255) / 255;
  G := ((Color shr 8) and 255) / 255;
  B := ((Color shr 16) and 255) / 255;
end;


procedure TLegend.LoadFromFile(AFileName: TFileName);
var
  Stream: TFileStream;
  Table:  TTable;
begin
  if ExtractFileExt(AFileName) <> TableExt then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    Table := TTable.Create(Self);
    try
      Table.TableName := AFileName;
      Table.Open;
      LoadFromTable(Table);
    finally
      Table.Free;
    end;
  end;
end;

procedure TLegend.LoadFromStream(Stream: TStream);
begin
  if Stream.Size - Length('<\DEFAULT>') < Stream.Position then
    raise Exception.Create(LoadResString(@rsFileNotFound))
  else
    Assert(True, LoadResString(@rsNotImplemented) + ': TLegend.LoadFromStream');
end;

procedure TLegend.LoadFromTable(Table: TTable);
var
  I:     integer;
  Index: integer;
  Level: TLevel;
begin
  Table.Open;
  if (Table.RecordCount = 0) then
  begin
    MessageDlg(LoadResString(@rsEmptyTable), mtError, [mbOK], 0);
    Exit;
  end;
  BeginUpdate;
  try
    FLevels.Clear;
    Index := Table.FieldByName(fldVISIBLE).Index;
    //Restricts number of classes!
    for I := 0 to Table.RecordCount -1  do
    begin
      Level := TLevel(FLevels.Add);
      Level.OnChange := ChangeLevel;

      Level.Visible    := Table.Fields[Index].AsVariant;
      Level.AutoCalc   := not Table.FieldByName('FILLFIXED').AsVariant;
      Level.Fill.Color := TColor(Table.FieldByName('FILLCOLOR').AsInteger);
      //  if Table.FieldByName('LINEFIXED').IsNull then C := 0
      Level.Line.Color := TColor(Table.FieldByName('LINECOLOR').AsInteger);
      Level.Value      := Table.FieldByName('VALUE').AsFloat;
      Table.Next;
    end;
    if LevelCount > 0 then
    begin
      Levels[0].AutoCalc := False;
      Levels[LevelCount - 2].AutoCalc := False;
    end;
    if LevelCount > 2 then
      Levels[1].AutoCalc := False;
    FileName := Table.TableName;
  finally
    EndUpdate;
  end;
  Table.Close;
end;

procedure TLegend.PasteFromClipboard;
begin
  AsString := ClipBoard.AsText;
end;

procedure TLegend.SaveToFile(AFileName: TFileName);
var
  Stream: TFileStream;
  Table:  TTable;
begin
  if UpperCase(ExtractFileExt(AFileName)) <> UpperCase(TableExt) then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyWrite);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
  begin
    Table := TTable.Create(Self);
    try
      Table.TableName := AFileName;
      Table.Open;
      SaveToTable(Table);
    finally
      Table.Free;
    end;
  end;
end;

procedure TLegend.SaveToStream(Stream: TStream);
var
  S: string;
begin
  S := AsString;
  if Length(S) > 0 then
    Stream.Write(S[1], Length(S));
end;

procedure TLegend.SaveToTable(Table: TTable);
var
  I: integer;
begin
  Table.Close;
  CreateLegendTable(Table);
  Table.Open;
  Table.Edit;
  begin
    for I := 0 to LevelCount - 1 do
      with Levels[I] do
        Table.AppendRecord([Visible, not AutoCalc, Fill.Color,
          0, 0, not AutoCalc, Line.Color, 0, 0, Value]);
  end;
  Table.Close;
end;

procedure TLegend.SetAllVisible;
var
  I: integer;
begin
  FLevels.BeginUpdate;
  try
    for I := 0 to FLevels.Count - 1 do
      Levels[I].Visible := True;
  finally
    FLevels.EndUpdate;
  end;
  Change;
end;

procedure TLegend.SetAsString(const Value: string);
begin

end;

procedure TLegend.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    if Mode = lmFromFile then
      LoadFromFile(FFileName);
  end;
end;

procedure TLegend.SetItems(Index: integer; const Value: TLevel);
begin
  FLevels.Items[Index].Assign(Value);
end;

procedure TLegend.SetLevelCount(Value: integer);
var
  I:     integer;
  Level: TLevel;
begin
  Value := Max(Value, 4);
  if LevelCount <> Value then
  begin
    BeginUpdate;
    try
      while LevelCount < Value do
      begin
        InsertLevel(LevelCount div 2);
      end;
      while LevelCount > Value do
      begin
        DeleteLevel(LevelCount div 2);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TLegend.SetLevels(Index: integer; const Value: TLevel);
var
  Level: TLevel;
begin
  if Index > FLevels.Count - 1 then
  begin
    Level := TLevel(FLevels.Add);
    Level.onChange := ChangeLevel;
  end
  else
    Level := Levels[Index];
  Level.Assign(Value);
end;

procedure TLegend.SetMaxValue(const Value: double);
begin
  FMaxValue := Value;
end;

procedure TLegend.SetMinValue(const Value: double);
begin
  FMinValue := Value;
end;

procedure TLegend.SetMode(const Value: TLegendMode);
begin
  if FMode <> Value then
  begin
    BeginUpdate;
    try
      FMode := Value;
      if (Mode = lmFromFile) and FileExists(FileName) then
        LoadFromFile(FileName);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfmMapLegend.SpinButtonDownClick(Sender: TObject);
begin
  Levels.DeleteLevel(CurrentLevel);
end;

procedure TfmMapLegend.SpinButtonUpClick(Sender: TObject);
begin
  Levels.InsertLevel(CurrentLevel);
end;

procedure TfmMapLegend.RadioGroupRampClick(Sender: TObject);
begin
  case RadioGroupRamp.ItemIndex of
    0:
    begin
      ListBoxLegends.Enabled := False;
      Regim := lmSpectrum;

      if Levels.LevelCount > 0 then
      begin
        Levels[1].Fill.Color := clRed;
        Levels[1].Line.Color := clRed;
        Levels[Levels.LevelCount - 1].Fill.Color := clYellow;
        Levels[Levels.LevelCount - 1].Line.Color := clYellow;
      end;
      StringGrid.Cells[FillColumn, 3] := IntToStr(clRed);
      StringGrid.Cells[FillColumn, StringGrid.RowCount - 1] := IntToStr(clYellow);
    end;
    1:
    begin
      ListBoxLegends.Enabled := True;
      Regim := lmFromFile;
    end;
  end;
  Levels.Mode := Regim;
  FillColorClasses;
end;

procedure TfmMapLegend.FormCreate(Sender: TObject);
begin
  inherited;
  ListBoxLegends.ItemIndex := 0;
end;

procedure TfmMapLegend.ListBoxLegendsClick(Sender: TObject);
begin
  Levels.FileName := ChangeFileExt(PathLevel +
    ListBoxLegends.Items[ListBoxLegends.ItemIndex], TableExt);
end;

procedure TfmMapLegend.SpeedButtonSaveClick(Sender: TObject);
var
  FileName: TFileName;
begin
  FileName := ChangeFileExt(PathLevel +
    ListBoxLegends.Items[ListBoxLegends.ItemIndex], TableExt);
  if not FileExists(FileName) then
  begin
    TableLevels.TableName := FileName;
    CreateLegendTable(TableLevels);
  end;
  Levels.SaveToFile(FileName);
end;


procedure TfmMapLegend.SpeedButtonSaveAsClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    SaveDialog.FileName   := ListBoxLegends.Items[ListBoxLegends.ItemIndex];
    SaveDialog.InitialDir := PathLevel;
    if SaveDialog.Execute then
    begin
      TableLevels.TableName := SaveDialog.FileName;
      if not CreateLegendTable(TableLevels) then
      begin
        MessageDlg(CRLF + TableLevels.TableName + CRLF + ' ?', mtError, [mbOK], 0);
        Exit;
      end;
      Levels.SaveToFile(ChangeFileExt(SaveDialog.FileName, TableExt));
      ReadFileList;
      if ListBoxLegends.Items.IndexOf(NameOnly(SaveDialog.FileName)) <> -1 then
        ListBoxLegends.ItemIndex :=
          ListBoxLegends.Items.IndexOf(NameOnly(SaveDialog.FileName))
      else
        ListBoxLegends.ItemIndex := 0;
      FillColorClasses;
    end;
  end;
end;

procedure TfmMapLegend.ButtonOKClick(Sender: TObject);
begin
  inherited;
  Levels.SaveToFile(ChangeFileExt(PathLevel +
    ListBoxLegends.Items[ListBoxLegends.ItemIndex], TableExt));
end;

procedure TfmMapLegend.ButtonResetClick(Sender: TObject);
begin
  Levels.FileName := SetExtention(ExpandPath(DirLegends) + tblLevels, TableExt);
  Levels.BuildDefault;
end;

procedure TfmMapLegend.StringGridDrawCell(Sender: TObject; ACol, ARow: integer;
  Rect: TRect; State: TGridDrawState);

var
  OldColor: TColor;

  {sub}
  procedure DrawCellText;
  var
    S: string;
    PosX, PosY: integer;
    OldColor: TColor;

  begin
    case ACol of
      0: S := IntToStr(integer(Levels[ARow].Visible));
      1: S := IntToStr(ARow + 1);
      2: S := IntToStr(integer(not Levels[ARow].AutoCalc));
      3: S := LoadResString(@rsColor);
      4: S := IntToStr(integer(not Levels[ARow].AutoCalc));
      5: S := LoadResString(@rsLine);
      6: S := FloatToStr(RoundTo(Levels[ARow].Value, Precision));
    end;
    if ACol = ValueColumn then
      if ARow = 0 then
        S := LoadResString(@rsEmpty)
      else
      if ARow = 1 then
        S := '>= ' + FloatToStr(RoundTo(Levels[ARow + 1].Value, Precision))
      else
      if ARow = Levels.LevelCount - 1 then
        S := '< ' + FloatToStr(RoundTo(Levels[ARow - 1].Value, Precision));

    StringGrid.Canvas.Brush.Color := StringGrid.Color;
    StringGrid.Canvas.FillRect(Rect);
    OldColor := StringGrid.Canvas.Font.Color;

    PosX := (Rect.Right - Rect.Left) div 2 - StringGrid.Canvas.TextWidth(S) div 2;
    PosY := (Rect.Bottom - Rect.Top) div 2 - StringGrid.Canvas.TextHeight(S) div 2;
    StringGrid.Canvas.TextRect(Rect, Rect.Left + PosX, Rect.Top + PosY, S);

    StringGrid.Canvas.Font.Color := OldColor;
  end;

  {sub}
  procedure DrawCellColor;
  var
    OldBMap, NewBMap: TBitmap;
    //P:TPen;
  begin
    { TODO -oVas -cMap : Correct fill and line styles. }
    OldColor := StringGrid.Canvas.Brush.Color;
    //  OldBMap := StringGrid.Canvas.Brush.Bitmap;
    StringGrid.Canvas.Brush.Color := clBtnFace;
    StringGrid.Canvas.FillRect(Rect);
    InflateRect(Rect, -2, -2);
    //  StringGrid.Canvas.Brush.Style :=
    //    FillObject[ARow].FillStyle.BrushStyle;

    StringGrid.Canvas.Brush.Color := Levels[ARow].Fill.Color;       //!
    //    StringGrid.Canvas.Brush.Color := Levels[ARow].Fill.Color;

    {  if Assigned(FillObject[ARow].FillStyle.BMap) and False then
      begin
        NewBMap := EmptyPattern;
        NewBMap.Assign(FillObject[ARow].FillStyle.BMap);
        if FillObject[Row].FillStyle.BkColor<>clNone then
          TransformBitMap(NewBMap,FillObject[ARow].FillStyle.FillColor,
                          FillObject[ARow].FillStyle.BkColor)
        else
          TransformBitMap(NewBMap,FillObject[ARow].FillStyle.FillColor,clBtnFace);
        StringGrid.Canvas.Brush.Bitmap := NewBMap;
      end;{}
    StringGrid.Canvas.FillRect(Rect);
    {  if Assigned(StringGrid.Canvas.Brush.Bitmap) then
        StringGrid.Canvas.Brush.Bitmap.Free;
      StringGrid.Canvas.Brush.BitMap := OldBMap;{}
    StringGrid.Canvas.Brush.Color := OldColor;
  end;

  {sub}
  procedure DrawCellLine;
  var
    R: TRect;
  begin
    OldColor := StringGrid.Canvas.Brush.Color;
    StringGrid.Canvas.Brush.Color := clBtnFace;
    StringGrid.Canvas.FillRect(Rect);
    StringGrid.Canvas.Pen.Color := Levels[ARow].Line.Color;
    StringGrid.Canvas.Pen.Width := Levels[ARow].Line.Size;

    StringGrid.Canvas.Pen.Style := psSolid;
    R.Left   := Rect.Left + 2;
    R.Right  := Rect.Right - 2;
    R.Top    := Rect.Top + 2;
    R.Bottom := Rect.Bottom - 2;
    StringGrid.Canvas.MoveTo(R.Left, (R.Bottom + R.Top) div 2);
    StringGrid.Canvas.LineTo(R.Right, (R.Bottom + R.Top) div 2);
    StringGrid.Canvas.Brush.Color := OldColor;
  end;

  {sub}
  procedure DrawCellCheck;
  var
    S:     string;
    R, R1: TRect;
    BMap:  TBitmap;
    Check: boolean;
    F:     TFont;
    PosX, PosY: integer;

  begin
    StringGrid.Canvas.Brush.Color := StringGrid.Color;
    StringGrid.Canvas.FillRect(Rect);
    Check := False;

    case ACol of
      0: Check := Levels[ARow].Visible;
      2: Check := not Levels[ARow].AutoCalc;
      4: Check := not Levels[ARow].AutoCalc;
    end;

    if Check then
    begin
      if (ACol = CheckedColumn) then
        S := 'Ö'
      else
        S := '·';
      F := TFont.Create;
      try
        F.Assign(StringGrid.Canvas.Font);
        try
          StringGrid.Canvas.Font.Name := 'SYMBOL';
          PosX := (Rect.Right - Rect.Left) div 2 -
            StringGrid.Canvas.TextWidth(S) div 2;
          PosY := (Rect.Bottom - Rect.Top) div 2 -
            StringGrid.Canvas.TextHeight(S) div 2;
          StringGrid.Canvas.TextRect(Rect, Rect.Left + PosX, Rect.Top + PosY, S);
        finally
          StringGrid.Canvas.Font.Assign(F);
        end;
      finally
        F.Free;
      end;
    end;
  end;

begin
  if (ACol = NoColumn) or (ACol = ValueColumn) then
  begin
    DrawCellText;
  end
  else
  begin
    if (ACol = LineColumn) then
      DrawCellLine
    else if (ACol = FillColumn) then
      DrawCellColor
    else if (ACol = CheckedColumn) or (ACol = CheckedFillColumn) or
      (ACol = CheckedLineColumn) then
      DrawCellCheck;
  end;
  if (gdSelected in State) then
    StringGrid.Canvas.DrawFocusRect(Rect);
end;

{ TPointStyle }

procedure TPointStyle.Assign(Source: TPersistent);
var
  S: TPointStyle;
begin
  if Source is TPointStyle then
  begin
    S      := TPointStyle(Source);
    FSize  := S.Size;
    FColor := S.Color;
    Change;
  end
  else
    inherited;
end;

procedure TPointStyle.Change;
begin
  if Assigned(FonChange) then
    FonChange(Self);
end;

procedure TPointStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TPointStyle.SetSize(const Value: double);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Change;
  end;
end;

{ TLineStyle }

procedure TLineStyle.Assign(Source: TPersistent);
begin
  if Source is TLineStyle then
  begin
    FColor := TLineStyle(Source).Color;
    FSize  := TLineStyle(Source).Size;
    FStippleFactor := TLineStyle(Source).StippleFactor;
    FStipplePattern := TLineStyle(Source).FStipplePattern;
    Change;
  end
  else
    inherited;
end;

procedure TLineStyle.Change;
begin
  if Assigned(FonChange) then
    FonChange(Self);
end;

procedure TLineStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TLineStyle.SetSize(const Value: integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Change;
  end;
end;

procedure TLineStyle.SetStippleFactor(const Value: byte);
begin
  if FStippleFactor <> Value then
  begin
    FStippleFactor := Value;
    Change;
  end;
end;

procedure TLineStyle.SetStipplePattern(const Value: word);
begin
  if FStipplePattern <> Value then
  begin
    FStipplePattern := Value;
    Change;
  end;
end;

{ TFillStyle }

procedure TFillStyle.Assign(Source: TPersistent);
var
  S: TFillStyle;
begin
  if Source is TFillStyle then
  begin
    BeginUpdate;
    try
      S      := TFillStyle(Source);
      FColor := S.Color;
      FTransparent := S.Transparent;
      FPatternColor := S.PatternColor;
      Pattern := S.Pattern;
      Texture := S.Texture;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TFillStyle.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TFillStyle.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    Change;
end;

procedure TFillStyle.Change;
begin
  if FUpdateCounter <> 0 then
    Exit;
  if Assigned(FonChange) then
    FonChange(Self);
end;

procedure TFillStyle.ChangeFill(Sender: TObject);
begin
  Change;
end;

destructor TFillStyle.Destroy;
begin
  FPattern.Free;
  FTexture.Free;
  inherited;
end;


procedure TFillStyle.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TFillStyle.SetPattern(const Value: TBitmap);
begin
  if FPattern <> Value then
  begin
    if Value = nil then
    begin
      FPattern.Free;
      FPattern := nil;
      ChangeFill(Self);
    end
    else
    begin
      if Pattern = nil then
      begin
        FPattern := TBitMap.Create;
        FPattern.OnChange := ChangeFill;
      end;
      FPattern.Assign(Value);
    end;
  end;
end;

procedure TFillStyle.SetPatternColor(const Value: TColor);
begin
  if FPatternColor <> Value then
  begin
    FPatternColor := Value;
    Change;
  end;
end;

procedure TFillStyle.SetTexture(const Value: TBitmap);
begin
  if FTexture <> Value then
  begin
    if Value = nil then
    begin
      FTexture.Free;
      FTexture := nil;
      Change;
    end
    else
    begin
      if FTexture = nil then
      begin
        FTexture := TBitMap.Create;
        FTexture.OnChange := ChangeFill;
      end;
      FTexture.Assign(Value);
    end;
  end;
end;

procedure TFillStyle.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Change;
  end;
end;

{ TLevel }

procedure TLevel.Assign(Source: TPersistent);
var
  S: TLevel;
begin
  if Source is TLevel then
  begin
    BeginUpdate;
    try
      S      := TLevel(Source);
      FVisible := S.Visible;
      FAutoCalc := S.AutoCalc;
      FValue := S.Value;
      FPoint.Assign(S.Point);
      FLine.Assign(S.Line);
      FFill.Assign(S.Fill);
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TLevel.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TLevel.Change;
begin
  if FUpdateCounter > 0 then
    Exit;
  if Assigned(fonChange) then
    FonChange(Self);
end;

procedure TLevel.ChangeStyle(Sender: TObject);
begin
  Change;
end;

constructor TLevel.Create(Collection: TCollection);
begin
  inherited;
  FPoint := TPointStyle.Create;
  FPoint.onChange := ChangeStyle;
  FLine  := TLineStyle.Create;
  FLine.onChange := ChangeStyle;
  FFill  := TFillStyle.Create;
  FFill.onChange := ChangeStyle;
end;

destructor TLevel.Destroy;
begin
  FPoint.Free;
  FLine.Free;
  FFill.Free;
  inherited;
end;

procedure TLevel.Edit;
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(nil);
  try
    ColorDialog.Color := Fill.Color;
    if ColorDialog.Execute then
    begin
      Line.Color := ColorDialog.Color;
      Fill.Color := ColorDialog.Color;
    end;
  finally
    ColorDialog.Free;
  end;
end;

procedure TLevel.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    Change;
end;

procedure TLevel.SetAutoCalc(const Value: boolean);
begin
  if FAutoCalc <> Value then
  begin
    FAutoCalc := Value;
    Change;
  end;
end;

procedure TLevel.SetFill(const Value: TFillStyle);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
  end;
end;

procedure TLevel.SetLine(const Value: TLineStyle);
begin
  if FLine <> Value then
  begin
    FLine.Assign(Value);
  end;
end;

procedure TLevel.SetPoint(const Value: TPointStyle);
begin
  if FPoint <> Value then
  begin
    FPoint.Assign(Value);
  end;
end;

procedure TLevel.SetValue(const Value: double);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Change;
  end;
end;

procedure TLevel.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Change;
  end;
end;

procedure TfmMapLegend.StringGridGetEditText(Sender: TObject;
  ACol, ARow: integer; var Value: string);
begin
  FOldValue := Levels[ARow].Value;
  Value     := FloatToStr(RoundTo(Levels[ARow].Value, Precision));
end;

procedure TfmMapLegend.StringGridKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
var
  Msg: TMessage;
begin
  case Key of
    VK_F2:
      Key := 0;
    VK_ESCAPE:
    begin
      if StringGrid.Row = 1 then
        Levels.MaxValue := FOldValue;
      if StringGrid.Row = StringGrid.RowCount - 1 then
        Levels.MinValue := FOldValue;
      Levels[StringGrid.Row].Value := FOldValue;
    end;
    VK_RETURN:
    begin
      if (StringGrid.Col = CheckedColumn) or (StringGrid.Col =
        CheckedFillColumn) or (StringGrid.Col = CheckedLineColumn) then
        ChangeChecked(StringGrid.Col);
      if (StringGrid.Col = FillColumn) or (StringGrid.Col = LineColumn) then
        Levels[CurrentLevel].Edit;
    end;
  end;
end;

procedure TfmMapLegend.StringGridKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and not (goEditing in StringGrid.Options) then
    Key := #0;
end;

procedure TfmMapLegend.StringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  ACol, ARow: integer;
begin
  StringGrid.MouseToCell(X, Y, ACol, ARow);
  if (ARow > 0) and (ARow <= StringGrid.RowCount - 1) then
    StringGrid.Row := ARow;
  if (ACol <= StringGrid.ColCount - 1) then
    StringGrid.Col := ACol;
  if (ssLeft in Shift) then
  begin
    if (ssDouble in Shift) and ((ACol = FillColumn) or (ACol = LineColumn)) and
      (ARow <> 0) then
      Levels[ARow].Edit;
    if (ssDouble in Shift) and ((ACol = CheckedColumn) or
      (ACol = CheckedFillColumn) or (ACol = CheckedLineColumn)) and (ARow <> 0) then
      ChangeChecked(ACol);
  end;
end;

procedure TfmMapLegend.StringGridSelectCell(Sender: TObject;
  ACol, ARow: integer; var CanSelect: boolean);
begin
  StringGrid.Options := StringGrid.Options - [goEditing];
  begin
    if (ACol = ValueColumn) then
      if (ARow = 1) or (ARow < Levels.LevelCount - 1) then
        StringGrid.Options := StringGrid.Options + [goEditing];
  end;
end;

procedure TfmMapLegend.StringGridSetEditText(Sender: TObject;
  ACol, ARow: integer; ValueAsText: string);
begin
  if ValueAsText = '' then
    ValueAsText := '0';
  Levels[ARow].Value := StrToFloat(ValueAsText);
  if (ARow = 1) or (ARow = Levels.LevelCount - 1) then
  begin
    if ARow = 1 then
      Levels.MaxValue := StrToFloat(ValueAsText)
    else
      Levels.MinValue := StrToFloat(ValueAsText);
    with Levels do
      Build(MinValue, MaxValue, LevelCount, Mode);
  end;
end;

procedure TfmMapLegend.MenuItemSetColorClick(Sender: TObject);
begin
  Levels[CurrentLevel].Edit;
end;

procedure TfmMapLegend.MenuItemSetLineClick(Sender: TObject);
begin
  Levels[CurrentLevel].Edit;
end;

procedure TfmMapLegend.MenuItemHideClick(Sender: TObject);
begin
  Levels[CurrentLevel].Visible := False;
end;

procedure TfmMapLegend.MenuItemShowClick(Sender: TObject);
begin
  Levels[CurrentLevel].Visible := True;
end;

procedure TfmMapLegend.MenuItemCheckAllClick(Sender: TObject);
begin
  Levels.SetAllVisible;
end;

procedure TfmMapLegend.PopupMenuPopup(Sender: TObject);
begin
  if Levels[CurrentLevel].Visible then
  begin
    MenuItemShow.Visible := False;
    MenuItemHide.Visible := True;
  end
  else
  begin
    MenuItemShow.Visible := True;
    MenuItemHide.Visible := False;
  end;

  case Regim of
    lmSpectrum:
    begin
      if (StringGrid.Row > 1) and (StringGrid.Row < StringGrid.RowCount - 1) then
      begin
        MenuItemSetLine.Enabled  := False;
        MenuItemSetColor.Enabled := False;
      end
      else
      begin
        MenuItemSetLine.Enabled  := True;
        MenuItemSetColor.Enabled := True;
      end;
    end;
    lmFromFile:
      MenuItemSetColor.Enabled := True;
  end;
end;

procedure TfmMapLegend.HeaderControlSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  case Section.Index of
    0: Levels.SetAllVisible; // Select all or deselect all levels
    1:
    begin
      Levels.FileName := SetExtention(ExpandPath(DirLegends) + tblLevels, TableExt);
      Levels.BuildDefault;
    end;
    2:
  end;
end;

end.
