//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The dialog to set parameters for calculation of sample contacts}

unit fComposeContacts;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Math,
  System.IniFiles,
  System.Contnrs,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Buttons,
  Vcl.ToolWin,
  CheckLst,
  Vcl.Samples.Spin,
  FMTBcd,
  SqlExpr,

  //DB
  Data.DB,
  Bde.DBTables,


  fInitialDialog,
  GBEditValue,
  dBase,
  dDialogs;

type
  TfmComposeContacts = class(TfmInitialDialog)
    ProgressBar:      TProgressBar;
    TableOutput:      TTable;
    GroupBoxDividing: TGroupBox;
    LabelMinimumSampleSplitting: TLabel;
    LabelLengthDependent: TLabel;
    CheckBoxDividingOfSamples: TCheckBox;
    CheckListBoxLengthDependantAttributes: TCheckListBox;
    TableCollars:     TTable;
    TableAssays:      TTable;
    TableInclins:     TTable;
    SQLQueryAssays:   TSQLQuery;
    QueryAssays:      TQuery;
    QueryInclins:     TQuery;
    GroupBoxOutput:   TGroupBox;
    ToolBarShowAs:    TToolBar;
    ToolButton3:      TToolButton;
    ToolButtonMap:    TToolButton;
    ToolButtonTable:  TToolButton;
    ToolButtonGraph:  TToolButton;
    ToolButton4:      TToolButton;
    PanelOutputPath:  TPanel;
    EditOutputName:   TEdit;
    SpeedButtonOutputBrowse: TSpeedButton;
    GroupBoxInput:    TGroupBox;
    GroupBoxAssays:   TGroupBox;
    StaticTextAssays: TStaticText;
    GroupBoxCollars:  TGroupBox;
    StaticTextCollars: TStaticText;
    GroupBoxInclins:  TGroupBox;
    StaticTextInclins: TStaticText;
    SpeedButtonAssaysInfo: TSpeedButton;
    SpeedButtonAssaysBrowse: TSpeedButton;
    SpeedButtonCollarsInfo: TSpeedButton;
    SpeedButtonCollarsBrowser: TSpeedButton;
    SpeedButtonInclinsInfo: TSpeedButton;
    SpeedButtonInclinsBrowser: TSpeedButton;
    RadioGroupSmoothing: TRadioGroup;
    SpinEditSmoothDegrees: TSpinEdit;
    GBEditValueMinLength: TGBEditValue;
    ImageListOutput: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure EditOutputNameChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxDividingOfSamplesClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ToolButtonShowAsClick(Sender: TObject);
    procedure SpeedButtonAssaysInfoClick(Sender: TObject);
    procedure SpeedButtonCollarsInfoClick(Sender: TObject);
    procedure SpeedButtonInclinsInfoClick(Sender: TObject);
    procedure SpeedButtonAssaysBrowseClick(Sender: TObject);
    procedure SpeedButtonCollarsBrowserClick(Sender: TObject);
    procedure SpeedButtonInclinsBrowserClick(Sender: TObject);
  private
     
    function CalculateXYZ: boolean;
    procedure CreateOutputTable;
  public
     
    OutModelType: integer;
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmComposeContacts: TfmComposeContacts;

implementation

uses
  uCommon,
  uGlobals,
  uProfuns,
  uResStrings,
  uDiscoCore,
  fEditGetStatist;

{$R *.dfm}

procedure TfmComposeContacts.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;

  TableAssays.TableName := ExpandPath(DirExploring) + tblAssays;
  if not FileExists(ChangeFileExt(TableAssays.TableName, TableExt)) then
    MessageDlg(LoadResString(@rsFileNotFound) + ' ' + TableAssays.TableName,
      mtInformation, [mbOK], 0);

  TableCollars.TableName := ExpandPath(DirExploring) + tblCollars;
  if not FileExists(ChangeFileExt(TableCollars.TableName, TableExt)) then
    MessageDlg(LoadResString(@rsFileNotFound) + ' ' + TableCollars.TableName,
      mtInformation, [mbOK], 0);

  TableInclins.TableName := ExpandPath(DirExploring) + tblInclins;
  if not FileExists(ChangeFileExt(TableInclins.TableName, TableExt)) then
    MessageDlg(LoadResString(@rsFileNotFound) + ' ' + TableInclins.TableName,
      mtInformation, [mbOK], 0);

  QueryAssays.DatabaseName  := ExpandPath(DirExploring);
  QueryInclins.DatabaseName := ExpandPath(DirExploring);


  StaticTextAssays.Caption  := ' ' + TableAssays.TableName;
  StaticTextCollars.Caption := ' ' + TableCollars.TableName;
  StaticTextInclins.Caption := ' ' + TableInclins.TableName;

  TableAssays.Open;
  TableInclins.Open;
  TableCollars.Open;
end;

procedure TfmComposeContacts.FormActivate(Sender: TObject);
begin
  inherited;
  PanelOutputPath.Caption := ExpandPath(DirDholes);
  EditOutputName.Text     := NameOnly(TableAssays.TableName);
  TableOutput.TableName   := ExpandPath(DirDholes) + EditOutputName.Text;
end;

procedure TfmComposeContacts.ToolButtonShowAsClick(Sender: TObject);
begin
  ToolBarShowAs.Tag := (Sender as TToolButton).Tag;
end;

procedure TfmComposeContacts.FormDeactivate(Sender: TObject);
begin
  TableCollars.Close;
  TableInclins.Close;
  TableAssays.Close;
end;


function GetT01(X1, X2, X: double; ErrorValue: double = 0): double;
begin
  Result := (X - X1) / (X2 - X1);
  if (Result < 0) or (Result > 1) then
    Result := ErrorValue;
end;

function GetT(X1, X2, X: double): double;
begin
  Result := (X - X1) / (X2 - X1);
end;

function LineInterpolate(V1, V2, T: double): double;
begin
  Result := V1 + (V2 - V1) * T;
end;

function GetPoint3d(X, Y, Z: double): TPoint3d;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Move(Point: TPoint3d; dX, dY, dZ: double): TPoint3D; overload;
begin
  Result := GetPoint3d(Point.X + dX, Point.Y + dY, Point.Z + dZ);
end;

type
  TDirection = record
    Azimuth: double;
    Zenith:  double;
  end;

function Direction(Azimuth, Zenith: double): TDirection;
begin
  Result.Azimuth := Azimuth;
  Result.Zenith  := Zenith;
end;

function Move(Point: TPoint3d; Direction: TDirection; Distance: double): TPoint3d;
  overload;
begin
  with Direction do
  begin
    Zenith  := DegToRad(Zenith);
    Azimuth := DegToRad(Azimuth);
    Result  := GetPoint3d(Point.X + Distance * sin(Zenith) * sin(Azimuth),
      Point.Y + Distance * sin(Zenith) * cos(Azimuth), Point.Z -
      Distance * cos(Zenith));
  end;
end;

type
  TDirectionRec = record
    Depth:     double;
    Direction: TDirection;
  end;

  TTraceItem = record
    Point: TPoint3D;
    Depth: double;
  end;

  TTrace = array of TTraceItem;

  TPointRec = class
  public
    Point: TPoint3D;
    Depth: double;
    constructor Create(ADepth: double; APoint: TPoint3d);
  end;

{ TPointRec }

constructor TPointRec.Create(ADepth: double; APoint: TPoint3d);
begin
  Point := APoint;
  Depth := ADepth;
end;

type
  TInclinatorState  = (isValidTrace);
  TInclinatorStates = set of TInclinatorState;
  TDirectionArray   = array of TDirectionRec;

  TPointList = class(TObjectList)
  private
    function GetItems(Index: integer): TPointRec;
  public
    property Items[Index: integer]: TPointRec Read GetItems; default;
  end;

{ TPointList }

function TPointList.GetItems(Index: integer): TPointRec;
begin
  Result := TPointRec(inherited Items[Index]);
end;

type
  TTraceSmoothing = (tsNone, tsAverage, tsByDegrees);

type
  TInclinator = class
  private
    FState:     TInclinatorStates;
    FTraceSmoothing: TTraceSmoothing;
    FPoints:    TPointList;
    FDataSet:   TDataSet;
    FDefaultZenith: double;
    FDirection: TDirectionArray;
    FLinkFieldName: string;
    FAutoSortPoints: boolean;
    FSmoothDegrees: double;

    function GetCollar: TPoint3d;
    procedure SetCollar(const Value: TPoint3d);
    function GetPoints: TPointList;
    procedure SetPoint(Depth: double; const Value: TPoint3d);
    function GetPoint(Depth: double): TPoint3d;
    function GetDirection(Depth: double): TDirection;
    procedure SetDirection(Depth: double; const Value: TDirection);
    property Points: TPointList read GetPoints write FPoints;
    property State: TInclinatorStates read FState write FState;
    procedure SetSmoothDegrees(Value: double);
  public
    constructor Create(ADataSet: TDataSet; ALinkFieldName: string;
      ADefaultZenith: double = 0);
    destructor Destoy;
    procedure Clear;
    procedure BuildTrace;
    procedure LoadFromDataSet;
    function GetTrace(Depth1, Depth2: double): TTrace;

    property Point[Depth: double]: TPoint3d read GetPoint write SetPoint; default;
    property Direction[Depth: double]: TDirection read GetDirection write SetDirection;
    property AutoSortPoints: boolean read FAutoSortPoints write FAutoSortPoints;
    property Collar: TPoint3d read GetCollar write SetCollar;
    property LinkFieldName: string read FLinkFieldName write FLinkFieldName;
    property TraceSmoothing: TTraceSmoothing read FTraceSmoothing write FTraceSmoothing;
    property SmoothDegrees: double read FSmoothDegrees write SetSmoothDegrees;
  end;

{ TInclinator }

constructor TInclinator.Create(ADataSet: TDataSet; ALinkFieldName: string;
  ADefaultZenith: double = 0);
begin
  inherited Create;
  FState   := [];
  FDataSet := ADataSet;
  FLinkFieldName := ALinkFieldName;
  FDefaultZenith := ADefaultZenith;
  FAutoSortPoints := True;
  FSmoothDegrees := 3;
  FTraceSmoothing := tsByDegrees;
end;

destructor TInclinator.Destoy;
begin
  Clear;
  FDirection := nil;
  inherited;
end;

procedure TInclinator.LoadFromDataSet;
var
  LinkValue: string;
begin
  if FDataSet <> nil then
  begin
    LinkValue := FDataSet.FieldByName(LinkFieldName).AsString;
    while not FDataSet.EOF and (LinkValue =
        FDataSet.FieldByName(LinkFieldName).AsString) do
    begin
      Direction[FDataSet.FieldByName(fldDEPTH).AsFloat] :=
        fComposeContacts.Direction(FDataSet.FieldByName(fldAZIMUTH).AsFloat,
        FDataSet.FieldByName(fldINCLINATION).AsFloat);
      FDataSet.Next;
    end;
  end;
end;

function TInclinator.GetPoint(Depth: double): TPoint3d;
var
  T: double;
  I: integer;
begin
  if not (isValidTrace in State) then
    BuildTrace;

  I := Points.Count - 2; // Points.Count>=2 after BuildTrace
  while (I > 0) and (Depth < TPointRec(Points[I]).Depth) do
    Dec(I);
  T := GetT(Points[I].Depth, Points[I + 1].Depth, Depth);
  Result.X := LineInterpolate(Points[I].Point.X, Points[I + 1].Point.X, T);
  Result.Y := LineInterpolate(Points[I].Point.Y, Points[I + 1].Point.Y, T);
  Result.Z := LineInterpolate(Points[I].Point.Z, Points[I + 1].Point.Z, T);
end;

function Compare(PointRec1, PointRec2: Pointer): integer;
begin
  with TPointRec(PointRec1) do
    if Depth < TPointRec(PointRec2).Depth then
      Result := -1
    else if Depth > TPointRec(PointRec2).Depth then
      Result := 1
    else
      Result := 0;
end;

procedure TInclinator.SetPoint(Depth: double; const Value: TPoint3d);
begin
  Points.Add(TPointRec.Create(Depth, Value));
  if AutoSortPoints then
    FPoints.Sort(Compare);
end;

procedure TInclinator.Clear;
begin
  Points.Clear;
  FDirection := nil;
  Exclude(FState, isValidTrace);
end;

function TInclinator.GetPoints: TPointList;
begin
  if not Assigned(FPoints) then
    FPoints := TPointList.Create(True);
  Result := FPoints;
end;

function DeltaAngle(A1, A2: double): double;
begin
  Result := A2 - A1;
  while Result < -180 do
    Result := Result + 360;
  while Result > 180 do
    Result := Result - 360;
end;

procedure TInclinator.BuildTrace;
var
  I, J: integer;
  N:    integer;
  Col:  TPoint3d;
  PrevDir, Dir: TDirection;
  dAzimuth, dZenith, H: double;
begin
  Col := Collar;
  Points.Clear;
  Point[-1] := Move(Col, Direction[0], -1);
  //Dummy point before a drillhole collar
  Point[0]  := Col; // Drillhole collar
  if (High(FDirection) > -1) and (Abs(FDirection[0].Depth) > 1E-9) then
  begin //if inclination not from 0, then calc the origin point of inclination
    Dir := FDirection[0].Direction;
    Point[FDirection[0].Depth] := Move(Col, Dir, FDirection[0].Depth);
  end;
  N := 1;
  for I := 0 to High(FDirection) - 1 do
  begin
    Dir := FDirection[I].Direction;
    dAzimuth := DeltaAngle(Dir.Azimuth, FDirection[I + 1].Direction.Azimuth);
    dZenith := DeltaAngle(Dir.Zenith, FDirection[I + 1].Direction.Zenith);
    H := FDirection[I + 1].Depth - FDirection[I].Depth;

    if (TraceSmoothing = tsByDegrees) then
      N := Max(1, Trunc(Max(Abs(dAzimuth), Abs(dZenith)) / SmoothDegrees));
    PrevDir := Dir;
    for J := 1 to N do
    begin
      case TraceSmoothing of
        tsNone, tsByDegrees:
        begin
          Dir.Azimuth := PrevDir.Azimuth + dAzimuth * ((J) / N);
          Dir.Zenith  := PrevDir.Zenith + dZenith * ((J) / N);
        end
        else
        begin //Average Smoothing
          Dir.Azimuth := PrevDir.Azimuth + dAzimuth / 2;
          Dir.Zenith  := PrevDir.Zenith + dZenith / 2;
        end;
      end;
      Point[FDirection[I].Depth + H * (J / N)] :=
        Move(Points[Points.Count - 1].Point, Dir, H / N);
    end;
  end;
  if (High(FDirection) > -1) then
  begin
    Dir := FDirection[High(FDirection)].Direction;
    Point[FDirection[High(FDirection)].Depth + 1] :=
      Move(Points[Points.Count - 1].Point, Dir, 1);
  end;
  Include(FState, isValidTrace);
end;

function TInclinator.GetDirection(Depth: double): TDirection;
var
  I: integer;
  T: double; {0<=T<=1}
begin
  if High(FDirection) < 0 then
    Result := fComposeContacts.Direction(0, 0)
  else
  begin
    I := High(FDirection);
    while (I >= 0) and (Depth < FDirection[I].Depth) do
      Dec(I);
    if I < 0 then
      Result := FDirection[0].Direction
    else if I = High(FDirection) then
      Result := FDirection[I].Direction
    else
      with FDirection[I].Direction do
      begin
        T := GetT01(FDirection[I].Depth, FDirection[I + 1].Depth, Depth);
        Result.Azimuth := LineInterpolate(Azimuth,
          FDirection[I + 1].Direction.Azimuth, T);
        Result.Zenith := LineInterpolate(Zenith,
          FDirection[I + 1].Direction.Zenith, T);
      end;
  end;
end;

{  x   Alpha=10°  L=1     X:=Sin(10)*1     X1:=Sin(10/10*1)*1/10  Y1:=Cos(10/10*1)*1/10
   |\             n=10    Y:=Cos(10)*1     X2:=Sin(10/10*2)*1/10  Y2:=Cos(10/10*2)*1/10
   \ \                                     Xn:=Sin(10/10*n)*1/10  Yn:=Cos(10/10*n)*1/10
    | \                                              n
    \  \                                            --
     |  \                                  X`=L/n*( >  Sin (10/n*i) )
     \   \                                          --
      |   \                                         i=1
      \    \                                         n
       |    \                                       --
       \     \                             Y`=L/n*( >  Cos (10/n*i) )
(X`,Y`) x     x (X,Y)                               --
                                                    i=1

                          n
                         --
dX= Sin(Alpha)*L - L/n*( >  Sin (Alpha/n*i) )
                         --
                         i=1

When we are divided the segment on n parts with alpha/n angle between ones
we get sequence of chords on a circle.
On n-->oo we will get arc of circle with Alpha degs and L length.
}

procedure TInclinator.SetDirection(Depth: double; const Value: TDirection);
var
  I: integer;
begin
  // If there is a contact on the depth, then correct it
  for I := 0 to High(FDirection) do
    if Abs(FDirection[I].Depth - Depth) < 1E-9 then
    begin
      if not CompareMem(@(FDirection[I].Direction), @Value, SizeOf(Value)) then
      begin
        FDirection[I].Direction := Value;
        Exclude(FState, isValidTrace);
      end;
      Exit;
    end;
  // If directions of two previous measurements are the same on this depth,
  // then we need to change the depth for previous record
  // reject next loop on add records not in ascending order of depth
  //{BEGIN
  I := High(FDirection);
  while (I > 0) and (Depth < FDirection[I].Depth) do
    Dec(I);
  if (I >= 0) and CompareMem(@(FDirection[I].Direction), @Value, SizeOf(Value)) and
    ((I = 0) or CompareMem(@(FDirection[I - 1].Direction), @Value,
    SizeOf(Value))) then
  begin
    FDirection[I].Depth := Depth;
    Exclude(FState, isValidTrace);
    Exit;
  end;
  {END}
  // add to array in ascending order of depth (distance from collar)
  SetLength(FDirection, High(FDirection) + 2);
  I := High(FDirection);
  while (I > 0) and (Depth < FDirection[I].Depth) do
  begin
    FDirection[I] := FDirection[I - 1];
    Dec(I);
  end;
  FDirection[I].Depth     := Depth;
  FDirection[I].Direction := Value;
  Exclude(FState, isValidTrace);
end;

function TInclinator.GetCollar: TPoint3d;
begin
  if Points.Count = 0 then
    Result := GetPoint3d(0, 0, 0)
  else
    Result := Points[0].Point;
end;

procedure TInclinator.SetCollar(const Value: TPoint3d);
var
  P: TPoint3d;
begin
  if Points.Count > 0 then
    P := Point[0];
  if (Points.Count = 0) or (not CompareMem(@P, @Value, SizeOf(Value))) then
  begin
    Points.Clear;
    Point[0] := Value;
    Exclude(FState, isValidTrace);
  end;
end;

function TInclinator.GetTrace(Depth1, Depth2: double): TTrace;
var
  I: integer;
begin
  if not (isValidTrace in State) then
    BuildTrace;
  SetLength(Result, 1);
  Result[0].Depth := Depth1;
  Result[0].Point := Point[Depth1];
  I := 0;
  while (I < Points.Count) and (Points[I].Depth <= Depth1) do
    Inc(I);
  while (I < Points.Count) and (Points[I].Depth < Depth2) do
  begin
    SetLength(Result, High(Result) + 2);
    Result[High(Result)].Depth := Points[I].Depth;
    Result[High(Result)].Point := Points[I].Point;
    Inc(I);
  end;
  SetLength(Result, High(Result) + 2);
  Result[High(Result)].Depth := Depth2;
  Result[High(Result)].Point := Point[Depth2];
end;

procedure TInclinator.SetSmoothDegrees(Value: double);
begin
  if Value < 0.5 then
    Value := 0.5
  else if Value > 179.5 then
    Value := 179.5;
  if FSmoothDegrees <> Value then
  begin
    FSmoothDegrees := Value;
  end;
end;


function TfmComposeContacts.CalculateXYZ: boolean;
var
  MessageStr: string;
  LinkFieldValue: string;
  EndOfLoop:  boolean;
  From1, From2, To1, To2: double;
  Profile:    string;
  Point:      TPOint3d;
  Inclinator: TInclinator;
  I, J, K:    integer;

  {sub}
  function CopyFieldValue(Dest, Source: TDataSet): boolean;
  var
    I:     integer;
    Field: TField;
  begin
    Result := True;
    for I := 0 to Source.FieldCount - 1 do
    begin
      try
        with Source.Fields[I] do
        begin
          Field := Dest.FindField(FieldName);
          if Field <> nil then
            Field.Value := Value;
          //      Dest.FieldByName(FieldName).Value:=Value;
        end;
      except
        Result := False;
      end;
    end;
  end; //CopyFieldValue

  {sub}
  procedure SavePoint(Point: TPoint3d; Depth, Length: double; CopyFields: boolean);
  begin
    TableOutput.Append;
    if CopyFields then
      CopyFieldValue(TableOutput, QueryAssays);
    TableOutput.FieldByName(fldX).AsFloat      := RoundTo(Point.X, Precision);
    TableOutput.FieldByName(fldY).AsFloat      := RoundTo(Point.Y, Precision);
    TableOutput.FieldByName(fldZ).AsFloat      := RoundTo(Point.Z, Precision);
    TableOutput.FieldByName(fldDHOLE).AsString := LinkFieldValue;
    TableOutput.FieldByName(fldPROFILE).AsString := Profile;
    TableOutput.FieldByName(fldDEPTH).AsFloat  := RoundTo(Depth, Precision);
    if CopyFields then
      TableOutput.FieldByName(fldLENGTH).AsFloat := RoundTo(Length, Precision);
    TableOutput.Post;
  end;

  {Sub}
  procedure SaveSample(AFrom, ATo: double; CopyFields: boolean);
  var
    H:     double;
    I, N:  integer;
    Trace: TTrace;
  begin
    Trace := nil;
    H     := Abs(ATo - AFrom);
    if (CheckBoxDividingOfSamples.Checked) and
      (H > GBEditValueMinLength.AsDouble) then
    begin
      Trace := Inclinator.GetTrace(AFrom, ATo);
      try
        N := High(Trace) - 1;
        for I := 0 to N do
          SavePoint(Trace[I].Point, Trace[I].Depth,
            Abs(Trace[I + 1].Depth - Trace[I].Depth), CopyFields);
      finally
        Trace := nil;
      end;
    end
    else
      SavePoint(Inclinator.Point[AFrom], AFrom, H, CopyFields);
  end;

begin
  MessageStr := '';
  begin
    CreateOutputTable;
    ProgressBar.Position := 0;
    ProgressBar.Min      := 0;
    ProgressBar.Max      := 2;
    with QueryAssays do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT * FROM "' + TableAssays.TableName + '" D ' {+
        'ORDER BY HOLE, D."FROM"'{});
      Open;
    end;
    ProgressBar.StepIt;
    with QueryInclins do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT DISTINCT * FROM "' + TableInclins.TableName +
        '" ' + 'ORDER BY ' + fldDHOLE + ', ' + fldDEPTH);
      Open;
    end;
    ProgressBar.StepIt;
    ProgressBar.Max      := QueryAssays.RecordCount;
    ProgressBar.Position := 0;
    TableCollars.Open;
    TableOutput.Open;
    QueryInclins.First;
    QueryAssays.First;
    TableOutput.Last;
    Inclinator := TInclinator.Create(QueryInclins, fldDHOLE);
    case RadioGroupSmoothing.ItemIndex of
      0: Inclinator.TraceSmoothing := tsNone;
      1: Inclinator.TraceSmoothing := tsAverage;
      2:
      begin
        Inclinator.TraceSmoothing := tsByDegrees;
        Inclinator.SmoothDegrees  := SpinEditSmoothDegrees.Value;
      end;
    end;
    Inclinator.AutoSortPoints := False;
    try
      K := 0;
      repeat
        Inc(K);
        ProgressBar.Position := QueryAssays.RecNo;
        LinkFieldValue := QueryAssays.FieldByName(fldDHOLE).AsString;
        // finding LinkFieldValue in fldDHOLE
        if TableCollars.Locate(fldDHOLE, LinkFieldValue, []) then
        begin
          Point := GetPoint3d(TableCollars.FieldByName(fldX).AsFloat,
            TableCollars.FieldByName(fldY).AsFloat,
            TableCollars.FieldByName(fldZ).AsFloat);

          Inclinator.Clear;
          if QueryInclins.Locate(fldDHOLE, LinkFieldValue, []) then
            Inclinator.LoadFromDataSet;
          Inclinator.Collar := Point;

          if TableCollars.FindField(fldPROFILE) <> nil then
            Profile := TableCollars.FieldByName(fldPROFILE).AsString;

          To2   := 0;
          From2 := 0;
          repeat
            To1   := To2;
            From2 := QueryAssays.FieldByName(fldFROM).AsFloat;
            To2   := QueryAssays.FieldByName(fldTO).AsFloat;

            if Abs(To1 - From2) > 0.01 then // End of Samples not Equal
              if To1 - From2 > 0.01 then
                // Add error Msg Samples intersect more then 10mm
              begin
              end
              else // Empty interval more then 10mm between Samples
              begin
                SaveSample(To1, From2, False);
                //              From1:=To1;
              end;

            SaveSample(From2, To2, True);

            QueryAssays.Next;
            EndOfLoop := QueryAssays.EOF;
            if (QueryAssays.FieldByName(fldDHOLE).AsString <>
              LinkFieldValue) or EndOfLoop then
            begin
              // save the sample from end of last sample to the bottom of hole
              From2 := To2;
              To2   := TableCollars.FieldByName(fldDEPTH).AsFloat;
              if To2 < From2 then
                To2 := From2;

              SaveSample(From2, To2, False);
              // save the hole bottom if necessary
              if From2 <> To2 then
                SavePoint(Inclinator.Point[To2], To2, 0, False);
            end;
          until (QueryAssays.FieldByName(fldDHOLE).AsString <>
              LinkFieldValue) or EndOfLoop;
        end
        else
        begin
          if MessageStr = '' then
            MessageStr := LoadResString(@rsCollarsNotFound) + ': ' + LinkFieldValue
          else
            MessageStr := MessageStr + ', ' + LinkFieldValue;
          repeat
            QueryAssays.Next; //next hole
            EndOfLoop := QueryAssays.EOF;
          until (QueryAssays.FieldByName(fldDHOLE).AsString <>
              LinkFieldValue) or EndOfLoop;
        end;
      until EndOfLoop;
    finally
      Inclinator.Free;
    end;
    Result := True;
  end;
  QueryInclins.Close;
  QueryAssays.Close;
  TableCollars.Close;
  QueryInclins.Free;
  QueryAssays.Free;
  TableOutput.First;
  for I := 1 to TableOutput.RecordCount do //write ID
  begin
    TableOutput.Edit;
    TableOutput.FieldByName(fldID).AsInteger := I;
    TableOutput.Post;
    TableOutput.Next;
  end;
  TableOutput.Close;
  if MessageStr <> '' then
    ShowMessage(MessageStr);
end;

procedure TfmComposeContacts.CreateOutputTable;
var
  FieldCount, I: integer;
begin
  try
    TableOutput.FieldDefs.Clear;
    FieldCount := TableAssays.FieldDefs.Count - 1;

    //Set ID on first position
    for I := 0 to FieldCount do
      with TableAssays.FieldDefs.Items[I] do
        if (System.Pos(fldID, UpperCase(Name)) = 1) then
          TableOutput.FieldDefs.Add(Name, DataType, Size, False);

    //Add required fields
    TableOutput.FieldDefs.Add(fldX, ftFloat, 0, False);
    TableOutput.FieldDefs.Add(fldY, ftFloat, 0, False);
    TableOutput.FieldDefs.Add(fldZ, ftFloat, 0, False);
    TableOutput.FieldDefs.Add(fldPROFILE, ftString, 16, False);

    for I := 0 to FieldCount do
      with TableAssays.FieldDefs.Items[I] do
        if (System.Pos(fldID, UpperCase(Name)) <> 1) and
          (UpperCase(Name) <> fldFROM) and (UpperCase(Name) <> fldTO) then
          try
            TableOutput.FieldDefs.Add(Name, DataType, Size, False);
          except
          end
        else if (UpperCase(Name) = fldFROM) then
          try
            TableOutput.FieldDefs.Add(fldDEPTH, ftFloat);
          except
          end;

    if TableOutput.FieldDefs.IndexOf(fldLENGTH) = -1 then
      TableOutput.FieldDefs.Add(fldLENGTH, ftString, 16, False);

    TableOutput.CreateTable;
  finally
  end;
end;

procedure TfmComposeContacts.CheckBoxDividingOfSamplesClick(Sender: TObject);
begin
  LabelMinimumSampleSplitting.Enabled := not LabelMinimumSampleSplitting.Enabled;
  GBEditValueMinLength.Enabled := not GBEditValueMinLength.Enabled;
  LabelLengthDependent.Enabled := not LabelLengthDependent.Enabled;
end;


procedure TfmComposeContacts.EditOutputNameChange(Sender: TObject);
begin
  TableOutput.TableName := PanelOutputPath.Caption + EditOutputName.Text;
end;



procedure TfmComposeContacts.SpeedButtonAssaysInfoClick(Sender: TObject);
begin
  if TableAssays.TableName <> '' then
  begin
    fmEditGetStatist := TfmEditGetStatist.Create(Self);
    fmEditGetStatist.TableName := TableAssays.TableName;
    fmEditGetStatist.ShowModal;
    fmEditGetStatist.Free;
  end;
end;

procedure TfmComposeContacts.SpeedButtonCollarsInfoClick(Sender: TObject);
begin
  if TableCollars.TableName <> '' then
  begin
    fmEditGetStatist := TfmEditGetStatist.Create(Self);
    fmEditGetStatist.TableName := TableCollars.TableName;
    fmEditGetStatist.ShowModal;
    fmEditGetStatist.Free;
  end;
end;

procedure TfmComposeContacts.SpeedButtonInclinsInfoClick(Sender: TObject);
begin
  if TableInclins.TableName <> '' then
  begin
    fmEditGetStatist := TfmEditGetStatist.Create(Self);
    fmEditGetStatist.TableName := TableInclins.TableName;
    fmEditGetStatist.ShowModal;
    fmEditGetStatist.Free;
  end;
end;

procedure TfmComposeContacts.SpeedButtonAssaysBrowseClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := ExpandPath(DirExploring);
    if OpenDialog.Execute then
    begin
      TableAssays.Close;
      TableAssays.TableName    := OpenDialog.FileName;
      StaticTextAssays.Caption := TableAssays.TableName;
    end;
  end;
end;

procedure TfmComposeContacts.SpeedButtonCollarsBrowserClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := ExpandPath(DirExploring);
    if OpenDialog.Execute then
    begin
      TableCollars.Close;
      TableCollars.TableName    := OpenDialog.FileName;
      StaticTextCollars.Caption := TableCollars.TableName;
    end;
  end;
end;

procedure TfmComposeContacts.SpeedButtonInclinsBrowserClick(Sender: TObject);
begin
  with dmDialogs do
  begin
    OpenDialog.InitialDir := ExpandPath(DirExploring);
    if OpenDialog.Execute then
    begin
      TableInclins.Close;
      TableInclins.TableName    := OpenDialog.FileName;
      StaticTextInclins.Caption := TableInclins.TableName;
    end;
  end;
end;

procedure TfmComposeContacts.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      ToolButtonMap.Down := ReadBool(Name, ToolButtonMap.Caption, True);
      if ToolButtonMap.Down then
        ToolButtonMap.Click;
      ToolButtonTable.Down := ReadBool(Name, ToolButtonTable.Caption, False);
      if ToolButtonTable.Down then
        ToolButtonTable.Click;
      ToolButtonGraph.Down := ReadBool(Name, ToolButtonGraph.Caption, False);
      if ToolButtonGraph.Down then
        ToolButtonGraph.Click;
      RadioGroupSmoothing.ItemIndex     :=
        ReadInteger(Name, RadioGroupSmoothing.Caption, 1);
      CheckBoxDividingOfSamples.Checked :=
        ReadBool(Name, CheckBoxDividingOfSamples.Caption, True);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeContacts.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupSmoothing.Caption, RadioGroupSmoothing.ItemIndex);
      WriteBool(Name, CheckBoxDividingOfSamples.Caption,
        CheckBoxDividingOfSamples.Checked);
      WriteBool(Name, ToolButtonMap.Caption, ToolButtonMap.Down);
      WriteBool(Name, ToolButtonTable.Caption, ToolButtonTable.Down);
      WriteBool(Name, ToolButtonGraph.Caption, ToolButtonGraph.Down);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeContacts.ButtonOKClick(Sender: TObject);
begin
  inherited;
  TableOutput.TableName := ExpandPath(DirDholes) + EditOutputName.Text;
  if FileExists(ChangeFileExt(TableOutput.TableName, TableExt)) then
  begin
    if MessageDlg(LoadResString(@rsReplace) + ' ' + TableOutput.TableName +
      '?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    begin
      ModalResult := mrNone;
      Exit;
    end;
  end;
  OutModelType := mtDholes;
  if ModalResult <> mrNone then
    CalculateXYZ
  else
    Exit;
  WriteIniFile;
end;


end.
