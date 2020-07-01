//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------
{!  The dialog for compositing samples by layers or horizons}


unit fComposeByHorizons;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Variants,
  System.Contnrs,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ToolWin,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  //DB
  Bde.DBTables,
  Data.DB,

  fMethodDialog,
  dBase;

type
  TfmComposeByHorizons = class(TfmMethodDialog)
    DBGridHorizonts:    TDBGrid;
    DataSourceHorizons: TDataSource;
    TableHorizons:      TTable;
    TableHorizonsStart: TFloatField;
    TableHorizonsThickness: TFloatField;
    TableHorizonsCount: TIntegerField;
    TableHorizonsTo:    TFloatField;
    GroupBoxHorizons:   TGroupBox;
    PanelTableHorizons: TPanel;
    SpeedButtonHorizonsBrowse: TSpeedButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TableHorizonsBeforePost(DataSet: TDataSet);
    procedure TableHorizonsBeforeEdit(DataSet: TDataSet);
    procedure TableHorizonsCalcFields(DataSet: TDataSet);
    procedure TableHorizonsAfterPost(DataSet: TDataSet);
    procedure ButtonOKClick(Sender: TObject);
  private
     
  public
     
    procedure DivideByPlanes(TableInput, TableOutput: TTable);
  end;

var
  fmComposeByHorizons: TfmComposeByHorizons;

//========================================================================
implementation
//========================================================================

uses
  uGlobals,
  uProfuns,
  uCommon,
  uFileCreator;

{$R *.DFM}

type
  THorizonDef = class
  private
    FFrom:  double;
    FStep:  double;
    FCount: integer;
    function GetTo: double;
  published
    property From: double read FFrom write FFrom;
    property Step: double read FStep write FStep;
    property Count: integer read FCount write FCount;
  public
    constructor Create(AFrom, AStep: double; ACount: integer);
    function FindNexZ(Z: double; var NextZ: double): boolean;
    property TTo: double read GetTo;
  end;

{ THorizonDef }

constructor THorizonDef.Create(AFrom, AStep: double; ACount: integer);
begin
  FFrom  := AFrom;
  FStep  := AStep;
  FCount := ACount;
  if FCount < 0 then
  begin
    FStep  := -FStep;
    FCount := -FCount;
  end;
  if FStep > 0 then
  begin
    FFrom := FFrom + FStep * FCount;
    FStep := -FStep;
  end;
end;

function THorizonDef.FindNexZ(Z: double; var NextZ: double): boolean;
var
  I: integer;
begin
  Result := False;
  if Z <= TTo then
    Exit;
  Result := True;
  for I := 0 to Count do
  begin
    NextZ := From + I * Step;
    if Z > NextZ then
      Exit;
  end;
end;

function THorizonDef.GetTo: double;
begin
  Result := From + Step * Count;
end;

type
  THorizonsDef = class
  private
    FDefList: TObjectList;
    function GetItems(index: integer): THorizonDef;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(Item: THorizonDef): integer; overload;
    function Add(AFrom, AStep: double; ACount: integer): integer; overload;
    procedure Clear;
    function FindNextZ(Z: double; var NextZ: double): boolean;
    property Items[index: integer]: THorizonDef Read GetItems;
    property Count: integer Read GetCount;
  end;

{ THorizonsDef }

function THorizonsDef.Add(Item: THorizonDef): integer;
begin
  Result := FDefList.Add(Item);
end;

function THorizonsDef.Add(AFrom, AStep: double; ACount: integer): integer;
begin
  Result := Add(THorizonDef.Create(AFrom, AStep, ACount));
end;

procedure THorizonsDef.Clear;
begin
  FDefList.Clear;
end;

constructor THorizonsDef.Create;
begin
  inherited;
  FDefList := TObjectList.Create;
end;

destructor THorizonsDef.Destroy;
begin
  FDefList.Free;
  inherited;
end;

function THorizonsDef.FindNextZ(Z: double; var NextZ: double): boolean;
var
  I:     integer;
  Init:  boolean;
  TempZ: double;
begin
  Init   := True;
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if Items[I].FindNexZ(Z, TempZ) then
      if Init then
      begin
        Init   := False;
        NextZ  := TempZ;
        Result := True;
      end
      else
        NextZ := Max(NextZ, TempZ);
  end;
end;

function THorizonsDef.GetCount: integer;
begin
  Result := FDefList.Count;
end;

function THorizonsDef.GetItems(Index: integer): THorizonDef;
begin
  Result := THorizonDef(FDefList.Items[Index]);
end;


//===========================================================\\
procedure TfmComposeByHorizons.FormCreate(Sender: TObject);
begin
  inherited;
  ToolButtonHoles.Click;
  ListBoxInputNamesClick(Self);
  with TableHorizons do
  begin
//    TableName := GetTempFileName('Temp.db');
    if TableHorizons.Exists then
    begin
      TableHorizons.Active := False;
      TableHorizons.DeleteTable;
    end;
    CreateTable;
    Open;
    Append;
    TableHorizonsStart.Value     := 395;
    TableHorizonsThickness.Value := -10;
    TableHorizonsCount.Value     := 5;
    Post;
    Append;
    TableHorizonsStart.Value     := 345;
    TableHorizonsThickness.Value := -12;
    TableHorizonsCount.Value     := 17;
    Post;
  end;
end;


procedure TfmComposeByHorizons.FormDestroy(Sender: TObject);
begin
  inherited;
  TableHorizons.Close;
  try
    TableHorizons.DeleteTable;
  except
  end;
end;


//============================================================\\

procedure TfmComposeByHorizons.DivideByPlanes(TableInput, TableOutput: TTable);

type
  TFieldValues = array of variant;
var
  PrevHole:      string;
  CurrHole:      string;
  X1, Y1, Z1, X2, Y2, Z2: real; //Sample contacts
  X_1, Y_1, Z_1, X_2, Y_2, Z_2: real; //Centers of Intervals
  DeltaX, DeltaY, DeltaZ: real;
  HorizonZ:      double;
  ValidHorizonZ: boolean;

  SampleLength, SumDistance: real; // The Distance from a cutting point

  FieldValues: TFieldValues;
  Horizons:    THorizonsDef;

  I: integer;
  T: double;

  {sub}
  procedure ReadValues;
  var
    I: integer;
  begin
    for I := 0 to TableOutput.FieldCount - 1 do
      try
        FieldValues[I] := Null;
        FieldValues[I] := TableOutput.Fields[I].AsVariant;
      except
      end;
  end;

  {sub}
  procedure SaveValues;
  var
    I: integer;
  begin
    for I := 0 to TableOutput.FieldCount - 1 do
      try
        TableOutput.Fields[I].AsVariant := FieldValues[I];
      except
      end;
  end;

  {sub}
  function ReadSample: boolean;
  begin
    CurrHole := TableOutput.FieldByName(fldDHOLE).AsString;
    X1 := TableOutput.FieldByName(fldX).AsFloat;
    Y1 := TableOutput.FieldByName(fldY).AsFloat;
    Z1 := TableOutput.FieldByName(fldZ).AsFloat;
    SampleLength := TableOutput.FieldByName(fldLENGTH).AsFloat;
    TableOutput.Next;
    Result := True;
    if CompareText(CurrHole, TableOutput.FieldByName(fldDHOLE).AsString) <> 0 then
      Result := ReadSample
    else if TableOutput.EOF then
      Result := False
    else
    begin
      X2 := TableOutput.FieldByName(fldX).AsFloat;
      Y2 := TableOutput.FieldByName(fldY).AsFloat;
      Z2 := TableOutput.FieldByName(fldZ).AsFloat;
    end;
  end;

begin
  if CompareText(TableInput.TableName, TableOutput.TableName) <> 0 then
  begin
    TableInput.Close;
    CopyFiles(ChangeFileExt(TableInput.TableName, '.*'), TableOutput.TableName,
      mtDholes, False);
  end;

  AddTableField(TableOutput.TableName, fldHORIZON, ftString, 16);
  TableOutput.Open;
  SetLength(FieldValues, TableOutput.FieldCount);
  try
    Horizons := THorizonsDef.Create;
    try
      for I := 1 to TableHorizons.RecordCount do
      begin
        TableHorizons.RecNo := I;
        Horizons.Add(TableHorizonsStart.AsFloat,
          TableHorizonsThickness.AsFloat,
          TableHorizonsCount.AsInteger);
      end;

      ProgressBar.Max      := TableOutput.RecordCount;
      ProgressBar.Position := 1;
      ProgressBar.Min      := 1;

      PrevHole := '';
      while ReadSample do
      begin
        ProgressBar.Position := TableOutput.RecNo;

        if CompareText(PrevHole, CurrHole) <> 0 then
        begin
          PrevHole      := CurrHole;
          ValidHorizonZ := Horizons.FindNextZ(Z1, HorizonZ);
        end;

        if (not ValidHorizonZ) then
          Continue;
        // for samples under bottom horizon we do nothing

        TableOutput.Prior;
        TableOutput.Edit;
        TableOutput.FieldByName(fldHORIZON).AsFloat :=
          RoundTo(HorizonZ, Precision);
        if Z2 = HorizonZ then
          ValidHorizonZ := Horizons.FindNextZ(Z2, HorizonZ)
        else if Z2 < HorizonZ then
        begin
          try
            T := Abs((Z1 - HorizonZ) / (Z1 - Z2));
          except
          end;
          TableOutput.FieldByName(fldLENGTH).AsFloat :=
            RoundTo(SampleLength * T, Precision);
          ReadValues;
        end;
        TableOutput.Post;
        TableOutput.Next;

        if z2 < HorizonZ then
        begin
          TableOutput.Insert;
          try
            SaveValues;
            TableOutput.FieldByName(fldLENGTH).AsFloat :=
              Abs(RoundTo(SampleLength * (1 - T), Precision));
            TableOutput.FieldByName(fldX).AsFloat      :=
              RoundTo(X1 + (X2 - X1) * T, Precision);
            TableOutput.FieldByName(fldY).AsFloat      :=
              RoundTo(Y1 + (Y2 - Y1) * T, Precision);
            TableOutput.FieldByName(fldZ).AsFloat      :=
              RoundTo(Z1 + (Z2 - Z1) * T, Precision);
            try
              TableOutput.FieldByName(fldDEPTH).AsFloat :=
                RoundTo(TableOutput.FieldByName(fldDEPTH).AsFloat +
                SampleLength * T, Precision);
            except
            end;
          except
          end;
          TableOutput.Post;
          ValidHorizonZ   := Horizons.FindNextZ(HorizonZ, HorizonZ);
          ProgressBar.Max := TableOutput.RecordCount;
        end;
      end;
      ProgressBar.Position := ProgressBar.Max;
      TableOutput.Close;
    finally
      Horizons.Free;
    end;
  finally
    FieldValues := nil;
  end;
end;

procedure TfmComposeByHorizons.TableHorizonsBeforePost(DataSet: TDataSet);
begin
  if (TableHorizonsCount.AsInteger = 0) or
    (Abs(TableHorizonsThickness.AsFloat) < 1E-2) then
    Abort;
end;

procedure TfmComposeByHorizons.TableHorizonsBeforeEdit(DataSet: TDataSet);
begin
  ButtonOK.Enabled := False;
end;

procedure TfmComposeByHorizons.TableHorizonsCalcFields(DataSet: TDataSet);
begin
  TableHorizonsTo.AsFloat := TableHorizonsStart.AsFloat +
    TableHorizonsThickness.AsFloat * TableHorizonsCount.AsInteger;
end;

procedure TfmComposeByHorizons.TableHorizonsAfterPost(DataSet: TDataSet);
begin
  ButtonOK.Enabled := True;
end;

procedure TfmComposeByHorizons.ButtonOKClick(Sender: TObject);
begin
  inherited;
  with dmBase do
  begin
    if (TableInput.TableName <> TableOutput.TableName) then
      CopyFiles(TableInput.TableName + TableExt,
        TableOutput.TableName + TableExt, OutModelType, False);
    if ModalResult <> mrNone then
      DivideByPlanes(TableInput, TableOutput)
    else
      Exit;
  end;
end;

end.
