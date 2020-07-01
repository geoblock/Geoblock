//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The Dialog to calculate coordinates of sample centers in drillholes}

unit fComposeCenters;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.IniFiles,
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
  //DB
  Bde.DBTables,
  Data.DB,

  fMethodDialog,
  GBEditValue,
  dBase;

type
  TfmComposeCenters = class(TfmMethodDialog)
    GroupBoxMode: TGroupBox;
    RadioButtonAveraging: TRadioButton;
    RadioButtonUndercutting: TRadioButton;
    GroupBoxParameters: TGroupBox;
    LabelOrigin: TLabel;
    LabelThickness: TLabel;
    LabelNumber: TLabel;
    RadioGroupInterval: TRadioGroup;
    LabelZ:      TLabel;
    CheckBoxAll: TCheckBox;
    GBEditValueOrigin: TGBEditValue;
    GBEditValueThickness: TGBEditValue;
    GBEditValueNumber: TGBEditValue;
    procedure ButtonOKClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure RadioGroupIntervalClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBoxAllClick(Sender: TObject);
  private
    // Private declarations
    procedure UpdateLabels;
    function CopyFieldsValue(Source, Dest: TBDEDataSet): boolean;
    procedure ExecuteCalculations;

  public
     
    /// The procedure is calculating sample centers in drill holes or strings
    procedure CalculateSampleCenters(TableHoles, TablePoints3D: TTable);
    procedure AveragingSamples(TableHoles, TablePoints3D: TTable);
    procedure UndercuttingSamples(TableHoles, TablePoints3D: TTable);
    procedure ReadIniFile;
    procedure WriteIniFile;
  end;

var
  fmComposeCenters: TfmComposeCenters;

implementation

uses
  uGlobals,
  uProfuns,
  uCommon;

{$R *.DFM}

{ TfmComposeCentres }

function TfmComposeCenters.CopyFieldsValue(Source, Dest: TBDEDataSet): boolean;
var
  I:     integer;
  Field: TField;
begin
  Result := True;
  for I := 4 to Source.FieldCount - 1 do //Skip ID, X, Y, Z
  begin
    try
      with Source.Fields[I] do
      begin
        Field := Dest.FindField(FieldName);
        if Field <> nil then
          Field.Value := Value;
      end;
    except
      Result := False;
    end;
  end;
end;

procedure TfmComposeCenters.UpdateLabels;
var
  Enabled: boolean;
begin
  Enabled := RadioGroupInterval.ItemIndex <> 0;

  RadioButtonUndercutting.Enabled := Enabled;
  RadioButtonAveraging.Enabled := Enabled;
  LabelOrigin.Enabled := Enabled;
  LabelThickness.Enabled := Enabled;
  LabelNumber.Enabled := Enabled;
  LabelZ.Enabled := Enabled;
  GBEditValueOrigin.Enabled := Enabled;
  GBEditValueThickness.Enabled := Enabled;
  GBEditValueNumber.Enabled := Enabled;

  if RadioGroupInterval.ItemIndex = 2 then
  begin
    LabelOrigin.Enabled := False;
    LabelZ.Enabled      := False;
    GBEditValueOrigin.Enabled := False;
    LabelNumber.Enabled := False;
    GBEditValueNumber.Enabled := False;
  end;
end;

procedure TfmComposeCenters.AveragingSamples(TableHoles, TablePoints3D: TTable);
type
  PRealVals = ^TRealVals;
  TRealVals = array[1..64] of Single;

  PIntegerVals = ^TIntegerVals;
  TIntegerVals = array[1..64] of integer;

var
  Drillhole: variant;
  X1, Y1, Z1, X2, Y2, Z2: Single; //Sample contacts
  X_1, Y_1, Z_1, X_2, Y_2, Z_2: Single; //Centers of Intervals
  EndOfHole: boolean;
  I, J, Code, CutCounts: integer;
  SampleLength, SumInterval: Single; // The Distance from a cutting point
  DeltaX, DeltaY, DeltaZ: Single;

  RealVals: PRealVals;
  IntegerVals: PIntegerVals;
  Nums: PIntegerVals;
  K:    integer;

begin
  New(RealVals);
  New(IntegerVals);
  New(Nums);
  TableHoles.Open;
  TablePoints3D.FieldDefs.Assign(TableHoles.FieldDefs);
  TablePoints3D.CreateTable;
  TablePoints3D.Open;

  ProgressBar.Min := 1;
  ProgressBar.Max := TableHoles.RecordCount;
  ProgressBar.Position := 1;
  K := 0;
  while not TableHoles.EOF do
  begin
    ProgressBar.Position := TableHoles.RecNo;
    Drillhole := TableHoles.FieldByName(fldDHOLE).AsString;

    X2  := TableHoles.FieldByName(fldX).AsFloat;
    Y2  := TableHoles.FieldByName(fldY).AsFloat;
    Z2  := TableHoles.FieldByName(fldZ).AsFloat;
    X_2 := X2;
    Y_2 := Y2;
    Z_2 := Z2;

    SumInterval := 0;

    TableHoles.Next;
    EndOfHole := TableHoles.EOF or
      (Drillhole <> TableHoles.FieldByName(fldDHOLE).AsString);

    while not EndOfHole do
    begin
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;

      X2 := TableHoles.FieldByName(fldX).AsFloat;
      Y2 := TableHoles.FieldByName(fldY).AsFloat;
      Z2 := TableHoles.FieldByName(fldZ).AsFloat;

      SampleLength := Norm([X2 - X1, Y2 - Y1, Z2 - Z1]);
      if RadioGroupInterval.ItemIndex = 1 then
        SumInterval := SumInterval + GBEditValueThickness.AsDouble //(Z2 - Z1)
      else
        SumInterval := SumInterval + SampleLength;

      if SumInterval <= GBEditValueThickness.AsDouble then
      begin // Accumulate values inside the SumInterval
        if SumInterval <= SampleLength then
        begin
          for J := 4 to TablePoints3D.FieldCount - 1 do
          begin
            Inc(Nums^[J]);
            if TablePoints3D.Fields[J].DataType = ftFloat then
              RealVals^[J] := TablePoints3D.Fields[J].AsFloat;
            if TablePoints3D.Fields[J].DataType = ftInteger then
              IntegerVals^[J] := TablePoints3D.Fields[J].AsInteger;
          end;
        end;
      end
      else // Store all intervals with the sample attributes
      begin
        CutCounts := Trunc(SumInterval / GBEditValueThickness.AsDouble);
        DeltaX    := 1;
        for I := 1 to CutCounts do
        begin
          //Calculate coordinates of the Cutpoint
          X_1 := X1 + I * DeltaX;
          TablePoints3D.Append;
          CopyFieldsValue(TableHoles, TablePoints3D);

          Inc(K);
          TablePoints3D.FieldByName(fldID).AsInteger := K;
          TablePoints3D.FieldByName(fldX).AsFloat := RoundTo((X_1 + X_2) / 2, Precision);
          TablePoints3D.FieldByName(fldY).AsFloat := RoundTo((Y_1 + Y_2) / 2, Precision);
          TablePoints3D.FieldByName(fldZ).AsFloat := RoundTo((Z_1 + Z_2) / 2, Precision);
          TablePoints3D.Post;
        end;

        SumInterval := 0;
      end;
      TableHoles.Next;
      EndOfHole := TableHoles.EOF or (Drillhole <>
        TableHoles.FieldByName(fldDHOLE).AsString);
    end;
  end;
  ProgressBar.Position := ProgressBar.Max;
  TablePoints3D.Close;
  TableHoles.Close;
  Dispose(RealVals);
  Dispose(IntegerVals);
  Dispose(Nums);
end;

procedure TfmComposeCenters.UndercuttingSamples(TableHoles, TablePoints3D: TTable);

type
  PRealVals = ^TRealVals;
  TRealVals = array[1..64] of Single;

  PIntegerVals = ^TIntegerVals;
  TIntegerVals = array[1..64] of integer;

var
  Drillhole:     variant;
  X1, Y1, Z1, X2, Y2, Z2: real; //Sample contacts
  X_2, Y_2, Z_2: real; //Centers of Intervals
  EndOfHole:     boolean;
  CutCounts:     integer;
  SampleLength, SumInterval: Single; // Distance from a cutting point
  DeltaX, DeltaY, DeltaZ: Single;

  RealVals: PRealVals;
  IntegerVals: PIntegerVals;
  Nums: PIntegerVals;
  T:    double; // For parametric line equation
  K:    integer;

begin
  New(RealVals);
  New(IntegerVals);
  New(Nums);
  TableHoles.Open;
  if TablePoints3D.Exists then
  begin
    TablePoints3D.Close;
    TablePoints3D.DeleteTable;
  end;
  TablePoints3D.FieldDefs.Assign(TableHoles.FieldDefs);
  TablePoints3D.CreateTable;
  TablePoints3D.Open;

  ProgressBar.Min := 1;
  ProgressBar.Max := TableHoles.RecordCount;
  ProgressBar.Position := 1;
  K := 0;
  while not TableHoles.EOF do
  begin
    ProgressBar.Position := TableHoles.RecNo;
    Drillhole := TableHoles.FieldByName(fldDHOLE).AsString;

    X2 := TableHoles.FieldByName(fldX).AsFloat;
    Y2 := TableHoles.FieldByName(fldY).AsFloat;
    Z2 := TableHoles.FieldByName(fldZ).AsFloat;

    if RadioGroupInterval.ItemIndex = 1 then
      SumInterval :=
        trunc((Z2 - GBEditValueOrigin.AsDouble) / GBEditValueThickness.AsDouble) *
        GBEditValueThickness.AsDouble - (Z2 - GBEditValueOrigin.AsDouble)
    else
      SumInterval := 0;

    if SumInterval < 0 then
      SumInterval := SumInterval + GBEditValueThickness.AsDouble;
    TableHoles.Next;
    EndOfHole := TableHoles.EOF or
      (Drillhole <> TableHoles.FieldByName(fldDHOLE).AsString);

    while not EndOfHole do
    begin
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;

      X2 := TableHoles.FieldByName(fldX).AsFloat;
      Y2 := TableHoles.FieldByName(fldY).AsFloat;
      Z2 := TableHoles.FieldByName(fldZ).AsFloat;

      if RadioGroupInterval.ItemIndex = 1 then
        SampleLength := Abs(Z2 - Z1)
      else
        SampleLength := Norm([X2 - X1, Y2 - Y1, Z2 - Z1]);
      //  SampleLength := SQRT(SQR(X2-X1)+SQR(Y2-Y1)+SQR(Z2-Z1));
      if RadioGroupInterval.ItemIndex = 1 then
        SumInterval := SumInterval + Abs(Z2 - Z1)
      else
        SumInterval := SumInterval + SampleLength;

      while SumInterval >= GBEditValueThickness.AsDouble do
      begin
        T := (SampleLength - (SumInterval - GBEditValueThickness.AsDouble)) /
          SampleLength;

        TablePoints3D.Append;
        CopyFieldsValue(TableHoles, TablePoints3D);

        Inc(K);
        TablePoints3D.FieldByName(fldID).AsInteger := K;
        TablePoints3D.FieldByName(fldX).AsFloat := RoundTo(X1 + (X2 - X1) * T, Precision);
        TablePoints3D.FieldByName(fldY).AsFloat := RoundTo(Y1 + (Y2 - Y1) * T, Precision);
        TablePoints3D.FieldByName(fldZ).AsFloat := RoundTo(Z1 + (Z2 - Z1) * T, Precision);
        TablePoints3D.Post;
        SumInterval := SumInterval - GBEditValueThickness.AsDouble;
      end;

      TableHoles.Next;
      EndOfHole := TableHoles.EOF or (Drillhole <>
        TableHoles.FieldByName(fldDHOLE).AsString);
    end;
  end;
  ProgressBar.Position := ProgressBar.Max;
  TablePoints3D.Close;
  TableHoles.Close;
  Dispose(RealVals);
  Dispose(IntegerVals);
  Dispose(Nums);
end;

procedure TfmComposeCenters.CalculateSampleCenters(TableHoles,
  TablePoints3D: TTable);
var
  ID: integer;
  Drillhole: variant;
  X1, Y1, Z1, X2, Y2, Z2: Single;
  EndOfHole: boolean;

begin
  TableHoles.Open;
  TablePoints3D.FieldDefs.Assign(TableHoles.FieldDefs);
  TablePoints3D.TableName := OutModelName;
  TablePoints3D.CreateTable;
  TablePoints3D.Open;

  ProgressBar.Min      := 1;
  ProgressBar.Max      := TableHoles.RecordCount;
  ProgressBar.Position := 1;
  //read the first hole;
  // while (end of table)
  //   goto start of current hole;
  //   read point2;
  //   goto next record;
  //   while (end of hole)
  //     point1:=point2;
  //     read point2;
  //     save (point1+point2/2);
  //   end while
  //   next drillhole;
  // end while
  ID := 0;
  while not TableHoles.EOF do
  begin
    ProgressBar.Position := TableHoles.RecNo;
    Drillhole := TableHoles.FieldByName(fldDHOLE).AsString;

    X2 := TableHoles.FieldByName(fldX).AsFloat;
    Y2 := TableHoles.FieldByName(fldY).AsFloat;
    Z2 := TableHoles.FieldByName(fldZ).AsFloat;

    TableHoles.Next;

    EndOfHole := TableHoles.EOF or
      (Drillhole <> TableHoles.FieldByName(fldDHOLE).AsString);

    while not EndOfHole do
    begin
      Inc(ID);
      X1 := X2;
      Y1 := Y2;
      Z1 := Z2;

      X2 := TableHoles.FieldByName(fldX).AsFloat;
      Y2 := TableHoles.FieldByName(fldY).AsFloat;
      Z2 := TableHoles.FieldByName(fldZ).AsFloat;

      TablePoints3D.Append;
      CopyFieldsValue(TableHoles, TablePoints3D);
      TablePoints3D.FieldByName(fldID).AsInteger := ID;
      TablePoints3D.FieldByName(fldX).AsFloat := RoundTo((X1 + X2) / 2, Precision);
      TablePoints3D.FieldByName(fldY).AsFloat := RoundTo((Y1 + Y2) / 2, Precision);
      TablePoints3D.FieldByName(fldZ).AsFloat := RoundTo((Z1 + Z2) / 2, Precision);
      TablePoints3D.Post;
      TableHoles.Next;
      EndOfHole := TableHoles.EOF or (Drillhole <>
        TableHoles.FieldByName(fldDHOLE).AsString);
    end;
  end;
  ProgressBar.Position := ProgressBar.Max;
  TablePoints3D.Close;
  TableHoles.Close;
end;

procedure TfmComposeCenters.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  UpdateLabels;
  PanelOutPath.Caption := ExpandPath(DirPoints3D);
  ListBoxInputNamesClick(Self);
end;

procedure TfmComposeCenters.RadioGroupIntervalClick(Sender: TObject);
begin
  UpdateLabels;
end;

procedure TfmComposeCenters.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  ListBoxRealAttributeClick(Self);
end;

procedure TfmComposeCenters.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ToolButtonHoles.Click;
end;

procedure TfmComposeCenters.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      RadioGroupInterval.ItemIndex := ReadInteger(Name, RadioGroupInterval.Caption, 0);
      GBEditValueOrigin.AsDouble     := ReadFloat(Name, LabelOrigin.Caption, 0);
      GBEditValueThickness.AsDouble  := ReadFloat(Name, LabelThickness.Caption, 15);
      GBEditValueNumber.AsDouble     := ReadFloat(Name, LabelNumber.Caption, 1);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeCenters.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteInteger(Name, RadioGroupInterval.Caption, RadioGroupInterval.ItemIndex);
      WriteFloat(Name, LabelOrigin.Caption, GBEditValueOrigin.AsDouble);
      WriteFloat(Name, LabelThickness.Caption, GBEditValueThickness.AsDouble);
      WriteFloat(Name, LabelNumber.Caption, GBEditValueNumber.AsDouble);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeCenters.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

procedure TfmComposeCenters.ExecuteCalculations;
begin
  with dmBase do
  begin
    if RadioGroupInterval.ItemIndex = 0 then
      CalculateSampleCenters(TableInput, TableOutput)
    else
    begin
      if RadioButtonAveraging.Checked then
        AveragingSamples(TableInput, TableOutput)
      else
        UndercuttingSamples(TableInput, TableOutput);
    end;
  end;
end;


procedure TfmComposeCenters.CheckBoxAllClick(Sender: TObject);
begin
  if CheckBoxAll.Checked then
  begin
    ListBoxRealAttribute.MultiSelect := True;
    ListBoxRealAttribute.SelectAll;
  end
  else
  begin
    ListBoxRealAttribute.MultiSelect := False;
    if ListBoxRealAttribute.Count > 0 then
      ListBoxRealAttribute.ItemIndex := 0
    else
      ListBoxRealAttribute.ItemIndex := -1;
  end;
end;


procedure TfmComposeCenters.ButtonOKClick(Sender: TObject);
begin
  inherited;
  OutModelType := mtPoints3D;
  if ModalResult <> mrNone then
    ExecuteCalculations
  else
    Exit;
end;


end.
