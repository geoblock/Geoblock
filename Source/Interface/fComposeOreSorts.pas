//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The Dialog to calculate ore intervals in drillholes}

unit fComposeOreSorts;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.Math,
  System.ImageList,
  Vcl.ImgList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms, 
  Vcl.Dialogs,
  Vcl.StdCtrls, 
  Vcl.Grids, 
  Vcl.DBGrids, 
  Vcl.ComCtrls,
  Vcl.ToolWin, 
  Vcl.Buttons, 
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  //DB
  Bde.DBTables, 
  Data.DB,

  
  fMethodDialog,
  dDialogs,
  dBase,
  GBEditValue;

type
  TIntervalRec = record
    Lo, Hi: double;
    Sort:   integer;
  end;

  TOreSortsParams = record
    Initialized:     boolean;
    TableOutputName: string;
    AttributeName:   string;
    CutoffGrade:     array of TIntervalRec;
    OutputFieldName: string;
    InInterval:      boolean;
    IntervalFieldName: string;
    MinThickness:    double;
  end;

type
  TfmComposeOreSorts = class(TfmMethodDialog)
    GroupBoxByIntervals: TGroupBox;
    CheckBoxByIntervals: TCheckBox;
    ListBoxIntegerAttributes: TListBox;
    LabelIndicator: TLabel;
    GroupBox: TGroupBox;
    StringGridClasses: TStringGrid;
    HeaderControlGrid: THeaderControl;
    LabelMinThickness: TLabel;
    Panel1:   TPanel;
    LabelNumberOfClasses: TLabel;
    EditOutputFieldName: TEdit;
    SpinEditClasses: TSpinEdit;
    LabelOutputField: TLabel;
    GBEditValueMinThickness: TGBEditValue;
    procedure SpinEditClassesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure CheckBoxByIntervalsClick(Sender: TObject);
    procedure ToolButtonInputClick(Sender: TObject);
    procedure ListBoxInputNamesClick(Sender: TObject);
    procedure ListBoxRealAttributeClick(Sender: TObject);
  private
     
    FInsideInterval: boolean;
    FisOnEditOutputFieldNameChange: boolean;
    procedure UpdateIntegerAttributes;
    procedure EnableLabels(const Value: boolean);
    procedure SortSamples;
    procedure SortInsideInterval;
    function GetParams: TOreSortsParams;
    procedure SetParams(const Value: TOreSortsParams);
    function GetAttributeName: string;
    procedure SetAttributeName(const Value: string);
    function GetCutoffGrade(Index: integer): TIntervalRec;
    procedure SetCutoffGrade(Index: integer; const Value: TIntervalRec);
    function GetIntervalFieldName: string;
    procedure SetIntervalFieldName(const Value: string);
    procedure SetInsideInterval(const Value: boolean);
    procedure SetMinThickness(const Value: double);
    procedure SetOutputFieldName(const Value: string);
    function GetMinThickness: double;
    function GetInsideInterval: boolean;
    function GetOutputFieldName: string;
    property isOnEditOutputFieldNameChange: boolean
      read FisOnEditOutputFieldNameChange write FisOnEditOutputFieldNameChange;
  public
    NSorts:  integer;
    OldHigh: double;
  public
     
    property CutoffGrade[index: integer]: TIntervalRec read GetCutoffGrade write SetCutoffGrade;
    property OutputFieldName: string read GetOutputFieldName write SetOutputFieldName;
    property InsideInterval: boolean read GetInsideInterval write SetInsideInterval;
    property IntervalFieldName: string read GetIntervalFieldName
      Write SetIntervalFieldName;
    property MinThickness: double read GetMinThickness write SetMinThickness;
    property Params: TOreSortsParams read GetParams write SetParams;
    procedure SetSortClasses;
  end;

var
  fmComposeOreSorts: TfmComposeOreSorts;
  PrevParams: TOreSortsParams;

implementation

uses
  uProfuns,
  uGlobals,
  uCommon,
  uFileCreator,
  fTableWindow;

{$R *.dfm}

type
  TInterval = class(TPersistent)
  private
    FLow, FHigh: double;
    FOreType:    integer;
    function GetHigh: double;
    function GetLow: double;
    procedure SetHigh(const Value: double);
    procedure SetLow(const Value: double);
  public
    constructor Create(ALow, AHigh: double; AOreType: integer);
  public
    procedure Assign(ALow, AHigh: double; AOreType: integer); reintroduce;
      overload;
    property Low: double read GetLow write SetLow;
    property High: double read GetHigh write SetHigh;
    property OreType: integer read FOreType write FOreType;
  end;

  TIntervals = class(TObject)
  private
    FList:      TList;
    FItemIndex: integer;
    FDefaultItemIndex: integer;
    function GetCount: integer;
    function GetCurrent: TInterval;
    function GetItemIndex: integer;
    function GetItems(Index: integer): TInterval;
    procedure SetItemIndex(const Value: integer);
    procedure SetItems(Index: integer; const Value: TInterval);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property DefaultItemIndex: integer read FDefaultItemIndex write FDefaultItemIndex;
    procedure Add(Low, High: double; OreType: integer);
    function IndexOf(AttributeValue: double): integer;
    procedure SetCurrent(AttributeValue: double);
    property Count: integer read GetCount;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Current: TInterval read GetCurrent;
    property Items[Index: integer]: TInterval read GetItems write SetItems;
  end;


procedure TfmComposeOreSorts.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;

  SetSortClasses;
  ToolButtonHoles.Click;

  dmBase.TableOutput.TableName := OutModelName;
  EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORESORT;

  PrevParams.CutoffGrade := nil; //destroy dynamic array
  {PrevParams := Params;}  //??
end;

procedure TfmComposeOreSorts.SetSortClasses;
var
  I, Sort: integer;
  Lo, Hi:  double;

begin
  // Instead of 100 here should be Max Value for active attribute
  for I := 0 to SpinEditClasses.Value - 1 do
  begin
    StringGridClasses.Cells[0, I] := IntToStr(I + 1);
    Lo := I * round(100 / SpinEditClasses.Value);
    StringGridClasses.Cells[1, I] := FloatToStr(Lo);
    Hi := (I + 1) * round(100 / SpinEditClasses.Value);
    StringGridClasses.Cells[2, I] := FloatToStr(Hi);
    StringGridClasses.Cells[3, I] := IntToStr(I);
  end;
end;


procedure TfmComposeOreSorts.ToolButtonInputClick(Sender: TObject);
begin
  inherited;
  OutModelType := mtDholes;
  PanelOutPath.Caption := ExpandPath(DirDholes);
  ListBoxInputNamesClick(Self);
end;

procedure TfmComposeOreSorts.ListBoxInputNamesClick(Sender: TObject);
begin
  inherited;
  UpdateIntegerAttributes;
end;


procedure TfmComposeOreSorts.EnableLabels(const Value: boolean);
begin
  LabelIndicator.Enabled    := Value;
  LabelMinThickness.Enabled := Value;
  ListBoxIntegerAttributes.Enabled := Value;
  GBEditValueMinThickness.Enabled := Value;
  case Value of
    True:
      if CompareText(OutputFieldName, fldORETYPE) = 0 then
        EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORESORT;
    False:
      if CompareText(OutputFieldName, fldORESORT) = 0 then
        EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORETYPE;
  end;
end;

//-----------------------------------------------------------------------------
procedure TfmComposeOreSorts.SetInsideInterval(const Value: boolean);
begin
  if InsideInterval <> Value then
  begin
    FInsideInterval := Value;
    CheckBoxByIntervals.Checked := Value;
    LabelIndicator.Enabled := Value;
    LabelMinThickness.Enabled := Value;
    ListBoxIntegerAttributes.Enabled := Value;
    GBEditValueMinThickness.Enabled := Value;
    case Value of
      True:
        if CompareText(OutputFieldName, fldORETYPE) = 0 then
          EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORESORT;
      False:
        if CompareText(OutputFieldName, fldORESORT) = 0 then
          EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORETYPE;
    end;
  end;
end;

function TfmComposeOreSorts.GetAttributeName: string;
begin
  try
    with ListBoxRealAttribute do
      Result := Items[ItemIndex];
  except
    Result := '';
  end;
end;

function TfmComposeOreSorts.GetCutoffGrade(Index: integer): TIntervalRec;
begin
  Result.Lo   := StrToFloat(StringGridClasses.Cells[1, 0]);
  Result.Hi   := StrToFloat(StringGridClasses.Cells[2, 0]);
  Result.Sort := StrToInt(StringGridClasses.Cells[3, 0]);
end;

function TfmComposeOreSorts.GetInsideInterval: boolean;
begin
  Result := FInsideInterval;
end;

function TfmComposeOreSorts.GetIntervalFieldName: string;
begin
  Result := ListBoxIntegerAttributes.Items[ListBoxIntegerAttributes.ItemIndex];
end;

function TfmComposeOreSorts.GetMinThickness: double;
begin
  Result := GBEditValueMinThickness.AsDouble;
end;

function TfmComposeOreSorts.GetOutputFieldName: string;
begin
  Result := EditOutputFieldName.Text;
end;

function TfmComposeOreSorts.GetParams: TOreSortsParams;
var
  I: integer;
begin
    Result.TableOutputName := OutModelName;
    Result.AttributeName   := ActiveAttribute;
    SetLength(Result.CutoffGrade, SpinEditClasses.Value);
    Result.OutputFieldName := OutputFieldName;
    Result.InInterval      := InsideInterval;
    Result.IntervalFieldName := IntervalFieldName;
    Result.MinThickness    := MinThickness;
    Result.Initialized     := True;
end;

procedure TfmComposeOreSorts.SetAttributeName(const Value: string);
var
  NewItemIndex: integer;
begin
  if CompareText(ActiveAttribute, Value) <> 0 then
  begin
    with ListBoxRealAttribute do
    begin
      NewItemIndex := Items.IndexOf(Value);
      if NewItemIndex >= 0 then
        ItemIndex := NewItemIndex;
    end;
  end;
end;

procedure TfmComposeOreSorts.UpdateIntegerAttributes;
var
  I: integer;
begin
  with dmBase do
  begin
    ListBoxIntegerAttributes.Items.Clear;
    try
      TableInput.Open;
      for I := 0 to TableInput.FieldCount - 1 do
      begin
        if IsIntegerAttribute(TableInput.Fields[I]) then
          ListBoxIntegerAttributes.Items.Add(TableInput.Fields[I].FieldName);
      end;
      TableInput.Close;
    except
      TableInput.Close;
    end;
    try
      CheckItemIndex(ListBoxIntegerAttributes);
    except
    end;
  end;
end;


procedure TfmComposeOreSorts.SetCutoffGrade(Index: integer;
  const Value: TIntervalRec);
begin
  StringGridClasses.Cells[1, 0] := ValToStr(Value.Lo);
  StringGridClasses.Cells[2, 0] := ValToStr(Value.Hi);
  StringGridClasses.Cells[3, 0] := ValToStr(Value.Sort);
end;

procedure TfmComposeOreSorts.SetIntervalFieldName(const Value: string);
var
  NewItemIndex: integer;
begin
  if CompareText(IntervalFieldName, Value) <> 0 then
    with ListBoxIntegerAttributes do
    begin
      NewItemIndex := Items.IndexOf(Value);
      if NewItemIndex >= 0 then
        ItemIndex := NewItemIndex;
    end;
end;

procedure TfmComposeOreSorts.SetMinThickness(const Value: double);
begin
  if MinThickness <> Value then
    GBEditValueMinThickness.AsDouble := Value;
end;

procedure TfmComposeOreSorts.SetOutputFieldName(const Value: string);
begin
  if EditOutputFieldName.Text <> Value then
    EditOutputFieldName.Text := Value;
end;

procedure TfmComposeOreSorts.SetParams(const Value: TOreSortsParams);
var
  I: integer;
begin
  with dmBase do
  begin
    TableOutput.TableName := Value.TableOutputName;
    ActiveAttribute := Value.AttributeName;
    try
      while TableTemp.RecordCount > Length(Value.CutoffGrade) do
        TableTemp.Delete;
    except
    end;
    for I := 0 to High(Value.CutoffGrade) do
    begin
      CutoffGrade[I + 1] := Value.CutoffGrade[I];
    end;
    InsideInterval    := Value.InInterval; // don`t change order of value assign
    IntervalFieldName := Value.IntervalFieldName;
    OutputFieldName   := Value.OutputFieldName;
    MinThickness      := Value.MinThickness;
  end;
end;

procedure TfmComposeOreSorts.SortInsideInterval;
type
  TSample = record
    OreSort: integer;
    Length:  double;
  end;
  TInterval = array of TSample;

var
  fldOreType: string;
  fldOreSort: string;
  Attribute:  string;
  CurrHole:   string;

  IntervalStart, IntervalEnd: integer;
  IntervalLength: double; // Sum of Sample length
  IntervalValue: double;
  ValueOreType: double;
  AttributeOreSort: integer;
  I: integer;
  Intervals: TIntervals;
  Interval: TInterval;
  SampleCount: integer; // In Interval

  {sub}
  function ReadInterval: boolean;
  begin
    with dmBase do
    begin
      try
        while not (TableOutput.EOF) and // skip waste
          (TableOutput.FieldByName(fldOreType).AsFloat = 0) do
          TableOutput.Next;
        if TableOutput.EOF then // no intervals
        begin
          Result := False;
          Exit;
        end;
      except
        Result := False;
        Exit;
      end;
      // reading interval
      CurrHole     := TableOutput.FieldByName(fldDHOLE).AsString;
      ValueOreType := TableOutput.FieldByName(fldOreType).AsFloat;
      Intervals.SetCurrent(TableOutput.FieldByName(Attribute).AsFloat);
      IntervalLength := 0;
      IntervalValue  := 0;
      SampleCount    := 0;
      IntervalStart  := TableOutput.RecNo;
      while not (TableOutput.EOF) and
        (TableOutput.FieldByName(fldOreType).AsFloat = ValueOreType) and
        (TableOutput.FieldByName(fldDHOLE).AsString = CurrHole) do
      begin
        Intervals.SetCurrent(TableOutput.FieldByName(Attribute).AsFloat);
        AttributeOreSort := Intervals.Current.OreType;
        Inc(SampleCount);
        if High(Interval) < SampleCount then
          SetLength(Interval, SampleCount + 10);
        Interval[SampleCount - 1].OreSort := AttributeOreSort;
        Interval[SampleCount - 1].Length :=
          TableOutput.FieldByName(fldLENGTH).AsFloat;
        IntervalValue  := IntervalValue + TableOutput.FieldByName(
          Attribute).AsFloat * TableOutput.FieldByName(fldLENGTH).AsFloat;
        IntervalLength := IntervalLength +
          TableOutput.FieldByName(fldLENGTH).AsFloat;
        TableOutput.Edit;
        TableOutput.FieldByName(fldOreSort).AsInteger := AttributeOreSort;
        TableOutput.Post;
        TableOutput.Next;
      end;
      try
        IntervalValue := IntervalValue / IntervalLength;
      except
      end;
      if not TableOutput.EOF then
        TableOutput.Prior;
      IntervalEnd := TableOutput.RecNo;
      Result      := True;
    end;
  end;

  {sub}
  function OptimizeInterval: boolean;
  var
    I, J:      integer;
    SubIntervals: TInterval;
    MaxLength: double;
    MinLength: double;
    CurrentOreSort: integer;

    {sub}
    function SumSample(AOreSort: integer; ALength: double): boolean;
    var
      J: integer;
    begin
      J := 0;
      while J <= High(SubIntervals) do
      begin
        if SubIntervals[J].OreSort = AOreSort then
        begin
          SubIntervals[J].Length := SubIntervals[J].Length + ALength;
          Break;
        end
        else
          Inc(J);
      end;
      if J > High(SubIntervals) then
      begin
        SetLength(SubIntervals, High(SubIntervals) + 2);
        with SubIntervals[High(SubIntervals)] do
        begin
          OreSort := AOreSort;
          Length  := ALength;
        end;
      end;
      Result := True;
    end;

    {sub}
    function AddSample(AOreSort: integer; ALength: double): boolean;
    var
      I: integer;
    begin
      I := High(SubIntervals);
      if (I < 0) or (SubIntervals[I].OreSort <> AOreSort) then
      begin //New sort
        SetLength(SubIntervals, Length(SubIntervals) + 1);
        I := High(SubIntervals);
        SubIntervals[I].OreSort := AOreSort;
        SubIntervals[I].Length := ALength;
      end
      else // The same sort
        SubIntervals[I].Length := SubIntervals[I].Length + ALength;
      Result := True;
    end;

  begin
    for I := 0 to SampleCount - 1 do
      SumSample(Interval[I].OreSort, Interval[I].Length);
    if High(SubIntervals) = 0 then
      Result := False //simple interval without division
    else
    begin
      { MaxLength:=SubIntervals[0].Length;
        CurrentOreSort:=SubIntervals[0].OreSort;
        for I:=1 to High(SubIntervals) do
        if MaxLength<SubIntervals[I].Length then
        begin
          MaxLength:=SubIntervals[I].Length;
          CurrentOreSort:=SubIntervals[I].OreSort;
        end;}
      if IntervalLength < GBEditValueMinThickness.AsDouble * 2 then
        //only one SubInterval possible
      begin
        Intervals.SetCurrent(IntervalValue);
        CurrentOreSort := Intervals.Current.OreType;
        for I := 0 to SampleCount - 1 do
          Interval[I].OreSort := CurrentOreSort;
      end
      else
      begin
        repeat
          SubIntervals := nil;
          for I := 0 to SampleCount - 1 do //calculate SubIntervals
            AddSample(Interval[I].OreSort, Interval[I].Length);
          J := 0; // find Intervals with minimum length
          MinLength := SubIntervals[0].Length;
          for I := 1 to High(SubIntervals) do
            if MinLength > SubIntervals[I].Length then
            begin
              MinLength := SubIntervals[I].Length;
              J := I;
            end;
          if (MinLength < GBEditValueMinThickness.AsDouble) and
            (Length(SubIntervals) > 1) then // Absorb some sorts
          begin
            I := 0;
            CurrentOreSort := Interval[0].OreSort;
            if J = 0 then
            begin
              while (I < SampleCount) and (CurrentOreSort = Interval[I].OreSort)
                do
              begin
                Inc(I);
              end;
              CurrentOreSort := Interval[I].OreSort;
              while I > 0 do
              begin
                Dec(I);
                Interval[I].OreSort := CurrentOreSort;
              end;
            end
            else
            begin
              while (I < SampleCount) and (J > 0) do
              begin
                if CurrentOreSort <> Interval[I].OreSort then
                begin
                  Dec(J);
                  if J > 0 then
                  begin
                    CurrentOreSort := Interval[I].OreSort;
                    Inc(I);
                  end;
                end
                else
                  Inc(I);
              end;
              while (I < SampleCount) and (CurrentOreSort <> Interval[I].OreSort)
                do
              begin
                Interval[I].OreSort := CurrentOreSort;
                Inc(I);
              end;
            end;
          end;
        until (MinLength >= GBEditValueMinThickness.AsDouble) or
          (High(SubIntervals) = 0);
      end;
      Result := True;
    end;
    SubIntervals := nil;
  end;

  {sub}
  function WriteInterval: boolean;
  var
    I: integer;
    J: integer;
  begin
    with dmBase do
    begin
      J := 0;
      for I := IntervalStart to IntervalEnd do
      begin
        TableOutput.RecNo := I;
        TableOutput.Edit;
        TableOutput.FieldByName(fldOreSort).AsInteger := Interval[J].OreSort;
        TableOutput.Post;
        Inc(J);
      end;
      Result := True;
    end;
  end;

begin
  with dmBase do
  begin
    TableOutput.Open;
    ListBoxRealAttribute.ItemIndex :=
      Max(Min(0, ListBoxRealAttribute.Items.Count - 1),
      ListBoxRealAttribute.ItemIndex);
    if ListBoxRealAttribute.ItemIndex < 0 then
    begin
      ModalResult      := mrNone;
      ButtonOK.Enabled := False;
      Exit;
    end;

    Attribute  := ListBoxRealAttribute.Items[ListBoxRealAttribute.ItemIndex];
    fldOreSort := EditOutputFieldName.Text;
    fldOreType :=
      ListBoxIntegerAttributes.Items[ListBoxIntegerAttributes.ItemIndex];
    if TableOutput.FindField(fldOreSort) = nil then
    begin
      TableOutput.Close;
      AddTableField(TableOutput.TableName, fldOreSort, ftInteger);
    end;
    TableOutput.Open;
    ProgressBar.Min      := 1;
    ProgressBar.Position := 1;
    ProgressBar.Max      := TableOutput.RecordCount;
    TableOutput.First;

    TableOutput.Open;
    TableOutput.First;
    while not TableOutput.EOF do
    begin
      if not ReadInterval then
        Break;
      if OptimizeInterval then
        WriteInterval;
      TableOutput.Next;
      ProgressBar.Position := TableOutput.RecNo;
    end;
    TableOutput.Close;
    TableTemp.Close;
  end;
end;


{ TIntervals }

procedure TIntervals.Add(Low, High: double; OreType: integer);
begin
  FList.Add(TInterval.Create(Low, High, OreType));
end;

constructor TIntervals.Create;
begin
  inherited;
  FList := TList.Create;
  DefaultItemIndex := -1;
end;

destructor TIntervals.Destroy;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FList.Free;
  inherited;
end;

function TIntervals.GetCount: integer;
begin
  Result := FList.Count;
end;

function TIntervals.GetCurrent: TInterval;
begin
  if ItemIndex >= 0 then
    Result := Items[ItemIndex]
  else
    Result := nil;
end;

function TIntervals.GetItemIndex: integer;
begin
  FItemIndex := Min(FItemIndex, Count - 1);
  if FItemIndex >= 0 then
    Result := FItemIndex
  else
    Result := Min(FDefaultItemIndex, Count - 1);
end;

function TIntervals.GetItems(Index: integer): TInterval;
begin
  Result := TInterval(FList.Items[Index]);
end;

function TIntervals.IndexOf(AttributeValue: double): integer;
begin
  Result := 0;
  while (Result < Count) and ((AttributeValue < Items[Result].Low) or
      (AttributeValue >= Items[Result].High)) do
    Inc(Result);
  if Result = Count then
    Result := -1;
end;

procedure TIntervals.SetCurrent(AttributeValue: double);
begin
  ItemIndex := IndexOf(AttributeValue);
end;

procedure TIntervals.SetItemIndex(const Value: integer);
begin
  if Min(Value, Count - 1) <> FItemIndex then
  begin
    FItemIndex := Min(Value, Count - 1);
  end;
end;

procedure TIntervals.SetItems(Index: integer; const Value: TInterval);
begin
  if FList.IndexOf(Value) < 0 then
    if (Index < Count) and (Index >= 0) then
    begin
      Items[Index].Free;
      FList[Index] := Value;
    end
    else
      FList.Add(Value);
end;

{ TInterval }

procedure TInterval.Assign(ALow, AHigh: double; AOreType: integer);
begin

end;

constructor TInterval.Create(ALow, AHigh: double; AOreType: integer);
begin

end;

function TInterval.GetHigh: double;
begin
  Result := Max(FLow, FHigh);
end;

function TInterval.GetLow: double;
begin
  Result := Min(FLow, FHigh);
end;

procedure TInterval.SetHigh(const Value: double);
begin
  if FHigh > FLow then
    FHigh := Value
  else
    FLow  := Value;
end;

procedure TInterval.SetLow(const Value: double);
begin
  if FHigh < FLow then
    FHigh := Value
  else
    FLow  := Value;
end;

procedure TfmComposeOreSorts.CheckBoxByIntervalsClick(Sender: TObject);
begin
  EnableLabels(CheckBoxByIntervals.Checked);
end;

procedure TfmComposeOreSorts.SpinEditClassesChange(Sender: TObject);
begin
  inherited;
  StringGridClasses.RowCount := SpinEditClasses.Value;
  SetSortClasses;
end;

procedure TfmComposeOreSorts.ListBoxRealAttributeClick(Sender: TObject);
begin
  inherited;
  EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORESORT;
end;

procedure TfmComposeOreSorts.SortSamples;

var
  Grade:  double;
  Lo, Hi: double;
  Sort:   integer;
  OreTypeFieldName: string;
  I, J:   integer;

begin
  with dmBase do
  begin
    TableOutput.Open;
    ListBoxRealAttribute.ItemIndex :=
      Max(Min(0, ListBoxRealAttribute.Items.Count - 1),
      ListBoxRealAttribute.ItemIndex);
    if ListBoxRealAttribute.ItemIndex < 0 then
    begin
      ModalResult      := mrNone;
      ButtonOK.Enabled := False;
      Exit;
    end;
    ActiveAttribute := ListBoxRealAttribute.Items[ListBoxRealAttribute.ItemIndex];

    OreTypeFieldName := EditOutputFieldName.Text;
    if TableOutput.FindField(OreTypeFieldName) = nil then
    begin
      TableOutput.Close;
      AddTableField(TableOutput.TableName, OreTypeFieldName, ftInteger);
      TableOutput.Open;
    end;
    ProgressBar.Min      := 1;
    ProgressBar.Position := 1;
    ProgressBar.Max      := TableOutput.RecordCount;
    TableOutput.First;

    for I := 0 to TableOutput.RecordCount - 1 do
    begin
      ProgressBar.Position := I;
      if not TableOutput.FieldByName(ActiveAttribute).IsNull then
      begin
        Grade := TableOutput.FieldByName(ActiveAttribute).AsFloat;
        TableOutput.Edit;
        for J := 0 to StringGridClasses.RowCount - 1 do
        begin
          Lo   := StrToVal(StringGridClasses.Cells[1, J]);
          Hi   := StrToVal(StringGridClasses.Cells[2, J]);
          Sort := StrToInt(StringGridClasses.Cells[3, J]);
          if (Grade >= Lo) and (Grade < Hi) then
          begin
            TableOutput.FieldByName(OreTypeFieldName).AsInteger := Sort;
            Break;
          end;
        end;
        TableOutput.Post;
      end;
      TableOutput.Next;
    end;
    TableOutput.Close;
  end;
end;


procedure TfmComposeOreSorts.ButtonOKClick(Sender: TObject);
begin
  inherited;
  begin
    if (InModelName <> OutModelName) then
      CopyFiles(InModelName + TableExt,
        OutModelName + TableExt, OutModelType, False);

    if ModalResult <> mrNone then
    begin
      if CheckBoxByIntervals.Checked then
        SortInsideInterval
      else
        SortSamples;
    end
    else
      Exit;
  end;
end;

end.
