//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The Dialog to calculate ore intervals in drillholes }

unit fComposeOreIntervals;

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
  dBase,
  GBEditValue;

type
  TfmComposeOreIntervals = class(TfmMethodDialog)
    GroupBoxConditions: TGroupBox;
    EditOutputFieldName: TEdit;
    LabelMinOreInterval: TLabel;
    LabelMaxWasteInterval: TLabel;
    LabelCutoffGrade: TLabel;
    LabelOutputField: TLabel;
    evMinOreInterval: TGBEditValue;
    evMaxWasteInterval: TGBEditValue;
    evCutOffGrade: TGBEditValue;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxRealAttributeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
     
    procedure DefineOreIntervals(TableHoles: TTable; FieldNameComponent: string;
      MinOreInteval, MaxWasteInterval: double; CutOff: double; FieldNameOre: string);
    procedure ReadIniFile;
    procedure WriteIniFile;

  end;

var
  fmComposeOreIntervals: TfmComposeOreIntervals;

//=============================================================
implementation
//=============================================================

uses
  cGlobals,
  uCommon,
  dDialogs,
  uFileCreator,
  cProfuns,
  cResStrings;

{$R *.DFM}

procedure TfmComposeOreIntervals.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  ToolButtonHoles.Click;
  EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORETYPE;
end;


 //--------------------------------------------------------
 // Description: calculate ore intervals for drillholes
 // Input params:
 //   TableHoles - a table with sample contact coordinates
 //                from the DHOLE directory
 // Result:
 //   TableDholeOut - a table with new ore intervals
 //--------------------------------------------------------

procedure TfmComposeOreIntervals.DefineOreIntervals(TableHoles: TTable;
  FieldNameComponent: string; MinOreInteval, MaxWasteInterval: double;
  CutOff: double; FieldNameOre: string);

const
  ReservPrefix = 'Q_';

type
  TDrillHoleArrayItem = record
    RecNo:   integer;
    SampleLength: double;
    Metall:  double;
    OreType: byte;
  end;

var
  DrillHoleArray: array of TDrillHoleArrayItem;
  Drillhole:      string;
  IsLengthExists: boolean;
  //  Moisture : Double;

  SampleCount: integer;

  {Sub}
  function BODH: boolean;
  begin
    Result := DrillHole <> TableHoles.FieldByName(fldDHOLE).AsString;
  end;

  {Sub}
  function EOF: boolean;
  begin
    Result := TableHoles.EOF;
  end;

  {Sub}
  function EODH: boolean;
  begin
    Result := (DrillHole <> TableHoles.FieldByName(fldDHOLE).AsString);
    Result := Result or EOF;
  end;

  {Sub}
  function GetSampleLength: double;
  begin
    if TableHoles.FieldByName(fldLENGTH).IsNull then
      Result := 0
    else
      Result := TableHoles.FieldByName(fldLENGTH).AsFloat;
  end;

  {Sub}
  procedure ReadDHole;
  begin
    Drillhole   := TableHoles.FieldByName(fldDHOLE).AsString;
    SampleCount := 0;
    repeat
      Inc(SampleCount);
      SetLength(DrillHoleArray, SampleCount);
      with DrillHoleArray[SampleCount - 1] do
      begin
        RecNo  := TableHoles.RecNo;
        SampleLength := GetSampleLength;
        Metall := TableHoles.FieldByName(FieldNameComponent).AsFloat;
        if Metall < CutOff then
          OreType := 0
        else
          OreType := 1;
      end;
      TableHoles.Next;
    until EODH;
  end;

  {Sub}
  procedure GetIntervals;

    function ReadInterval(A: array of TDrillHoleArrayItem;
    var First, Last: integer): boolean;
    var
      OreType: byte;
    begin
      Result := First <= High(A);
      if not Result then
        Exit;
      OreType := A[First].OreType;
      Last    := First + 1;
      while ((Last <= High(A)) and (A[Last].OreType = OreType)) do
        Inc(Last);
    end;

    {CheckMetroprocent return True if a meterpercent of interval more
    then mininum of метропроцента, otherwise return False and обнуляет OreType}
    {Sub}
    function CheckMetroprocent(var A: array of TDrillHoleArrayItem;
      First, Last: integer; AMetroProcent: double): boolean;
    var
      MetroProcent: double;
      I: integer;
    begin
      MetroProcent := 0;
      for I := First to Last - 1 do
      begin
        MetroProcent := MetroProcent + A[I].SampleLength * A[I].Metall;
      end;
      Result := MetroProcent >= AMetroProcent;
      if not Result then
        for I := First to Last - 1 do
        begin
          A[I].OreType := 0;
        end;
    end;

    {GetMetroprocent return a meterpercent for interval}
    {Sub}
    function GetMetroprocent(A: array of TDrillHoleArrayItem;
      First, Last: integer): double;
    var
      I: integer;
    begin
      Result := 0;
      for I := First to Last - 1 do
      begin
        Result := Result + A[I].SampleLength * A[I].Metall;
      end;
    end;

    {Sub}
    function GetIntervalLength(A: array of TDrillHoleArrayItem;
      First, Last: integer): double;
    begin
      Result := 0;
      for First := First to Last - 1 do
      begin
        Result := Result + A[First].SampleLength;
      end;
    end;

    {Sub}
    function GetConcentration(A: array of TDrillHoleArrayItem;
      First, Last: integer): double;
    var
      Length: double;
    begin
      Result := 0;
      Length := 0;
      for First := First to Last - 1 do
      begin
        Length := Length + A[First].SampleLength;
        Result := Result + A[First].Metall * A[First].SampleLength;
      end;
      if Length > 0 then
        Result := Result / Length
      else
        Result := 0;
    end;

  var
    IntervalStart:  integer;
    IntervalEnd:    integer;
    Interval2Start: integer;
    Interval2End:   integer;
    WasteStart, WasteEnd: integer;
    WasteLength:    double;
    ReloadInterval: boolean;

    {Sub}
    procedure OptimizeIntervals;
    var
      I: integer;
      Concentration1, Concentration2: double;
    begin
      Concentration1 := GetConcentration(DrillHoleArray, IntervalStart, WasteEnd);
      Concentration2 := GetConcentration(DrillHoleArray, IntervalEnd,
        Interval2End);
      if (GetIntervalLength(DrillHoleArray, IntervalStart, IntervalEnd) <
        MaxWasteInterval) and (GetIntervalLength(DrillHoleArray,
        Interval2Start, Interval2End) < MaxWasteInterval) and
        (GetConcentration(DrillHoleArray, IntervalStart, Interval2End) >= CutOff) then
      begin
        ReloadInterval := True;
        for I := WasteStart to WasteEnd - 1 do
          DrillHoleArray[I].OreType := 1;
        exit;
      end;
      if (Concentration1 >= CutOff) and (Concentration2 >= CutOff) then
      begin
        ReloadInterval := True;
        for I := WasteStart to WasteEnd - 1 do
          DrillHoleArray[I].OreType := 1;
      end
      else
      begin
        if (GetMetroprocent(DrillHoleArray, IntervalStart, IntervalEnd) >=
          GetMetroprocent(DrillHoleArray, Interval2Start, Interval2End)) then
          //        if Concentration1>Concentration2 then
        begin
          I := WasteEnd;
          while (I < Interval2End) and
            (GetIntervalLength(DrillHoleArray, WasteStart, I) < MaxWasteInterval)
            do
            Inc(I);
          for I := I - 1 downto WasteEnd do
            DrillHoleArray[I].OreType := 0;
        end
        else
        begin
          I := WasteStart;
          while (I > IntervalStart) and
            (GetIntervalLength(DrillHoleArray, I, WasteEnd) < MaxWasteInterval)
            do
            Dec(I);
          for I := I to WasteStart - 1 do
            DrillHoleArray[I].OreType := 0;
          ReadInterval(DrillHoleArray, IntervalStart, IntervalEnd);
          ReloadInterval := CheckMetroprocent(DrillHoleArray,
            IntervalStart, IntervalEnd, CutOff * MinOreInteval);
          if not ReloadInterval then
            WasteLength := MaxWasteInterval + 100;
        end;
      end;
    end;

  begin
    IntervalStart := 0;
    IntervalEnd   := 0;
    if DrillHoleArray[0].OreType = 0 then
      ReadInterval(DrillHoleArray, IntervalStart, IntervalEnd);
    IntervalStart := IntervalEnd;
    while (IntervalStart <= High(DrillHoleArray)) do
    begin
      // Skip the waste interval
      if DrillHoleArray[IntervalStart].OreType = 0 then
      begin
        ReadInterval(DrillHoleArray, IntervalStart, IntervalEnd);
        IntervalStart := IntervalEnd;
      end;

      // Define the interval
      ReloadInterval := False;
      if ReadInterval(DrillHoleArray, IntervalStart, IntervalEnd) then
      begin
        repeat
          WasteStart := IntervalEnd;
          if ReadInterval(DrillHoleArray, WasteStart, WasteEnd) then
            WasteLength := GetIntervalLength(DrillHoleArray, WasteStart,
              WasteEnd)
          else
            WasteLength := MaxWasteInterval + 100;

          //under the dhole bottom is the waste interval
          if WasteLength < MaxWasteInterval then
          begin
            Interval2Start := WasteEnd;
            if ReadInterval(DrillHoleArray, Interval2Start, Interval2End) then
            begin
              OptimizeIntervals;
            end
            else
              WasteLength := MaxWasteInterval + 100;
            //under the dhole bottom is the waste interval
          end;

          if (not ReloadInterval) and (WasteLength >= MaxWasteInterval) then
          begin
            if not CheckMetroprocent(DrillHoleArray, IntervalStart,
              IntervalEnd, CutOff * MinOreInteval) then
              WasteLength   := MaxWasteInterval + 100
            else
              IntervalStart := IntervalEnd;
          end;
        until (WasteLength >= MaxWasteInterval) or (ReloadInterval);
      end;
    end;
  end;

  {Sub}
  procedure SaveDhole;
  var
    I: integer;
  begin
    for I := 0 to High(DrillHoleArray) do
    begin
      TableHoles.RecNo := DrillHoleArray[I].RecNo;
      TableHoles.Edit;
      try
        TableHoles.FieldByName(FieldNameOre).AsFloat :=
          DrillHoleArray[I].OreType;
      except
      end;
      TableHoles.Post;
    end;
    TableHoles.Next;
  end;

  {\Subroutines}

begin
  TableHoles.Close;
  AddTableField(TableHoles.TableName, FieldNameOre, ftInteger); //It was float?
  TableHoles.Open;
  ProgressBar.Min := 0;
  ProgressBar.Max := TableHoles.RecordCount;
  Drillhole      := TableHoles.FieldByName(fldDHOLE).AsString;
  IsLengthExists := TableHoles.FindField(fldLENGTH) <> nil;
  if not IsLengthExists then
  begin
    ShowMessage(LoadResString(@rsFieldsNotFound) + ':' + fldLENGTH);
    TableHoles.Close;
    Exit;
  end;
  while not EOF do
  begin
    ProgressBar.Position := TableHoles.RecNo;
    ReadDHole;
    GetIntervals;
    SaveDHole;
  end;
  TableHoles.Close;
end;

procedure TfmComposeOreIntervals.ListBoxRealAttributeClick(Sender: TObject);
begin
  inherited;
  EditOutputFieldName.Text := GetCurrentItem(ListBoxRealAttribute) + '_' + fldORETYPE;
end;


procedure TfmComposeOreIntervals.ReadIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      evMinOreInterval.AsDouble   :=
        ReadFloat(Name, LabelMinOreInterval.Caption, 4);
      evMaxWasteInterval.AsDouble :=
        ReadFloat(Name, LabelMaxWasteInterval.Caption, 4);
      evCutOffGrade.AsDouble      :=
        ReadFloat(Name, LabelCutOffGrade.Caption, 30);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeOreIntervals.WriteIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  with IniFile do
    try
      WriteFloat(Name, LabelMinOreInterval.Caption, evMinOreInterval.AsDouble);
      WriteFloat(Name, LabelMaxWasteInterval.Caption, evMaxWasteInterval.AsDouble);
      WriteFloat(Name, LabelCutOffGrade.Caption, evCutOffGrade.AsDouble);
    finally
      IniFile.Free;
    end;
end;

procedure TfmComposeOreIntervals.ButtonOKClick(Sender: TObject);
begin
  inherited;
  begin
    if (InModelName <> OutModelName) then
      CopyFiles(InModelName + TableExt,
        OutModelName + TableExt, OutModelType, False);

    if ModalResult <> mrNone then
    begin
      with dmBase do
      begin
        TableOutput.TableName := OutModelName;

        DefineOreIntervals(TableOutput,
          GetCurrentItem(ListBoxRealAttribute), evMinOreInterval.asDouble,
          evMaxWasteInterval.asDouble, evCutOffGrade.asDouble,
          EditOutputFieldName.Text);
      end;
    end
    else
      Exit;
  end;
end;

procedure TfmComposeOreIntervals.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
