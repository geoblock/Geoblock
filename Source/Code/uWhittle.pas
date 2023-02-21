//
// This unit is part of the Geoblock software, http://sourceforge.net/projects/geoblock
//
(* Import/export Whittle's file of blocks *)

unit uWhittle;

interface

uses 
  System.Classes, 
  System.SysUtils, 
  Vcl.Controls,
  Vcl.Dialogs;

type
  TwtFileFormat = (ffComma, ffFixed);

type
  TRockType = class(TPersistent)
  public
    RockTypeCode:      string[4];
    RockTypeMiningCAF: double;
    RehabilititationCostPerTonne: double;
    ProcessingThroughputFactor: double;
  end;

  TRockTypeList = class(TList)
  public
    function IndexByName(Name: string): integer;
    procedure Assign(Source: TObject); virtual;
    procedure AddRockType(Value: string);
    procedure Clear; override;
  end;

  TElement = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    ElementTypeCode:      string[4];
    PositionInModelFile:  integer;
    DPElementUnitsInBlock: integer; // DP=Decimal Places for
    DPTotalsElementUnits: integer;
    DPElementGradesAndCutoffs: integer;
    property AsString: string Read GetAsString Write SetAsString;
  end;

  TElementList = class(TList)
  private
    function GetItems(Index: integer): TElement;
    procedure SetItems(Index: integer; const Value: TElement);
  public
    function IndexByName(Name: string): integer;
    procedure AddAsString(Value: string);
    procedure Assign(Source: TObject); virtual;
    procedure Clear; override;
    procedure SaveToStream(Stream: TStream);
    property Items[Index: integer]: TElement Read GetItems Write SetItems; default;
  end;

type
  TCustomGridParametersFile = class(TComponent)
  private
    FDX: double;
    FDY: double;
    FDZ: double;

    FXO: double;
    FYO: double;
    FZO: double;

    FNX: integer;
    FNY: integer;
    FNZ: integer;

    FActiveBlocksIndicator: integer;
    FSubRegionsCount: integer;
    FMiningCAF:     integer;
    FProcessingCAF: integer;
    FPrintUnprocessingMineralisationFlag: integer;
    FRestartInterval: double;

    FFileName:   TFileName;
    FFileFormat: TwtFileFormat;
    FChanged:    boolean;
    FonChange:   TNotifyEvent;

    procedure Change;

    function GetFromStreamLineType(Stream: TStream): integer; virtual;
    procedure LoadFromStreamLineType1(Stream: TStream); virtual;
    procedure LoadFromStreamLineType2(Stream: TStream); virtual;
    function LoadFromSrteamNChars(Stream: TStream; N: integer): string; virtual;
    function LoadFromStreamEOL(Stream: TStream): boolean; virtual;
    function LoadFromStreamCommentLine(Stream: TStream): string;
    procedure ReadStringLnFromStream(Stream: TStream; var Value: string);
      virtual;
    procedure SaveToStreamLineType1(Stream: TStream); virtual;
    procedure SaveToStreamLineType2(Stream: TStream); virtual;
    procedure SkipNCharsInStream(Stream: TStream; N: integer); virtual;
    procedure SkipCommentLineInStream(Stream: TStream); virtual;
    procedure SetChanged(const Value: boolean);
    procedure SetDX(const Value: double);
    procedure SetDY(const Value: double);
    procedure SetDZ(const Value: double);
    procedure SetXO(const Value: double);
    procedure SetYO(const Value: double);
    procedure SetZO(const Value: double);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileFormat(const Value: TwtFileFormat);
    procedure SetNX(const Value: integer);
    procedure SetNY(const Value: integer);
    procedure SetNZ(const Value: integer);
    procedure SetActiveBlocksIndicator(const Value: integer);
    procedure SetSubRegionsCount(const Value: integer);
    procedure SetMiningCAF(const Value: integer);
    procedure SetProcessingCAF(const Value: integer);
    procedure SetPrintUnprocessingMineralisationFlag(const Value: integer);
    procedure SetRestartInterval(const Value: double);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    function EditDialog: TModalResult; virtual;
  published
    property XO: double Read FXO Write SetXO;
    property YO: double Read FYO Write SetYO;
    property ZO: double Read FZO Write SetZO;

    property DX: double Read FDX Write SetDX;
    property DY: double Read FDY Write SetDY;
    property DZ: double Read FDZ Write SetDZ;

    property NX: integer Read FNX Write SetNX;
    property NY: integer Read FNY Write SetNY;
    property NZ: integer Read FNZ Write SetNZ;

    property ActiveBlocksIndicator: integer
      Read FActiveBlocksIndicator Write SetActiveBlocksIndicator;
    property SubRegionsCount: integer Read FSubRegionsCount Write SetSubRegionsCount;
    property MiningCAF: integer Read FMiningCAF Write SetMiningCAF;
    property ProcessingCAF: integer Read FProcessingCAF Write SetProcessingCAF;
    property PrintUnprocessingMineralisationFlag: integer
      Read FPrintUnprocessingMineralisationFlag Write SetPrintUnprocessingMineralisationFlag;
    property RestartInterval: double Read FRestartInterval Write SetRestartInterval;

    property FileFormat: TwtFileFormat Read FFileFormat Write SetFileFormat;
    property FileName: TFileName Read FFileName Write SetFileName;

    property Changed: boolean Read FChanged Write SetChanged;
    property onChange: TNotifyEvent Read FonChange Write FonChange;
  end;

  Twt4XParametersFile = class(TCustomGridParametersFile)
  private
    FRockList: TRockTypeList;
    FElements: TElementList;
    procedure LoadFromStreamLineType2(Stream: TStream); override;
    procedure LoadFromStreamLineType3(Stream: TStream); virtual;
    procedure LoadFromStreamLineType18(Stream: TStream); virtual;
    procedure LoadFromStreamLineType21(Stream: TStream); virtual;

    procedure SaveToStreamLineType2(Stream: TStream); override;
    procedure SaveToStreamLineType3(Stream: TStream); virtual;
    procedure SaveToStreamLinesType18(Stream: TStream); virtual;
    procedure SaveToStreamLinesType21(Stream: TStream); virtual;
    procedure SetRockList(const Value: TRockTypeList);
    procedure SetElements(const Value: TElementList);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditDialog: TModalResult; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property RockList: TRockTypeList Read FRockList Write SetRockList;
    property Elements: TElementList Read FElements Write SetElements;
  end;

//==========================================================================
implementation
//==========================================================================

uses
  fFileEditGridPars,
  fFileEditWhittlePars;

{ TCustomGridParametersFile }

procedure TCustomGridParametersFile.Assign(Source: TPersistent);
var
  Src: TCustomGridParametersFile;
begin
  if Source is TCustomGridParametersFile then
  begin
    Src := TCustomGridParametersFile(Source);

    FileName   := Src.FileName;
    FileFormat := Src.FileFormat;

    DX := Src.DX;
    DY := Src.DY;
    DZ := Src.DZ;

    XO := Src.XO;
    YO := Src.YO;
    ZO := Src.ZO;

    NX := Src.NX;
    NY := Src.NY;
    NZ := Src.NZ;

    ActiveBlocksIndicator := Src.ActiveBlocksIndicator;
    SubRegionsCount := Src.SubRegionsCount;
    MiningCAF     := Src.MiningCAF;
    ProcessingCAF := Src.ProcessingCAF;
    PrintUnprocessingMineralisationFlag :=
      Src.PrintUnprocessingMineralisationFlag;
    RestartInterval := Src.RestartInterval;

  end
  else
    inherited Assign(Source);
end;

procedure TCustomGridParametersFile.AssignTo(Dest: TPersistent);
begin
  Dest.Assign(Self);
end;

procedure TCustomGridParametersFile.Change;
begin
  if Assigned(onChange) then
    onChange(Self);
  Changed := True;
end;

constructor TCustomGridParametersFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDX      := 10;
  FDY      := 10;
  FDZ      := 10;
  FXO      := 0;
  FYO      := 0;
  FZO      := 0;
  FFileName := '';
  FFileFormat := ffFixed;
  FChanged := False;
end;

function TCustomGridParametersFile.EditDialog: TModalResult;
var
  Dialog: TfmFileEditGridPars;
begin
  Dialog := TfmFileEditGridPars.Create(Self);
  try
    Dialog.Component := Self;
    Result := Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

function TCustomGridParametersFile.GetFromStreamLineType(Stream: TStream): integer;
var
  S: string;
begin
  SetLength(S, 3);
  Stream.Read(PChar(S)^, 3);
  Stream.Seek(-3, soFromCurrent);
  try
    Result := StrToInt(S);
  except
    on EConvertError do
    begin
      Result := -1;
    end;
  end;
end;

function TCustomGridParametersFile.LoadFromStreamEOL(Stream: TStream): boolean;
var
  OldPosition: integer;
begin
  Result      := False;
  OldPosition := Stream.Position;
  if string(LoadFromSrteamNChars(Stream, 2)) = #13#10 then
    Result := True
  else
    Stream.Position := OldPosition;
end;

procedure TCustomGridParametersFile.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
    FFileName := FileName;
  finally
    Stream.Free;
  end;
end;

function TCustomGridParametersFile.LoadFromSrteamNChars(Stream: TStream;
  N: integer): string;
begin
{$O-}
  SetLength(Result, N);
  Stream.Read(PChar(Result)^, N);
end;

procedure TCustomGridParametersFile.LoadFromStream(Stream: TStream);
var
  LineType: integer;
begin
  while Stream.Position < Stream.Size - 2 do
  begin
    LineType := GetFromStreamLineType(Stream);
    SkipNCharsInStream(Stream, 2);
    case LineType of
      1:
        LoadFromStreamLineType1(Stream);
      else
        SkipCommentLineInStream(Stream);
    end;
  end;
end;

function TCustomGridParametersFile.LoadFromStreamCommentLine(Stream: TStream): string;
var
  C: char;
begin
  C      := #0;
  Result := '';
  while (Stream.Position < Stream.Size - 2) and (C <> #10) do
  begin
    Stream.Read(C, 1);
    if not (C in [#10, #13]) then
      Result := Result + C;
  end;
end;

procedure TCustomGridParametersFile.LoadFromStreamLineType1(Stream: TStream);
var
  S: string;
begin
  SkipNCharsInStream(Stream, 5);
  S := Trim(LoadFromSrteamNChars(Stream, 1 - (6 - 15)));
  if S <> '' then
    DX := StrToFloat(S);
  S := Trim(LoadFromSrteamNChars(Stream, 1 - (16 - 25)));
  if S <> '' then
    DY := StrToFloat(S);
  S := Trim(LoadFromSrteamNChars(Stream, 1 - (26 - 35)));
  if S <> '' then
    DZ := StrToFloat(S);
  if not LoadFromStreamEOL(Stream) then
  begin
    S := Trim(LoadFromSrteamNChars(Stream, 1 - (36 - 45)));
    if S <> '' then
      XO := StrToFloat(S);
    S := Trim(LoadFromSrteamNChars(Stream, 1 - (46 - 55)));
    if S <> '' then
      YO := StrToFloat(S);
    S := Trim(LoadFromSrteamNChars(Stream, 1 - (56 - 65)));
    if S <> '' then
      ZO := StrToFloat(S);
  end;
end;

procedure TCustomGridParametersFile.SetChanged(const Value: boolean);
begin
  if FChanged <> Value then
  begin
    FChanged := Value;
    if Value then
      Change;
  end;
end;

procedure TCustomGridParametersFile.SetDX(const Value: double);
begin
  if FDX <> Value then
  begin
    FDX     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetDY(const Value: double);
begin
  if FDY <> Value then
  begin
    FDY     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetDZ(const Value: double);
begin
  if FDZ <> Value then
  begin
    FDZ     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetFileFormat(const Value: TwtFileFormat);
begin
  if FFileFormat <> Value then
  begin
    FFileFormat := Value;
    Change;
  end;
end;

procedure TCustomGridParametersFile.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    Change;
  end;
end;

procedure TCustomGridParametersFile.SetXO(const Value: double);
begin
  if FXO <> Value then
  begin
    FXO     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetYO(const Value: double);
begin
  if FYO <> Value then
  begin
    FYO     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetZO(const Value: double);
begin
  if FZO <> Value then
  begin
    FZO     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SkipCommentLineInStream(Stream: TStream);
var
  C: char;
begin
  C := #0;
  while (Stream.Position < Stream.Size - 2) and (C <> #10) do
    Stream.Read(C, 1);
end;

procedure TCustomGridParametersFile.SkipNCharsInStream(Stream: TStream; N: integer);
var
  Str: Pointer;
begin
  GetMem(Str, N);
  try
    Stream.Read(Str^, N);
  finally
    FreeMem(Str, N);
  end;
end;

procedure TCustomGridParametersFile.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomGridParametersFile.SaveToStream(Stream: TStream);
begin
  SaveToStreamLineType1(Stream);
end;

procedure TCustomGridParametersFile.SaveToStreamLineType1(Stream: TStream);
var
  S: string;
begin
  S := '  1  ';
  S := S + Format('%10.10g', [DX]);
  S := S + Format('%10.10g', [DY]);
  S := S + Format('%10.10g', [DZ]);
  if (XO <> 0) or (YO <> 0) or (ZO <> 0) then
  begin
    S := S + Format('%10.10g', [XO]);
    S := S + Format('%10.10g', [YO]);
    S := S + Format('%10.10g', [ZO]);
  end;
  S := S + #13#10;
  Stream.Write(PChar(S)^, Length(S));
end;

procedure TCustomGridParametersFile.SetNX(const Value: integer);
begin
  if FNX <> Value then
  begin
    FNX     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetNY(const Value: integer);
begin
  if FNY <> Value then
  begin
    FNY     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetNZ(const Value: integer);
begin
  if FNZ <> Value then
  begin
    FNZ     := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetActiveBlocksIndicator(const Value: integer);
begin
  if FActiveBlocksIndicator <> Value then
  begin
    FActiveBlocksIndicator := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetSubRegionsCount(const Value: integer);
begin
  if FSubRegionsCount <> Value then
  begin
    FSubRegionsCount := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetMiningCAF(const Value: integer);
begin
  if FMiningCAF <> Value then
  begin
    FMiningCAF := Value;
    Changed    := True;
  end;
end;

procedure TCustomGridParametersFile.SetProcessingCAF(const Value: integer);
begin
  if FProcessingCAF <> Value then
  begin
    FProcessingCAF := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetPrintUnprocessingMineralisationFlag(
  const Value: integer);
begin
  if FPrintUnprocessingMineralisationFlag <> Value then
  begin
    FPrintUnprocessingMineralisationFlag := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.SetRestartInterval(const Value: double);
begin
  if FRestartInterval <> Value then
  begin
    FRestartInterval := Value;
    Changed := True;
  end;
end;

procedure TCustomGridParametersFile.ReadStringLnFromStream(Stream: TStream;
  var Value: string);
var
  C: char;
begin
  Value := '';
  C     := #0;
  while (C <> #10) and (C <> #13) and (Stream.Position <= Stream.Size - 1) do
  begin
    Stream.Read(C, 1);
    if (C <> #10) or (C <> #13) then
      Value := Value + C;
  end;
  if (Stream.Position <> Stream.Size - 1) then
  begin
    Stream.Read(C, 1);
    if (C <> #10) or (C <> #13) then
      Stream.Seek(-1, soFromCurrent);
  end;
end;

procedure TCustomGridParametersFile.LoadFromStreamLineType2(Stream: TStream);
begin

end;

procedure TCustomGridParametersFile.SaveToStreamLineType2(Stream: TStream);
begin

end;

{ Twt4XParametersFile }

procedure Twt4XParametersFile.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is Twt4XParametersFile then
  begin
    RockList := Twt4XParametersFile(Source).RockList;
    Elements := Twt4XParametersFile(Source).Elements;
  end;
end;

constructor Twt4XParametersFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRockList := TRockTypeList.Create;
  FElements := TElementList.Create;
end;

destructor Twt4XParametersFile.Destroy;
begin
  FElements.Free;
  FRockList.Free;
  inherited Destroy;
end;

function Twt4XParametersFile.EditDialog: TModalResult;
var
  Dialog: TfmFileEditWhittlePars;
begin
  Dialog := TfmFileEditWhittlePars.Create(Self);
  try
    Dialog.Component := Self;
    Dialog.Component.Change;
    Result := Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

procedure Twt4XParametersFile.LoadFromStream(Stream: TStream);
var
  LineType: integer;
begin
  while Stream.Position < Stream.Size - 2 do
  begin
    LineType := GetFromStreamLineType(Stream);
    if LineType > 0 then
    begin
      case LineType of
        1: LoadFromStreamLineType1(Stream);
        2: LoadFromStreamLineType2(Stream);
        3: LoadFromStreamLineType3(Stream);
        18: LoadFromStreamLineType18(Stream);
        21: LoadFromStreamLineType21(Stream);
        else
          SkipCommentLineInStream(Stream);
      end;
    end
    else
      LoadFromStreamCommentLine(Stream);
  end;
end;

procedure Twt4XParametersFile.LoadFromStreamLineType18(Stream: TStream);
var
  S: string;
begin
  ReadStringLnFromStream(Stream, S);
  Elements.AddAsString(S);
end;

procedure Twt4XParametersFile.LoadFromStreamLineType2(Stream: TStream);
var
  S: string;
begin
  SkipNCharsInStream(Stream, -(1 - 11));
  S := Trim(LoadFromSrteamNChars(Stream, -(11 - 16)));
  if S <> '' then
    NX := StrToInt(S);

  SkipNCharsInStream(Stream, -(16 - 21));
  S := Trim(LoadFromSrteamNChars(Stream, -(21 - 26)));
  if S <> '' then
    NY := StrToInt(S);

  SkipNCharsInStream(Stream, -(26 - 31));
  S := Trim(LoadFromSrteamNChars(Stream, -(31 - 36)));
  if S <> '' then
    NZ := StrToInt(S);

  SkipCommentLineInStream(Stream);
end;

procedure Twt4XParametersFile.LoadFromStreamLineType21(Stream: TStream);
var
  S: string;
begin
  ReadStringLnFromStream(Stream, S);
  RockList.AddRockType(S);
end;

procedure Twt4XParametersFile.LoadFromStreamLineType3(Stream: TStream);
var
  S: string;
begin
  SkipNCharsInStream(Stream, -(1 - 15));
  S := Trim(LoadFromSrteamNChars(Stream, -(15 - 16)));
  if S <> '' then
    ActiveBlocksIndicator := StrToInt(S);

  SkipNCharsInStream(Stream, -(16 - 21));
  S := Trim(LoadFromSrteamNChars(Stream, -(21 - 26)));
  if S <> '' then
    SubRegionsCount := StrToInt(S);

  SkipNCharsInStream(Stream, -(26 - 45));
  S := Trim(LoadFromSrteamNChars(Stream, -(45 - 46)));
  if S <> '' then
    MiningCAF := StrToInt(S);

  SkipNCharsInStream(Stream, -(46 - 55));
  S := Trim(LoadFromSrteamNChars(Stream, -(55 - 56)));
  if S <> '' then
    ProcessingCAF := StrToInt(S);

  SkipNCharsInStream(Stream, -(56 - 59));
  S := Trim(LoadFromSrteamNChars(Stream, -(59 - 60)));
  if S <> '' then
    PrintUnprocessingMineralisationFlag := StrToInt(S);

  SkipNCharsInStream(Stream, -(60 - 66));
  S := Trim(LoadFromSrteamNChars(Stream, -(66 - 75)));
  if S <> '' then
    RestartInterval := StrToFloat(S);

  SkipCommentLineInStream(Stream);
end;

procedure Twt4XParametersFile.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  SaveToStreamLineType2(Stream);
  SaveToStreamLineType3(Stream);
  SaveToStreamLinesType18(Stream);
  SaveToStreamLinesType21(Stream);
end;

procedure Twt4XParametersFile.SaveToStreamLinesType18(Stream: TStream);
begin
  Elements.SaveToStream(Stream);
end;

procedure Twt4XParametersFile.SaveToStreamLinesType21(Stream: TStream);
var
  I: integer;
  S: string;
begin
  for I := 1 to RockList.Count do
    with TRockType(RockList.Items[I - 1]) do
    begin
      S := Format(' 21  %-4s      %10s%10s%10s', [RockTypeCode,
        Format('%.10g', [RockTypeMiningCAF]), Format('%.10g',
        [RehabilititationCostPerTonne]), Format('%.10g',
        [ProcessingThroughputFactor])]) + #13#10;
      Stream.Write(PChar(S)^, Length(S));
    end;
end;

procedure Twt4XParametersFile.SaveToStreamLineType2(Stream: TStream);
var
  S: string;
begin
  S := '  2  ';
  S := S + Format('%10s', [IntToStr(NX)]);
  S := S + Format('%10s', [IntToStr(NY)]);
  S := S + Format('%10s', [IntToStr(NZ)]);
  S := S + #13#10;
  Stream.Write(PChar(S)^, Length(S));
end;

procedure Twt4XParametersFile.SaveToStreamLineType3(Stream: TStream);
var
  S: string;
begin
  S := '  3  ';
  S := S + Format('%10s', [IntToStr(ActiveBlocksIndicator)]);
  S := S + Format('%10s', [IntToStr(SubRegionsCount)]);
  S := S + Format('%20s', [IntToStr(MiningCAF)]);
  S := S + Format('%10s', [IntToStr(ProcessingCAF)]);
  S := S + Format('%04s', [IntToStr(PrintUnprocessingMineralisationFlag)]);
  S := S + Format('%16s', [FloatToStr(RestartInterval)]);
  S := S + #13#10;
  Stream.Write(PChar(S)^, Length(S));
end;

procedure Twt4XParametersFile.SetElements(const Value: TElementList);
begin
  FElements.Assign(Value);
  Changed := True;
end;

procedure Twt4XParametersFile.SetRockList(const Value: TRockTypeList);
begin
  FRockList.Assign(Value);
  Changed := True;
end;

{ TRockTypeList }

procedure TRockTypeList.AddRockType(Value: string);
var
  RockType: TRockType;
  S:    string[4];
  D:    double;
  Code: integer;
begin
  RockType := TRockType.Create;
  with RockType do
  begin
    S := trim(Copy(Value, 6, 4));
    RockTypeCode := S;
    Val(Copy(Value, 16, 10), D, Code);
    if Code = 0 then
      RockTypeMiningCAF := D;
    Val(Copy(Value, 26, 10), D, Code);
    if Code = 0 then
      RehabilititationCostPerTonne := D;
    Val(Copy(Value, 36, 10), D, Code);
    if Code = 0 then
      ProcessingThroughputFactor := D;
  end;
  Add(RockType);
end;

procedure TRockTypeList.Assign(Source: TObject);
var
  I: integer;
  RockTypeItem: TRockType;
begin
  if Source is TRockTypeList then
    with TRockTypeList(Source) do
    begin
      Self.Clear;
      for I := 0 to Count - 1 do
        with TRockType(Items[I]) do
        begin
          RockTypeItem := TRockType.Create;
          RockTypeItem.RockTypeCode := RockTypeCode;
          RockTypeItem.RockTypeMiningCAF := RockTypeMiningCAF;
          RockTypeItem.RehabilititationCostPerTonne :=
            RehabilititationCostPerTonne;
          RockTypeItem.ProcessingThroughputFactor := ProcessingThroughputFactor;
          Self.Add(RockTypeItem);
        end;
    end
  else
    raise EConvertError.Create('Can`t Assign ' + Source.ClassName +
      ' to ' + Self.ClassName);
end;

procedure TRockTypeList.Clear;
var
  I: integer;
begin
  for i := 0 to Count - 1 do
    TObject(Items[I]).Free;
  inherited Clear;
end;

function TRockTypeList.IndexByName(Name: string): integer;
begin
  Result := Count;
  repeat
    Dec(Result);
  until (Result < 0) or (UpperCase(TRockType(Items[Result]).RockTypeCode) =
      UpperCase(Name));
end;

{ TElementList }

procedure TElementList.AddAsString(Value: string);
var
  ElementItem: TElement;
begin
  ElementItem := TElement.Create;
  ElementItem.AsString := Value;
  Add(ElementItem);
end;

procedure TElementList.Assign(Source: TObject);
var
  I: integer;
  ElementItem: TElement;
begin
  if Source is TElementList then
    with TElementList(Source) do
    begin
      Self.Clear;
      for I := 0 to Count - 1 do
        with Items[I] do
        begin
          ElementItem := TElement.Create;
          ElementItem.ElementTypeCode := ElementTypeCode;
          ElementItem.PositionInModelFile := PositionInModelFile;
          ElementItem.DPElementUnitsInBlock := DPElementUnitsInBlock;
          ElementItem.DPTotalsElementUnits := DPTotalsElementUnits;
          ElementItem.DPElementGradesAndCutoffs := DPElementGradesAndCutoffs;
          Self.Add(ElementItem);
        end;
    end
  else
    raise EConvertError.CreateFmt('Cannot Assign %s to %s',
      [Source.ClassName, ClassName]);
end;

procedure TElementList.Clear;
var
  I: integer;
begin
  for i := 0 to Count - 1 do
    Items[I].Free;
  inherited Clear;
end;

function TElementList.GetItems(Index: integer): TElement;
begin
  Result := TElement(Get(Index));
end;

function TElementList.IndexByName(Name: string): integer;
begin
  Result := Count;
  repeat
    Dec(Result);
  until (Result < 0) or (UpperCase(Items[Result].ElementTypeCode) = UpperCase(Name));
end;

procedure TElementList.SaveToStream(Stream: TStream);
var
  I: integer;
  S: string;
begin
  for I := 0 to Count - 1 do
  begin
    S := Items[I].AsString + #13#10;
    Stream.Write(PChar(S)^, Length(S));
  end;
end;

procedure TElementList.SetItems(Index: integer; const Value: TElement);
begin
  Put(Index, Value);
end;

{ TElement }

function TElement.GetAsString: string;
begin
  Result := Format(' 18  %-4s           %5d%5d%5d%5d',
    [ElementTypeCode, PositionInModelFile, DPElementUnitsInBlock,
    DPTotalsElementUnits, DPElementGradesAndCutoffs]);
end;

procedure TElement.SetAsString(const Value: string);
begin
  ElementTypeCode      := Trim(Copy(Value, 6, -(6 - 10)));
  PositionInModelFile  := StrToInt(Copy(Value, 21, -(21 - 26)));
  DPElementUnitsInBlock := StrToInt(Copy(Value, 26, -(26 - 31)));
  DPTotalsElementUnits := StrToInt(Copy(Value, 31, -(31 - 36)));
  DPElementGradesAndCutoffs := StrToInt(Copy(Value, 36, -(36 - 41)));
end;

end.
