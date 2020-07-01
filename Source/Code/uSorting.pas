//----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//----------------------------------------------------------------------------
{
  Sorting routines
}

unit uSorting;

interface

uses
  System.Classes,
  System.SysUtils;

type
  PRecCoord = ^TRecCoord;

  TRecCoord = record
    No: integer;
    At: extended;
  end;

type
  TSortCount = {0..High} integer;

  PSortArray = ^TSortArray;
  TSortArray = array of TRecCoord;

type
  TEnumMinus1_1 = -1..1;

  TSortThread = class(TThread)
  private
    FSortArray: TSortArray;
    FSize:      integer;
    FCount:     TSortCount;
    FTime:      TDateTime;
    FTimeGo:    boolean;
    procedure TimeStart;
    procedure TimeStop;
    function GetTimeAsStr: string;
    procedure SetCount(const Value: TSortCount);
  protected
   { The Execute method is called when the thread starts }
    procedure Execute; override;
    procedure Sort(var A: array of TRecCoord); virtual; abstract;
  public
    constructor Create(var SortArray: TSortArray; SortTerminate: TNotifyEvent = nil;
      ACount: TSortCount = 0);
  published
    property TimeAsStr: string Read GetTimeAsStr;
    property Count: TSortCount Read FCount Write SetCount;
  end;

  TFindMinSort = class(TSortThread)
  protected
    procedure Sort(var A: array of TRecCoord); override;
  end;

  TQuickSort = class(TSortThread)
  protected
    procedure Sort(var A: array of TRecCoord); override;
  end;

  TFindMaxSort = class(TSortThread)
  protected
    procedure Sort(var A: array of TRecCoord); override;
  end;

procedure FindMinSort(var A: array of TRecCoord; Count: integer);
procedure QuickSort(var A: array of TRecCoord; iLo, iHi: integer);

//==============================================================
implementation
//==============================================================

procedure QuickSort(var A: array of TRecCoord; iLo, iHi: integer);
var
  Mid:    double;
  Temp:   TRecCoord;
  Lo, Hi: integer;
begin
  Lo  := iLo;
  Hi  := iHi;
  Mid := A[(Lo + Hi) div 2].At;
  repeat
    while A[Lo].At < Mid do
      Inc(Lo);
    while A[Hi].At > Mid do
      Dec(Hi);
    if Lo <= Hi then
    begin
      Temp  := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := Temp;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then
    QuickSort(A, iLo, Hi);
  if Lo < iHi then
    QuickSort(A, Lo, iHi);
end;

procedure FindMinSort(var A: array of TRecCoord; Count: integer);
var
  I, J, T: integer;
  MinA:    double;
  Temp:    TRecCoord;
begin
  for I := low(A) to Count - 2 do
  begin
    MinA := A[I].At;
    T    := I;
    for J := I + 1 to Count - 1 do
      if A[J].At < MinA then
      begin
        MinA := A[J].At;
        T    := J;
      end;
    if T <> I then
    begin
      Temp := A[T];
      A[T] := A[I];
      A[I] := Temp;
    end;
  end;
end;

{ TSortThread }

constructor TSortThread.Create(var SortArray: TSortArray;
  SortTerminate: TNotifyEvent; ACount: TSortCount);
begin
  FSortArray := SortArray;
  FSize      := High(SortArray) - Low(SortArray) + 1;
  if ACount = 0 then
    FCount := FSize
  else
    FCount := ACount;
  FreeOnTerminate := True;
  FTimeGo := False;
  FTime   := Time;
  OnTerminate := SortTerminate;
  inherited Create(False);
end;

//---------------------------------------------------------------

procedure TSortThread.Execute;
begin
  Sort(FSortArray);
end;

procedure TSortThread.TimeStart;
begin
  FTime   := Time;
  FTimeGo := True;
end;

procedure TSortThread.TimeStop;
begin
  FTime   := Time - FTime;
  FTimeGo := False;
end;

//---------------------------------------------------------------

function TSortThread.GetTimeAsStr: string;
begin
  if FTimeGo then
    Result := TimeToStr(Time - FTime)
  else
    Result := TimeToStr(FTime);
end;

procedure TSortThread.SetCount(const Value: TSortCount);
begin
  FCount := Value;
end;

//---------------------------------------------------------------

procedure TFindMinSort.Sort(var A: array of TRecCoord);
var
  I, J, T: integer;
  MinA:    double;
  Temp:    TRecCoord;
begin
  TimeStart;
  for I := low(A) to Count do
  begin
    MinA := A[I].At;
    T    := I;
    for J := Low(A) to I - 1 do
      if A[J].At < MinA then
      begin
        MinA := A[J].At;
        T    := J;
      end;
    if T <> I then
    begin
      Temp := A[T];
      A[T] := A[I];
      A[I] := Temp;
      if Terminated then
        Exit;
    end;
  end;
  TimeStop;
end;

//---------------------------------------------------------------

procedure TFindMaxSort.Sort(var A: array of TRecCoord);
var
  I, J, T: integer;
  MaxA:    double;
  Temp:    TRecCoord;
begin
  TimeStart;
  for I := High(A) downto High(A) - Count + 1 do
  begin
    MaxA := A[I].At;
    T    := I;
    for J := Low(A) to I - 1 do
      if A[J].At > MaxA then
      begin
        MaxA := A[J].At;
        T    := J;
      end;
    if T <> I then
    begin
      Temp := A[T];
      A[T] := A[I];
      A[I] := Temp;
      if Terminated then
        Exit;
    end;
  end;
  TimeStop;
end;

//---------------------------------------------------------------

procedure TQuickSort.Sort(var A: array of TRecCoord);

  procedure QuickSort(var A: array of TRecCoord; iLo, iHi: integer);
  var
    Mid:    double;
    Temp:   TRecCoord;
    Lo, Hi: integer;
  begin
    Lo  := iLo;
    Hi  := iHi;
    Mid := A[(Lo + Hi) div 2].At;
    repeat
      while A[Lo].At < Mid do
        Inc(Lo);
      while A[Hi].At > Mid do
        Dec(Hi);
      if Lo <= Hi then
      begin
        Temp  := A[Lo];
        A[Lo] := A[Hi];
        A[Hi] := Temp;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Terminated then
      Exit;
    if Hi > iLo then
      QuickSort(A, iLo, Hi);
    if Lo < iHi then
      QuickSort(A, Lo, iHi);
  end;

begin
  TimeStart;
  QuickSort(A, Low(A), High(A));
  TimeStop;
end;

end.
