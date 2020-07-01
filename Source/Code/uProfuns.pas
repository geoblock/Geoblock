 //------------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //------------------------------------------------------------------------------

{! The unit includes procedures and functions, that
   implement some calculation routines}

unit uProfuns;

interface

uses
  System.SysUtils,
  System.Math;


const
  Delimeter: set of AnsiChar = [' ', #9, ',', ';'];
  MaxFieldCount = 255;

type
  TNamesArray = array of string;

type
  PWord = ^word;
  float = single;

  TFloatPoint = record
    X, Y, Z: float;
  end;

const
  Epsilon: double = 0.00001;

function ChangeModelTable(DirSource, DirDest: string; AFileName: TFileName): string;

procedure ClearArray(var A: array of real);
function DirAng(x1, y1, x2, y2: Float): Float;
function Sign(Value: double): shortint;

procedure SkipDelimeters(s: string; var Pos: word);
procedure SkipNotDelimeters(s: string; var Pos: word);
function GetFieldTxt(S: AnsiString; var Pos: word): AnsiString;
function GetFieldTypes(S: AnsiString; var FieldTypes: TNamesArray): word;
function GetFieldNames(S: AnsiString; var FieldNames: TNamesArray): word;
function CmpFieldsTypeArray(FTA1, FTA2: TNamesArray; From, Till: word): boolean;

procedure GetFieldsInfo(var FD: TextFile; var NameCount: word;
  var FieldNames: TNamesArray; var FieldsCount: word; var FieldTypes: TNamesArray);

function BlockOnLine(PL1, Pl2, P: TFloatPoint; D_x, D_y: Float; var L: Float): boolean;
{ If two lines intersect
 Input: P1: a point on line L1;
        P2: a point on line L2;
 Output:
   t:parameter value t with respect to line L2 when lines intersect
   True:lines intersect
   False:lines not intersect}
function Intersect(P1, P2, PL1, PL2: TFloatPoint; var Po: TFloatPoint): boolean;
{Input:
   P1,L1:point on line L1 and directional vector;
   P2,L2:the same for L2;
 Output:
   t: parameter value t with respoct to line L2 when lines intersect
   True: lines intersect;
   False: lines not have common points}
function Line_Line(P1, L1, P2, L2: TFloatPoint; var T: Float): boolean;
{ Input:
    P1:point on line
    L:directional vector
    P:check point
 Output:
    -1:check point is left from line
     0:check point on line
     1:check point right from line}
function Point_Line(P1, L, P: TFloatPoint): integer;
{If point lies in line segment
  Input:
    P1: origin of segment
    P2: end of segment
    P:  check point
  Output:
    True:check point is in segment
    False: check point is not in segment}
function Point_Section(P1, P2, P: TFloatPoint): boolean;
function Section_Section(P1, P2, P3, P4: TFloatPoint): boolean;

// Factorial of x
function Fact(x: integer): double;
//Permutations of n taken r at a time
function Perm(n, r: integer): double;
//Combinations of n taken r at a time
function Comb(n, r: integer): double;

function StrAng(Angle: Float): string;
function NameOnly(const AFileName: TFileName): TFileName;
function ReductionToRange(LowLimit, Value, HighLimit: integer): integer;

// Correct for point or comma delimeters
function StrToVal(S: string): double;
// Right result with point or comma delimeters
function ValToStr(AValue: double): string;
function ExtractCharQuotedStr(const Src: string; Quote: char): string;
function CharQuotedStr(const Src: string; Quote: char = ''''): string;

procedure GetHistogram(var f: array of integer; Lo, Hi, MaxData, MaxClass: integer;
  Data: array of single);

//======================================================================
implementation
//======================================================================

function ChangeModelTable(DirSource, DirDest: string; AFileName: TFileName): string;
var
  StartP: integer;
begin
  Result := AFileName;
  StartP := Pos(DirSource, Result);
  if StartP > 0 then
  begin
    Delete(Result, StartP, Length(DirSource));
    Insert(DirDest, Result, StartP);
  end;
end;

procedure ClearArray(var A: array of real);
var
  I: integer;
begin
  for I := 0 to High(A) do
    A[I] := 0;
end;

function Sign(Value: double): shortint;
begin
  if Value > 0 then
    Result := 1
  else if Value < 0 then
    Result := -1
  else
    Result := 0;
end;

function CmpFieldsTypeArray(FTA1, FTA2: TNamesArray; From, Till: word): boolean;

var
  I: word;
begin
  I      := From;
  Result := I <= Till;
  while (I <= Till) and Result do
  begin
    Result := (FTA1[I] = FTA2[I]);
    Inc(I);
  end;
end;

function DirAng(x1, y1, x2, y2: Float): Float;
var
  dX, dY, Ratio: Float;
  Alfa: Float;
begin
  Alfa := 0;
  dX   := (x2 - x1);
  dY   := (y2 - y1);
  if dX <> 0 then
  begin
    Ratio := dY / dX;
    if (dX > 0) and (dY > 0) then //I
      Alfa := Abs(ArcTan(Ratio)) * 180 / PI;
    if (dX < 0) and (dY > 0) then //II
      Alfa := 180 - Abs(ArcTan(Ratio)) * 180 / PI;
    if (dX < 0) and (dY < 0) then //III
      Alfa := 180 + Abs(ArcTan(Ratio)) * 180 / PI;
    if (dX > 0) and (dY < 0) then //IV
      Alfa := 360 - Abs(ArcTan(Ratio)) * 180 / PI;
  end
  else
  begin
    // dX = 0
    if dY > 0 then
      Alfa := 90
    else if dY < 0 then
      Alfa := 270
    else
      Alfa := 0;
  end;
  Result := Alfa;
end;

function GetFieldTxt(S: AnsiString; var Pos: word): AnsiString;
begin
  Result := '';
  while (Pos <= Length(S)) and (CharInSet(S[Pos], Delimeter)) do
    Inc(Pos);
  while (Pos <= Length(S)) and not (CharInSet(S[Pos], Delimeter)) do
  begin
    Result := Result + S[Pos];
    Inc(Pos);
  end;
  GetFieldTxt := Result;
end;

function GetFieldTypes(S: AnsiString; var FieldTypes: TNamesArray): word;
var
  FieldPos: word;
  FieldAsString: AnsiString;
  Code: integer;
  FieldAsNumber: real;
begin
  Result := 0;
  SetLength(FieldTypes, Result);
  FieldPos := 1;
  while FieldPos <= Length(S) do
  begin
    FieldAsString := GetFieldTxt(S, FieldPos);
    if FieldAsString <> '' then
    begin
      Inc(Result);
      SetLength(FieldTypes, Result);
      Val(FieldAsString, FieldAsNumber, Code);
      if Code = 0 then
        FieldTypes[Result - 1] := 'N'
      else
        FieldTypes[Result - 1] := 'A20';
    end;
  end;
end;

function GetFieldNames(S: AnsiString; var FieldNames: TNamesArray): word;
var
  FieldPos:      word;
  FieldAsString: AnsiString;
begin
  Result := 0;
  SetLength(FieldNames, Result);
  FieldPos := 2;
  while FieldPos <= Length(S) do
  begin
    FieldAsString := GetFieldTxt(S, FieldPos);
    if FieldAsString <> '' then
    begin
      Inc(Result);
      SetLength(FieldNames, Result);
      FieldNames[Result - 1] := FieldAsString;
    end;
  end;
end;

procedure GetFieldsInfo(var FD: TextFile; var NameCount: word;
  var FieldNames: TNamesArray; var FieldsCount: word; var FieldTypes: TNamesArray);
var
  S:   string;
  FTA: TNamesArray;
begin
  Reset(FD);
  if not EOF(FD) then
    Readln(FD, S);
  FieldsCount := GetFieldTypes(S, FTA);
  NameCount   := GetFieldNames(S, FieldNames);
  if not EOF(FD) then
    Readln(FD, S);
  if not EOF(FD) then
    Readln(FD, S);
  Close(FD);
  FieldsCount := GetFieldTypes(S, FieldTypes);
  if CmpFieldsTypeArray(FieldTypes, FTA, 0, FieldsCount - 1) then
    NameCount := 0;
  FTA := nil;
end;

procedure SkipDelimeters(S: string; var Pos: word);
begin
  while (Pos <= Length(S)) and (CharInSet(S[Pos], Delimeter)) do
    Inc(Pos);
end;

procedure SkipNotDelimeters(s: string; var Pos: word);
begin
  while (Pos <= length(S)) and not (CharInSet(S[Pos], Delimeter)) do
    Inc(Pos);
end;

function BlockOnLine(PL1, PL2, P: TFloatPoint; D_x, D_y: Float; var L: Float): boolean;
var
  i, j:    integer;
  P1, P2:  TFloatPoint;
  Po, Len: array[1..4] of TFloatPoint;
begin
  for i := 1 to 4 do
  begin
    Po[i].X  := 0;
    Po[i].Y  := 0;
    Len[i].X := 0;
    Len[i].Y := 0;
  end;
  BlockOnLine := False;

  P1.X := P.X - d_x / 2; {'-}
  P1.Y := P.Y - d_y / 2;
  P2.X := P.X + d_x / 2;
  P2.Y := P.Y - d_y / 2;

  if Intersect(P1, P2, PL1, PL2, Po[1]) then
    BlockOnLine := True;

  P2.X := P.X - d_x / 2; {[}
  P2.Y := P.Y + d_y / 2;

  if Intersect(P1, P2, PL1, PL2, Po[2]) then
    BlockOnLine := True;

  P1.X := P.X + d_x / 2; {_.}
  P1.Y := P.Y + d_y / 2;
  P2.X := P.X - d_x / 2;
  P2.Y := P.Y + d_y / 2;

  if Intersect(P1, P2, PL1, PL2, Po[3]) then
    BlockOnLine := True;

  P2.X := P.X + d_x / 2; {]}
  P2.Y := P.Y - d_y / 2;
  if Intersect(P1, P2, PL1, PL2, Po[4]) then
    BlockOnLine := True;

  j := 1;
  for i := 1 to 4 do
  begin
    if (Po[i].X <> 0) and (Po[i].X <> 0) then
    begin
      Len[j].X := Po[i].X;
      Len[j].Y := Po[i].Y;
      Inc(j);
    end;
  end;
  L := sqrt(sqr(Len[2].X - Len[1].X) + sqr(Len[2].Y - Len[1].Y));
  (*if L<=1 then  Block_Line:=False
   else Block_Line:=True;
   if L>sqrt(sqr(d_x)+sqr(d_y)) then  L:=sqrt(sqr(d_x)+sqr(d_y));*)
end;

function Point_Line(P1, L, P: TFloatPoint): integer;
var
  a, b, c: Float;
begin
  a := P.X - P1.X;
  b := P.Y - P1.Y;
  c := b * L.X - a * L.Y;
  // if Abs(c)<10E-5 then Point_Line:=0
  if Abs(c) < Epsilon then
    Point_Line := 0
  else if c > 0 then
    Point_Line := -1
  else
    Point_Line := 1;
end;

function Point_Section(P1, P2, P: TFloatPoint): boolean;
var
  t: Float;
  L: TFloatPoint;
begin
  L.X := P2.X - P1.X;
  L.Y := P2.Y - P1.Y;
  if Point_Line(P1, L, P) = 0 then
  begin
    if Abs(L.X) < Abs(L.Y) then
      t := (P.Y - P1.Y) / L.Y
    else
      t := (P.X - P1.X) / L.X;
    if (t <= 1) and (t >= 0) then
      Point_Section := True
    else
      Point_Section := False;
  end
  else
    Point_Section := False;
end;

function Intersect(P1, P2, PL1, PL2: TFloatPoint; var Po: TFloatPoint): boolean;
var
  a, b, c, d, d1, t:     Float;
  L1, L2, Poo, VL1, VL2: TFloatPoint;
begin
  L1.X := P2.X - P1.X;
  L1.Y := P2.Y - P1.Y;
  L2.X := PL2.X - PL1.X;
  L2.Y := PL2.Y - PL1.Y;
  a    := PL1.X - P1.X;
  b    := PL1.Y - P1.Y;

  d1 := a * L1.Y - b * L1.X;
  d  := L1.X * L2.Y - L1.Y * L2.X;

  if abs(d) < Epsilon then
    Intersect := False
  else
  begin
    t     := d1 / d;
    Poo.X := (PL1.X + L2.X * t);
    Poo.Y := (PL1.Y + L2.Y * t);

    if (PL1.X) > (PL2.X) then
    begin
      VL2.X := (PL1.X);
      VL1.X := (PL2.X);
    end
    else
    begin
      VL2.X := (PL2.X);
      VL1.X := (PL1.X);
    end;
    if (PL1.Y) > (PL2.Y) then
    begin
      VL2.Y := (PL1.Y);
      VL1.Y := (PL2.Y);
    end
    else
    begin
      VL2.Y := (PL2.Y);
      VL1.Y := (PL1.Y);
    end;
    a := sqrt(sqr((L1.X)) + sqr((L1.Y)));
    b := sqrt(sqr((L2.X)) + sqr((L2.Y)));
    if a < b then
    begin
      c := sqrt(sqr(Poo.X - P1.X) + sqr(Poo.Y - P1.Y));
      if (c < a) and (((Poo.X) >= (VL1.x)) and ((Poo.X) <= (VL2.x)) and
        ((Poo.y) >= (VL1.y)) and ((Poo.y) <= (VL2.y))) then
      begin
        Po.X      := Poo.X;
        Po.Y      := Poo.Y;
        Intersect := True;
      end
      else
      begin
        Po.X      := 0;
        Po.Y      := 0;
        Intersect := False;
      end;
    end
    else
    begin
      c := sqrt(sqr(Poo.X - PL1.X) + sqr(Poo.Y - PL1.Y));
      if (c < b) and (((Poo.X) >= (VL1.x)) and ((Poo.X) <= (VL2.x)) and
        ((Poo.y) >= (VL1.y)) and ((Poo.y) <= (VL2.y))) then
      begin
        Po.X      := Poo.X;
        Po.Y      := Poo.Y;
        Intersect := True;
      end
      else
      begin
        Po.X      := 0;
        Po.Y      := 0;
        Intersect := False;
      end;
    end;
  end; {d<0.00001}
end;

procedure Sect_Sect(X11, Y11, X12, Y12, X21, Y21, X22, Y22: Float; var X0, Y0: Float);
var
  a, b, c, d, d1, t: Float;
  L1X, L1Y, L2X, L2Y, X00, Y00, VL1X, VL1Y, VL2X, VL2Y: Float;
begin
  L1X := X12 - X11;
  L1Y := Y12 - Y11;
  L2X := X22 - X21;
  L2Y := Y22 - Y21;
  a   := X21 - X11;
  b   := Y21 - Y11;

  d1 := a * L1Y - b * L1X;
  d  := L1X * L2Y - L1Y * L2X;

  if abs(d) >= Epsilon then
  begin
    t   := d1 / d;
    X00 := (X21 + L2X * t);
    Y00 := (Y21 + L2Y * t);

    if (X21) > (X22) then
    begin
      VL2X := (X21);
      VL1X := (X22);
    end
    else
    begin
      VL2X := (X22);
      VL1X := (X21);
    end;
    if (Y21) > (Y22) then
    begin
      VL2Y := (Y21);
      VL1Y := (Y22);
    end
    else
    begin
      VL2Y := (Y22);
      VL1Y := (Y21);
    end;

    a := sqrt(sqr((L1X)) + sqr((L1Y)));
    b := sqrt(sqr((L2X)) + sqr((L2Y)));
    if a < b then
    begin
      c := sqrt(sqr(X00 - X11) + sqr(Y00 - Y11));
      if (c < a) and (((X00) >= (VL1x)) and ((X00) <= (VL2x)) and
        ((y00) >= (VL1y)) and ((y00) <= (VL2y))) then
      begin
        X0 := X00;
        Y0 := Y00;
      end
      else
      begin
        X0 := 0;
        Y0 := 0;
      end;
    end
    else
    begin
      c := sqrt(sqr(X00 - X21) + sqr(Y00 - Y21));
      if (c < b) and (((X00) >= (VL1x)) and ((X00) <= (VL2x)) and
        ((y00) >= (VL1y)) and ((y00) <= (VL2y))) then
      begin
        X0 := X00;
        Y0 := Y00;
      end
      else
      begin
        X0 := 0;
        Y0 := 0;
      end;
    end;
  end; {d>0.00001}
end;

function Line_Line(P1, L1, P2, L2: TFloatPoint; var t: Float): boolean;
var
  a, b, d, d1: Float;
begin
  a  := P2.X - P1.X;
  b  := P2.Y - P1.Y;
  d1 := a * L1.Y - b * L1.X;
  d  := L1.X * L2.Y - L1.Y * L2.X;
  if abs(d) < Epsilon then
    Line_Line := False
  else
  begin
    t := d1 / d;
    Line_Line := True;
  end;
end;

function Section_Section(P1, P2, P3, P4: TFloatPoint): boolean;
var
  L1, L2: TFloatPoint;
  a, b, d, d1, d2: Float;
begin
  L1.X := P2.X - P1.X;
  L1.Y := P2.Y - P1.Y;
  L2.X := P4.X - P3.X;
  L2.Y := P4.Y - P3.Y;
  a    := P3.X - P1.X;
  b    := P3.Y - P1.Y;
  d    := L1.X * L2.Y - L1.Y * L2.X;
  if d <> 0 then
  begin
    d1 := (a * L2.Y - b * L2.X) / d;
    d2 := (a * L1.Y - b * L1.X) / d;
    if (d1 >= 0) and (d1 <= 1) and (d2 >= 0) and (d2 <= 1) then
      Section_Section := True
    else
      Section_Section := False;
  end
  else
    Section_Section := False;
end;

function Fact(x: integer): double;
var
  loop: integer;
  mult: double;
begin
  mult := 1;
  for loop := 1 to X do
    mult := mult * loop;
  Fact := mult;
end;

function Perm(n, r: integer): double;
begin
  Perm := Fact(n) / Fact(n - r);
end;

function Comb(n, r: integer): double;
begin
  Comb := Perm(n, r) / Fact(r);
end;

function StrAng(Angle: Float): string;
var
  Grad, Min, Sec:    Float;
  sGrad, sMin, sSec: string;
begin
  Grad   := Int(Angle);
  sGrad  := FloatToStr(Grad);
  //Str(Grad: 3: 0, sGrad);
  Min    := Frac(Angle) * 60;
  sMin   := FloatToStr(Min);
  //Str(Min: 2: 0, string(sMin));
  Sec    := Frac(Min) * 60;
  sSec   := FloatToStr(Sec);
  //Str(Sec: 2: 0, string(sSec));
  Result := sGrad + 'ø' + sMin + '''' + sSec + '"';
end; // StrAng

function NameOnly(const AFileName: TFileName): TFileName;
begin
  Result := ChangeFileExt(ExtractFileName(AFileName), '');
end;

function ReductionToRange(LowLimit, Value, HighLimit: integer): integer;

begin
  Result := Min(Max(LowLimit, Value), HighLimit);
end;

function StrToVal(S: string): double;
begin
  try
    Result := StrToFloat(S);
  except
    if Pos(',', S) > 0 then
      S[Pos(',', S)] := '.'
    else if Pos('.', S) > 0 then
      S[Pos('.', S)] := ',';
    try
      Result := StrToFloat(S);
    except
      Result := 0;
    end;
  end;
end;

function ValToStr(AValue: double): string;
var
  S, Sv: string;
begin
  SV := FloatToStr(0.5);
  System.Str(AValue: 0: 2, S);
  S[Pos('.', S)] := SV[2];
  Result := S;
end;

function ExtractCharQuotedStr(const Src: string; Quote: char): string;
var
  I:     integer;
  Count: integer;
  CopyQuote: boolean;
begin
  Result := Src;
  if (Src = '') or (Src[1] <> Quote) then
    Exit;
  CopyQuote := False;
  Count     := 0;
  for I := 2 to Length(Src) - 1 do
  begin
    if (Src[I] <> Quote) or CopyQuote then
    begin
      Inc(Count);
      Result[Count] := Src[I];
    end;
    CopyQuote := (Src[I] = Quote) and (not CopyQuote);
  end;
  SetLength(Result, Count);
end;


function CharQuotedStr(const Src: string; Quote: char = ''''): string;
var
  I:     integer;
  Count: integer;
begin
  Result := Src;
  Count  := 2;
  for I := 1 to Length(Src) do
  begin
    if Src[I] = Quote then
      Inc(Count);
  end;
  SetLength(Result, Length(Src) + Count);
  Count := 1;
  Result[Count] := Quote;
  for I := 1 to Length(Src) do
  begin
    Inc(Count);
    Result[Count] := Src[I];
    if Src[I] = Quote then
    begin
      Inc(Count);
      Result[Count] := Src[I];
    end;
  end;
  Inc(Count);
  Result[Count] := Quote;
end;

procedure GetHistogram(var f: array of integer; Lo, Hi, MaxData, MaxClass: integer;
  Data: array of single);
var
  I, J: integer;
  Step: integer;
begin
  Step := round((Hi - Lo) / MaxClass);
  for I := 1 to MaxData do
    for J := 1 to MaxClass do
    begin
      if (Data[I] >= (J - 1) * Step) and (Data[I] < J * Step) then
        f[J] := f[J] + 1;
    end;
end;

end.
