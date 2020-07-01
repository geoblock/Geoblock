//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
{! Additional geoblock vector geometry routines }

unit GBGeometry;

interface

uses
  System.Math,
  System.Math.Vectors,

  
  GLVectorTypes,
  GLVectorGeometry;

type
  TVector2i = array [0..1] of longint;
  TVector2f = array [0..1] of single;
  TVector2d = array [0..1] of double;

  TSegment2D = record
    case byte of
      0: (StartPoint, EndPoint: TVector2D);
      1: (XStart, YStart, XEnd, YEnd: double);
  end;

  TSegment3D = record
    case byte of
      0: (StartPoint, EndPoint: TVector3d);
      1: (XStart, YStart, ZStart,
        XEnd, YEnd, Zend: double);
  end;


type
  TPolygonArr2d = array of TVector2D;
  TPolygonArr3d = array of TVector3d;

function MakeVector4d(V: array of double): TVector4d;
function MakeVector3d(V: array of double): TVector3d;

function GetNormal(V1, V2: TVector4d): TVector4d; overload;
function GetNormal(V1, V2: TVector3d): TVector3d; overload;

function CreateTranslationMatrix4d(V: TVector3d): TMatrix4d; overload;

function Vector4dTo3d(V: TVector4d): TVector3d;
function Vector4dTo2d(V: TVector4d): TVector2d;
function Vector3dTo4d(V: TVector3d): TVector4d;

function VectorTransform4d(V: TVector4d; M: TMatrix4d): TVector4d; overload;
function NormalizeVector3d(V: TVector3d): TVector3d; overload;
function CreateRotationMatrix4d(Axis: TVector3d; Angle: double): TMatrix4d; overload;

function MultiplyMatrix4d(M1, M2: TMatrix4d): TMatrix4d; overload;

function Polygon3dTo2d(Polygon: TPolygonArr3d; var M: TMatrix4d): TPolygonArr2d;
function PntInPoly(Polygon: array of TSegment3D; Point: array of single;
  SegmentCount: integer): boolean;
function GetPolygonArea(Polygon: TPolygonArr2d): extended;

//=============================================================================
implementation
//=============================================================================

function MakeVector4d(V: array of double): TVector4d;
begin
  Result := MakeDblVector(V);
end;

// create a vector from given values
function MakeVector3d(V: array of double): TVector3d;
         // EAX contains address of V
         // ECX contains address to result vector
         // EDX contains highest index of V

asm
         PUSH    EDI
         PUSH    ESI
         MOV     EDI,ECX
         MOV     ESI,EAX
         MOV     ECX,6
         REP     MOVSD
         POP     ESI
         POP     EDI
end;

function GetNormal(V1, V2: TVector4d): TVector4d;
begin
  Result.X := V1.V[1] * V2.V[2] - V1.V[2] * V2.V[1];
  Result.Y := V1.V[2] * V2.V[0] - V1.V[0] * V2.V[2];
  Result.Z := V1.V[0] * V2.V[1] - V1.V[1] * V2.V[0];
  Result.W := 1;
end;

function GetNormal(V1, V2: TVector3d): TVector3d;
begin
  Result.X := V1.V[1] * V2.V[2] - V1.V[2] * V2.V[1];
  Result.Y := V1.V[2] * V2.V[0] - V1.V[0] * V2.V[2];
  Result.Z := V1.V[0] * V2.V[1] - V1.V[1] * V2.V[0];
end;

function CreateTranslationMatrix4d(V: TVector3d): TMatrix4d;
  // create translation matrix

begin
  Result := IdentityHmgDblMatrix;
  Result.V[3].X := V.X;
  Result.V[3].Y := V.Y;
  Result.V[3].Z := V.Z;
end;

function Vector4dTo3d(V: TVector4d): TVector3d;
begin
  Result.V[0] := V.V[0] / V.V[3];
  Result.V[1] := V.V[1] / V.V[3];
  Result.V[2] := V.V[2] / V.V[3];
end;

function Vector4dTo2d(V: TVector4d): TVector2d;
begin
  Result[0] := V.V[0] / V.V[3];
  Result[1] := V.V[1] / V.V[3];
end;

function Vector3dTo4d(V: TVector3d): TVector4d;
begin
  Result.V[0] := V.V[0];
  Result.V[1] := V.V[1];
  Result.V[2] := V.V[2];
  Result.V[3] := 1;
end;

//* transform a homogeneous vector by multiplying it with a matrix
function VectorTransform4d(V: TVector4d; M: TMatrix4d): TVector4d; register;

var
  TV: TVector4d;

begin
  TV.V[0]  := V.V[0] * M.V[0].V[0] + V.V[1] * M.V[1].V[0] + V.V[2] * M.V[2].V[0] + V.V[3] * M.V[3].V[0];
  TV.V[1]  := V.V[0] * M.V[0].V[1] + V.V[1] * M.V[1].V[1] + V.V[2] * M.V[2].V[1] + V.V[3] * M.V[3].V[1];
  TV.V[2]  := V.V[0] * M.V[0].V[2] + V.V[1] * M.V[1].V[2] + V.V[2] * M.V[2].V[2] + V.V[3] * M.V[3].V[2];
  TV.V[3]  := V.V[0] * M.V[0].V[3] + V.V[1] * M.V[1].V[3] + V.V[2] * M.V[2].V[3] + V.V[3] * M.V[3].V[3];
  Result := TV;
end;

function NormalizeVector3d(V: TVector3d): TVector3d; overload;
var
  Len: extended;
begin
  Len := Norm(V.V);
  Result.V[0] := V.V[0] / Len;
  Result.V[1] := V.V[1] / Len;
  Result.V[2] := V.V[2] / Len;
end;

{* Create a rotation matrix along the given axis by the given angle in radians.
   The axis is a set of direction cosines}
function CreateRotationMatrix4d(Axis: TVector3d; Angle: double): TMatrix4d;

var
  Cosine, Sine, one_minus_cosine: extended;

begin
  SinCos(Angle, Sine, Cosine);
  one_minus_cosine := 1 - cosine;

  Result.V[0].V[0] := SQR(Axis.V[0]) + (1 - SQR(axis.V[0])) * cosine;
  Result.V[0].V[1] := Axis.V[0] * Axis.V[1] * one_minus_cosine + Axis.V[2] * sine;
  Result.V[0].V[2] := Axis.V[0] * Axis.V[2] * one_minus_cosine - Axis.V[1] * sine;
  Result.V[0].V[3] := 0;

  Result.V[1].V[0] := Axis.V[0] * Axis.V[1] * one_minus_cosine - Axis.V[2] * sine;
  Result.V[1].V[1] := SQR(Axis.V[1]) + (1 - SQR(axis.V[1])) * cosine;
  Result.V[1].V[2] := Axis.V[1] * Axis.V[2] * one_minus_cosine + Axis.V[0] * sine;
  Result.V[1].V[3] := 0;

  Result.V[2].V[0] := Axis.V[0] * Axis.V[2] * one_minus_cosine + Axis.V[1] * sine;
  Result.V[2].V[1] := Axis.V[1] * Axis.V[2] * one_minus_cosine - Axis.V[0] * sine;
  Result.V[2].V[2] := SQR(Axis.V[2]) + (1 - SQR(axis.V[2])) * cosine;
  Result.V[2].V[3] := 0;

  Result.V[3].V[0] := 0;
  Result.V[3].V[1] := 0;
  Result.V[3].V[2] := 0;
  Result.V[3].V[3] := 1;
end;

function MultiplyMatrix4d(M1, M2: TMatrix4d): TMatrix4d; overload;
  // multiply two 4x4 matrices

var
  I, J: integer;
  TM:   TMatrix4d;

begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      TM.V[I].V[J] := M1.V[I].V[0] * M2.V[0].V[J] + M1.V[I].V[1] * M2.V[1].V[J] +
        M1.V[I].V[2] * M2.V[2].V[J] + M1.V[I].V[3] * M2.V[3].V[J];
  Result := TM;
end;

{! Polygon3dTo2d function
1. With three polygon vertices (that not lie on line, n<>(0;0;0))
   we get normal n to the polygon,
2. alpha=arccos(z/|n|), where alpha - rotation angle,
                            z    - z of normal,
                            |n|   - normal length
3. r = Oz x n, где r  - axes of rotation,
                   Oz - basis along z (0;0;1)
4. transformation of data:
  а) move origin of coordinates to the polygon center,
  б) rotate polygon and the point around r to -alpha degrees;
}

function Polygon3dTo2d(Polygon: TPolygonArr3d; var M: TMatrix4d): TPolygonArr2d;
var
  N:     TVector3d; //Normal to polygon
  R:     TVector3d; //Rotation axis
  O:     TVector3d; //Polygon centre
  I, J, K: integer;
  Alpha: double;
  Vector3f: TVector3f;
  Vector4d: TVector4d;

begin
  {1} K := High(Polygon) - 1;
  J     := K + 1;
  I     := 0;
  repeat
    N := GetNormal(MakeVector3d([Polygon[K].V[0] - Polygon[J].V[0],
      Polygon[K].V[1] - Polygon[J].V[1], Polygon[K].V[2] - Polygon[J].V[2]]),
      MakeVector3d([Polygon[I].V[0] - Polygon[J].V[0], Polygon[I].V[1] -
      Polygon[J].V[1], Polygon[I].V[2] - Polygon[J].V[2]]));
    K := J;
    J := I;
    I := I + 1;
    Vector3f.V[0] := N.V[0];
    Vector3f.V[1] := N.V[1];
    Vector3f.V[2] := N.V[2];
  until (I > High(Polygon)) or (VectorNorm(Vector3f) > 1E-9);

  {2} Alpha := ArcCos(N.V[2] / VectorNorm(Vector3f));
  {3} R     := GetNormal(N, MakeVector3d([0, 0, 1]));
  {4} M     := IdentityHmgDblMatrix;
  {a} O     := MakeVector3d([0, 0, 0]);
  for i := 0 to High(Polygon) do
  begin
    O.V[0] := O.V[0] + Polygon[I].V[0];
    O.V[1] := O.V[1] + Polygon[I].V[1];
    O.V[2] := O.V[2] + Polygon[I].V[2];
  end;
  O.V[0] := -O.V[0] / (High(Polygon) + 1);
  O.V[1] := -O.V[1] / (High(Polygon) + 1);
  O.V[2] := -O.V[2] / (High(Polygon) + 1);

  M := CreateTranslationMatrix4d(O);

  {б} R := NormalizeVector3d(R);
  M     := MultiplyMatrix4d(M, CreateRotationMatrix4d(R, Alpha));

  for I := 0 to High(Polygon) do
  begin
    Vector4d   := Vector3dTo4d(Polygon[I]);
    Polygon[I] := Vector4dTo3d(VectorTransform4d(Vector4d, M));
  end;
  SetLength(Result, High(Polygon) + 1);
  for I := 0 to High(Polygon) do
  begin
    Result[I] := Vector4dTo2d(VectorTransform4d(Vector3dTo4d(Polygon[I]), M));
  end;
end;

 //* The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
 // with some minor modifications for speed.  It returns 1 for strictly
 // interior points, 0 for strictly exterior, and 0 or 1 for points on
 // the boundary.
function PntInPoly(Polygon: array of TSegment3D; Point: array of single;
  SegmentCount: integer): boolean;
(*
 TSegment3D = record
  case Byte of
      0 : (StartPoint, EndPoint: TVector3d);
      1 : (XStart, YStart, ZStart,
           XEnd, YEnd, Zend : Double);
  end;
*)
var
  I: integer;
begin
  Result := False;
  for I := 0 to SegmentCount - 1 do
  begin
    if ((((Polygon[I].YStart <= Point[1]) and (Point[1] < Polygon[I].YEnd)) or
      ((Polygon[I].YEnd <= Point[1]) and (Point[1] < Polygon[I].YStart))) and
      (Point[0] < (Polygon[I].XEnd - Polygon[I].XStart) *
      (Point[1] - Polygon[I].YStart) / (Polygon[I].YEnd - Polygon[I].YStart) +
      Polygon[I].XStart)) then
      Result := not Result;
  end;
end;

function GetPolygonArea(Polygon: TPolygonArr2d): extended;
var
  I, J: integer;
begin
  Result := 0;
  J      := High(Polygon);
  for I := 0 to High(Polygon) do
  begin
    Result := Result + (Polygon[I, 1] + Polygon[J, 1]) *
      (Polygon[I, 0] - Polygon[J, 0]);
    J      := I;
  end;
  Result := Abs(Result / 2);
end;


end.
