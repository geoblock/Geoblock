//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
{
  Polynomial Regression Interpolation
}
unit uPolynomialRegression;

interface

uses
  System.SysUtils,
  System.Math,
  System.Classes,

  uGlobals,
  uCommon,
  uProfuns,
  uInterpol,
  uSorting;

function PolyRegressInterpolation(PointsAr, NodesAr: TCoordinateArray;
  order: integer): TCoordinateArray;

function CalcKoef(A: TMatrix2D; V: TVectorD): TVectorD;
function ChengeCol(Al: TMatrix2D; V: TVectorD; k: integer): TMatrix2D;
function MultVectors(A, B: TVectorD): TVectorD;
function GetVector2D(order: integer; X, Y: TVectorD): TMatrix2D;
function GetVector3D(order: integer; X, Y, Z: TVectorD): TMatrix2D;
function DetMatrix(Arr: TMatrix2D): double;
function Del_RowCol(A: TMatrix2D; l: integer): TMatrix2D;

type
  TDoubleArray = array of double;
  TMatrixData  = array of array of extended;

var
//  Vect: TMatrixD;// array of TDoubleArray; //Main Vector
  si: integer;
//Size of Vect
const
  Vect2DSize: array [1..4] of integer = (3, 6, 10, 15);
  Vect3DSize: array [1..4] of integer = (4, 10, 19, 31);

//=========================================================================
implementation
//=========================================================================

 //Main function
function PolyRegressInterpolation(PointsAr, NodesAr: TCoordinateArray;
  order: integer): TCoordinateArray;
var
  I, J: integer; //vars for iterations

  Px, Py, Pz, Pv: TVectorD;//massive of Points cordinate

  Re:    double;
  VectSize: integer;//Size of main Vector
  NodeX, NodeY, NodeZ, NodeV: TVectorD; //massive of Nodes cordinate
  CoefB: TVectorD;
  SumsMatrix: TMatrix2D;// Matrix of sums of vectors;
  Vect:  TMatrix2D;
  CalcMatrix: TMatrix2D;
  SumsVector: TVectorD; //Vector of sums VALUES

  PointsCount, NodesCount: integer;// Count of Points
begin
  PointsCount := length(PointsAr);
  NodesCount  := length(NodesAr);

  // Points
  //Set massives length
  SetLength(Px, PointsCount);
  try
    SetLength(Py, PointsCount);
    SetLength(Pz, PointsCount);
    try
      SetLength(Pv, PointsCount);
      //Load Points Coordinates into massives

      for I := 0 to PointsCount - 1 do
      begin
        Px[I] := PointsAr[I].X;
        Py[I] := PointsAr[I].Y;
        Pz[I] := PointsAr[I].Z;
        Pv[I] := PointsAr[I].Value;//???? m.b. FieldName

      end;
      //Nodes
      //Set massives length
      SetLength(NodeX, NodesCount);
      SetLength(NodeY, NodesCount);
      SetLength(NodeZ, NodesCount);
      SetLength(NodeV, NodesCount);

      //Load Nodes Coordinates into massives

      for I := 0 to NodesCount - 1 do
      begin
        NodeX[I] := NodesAr[I].X;
        NodeY[I] := NodesAr[I].Y;
        NodeZ[I] := NodesAr[I].Z;
        //  NodeV[I]:=TableNodes.FieldValues[fldZ];//???? m.b. FieldName

      end;

      //Routain
      //Getting memory for Main Vector: Vect

      if Mode3D then
      begin //for 3d points
        VectSize := Vect3DSize[order];
        SetLength(Vect, VectSize, PointsCount);
        Vect := GetVector3D(order, Px, Py, Pz);
      end
      else
      begin  //for 2D points
        VectSize := Vect2DSize[order];
        SetLength(Vect, VectSize, PointsCount);
        Vect := GetVector2D(order, Px, Py);
      end;

      ////Get Matrix of Sums of Vectors
      SetLength(SumsMatrix, VectSize, VectSize);
      for i := 0 to VectSize - 1 do
      begin
        for j := 0 to VectSize - 1 do
        begin

          SumsMatrix[i, j] := Sum(MultVectors(Vect[i], Vect[j]));
        end;

      end;

      ///Get Vector of Sums Values
      SetLength(SumsVector, VectSize);
      for i := 0 to VectSize - 1 do
      begin
        SumsVector[i] := Sum(MultVectors(Vect[i], Pv));
      end;
      // Calculating Coeficients B
      SetLength(CoefB, VectSize);
      CoefB := CalcKoef(SumsMatrix, SumsVector);

      SetLength(CalcMatrix, VectSize, NodesCount);
      if Mode3D then
      begin
        CalcMatrix := GetVector3D(order, NodeX, NodeY, NodeZ);
      end
      else
      begin
        CalcMatrix := GetVector2D(order, NodeX, NodeY);
      end;
      //Calc Node Values
      for i := 0 to NodesCount - 1 do
      begin
        Re := 0;
        for j := 0 to VectSize - 1 do
        begin
          Re := Re + CalcMatrix[j, i] * CoefB[j];
        end;

        NodesAr[i].Value := Re;
      end;
      ///
      CalcMatrix := nil;
      CoefB      := nil;
      ////////////////// end of function
    finally
      Pv := nil;
    end;

  finally
    Py := nil;
  end;
  Px    := nil;
  Pz    := nil;
  NodeX := nil;
  NodeY := nil;
  NodeZ := nil;
  NodeV := nil;

  Result := NodesAr;
end;

///////////////////////////////////
//Multiplication elements of two vectors !! index begin from 0 i.e. A[0]
//for Main function
function MultVectors(A, B: TVectorD): TVectorD;
var
  size, i: integer;
begin
  size := length(A);
  SetLength(Result, size);

  for i := 0 to size - 1 do
    Result[i] := A[i] * B[i];
end;

//////////////////////////////////
//for Main Function
function GetVector2D(order: integer; X, Y: TVectorD): TMatrix2D;
var
  i, size: integer;
  OneVect: TVectorD;
  Vect:    TMatrix2D;
  //value, x2, x3, x4, y2, y3, y4, z2, z3, z4: TDoubleArray;
  // must be in uGlobals vars

begin
  //Get size of vectors
  size := length(X);

  // Vector of ones
  SetLength(OneVect, size);
  for i := 0 to size - 1 do
    OneVect[i] := 1;

  SetLength(Result, Vect2DSize[order], size);

  //Main Vector
  case order of
    1:
    begin
      Result[0] := OneVect;
      Result[1] := X;
      Result[2] := Y;
    end;
    2:
    begin
      Result[0] := OneVect;
      Result[1] := X;
      Result[2] := Y;
      Result[3] := MultVectors(X, X);
      Result[4] := MultVectors(X, Y);
      Result[5] := MultVectors(Y, Y);
    end;
    3:
    begin
      Result[0] := OneVect;
      Result[1] := X;
      Result[2] := Y;
      Result[3] := MultVectors(X, X);
      Result[4] := MultVectors(X, Y);
      Result[5] := MultVectors(Y, Y);
      Result[6] := MultVectors(X, MultVectors(X, X));
      Result[7] := MultVectors(X, MultVectors(X, Y));
      Result[8] := MultVectors(X, MultVectors(Y, Y));
      Result[9] := MultVectors(Y, MultVectors(Y, Y));
    end;
    4:
    begin
      Result[0]  := OneVect;
      Result[1]  := X;
      Result[2]  := Y;
      Result[3]  := MultVectors(X, X);
      Result[4]  := MultVectors(X, Y);
      Result[5]  := MultVectors(Y, Y);
      Result[6]  := MultVectors(X, MultVectors(X, X));
      Result[7]  := MultVectors(X, MultVectors(X, Y));
      Result[8]  := MultVectors(X, MultVectors(Y, Y));
      Result[9]  := MultVectors(Y, MultVectors(Y, Y));
      Result[10] := MultVectors(MultVectors(X, X), MultVectors(X, X));
      Result[11] := MultVectors(MultVectors(X, X), MultVectors(X, Y));
      Result[12] := MultVectors(MultVectors(X, X), MultVectors(Y, Y));
      Result[13] := MultVectors(MultVectors(X, Y), MultVectors(Y, Y));
      Result[14] := MultVectors(MultVectors(Y, Y), MultVectors(Y, Y));
    end;
  end;
  OneVect := nil;
end;

 //////////////////////////////////
 //for Main Function
function GetVector3D(order: integer; X, Y, Z: TVectorD): TMatrix2D;
var
  i, size: integer;
  OneVect: TVectorD;
  Vect:    TMatrix2D;
  //value, x2, x3, x4, y2, y3, y4, z2, z3, z4: TDoubleArray;
  // must be in uGlobals vars

begin
  //Get size of vectors
  size := length(X);

  // Vector of ones
  SetLength(OneVect, size);
  for i := 0 to size - 1 do
    OneVect[i] := 1;

  SetLength(Result, Vect3DSize[order], size);

  //Main Vector
  case order of
    1:
    begin
      Result[0] := OneVect;
      Result[1] := X;
      Result[2] := Y;
      Result[3] := Z;
    end;
    2:
    begin
      Result[0] := OneVect;
      Result[1] := X;
      Result[2] := Y;
      Result[3] := Z;
      Result[4] := MultVectors(X, Y);
      Result[5] := MultVectors(X, Z);
      Result[6] := MultVectors(Y, Z);
      Result[7] := MultVectors(X, X);
      Result[8] := MultVectors(Y, Y);
      Result[9] := MultVectors(Z, Z);
    end;
    3:
    begin
      Result[0]  := OneVect;
      Result[1]  := X;
      Result[2]  := Y;
      Result[3]  := Z;
      Result[4]  := MultVectors(X, Y);
      Result[5]  := MultVectors(X, Z);
      Result[6]  := MultVectors(Y, Z);
      Result[7]  := MultVectors(X, X);
      Result[8]  := MultVectors(Y, Y);
      Result[9]  := MultVectors(Z, Z);
      Result[10] := MultVectors(Y, MultVectors(X, X));
      Result[11] := MultVectors(X, MultVectors(Y, Y));
      Result[12] := MultVectors(Z, MultVectors(X, X));
      Result[13] := MultVectors(X, MultVectors(Z, Z));
      Result[14] := MultVectors(Z, MultVectors(Y, Y));
      Result[15] := MultVectors(Y, MultVectors(Z, Z));
      Result[16] := MultVectors(X, MultVectors(X, X));
      Result[17] := MultVectors(Y, MultVectors(Y, Y));
      Result[18] := MultVectors(Z, MultVectors(Z, Z));
    end;
    4:
    begin
      Result[0]  := OneVect;
      Result[1]  := X;
      Result[2]  := Y;
      Result[3]  := Z;
      Result[4]  := MultVectors(X, Y);
      Result[5]  := MultVectors(X, Z);
      Result[6]  := MultVectors(Y, Z);
      Result[7]  := MultVectors(X, X);
      Result[8]  := MultVectors(Y, Y);
      Result[9]  := MultVectors(Z, Z);
      Result[10] := MultVectors(Y, MultVectors(X, X));
      Result[11] := MultVectors(X, MultVectors(Y, Y));
      Result[12] := MultVectors(Z, MultVectors(X, X));
      Result[13] := MultVectors(X, MultVectors(Z, Z));
      Result[14] := MultVectors(Z, MultVectors(Y, Y));
      Result[15] := MultVectors(Y, MultVectors(Z, Z));
      Result[16] := MultVectors(X, MultVectors(X, X));
      Result[17] := MultVectors(Y, MultVectors(Y, Y));
      Result[18] := MultVectors(Z, MultVectors(Z, Z));
      Result[19] := MultVectors(MultVectors(X, X), MultVectors(X, Y));
      Result[20] := MultVectors(MultVectors(X, X), MultVectors(Y, Y));
      Result[21] := MultVectors(MultVectors(X, Y), MultVectors(Y, Y));
      Result[22] := MultVectors(MultVectors(X, X), MultVectors(X, Z));
      Result[23] := MultVectors(MultVectors(X, X), MultVectors(Z, Z));
      Result[24] := MultVectors(MultVectors(X, Z), MultVectors(Z, Z));
      Result[25] := MultVectors(MultVectors(Y, Y), MultVectors(Y, Z));
      Result[26] := MultVectors(MultVectors(Y, Y), MultVectors(Z, Z));
      Result[27] := MultVectors(MultVectors(Y, Z), MultVectors(Z, Z));
      Result[28] := MultVectors(MultVectors(X, X), MultVectors(X, X));
      Result[29] := MultVectors(MultVectors(Y, Y), MultVectors(Y, Y));
      Result[30] := MultVectors(MultVectors(Z, Z), MultVectors(Z, Z));
    end;
  end;
  OneVect := nil;
end;

 /////////////////////////////////////
 //for CalcKoef function
 //Delete 0 Row and i Colomn from Matrix
function Del_RowCol(A: TMatrix2D; l: integer): TMatrix2D;
var
  i, j, p, k, dim: integer;
  B: TMatrix2D;
begin
  dim := Length(A);
  SetLength(B, dim - 1, dim - 1);

  for i := 1 to dim - 1 do
  begin
    p := 0;
    for j := 0 to dim - 1 do
    begin
      if j <> l then
      begin
        B[i - 1, p] := A[i, j];
        p := p + 1;
      end;
    end;
  end;

 {for i:=0 to dim-1 do begin
   k:=0;
   for j:=0 to dim-1 do
   begin
    if i<>l then
      begin
      if j<>t then
        begin
          begin
          B[p-i,k]:=A[i,j];
          k:=k+1;
          end;
        end;
   end;
  p:=p+1;
 end; }
  Result := B;
  B      := nil;
end;

 /////////////////////////////////////
 //Determinant of Matrix
function DetMatrix(Arr: TMatrix2D): double;
var
  Temp, A: TMatrixData;
  Cols, Rows, Count: word;
  i, j, k: integer;
begin {det}
  Count  := Length(Arr);
  Result := 1;
  SetLength(A, Count, Count);
  SetLength(Temp, 1, Count);
  for i := 0 to Count - 1 do
    for j := 0 to Count - 1 do
      A[i, j] := Arr[i, j];
  for i := 0 to Count - 2 do {Start of transposition to the high triangular view}
  begin
    for j := i to Count - 1 do                                 {*  Search   }
    begin                                                      {*  of null  }
      Rows := 0;                                               {*  строк    }
      Cols := 0;                                               {*  and      }
      for k := i to Count - 1 do                               {*  столбцов }
      begin                                                    {*  in       }
        Rows := Rows + Ord(A[j, k] = 0);                       {*  matrix   }
        Cols := Cols + Ord(A[k, j] = 0);                       {*           }
      end;                                                     {*           }
      if Rows + Cols = 0 then
        Break;
      if (Cols = Count - i) or (Rows = Count - i) then
      begin
        Result := 0;
        Exit;
      end;
    end;
    if A[i, i] = 0 then
      for j := i + 1 to Count - 1 do
        if A[j, i] <> 0 then
        begin
          Result  := -Result;                  {* меняем строку              }
          Temp[0] := A[i];                     {* на строку с                }
          A[i]    := A[j];                     {* первым                     }
          A[j]    := Temp[0];                  {* ненулевым                  }
          Break;                               {* элементом                  }
        end;
    for j := i + 1 to Count - 1 do
      if A[j, i] <> 0 then
      begin
        for k := i + 1 to Count - 1 do
          A[j, k] := A[j, k] - A[i, k] * A[j, i] / A[i, i];
        A[j, i] := 0;
      end;
  end; {of transposition}
  for i := 0 to Count - 1 do     { Определитель как произведение }
    Result := Result * A[i, i];  { элементов на главной диагонали}
end;

 //////////////////////////////////////
 //for CalcKoef function
 ////////////////////////////////////////
 //for CalcKoef function
 //Changing i Colomn of Matrix to Vector V
function ChengeCol(Al: TMatrix2D; V: TVectorD; k: integer): TMatrix2D;

var
  i, j: integer;
  R:    TMatrix2D;
begin
  SetLength(R, length(Al), length(Al));
  for i := 0 to length(Al) - 1 do
  begin
    for j := 0 to length(Al) - 1 do
    begin
      R[i, j] := Al[i, j];
    end;
  end;
  for i := 0 to length(R) - 1 do
  begin
    R[i, k] := V[i];
  end;
  Result := R;
  R      := nil;
end;


 ///////////////////////////////////
 //Calc linear
function CalcKoef(A: TMatrix2D; V: TVectorD): TVectorD;

var
  Ai, AA: TMatrix2D;
  B:      TVectorD;
  det, di: double;
  dim, i, j: integer;
begin
  dim := length(A);
  SetLength(AA, dim, dim);
  SetLength(B, dim);

  for i := 0 to dim - 1 do
  begin
    for j := 0 to dim - 1 do
    begin
      AA[i, j] := A[i, j];
    end;
  end;
  det := DetMatrix(AA);
  for i := 0 to dim - 1 do
  begin
    SetLength(Ai, dim, dim);
    Ai   := ChengeCol(AA, V, i);
    di   := DetMatrix(Ai);
    B[i] := di / det;
    Ai   := nil;
  end;
  Result := B;
  B      := nil;
  AA     := nil;
 end;

end.
 {//Getting Matrix Determenant
 function DetMatrixOld(A: TMatrixD): double;
 var
 At:TMatrixD;
  det: double;
  dim, i, j: integer;}{ Счетчики }{  mn:integer;   { Множитель, определяющий знак }
 {  num, num2:integer;  { Количество оставшихся столбцов частных матриц }
{  p:double;       { Промежуточная переменная }{  re:double;     { Результат }{  begin
  //rez:=0;
 //det:=0;
 num2:=length(A);
 SetLength(At,num2,num2);
    for i:=0 to num2-1 do begin
      for j:=0 to num2-1 do At[i,j]:=A[i,j];
    end;

re:=1 ;
  num:=num2;
{ Уменьшение размера матрицы до 2х2 }{  while (num>2) do begin
  { Поиск ененулевого элемента }{    j:=0;
    while (At[num-1,j]=0)and(j<(num-1)) do j:=j+1;
  { Поиск наиболее близкого к единице ненулевого элемента }{    for i:=0 to num-1 do
      if (abs(At[num-1,i]-1)<abs(At[num-1,j]-1))and(At[num-1,i]<>0) then j:=i;
  { Проверка наличия такового }{    if (At[num-1,j]<>0) then begin
  { Выяснение знака текущего множителя }{      if ((j-1) mod 2 = (num-1) mod 2) then mn:=-1 else mn:=1;
  { Перемена мест столбцов матрицы так чтобы выбранный элемент стал
     крайним правым нижним }{      for i:=0 to num-1 do begin
        p:=At[i,j];
        At[i,j]:=At[i,num-1];
        At[i,num-1]:=p;
      end;
  { Прибавление ко всем столбцам матрицы крайнего левого умноженного на
    множитель с целью получения нулей в нижней строке кроме правого элемента }{+      for i:=0 to num-2 do begin
        p:=-At[num-1,i]/At[num-1,num-1];
        for j:=0 to num-1 do
          At[j,i]:=At[j,i]+At[j,num-1]*p;
      end;
  { Учитывается множитель. Значение элемента сохраняется в самой матрице }{      At[num-1,num-1]:=At[num-1,num-1]*mn;
    end else At[num-1,num-1]:=0;
    num:=num-1;
  end;
{ Вычисление определителя оставшейся матрицы и умножение на все промежуточные
  множители }{  re:=At[0,0]*At[1,1]-At[0,1]*At[1,0];
  for i:=2 to num2-1 do re:=re*At[i,i];

 result :=re;
 At:=nil;
end;
} {case dim of
 2: det:=A[0,0]*A[1,1]-A[0,1]*A[1,0];
 3: det:=A[0,0]*A[1,1]*A[2,2]+A[0,1]*A[1,2]*A[2,0]+A[1,0]*A[0,2]*A[2,1]-A[0,2]*A[1,1]*A[2,0]-A[0,1]*A[2,2]*A[1,0]-A[0,0]*A[1,2]*A[2,1];
 4: begin
    det:=A[0,0]*(A[1,1]*A[2,2]*A[3,3]+A[1,2]*A[2,3]*A[3,1]+A[2,1]*A[1,3]*A[3,2]-A[1,3]*A[2,2]*A[3,1]-A[1,2]*A[3,3]*A[2,1]-A[1,1]*A[2,3]*A[3,2]) -A[0,1]*(A[1,0]*A[2,2]*A[3,3]+A[1,2]*A[2,3]*A[3,0]+A[2,0]*A[1,3]*A[3,2]-A[1,3]*A[2,2]*A[3,0]-A[1,2]*A[3,3]*A[2,0]-A[1,0]*A[2,3]*A[3,2] )+A[0,2]*(A[1,0]*A[2,1]*A[3,3]+A[1,1]*A[2,3]*A[3,0]+A[2,0]*A[1,3]*A[3,1]-A[1,3]*A[2,1]*A[3,0]-A[1,1]*A[3,3]*A[2,0]-A[1,0]*A[2,3]*A[3,1] )-A[0,3]*(A[1,0]*A[2,1]*A[3,2]+A[1,1]*A[2,2]*A[3,0]+A[2,0]*A[1,2]*A[3,1]-A[1,2]*A[2,1]*A[3,0]-A[1,1]*A[3,2]*A[2,0]-A[1,0]*A[2,2]*A[3,1]);
    end;

 else
    begin
    for i:=0 to dim-1 do
      begin
        det:=det+Power(-1,i+2)*A[0,i]*DetMatrix(Del_RowCol(A,i));

      end;


    end;
  end;
  result:=det;
  end;  }{
/////////////////////////////////////
//////////////////Main function old
function PolinomRegression(TablePoints, TableNodes: TFDTable;
  order: integer; FieldName: String; ProgressBar: TProgressBar): boolean;

  var
  I,J: integer; //vars for iterations

  Px, Py, Pz, Pv: TDoubleArray;//massive of Points cordinate

  NodeX, NodeY, NodeZ, NodeV : TDoubleArray; //massive of Nodes cordinate

  SumsMatrix: array of TDoubleArray;// Matrix of sums of vectors;
  SumsVector: TDoubleArray;
  /////////////////////
  PointsCount, NodesCount: integer;// Count of Points

  begin
  // only fo 2Grid
  PointsCount:=TablePoints.RecordCount;
  NodesCount:=TableNodes.RecordCount;

  // Points
  //Set massives length
  SetLength(Px, PointsCount);
  try
    SetLength(Py, PointsCount);
    try
    SetLength(Pv, PointsCount);
    //Load Points Coordinates into massives
    TablePoints.First;
    for I:=0 to PointsCount-1 do
    begin
      Px[I]:=TablePoints.FieldValues[fldX];
      Py[I]:=TablePoints.FieldValues[fldY];
      Pv[I]:=TablePoints.FieldValues[fldZ];//???? m.b. FieldName
    TablePoints.Next;
    end;
    //Nodes
   //Set massives length
   SetLength(NodeX, NodesCount);
   SetLength(NodeY, NodesCount);
   SetLength(NodeV, NodesCount);

   //Load Nodes Coordinates into massives
        TableNodes.First;
    for I:=0 to NodesCount-1 do
    begin
      NodeX[I]:=TableNodes.FieldValues[fldX];
      NodeY[I]:=TableNodes.FieldValues[fldY];
    //  NodeV[I]:=TableNodes.FieldValues[fldZ];//???? m.b. FieldName
    TableNodes.Next;
    end;
    ///////////////////////////////////
    //Routain
    //Getting memory for Main Vector: Vect
    SetLength(Vect,Vect2DSize[order],PointsCount);
    GetVector2D(order,Px,Py);

    ////Get Matrix of Sums of Vectors
    SetLength(SumsMatrix ,Vect2DSize[order],Vect2DSize[order]);
    for i:=0 to Vect2DSize[order]-1 do
      begin
      for j:=0 to Vect2DSize[order]-1 do
        begin

        SumsMatrix[i,j]:=Sum(MultVectors(Vect[i],Vect[j]));
        end;

      end;

    ///Get Vector of Sums Values
    SetLength(SumsVector ,Vect2DSize[order]);
      for i:=0 to Vect2DSize[order]-1 do
        begin
        SumsVector[i]:=Sum(MultVectors(Vect[i],Pv));
        end;

    ////////////////// end of function
    finally
    Pv:=nil;
    end;

  finally
   Py:=nil;
  end;
  Px:=nil;
  end;
}{
//////////////////////////////////////////
//Method Gaysa!!
function CalculateKoef(KoefMatrix : TMatrixD; ValueVector: TVectorD): TMatrixD;
var
dim: Integer;// Length of Matrix
i, j, L: integer;// iteration
A: TMatrixD; //
k: double;
begin
  dim:=Length(KoefMatrix);
  SetLength(A,dim,dim+1 );
  //Loading KoefMatrix into A
  for i:=0 to dim-1 do
    begin
      for j:=0 to dim-1 do
        begin
        A[i,j]:=KoefMatrix[i,j];
        end;
    end;
  //Loading ValueVector into A
  for i:=0 to dim-1 do
    begin
    A[i,dim]:=ValueVector[i];
    end;
  //Calculation

  for i:=0 to dim-2 do
    begin

    for j:=i to dim-2 do
      begin
      k:=-A[i,i]/A[j+1,i];
      for l:=0 to dim do
        begin
        A[j+1, l]:= k*A[j+1, l];
        A[j+1,l]:=A[j+1,l]+A[i,l];
        end;

      end;


    end;

Result:=A;
end;
}