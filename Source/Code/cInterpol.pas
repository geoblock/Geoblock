//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
(* Constants, types and vars for interpolation routines *)


unit cInterpol;

interface

uses
  System.SysUtils,
  System.Classes,
  cDiscoCore;

const
  NEAR_ZERO = -1E-15;
  NUMNEGATIVE = -1000000000;
  NumPositive = 1000000000;
  VAL_BLANK = NUMNEGATIVE;
  EPSLON = 0.00001;
  UNEST = -999.0;

  //const
  MAXCLASS = 255; // grade classes
  //  MAXNP = 100; // max number of total interpolated points
  //  MAXNH = 20; // variogramms steps
  //  MAXPOINTS = 100000;

  // Search options
  NORMAL_SEARCH = 0;
  OCTANT_SEARCH = 2;

  SB_NUM_BLOCK = 30;  // number of blocks for each dimension (for superblock.pas unit)

  // User Adjustable Parameters for Variograms
  MAXDIR = 5;     // Maximum number of directions
  MAXLAG = 200;   // Maximum number of lags

type
  //  TVectorf = array[0..MAXSHORT - 1] of Single;
  //  TMatrixf = array[0..MAXCLASS - 1] of TVectorf;

  // Data structures for experimental variograms and variogram models

(* The azimuth is measured in degrees clockwise from north, e.g., azm=0 is north,
   azm=90 is east, and azm=135 is south-east.
   The dip angle is measured in negative degrees down from horizontal i.e.,
   dip=0 is horizontal, dip=-90 is vertical downward, and dip=-45 is dipping down
   at 45 degrees.
   Bandwh is the horizontal ``bandwidth'' or maximum acceptable horizontal
   deviation from the direction vector.
   Bandwd is the vertical ``bandwidth''  or maximum acceptable deviation
   perpendicular to the dip direction in the vertical plane.
 *)

  TVariogramDirection = record
    Azimuth: double;                // azm - azimuth angle
    Azimuth_tolerance: double;      // atol - half window azimuth tolerance
    Azimuth_bandwidth: double;      // bandwh - azimuth bandwidth
    Dip:     double;                // dip - dip angle
    dip_tolerance: double;          // dtol - half window dip tolerance
    dip_bandwidth: double;          // bandwd - dip bandwidth
  end;

  PVariogramDirection = ^TVariogramDirection;

  TVariogramData = record
    Lag:      integer;                      // lag index
    Direction: integer;                     // direction index
    Distance: double;                       // average separation distance
    Value:    double;                       // semivariogram value
    Npair:    integer;                      // number of pairs in the lag
    Head_mean, tail_mean: double;
    Head_variance, tail_variance: double;   // for correlogram
  end;

(*  Variogram types:
1  - Traditional semivariogram
2  - Traditional cross semivariogram
3  - Covariance
4  - Correlogram
5  - General relative semivariogram
6  - Pairwise relative semivariogram
7  - Semivariogram of logarithms
8  - Semimadogram
9  - Indicator semivariogram (continuous variable)
10 - Indicator semivariogram (categorical variable)
*)

  TExpVariogram = record
    Var_type: integer;       // variogram type (see above)
    Standardized: boolean;
    // if true, semivariogram values will be divided by the variance
    Cross: integer;
    // =0,  direct variogram (tail and head values are taken from value);
    // =1,  tail value = value,  head value = value2;
    // =-1, tail value = value2, head value = value.
    NLag:  integer;           // number of lags
    Lag_distance: double;
    Lag_tolerance: double;
    Cut:   double;
    // cut value (for indicator semivariogram, var_type = 9 or 10)
    NDir:  integer;           // number of directions
    Directions: array [1..MAXDIR] of TVariogramDirection;
    DataV:  array of TVariogramData;
  end;

  TVariogramModelStructure = record
    Model_type: integer;
    Nugget:     double;
    Contribution: double;
    Range:      double;
    Anis1, Anis2: double;
    Azimuth, Dip, Plunge: double;
  end;

  PVariogramModelStructure = ^TVariogramModelStructure;

  TVariogramModel = record
    Nst_count:  integer;
    Structures: array [1..4] of TVariogramModelStructure;
  end;

  // Structures for receiving data into interpolation procedures
  TCoordinate = record
    ID:      integer;
    X, Y, Z:  double;     // coordinates
    Value:    double;
    Value2:   double;     // needed for cross variograms
    Variance: double;     // needed for kriging routines
    Error:    double;     // needed for cross validation
  end;

  TCoordinateArray = array of TCoordinate;

  PCoordinate = ^TCoordinate;

  TICoordinate = record
    X, Y, Z: integer;
  end;

  TICoordinateArray = array of TICoordinate;

  TGridPattern = record
    Size:    integer;
    Pattern: TICoordinateArray;
  end;

  TGridPatternArray = array of TGridPattern;

  TCoordList = record
    Item: PCoordinate;
    Next: Pointer;
  end;
  PCoordList = ^TCoordList;

  TCoordListArray = array[0..SB_NUM_BLOCK - 1, 0..SB_NUM_BLOCK - 1,
    0..SB_NUM_BLOCK - 1] of
    TCoordList;

  TCoordMatch = record
    ID: integer;
    Distance, Value: double;
  end;

  TCoordMatchArray = array of TCoordMatch;

  //for Polynomial Regression
  TVectorD = array of Single;
//  TMatrixD = array of TVectorD;
  TMatrix2D = array of TVectorD;
  TMatrix3D = array of TMatrix2D;


  { Data structure for Inverse Distance parameters }
  TInvDistPars = record
    ExtraValue: Double;
    Power: Integer;
    SearchMode: Integer;
    NP:    Integer;            //Minimum amount of total points
    RegionNP: integer;         //amount of points in each quadrant (octant)
    Ratio: Double;
    Angle: Integer;
    VertAnisotropy: Double;
    UseTriangulation: Boolean;
  end;

 // Data structure for Kriging parameters
  TKrigingParams = record
    K_type:     integer;      //Type of Kriging
    Drift:      array [1..9] of integer;
    Min_points, max_points: integer;
    Max_radius: double;
    Search_type: integer;  // normal or points per octant
    Points_per_octant: integer;
    Model:      TVariogramModel;
  end;


//-------------------------------- VARS -------------------------------------\\
var
  Mode3D: boolean; // 2D or 3D mode of interpolation

  PolyRegressOrder: integer = 1;   //Polynomial Regression
  InvDistPars:     TInvDistPars;  // Inverse Distance
  KrigingParams:   TKRigingParams;

procedure CreateMatrix(var a: TMatrix2d; n, m: integer);
procedure FreeMatrix(var a: TMatrix2d);
function TransposeMatrix(A: TMatrix2d; n, m: integer): TMatrix2d;
function MulMatrices(a, b: TMatrix2d; n, m, k: integer): TMatrix2d;
function MulMatrixVector(a: TMatrix2d; b: TVectord; n, m: integer): TVectord;
function InverseMatrix(a: TMatrix2d; n: word): TMatrix2d;


implementation

procedure CreateMatrix(var a: TMatrix2d; n, m: integer);
var
  I: integer;
begin
  SetLength(a, n + 1);
  for I := 1 to n do
    SetLength(a[I], m + 1);
end;

procedure FreeMatrix(var a: TMatrix2d);
var
  i: integer;
begin
  for i := 1 to High(a) do
    SetLength(a[i], 0);
  SetLength(a, 0);
end;

function TransposeMatrix(A: TMatrix2d; n, m: integer): TMatrix2d;
var
  i, j: integer;
begin
  CreateMatrix(Result, m, n);
  for i := 1 to n do
    for j := 1 to m do
      Result[j, i] := a[i, j];
end;

function MulMatrices(a, b: TMatrix2d; n, m, k: integer): TMatrix2d;
var
  i, j, l: integer;
begin
  CreateMatrix(Result, n, k);
  for i := 1 to n do
    for j := 1 to k do
    begin
      Result[i, j] := 0;
      for l := 1 to m do
        Result[i, j] := Result[i, j] + a[i, l] * b[l, j];
    end;
end;

function MulMatrixVector(a: TMatrix2d; b: TVectord; n, m: integer): TVectord;
var
  i, j: integer;
begin
  SetLength(Result, n + 1);
  for i := 1 to n do
  begin
    Result[i] := 0;
    for j := 1 to m do
      Result[i] := Result[i] + a[i, j] * b[j];
  end;
end;

function InverseMatrix(a: TMatrix2d; n: word): TMatrix2d;
const
  eps = 1e-20;
var
  p: TMatrix2d;
  i, j, k, l: integer;
  d: extended;
begin
  CreateMatrix(p, n, n);
  for i := 1 to n do
    for j := 1 to n do
      if i = j then
        p[i, j] := 1
      else
        p[i, j] := 0;
  for i := 1 to n do
  begin
    l := 1;
    while (abs(a[i, l]) < eps) and (l <= n) do
      l := l + 1;
    if l > n then
      exit;
    d := a[i, l];
    for j := 1 to n do
    begin
      a[i, j] := a[i, j] / d;
      p[i, j] := p[i, j] / d;
    end;
    for j := 1 to n do
      if j <> i then
      begin
        d := a[j, l];
        for k := 1 to n do
        begin
          a[j, k] := a[j, k] - d * a[i, k];
          p[j, k] := p[j, k] - d * p[i, k];
        end;
      end;
  end;
  CreateMatrix(Result, n, n);
  for i := 1 to n do
  begin
    j := 1;
    while abs(a[i, j] - 1) > eps do
      j := j + 1;
    Result[j] := p[i];
  end;
  FreeMatrix(p);
end;

end.
