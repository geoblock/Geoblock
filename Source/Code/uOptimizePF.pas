//
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//
unit uOptimizePF;

interface

uses
  System.Math,
  Vcl.ComCtrls,
  uGlobals,
  uCommon,
  uInterpol;

(*
 The deposit is divided into two- and three-dimensional array
 of rectangular blocks and Economic Block Values (EBV) is assigned to each.
 These values are stored in a two- and three-dimensional matrix value
 Val(i, j, k) with dimensions:
    numx - number of rows
    numy - number of columns
    numz – umber of levels
 The maximum values of these dimensions are set in a parameter statement
 to nx, ny and nz respectively.
 Log(i, j, k) is a matrix with the same dimensions as val and which is used to indicate
 whether block (i, j, k) is inside (=1) or outside (=O) the pit
 iplan(i, j) has dimensions IK and JK respectively and defines contours of the optimum
 pit by storing the pit level at horizontal location (i, j)

 Other working matrices have dimensions:
 iroot(lkm, 2), itree(nem), ipath(ipkm, 3), nd(nem, 2), d(nem), norm(knrn):
 a sufficient value for each of them is numx * numy * numz /20
	- input file containing Economic Block Values
	- input file containing dimensions of the block model
	- output file of results
*)

const
  lkmax = 2000;
  nemax = 2000;
  ipkmax = 2000;
  knmax = 2000;
  nxmax = 50;
  nymax = 50;
  nzmax = 13;

var
//  arrays with dimensions
  val: array[0..nxmax, 0..nymax, 0..nzmax] of Single;
  log: array[0..nxmax, 0..nymax, 0..nzmax] of Single;
  d: array[0..nemax] of Single; // contains value of tree
  iplan: array [0..nxmax, 0..nymax] of Single;
  iroot: array [0..nemax, 0..2] of Integer;
  itree: array [0..nemax] of Integer; // contains tree number
  nd: array [0..nemax, 0..2] of Integer;
  ipath: array [0..ipkmax, 0..2] of Integer;
  norm: array [0..knmax] of Integer;

function OptimizePF(var InBlocks: TCoordinateArray;
  var OutBlocks: TCoordinateArray; Params: TInvDistPars;
  ProgressBar: TProgressBar): Boolean;

//========================================================================
implementation
//========================================================================

var
  i,j,k: Integer;
  numx,numy,numz: Integer; //number of blocks in the x, y and z directions
  ixdim,iydim,izdim : Integer; //dimensions of the blocks for x, y and z axes
  ires: Integer; // element of itree;

(******************************************************************************)
//Read total values of blocks included in pit
(******************************************************************************)
procedure Initialize;
begin
  for i:=1 to numx do
    for j:=1 to numy do
      for k:=1 to numz do
  //      readln(val(i j,k);
  //
  //  Initialize arrays and variables
  //
  for i := 1 to numx do
    for j := 1 to numy do
      iplan[i,j] := 0;

  for i := 1 to numx do  //Fortran: do 15 i=1,numx do 15 k=l,numz
    for j := 1 to numy do
      for k := 1 to numz do
      begin
        log[i,j,k] := 0;
        if (val[i,j,k] = 0)
        then log[i,j,k] := 1;
  end;
end;

//**************************************************************************
//	Begin with uppermost level of blocks and remove all positive valued
//	blocks. These blocks belong to optimal open pit: add their values
//	to s, record their inclusion in the pit via log(i j, 1) and add them
//	to the contour array iplan(i,j)
//**************************************************************************
procedure PseudoFlow;
var
  s: Single; //sum of values
  cpm: Single;
  i, j, k, l, m, n: Integer;


  {sub} procedure Coord(n, numx, numy, k, j, i: Integer);
  //	subroutine to determine the array index co-ordinates of node (block)
  //	n given that there are ik (x direction) * jk (y direction) nodes
  //	on each horizontal level
  //	Array index co-ordinates are returned as (i,j,k)
  var
    ik, kt, jt: Integer;
  begin
    kt := round(n/(numx*numy));
    k := kt+1;
    if (n = kt*numx*numy) then
      k:=k-1;
    jt := round((n-numx*numy*(k-1))/numx);
    if ((n-numx*numy*(k-1)) = ik*jt) then
      j := j - 1;
    i := n-numx*numy*(k-1)-numx*(j-1);
  end;

begin
  //code to be included
end;


//=================================================================

function OptimizePF(var InBlocks: TCoordinateArray;
  var OutBlocks: TCoordinateArray; Params: TInvDistPars;
  ProgressBar: TProgressBar): Boolean;

begin
  PseudoFlow;
end;


end.
