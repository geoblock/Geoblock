//----------------------------------------------------------------------------
// The unit of Geoblock, http://sourceforge.net/projects/geoblock
//---------------------------------------------------------------------------

{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
% Copyright (C) 1996, The Board of Trustees of the Leland Stanford     %
% Junior University.  All rights reserved.                             %
%                                                                      %
% The programs in GSLIB are distributed in the hope that they will be  %
% useful, but WITHOUT ANY WARRANTY.  No author or distributor accepts  %
% responsibility to anyone for the consequences of using them or for   %
% whether they serve any particular purpose or work at all, unless he  %
% says so in writing.  Everyone is granted permission to copy, modify  %
% and redistribute the programs in GSLIB, but only under the condition %
% that this notice and the above copyright notice remain intact.       %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{! Kriging (SK,OK,KT) of a 3-D Rectangular Grid }

{  MAXSBX    maximum super block nodes in X direction
   MAXSBY    maximum super block nodes in Y direction
   MAXSBZ    maximum super block nodes in Z direction
   MAXDT     maximum number of drift terms
   MAXNST    maximum number of nested structures
}

unit uKriging;

interface

uses
  System.SysUtils,
  System.Math,
  Vcl.ComCtrls,

  uCommon,
  uInterpol;

const
  // User Adjustable Parameters
  MAXSBX = 21;
  MAXSBY = 21;
  MAXSBZ = 11;
  MAXDT  = 9;
  MAXNST = 4;

  // Fixed Parameters
  MAXSB  = MAXSBX * MAXSBY * MAXSBZ;
  MAXROT = MAXNST + 1;
  EPSLON = 0.000001;
  PMX    = 999.0;

type
  TRotmat      = array[1..3, 1..3] of double;
  TRotmatArray = array [1..MAXROT] of TRotmat;


procedure Kt_3d(var Points: TCoordinateArray; var Nodes: TCoordinateArray;
  Params: TKrigingParams; ProgressBar: TProgressBar);

//=========================================================================
implementation
//=========================================================================

{c-----------------------------------------------------------------------
c    Squared Anisotropic Distance Calculation Given Matrix Indicator
c    ***************************************************************
c This routine calculates the anisotropic distance between two points
c  given the coordinates of each point and a definition of the
c  anisotropy.
c
c INPUT VARIABLES:
c   x1,y1,z1         Coordinates of first point
c   x2,y2,z2         Coordinates of second point
c   ind              The rotation matrix to use
c   rotmat           The rotation matrices
c
c OUTPUT VARIABLES:
c   sqdist           The squared distance accounting for the anisotropy
c                      and the rotation of coordinates (if any).
c
c-----------------------------------------------------------------------}
function SqDist(x1, y1, z1, x2, y2, z2: double; rotmat: TRotmat): double;
var
  dx, dy, dz: double;
  i: integer;
  sqdst, cont: double;

begin
  // Compute component distance vectors and the squared distance
  dx    := x1 - x2;
  dy    := y1 - y2;
  dz    := z1 - z2;
  sqdst := 0.0;
  for i := 1 to 3 do
  begin
    cont  := rotmat[i, 1] * dx + rotmat[i, 2] * dy + rotmat[i, 3] * dz;
    sqdst := sqdst + cont * cont;
  end;
  Result := sqdst;
end;

//----------------------------------------------------------------------------//

procedure Swap(var a, b: double);
var
  t: double;
begin
  t := a;
  a := b;
  b := t;
end;

{c-----------------------------------------------------------------------
c                      Sorting Subroutine
c                      ******************
c INPUT PARAMETERS:
c   ib,ie        start and end index of the array to be sorteda
c   a            array, a portion of which has to be sorted.
c   iperm        0 no other array is permuted.
c                1 array b is permuted according to array a
c                2 arrays b,c are permuted.
c                3 arrays b,c,d are permuted.
c                4 arrays b,c,d,e are permuted.
c                5 arrays b,c,d,e,f are permuted.
c                6 arrays b,c,d,e,f,g are permuted.
c                7 arrays b,c,d,e,f,g,h are permuted.
c               >7 no other array is permuted.
c
c   b,c,d,e,f,g,h  arrays to be permuted according to array a.
c
c OUTPUT PARAMETERS:
c    a      = the array, a portion of which has been sorted.
c    b,c,d,e,f,g,h  =arrays permuted according to array a (see iperm)
c-----------------------------------------------------------------------}
procedure Sortem(ib, ie: integer; var a: array of double; iperm: integer;
  var b, c, d, e, f, g, h: array of double);

var
  i, j, k: integer;
  t: double;

begin
  for i := ib to ie - 1 do
  begin
    t := a[i];
    k := i;
    for j := i + 1 to ie do
      if a[j] < t then
      begin
        t := a[j];
        k := j;
      end;
    if i <> k then
    begin
      swap(a[i], a[k]);
      if iperm > 0 then
      begin
        swap(b[i], b[k]);
        if iperm > 1 then
        begin
          swap(c[i], c[k]);
          if iperm > 2 then
          begin
            swap(d[i], d[k]);
            if iperm > 3 then
            begin
              swap(e[i], e[k]);
              if iperm > 4 then
              begin
                swap(f[i], f[k]);
                if iperm > 5 then
                begin
                  swap(g[i], g[k]);
                  if iperm > 6 then
                    swap(h[i], h[k]);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//{c-----------------------------------------------------------------------
// c     Gets the coordinate index location of a point within a grid
// c     ***********************************************************
// c
// c n       number of "nodes" or "cells" in this coordinate direction
// c min     origin at the center of the first cell
// c siz     size of the cells
// c loc     location of the point being considered
// c index   output index within [1,n]
// c inflag  true if the location is actually in the grid (false otherwise
// c         e.g., if the location is outside then index will be set to
// c         nearest boundary
// c-----------------------------------------------------------------------}

procedure GetIndx(n: integer; min, siz, loc: double; var index: integer;
  var inflag: boolean);

begin
  // Compute the index of "loc"
  index := trunc((loc - min) / siz + 1.5);

  // Check to see if in or out
  if (index < 1) then
  begin
    index  := 1;
    inflag := False;
  end
  else if (index > n) then
  begin
    index  := n;
    inflag := False;
  end
  else
    inflag := True;
end;

{c-----------------------------------------------------------------------
c           Establish Super Block Search Limits and Sort Data
c           *************************************************
c This subroutine sets up a 3-D "super block" model and orders the data
c by super block number.  The limits of the super block is set to the
c minimum and maximum limits of the grid; data outside are assigned to
c the nearest edge block.
c
c The idea is to establish a 3-D block network that contains all the
c relevant data.  The data are then sorted by their index location in
c the search network, i.e., the index location is given after knowing
c the block index in each coordinate direction (ix,iy,iz):
c          ii = (iz-1)*nxsup*nysup + (iy-1)*nxsup + ix
c An array, the same size as the number of super blocks, is constructed
c that contains the cumulative number of data in the model.  With this
c array it is easy to quickly check what data are located near any given
c location.
c
c INPUT VARIABLES:
c   nx,xmn,xsiz      Definition of the X grid being considered
c   ny,ymn,ysiz      Definition of the Y grid being considered
c   nz,zmn,zsiz      Definition of the Z grid being considered
c   nd               Number of data
c   x(nd)            X coordinates of the data
c   y(nd)            Y coordinates of the data
c   z(nd)            Z coordinates of the data
c   vr(nd)           Variable at each location.
c   tmp(nd)          Temporary storage to keep track of the super block
c                      index associated to each data (uses the same
c                      storage already allocated for the simulation)
c   nsec             Number of secondary variables to carry with vr
c   sec1(nd)         First secondary variable (if nsec >= 1)
c   sec2(nd)         Second secondary variable (if nsec >= 2)
c   sec3(nd)         Third secondary variable (if nsec = 3)
c   MAXSB[X,Y,Z]     Maximum size of super block network
c
c OUTPUT VARIABLES:
c   nisb()                Array with cumulative number of data in each
c                           super block.
c   nxsup,xmnsup,xsizsup  Definition of the X super block grid
c   nysup,ymnsup,ysizsup  Definition of the Y super block grid
c   nzsup,zmnsup,zsizsup  Definition of the Z super block grid
c
c EXTERNAL REFERENCES:
c   sortem           Sorting routine to sort the data
c
c-----------------------------------------------------------------------}

procedure SetSupr(nx: integer; xmn, xsiz: double; ny: integer;
  ymn, ysiz: double; nz: integer; zmn, zsiz: double; nd: integer;
  var x, y, z, vr: array of double; var tmp: array of double;
  nsec: integer; var sec1, sec2, sec3: array of double; var nisb: array of integer;
  var nxsup: integer; var xmnsup, xsizsup: double; var nysup: integer;
  var ymnsup, ysizsup: double; var nzsup: integer; var zmnsup, zsizsup: double);

var
  inflag: boolean;
  i, ii, ix, iy, iz, nsort: integer;

begin
  // Establish the number and size of the super blocks
  nxsup   := min(nx, MAXSBX);
  nysup   := min(ny, MAXSBY);
  nzsup   := min(nz, MAXSBZ);
  xsizsup := nx * xsiz / nxsup;
  ysizsup := ny * ysiz / nysup;
  zsizsup := nz * zsiz / nzsup;
  xmnsup  := (xmn - 0.5 * xsiz) + 0.5 * xsizsup;
  ymnsup  := (ymn - 0.5 * ysiz) + 0.5 * ysizsup;
  zmnsup  := (zmn - 0.5 * zsiz) + 0.5 * zsizsup;

  // Initialize the extra super block array to zeros
  for i := 1 to nxsup * nysup * nzsup do
    nisb[i] := 0;

  // Loop over all the data assigning the data to a super block and
  // accumulating how many data are in each super block
  for i := 1 to nd do
  begin
    getindx(nxsup, xmnsup, xsizsup, x[i], ix, inflag);
    getindx(nysup, ymnsup, ysizsup, y[i], iy, inflag);
    getindx(nzsup, zmnsup, zsizsup, z[i], iz, inflag);
    ii     := ix + (iy - 1) * nxsup + (iz - 1) * nxsup * nysup;
    tmp[i] := ii;
    nisb[ii] := nisb[ii] + 1;
  end;

  // Sort the data by ascending super block number
  nsort := 4 + nsec;
  sortem(1, nd, tmp, nsort, x, y, z, vr, sec1, sec2, sec3);

  // Set up array nisb with the starting address of the block data
  for i := 1 to (nxsup * nysup * nzsup - 1) do
    nisb[i + 1] := nisb[i] + nisb[i + 1];
end;

{c-----------------------------------------------------------------------
c             Establish Which Super Blocks to Search
c             **************************************
c This subroutine establishes which super blocks must be searched given
c that a point being estimated/simulated falls within a super block
c centered at 0,0,0.
c
c INPUT VARIABLES:
c   nxsup,xsizsup    Definition of the X super block grid
c   nysup,ysizsup    Definition of the Y super block grid
c   nzsup,zsizsup    Definition of the Z super block grid
c   rotmat           rotation matrices
c   radsqd           squared search radius
c
c OUTPUT VARIABLES:
c   nsbtosr          Number of super blocks to search
c   ixsbtosr         X offsets for super blocks to search
c   iysbtosr         Y offsets for super blocks to search
c   izsbtosr         Z offsets for super blocks to search
c
c EXTERNAL REFERENCES:
c   sqdist           Computes anisotropic squared distance
c
c-----------------------------------------------------------------------}
procedure PickSup(nxsup: integer; xsizsup: double; nysup: integer;
  ysizsup: double; nzsup: integer; zsizsup: double; rotmat: TRotmat;
  radsqd: double; var nsbtosr: integer;
  var ixsbtosr, iysbtosr, izsbtosr: array of integer);

var
  hsqd, shortest, xo, yo, zo, xdis, ydis, zdis: double;
  i, i1, i2, j, j1, j2, k, k1, k2: integer;

begin
  // MAIN Loop over all possible super blocks
  nsbtosr := 0;
  for i := -(nxsup - 1) to (nxsup - 1) do
    for j := -(nysup - 1) to (nysup - 1) do
      for k := -(nzsup - 1) to (nzsup - 1) do
      begin
        xo := i * xsizsup;
        yo := j * ysizsup;
        zo := k * zsizsup;

        // Find the closest distance between the corners of the super blocks
        shortest := 1.0e21;
        for i1 := -1 to 1 do
          for j1 := -1 to 1 do
            for k1 := -1 to 1 do
              for i2 := -1 to 1 do
                for j2 := -1 to 1 do
                  for k2 := -1 to 1 do
                    if (i1 <> 0) and (j1 <> 0) and (k1 <> 0) and
                      (i2 <> 0) and (j2 <> 0) and (k2 <> 0) then
                    begin
                      xdis := (i1 - i2) * 0.5 * xsizsup + xo;
                      ydis := (j1 - j2) * 0.5 * ysizsup + yo;
                      zdis := (k1 - k2) * 0.5 * zsizsup + zo;
                      hsqd := sqdist(0.0, 0.0, 0.0, xdis, ydis, zdis, rotmat);
                      if (hsqd < shortest) then
                        shortest := hsqd;
                    end;

        // Keep this super block if it is close enoutgh
        if (shortest <= radsqd) then
        begin
          nsbtosr := nsbtosr + 1;
          ixsbtosr[nsbtosr] := i;
          iysbtosr[nsbtosr] := j;
          izsbtosr[nsbtosr] := k;
        end;
      end;
end;

//----------------------------------------------------------------------------//

procedure SrchSupr(xloc, yloc, zloc, radsqd: double; rotmat: TRotmat;
  nsbtosr: integer; ixsbtosr, iysbtosr, izsbtosr: array of integer;
  noct: integer; nd: integer; x, y, z, tmp: array of double;
  nisb: array of integer; nxsup: integer; xmnsup, xsizsup: double;
  nysup: integer; ymnsup, ysizsup: double; nzsup: integer;
  zmnsup, zsizsup: double; var nclose: integer; var close: array of double;
  var infoct: integer);
{c-----------------------------------------------------------------------
c              Search Within Super Block Search Limits
c              ***************************************
c This subroutine searches through all the data that have been tagged in
c the super block subroutine.  The close data are passed back in the
c index array "close".  An octant search is allowed.
c
c INPUT VARIABLES:
c   xloc,yloc,zloc   location of point being estimated/simulated
c   radsqd           squared search radius
c   rotmat           rotation matrix
c   nsbtosr          Number of super blocks to search
c   ixsbtosr         X offsets for super blocks to search
c   iysbtosr         Y offsets for super blocks to search
c   izsbtosr         Z offsets for super blocks to search
c   noct             If >0 then data will be partitioned into octants
c   nd               Number of data
c   x(nd)            X coordinates of the data
c   y(nd)            Y coordinates of the data
c   z(nd)            Z coordinates of the data
c   tmp(nd)          Temporary storage to keep track of the squared
c                      distance associated with each data
c   nisb()                Array with cumulative number of data in each
c                           super block.
c   nxsup,xmnsup,xsizsup  Definition of the X super block grid
c   nysup,ymnsup,ysizsup  Definition of the X super block grid
c   nzsup,zmnsup,zsizsup  Definition of the X super block grid
c
c OUTPUT VARIABLES:
c   nclose           Number of close data
c   close()          Index of close data
c   infoct           Number of informed octants (only computes if
c                      performing an octant search)
c
c EXTERNAL REFERENCES:
c   sqdist           Computes anisotropic squared distance
c   sortem           Sorts multiple arrays in ascending order
c-----------------------------------------------------------------------}
var
  i, ii, ix, iy, iz, isup, ixsup, iysup, izsup, j, iq, nums, nt, na: integer;
  inflag:     boolean;
  hsqd, h:    double;
  c, d, e, f, g, hh: array of double;  // fake variables needed for sortem
  inoct:      array[1..8] of integer;
  dx, dy, dz: double;

begin
  // Determine the super block location of point being estimated:
  getindx(nxsup, xmnsup, xsizsup, xloc, ix, inflag);
  getindx(nysup, ymnsup, ysizsup, yloc, iy, inflag);
  getindx(nzsup, zmnsup, zsizsup, zloc, iz, inflag);

  // Loop over all the possible Super Blocks
  nclose := 0;
  for isup := 1 to nsbtosr do
  begin

    // Is this super block within the grid system
    ixsup := ix + ixsbtosr[isup];
    iysup := iy + iysbtosr[isup];
    izsup := iz + izsbtosr[isup];
    if (ixsup <= 0) or (ixsup > nxsup) or (iysup <= 0) or (iysup > nysup) or
      (izsup <= 0) or (izsup > nzsup) then
      continue;

    // Figure out how many samples in this super block
    ii := ixsup + (iysup - 1) * nxsup + (izsup - 1) * nxsup * nysup;
    if (ii = 1) then
    begin
      nums := nisb[ii];
      i    := 0;
    end
    else
    begin
      nums := nisb[ii] - nisb[ii - 1];
      i    := nisb[ii - 1];
    end;

    // Loop over all the data in this super block
    for ii := 1 to nums do
    begin
      i    := i + 1;
      // Check squared distance
      hsqd := sqdist(xloc, yloc, zloc, x[i], y[i], z[i], rotmat);
      if (hsqd > radsqd) then
        continue;

      // Accept this sample
      nclose      := nclose + 1;
      Close[nclose] := i;
      tmp[nclose] := hsqd;
    end;
  end;

  // Sort the nearby samples by distance to point being estimated
  sortem(1, nclose, tmp, 1, Close, c, d, e, f, g, hh);

  // If we aren't doing an octant search then just return
  if (noct <= 0) then
    exit;

  // PARTITION THE DATA INTO OCTANTS
  for i := 1 to 8 do
    inoct[i] := 0;

  // Now pick up the closest samples in each octant
  nt := 8 * noct;
  na := 0;
  for j := 1 to nclose do
  begin
    i  := trunc(Close[j]);
    h  := tmp[j];
    dx := x[i] - xloc;
    dy := y[i] - yloc;
    dz := z[i] - zloc;
    if (dz < 0) then
    begin
      iq := 8;
      if (dx <= 0.0) and (dy > 0.0) then
        iq := 5;
      if (dx > 0.0) and (dy >= 0.0) then
        iq := 6;
      if (dx < 0.0) and (dy <= 0.0) then
        iq := 7;
    end
    else
    begin
      iq := 4;
      if (dx <= 0.0) and (dy > 0.0) then
        iq := 1;
      if (dx > 0.0) and (dy >= 0.0) then
        iq := 2;
      if (dx < 0.0) and (dy <= 0.0) then
        iq := 3;
    end;
    inoct[iq] := inoct[iq] + 1;

    // Keep this sample if the maximum has not been exceeded
    if (inoct[iq] <= noct) then
    begin
      na      := na + 1;
      Close[na] := i;
      tmp[na] := h;
      if (na = nt) then
        break;
    end;
  end;

  // End of data selection. Compute number of informed octants and return
  nclose := na;
  infoct := 0;
  for i := 1 to 8 do
    if (inoct[i] > 0) then
      infoct := infoct + 1;
end;

//----------------------------------------------------------------------------//

procedure Cova3(x1, y1, z1, x2, y2, z2: double; nst: integer;
  c0: double; it: array of integer; cc, aa: array of double;
  rotmat: TRotmatArray; var cmax, cova: double);
{
c-----------------------------------------------------------------------
c                    Covariance Between Two Points
c                    *****************************
c This subroutine calculated the covariance associated with a variogram
c model specified by a nugget effect and nested varigoram structures.
c The anisotropy definition can be different for each nested structure.
c
c INPUT VARIABLES:
c   x1,y1,z1         coordinates of first point
c   x2,y2,z2         coordinates of second point
c   nst              number of nested structures (maximum of 4)
c   c0               isotropic nugget constant
c   it(i)            type of each nested structure:
c                      1. spherical model of range a;
c                      2. exponential model of parameter a;
c                           i.e. practical range is 3a
c                      3. gaussian model of parameter a;
c                           i.e. practical range is a*sqrt(3)
c                      4. power model of power a (a must be gt. 0  and
c                           lt. 2).  if linear model, a=1,c=slope.
c                      5. hole effect model
c   cc(i)            multiplicative factor of each nested structure.
c                      (sill-c0) for spherical, exponential,and gaussian
c                      slope for linear model.
c   aa(i)            parameter "a" of each nested structure.
c   rotmat           rotation matrix
c
c OUTPUT VARIABLES:
c   cmax             maximum covariance
c   cova             covariance between (x1,y1,z1) and (x2,y2,z2)
c
c EXTERNAL REFERENCES: sqdist    computes anisotropic squared distance
c                      rotmat    computes rotation matrix for distance
c-----------------------------------------------------------------------
}

const
  PI     = 3.14159265;
  EPSLON = 1.0e-10;

var
  hsqd, h, hr: double;
  iss: integer;

begin
  // Calculate the maximum covariance value (used for zero distances and
  // for power model covariance)
  cmax := c0;
  for iss := 1 to nst do
    if (it[iss] = 4) then
      cmax := cmax + PMX
    else
      cmax := cmax + cc[iss];

  // Check for "zero" distance, return with cmax if so
  hsqd := sqdist(x1, y1, z1, x2, y2, z2, rotmat[1]);
  if (hsqd < EPSLON) then
  begin
    cova := cmax;
    exit;
  end;

  // Loop over all the structures
  cova := 0.0;
  for iss := 1 to nst do
  begin
    // Compute the appropriate distance
    if (iss <> 1) then
      hsqd := sqdist(x1, y1, z1, x2, y2, z2, rotmat[iss]);
    h := sqrt(hsqd);

    // Spherical Variogram Model?
    if (it[iss] = 1) then
    begin
      hr := h / aa[iss];
      if (hr < 1.0) then
        cova := cova + cc[iss] * (1.0 - hr * (1.5 - 0.5 * hr * hr));
    end
    // Exponential Variogram Model?
    else if (it[iss] = 2) then
      cova := cova + cc[iss] * exp(-3.0 * h / aa[iss])
    // Gaussian Variogram Model?
    else if (it[iss] = 3) then
      cova := cova + cc[iss] * exp(-3. * (h / aa[iss]) * (h / aa[iss]))
    // Power Variogram Model?
    else if (it[iss] = 4) then
      cova := cova + cmax - cc[iss] * (Power(h, aa[iss]))
    // Hole Effect Model?
    else if (it[iss] = 5) then
    begin
      //      d := 10.0 * aa[iss];
      //      cova = cova + cc[iss]*exp(-3.0*h/d)*cos(h/aa[iss]*PI);
      cova := cova + cc[iss] * cos(h / aa[iss] * PI);
    end;
  end;
end;

//----------------------------------------------------------------------------//

procedure KtSol(n, ns, nv: integer; a, b: array of double; var x: array of double;
  var ktilt: integer);
{c-----------------------------------------------------------------------
c Solution of a system of linear equations by gaussian elimination with
c partial pivoting.  Several right hand side matrices and several
c variables are allowed.
c
c         NOTE: All input matrices must be in double precision
c
c INPUT/OUTPUT VARIABLES:
c   n                Number of equations
c   ns               Number of right hand side matrices
c   nv               Number of variables.
c   a(n*n*nv)        left hand side matrices versus columnwise.
c   b(n*ns*nv)       input right hand side matrices.
c   x(n*ns*nv)       solution matrices.
c   ktilt            indicator of singularity
c                      =  0  everything is ok.
c                      = -1 n.le.1
c                      =  k  a null pivot appeared at the kth iteration.
c-----------------------------------------------------------------------}
var
  tol, t: double;
  i, i1, i2, il, iv, j, j1, j2, k, kb, kp1: integer;
  ntn, nm1, npiv, ipiv, itot, kdiag, nmk, nva, nvb, nvb1, nvb2: integer;

begin
  // Make sure there are equations to solve
  if (n <= 1) then
  begin
    ktilt := -1;
    exit;
  end;

  // Initialization
  tol   := 0.1e-10;
  ktilt := 0;
  ntn   := n * n;
  nm1   := n - 1;

  // Triangulation is done variable by variable
  for iv := 1 to nv do
  begin

    // Indices of location in vectors a and b
    nva := ntn * (iv - 1);
    nvb := n * ns * (iv - 1);

    // Gaussian elimination with partial pivoting
    for k := 1 to nm1 do
    begin
      kp1 := k + 1;

      // Indice of the diagonal element in the kth row
      kdiag := nva + (k - 1) * n + k;

      // Find the pivot - interchange diagonal element/pivot:
      npiv := kdiag;
      ipiv := k;
      i1   := kdiag;
      for i := kp1 to n do
      begin
        i1 := i1 + 1;
        if (abs(a[i1]) > abs(a[npiv])) then
        begin
          npiv := i1;
          ipiv := i;
        end;
      end;
      t := a[npiv];
      a[npiv] := a[kdiag];
      a[kdiag] := t;

      // Test for singularity
      if (abs(a[kdiag]) < tol) then
      begin
        ktilt := k;
        exit;
      end;

      // Compute multipliers
      i1 := kdiag;
      for i := kp1 to n do
      begin
        i1    := i1 + 1;
        a[i1] := -a[i1] / a[kdiag];
      end;

      // Interchange and eliminate column per column
      j1 := kdiag;
      j2 := npiv;
      for j := kp1 to n do
      begin
        j1    := j1 + n;
        j2    := j2 + n;
        t     := a[j2];
        a[j2] := a[j1];
        a[j1] := t;
        i1    := j1;
        i2    := kdiag;
        for i := kp1 to n do
        begin
          i1    := i1 + 1;
          i2    := i2 + 1;
          a[i1] := a[i1] + a[i2] * a[j1];
        end;
      end;

      // Interchange and modify the ns right hand matrices
      i1 := nvb + ipiv;
      i2 := nvb + k;
      for i := 1 to ns do
      begin
        t     := b[i1];
        b[i1] := b[i2];
        b[i2] := t;
        j1    := i2;
        j2    := kdiag;
        for j := kp1 to n do
        begin
          j1    := j1 + 1;
          j2    := j2 + 1;
          b[j1] := b[j1] + b[i2] * a[j2];
        end;
        i1 := i1 + n;
        i2 := i2 + n;
      end;
    end;

    // Test for singularity for the last pivot
    kdiag := ntn * iv;
    if (abs(a[kdiag]) < tol) then
    begin
      ktilt := n;
      exit;
    end;
  end;

  // End of triangulation. Now, solve back variable per variable
  for iv := 1 to nv do
  begin

    // Indices of location in vectors a and b
    nva  := ntn * iv;
    nvb1 := n * ns * (iv - 1) + 1;
    nvb2 := n * ns * iv;

    // Back substitution with the ns right hand matrices
    for il := 1 to ns do
    begin
      for k := 1 to nm1 do
      begin
        nmk   := n - k;
        // Indice of the diagonal element of the (n-k+1)th row and of
        // the (n-k+1)th element of the left hand side.
        kdiag := nva - (n + 1) * (k - 1);
        kb    := nvb2 - (il - 1) * n - k + 1;
        b[kb] := b[kb] / a[kdiag];
        t     := -b[kb];
        i1    := kb;
        i2    := kdiag;
        for i := 1 to nmk do
        begin
          i1    := i1 - 1;
          i2    := i2 - 1;
          b[i1] := b[i1] + a[i2] * t;
        end;
      end;
      kdiag := kdiag - n - 1;
      kb    := kb - 1;
      b[kb] := b[kb] / a[kdiag];
    end;
    // End of back substitution
  end;

  // Restitution of the solution
  itot := n * ns * nv;
  for i := 1 to itot do
    x[i] := b[i];
end;

//----------------------------------------------------------------------------//

procedure SetRot(ang1, ang2, ang3, anis1, anis2: double; var rotmat: TRotmat);
{c-----------------------------------------------------------------------
c              Sets up an Anisotropic Rotation Matrix
c              **************************************
c Sets up the matrix to transform cartesian coordinates to coordinates
c accounting for angles and anisotropy (see manual for a detailed
c definition):
c
c INPUT PARAMETERS:
c   ang1             Azimuth angle for principal direction
c   ang2             Dip angle for principal direction
c   ang3             Third rotation angle
c   anis1            First anisotropy ratio
c   anis2            Second anisotropy ratio
c   rotmat           rotation matrix
c-----------------------------------------------------------------------}
const
  DEG2RAD = 3.141592654 / 180.0;
  EPSLON  = 1.0e-20;
  PMX     = 999.0;

var
  alpha, beta, theta, afac1, afac2, sina, sinb, sint, cosa, cosb, cost: double;

begin
  // Converts the input angles to three angles which make more
  // mathematical sense:
  //         alpha   angle between the major axis of anisotropy and the
  //                 E-W axis. Note: Counter clockwise is positive.
  //         beta    angle between major axis and the horizontal plane.
  //                 (The dip of the ellipsoid measured positive down)
  //         theta   Angle of rotation of minor axis about the major axis
  //                 of the ellipsoid.
  if (ang1 >= 0.0) and (ang1 < 270.0) then
    alpha := (90.0 - ang1) * DEG2RAD
  else
    alpha := (450.0 - ang1) * DEG2RAD;
  beta := -1.0 * ang2 * DEG2RAD;
  theta := ang3 * DEG2RAD;

  // Get the required sines and cosines
  sina := sin(alpha);
  sinb := sin(beta);
  sint := sin(theta);
  cosa := cos(alpha);
  cosb := cos(beta);
  cost := cos(theta);

  // Construct the rotation matrix in the required memory
  afac1 := 1.0 / max(anis1, EPSLON);
  afac2 := 1.0 / max(anis2, EPSLON);
  rotmat[1, 1] := (cosb * cosa);
  rotmat[1, 2] := (cosb * sina);
  rotmat[1, 3] := (-sinb);
  rotmat[2, 1] := afac1 * (-cost * sina + sint * sinb * cosa);
  rotmat[2, 2] := afac1 * (cost * cosa + sint * sinb * sina);
  rotmat[2, 3] := afac1 * (sint * cosb);
  rotmat[3, 1] := afac2 * (sint * sina + cost * sinb * cosa);
  rotmat[3, 2] := afac2 * (-sint * cosa + cost * sinb * sina);
  rotmat[3, 3] := afac2 * (cost * cosb);


{  rotmat[1,1] := 1; rotmat[1,2] := 0; rotmat[1,3] := 0;
  rotmat[2,1] := 0; rotmat[2,2] := 1; rotmat[2,3] := 0;
  rotmat[3,1] := 0; rotmat[3,2] := 0; rotmat[3,3] := 1;
}
end;


//----------------------------------------------------------------------------//

procedure Kt_3d(var Points: TCoordinateArray; var Nodes: TCoordinateArray;
  Params: TKrigingParams; ProgressBar: TProgressBar);
{
c-----------------------------------------------------------------------
c
c                Krige a 3-D Grid of Rectangular Blocks
c                **************************************
c
c This subroutine estimates point or block values of one variable by
c simple, ordinary, or kriging with a trend model.  It is also possible
c to estimate the trend directly.
c
c PROGRAM NOTES:
c
c   1. The data and parameters are passed in common blocks defined
c      in kt3d.inc.  Local storage is allocated in the subroutine
c      for kriging matrices, i.e.,
c         - xa,ya,za,vra   arrays for data within search neighborhood
c         - a,r,rr,s       kriging arrays
c         - xdb,ydb,zdb    relative position of discretization points
c         - cbb            block covariance
c   2. The kriged value and the kriging variance is written to Fortran
c      unit number "lout".
c
c Original:  A.G. Journel and C. Lemmer                             1981
c Revisions: A.G. Journel and C. Kostov                             1984
c-----------------------------------------------------------------------
}
var
  ixsbtosr, iysbtosr, izsbtosr: array[0..8 * MAXSB] of integer;

  // The data and other input variables
  nd:      integer;
  tmin, tmax: double;
  nx, ny, nz: integer;
  xmn, ymn, zmn, xsiz, ysiz, zsiz, xmx, ymx, zmx: double;
  ndmax, ndmin: integer;
  radius, radius1, radius2{, aa1, aa2}: double;
  noct, nxdis, nydis, nzdis, itrend, ktype: integer;
  skmean:  double;
  koption: integer;

  idrif: array [0..MAXDT] of integer;
  nisb:  array [0..MAXSB] of integer;

  // Variogram params
  nst: integer;
  it:  array [0..MAXNST] of integer;
  c0:  double;
  cc, aa, ang1, ang2, ang3, anis1, anis2: array [0..MAXNST] of double;

  bv:     array [0..9] of double;
  x, y, z, vr, ve: array of double;
  xa, ya, za, vra, vea, sec2, sec3: array of double;
  xdb, ydb, zdb: array of double;
  r, rr, s: array of double;
  a:      array of double;
  rotmat: TRotmatArray;

  // Superblock structrures
  nxsup, nysup, nzsup, nsbtosr, nclose, infoct: integer;
  xmnsup, xsizsup, ymnsup, ysizsup, zmnsup, zsizsup: double;
  tmp, close: array of double;

  /// Local variables
  First, accept, estimated: boolean;
  na, ndb, nsec, neq, mdt, kadim, ksdim, nrhs, nv, meq: integer;
  isrot, ising: integer;
  iss, index, ind, i, iii, im, ix, iy, iz, j, k, nloop: integer;
  sang1, sang2, sang3, sanis1, sanis2, unbias: double;
  radsqd, covmax, cov, cmax, cbb, cb, cb1, est, estv, truee, resc,
  resce, err, extest: double;
  xdis, ydis, zdis, xloc, yloc, zloc, t, dx, dy, dz: double;

  // debug
  nfound, nktsol, ndrift: integer;
begin
  /// Initialization

  // trimming limits:
  tmin := -1e10;
  tmax := 1e10;

  // kriging option:
  koption := 0;   // 0 - grid, 1 - cross, 2 - jackknife
{
// nx, xmn, xsiz:
  nx := 10; xmn := -1051; xsiz := 86;

// ny, ymn, ysiz:
  ny := 10; ymn := -2151; ysiz := 54;

// nz, zmn, zsiz:
  nz := 10; zmn := -197;  zsiz := 46;
}

  nx := 20;
  ny := 20;
  nz := 10;
  GetMinMaxXYZ(Nodes, xmn, xmx, ymn, ymx, zmn, zmx);
  xsiz := (xmx - xmn) / nx;
  ysiz := (ymx - ymn) / ny;
  zsiz := (zmx - zmn) / nz;

  // block discretization:
  nxdis := 1;
  nydis := 1;
  nzdis := 1;  // =1 for point kriging

  // ndmin,ndmax:
  //  ndmin := 10; ndmax := 20;
  ndmin := Params.min_points;
  ndmax := Params.max_points;

  // max per octant:
  noct := 0;  // 0 -> not used
  if Params.search_type = NORMAL_SEARCH then
    noct := 0
  else
    noct := Params.points_per_octant;

  // maximum search radii:
  //  radius := 500;  radius1 := 100;  radius2 := 100;
  radius  := Params.max_radius;
  radius1 := radius;
  radius2 := radius;   /// all directions while superblock search are equal - MR

  if (radius < EPSLON) then
    exit; // radius must be greater than zero
  radsqd := radius * radius;
  sanis1 := radius1 / radius;
  sanis2 := radius2 / radius;

  // search anisotropy angles:
  sang1 := 0;
  sang2 := 0;
  sang3 := 0;

  // ktype, skmean:
  //  ktype := 1;   // 0 - SK, 1 - OK, 2 - non-st SK, 3 - exdrift
  //  ktype := 0;   // 0 - SK, 1 - OK, 2 - non-st SK, 3 - exdrift
  ktype  := Params.k_type;
  skmean := 2.302;  // used if SK and non-st SK

  // drift terms:
  //  for i := 1 to 9 do idrif[i] := 0;
  for i := 1 to 9 do
    idrif[i] := Params.drift[i];

  // itrend:
  itrend := 0;  // 0 - variable, 1 - estimate trend

  // nst, c0  (number of nested structures, nugget effect):
  //  nst := 1;
  //  c0 := 0.2;
  nst := Params.model.nst_count;
  c0  := Params.model.structures[1].nugget;

  for i := 1 to nst do
    with Params.model do
    begin
      // it,cc,ang[1,2,3]:
      //    it[i] := 1; cc[i] := 0.8; ang1[i] := 0; ang2[i] := 0; ang3[i] := 0;
      it[i]   := structures[i].model_type;
      cc[i]   := structures[i].contribution;
      ang1[i] := structures[i].azimuth;
      ang2[i] := structures[i].dip;
      ang3[i] := structures[i].plunge;

      // a1,a2,a3:
      //    aa[i] := 20; aa1 := 0; aa2 := 0;
      aa[i] := structures[i].range; // aa1 := 10; aa2 := 10;

      //    anis1[i] := aa1 / max(aa[i],EPSLON);
      //    anis2[i] := aa2 / max(aa[i],EPSLON);
      anis1[i] := structures[i].anis1;
      anis2[i] := structures[i].anis2;
      if (it[i] = 4) then
      begin
        if (aa[i] < 0.0) then
          exit; // ' INVALID power variogram'
        if (aa[i] > 2.0) then
          exit; // ' INVALID power variogram'
      end;
    end;

  /// Get memory for arrays
  nd := High(Points) + 1;
  SetLength(x, nd + 1);
  SetLength(y, nd + 1);
  SetLength(z, nd + 1);
  SetLength(vr, nd + 1);
  SetLength(ve, nd + 1);
  SetLength(tmp, nd + 1);
  SetLength(Close, nd + 1);

   SetLength(xa, ndmax+1); SetLength(ya, ndmax+1); SetLength(za, ndmax+1);
   SetLength(vra, ndmax+1); SetLength(vea, ndmax+1);
  {
  SetLength(xa, 65);  SetLength(ya, 65);  SetLength(za, 65);
  SetLength(vra, 65);  SetLength(vea, 65);
  }
  meq := ndmax+MAXDT+2;
  ///meq := 64 + MAXDT + 2;
  SetLength(r, meq + 1);
  SetLength(rr, meq + 1);
  SetLength(s, meq + 1);
  SetLength(a, meq * meq + 1);

  for iii := 1 to nd do
  begin
    x[iii]  := Points[iii - 1].x;
    y[iii]  := Points[iii - 1].y;
    z[iii]  := Points[iii - 1].z;
    vr[iii] := Points[iii - 1].Value;

    // Establish the external drift variable (if needed) in array ve[]
    ve[iii] := 1.0;
    if (ktype = 3) then
    begin
      //        ve[iii] = var(iextv)
      // External drift variable must be present at all data locations

    end;
  end;

  // Set up for cross validation
  if (koption = 1) then
  begin
    SetLength(Nodes, High(Points) + 1);
    for iss := 0 to High(Points) do
      Nodes[iss] := Points[iss];
  end;


  ///----------------- Main kriging subroutine starts here ---------------------

  // Set up the rotation/anisotropy matrices that are needed for the
  // variogram and search.  Also compute the maximum covariance for
  // the rescaling factor
  radsqd := radius * radius;
  covmax := c0;
  for iss := 1 to nst do
  begin
    setrot(ang1[iss], ang2[iss], ang3[iss], anis1[iss], anis2[iss], rotmat[iss]);
    if (it[iss] = 4) then
      covmax := covmax + PMX
    else
      covmax := covmax + cc[iss];
  end;
  isrot := MAXNST + 1;
  setrot(sang1, sang2, sang3, sanis1, sanis2, rotmat[isrot]);

  // Finish computing the rescaling factor and stop if unacceptable
  if (radsqd < 1.0) then
    resc := 2.0 * radius / max(covmax, 0.0001)
  else
    resc := (4.0 * radsqd) / max(covmax, 0.0001);
  if (resc <= 0.0) then
  begin
    //            write(*,*) 'ERROR KT3D: The rescaling value is wrong ',resc
    //            write(*,*) '            Maximum covariance: ',covmax
    //            write(*,*) '            search radius:      ',radius
    exit;
  end;
  resc := 1.0 / resc;

  // Set up for super block searching
  nsec := 1;
  setsupr(nx, xmn, xsiz, ny, ymn, ysiz, nz, zmn, zsiz, nd, x, y, z,
    vr, tmp, nsec, ve, sec2, sec3, nisb,
    nxsup, xmnsup, xsizsup, nysup, ymnsup, ysizsup, nzsup, zmnsup, zsizsup);
  picksup(nxsup, xsizsup, nysup, ysizsup, nzsup, zsizsup,
    rotmat[isrot], radsqd, nsbtosr, ixsbtosr, iysbtosr, izsbtosr);

  // Compute the number of drift terms, if an external drift is being
  // considered then it is one more drift term, if SK is being considered
  // then we will set all the drift terms off and mdt to 0):
  mdt := 1;
  for i := 1 to 9 do
  begin
    if (ktype = 0) or (ktype = 2) then
      idrif[i] := 0;
    if (idrif[i] < 0) or (idrif[i] > 1) then
    begin
      // 'ERROR KT3D: invalid drift term',idrif(i)
      exit;
    end;
    mdt := mdt + idrif[i];
  end;
  if (ktype = 3) then
    mdt := mdt + 1;
  if (ktype = 0) then
    mdt := 0;
  if (ktype = 2) then
    mdt := 0;

  // Set up the discretization points per block.  Figure out how many
  // are needed, the spacing, and fill the xdb,ydb, and zdb arrays with
  // the offsets relative to the block center (this only gets done once)

  // In all cases the offsets are relative to the lower left corner.
  // This is done for rescaling the drift terms in the kriging matrix.
  if (nxdis < 1) then
    nxdis := 1;
  if (nydis < 1) then
    nydis := 1;
  if (nzdis < 1) then
    nzdis := 1;
  ndb := nxdis * nydis * nzdis;
  SetLength(xdb, ndb + 1);
  SetLength(ydb, ndb + 1);
  SetLength(zdb, ndb + 1);
  xdis := xsiz / nxdis;
  ydis := ysiz / nydis;
  zdis := zsiz / nzdis;
  i    := 0;
  xloc := -0.5 * (xsiz + xdis);
  for ix := 1 to nxdis do
  begin
    xloc := xloc + xdis;
    yloc := -0.5 * (ysiz + ydis);
    for iy := 1 to nydis do
    begin
      yloc := yloc + ydis;
      zloc := -0.5 * (zsiz + zdis);
      for iz := 1 to nzdis do
      begin
        zloc   := zloc + zdis;
        i      := i + 1;
        xdb[i] := xloc + 0.5 * xsiz;
        ydb[i] := yloc + 0.5 * ysiz;
        zdb[i] := zloc + 0.5 * zsiz;
      end;
    end;
  end;

  // Calculate Block Covariance. Check for point kriging.
  cova3(xdb[1], ydb[1], zdb[1], xdb[1], ydb[1], zdb[1], nst, c0, it,
    cc, aa, rotmat, cmax, cov);

  // Set the ``unbias'' variable so that the matrix solution is more stable
  unbias := cov;
  cbb    := cov;
  if (ndb > 1) then
  begin
    cbb := 0.0;
    for i := 1 to ndb do
    begin
      for j := 1 to ndb do
      begin
        cova3(xdb[i], ydb[i], zdb[i], xdb[j], ydb[j], zdb[j], nst,
          c0, it, cc, aa, rotmat, cmax, cov);
        if (i = j) then
          cov := cov - c0;
        cbb := cbb + cov;
      end;
    end;
    cbb := cbb / (ndb * ndb);
  end;

  // Mean values of the drift functions
  for i := 1 to 9 do
    bv[i] := 0.0;
  for i := 1 to ndb do
  begin
    bv[1] := bv[1] + xdb[i];
    bv[2] := bv[2] + ydb[i];
    bv[3] := bv[3] + zdb[i];
    bv[4] := bv[4] + xdb[i] * xdb[i];
    bv[5] := bv[5] + ydb[i] * ydb[i];
    bv[6] := bv[6] + zdb[i] * zdb[i];
    bv[7] := bv[7] + xdb[i] * ydb[i];
    bv[8] := bv[8] + xdb[i] * zdb[i];
    bv[9] := bv[9] + ydb[i] * zdb[i];
  end;
  for i := 1 to 9 do
    bv[i] := (bv[i] / ndb) * resc;

  with ProgressBar do
  begin
    Min      := Low(Nodes);
    Max      := High(Nodes) + 1;
    Position := Min;
    Step     := 1;
  end;

  //  if (koption=0) then nloop := High(Nodes)+1 else nloop := 10000000;  /// why ???
  nloop := High(Nodes) + 1;

  // MAIN LOOP OVER ALL THE BLOCKS IN THE GRID
  for index := 1 to nloop do
  begin
    estimated := False;

    xloc   := Nodes[index - 1].x;
    yloc   := Nodes[index - 1].y;
    zloc   := Nodes[index - 1].z;
    truee  := Nodes[index - 1].Value;
    ///        extest := Nodes[index-1].ext_value;
    extest := 1;

    // Where are we making an estimate?
{    if (koption=0) then begin
      xloc := Nodes[index-1].x;
      yloc := Nodes[index-1].y;
      zloc := Nodes[index-1].z;
    end
    else begin
      xloc := Jackknife[index-1].x;
      yloc := Jackknife[index-1].y;
      zloc := Jackknife[index-1].z;
      truee := Jackknife[index-1].value;
//        extest := Jackknife[index-1].ext_value;
    end;
}

    // Read in the external drift variable for this grid node if needed
    if (ktype = 2) or (ktype = 3) then
    begin
      if (koption = 0) then
      begin
        ///            extest = var(iextve)
        ///               extest := Nodes[index].ext_drift_var;
      end;
      if (extest < tmin) or (extest >= tmax) then
      begin
        est  := UNEST;
        estv := UNEST;
        estimated := True;
      end;
      if not estimated then
        resce := covmax / max(extest, 0.0001);
    end;

    if not estimated then
    begin
      // Finds the nearest samples
      srchsupr(xloc, yloc, zloc, radsqd, rotmat[isrot], nsbtosr,
        ixsbtosr, iysbtosr, izsbtosr, noct, nd, x, y, z, tmp,
        nisb, nxsup, xmnsup, xsizsup, nysup, ymnsup, ysizsup,
        nzsup, zmnsup, zsizsup, nclose, close, infoct);

      // Loads the nearest data in xa,ya,za,vra,vea
      na := 0;
      for i := 1 to nclose do
      begin
        ind    := Trunc(Close[i] + 0.5);
        accept := True;
        if (koption <> 0) and ((abs(x[ind] - xloc) + abs(y[ind] - yloc) +
          abs(z[ind] - zloc)) < EPSLON) then
          accept := False;
        if (accept) then
          if (na < ndmax) then
          begin
            na      := na + 1;
            xa[na]  := x[ind] - xloc + 0.5 * xsiz;
            ya[na]  := y[ind] - yloc + 0.5 * ysiz;
            za[na]  := z[ind] - zloc + 0.5 * zsiz;
{
            xa[na] := x[ind];
            ya[na] := y[ind];
            za[na] := z[ind];
}
            vra[na] := vr[ind];
            vea[na] := ve[ind];
          end;
      end;

      // Test number of samples found
      if (na < ndmin) then
      begin
        est  := UNEST;
        estv := UNEST;
        estimated := True;
        Inc(nfound);  //debug
      end;

      // Test if there are enough samples to estimate all drift terms
      if (na >= 1) and (na <= mdt) then
      begin
        //            ' Encountered a location where there were too few data ',/,
        //            ' to estimate all of the drift terms but there would be',/,
        //            ' enough data for OK or SK.   KT3D currently leaves ',/,
        //            ' these locations unestimated.'
        est  := UNEST;
        estv := UNEST;
        estimated := True;
        Inc(ndrift);
      end;

      if not estimated then
      begin
        // There are enough samples - proceed with estimation.
        if (na <= 1) then
        begin
          // Handle the situation of only one sample
          cova3(xa[1], ya[1], za[1], xa[1], ya[1], za[1], nst, c0,
            it, cc, aa, rotmat, cmax, cb1);

          // Establish Right Hand Side Covariance
          if (ndb <= 1) then
            cova3(xa[1], ya[1], za[1], xdb[1], ydb[1], zdb[1], nst,
              c0, it, cc, aa, rotmat, cmax, cb)
          else
          begin
            cb := 0.0;
            for i := 1 to ndb do
            begin
              cova3(xa[1], ya[1], za[1], xdb[i], ydb[i], zdb[i], nst, c0,
                it, cc, aa, rotmat, cmax, cov);
              cb := cb + cov;
              dx := xa[1] - xdb[i];
              dy := ya[1] - ydb[i];
              dz := za[1] - zdb[i];
              if ((dx * dx + dy * dy + dz * dz) < EPSLON) then
                cb := cb - c0;
            end;
            cb := cb / ndb;
          end;
          est  := vra[1];
          estv := cbb - 2.0 * cb + cb1;
          estimated := True;
        end;   // if na<=1

        if not estimated then
        begin
          // Go ahead and set up the OK portion of the kriging matrix
          neq := mdt + na;

          // Initialize the main kriging matrix
          First := False;
          for i := 1 to neq * neq do
            a[i] := 0.0;

          // Fill in the kriging matrix
          for i := 1 to na do
            for j := i to na do
            begin
              cova3(xa[i], ya[i], za[i], xa[j], ya[j], za[j], nst,
                c0, it, cc, aa, rotmat, cmax, cov);
              a[neq * (i - 1) + j] := cov;
              a[neq * (j - 1) + i] := cov;
            end;

          // Fill in the OK unbiasedness portion of the matrix (if not doing SK)
          if (neq > na) then
            for i := 1 to na do
            begin
              a[neq * (i - 1) + na + 1] := unbias;
              a[neq * na + i] := unbias;
            end;

          // Set up the right hand side
          for i := 1 to na do
          begin
            if (ndb <= 1) then
              cova3(xa[i], ya[i], za[i], xdb[1], ydb[1], zdb[1], nst, c0,
                it, cc, aa, rotmat, cmax, cb)
            else
            begin
              cb := 0.0;
              for j := 1 to ndb do
              begin
                cova3(xa[i], ya[i], za[i], xdb[j], ydb[j], zdb[j], nst, c0,
                  it, cc, aa, rotmat, cmax, cov);
                cb := cb + cov;
                dx := xa[i] - xdb[j];
                dy := ya[i] - ydb[j];
                dz := za[i] - zdb[j];
                if ((dx * dx + dy * dy + dz * dz) < EPSLON) then
                  cb := cb - c0;
              end;
              cb := cb / ndb;
            end;
            r[i] := cb;
          end;
          if (neq > na) then
            r[na + 1] := unbias;

          // Add the additional unbiasedness constraints
          im := na + 1;

          /// Process all drift terms
          for i := 1 to 9 do
            if idrif[i] = 1 then
            begin
              Inc(im);
              case i of
                1: t := xa[k] * resc;   // First drift term (linear in "x")
                2: t := ya[k] * resc;   // Second drift term (linear in "y")
                3: t := za[k] * resc;   // Third drift term (linear in "z")
                4: t := xa[k] * xa[k] * resc;  // Fourth drift term (quadratic in "x")
                5: t := ya[k] * ya[k] * resc;  // Fifth drift term (quadratic in "y")
                6: t := za[k] * za[k] * resc;  // Sixth drift term (quadratic in "z")
                7: t := xa[k] * ya[k] * resc;  // Seventh drift term (quadratic in "xy")
                8: t := xa[k] * za[k] * resc;  // Eighth drift term (quadratic in "xz")
                9: t := ya[k] * za[k] * resc;  // Ninth drift term (quadratic in "yz")
              end;
              for k := 1 to na do
              begin
                a[neq * (im - 1) + k] := t;
                a[neq * (k - 1) + im] := t;
              end;
              r[im] := bv[i];
            end;

          // External drift term (specified by external variable)
          if (ktype = 3) then
          begin
            im := im + 1;
            for k := 1 to na do
            begin
              a[neq * (im - 1) + k] := vea[k] * resce;
              a[neq * (k - 1) + im] := vea[k] * resce;
            end;
            r[im] := extest * resce;
          end;

          // Copy the right hand side to compute the kriging variance later
          for k := 1 to neq do
            rr[k] := r[k];
          kadim := neq * neq;
          ksdim := neq;
          nrhs  := 1;
          nv    := 1;

          // If estimating the trend then reset all the right hand side terms=0.0
          if (itrend >= 1) then
            for i := 1 to na do
            begin
              r[i]  := 0.0;
              rr[i] := 0.0;
            end;

          // Solve the kriging system
          ktsol(neq, nrhs, nv, a, r, s, ising);

          // Compute the solution
          if (ising <> 0) then
          begin
            est  := UNEST;
            estv := UNEST;
            Inc(nktsol);     // debug
          end
          else
          begin
            est  := 0.0;
            estv := cbb;
            if (ktype = 2) then
              skmean := extest;
            for j := 1 to neq do
            begin
              estv := estv - s[j] * rr[j];
              if (j <= na) then
              begin
                if (ktype = 0) then
                  est := est + s[j] * (vra[j] - skmean)
                else if (ktype = 2) then
                  est := est + s[j] * (vra[j] - vea[j])
                else
                  est := est + s[j] * vra[j];
              end;
            end;
            if (ktype = 0) or (ktype = 2) then
              est := est + skmean;
          end;
        end;
      end;
    end;
    // END OF MAIN KRIGING LOOP


    /// Modificated block below ! (MR)
    if (koption = 0) or (koption = 2) then
    begin
      Nodes[index - 1].Value    := est;
      Nodes[index - 1].variance := estv;
      Nodes[index - 1].error    := 0;
    end
    else
    begin
      err := UNEST;
      if (truee <> UNEST) and (est <> UNEST) then
        err := est - truee;
      Nodes[index - 1].Value := est;
      Nodes[index - 1].variance := estv;
      Nodes[index - 1].error := err;
      Nodes[index - 1].x := xloc;
      Nodes[index - 1].y := yloc;
      Nodes[index - 1].z := zloc;
      ///      Nodes[index-1].value := truee;
      ///      Nodes[index-1].value := est;
      ///      Nodes[index-1].value_variance := estv;
      ///      Nodes[index-1].err := err;
    end;
    ProgressBar.StepIt;
  end;

  /// debug messages
  ///  ShowMessage('found='+IntToStr(nfound));
  ///  ShowMessage('drift='+IntToStr(ndrift));
  ///  ShowMessage('ktsol='+IntToStr(nktsol));

  // All finished the kriging
  SetLength(a, 0);
  SetLength(r, 0);
  SetLength(rr, 0);
  SetLength(s, 0);
  SetLength(xa, 0);
  SetLength(ya, 0);
  SetLength(za, 0);
  SetLength(vra, 0);
  SetLength(vea, 0);
  SetLength(xdb, 0);
  SetLength(ydb, 0);
  SetLength(zdb, 0);
  SetLength(x, 0);
  SetLength(y, 0);
  SetLength(z, 0);
  SetLength(vr, 0);
  SetLength(ve, 0);
  SetLength(tmp, 0);
  SetLength(Close, 0);
end;

end.
