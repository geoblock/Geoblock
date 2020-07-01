 //-------------------------------------------------------------------------
 // This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
 //-------------------------------------------------------------------------
 {! It creates Variogram for Irregular Spaced 3-D Data}

(*C Copyright (C) 1996, The Board of Trustees of the Leland Stanford
C Junior University.  All rights reserved.
C The programs in GSLIB are distributed in the hope that they will be
C useful, but WITHOUT ANY WARRANTY.  No author or distributor accepts
C responsibility to anyone for the consequences of using them or for
C whether they serve any particular purpose or work at all, unless he
C says so in writing.  Everyone is granted permission to copy, modify
C and redistribute the programs in GSLIB, but only under the condition
C that this notice and the above copyright notice remain intact.       *)

unit uVariograms;

interface

uses
  System.Math,
  Vcl.ComCtrls,

  
  uInterpol;

const
  vtSemivariogram = 1;
  vtCrossSemivariogram = 2;
  vtCovariance   = 3;
  vtCorrelogram  = 4;
  vtGeneralRelativeSemivariogram = 5;
  vtPairwiseRelativeSemivariogram = 6;
  vtLogSemivariogram = 7;
  vtSemimadogram = 8;
  vtIndicatorSemivariogram_Continuous = 9;
  vtIndicatorSemivariogram_Categorical = 10;
  VariogramNames: array [1..10] of string =
    ('Variogram', 'CrossVariogram', 'Covariance', 'Correlogram',
    'GeneralRelativeVariogram', 'PairwiseRelativeVariogram',
    'Variogram (with Logarithms)', 'Madogram', 'IndicatorVariogram (Continuous)',
    'IndicatorVariogram (Categorical)');

{-----------------------------------------------------------------------
 The following Parameters control static dimensioning within gamv3:
   MAXDIR    maximum number of directions possible at one time
   MAXLAG    maximum number of lags at one time
   EPSLON    a small number to avoid dividing by zero
-----------------------------------------------------------------------}
  // Fixed Parameters
  MAXLG  = MAXLAG + 2;
  MXDLV  = MAXDIR * MAXLG;
  EPSLON = 1.0e-20;

procedure gamv3(Points: TCoordinateArray; var Variogram: TExpVariogram;
  ProgressBar: TProgressBar);

function CalcVariogramModelValue(m: TVariogramModel; h: double): double;

//======================================================================
implementation
//======================================================================

function CalcVariogramModelValue(m: TVariogramModel; h: double): double;
var
  i:  integer;
  s:  double;
  hr: double;
begin
  s := 0;
  for i := 1 to m.nst_count do
    with m.structures[i] do
    begin
      s  := s + nugget;
      hr := h / range;
      case model_type of
        // Spherical Variogram Model
        1: if hr < 1.0 then
            s := s + contribution * (hr * (1.5 - 0.5 * hr * hr))
          else
            s := s + contribution;
        // Exponential Variogram Model
        2: s := s + contribution * (1 - exp(-3 * hr));
        // Gaussian Variogram Model
        3: s := s + contribution * (1 - exp(-3 * hr * hr));
        // Power Variogram Model
        4: if (range > 0) and (range <= 2) then
            s := s + contribution * Power(h, range);
      end;
    end;
  Result := s;
end;

//-----------------------------------------------------------------------------

{c-----------------------------------------------------------------------
c
c              Variogram of 3-D Irregularly Spaced Data
c              ****************************************
c
c This subroutine computes a variety of spatial continuity measures of a
c set for irregularly spaced data.  The user can specify any combination
c of direct and cross variograms using any of eight "variogram" measures
c
c INPUT VARIABLES:
c   nd               Number of data (no missing values)
c   x(nd)            X coordinates of the data
c   y(nd)            Y coordinates of the data
c   z(nd)            Z coordinates of the data
c   nv               The number of variables
c   vr(nd,nv)        Data values
c   tmin,tmax        Trimming limits
c   nlag             Number of lags to calculate
c   xlag             Length of the unit lag
c   xltol            Distance tolerance (if <0 then set to xlag/2)
c   ndir             Number of directions to consider
c   azm(ndir)        Azimuth angle of direction (measured positive
c                      degrees clockwise from NS).
c   atol(ndir)       Azimuth (half window) tolerances
c   bandwh           Maximum Horizontal bandwidth (i.e., the deviation
c                      perpendicular to the defined azimuth).
c   dip(ndir)        Dip angle of direction (measured in negative
c                      degrees down from horizontal).
c   dtol(ndir)       Dip (half window) tolerances
c   bandwd           Maximum "Vertical" bandwidth (i.e., the deviation
c                      perpendicular to the defined dip).
c   isill            1=attempt to standardize, 0=do not
c   sills            the sills (variances) to standardize with
c   nvarg            Number of variograms to compute
c   ivtail(nvarg)    Variable for the tail of each variogram
c   ivhead(nvarg)    Variable for the head of each variogram
c   ivtype(nvarg)    Type of variogram to compute:
c                      1. semivariogram
c                      2. cross-semivariogram
c                      3. covariance
c                      4. correlogram
c                      5. general relative semivariogram
c                      6. pairwise relative semivariogram
c                      7. semivariogram of logarithms
c                      8. rodogram
c                      9. indicator semivariogram (continuous)
c                     10. indicator semivariogram (categorical)
c
c
c OUTPUT VARIABLES:  The following arrays are ordered by direction,
c                    then lag, i.e.,
c                      iloc = (id-1)*MAXLG+il
c   np()             Number of pairs
c   dis()            Distance of pairs falling into this lag
c   gam()            Semivariogram, covariance, correlogram,... value
c   hm()             Mean of the tail data
c   tm()             Mean of the head data
c   hv()             Variance of the tail data
c   tv()             Variance of the head data
c
c Original:  A.G. Journel                                           1978
c Revisions: K. Guertin                                             1980
c-----------------------------------------------------------------------}
procedure gamv3(Points: TCoordinateArray; var Variogram: TExpVariogram;
  ProgressBar: TProgressBar);

const
  PI = 3.14159265;

var
  x, y, z: array of double;
  vr:      array of array [1..2 + 1] of double;
  vrmin, vrmax: array[1..2 + 1] of double;
  azm, atol, bandwh, dip, dtol, bandwd: array [1..MAXDIR] of double;
  xlag, xltol, tmin, tmax: double;

  dis, gam, hm, tm, hv, tv, np: array [1..MXDLV] of double;
  sills: array [1..2 + 1] of double;
  nd, nlag, ndir, nvar, isill, ncut, indflag: integer;
  ivtail, ivhead, ivtype, ivc: integer;
  cut:   double;

  uvxazm, uvyazm, uvzdec, uvhdec, csatol, csdtol: array [1..MAXDIR] of double;
  omni: boolean;

  i, j, id, iv, jv, ii, il, it, iii, ilag: integer;
  lagbeg, lagend, nsiz: integer;
  p0, p1, dx, dy, dxy, dxs, dys, dzs, dz, azmuth, declin, xs, dismxs,
  h, hs, band: double;
  dcazm, dcdec, vrt, vrh, vrhpr, vrtpr, gamma, rnum, variance, htave: double;

begin
  // INITIALIZATION
  ivtype := Variogram.var_type;

  /// trimming limits (min and max values of coordinates)
  tmin := -1e21;
  tmax := 1e21;

  /// number of lags
  nlag := Variogram.nlag;

  /// lag separation distance
  xlag := Variogram.lag_distance;

  /// lag tolerance
  xltol := Variogram.lag_tolerance;

  /// number of directions
  ndir := Variogram.ndir;

  // azm, atol, bandwh (horizontal)
  // dip, dtol, bandwd (vertical)

  for i := 1 to ndir do
  begin
    azm[i]    := Variogram.directions[i].azimuth;
    atol[i]   := Variogram.directions[i].azimuth_tolerance;
    bandwh[i] := Variogram.directions[i].azimuth_bandwidth;
    dip[i]    := Variogram.directions[i].dip;
    dtol[i]   := Variogram.directions[i].dip_tolerance;
    bandwd[i] := Variogram.directions[i].dip_bandwidth;
  end;

  // flag to standardize sills (0=no, 1=yes)
  if Variogram.standardized then
    isill := 1
  else
    isill := 0;

  id := Variogram.cross;
  if (id < -1) or (id > 1) then
    Exit;


  // Put all the data into local variables
  // nvar - number of used variables while calcutating variogram (1 or 2 [for cross variograms])
  nd := High(Points) + 1;
  SetLength(x, nd + 1);
  SetLength(y, nd + 1);
  SetLength(z, nd + 1);
  SetLength(vr, nd + 1);
  for i := 1 to nd do
  begin
    x[i] := Points[i - 1].x;
    y[i] := Points[i - 1].y;
    z[i] := Points[i - 1].z;
    case id of
      -1:
      begin
        vr[i, 1] := Points[i - 1].Value;
        vr[i, 2] := Points[i - 1].value2;
        ivhead   := 1;
        ivtail   := 2;
        nvar     := 2;
      end;
      0:
      begin
        vr[i, 1] := Points[i - 1].Value;
        ivhead   := 1;
        ivtail   := 1;
        nvar     := 1;
      end;
      1:
      begin
        vr[i, 1] := Points[i - 1].Value;
        vr[i, 2] := Points[i - 1].value2;
        ivhead   := 2;
        ivtail   := 1;
        nvar     := 2;
      end;
    end;
  end;

  // number of cutoffs
  ncut := 0;

  if (ivtype = 9) or (ivtype = 10) then
  begin
    ncut := 1;
    // indicator threshold
    cut  := Variogram.cut;

    if ivtype = 9 then
      indflag := 1;
    if ivtype = 10 then
      indflag := 0;
    ivc := ivtail;
    ivtail := nvar + ncut;
    ivhead := nvar + ncut;
  end;

  for iv := 1 to nvar do
    sills[iv] := UNEST;

  // Construct Indicator Variables if necessary
  if ncut > 0 then
  begin
    iv := ivc;
    jv := nvar;
    p0 := 0.0;
    p1 := 0.0;
    for id := 1 to nd do
      if (vr[id, iv] <= tmin) or (vr[id, iv] > tmax) then
        vr[id, jv] := tmin - EPSLON
      else if indflag = 1 then
      begin
        if vr[id, iv] < cut then
        begin
          vr[id, jv] := 0.0;
          p0 := p0 + 1.0;
        end
        else
        begin
          vr[id, jv] := 1.0;
          p1 := p1 + 1.0;
        end;
      end
      else
      begin
        vr[id, jv] := 0.0;
        if trunc(vr[id, iv] + 0.5) = trunc(cut + 0.5) then
          vr[id, jv] := 1.0;
      end;

    p0 := p0 / max((p1 + p0), 1.0);
    sills[jv] := p0 * (1.0 - p0);
  end;

  // Establish minimums and maximums
  for i := 1 to 2 + 1 do
  begin
    vrmin[i] := 1.0e21;
    vrmax[i] := -1.0e21;
  end;
  for id := 1 to nd do
    for iv := 1 to nvar + ncut do
      if (vr[id, iv] >= tmin) and (vr[id, iv] < tmax) then
      begin
        if vr[id, iv] < vrmin[iv] then
          vrmin[iv] := vr[id, iv];
        if vr[id, iv] > vrmax[iv] then
          vrmax[iv] := vr[id, iv];
      end;

  // THE MAIN GAMV SUBROUTINE STARTS HERE !!!

  // Define the distance tolerance if it isn't already
  if (xltol <= 0.0) then
    xltol := 0.5 * xlag;

  // Define the angles and tolerance for each direction
  for id := 1 to ndir do
  begin

    // The mathematical azimuth is measured counterclockwise from EW and
    // not clockwise from NS as the conventional azimuth is:
    azmuth     := (90.0 - azm[id]) * PI / 180.0;
    uvxazm[id] := cos(azmuth);
    uvyazm[id] := sin(azmuth);
    if (atol[id] <= 0.0) then
      csatol[id] := cos(45.0 * PI / 180.0)
    else
      csatol[id] := cos(atol[id] * PI / 180.0);

    // The declination is measured positive down from vertical (up) rather
    // than negative down from horizontal
    declin     := (90.0 - dip[id]) * PI / 180.0;
    uvzdec[id] := cos(declin);
    uvhdec[id] := sin(declin);
    if (dtol[id] <= 0.0) then
      csdtol[id] := cos(45.0 * PI / 180.0)
    else
      csdtol[id] := cos(dtol[id] * PI / 180.0);
  end;

  // Initialize the arrays for each direction, variogram, and lag
  nsiz := ndir * MAXLG;
  for i := 1 to nsiz do
  begin
    np[i]  := 0.0;
    dis[i] := 0.0;
    gam[i] := 0.0;
    hm[i]  := 0.0;
    tm[i]  := 0.0;
    hv[i]  := 0.0;
    tv[i]  := 0.0;
  end;
  dismxs := sqr((nlag + 0.5 - EPSLON) * xlag);

  with ProgressBar do
  begin
    Min      := 1;
    Max      := nd;
    Position := Min;
    Step     := 1;
  end;

  // MAIN LOOP OVER ALL PAIRS

  for i := 1 to nd do
  begin
    for j := i to nd do
    begin   //label 4

      // Definition of the lag corresponding to the current pair
      dx  := x[j] - x[i];
      dy  := y[j] - y[i];
      dz  := z[j] - z[i];
      dxs := dx * dx;
      dys := dy * dy;
      dzs := dz * dz;
      hs  := dxs + dys + dzs;
      if (hs > dismxs) then
        continue;  //goto 4
      if (hs < 0.0) then
        hs := 0.0;
      h := sqrt(hs);

      // Determine which lag this is and skip if outside the defined distance
      // tolerance
      if (h <= EPSLON) then
      begin
        lagbeg := 1;
        lagend := 1;
      end
      else
      begin
        lagbeg := -1;
        lagend := -1;
        for ilag := 2 to nlag + 2 do
          if (h >= (xlag * (ilag - 2) - xltol)) and
            (h <= (xlag * (ilag - 2) + xltol)) then
          begin
            if (lagbeg < 0) then
              lagbeg := ilag;
            lagend := ilag;
          end;
        if (lagend < 0) then
          continue;   // go to 4
      end;

      // Definition of the direction corresponding to the current pair. All
      // directions are considered (overlapping of direction tolerance cones
      // is allowed)
      for id := 1 to ndir do
      begin   // label 5

        // Check for an acceptable azimuth angle
        dxy := sqrt(max((dxs + dys), 0.0));
        if (dxy < EPSLON) then
          dcazm := 1.0
        else
          dcazm := (dx * uvxazm[id] + dy * uvyazm[id]) / dxy;
        if (abs(dcazm) < csatol[id]) then
          continue;  // go to 5

        // Check the horizontal bandwidth criteria (maximum deviation
        // perpendicular to the specified direction azimuth)
        band := uvxazm[id] * dy - uvyazm[id] * dx;
        if (abs(band) > bandwh[id]) then
          continue;   // go to 5

        // Check for an acceptable dip angle
        if (dcazm < 0.0) then
          dxy := -dxy;
        if (lagbeg = 1) then
          dcdec := 0.0
        else
        begin
          dcdec := (dxy * uvhdec[id] + dz * uvzdec[id]) / h;
          if (abs(dcdec) < csdtol[id]) then
            continue; // go to 5
        end;

        // Check the vertical bandwidth criteria (maximum deviation perpendicular
        // to the specified dip direction)
        band := uvhdec[id] * dz - uvzdec[id] * dxy;
        if (abs(band) > bandwd[id]) then
          continue;  //go to 5

        // Check whether or not an omni-directional variogram is being computed
        omni := False;
        if (atol[id] >= 90.0) then
          omni := True;

        // For this variogram, sort out which is the tail and the head value
        it := ivtype;
        if (dcazm >= 0.0) and (dcdec >= 0.0) then
        begin
          vrh := vr[i, ivtail];
          vrt := vr[j, ivhead];
          if omni or (it = 2) then
          begin
            vrtpr := vr[i, ivhead];
            vrhpr := vr[j, ivtail];
          end;
        end
        else
        begin
          vrh := vr[j, ivtail];
          vrt := vr[i, ivhead];
          if omni or (it = 2) then
          begin
            vrtpr := vr[j, ivhead];
            vrhpr := vr[i, ivtail];
          end;
        end;

        // Reject this pair on the basis of missing values
        if (vrt < tmin) or (vrh < tmin) or (vrt > tmax) or (vrh > tmax) then
          continue;  // go to 6
        if (it = 2) and ((vrtpr < tmin) or (vrhpr < tmin) or (vrtpr > tmax) or
          (vrhpr > tmax)) then
          continue;  // go to 6

        //             COMPUTE THE APPROPRIATE "VARIOGRAM" MEASURE

        // The Semivariogram
        if (it = 1) or (it = 5) or (it >= 9) then
          for il := lagbeg to lagend do
          begin
            ii      := (id - 1) * MAXLG + il;
            np[ii]  := np[ii] + 1.0;
            dis[ii] := dis[ii] + h;
            tm[ii]  := tm[ii] + vrt;
            hm[ii]  := hm[ii] + vrh;
            gam[ii] := gam[ii] + (vrh - vrt) * (vrh - vrt);
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              gam[ii] := gam[ii] + (vrhpr - vrtpr) * (vrhpr - vrtpr);
            end;
          end

        // The Traditional Cross Semivariogram
        else if (it = 2) then
          for il := lagbeg to lagend do
          begin
            ii      := (id - 1) * MAXLG + il;
            np[ii]  := np[ii] + 1.0;
            dis[ii] := dis[ii] + h;
            tm[ii]  := tm[ii] + 0.5 * (vrt + vrtpr);
            hm[ii]  := hm[ii] + 0.5 * (vrh + vrhpr);
            gam[ii] := gam[ii] + (vrhpr - vrh) * (vrt - vrtpr);
          end

        // The Covariance
        else if (abs(it) = 3) then
          for il := lagbeg to lagend do
          begin
            ii      := (id - 1) * MAXLG + il;
            np[ii]  := np[ii] + 1.0;
            dis[ii] := dis[ii] + h;
            tm[ii]  := tm[ii] + vrt;
            hm[ii]  := hm[ii] + vrh;
            gam[ii] := gam[ii] + vrh * vrt;
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              gam[ii] := gam[ii] + vrhpr * vrtpr;
            end;
          end

        // The Correlogram
        else if (it = 4) then
          for il := lagbeg to lagend do
          begin
            ii      := (id - 1) * MAXLG + il;
            np[ii]  := np[ii] + 1.0;
            dis[ii] := dis[ii] + h;
            tm[ii]  := tm[ii] + vrt;
            hm[ii]  := hm[ii] + vrh;
            hv[ii]  := hv[ii] + vrh * vrh;
            tv[ii]  := tv[ii] + vrt * vrt;
            gam[ii] := gam[ii] + vrh * vrt;
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              hv[ii]  := hv[ii] + vrhpr * vrhpr;
              tv[ii]  := tv[ii] + vrtpr * vrtpr;
              gam[ii] := gam[ii] + vrhpr * vrtpr;
            end;
          end

        // The Pairwise Relative
        else if (it = 6) then
          for il := lagbeg to lagend do
          begin
            ii := (id - 1) * MAXLG + il;
            if (abs(vrt + vrh) > EPSLON) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrt;
              hm[ii]  := hm[ii] + vrh;
              gamma   := 2.0 * (vrt - vrh) / (vrt + vrh);
              gam[ii] := gam[ii] + gamma * gamma;
            end;
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) and (abs(vrtpr + vrhpr) > EPSLON) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              gamma   := 2.0 * (vrt - vrh) / (vrt + vrh);
              gam[ii] := gam[ii] + gamma * gamma;
            end;
          end

        // Variogram of Logarithms
        else if (it = 7) then
          for il := lagbeg to lagend do
          begin
            ii := (id - 1) * MAXLG + il;
            if (vrt > EPSLON) and (vrh > EPSLON) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrt;
              hm[ii]  := hm[ii] + vrh;
              gamma   := ln(vrt) - ln(vrh);
              gam[ii] := gam[ii] + gamma * gamma;
            end;
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) and (vrtpr > EPSLON) and
              (vrhpr > EPSLON) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              gamma   := ln(vrt) - ln(vrh);
              gam[ii] := gam[ii] + gamma * gamma;
            end;
          end

        // Madogram
        else if (it = 8) then
          for il := lagbeg to lagend do
          begin
            ii      := (id - 1) * MAXLG + il;
            np[ii]  := np[ii] + 1.0;
            dis[ii] := dis[ii] + h;
            tm[ii]  := tm[ii] + vrt;
            hm[ii]  := hm[ii] + vrh;
            gam[ii] := gam[ii] + abs(vrh - vrt);
            if omni and (vrtpr >= tmin) and (vrhpr >= tmin) and
              (vrtpr < tmax) and (vrhpr < tmax) then
            begin
              np[ii]  := np[ii] + 1.0;
              dis[ii] := dis[ii] + h;
              tm[ii]  := tm[ii] + vrtpr;
              hm[ii]  := hm[ii] + vrhpr;
              gam[ii] := gam[ii] + abs(vrhpr - vrtpr);
            end;
          end;

        // Finish loops over directions and the double data loops
      end;
    end;
    ProgressBar.StepIt;
  end;

  // Get average values for gam, hm, tm, hv, and tv, then compute
  // the correct "variogram" measure
  for id := 1 to ndir do
    for il := 1 to nlag + 2 do
    begin
      i := (id - 1) * MAXLG + il;
      if np[i] <= 0.0 then
        continue;  // go to 7
      rnum   := np[i];
      dis[i] := dis[i] / rnum;
      gam[i] := gam[i] / rnum;
      hm[i]  := hm[i] / rnum;
      tm[i]  := tm[i] / rnum;
      hv[i]  := hv[i] / rnum;
      tv[i]  := tv[i] / rnum;
      it     := ivtype;

      // Attempt to standardize
      if (isill = 1) and (ivtail = ivhead) then
      begin
        iii := ivtail;
        if ((it = 1) or (it >= 9)) and (sills[iii] > 0.0) then
          gam[i] := gam[i] / sills[iii];
      end;

      if (it = 1) or (it = 2) then
        gam[i] := 0.5 * gam[i]
      else if (abs(it) = 3) then
      begin
        gam[i] := gam[i] - hm[i] * tm[i];
        if (it < 0) then
          if (sills[ivtail] < 0.0) or (sills[ivhead] < 0.0) then
            gam[i] := UNEST
          else
          begin
            variance := sqrt(sills[ivtail]) * sqrt(sills[ivhead]);
            gam[i]   := variance - gam[i];
          end;
      end
      else if (it = 4) then
      begin
        hv[i] := hv[i] - hm[i] * hm[i];
        if hv[i] < 0.0 then
          hv[i] := 0.0;
        hv[i] := sqrt(hv[i]);
        tv[i] := tv[i] - tm[i] * tm[i];
        if tv[i] < 0.0 then
          tv[i] := 0.0;
        tv[i] := sqrt(tv[i]);
        if (hv[i] * tv[i]) < EPSLON then
          gam[i] := 0.0
        else
          gam[i] := (gam[i] - hm[i] * tm[i]) / (hv[i] * tv[i]);

        // Square "hv" and "tv" so that we return the variance
        hv[i] := hv[i] * hv[i];
        tv[i] := tv[i] * tv[i];
      end
      else if it = 5 then
      begin
        htave := 0.5 * (hm[i] + tm[i]);
        htave := htave * htave;
        if htave < EPSLON then
          gam[i] := 0.0
        else
          gam[i] := gam[i] / htave;
      end
      else if it >= 6 then
        gam[i] := 0.5 * gam[i];
    end;
  // END OF MAIN LOOP OVER ALL PAIRS

  SetLength(x, 0);
  SetLength(y, 0);
  SetLength(z, 0);
  SetLength(vr, 0);

  // Store the results
  SetLength(Variogram.DataV, ndir * (nlag + 2));

  // Loop over all the directions (note the direction in the title)
  j := 0;
  for id := 1 to ndir do
    for il := 1 to nlag + 2 do
    begin
      i := (id - 1) * MAXLG + il;
      Variogram.DataV[j].lag := il;
      Variogram.DataV[j].direction := id;
      Variogram.DataV[j].distance := dis[i];
      Variogram.DataV[j].Value := gam[i];
      Variogram.DataV[j].npair := trunc(np[i]);
      Variogram.DataV[j].head_mean := hm[i];
      Variogram.DataV[j].tail_mean := tm[i];
      Variogram.DataV[j].head_variance := hv[i];
      Variogram.DataV[j].tail_variance := tv[i];
      Inc(j);
    end;
end;

end.
