//
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//
unit uOptimizeLG;

interface

uses
  System.Math,
  Vcl.ComCtrls,
  cGlobals,
  uCommon,
  cInterpol;

(*
 Source code LG23D.F was adopted from paper of P.A.Dowd (1994)
 Lerchs-Grossman method for determining open pit limits

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

function OptimizeLG(var InBlocks: TCoordinateArray;
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
procedure LerchsGrossman;
var
  s: Single; //sum of values
  cpm: Single;
  i, j, k, l, m, n: Integer;
  la, l1, l2, lg, lk, lkm, lir, lt, ltr, ltc, lar, lsw, lcon : Integer;
  mbs, mbw, mes, mew, md, mdl, mc, mem, ml: Integer;
  na, na1, na2, naf, nc, ne, nl, nel, ny, n1, n2, nf, nn, nod, nodl, ndc, nz : Integer;
  nem, node, nit, nitk, nnd : Integer;
  im, jm, ipkm, ipa, ipk, ipl, ip, iq: Integer;
  knm : Integer;
  kl, km, kn, ks, kt, kollu, kol, koll: Integer;
  ntk, nts, nds, ndw, ntw: Integer;
  sm : Single;
  label 25, 30, 32, 35, 40, 41, 42, 45, 50, 55, 60, 65, 70, 75,
        80, 90, 95, 100, 105, 110, 120, 125, 130, 135, 140, 145, 150, 155,
        160, 165, 170, 175, 180, 185, 190, 195,
        200, 205, 210, 215, 220, 230, 235, 240, 245,
        250, 255, 260, 265, 270, 275, 280, 285, 290,
        300, 305, 310, 315, 320, 325, 330, 335, 340,
        345, 350, 355, 360, 365, 370, 380, 400, 420, 450;

  {sub} procedure coord(n, numx, numy, k, j, i: Integer);
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
  for i := 1 to numx do     //do 20
    for j := 1 to numy do    // do 20
    begin
      if (val[i,j,1] <= 0) then break; // goto 20
      s := s + val[i,j,1];
      log[i,j,1] := 1;
      iplan[i,j] := 1;
{20}end; // continue

// Increment the level counter k by 1 and add the blocks on the k-th level
     k := 1;
25:  if (k >= numz) then goto 400 { print results}
     else
     Inc(k); //k++;
30:  if(lk >= 0) then goto 35;
32:  ks := k;
      goto 380;
35:  ltr := l;

// Connect blocks on Kth level to the root and establish trees
// itree - contains tree number
// d - contains value of tree

40:  nts := iroot[ltr,1];
     nds := itree[nts];
     if(d[nds] > 0) then goto 55;
41:  if (ltr < lk) then
     begin
       ltr := ltr+1;
       goto 40;
     end; //endif
42:  ltc := l;
45:  if (ltc > lk) then
     begin
       ks := k;
       goto 380;
     end; //endif
     nts := iroot[ltc, 1];
     nds := itree[nts];
     if (d[nds] <= 0) then goto 50;
     lar := ltc;
     lsw := 4;
     goto 340;

50:  Inc(ltc);          //ltc++;
     goto 45;

55:  lar := ltr;
60:  lsw := l;
     goto 340;
65:  coord(node, numx, numy, kl, j, i);
     if (kl = l) then goto 350;
70:  ny := (lg - 1)*numx*numy+(n-1)*numx + m;
     for i := 1 to lk do
     begin //do 80
       lir := l;
       ntw := iroot[l, 1];
       ndw := itree[ntw];
       if (d[ndw] > 0) then  goto 80;
       ntk := ntw + iroot[l,2] - l;
       for lt := ntw to ntk do
       begin //do 75
          na := itree[lt];
          na1 := nd[na, 1];
          na2 := nd[na, 2];
          if (ny = na1) or
             (ny = na2) then goto 95;
75:    end;  //continue
80:  end;  //continue
     goto 350;
90:  ny := (lg-l)*numx*numy+(n-l)*numx + m;
     cpm := val[m,n,lg];
     log[m,n,lg] := 2;
     Inc(ne);             //ne++;
     nd[ne, l] := 0;
     nd[ne, 2] := ny;
     d[ne] := cpm;
     itree[ne]:= ne;
     Inc(lk);             // ik++;
     iroot[lk, 1] := ne;
     iroot[lk, 2] := l;
     if(lkm < lk) then lkm := lk;
     if (nem < ne) then nem := ne;
     lir := lk;
95:  nd[nds, l] := node;
     nd[nds, 2] := ny;
     mbw := iroot[lir, 1];
     mew := iroot[lir, 2] + mbw - 1;
     mbs := iroot[lar, 1];
     mes := mbs + iroot[lar,2] - 1;
     iroot[lir, 2] := iroot[lir, 2] + iroot[lar, 2];
     iroot[lar, 1] := 0;
     iroot[lar, 2] := 0;
     if (mew + l = mbs) then
       goto 100
     else
       goto 140; //? 120;

100:  ires := itree[mbs];
      n1 := mew + l;
      n2 := mbs - l;

      for n:= n1 to n2 do
      begin //do 105
        nf := n2 - n + n1;
        itree[nf+1] := itree[nf];
      end;  //continue
105:  itree[mew + l] := ires;
      for l := 1 to lk do
      begin // do 110
        if (iroot[l,l] = 0) then goto 110;
        if not((iroot[l, l] > mew) and (iroot[l,1] <= mbs)) then
       {+} goto 110;
        iroot[l, 1] := iroot[l, 1] + 1;
      end; //continue
110:  if (mbs = mes) then goto 140;
      mbs := mbs + 1;
      mew := mew+1;
      goto 100;

120:  for m := mbs to mes do
      begin  //do 135
        ires := itree[mbs];
        n1 := mbs + 1;
        n2 := mew;
        for n := n1 to n2 do
        begin//do 125
          itree[n - 1] := itree[n];
        end; //continue
125:    itree[mew] := ires;
        mbw := mbw - 1;
        for l := 1 to lk do
        begin  //do 130
          if (iroot[l,l] = 0) then goto 130;
          if not((iroot[l,l] >= mbs) and (iroot[l,l] <= mew)) then
          {+} goto 130;
          iroot[l, 1] := iroot[l, 1] - 1;
130:    end; //continue
135:  end; //continue

140:  lcon := l;
      goto 310;
145: //continue
      ipa := ip;
150:  n := ipath[ipa, 0];
      if (n = nds) then goto 155;
      d[n] := d[nds] - d[n];
      ipa := ipath[ipa, 2];
      if (ipa <> 0) then goto 150;
155:  lar := lir;
      lsw := 3;
      goto 340;
160:  if (node <> ny) then goto 350;
      ipa := ip;
165:  nn := ipath[ipa, 1];
      d[nn] := d[nn] + d[nds];
      ipa := ipath[ipa, 2];
      if (ipa <> 0) then goto 165;
170:  kn := 1;
      norm[kn] := lir;
175:  for kt := l to kn do
      begin //do 180
        if (norm[kt] = 0) then break; //goto 180
        lar := norm[kt];
        lsw := 2;
        goto 340;
      end; //continue
180:  goto 30;
185: //continue
      for ip := l to ipk do
      begin //do 190
        if (ip = 1) then break;   // goto 190
        md := ipath[ip, 1];
        nod := abs(ipath[ip, 2]);
        if (ipath[ip, 2] < 0) and (d[md] <= 0) then goto 195;
        if (ipath[ip, 2] > 0) and (d[md] > 0) then goto 195;
      end;  //continue
190:  norm[kt] := 0;
      goto 175;
195:  nd[md, 1] := 0;
      nd[md, 2] := nod;
      nodl := nod;
      ipl := ip;
200:  iq := ipath[ip, 2];
      mdl := ipath[iq, 0];
      d[mdl] := d[mdl] - d[md];
      if (ipath[iq, 2] = 0) then goto 205;
      ip := iq;
      goto 200;

205:  for iq := ipl to ipk do
      begin //do 230
        mc := ipath[iq, 1];
        ndc := abs(ipath[iq, 2]);
        naf := ipath[iq, 2];
        if (ndc = nodl) then goto 215;
        ip := naf;
210:    if (ip = ipl) then goto 215;
        ip := ipath[ip, 2];
        if (ip < ipl) then goto 230;
        goto 210;
215:    for n := nit to nitk do
        begin //do 225
          if (itree[n] <> mc) then break; // goto 225;
          if (n = nitk) then break; //goto 225
          mem := itree[n];
          n1 := n + l;
          for nz := n1 to nitk do
          begin //do 220
            itree[nz - 1] := itree[nz];
          end; //	continue
220:      itree[nitk] := mem;
          goto 230;
        end; //continue
      end; //continue
230:  for n := nit to nitk do
      begin //do 235
        if (itree[n] = md) then break; //goto 240;
      end; //continue
240:  iroot[lar, 2] := n - nit;
      Inc(lk);           //lk++;
      iroot[lk, 1] := n;
      iroot[lk, 2] := nitk - n + l;
      Inc(kn);           //kn++;
      norm[kn] := lk;
      if (knm < kn) then knm := kn;
      if (lkm < lk) then lkm := lk;
      goto 175;

245:  n := ipath[l, 1];
      s := s + d[n];
      for ip:=l to ipk do
      begin //do 250
        n := ipath[ip, 1];
        nd[n, 1]:=0;
        nd[n, 2]:=0;
        d[n] := 0;
        node := abs(ipath[ip, 1]);
        coord (node, numx, numy, kl, j, i);
        log[i, j, kl] := 1;
        if (iplan[i, j] < kl) then iplan[i, j] := kl;
      end; //continue
250:  nel := ne;
      n := 0;
255:  Inc(n);       //n++;
260:  if (n = ne) then goto 280;
      if (nd[n, 1] = 0) then goto 265;
      goto 255;
265:  n1 := n;
      n2 := ne - 1;
      for na := n1 to n2 do begin //do 270
         nd[na, 1] := nd[na+1, 1];
         nd[na, 2] := nd[na+l, 2];
         d[na] := d[na + 1];
      end; //continue
270:  Dec(ne);      //ne--;
      ml := nel;
      for m := 1 to ml do begin //do 275
        if (itree[m] > n) then itree[m] := itree[m] - 1;
      end; // continue
275:  goto 260;
280:  if (nd[ne, 2] = 0) then
        Dec(ne);     //ne--
      for n := nit to nitk do
      begin //do 285
        itree[n] := 0;
      end; //continue
285:  iroot[lar, 1] := 0;
      iroot[lar, 2] := 0;
      lcon := 2;
      goto 310;
290: //continue
      if (nitk = nel) then goto 300;
      nl := nitk + 1;
      for n := nl to nel do
      begin //do 295
        itree[nit + n - nl] := itree[n];
      end; //continue
300:  for l := 1 to lk do begin //do 305
        if (iroot[l, 1] < nit) then break; //goto 305
        iroot[l, 1] := iroot[l, 1] - mc;
      end; //continue
305:  goto 42;

310:  l := 0;
315:  Inc(l);   //l++;
320:  if(l = lk) then goto 335;
      if(iroot[l, 1] = 0) then goto 325;
      goto 315;
325:  l1 := l;
      l2 := lk - 1;
      for la := l1 to l2 do begin //do 330
        iroot[la, 1] := iroot[la+l, 1];
        iroot[la, 2] := iroot[la+l, 2];
      end; //continue
330:  if (lir > l1) then lir := lir - 1;
      Dec(lk);     //lk--;

335:  if(iroot[lk, 1] = 0) then
        Dec(lk);  //lk--;
      if (lk = 0) then goto 32  else goto 145; ///? 290, lcon;
340:  nit := iroot[lar, 1];
      nc := iroot[lar, 2];
      nitk := nit + nc - 1;
      ipk := l;
      nnd := itree[nit];
      ipath[l,0] := nnd;
      ipath[l,1] := nd[nnd, 1];
      ipath[l,2] := 0;
      ip := l;
345:  node := abs(ipath[ip, 1]);
      nn := ipath[ip, 0];
///?      goto 65, 350, 160, 35O, lsw;
350:  for n := nit to nitk do
      begin //do 360
        nnd := itree[n];
        if (nnd = nn) then break; //goto 360
        if (node <> nd[nnd, 1]) then goto 355;
        Inc(ipk); //ipk++;
        ipath[ipk, 1] := nd[nnd, 1];
        ipath[ipk, 0] := nnd;
        ipath[ipk, 2] := ip;
        if (ipkm < ipk) then ipkm := ipk;
        goto 360;
355:    if (node <> nd[nnd, 1]) then break; //goto 360
        Inc(ipk);  //ipk := ipk + 1;
        ipath[ipk, 1] := -nd[nnd, 0];
        ipath[ipk, 0] := nnd;
        ipath[ipk, 2] := ip;
        if (ipkm < ipk) then ipkm := ipk;
      end; //continue
360:  if (ip = ipk) then goto 365 else goto 370;
365:  Inc(ip);          //ip++;
      goto 345;
370: ///? goto (41, 185, 450, 245), lsw;
380:  im := 0;
      jm := 0;
      sm := 0;
      for i := 2 to numx - 1 do //do 385
        for j := 2 to numy - 1 do begin //do 385
          if (log[i,j,ks] > 0) then break; //goto 385
          if (val[i,j,ks] <= 0) then break; //goto 385
          if (val[i,j,ks] <= sm) then break; //goto 385
          sm := val[i,j,ks];
          im := i;
          jm := j;
          km := ks;
{385} end; //continue
      if (sm = 0) then goto 25;
      log[im, jm, km] := 2;
      Inc(ne);       //ne++;
      nd[ne, 1] := 0;
      nd[ne, 2] := (km - 1)*numx*numy + (jm - 1)*numx+im;
      d[ne] := sm;
      itree[ne] := ne;
      Inc(lk); //lk := lk+1;
      iroot[lk, 1] := ne;
      iroot[lk, 2] := 1;
      if (nem < ne) then nem := ne;
      if (lkm < lk) then lkm := lk;
      nds := ne;
      ltr := lk;
      goto 55;

(****************************************************************************)
// print results
(****************************************************************************)
400:  write(6,910);
      kollu:=0;
      kol:=0;
      koll:=0;
      for k := 1 to numz do
      for j := 1 to numy do
      for i := 1 to numx do
      begin
        if (log[i,j,k] = 1) then iplan[i,j] := k;
        if (log[i,j,k] = 1) and (val[i,j,k] < 0) then kol := kol + 1;
        if (log[i,j,k] = 1) and (val[i,j,k] > 0) then koll := koll + 1;
        if (log[i,j,k] =  1) and (val[i,j,k] <> 0) then kollu := kollu + 1;
      end; //405, 410	415 continue

      write(6,'*','total number of blocks in pitl',kollu);
      write(6,'*','number of positive blocks in pitl',koll);
      write(6,'*','humber of negative blocks in pitl',kol);
      for i:= l to numx do
      begin
        //Output of result plan;
///     write(6,905) (iplan[i,j], j := 1, numy);
      end;
(*
420: //continue
///    stop;
450:  zz := 1;
///   write(6,900) zz,nem,lkm,node
     close(4)
     close(5)
     close(6)
     stop;
*)
end; // of LG


//=================================================================

function OptimizeLG(var InBlocks: TCoordinateArray;
  var OutBlocks: TCoordinateArray; Params: TInvDistPars;
  ProgressBar: TProgressBar): Boolean;

begin
  LerchsGrossman;
end;


end.
