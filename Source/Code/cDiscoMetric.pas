//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------

(* Area/Volume Library Routines.
   This unit contains routines to compute areas and volumes
   of various geometric objects
*)


unit cDiscoMetric;

interface

uses
   System.Math,
   cDiscoCore;

//Area
{! Returns the area of a right or obtuse triangle}
function AreaTriangle(a, b, c: double): double;
{! Returns the area of a polygon}
function AreaPolygon2D(Polygon: TPolyline2D): double;
{! Returns the area of a circle}
function AreaCircle(GMode: byte; dia, circum: double): double;
{! Returns the area of a circular sector}
function AreaCircularSector(GMode: byte; al, rad, aa, sa, ea: double): double;

{! Returns the area of a circular segment}
function AreaCircularSegment(al, rad, Height, chord: double): double;
{!The function returns the area under the cycloid curve given the radius
  (rad) of the generating circle}
function AreaCycloid(rad: double): double;
{!  Returns the area of an ellipse}
function AreaEllipse(majaxis, minaxis: double): double;
{!  Computes the area of a fillet or ribbon}
function AreaFillet(GMode: byte; rad, chord: double): double;
{!  Returns the area of a hyperbola}
function AreaHyperbola(A, B: double; p1: TPoint2D): double;
{!   Returns the area of a parabola}
function AreaParabola(p1, v1: TPoint2D): double;
{!  Returns the area of a segment of a parabola}
function AreaParabolaSegment(p1, p2: TPoint2D; sh: double): double;
{!  Returns the area of an irregular polygon}
function AreaPoly(poly: xypolygon; nrpnts: longint): double;
{!   Returns the area of a regular polygon}
function AreaRectangle(A, B: double): double;
{!  Returns the area of a square, rectangle or parallelogram}
function AreaRegPolygon(GMode: byte; rad, sl: double; nrsides: longint): double;

{!Returns the area of a ring}
function AreaRing(intrad, extrad: double): double;
{! Returns the area of a ring sector}
function AreaRingSector(aa, intrad, outrad: double): double;
{! Returns the area of a trapezoid}
function AreaTrapezoid(A, B, h: double): double;

//Volume
{! Returns the volume of a cone }
function VolumeCone(rad, Height: double): double;
{! Computes the volume of a cylinder }
function VolCylinder(rad, h: double): double;
{! Returns the volume of an ellipsoid }
function VolEllipsoid(A, B: double; c: double): double;
{! Returns the volume of a frustum of a cone }
function VolFrustumCone(brad, trad, Height: double): double;
{! Returns the volume of the frustum of a pyramid}
function VolFrustumPyramid(BaseArea, TopArea, h: double): double;
{! Returns the volume of a hollow cylinder}
function VolHCylinder(irad, orad, h: double): double;
{! Returns the volume of a hollow sphere}
function VolHSphere(odia, idia: double): double;
{!   Returns the volume of a paraboloid}
function VolParaboloid(rad, Height: double): double;
{!  Returns the volume of a paraboloidal segment}
function VolParaboloidalSeg(rad1, rad2, Height: double): double;
{!   Returns the volume of a prism}
function VolPrism(areaendsurface, h: double): double;
{! The function returns the volume of a portion of a cylinder}
function VolPtCyl(GMode: byte; A, B: double; dia, h1, h2: double): double;

{! Returns the volume of a pyramid }
function VolPyramid(basearea, h: double): double;
{! Returns the volume of a rectangular solid }
function VolumeRectSolid(s1, s2, s3: double): double;
{!  Returns the volume of a sphere}
function VolSphere(dia: double): double;
{!   Returns the volume of a sphereical sector}
function VolSphereSector(Height, dia: double): double;
{!  Returns the volume of a spherical segment}
function VolSphereSeg(rad, Height, Width: double): double;
{!   Returns the volume of a spherical wedge}
function VolSphereWedge(wangle, rad: double): double;
{!  Returns the volume of a spherical zone}
function VolSphereZone(wtzone, wbzone, Height: double): double;
{!   Returns the volume of a torus}
function VolTorus(rad, dia: double): double;

//====================================================================
implementation
//====================================================================

 {                 A R E A   R O U T I N E S                   }
function AreaTriangle(a, b, c: double): double;
var
  s: double;   //Length of the perimeter divided by 2*)
  sa, sb, sc: double;  //S minus sides A, B and C respectively*)
begin
  s      := 0.5 * (a + b + c);
  sa     := s - a;
  sb     := s - b;
  sc     := s - c;
  Result := sqrt(s * sa * sb * sc);
end;

function AreaPolygon2D(Polygon: TPolyline2D): double;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to High(Polygon) do
  begin
    Result := Result + (Polygon[I].YStart + Polygon[I].YEnd) *
      (Polygon[I].XEnd - Polygon[I].XStart);
  end;
  Result := Abs(Result / 2);
end;

function AreaCircle(GMode: byte; dia, circum: double): double;
begin
  Result := 0.0;
  case GMode of
    0: Result := PI * (dia / 2) * (dia / 2); //Use diameter
    1: Result := PI * (circum / (2 * PI)) * (circum / (2 * PI));
  end;
end;

function AreaCircularSector(GMode: byte; al, rad, aa, sa, ea: double): double;
begin
  Result := 0.0;
  case GMode of
    0: // ArcLength and Radius given
      Result := 0.5 * rad * al;
    1: //ArcLength and Angle given
      Result := ((ARADIAN * al) / aa) * 0.5 * al;
    2: //ArcLength and Start and End Angle given
      Result := ((ARADIAN * al) / (ea - sa)) * 0.5 * al;
    3:  //Radius and Angle given
      Result := (DegToRad(1.0) / 2) * aa * (rad * rad);
    4:  //Radius and Start and End Angle given
      Result := (DegToRad(1.0) / 2) * (ea - sa) * (rad * rad);
    else
      Result := 0;
  end;
end;

function AreaCircularSegment(al, rad, Height, chord: double): double;
begin
  Result := 0.5 * ((rad * al) - chord * (rad - Height));
end;

function AreaCycloid(rad: double): double;
begin
  Result := 3 * PI * rad * rad;
end;

function AreaEllipse(majaxis, minaxis: double): double;
begin
  Result := PI * majaxis * minaxis;
end;

function AreaFillet(GMode: byte; rad, chord: double): double;
var
  srad: double;
begin
  Result := 0.0;
  case GMode of
    0: //Use Radius to compute Area
    begin
      srad   := rad * rad;
      Result := srad - ((PI * srad) / 4);
    end;
    1: //Use Chord Length to Compute Area
      Result := 0.1075 * chord * chord;
    else
      Result := 0;
  end;
end;

function AreaHyperbola(A, B: double; p1: TPoint2D): double;
var
  xy, ab, hyplog: double;
begin
  if (a >= 0) and (b >= 0) then
  begin
    xy     := p1.x * p1.y;
    ab     := a * b;
    hyplog := ln((p1.x / a) + (p1.y / b));
    Result := (xy / 2) - ((ab / 2) * hyplog);
  end
  else
    Result := 0.0;
end;

//! It returns the area of a regular porabola
function AreaParabola(p1, v1: TPoint2D): double;
var
  dx, dy: double;
begin
  dx := abs(p1.x - v1.x);
  dy := abs(p1.y - v1.y);
  if (dx >= 0) or (dy >= 0) then
    Result := (2.0 / 3.0) * dx * dy
  else
    Result := 0.0;
end;

function AreaParabolaSegment(p1, p2: TPoint2D; sh: double): double;
var
  a1, a2: double;
begin
  a1 := abs((p1.y * p1.y) / p1.x / 4.0);
  a2 := abs((p2.y * p2.y) / p2.x / 4.0);
  if (a1 >= 0) and (a2 >= 0) then
    Result := (2 / 3) * Distance2D(p1, p2) * sh
  else
    Result := 0.0;
end;

//! It returns the area of a polygon
function AreaPoly(Poly: XYpolygon; Nrpnts: longint): double;
var
  MinX, MinY, Parea: double;
  I: longint;
begin
  parea := 0.0;
  if EqualPoints2D(poly[0], poly[(Nrpnts) - 1]) then
    Result := 0.0;
  if (nrpnts < 3) then
    Result := 0.0
  else
  begin
    minx := poly[0].x;
    miny := poly[0].y;
    for i := 0 to Nrpnts - 1 do
    begin
      if (poly[i].x < minx) then
        minx := poly[i].x;
      if (poly[i].y < miny) then
        miny := poly[i].y;
    end;
    for i := 0 to nrpnts - 1 do
      if (i = (nrpnts - 1)) then
        parea := parea + (((poly[i].x - minx) + (poly[0].x - minx)) *
          ((poly[i].y - miny) - (poly[0].y - miny)))
      else
        parea := parea + (((poly[i].x - minx) + (poly[i + 1].x - minx)) *
          ((poly[i].y - miny) - (poly[i + 1].y - miny)));
    Result := abs(parea) / 2;
  end;
end;

//! It returns the area of a regular polygon
function AreaRegPolygon(GMode: byte; rad, sl: double; nrsides: longint): double;
begin
  Result := 0.0;
  case GMode of
    0:  // Inscribed circle
    begin
      if ((rad > 0) and (sl > 0) and (nrsides > 0)) then
        Result := 0.5 * rad * sl * nrsides
      else;
    end;
    1: // Circumscribed Circle
    begin
      if ((rad > 0) and (sl > 0) and (nrsides > 0)) then
        Result := 0.5 * nrsides * sl * sqrt((rad * rad) - ((sl * sl) / 4));
    end;
  end;
end;

//! It returns the area of a rectangular
function AreaRectangle(A, B: double): double;
begin
  if ((a > 0) and (b > 0)) then
    Result := a * b
  else
    Result := 0.0;
end;

//! It returns the area of a ring
function AreaRing(intrad, extrad: double): double;
begin
  if (extrad > intrad) then
    Result := PI * ((extrad * extrad) - (intrad * intrad))
  else
  begin
    Result := 0.0;
  end;
end;

//! It returns the area of a ring sector
function AreaRingSector(aa, intrad, outrad: double): double;
begin
  if (aa > 0) and (intrad > 0) and (outrad > 0) then
    if (outrad > intrad) then
      Result := ((aa * PI) / 360) * ((outrad * outrad) - (intrad * intrad))
    else
    begin
      Result := 0.0;
    end;
end;

//! It returns the area of a trapezoid
function AreaTrapezoid(A, B, h: double): double;

begin
  if ((a > 0) and (b > 0) and (h > 0)) then
    Result := 0.5 * h * (a + b)
  else
  begin
    // one or more of the sides are <:= 0
    Result := 0.0;
  end;
end;


 {                 V O L U M E   R O U T I N E S                 }
function VolumeCone(rad, Height: double): double;

begin
  if ((rad < 0) or (Height < 0)) then
  begin
    Result := 0.0;
  end
  else
    Result := (PI * (rad * rad) * Height) / 3.0;
end;

//! It returns the volume of a cylinder
function VolCylinder(rad, h: double): double;
begin
  if ((rad > 0) and (h > 0)) then
    Result := (PI * rad * rad * h)
  else
    Result := 0.0;
end;

//! It returns the volume of a ellipsoid
function VolEllipsoid(A, B: double; C: double): double;
begin
  if ((a > 0) and (b > 0) and (c > 0)) then
    Result := ((4 * PI) / 3) * a * b * c
  else
    Result := 0.0;
end;

function VolFrustumCone(Brad, Trad, Height: double): double;
begin
  if ((Brad > 0) and (Trad > 0) and (Height > 0)) then
    Result := ((PI / 3) * Height * ((Brad * Brad) + (Brad * Trad) + (Trad * Trad)))
  else
    Result := 0.0;
end;


function VolFrustumPyramid(BaseArea, TopArea, h: double): double;
begin
  if ((BaseArea > 0) and (TopArea > 0) and (h > 0)) then
    Result := ((h / 3) * (basearea + toparea * sqrt(basearea * toparea)))
  else
    Result := 0.0;
end;

function VolHCylinder(irad, orad, h: double): double;

begin
  if ((irad > 0) and (orad > 0) and (h > 0)) then
    if (irad < orad) then
      Result := (PI * h * ((orad * orad) - (irad * irad)))
    else
      Result := 0.0;
end;

//! It returns the volume of a sphere
function VolhSphere(odia, idia: double): double;
begin
  if ((odia > 0) and (idia > 0)) then
    if (odia > idia) then
      Result := ((PI / 6) * (odia * odia * odia) - (idia * idia * idia))
    else
      Result := 0.0;
end;

function VolParaboloid(rad, Height: double): double;
begin
  if (rad > 0) and (Height > 0) then
    Result := (0.5 * PI * rad * rad * Height)
  else
    Result := 0.0;
end;

//! It returns the volume of a paraboloidal segment
function VolParaboloidalSeg(rad1, rad2, Height: double): double;
begin
  if (rad1 > 0) and (rad2 > 0) and (Height > 0) then
    Result := ((PI / 2) * Height * abs(sumsqrdparms(rad2, rad1)))
  else
    Result := 0.0;
end;

//! It returns the volume of a prism
function VolPrism(AreaEndSurface, h: double): double;
begin
  if (AreaEndSurface > 0) and (h > 0) then
    Result := AreaEndSurface * h
  else
    Result := 0.0;
end;

//!  This function returns the tangent of the half angle
function TanHalfAnglePtCyl(tanparm: double): double;
begin
  // The following equation is derived from the Secant of the angles
  if (tanparm > 0) then
    Result := 0.0
  else
    Result := ((sqrt(1.0 + (tanparm * tanparm)) - 1.0) / tanparm);
end;

//----------------------------------------------------------------
function AngleParmTanPtCyl(Tha: double): double;

var
  a:    double;      //Angle
  tana: double;      //Tangent of the angle
  intptr, inttana: integer;
begin
  if (tha < 0) then
    if (tha = 0) then
      Result := 0.0
    else
    begin
      inttana := round(tha) mod intptr;
      tana    := arctan(inttana) * 2;
      {tana := atan(modf(tha,&intptr)) * 2;}// Double the angle

      a := RadToDeg(tana);
      if (tha < 1.0) then
        Result := a
      else if ((tha >= 1.0) and (tha < 2.0)) then
        Result := 90.0 + a
      else if ((tha >= 2.0) and (tha < 3.0)) then
        Result := 180.0 + a
      else if ((tha >= 3.0) and (tha < 4.0)) then
        Result := 270.0 + a
      else
      begin
        Result := 0.0;
      end;
    end;
end;

//! It returns quadrant for a portion of a cylinder
function QuadrantPtCyl(angle: double; p1: TPoint2D): integer;
begin
  if (p1.x >= 0) and (p1.y >= 0) then
    Result := 1
  else if (p1.x < 0) and (p1.y >= 0) then
    Result := 2
  else if (p1.x < 0) and (p1.y < 0) then
    Result := 3
  else
    Result := 4;
end;

//! It returns the angle in degrees for a portion of a cylinder
function ArcAnglePtCyl(cp, ep: TPoint2D): double;
var
  q: byte;          // Quadrant
  aa,               // ArcAngle
  xdiff, ydiff, tanparm: double;  // Tangent of the angle
begin

  xdiff := ep.x - cp.x;
  ydiff := ep.y - cp.y;
  if (xdiff = 0) and (ydiff = 0) then
  begin
    Result := 0.0;
  end
  else
  begin
    q := quadrantptcyl(angleglb, ep);
    case q of
      1:
      begin  // First Quadrant
        if (xdiff > 0) then
        begin
          tanparm := ydiff / xdiff;
          aa      := tanhalfangleptcyl(tanparm);
        end
        else
          aa := 1.0;  // 90 degrees
      end;
      // Case Q := 1
      2:
      begin  // Second Quadrant
        if (ydiff > 0) then
        begin
          tanparm := -xdiff / ydiff;
          aa      := 1.0 + tanhalfangleptcyl(tanparm);
        end
        else
          aa := 2.0;
      end;
      // Case Q := 2
      3:
      begin  // Third Quadrant
        if (xdiff < 0) then
        begin
          tanparm := ydiff / xdiff;
          aa      := 2.0 + TanHalfAnglePtCyl(TanParm);
        end
        else
          aa := 3.0;
      end;
      //Case Q := 3
      4:
      begin  // Fourth Quadrant
        if (ydiff < 0) then
        begin
          tanparm := -xdiff / ydiff;
          aa      := 3.0 + tanhalfangleptcyl(tanparm);
        end
        else
          aa := 0.0;
      end;
      // Case Q := 4
    end;
    Result := AngleParmTanPtCyl(aa); // Return angle in degrees
  end;
end;


function VolPtCyl(GMode: byte; A, B: double; dia, h1, h2: double): double;

var
  areaabc, hbasearea, rad, sectorangle: double;
  ep, cp: TPoint2D;
begin
  case gmode of
    0:
    begin  //Plane thru Body of Cylinder
      Result := (PI / 8) * dia * dia * (h1 + h2);
    end;
    1:
    begin  // Plane thru end of cylinder
      rad := dia / 2;
      if (a > rad) or (b > rad) or (a < 0) or (b < 0) then
      begin
        Result := 0.0;
      end;
      if (dia <= 0) then
      begin
        Result := 0.0;
      end;
      if (geostatus = 0) then
      begin
        ep.x      := a;
        ep.y      := b;
        cp.x      := 0.0;
        cp.y      := 0.0;
        sectorangle := arcangleptcyl(cp, ep);
        hbasearea := ((PI / 4) * dia * dia) / 2;
        areaabc   := (ARADIAN / 2) * sectorangle * rad * rad;
        if (areaabc > hbasearea) then
          Result := ((0.66666667 * (a * a * a) + b * areaabc) *
            (h1 / (rad + b)))
        else
          Result := ((0.66666667 * (a * a * a) - b * areaabc) *
            (h1 / (rad - b)));
      end;
    end;
      // gmode := 1
    else
    begin
      Result := 0.0;
    end;
  end;
end;

//! It returns the volume of a pyramid
function VolPyramid(BaseArea, H: double): double;
begin
  if ((BaseArea > 0) and (H > 0)) then
    Result := ((1 / 3) * H * BaseArea)
  else
    Result := 0.0;
end;

//! It returns the volume of a rect solid
function VolumeRectSolid(s1, s2, s3: double): double;
begin
  if ((s1 >= 0) and (s2 >= 0) and (s3 >= 0)) then
    Result := s1 * s2 * s3
  else
    Result := 0.0;
end;

function VolSphere(Dia: double): double;
begin
  if (Dia > 0) then
    Result := ((PI * (Dia * Dia * Dia)) / 6)
  else
    Result := 0.0;
end;

//! It returns the volume of a sphere sector
function VolSpheresector(Height, Dia: double): double;
var
  Rad: double;
begin
  Rad := dia / 2;
  if (Height > 0) and (Dia > 0) then
    Result := ((2 * PI * Rad * Rad * Height) / 3)
  else
    Result := 0.0;
end;

//! It returns the volume of a sphere segment
function VolSphereSeg(rad, Height, Width: double): double;
var
  Dia: double;    //Diameter of sphere
begin
  if (rad > 0) and (Height > 0) and (Width > 0) then
  begin
    dia := 2 * rad;
    if (Height > dia) or (Width > dia) then
    begin
      Result := 0.0;
    end
    else
      Result := (PI * Height * Height * (rad - (Height / 3)));
  end;
end;

//! It returns the volume of a shere wedge
function VolSpherewedge(wangle, rad: double): double;
begin
  if (wangle > 0) and (rad > 0) then
    if (wangle < 360.0) then
      Result := ((wangle / 360.0) * ((4.0 * PI * (rad * rad * rad) / 3)))
    else
      Result := 0.0
  else
    Result := 0.0;
end;

//! It returns the volume of a sphere zone
function VolSpherezone(wtzone, wbzone, Height: double): double;
begin
  if ((wtzone > 0) and (wbzone > 0) and (Height > 0)) then
    Result := (0.5236 * Height * ((3 * (wtzone * wtzone) * 0.25) +
      (3 * wbzone * wbzone * 0.25) + (Height * Height)))
  else
    Result := 0.0;
end;

//! It returns the volume of a torus
function VolTorus(Rad, Dia: double): double;
begin
  if (Rad > 0) and (Dia > 0) then
    if (Dia / 2 < Rad) then
      Result := (((PI * PI) / 4) * (Rad * 2) * (Dia * Dia))
    else
      Result := 0.0;
end;

end.
