// -------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
// -------------------------------------------------------------------------
{ ! Inverse distance weighted interpolation - IDW }

unit uInverseDistance;

interface

uses
  System.SysUtils,
  System.Math,

  uGlobals,
  uCommon,
  uInterpol,
  uSuperblock;

{ Inverse Distance Weighted Interpolation with Superblock teñhnique}
function InverseDistanceInterpolation(var Points: TCoordinateArray;
  var Nodes: TCoordinateArray; Params: TInvDistPars): Boolean;

//=========================================================================
implementation
//=========================================================================

function InverseDistanceInterpolation(var Points: TCoordinateArray;
  var Nodes: TCoordinateArray; Params: TInvDistPars): Boolean;

var
  I, K, Nearest, Found, Pattern_len: Integer;
  Sum_Value, Sum_Dist, Inv_Dist, G: Double;
  Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Dist: Double;
  Super_Block: TCoordListArray; // superblock structure definition
  Pattern: TGridPatternArray;
  Dist_Match: TCoordMatchArray;

begin
  Nearest := Params.NP;

  GetMinMaxXYZ(Points, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax);

  Initialize_super_block(Super_block, Pattern, Pattern_len);
  Load_Super_Block(Super_Block, Points, High(Points) + 1,
    Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Dist);
  // the main loop for all nodes of the grid
  for I := Low(Nodes) to High(Nodes) do
  begin
    Dist_match := Get_sb_nearest_neighbors(Nodes[I].X, Nodes[I].Y, Nodes[I].Z,
      Pattern, Pattern_len, Super_block, Nearest, Found, Dist, Xmin, Ymin, Zmin);
    Sum_value := 0;
    Sum_dist := 0;
    for K := 0 to Found - 1 do
      // if distance to the closest point <0.001, use the attribute value of this point
      if Dist_match[K].Distance < 0.001 then
      begin
        Sum_value := Dist_match[K].Value;
        Sum_dist := 1;
        Break;
      end
      else
      begin
        case Params.Power of
          1: Inv_dist := 1 / Dist_match[K].Distance;
          2: Inv_dist := 1 / (Dist_match[K].Distance * Dist_match[K].Distance);
        else
          Inv_dist := 1 / Power(Dist_match[K].Distance, Params.Power);
        end;
        G := Dist_match[K].Value;
        Sum_dist := Sum_dist + Inv_dist;
        Sum_value := Sum_value + G * Inv_dist;
      end;
    SetLength(Dist_match, 0);
    Nodes[I].Value := Sum_Value / Sum_Dist;

  end;
  Release_Super_Block(Super_Block, Pattern, Pattern_Len);

  Result := True;
end;

end.
