//-------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-------------------------------------------------------------------------
{! Closest Point interpolation - CPI }

unit uClosestPointInt;

interface

uses
  System.SysUtils,
  System.Math,
  Vcl.ComCtrls, // there is Vcl.Dialogs.TTaskDialog.ProgressBar

  uGlobals,
  uCommon,
  uProfuns,
  uInterpol,
  uSuperblock;

/// Returns IDs of closest points for all nodes
function ClosestPointGridding(var Points: TCoordinateArray; var Nodes: TCoordinateArray;
  ProgressBar: TProgressBar): boolean;

//=========================================================================
implementation
//=========================================================================

function ClosestPointGridding(var Points: TCoordinateArray; var Nodes: TCoordinateArray;
  ProgressBar: TProgressBar): boolean;
var
  I, Nearest, Found, Pattern_len: integer;
  Xmin, Xmax, Ymin, Ymax, Zmin, Zmax, Dist: double;
  Super_Block: TCoordListArray;     // superblock structure definition
  Pattern:     TGridPatternArray;
  Dist_match:  TCoordMatchArray;

begin
  Nearest := 1;
  with ProgressBar do
  begin
    Min      := Low(Nodes);
	  Max      := High(Nodes) + 1;
	  Position := Min;
	  Step     := 1;
  end;
  // initialize superblock structure
  GetMinMaxXYZ(Points, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax);
  Initialize_Super_Block(Super_block, Pattern, Pattern_len);
  Load_Super_Block(Super_block, Points, High(Points) + 1, Xmin, Xmax,
    Ymin, Ymax, Zmin, Zmax, Dist);

  // the main loop for all grid nodes
  for i := Low(Nodes) to High(Nodes) do
  begin
    Dist_match     := Get_sb_Nearest_Neighbors(Nodes[i].X, Nodes[i].Y,
      Nodes[i].z, Pattern, Pattern_len, Super_Block, Nearest, Found,
      Dist, Xmin, Ymin, Zmin);
    Nodes[i].ID   := Dist_match[0].ID;
    Nodes[i].Value := Dist_match[0].Value;
    SetLength(Dist_match, 0);
    ProgressBar.StepIt;
  end;
  Release_Super_Block(Super_Block, Pattern, Pattern_Len);

  Result := True;
end;

end.
