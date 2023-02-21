//-----------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
(* Basic geometry calculator for 2d/3d bodies *)


unit fToolsGeometryCalculator;

interface

uses
  System.SysUtils, 
  System.Variants, 
  System.Classes, 
  System.Math,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.Dialogs, 
  Vcl.StdCtrls,
  Vcl.ComCtrls, 
  Vcl.ExtCtrls, 
  Vcl.Grids, 
  Vcl.Buttons,

  
  fPageDialog,
  GBEditValue,
  GBEditRange;

type
  TfmToolsGeometryCalculator = class(TfmPageDialog)
    TabSheetCone: TTabSheet;
    TabSheetCylinder: TTabSheet;
    TabSheetEllipse: TTabSheet;
    TabSheetPolygon: TTabSheet;
    TabSheetRectangle: TTabSheet;
    TabSheetToroid: TTabSheet;
    GroupBoxCone: TGroupBox;
    ImageCone:   TImage;
    LabelConeRadius: TLabel;
    LabelConeHeight: TLabel;
    LabelConeSurface: TLabel;
    LabelConeVolume: TLabel;
    LabelConeLength: TLabel;
    EditScaleDoubleConeRadius: TGBEditScaleDouble;
    EditScaleDoubleConeHeight: TGBEditScaleDouble;
    EditScaleDoubleConeSurface: TGBEditScaleDouble;
    EditScaleDoubleConeVolume: TGBEditScaleDouble;
    EditScaleDoubleConeLength: TGBEditScaleDouble;
    GroupBoxCylinder: TGroupBox;
    ImageCylinder: TImage;
    LabelCylinderRadius: TLabel;
    LabelCylinderLength: TLabel;
    LabelCylinderSurface: TLabel;
    LabelCylinderVolume: TLabel;
    EditScaleDoubleCylinderRadius: TGBEditScaleDouble;
    EditScaleDoubleCylinderLength: TGBEditScaleDouble;
    EditScaleDoubleCylinderSurface: TGBEditScaleDouble;
    EditScaleDoubleCylinderVolume: TGBEditScaleDouble;
    GroupBoxEllipse: TGroupBox;
    ImageEllipse: TImage;
    LabelEllipsePerimeter: TLabel;
    LabelEllipseArea: TLabel;
    EditScaleDoubleEllipseMinRadius: TGBEditScaleDouble;
    EditScaleDoubleEllipseMaxRadius: TGBEditScaleDouble;
    EditScaleDoubleEllipsePerimeter: TGBEditScaleDouble;
    EditScaleDoubleEllipseArea: TGBEditScaleDouble;
    GroupBoxPolygon: TGroupBox;
    ImagePolygon: TImage;
    LabelSideLength: TLabel;
    EditScaleDoublePolySideLength: TGBEditScaleDouble;
    StringGridPolygon: TStringGrid;
    GroupBoxRect: TGroupBox;
    Image2:      TImage;
    LabelRectWidth: TLabel;
    LabelRectHeight: TLabel;
    LabelRectDiagonal: TLabel;
    LabelRectPerimeter: TLabel;
    LabelRectArea: TLabel;
    EditScaleDoubleRectWidth: TGBEditScaleDouble;
    EditScaleDoubleRectHeight: TGBEditScaleDouble;
    EditScaleDoubleRectDiagonal: TGBEditScaleDouble;
    EditScaleDoubleRectPerimeter: TGBEditScaleDouble;
    EditScaleDoubleRectArea: TGBEditScaleDouble;
    GroupBoxToroid: TGroupBox;
    ImageToroid: TImage;
    LabelToroidRingRadius: TLabel;
    LabelToroidCrossSectionRadius: TLabel;
    LabelToroidSurface: TLabel;
    LabelToroidVolume: TLabel;
    EditScaleDoubleToroidRingRadius: TGBEditScaleDouble;
    EditScaleDoubleToroidCrossSectionRadius: TGBEditScaleDouble;
    EditScaleDoubleToroidSurface: TGBEditScaleDouble;
    EditScaleDoubleToroidVolume: TGBEditScaleDouble;
    TabSheetCircle: TTabSheet;
    TabSheetParallelepiped: TTabSheet;
    TabSheetSphere: TTabSheet;
    TabSheetSegment: TTabSheet;
    TabSheetPrism: TTabSheet;
    TabSheetPyramid: TTabSheet;
    GroupBoxCircle: TGroupBox;
    LabelCircleArea: TLabel;
    LabelCirclePerimeter: TLabel;
    LabelCircleDiameter: TLabel;
    LabelCircleRadius: TLabel;
    ImageCircle: TImage;
    EditScaleDoubleCircleRadius: TGBEditScaleDouble;
    EditScaleDoubleCirclePerimeter: TGBEditScaleDouble;
    EditScaleDoubleCircleDiameter: TGBEditScaleDouble;
    EditScaleDoubleCircleArea: TGBEditScaleDouble;
    GroupBoxParallelepiped: TGroupBox;
    ImageCube:   TImage;
    LabelCubeWidth: TLabel;
    LabelCubeLength: TLabel;
    LabelCubeHeight: TLabel;
    LabelCubeSurface: TLabel;
    LabelCubeVolume: TLabel;
    EditScaleDoubleWidth: TGBEditScaleDouble;
    EditScaleDoubleDepth: TGBEditScaleDouble;
    EditScaleDoubleHeight: TGBEditScaleDouble;
    EditScaleDoubleSurface: TGBEditScaleDouble;
    EditScaleDoubleVolume: TGBEditScaleDouble;
    GroupBoxSphere: TGroupBox;
    Image1:      TImage;
    LabelSphereRadius: TLabel;
    LabelSphereSurface: TLabel;
    LabelSphereVolume: TLabel;
    EditScaleDoubleSphereRadius: TGBEditScaleDouble;
    EditScaleDoubleSphereSurface: TGBEditScaleDouble;
    EditScaleDoubleSphereVolume: TGBEditScaleDouble;
    GroupBoxSegment: TGroupBox;
    ImageSegment: TImage;
    LabelSegmentRadius: TLabel;
    LabelSegmentHeight: TLabel;
    LabelSegmentVolume: TLabel;
    GroupBoxPrizm: TGroupBox;
    ImagePrizm:  TImage;
    LabelPrizmLength: TLabel;
    LabelPrizmWidth: TLabel;
    LabelPrismVolume: TLabel;
    LabelPrizmHeight: TLabel;
    GroupBoxPyramid: TGroupBox;
    ImagePyramid: TImage;
    LabelPyramidLength: TLabel;
    LabelPyramidWidth: TLabel;
    LabelPyramidVolume: TLabel;
    LabelPyramidHeight: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText5: TStaticText;
    SpeedButtonCopy: TSpeedButton;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    StaticText15: TStaticText;
    StaticText16: TStaticText;
    StaticText17: TStaticText;
    StaticText18: TStaticText;
    StaticText19: TStaticText;
    StaticText20: TStaticText;
    StaticText21: TStaticText;
    StaticText22: TStaticText;
    StaticText4: TStaticText;
    StaticText23: TStaticText;
    StaticText24: TStaticText;
    StaticText25: TStaticText;
    StaticText26: TStaticText;
    StaticText27: TStaticText;
    StaticText28: TStaticText;
    StaticText29: TStaticText;
    GBEditValueSegmentRadius: TGBEditValue;
    GBEditValueSegmentHeight: TGBEditValue;
    GBEditValueSegmentVolume: TGBEditValue;
    GBEditValuePrizmLength: TGBEditValue;
    GBEditValuePrizmWidth: TGBEditValue;
    GBEditValuePrizmHeight: TGBEditValue;
    GBEditValuePrizmVolume: TGBEditValue;
    GBEditValuePyramidLength: TGBEditValue;
    GBEditValuePyramidWidth: TGBEditValue;
    GBEditValuePyramidHeight: TGBEditValue;
    GBEditValuePyramidVolume: TGBEditValue;
    procedure EditScaleSphereChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditScaleToroidChange(Sender: TObject);
    procedure EditScaleConeChange(Sender: TObject);
    procedure EditScaleCylinderChange(Sender: TObject);
    procedure EditScaleEllipseChange(Sender: TObject);
    procedure EditScalePolygonChange(Sender: TObject);
    procedure EditScaleRectangleChange(Sender: TObject);
    procedure EditScaleParallelepipedChange(Sender: TObject);
    procedure EditValueSegmentChange(Sender: TObject);
    procedure EditValuePrismChange(Sender: TObject);
    procedure EditValuePyramidChange(Sender: TObject);
    procedure EditScaleDoubleCircleRadiusChange(Sender: TObject);
    procedure EditScaleDoubleCircleDiameterChange(Sender: TObject);
    procedure SpeedButtonCopyClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    procedure SetStringGridHeadings;
  public
     
  end;

var
  fmToolsGeometryCalculator: TfmToolsGeometryCalculator;

//========================================================================
implementation
//========================================================================

uses
  cProfuns,
  cGlobals,
  cResStrings;

{$R *.dfm}

procedure TfmToolsGeometryCalculator.SetStringGridHeadings;
begin
  StringGridPolygon.Cells[0, 0] := LoadResString(@rsPolygon);
  StringGridPolygon.Cells[1, 0] := LoadResString(@rsSide);
  StringGridPolygon.Cells[2, 0] := LoadResString(@rsPerimeter);
  StringGridPolygon.Cells[3, 0] := LoadResString(@rsArea);
  StringGridPolygon.Cells[4, 0] := 'Rmax';
  StringGridPolygon.Cells[5, 0] := 'Rmin';
  StringGridPolygon.Cells[6, 0] := LoadResString(@rsSector);

  StringGridPolygon.Cells[0, 1]  := LoadResString(@rsTriangle);
  StringGridPolygon.Cells[0, 2]  := LoadResString(@rsSquare);
  StringGridPolygon.Cells[0, 3]  := LoadResString(@rsPentagon);
  StringGridPolygon.Cells[0, 4]  := LoadResString(@rsHexagon);
  StringGridPolygon.Cells[0, 5]  := LoadResString(@rsHeptagon);
  StringGridPolygon.Cells[0, 6]  := LoadResString(@rsOctagon);
  StringGridPolygon.Cells[0, 7]  := LoadResString(@rsNonagon);
  StringGridPolygon.Cells[0, 8]  := LoadResString(@rsDecagon);
  StringGridPolygon.Cells[0, 9]  := LoadResString(@rsUndecagon);
  StringGridPolygon.Cells[0, 10] := LoadResString(@rsDodecagon);
end;


procedure TfmToolsGeometryCalculator.FormCreate(Sender: TObject);
begin
  inherited;
  SetStringGridHeadings;
  PageControlChange(Sender);
end;


procedure TfmToolsGeometryCalculator.PageControlChange(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: EditScaleSphereChange(Sender);
    1: EditScaleToroidChange(Sender);
    2: EditScaleConeChange(Sender);
    3: EditScaleCylinderChange(Sender);
    4: EditScaleEllipseChange(Sender);
    5: EditScalePolygonChange(Sender);
    6: EditScaleRectangleChange(Sender);
    7: EditScaleDoubleCircleRadiusChange(Sender);
    8: EditScaleParallelepipedChange(Sender);
    9: EditValueSegmentChange(Sender);
    10: EditValuePrismChange(Sender);
    11: EditValuePyramidChange(Sender);
  end;
end;


//--------------------------Sphere----------------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleSphereChange(Sender: TObject);
begin
  EditScaleDoubleSphereSurface.Value :=
    RoundTo(4 * Pi * (EditScaleDoubleSphereRadius.Value) *
    (EditScaleDoubleSphereRadius.Value), Precision);
  EditScaleDoubleSphereVolume.Value  :=
    RoundTo((4 * Pi * Power(EditScaleDoubleSphereRadius.Value, 3)) /
    3, Precision);
end;

//----------------------------Toroid----------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleToroidChange(Sender: TObject);
begin
  EditScaleDoubleToroidSurface.Value :=
    RoundTo(2 * Sqr(Pi) * EditScaleDoubleToroidRingRadius.Value *
    Sqr(EditScaleDoubleToroidCrossSectionRadius.Value), Precision);
  EditScaleDoubleToroidVolume.Value  :=
    RoundTo(2 * Sqr(Pi) * Sqr(EditScaleDoubleToroidCrossSectionRadius.Value),
    Precision);
end;

//------------------------Cone----------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleConeChange(Sender: TObject);
begin
  EditScaleDoubleConeLength.Value  :=
    RoundTo(Sqrt(Sqr(EditScaleDoubleConeHeight.Value) +
    (Sqr(EditScaleDoubleConeRadius.Value))), Precision);
  EditScaleDoubleConeSurface.Value :=
    RoundTo(Pi * EditScaleDoubleConeRadius.Value *
    (EditScaleDoubleConeRadius.Value + EditScaleDoubleConeLength.Value), Precision);
  EditScaleDoubleConeVolume.Value  :=
    RoundTo((Pi * Sqr(EditScaleDoubleConeRadius.Value) *
    EditScaleDoubleConeHeight.Value) / 3, Precision);
end;

//------------------------Cylinder----------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleCylinderChange(Sender: TObject);
begin
  EditScaleDoubleCylinderSurface.Value :=
    RoundTo(2 * Pi * EditScaleDoubleCylinderRadius.Value *
    (EditScaleDoubleCylinderRadius.Value * EditScaleDoubleCylinderLength.Value),
    Precision);
  EditScaleDoubleCylinderVolume.Value  :=
    RoundTo(Pi * Sqr(EditScaleDoubleCylinderRadius.Value) *
    EditScaleDoubleCylinderLength.Value, Precision);
end;

//---------------------------Ellipse----------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleEllipseChange(Sender: TObject);
begin
  EditScaleDoubleEllipsePerimeter.Value :=
    RoundTo(Pi * (1.5 * (EditScaleDoubleEllipseMinRadius.Value +
    EditScaleDoubleEllipseMaxRadius.Value) -
    Sqrt(EditScaleDoubleEllipseMinRadius.Value *
    EditScaleDoubleEllipseMaxRadius.Value)), Precision);
  EditScaleDoubleEllipseArea.Value      :=
    RoundTo(Pi * (EditScaleDoubleEllipseMinRadius.Value *
    EditScaleDoubleEllipseMaxRadius.Value), Precision);
end;

//----------------------------Polygon-------------------------------\\
procedure TfmToolsGeometryCalculator.EditScalePolygonChange(Sender: TObject);
var
  I: integer;
  Area, Rmin, Rmax, Perimeter, Sector: double;
  S: string;
begin
  for I := 3 to 12 do
  begin
    Str(I, S);
    StringGridPolygon.Cells[1, I - 2] := S;
    Perimeter := I * EditScaleDoublePolySideLength.Value;
    Str(Perimeter: 6: Precision, S);
    StringGridPolygon.Cells[2, I - 2] := S;

    Rmin := (EditScaleDoublePolySideLength.Value / 2) / Tan(DegToRad(180 / I));
    Str(Rmin: 6: Precision, S);
    StringGridPolygon.Cells[5, I - 2] := S;

    Rmax := (EditScaleDoublePolySideLength.Value / 2) / Sin(DegToRad(180 / I));
    Str(Rmax: 6: Precision, S);
    StringGridPolygon.Cells[4, I - 2] := S;

    Sector := 360 / I;
    Str(Sector: 6: Precision, S);
    StringGridPolygon.Cells[6, I - 2] := S;

    Area := (Perimeter * Rmin) / 2;
    Str(Area: 6: Precision, S);
    StringGridPolygon.Cells[3, I - 2] := S;
  end;
end;

//---------------------------Rectangle------------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleRectangleChange(Sender: TObject);
begin
  EditScaleDoubleRectDiagonal.Value  :=
    RoundTo(Sqrt(Sqr(EditScaleDoubleRectWidth.Value) +
    (Sqr(EditScaleDoubleRectHeight.Value))), Precision);
  EditScaleDoubleRectPerimeter.Value :=
    RoundTo(2 * (EditScaleDoubleRectWidth.Value) + 2 *
    (EditScaleDoubleRectHeight.Value), Precision);
  EditScaleDoubleRectArea.Value      :=
    RoundTo(EditScaleDoubleRectWidth.Value * EditScaleDoubleRectHeight.Value,
    Precision);
end;

//-------------------------Parallelepiped-----------------------\\
procedure TfmToolsGeometryCalculator.EditScaleParallelepipedChange(Sender: TObject);
begin
  EditScaleDoubleSurface.Value :=
    2 * (EditScaleDoubleWidth.Value * EditScaleDoubleDepth.Value +
    EditScaleDoubleDepth.Value * EditScaleDoubleHeight.Value +
    EditScaleDoubleHeight.Value * EditScaleDoubleWidth.Value);
  EditScaleDoubleVolume.Value  :=
    EditScaleDoubleWidth.Value * EditScaleDoubleDepth.Value *
    EditScaleDoubleHeight.Value;
end;

//-----------------------SphereSegment-------------------------\\
procedure TfmToolsGeometryCalculator.EditValueSegmentChange(Sender: TObject);
begin
  GBEditValueSegmentVolume.asDouble :=
    RoundTo(Pi * GBEditValueSegmentHeight.asDouble *
    (Sqr(GBEditValueSegmentRadius.asDouble) + Sqr(GBEditValueSegmentHeight.asDouble)) /
    6, Precision);
end;

//-------------------------- Prism--------------------------------\\
procedure TfmToolsGeometryCalculator.EditValuePrismChange(Sender: TObject);
begin
  GBEditValuePrizmVolume.asDouble :=
    RoundTo(GBEditValuePrizmLength.asDouble * GBEditValuePrizmWidth.asDouble *
    GBEditValuePrizmHeight.asDouble / 2, Precision);
end;

//--------------------------- Pyramid---------------------------------\\
procedure TfmToolsGeometryCalculator.EditValuePyramidChange(Sender: TObject);
begin
  GBEditValuePyramidVolume.asDouble :=
    RoundTo(GBEditValuePyramidLength.asDouble * GBEditValuePyramidWidth.asDouble *
    GBEditValuePyramidHeight.asDouble / 3, Precision);
end;

//--------------------------Circle---------------------------------\\
procedure TfmToolsGeometryCalculator.EditScaleDoubleCircleRadiusChange(
  Sender: TObject);
begin
  EditScaleDoubleCircleDiameter.Value  :=
    RoundTo(2 * EditScaleDoubleCircleRadius.Value, Precision);
  EditScaleDoubleCirclePerimeter.Value :=
    RoundTo(2 * Pi * EditScaleDoubleCircleRadius.Value, Precision);
  EditScaleDoubleCircleArea.Value      :=
    RoundTo(Pi * Sqr(EditScaleDoubleCircleRadius.Value), Precision);
end;

procedure TfmToolsGeometryCalculator.EditScaleDoubleCircleDiameterChange(
  Sender: TObject);
begin
  EditScaleDoubleCircleRadius.Value    :=
    RoundTo(EditScaleDoubleCircleDiameter.Value / 2, Precision);
  EditScaleDoubleCirclePerimeter.Value :=
    RoundTo(2 * Pi * EditScaleDoubleCircleRadius.Value, Precision);
  EditScaleDoubleCircleArea.Value      :=
    RoundTo(Pi * Sqr(EditScaleDoubleCircleRadius.Value), Precision);
end;

procedure TfmToolsGeometryCalculator.SpeedButtonCopyClick(Sender: TObject);
begin
  //Add the primitive to the MapWindow as the solid dataset
  //...
end;


end.
