//--------------------------------------------------------------------------
// The modeling system Geoblock http://sourceforge.net/projects/geoblock
//--------------------------------------------------------------------------
(* The basic geocalculator *)

unit fToolsGeologyCalculator;

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
  Vcl.Samples.Spin,
  
  fPageDialog,
  GBEditValue, 
  GBEditRange;

type
  TfmToolsGeologyCalculator = class(TfmPageDialog)
    TabSheetDThick: TTabSheet;
    TabSheetObjectHeight: TTabSheet;
    TabSheet2Points: TTabSheet;
    TabSheet3Points: TTabSheet;
    GroupBoxHeight: TGroupBox;
    ImageHeight:    TImage;
    LabelAngle:     TLabel;
    LabelInstrumentHeight: TLabel;
    LabelDistance:  TLabel;
    LabelObjectHeight: TLabel;
    EditScaleDoubleAngle: TGBEditScaleDouble;
    EditScaleDoubleDistance: TGBEditScaleDouble;
    EditScaleDoubleInstrHeight: TGBEditScaleDouble;
    LabelObjectHeightTitle: TLabel;
    GroupBoxDThick: TGroupBox;
    ImageDThick:    TImage;
    LabelDThickTitle: TLabel;
    LabelVertThickness: TLabel;
    StaticTextDv:   TStaticText;
    EditScaleDoubleVertThickness: TGBEditScaleDouble;
    LabelTrueThickness: TLabel;
    StaticTextD:    TStaticText;
    EditScaleDoubleTrueThickness: TGBEditScaleDouble;
    LabelHorizThickness: TLabel;
    StaticTextDh:   TStaticText;
    EditScaleDoubleHorizThickness: TGBEditScaleDouble;
    LabelSeamAngle: TLabel;
    StaticTextA:    TStaticText;
    EditScaleDoubleSeamAngle: TGBEditScaleDouble;
    GroupBox2Points: TGroupBox;
    Label2PointsTitle: TLabel;
    Label2pNo:      TLabel;
    Label2pAngles:  TLabel;
    Label2pBearing: TLabel;
    Label2pInclination: TLabel;
    Label2pDistance: TLabel;
    GroupBox3Points: TGroupBox;
    Label3PointsTitle: TLabel;
    Label3pPoint:   TLabel;
    LabelStrike:    TLabel;
    LabelDipAngle:  TLabel;
    LabelDipDirection: TLabel;
    LabelArea:      TLabel;
    Label3PointsAngles: TLabel;
    StaticText1:    TStaticText;
    StaticText2:    TStaticText;
    StaticText3:    TStaticText;
    StaticText4:    TStaticText;
    StaticText5:    TStaticText;
    StaticText6:    TStaticText;
    StaticText7:    TStaticText;
    StaticText8:    TStaticText;
    StaticText9:    TStaticText;
    StaticText10:   TStaticText;
    StaticText11:   TStaticText;
    EditScaleObjectHeight: TGBEditScaleDouble;
    GBEditValueXp1: TGBEditValue;
    GBEditValueYp1: TGBEditValue;
    GBEditValueZp1: TGBEditValue;
    GBEditValueXp2: TGBEditValue;
    GBEditValueYp2: TGBEditValue;
    GBEditValueZp2: TGBEditValue;
    GBEditValueDistance: TGBEditValue;
    GBEditValueBearing: TGBEditValue;
    GBEditValueDip: TGBEditValue;
    SpinEditPrecision: TSpinEdit;
    Label1: TLabel;
    GBEditValue1: TGBEditValue;
    GBEditValue2: TGBEditValue;
    GBEditValue3: TGBEditValue;
    GBEditValue4: TGBEditValue;
    GBEditValue5: TGBEditValue;
    GBEditValue6: TGBEditValue;
    GBEditValue7: TGBEditValue;
    GBEditValue8: TGBEditValue;
    GBEditValue9: TGBEditValue;
    GBEditValue10: TGBEditValue;
    GBEditValue11: TGBEditValue;
    GBEditValue12: TGBEditValue;
    GBEditValue13: TGBEditValue;
    procedure EditScaleObjectHeightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditScaleDoubleVertThicknessChange(Sender: TObject);
    procedure EditScaleDoubleHorizThicknessChange(Sender: TObject);
    procedure EditScaleDoubleSeamAngleChange(Sender: TObject);
    procedure EditScaleDoubleTrueThicknessChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PageControlChange(Sender: TObject);
    procedure EditValueLineamentChange(Sender: TObject);
    procedure EditValueSeamChange(Sender: TObject);
  private
    FUpdateCount: integer;
    function GetInUpdate: boolean;
    procedure SetInUpdate(Value: boolean);
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property InUpdate: boolean Read GetInUpdate Write SetInUpdate;
  end;

var
  fmToolsGeologyCalculator: TfmToolsGeologyCalculator;

implementation

uses
  cProfuns,
  cGlobals;

{$R *.dfm}


procedure TfmToolsGeologyCalculator.FormCreate(Sender: TObject);
begin
  inherited;
  ReadIniFile;
  PageControlChange(Sender);
end;


procedure TfmToolsGeologyCalculator.PageControlChange(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: EditScaleObjectHeightChange(Sender);
    1: EditScaleDoubleTrueThicknessChange(Sender);
    2: EditValueLineamentChange(Sender);
    3: EditValueSeamChange(Sender);
  end;
end;

procedure TfmToolsGeologyCalculator.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfmToolsGeologyCalculator.EndUpdate;
begin
  FUpdateCount := Max(0, FUpdateCount - 1);
end;

function TfmToolsGeologyCalculator.GetInUpdate: boolean;
begin
  Result := boolean(FUpdateCount);
end;

procedure TfmToolsGeologyCalculator.SetInUpdate(Value: boolean);
begin
  case Value of
    False: EndUpdate;
    True: BeginUpdate;
  end;
end;

//----------------------- Object Height ----------------------------\\
procedure TfmToolsGeologyCalculator.EditScaleObjectHeightChange(Sender: TObject);

begin
  if not inUpdate then
  begin
    BeginUpdate;
    try
      EditScaleObjectHeight.Value :=
        RoundTo(EditScaleDoubleDistance.Value *
        Tan(DegToRad(EditScaleDoubleAngle.Value)) +
        EditScaleDoubleInstrHeight.Value, Precision);
    finally
      EndUpdate;
    end;
  end;
end;


//----------------------- DThick page ------------------------------\\
procedure TfmToolsGeologyCalculator.EditScaleDoubleVertThicknessChange(
  Sender: TObject);
begin
  if not inUpdate then
  begin
    BeginUpdate;
    try
      EditScaleDoubleTrueThickness.Value  :=
        RoundTo(Abs(EditScaleDoubleVertThickness.Value *
        Cos(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value);
      EditScaleDoubleHorizThickness.Value :=
        RoundTo(Abs(EditScaleDoubleVertThickness.Value /
        Tan(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfmToolsGeologyCalculator.EditScaleDoubleHorizThicknessChange(
  Sender: TObject);
begin
  if not inUpdate then
  begin
    inUpdate := True;
    try
      EditScaleDoubleVertThickness.Value :=
        RoundTo(Abs(EditScaleDoubleHorizThickness.Value *
        Tan(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value);
      EditScaleDoubleTrueThickness.Value :=
        RoundTo(Abs(EditScaleDoubleHorizThickness.Value *
        Sin(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value);
    finally
      inUpdate := False;
    end;
  end;
end;

procedure TfmToolsGeologyCalculator.EditScaleDoubleSeamAngleChange(Sender: TObject);
begin
  if not inUpdate then
  begin
    EditScaleDoubleTrueThicknessChange(EditScaleDoubleTrueThickness);
  end;
end;

procedure TfmToolsGeologyCalculator.EditScaleDoubleTrueThicknessChange(
  Sender: TObject);
begin
  if not inUpdate then
  begin
    BeginUpdate;
    try
      if Frac(EditScaleDoubleSeamAngle.Value / 180) <> 0 then //sin <> 0
        EditScaleDoubleHorizThickness.Value :=
          RoundTo(Abs(EditScaleDoubleTrueThickness.Value /
          Sin(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value)
      else if EditScaleDoubleTrueThickness.Value = 0 then
        EditScaleDoubleHorizThickness.Value := 0
      else
        EditScaleDoubleHorizThickness.Text  := 'oo';
      if Frac((EditScaleDoubleSeamAngle.Value + 90) / 180) <> 0 then //cos<>0
        EditScaleDoubleVertThickness.Value :=
          RoundTo(Abs(EditScaleDoubleTrueThickness.Value /
          Cos(DegToRad(EditScaleDoubleSeamAngle.Value))), SpinEditPrecision.Value)
      else if EditScaleDoubleTrueThickness.Value = 0 then
        EditScaleDoubleVertThickness.Value := 0
      else
        EditScaleDoubleVertThickness.Text  := 'oo';
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfmToolsGeologyCalculator.EditValueLineamentChange(Sender: TObject);
begin
  { TODO -oPW -cSurvey : Calculate azimuth and dip }
  GBEditValueDistance.AsDouble :=
    RoundTo(Sqrt(Sqr(GBEditValueXp2.AsDouble - GBEditValueXp1.AsDouble) +
                 Sqr(GBEditValueYp2.AsDouble - GBEditValueYp1.AsDouble) +
                 Sqr(GBEditValueZp2.AsDouble - GBEditValueZp1.AsDouble)), Precision);
end;

procedure TfmToolsGeologyCalculator.EditValueSeamChange(Sender: TObject);
begin
  { TODO 1 -oPW -cSurvey : Calculate azimuth, dip, strike and area }
end;

procedure TfmToolsGeologyCalculator.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  WriteIniFile;
  inherited;
end;

end.
