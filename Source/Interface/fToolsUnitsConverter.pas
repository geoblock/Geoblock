//-----------------------------------------------------------------------------
// This unit is part of the Geoblock, http://sourceforge.net/projects/geoblock
//-----------------------------------------------------------------------------
{! The Dialog for unit conversion }

unit fToolsUnitsConverter;

interface

uses
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
  Vcl.DBGrids,
  Vcl.Mask, 
  Vcl.DBCtrls,
  //DB
  Data.DB, 
  Bde.DBTables,

  fPageDialog,
  GBEditValue,
  GBEditRange,
  gnuGettext;

type
  TfmToolsUnitsConverter = class(TfmPageDialog)
    TabSheetMass:    TTabSheet;
    TabSheetEnergy:  TTabSheet;
    TabSheetAngle:   TTabSheet;
    TabSheetTime:    TTabSheet;
    TabSheetVolume:  TTabSheet;
    TabSheetArea:    TTabSheet;
    TabSheetDensity: TTabSheet;
    TabSheetForce:   TTabSheet;
    TabSheetLength:  TTabSheet;
    TabSheetFlow:    TTabSheet;
    TableUnits:      TTable;
    DataSourceUnits: TDataSource;
    DBGridUnits:     TDBGrid;
    TabSheetTemperature: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure TableUnitsAfterPost(DataSet: TDataSet);
    procedure DBGridUnitsKeyPress(Sender: TObject; var Key: char);
    procedure FormActivate(Sender: TObject);
  private
    FUpdateCount: integer;
    function GetInUpdate: boolean;
    procedure SetInUpdate(Value: boolean);
    procedure SetWorkField;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property inUpdate: boolean Read GetInUpdate Write SetInUpdate;
  end;

var
  fmToolsUnitsConverter: TfmToolsUnitsConverter;

//=================================================================
implementation
//=================================================================

uses
  uGlobals,
  uCommon,
  uResStrings,
  uProfuns;

{$R *.DFM}

{ TfmToolsUnitsConverter }

procedure TfmToolsUnitsConverter.FormCreate(Sender: TObject);
begin
  inherited;
  SetWorkField;
  TableUnits.DatabaseName := ExpandPath(DirDataReference);
  TableUnits.Open;
end;

procedure TfmToolsUnitsConverter.FormActivate(Sender: TObject);
begin
  PageControlChange(Self);
end;


procedure TfmToolsUnitsConverter.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfmToolsUnitsConverter.EndUpdate;
begin
  FUpdateCount := Max(0, FUpdateCount - 1);
end;

function TfmToolsUnitsConverter.GetInUpdate: boolean;
begin
  Result := boolean(FUpdateCount);
end;

procedure TfmToolsUnitsConverter.SetInUpdate(Value: boolean);
begin
  case Value of
    False: EndUpdate;
    True: BeginUpdate;
  end;
end;

procedure TfmToolsUnitsConverter.SetWorkField;
begin
  DBGridUnits.Columns.Add;
  DBGridUnits.Columns[0].FieldName := fldNAME;
  DBGridUnits.Columns[0].Title.Caption := _('Parameter'); //LoadResString(@sgbParameter);
  DBGridUnits.Columns[0].Width    := 200;
  DBGridUnits.Columns[0].ReadOnly := True;

  DBGridUnits.Columns.Add;
  DBGridUnits.Columns[1].FieldName := fldVALUE;
  DBGridUnits.Columns[1].Title.Caption := _('Value');//LoadResString(@sgbValue);
  DBGridUnits.Columns[1].Width := 200;

  DBGridUnits.Columns.Add;
  DBGridUnits.Columns[2].FieldName := fldFACTOR;
  DBGridUnits.Columns[2].Title.Caption := _('Factor');//LoadResString(@sgbFactor);
  DBGridUnits.Columns[2].Visible   := False;
end;

procedure TfmToolsUnitsConverter.PageControlChange(Sender: TObject);
begin
  DBGridUnits.Visible := True;
  case PageControl.ActivePage.Tag of
    0: TableUnits.Filter  := fldCATEGORY + ' = ''ANGLE'' ';
    1: TableUnits.Filter  := fldCATEGORY + ' = ''AREA'' ';
    2: TableUnits.Filter  := fldCATEGORY + ' = ''DENSITY'' ';
    3: TableUnits.Filter  := fldCATEGORY + ' = ''ENERGY'' ';
    4: TableUnits.Filter  := fldCATEGORY + ' = ''FLOW'' ';
    5: TableUnits.Filter  := fldCATEGORY + ' = ''FORCE'' ';
    6: TableUnits.Filter  := fldCATEGORY + ' = ''LENGTH'' ';
    7: TableUnits.Filter  := fldCATEGORY + ' = ''MASS'' ';
    8: TableUnits.Filter  := fldCATEGORY + ' = ''TEMPERATURE'' ';
    9: TableUnits.Filter  := fldCATEGORY + ' = ''TIME'' ';
    10: TableUnits.Filter := fldCATEGORY + ' = ''VOLUME'' ';
  end;
  TableUnits.Filtered := True;
end;

procedure TfmToolsUnitsConverter.FormDestroy(Sender: TObject);
begin
  TableUnits.Close;
  inherited;
end;

procedure TfmToolsUnitsConverter.TableUnitsAfterPost(DataSet: TDataSet);
var
  Value: double;
  Short: string;
  RecNo: integer;
  Centigrade, Kelvin, Fahrenheit: double;

begin
  TableUnits.AfterPost := nil;
  try
    RecNo := TableUnits.RecNo;
    if PageControl.ActivePage.PageIndex <> 8 then // or Tag:= 8 for Tempreture
    begin
      Value := TableUnits.FieldByName(fldVALUE).AsFloat *
        TableUnits.FieldByName(fldFACTOR).AsFloat;
      if TableUnits.FindFirst then
        repeat
          TableUnits.Edit;
          TableUnits.FieldByName(fldVALUE).AsFloat :=
            RoundTo(Value / TableUnits.FieldByName(fldFACTOR).AsFloat, -2);
          TableUnits.Post;
        until not TableUnits.FindNext;
    end
    else   //Temperature Scales
    begin
      Value := TableUnits.FieldByName(fldVALUE).AsFloat;
      Value := RoundTo(Value, -2);
      Short := TableUnits.FieldByName(fldSHORT).AsString;
      if Short = 'C' then
      begin
        if Value < -273 then
          Value := -273;
        Centigrade := Value;
        Kelvin     := Value + 273;
        Fahrenheit := ((9 / 5) * Value) + 32;
      end
      else
      if Short = 'K' then
      begin
        if Value < 0 then
          Value := 0;
        Centigrade := Value - 273;
        Kelvin     := Value;
        Fahrenheit := ((9 / 5) * Centigrade) + 32;
      end
      else
      if Short = 'F' then
      begin
        if Value < -459.4 then
          Value := -459.4;
        Fahrenheit := Value;
        Centigrade := ((5 / 9) * Fahrenheit) - 32;
        Kelvin     := Centigrade + 273;
      end;
      if TableUnits.FindFirst then
      begin
        TableUnits.Edit;
        TableUnits.FieldByName(fldVALUE).AsFloat := Centigrade;
        TableUnits.Post;
      end;
      if TableUnits.FindNext then
      begin
        TableUnits.Edit;
        TableUnits.FieldByName(fldVALUE).AsFloat := Kelvin;
        TableUnits.Post;
      end;
      if TableUnits.FindNext then
      begin
        TableUnits.Edit;
        TableUnits.FieldByName(fldVALUE).AsFloat := Fahrenheit;
        TableUnits.Post;
      end;
    end;
    TableUnits.RecNo := RecNo;
  finally
    TableUnits.AfterPost := TableUnitsAfterPost;
  end;
end;

procedure TfmToolsUnitsConverter.DBGridUnitsKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and (TableUnits.State = dsEdit) then
    TableUnits.Post;
end;

(*//Angle
procedure TfmToolsUnitsConverter.EditScaleDoubleAngleDegreesChange(Sender: TObject);
var
  Deg : Variant;
begin
  if not inUpdate then
  begin
    BeginUpdate;
    try
      Deg := EditScaleDoubleAngleDegrees.Value;
      EditScaleDoubleAngleRadians.Value := DegToRad(EditScaleDoubleAngleDegrees.Value);
      EditScaleDoubleAngleDeg.Value := ((Int(Deg)/360-Int(Int(Deg)/360))*360);
      Deg := Deg*60;
      EditScaleDoubleAngleMin.Value := Abs(Trunc(Deg)) mod 60;
      Deg := Deg*60;
      EditScaleDoubleAngleSec.Value := Abs(Trunc(Deg) mod 60) + Abs(Frac(Deg)) ;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfmToolsUnitsConverter.EditScaleDoubleAngleDegChange(Sender: TObject);
begin
  if not inUpdate then
  begin
    BeginUpdate;
    try
      EditScaleDoubleAngleDegrees.Value :=
            EditScaleDoubleAngleDeg.Value+
            EditScaleDoubleAngleMin.Value/60+
            EditScaleDoubleAngleSec.Value/3600;
      EditScaleDoubleAngleRadians.Value :=
            DegToRad(EditScaleDoubleAngleDegrees.Value);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfmToolsUnitsConverter.EditScaleDoubleAngleRadiansChange(Sender: TObject);
begin
  EditScaleDoubleAngleDegrees.Value := RadToDeg(EditScaleDoubleAngleRadians.Value);
end;
*)

end.
